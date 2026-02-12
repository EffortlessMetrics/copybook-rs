//! Field projection for copybook schemas
//!
//! Provides functionality to create subset schemas by selecting specific fields
//! from a copybook, with automatic dependency resolution for ODO counters and
//! parent groups.

use crate::schema::{Field, Occurs, Schema};
use crate::{Error, ErrorCode, Result};
use std::collections::HashSet;

/// Project schema to include only selected fields plus dependencies
///
/// This function creates a new schema containing only the selected fields
/// and their required dependencies:
/// - ODO counter fields (DEPENDING ON) are automatically included
/// - Parent groups are included to maintain structure
/// - RENAMES aliases are resolved using `Schema::resolve_alias_to_target()`
/// - Group selection includes all child fields
///
/// # Arguments
/// * `schema` - Source schema to project from
/// * `selections` - List of field names or paths to include
///
/// # Returns
/// A new `Schema` with the projected subset of fields
///
/// # Errors
/// - `CBKS701_PROJECTION_INVALID_ODO`: ODO array selected but counter not accessible
/// - `CBKS702_PROJECTION_UNRESOLVED_ALIAS`: RENAMES alias spans unselected fields
/// - `CBKS703_PROJECTION_FIELD_NOT_FOUND`: Selected field doesn't exist in schema
///
/// # Example
/// ```text
/// use copybook_core::{parse_copybook, projection::project_schema};
///
/// let copybook = "01 CUSTOMER.\n   05 ID PIC 9(6).\n   05 NAME PIC X(30).";
/// let schema = parse_copybook(copybook)?;
/// let projected = project_schema(&schema, &["ID".to_string()])?;
/// ```
pub fn project_schema(schema: &Schema, selections: &[String]) -> Result<Schema> {
    if selections.is_empty() {
        return Ok(Schema::from_fields(Vec::new()));
    }

    // Step 1: Normalize and resolve field selections
    let mut selected_paths = HashSet::new();

    for selection in selections {
        let normalized = selection.trim();

        // Try to find the field by path or name
        let field = find_field_by_name_or_path(schema, normalized).ok_or_else(|| {
            Error::new(
                ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND,
                format!("Field '{}' not found in schema", normalized),
            )
        })?;

        // If it's a level-66 RENAMES alias, expand to target fields
        if field.level == 66 {
            if let Some(ref resolved) = field.resolved_renames {
                // Add all member fields from the alias
                for member_path in &resolved.members {
                    selected_paths.insert(member_path.clone());

                    if let Some(member_field) = schema.find_field(member_path) {
                        // If member is a group, collect all its children
                        if member_field.is_group() {
                            collect_group_fields(member_field, &mut selected_paths);
                        }
                        // Always include level-88 condition children when present
                        collect_level88_children(member_field, &mut selected_paths);
                    }
                }
            } else {
                // RENAMES field without resolved members - this shouldn't happen
                return Err(Error::new(
                    ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS,
                    format!("RENAMES alias '{}' has no resolved members", field.name),
                ));
            }
        } else {
            // Regular field or group
            selected_paths.insert(field.path.clone());

            // If it's a group, include all children
            if field.is_group() {
                collect_group_fields(field, &mut selected_paths);
            }
            // Always include level-88 condition children when present
            collect_level88_children(field, &mut selected_paths);
        }
    }

    // Step 2: Add parent groups first so ODO detection sees ancestor arrays
    add_parent_groups(schema, &mut selected_paths);

    // Step 3: Find and add ODO counter dependencies (after parents are present)
    let odo_counters = find_odo_counters(schema, &schema.fields, &selected_paths);
    if !odo_counters.is_empty() {
        selected_paths.extend(odo_counters);
        // Ensure newly added counters bring their parent groups along
        add_parent_groups(schema, &mut selected_paths);
    }

    // Step 4: Validate projection has no structural errors
    validate_projection(schema, &selected_paths)?;

    // Step 5: Build new schema with filtered field tree
    let projected_fields = filter_fields(&schema.fields, &selected_paths);

    // Step 6: Create new schema with recalculated properties
    let mut projected_schema = Schema::from_fields(projected_fields);

    // Update tail_odo if the ODO array is included
    if let Some(ref tail_odo) = schema.tail_odo
        && let Some(array_field) = find_field_by_name_or_path(schema, &tail_odo.array_path)
        && selected_paths.contains(&array_field.path)
    {
        projected_schema.tail_odo = Some(tail_odo.clone());
    }

    // Preserve the physical record contract regardless of projection contents
    projected_schema.lrecl_fixed = schema.lrecl_fixed;

    // Recalculate fingerprint for the new schema
    projected_schema.calculate_fingerprint();

    Ok(projected_schema)
}

/// Find a field by name or full path
///
/// First tries exact path match, then searches by field name recursively
fn find_field_by_name_or_path<'a>(schema: &'a Schema, name_or_path: &str) -> Option<&'a Field> {
    // Try exact path match first
    if let Some(field) = schema.find_field(name_or_path) {
        return Some(field);
    }

    // Try alias lookup (level-66)
    if let Some(field) = schema.find_field_or_alias(name_or_path) {
        return Some(field);
    }

    // Search by name in all fields
    find_field_by_name_recursive(&schema.fields, name_or_path)
}

/// Recursively search for a field by name (case-insensitive)
fn find_field_by_name_recursive<'a>(fields: &'a [Field], name: &str) -> Option<&'a Field> {
    for field in fields {
        // Check if this field's name matches (case-insensitive)
        if field.name.eq_ignore_ascii_case(name) {
            return Some(field);
        }
        // Recurse into children
        if let Some(found) = find_field_by_name_recursive(&field.children, name) {
            return Some(found);
        }
    }
    None
}

/// Collect all fields under a group recursively
fn collect_group_fields(field: &Field, collected: &mut HashSet<String>) {
    for child in &field.children {
        collected.insert(child.path.clone());
        if child.is_group() {
            collect_group_fields(child, collected);
        }
    }
}

/// Collect level-88 condition children for a field
fn collect_level88_children(field: &Field, collected: &mut HashSet<String>) {
    for child in &field.children {
        if child.level == 88 {
            collected.insert(child.path.clone());
        }
    }
}

/// Find ODO counter dependencies in selected fields
///
/// Returns full paths to counter fields that need to be included
fn find_odo_counters(
    schema: &Schema,
    fields: &[Field],
    selected: &HashSet<String>,
) -> HashSet<String> {
    fn scan_fields(
        schema: &Schema,
        fields: &[Field],
        selected: &HashSet<String>,
        counters: &mut HashSet<String>,
    ) {
        for field in fields {
            // Check if this field is selected and has ODO
            if selected.contains(&field.path)
                && let Some(Occurs::ODO { counter_path, .. }) = &field.occurs
                && let Some(counter_field) = find_field_by_name_or_path(schema, counter_path)
            {
                counters.insert(counter_field.path.clone());
            }

            // Recurse into children
            scan_fields(schema, &field.children, selected, counters);
        }
    }

    let mut counters = HashSet::new();
    scan_fields(schema, fields, selected, &mut counters);
    counters
}

/// Add parent groups to maintain structural integrity
fn add_parent_groups(schema: &Schema, selected: &mut HashSet<String>) {
    let paths_to_check: Vec<String> = selected.iter().cloned().collect();

    for path in paths_to_check {
        // Walk up the path hierarchy
        let mut current_path = path.as_str();

        while let Some(parent_path) = get_parent_path(current_path) {
            if selected.insert(parent_path.to_string()) {
                // If we added a new parent, check if there's a field for it
                if let Some(_parent_field) = schema.find_field(parent_path) {
                    // Parent exists and was added to selected set
                }
            }
            current_path = parent_path;
        }
    }
}

/// Get parent path from a field path (e.g., "A.B.C" -> "A.B")
fn get_parent_path(path: &str) -> Option<&str> {
    path.rfind('.').map(|idx| &path[..idx])
}

/// Validate projection has no structural errors
fn validate_projection(schema: &Schema, selected: &HashSet<String>) -> Result<()> {
    // Check all selected ODO fields have accessible counters
    for path in selected {
        if let Some(field) = schema.find_field(path)
            && let Some(Occurs::ODO { counter_path, .. }) = &field.occurs
        {
            // Resolve counter_path to full path
            if let Some(counter_field) = find_field_by_name_or_path(schema, counter_path) {
                if !selected.contains(&counter_field.path) {
                    return Err(Error::new(
                        ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
                        format!(
                            "ODO array '{}' requires counter '{}' which is not selected",
                            field.path, counter_path
                        ),
                    ));
                }
            } else {
                // Counter not found in schema - this is also an error
                return Err(Error::new(
                    ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
                    format!(
                        "ODO array '{}' references non-existent counter '{}'",
                        field.path, counter_path
                    ),
                ));
            }
        }
    }

    Ok(())
}

/// Filter fields to include only selected paths
fn filter_fields(fields: &[Field], selected: &HashSet<String>) -> Vec<Field> {
    let mut result = Vec::new();

    for field in fields {
        if selected.contains(&field.path) {
            // Clone the field and recursively filter its children
            let mut filtered_field = field.clone();
            filtered_field.children = filter_fields(&field.children, selected);
            result.push(filtered_field);
        }
    }

    result
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;
    use crate::schema::{FieldKind, ResolvedRenames, TailODO};

    fn create_simple_schema() -> Schema {
        let mut root = Field::new(1, "ROOT".to_string());
        root.path = "ROOT".to_string();
        root.kind = FieldKind::Group;

        let mut field1 = Field::new(5, "FIELD1".to_string());
        field1.path = "ROOT.FIELD1".to_string();
        field1.kind = FieldKind::Alphanum { len: 10 };
        field1.len = 10;

        let mut field2 = Field::new(5, "FIELD2".to_string());
        field2.path = "ROOT.FIELD2".to_string();
        field2.kind = FieldKind::ZonedDecimal {
            digits: 5,
            scale: 0,
            signed: false,
            sign_separate: None,
        };
        field2.len = 5;

        root.children = vec![field1, field2];

        Schema::from_fields(vec![root])
    }

    #[test]
    fn test_simple_field_selection() {
        let schema = create_simple_schema();
        let projected = project_schema(&schema, &["FIELD1".to_string()]).unwrap();

        // Should include ROOT (parent), ROOT.FIELD1
        assert_eq!(projected.fields.len(), 1);
        assert_eq!(projected.fields[0].name, "ROOT");
        assert_eq!(projected.fields[0].children.len(), 1);
        assert_eq!(projected.fields[0].children[0].name, "FIELD1");
    }

    #[test]
    fn test_nonexistent_field() {
        let schema = create_simple_schema();
        let result = project_schema(&schema, &["NONEXISTENT".to_string()]);

        assert!(result.is_err());
        if let Err(err) = result {
            assert_eq!(err.code, ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND);
        }
    }

    #[test]
    fn test_group_selection_includes_children() {
        let schema = create_simple_schema();
        let projected = project_schema(&schema, &["ROOT".to_string()]).unwrap();

        // Selecting ROOT should include all children
        assert_eq!(projected.fields.len(), 1);
        assert_eq!(projected.fields[0].name, "ROOT");
        assert_eq!(projected.fields[0].children.len(), 2);
    }

    #[test]
    fn test_odo_counter_auto_included() {
        let mut root = Field::new(1, "ROOT".to_string());
        root.path = "ROOT".to_string();
        root.kind = FieldKind::Group;

        let mut counter = Field::new(5, "COUNTER".to_string());
        counter.path = "ROOT.COUNTER".to_string();
        counter.kind = FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
            sign_separate: None,
        };
        counter.len = 3;

        let mut odo_array = Field::new(5, "ITEMS".to_string());
        odo_array.path = "ROOT.ITEMS".to_string();
        odo_array.kind = FieldKind::Group;
        odo_array.occurs = Some(Occurs::ODO {
            min: 1,
            max: 10,
            counter_path: "ROOT.COUNTER".to_string(),
        });

        let mut item_field = Field::new(10, "ITEM_ID".to_string());
        item_field.path = "ROOT.ITEMS.ITEM_ID".to_string();
        item_field.kind = FieldKind::Alphanum { len: 5 };
        item_field.len = 5;

        odo_array.children = vec![item_field];
        root.children = vec![counter, odo_array];

        let schema = Schema::from_fields(vec![root]);

        // Select only ITEMS - counter should be auto-included
        let projected = project_schema(&schema, &["ITEMS".to_string()]).unwrap();

        // Should have ROOT with both COUNTER and ITEMS
        assert_eq!(projected.fields.len(), 1);
        assert_eq!(projected.fields[0].children.len(), 2);

        let child_names: Vec<&str> = projected.fields[0]
            .children
            .iter()
            .map(|f| f.name.as_str())
            .collect();
        assert!(child_names.contains(&"COUNTER"));
        assert!(child_names.contains(&"ITEMS"));
    }

    #[test]
    fn test_odo_counter_added_when_selecting_leaf() {
        let mut root = Field::new(1, "ROOT".to_string());
        root.path = "ROOT".to_string();
        root.kind = FieldKind::Group;

        let mut counter = Field::new(5, "CTR".to_string());
        counter.path = "ROOT.CTR".to_string();
        counter.kind = FieldKind::ZonedDecimal {
            digits: 2,
            scale: 0,
            signed: false,
            sign_separate: None,
        };
        counter.len = 2;

        let mut odo_array = Field::new(5, "ITEMS".to_string());
        odo_array.path = "ROOT.ITEMS".to_string();
        odo_array.kind = FieldKind::Group;
        odo_array.occurs = Some(Occurs::ODO {
            min: 0,
            max: 5,
            counter_path: "ROOT.CTR".to_string(),
        });

        let mut item_field = Field::new(10, "ITEM_ID".to_string());
        item_field.path = "ROOT.ITEMS.ITEM_ID".to_string();
        item_field.kind = FieldKind::Alphanum { len: 3 };
        item_field.len = 3;

        odo_array.children = vec![item_field];
        root.children = vec![counter, odo_array];

        let schema = Schema::from_fields(vec![root]);

        // Select only the leaf inside the ODO array; counter should still be added
        let projected = project_schema(&schema, &["ITEM_ID".to_string()]).unwrap();
        let root_children = &projected.fields[0].children;
        assert_eq!(root_children.len(), 2);
        assert!(root_children.iter().any(|f| f.name == "CTR"));
        assert!(root_children.iter().any(|f| f.name == "ITEMS"));
    }

    #[test]
    fn test_renames_alias_expansion() {
        let mut root = Field::new(1, "ROOT".to_string());
        root.path = "ROOT".to_string();
        root.kind = FieldKind::Group;

        let mut field1 = Field::new(5, "FIELD1".to_string());
        field1.path = "ROOT.FIELD1".to_string();
        field1.kind = FieldKind::Alphanum { len: 10 };
        field1.len = 10;

        let mut field2 = Field::new(5, "FIELD2".to_string());
        field2.path = "ROOT.FIELD2".to_string();
        field2.kind = FieldKind::Alphanum { len: 10 };
        field2.len = 10;

        // Create level-66 RENAMES alias
        let mut alias = Field::new(66, "ALIAS".to_string());
        alias.path = "ROOT.ALIAS".to_string();
        alias.level = 66;
        alias.kind = FieldKind::Renames {
            from_field: "FIELD1".to_string(),
            thru_field: "FIELD2".to_string(),
        };
        alias.resolved_renames = Some(ResolvedRenames {
            offset: 0,
            length: 20,
            members: vec!["ROOT.FIELD1".to_string(), "ROOT.FIELD2".to_string()],
        });

        root.children = vec![field1, field2, alias];

        let schema = Schema::from_fields(vec![root]);

        // Select ALIAS - should expand to FIELD1 and FIELD2
        let projected = project_schema(&schema, &["ALIAS".to_string()]).unwrap();

        // Should have ROOT with FIELD1 and FIELD2 (not the alias itself)
        assert_eq!(projected.fields.len(), 1);
        assert_eq!(projected.fields[0].children.len(), 2);

        let child_names: Vec<&str> = projected.fields[0]
            .children
            .iter()
            .map(|f| f.name.as_str())
            .collect();
        assert!(child_names.contains(&"FIELD1"));
        assert!(child_names.contains(&"FIELD2"));
    }

    #[test]
    fn test_empty_selection() {
        let schema = create_simple_schema();
        let projected = project_schema(&schema, &[]).unwrap();

        assert_eq!(projected.fields.len(), 0);
    }

    #[test]
    fn test_collect_group_fields() {
        let mut group = Field::new(5, "GROUP".to_string());
        group.path = "GROUP".to_string();
        group.kind = FieldKind::Group;

        let mut child1 = Field::new(10, "CHILD1".to_string());
        child1.path = "GROUP.CHILD1".to_string();
        child1.kind = FieldKind::Alphanum { len: 5 };

        let mut child2 = Field::new(10, "CHILD2".to_string());
        child2.path = "GROUP.CHILD2".to_string();
        child2.kind = FieldKind::Alphanum { len: 5 };

        group.children = vec![child1, child2];

        let mut collected = HashSet::new();
        collect_group_fields(&group, &mut collected);

        assert_eq!(collected.len(), 2);
        assert!(collected.contains("GROUP.CHILD1"));
        assert!(collected.contains("GROUP.CHILD2"));
    }

    #[test]
    fn test_lrecl_preserved_even_with_tail_odo() {
        let mut root = Field::new(1, "ROOT".to_string());
        root.path = "ROOT".to_string();
        root.kind = FieldKind::Group;

        let mut counter = Field::new(5, "CTR".to_string());
        counter.path = "ROOT.CTR".to_string();
        counter.kind = FieldKind::ZonedDecimal {
            digits: 2,
            scale: 0,
            signed: false,
            sign_separate: None,
        };
        counter.len = 2;

        let mut odo_array = Field::new(5, "ITEMS".to_string());
        odo_array.path = "ROOT.ITEMS".to_string();
        odo_array.kind = FieldKind::Group;
        odo_array.occurs = Some(Occurs::ODO {
            min: 0,
            max: 5,
            counter_path: "ROOT.CTR".to_string(),
        });

        let mut item_field = Field::new(10, "ITEM_ID".to_string());
        item_field.path = "ROOT.ITEMS.ITEM_ID".to_string();
        item_field.kind = FieldKind::Alphanum { len: 3 };
        item_field.len = 3;

        odo_array.children = vec![item_field];
        root.children = vec![counter, odo_array];

        let mut schema = Schema::from_fields(vec![root]);
        schema.lrecl_fixed = Some(32);
        schema.tail_odo = Some(TailODO {
            counter_path: "ROOT.CTR".to_string(),
            min_count: 0,
            max_count: 5,
            array_path: "ROOT.ITEMS".to_string(),
        });

        let projected = project_schema(&schema, &["CTR".to_string()]).unwrap();
        assert_eq!(projected.lrecl_fixed, Some(32));
        assert!(projected.tail_odo.is_none());
    }
}
