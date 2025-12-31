//! Integration tests for field projection functionality
//!
//! Tests the `project_schema` function with various COBOL copybook patterns
//! including ODO dependencies, RENAMES aliases, and nested groups.

use copybook_core::{ErrorCode, parse_copybook, project_schema};

#[test]
fn test_projection_simple_field_selection() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  CUSTOMER-NAME       PIC X(30).
            05  CUSTOMER-ADDRESS    PIC X(50).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");
    let projected =
        project_schema(&schema, &["CUSTOMER-ID".to_string()]).expect("Projection failed");

    // Should include CUSTOMER-RECORD (parent) and CUSTOMER-ID
    assert_eq!(projected.fields.len(), 1);
    assert_eq!(projected.fields[0].name, "CUSTOMER-RECORD");
    assert_eq!(projected.fields[0].children.len(), 1);
    assert_eq!(projected.fields[0].children[0].name, "CUSTOMER-ID");
}

#[test]
fn test_projection_multiple_fields() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  CUSTOMER-NAME       PIC X(30).
            05  CUSTOMER-ADDRESS    PIC X(50).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");
    let projected = project_schema(
        &schema,
        &["CUSTOMER-ID".to_string(), "CUSTOMER-NAME".to_string()],
    )
    .expect("Projection failed");

    // Should include both selected fields
    assert_eq!(projected.fields[0].children.len(), 2);

    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(child_names.contains(&"CUSTOMER-ID"));
    assert!(child_names.contains(&"CUSTOMER-NAME"));
}

#[test]
fn test_projection_group_selection_includes_all_children() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-INFO.
                10  CUSTOMER-ID     PIC 9(6).
                10  CUSTOMER-NAME   PIC X(30).
            05  ORDER-COUNT         PIC 9(3).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");
    let projected =
        project_schema(&schema, &["CUSTOMER-INFO".to_string()]).expect("Projection failed");

    // Selecting group should include all its children
    assert_eq!(projected.fields.len(), 1);
    assert_eq!(projected.fields[0].name, "CUSTOMER-RECORD");
    assert_eq!(projected.fields[0].children.len(), 1);
    assert_eq!(projected.fields[0].children[0].name, "CUSTOMER-INFO");
    assert_eq!(projected.fields[0].children[0].children.len(), 2);
}

#[test]
fn test_projection_odo_auto_includes_counter() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  ORDER-COUNT         PIC 9(3).
            05  ORDERS OCCURS 1 TO 100 TIMES
                    DEPENDING ON ORDER-COUNT.
                10  ORDER-ID        PIC 9(8).
                10  ORDER-AMOUNT    PIC 9(7)V99.
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Select only ORDERS - counter should be auto-included
    let projected = project_schema(&schema, &["ORDERS".to_string()]).expect("Projection failed");

    // Should have both ORDER-COUNT and ORDERS
    assert_eq!(projected.fields[0].children.len(), 2);

    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(
        child_names.contains(&"ORDER-COUNT"),
        "ORDER-COUNT should be auto-included"
    );
    assert!(child_names.contains(&"ORDERS"), "ORDERS should be included");
}

#[test]
fn test_projection_nested_group_with_odo() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  ORDER-INFO.
                10  ORDER-COUNT     PIC 9(3).
                10  ORDERS OCCURS 1 TO 50 TIMES
                        DEPENDING ON ORDER-COUNT.
                    15  ORDER-ID    PIC 9(8).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Select ORDERS - should include ORDER-COUNT and parent ORDER-INFO
    let projected = project_schema(&schema, &["ORDERS".to_string()]).expect("Projection failed");

    // Should have CUSTOMER-RECORD -> ORDER-INFO -> [ORDER-COUNT, ORDERS]
    assert_eq!(projected.fields.len(), 1);
    assert_eq!(projected.fields[0].name, "CUSTOMER-RECORD");
    assert_eq!(projected.fields[0].children.len(), 1);
    assert_eq!(projected.fields[0].children[0].name, "ORDER-INFO");
    assert_eq!(projected.fields[0].children[0].children.len(), 2);

    let order_info_children: Vec<&str> = projected.fields[0].children[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(order_info_children.contains(&"ORDER-COUNT"));
    assert!(order_info_children.contains(&"ORDERS"));
}

#[test]
fn test_projection_renames_alias_expansion() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  FIRST-NAME          PIC X(20).
            05  LAST-NAME           PIC X(30).
            05  MIDDLE-INITIAL      PIC X(1).
            66  FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
            05  ADDRESS             PIC X(50).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Select FULL-NAME alias - should expand to FIRST-NAME and LAST-NAME
    let projected = project_schema(&schema, &["FULL-NAME".to_string()]).expect("Projection failed");

    // Should include FIRST-NAME and LAST-NAME (not MIDDLE-INITIAL or ADDRESS)
    assert_eq!(projected.fields.len(), 1);

    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(child_names.contains(&"FIRST-NAME"));
    assert!(child_names.contains(&"LAST-NAME"));
    assert!(
        !child_names.contains(&"MIDDLE-INITIAL"),
        "MIDDLE-INITIAL should not be included"
    );
    assert!(
        !child_names.contains(&"ADDRESS"),
        "ADDRESS should not be included"
    );
    assert!(
        !child_names.contains(&"FULL-NAME"),
        "FULL-NAME alias itself should not appear as a storage field"
    );
}

#[test]
fn test_projection_renames_with_regular_fields() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  FIRST-NAME          PIC X(20).
            05  LAST-NAME           PIC X(30).
            66  FULL-NAME RENAMES FIRST-NAME THRU LAST-NAME.
            05  CUSTOMER-ID         PIC 9(6).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Select both alias and regular field
    let projected = project_schema(
        &schema,
        &["FULL-NAME".to_string(), "CUSTOMER-ID".to_string()],
    )
    .expect("Projection failed");

    // Should include FIRST-NAME, LAST-NAME, and CUSTOMER-ID
    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert_eq!(child_names.len(), 3);
    assert!(child_names.contains(&"FIRST-NAME"));
    assert!(child_names.contains(&"LAST-NAME"));
    assert!(child_names.contains(&"CUSTOMER-ID"));
}

#[test]
fn test_projection_field_not_found_error() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  CUSTOMER-NAME       PIC X(30).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");
    let result = project_schema(&schema, &["NONEXISTENT-FIELD".to_string()]);

    assert!(result.is_err());
    if let Err(err) = result {
        assert_eq!(err.code, ErrorCode::CBKS703_PROJECTION_FIELD_NOT_FOUND);
        assert!(err.message.contains("NONEXISTENT-FIELD"));
    }
}

#[test]
fn test_projection_empty_selection() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  CUSTOMER-NAME       PIC X(30).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");
    let projected = project_schema(&schema, &[]).expect("Projection failed");

    // Empty selection should return empty schema
    assert_eq!(projected.fields.len(), 0);
}

#[test]
fn test_projection_level88_with_parent_field() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-STATUS     PIC X(1).
                88  STATUS-ACTIVE   VALUE 'A'.
                88  STATUS-INACTIVE VALUE 'I'.
            05  CUSTOMER-ID         PIC 9(6).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Select CUSTOMER-STATUS - should include level-88 conditions as children
    let projected =
        project_schema(&schema, &["CUSTOMER-STATUS".to_string()]).expect("Projection failed");

    assert_eq!(projected.fields.len(), 1);
    assert_eq!(projected.fields[0].name, "CUSTOMER-RECORD");

    // Find CUSTOMER-STATUS field
    let status_field = projected.fields[0]
        .children
        .iter()
        .find(|f| f.name == "CUSTOMER-STATUS")
        .expect("CUSTOMER-STATUS not found");

    // Level-88 conditions should be included as children
    assert_eq!(status_field.children.len(), 2);

    let condition_names: Vec<&str> = status_field
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(condition_names.contains(&"STATUS-ACTIVE"));
    assert!(condition_names.contains(&"STATUS-INACTIVE"));
}

#[test]
fn test_projection_complex_nested_structure() {
    let copybook = r#"
        01  TRANSACTION-RECORD.
            05  HEADER.
                10  TRANSACTION-ID  PIC 9(10).
                10  TIMESTAMP       PIC 9(8).
            05  BODY.
                10  ITEM-COUNT      PIC 9(3).
                10  ITEMS OCCURS 1 TO 100 TIMES
                        DEPENDING ON ITEM-COUNT.
                    15  ITEM-CODE   PIC X(10).
                    15  QUANTITY    PIC 9(5).
            05  FOOTER.
                10  TOTAL-AMOUNT    PIC 9(9)V99.
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Select ITEMS and TOTAL-AMOUNT - should include necessary structure
    let projected = project_schema(&schema, &["ITEMS".to_string(), "TOTAL-AMOUNT".to_string()])
        .expect("Projection failed");

    // Should have TRANSACTION-RECORD with BODY (containing ITEM-COUNT and ITEMS) and FOOTER
    assert_eq!(projected.fields.len(), 1);
    assert_eq!(projected.fields[0].name, "TRANSACTION-RECORD");

    let top_level_children: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();

    // Should include BODY (for ITEMS) and FOOTER (for TOTAL-AMOUNT)
    assert!(top_level_children.contains(&"BODY"));
    assert!(top_level_children.contains(&"FOOTER"));

    // HEADER should not be included
    assert!(!top_level_children.contains(&"HEADER"));
}

#[test]
fn test_projection_preserves_occurs_info() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  ORDER-COUNT         PIC 9(3).
            05  ORDERS OCCURS 1 TO 50 TIMES
                    DEPENDING ON ORDER-COUNT.
                10  ORDER-ID        PIC 9(8).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");
    let projected = project_schema(&schema, &["ORDERS".to_string()]).expect("Projection failed");

    // Find ORDERS field and verify OCCURS information is preserved
    let orders_field = projected.fields[0]
        .children
        .iter()
        .find(|f| f.name == "ORDERS")
        .expect("ORDERS not found");

    assert!(orders_field.occurs.is_some());
    if let Some(copybook_core::Occurs::ODO {
        min,
        max,
        counter_path,
    }) = &orders_field.occurs
    {
        assert_eq!(*min, 1);
        assert_eq!(*max, 50);
        // counter_path may be a short name or full path depending on parser
        assert!(counter_path == "ORDER-COUNT" || counter_path == "CUSTOMER-RECORD.ORDER-COUNT");
    } else {
        panic!("Expected ODO occurs");
    }
}

#[test]
fn test_projection_multiple_odo_arrays() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  ORDER-COUNT         PIC 9(3).
            05  ORDERS OCCURS 1 TO 50 TIMES
                    DEPENDING ON ORDER-COUNT.
                10  ORDER-ID        PIC 9(8).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Select just the ODO array
    let projected = project_schema(&schema, &["ORDERS".to_string()]).expect("Projection failed");

    // Counter should be auto-included
    let child_names: Vec<&str> = projected.fields[0]
        .children
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(child_names.contains(&"ORDER-COUNT"));
    assert!(child_names.contains(&"ORDERS"));
}

#[test]
fn test_projection_tail_odo_preserved() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  ORDER-COUNT         PIC 9(3).
            05  ORDERS OCCURS 1 TO 100 TIMES
                    DEPENDING ON ORDER-COUNT.
                10  ORDER-ID        PIC 9(8).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Verify tail_odo is set in original schema
    assert!(schema.tail_odo.is_some());

    // Project including the ODO array
    let projected = project_schema(&schema, &["ORDERS".to_string()]).expect("Projection failed");

    // tail_odo should be preserved since we included the ODO array
    assert!(projected.tail_odo.is_some());
    if let Some(tail_odo) = &projected.tail_odo {
        // Parser stores short names in tail_odo, not full paths
        assert_eq!(tail_odo.array_path, "ORDERS");
        assert_eq!(tail_odo.counter_path, "ORDER-COUNT");
        assert_eq!(tail_odo.min_count, 1);
        assert_eq!(tail_odo.max_count, 100);
    }
}

#[test]
fn test_projection_tail_odo_cleared_when_not_selected() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  ORDER-COUNT         PIC 9(3).
            05  ORDERS OCCURS 1 TO 100 TIMES
                    DEPENDING ON ORDER-COUNT.
                10  ORDER-ID        PIC 9(8).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Project excluding the ODO array
    let projected =
        project_schema(&schema, &["CUSTOMER-ID".to_string()]).expect("Projection failed");

    // tail_odo should be cleared since ODO array is not included
    assert!(projected.tail_odo.is_none());
}

#[test]
fn test_projection_with_qualified_path() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-INFO.
                10  CUSTOMER-ID     PIC 9(6).
                10  CUSTOMER-NAME   PIC X(30).
            05  ORDER-INFO.
                10  ORDER-COUNT     PIC 9(3).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");

    // Try selecting with qualified path
    let projected = project_schema(
        &schema,
        &["CUSTOMER-RECORD.CUSTOMER-INFO.CUSTOMER-ID".to_string()],
    )
    .expect("Projection failed");

    // Should include the hierarchy
    assert_eq!(projected.fields.len(), 1);
    assert_eq!(projected.fields[0].name, "CUSTOMER-RECORD");
    assert_eq!(projected.fields[0].children.len(), 1);
    assert_eq!(projected.fields[0].children[0].name, "CUSTOMER-INFO");
    assert_eq!(projected.fields[0].children[0].children.len(), 1);
    assert_eq!(
        projected.fields[0].children[0].children[0].name,
        "CUSTOMER-ID"
    );
}

#[test]
fn test_projection_fingerprint_updated() {
    let copybook = r#"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  CUSTOMER-NAME       PIC X(30).
            05  CUSTOMER-ADDRESS    PIC X(50).
    "#;

    let schema = parse_copybook(copybook).expect("Parse failed");
    let original_fingerprint = schema.fingerprint.clone();

    let projected =
        project_schema(&schema, &["CUSTOMER-ID".to_string()]).expect("Projection failed");

    // Projected schema should have different fingerprint
    assert_ne!(projected.fingerprint, original_fingerprint);
    assert!(!projected.fingerprint.is_empty());
}

/// Test CBKS701_PROJECTION_INVALID_ODO: ODO array with non-existent counter
#[test]
fn test_projection_cbks701_invalid_odo_missing_counter() {
    use copybook_core::schema::{Field, FieldKind, Occurs, Schema};

    // Create a synthetic schema with ODO referencing a non-existent counter
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut odo_array = Field::new(5, "ITEMS".to_string());
    odo_array.path = "ROOT.ITEMS".to_string();
    odo_array.kind = FieldKind::Group;
    odo_array.occurs = Some(Occurs::ODO {
        min: 1,
        max: 10,
        counter_path: "NONEXISTENT-COUNTER".to_string(), // Non-existent counter
    });

    let mut item_field = Field::new(10, "ITEM-ID".to_string());
    item_field.path = "ROOT.ITEMS.ITEM-ID".to_string();
    item_field.kind = FieldKind::Alphanum { len: 5 };
    item_field.len = 5;

    odo_array.children = vec![item_field];
    root.children = vec![odo_array];

    let schema = Schema::from_fields(vec![root]);

    // Attempt to project ITEMS without counter - should fail with CBKS701
    let result = project_schema(&schema, &["ITEMS".to_string()]);

    assert!(
        result.is_err(),
        "Expected CBKS701 error for missing counter"
    );
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKS701_PROJECTION_INVALID_ODO,
            "Expected CBKS701_PROJECTION_INVALID_ODO"
        );
        assert!(
            err.message.contains("ITEMS"),
            "Error should mention the ODO array"
        );
    }
}

/// Test CBKS702_PROJECTION_UNRESOLVED_ALIAS: RENAMES without resolved metadata
#[test]
fn test_projection_cbks702_unresolved_alias() {
    use copybook_core::schema::{Field, FieldKind, Schema};

    // Create a synthetic schema with RENAMES that has no resolved_renames
    let mut root = Field::new(1, "ROOT".to_string());
    root.path = "ROOT".to_string();
    root.kind = FieldKind::Group;

    let mut field1 = Field::new(5, "FIELD1".to_string());
    field1.path = "ROOT.FIELD1".to_string();
    field1.kind = FieldKind::Alphanum { len: 10 };
    field1.len = 10;

    // Create level-66 RENAMES without resolved_renames metadata
    let mut alias = Field::new(66, "BROKEN-ALIAS".to_string());
    alias.path = "ROOT.BROKEN-ALIAS".to_string();
    alias.level = 66;
    alias.kind = FieldKind::Renames {
        from_field: "FIELD1".to_string(),
        thru_field: "FIELD1".to_string(),
    };
    // Intentionally NOT setting resolved_renames - this simulates a broken alias
    alias.resolved_renames = None;

    root.children = vec![field1, alias];

    let schema = Schema::from_fields(vec![root]);

    // Attempt to project the broken alias - should fail with CBKS702
    let result = project_schema(&schema, &["BROKEN-ALIAS".to_string()]);

    assert!(
        result.is_err(),
        "Expected CBKS702 error for unresolved alias"
    );
    if let Err(err) = result {
        assert_eq!(
            err.code,
            ErrorCode::CBKS702_PROJECTION_UNRESOLVED_ALIAS,
            "Expected CBKS702_PROJECTION_UNRESOLVED_ALIAS"
        );
        assert!(
            err.message.contains("BROKEN-ALIAS"),
            "Error should mention the alias name"
        );
    }
}
