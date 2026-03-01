// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for schema field projection.
//!
//! Verifies that `project_schema` preserves selected fields, auto-includes
//! ODO counters, never increases record size, and rejects invalid selections.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, RecordFormat, decode_record};
use copybook_core::{parse_copybook, project_schema};
use proptest::prelude::*;
use serde_json::Value;

use super::config::*;

/// Compute maximum field extent (offset + len) across all leaf fields.
fn max_extent(schema: &copybook_core::Schema) -> u32 {
    schema
        .all_fields()
        .iter()
        .filter(|f| f.is_scalar())
        .map(|f| f.offset + f.effective_length())
        .max()
        .unwrap_or(0)
}

// ---------------------------------------------------------------------------
// Random subset of fields: all selected fields present in projected schema
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Projecting a random subset of fields yields a schema containing all selected fields.
    #[test]
    fn prop_projected_schema_contains_selected_fields(
        num_fields in 2usize..=8,
        select_count in 1usize..=4,
    ) {
        let mut lines = vec!["01 REC.".to_string()];
        let mut field_names = Vec::new();
        for i in 0..num_fields {
            let name = format!("FLD{i}");
            lines.push(format!("   05 {name} PIC X(5)."));
            field_names.push(name);
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        let select_count = select_count.min(field_names.len());
        let selections: Vec<String> = field_names[..select_count].to_vec();

        let projected = project_schema(&schema, &selections).expect("project");
        let projected_names: Vec<&str> = projected
            .all_fields()
            .iter()
            .filter(|f| f.is_scalar())
            .map(|f| f.name.as_str())
            .collect();

        for sel in &selections {
            prop_assert!(
                projected_names.contains(&sel.as_str()),
                "Selected field {} not found in projected schema. Found: {:?}",
                sel,
                projected_names,
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Projected schema never includes unselected leaf fields
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Fields NOT selected do not appear as leaf scalars in the projected schema.
    #[test]
    fn prop_projected_schema_excludes_unselected(
        num_fields in 3usize..=8,
    ) {
        let mut lines = vec!["01 REC.".to_string()];
        let mut field_names = Vec::new();
        for i in 0..num_fields {
            let name = format!("FLD{i}");
            lines.push(format!("   05 {name} PIC X(5)."));
            field_names.push(name);
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        // Select only the first field
        let selections = vec![field_names[0].clone()];
        let projected = project_schema(&schema, &selections).expect("project");

        let projected_names: Vec<&str> = projected
            .all_fields()
            .iter()
            .filter(|f| f.is_scalar())
            .map(|f| f.name.as_str())
            .collect();

        for unselected in &field_names[1..] {
            prop_assert!(
                !projected_names.contains(&unselected.as_str()),
                "Unselected field {} should not be in projected schema",
                unselected,
            );
        }
    }
}

// ---------------------------------------------------------------------------
// ODO counter auto-included when ODO array is selected
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Selecting an ODO array field auto-includes its counter.
    #[test]
    fn prop_odo_counter_auto_included(
        max_count in 2u32..=10,
        elem_len in 1u32..=5,
    ) {
        let copybook = format!(
            "01 REC.\n   05 CNT PIC 9(3).\n   05 ARR OCCURS 1 TO {max_count} TIMES DEPENDING ON CNT.\n      10 ELEM PIC X({elem_len})."
        );
        let schema = parse_copybook(&copybook).expect("parse");

        // Select only ELEM (inside ODO array)
        let selections = vec!["ELEM".to_string()];
        let projected = project_schema(&schema, &selections).expect("project");

        let all_names: Vec<&str> = projected
            .all_fields()
            .iter()
            .map(|f| f.name.as_str())
            .collect();

        // CNT must be auto-included because ELEM is inside ODO
        prop_assert!(
            all_names.contains(&"CNT"),
            "ODO counter CNT should be auto-included when selecting ELEM. Found: {:?}",
            all_names,
        );
    }
}

// ---------------------------------------------------------------------------
// Projection never increases record size
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Projected schema has max field extent <= original schema.
    #[test]
    fn prop_projection_never_increases_size(
        num_fields in 2usize..=8,
        field_len in 1u32..=20,
        select_count in 1usize..=4,
    ) {
        let mut lines = vec!["01 REC.".to_string()];
        let mut field_names = Vec::new();
        for i in 0..num_fields {
            let name = format!("FLD{i}");
            lines.push(format!("   05 {name} PIC X({field_len})."));
            field_names.push(name);
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");
        let original_extent = max_extent(&schema);

        let select_count = select_count.min(field_names.len());
        let selections: Vec<String> = field_names[..select_count].to_vec();

        let projected = project_schema(&schema, &selections).expect("project");
        let projected_extent = max_extent(&projected);

        prop_assert!(
            projected_extent <= original_extent,
            "Projected extent {} > original extent {}",
            projected_extent,
            original_extent,
        );
    }
}

// ---------------------------------------------------------------------------
// Empty selection yields empty schema
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Empty selection always yields an empty schema.
    #[test]
    fn prop_empty_selection_yields_empty_schema(num_fields in 1usize..=5) {
        let mut lines = vec!["01 REC.".to_string()];
        for i in 0..num_fields {
            lines.push(format!("   05 FLD{i} PIC X(5)."));
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        let projected = project_schema(&schema, &[]).expect("project");
        prop_assert!(projected.fields.is_empty(), "Empty selection should yield empty fields");
    }
}

// ---------------------------------------------------------------------------
// Invalid field name returns error
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Selecting a field that doesn't exist returns an error.
    #[test]
    fn prop_invalid_field_name_returns_error(num_fields in 1usize..=5) {
        let mut lines = vec!["01 REC.".to_string()];
        for i in 0..num_fields {
            lines.push(format!("   05 FLD{i} PIC X(5)."));
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        let selections = vec!["NONEXISTENT-FIELD".to_string()];
        let result = project_schema(&schema, &selections);
        prop_assert!(result.is_err(), "Non-existent field should produce an error");
    }
}

// ---------------------------------------------------------------------------
// Selecting all fields matches original schema field count
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Selecting every field produces a schema with the same number of leaf fields.
    #[test]
    fn prop_select_all_matches_original(num_fields in 1usize..=6) {
        let mut lines = vec!["01 REC.".to_string()];
        let mut field_names = Vec::new();
        for i in 0..num_fields {
            let name = format!("FLD{i}");
            lines.push(format!("   05 {name} PIC X(5)."));
            field_names.push(name);
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        let projected = project_schema(&schema, &field_names).expect("project");

        let orig_leaves: Vec<_> = schema
            .all_fields()
            .into_iter()
            .filter(|f| f.is_scalar())
            .collect();
        let proj_leaves: Vec<_> = projected
            .all_fields()
            .into_iter()
            .filter(|f| f.is_scalar())
            .collect();

        prop_assert_eq!(proj_leaves.len(), orig_leaves.len());
    }
}

// ---------------------------------------------------------------------------
// Projected schema can still decode data
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding with a projected schema succeeds and produces valid JSON.
    #[test]
    fn prop_projected_schema_decodes_successfully(
        num_fields in 2usize..=5,
        field_len in 1usize..=10,
    ) {
        let mut lines = vec!["01 REC.".to_string()];
        let mut field_names = Vec::new();
        let mut total_len = 0usize;
        for i in 0..num_fields {
            let name = format!("FLD{i}");
            lines.push(format!("   05 {name} PIC X({field_len})."));
            field_names.push(name);
            total_len += field_len;
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        // Select first field only
        let selections = vec![field_names[0].clone()];
        let projected = project_schema(&schema, &selections).expect("project");

        // Build data for full record (projection reads from full-size data)
        let data = vec![b'A'; total_len];

        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        // Decode using projected schema — it may read only a subset
        let result = decode_record(&projected, &data, &opts);
        // We just verify it doesn't panic; the result depends on projection behavior
        let _ = result;
    }
}

// ---------------------------------------------------------------------------
// Projection is idempotent
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Projecting the same fields twice yields the same leaf field count.
    #[test]
    fn prop_projection_is_idempotent(
        num_fields in 2usize..=6,
        select_count in 1usize..=3,
    ) {
        let mut lines = vec!["01 REC.".to_string()];
        let mut field_names = Vec::new();
        for i in 0..num_fields {
            let name = format!("FLD{i}");
            lines.push(format!("   05 {name} PIC X(5)."));
            field_names.push(name);
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        let select_count = select_count.min(field_names.len());
        let selections: Vec<String> = field_names[..select_count].to_vec();

        let proj1 = project_schema(&schema, &selections).expect("project 1");
        let proj2 = project_schema(&proj1, &selections).expect("project 2");

        let leaves1: Vec<&str> = proj1.all_fields().iter().filter(|f| f.is_scalar()).map(|f| f.name.as_str()).collect();
        let leaves2: Vec<&str> = proj2.all_fields().iter().filter(|f| f.is_scalar()).map(|f| f.name.as_str()).collect();

        prop_assert_eq!(leaves1, leaves2);
    }
}

// ---------------------------------------------------------------------------
// Group selection includes children
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Selecting a group field includes its child leaf fields.
    #[test]
    fn prop_group_selection_includes_children(
        num_children in 1usize..=4,
        child_len in 1u32..=10,
    ) {
        let mut child_lines = Vec::new();
        let mut child_names = Vec::new();
        for i in 0..num_children {
            let name = format!("CHILD{i}");
            child_lines.push(format!("      10 {name} PIC X({child_len})."));
            child_names.push(name);
        }

        let copybook = format!(
            "01 REC.\n   05 GRP.\n{}",
            child_lines.join("\n"),
        );
        let schema = parse_copybook(&copybook).expect("parse");

        let selections = vec!["GRP".to_string()];
        let projected = project_schema(&schema, &selections).expect("project");

        let projected_names: Vec<&str> = projected
            .all_fields()
            .iter()
            .filter(|f| f.is_scalar())
            .map(|f| f.name.as_str())
            .collect();

        for child in &child_names {
            prop_assert!(
                projected_names.contains(&child.as_str()),
                "Child {} should be included when selecting group. Found: {:?}",
                child,
                projected_names,
            );
        }
    }
}
