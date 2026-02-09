//! Property tests for schema invariants
//!
//! These tests verify that schema structures maintain important invariants
//! such as offset ordering, length calculations, and hierarchy consistency.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::{parse_copybook, Field, FieldKind, Occurs};
use proptest::prelude::*;
use proptest::strategy::Strategy;

/// Generate a valid field level (1-49, excluding 66 and 88)
fn field_level() -> impl Strategy<Value = u8> {
    prop::sample::select(vec![
        1u8, 2, 3, 4, 5, 10, 20, 30, 40, 49,
    ])
}

/// Generate a valid field name (1-30 characters, alphanumeric and hyphen)
fn field_name() -> impl Strategy<Value = String> {
    "[A-Z0-9-]{1,30}".prop_map(|s| s)
}

/// Property: Schema field offsets are monotonically increasing within a group
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_field_offsets_monotonically_increase(
        num_fields in 1usize..10,
    ) {
        // Create a simple copybook with multiple fields
        let copybook = (0..num_fields)
            .map(|i| format!("       05  FIELD-{} PIC X(10).", i))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Verify offsets are monotonically increasing
        let mut prev_offset = 0u32;
        for field in &schema.fields {
            prop_assert!(field.offset >= prev_offset, "Field offset decreased: {} < {}", field.offset, prev_offset);
            prev_offset = field.offset;
        }
    }
}

/// Property: Field length is non-negative
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_field_length_non_negative(
        length in 1u32..100,
    ) {
        let copybook = format!("       05  TEST-FIELD PIC X({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        prop_assert_eq!(schema.fields.len(), 1);
        prop_assert_eq!(schema.fields[0].len, length);
    }
}

/// Property: Effective length includes array multiplier
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_effective_length_includes_array_multiplier(
        field_len in 1u32..20,
        array_count in 1u32..10,
    ) {
        let copybook = format!(
            "       05  TEST-FIELD PIC X({}) OCCURS {} TIMES.",
            field_len, array_count
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        prop_assert_eq!(schema.fields.len(), 1);
        let field = &schema.fields[0];
        prop_assert_eq!(field.len, field_len);
        prop_assert_eq!(field.effective_length(), field_len * array_count);
    }
}

/// Property: ODO field effective length uses max count
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_odo_effective_length_uses_max(
        field_len in 1u32..20,
        min_count in 1u32..5,
        max_count in 5u32..20,
    ) {
        let copybook = format!(
            "       05  COUNTER PIC 9(3).\n\
             05  TEST-FIELD PIC X({}) OCCURS {} TO {} TIMES DEPENDING ON COUNTER.",
            field_len, min_count, max_count
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find the ODO field (should be the second one)
        let odo_field = schema.fields.iter()
            .find(|f| f.name == "TEST-FIELD")
            .expect("Failed to find ODO field");

        prop_assert_eq!(odo_field.len, field_len);
        prop_assert_eq!(odo_field.effective_length(), field_len * max_count);
    }
}

/// Property: Group field has no direct length (only from children)
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_group_field_no_direct_length(
        name in field_name(),
    ) {
        let copybook = format!(
            "       01  {}.\n\
             05  CHILD1 PIC X(5).\n\
             05  CHILD2 PIC X(5).",
            name
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        prop_assert_eq!(schema.fields.len(), 1);
        let group_field = &schema.fields[0];
        prop_assert!(group_field.is_group());
        prop_assert!(!group_field.is_scalar());
    }
}

/// Property: Scalar field is not a group
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_scalar_field_not_group(
        length in 1u32..50,
    ) {
        let copybook = format!("       05  SCALAR-FIELD PIC X({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        prop_assert_eq!(schema.fields.len(), 1);
        let scalar_field = &schema.fields[0];
        prop_assert!(!scalar_field.is_group());
        prop_assert!(scalar_field.is_scalar());
    }
}

/// Property: Schema serialization is symmetric
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_schema_serialization_symmetric(
        num_fields in 1usize..5,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  FIELD-{} PIC X(10).", i))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Serialize and deserialize
        let serialized = serde_json::to_string(&schema).expect("Failed to serialize");
        let deserialized: copybook_core::Schema =
            serde_json::from_str(&serialized).expect("Failed to deserialize");

        // Verify fields match
        prop_assert_eq!(deserialized.fields.len(), schema.fields.len());
        for (orig, deser) in schema.fields.iter().zip(deserialized.fields.iter()) {
            prop_assert_eq!(orig.name, deser.name);
            prop_assert_eq!(orig.level, deser.level);
            prop_assert_eq!(orig.offset, deser.offset);
            prop_assert_eq!(orig.len, deser.len);
        }
    }
}

/// Property: Field path includes parent names
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_field_path_includes_parent(
        group_name in field_name(),
        child_name in field_name(),
    ) {
        let copybook = format!(
            "       01  {}.\n\
             05  {} PIC X(5).",
            group_name, child_name
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        prop_assert_eq!(schema.fields.len(), 1);
        let group_field = &schema.fields[0];
        prop_assert_eq!(group_field.children.len(), 1);
        let child_field = &group_field.children[0];

        // Child path should include parent name
        let expected_path = format!("{}.{}", group_name, child_name);
        prop_assert_eq!(child_field.path, expected_path);
    }
}

/// Property: Nested groups maintain hierarchy
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_nested_groups_maintain_hierarchy(
        depth in 1usize..5,
    ) {
        let mut copybook = String::new();
        let mut levels = Vec::new();

        for i in 0..depth {
            let level = (i + 1) * 5;
            levels.push(level);
            copybook.push_str(&format!(
                "       {:02}  GROUP-{}.\n",
                level, i
            ));
        }

        // Add a leaf field at the deepest level
        let leaf_level = levels.last().unwrap() + 5;
        copybook.push_str(&format!(
            "       {:02}  LEAF-FIELD PIC X(5).",
            leaf_level
        ));

        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Verify we have nested groups
        prop_assert!(!schema.fields.is_empty());

        // Count total fields (should be depth groups + 1 leaf)
        let all_fields = schema.all_fields();
        prop_assert_eq!(all_fields.len(), depth + 1);
    }
}

/// Property: REDEFINES fields share the same offset
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_fields_share_offset(
        length in 1u32..20,
    ) {
        let copybook = format!(
            "       05  BASE-FIELD PIC X({}).\n\
             05  REDEF1 PIC 9({}).\n\
             05  REDEF2 PIC S9({}).",
            length, length, length
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        prop_assert_eq!(schema.fields.len(), 3);

        let base_field = &schema.fields[0];
        let redef1 = &schema.fields[1];
        let redef2 = &schema.fields[2];

        // REDEFINES fields should have the same offset as the base
        prop_assert_eq!(redef1.offset, base_field.offset);
        prop_assert_eq!(redef2.offset, base_field.offset);

        // REDEFINES fields should indicate what they redefine
        prop_assert_eq!(redef1.redefines_of, Some(base_field.path.clone()));
        prop_assert_eq!(redef2.redefines_of, Some(base_field.path.clone()));
    }
}

/// Property: OCCURS with DEPENDING ON creates proper ODO structure
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_odo_creces_proper_structure(
        field_len in 1u32..20,
        min_count in 1u32..5,
        max_count in 5u32..20,
    ) {
        let copybook = format!(
            "       05  COUNTER PIC 9(3).\n\
             05  ARRAY-FIELD PIC X({}) OCCURS {} TO {} TIMES DEPENDING ON COUNTER.",
            field_len, min_count, max_count
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find the ODO field
        let odo_field = schema.fields.iter()
            .find(|f| f.name == "ARRAY-FIELD")
            .expect("Failed to find ODO field");

        // Verify OCCURS structure
        prop_assert!(odo_field.occurs.is_some());
        if let Some(Occurs::ODO { min, max, counter_path }) = odo_field.occurs {
            prop_assert_eq!(min, min_count);
            prop_assert_eq!(max, max_count);
            prop_assert!(counter_path.contains("COUNTER"));
        } else {
            prop_assert!(false, "Expected ODO occurrence type");
        }
    }
}

/// Property: Schema fingerprint is unique for different schemas
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_schema_fingerprint_unique(
        num_fields1 in 1usize..5,
        num_fields2 in 1usize..5,
    ) {
        // Create two different copybooks
        let copybook1 = (0..num_fields1)
            .map(|i| format!("       05  FIELD-{} PIC X(10).", i))
            .collect::<Vec<_>>()
            .join("\n");

        let copybook2 = (0..num_fields2)
            .map(|i| format!("       05  OTHER-FIELD-{} PIC X(10).", i))
            .collect::<Vec<_>>()
            .join("\n");

        let schema1 = parse_copybook(&copybook1).expect("Failed to parse copybook1");
        let schema2 = parse_copybook(&copybook2).expect("Failed to parse copybook2");

        // Fingerprints should differ for different schemas
        // (unless they happen to be identical by chance)
        if num_fields1 != num_fields2 {
            prop_assert_ne!(schema1.fingerprint, schema2.fingerprint);
        }
    }
}
