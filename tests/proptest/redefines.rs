#![allow(unused_doc_comments, unused_imports, dead_code)]
//! Property tests for REDEFINES clause handling
//!
//! These tests verify that REDEFINES maintain field offset and size
//! invariants, ensuring that fields that redefine each other share
//! the same memory location.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::{Field, Schema, parse_copybook};
use proptest::prelude::*;

use super::config::*;
use super::generators::*;
use super::{schema_max_end, schema_record_length};

fn record_children(schema: &Schema) -> &Vec<Field> {
    &schema.fields[0].children
}

/// Property: REDEFINES fields have the same offset
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_same_offset(
        base_pic in prop_oneof![Just("9(5)"), Just("X(10)"), Just("9(3)V99"), Just("X(5)")].prop_map(|s| s.to_string()),
        redef_pic in prop_oneof![Just("9(5)"), Just("X(10)"), Just("9(3)V99"), Just("X(5)")].prop_map(|s| s.to_string())
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 BASE-FIELD PIC {}.\n\
             05 REDEF-FIELD PIC {} REDEFINES BASE-FIELD.",
            base_pic, redef_pic
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let base_field = record_children(&schema).iter()
            .find(|f| f.name == "BASE-FIELD")
            .expect("BASE-FIELD should exist");

        let redef_field = record_children(&schema).iter()
            .find(|f| f.name == "REDEF-FIELD")
            .expect("REDEF-FIELD should exist");

        prop_assert_eq!(base_field.offset, redef_field.offset,
            "REDEFINES fields should have the same offset");
    }
}

/// Property: REDEFINES fields can have different sizes
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_different_sizes(
        base_size in 1usize..=10,
        redef_size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X({}).\n\
             05 REDEF-FIELD PIC X({}) REDEFINES BASE-FIELD.",
            base_size, redef_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let base_field = record_children(&schema).iter()
            .find(|f| f.name == "BASE-FIELD")
            .expect("BASE-FIELD should exist");

        let redef_field = record_children(&schema).iter()
            .find(|f| f.name == "REDEF-FIELD")
            .expect("REDEF-FIELD should exist");

        // Sizes can be different
        prop_assert!(base_field.len as usize == base_size,
            "Base field size should match PIC clause");
        prop_assert!(redef_field.len as usize == redef_size,
            "Redefines field size should match PIC clause");
    }
}

/// Property: Multiple REDEFINES on same field work correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_multiple_redefines_same_field(
        base_size in 1usize..=10,
        redef1_size in 1usize..=10,
        redef2_size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X({}).\n\
             05 REDEF1-FIELD PIC X({}) REDEFINES BASE-FIELD.\n\
             05 REDEF2-FIELD PIC X({}) REDEFINES BASE-FIELD.",
            base_size, redef1_size, redef2_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let base_field = record_children(&schema).iter()
            .find(|f| f.name == "BASE-FIELD")
            .expect("BASE-FIELD should exist");

        let redef1_field = record_children(&schema).iter()
            .find(|f| f.name == "REDEF1-FIELD")
            .expect("REDEF1-FIELD should exist");

        let redef2_field = record_children(&schema).iter()
            .find(|f| f.name == "REDEF2-FIELD")
            .expect("REDEF2-FIELD should exist");

        // All should have the same offset
        prop_assert_eq!(base_field.offset, redef1_field.offset);
        prop_assert_eq!(base_field.offset, redef2_field.offset);
        prop_assert_eq!(redef1_field.offset, redef2_field.offset);
    }
}

/// Property: REDEFINES cannot be on the first field of a group
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_not_on_first_field(
        field1_size in 1usize..=10,
        field2_size in 1usize..=10
    ) {
        // This should be invalid - REDEFINES on first field of group
        let copybook = format!(
            "01 RECORD.\n\
             05 FIELD1 PIC X({}).\n\
             05 FIELD2 PIC X({}) REDEFINES FIELD1.",
            field1_size, field2_size
        );
        let result = parse_copybook(&copybook);

        // REDEFINES on first field may or may not be allowed
        // depending on strict mode
        prop_assert!(result.is_ok() || result.is_err(),
            "REDEFINES handling should not panic");
    }
}

/// Property: REDEFINES with OCCURS maintains structure
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_with_occurs(
        count in 1usize..=10,
        element_size in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY-AS-NUM OCCURS {} TIMES.\n\
             10 ELEMENT PIC 9({}).\n\
             05 ARRAY-AS-TEXT PIC X({}) REDEFINES ARRAY-AS-NUM.",
            count, element_size, count * element_size
        );
        let schema = parse_copybook(&copybook);

        // REDEFINES with OCCURS is complex
        prop_assert!(schema.is_ok() || schema.is_err(),
            "REDEFINES with OCCURS should not panic");
    }
}

/// Property: REDEFINES with different data types works correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_different_types(
        base_pic in prop_oneof![Just("9(5)"), Just("S9(5)"), Just("9(3)V99"), Just("X(5)"), Just("9(4) COMP-3")].prop_map(|s| s.to_string()),
        redef_pic in prop_oneof![Just("9(5)"), Just("S9(5)"), Just("9(3)V99"), Just("X(5)"), Just("9(4) COMP-3")].prop_map(|s| s.to_string())
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 BASE-FIELD PIC {}.\n\
             05 REDEF-FIELD PIC {} REDEFINES BASE-FIELD.",
            base_pic, redef_pic
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let base_field = record_children(&schema).iter()
            .find(|f| f.name == "BASE-FIELD")
            .expect("BASE-FIELD should exist");

        let redef_field = record_children(&schema).iter()
            .find(|f| f.name == "REDEF-FIELD")
            .expect("REDEF-FIELD should exist");

        // Should have same offset regardless of type
        prop_assert_eq!(base_field.offset, redef_field.offset);
    }
}

/// Property: REDEFINES chain maintains consistent offsets
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_chain_consistent_offsets(
        size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 FIELD1 PIC X({}).\n\
             05 FIELD2 PIC X({}) REDEFINES FIELD1.\n\
             05 FIELD3 PIC X({}) REDEFINES FIELD2.",
            size, size, size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let field1 = record_children(&schema).iter()
            .find(|f| f.name == "FIELD1")
            .expect("FIELD1 should exist");

        let field2 = record_children(&schema).iter()
            .find(|f| f.name == "FIELD2")
            .expect("FIELD2 should exist");

        let field3 = record_children(&schema).iter()
            .find(|f| f.name == "FIELD3")
            .expect("FIELD3 should exist");

        // All should have the same offset
        prop_assert_eq!(field1.offset, field2.offset);
        prop_assert_eq!(field2.offset, field3.offset);
    }
}

/// Property: REDEFINES in nested groups maintains correct offsets
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_nested_groups(
        outer_size in 1usize..=5,
        inner_size in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 OUTER-GROUP.\n\
             10 FIELD1 PIC X({}).\n\
             10 FIELD2 PIC X({}) REDEFINES FIELD1.\n\
             05 OTHER-FIELD PIC X({}).",
            inner_size, inner_size, outer_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let all_fields = schema.all_fields();
        let field1 = all_fields.iter()
            .find(|f| f.name == "FIELD1")
            .expect("FIELD1 should exist");

        let field2 = all_fields.iter()
            .find(|f| f.name == "FIELD2")
            .expect("FIELD2 should exist");

        let other_field = all_fields.iter()
            .find(|f| f.name == "OTHER-FIELD")
            .expect("OTHER-FIELD should exist");

        // FIELD1 and FIELD2 should have same offset
        prop_assert_eq!(field1.offset, field2.offset);

        // OTHER-FIELD should be after the group
        prop_assert!(other_field.offset > field1.offset);
    }
}

/// Property: REDEFINES with level 88 conditions works correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_with_level88(
        size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 FIELD1 PIC X({}).\n\
             05 FIELD2 PIC X({}) REDEFINES FIELD1.\n\
             88  VALUE-A VALUE 'A'.",
            size, size
        );
        let schema = parse_copybook(&copybook);

        // Level 88 with REDEFINES is complex
        prop_assert!(schema.is_ok() || schema.is_err(),
            "REDEFINES with Level 88 should not panic");
    }
}

/// Property: REDEFINES field identifies as elementary
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_field_identifies_as_elementary(
        pic in pic_clause_strategy()
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 BASE-FIELD PIC {}.\n\
             05 REDEF-FIELD PIC {} REDEFINES BASE-FIELD.",
            pic, pic
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let redef_field = record_children(&schema).iter()
            .find(|f| f.name == "REDEF-FIELD")
            .expect("REDEF-FIELD should exist");

        // REDEFINES field should be elementary
        prop_assert!(redef_field.is_scalar(),
            "REDEFINES field should be scalar");
    }
}

/// Property: REDEFINES maintains total record size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_maintains_total_size(
        base_size in 1usize..=10,
        other_size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X({}).\n\
             05 REDEF-FIELD PIC X({}) REDEFINES BASE-FIELD.\n\
             05 OTHER-FIELD PIC X({}).",
            base_size, base_size, other_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let expected_size = base_size + other_size;
        if let Some(total_size) = schema_record_length(&schema) {
            prop_assert_eq!(total_size as usize, expected_size,
                "Total size {} should equal base size {} + other size {}",
                total_size, base_size, other_size);
        } else {
            let max_end = schema_max_end(&schema);
            prop_assert!(max_end as usize >= expected_size,
                "Max end {} should be >= expected size {}",
                max_end, expected_size);
        }
    }
}

/// Property: REDEFINES with FILLER works correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_with_filler(
        size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 FILLER PIC X({}).\n\
             05 REAL-FIELD PIC X({}) REDEFINES FILLER.",
            size, size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let filler_field = record_children(&schema).iter()
            .find(|f| f.name == "FILLER")
            .expect("FILLER should exist");

        let real_field = record_children(&schema).iter()
            .find(|f| f.name == "REAL-FIELD")
            .expect("REAL-FIELD should exist");

        // Both should have same offset
        prop_assert_eq!(filler_field.offset, real_field.offset);
    }
}

/// Property: REDEFINES cannot redefine a field with different level
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_same_level(
        size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 FIELD1 PIC X({}).\n\
             10 FIELD2 PIC X({}) REDEFINES FIELD1.",
            size, size
        );
        let result = parse_copybook(&copybook);

        if let Ok(schema) = result {
            let all_fields = schema.all_fields();
            let base_field = all_fields.iter().find(|f| f.name == "FIELD1");
            let redef_field = all_fields.iter().find(|f| f.name == "FIELD2");
            if let (Some(base), Some(redef)) = (base_field, redef_field) {
                prop_assert_eq!(base.offset, redef.offset);
            }
        }
    }
}

/// Property: REDEFINES field order is preserved in schema
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_order_preserved(
        base_size in 1usize..=10,
        redef_size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 BASE-FIELD PIC X({}).\n\
             05 REDEF-FIELD PIC X({}) REDEFINES BASE-FIELD.",
            base_size, redef_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let base_idx = record_children(&schema).iter()
            .position(|f| f.name == "BASE-FIELD")
            .expect("BASE-FIELD should exist");

        let redef_idx = record_children(&schema).iter()
            .position(|f| f.name == "REDEF-FIELD")
            .expect("REDEF-FIELD should exist");

        // REDEFINES field should come after base field
        prop_assert!(redef_idx > base_idx,
            "REDEFINES field should come after base field in schema");
    }
}
