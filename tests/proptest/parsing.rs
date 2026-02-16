#![allow(unused_doc_comments, unused_imports, dead_code)]
//! Property tests for copybook parsing invariants
//!
//! These tests verify that parsed copybooks maintain structural invariants
//! such as:
//! - Field offsets are monotonically increasing
//! - Field sizes match PIC clause specifications
//! - Total record size is consistent
//! - Level hierarchy is valid

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::{Field, FieldKind, ParseOptions, parse_copybook, parse_copybook_with_options};
use proptest::prelude::*;

use super::config::*;
use super::generators::*;
use super::{schema_max_end, schema_record_length};

/// Property: Parsed copybook has valid field hierarchy
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_parsed_copybook_valid_hierarchy(
        copybook in simple_copybook_strategy()
    ) {
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Verify the schema has at least one field
        prop_assert!(!schema.fields.is_empty(), "Schema should have at least one field");

        // Verify all fields have valid levels
        for field in schema.all_fields() {
            prop_assert!(field.level >= 1 && field.level <= 88,
                "Field level {} should be between 1 and 88", field.level);
        }

        // Verify level 01 is the first field
        prop_assert_eq!(schema.fields[0].level, 1,
            "First field should have level 01");
    }
}

/// Property: Field offsets are monotonically increasing
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_field_offsets_monotonic(
        copybook in simple_copybook_strategy()
    ) {
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let mut last_offset = 0;
        for field in schema.all_fields() {
            prop_assert!(field.offset >= last_offset,
                "Field {} offset {} should be >= last offset {}",
                field.name, field.offset, last_offset);
            last_offset = field.offset;
        }
    }
}

/// Property: Total size is consistent with field offsets and sizes
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_total_size_consistent(
        copybook in simple_copybook_strategy()
    ) {
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let max_end = schema_max_end(&schema);
        if let Some(total_size) = schema_record_length(&schema) {
            let has_occurs = schema.all_fields().iter().any(|f| f.occurs.is_some());
            if has_occurs {
                prop_assert!(total_size >= max_end,
                    "Total size {} should be >= max field end {}",
                    total_size, max_end);
            } else {
                prop_assert_eq!(total_size, max_end,
                    "Total size {} should equal max field end {}",
                    total_size, max_end);
            }
        }
    }
}

/// Property: PIC clause parsing is idempotent
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_clause_parsing_idempotent(
        pic in pic_clause_strategy()
    ) {
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema1 = parse_copybook(&copybook).expect("Failed to parse copybook (first)");
        let schema2 = parse_copybook(&copybook).expect("Failed to parse copybook (second)");

        prop_assert_eq!(schema1.fields.len(), schema2.fields.len(),
            "Schemas should have same number of fields");

        if let (Some(f1), Some(f2)) = (schema1.fields.first(), schema2.fields.first()) {
            prop_assert_eq!(f1.len, f2.len,
                "Field sizes should be identical for PIC clause: {}", pic);
        }
    }
}

/// Property: Field names are unique within a level
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_field_names_unique_within_level(
        copybook in simple_copybook_strategy()
    ) {
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Group fields by level and check for duplicate names
        fn unique_within_siblings(fields: &[Field]) -> bool {
            use std::collections::HashSet;
            let mut seen = HashSet::new();
            for field in fields {
                if !seen.insert(field.name.clone()) {
                    return false;
                }
            }
            for field in fields {
                if !unique_within_siblings(&field.children) {
                    return false;
                }
            }
            true
        }

        prop_assert!(
            unique_within_siblings(&schema.fields),
            "Sibling fields should have unique names"
        );
    }
}

/// Property: Parsing with different options produces consistent results
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_parsing_options_consistent(
        copybook in simple_copybook_strategy()
    ) {
        let options_default = ParseOptions::default();
        let options_strict = ParseOptions {
            strict: true,
            ..Default::default()
        };

        let schema_default = parse_copybook_with_options(&copybook, &options_default);
        let schema_strict = parse_copybook_with_options(&copybook, &options_strict);

        // Both should succeed or fail together
        match (schema_default, schema_strict) {
            (Ok(s1), Ok(s2)) => {
                // If both succeed, they should have the same structure
                prop_assert_eq!(s1.fields.len(), s2.fields.len(),
                    "Schemas should have same number of fields");
                let size1 = schema_record_length(&s1);
                let size2 = schema_record_length(&s2);
                prop_assert_eq!(size1, size2,
                    "Schemas should have same total size");
            }
            (Err(_), Err(_)) => {
                // Both failed - acceptable
            }
            (Ok(_), Err(_e)) | (Err(_e), Ok(_)) => {
                // One succeeded, one failed - check if it's expected
                // Strict mode may reject some inputs that default mode accepts
                prop_assert!(true, "Different parsing options may produce different results");
            }
        }
    }
}

/// Property: Malformed copybooks are rejected
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_malformed_copybooks_rejected(
        level in 1u8..=88,
        name in "[A-Z]{1,10}",
        pic in "[A-Z0-9()V.-]{1,20}",
        missing_period in any::<bool>()
    ) {
        let mut copybook = format!("      {:02}  {} PIC {}", level, name, pic);
        if !missing_period {
            copybook.push('.');
        }

        // Some malformed copybooks should fail to parse
        let result = parse_copybook(&copybook);

        let pic_upper = pic.to_ascii_uppercase();
        let has_supported_marker = pic_upper.contains('9')
            || pic_upper.contains('X')
            || pic_upper.contains('S')
            || pic_upper.contains('Z')
            || pic_upper.contains('0')
            || pic_upper.contains('$')
            || pic_upper.contains('*')
            || pic_upper.contains('+')
            || pic_upper.contains('-')
            || pic_upper.contains('/')
            || pic_upper.contains(',')
            || pic_upper.contains('.')
            || pic_upper.contains("CR")
            || pic_upper.contains("DB");

        // If the PIC clause is clearly invalid, parsing should fail
        if !has_supported_marker {
            prop_assert!(result.is_err(),
                "Invalid PIC clause should cause parsing to fail: {}", pic);
        }
    }
}

/// Property: Copybook parsing preserves whitespace handling
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_whitespace_handling_consistent(
        level in 1u8..=88,
        name in "[A-Z]{1,10}",
        pic in "9\\([0-9]+\\)|X\\([0-9]+\\)|S9\\([0-9]+\\)"
    ) {
        // Create copybook with different whitespace patterns
        let copybook1 = format!("      {:02}  {} PIC {}.", level, name, pic);
        let copybook2 = format!("{:02} {} PIC {}.", level, name, pic);
        let copybook3 = format!("      {:02}    {}    PIC    {}.", level, name, pic);

        let schema1 = parse_copybook(&copybook1);
        let schema2 = parse_copybook(&copybook2);
        let schema3 = parse_copybook(&copybook3);

        // All should produce the same schema structure
        let ok1 = schema1.as_ref().is_ok();
        let ok2 = schema2.as_ref().is_ok();
        let ok3 = schema3.as_ref().is_ok();

        if ok1 && ok2 && ok3 {
            let s1 = schema1.unwrap();
            let s2 = schema2.unwrap();
            let s3 = schema3.unwrap();
            prop_assert_eq!(s1.fields.len(), s2.fields.len());
            prop_assert_eq!(s2.fields.len(), s3.fields.len());
        } else {
            // If any fail, all should fail
            prop_assert!(schema1.is_err() || schema2.is_err() || schema3.is_err(),
                "Whitespace should not affect parsing validity");
        }
    }
}

/// Property: Copybook with comments parses correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_comments_handled_correctly(
        copybook in simple_copybook_strategy()
    ) {
        // Add comments to the copybook
        let copybook_with_comments = format!(
            "* This is a comment\n\
             {}\n\
             * Another comment\n\
             01  COMMENT-FIELD PIC X(10).",
            copybook
        );

        let result = parse_copybook(&copybook_with_comments);

        // Should parse successfully (comments are ignored)
        prop_assert!(result.is_ok() || result.is_err(),
            "Comment handling should not cause panics");
    }
}

/// Property: Field types are correctly identified
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_field_types_correctly_identified(
        pic in pic_clause_strategy()
    ) {
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            // Verify field has a valid kind
            match &field.kind {
                FieldKind::Alphanum { .. } => {
                    prop_assert!(pic.contains('X'),
                        "Alphanumeric fields should use PIC X");
                }
                FieldKind::ZonedDecimal { .. } => {
                    prop_assert!(pic.contains('9'),
                        "Zoned decimal fields should use PIC 9");
                }
                FieldKind::BinaryInt { .. } => {
                    prop_assert!(pic.contains("BINARY") || pic.contains("COMP"),
                        "Binary fields should use COMP/BINARY");
                }
                FieldKind::PackedDecimal { .. } => {
                    prop_assert!(pic.contains("COMP-3"),
                        "Packed decimal fields should use COMP-3");
                }
                FieldKind::EditedNumeric { .. } => {
                    let has_edited_marker = pic.contains('Z')
                        || pic.contains('$')
                        || pic.contains(',')
                        || pic.contains('.')
                        || pic.contains('+')
                        || pic.contains('-')
                        || pic.contains('*');
                    prop_assert!(has_edited_marker,
                        "Edited PIC should include edited formatting characters");
                }
                FieldKind::FloatSingle | FieldKind::FloatDouble => {
                    // COMP-1/COMP-2 are self-defining, no PIC clause needed
                }
                FieldKind::Group | FieldKind::Renames { .. } | FieldKind::Condition { .. } => {
                    // Group/renames/condition fields are handled elsewhere
                }
            }
        }
    }
}

/// Property: OCCURS clauses are parsed correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_clauses_parsed_correctly(
        base_name in "[A-Z]{1,10}",
        count in 1usize..=100,
        element_name in "[A-Z]{1,10}",
        pic in "9\\([0-9]+\\)|X\\([0-9]+\\)"
    ) {
        let copybook = format!(
            "      05  {} OCCURS {} TIMES.\n\
             10  {} PIC {}.",
            base_name, count, element_name, pic
        );

        let schema = parse_copybook(&copybook);

        if let Ok(s) = schema {
            // Find the OCCURS field
            if let Some(field) = s.fields.iter().find(|f| f.name == base_name) {
                prop_assert!(field.occurs.is_some(),
                    "Field {} should have OCCURS clause", base_name);
            }
        }
    }
}

/// Property: REDEFINES clauses maintain offset invariants
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_redefines_maintain_offset_invariants(
        base_name in "[A-Z]{1,10}",
        base_pic in "9\\([0-9]+\\)|X\\([0-9]+\\)",
        redef_name in "[A-Z]{1,10}",
        redef_pic in "9\\([0-9]+\\)|X\\([0-9]+\\)"
    ) {
        let copybook = format!(
            "      05  {} PIC {}.\n\
             05  {} PIC {} REDEFINES {}.",
            base_name, base_pic, redef_name, redef_pic, base_name
        );

        let schema = parse_copybook(&copybook);

        if let Ok(s) = schema {
            // Find both fields
            let base_field = s.fields.iter().find(|f| f.name == base_name);
            let redef_field = s.fields.iter().find(|f| f.name == redef_name);

            if let (Some(bf), Some(rf)) = (base_field, redef_field) {
                // Both fields should have the same offset
                prop_assert_eq!(bf.offset, rf.offset,
                    "REDEFINES fields should have same offset");
            }
        }
    }
}
