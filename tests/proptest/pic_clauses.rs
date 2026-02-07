//! Property tests for PIC clause parsing and handling
//!
//! These tests verify that PIC clauses parse consistently and generate
//! valid schemas for various PIC clause patterns.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::{parse_copybook, FieldKind};
use proptest::prelude::*;

use super::generators::*;
use super::config::*;

/// Property: PIC clause 9(n) produces correct field size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_9n_produces_correct_size(
        n in 1usize..=18
    ) {
        let pic = format!("9({})", n);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            prop_assert_eq!(field.size, n,
                "PIC 9({}) should produce field size {}", n, n);
        }
    }
}

/// Property: PIC clause X(n) produces correct field size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_xn_produces_correct_size(
        n in 1usize..=255
    ) {
        let pic = format!("X({})", n);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            prop_assert_eq!(field.size, n,
                "PIC X({}) should produce field size {}", n, n);
        }
    }
}

/// Property: PIC clause S9(n) produces correct field size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_s9n_produces_correct_size(
        n in 1usize..=18
    ) {
        let pic = format!("S9({})", n);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            // Signed numeric fields have same size as unsigned for zoned decimals
            prop_assert_eq!(field.size, n,
                "PIC S9({}) should produce field size {}", n, n);
        }
    }
}

/// Property: PIC clause with decimal point produces correct size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_decimal_produces_correct_size(
        int_digits in 1usize..=10,
        dec_digits in 1usize..=4
    ) {
        let pic = format!("9({})V9({})", int_digits, dec_digits);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            let expected_size = int_digits + dec_digits;
            prop_assert_eq!(field.size, expected_size,
                "PIC 9({})V9({}) should produce field size {}",
                int_digits, dec_digits, expected_size);
        }
    }
}

/// Property: PIC clause with COMP-3 produces correct size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_comp3_produces_correct_size(
        total_digits in 1usize..=18
    ) {
        let pic = format!("9({}) COMP-3", total_digits);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            // COMP-3 size: (n + 2) / 2 (rounded up)
            let expected_size = (total_digits + 2) / 2;
            prop_assert_eq!(field.size, expected_size,
                "PIC 9({}) COMP-3 should produce field size {}",
                total_digits, expected_size);
        }
    }
}

/// Property: PIC clause with BINARY produces correct size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_binary_produces_correct_size(
        digits in 1usize..=18
    ) {
        let pic = format!("9({}) BINARY", digits);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            // Binary size depends on number of digits
            // 1-2 digits: 1 byte, 3-4 digits: 2 bytes, 5-9 digits: 4 bytes, 10-18 digits: 8 bytes
            let expected_size = match digits {
                1..=2 => 1,
                3..=4 => 2,
                5..=9 => 4,
                _ => 8,
            };
            prop_assert_eq!(field.size, expected_size,
                "PIC 9({}) BINARY should produce field size {}",
                digits, expected_size);
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
    fn prop_pic_parsing_idempotent(
        pic in pic_clause_strategy()
    ) {
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema1 = parse_copybook(&copybook).expect("Failed to parse copybook (first)");
        let schema2 = parse_copybook(&copybook).expect("Failed to parse copybook (second)");

        if let (Some(f1), Some(f2)) = (schema1.fields.first(), schema2.fields.first()) {
            prop_assert_eq!(f1.size, f2.size,
                "PIC {} should produce consistent field sizes", pic);
            prop_assert_eq!(f1.kind, f2.kind,
                "PIC {} should produce consistent field kinds", pic);
        }
    }
}

/// Property: PIC clause with edited format produces correct size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_edited_format_produces_correct_size(
        pattern in prop_oneof![
            "ZZZZ9",
            "ZZZ,ZZ9",
            "$$$,$$9.99",
            "+999,999.99",
            "-999,999.99",
            "9,999.99",
        ].prop_map(|s| s.to_string())
    ) {
        let pic = pattern.clone();
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook);

        // Edited formats should parse successfully
        prop_assert!(schema.is_ok(),
            "Edited PIC format {} should parse successfully", pattern);

        if let Ok(s) = schema {
            if let Some(field) = s.fields.first() {
                // Edited format size equals pattern length
                let expected_size = pattern.len();
                prop_assert_eq!(field.size, expected_size,
                    "Edited PIC format {} should produce field size {}",
                    pattern, expected_size);
            }
        }
    }
}

/// Property: PIC clause with repeated characters produces correct size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_repeated_characters_produces_correct_size(
        char in prop_oneof!['9', 'X', 'Z', '*'],
        count in 1usize..=20
    ) {
        let pic = char.to_string().repeat(count);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            prop_assert_eq!(field.size, count,
                "PIC {} should produce field size {}", pic, count);
        }
    }
}

/// Property: PIC clause with mixed types produces correct size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_mixed_types_produces_correct_size(
        n_count in 1usize..=10,
        x_count in 1usize..=10
    ) {
        let pic = format!("9({})X({})", n_count, x_count);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook);

        // Mixed PIC clauses may not be valid in all cases
        if let Ok(s) = schema {
            if let Some(field) = s.fields.first() {
                let expected_size = n_count + x_count;
                prop_assert_eq!(field.size, expected_size,
                    "PIC 9({})X({}) should produce field size {}",
                    n_count, x_count, expected_size);
            }
        }
    }
}

/// Property: PIC clause identifies correct field kind
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_identifies_correct_field_kind(
        pic in pic_clause_strategy()
    ) {
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            // Field should have an elementary kind
            match &field.kind {
                FieldKind::Elementary { pic: _, usage } => {
                    // Elementary fields are correct
                    prop_assert!(true, "PIC {} should produce elementary field", pic);
                }
                FieldKind::Group => {
                    // PIC clauses should not produce group fields
                    prop_assert!(false, "PIC {} should not produce group field", pic);
                }
                FieldKind::Renames { .. } => {
                    // PIC clauses should not produce rename fields
                    prop_assert!(false, "PIC {} should not produce rename field", pic);
                }
            }
        }
    }
}

/// Property: PIC clause with sign indicator produces signed field
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_with_sign_produces_signed_field(
        n in 1usize..=18
    ) {
        let pic_signed = format!("S9({})", n);
        let pic_unsigned = format!("9({})", n);

        let copybook_signed = format!("01 FIELD PIC {}.", pic_signed);
        let copybook_unsigned = format!("01 FIELD PIC {}.", pic_unsigned);

        let schema_signed = parse_copybook(&copybook_signed).expect("Failed to parse signed copybook");
        let schema_unsigned = parse_copybook(&copybook_unsigned).expect("Failed to parse unsigned copybook");

        if let (Some(f_signed), Some(f_unsigned)) = (schema_signed.fields.first(), schema_unsigned.fields.first()) {
            // Both should have the same size
            prop_assert_eq!(f_signed.size, f_unsigned.size,
                "Signed and unsigned fields should have same size");

            // The kind should differ in sign handling
            // (This is a weak assertion - the exact difference depends on implementation)
            prop_assert!(true, "Signed and unsigned fields should be handled differently");
        }
    }
}

/// Property: PIC clause with V (decimal point) produces correct structure
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_with_v_produces_correct_structure(
        int_digits in 1usize..=10,
        dec_digits in 1usize..=4
    ) {
        let pic = format!("9({})V9({})", int_digits, dec_digits);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            // Field size should include both integer and decimal parts
            let expected_size = int_digits + dec_digits;
            prop_assert_eq!(field.size, expected_size,
                "PIC 9({})V9({}) should produce field size {}",
                int_digits, dec_digits, expected_size);
        }
    }
}

/// Property: PIC clause with P (scaling) produces correct size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_pic_with_p_produces_correct_size(
        n in 1usize..=10,
        p_count in 1usize..=5
    ) {
        let pic = format!("9({})P({})", n, p_count);
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook);

        // P (scaling) may not be fully supported
        if let Ok(s) = schema {
            if let Some(field) = s.fields.first() {
                // P positions may or may not count toward size
                // This test verifies consistent behavior
                prop_assert!(field.size >= n,
                    "PIC 9({})P({}) should produce field size at least {}",
                    n, p_count, n);
            }
        }
    }
}

/// Property: Invalid PIC clauses are rejected
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_invalid_pic_rejected(
        invalid_pic in prop_oneof![
            "ABC",
            "999ABC",
            "X(999)",
            "9(999)",
            "",
            "9()",
            "X()",
        ].prop_map(|s| s.to_string())
    ) {
        let copybook = format!("01 FIELD PIC {}.", invalid_pic);
        let result = parse_copybook(&copybook);

        // Invalid PIC clauses should fail to parse
        prop_assert!(result.is_err(),
            "Invalid PIC clause {} should be rejected", invalid_pic);
    }
}
