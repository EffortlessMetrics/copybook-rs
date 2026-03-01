// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Deep edge-case tests for the core parser.
//!
//! These tests target boundary conditions and unusual-but-valid COBOL constructs
//! that are not covered by the existing parser test suites.

use copybook_core::{
    ErrorCode, FieldKind, Occurs, ParseOptions, parse_copybook, parse_copybook_with_options,
};

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

fn find_field<'a>(schema: &'a copybook_core::Schema, name: &str) -> &'a copybook_core::Field {
    schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{name}' not found in schema"))
}

fn enable_sign_separate() {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::SignSeparate);
    FeatureFlags::set_global(flags);
}

fn enable_comp_float() {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::Comp1);
    flags.enable(Feature::Comp2);
    FeatureFlags::set_global(flags);
}

// ===========================================================================
// 1. Empty / whitespace-only copybook
// ===========================================================================

#[test]
fn test_empty_string_returns_error() {
    let err = parse_copybook("").unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_whitespace_only_returns_error() {
    let err = parse_copybook("   \n  \t  \n   ").unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_newlines_only_returns_error() {
    let err = parse_copybook("\n\n\n").unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

// ===========================================================================
// 2. Single-field copybook (minimal valid input)
// ===========================================================================

#[test]
fn test_single_elementary_field() {
    let cpy = "01 RECORD.\n   05 FIELD PIC X.";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 1);
    let root = &schema.fields[0];
    assert_eq!(root.name, "RECORD");
    assert_eq!(root.children.len(), 1);
    let f = &root.children[0];
    assert_eq!(f.name, "FIELD");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 1 }));
    assert_eq!(f.len, 1);
}

#[test]
fn test_single_01_level_no_children() {
    // A bare 01-level with PIC is valid (elementary 01)
    let cpy = "01 SOLO-FIELD PIC X(5).";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "SOLO-FIELD");
    assert!(matches!(
        schema.fields[0].kind,
        FieldKind::Alphanum { len: 5 }
    ));
}

// ===========================================================================
// 3. Deeply nested groups (10 levels)
// ===========================================================================

#[test]
fn test_ten_level_deep_nesting() {
    let cpy = r"
       01 ROOT.
          02 L02.
             03 L03.
                04 L04.
                   05 L05.
                      10 L10.
                         15 L15.
                            20 L20.
                               25 L25.
                                  49 LEAF PIC X(4).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // Traverse the nesting chain
    let root = &schema.fields[0];
    assert_eq!(root.name, "ROOT");
    assert!(matches!(root.kind, FieldKind::Group));

    let l02 = &root.children[0];
    assert_eq!(l02.name, "L02");
    let l03 = &l02.children[0];
    assert_eq!(l03.name, "L03");
    let l04 = &l03.children[0];
    assert_eq!(l04.name, "L04");
    let l05 = &l04.children[0];
    assert_eq!(l05.name, "L05");
    let l10 = &l05.children[0];
    assert_eq!(l10.name, "L10");
    let l15 = &l10.children[0];
    assert_eq!(l15.name, "L15");
    let l20 = &l15.children[0];
    assert_eq!(l20.name, "L20");
    let l25 = &l20.children[0];
    assert_eq!(l25.name, "L25");
    let leaf = &l25.children[0];
    assert_eq!(leaf.name, "LEAF");
    assert!(matches!(leaf.kind, FieldKind::Alphanum { len: 4 }));

    // Leaf should still be locatable via all_fields
    let found = find_field(&schema, "LEAF");
    assert_eq!(found.len, 4);
}

// ===========================================================================
// 4. Maximum field name length (30 characters per COBOL standard)
// ===========================================================================

#[test]
fn test_max_field_name_length_30_chars() {
    // Exactly 30 characters: "ABCDEFGHIJ-KLMNOPQRST-UVWXYZ01"
    let name = "ABCDEFGHIJ-KLMNOPQRST-UVWXYZ01";
    assert_eq!(name.len(), 31); // adjust if needed
    // Try a valid 30-char name
    let name30 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ-ABC";
    assert_eq!(name30.len(), 30);
    let cpy = format!("01 REC.\n   05 {name30} PIC X(10).");
    let schema = parse_copybook(&cpy).unwrap();
    let f = find_field(&schema, name30);
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 10 }));
}

// ===========================================================================
// 5. All PIC clause types
// ===========================================================================

#[test]
fn test_pic_x_alphanumeric() {
    let cpy = "01 REC.\n   05 F PIC X(15).";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 15 }));
}

#[test]
fn test_pic_9_unsigned_numeric() {
    let cpy = "01 REC.\n   05 F PIC 9(7).";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    match &f.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => {
            assert_eq!(*digits, 7);
            assert_eq!(*scale, 0);
            assert!(!signed);
        }
        other => panic!("expected ZonedDecimal, got {other:?}"),
    }
}

#[test]
fn test_pic_s9_signed_numeric() {
    let cpy = "01 REC.\n   05 F PIC S9(5).";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    match &f.kind {
        FieldKind::ZonedDecimal { digits, signed, .. } => {
            assert_eq!(*digits, 5);
            assert!(signed);
        }
        other => panic!("expected ZonedDecimal, got {other:?}"),
    }
}

#[test]
fn test_pic_v_implied_decimal() {
    let cpy = "01 REC.\n   05 F PIC 9(3)V99.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    match &f.kind {
        FieldKind::ZonedDecimal { digits, scale, .. } => {
            assert_eq!(*digits, 5);
            assert_eq!(*scale, 2);
        }
        other => panic!("expected ZonedDecimal, got {other:?}"),
    }
    assert_eq!(f.len, 5, "V does not add storage");
}

#[test]
fn test_pic_z_zero_suppression_edited() {
    let cpy = "01 REC.\n   05 F PIC ZZ9.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(
        matches!(f.kind, FieldKind::EditedNumeric { .. }),
        "expected EditedNumeric, got {:?}",
        f.kind
    );
}

#[test]
fn test_pic_asterisk_check_protect_edited() {
    let cpy = "01 REC.\n   05 F PIC ***9.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(
        matches!(f.kind, FieldKind::EditedNumeric { .. }),
        "expected EditedNumeric, got {:?}",
        f.kind
    );
}

#[test]
fn test_pic_dollar_currency_edited() {
    let cpy = "01 REC.\n   05 F PIC $ZZ9.99.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(
        matches!(f.kind, FieldKind::EditedNumeric { .. }),
        "expected EditedNumeric, got {:?}",
        f.kind
    );
}

#[test]
fn test_pic_plus_sign_edited() {
    let cpy = "01 REC.\n   05 F PIC +ZZ9.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(
        matches!(f.kind, FieldKind::EditedNumeric { .. }),
        "expected EditedNumeric, got {:?}",
        f.kind
    );
}

#[test]
fn test_pic_minus_sign_edited() {
    let cpy = "01 REC.\n   05 F PIC -ZZ9.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(
        matches!(f.kind, FieldKind::EditedNumeric { .. }),
        "expected EditedNumeric, got {:?}",
        f.kind
    );
}

#[test]
fn test_pic_b_insertion_edited() {
    let cpy = "01 REC.\n   05 F PIC 99B99.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(
        matches!(f.kind, FieldKind::EditedNumeric { .. }),
        "expected EditedNumeric, got {:?}",
        f.kind
    );
}

// ===========================================================================
// 6. REDEFINES pointing to non-existent field
// ===========================================================================

#[test]
fn test_redefines_nonexistent_target_errors() {
    let cpy = r"
       01 REC.
          05 ACTUAL-FIELD PIC X(10).
          05 BAD-REDEF REDEFINES GHOST-FIELD PIC X(10).
    ";
    let result = parse_copybook(cpy);
    assert!(
        result.is_err(),
        "REDEFINES of non-existent field should fail"
    );
}

// ===========================================================================
// 7. ODO pointing to non-existent counter
// ===========================================================================

#[test]
fn test_odo_nonexistent_counter_error() {
    let cpy = r"
       01 REC.
          05 ITEMS OCCURS 1 TO 10 TIMES
                   DEPENDING ON MISSING-COUNTER
                   PIC X(5).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKS121_COUNTER_NOT_FOUND);
}

// ===========================================================================
// 8. Level 88 with multiple VALUE clauses
// ===========================================================================

#[test]
fn test_level88_multiple_values_space_separated() {
    let cpy = r"
       01 REC.
          05 CODE PIC X(2).
             88 VALID-CODES VALUE 'AA' 'BB' 'CC' 'DD'.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let vc = find_field(&schema, "VALID-CODES");
    assert_eq!(vc.level, 88);
    match &vc.kind {
        FieldKind::Condition { values } => {
            assert!(
                values.len() >= 4,
                "expected at least 4 values, got {values:?}"
            );
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn test_level88_multiple_values_comma_separated() {
    let cpy = r"
       01 REC.
          05 STATUS PIC X(1).
             88 ACTIVE VALUE 'A', 'B', 'C'.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "ACTIVE");
    match &f.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 3, "expected 3 comma-separated values");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

// ===========================================================================
// 9. Level 88 with THRU range
// ===========================================================================

#[test]
fn test_level88_thru_range() {
    let cpy = r"
       01 REC.
          05 SCORE PIC 9(3).
             88 PASSING VALUE 60 THRU 100.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "PASSING");
    assert_eq!(f.level, 88);
    match &f.kind {
        FieldKind::Condition { values } => {
            assert!(!values.is_empty(), "THRU range should produce values");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn test_level88_through_keyword() {
    let cpy = r"
       01 REC.
          05 GRADE PIC X(1).
             88 LETTER-GRADES VALUE 'A' THROUGH 'F'.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "LETTER-GRADES");
    assert_eq!(f.level, 88);
    match &f.kind {
        FieldKind::Condition { values } => {
            assert!(!values.is_empty());
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn test_level88_mixed_values_and_thru() {
    let cpy = r"
       01 REC.
          05 NUM PIC 9(3).
             88 SPECIAL VALUE 0 100 THRU 199 500 THRU 599 999.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "SPECIAL");
    match &f.kind {
        FieldKind::Condition { values } => {
            assert!(
                values.len() >= 2,
                "mixed values/ranges should produce multiple entries: {values:?}"
            );
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

// ===========================================================================
// 10. COMP, COMP-1, COMP-2, COMP-3 usage clauses
// ===========================================================================

#[test]
fn test_comp_binary_int_sizes() {
    let cpy = r"
       01 REC.
          05 SMALL  PIC 9(4)  COMP.
          05 MED    PIC 9(9)  COMP.
          05 BIG    PIC 9(18) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let small = find_field(&schema, "SMALL");
    assert!(matches!(
        small.kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: false
        }
    ));
    assert_eq!(small.len, 2);

    let med = find_field(&schema, "MED");
    assert!(matches!(
        med.kind,
        FieldKind::BinaryInt {
            bits: 32,
            signed: false
        }
    ));
    assert_eq!(med.len, 4);

    let big = find_field(&schema, "BIG");
    assert!(matches!(
        big.kind,
        FieldKind::BinaryInt {
            bits: 64,
            signed: false
        }
    ));
    assert_eq!(big.len, 8);
}

#[test]
fn test_comp3_packed_decimal() {
    let cpy = r"
       01 REC.
          05 AMT PIC S9(7)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "AMT");
    match &f.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 9);
            assert_eq!(*scale, 2);
            assert!(signed);
        }
        other => panic!("expected PackedDecimal, got {other:?}"),
    }
    // COMP-3 size = ceil((digits+1)/2) = ceil(10/2) = 5
    assert_eq!(f.len, 5);
}

#[test]
fn test_comp1_single_precision_float() {
    enable_comp_float();
    let cpy = "01 REC.\n   05 F COMP-1.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(matches!(f.kind, FieldKind::FloatSingle));
    assert_eq!(f.len, 4);
}

#[test]
fn test_comp2_double_precision_float() {
    enable_comp_float();
    let cpy = "01 REC.\n   05 F COMP-2.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F");
    assert!(matches!(f.kind, FieldKind::FloatDouble));
    assert_eq!(f.len, 8);
}

// ===========================================================================
// 11. SIGN SEPARATE LEADING / TRAILING
// ===========================================================================

#[test]
fn test_sign_separate_leading_adds_byte() {
    enable_sign_separate();
    let cpy = r"
       01 REC.
          05 AMT PIC S9(5) SIGN IS LEADING SEPARATE.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "AMT");
    match &f.kind {
        FieldKind::ZonedDecimal {
            signed,
            sign_separate: Some(info),
            ..
        } => {
            assert!(signed);
            assert_eq!(info.placement, copybook_core::SignPlacement::Leading);
        }
        other => panic!("expected ZonedDecimal with sign separate leading, got {other:?}"),
    }
    assert_eq!(f.len, 6, "5 digits + 1 sign byte = 6");
}

#[test]
fn test_sign_separate_trailing_adds_byte() {
    enable_sign_separate();
    let cpy = r"
       01 REC.
          05 BAL PIC S9(7)V99 SIGN TRAILING SEPARATE.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BAL");
    match &f.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate: Some(info),
        } => {
            assert_eq!(*digits, 9);
            assert_eq!(*scale, 2);
            assert!(signed);
            assert_eq!(info.placement, copybook_core::SignPlacement::Trailing);
        }
        other => panic!("expected ZonedDecimal with sign separate trailing, got {other:?}"),
    }
    // 9 digit positions + 1 sign byte = 10
    assert_eq!(f.len, 10);
}

// ===========================================================================
// 12. BLANK WHEN ZERO clause
// ===========================================================================

#[test]
fn test_blank_when_zero_on_numeric() {
    let cpy = r"
       01 REC.
          05 AMOUNT PIC 9(5) BLANK WHEN ZERO.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "AMOUNT");
    assert!(f.blank_when_zero, "blank_when_zero should be true");
}

#[test]
fn test_blank_when_zero_on_edited() {
    let cpy = r"
       01 REC.
          05 DISP-AMT PIC ZZ9 BLANK WHEN ZERO.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "DISP-AMT");
    assert!(f.blank_when_zero);
}

// ===========================================================================
// 13. Multiple 01-level records in same copybook
// ===========================================================================

#[test]
fn test_multiple_01_records_parsed() {
    let cpy = r"
       01 HEADER.
          05 HDR-TYPE PIC X(4).
       01 DETAIL.
          05 DTL-KEY PIC 9(10).
          05 DTL-DATA PIC X(50).
       01 TRAILER.
          05 TRL-COUNT PIC 9(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 3, "should have 3 top-level records");
    assert_eq!(schema.fields[0].name, "HEADER");
    assert_eq!(schema.fields[1].name, "DETAIL");
    assert_eq!(schema.fields[2].name, "TRAILER");
}

#[test]
fn test_multiple_01_records_independent_offsets() {
    let cpy = r"
       01 REC-A.
          05 A1 PIC X(10).
          05 A2 PIC X(20).
       01 REC-B.
          05 B1 PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    // Each 01-level record has its own offset space
    let a1 = find_field(&schema, "A1");
    let b1 = find_field(&schema, "B1");
    assert_eq!(a1.offset, 0);
    assert_eq!(b1.offset, 0, "B1 in second record should start at 0");
}

// ===========================================================================
// 14. Comments — traditional (col-7 *) and inline (*>)
// ===========================================================================

#[test]
fn test_traditional_comment_lines_skipped() {
    let cpy = "      * This is a comment\n       01 REC.\n          05 F PIC X(5).\n";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(find_field(&schema, "F").len, 5);
}

#[test]
fn test_inline_comments_ignored() {
    let cpy = r"
01 REC. *> record definition
   05 NAME PIC X(20). *> customer name
   05 CODE PIC 9(3). *> status code
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(find_field(&schema, "NAME").len, 20);
    assert_eq!(find_field(&schema, "CODE").len, 3);
}

#[test]
fn test_comment_only_copybook_errors() {
    let cpy = "      * just a comment\n      * and another\n";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_mixed_comments_and_data() {
    let cpy = "      * File header comment\n       01 REC.\n      * Group description\n          05 F1 PIC X(10).\n          05 F2 PIC 9(5). *> inline\n";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 2);
}

// ===========================================================================
// 15. Continuation lines (column-7 dash)
// ===========================================================================

#[test]
fn test_continuation_line_field_name() {
    let cpy = "       01 VERY-LONG-FIELD-NAME-THAT-NEEDS-\n      -    CONTINUATION PIC X(20).\n";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(
        schema.fields[0].name,
        "VERY-LONG-FIELD-NAME-THAT-NEEDS-CONTINUATION"
    );
}

// ===========================================================================
// 16. Sequence numbers in columns 1-6
// ===========================================================================

#[test]
fn test_sequence_numbers_stripped() {
    let cpy = "000100 01 REC.\n000200    05 F1 PIC X(10).\n000300    05 F2 PIC 9(5).\n";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 1);
    let root = &schema.fields[0];
    assert_eq!(root.name, "REC");
    assert_eq!(root.children.len(), 2);
    assert_eq!(root.children[0].name, "F1");
    assert_eq!(root.children[1].name, "F2");
}

#[test]
fn test_sequence_numbers_with_traditional_comments() {
    let cpy = "000100* Comment line\n000200 01 REC.\n000300    05 F PIC X(5).\n";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(find_field(&schema, "F").len, 5);
}

// ===========================================================================
// Additional edge cases
// ===========================================================================

#[test]
fn test_filler_field_with_emit_filler() {
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    let cpy = r"
       01 REC.
          05 F1     PIC X(5).
          05 FILLER PIC X(3).
          05 F2     PIC X(5).
    ";
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    let root = &schema.fields[0];
    assert!(root.children.len() >= 3, "should include FILLER");
}

#[test]
fn test_filler_field_excluded_by_default() {
    let cpy = r"
       01 REC.
          05 F1     PIC X(5).
          05 FILLER PIC X(3).
          05 F2     PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];
    // FILLER should be excluded by default (emit_filler=false)
    let has_filler = root
        .children
        .iter()
        .any(|c| c.name == "FILLER" || c.name.starts_with("_filler_"));
    assert!(!has_filler, "FILLER should be excluded by default");
}

#[test]
fn test_level88_no_storage_consumption() {
    let cpy = r"
       01 REC.
          05 FLAG PIC X(1).
             88 YES-VAL VALUE 'Y'.
             88 NO-VAL  VALUE 'N'.
          05 DATA PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let data = find_field(&schema, "DATA");
    assert_eq!(
        data.offset, 1,
        "DATA should start right after 1-byte FLAG; 88s consume no storage"
    );
}

#[test]
fn test_occurs_fixed_count() {
    let cpy = r"
       01 REC.
          05 ITEMS PIC X(10) OCCURS 5 TIMES.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "ITEMS");
    assert!(matches!(f.occurs, Some(Occurs::Fixed { count: 5 })));
}

#[test]
fn test_usage_keyword_optional() {
    // Both "USAGE COMP" and bare "COMP" should work
    let cpy = r"
       01 REC.
          05 F1 PIC 9(4) USAGE COMP.
          05 F2 PIC 9(4) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f1 = find_field(&schema, "F1");
    let f2 = find_field(&schema, "F2");
    assert!(matches!(f1.kind, FieldKind::BinaryInt { bits: 16, .. }));
    assert!(matches!(f2.kind, FieldKind::BinaryInt { bits: 16, .. }));
}
