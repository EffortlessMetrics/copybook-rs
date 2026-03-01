// SPDX-License-Identifier: AGPL-3.0-or-later
//! Extended parser tests for copybook-core.
//!
//! Covers additional ODO configurations, REDEFINES variations, Level-88 edge
//! cases, RENAMES (66), SIGN SEPARATE, VALUE clauses, BLANK WHEN ZERO, nested
//! groups at various levels, PIC patterns, COMP variants, inline comments,
//! and error cases that extend the coverage in `parser_comprehensive.rs`.

#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

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
        .unwrap_or_else(|| panic!("field '{}' not found in schema", name))
}

// ===========================================================================
// ODO — additional configurations
// ===========================================================================

#[test]
fn test_odo_min_zero() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(3).
          05 ITEMS     OCCURS 0 TO 50 TIMES
                       DEPENDING ON CNT
                       PIC X(4).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let items = find_field(&schema, "ITEMS");
    match &items.occurs {
        Some(Occurs::ODO { min, max, .. }) => {
            assert_eq!(*min, 0);
            assert_eq!(*max, 50);
        }
        other => panic!("expected ODO, got {:?}", other),
    }
}

#[test]
fn test_odo_group_with_children() {
    let cpy = r"
       01 REC.
          05 NUM-LINES   PIC 9(2).
          05 LINE-ITEM   OCCURS 1 TO 20 TIMES
                         DEPENDING ON NUM-LINES.
             10 ITEM-ID  PIC 9(6).
             10 ITEM-DESC PIC X(30).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let li = find_field(&schema, "LINE-ITEM");
    match &li.occurs {
        Some(Occurs::ODO { min, max, counter_path }) => {
            assert_eq!(*min, 1);
            assert_eq!(*max, 20);
            assert_eq!(counter_path, "NUM-LINES");
        }
        other => panic!("expected ODO, got {:?}", other),
    }
    assert_eq!(li.children.len(), 2);
    assert_eq!(li.children[0].name, "ITEM-ID");
    assert_eq!(li.children[1].name, "ITEM-DESC");
}

#[test]
fn test_odo_counter_in_sibling_group() {
    let cpy = r"
       01 REC.
          05 HEADER.
             10 HDR-CNT PIC 9(3).
          05 ENTRIES    OCCURS 1 TO 10 TIMES
                        DEPENDING ON HDR-CNT
                        PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let entries = find_field(&schema, "ENTRIES");
    match &entries.occurs {
        Some(Occurs::ODO { counter_path, .. }) => {
            assert_eq!(counter_path, "HDR-CNT");
        }
        other => panic!("expected ODO, got {:?}", other),
    }
}

#[test]
fn test_odo_large_bounds() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(5).
          05 DATA      OCCURS 1 TO 99999 TIMES
                       DEPENDING ON CNT
                       PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let data = find_field(&schema, "DATA");
    match &data.occurs {
        Some(Occurs::ODO { min, max, .. }) => {
            assert_eq!(*min, 1);
            assert_eq!(*max, 99999);
        }
        other => panic!("expected ODO, got {:?}", other),
    }
}

#[test]
fn test_odo_tail_odo_metadata() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(2).
          05 ITEMS     OCCURS 1 TO 50 TIMES
                       DEPENDING ON CNT
                       PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let tail = schema.tail_odo.as_ref().expect("tail_odo should be set");
    assert_eq!(tail.counter_path, "CNT");
    assert_eq!(tail.min_count, 1);
    assert_eq!(tail.max_count, 50);
}

// ===========================================================================
// REDEFINES — additional scenarios
// ===========================================================================

#[test]
fn test_redefines_group_to_elementary() {
    let cpy = r"
       01 REC.
          05 DATE-GROUP.
             10 DATE-YYYY PIC 9(4).
             10 DATE-MM   PIC 9(2).
             10 DATE-DD   PIC 9(2).
          05 DATE-STRING REDEFINES DATE-GROUP PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let ds = find_field(&schema, "DATE-STRING");
    assert_eq!(ds.redefines_of.as_deref(), Some("DATE-GROUP"));
    assert!(matches!(ds.kind, FieldKind::Alphanum { len: 8 }));
}

#[test]
fn test_redefines_preserves_original_offset() {
    let cpy = r"
       01 REC.
          05 PREFIX     PIC X(4).
          05 ORIG       PIC X(10).
          05 ALT        REDEFINES ORIG PIC 9(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let orig = find_field(&schema, "ORIG");
    let alt = find_field(&schema, "ALT");
    assert_eq!(orig.offset, alt.offset, "REDEFINES should share offset");
    assert_eq!(orig.len, alt.len, "REDEFINES should share length");
}

#[test]
fn test_redefines_group_with_different_children() {
    let cpy = r"
       01 REC.
          05 VARIANT-A.
             10 FA-1 PIC X(5).
             10 FA-2 PIC X(5).
          05 VARIANT-B REDEFINES VARIANT-A.
             10 FB-1 PIC 9(3).
             10 FB-2 PIC 9(7).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let vb = find_field(&schema, "VARIANT-B");
    assert_eq!(vb.redefines_of.as_deref(), Some("VARIANT-A"));
    assert!(matches!(vb.kind, FieldKind::Group));
    assert_eq!(vb.children.len(), 2);
    assert_eq!(vb.children[0].name, "FB-1");
    assert_eq!(vb.children[1].name, "FB-2");
}

// ===========================================================================
// Level-88 — additional scenarios
// ===========================================================================

#[test]
fn test_level88_on_numeric_field() {
    let cpy = r"
       01 REC.
          05 AMOUNT    PIC 9(5).
             88 ZERO-AMT VALUE 0.
             88 MAX-AMT  VALUE 99999.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let zero = find_field(&schema, "ZERO-AMT");
    assert_eq!(zero.level, 88);
    match &zero.kind {
        FieldKind::Condition { values } => assert!(!values.is_empty()),
        other => panic!("expected Condition, got {:?}", other),
    }
    let max_a = find_field(&schema, "MAX-AMT");
    assert_eq!(max_a.level, 88);
}

#[test]
fn test_level88_comma_separated_values() {
    let cpy = r"
       01 REC.
          05 STATUS    PIC X(1).
             88 VALID-STATUS VALUE 'A', 'B', 'C'.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let vs = find_field(&schema, "VALID-STATUS");
    match &vs.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 3, "should have 3 comma-separated values");
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_level88_mixed_values_and_ranges() {
    let cpy = r"
       01 REC.
          05 CODE       PIC 9(3).
             88 SPECIAL  VALUE 0 100 THRU 199 999.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let special = find_field(&schema, "SPECIAL");
    match &special.kind {
        FieldKind::Condition { values } => {
            assert!(
                values.len() >= 2,
                "should have multiple values/ranges, got {:?}",
                values
            );
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_level88_children_are_non_storage() {
    let cpy = r"
       01 REC.
          05 FLAG       PIC X(1).
             88 YES-FLAG VALUE 'Y'.
             88 NO-FLAG  VALUE 'N'.
          05 DATA       PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let flag = find_field(&schema, "FLAG");
    // Level-88 conditions should not consume storage
    assert_eq!(flag.len, 1, "FLAG should be 1 byte");
    let data = find_field(&schema, "DATA");
    assert_eq!(data.offset, 1, "DATA should start at offset 1 (after FLAG)");
}

// ===========================================================================
// RENAMES (level 66) — additional scenarios
// ===========================================================================

#[test]
fn test_renames_through_keyword() {
    let cpy = r"
       01 REC.
          05 FIRST-NAME  PIC X(15).
          05 LAST-NAME   PIC X(20).
       66 FULL-NAME     RENAMES FIRST-NAME THROUGH LAST-NAME.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let alias = find_field(&schema, "FULL-NAME");
    assert_eq!(alias.level, 66);
    match &alias.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FIRST-NAME");
            assert_eq!(thru_field, "LAST-NAME");
        }
        other => panic!("expected Renames, got {:?}", other),
    }
}

// ===========================================================================
// SIGN SEPARATE LEADING / TRAILING — requires feature flag
// ===========================================================================

fn enable_sign_separate() {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::SignSeparate);
    FeatureFlags::set_global(flags);
}

#[test]
fn test_sign_separate_leading() {
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
        other => panic!("expected ZonedDecimal with sign separate, got {:?}", other),
    }
    // SIGN SEPARATE adds one byte for the sign character
    assert_eq!(f.len, 6, "5 digits + 1 sign byte = 6");
}

#[test]
fn test_sign_separate_trailing() {
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
        other => panic!("expected ZonedDecimal with sign separate, got {:?}", other),
    }
}

// ===========================================================================
// VALUE clauses on non-88 fields (skipped but should not error)
// ===========================================================================

#[test]
fn test_value_clause_on_alphanumeric_no_error() {
    let cpy = r"
       01 REC.
          05 COUNTRY PIC X(3) VALUE 'USA'.
          05 NAME    PIC X(20).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let country = find_field(&schema, "COUNTRY");
    assert!(matches!(country.kind, FieldKind::Alphanum { len: 3 }));
}

#[test]
fn test_value_clause_on_numeric_no_error() {
    let cpy = r"
       01 REC.
          05 COUNTER PIC 9(5) VALUE 0.
          05 TOTAL   PIC 9(7)V99 VALUE 0.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let counter = find_field(&schema, "COUNTER");
    match &counter.kind {
        FieldKind::ZonedDecimal { digits: 5, .. } => {}
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

#[test]
fn test_value_clause_spaces_literal() {
    let cpy = r"
       01 REC.
          05 BUFFER PIC X(80) VALUE SPACES.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let buf = find_field(&schema, "BUFFER");
    assert!(matches!(buf.kind, FieldKind::Alphanum { len: 80 }));
}

#[test]
fn test_value_clause_zeros_literal() {
    let cpy = r"
       01 REC.
          05 ACCUM PIC 9(10) VALUE ZEROS.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let accum = find_field(&schema, "ACCUM");
    match &accum.kind {
        FieldKind::ZonedDecimal { digits: 10, .. } => {}
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

// ===========================================================================
// BLANK WHEN ZERO — additional
// ===========================================================================

#[test]
fn test_blank_when_zero_on_edited_numeric() {
    let cpy = r"
       01 REC.
          05 DISPLAY-AMT PIC ZZ9 BLANK WHEN ZERO.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "DISPLAY-AMT");
    assert!(f.blank_when_zero);
}

// ===========================================================================
// Nested groups at various levels
// ===========================================================================

#[test]
fn test_mixed_level_numbers() {
    let cpy = r"
       01 TRANSACTION.
          02 TXN-HEADER.
             03 TXN-TYPE   PIC X(2).
             03 TXN-DATE   PIC 9(8).
          02 TXN-BODY.
             05 AMOUNT     PIC 9(9)V99.
             05 DETAIL.
                10 DETAIL-CODE PIC X(4).
                10 DETAIL-TEXT PIC X(40).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let txn = &schema.fields[0];
    assert_eq!(txn.name, "TRANSACTION");
    assert_eq!(txn.children.len(), 2);

    let header = &txn.children[0];
    assert_eq!(header.name, "TXN-HEADER");
    assert_eq!(header.children.len(), 2);

    let body = &txn.children[1];
    assert_eq!(body.name, "TXN-BODY");
    assert_eq!(body.children.len(), 2);

    let detail = find_field(&schema, "DETAIL");
    assert!(matches!(detail.kind, FieldKind::Group));
    assert_eq!(detail.children.len(), 2);
}

#[test]
fn test_group_total_length_equals_children() {
    let cpy = r"
       01 REC.
          05 GRP.
             10 A PIC X(5).
             10 B PIC X(10).
             10 C PIC 9(3).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let grp = find_field(&schema, "GRP");
    assert_eq!(grp.len, 18, "group length should be 5+10+3=18");
}

#[test]
fn test_deeply_nested_offsets_correct() {
    let cpy = r"
       01 REC.
          05 A PIC X(2).
          05 GRP.
             10 B PIC X(3).
             10 C PIC X(4).
          05 D PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let a = find_field(&schema, "A");
    let b = find_field(&schema, "B");
    let c = find_field(&schema, "C");
    let d = find_field(&schema, "D");
    assert_eq!(a.offset, 0);
    assert_eq!(b.offset, 2);
    assert_eq!(c.offset, 5);
    assert_eq!(d.offset, 9);
}

#[test]
fn test_four_level_hierarchy() {
    let cpy = r"
       01 ROOT.
          05 LEVEL-5.
             10 LEVEL-10.
                15 LEAF PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.name, "ROOT");
    assert_eq!(root.level, 1);
    let l5 = &root.children[0];
    assert_eq!(l5.name, "LEVEL-5");
    let l10 = &l5.children[0];
    assert_eq!(l10.name, "LEVEL-10");
    let leaf = &l10.children[0];
    assert_eq!(leaf.name, "LEAF");
    assert!(matches!(leaf.kind, FieldKind::Alphanum { len: 8 }));
}

// ===========================================================================
// PIC clause patterns — additional
// ===========================================================================

#[test]
fn test_pic_repeated_nines_with_parens() {
    let cpy = r"
       01 REC.
          05 F1 PIC 9(3).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    match &f.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => {
            assert_eq!(*digits, 3);
            assert_eq!(*scale, 0);
            assert!(!signed);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

#[test]
fn test_pic_repeated_x_no_parens() {
    let cpy = r"
       01 REC.
          05 F1 PIC XXXX.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 4 }));
}

#[test]
fn test_pic_s9v99_implied_decimal() {
    let cpy = r"
       01 REC.
          05 RATE PIC S9V99.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "RATE");
    match &f.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => {
            assert_eq!(*digits, 3);
            assert_eq!(*scale, 2);
            assert!(signed);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
    assert_eq!(f.len, 3);
}

#[test]
fn test_pic_large_alphanumeric() {
    let cpy = r"
       01 REC.
          05 BIG-FIELD PIC X(1000).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BIG-FIELD");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 1000 }));
    assert_eq!(f.len, 1000);
}

#[test]
fn test_pic_9v9_combinations() {
    let cpy = r"
       01 REC.
          05 F1 PIC 9(5)V9(3).
          05 F2 PIC S9(3)V9(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f1 = find_field(&schema, "F1");
    match &f1.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => {
            assert_eq!(*digits, 8);
            assert_eq!(*scale, 3);
            assert!(!signed);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
    let f2 = find_field(&schema, "F2");
    match &f2.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => {
            assert_eq!(*digits, 8);
            assert_eq!(*scale, 5);
            assert!(signed);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

#[test]
fn test_pic_single_9_with_parens() {
    let cpy = r"
       01 REC.
          05 DIGIT PIC 9(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "DIGIT");
    match &f.kind {
        FieldKind::ZonedDecimal {
            digits, signed, ..
        } => {
            assert_eq!(*digits, 1);
            assert!(!signed);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
    assert_eq!(f.len, 1);
}

// ===========================================================================
// COMP / BINARY — additional variants
// ===========================================================================

#[test]
fn test_comp_signed_variants() {
    let cpy = r"
       01 REC.
          05 SMALL-S PIC S9(4)  COMP.
          05 MED-S   PIC S9(9)  COMP.
          05 BIG-S   PIC S9(18) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let s = find_field(&schema, "SMALL-S");
    assert!(matches!(
        s.kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: true
        }
    ));
    let m = find_field(&schema, "MED-S");
    assert!(matches!(
        m.kind,
        FieldKind::BinaryInt {
            bits: 32,
            signed: true
        }
    ));
    let b = find_field(&schema, "BIG-S");
    assert!(matches!(
        b.kind,
        FieldKind::BinaryInt {
            bits: 64,
            signed: true
        }
    ));
}

#[test]
fn test_comp3_unsigned() {
    let cpy = r"
       01 REC.
          05 AMT PIC 9(5) COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "AMT");
    match &f.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 5);
            assert_eq!(*scale, 0);
            assert!(!signed);
        }
        other => panic!("expected PackedDecimal, got {:?}", other),
    }
}

#[test]
fn test_comp3_with_decimal() {
    let cpy = r"
       01 REC.
          05 PRICE PIC 9(5)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "PRICE");
    match &f.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 7);
            assert_eq!(*scale, 2);
            assert!(!signed);
        }
        other => panic!("expected PackedDecimal, got {:?}", other),
    }
}

#[test]
fn test_usage_comp_keyword() {
    let cpy = r"
       01 REC.
          05 F1 PIC 9(4) USAGE COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: false
        }
    ));
}

// ===========================================================================
// COMP-1 / COMP-2 — feature-gated floating point
// ===========================================================================

fn enable_comp_float() {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    let mut flags = FeatureFlags::default();
    flags.enable(Feature::Comp1);
    flags.enable(Feature::Comp2);
    FeatureFlags::set_global(flags);
}

#[test]
fn test_comp1_single_precision() {
    enable_comp_float();
    let cpy = r"
       01 REC.
          05 TEMP COMP-1.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "TEMP");
    assert!(matches!(f.kind, FieldKind::FloatSingle));
    assert_eq!(f.len, 4, "COMP-1 is 4 bytes");
}

#[test]
fn test_comp2_double_precision() {
    enable_comp_float();
    let cpy = r"
       01 REC.
          05 RATIO COMP-2.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "RATIO");
    assert!(matches!(f.kind, FieldKind::FloatDouble));
    assert_eq!(f.len, 8, "COMP-2 is 8 bytes");
}

// ===========================================================================
// Inline comments (COBOL-2002 *>)
// ===========================================================================

#[test]
fn test_inline_comment_preserves_field_data() {
    let cpy = r"
       01 REC.
          05 ID     PIC 9(6). *> customer identifier
          05 NAME   PIC X(30). *> full name
          05 BAL    PIC S9(7)V99. *> current balance
    ";
    let schema = parse_copybook(cpy).unwrap();
    let id = find_field(&schema, "ID");
    match &id.kind {
        FieldKind::ZonedDecimal { digits: 6, .. } => {}
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
    let name = find_field(&schema, "NAME");
    assert!(matches!(name.kind, FieldKind::Alphanum { len: 30 }));
    let bal = find_field(&schema, "BAL");
    match &bal.kind {
        FieldKind::ZonedDecimal {
            digits: 9,
            scale: 2,
            signed: true,
            ..
        } => {}
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

#[test]
fn test_traditional_comment_lines_ignored() {
    let cpy =
        "      * This is a comment line\n       01 REC.\n          05 F1 PIC X(5).\n";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 5 }));
}

// ===========================================================================
// Error cases
// ===========================================================================

#[test]
fn test_error_odo_counter_not_found_descriptive() {
    let cpy = r"
       01 REC.
          05 ITEMS OCCURS 1 TO 5 TIMES
                   DEPENDING ON GHOST-COUNTER
                   PIC X(3).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKS121_COUNTER_NOT_FOUND);
}

#[test]
fn test_error_only_comments_is_empty() {
    let cpy = "      * comment only\n      * another comment\n";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_error_whitespace_only_is_empty() {
    let cpy = "          \n      \n   ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_error_odo_not_tail_with_trailer() {
    let cpy = r"
       01 REC.
          05 CNT      PIC 9(2).
          05 VARY     OCCURS 1 TO 10 TIMES
                      DEPENDING ON CNT
                      PIC X(5).
          05 TRAILER  PIC X(4).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP021_ODO_NOT_TAIL);
}

// ===========================================================================
// Complex mixed scenarios
// ===========================================================================

#[test]
fn test_enterprise_customer_record() {
    let cpy = r"
       01 CUSTOMER-RECORD.
          05 CUST-ID           PIC 9(10).
          05 CUST-NAME.
             10 FIRST-NAME     PIC X(20).
             10 LAST-NAME      PIC X(30).
          05 CUST-TYPE         PIC X(1).
             88 INDIVIDUAL     VALUE 'I'.
             88 CORPORATE      VALUE 'C'.
          05 ACCT-COUNT        PIC 9(2).
          05 ACCOUNTS OCCURS 1 TO 10 TIMES
                      DEPENDING ON ACCT-COUNT.
             10 ACCT-NUMBER    PIC 9(12).
             10 ACCT-BALANCE   PIC S9(9)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let rec = &schema.fields[0];
    assert_eq!(rec.name, "CUSTOMER-RECORD");

    let name_grp = find_field(&schema, "CUST-NAME");
    assert!(matches!(name_grp.kind, FieldKind::Group));
    assert_eq!(name_grp.children.len(), 2);

    let ind = find_field(&schema, "INDIVIDUAL");
    assert_eq!(ind.level, 88);
    let corp = find_field(&schema, "CORPORATE");
    assert_eq!(corp.level, 88);

    let accts = find_field(&schema, "ACCOUNTS");
    match &accts.occurs {
        Some(Occurs::ODO { min, max, counter_path }) => {
            assert_eq!(*min, 1);
            assert_eq!(*max, 10);
            assert_eq!(counter_path, "ACCT-COUNT");
        }
        other => panic!("expected ODO, got {:?}", other),
    }

    let bal = find_field(&schema, "ACCT-BALANCE");
    assert!(matches!(
        bal.kind,
        FieldKind::PackedDecimal { signed: true, .. }
    ));
}

#[test]
fn test_redefines_with_level88() {
    let cpy = r"
       01 REC.
          05 RAW-CODE    PIC X(2).
             88 CODE-OK  VALUE 'OK'.
             88 CODE-ERR VALUE 'ER'.
          05 NUM-CODE    REDEFINES RAW-CODE PIC 9(2).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let raw = find_field(&schema, "RAW-CODE");
    let conditions: Vec<_> = raw.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(conditions.len(), 2);

    let num = find_field(&schema, "NUM-CODE");
    assert_eq!(num.redefines_of.as_deref(), Some("RAW-CODE"));
}

#[test]
fn test_multiple_01_records_independent() {
    let cpy = r"
       01 HEADER-REC.
          05 HDR-TYPE PIC X(4).
          05 HDR-LEN  PIC 9(4).
       01 DETAIL-REC.
          05 DTL-KEY  PIC 9(10).
          05 DTL-DATA PIC X(100).
       01 TRAILER-REC.
          05 TRL-TYPE PIC X(4).
          05 TRL-COUNT PIC 9(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 3);
    assert_eq!(schema.fields[0].name, "HEADER-REC");
    assert_eq!(schema.fields[1].name, "DETAIL-REC");
    assert_eq!(schema.fields[2].name, "TRAILER-REC");
}

#[test]
fn test_occurs_fixed_with_redefines_inside() {
    let cpy = r"
       01 REC.
          05 ENTRIES OCCURS 5 TIMES.
             10 RAW-VAL  PIC X(4).
             10 NUM-VAL  REDEFINES RAW-VAL PIC 9(4).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let entries = find_field(&schema, "ENTRIES");
    assert!(matches!(entries.occurs, Some(Occurs::Fixed { count: 5 })));
    let num = find_field(&schema, "NUM-VAL");
    assert_eq!(num.redefines_of.as_deref(), Some("RAW-VAL"));
}

#[test]
fn test_lrecl_fixed_calculated() {
    let cpy = r"
       01 REC.
          05 A PIC X(10).
          05 B PIC 9(5).
          05 C PIC X(20).
    ";
    let schema = parse_copybook(cpy).unwrap();
    if let Some(lrecl) = schema.lrecl_fixed {
        assert_eq!(lrecl, 35, "10+5+20=35");
    }
}

#[test]
fn test_parse_options_strict_mode() {
    let cpy = r"
       01 REC.
          05 F1 PIC X(10).
    ";
    let opts = ParseOptions {
        strict: true,
        ..ParseOptions::default()
    };
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    assert!(!schema.fields.is_empty());
}

#[test]
fn test_parse_options_emit_filler() {
    let cpy = r"
       01 REC.
          05 F1     PIC X(5).
          05 FILLER PIC X(3).
          05 F2     PIC X(5).
    ";
    let opts = ParseOptions {
        emit_filler: true,
        ..ParseOptions::default()
    };
    let schema = parse_copybook_with_options(cpy, &opts).unwrap();
    let rec = &schema.fields[0];
    assert!(rec.children.len() >= 3);
}

#[test]
fn test_mixed_usage_types_in_record() {
    let cpy = r"
       01 REC.
          05 ALPHA-FLD    PIC X(20).
          05 ZONED-FLD    PIC S9(7)V99.
          05 BINARY-FLD   PIC 9(9) COMP.
          05 PACKED-FLD   PIC S9(5)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let alpha = find_field(&schema, "ALPHA-FLD");
    assert!(matches!(alpha.kind, FieldKind::Alphanum { len: 20 }));
    let zoned = find_field(&schema, "ZONED-FLD");
    assert!(matches!(
        zoned.kind,
        FieldKind::ZonedDecimal { signed: true, .. }
    ));
    let binary = find_field(&schema, "BINARY-FLD");
    assert!(matches!(
        binary.kind,
        FieldKind::BinaryInt { signed: false, .. }
    ));
    let packed = find_field(&schema, "PACKED-FLD");
    assert!(matches!(
        packed.kind,
        FieldKind::PackedDecimal { signed: true, .. }
    ));
}

#[test]
fn test_level88_inside_odo_group() {
    let cpy = r"
       01 REC.
          05 CNT          PIC 9(2).
          05 ITEMS OCCURS 1 TO 10 TIMES
                   DEPENDING ON CNT.
             10 ITEM-CODE PIC X(3).
                88 RUSH   VALUE 'RSH'.
             10 ITEM-QTY  PIC 9(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let rush = find_field(&schema, "RUSH");
    assert_eq!(rush.level, 88);
    match &rush.kind {
        FieldKind::Condition { values } => assert_eq!(values, &["RSH"]),
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_binary_field_sizes() {
    let cpy = r"
       01 REC.
          05 B1 PIC 9(1)  COMP.
          05 B2 PIC 9(4)  COMP.
          05 B3 PIC 9(5)  COMP.
          05 B4 PIC 9(9)  COMP.
          05 B5 PIC 9(10) COMP.
          05 B6 PIC 9(18) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();
    // PIC 9(1)-9(4) -> 2 bytes (16 bits)
    assert_eq!(find_field(&schema, "B1").len, 2);
    assert_eq!(find_field(&schema, "B2").len, 2);
    // PIC 9(5)-9(9) -> 4 bytes (32 bits)
    assert_eq!(find_field(&schema, "B3").len, 4);
    assert_eq!(find_field(&schema, "B4").len, 4);
    // PIC 9(10)-9(18) -> 8 bytes (64 bits)
    assert_eq!(find_field(&schema, "B5").len, 8);
    assert_eq!(find_field(&schema, "B6").len, 8);
}

#[test]
fn test_comp3_packed_field_sizes() {
    let cpy = r"
       01 REC.
          05 P1 PIC 9(1)   COMP-3.
          05 P3 PIC 9(3)   COMP-3.
          05 P5 PIC 9(5)   COMP-3.
          05 P7 PIC S9(7)  COMP-3.
          05 P9 PIC S9(9)  COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    // Packed decimal: ceil((digits+1)/2) bytes
    assert_eq!(find_field(&schema, "P1").len, 1); // ceil(2/2)=1
    assert_eq!(find_field(&schema, "P3").len, 2); // ceil(4/2)=2
    assert_eq!(find_field(&schema, "P5").len, 3); // ceil(6/2)=3
    assert_eq!(find_field(&schema, "P7").len, 4); // ceil(8/2)=4
    assert_eq!(find_field(&schema, "P9").len, 5); // ceil(10/2)=5
}

#[test]
fn test_schema_fingerprint_changes_with_content() {
    let cpy_a = r"
       01 REC.
          05 A PIC X(10).
    ";
    let cpy_b = r"
       01 REC.
          05 B PIC X(20).
    ";
    let schema_a = parse_copybook(cpy_a).unwrap();
    let schema_b = parse_copybook(cpy_b).unwrap();
    assert_ne!(
        schema_a.fingerprint, schema_b.fingerprint,
        "different schemas should have different fingerprints"
    );
}
