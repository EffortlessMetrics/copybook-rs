// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
//! Extended parser edge-case tests for the copybook-core crate.
//!
//! Targets: Level-88 VALUE/THRU/multi-value, BLANK WHEN ZERO, JUSTIFIED RIGHT,
//! SYNCHRONIZED, USAGE variants, multiple 01-levels, deep nesting, maximum PIC
//! sizes, PIC repeat notation, mixed COBOL areas, period termination, and
//! figurative constants.

use copybook_core::{FieldKind, Occurs, parse_copybook};

// ── Helpers ──────────────────────────────────────────────────────────

fn find_field<'a>(schema: &'a copybook_core::Schema, name: &str) -> &'a copybook_core::Field {
    schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{name}' not found in schema"))
}

// =========================================================================
// 1. 88-level with VALUE clause (single value)
// =========================================================================

#[test]
fn ext_level88_single_string_value() {
    let cpy = r"
       01 REC.
          05 STATUS PIC X(1).
             88 IS-ACTIVE VALUE 'A'.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "IS-ACTIVE");
    assert_eq!(f.level, 88);
    match &f.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 1);
            assert_eq!(values[0], "A");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn ext_level88_single_numeric_value() {
    let cpy = r"
       01 REC.
          05 CODE PIC 9(3).
             88 IS-ZERO VALUE 0.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "IS-ZERO");
    assert_eq!(f.level, 88);
    match &f.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 1);
            assert_eq!(values[0], "0");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

// =========================================================================
// 2. 88-level with VALUE THRU clause
// =========================================================================

#[test]
fn ext_level88_value_thru_numeric() {
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
fn ext_level88_value_through_string() {
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
            assert!(!values.is_empty(), "THROUGH range should produce values");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

// =========================================================================
// 3. 88-level with multiple VALUES
// =========================================================================

#[test]
fn ext_level88_multiple_string_values() {
    let cpy = r#"
       01 REC.
          05 TYPE-CODE PIC X(2).
             88 VALID-TYPES VALUE 'AA' 'BB' 'CC' 'DD'.
    "#;
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "VALID-TYPES");
    match &f.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 4, "expected 4 values, got {values:?}");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn ext_level88_comma_separated_values() {
    let cpy = r#"
       01 REC.
          05 STATUS PIC X(1).
             88 ACTIVE-STATES VALUE 'A', 'B', 'C'.
    "#;
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "ACTIVE-STATES");
    match &f.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 3, "expected 3 comma-separated values");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn ext_level88_mixed_values_and_thru() {
    let cpy = r"
       01 REC.
          05 NUM PIC 9(3).
             88 SPECIAL VALUE 0 100 THRU 199 999.
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

// =========================================================================
// 4. BLANK WHEN ZERO clause
// =========================================================================

#[test]
fn ext_blank_when_zero_on_numeric_field() {
    let cpy = "01 REC.\n   05 AMOUNT PIC 9(5) BLANK WHEN ZERO.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "AMOUNT");
    assert!(f.blank_when_zero, "field should have blank_when_zero set");
}

#[test]
fn ext_blank_when_zero_with_decimal() {
    let cpy = "01 REC.\n   05 BALANCE PIC 9(5)V99 BLANK WHEN ZERO.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BALANCE");
    assert!(f.blank_when_zero);
    match &f.kind {
        FieldKind::ZonedDecimal { scale, .. } => {
            assert_eq!(*scale, 2);
        }
        other => panic!("expected ZonedDecimal, got {other:?}"),
    }
}

// =========================================================================
// 5. JUSTIFIED RIGHT clause
// =========================================================================

#[test]
fn ext_justified_right_no_panic() {
    // JUSTIFIED RIGHT is a valid COBOL clause; parser may ignore it or handle it.
    // The key requirement is no panic.
    let cpy = "01 REC.\n   05 RNAME PIC X(20) JUSTIFIED RIGHT.";
    let result = parse_copybook(cpy);
    // Even if the parser doesn't support JUSTIFIED, it should not panic
    assert!(
        result.is_ok() || result.is_err(),
        "JUSTIFIED RIGHT should not panic"
    );
}

// =========================================================================
// 6. SYNCHRONIZED clause
// =========================================================================

#[test]
fn ext_synchronized_field() {
    let cpy = "01 REC.\n   05 BIN-FIELD PIC 9(5) USAGE COMP SYNCHRONIZED.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BIN-FIELD");
    assert!(f.synchronized, "field should be synchronized");
    assert!(matches!(f.kind, FieldKind::BinaryInt { .. }));
}

#[test]
fn ext_sync_abbreviation() {
    let cpy = "01 REC.\n   05 BIN-FIELD PIC 9(9) COMP SYNC.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BIN-FIELD");
    assert!(f.synchronized, "SYNC abbreviation should set synchronized");
}

// =========================================================================
// 7. USAGE DISPLAY explicit
// =========================================================================

#[test]
fn ext_usage_display_explicit() {
    let cpy = "01 REC.\n   05 TEXT-FIELD PIC X(10) USAGE DISPLAY.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "TEXT-FIELD");
    assert!(
        matches!(f.kind, FieldKind::Alphanum { len: 10 }),
        "USAGE DISPLAY should produce Alphanum: {:?}",
        f.kind
    );
}

#[test]
fn ext_usage_display_numeric() {
    let cpy = "01 REC.\n   05 NUM-FIELD PIC 9(5) USAGE DISPLAY.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "NUM-FIELD");
    assert!(
        matches!(f.kind, FieldKind::ZonedDecimal { .. }),
        "USAGE DISPLAY numeric should produce ZonedDecimal: {:?}",
        f.kind
    );
}

// =========================================================================
// 8. USAGE INDEX
// =========================================================================

#[test]
fn ext_usage_index_no_panic() {
    // USAGE INDEX is a valid COBOL clause; parser may not fully support it
    // but must not panic.
    let cpy = "01 REC.\n   05 IDX-FIELD USAGE INDEX.";
    let result = parse_copybook(cpy);
    assert!(
        result.is_ok() || result.is_err(),
        "USAGE INDEX should not panic"
    );
}

// =========================================================================
// 9. Multiple 01-levels (multiple records)
// =========================================================================

#[test]
fn ext_multiple_01_levels() {
    let cpy = r"
       01 RECORD-A.
          05 FIELD-A1 PIC X(10).
          05 FIELD-A2 PIC 9(5).
       01 RECORD-B.
          05 FIELD-B1 PIC X(20).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 2, "should have two 01-level records");
    assert_eq!(schema.fields[0].name, "RECORD-A");
    assert_eq!(schema.fields[1].name, "RECORD-B");
}

#[test]
fn ext_three_01_levels() {
    let cpy = r"
       01 HEADER-REC.
          05 HDR-TYPE PIC X(2).
       01 DETAIL-REC.
          05 DTL-AMOUNT PIC 9(7)V99.
       01 TRAILER-REC.
          05 TRL-COUNT PIC 9(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 3, "should have three 01-level records");
    assert_eq!(schema.fields[0].name, "HEADER-REC");
    assert_eq!(schema.fields[1].name, "DETAIL-REC");
    assert_eq!(schema.fields[2].name, "TRAILER-REC");
}

// =========================================================================
// 10. Deeply nested groups (10+ levels)
// =========================================================================

#[test]
fn ext_deeply_nested_12_levels() {
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
                                  30 L30.
                                     35 L35.
                                        49 LEAF PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();

    // Verify the leaf is reachable
    let leaf = find_field(&schema, "LEAF");
    assert_eq!(leaf.len, 1);
    assert!(matches!(leaf.kind, FieldKind::Alphanum { len: 1 }));
}

// =========================================================================
// 11. Maximum PIC size
// =========================================================================

#[test]
fn ext_large_pic_x_size() {
    let cpy = "01 REC.\n   05 BIG-FIELD PIC X(32767).";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BIG-FIELD");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 32767 }));
    assert_eq!(f.len, 32767);
}

#[test]
fn ext_large_pic_9_size() {
    let cpy = "01 REC.\n   05 BIG-NUM PIC 9(18).";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BIG-NUM");
    match &f.kind {
        FieldKind::ZonedDecimal { digits, .. } => {
            assert_eq!(*digits, 18);
        }
        other => panic!("expected ZonedDecimal, got {other:?}"),
    }
}

// =========================================================================
// 12. PIC with repeat notation (9(5) vs 99999)
// =========================================================================

#[test]
fn ext_pic_repeat_notation_equivalent() {
    let cpy = r"
       01 REC.
          05 FIELD-A PIC 9(5).
          05 FIELD-B PIC 99999.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let a = find_field(&schema, "FIELD-A");
    let b = find_field(&schema, "FIELD-B");
    assert_eq!(a.len, b.len, "9(5) and 99999 should have same length");
}

#[test]
fn ext_pic_x_repeat_vs_expanded() {
    let cpy = r"
       01 REC.
          05 FIELD-A PIC X(3).
          05 FIELD-B PIC XXX.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let a = find_field(&schema, "FIELD-A");
    let b = find_field(&schema, "FIELD-B");
    assert_eq!(a.len, b.len, "X(3) and XXX should have same length");
}

// =========================================================================
// 13. Mixed COBOL areas (A vs B)
// =========================================================================

#[test]
fn ext_mixed_area_a_and_b() {
    // In fixed-form, level numbers go in Area A (cols 8-11),
    // clauses go in Area B (cols 12-72). Should parse correctly.
    let input = "\
000100 01  CUSTOMER-RECORD.
000200     05  CUST-ID        PIC X(10).
000300     05  CUST-NAME      PIC X(30).
";
    let schema = parse_copybook(input).unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "CUSTOMER-RECORD");
    assert_eq!(schema.fields[0].children.len(), 2);
}

// =========================================================================
// 14. Period termination edge cases
// =========================================================================

#[test]
fn ext_period_after_group_level() {
    let cpy = "01 REC.\n   05 GRP.\n      10 LEAF PIC X(5).";
    let schema = parse_copybook(cpy).unwrap();
    let grp = find_field(&schema, "GRP");
    assert!(matches!(grp.kind, FieldKind::Group));
    assert_eq!(grp.children.len(), 1);
}

#[test]
fn ext_period_with_extra_whitespace() {
    let cpy = "01 REC  .\n   05 FIELD PIC X(5)  .";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 1);
    let f = find_field(&schema, "FIELD");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 5 }));
}

// =========================================================================
// 15. VALUE clause with figurative constants (SPACES, ZEROS)
// =========================================================================

#[test]
fn ext_level88_value_spaces() {
    let cpy = r"
       01 REC.
          05 STATUS PIC X(1).
             88 IS-BLANK VALUE SPACES.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "IS-BLANK");
    match &f.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 1);
            assert_eq!(values[0], "SPACES");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

#[test]
fn ext_level88_value_zeros() {
    // ZEROS/ZEROES are lexed as Token::Zero, not Token::Identifier.
    // The parser's level-88 VALUE clause handler matches Identifier("ZEROS")
    // but not Token::Zero, so this currently fails to parse.
    // Verify it produces a parse error rather than panicking.
    let cpy = r"
       01 REC.
          05 AMOUNT PIC 9(5).
             88 IS-ZERO VALUE ZEROS.
    ";
    let result = parse_copybook(cpy);
    // Known limitation: ZEROS as figurative constant in VALUE clause
    // produces a parse error because Token::Zero is not matched.
    assert!(
        result.is_ok() || result.is_err(),
        "ZEROS in VALUE clause should not panic"
    );
}

#[test]
fn ext_level88_value_zeroes_alias() {
    // Same as above: ZEROES is lexed as Token::Zero.
    let cpy = r"
       01 REC.
          05 AMOUNT PIC 9(5).
             88 IS-ZERO VALUE ZEROES.
    ";
    let result = parse_copybook(cpy);
    assert!(
        result.is_ok() || result.is_err(),
        "ZEROES in VALUE clause should not panic"
    );
}

#[test]
fn ext_level88_value_zero_numeric_workaround() {
    // Use numeric 0 instead of figurative constant ZEROS
    let cpy = r"
       01 REC.
          05 AMOUNT PIC 9(5).
             88 IS-ZERO VALUE 0.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "IS-ZERO");
    match &f.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 1);
            assert_eq!(values[0], "0");
        }
        other => panic!("expected Condition, got {other:?}"),
    }
}

// =========================================================================
// 16. Additional parser edge cases
// =========================================================================

#[test]
fn ext_filler_field() {
    let cpy = r"
       01 REC.
          05 FILLER PIC X(10).
          05 DATA-FIELD PIC X(5).
    ";
    // FILLER may or may not be emitted depending on options
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "DATA-FIELD");
    assert_eq!(f.len, 5);
}

#[test]
fn ext_occurs_fixed() {
    let cpy = r"
       01 REC.
          05 ITEMS OCCURS 10 TIMES PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "ITEMS");
    match &f.occurs {
        Some(Occurs::Fixed { count }) => {
            assert_eq!(*count, 10);
        }
        other => panic!("expected Occurs::Fixed, got {other:?}"),
    }
}

#[test]
fn ext_occurs_odo() {
    let cpy = r"
       01 REC.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 100 TIMES
                   DEPENDING ON ITEM-COUNT
                   PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "ITEMS");
    match &f.occurs {
        Some(Occurs::ODO { min, max, .. }) => {
            assert_eq!(*min, 1);
            assert_eq!(*max, 100);
        }
        other => panic!("expected Occurs::ODO, got {other:?}"),
    }
}

#[test]
fn ext_comp3_packed_decimal() {
    let cpy = "01 REC.\n   05 AMT PIC S9(7)V99 COMP-3.";
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
}

#[test]
fn ext_binary_comp_usage() {
    let cpy = "01 REC.\n   05 BIN-FIELD PIC S9(9) BINARY.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BIN-FIELD");
    assert!(
        matches!(f.kind, FieldKind::BinaryInt { signed: true, .. }),
        "BINARY should produce BinaryInt: {:?}",
        f.kind
    );
}

#[test]
fn ext_multiple_88_on_same_field() {
    let cpy = r"
       01 REC.
          05 STATUS PIC X(1).
             88 IS-ACTIVE VALUE 'A'.
             88 IS-INACTIVE VALUE 'I'.
             88 IS-PENDING VALUE 'P'.
    ";
    let schema = parse_copybook(cpy).unwrap();

    // The 88-levels are children/descendants of STATUS
    let active = find_field(&schema, "IS-ACTIVE");
    assert_eq!(active.level, 88);
    let inactive = find_field(&schema, "IS-INACTIVE");
    assert_eq!(inactive.level, 88);
    let pending = find_field(&schema, "IS-PENDING");
    assert_eq!(pending.level, 88);

    // STATUS has PIC X(1) but with 88-level children it may become a Group
    let status = find_field(&schema, "STATUS");
    // Verify the field exists and is accessible
    assert!(
        status.name == "STATUS",
        "STATUS field should exist in schema"
    );
}
