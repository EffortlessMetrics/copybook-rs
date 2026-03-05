// SPDX-License-Identifier: AGPL-3.0-or-later
//! PIC clause variety tests for copybook-core parser.
//!
//! Validates all standard PIC patterns (9, X, A, S9, 9V9, S9V9),
//! COMP-3, COMP/BINARY patterns, large digit counts, PIC with VALUE
//! clause, and BLANK WHEN ZERO.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use copybook_core::{FieldKind, parse_copybook};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn find_field<'a>(schema: &'a copybook_core::Schema, name: &str) -> &'a copybook_core::Field {
    schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{name}' not found in schema"))
}

fn parse(text: &str) -> copybook_core::Schema {
    parse_copybook(text).expect("parse failed")
}

// ===========================================================================
// 1. Standard PIC patterns
// ===========================================================================

#[test]
fn test_pic_x_basic() {
    let cpy = "       01  REC.\n           05  F1 PIC X(10).\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 10 }));
    assert_eq!(f.len, 10);
}

#[test]
fn test_pic_x_single() {
    let cpy = "       01  REC.\n           05  F1 PIC X.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 1 }));
    assert_eq!(f.len, 1);
}

#[test]
fn test_pic_9_basic() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(6).\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 6,
            scale: 0,
            signed: false,
            ..
        }
    ));
    assert_eq!(f.len, 6);
}

#[test]
fn test_pic_9_single_digit() {
    let cpy = "       01  REC.\n           05  F1 PIC 9.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 1,
            scale: 0,
            signed: false,
            ..
        }
    ));
    assert_eq!(f.len, 1);
}

#[test]
fn test_pic_x_repeated() {
    let cpy = "       01  REC.\n           05  F1 PIC X(5).\n           05  F2 PIC X(15).\n";
    let schema = parse(cpy);
    assert!(matches!(
        find_field(&schema, "F1").kind,
        FieldKind::Alphanum { len: 5 }
    ));
    assert!(matches!(
        find_field(&schema, "F2").kind,
        FieldKind::Alphanum { len: 15 }
    ));
    assert_eq!(schema.lrecl_fixed, Some(20));
}

#[test]
fn test_pic_s9_signed_zoned() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(5).\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 0,
            signed: true,
            ..
        }
    ));
    // Zoned signed occupies same bytes as digits (sign in zone nibble)
    assert_eq!(f.len, 5);
}

#[test]
fn test_pic_9v9_implied_decimal() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5)V9(2).\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 7,
            scale: 2,
            signed: false,
            ..
        }
    ));
    assert_eq!(f.len, 7);
}

#[test]
fn test_pic_9v99() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(3)V99.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: false,
            ..
        }
    ));
    assert_eq!(f.len, 5);
}

#[test]
fn test_pic_s9v9_signed_decimal() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(7)V99.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 9,
            scale: 2,
            signed: true,
            ..
        }
    ));
    assert_eq!(f.len, 9);
}

#[test]
fn test_pic_s9v9_explicit_scale() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(4)V9(3).\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 7,
            scale: 3,
            signed: true,
            ..
        }
    ));
    assert_eq!(f.len, 7);
}

// ===========================================================================
// 2. COMP-3 (packed decimal) patterns
// ===========================================================================

#[test]
fn test_comp3_s9_5_v99() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(5)V99 COMP-3.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true
        }
    ));
    // COMP-3: (7 + 1) / 2 = 4 bytes
    assert_eq!(f.len, 4);
}

#[test]
fn test_comp3_9_3() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(3) COMP-3.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::PackedDecimal {
            digits: 3,
            scale: 0,
            signed: false
        }
    ));
    // COMP-3 unsigned: (3 + 1) / 2 = 2 bytes
    assert_eq!(f.len, 2);
}

#[test]
fn test_comp3_s9_9_v99() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(9)V99 COMP-3.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::PackedDecimal {
            digits: 11,
            scale: 2,
            signed: true
        }
    ));
    // (11 + 1) / 2 = 6 bytes
    assert_eq!(f.len, 6);
}

#[test]
fn test_comp3_s9_13_v99() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(13)V99 COMP-3.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::PackedDecimal {
            digits: 15,
            scale: 2,
            signed: true
        }
    ));
    // (15 + 1) / 2 = 8 bytes
    assert_eq!(f.len, 8);
}

#[test]
fn test_comp3_single_digit() {
    let cpy = "       01  REC.\n           05  F1 PIC 9 COMP-3.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::PackedDecimal {
            digits: 1,
            scale: 0,
            signed: false
        }
    ));
    // (1 + 1) / 2 = 1 byte
    assert_eq!(f.len, 1);
}

// ===========================================================================
// 3. COMP / BINARY patterns
// ===========================================================================

#[test]
fn test_comp_s9_4() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(4) COMP.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: true
        }
    ));
    assert_eq!(f.len, 2);
}

#[test]
fn test_comp_s9_9() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(9) COMP.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::BinaryInt {
            bits: 32,
            signed: true
        }
    ));
    assert_eq!(f.len, 4);
}

#[test]
fn test_comp_s9_18() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(18) COMP.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::BinaryInt {
            bits: 64,
            signed: true
        }
    ));
    assert_eq!(f.len, 8);
}

#[test]
fn test_comp_unsigned_9_4() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(4) COMP.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: false
        }
    ));
    assert_eq!(f.len, 2);
}

#[test]
fn test_comp_9_2_halfword() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(2) COMP.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::BinaryInt { bits: 16, .. }));
    assert_eq!(f.len, 2);
}

#[test]
fn test_comp_9_5_fullword() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5) COMP.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::BinaryInt { bits: 32, .. }));
    assert_eq!(f.len, 4);
}

#[test]
fn test_comp_9_10_doubleword() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(10) COMP.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::BinaryInt { bits: 64, .. }));
    assert_eq!(f.len, 8);
}

// ===========================================================================
// 4. Large digit counts
// ===========================================================================

#[test]
fn test_pic_9_18_large_zoned() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(18).\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 18,
            scale: 0,
            signed: false,
            ..
        }
    ));
    assert_eq!(f.len, 18);
}

#[test]
fn test_pic_x_large() {
    let cpy = "       01  REC.\n           05  F1 PIC X(1000).\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 1000 }));
    assert_eq!(f.len, 1000);
}

#[test]
fn test_pic_s9_18_comp3_max_packed() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(18) COMP-3.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::PackedDecimal {
            digits: 18,
            scale: 0,
            signed: true
        }
    ));
    // (18 + 1) / 2 = 10 bytes (rounded up)
    assert_eq!(f.len, 10);
}

// ===========================================================================
// 5. PIC with all numeric modifiers — combined record
// ===========================================================================

#[test]
fn test_mixed_pic_types_in_one_record() {
    let cpy = r"
       01  REC.
           05  F-ALPHA     PIC X(10).
           05  F-ZONED     PIC 9(6).
           05  F-SIGNED    PIC S9(5).
           05  F-DEC       PIC 9(5)V99.
           05  F-SDEC      PIC S9(7)V99.
           05  F-COMP3     PIC S9(5)V99 COMP-3.
           05  F-COMP      PIC S9(4) COMP.
    ";
    let schema = parse(cpy);

    assert!(matches!(
        find_field(&schema, "F-ALPHA").kind,
        FieldKind::Alphanum { len: 10 }
    ));
    assert!(matches!(
        find_field(&schema, "F-ZONED").kind,
        FieldKind::ZonedDecimal {
            digits: 6,
            scale: 0,
            signed: false,
            ..
        }
    ));
    assert!(matches!(
        find_field(&schema, "F-SIGNED").kind,
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 0,
            signed: true,
            ..
        }
    ));
    assert!(matches!(
        find_field(&schema, "F-DEC").kind,
        FieldKind::ZonedDecimal {
            digits: 7,
            scale: 2,
            signed: false,
            ..
        }
    ));
    assert!(matches!(
        find_field(&schema, "F-SDEC").kind,
        FieldKind::ZonedDecimal {
            digits: 9,
            scale: 2,
            signed: true,
            ..
        }
    ));
    assert!(matches!(
        find_field(&schema, "F-COMP3").kind,
        FieldKind::PackedDecimal {
            digits: 7,
            scale: 2,
            signed: true
        }
    ));
    assert!(matches!(
        find_field(&schema, "F-COMP").kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: true
        }
    ));

    // LRECL = 10 + 6 + 5 + 7 + 9 + 4 + 2 = 43
    assert_eq!(schema.lrecl_fixed, Some(43));
}

#[test]
fn test_offsets_of_mixed_types() {
    let cpy = r"
       01  REC.
           05  A PIC X(10).
           05  B PIC S9(5)V99 COMP-3.
           05  C PIC S9(9) COMP.
           05  D PIC 9(3).
    ";
    let schema = parse(cpy);
    assert_eq!(find_field(&schema, "A").offset, 0);
    assert_eq!(find_field(&schema, "B").offset, 10); // after X(10)
    assert_eq!(find_field(&schema, "C").offset, 14); // 10 + 4 bytes COMP-3
    assert_eq!(find_field(&schema, "D").offset, 18); // 14 + 4 bytes COMP
    assert_eq!(schema.lrecl_fixed, Some(21)); // 18 + 3
}

// ===========================================================================
// 6. PIC with VALUE clause (Level-88 conditions)
// ===========================================================================

#[test]
fn test_level_88_condition_value() {
    let cpy = r"
       01  REC.
           05  STATUS PIC X(1).
               88  ACTIVE VALUE 'A'.
               88  INACTIVE VALUE 'I'.
           05  FILLER-END PIC X(4).
    ";
    let schema = parse(cpy);
    let all = schema.all_fields();
    let conditions: Vec<_> = all.iter().filter(|f| f.level == 88).collect();
    assert_eq!(conditions.len(), 2);
    assert!(matches!(conditions[0].kind, FieldKind::Condition { .. }));
    assert!(matches!(conditions[1].kind, FieldKind::Condition { .. }));
    // Level-88 doesn't add storage — LRECL is from STATUS + FILLER-END
    assert_eq!(schema.lrecl_fixed, Some(5));
}

#[test]
fn test_level_88_multiple_values() {
    let cpy = r"
       01  REC.
           05  TYPE-CODE PIC X(2).
               88  VALID-TYPES VALUE 'AA' 'BB' 'CC'.
           05  PADDING PIC X(3).
    ";
    let schema = parse(cpy);
    let all = schema.all_fields();
    let cond = all
        .iter()
        .find(|f| f.name == "VALID-TYPES")
        .expect("condition not found");
    assert!(matches!(&cond.kind, FieldKind::Condition { values } if values.len() == 3));
    // Level-88 doesn't add storage — LRECL is TYPE-CODE + PADDING
    assert_eq!(schema.lrecl_fixed, Some(5));
}

// ===========================================================================
// 7. BLANK WHEN ZERO
// ===========================================================================

#[test]
fn test_blank_when_zero_flag() {
    let cpy = r"
       01  REC.
           05  AMOUNT PIC 9(5) BLANK WHEN ZERO.
           05  NAME   PIC X(10).
    ";
    let schema = parse(cpy);
    let amount = find_field(&schema, "AMOUNT");
    assert!(amount.blank_when_zero);
    let name = find_field(&schema, "NAME");
    assert!(!name.blank_when_zero);
}

#[test]
fn test_blank_when_zero_with_decimal() {
    let cpy = r"
       01  REC.
           05  RATE PIC 9(3)V99 BLANK WHEN ZERO.
    ";
    let schema = parse(cpy);
    let rate = find_field(&schema, "RATE");
    assert!(rate.blank_when_zero);
    assert!(matches!(
        rate.kind,
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: false,
            ..
        }
    ));
    assert_eq!(rate.len, 5);
}

// ===========================================================================
// 8. Repeated PIC patterns (shorthand notation)
// ===========================================================================

#[test]
fn test_pic_xxx_shorthand() {
    let cpy = "       01  REC.\n           05  F1 PIC XXX.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 3 }));
    assert_eq!(f.len, 3);
}

#[test]
fn test_pic_999_shorthand() {
    let cpy = "       01  REC.\n           05  F1 PIC 999.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
            ..
        }
    ));
    assert_eq!(f.len, 3);
}

#[test]
fn test_pic_s999v99_shorthand() {
    let cpy = "       01  REC.\n           05  F1 PIC S999V99.\n";
    let schema = parse(cpy);
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::ZonedDecimal {
            digits: 5,
            scale: 2,
            signed: true,
            ..
        }
    ));
    assert_eq!(f.len, 5);
}
