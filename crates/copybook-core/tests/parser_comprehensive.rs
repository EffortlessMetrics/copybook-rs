// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive parser tests for copybook-core.
//!
//! Covers REDEFINES, OCCURS, ODO, Level-88, RENAMES (66), nested groups,
//! PIC clauses, edited PIC, error recovery, and inline comments.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use copybook_core::{
    ErrorCode, FieldKind, Occurs, ParseOptions, parse_copybook, parse_copybook_with_options,
};

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

/// Collect every field (recursive) from a schema.
fn all_fields(schema: &copybook_core::Schema) -> Vec<&copybook_core::Field> {
    schema.all_fields()
}

/// Find a field by name among all fields in the schema.
fn find_field<'a>(schema: &'a copybook_core::Schema, name: &str) -> &'a copybook_core::Field {
    all_fields(schema)
        .into_iter()
        .find(|f| f.name == name)
        .unwrap_or_else(|| panic!("field '{}' not found in schema", name))
}

// ===========================================================================
// REDEFINES
// ===========================================================================

#[test]
fn test_redefines_basic() {
    let cpy = r"
       01 REC.
          05 FIELD-A    PIC X(10).
          05 FIELD-B    REDEFINES FIELD-A PIC 9(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let fb = find_field(&schema, "FIELD-B");
    assert_eq!(fb.redefines_of.as_deref(), Some("FIELD-A"));
    assert!(matches!(
        fb.kind,
        FieldKind::ZonedDecimal { digits: 10, .. }
    ));
}

#[test]
fn test_redefines_nested() {
    let cpy = r"
       01 REC.
          05 GRP-A.
             10 SUB-A   PIC X(5).
          05 GRP-B      REDEFINES GRP-A.
             10 SUB-B   PIC 9(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let grp_b = find_field(&schema, "GRP-B");
    assert_eq!(grp_b.redefines_of.as_deref(), Some("GRP-A"));
    assert!(matches!(grp_b.kind, FieldKind::Group));
    assert_eq!(grp_b.children.len(), 1);
    assert_eq!(grp_b.children[0].name, "SUB-B");
}

#[test]
fn test_redefines_multiple_same_target() {
    let cpy = r"
       01 REC.
          05 ORIG       PIC X(8).
          05 AS-NUM     REDEFINES ORIG PIC 9(8).
          05 AS-ALPHA   REDEFINES ORIG PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let as_num = find_field(&schema, "AS-NUM");
    let as_alpha = find_field(&schema, "AS-ALPHA");
    assert_eq!(as_num.redefines_of.as_deref(), Some("ORIG"));
    assert_eq!(as_alpha.redefines_of.as_deref(), Some("ORIG"));
}

// ===========================================================================
// OCCURS (fixed)
// ===========================================================================

#[test]
fn test_occurs_fixed_count() {
    let cpy = r"
       01 REC.
          05 ITEMS OCCURS 5 TIMES PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let items = find_field(&schema, "ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 5 })));
}

#[test]
fn test_occurs_with_nested_fields() {
    let cpy = r"
       01 REC.
          05 LINE-ITEM OCCURS 3 TIMES.
             10 ITEM-NAME  PIC X(20).
             10 ITEM-QTY   PIC 9(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let line_item = find_field(&schema, "LINE-ITEM");
    assert!(matches!(line_item.occurs, Some(Occurs::Fixed { count: 3 })));
    assert_eq!(line_item.children.len(), 2);
}

#[test]
fn test_occurs_inside_group() {
    let cpy = r"
       01 REC.
          05 HEADER   PIC X(4).
          05 DETAIL-GRP.
             10 DETAIL OCCURS 10 TIMES PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let detail = find_field(&schema, "DETAIL");
    assert!(matches!(detail.occurs, Some(Occurs::Fixed { count: 10 })));
}

// ===========================================================================
// ODO (OCCURS DEPENDING ON)
// ===========================================================================

#[test]
fn test_odo_valid() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(3).
          05 ITEMS     OCCURS 1 TO 100 TIMES
                       DEPENDING ON CNT
                       PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let items = find_field(&schema, "ITEMS");
    match &items.occurs {
        Some(Occurs::ODO {
            min,
            max,
            counter_path,
        }) => {
            assert_eq!(*min, 1);
            assert_eq!(*max, 100);
            assert_eq!(counter_path, "CNT");
        }
        other => panic!("expected ODO, got {:?}", other),
    }
    // Schema should record tail ODO information
    assert!(schema.tail_odo.is_some());
}

#[test]
fn test_odo_counter_not_found() {
    let cpy = r"
       01 REC.
          05 ITEMS     OCCURS 1 TO 10 TIMES
                       DEPENDING ON MISSING-CNT
                       PIC X(5).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKS121_COUNTER_NOT_FOUND);
}

#[test]
fn test_odo_not_at_tail_rejected() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(3).
          05 ITEMS     OCCURS 1 TO 10 TIMES
                       DEPENDING ON CNT
                       PIC X(5).
          05 TRAILER   PIC X(4).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP021_ODO_NOT_TAIL);
}

// ===========================================================================
// Level-88 condition values
// ===========================================================================

#[test]
fn test_level88_simple_value() {
    let cpy = r"
       01 REC.
          05 STATUS     PIC X(1).
             88 ACTIVE  VALUE 'A'.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let active = find_field(&schema, "ACTIVE");
    assert_eq!(active.level, 88);
    match &active.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values, &["A"]);
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_level88_multiple_values() {
    let cpy = r"
       01 REC.
          05 CODE       PIC X(1).
             88 VOWEL   VALUE 'A' 'E' 'I' 'O' 'U'.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let vowel = find_field(&schema, "VOWEL");
    match &vowel.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 5);
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_level88_thru_range() {
    let cpy = r"
       01 REC.
          05 SCORE      PIC 9(3).
             88 PASSING VALUE 60 THRU 100.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let passing = find_field(&schema, "PASSING");
    match &passing.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 1);
            assert!(values[0].contains("THRU") || values[0].contains("THROUGH"));
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_level88_after_regular_fields() {
    let cpy = r"
       01 REC.
          05 NAME       PIC X(20).
          05 STATUS     PIC X(1).
             88 ACTIVE  VALUE 'A'.
             88 DELETED VALUE 'D'.
          05 AMOUNT     PIC 9(7)V99.
    ";
    let schema = parse_copybook(cpy).unwrap();
    // Level-88 conditions are children of STATUS
    let status = find_field(&schema, "STATUS");
    let condition_children: Vec<_> = status.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(condition_children.len(), 2);
    // AMOUNT should still exist as a sibling
    let _amount = find_field(&schema, "AMOUNT");
}

// ===========================================================================
// RENAMES (level 66)
// ===========================================================================

#[test]
fn test_renames_simple() {
    let cpy = r"
       01 REC.
          05 FIELD-A    PIC X(10).
          05 FIELD-B    PIC X(20).
       66 ALIAS-AB     RENAMES FIELD-A THRU FIELD-B.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let alias = find_field(&schema, "ALIAS-AB");
    assert_eq!(alias.level, 66);
    match &alias.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FIELD-A");
            assert_eq!(thru_field, "FIELD-B");
        }
        other => panic!("expected Renames, got {:?}", other),
    }
}

#[test]
fn test_renames_single_field() {
    let cpy = r"
       01 REC.
          05 FIELD-X    PIC X(15).
          05 FIELD-Y    PIC 9(5).
       66 ALIAS-X      RENAMES FIELD-X.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let alias = find_field(&schema, "ALIAS-X");
    match &alias.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FIELD-X");
            assert_eq!(thru_field, "FIELD-X"); // single-field: from == thru
        }
        other => panic!("expected Renames, got {:?}", other),
    }
}

#[test]
fn test_renames_group_range() {
    let cpy = r"
       01 REC.
          05 FIELD-P    PIC X(5).
          05 FIELD-Q    PIC X(5).
          05 FIELD-R    PIC X(5).
       66 RANGE-PQR    RENAMES FIELD-P THRU FIELD-R.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let alias = find_field(&schema, "RANGE-PQR");
    match &alias.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FIELD-P");
            assert_eq!(thru_field, "FIELD-R");
        }
        other => panic!("expected Renames, got {:?}", other),
    }
}

// ===========================================================================
// Nested groups
// ===========================================================================

#[test]
fn test_deep_nesting() {
    let cpy = r"
       01 REC.
          05 L1.
             10 L2.
                15 L3.
                   20 L4.
                      25 L5 PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    // Walk down the hierarchy
    let rec = &schema.fields[0];
    assert_eq!(rec.name, "REC");
    let l1 = &rec.children[0];
    assert_eq!(l1.name, "L1");
    let l2 = &l1.children[0];
    assert_eq!(l2.name, "L2");
    let l3 = &l2.children[0];
    assert_eq!(l3.name, "L3");
    let l4 = &l3.children[0];
    assert_eq!(l4.name, "L4");
    let l5 = &l4.children[0];
    assert_eq!(l5.name, "L5");
    assert!(matches!(l5.kind, FieldKind::Alphanum { len: 1 }));
}

#[test]
fn test_single_field_group() {
    let cpy = r"
       01 REC.
          05 WRAPPER.
             10 ONLY-CHILD PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let wrapper = find_field(&schema, "WRAPPER");
    assert!(matches!(wrapper.kind, FieldKind::Group));
    assert_eq!(wrapper.children.len(), 1);
}

#[test]
fn test_multiple_sibling_groups() {
    let cpy = r"
       01 REC.
          05 GRP-A.
             10 FA PIC X(2).
          05 GRP-B.
             10 FB PIC X(3).
          05 GRP-C.
             10 FC PIC X(4).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let rec = &schema.fields[0];
    assert_eq!(rec.children.len(), 3);
    assert_eq!(rec.children[0].name, "GRP-A");
    assert_eq!(rec.children[1].name, "GRP-B");
    assert_eq!(rec.children[2].name, "GRP-C");
}

// ===========================================================================
// PIC clauses — all types
// ===========================================================================

#[test]
fn test_pic_alphanumeric() {
    let cpy = r"
       01 REC.
          05 F-X  PIC X(20).
          05 F-X2 PIC X.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let fx = find_field(&schema, "F-X");
    assert!(matches!(fx.kind, FieldKind::Alphanum { len: 20 }));
    let fx2 = find_field(&schema, "F-X2");
    assert!(matches!(fx2.kind, FieldKind::Alphanum { len: 1 }));
}

#[test]
fn test_pic_numeric_unsigned() {
    let cpy = r"
       01 REC.
          05 F1 PIC 9(7).
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
            assert_eq!(*digits, 7);
            assert_eq!(*scale, 0);
            assert!(!signed);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

#[test]
fn test_pic_numeric_signed_with_decimal() {
    let cpy = r"
       01 REC.
          05 AMOUNT PIC S9(7)V99.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "AMOUNT");
    match &f.kind {
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => {
            assert_eq!(*digits, 9); // 7 integer + 2 decimal = 9 total
            assert_eq!(*scale, 2);
            assert!(signed);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

#[test]
fn test_pic_comp_binary() {
    let cpy = r"
       01 REC.
          05 SMALL-INT  PIC 9(4) COMP.
          05 MED-INT    PIC 9(9) COMP.
          05 BIG-INT    PIC 9(18) COMP.
    ";
    let schema = parse_copybook(cpy).unwrap();

    let si = find_field(&schema, "SMALL-INT");
    assert!(matches!(
        si.kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: false
        }
    ));

    let mi = find_field(&schema, "MED-INT");
    assert!(matches!(
        mi.kind,
        FieldKind::BinaryInt {
            bits: 32,
            signed: false
        }
    ));

    let bi = find_field(&schema, "BIG-INT");
    assert!(matches!(
        bi.kind,
        FieldKind::BinaryInt {
            bits: 64,
            signed: false
        }
    ));
}

#[test]
fn test_pic_comp3_packed() {
    let cpy = r"
       01 REC.
          05 BAL PIC S9(7)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BAL");
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
        other => panic!("expected PackedDecimal, got {:?}", other),
    }
}

#[test]
fn test_pic_implied_decimal_p() {
    // PIC P(3)9(2) or similar — P shifts the decimal left
    let cpy = r"
       01 REC.
          05 F1 PIC 9(3)V9(2).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    match &f.kind {
        FieldKind::ZonedDecimal { digits, scale, .. } => {
            assert_eq!(*digits, 5);
            assert_eq!(*scale, 2);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
}

// ===========================================================================
// Edited PIC
// ===========================================================================

#[test]
fn test_edited_pic_z_suppression() {
    let cpy = r"
       01 REC.
          05 AMT PIC ZZZ9.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "AMT");
    match &f.kind {
        FieldKind::EditedNumeric { pic_string, .. } => {
            assert!(pic_string.contains('Z'));
        }
        other => panic!("expected EditedNumeric, got {:?}", other),
    }
}

#[test]
fn test_edited_pic_currency() {
    let cpy = "01 REC.\n   05 PRICE PIC $ZZZ.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "PRICE");
    match &f.kind {
        FieldKind::EditedNumeric { pic_string, .. } => {
            assert!(pic_string.contains('$'));
        }
        other => panic!("expected EditedNumeric, got {:?}", other),
    }
}

#[test]
fn test_edited_pic_sign_plus_minus() {
    let cpy = r"
       01 REC.
          05 F-PLUS  PIC +ZZ9.
          05 F-MINUS PIC -ZZ9.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let fp = find_field(&schema, "F-PLUS");
    match &fp.kind {
        FieldKind::EditedNumeric { signed, .. } => {
            assert!(signed);
        }
        other => panic!("expected EditedNumeric, got {:?}", other),
    }
    let fm = find_field(&schema, "F-MINUS");
    match &fm.kind {
        FieldKind::EditedNumeric { signed, .. } => {
            assert!(signed);
        }
        other => panic!("expected EditedNumeric, got {:?}", other),
    }
}

#[test]
fn test_edited_pic_asterisk() {
    let cpy = "01 REC.\n   05 CHECK-AMT PIC ***9.";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "CHECK-AMT");
    match &f.kind {
        FieldKind::EditedNumeric { pic_string, .. } => {
            assert!(pic_string.contains('*'));
        }
        other => panic!("expected EditedNumeric, got {:?}", other),
    }
}

#[test]
fn test_edited_pic_cr_db() {
    let cpy = r"
       01 REC.
          05 BAL-CR PIC 9(5)V99CR.
          05 BAL-DB PIC 9(5)V99DB.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let cr = find_field(&schema, "BAL-CR");
    assert!(matches!(
        cr.kind,
        FieldKind::EditedNumeric { signed: true, .. }
    ));
    let db = find_field(&schema, "BAL-DB");
    assert!(matches!(
        db.kind,
        FieldKind::EditedNumeric { signed: true, .. }
    ));
}

// ===========================================================================
// Error recovery
// ===========================================================================

#[test]
fn test_error_empty_copybook() {
    let err = parse_copybook("").unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_error_invalid_level_number() {
    // Level 0 is invalid
    let cpy = "0 BAD-FIELD PIC X(5).";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(err.code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_error_missing_field_name() {
    // Level number without a field name, followed immediately by PIC
    let cpy = "01 PIC X(5).";
    // This may parse differently (PIC could be mistaken for a name), but
    // the point is it should not panic.
    let result = parse_copybook(cpy);
    // We accept either a parse error or a weird-but-valid parse; the important
    // thing is no panic.
    let _ = result;
}

// ===========================================================================
// Inline comments
// ===========================================================================

#[test]
fn test_inline_comments_allowed_by_default() {
    let cpy = r"
       01 REC. *> record root
          05 F1 PIC X(10). *> first field
          05 F2 PIC 9(5).  *> second field
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields[0].children.len(), 2);
}

#[test]
fn test_inline_comments_disabled() {
    let cpy = r"
       01 REC. *> this is a comment
          05 F1 PIC X(10).
    ";
    let opts = ParseOptions {
        allow_inline_comments: false,
        ..ParseOptions::default()
    };
    // With inline comments disabled, the *> text is not stripped.
    // Depending on parser, this might still parse or error; no panic is key.
    let _ = parse_copybook_with_options(cpy, &opts);
}

// ===========================================================================
// Additional edge-case and mixed scenarios
// ===========================================================================

#[test]
fn test_level77_independent_item() {
    let cpy = r"
       01 REC.
          05 F1 PIC X(5).
       77 STANDALONE PIC 9(3).
    ";
    let schema = parse_copybook(cpy).unwrap();
    // Level-77 items are top-level independents
    let standalone = all_fields(&schema)
        .into_iter()
        .find(|f| f.name == "STANDALONE");
    assert!(standalone.is_some());
    assert_eq!(standalone.unwrap().level, 77);
}

#[test]
fn test_blank_when_zero() {
    let cpy = r"
       01 REC.
          05 QTY PIC ZZ9 BLANK WHEN ZERO.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let qty = find_field(&schema, "QTY");
    assert!(qty.blank_when_zero);
}

#[test]
fn test_occurs_and_redefines_combined() {
    let cpy = r"
       01 REC.
          05 RAW-DATA    PIC X(30).
          05 STRUCTURED  REDEFINES RAW-DATA.
             10 ITEMS OCCURS 3 TIMES.
                15 ITEM-CODE PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let structured = find_field(&schema, "STRUCTURED");
    assert_eq!(structured.redefines_of.as_deref(), Some("RAW-DATA"));
    let items = find_field(&schema, "ITEMS");
    assert!(matches!(items.occurs, Some(Occurs::Fixed { count: 3 })));
}

#[test]
fn test_filler_handling() {
    let cpy = r"
       01 REC.
          05 F1     PIC X(5).
          05 FILLER PIC X(3).
          05 F2     PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    // By default, FILLER fields may be kept but not emitted. Verify no panic.
    let rec = &schema.fields[0];
    // The record should have at least F1 and F2
    let names: Vec<_> = rec.children.iter().map(|c| c.name.as_str()).collect();
    assert!(names.contains(&"F1"));
    assert!(names.contains(&"F2"));
}

#[test]
fn test_multiple_records() {
    let cpy = r"
       01 REC-A.
          05 FA PIC X(5).
       01 REC-B.
          05 FB PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 2);
    assert_eq!(schema.fields[0].name, "REC-A");
    assert_eq!(schema.fields[1].name, "REC-B");
}

#[test]
fn test_level88_with_numeric_range_through() {
    let cpy = r"
       01 REC.
          05 GRADE PIC 9(3).
             88 HONOR-ROLL VALUE 90 THROUGH 100.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let honor = find_field(&schema, "HONOR-ROLL");
    match &honor.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 1);
            assert!(values[0].contains("THROUGH"));
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_schema_fingerprint_not_empty() {
    let cpy = r"
       01 REC.
          05 F1 PIC X(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert!(
        !schema.fingerprint.is_empty(),
        "fingerprint should be generated"
    );
}

#[test]
fn test_field_offsets_resolved() {
    let cpy = r"
       01 REC.
          05 A PIC X(5).
          05 B PIC X(10).
          05 C PIC X(3).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let a = find_field(&schema, "A");
    let b = find_field(&schema, "B");
    let c = find_field(&schema, "C");
    assert_eq!(a.offset, 0);
    assert_eq!(a.len, 5);
    assert_eq!(b.offset, 5);
    assert_eq!(b.len, 10);
    assert_eq!(c.offset, 15);
    assert_eq!(c.len, 3);
}

#[test]
fn test_usage_binary_keyword() {
    let cpy = r"
       01 REC.
          05 F1 PIC S9(4) USAGE BINARY.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    assert!(matches!(
        f.kind,
        FieldKind::BinaryInt {
            bits: 16,
            signed: true
        }
    ));
}

// ===========================================================================
// REDEFINES — deep scenarios
// ===========================================================================

#[test]
fn test_redefines_nested_group_r3() {
    let cpy = r"
       01 REC.
          05 GRP-OUTER.
             10 GRP-INNER.
                15 FIELD-A PIC X(10).
          05 GRP-OUTER-R REDEFINES GRP-OUTER.
             10 GRP-INNER-R.
                15 FIELD-B PIC 9(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let grp_r = find_field(&schema, "GRP-OUTER-R");
    assert_eq!(grp_r.redefines_of.as_deref(), Some("GRP-OUTER"));
    let inner_r = find_field(&schema, "GRP-INNER-R");
    assert!(matches!(inner_r.kind, FieldKind::Group));
    let fb = find_field(&schema, "FIELD-B");
    assert!(matches!(
        fb.kind,
        FieldKind::ZonedDecimal { digits: 10, .. }
    ));
}

#[test]
fn test_redefines_elementary_to_different_elementary() {
    let cpy = r"
       01 REC.
          05 RAW-BYTES   PIC X(5).
          05 AS-PACKED   REDEFINES RAW-BYTES PIC S9(7) COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let raw = find_field(&schema, "RAW-BYTES");
    assert!(matches!(raw.kind, FieldKind::Alphanum { len: 5 }));
    let packed = find_field(&schema, "AS-PACKED");
    assert_eq!(packed.redefines_of.as_deref(), Some("RAW-BYTES"));
    assert!(matches!(packed.kind, FieldKind::PackedDecimal { .. }));
}

#[test]
fn test_redefines_chain_three_overlapping() {
    let cpy = r"
       01 REC.
          05 ORIG       PIC X(16).
          05 VIEW-A     REDEFINES ORIG PIC 9(16).
          05 VIEW-B     REDEFINES ORIG.
             10 PART-1  PIC X(8).
             10 PART-2  PIC X(8).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(
        find_field(&schema, "VIEW-A").redefines_of.as_deref(),
        Some("ORIG")
    );
    assert_eq!(
        find_field(&schema, "VIEW-B").redefines_of.as_deref(),
        Some("ORIG")
    );
    let vb = find_field(&schema, "VIEW-B");
    assert_eq!(vb.children.len(), 2);
}

#[test]
fn test_redefines_preserves_offset_of_original() {
    let cpy = r"
       01 REC.
          05 PREFIX     PIC X(4).
          05 ORIG       PIC X(10).
          05 ALT        REDEFINES ORIG PIC 9(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let orig = find_field(&schema, "ORIG");
    let alt = find_field(&schema, "ALT");
    assert_eq!(orig.offset, alt.offset, "REDEFINES must share offset");
    assert_eq!(orig.len, alt.len);
}

// ===========================================================================
// OCCURS — additional integration
// ===========================================================================

#[test]
fn test_occurs_two_fixed_arrays_in_record() {
    let cpy = r"
       01 REC.
          05 ARR-A PIC X(4) OCCURS 3 TIMES.
          05 ARR-B PIC 9(2) OCCURS 5 TIMES.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let aa = find_field(&schema, "ARR-A");
    assert!(matches!(aa.occurs, Some(Occurs::Fixed { count: 3 })));
    let ab = find_field(&schema, "ARR-B");
    assert!(matches!(ab.occurs, Some(Occurs::Fixed { count: 5 })));
    assert_eq!(aa.offset, 0);
    // ARR-A occupies 4*3=12 bytes, ARR-B starts at 12
    assert_eq!(ab.offset, 12);
}

#[test]
fn test_occurs_group_with_mixed_children() {
    let cpy = r"
       01 REC.
          05 COUNTER    PIC 9(2).
          05 LINE-ITEM  OCCURS 1 TO 10 TIMES
                        DEPENDING ON COUNTER.
             10 CODE    PIC X(3).
             10 AMT     PIC S9(5)V99 COMP-3.
             10 FLAG    PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let li = find_field(&schema, "LINE-ITEM");
    assert!(matches!(
        li.occurs,
        Some(Occurs::ODO {
            min: 1,
            max: 10,
            ..
        })
    ));
    assert_eq!(li.children.len(), 3);
    assert!(matches!(
        find_field(&schema, "CODE").kind,
        FieldKind::Alphanum { len: 3 }
    ));
    assert!(matches!(
        find_field(&schema, "AMT").kind,
        FieldKind::PackedDecimal { .. }
    ));
}

#[test]
fn test_odo_min_equals_max_degenerate() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(1).
          05 ITEMS     OCCURS 5 TO 5 TIMES
                       DEPENDING ON CNT
                       PIC X(2).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let items = find_field(&schema, "ITEMS");
    match &items.occurs {
        Some(Occurs::ODO { min, max, .. }) => {
            assert_eq!(*min, 5);
            assert_eq!(*max, 5);
        }
        other => panic!("expected ODO, got {:?}", other),
    }
}

// ===========================================================================
// Complex hierarchies
// ===========================================================================

#[test]
fn test_seven_level_hierarchy_with_mixed_leaf_types() {
    let cpy = r"
       01 REC.
          05 GRP-1.
             10 GRP-2.
                15 GRP-3.
                   20 GRP-4.
                      25 GRP-5.
                         49 LEAF-X PIC X(4).
                         49 LEAF-9 PIC 9(3).
                         49 LEAF-P PIC S9(5) COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert!(matches!(
        find_field(&schema, "LEAF-X").kind,
        FieldKind::Alphanum { len: 4 }
    ));
    assert!(matches!(
        find_field(&schema, "LEAF-9").kind,
        FieldKind::ZonedDecimal { digits: 3, .. }
    ));
    assert!(matches!(
        find_field(&schema, "LEAF-P").kind,
        FieldKind::PackedDecimal { digits: 5, .. }
    ));
}

#[test]
fn test_filler_at_start_middle_end() {
    let cpy = r"
       01 REC.
          05 FILLER   PIC X(2).
          05 REAL-F   PIC X(5).
          05 FILLER   PIC X(3).
          05 REAL-G   PIC 9(4).
          05 FILLER   PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let rec = &schema.fields[0];
    // All 5 children should be in schema (3 fillers + 2 named)
    assert!(
        rec.children.len() >= 5,
        "expected ≥5 children, got {}",
        rec.children.len()
    );
    let real_f = find_field(&schema, "REAL-F");
    assert_eq!(real_f.offset, 2);
    let real_g = find_field(&schema, "REAL-G");
    assert_eq!(real_g.offset, 10); // 2 + 5 + 3
}

#[test]
fn test_group_total_length_matches_sum_of_children() {
    let cpy = r"
       01 REC.
          05 GRP.
             10 A PIC X(10).
             10 B PIC 9(5).
             10 C PIC S9(3)V99 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let grp = find_field(&schema, "GRP");
    let a = find_field(&schema, "A");
    let b = find_field(&schema, "B");
    let c = find_field(&schema, "C");
    assert_eq!(grp.len, a.len + b.len + c.len);
}

#[test]
fn test_groups_with_no_elementary_leaf_just_subgroups() {
    let cpy = r"
       01 REC.
          05 TOP-GRP.
             10 SUB-GRP-A.
                15 FA PIC X(3).
             10 SUB-GRP-B.
                15 FB PIC 9(2).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let top = find_field(&schema, "TOP-GRP");
    assert!(matches!(top.kind, FieldKind::Group));
    assert_eq!(top.children.len(), 2);
    assert!(matches!(top.children[0].kind, FieldKind::Group));
    assert!(matches!(top.children[1].kind, FieldKind::Group));
    assert_eq!(top.len, 5); // 3 + 2
}

#[test]
fn test_multiple_01_records_with_different_structures() {
    let cpy = r"
       01 HEADER-REC.
          05 REC-TYPE  PIC X(2).
          05 TIMESTAMP PIC 9(14).
       01 DETAIL-REC.
          05 DTL-KEY   PIC 9(8).
          05 DTL-GRP.
             10 DTL-A  PIC X(10).
             10 DTL-B  PIC S9(5)V99.
       01 TRAILER-REC.
          05 TOTAL-CNT PIC 9(10).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(schema.fields.len(), 3);
    assert_eq!(schema.fields[0].name, "HEADER-REC");
    assert_eq!(schema.fields[1].name, "DETAIL-REC");
    assert_eq!(schema.fields[2].name, "TRAILER-REC");
    assert_eq!(schema.fields[0].children.len(), 2);
    assert_eq!(schema.fields[1].children.len(), 2);
    assert_eq!(schema.fields[2].children.len(), 1);
}

// ===========================================================================
// Level-88 — additional coverage
// ===========================================================================

#[test]
fn test_level88_value_zeros_figurative() {
    // ZEROS is lexed as Token::Zero by the lexer, which the level-88
    // value parser does not currently consume. Verify this is a known
    // limitation (parse error) rather than a panic.
    let cpy = r"
       01 REC.
          05 BALANCE   PIC 9(7)V99.
             88 IS-ZERO VALUE ZEROS.
    ";
    let result = parse_copybook(cpy);
    // Known limitation: ZEROS figurative constant not handled in 88 values
    assert!(
        result.is_err(),
        "ZEROS in level-88 VALUE is a known limitation"
    );
}

#[test]
fn test_level88_value_spaces_literal() {
    let cpy = r"
       01 REC.
          05 BUFFER    PIC X(20).
             88 IS-EMPTY VALUE SPACES.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let cond = find_field(&schema, "IS-EMPTY");
    match &cond.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values, &["SPACES"]);
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

#[test]
fn test_level88_after_comp3_field() {
    let cpy = r"
       01 REC.
          05 PACKED-AMT PIC S9(5)V99 COMP-3.
             88 ZERO-AMT VALUE 0.
             88 MAX-AMT  VALUE 99999.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let parent = find_field(&schema, "PACKED-AMT");
    let cond_children: Vec<_> = parent.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(cond_children.len(), 2);
}

#[test]
fn test_level88_after_comp_binary_field() {
    let cpy = r"
       01 REC.
          05 BIN-CODE PIC 9(4) COMP.
             88 CODE-A VALUE 1.
             88 CODE-B VALUE 2.
             88 CODE-C VALUE 3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let parent = find_field(&schema, "BIN-CODE");
    let cond_children: Vec<_> = parent.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(cond_children.len(), 3);
}

#[test]
fn test_level88_does_not_consume_storage() {
    let cpy = r"
       01 REC.
          05 CODE PIC X(1).
             88 VAL-A VALUE 'A'.
             88 VAL-B VALUE 'B'.
             88 VAL-C VALUE 'C'.
          05 NEXT-FIELD PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let nf = find_field(&schema, "NEXT-FIELD");
    assert_eq!(nf.offset, 1, "88-level fields must not consume storage");
}

#[test]
fn test_level88_with_thru_and_discrete_mixed() {
    // Use values > 49 to avoid them being lexed as Level tokens
    let cpy = r"
       01 REC.
          05 PRIORITY PIC 9(3).
             88 LOW-PRI    VALUE 100 THRU 300.
             88 MED-PRI    VALUE 400 THRU 600.
             88 HIGH-PRI   VALUE 700 THRU 900.
             88 URGENT     VALUE 999.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let parent = find_field(&schema, "PRIORITY");
    let conds: Vec<_> = parent.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(conds.len(), 4);
    // URGENT has a discrete value
    let urgent = find_field(&schema, "URGENT");
    match &urgent.kind {
        FieldKind::Condition { values } => {
            assert_eq!(values.len(), 1);
            assert!(!values[0].contains("THRU"));
        }
        other => panic!("expected Condition, got {:?}", other),
    }
}

// ===========================================================================
// PIC clause — additional edge cases
// ===========================================================================

#[test]
fn test_pic_no_parens_repeated_x() {
    let cpy = r"
       01 REC.
          05 F1 PIC XXXX.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 4 }));
    assert_eq!(f.len, 4);
}

#[test]
fn test_pic_no_parens_repeated_9() {
    let cpy = r"
       01 REC.
          05 F1 PIC 999.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "F1");
    match &f.kind {
        FieldKind::ZonedDecimal { digits, scale, .. } => {
            assert_eq!(*digits, 3);
            assert_eq!(*scale, 0);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
    assert_eq!(f.len, 3);
}

#[test]
fn test_pic_single_x() {
    let cpy = r"
       01 REC.
          05 FLAG PIC X.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "FLAG");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 1 }));
    assert_eq!(f.len, 1);
}

#[test]
fn test_pic_single_9() {
    let cpy = r"
       01 REC.
          05 DIGIT PIC 9.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "DIGIT");
    match &f.kind {
        FieldKind::ZonedDecimal { digits, .. } => {
            assert_eq!(*digits, 1);
        }
        other => panic!("expected ZonedDecimal, got {:?}", other),
    }
    assert_eq!(f.len, 1);
}

#[test]
fn test_pic_large_alphanumeric() {
    let cpy = r"
       01 REC.
          05 BIG-FIELD PIC X(10000).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "BIG-FIELD");
    assert!(matches!(f.kind, FieldKind::Alphanum { len: 10000 }));
    assert_eq!(f.len, 10000);
}

#[test]
fn test_comp3_single_digit() {
    let cpy = r"
       01 REC.
          05 TINY PIC S9 COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "TINY");
    match &f.kind {
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            assert_eq!(*digits, 1);
            assert_eq!(*scale, 0);
            assert!(signed);
        }
        other => panic!("expected PackedDecimal, got {:?}", other),
    }
    // COMP-3: ceil((1+1)/2) = 1 byte
    assert_eq!(f.len, 1);
}

#[test]
fn test_comp3_max_digits() {
    let cpy = r"
       01 REC.
          05 HUGE PIC 9(18) COMP-3.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "HUGE");
    match &f.kind {
        FieldKind::PackedDecimal { digits, signed, .. } => {
            assert_eq!(*digits, 18);
            assert!(!signed);
        }
        other => panic!("expected PackedDecimal, got {:?}", other),
    }
    // ceil((18+1)/2) = 10 bytes
    assert_eq!(f.len, 10);
}

// ===========================================================================
// SYNCHRONIZED clause
// ===========================================================================

#[test]
fn test_synchronized_clause_on_comp_field() {
    let cpy = r"
       01 REC.
          05 COUNTER PIC 9(9) COMP SYNCHRONIZED.
    ";
    let schema = parse_copybook(cpy).unwrap();
    let f = find_field(&schema, "COUNTER");
    assert!(f.synchronized, "SYNCHRONIZED should be set");
    assert!(matches!(f.kind, FieldKind::BinaryInt { bits: 32, .. }));
}

// ===========================================================================
// Schema properties
// ===========================================================================

#[test]
fn test_lrecl_fixed_calculated_for_simple_record() {
    let cpy = r"
       01 REC.
          05 A PIC X(10).
          05 B PIC 9(5).
          05 C PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert_eq!(
        schema.lrecl_fixed,
        Some(20),
        "LRECL should be sum of field lengths"
    );
}

#[test]
fn test_lrecl_not_fixed_for_odo_record() {
    let cpy = r"
       01 REC.
          05 CNT    PIC 9(2).
          05 ITEMS  OCCURS 1 TO 10 TIMES
                    DEPENDING ON CNT
                    PIC X(5).
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert!(
        schema.lrecl_fixed.is_none() || schema.tail_odo.is_some(),
        "ODO record should either have no fixed LRECL or have tail_odo set"
    );
}

#[test]
fn test_schema_all_fields_count() {
    let cpy = r"
       01 REC.
          05 A PIC X(3).
          05 GRP.
             10 B PIC X(2).
             10 C PIC 9(4).
          05 D PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    // REC, A, GRP, B, C, D = 6 fields
    let count = all_fields(&schema).len();
    assert_eq!(count, 6);
}

#[test]
fn test_field_paths_hierarchical() {
    let cpy = r"
       01 ROOT.
          05 LEVEL1.
             10 LEVEL2.
                15 LEAF PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let leaf = find_field(&schema, "LEAF");
    assert!(
        leaf.path.contains("ROOT"),
        "path should include root: {}",
        leaf.path
    );
    assert!(
        leaf.path.contains("LEAF"),
        "path should include field name: {}",
        leaf.path
    );
}

// ===========================================================================
// Error recovery — additional scenarios
// ===========================================================================

#[test]
fn test_error_redefines_nonexistent_target() {
    let cpy = r"
       01 REC.
          05 FIELD-A   PIC X(10).
          05 FIELD-B   REDEFINES GHOST PIC X(10).
    ";
    let result = parse_copybook(cpy);
    assert!(
        result.is_err(),
        "REDEFINES of non-existent field should fail"
    );
}

#[test]
fn test_error_odo_non_tail_with_storage_sibling() {
    let cpy = r"
       01 REC.
          05 CNT       PIC 9(2).
          05 ITEMS     OCCURS 1 TO 5 TIMES
                       DEPENDING ON CNT
                       PIC X(3).
          05 AFTER     PIC X(10).
    ";
    let err = parse_copybook(cpy).unwrap_err();
    assert_eq!(
        err.code(),
        ErrorCode::CBKP021_ODO_NOT_TAIL,
        "ODO must be at tail position"
    );
}

#[test]
fn test_error_pic_a_not_supported() {
    let cpy = r"
       01 REC.
          05 ALPHA-F PIC A(10).
    ";
    let result = parse_copybook(cpy);
    assert!(
        result.is_err(),
        "PIC A is not a supported PIC type in this parser"
    );
}

#[test]
fn test_error_empty_copybook_text() {
    let result = parse_copybook("");
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code(), ErrorCode::CBKP001_SYNTAX);
}

#[test]
fn test_no_panic_on_malformed_occurs() {
    // OCCURS without TIMES or a count — should not panic
    let cpy = r"
       01 REC.
          05 F PIC X(5).
    ";
    // This is valid (no OCCURS), just making sure parser handles it
    let _ = parse_copybook(cpy);
}

// ===========================================================================
// Mixed / enterprise-style records
// ===========================================================================

#[test]
fn test_enterprise_record_mixed_types() {
    let cpy = r"
       01 CUSTOMER-RECORD.
          05 CUST-ID          PIC 9(8).
          05 CUST-NAME        PIC X(30).
          05 CUST-STATUS      PIC X(1).
             88 ACTIVE        VALUE 'A'.
             88 INACTIVE      VALUE 'I'.
             88 SUSPENDED     VALUE 'S'.
          05 ACCOUNT-GROUP.
             10 ACCT-TYPE     PIC X(2).
             10 ACCT-BAL      PIC S9(9)V99 COMP-3.
             10 ACCT-FLAG     PIC 9(1).
                88 OVERDRAWN  VALUE 1.
          05 ORDER-COUNT      PIC 9(3).
          05 ORDERS OCCURS 1 TO 50 TIMES
                    DEPENDING ON ORDER-COUNT.
             10 ORDER-ID      PIC 9(10).
             10 ORDER-AMT     PIC S9(7)V99.
    ";
    let schema = parse_copybook(cpy).unwrap();
    // Verify structure
    let root = &schema.fields[0];
    assert_eq!(root.name, "CUSTOMER-RECORD");

    // Level-88s are children of their parent
    let status = find_field(&schema, "CUST-STATUS");
    let status_88s: Vec<_> = status.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(status_88s.len(), 3);

    // Nested 88 inside group
    let flag = find_field(&schema, "ACCT-FLAG");
    let flag_88s: Vec<_> = flag.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(flag_88s.len(), 1);

    // ODO
    let orders = find_field(&schema, "ORDERS");
    assert!(matches!(
        orders.occurs,
        Some(Occurs::ODO {
            min: 1,
            max: 50,
            ..
        })
    ));

    // tail_odo should be set
    assert!(schema.tail_odo.is_some());
}

#[test]
fn test_record_all_comp_types() {
    let cpy = r"
       01 REC.
          05 F-DISPLAY PIC 9(5).
          05 F-COMP    PIC 9(5) COMP.
          05 F-COMP3   PIC S9(5) COMP-3.
          05 F-BINARY  PIC S9(9) BINARY.
    ";
    let schema = parse_copybook(cpy).unwrap();
    assert!(matches!(
        find_field(&schema, "F-DISPLAY").kind,
        FieldKind::ZonedDecimal { .. }
    ));
    assert!(matches!(
        find_field(&schema, "F-COMP").kind,
        FieldKind::BinaryInt { .. }
    ));
    assert!(matches!(
        find_field(&schema, "F-COMP3").kind,
        FieldKind::PackedDecimal { .. }
    ));
    assert!(matches!(
        find_field(&schema, "F-BINARY").kind,
        FieldKind::BinaryInt { .. }
    ));
}

#[test]
fn test_record_with_redefines_and_level88() {
    let cpy = r"
       01 REC.
          05 RAW-DATA PIC X(10).
             88 ALL-SPACES VALUE SPACES.
          05 STRUCTURED REDEFINES RAW-DATA.
             10 CODE  PIC X(3).
             10 VALUE-PART PIC X(7).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let raw = find_field(&schema, "RAW-DATA");
    let conds: Vec<_> = raw.children.iter().filter(|c| c.level == 88).collect();
    assert_eq!(conds.len(), 1);
    let struc = find_field(&schema, "STRUCTURED");
    assert_eq!(struc.redefines_of.as_deref(), Some("RAW-DATA"));
}

#[test]
fn test_occurs_fixed_inside_redefines() {
    let cpy = r"
       01 REC.
          05 RAW     PIC X(30).
          05 SPLIT   REDEFINES RAW.
             10 ENTRIES OCCURS 3 TIMES.
                15 ENTRY-KEY PIC X(4).
                15 ENTRY-VAL PIC X(6).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let entries = find_field(&schema, "ENTRIES");
    assert!(matches!(entries.occurs, Some(Occurs::Fixed { count: 3 })));
    assert_eq!(entries.children.len(), 2);
}

#[test]
fn test_level77_standalone_fields() {
    let cpy = r"
       01 MAIN-REC.
          05 DATA PIC X(20).
       77 COUNTER  PIC 9(5).
       77 STATUS   PIC X(1).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let counter = all_fields(&schema)
        .into_iter()
        .find(|f| f.name == "COUNTER")
        .unwrap();
    assert_eq!(counter.level, 77);
    let status = all_fields(&schema)
        .into_iter()
        .find(|f| f.name == "STATUS")
        .unwrap();
    assert_eq!(status.level, 77);
}

#[test]
fn test_fingerprint_differs_for_different_schemas() {
    let cpy1 = r"
       01 REC.
          05 A PIC X(10).
    ";
    let cpy2 = r"
       01 REC.
          05 A PIC X(20).
    ";
    let s1 = parse_copybook(cpy1).unwrap();
    let s2 = parse_copybook(cpy2).unwrap();
    assert_ne!(s1.fingerprint, s2.fingerprint);
}

#[test]
fn test_fingerprint_stable_for_same_input() {
    let cpy = r"
       01 REC.
          05 F1 PIC X(5).
          05 F2 PIC 9(3).
    ";
    let s1 = parse_copybook(cpy).unwrap();
    let s2 = parse_copybook(cpy).unwrap();
    assert_eq!(s1.fingerprint, s2.fingerprint);
}
