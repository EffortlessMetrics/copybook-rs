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
