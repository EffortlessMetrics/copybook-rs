// SPDX-License-Identifier: AGPL-3.0-or-later
//! Phase E1: Edited PIC comprehensive tests
//!
//! Tests for edited PIC parsing (E1 phase) and codec rejection.
//! Edited PIC clauses should now parse successfully into schema
//! but reject during decode/encode operations.
//!
//! Note: Current lexer limitations mean complex decimal patterns are not yet supported.
//! Tests focus on basic edited patterns that work with current tokenization.

use copybook_core::{ErrorCode, FieldKind, parse_copybook};

/// Test E1.1: Zero suppression (Z) parses successfully
#[test]
fn test_e1_zero_suppression_parses() {
    let copybook = "01 REC.\n   05 AMOUNT PIC ZZZZ.";
    let schema = parse_copybook(copybook).unwrap();

    let amount = &schema.fields[0].children[0];
    assert_eq!(amount.name, "AMOUNT");

    // Check that it's EditedNumeric
    if let FieldKind::EditedNumeric {
        pic_string, width, ..
    } = &amount.kind
    {
        assert_eq!(pic_string, "ZZZZ");
        assert_eq!(*width, 4);
    } else {
        panic!("Expected EditedNumeric, got {:?}", amount.kind);
    }
}

/// Test E1.2: Currency symbol ($) parses successfully
#[test]
fn test_e1_currency_symbol_parses() {
    let copybook = "01 REC.\n   05 PRICE PIC $ZZZ.";
    let schema = parse_copybook(copybook).unwrap();

    let price = &schema.fields[0].children[0];
    if let FieldKind::EditedNumeric {
        pic_string, width, ..
    } = &price.kind
    {
        assert_eq!(pic_string, "$ZZZ");
        assert_eq!(*width, 4); // $ + 3Z
    } else {
        panic!("Expected EditedNumeric, got {:?}", price.kind);
    }
}

/// Test E1.3: Asterisk fill (*) parses successfully
#[test]
fn test_e1_asterisk_fill_parses() {
    let copybook = "01 REC.\n   05 CHECK-AMT PIC ****9.";
    let schema = parse_copybook(copybook).unwrap();

    let check_amt = &schema.fields[0].children[0];
    if let FieldKind::EditedNumeric {
        pic_string, width, ..
    } = &check_amt.kind
    {
        assert_eq!(pic_string, "****9");
        assert_eq!(*width, 5);
    } else {
        panic!("Expected EditedNumeric, got {:?}", check_amt.kind);
    }
}

/// Test E1.4: SIGN clause rejected as unsupported edited PIC
#[test]
fn test_e1_sign_clause_without_separate_rejected() {
    // SIGN LEADING without SEPARATE is a syntax error (overpunching is handled by S in PIC)
    // SIGN SEPARATE is always-enabled, so the feature-flag path is no longer hit;
    // instead the parser validates that SEPARATE must be present.
    let copybook = "01 REC.\n   05 SIGNED-AMT PIC S9(5) SIGN LEADING.";
    let Err(error) = parse_copybook(copybook) else {
        panic!("SIGN clause without SEPARATE should be rejected");
    };
    assert_eq!(error.code, ErrorCode::CBKP001_SYNTAX);
}

/// Test E1.5: Verify edited PIC fields have storage in schema
#[test]
fn test_e1_edited_pic_has_storage() {
    let copybook = "01 REC.\n   05 BEFORE PIC X(5).\n   05 EDITED PIC ZZZZ.\n   05 AFTER PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();

    let rec = &schema.fields[0];
    assert_eq!(rec.children.len(), 3);

    let before = &rec.children[0];
    let edited = &rec.children[1];
    let after = &rec.children[2];

    // Check offsets: BEFORE at 0, EDITED at 5, AFTER at 9
    assert_eq!(before.offset, 0);
    assert_eq!(before.len, 5);

    assert_eq!(edited.offset, 5);
    assert_eq!(edited.len, 4); // Width of ZZZZ

    assert_eq!(after.offset, 9);
    assert_eq!(after.len, 5);

    // Check total LRECL
    assert_eq!(schema.lrecl_fixed, Some(14)); // 5 + 4 + 5
}

/// Test E1.6: Edited PIC in OCCURS context
#[test]
fn test_e1_edited_pic_in_occurs() {
    let copybook = "01 REC.\n   05 AMOUNTS OCCURS 3 TIMES.\n      10 AMT PIC ZZZZ.";
    let schema = parse_copybook(copybook).unwrap();

    let amounts = &schema.fields[0].children[0];
    assert!(amounts.occurs.is_some());

    let amt = &amounts.children[0];
    assert!(matches!(amt.kind, FieldKind::EditedNumeric { .. }));

    // Check that OCCURS is calculated correctly
    assert_eq!(amounts.len, 4); // Base size of ZZZZ
    assert_eq!(schema.lrecl_fixed, Some(12)); // 4 * 3
}

/// Test E1.7: Multiple edited fields in same record
#[test]
fn test_e1_multiple_edited_fields() {
    let copybook = r#"
01 TRANSACTION.
   05 AMT1 PIC ZZZ.
   05 AMT2 PIC $ZZ.
   05 AMT3 PIC **9.
"#;
    let schema = parse_copybook(copybook).unwrap();

    let transaction = &schema.fields[0];
    assert_eq!(transaction.children.len(), 3);

    // All three should be EditedNumeric
    for child in &transaction.children {
        assert!(
            matches!(child.kind, FieldKind::EditedNumeric { .. }),
            "Field {} should be EditedNumeric",
            child.name
        );
    }

    // Check cumulative offsets
    let amt1 = &transaction.children[0];
    let amt2 = &transaction.children[1];
    let amt3 = &transaction.children[2];

    assert_eq!(amt1.offset, 0);
    assert_eq!(amt1.len, 3); // ZZZ

    assert_eq!(amt2.offset, 3);
    assert_eq!(amt2.len, 3); // $ZZ

    assert_eq!(amt3.offset, 6);
    assert_eq!(amt3.len, 3); // **9

    assert_eq!(schema.lrecl_fixed, Some(9)); // 3 + 3 + 3
}

/// Test E1.8: Z with repetition count
#[test]
fn test_e1_z_with_repetition() {
    let copybook = "01 REC.\n   05 AMOUNT PIC Z(5).";
    let schema = parse_copybook(copybook).unwrap();

    let amount = &schema.fields[0].children[0];
    if let FieldKind::EditedNumeric {
        pic_string, width, ..
    } = &amount.kind
    {
        assert_eq!(pic_string, "Z(5)");
        assert_eq!(*width, 5);
    } else {
        panic!("Expected EditedNumeric, got {:?}", amount.kind);
    }
}

/// Test E1.9: Plus/minus sign editing
#[test]
fn test_e1_plus_minus_signs() {
    let copybook1 = "01 REC.\n   05 SIGNED PIC +999.";
    let schema1 = parse_copybook(copybook1).unwrap();

    let signed1 = &schema1.fields[0].children[0];
    assert!(matches!(signed1.kind, FieldKind::EditedNumeric { .. }));

    let copybook2 = "01 REC.\n   05 SIGNED PIC -999.";
    let schema2 = parse_copybook(copybook2).unwrap();

    let signed2 = &schema2.fields[0].children[0];
    assert!(matches!(signed2.kind, FieldKind::EditedNumeric { .. }));
}

/// Test E1.10: Schema fingerprint includes edited PIC
#[test]
fn test_e1_schema_fingerprint_includes_edited() {
    let copybook = "01 REC.\n   05 EDITED PIC ZZZ.";
    let schema = parse_copybook(copybook).unwrap();

    // Fingerprint should be non-empty and consistent
    assert!(!schema.fingerprint.is_empty());

    // Parse again and ensure fingerprint is identical
    let schema2 = parse_copybook(copybook).unwrap();
    assert_eq!(schema.fingerprint, schema2.fingerprint);
}
