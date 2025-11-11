//! Tests for level-66 RENAMES parsing (Issue #122, Slice-1)
//!
//! This test suite validates:
//! - Basic RENAMES syntax parsing (66 NAME RENAMES FROM THRU/THROUGH TO .)
//! - Both THRU and THROUGH keyword variants
//! - Proper error handling for invalid RENAMES syntax
//!
//! Note: Resolver/projection logic for RENAMES is deferred to Slice-2.
//! These tests only verify that the parser accepts valid RENAMES syntax
//! and produces appropriate AST nodes.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::panic)]
#![allow(clippy::single_char_pattern)]

use copybook_core::{parse_copybook, schema::FieldKind};

/// Test basic RENAMES with THRU keyword
#[test]
fn test_basic_renames_with_thru() {
    let copybook = r"
       01 RECORD-A.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC 9(5).
          05 FIELD-3 PIC X(20).
       66 ALIAS-A RENAMES FIELD-1 THRU FIELD-3.
    ";

    let schema = parse_copybook(copybook).expect("should parse valid RENAMES");

    // Find the RENAMES field
    let alias_field = schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == "ALIAS-A")
        .expect("ALIAS-A field should exist");

    assert_eq!(alias_field.level, 66);
    match &alias_field.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FIELD-1");
            assert_eq!(thru_field, "FIELD-3");
        }
        _ => panic!("Expected Renames field kind, got {:?}", alias_field.kind),
    }
}

/// Test basic RENAMES with THROUGH keyword (synonym for THRU)
#[test]
fn test_basic_renames_with_through() {
    let copybook = r"
       01 RECORD-B.
          05 FIELD-X PIC X(10).
          05 FIELD-Y PIC 9(5).
          05 FIELD-Z PIC X(20).
       66 ALIAS-B RENAMES FIELD-X THROUGH FIELD-Z.
    ";

    let schema = parse_copybook(copybook).expect("should parse RENAMES with THROUGH");

    let alias_field = schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == "ALIAS-B")
        .expect("ALIAS-B field should exist");

    assert_eq!(alias_field.level, 66);
    match &alias_field.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FIELD-X");
            assert_eq!(thru_field, "FIELD-Z");
        }
        _ => panic!("Expected Renames field kind"),
    }
}

/// Test RENAMES at different positions in copybook
#[test]
fn test_renames_positioning() {
    let copybook = r"
       01 RECORD-C.
          05 PART-1 PIC X(10).
          05 PART-2 PIC X(10).
          05 PART-3 PIC X(10).
       66 FIRST-HALF RENAMES PART-1 THRU PART-2.
       66 SECOND-HALF RENAMES PART-2 THRU PART-3.
       66 FULL-RECORD RENAMES PART-1 THRU PART-3.
    ";

    let schema = parse_copybook(copybook).expect("should parse multiple RENAMES");

    let renames_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| matches!(f.kind, FieldKind::Renames { .. }))
        .collect();

    assert_eq!(renames_fields.len(), 3, "should have 3 RENAMES fields");
}

/// Test RENAMES with hyphenated field names
#[test]
fn test_renames_with_hyphenated_names() {
    let copybook = r"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID PIC 9(10).
          05 CUSTOMER-NAME PIC X(50).
          05 CUSTOMER-ADDRESS PIC X(100).
       66 CUSTOMER-HEADER RENAMES CUSTOMER-ID THRU CUSTOMER-NAME.
    ";

    let schema = parse_copybook(copybook).expect("should parse RENAMES with hyphens");

    let header_field = schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == "CUSTOMER-HEADER")
        .expect("CUSTOMER-HEADER field should exist");

    match &header_field.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "CUSTOMER-ID");
            assert_eq!(thru_field, "CUSTOMER-NAME");
        }
        _ => panic!("Expected Renames field kind"),
    }
}

/// Test that level-66 is required for RENAMES
#[test]
fn test_renames_requires_level_66() {
    let copybook = r"
       01 RECORD-D.
          05 FIELD-A PIC X(10).
          05 FIELD-B PIC X(10).
       05 WRONG-LEVEL RENAMES FIELD-A THRU FIELD-B.
    ";

    let result = parse_copybook(copybook);
    assert!(result.is_err(), "should reject RENAMES on non-66 level");

    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("level-66") || err_msg.contains("66"),
        "error should mention level-66 requirement"
    );
}

/// Test that RENAMES requires THRU or THROUGH keyword
#[test]
fn test_renames_requires_thru_keyword() {
    let copybook = r"
       01 RECORD-E.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC X(10).
       66 ALIAS-E RENAMES FIELD-1 FIELD-2.
    ";

    let result = parse_copybook(copybook);
    assert!(
        result.is_err(),
        "should reject RENAMES without THRU/THROUGH"
    );

    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("THRU") || err_msg.contains("THROUGH"),
        "error should mention THRU/THROUGH keyword"
    );
}

/// Test that RENAMES requires field name after RENAMES keyword
#[test]
fn test_renames_requires_from_field() {
    let copybook = r"
       01 RECORD-F.
          05 FIELD-X PIC X(10).
       66 ALIAS-F RENAMES THRU FIELD-X.
    ";

    let result = parse_copybook(copybook);
    assert!(result.is_err(), "should reject RENAMES without from-field");

    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("field name") || err_msg.contains("Expected"),
        "error should indicate missing field name"
    );
}

/// Test that RENAMES requires field name after THRU keyword
#[test]
fn test_renames_requires_thru_field() {
    let copybook = r"
       01 RECORD-G.
          05 FIELD-Y PIC X(10).
       66 ALIAS-G RENAMES FIELD-Y THRU.
    ";

    let result = parse_copybook(copybook);
    assert!(result.is_err(), "should reject RENAMES without thru-field");
}

/// Test that RENAMES requires period terminator
#[test]
fn test_renames_requires_period() {
    let copybook = r"
       01 RECORD-H.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC X(10).
       66 ALIAS-H RENAMES FIELD-1 THRU FIELD-2
    ";

    let result = parse_copybook(copybook);
    assert!(result.is_err(), "should reject RENAMES without period");

    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("period") || err_msg.contains("."),
        "error should mention missing period"
    );
}

/// Test RENAMES with numeric field names
#[test]
fn test_renames_with_numeric_names() {
    let copybook = r"
       01 REC1.
          05 FLD1 PIC 9(5).
          05 FLD2 PIC 9(5).
          05 FLD3 PIC 9(5).
       66 ALIAS1 RENAMES FLD1 THRU FLD3.
    ";

    let schema = parse_copybook(copybook).expect("should parse RENAMES with numeric names");

    let alias_field = schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == "ALIAS1")
        .expect("ALIAS1 field should exist");

    match &alias_field.kind {
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FLD1");
            assert_eq!(thru_field, "FLD3");
        }
        _ => panic!("Expected Renames field kind"),
    }
}

/// Test that RENAMES keyword is required after level-66 name
#[test]
fn test_renames_keyword_required() {
    let copybook = r"
       01 RECORD-I.
          05 FIELD-A PIC X(10).
          05 FIELD-B PIC X(10).
       66 ALIAS-I FIELD-A THRU FIELD-B.
    ";

    let result = parse_copybook(copybook);
    assert!(
        result.is_err(),
        "should reject level-66 without RENAMES keyword"
    );

    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("RENAMES"),
        "error should mention RENAMES keyword requirement"
    );
}
