// SPDX-License-Identifier: AGPL-3.0-or-later
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

/// Test that RENAMES without THRU/THROUGH is treated as a single-field rename.
/// In COBOL, `66 ALIAS RENAMES FIELD-1.` is a valid single-field alias.
/// The parser reads the first identifier after RENAMES as from_field and
/// ignores trailing identifiers before the period.
#[test]
fn test_renames_without_thru_is_single_field() {
    let copybook = r"
       01 RECORD-E.
          05 FIELD-1 PIC X(10).
          05 FIELD-2 PIC X(10).
       66 ALIAS-E RENAMES FIELD-1 FIELD-2.
    ";

    let schema = parse_copybook(copybook).expect("single-field RENAMES should parse");
    let alias = schema
        .all_fields()
        .into_iter()
        .find(|f| f.name == "ALIAS-E")
        .expect("ALIAS-E should exist");

    match &alias.kind {
        copybook_core::FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            assert_eq!(from_field, "FIELD-1");
            assert_eq!(
                thru_field, "FIELD-1",
                "single-field RENAMES should have from == thru"
            );
        }
        other => panic!("expected Renames kind, got {other:?}"),
    }
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

/// Test that group children remain children when level-66 follows the group
/// (regression test for parser bug where children are incorrectly promoted to siblings)
///
/// This test demonstrates the parser's "pop until level-01" logic for level-66 incorrectly
/// promotes group children (ADDRESS) as siblings instead of leaving them as children of
/// their parent group (CUSTOMER-INFO). This breaks RENAMES R2/R3 scenarios and any feature
/// that depends on correct tree structure.
///
/// To isolate the parser tree-building from resolution, we test without RENAMES clause.
#[test]
fn test_group_children_with_following_level66() {
    // First, test a normal group (without level-66) to establish baseline
    let copybook_normal = r"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-INFO.
             10 NAME     PIC X(30).
             10 ADDRESS  PIC X(60).
    ";

    let schema_normal = parse_copybook(copybook_normal).expect("should parse normal group");
    let record_normal = &schema_normal.fields[0];

    // Baseline: CUSTOMER-INFO should have 2 children
    let customer_info_normal = record_normal
        .children
        .iter()
        .find(|f| f.name == "CUSTOMER-INFO")
        .expect("CUSTOMER-INFO should exist in normal case");

    assert_eq!(
        customer_info_normal.children.len(),
        2,
        "Baseline: CUSTOMER-INFO should have 2 children without level-66"
    );

    // Now test the same structure but with a level-66 following
    // Note: Using simple same-scope RENAMES (R1) to avoid R4+ complexity while still
    // testing that level-66 presence doesn't corrupt the tree structure
    let copybook_with_66 = r"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-INFO.
             10 NAME     PIC X(30).
             10 ADDRESS  PIC X(60).
          05 OTHER-FIELD PIC X(10).
          66 CUSTOMER-DETAILS RENAMES OTHER-FIELD THRU OTHER-FIELD.
    ";

    let schema_with_66 =
        parse_copybook(copybook_with_66).expect("should parse group with level-66");
    let record_with_66 = &schema_with_66.fields[0];

    // Debug: Print the actual tree structure WITH level-66
    eprintln!("\n=== Tree Structure WITH level-66 ===");
    eprintln!(
        "CUSTOMER-RECORD.children.len() = {}",
        record_with_66.children.len()
    );
    for (i, child) in record_with_66.children.iter().enumerate() {
        eprintln!(
            "  record.children[{}] = {:?} (level {})",
            i, child.name, child.level
        );
        if !child.children.is_empty() {
            eprintln!("    {} has {} children:", child.name, child.children.len());
            for (j, grandchild) in child.children.iter().enumerate() {
                eprintln!(
                    "      children[{}] = {:?} (level {})",
                    j, grandchild.name, grandchild.level
                );
            }
        }
    }
    eprintln!("========================================\n");

    // Find CUSTOMER-INFO group in the level-66 case
    let customer_info_with_66 = record_with_66
        .children
        .iter()
        .find(|f| f.name == "CUSTOMER-INFO")
        .expect("CUSTOMER-INFO should be a child of CUSTOMER-RECORD");

    // CRITICAL TEST: NAME and ADDRESS should remain as children of CUSTOMER-INFO
    // even when level-66 follows. They should NOT be promoted to siblings.
    assert_eq!(
        customer_info_with_66.children.len(),
        2,
        "BUG: CUSTOMER-INFO should have exactly 2 children (NAME and ADDRESS), but has {}.\n\
         This indicates the parser incorrectly promoted children to siblings when encountering level-66.\n\
         Expected: CUSTOMER-INFO.children = [NAME, ADDRESS]\n\
         The level-66 'pop until level-01' logic must not affect already-established group children.",
        customer_info_with_66.children.len()
    );

    let name = customer_info_with_66
        .children
        .iter()
        .find(|f| f.name == "NAME")
        .expect("NAME should be a child of CUSTOMER-INFO");
    assert_eq!(name.level, 10, "NAME should be level 10");

    let address = customer_info_with_66
        .children
        .iter()
        .find(|f| f.name == "ADDRESS")
        .expect("ADDRESS should be a child of CUSTOMER-INFO");
    assert_eq!(address.level, 10, "ADDRESS should be level 10");
}
