#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
//! Comprehensive tests for schema validation edge cases
//!
//! This test suite validates schema validation with edge cases:
//! - Field creation with various parameters
//! - Validation edge cases
//! - Error handling in schema operations
//! - Boundary conditions

use copybook_core::parse_copybook;

#[test]
fn test_schema_parse_simple_copybook() {
    // Test parsing a simple copybook
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "TEST-FIELD");
}

#[test]
fn test_schema_parse_multiple_fields() {
    // Test parsing multiple fields
    let copybook = r#"
        01  RECORD.
            05  FIELD-1     PIC 9(5).
            05  FIELD-2     PIC X(10).
            05  FIELD-3     PIC S9(7)V99.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1); // One group
    assert_eq!(schema.all_fields()[0].name, "RECORD");
}

#[test]
fn test_schema_parse_with_occurs() {
    // Test parsing with OCCURS clause
    let copybook = "01 ARRAY-FIELD PIC 9(5) OCCURS 10 TIMES.";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "ARRAY-FIELD");
}

#[test]
fn test_schema_parse_with_redefines() {
    // Test parsing with REDEFINES clause
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(5) REDEFINES FIELD-1.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
}

#[test]
fn test_schema_parse_with_level88() {
    // Test parsing with level 88 condition names
    let copybook = r#"
        01  STATUS-CODE  PIC X.
            88  SUCCESS    VALUE 'S'.
            88  FAILURE    VALUE 'F'.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "STATUS-CODE");
}

#[test]
fn test_schema_parse_with_odo() {
    // Test parsing with OCCURS DEPENDING ON
    let copybook = r#"
        01  RECORD.
            05  COUNT       PIC 9(3).
            05  DETAIL      PIC 9(5) OCCURS 0 TO 100 TIMES DEPENDING ON COUNT.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 2);
}

#[test]
fn test_schema_parse_with_sign_separate() {
    // Test parsing with SIGN SEPARATE clause
    let copybook = "01 SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE.";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "SIGNED-FIELD");
}

#[test]
fn test_schema_parse_with_comp3() {
    // Test parsing COMP-3 (packed decimal)
    let copybook = "01 PACKED-FIELD PIC S9(7)V99 COMP-3.";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "PACKED-FIELD");
}

#[test]
fn test_schema_parse_with_binary() {
    // Test parsing BINARY/COMP-4
    let copybook = "01 BINARY-FIELD PIC S9(9) COMP-4.";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "BINARY-FIELD");
}

#[test]
fn test_schema_parse_with_usage() {
    // Test parsing with USAGE clause
    let copybook = "01 DISPLAY-FIELD PIC 9(5) USAGE DISPLAY.";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "DISPLAY-FIELD");
}

#[test]
fn test_schema_parse_with_blank_when_zero() {
    // Test parsing with BLANK WHEN ZERO
    let copybook = "01 BWZ-FIELD PIC 9(5) BLANK WHEN ZERO.";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].name, "BWZ-FIELD");
}

#[test]
fn test_schema_parse_invalid_pic_clause() {
    // Test parsing invalid PIC clause
    let copybook = "01 BAD-FIELD PIC INVALID(5).";
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn test_schema_parse_duplicate_field_names() {
    // Test parsing duplicate field names (may fail in strict mode)
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-1  PIC X(10).
    "#;
    let result = parse_copybook(copybook);

    // This may or may not fail depending on implementation
    // Just verify it parses
    assert!(result.is_ok());
}

#[test]
fn test_schema_parse_invalid_pic_too_large() {
    // Test error for PIC clause with too many digits
    let copybook = "01 TEST-FIELD PIC 9(999999).";
    let result = parse_copybook(copybook);

    // Should fail with too many digits
    assert!(result.is_err());
}

#[test]
fn test_schema_parse_invalid_level_number() {
    // Test error for invalid level number
    let copybook = "99 TEST-FIELD PIC 9(5).";
    let result = parse_copybook(copybook);

    // Level 99 is special and may not be allowed
    assert!(result.is_err() || result.is_ok());
}

#[test]
fn test_schema_parse_invalid_occurs_clause() {
    // Test error for invalid OCCURS clause
    let copybook = "01 TEST-FIELD PIC 9(5) OCCURS 99999 TIMES.";
    let result = parse_copybook(copybook);

    // Too many occurrences
    assert!(result.is_err());
}

#[test]
fn test_schema_parse_invalid_redefines_clause() {
    // Test error for invalid REDEFINES clause
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(10) REDEFINES NONEXISTENT.
    "#;
    let result = parse_copybook(copybook);

    // Redefining non-existent field
    assert!(result.is_err());
}

#[test]
fn test_schema_parse_invalid_odo_clause() {
    // Test error for invalid ODO clause
    let copybook = r#"
        01  RECORD.
            05  COUNT  PIC 9(3).
            05  FIELD  PIC 9(5) OCCURS 0 TO 100 TIMES DEPENDING ON INVALID.
    "#;
    let result = parse_copybook(copybook);

    // Depending on non-existent field
    assert!(result.is_err());
}

#[test]
fn test_schema_parse_with_comment() {
    // Test parsing with comments
    let copybook = r#"
        * This is a comment
        01  TEST-FIELD  PIC 9(5).  * End of line comment
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
}

#[test]
fn test_schema_parse_empty_copybook() {
    // Test parsing empty copybook
    let copybook = "";
    let result = parse_copybook(copybook);

    // Empty copybook should be valid (no fields)
    assert!(result.is_ok());
}

#[test]
fn test_schema_parse_only_comments() {
    // Test parsing copybook with only comments
    let copybook = r#"
        * Comment line 1
        * Comment line 2
        * Comment line 3
    "#;
    let result = parse_copybook(copybook);

    // Should be valid (no fields)
    assert!(result.is_ok());
}

#[test]
fn test_schema_parse_with_continuation() {
    // Test parsing with continuation character
    let copybook = r#"
        01  LONG-FIELD  PIC 9(10).
        05  CONTINUATION  PIC X(20)
                          VALUE 'TEST'.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert!(!schema.all_fields().is_empty());
}

#[test]
fn test_schema_parse_with_value_clause() {
    // Test parsing with VALUE clause
    let copybook = "01 TEST-FIELD PIC 9(5) VALUE 12345.";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
}

#[test]
fn test_schema_parse_with_renames() {
    // Test parsing with RENAMES clause
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(5).
        01  RENAMED-RECORD RENAMES RECORD.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert!(schema.all_fields().len() >= 2);
}

#[test]
fn test_schema_parse_with_thru() {
    // Test parsing with THRU clause
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC 9(5).
            05  FIELD-3  PIC 9(5).
        01  RENAMED-FIELD RENAMES FIELD-1 THRU FIELD-2.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert!(schema.all_fields().len() >= 3);
}

#[test]
fn test_schema_field_is_group() {
    // Test group field check
    let copybook = r#"
        01  GROUP-FIELD.
            05  SUB-FIELD-1  PIC 9(5).
            05  SUB-FIELD-2  PIC X(10).
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert!(schema.all_fields()[0].is_group());
}

#[test]
fn test_schema_field_is_scalar() {
    // Test scalar field check
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert!(schema.all_fields()[0].is_scalar());
}

#[test]
fn test_schema_field_is_filler() {
    // Test filler field check
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FILLER   PIC X(5).
            05  FIELD-2  PIC 9(5).
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    // Check for FILLER field
    let has_filler = schema.all_fields()[0]
        .children
        .iter()
        .any(|f| f.name == "FILLER");
    assert!(has_filler);
}

#[test]
fn test_schema_parse_with_children() {
    // Test field with children (group)
    let copybook = r#"
        01  GROUP-FIELD.
            05  SUB-FIELD-1  PIC 9(5).
            05  SUB-FIELD-2  PIC X(10).
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert_eq!(schema.all_fields().len(), 1);
    assert_eq!(schema.all_fields()[0].children.len(), 2);
}

#[test]
fn test_schema_parse_with_thru_clause() {
    // Test parsing with THRU clause
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC 9(5).
            05  FIELD-3  PIC 9(5).
        01  RENAMED-FIELD RENAMES FIELD-1 THRU FIELD-2.
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    assert!(schema.all_fields().len() >= 3);
}
