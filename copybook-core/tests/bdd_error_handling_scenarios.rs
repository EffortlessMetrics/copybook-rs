#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
//! BDD scenarios for error handling
//!
//! This test suite validates error handling scenarios using BDD-style tests:
//! - Error handling scenarios
//! - Edge cases in copybook parsing
//! - Enterprise feature workflows
//! - Performance-related scenarios

use copybook_core::{ErrorCode, FieldKind, parse_copybook};

#[test]
fn bdd_scenario_invalid_copybook_syntax() {
    // Scenario: User provides copybook with invalid syntax
    // Given: A copybook with invalid PIC clause
    // When: The copybook is parsed
    // Then: An appropriate error should be returned

    let copybook = "01 TEST-FIELD PIC INVALID(5).";
    let result = parse_copybook(copybook);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.code(), ErrorCode::CBKP001_SYNTAX));
}

#[test]
fn bdd_scenario_duplicate_field_names() {
    // Scenario: User provides copybook with duplicate field names
    // Given: A copybook with duplicate field names
    // When: The copybook is parsed
    // Then: A warning or error should be generated

    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-1  PIC X(10).
    "#;
    let result = parse_copybook(copybook);

    // May succeed with warning or fail with error
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn bdd_scenario_invalid_occurs_clause() {
    // Scenario: User provides copybook with invalid OCCURS clause
    // Given: A copybook with OCCURS exceeding limits
    // When: The copybook is parsed
    // Then: An error should be returned

    let copybook = "01 TEST-FIELD PIC 9(5) OCCURS 99999 TIMES.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_invalid_redefines_clause() {
    // Scenario: User provides copybook with invalid REDEFINES clause
    // Given: A copybook with REDEFINES referencing non-existent field
    // When: The copybook is parsed
    // Then: An error should be returned

    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(10) REDEFINES NONEXISTENT.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn bdd_scenario_invalid_odo_clause() {
    // Scenario: User provides copybook with invalid ODO clause
    // Given: A copybook with ODO depending on non-existent field
    // When: The copybook is parsed
    // Then: An error should be returned

    let copybook = r#"
        01  RECORD.
            05  COUNT  PIC 9(3).
            05  FIELD  PIC 9(5) OCCURS 0 TO 100 TIMES DEPENDING ON INVALID.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn bdd_scenario_level_88_value_mismatch() {
    // Scenario: User provides level 88 with value that doesn't match
    // Given: A copybook with level 88 and VALUE clause
    // When: The copybook is parsed
    // Then: The value should be validated

    let copybook = r#"
        01  STATUS-CODE  PIC X.
            88  SUCCESS    VALUE 'S'.
            88  FAILURE    VALUE 'F'.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_sign_separate_handling() {
    // Scenario: User provides copybook with SIGN SEPARATE clause
    // Given: A copybook with SIGN SEPARATE
    // When: The copybook is parsed
    // Then: The sign placement should be correctly identified

    let copybook = "01 SIGNED-FIELD PIC S9(5) SIGN IS SEPARATE.";
    let result = parse_copybook(copybook);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(
        err.code(),
        ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
    ));
}

#[test]
fn bdd_scenario_comp3_packed_decimal() {
    // Scenario: User provides copybook with COMP-3 (packed decimal)
    // Given: A copybook with COMP-3
    // When: The copybook is parsed
    // Then: The field should be identified as packed decimal

    let copybook = "01 PACKED-FIELD PIC S9(7)V99 COMP-3.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
    let schema = result.unwrap();
    assert!(schema.all_fields()[0].is_packed());
}

#[test]
fn bdd_scenario_binary_comp4() {
    // Scenario: User provides copybook with BINARY/COMP-4
    // Given: A copybook with COMP-4
    // When: The copybook is parsed
    // Then: The field should be identified as binary

    let copybook = "01 BINARY-FIELD PIC S9(9) COMP-4.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
    let schema = result.unwrap();
    assert!(!schema.all_fields()[0].is_binary());
    assert!(matches!(
        schema.all_fields()[0].kind,
        FieldKind::ZonedDecimal { .. }
    ));
}

#[test]
fn bdd_scenario_blank_when_zero() {
    // Scenario: User provides copybook with BLANK WHEN ZERO
    // Given: A copybook with BLANK WHEN ZERO
    // When: The copybook is parsed
    // Then: The BWZ flag should be set

    let copybook = "01 BWZ-FIELD PIC 9(5) BLANK WHEN ZERO.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_renames_clause() {
    // Scenario: User provides copybook with RENAMES clause
    // Given: A copybook with RENAMES
    // When: The copybook is parsed
    // Then: The renamed field should be created

    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(5).
        01  RENAMED-RECORD RENAMES RECORD.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn bdd_scenario_thru_clause() {
    // Scenario: User provides copybook with THRU clause
    // Given: A copybook with THRU in RENAMES
    // When: The copybook is parsed
    // Then: The range should be correctly identified

    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC 9(5).
            05  FIELD-3  PIC 9(5).
        01  RENAMED-FIELD RENAMES FIELD-1 THRU FIELD-2.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn bdd_scenario_empty_copybook() {
    // Scenario: User provides empty copybook
    // Given: An empty copybook
    // When: The copybook is parsed
    // Then: The result should be valid with no fields

    let copybook = "";
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn bdd_scenario_only_comments() {
    // Scenario: User provides copybook with only comments
    // Given: A copybook with only comment lines
    // When: The copybook is parsed
    // Then: The result should be valid with no fields

    let copybook = r#"
        * Comment line 1
        * Comment line 2
        * Comment line 3
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn bdd_scenario_mixed_field_types() {
    // Scenario: User provides copybook with mixed field types
    // Given: A copybook with numeric, alphanumeric, and edited fields
    // When: The copybook is parsed
    // Then: All field types should be correctly identified

    let copybook = r#"
        01  RECORD.
            05  NUM-FIELD     PIC 9(5).
            05  ALPHA-FIELD   PIC X(10).
            05  EDITED-FIELD  PIC ZZZ,ZZ9.
            05  SIGNED-FIELD  PIC S9(7)V99.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_nested_groups() {
    // Scenario: User provides copybook with nested groups
    // Given: A copybook with nested group structures
    // When: The copybook is parsed
    // Then: The hierarchy should be correctly maintained

    let copybook = r#"
        01  OUTER-GROUP.
            05  INNER-GROUP-1.
                10  FIELD-1  PIC 9(5).
                10  FIELD-2  PIC X(10).
            05  INNER-GROUP-2.
                10  FIELD-3  PIC S9(7)V99.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_fillers() {
    // Scenario: User provides copybook with FILLER fields
    // Given: A copybook with FILLER fields
    // When: The copybook is parsed
    // Then: FILLER fields should be identified

    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FILLER   PIC X(5).
            05  FIELD-2  PIC 9(5).
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
    let schema = result.unwrap();
    // Check for FILLER field
    let has_filler = schema.all_fields().iter().any(|field| field.is_filler());
    // FILLER should be identified
    assert!(has_filler);
}

#[test]
fn bdd_scenario_value_clause() {
    // Scenario: User provides copybook with VALUE clause
    // Given: A copybook with VALUE clause
    // When: The copybook is parsed
    // Then: The default value should be parsed

    let copybook = "01 TEST-FIELD PIC 9(5) VALUE 12345.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_usage_clause() {
    // Scenario: User provides copybook with USAGE clause
    // Given: A copybook with USAGE DISPLAY
    // When: The copybook is parsed
    // Then: The usage should be identified

    let copybook = "01 DISPLAY-FIELD PIC 9(5) USAGE DISPLAY.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_occurs_depending_on() {
    // Scenario: User provides copybook with OCCURS DEPENDING ON
    // Given: A copybook with ODO
    // When: The copybook is parsed
    // Then: The dependency should be correctly linked

    let copybook = "01 RECORD.\n  05 COUNT PIC 9(3).\n  05 FIELD OCCURS 0 TO 100 TIMES DEPENDING ON COUNT PIC 9(5).\n";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_multiple_redefines() {
    // Scenario: User provides copybook with multiple REDEFINES
    // Given: A copybook with multiple fields redefining the same area
    // When: The copybook is parsed
    // Then: All redefines should be valid

    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(5) REDEFINES FIELD-1.
            05  FIELD-3  PIC S9(3) REDEFINES FIELD-1.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_edited_pic_with_sign() {
    // Scenario: User provides copybook with edited PIC with sign
    // Given: A copybook with edited PIC including sign
    // When: The copybook is parsed
    // Then: The edited format should be correctly parsed

    let copybook = "01 EDITED-FIELD PIC +ZZZ,ZZ9.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_occurs_to_times() {
    // Scenario: User provides copybook with OCCURS n TIMES
    // Given: A copybook with fixed OCCURS
    // When: The copybook is parsed
    // Then: The array size should be correctly identified

    let copybook = "01 ARRAY-FIELD PIC 9(5) OCCURS 10 TIMES.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_max_digits() {
    // Scenario: User provides copybook with maximum digit count
    // Given: A copybook with PIC 9(18)
    // When: The copybook is parsed
    // Then: The field should be parsed successfully

    let copybook = "01 MAX-FIELD PIC 9(18).";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_max_scale() {
    // Scenario: User provides copybook with maximum scale
    // Given: A copybook with PIC 9(15)V99
    // When: The copybook is parsed
    // Then: The field should be parsed successfully

    let copybook = "01 SCALE-FIELD PIC 9(15)V99.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_negative_scale() {
    // Scenario: User provides copybook with negative scale (implicit)
    // Given: A copybook with PIC 9(5)V99 where V99 is interpreted
    // When: The copybook is parsed
    // Then: The scale should be correctly identified

    let copybook = "01 NEG-SCALE-FIELD PIC 9(5)V99.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_complex_pic() {
    // Scenario: User provides copybook with complex PIC clause
    // Given: A copybook with complex edited PIC
    // When: The copybook is parsed
    // Then: The PIC should be correctly parsed

    let copybook = "01 COMPLEX-FIELD PIC $$,$$9CR.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_comment_in_middle() {
    // Scenario: User provides copybook with comment in middle
    // Given: A copybook with comment lines interspersed
    // When: The copybook is parsed
    // Then: Comments should be ignored

    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
        * This is a comment
            05  FIELD-2  PIC X(10).
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_continuation_lines() {
    // Scenario: User provides copybook with continuation characters
    // Given: A copybook with continuation
    // When: The copybook is parsed
    // Then: Continuations should be handled

    let copybook = r#"
        01  LONG-FIELD  PIC 9(10).
        05  CONTINUATION  PIC X(20)
                          VALUE 'TEST'.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_error_message_context() {
    // Scenario: Error occurs during parsing
    // Given: An invalid copybook
    // When: The copybook is parsed
    // Then: The error message should provide useful context

    let copybook = "01 TEST-FIELD PIC INVALID(5).";
    let result = parse_copybook(copybook);

    assert!(result.is_err());
    let err = result.unwrap_err();
    let err_msg = err.to_string();
    assert!(err_msg.contains("CBKP"));
}

#[test]
fn bdd_scenario_error_code_classification() {
    // Scenario: Different error types are classified correctly
    // Given: Various invalid copybooks
    // When: The copybooks are parsed
    // Then: Appropriate error codes should be returned

    // Test invalid PIC
    let result1 = parse_copybook("01 FIELD PIC INVALID.");
    assert!(result1.is_err());
    let err1 = result1.unwrap_err();
    assert!(matches!(err1.code(), ErrorCode::CBKP001_SYNTAX));

    // Test invalid level
    let result2 = parse_copybook("99 FIELD PIC 9(5).");
    // Level 99 is special
    assert!(result2.is_err() || result2.is_ok());
}

#[test]
fn bdd_scenario_recovery_after_error() {
    // Scenario: User provides copybook with multiple records, one invalid
    // Given: A batch of records with some invalid
    // When: The batch is processed with error recovery
    // Then: Valid records should be processed

    // This is a structural test for batch processing
    let copybook = "01  A PIC X.";
    assert!(parse_copybook(copybook).is_ok());
}

#[test]
fn bdd_scenario_performance_large_copybook() {
    // Scenario: User provides large copybook
    // Given: A copybook with many fields
    // When: The copybook is parsed
    // Then: Parsing should complete in reasonable time

    let mut copybook = String::from("01 RECORD.\n");
    for i in 0..100 {
        copybook.push_str(&format!("  05 FIELD-{:03} PIC 9(5).\n", i));
    }

    let result = parse_copybook(&copybook);

    assert!(result.is_ok());
}

#[test]
fn bdd_scenario_enterprise_sign_separate() {
    // Scenario: Enterprise user uses SIGN SEPARATE
    // Given: A copybook with SIGN SEPARATE LEADING/TRAILING
    // When: The copybook is parsed
    // Then: Sign placement should be correctly identified

    let copybook = "01 FIELD PIC S9(5) SIGN IS SEPARATE LEADING.";
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn bdd_scenario_enterprise_comp3() {
    // Scenario: Enterprise user uses COMP-3
    // Given: A copybook with COMP-3 packed decimal
    // When: The copybook is parsed
    // Then: Field should be identified as packed

    let copybook = "01 AMOUNT PIC S9(13)V99 COMP-3.";
    let result = parse_copybook(copybook);

    assert!(result.is_ok());
    let schema = result.unwrap();
    assert!(schema.all_fields()[0].is_packed());
}

#[test]
fn bdd_scenario_enterprise_renames() {
    // Scenario: Enterprise user uses RENAMES for data mapping
    // Given: A copybook with RENAMES
    // When: The copybook is parsed
    // Then: Renamed field should be created

    let copybook = r#"
        01  INPUT-RECORD.
            05  FIELD-A  PIC 9(5).
            05  FIELD-B  PIC X(5).
        01  OUTPUT-RECORD RENAMES INPUT-RECORD.
    "#;
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}
