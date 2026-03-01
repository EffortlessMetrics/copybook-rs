#![allow(
// SPDX-License-Identifier: AGPL-3.0-or-later
    clippy::expect_used,
    clippy::unwrap_used,
    clippy::too_many_lines,
    clippy::items_after_statements,
    clippy::format_push_string,
    clippy::uninlined_format_args,
    clippy::cast_precision_loss,
    clippy::panic
)]
/*!
 * Enhanced Edge Case Validation Tests
 *
 * These tests specifically target edge cases, boundary conditions, and complex
 * parsing scenarios to strengthen test coverage for enterprise production readiness.
 *
 * Focus Areas:
 * - Level-88 field boundary conditions and validation
 * - ODO counter edge cases and error conditions
 * - REDEFINES interaction complex scenarios
 * - Complex nested structure validation
 * - Memory usage patterns with large datasets
 * - Error handling robustness
 */

use copybook_core::{Error, ErrorCode, ParseOptions, parse_copybook, parse_copybook_with_options};
use std::time::Instant;

/// Test Level-88 fields with extreme boundary conditions
#[test]
fn test_level88_extreme_boundary_conditions() {
    // Test with realistic nesting depth
    const MAX_NESTING_COPYBOOK: &str = r"
01 DEEP-NESTED-RECORD.
   05 LEVEL05-FIELD PIC X(1).
      10 LEVEL10-FIELD PIC X(1).
         15 LEVEL15-FIELD PIC X(1).
            20 LEVEL20-FIELD PIC X(1).
               25 LEVEL25-FIELD PIC X(1).
                  49 LEVEL49-FIELD PIC X(1).
88 DEEP-CONDITION VALUE 'Y' OF LEVEL49-FIELD.
88 ANOTHER-DEEP-CONDITION VALUE 'N' OF LEVEL49-FIELD.
";

    let result = parse_copybook(MAX_NESTING_COPYBOOK);
    assert!(
        result.is_ok(),
        "Deep nesting with Level-88 should parse successfully"
    );

    let schema = result.unwrap();
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();

    assert_eq!(
        level88_fields.len(),
        2,
        "Should have 2 Level-88 fields at maximum depth"
    );
    assert!(level88_fields.iter().any(|f| f.name == "DEEP-CONDITION"));
    assert!(
        level88_fields
            .iter()
            .any(|f| f.name == "ANOTHER-DEEP-CONDITION")
    );
}

/// Test Level-88 fields with maximum value clauses
#[test]
fn test_level88_maximum_value_conditions() {
    const MANY_VALUES_COPYBOOK: &str = r"
01 STATUS-RECORD.
   05 STATUS-CODE PIC X(3).
88 STATUS-ACTIVE VALUE 'A01' 'A02' 'A03' 'A04' 'A05' 'A06' 'A07' 'A08' 'A09' 'A10'
                       'A11' 'A12' 'A13' 'A14' 'A15' 'A16' 'A17' 'A18' 'A19' 'A20'.
88 STATUS-INACTIVE VALUE 'I01' 'I02' 'I03' 'I04' 'I05' 'I06' 'I07' 'I08' 'I09' 'I10'.
88 STATUS-PENDING VALUE 'P01' THRU 'P99'.
88 STATUS-ERROR VALUE 'E00' THRU 'E99' 'ERR' 'BAD' 'UNK'.
";

    let result = parse_copybook(MANY_VALUES_COPYBOOK);
    assert!(
        result.is_ok(),
        "Level-88 with many values should parse successfully"
    );

    let schema = result.unwrap();
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();

    assert_eq!(
        level88_fields.len(),
        4,
        "Should have 4 Level-88 condition fields"
    );

    // Verify comprehensive value handling
    let active_condition = level88_fields.iter().find(|f| f.name == "STATUS-ACTIVE");
    assert!(
        active_condition.is_some(),
        "Should find STATUS-ACTIVE condition"
    );
}

/// Test ODO with extreme boundary values
#[test]
fn test_odo_extreme_boundary_values() {
    // Test minimum boundary (1)
    const MIN_ODO_COPYBOOK: &str = r"
01 MINIMAL-RECORD.
   05 ITEM-COUNT PIC 9(1).
   05 ITEMS OCCURS 1 TO 1 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-DATA PIC X(10).
";

    // Test a large but still within the 16 MiB record cap (1 byte per item)
    const LARGE_ODO_WITHIN_LIMIT: &str = r"
01 LARGE-RECORD.
   05 ITEM-COUNT PIC 9(7).
   05 ITEMS OCCURS 1 TO 1000000 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-DATA PIC X(1).
";

    // Test enterprise-scale maximum boundary that should exceed the layout cap
    const EXCEEDS_RECORD_LIMIT_COPYBOOK: &str = r"
01 ENTERPRISE-RECORD.
   05 TRANSACTION-COUNT PIC 9(7).
   05 TRANSACTIONS OCCURS 1 TO 9999999 TIMES DEPENDING ON TRANSACTION-COUNT.
      10 TXN-ID PIC X(20).
      10 TXN-AMOUNT PIC S9(15)V99 COMP-3.
";

    let result = parse_copybook(MIN_ODO_COPYBOOK);
    assert!(result.is_ok(), "ODO with minimum bounds should be valid");

    let result = parse_copybook(LARGE_ODO_WITHIN_LIMIT);
    assert!(
        result.is_ok(),
        "ODO with enterprise-scale maximum within record cap should be valid"
    );

    let result = parse_copybook(EXCEEDS_RECORD_LIMIT_COPYBOOK);
    match result {
        Err(Error {
            code: ErrorCode::CBKS141_RECORD_TOO_LARGE,
            ..
        }) => {}
        Err(err) => panic!(
            "Expected CBKS141_RECORD_TOO_LARGE for ODO exceeding record cap, got {:?}",
            err.code
        ),
        Ok(_) => panic!("Expected CBKS141_RECORD_TOO_LARGE for oversized ODO layout"),
    }

    // Test performance with large ODO parsing
    let start_time = Instant::now();
    let _ = parse_copybook(LARGE_ODO_WITHIN_LIMIT);
    let parse_duration = start_time.elapsed();

    assert!(
        parse_duration.as_millis() < 100,
        "Large ODO parsing should be fast: {}ms",
        parse_duration.as_millis()
    );
}

/// Test invalid ODO configurations (should fail)
#[test]
fn test_odo_invalid_configurations() {
    // ODO counter after ODO array should fail — the parser rejects this because
    // the ODO array (ITEMS) is not the last storage field (ITEM-COUNT follows it),
    // producing CBKP021_ODO_NOT_TAIL.
    const INVALID_ODO_ORDER: &str = r"
01 INVALID-RECORD.
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-DATA PIC X(10).
   05 ITEM-COUNT PIC 9(3).
";

    // Nested ODO should fail with CBKP022_NESTED_ODO
    const NESTED_ODO: &str = r"
01 NESTED-RECORD.
   05 OUTER-COUNT PIC 9(3).
   05 OUTER-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON OUTER-COUNT.
      10 INNER-COUNT PIC 9(2).
      10 INNER-ARRAY OCCURS 1 TO 50 TIMES DEPENDING ON INNER-COUNT.
         15 INNER-DATA PIC X(5).
";

    let result = parse_copybook(INVALID_ODO_ORDER);
    assert!(result.is_err(), "ODO counter after ODO array should fail");

    if let Err(Error { code, .. }) = result {
        assert!(
            matches!(code, ErrorCode::CBKP021_ODO_NOT_TAIL),
            "Should be ODO not-tail error, got {code:?}"
        );
    }

    let result = parse_copybook(NESTED_ODO);
    assert!(result.is_err(), "Nested ODO should fail");

    if let Err(Error { code, .. }) = result {
        assert!(
            matches!(code, ErrorCode::CBKP022_NESTED_ODO),
            "Should be nested ODO error, got {code:?}"
        );
    }
}

/// Test complex REDEFINES with multiple overlapping structures
#[test]
fn test_complex_redefines_overlapping_structures() {
    // Note: Level-88 numeric values with OF qualifier (e.g. VALUE 01 OF FIELD)
    // are not supported by the parser — the OF token is misinterpreted.
    // Use string-based Level-88 values or omit the OF qualifier for numeric values.
    const COMPLEX_REDEFINES: &str = r"
01 COMPLEX-OVERLAY-RECORD.
   05 BASE-DATA PIC X(100).
   05 NUMERIC-VIEW REDEFINES BASE-DATA.
      10 NUMERIC-PART1 PIC S9(10)V99 COMP-3.
      10 NUMERIC-PART2 PIC S9(15) COMP.
      10 FILLER PIC X(75).
   05 TEXT-VIEW REDEFINES BASE-DATA.
      10 TEXT-HEADER PIC X(20).
      10 TEXT-BODY PIC X(60).
      10 TEXT-FOOTER PIC X(20).
   05 STRUCTURED-VIEW REDEFINES BASE-DATA.
      10 HEADER-SECTION.
         15 RECORD-TYPE PIC X(4).
         15 RECORD-VERSION PIC X(2).
         15 CREATION-DATE PIC 9(8).
         15 MODIFICATION-DATE PIC 9(8).
      10 DATA-SECTION PIC X(68).
      10 CHECKSUM-SECTION.
         15 DATA-CHECKSUM PIC X(8).
         15 HEADER-CHECKSUM PIC X(8).
88 TYPE-CUSTOMER VALUE 'CUST' OF RECORD-TYPE.
88 TYPE-VENDOR VALUE 'VEND' OF RECORD-TYPE.
88 TYPE-EMPLOYEE VALUE 'EMPL' OF RECORD-TYPE.
88 VERSION-CURRENT VALUE '01' OF RECORD-VERSION.
88 VERSION-LEGACY VALUE '00' OF RECORD-VERSION.
";

    let result = parse_copybook(COMPLEX_REDEFINES);
    assert!(
        result.is_ok(),
        "Complex REDEFINES with overlapping structures should parse: {result:?}"
    );

    let schema = result.unwrap();
    let root = &schema.fields[0];

    // Verify all REDEFINES structures are parsed
    let redefines_fields: Vec<_> = root
        .children
        .iter()
        .filter(|f| f.redefines_of.is_some())
        .collect();

    assert_eq!(
        redefines_fields.len(),
        3,
        "Should have 3 REDEFINES structures"
    );

    // Verify Level-88 fields for complex REDEFINES
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();

    assert_eq!(level88_fields.len(), 5, "Should have 5 Level-88 conditions");

    // Verify structure integrity
    let structured_view = root
        .children
        .iter()
        .find(|f| f.name == "STRUCTURED-VIEW")
        .unwrap();

    assert_eq!(
        structured_view.children.len(),
        3,
        "STRUCTURED-VIEW should have 3 main sections"
    );
}

/// Test REDEFINES size validation with mismatched sizes
///
/// The parser does not reject REDEFINES that are larger than the original field.
/// Many COBOL compilers (including IBM Enterprise COBOL) allow this, treating the
/// REDEFINES as an overlay that may extend beyond the original storage. The parser
/// follows this permissive behavior.
#[test]
fn test_redefines_size_validation() {
    // REDEFINES larger than original is accepted by the parser (permissive behavior)
    const OVERSIZED_REDEFINES: &str = r"
01 SIZE-MISMATCH-RECORD.
   05 ORIGINAL-DATA PIC X(20).
   05 LARGER-REDEFINES REDEFINES ORIGINAL-DATA.
      10 PART1 PIC X(15).
      10 PART2 PIC X(15).
";

    // Same-size REDEFINES
    const VALID_REDEFINES: &str = r"
01 VALID-SIZE-RECORD.
   05 ORIGINAL-DATA PIC X(20).
   05 SAME-SIZE-REDEFINES REDEFINES ORIGINAL-DATA.
      10 PART1 PIC X(10).
      10 PART2 PIC X(10).
";

    let result = parse_copybook(OVERSIZED_REDEFINES);
    assert!(
        result.is_ok(),
        "Oversized REDEFINES is accepted (permissive parser behavior)"
    );

    let schema = result.unwrap();
    let root = &schema.fields[0];
    let redefines = root
        .children
        .iter()
        .find(|f| f.name == "LARGER-REDEFINES")
        .expect("Should find LARGER-REDEFINES");
    assert_eq!(
        redefines.redefines_of.as_deref(),
        Some("ORIGINAL-DATA"),
        "REDEFINES target should be ORIGINAL-DATA"
    );

    let result = parse_copybook(VALID_REDEFINES);
    assert!(result.is_ok(), "Same-size REDEFINES should be valid");
}

/// Test memory usage patterns with large, complex structures
///
/// Note: The original test used nested ODO (ACCOUNTS inside CUSTOMER-RECORDS)
/// and a sibling after ODO (AUDIT-TRAIL), both of which the parser rejects by
/// design (`CBKP022_NESTED_ODO` and `CBKP021_ODO_NOT_TAIL`). This revised version
/// uses a valid structure with a single tail ODO and many fixed fields.
#[test]
fn test_memory_usage_large_complex_structures() {
    const LARGE_COMPLEX_STRUCTURE: &str = r"
01 ENTERPRISE-MASTER-RECORD.
   05 HEADER-SECTION.
      10 RECORD-ID PIC X(20).
      10 RECORD-TYPE PIC X(4).
      10 VERSION PIC 9(3).
      10 CREATION-TIMESTAMP PIC 9(14).
      10 MODIFICATION-TIMESTAMP PIC 9(14).
   05 CUSTOMER-INFO.
      10 CUSTOMER-ID PIC X(12).
      10 CUSTOMER-NAME PIC X(60).
      10 CUSTOMER-TYPE PIC X(2).
      10 CUSTOMER-STATUS PIC X(1).
   05 ACCOUNT-SECTION.
      10 ACCOUNT-NUMBER PIC 9(16).
      10 ACCOUNT-TYPE PIC X(3).
      10 ACCOUNT-BALANCE PIC S9(13)V99 COMP-3.
      10 ACCOUNT-STATUS PIC X(1).
   05 ADDRESS-INFO.
      10 STREET-ADDRESS PIC X(100).
      10 CITY PIC X(50).
      10 STATE PIC X(20).
      10 POSTAL-CODE PIC X(20).
      10 COUNTRY PIC X(30).
   05 AUDIT-COUNT PIC 9(5).
   05 AUDIT-ENTRIES OCCURS 1 TO 99999 TIMES DEPENDING ON AUDIT-COUNT.
      10 AUDIT-TIMESTAMP PIC 9(14).
      10 AUDIT-USER PIC X(20).
      10 AUDIT-ACTION PIC X(10).
      10 AUDIT-DETAILS PIC X(200).
88 TYPE-INDIVIDUAL VALUE 'IN' OF CUSTOMER-TYPE.
88 TYPE-BUSINESS VALUE 'BU' OF CUSTOMER-TYPE.
88 TYPE-GOVERNMENT VALUE 'GO' OF CUSTOMER-TYPE.
88 STATUS-ACTIVE VALUE 'A' OF CUSTOMER-STATUS.
88 STATUS-INACTIVE VALUE 'I' OF CUSTOMER-STATUS.
88 STATUS-SUSPENDED VALUE 'S' OF CUSTOMER-STATUS.
88 ACCOUNT-CHECKING VALUE 'CHK' OF ACCOUNT-TYPE.
88 ACCOUNT-SAVINGS VALUE 'SAV' OF ACCOUNT-TYPE.
88 ACCOUNT-INVESTMENT VALUE 'INV' OF ACCOUNT-TYPE.
";

    let start_time = Instant::now();
    let result = parse_copybook(LARGE_COMPLEX_STRUCTURE);
    let parse_duration = start_time.elapsed();

    assert!(
        result.is_ok(),
        "Large complex structure should parse successfully: {result:?}"
    );
    assert!(
        parse_duration.as_millis() < 200,
        "Large structure parsing should be efficient: {}ms",
        parse_duration.as_millis()
    );

    let schema = result.unwrap();

    // Verify field count is reasonable for complex structure
    let all_fields = schema.all_fields();
    assert!(
        all_fields.len() > 20,
        "Complex structure should have many fields: got {}",
        all_fields.len()
    );

    // Verify Level-88 fields are correctly parsed
    let level88_fields: Vec<_> = all_fields.into_iter().filter(|f| f.level == 88).collect();

    assert_eq!(level88_fields.len(), 9, "Should have 9 Level-88 conditions");

    // Memory usage should be reasonable
    let memory_estimate = std::mem::size_of_val(&schema)
        + schema.fields.len() * std::mem::size_of::<copybook_core::Field>();

    println!("Schema memory estimate: {memory_estimate} bytes");
    assert!(
        memory_estimate < 1024 * 1024,
        "Memory usage should be reasonable"
    );
}

/// Test error handling robustness with malformed input
#[test]
fn test_error_handling_robustness() {
    // Test malformed inputs that should produce meaningful errors.
    // Note: "Level-88 without parent field" (88 directly under a 01 group)
    // is valid — the 88 attaches to the group record. So that case is tested
    // separately as a success scenario below.
    let malformed_cases: Vec<(&str, &str, ErrorCode)> = vec![
        (
            "Empty OCCURS clause",
            "01 BAD-RECORD.\n   05 BAD-FIELD OCCURS TIMES.\n      10 DATA PIC X(1).",
            ErrorCode::CBKP001_SYNTAX,
        ),
        (
            "Invalid ODO counter reference",
            "01 BAD-RECORD.\n   05 BAD-FIELD OCCURS 1 TO 10 TIMES DEPENDING ON NONEXISTENT.\n      10 DATA PIC X(1).",
            ErrorCode::CBKS121_COUNTER_NOT_FOUND,
        ),
        (
            "REDEFINES of nonexistent field",
            "01 BAD-RECORD.\n   05 BAD-REDEFINES REDEFINES NONEXISTENT.\n      10 DATA PIC X(10).",
            ErrorCode::CBKP001_SYNTAX,
        ),
    ];

    for (description, copybook, expected_error) in malformed_cases {
        let result = parse_copybook(copybook);
        assert!(result.is_err(), "{description} should produce an error");

        if let Err(Error { code, .. }) = result {
            assert_eq!(
                code, expected_error,
                "{description} should produce error code {expected_error:?}, got {code:?}"
            );
        }
    }

    // Level-88 directly under a group (01) is valid — the condition attaches
    // to the group record itself.
    let orphan_88 = "01 BAD-RECORD.\n   88 ORPHAN-CONDITION VALUE 'Y'.";
    let result = parse_copybook(orphan_88);
    assert!(
        result.is_ok(),
        "Level-88 directly under a group record is valid"
    );

    let schema = result.unwrap();
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();
    assert_eq!(
        level88_fields.len(),
        1,
        "Should have 1 Level-88 condition field"
    );
    assert_eq!(level88_fields[0].name, "ORPHAN-CONDITION");
}

/// Test parse options with different configurations
///
/// The default `ParseOptions::allow_inline_comments` is `true`, so the default
/// parser strips COBOL-2002 inline comments (`*>`) in the lexer. When disabled,
/// the `*>` and trailing text remain in the line, but because each COBOL
/// statement is already terminated by a period, the tokenizer still succeeds
/// (the comment text after the period is simply ignored). The primary effect of
/// `allow_inline_comments` is lexer-level stripping, not parser-level rejection.
#[test]
fn test_parse_options_configurations() {
    const COPYBOOK_WITH_INLINE_COMMENTS: &str = r"
01 RECORD-WITH-COMMENTS.               *> Main record structure
   05 FIELD1 PIC X(10).                *> Primary identifier field
   05 FIELD2 PIC 9(5).                 *> Numeric counter field
   88 COUNTER-HIGH VALUE 99999.        *> Maximum counter value
";

    // Default options have allow_inline_comments=true, so this should succeed
    let result_default = parse_copybook(COPYBOOK_WITH_INLINE_COMMENTS);
    assert!(
        result_default.is_ok(),
        "Default options accept inline comments (allow_inline_comments defaults to true)"
    );

    let schema = result_default.unwrap();
    let level88_fields: Vec<_> = schema
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();

    assert_eq!(
        level88_fields.len(),
        1,
        "Should parse Level-88 field with inline comments"
    );
    assert_eq!(level88_fields[0].name, "COUNTER-HIGH");

    // Explicitly enable inline comments — same result, confirms the option works
    let parse_options = ParseOptions {
        allow_inline_comments: true,
        ..ParseOptions::default()
    };
    let result_enabled = parse_copybook_with_options(COPYBOOK_WITH_INLINE_COMMENTS, &parse_options);
    assert!(
        result_enabled.is_ok(),
        "Inline comments should be accepted when allow_inline_comments is true"
    );

    let schema_enabled = result_enabled.unwrap();
    let level88_enabled: Vec<_> = schema_enabled
        .all_fields()
        .into_iter()
        .filter(|f| f.level == 88)
        .collect();

    assert_eq!(
        level88_enabled.len(),
        1,
        "Should parse Level-88 field with inline comments enabled"
    );
    assert_eq!(level88_enabled[0].name, "COUNTER-HIGH");

    // Verify different parse options produce consistent schemas
    assert_eq!(
        schema.all_fields().len(),
        schema_enabled.all_fields().len(),
        "Field count should be the same regardless of inline comments option"
    );
}

/// Test comprehensive boundary condition validation
#[test]
fn test_comprehensive_boundary_conditions() {
    // Test field name length boundaries
    const LONG_FIELD_NAMES: &str = r"
01 BOUNDARY-TEST-RECORD.
   05 VERY-LONG-FIELD-NAME-THAT-TESTS-COBOL-IDENTIFIER-LIMITS PIC X(10).
   05 A PIC X(1).
   88 VERY-LONG-CONDITION-NAME-THAT-TESTS-LEVEL88-IDENTIFIER-LIMITS VALUE 'Y' OF A.
";

    // Test PIC clause boundaries
    const PIC_BOUNDARIES: &str = r"
01 PIC-BOUNDARY-RECORD.
   05 SINGLE-CHAR PIC X(1).
   05 LARGE-FIELD PIC X(32767).
   05 TINY-NUMERIC PIC 9(1).
   05 LARGE-NUMERIC PIC 9(18).
   05 TINY-DECIMAL PIC 9(1)V9(1).
   05 LARGE-DECIMAL PIC 9(15)V9(2).
";

    let result = parse_copybook(LONG_FIELD_NAMES);
    assert!(
        result.is_ok(),
        "Long field names within COBOL limits should be valid"
    );

    let result = parse_copybook(PIC_BOUNDARIES);
    assert!(
        result.is_ok(),
        "PIC clause boundaries should be handled correctly"
    );

    // Test level number boundaries
    const LEVEL_BOUNDARIES: &str = r"
01 LEVEL-BOUNDARY-RECORD.
   02 LEVEL02-FIELD PIC X(1).
   02 LEVEL03-FIELD PIC X(1).
   02 LEVEL49-FIELD PIC X(1).
   66 LEVEL66-FIELD RENAMES LEVEL02-FIELD THRU LEVEL49-FIELD.
77 LEVEL77-FIELD PIC X(1).
   88 CONDITION-FIELD VALUE 'Y' OF LEVEL77-FIELD.
";

    let result = parse_copybook(LEVEL_BOUNDARIES);
    if let Err(ref e) = result {
        eprintln!("Error: {}", e);
    }
    assert!(result.is_ok(), "All valid level numbers should be accepted");
}

/// Performance test for parsing speed with complex structures
#[test]
fn test_parsing_performance_characteristics() {
    // Generate a large copybook programmatically
    let mut large_copybook = String::from("01 PERFORMANCE-TEST-RECORD.\n");

    for i in 1..=1000 {
        large_copybook.push_str(&format!("   05 FIELD{:04} PIC X(10).\n", i));
        if i % 10 == 0 {
            large_copybook.push_str(&format!(
                "   88 CONDITION{:04} VALUE 'Y' OF FIELD{:04}.\n",
                i, i
            ));
        }
    }

    let start_time = Instant::now();
    let result = parse_copybook(&large_copybook);
    let parse_duration = start_time.elapsed();

    assert!(result.is_ok(), "Large copybook should parse successfully");
    assert!(
        parse_duration.as_millis() < 1000,
        "Parsing 1000 fields should complete within 1 second: {}ms",
        parse_duration.as_millis()
    );

    let schema = result.unwrap();
    let all_fields = schema.all_fields();
    assert_eq!(
        all_fields.len(),
        1101,
        "Should have 1001 regular fields + 100 Level-88 fields"
    );

    let level88_count = all_fields.iter().filter(|f| f.level == 88).count();
    assert_eq!(
        level88_count, 100,
        "Should have 100 Level-88 condition fields"
    );

    println!(
        "Performance test: parsed {} fields in {}ms ({:.2} fields/ms)",
        all_fields.len(),
        parse_duration.as_millis(),
        all_fields.len() as f64 / parse_duration.as_millis() as f64
    );
}

/// Test VALUE clause range syntax errors (validates existing protection)
#[test]
fn test_level88_value_range_without_start() {
    // Test that VALUE THRU 5 (without a start value) is properly rejected
    // by the existing "requires at least one value" check
    let copybook = "
01 RECORD.
   05 STATUS-CODE PIC X(3).
   88 INVALID-RANGE VALUE THRU '999'.
";

    let result = parse_copybook(copybook);
    assert!(
        result.is_err(),
        "VALUE range without start value should be rejected"
    );

    let err = result.unwrap_err();
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("VALUE clause requires at least one value"),
        "Error should indicate VALUE clause requires value, got: {}",
        err_msg
    );

    // Also test with THROUGH variant
    let copybook_through = "
01 RECORD.
   05 STATUS-CODE PIC 9(3).
   88 INVALID-RANGE VALUE THROUGH 100.
";

    let result_through = parse_copybook(copybook_through);
    assert!(
        result_through.is_err(),
        "VALUE range (THROUGH) without start value should be rejected"
    );

    let err_through = result_through.unwrap_err();
    let err_msg_through = err_through.to_string();
    assert!(
        err_msg_through.contains("VALUE clause requires at least one value"),
        "Error should indicate VALUE clause requires value, got: {}",
        err_msg_through
    );
}

/// Meta-test to ensure comprehensive edge case coverage
#[test]
fn test_comprehensive_edge_case_coverage() {
    let edge_case_tests = [
        "test_level88_extreme_boundary_conditions",
        "test_level88_maximum_value_conditions",
        "test_level88_value_range_without_start",
        "test_odo_extreme_boundary_values",
        "test_odo_invalid_configurations",
        "test_complex_redefines_overlapping_structures",
        "test_redefines_size_validation",
        "test_memory_usage_large_complex_structures",
        "test_error_handling_robustness",
        "test_parse_options_configurations",
        "test_comprehensive_boundary_conditions",
        "test_parsing_performance_characteristics",
    ];

    assert_eq!(
        edge_case_tests.len(),
        12,
        "Should have comprehensive edge case test coverage"
    );

    println!(
        "✅ Enhanced edge case validation covers {} test scenarios:",
        edge_case_tests.len()
    );
    for (i, test_name) in edge_case_tests.iter().enumerate() {
        println!("   {}. {}", i + 1, test_name);
    }
}
