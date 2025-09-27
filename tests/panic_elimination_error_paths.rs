/// Tests feature spec: panic-elimination-api-contracts.md#error-handling-contracts
/// Issue #33 - Error Path Validation Tests
///
/// This module provides comprehensive testing for structured error handling that replaces
/// panic-prone patterns. All error paths use existing CBKP*/CBKS*/CBKD*/CBKE* codes
/// with enhanced context information for enterprise debugging and audit trails.

#[cfg(test)]
mod error_path_validation {
    use copybook_core::{parse_copybook, error::{Error, ErrorCode, Result}};
    use copybook_codec::{decode_record, DecodeOptions, Codepage};
    use std::collections::HashMap;

    /// AC3: Error taxonomy integration for parser state errors
    /// Tests panic-safe parser operations with CBKP* error codes
    #[test] // AC:33:ERROR_PATH:PARSER_STATE_SAFETY
    fn test_parser_state_error_handling() {
        // Test case: Parser stack underflow scenario
        let malformed_copybook = "01 INCOMPLETE-FIELD"; // Missing PIC clause

        let result = parse_copybook(malformed_copybook);

        // Should return structured error instead of panicking
        assert!(result.is_err(), "Parser should return error for malformed copybook");

        let error = result.unwrap_err();

        // Validate error uses appropriate CBKP* code for parser errors
        assert!(
            matches!(error.code, ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE),
            "Parser error should use CBKP* code, got {:?}", error.code
        );

        // Validate enhanced error context for debugging
        assert!(
            !error.message.is_empty(),
            "Parser error should provide descriptive message"
        );
    }

    /// AC3: Numeric conversion error path validation
    /// Tests panic-safe numeric operations with CBKD* error codes
    #[test] // AC:33:ERROR_PATH:NUMERIC_SAFETY
    fn test_numeric_conversion_error_handling() {
        // Create schema with packed decimal field for testing
        let copybook = r#"
        01 TEST-RECORD.
            05 PACKED-FIELD PIC S9(5)V99 COMP-3.
        "#;

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Invalid packed decimal data (should trigger safe error handling)
        let invalid_packed_data = vec![0xFF, 0xFF, 0xFF, 0xFF]; // Invalid nibbles

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &invalid_packed_data, &options);

        // Should return structured error instead of panicking
        assert!(result.is_err(), "Invalid packed decimal should return error");

        let error = result.unwrap_err();

        // Validate error uses appropriate CBKD* code for data errors
        assert!(
            matches!(
                error.code,
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE |
                ErrorCode::CBKD301_RECORD_TOO_SHORT
            ),
            "Numeric error should use CBKD* code, got {:?}", error.code
        );
    }

    /// AC3: Data validation error path testing
    /// Tests bounds checking and validation with structured error responses
    #[test] // AC:33:ERROR_PATH:DATA_VALIDATION_SAFETY
    fn test_data_validation_error_handling() {
        let copybook = r#"
        01 TEST-RECORD.
            05 DISPLAY-FIELD PIC X(10).
            05 NUMERIC-FIELD PIC 9(5).
        "#;

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Record too short (should trigger bounds checking error)
        let short_data = vec![0x40; 5]; // Only 5 bytes for 15-byte record

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &short_data, &options);

        assert!(result.is_err(), "Short record should return error");

        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKD301_RECORD_TOO_SHORT);

        // Validate enhanced context information
        assert!(
            error.message.contains("too short") || error.message.contains("bounds"),
            "Error message should indicate bounds issue: {}", error.message
        );
    }

    /// AC3: Character conversion error path validation
    /// Tests EBCDIC/ASCII conversion with CBKC* error codes
    #[test] // AC:33:ERROR_PATH:CHARACTER_CONVERSION_SAFETY
    fn test_character_conversion_error_handling() {
        let copybook = r#"
        01 TEST-RECORD.
            05 TEXT-FIELD PIC X(5).
        "#;

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Invalid EBCDIC bytes for specified codepage
        let invalid_ebcdic_data = vec![0x00, 0x01, 0x02, 0x03, 0x04]; // Control characters

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &invalid_ebcdic_data, &options);

        // Should handle gracefully (may succeed with replacement chars or return error)
        match result {
            Ok(_) => {
                // Character conversion succeeded with replacement - acceptable
            }
            Err(error) => {
                // Should use appropriate CBKC* code for character conversion errors
                assert!(
                    matches!(error.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE),
                    "Character conversion error should use CBKC* code, got {:?}", error.code
                );
            }
        }
    }

    /// AC3: REDEFINES resolution error path testing
    /// Tests schema resolution with CBKS* error codes
    #[test] // AC:33:ERROR_PATH:REDEFINES_SAFETY
    fn test_redefines_resolution_error_handling() {
        // Test case: REDEFINES with missing target
        let copybook_with_missing_redefines = r#"
        01 TEST-RECORD.
            05 FIELD-A PIC X(10).
            05 FIELD-B REDEFINES NONEXISTENT-FIELD PIC 9(10).
        "#;

        let result = parse_copybook(copybook_with_missing_redefines);

        assert!(result.is_err(), "REDEFINES with missing target should error");

        let error = result.unwrap_err();

        // Should use schema validation error code
        assert!(
            matches!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND),
            "REDEFINES error should use CBKS* code, got {:?}", error.code
        );

        assert!(
            error.message.contains("REDEFINES") || error.message.contains("NONEXISTENT"),
            "Error message should reference REDEFINES issue: {}", error.message
        );
    }

    /// AC3: ODO bounds checking error path validation
    /// Tests ODO (Occurs Depending On) with validation errors
    #[test] // AC:33:ERROR_PATH:ODO_SAFETY
    fn test_odo_bounds_error_handling() {
        let copybook_with_odo = r#"
        01 TEST-RECORD.
            05 ARRAY-COUNT PIC 9(3).
            05 DYNAMIC-ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON ARRAY-COUNT.
                10 ITEM-DATA PIC X(5).
        "#;

        let schema = parse_copybook(copybook_with_odo).expect("Valid ODO copybook should parse");

        // Test case: ODO counter exceeds maximum bounds
        let mut test_data = vec![0x40; 1000]; // Much larger than max 100 items * 5 bytes
        // Set count to exceed bounds (999 in EBCDIC digits)
        test_data[0] = 0xF9; // '9'
        test_data[1] = 0xF9; // '9'
        test_data[2] = 0xF9; // '9'

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &test_data, &options);

        // Should handle ODO bounds violation gracefully
        match result {
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKS301_ODO_CLIPPED |
                        ErrorCode::CBKS302_ODO_RAISED |
                        ErrorCode::CBKD301_RECORD_TOO_SHORT
                    ),
                    "ODO bounds error should use appropriate code, got {:?}", error.code
                );
            }
            Ok(_) => {
                // ODO processing succeeded with clipping - also acceptable
            }
        }
    }

    /// AC2: API compatibility preservation during error enhancement
    /// Ensures existing error handling patterns continue to work
    #[test] // AC:33:ERROR_PATH:API_COMPATIBILITY
    fn test_error_api_compatibility() {
        let invalid_copybook = "INVALID SYNTAX";

        let result = parse_copybook(invalid_copybook);

        assert!(result.is_err(), "Invalid syntax should return error");

        let error = result.unwrap_err();

        // Validate error structure maintains existing API
        assert!(error.code.to_string().starts_with("CBKP"), "Error code should be CBKP*");
        assert!(!error.message.is_empty(), "Error message should be present");

        // Validate error can be formatted for display (existing API)
        let error_display = format!("{}", error);
        assert!(!error_display.is_empty(), "Error should format for display");

        let error_debug = format!("{:?}", error);
        assert!(!error_debug.is_empty(), "Error should format for debug");
    }

    /// AC3: Comprehensive error code coverage validation
    /// Tests that all new error handling paths use existing taxonomy
    #[test] // AC:33:ERROR_PATH:ERROR_CODE_COVERAGE
    fn test_comprehensive_error_code_coverage() {
        let error_scenarios = create_comprehensive_error_scenarios();

        for (scenario_name, test_data, expected_error_family) in error_scenarios {
            let result = execute_error_scenario(test_data);

            if let Err(error) = result {
                let error_code_str = error.code.to_string();

                assert!(
                    error_code_str.starts_with(&expected_error_family),
                    "Scenario '{}' should produce {} error, got {}",
                    scenario_name, expected_error_family, error_code_str
                );

                // Validate error provides sufficient context
                assert!(
                    error.message.len() >= 10,
                    "Scenario '{}' error message too brief: '{}'",
                    scenario_name, error.message
                );
            }
        }
    }

    /// AC8: Enhanced error context for enterprise debugging
    /// Tests that error messages provide detailed context for production debugging
    #[test] // AC:33:ERROR_PATH:ENTERPRISE_CONTEXT
    fn test_enhanced_error_context() {
        let copybook_with_errors = r#"
        01 COMPLEX-RECORD.
            05 FIELD-A PIC X(10).
            05 FIELD-B PIC 9(5)V99 COMP-3.
            05 FIELD-C REDEFINES MISSING-TARGET PIC X(15).
        "#;

        let result = parse_copybook(copybook_with_errors);

        assert!(result.is_err(), "Complex error scenario should fail");

        let error = result.unwrap_err();

        // Validate enhanced context provides debugging information
        assert!(
            error.message.contains("FIELD-C") || error.message.contains("MISSING-TARGET"),
            "Error context should reference specific field: {}", error.message
        );

        // Validate error context includes operation context
        if let Some(context) = &error.context {
            assert!(
                context.details.is_some(),
                "Error context should provide operation details"
            );
        }
    }

    // Helper functions for error scenario testing

    fn create_comprehensive_error_scenarios() -> Vec<(String, ErrorTestData, String)> {
        vec![
            (
                "Parser syntax error".to_string(),
                ErrorTestData::InvalidCopybook("INVALID SYNTAX".to_string()),
                "CBKP".to_string()
            ),
            (
                "Schema validation error".to_string(),
                ErrorTestData::InvalidCopybook("01 FIELD REDEFINES MISSING PIC X(5).".to_string()),
                "CBKS".to_string()
            ),
            (
                "Data bounds error".to_string(),
                ErrorTestData::ShortRecord {
                    copybook: "01 FIELD PIC X(10).".to_string(),
                    data_length: 5
                },
                "CBKD".to_string()
            ),
            (
                "Character conversion error".to_string(),
                ErrorTestData::InvalidCharacterData {
                    copybook: "01 FIELD PIC X(5).".to_string(),
                    invalid_bytes: vec![0x00, 0x01, 0x02, 0x03, 0x04]
                },
                "CBKC".to_string()
            ),
        ]
    }

    #[derive(Debug, Clone)]
    enum ErrorTestData {
        InvalidCopybook(String),
        ShortRecord { copybook: String, data_length: usize },
        InvalidCharacterData { copybook: String, invalid_bytes: Vec<u8> },
    }

    fn execute_error_scenario(test_data: ErrorTestData) -> Result<()> {
        match test_data {
            ErrorTestData::InvalidCopybook(copybook) => {
                parse_copybook(&copybook)?;
                Ok(())
            }
            ErrorTestData::ShortRecord { copybook, data_length } => {
                let schema = parse_copybook(&copybook)?;
                let short_data = vec![0x40; data_length];
                let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                decode_record(&schema, &short_data, &options)?;
                Ok(())
            }
            ErrorTestData::InvalidCharacterData { copybook, invalid_bytes } => {
                let schema = parse_copybook(&copybook)?;
                let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                decode_record(&schema, &invalid_bytes, &options)?;
                Ok(())
            }
        }
    }
}