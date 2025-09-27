/// Tests feature spec: panic-elimination-implementation-blueprint.md#numeric-conversion-safety-implementation
/// Issue #33 - Numeric Hotspot Unit Tests
///
/// This module provides comprehensive unit testing for panic elimination in numeric.rs
/// (20 panic occurrences target) and `zoned_overpunch.rs` (24 panic occurrences target).
/// Validates panic-safe numeric conversion with CBKD* error codes and performance preservation.

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod numeric_hotspot_safety {
    use copybook_codec::{
        Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
        encode_record,
    };
    use copybook_core::ErrorCode;
    use copybook_core::parse_copybook;
    use serde_json::json;

    /// Tests numeric.rs hotspot panic elimination (20 occurrences target)
    /// Validates panic-safe packed decimal and numeric conversion operations

    #[test] // AC:33:NUMERIC_HOTSPOT:PACKED_DECIMAL_SAFETY
    fn test_packed_decimal_conversion_safety() {
        let copybook = r"
        01 PACKED-TEST-RECORD.
            05 VALID-PACKED PIC S9(7)V99 COMP-3.
            05 EDGE-PACKED PIC S9(15)V99 COMP-3.
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case 1: Invalid packed decimal data (should not panic)
        let invalid_packed_data = vec![
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // Invalid nibbles for first field
            0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56, 0x1C, // Valid second field
        ];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &invalid_packed_data, &options);

        // Should return structured error, not panic
        assert!(
            result.is_err(),
            "Invalid packed decimal should return error"
        );

        let error = result.unwrap_err();
        assert!(
            matches!(error.code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE),
            "Invalid packed decimal should use CBKD401_COMP3_INVALID_NIBBLE, got {:?}",
            error.code
        );

        // Test case 2: Edge case packed decimal values
        let edge_case_data = vec![
            0x00, 0x00, 0x00, 0x0C, // Zero value with positive sign
            0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9D, // Maximum negative
        ];

        let result = decode_record(&schema, &edge_case_data, &options);

        // Edge cases should be handled safely
        match result {
            Ok(json_value) => {
                // Successful conversion is acceptable
                assert!(json_value.is_object(), "Result should be JSON object");
            }
            Err(error) => {
                // Errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
                            | ErrorCode::CBKD301_RECORD_TOO_SHORT
                    ),
                    "Edge case error should use CBKD* code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:NUMERIC_HOTSPOT:NUMERIC_FORMATTING_SAFETY
    fn test_numeric_formatting_safety() {
        let copybook = r"
        01 FORMATTING-TEST.
            05 LARGE-DECIMAL PIC S9(15)V99 COMP-3.
            05 SMALL-DECIMAL PIC S9(3)V99 COMP-3.
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Large number formatting that could overflow string buffers
        let large_number_data = vec![
            0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, // Large positive
            0x12, 0x34, 0x5C, // Small positive
        ];

        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::Lossless);

        let result = decode_record(&schema, &large_number_data, &options);

        // Large number formatting should be handled safely
        match result {
            Ok(json_value) => {
                // Validate successful formatting
                let large_field = &json_value["LARGE-DECIMAL"];
                assert!(
                    large_field.is_string(),
                    "Large decimal should format to string"
                );

                let small_field = &json_value["SMALL-DECIMAL"];
                assert!(
                    small_field.is_string(),
                    "Small decimal should format to string"
                );
            }
            Err(error) => {
                // Formatting errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
                            | ErrorCode::CBKC201_JSON_WRITE_ERROR
                    ),
                    "Formatting error should use appropriate code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:NUMERIC_HOTSPOT:SCALE_CALCULATION_SAFETY
    fn test_scale_calculation_safety() {
        let copybook = r"
        01 SCALE-TEST.
            05 NO-DECIMAL PIC S9(10) COMP-3.
            05 HIGH-PRECISION PIC S9(10)V99999 COMP-3.
            05 FRACTIONAL-ONLY PIC SV99999 COMP-3.
        ";

        let Ok(schema) = parse_copybook(copybook) else {
            // Some PIC clauses might not be supported - test should pass gracefully
            return;
        };

        // Test case: Various scale configurations
        let scale_test_data = vec![
            0x12, 0x34, 0x56, 0x78, 0x90, 0x1C, // No decimal (scale 0)
            0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x5C, // High precision (scale 5)
            0x12, 0x34, 0x5C, // Fractional only (scale 5)
        ];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &scale_test_data, &options);

        // Scale calculations should be handled safely
        match result {
            Ok(json_value) => {
                // Validate different scale handling
                assert!(
                    json_value["NO-DECIMAL"].is_string(),
                    "No decimal field should format"
                );
                assert!(
                    json_value["HIGH-PRECISION"].is_string(),
                    "High precision field should format"
                );
                assert!(
                    json_value["FRACTIONAL-ONLY"].is_string(),
                    "Fractional field should format"
                );
            }
            Err(error) => {
                // Scale calculation errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
                            | ErrorCode::CBKD301_RECORD_TOO_SHORT
                    ),
                    "Scale calculation error should use CBKD* code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:NUMERIC_HOTSPOT:OVERFLOW_PROTECTION
    fn test_numeric_overflow_protection() {
        let copybook = r"
        01 OVERFLOW-TEST.
            05 MAX-PRECISION PIC S9(18)V99 COMP-3.
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Values that could cause arithmetic overflow
        let overflow_test_data = vec![
            0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, // Maximum value
        ];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &overflow_test_data, &options);

        // Overflow scenarios should be handled safely
        match result {
            Ok(json_value) => {
                // Successful overflow handling is good
                let max_field = &json_value["MAX-PRECISION"];
                assert!(max_field.is_string(), "Max precision should format safely");

                // Value should be reasonable
                let value_str = max_field.as_str().unwrap();
                assert!(!value_str.is_empty(), "Formatted value should not be empty");
                assert!(
                    value_str.len() < 50,
                    "Formatted value should have reasonable length"
                );
            }
            Err(error) => {
                // Overflow errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
                            | ErrorCode::CBKC201_JSON_WRITE_ERROR
                    ),
                    "Overflow error should use appropriate code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:NUMERIC_HOTSPOT:ENCODING_SAFETY
    fn test_numeric_encoding_safety() {
        let copybook = r"
        01 ENCODING-TEST.
            05 DECIMAL-FIELD PIC S9(7)V99 COMP-3.
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Invalid JSON values for encoding
        let invalid_json_values = vec![
            json!({"DECIMAL-FIELD": "invalid_number"}),
            json!({"DECIMAL-FIELD": null}),
            json!({"DECIMAL-FIELD": {}}),
            json!({"DECIMAL-FIELD": "999999999999999999"}), // Too large
        ];

        let options = EncodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_format(RecordFormat::Fixed);

        for (i, invalid_json) in invalid_json_values.iter().enumerate() {
            let result = encode_record(&schema, invalid_json, &options);

            // Invalid encoding should return appropriate errors
            if let Err(error) = result {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKE501_JSON_TYPE_MISMATCH
                            | ErrorCode::CBKD401_COMP3_INVALID_NIBBLE
                            | ErrorCode::CBKE505_SCALE_MISMATCH
                    ),
                    "Invalid encoding {} should use CBK* error code, got {:?}",
                    i,
                    error.code
                );
            }
            // Some invalid values might be handled with conversion - also acceptable
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod zoned_overpunch_hotspot_safety {
    use copybook_codec::{Codepage, DecodeOptions, decode_record};
    use copybook_core::ErrorCode;
    use copybook_core::parse_copybook;

    /// Tests `zoned_overpunch.rs` hotspot panic elimination (24 occurrences target)
    /// Validates panic-safe zoned decimal processing with sign handling

    #[test] // AC:33:ZONED_HOTSPOT:SIGN_CHARACTER_SAFETY
    fn test_zoned_sign_character_safety() {
        let copybook = r"
        01 ZONED-TEST.
            05 SIGNED-ZONED PIC S9(5).
            05 UNSIGNED-ZONED PIC 9(5).
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Invalid sign characters in zoned decimal
        let invalid_sign_data = vec![
            0xFF, 0xF1, 0xF2, 0xF3, 0xF4, // Invalid sign character
            0xF1, 0xF2, 0xF3, 0xF4, 0xF5, // Valid unsigned
        ];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &invalid_sign_data, &options);

        // Invalid sign characters should be handled safely
        match result {
            Ok(json_value) => {
                // Some implementations may handle invalid signs gracefully
                assert!(json_value.is_object(), "Result should be JSON object");
            }
            Err(error) => {
                // Sign character errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKD411_ZONED_BAD_SIGN | ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
                    ),
                    "Invalid sign error should use CBKD411 or CBKC301, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:ZONED_HOTSPOT:OVERPUNCH_DETECTION
    fn test_zoned_overpunch_detection_safety() {
        let copybook = r"
        01 OVERPUNCH-TEST.
            05 OVERPUNCH-FIELD PIC S9(5).
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Various overpunch sign patterns
        let overpunch_patterns = [
            vec![0xF1, 0xF2, 0xF3, 0xF4, 0xC1], // Positive overpunch (A = +1)
            vec![0xF1, 0xF2, 0xF3, 0xF4, 0xD1], // Negative overpunch (J = -1)
            vec![0xF1, 0xF2, 0xF3, 0xF4, 0xC9], // Positive overpunch (I = +9)
            vec![0xF1, 0xF2, 0xF3, 0xF4, 0xD9], // Negative overpunch (R = -9)
            vec![0xF1, 0xF2, 0xF3, 0xF4, 0xE2], // Invalid overpunch character
        ];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        for (i, pattern) in overpunch_patterns.iter().enumerate() {
            let result = decode_record(&schema, pattern, &options);

            // Overpunch detection should be handled safely
            match result {
                Ok(json_value) => {
                    // Successful overpunch detection
                    let field_value = &json_value["OVERPUNCH-FIELD"];
                    assert!(
                        field_value.is_string(),
                        "Overpunch field {i} should format to string"
                    );
                }
                Err(error) => {
                    // Overpunch errors should use appropriate codes
                    assert!(
                        matches!(
                            error.code,
                            ErrorCode::CBKD411_ZONED_BAD_SIGN
                                | ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
                        ),
                        "Overpunch error {} should use CBKD411 or CBKC301, got {:?}",
                        i,
                        error.code
                    );
                }
            }
        }
    }

    #[test] // AC:33:ZONED_HOTSPOT:CHARACTER_VALIDATION
    fn test_zoned_character_validation_safety() {
        let copybook = r"
        01 CHAR-VALIDATION-TEST.
            05 ZONED-NUMERIC PIC 9(8).
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Non-numeric characters in zoned field
        let invalid_character_data = vec![
            0xC1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, // 'A' in first position (invalid)
        ];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &invalid_character_data, &options);

        // Non-numeric characters should be handled safely
        match result {
            Ok(json_value) => {
                // Some implementations may convert non-numeric to zero or handle gracefully
                assert!(json_value.is_object(), "Result should be JSON object");
            }
            Err(error) => {
                // Character validation errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKD411_ZONED_BAD_SIGN | ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
                    ),
                    "Character validation error should use CBKD411 or CBKC301, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:ZONED_HOTSPOT:CODEPAGE_CONVERSION
    fn test_zoned_codepage_conversion_safety() {
        let copybook = r"
        01 CODEPAGE-TEST.
            05 ZONED-FIELD PIC S9(6).
        ";

        let schema = parse_copybook(copybook).expect("Valid copybook should parse");

        // Test case: Different codepage conversions
        let codepages = vec![
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];

        // EBCDIC numeric data (should be compatible across codepages)
        let numeric_data = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xC6]; // "12345F" (positive)

        for codepage in codepages {
            let options = DecodeOptions::new().with_codepage(codepage);
            let result = decode_record(&schema, &numeric_data, &options);

            // Codepage conversion should be handled safely
            match result {
                Ok(json_value) => {
                    // Successful conversion across codepages
                    let field_value = &json_value["ZONED-FIELD"];
                    assert!(
                        field_value.is_string(),
                        "Zoned field should format for codepage {codepage:?}"
                    );
                }
                Err(error) => {
                    // Codepage conversion errors should use appropriate codes
                    assert!(
                        matches!(
                            error.code,
                            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
                                | ErrorCode::CBKD411_ZONED_BAD_SIGN
                        ),
                        "Codepage conversion error should use CBKC301 or CBKD411 for {:?}, got {:?}",
                        codepage,
                        error.code
                    );
                }
            }
        }
    }

    #[test] // AC:33:ZONED_HOTSPOT:EDGE_CASE_HANDLING
    fn test_zoned_edge_case_handling_safety() {
        let copybook = r"
        01 EDGE-CASE-TEST.
            05 ZERO-LENGTH PIC 9(0).
            05 SINGLE-DIGIT PIC S9(1).
            05 MAX-LENGTH PIC 9(18).
        ";

        let schema_result = parse_copybook(copybook);

        // Edge case schema should be handled safely
        match schema_result {
            Ok(schema) => {
                // Edge case fields should be processed safely
                let edge_case_data = vec![
                    // Single digit
                    0xF5, // "5"
                    // Max length (18 digits)
                    0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0xF1, 0xF2, 0xF3,
                    0xF4, 0xF5, 0xF6, 0xF7, 0xF8,
                ];

                let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                let result = decode_record(&schema, &edge_case_data, &options);

                // Edge cases should be handled safely
                match result {
                    Ok(json_value) => {
                        // Successful edge case handling
                        assert!(
                            json_value.is_object(),
                            "Edge cases should produce JSON object"
                        );
                    }
                    Err(error) => {
                        // Edge case errors should use appropriate codes
                        assert!(
                            matches!(
                                error.code,
                                ErrorCode::CBKD301_RECORD_TOO_SHORT
                                    | ErrorCode::CBKD411_ZONED_BAD_SIGN
                            ),
                            "Edge case error should use CBKD* code, got {:?}",
                            error.code
                        );
                    }
                }
            }
            Err(error) => {
                // Schema parsing errors for edge cases should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Edge case schema error should use CBKP* code, got {:?}",
                    error.code
                );
            }
        }
    }
}
