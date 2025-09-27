/// Tests feature spec: panic-elimination-implementation-blueprint.md#migration-patterns-for-zero-unsafe-code-policy
/// Issue #33 - Property Tests for Random COBOL Data with Panic-Safe Guarantees
///
/// This module provides comprehensive property-based testing for panic elimination
/// using random COBOL data generation. Validates that all COBOL processing operations
/// never panic regardless of input data patterns, ensuring enterprise reliability.

#[cfg(test)]
mod property_panic_safety {
    use copybook_core::{parse_copybook, Schema};
    use copybook_codec::{decode_record, encode_record, DecodeOptions, EncodeOptions, Codepage, JsonNumberMode, RecordFormat};
    use copybook_core::{Error, ErrorCode, Result};
    use serde_json::{json, Value as JsonValue};
    use std::panic;

    // Note: In real implementation, would use proptest crate for property testing
    // For this scaffolding, we simulate property tests with deterministic random data

    /// Property: All COBOL parsing operations never panic on arbitrary input
    /// Tests parser resilience with random copybook text generation
    #[test] // AC:33:PROPERTY:PARSER_NEVER_PANICS
    fn property_parser_never_panics_on_arbitrary_input() {
        let random_copybook_inputs = generate_random_copybook_inputs(100);

        for (i, random_input) in random_copybook_inputs.iter().enumerate() {
            // Capture any panics during parsing
            let parse_result = panic::catch_unwind(|| {
                parse_copybook(random_input)
            });

            // Property: Parser should never panic, regardless of input
            assert!(
                parse_result.is_ok(),
                "Parser panicked on random input {}: '{}'", i, random_input
            );

            // If parsing doesn't panic, validate result is either Ok or structured Error
            match parse_result.unwrap() {
                Ok(schema) => {
                    // Successful parsing should produce valid schema
                    assert!(!schema.fields.is_empty() || schema.record_length >= 0,
                            "Valid schema should have fields or zero length for input {}", i);
                }
                Err(error) => {
                    // Failed parsing should use appropriate error codes
                    assert!(
                        is_valid_copybook_error_code(&error.code),
                        "Parse error {} should use valid CBK* code: {:?}", i, error.code
                    );
                }
            }
        }
    }

    /// Property: All data decoding operations never panic on arbitrary binary data
    /// Tests decoder resilience with random binary data generation
    #[test] // AC:33:PROPERTY:DECODER_NEVER_PANICS
    fn property_decoder_never_panics_on_arbitrary_data() {
        // Use a simple but realistic schema for property testing
        let test_schema = create_property_test_schema();

        let random_data_samples = generate_random_binary_data(200, test_schema.record_length);

        let decode_options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::Lossless);

        for (i, random_data) in random_data_samples.iter().enumerate() {
            // Capture any panics during decoding
            let decode_result = panic::catch_unwind(|| {
                decode_record(&test_schema, random_data, &decode_options)
            });

            // Property: Decoder should never panic, regardless of data
            assert!(
                decode_result.is_ok(),
                "Decoder panicked on random data sample {}", i
            );

            // If decoding doesn't panic, validate result is either Ok or structured Error
            match decode_result.unwrap() {
                Ok(json_value) => {
                    // Successful decoding should produce valid JSON
                    assert!(
                        json_value.is_object(),
                        "Valid decode should produce JSON object for sample {}", i
                    );
                }
                Err(error) => {
                    // Failed decoding should use appropriate error codes
                    assert!(
                        is_valid_decode_error_code(&error.code),
                        "Decode error {} should use valid CBK* code: {:?}", i, error.code
                    );
                }
            }
        }
    }

    /// Property: All encoding operations never panic on arbitrary JSON input
    /// Tests encoder resilience with random JSON data generation
    #[test] // AC:33:PROPERTY:ENCODER_NEVER_PANICS
    fn property_encoder_never_panics_on_arbitrary_json() {
        let test_schema = create_property_test_schema();

        let random_json_samples = generate_random_json_data(150);

        let encode_options = EncodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_format(RecordFormat::Fixed);

        for (i, random_json) in random_json_samples.iter().enumerate() {
            // Capture any panics during encoding
            let encode_result = panic::catch_unwind(|| {
                encode_record(&test_schema, random_json, &encode_options)
            });

            // Property: Encoder should never panic, regardless of JSON input
            assert!(
                encode_result.is_ok(),
                "Encoder panicked on random JSON sample {}: {:?}", i, random_json
            );

            // If encoding doesn't panic, validate result is either Ok or structured Error
            match encode_result.unwrap() {
                Ok(binary_data) => {
                    // Successful encoding should produce binary data
                    assert!(
                        !binary_data.is_empty() || test_schema.record_length == 0,
                        "Valid encode should produce data for sample {}", i
                    );
                }
                Err(error) => {
                    // Failed encoding should use appropriate error codes
                    assert!(
                        is_valid_encode_error_code(&error.code),
                        "Encode error {} should use valid CBK* code: {:?}", i, error.code
                    );
                }
            }
        }
    }

    /// Property: Round-trip processing never panics and maintains data consistency
    /// Tests complete pipeline resilience with random valid data
    #[test] // AC:33:PROPERTY:ROUNDTRIP_NEVER_PANICS
    fn property_roundtrip_never_panics_with_consistent_data() {
        let test_schema = create_property_test_schema();

        let valid_data_samples = generate_valid_test_data(50, &test_schema);

        let decode_options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::String);

        let encode_options = EncodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_format(RecordFormat::Fixed);

        for (i, valid_data) in valid_data_samples.iter().enumerate() {
            // Step 1: Decode (should never panic)
            let decode_result = panic::catch_unwind(|| {
                decode_record(&test_schema, valid_data, &decode_options)
            });

            assert!(
                decode_result.is_ok(),
                "Round-trip decode panicked on sample {}", i
            );

            if let Ok(Ok(json_data)) = decode_result {
                // Step 2: Encode back (should never panic)
                let encode_result = panic::catch_unwind(|| {
                    encode_record(&test_schema, &json_data, &encode_options)
                });

                assert!(
                    encode_result.is_ok(),
                    "Round-trip encode panicked on sample {}", i
                );

                if let Ok(Ok(reencoded_data)) = encode_result {
                    // Step 3: Decode again (should never panic)
                    let redecode_result = panic::catch_unwind(|| {
                        decode_record(&test_schema, &reencoded_data, &decode_options)
                    });

                    assert!(
                        redecode_result.is_ok(),
                        "Round-trip redecode panicked on sample {}", i
                    );

                    // Property: Round-trip should maintain basic consistency
                    if let Ok(Ok(redecoded_json)) = redecode_result {
                        validate_roundtrip_consistency(&json_data, &redecoded_json, i);
                    }
                }
            }
        }
    }

    /// Property: Numeric conversion operations never panic on edge cases
    /// Tests numeric processing with boundary values and invalid patterns
    #[test] // AC:33:PROPERTY:NUMERIC_NEVER_PANICS
    fn property_numeric_conversion_never_panics() {
        let numeric_test_schema = create_numeric_property_test_schema();

        let numeric_edge_cases = generate_numeric_edge_case_data(100);

        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::Lossless);

        for (i, edge_case_data) in numeric_edge_cases.iter().enumerate() {
            // Capture any panics during numeric processing
            let numeric_result = panic::catch_unwind(|| {
                decode_record(&numeric_test_schema, edge_case_data, &options)
            });

            // Property: Numeric processing should never panic
            assert!(
                numeric_result.is_ok(),
                "Numeric processing panicked on edge case {}", i
            );

            // Validate result structure
            match numeric_result.unwrap() {
                Ok(json_value) => {
                    // Successful numeric processing
                    assert!(json_value.is_object(), "Numeric result should be object");
                    validate_numeric_json_structure(&json_value);
                }
                Err(error) => {
                    // Numeric errors should use appropriate codes
                    assert!(
                        matches!(
                            error.code,
                            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE |
                            ErrorCode::CBKD411_ZONED_BAD_SIGN |
                            ErrorCode::CBKD301_RECORD_TOO_SHORT |
                            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE
                        ),
                        "Numeric error {} should use numeric error code: {:?}", i, error.code
                    );
                }
            }
        }
    }

    /// Property: Character conversion never panics across all codepages
    /// Tests character processing with various EBCDIC codepages and invalid bytes
    #[test] // AC:33:PROPERTY:CHARACTER_CONVERSION_NEVER_PANICS
    fn property_character_conversion_never_panics() {
        let character_test_schema = create_character_property_test_schema();

        let codepages = vec![
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];

        let character_test_data = generate_character_test_data(50);

        for (cp_idx, codepage) in codepages.iter().enumerate() {
            let options = DecodeOptions::new().with_codepage(*codepage);

            for (data_idx, test_data) in character_test_data.iter().enumerate() {
                // Capture any panics during character conversion
                let char_result = panic::catch_unwind(|| {
                    decode_record(&character_test_schema, test_data, &options)
                });

                // Property: Character conversion should never panic
                assert!(
                    char_result.is_ok(),
                    "Character conversion panicked on codepage {:?} with data {}", codepage, data_idx
                );

                // Validate result handling
                match char_result.unwrap() {
                    Ok(json_value) => {
                        // Successful character conversion
                        assert!(json_value.is_object(), "Character result should be object");
                        validate_character_json_fields(&json_value);
                    }
                    Err(error) => {
                        // Character conversion errors should use appropriate codes
                        assert!(
                            matches!(
                                error.code,
                                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE |
                                ErrorCode::CBKD301_RECORD_TOO_SHORT
                            ),
                            "Character error codepage {} data {} should use character error code: {:?}",
                            cp_idx, data_idx, error.code
                        );
                    }
                }
            }
        }
    }

    /// Property: ODO and REDEFINES processing never panics with complex structures
    /// Tests advanced COBOL features with random data patterns
    #[test] // AC:33:PROPERTY:ODO_REDEFINES_NEVER_PANICS
    fn property_odo_redefines_never_panics() {
        let complex_schemas = create_complex_property_test_schemas();

        for (schema_idx, schema) in complex_schemas.iter().enumerate() {
            let complex_test_data = generate_complex_structure_data(30, schema);

            let options = DecodeOptions::new()
                .with_codepage(Codepage::CP037)
                .with_json_number_mode(JsonNumberMode::String);

            for (data_idx, test_data) in complex_test_data.iter().enumerate() {
                // Capture any panics during complex structure processing
                let complex_result = panic::catch_unwind(|| {
                    decode_record(schema, test_data, &options)
                });

                // Property: Complex structure processing should never panic
                assert!(
                    complex_result.is_ok(),
                    "Complex structure processing panicked on schema {} data {}", schema_idx, data_idx
                );

                // Validate result handling
                match complex_result.unwrap() {
                    Ok(json_value) => {
                        // Successful complex processing
                        assert!(json_value.is_object(), "Complex result should be object");
                        validate_complex_json_structure(&json_value, schema_idx);
                    }
                    Err(error) => {
                        // Complex structure errors should use appropriate codes
                        assert!(
                            is_valid_complex_error_code(&error.code),
                            "Complex error schema {} data {} should use valid code: {:?}",
                            schema_idx, data_idx, error.code
                        );
                    }
                }
            }
        }
    }

    // Helper functions for property test data generation

    fn create_property_test_schema() -> Schema {
        let copybook = r#"
        01 PROPERTY-TEST-RECORD.
            05 TEXT-FIELD PIC X(10).
            05 NUMERIC-FIELD PIC 9(8).
            05 DECIMAL-FIELD PIC S9(6)V99 COMP-3.
            05 SIGNED-FIELD PIC S9(5).
        "#;

        parse_copybook(copybook).expect("Property test schema should be valid")
    }

    fn create_numeric_property_test_schema() -> Schema {
        let copybook = r#"
        01 NUMERIC-PROPERTY-TEST.
            05 PACKED-DECIMAL PIC S9(9)V99 COMP-3.
            05 ZONED-DECIMAL PIC S9(7).
            05 BINARY-FIELD PIC S9(9) COMP.
            05 DISPLAY-NUMERIC PIC 9(8).
        "#;

        parse_copybook(copybook).expect("Numeric property test schema should be valid")
    }

    fn create_character_property_test_schema() -> Schema {
        let copybook = r#"
        01 CHARACTER-PROPERTY-TEST.
            05 SHORT-TEXT PIC X(5).
            05 LONG-TEXT PIC X(50).
            05 MIXED-FIELD PIC X(20).
        "#;

        parse_copybook(copybook).expect("Character property test schema should be valid")
    }

    fn create_complex_property_test_schemas() -> Vec<Schema> {
        let complex_copybooks = vec![
            r#"
            01 ODO-TEST-RECORD.
                05 COUNT-FIELD PIC 9(3).
                05 ARRAY-FIELD OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
                    10 ITEM-DATA PIC X(5).
            "#,
            r#"
            01 REDEFINES-TEST-RECORD.
                05 BASE-FIELD PIC X(20).
                05 REDEFINED-FIELD REDEFINES BASE-FIELD.
                    10 PART-A PIC X(10).
                    10 PART-B PIC X(10).
            "#,
        ];

        complex_copybooks
            .iter()
            .filter_map(|cb| parse_copybook(cb).ok())
            .collect()
    }

    fn generate_random_copybook_inputs(count: usize) -> Vec<String> {
        let mut inputs = Vec::new();

        // Generate various invalid and edge case copybook patterns
        for i in 0..count {
            let random_input = match i % 10 {
                0 => "".to_string(), // Empty input
                1 => "INVALID SYNTAX".to_string(), // Invalid syntax
                2 => "01".to_string(), // Incomplete
                3 => "01 FIELD PIC".to_string(), // Missing PIC clause
                4 => "01 FIELD PIC X().".to_string(), // Empty PIC size
                5 => "01 FIELD PIC X(999999).".to_string(), // Excessive size
                6 => "01 FIELD REDEFINES MISSING PIC X(5).".to_string(), // Missing REDEFINES target
                7 => format!("01 FIELD-{} PIC 9({}).", i, i % 20), // Variable patterns
                8 => "01 FIELD PIC X(10) COMP COMP-3.".to_string(), // Conflicting usage
                9 => generate_nested_structure(i % 5), // Nested structures
                _ => "01 SIMPLE PIC X(5).".to_string(), // Simple valid case
            };
            inputs.push(random_input);
        }

        inputs
    }

    fn generate_random_binary_data(count: usize, record_size: usize) -> Vec<Vec<u8>> {
        let mut data_samples = Vec::new();

        for i in 0..count {
            let sample = match i % 8 {
                0 => vec![0x00; record_size], // All zeros
                1 => vec![0xFF; record_size], // All 0xFF
                2 => vec![0x40; record_size], // EBCDIC spaces
                3 => (0..record_size).map(|j| (j % 256) as u8).collect(), // Sequential
                4 => vec![0xF0; record_size], // EBCDIC zero digits
                5 => generate_mixed_binary_data(record_size), // Mixed valid/invalid
                6 => vec![], // Empty data
                7 => vec![0x40; record_size / 2], // Too short
                _ => generate_random_bytes(record_size, i), // Pseudo-random
            };
            data_samples.push(sample);
        }

        data_samples
    }

    fn generate_random_json_data(count: usize) -> Vec<JsonValue> {
        let mut json_samples = Vec::new();

        for i in 0..count {
            let sample = match i % 10 {
                0 => json!(null), // Null value
                1 => json!({}), // Empty object
                2 => json!({"FIELD": null}), // Null field
                3 => json!({"FIELD": "invalid_number"}), // Invalid number
                4 => json!({"FIELD": 999999999999999i64}), // Large number
                5 => json!({"FIELD": ""}), // Empty string
                6 => json!({"FIELD": "A".repeat(1000)}), // Long string
                7 => json!({"FIELD": [1, 2, 3]}), // Array (invalid for most fields)
                8 => json!({"FIELD": {"nested": "object"}}), // Nested object
                9 => generate_valid_test_json(i), // Valid test data
                _ => json!({"FIELD": "test"}), // Simple valid case
            };
            json_samples.push(sample);
        }

        json_samples
    }

    fn generate_valid_test_data(count: usize, schema: &Schema) -> Vec<Vec<u8>> {
        let mut valid_samples = Vec::new();

        for i in 0..count {
            let mut data = Vec::new();

            // Generate valid EBCDIC test data based on schema structure
            // This is simplified - real implementation would analyze schema fields

            // TEXT-FIELD (10 bytes)
            data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40]);

            // NUMERIC-FIELD (8 bytes)
            data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]);

            // DECIMAL-FIELD (5 bytes COMP-3)
            data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x9C]);

            // SIGNED-FIELD (5 bytes)
            data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4, 0xC5]);

            // Add some variation based on iteration
            if i % 3 == 0 {
                data[10] = 0xF9; // Modify numeric field
            }
            if i % 5 == 0 {
                data[data.len() - 1] = 0xD5; // Negative sign
            }

            valid_samples.push(data);
        }

        valid_samples
    }

    fn generate_numeric_edge_case_data(count: usize) -> Vec<Vec<u8>> {
        let mut edge_cases = Vec::new();

        for i in 0..count {
            let case = match i % 12 {
                0 => vec![0x00, 0x00, 0x00, 0x00, 0x0C, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0], // Zero values
                1 => vec![0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF], // Invalid nibbles
                2 => vec![0x99, 0x99, 0x99, 0x99, 0x9C, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4], // Maximum values
                3 => vec![0x12, 0x34, 0x56, 0x78, 0x9D, 0xF1, 0xF2, 0xF3, 0xF4, 0xC5, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6], // Negative values
                4 => vec![0x00, 0x00, 0x1C], // Too short
                5 => vec![0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56, 0x78, 0x90], // Too long
                6 => vec![0x1A, 0x2B, 0x3C, 0x4D, 0x5E], // Invalid sign nibbles
                7 => vec![0xA1, 0xB2, 0xC3, 0xD4, 0xE5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0], // Mixed invalid characters
                8 => vec![0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0], // Letters in numeric
                9 => vec![0xF1, 0xF2, 0xF3, 0xF4, 0x4F], // Invalid overpunch
                10 => vec![0xF1, 0xF2, 0xF3, 0xF4, 0xC0], // Invalid positive overpunch
                11 => generate_random_numeric_data(i), // Pseudo-random numeric
                _ => vec![0x12, 0x34, 0x5C, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8], // Valid case
            };
            edge_cases.push(case);
        }

        edge_cases
    }

    fn generate_character_test_data(count: usize) -> Vec<Vec<u8>> {
        let mut char_data = Vec::new();

        for i in 0..count {
            let data = match i % 8 {
                0 => vec![0x00; 75], // Control characters
                1 => vec![0x40; 75], // EBCDIC spaces
                2 => vec![0xFF; 75], // High bit set
                3 => (0x40..=0x4F).cycle().take(75).collect(), // Limited range
                4 => (0xC1..=0xC9).cycle().take(75).collect(), // Letters A-I
                5 => (0xF0..=0xF9).cycle().take(75).collect(), // Digits 0-9
                6 => generate_mixed_ebcdic_data(75), // Mixed valid EBCDIC
                7 => generate_invalid_ebcdic_data(75), // Invalid EBCDIC sequences
                _ => vec![0x40; 75], // Default to spaces
            };
            char_data.push(data);
        }

        char_data
    }

    fn generate_complex_structure_data(count: usize, schema: &Schema) -> Vec<Vec<u8>> {
        let mut complex_data = Vec::new();

        for i in 0..count {
            // Generate data based on schema structure
            // This is simplified - real implementation would analyze ODO fields, etc.
            let mut data = Vec::new();

            // Add basic data pattern
            data.extend_from_slice(&[0xF0; 3]); // Count field
            data.extend_from_slice(&[0x40; 50]); // Variable data

            // Add variation based on iteration
            if i % 3 == 0 {
                data[0] = 0xF1; // Count = 1
            } else if i % 3 == 1 {
                data[0] = 0xF5; // Count = 5
            } else {
                data[0] = 0xF0; // Count = 0
            }

            complex_data.push(data);
        }

        complex_data
    }

    // Additional helper functions

    fn generate_nested_structure(depth: usize) -> String {
        let mut result = String::new();
        result.push_str("01 NESTED-RECORD.\n");

        for level in 0..depth {
            let level_num = 5 + (level * 5);
            result.push_str(&format!("    {:02} LEVEL-{} PIC X({}).\n", level_num, level, level + 1));
        }

        result
    }

    fn generate_mixed_binary_data(size: usize) -> Vec<u8> {
        (0..size).map(|i| {
            match i % 4 {
                0 => 0x40, // Space
                1 => 0xF0 + (i % 10) as u8, // Digit
                2 => 0xC1 + (i % 9) as u8, // Letter
                3 => 0x00, // Control
                _ => 0x40,
            }
        }).collect()
    }

    fn generate_random_bytes(size: usize, seed: usize) -> Vec<u8> {
        // Simple pseudo-random generator for testing
        (0..size).map(|i| ((seed + i) * 17 + 42) as u8 % 256).collect()
    }

    fn generate_valid_test_json(index: usize) -> JsonValue {
        json!({
            "TEXT-FIELD": format!("Test{:04}", index),
            "NUMERIC-FIELD": format!("{:08}", index % 100000000),
            "DECIMAL-FIELD": format!("{:.2}", (index as f64) / 100.0),
            "SIGNED-FIELD": format!("{}", (index as i32) - 50000)
        })
    }

    fn generate_random_numeric_data(seed: usize) -> Vec<u8> {
        let mut data = Vec::new();
        for i in 0..26 {
            let byte = ((seed + i) * 7 + 13) as u8 % 256;
            data.push(byte);
        }
        data
    }

    fn generate_mixed_ebcdic_data(size: usize) -> Vec<u8> {
        (0..size).map(|i| {
            match i % 6 {
                0..=2 => 0x40 + (i % 32) as u8, // Printable range
                3..=4 => 0xC1 + (i % 26) as u8, // Letters
                5 => 0xF0 + (i % 10) as u8, // Digits
                _ => 0x40,
            }
        }).collect()
    }

    fn generate_invalid_ebcdic_data(size: usize) -> Vec<u8> {
        (0..size).map(|i| {
            match i % 4 {
                0 => 0x00 + (i % 32) as u8, // Control characters
                1 => 0x80 + (i % 32) as u8, // High range
                2 => 0xFF, // Invalid
                3 => 0x20 + (i % 16) as u8, // ASCII range (invalid in EBCDIC)
                _ => 0x00,
            }
        }).collect()
    }

    // Validation helper functions

    fn validate_roundtrip_consistency(original: &JsonValue, roundtrip: &JsonValue, sample_index: usize) {
        // Basic consistency checks for round-trip processing
        if original.is_object() && roundtrip.is_object() {
            let orig_obj = original.as_object().unwrap();
            let trip_obj = roundtrip.as_object().unwrap();

            assert_eq!(
                orig_obj.keys().count(),
                trip_obj.keys().count(),
                "Round-trip sample {} should preserve field count", sample_index
            );
        }
    }

    fn validate_numeric_json_structure(json_value: &JsonValue) {
        if let Some(obj) = json_value.as_object() {
            for (field_name, field_value) in obj {
                // Numeric fields should be strings or numbers
                assert!(
                    field_value.is_string() || field_value.is_number(),
                    "Numeric field {} should be string or number, got {:?}", field_name, field_value
                );
            }
        }
    }

    fn validate_character_json_fields(json_value: &JsonValue) {
        if let Some(obj) = json_value.as_object() {
            for (field_name, field_value) in obj {
                // Character fields should be strings
                assert!(
                    field_value.is_string(),
                    "Character field {} should be string, got {:?}", field_name, field_value
                );
            }
        }
    }

    fn validate_complex_json_structure(json_value: &JsonValue, schema_index: usize) {
        assert!(
            json_value.is_object(),
            "Complex structure schema {} should produce object", schema_index
        );

        // Additional validation could check for specific ODO/REDEFINES patterns
    }

    fn is_valid_copybook_error_code(code: &ErrorCode) -> bool {
        matches!(code, ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE | ErrorCode::CBKP021_ODO_NOT_TAIL)
    }

    fn is_valid_decode_error_code(code: &ErrorCode) -> bool {
        let code_str = code.to_string();
        code_str.starts_with("CBKD") || code_str.starts_with("CBKC") || code_str.starts_with("CBKS")
    }

    fn is_valid_encode_error_code(code: &ErrorCode) -> bool {
        let code_str = code.to_string();
        code_str.starts_with("CBKE") || code_str.starts_with("CBKD") || code_str.starts_with("CBKC")
    }

    fn is_valid_complex_error_code(code: &ErrorCode) -> bool {
        let code_str = code.to_string();
        code_str.starts_with("CBK") // Any valid copybook-rs error code
    }
}