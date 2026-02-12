/// Tests feature spec: panic-elimination-api-contracts.md#comprehensive-api-surface-validation
/// Issue #33 - Integration Tests for End-to-End COBOL Processing
///
/// This module provides comprehensive integration testing for panic elimination across
/// the entire copybook-rs pipeline. Validates end-to-end COBOL processing with structured
/// error handling, enterprise scenarios, and golden fixture compatibility.


#![allow(clippy::unwrap_used, clippy::expect_used)]
#[cfg(test)]
mod integration_panic_safety {
    use copybook_core::{parse_copybook, Schema};
    use copybook_codec::{
        decode_record, encode_record, decode_file_to_jsonl,
        DecodeOptions, EncodeOptions, Codepage, JsonNumberMode, RecordFormat
    };
    use copybook_core::{Error, ErrorCode, Result};
    use serde_json::{json, Value as JsonValue};
    use std::io::Cursor;

    /// AC7: End-to-end COBOL processing with panic-safe error handling
    /// Tests complete pipeline from copybook parsing through data conversion
    #[test] // AC:33:INTEGRATION:END_TO_END_PIPELINE
    fn test_end_to_end_pipeline_safety() {
        // Test case: Complete enterprise COBOL scenario
        let enterprise_copybook = r#"
        01 CUSTOMER-RECORD.
            05 CUSTOMER-HEADER.
                10 CUSTOMER-ID PIC 9(10).
                10 RECORD-TYPE PIC X(3).
                10 STATUS-FLAGS.
                    15 ACTIVE-FLAG PIC X.
                    15 VIP-FLAG PIC X.
                    15 CREDIT-FLAG REDEFINES VIP-FLAG PIC 9.
            05 CUSTOMER-DATA.
                10 PERSONAL-INFO.
                    15 FIRST-NAME PIC X(20).
                    15 LAST-NAME PIC X(30).
                    15 BIRTH-DATE PIC 9(8).
                10 ADDRESS-INFO.
                    15 STREET PIC X(40).
                    15 CITY PIC X(25).
                    15 STATE PIC X(2).
                    15 ZIP PIC 9(5).
            05 ACCOUNT-SECTION OCCURS 1 TO 5 TIMES DEPENDING ON CUSTOMER-ID.
                10 ACCOUNT-NUMBER PIC 9(12).
                10 ACCOUNT-TYPE PIC X(10).
                10 BALANCE PIC S9(10)V99 COMP-3.
                10 LAST-ACTIVITY PIC 9(8).
        "#;

        // Step 1: Parse copybook (should be panic-safe)
        let schema_result = parse_copybook(enterprise_copybook);

        match schema_result {
            Ok(schema) => {
                // Step 2: Create test data for decoding
                let test_data = create_enterprise_test_data();

                let decode_options = DecodeOptions::new()
                    .with_codepage(Codepage::CP037)
                    .with_json_number_mode(JsonNumberMode::Lossless)
                    .with_emit_meta(true);

                // Step 3: Decode data (should be panic-safe)
                let decode_result = decode_record(&schema, &test_data, &decode_options);

                match decode_result {
                    Ok(json_data) => {
                        // Step 4: Validate decoded data structure
                        assert!(json_data.is_object(), "Decoded data should be JSON object");
                        assert!(json_data["CUSTOMER-HEADER"].is_object(), "Header section should exist");
                        assert!(json_data["CUSTOMER-DATA"].is_object(), "Data section should exist");

                        // Step 5: Encode data back (round-trip test)
                        let encode_options = EncodeOptions::new()
                            .with_codepage(Codepage::CP037)
                            .with_format(RecordFormat::Fixed);

                        let encode_result = encode_record(&schema, &json_data, &encode_options);

                        match encode_result {
                            Ok(encoded_data) => {
                                // Step 6: Validate round-trip consistency
                                assert!(!encoded_data.is_empty(), "Encoded data should not be empty");
                                assert!(encoded_data.len() >= test_data.len(), "Encoded data should have reasonable length");

                                // Step 7: Decode again to verify consistency
                                let redecode_result = decode_record(&schema, &encoded_data, &decode_options);

                                match redecode_result {
                                    Ok(redecoded_data) => {
                                        // Validate round-trip consistency
                                        validate_json_consistency(&json_data, &redecoded_data);
                                    }
                                    Err(error) => {
                                        // Redecode errors should use appropriate codes
                                        assert!(
                                            is_valid_error_code(&error.code),
                                            "Redecode error should use valid code: {:?}", error.code
                                        );
                                    }
                                }
                            }
                            Err(error) => {
                                // Encode errors should use appropriate codes
                                assert!(
                                    matches!(
                                        error.code,
                                        ErrorCode::CBKE501_TYPE_MISMATCH |
                                        ErrorCode::CBKE502_OUT_OF_BOUNDS |
                                        ErrorCode::CBKC201_JSON_WRITE_ERROR
                                    ),
                                    "Encode error should use CBK* code, got {:?}", error.code
                                );
                            }
                        }
                    }
                    Err(error) => {
                        // Decode errors should use appropriate codes
                        assert!(
                            is_valid_error_code(&error.code),
                            "Decode error should use valid code: {:?}", error.code
                        );
                    }
                }
            }
            Err(error) => {
                // Parse errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX |
                        ErrorCode::CBKP021_ODO_NOT_TAIL |
                        ErrorCode::CBKS121_COUNTER_NOT_FOUND
                    ),
                    "Parse error should use CBK* code, got {:?}", error.code
                );
            }
        }
    }

    /// AC7: File processing integration with panic-safe error handling
    /// Tests large-scale file processing scenarios
    #[test] // AC:33:INTEGRATION:FILE_PROCESSING_SAFETY
    fn test_file_processing_integration_safety() {
        let file_processing_copybook = r#"
        01 TRANSACTION-RECORD.
            05 TRANSACTION-ID PIC 9(15).
            05 TIMESTAMP PIC 9(14).
            05 AMOUNT PIC S9(10)V99 COMP-3.
            05 CURRENCY PIC X(3).
            05 DESCRIPTION PIC X(50).
        "#;

        let schema = match parse_copybook(file_processing_copybook) {
            Ok(s) => s,
            Err(error) => {
                assert!(
                    matches!(error.code, ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE),
                    "File processing schema error should use CBKP* code, got {:?}", error.code
                );
                return; // Skip rest of test if schema parsing fails
            }
        };

        // Create multi-record test data
        let multi_record_data = create_multi_record_test_data(10); // 10 records

        let input = Cursor::new(multi_record_data);
        let mut output = Cursor::new(Vec::new());

        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::String);

        // Test file processing (should be panic-safe)
        let processing_result = decode_file_to_jsonl(&schema, input, &mut output, &options);

        match processing_result {
            Ok(summary) => {
                // Validate successful file processing
                assert!(summary.records_processed > 0, "Should process multiple records");
                assert!(summary.bytes_processed > 0, "Should process bytes");
                assert_eq!(summary.records_failed, 0, "All records should succeed in valid test");

                // Validate output contains JSON lines
                let output_data = output.into_inner();
                let output_str = String::from_utf8_lossy(&output_data);
                let lines: Vec<&str> = output_str.lines().collect();
                assert!(!lines.is_empty(), "Output should contain JSON lines");

                // Validate each line is valid JSON
                for (i, line) in lines.iter().enumerate() {
                    if !line.trim().is_empty() {
                        let json_result: serde_json::Result<JsonValue> = serde_json::from_str(line);
                        assert!(json_result.is_ok(), "Line {} should be valid JSON: {}", i, line);
                    }
                }
            }
            Err(error) => {
                // File processing errors should use appropriate codes
                assert!(
                    is_valid_error_code(&error.code),
                    "File processing error should use valid code: {:?}", error.code
                );
            }
        }
    }

    /// AC7: Enterprise scenario integration with complex COBOL features
    /// Tests advanced COBOL features under panic elimination
    #[test] // AC:33:INTEGRATION:ENTERPRISE_SCENARIO_SAFETY
    fn test_enterprise_scenario_integration_safety() {
        let complex_enterprise_copybook = r#"
        01 INSURANCE-CLAIM-RECORD.
            05 CLAIM-HEADER.
                10 CLAIM-NUMBER PIC 9(12).
                10 POLICY-NUMBER PIC X(15).
                10 CLAIM-TYPE PIC X(5).
                10 STATUS-CODE PIC 9(2).
            05 CLAIMANT-INFO.
                10 CLAIMANT-ID PIC 9(10).
                10 CLAIMANT-NAME PIC X(40).
                10 CONTACT-INFO.
                    15 PHONE PIC 9(10).
                    15 EMAIL PIC X(50).
                    15 ADDRESS.
                        20 STREET PIC X(50).
                        20 CITY PIC X(30).
                        20 STATE PIC X(2).
                        20 ZIP PIC 9(9).
            05 FINANCIAL-DATA.
                10 CLAIM-AMOUNT PIC S9(12)V99 COMP-3.
                10 DEDUCTIBLE PIC S9(8)V99 COMP-3.
                10 COVERAGE-LIMITS.
                    15 MAX-COVERAGE PIC S9(12)V99 COMP-3.
                    15 REMAINING-COVERAGE PIC S9(12)V99 COMP-3.
            05 INCIDENT-DETAILS OCCURS 1 TO 10 TIMES DEPENDING ON STATUS-CODE.
                10 INCIDENT-DATE PIC 9(8).
                10 INCIDENT-TYPE PIC X(20).
                10 INCIDENT-AMOUNT PIC S9(10)V99 COMP-3.
                10 INCIDENT-DESCRIPTION PIC X(100).
            05 ADJUSTER-NOTES.
                10 NOTE-COUNT PIC 9(3).
                10 NOTES OCCURS 1 TO 50 TIMES DEPENDING ON NOTE-COUNT.
                    15 NOTE-DATE PIC 9(8).
                    15 NOTE-TEXT PIC X(200).
        "#;

        // Test complex enterprise scenario
        let schema_result = parse_copybook(complex_enterprise_copybook);

        match schema_result {
            Ok(schema) => {
                // Validate complex schema parsing
                assert!(!schema.fields.is_empty(), "Complex schema should have fields");
                assert!(schema_record_length(&schema) > 0, "Complex schema should have length");

                // Test with enterprise-scale data
                let enterprise_data = create_complex_enterprise_data();

                let options = DecodeOptions::new()
                    .with_codepage(Codepage::CP037)
                    .with_json_number_mode(JsonNumberMode::Lossless)
                    .with_emit_meta(true);

                let decode_result = decode_record(&schema, &enterprise_data, &options);

                match decode_result {
                    Ok(json_data) => {
                        // Validate complex data structure
                        assert!(json_data["CLAIM-HEADER"].is_object(), "Claim header should exist");
                        assert!(json_data["CLAIMANT-INFO"].is_object(), "Claimant info should exist");
                        assert!(json_data["FINANCIAL-DATA"].is_object(), "Financial data should exist");

                        // Validate ODO arrays are handled
                        if json_data["INCIDENT-DETAILS"].is_array() {
                            let incidents = json_data["INCIDENT-DETAILS"].as_array().unwrap();
                            assert!(!incidents.is_empty(), "Should have incident details");
                        }

                        // Test enterprise performance under load
                        for _ in 0..100 {
                            let load_result = decode_record(&schema, &enterprise_data, &options);
                            match load_result {
                                Ok(_) => {
                                    // Successful load processing
                                }
                                Err(error) => {
                                    // Load errors should use appropriate codes
                                    assert!(
                                        is_valid_error_code(&error.code),
                                        "Load test error should use valid code: {:?}", error.code
                                    );
                                    break; // Stop load test on error
                                }
                            }
                        }
                    }
                    Err(error) => {
                        // Complex decode errors should use appropriate codes
                        assert!(
                            is_valid_error_code(&error.code),
                            "Complex decode error should use valid code: {:?}", error.code
                        );
                    }
                }
            }
            Err(error) => {
                // Complex parsing errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX |
                        ErrorCode::CBKP021_ODO_NOT_TAIL |
                        ErrorCode::CBKS121_COUNTER_NOT_FOUND
                    ),
                    "Complex parse error should use CBK* code, got {:?}", error.code
                );
            }
        }
    }

    fn schema_record_length(schema: &Schema) -> u32 {
        schema
            .lrecl_fixed
            .unwrap_or_else(|| {
                schema
                    .all_fields()
                    .iter()
                    .map(|field| field.offset + field.len)
                    .max()
                    .unwrap_or(0)
            })
    }

    /// AC7: Golden fixtures integration with panic elimination
    /// Tests compatibility with existing golden fixture framework
    #[test] // AC:33:INTEGRATION:GOLDEN_FIXTURES_COMPATIBILITY
    fn test_golden_fixtures_compatibility_safety() {
        // Test basic golden fixture pattern with panic safety
        let golden_fixture_copybook = r#"
        01 GOLDEN-TEST-RECORD.
            05 HEADER-SECTION.
                10 RECORD-ID PIC 9(8).
                10 VERSION PIC 9(2).
            05 DATA-SECTION.
                10 STRING-FIELD PIC X(20).
                10 NUMERIC-FIELD PIC 9(10).
                10 DECIMAL-FIELD PIC S9(8)V99 COMP-3.
            05 FLAGS-SECTION.
                10 STATUS-FLAGS PIC X(8).
                10 OPTION-FLAGS REDEFINES STATUS-FLAGS.
                    15 OPTION-A PIC X.
                    15 OPTION-B PIC X.
                    15 OPTION-C PIC X.
                    15 FILLER PIC X(5).
        "#;

        let schema = match parse_copybook(golden_fixture_copybook) {
            Ok(s) => s,
            Err(error) => {
                assert!(
                    matches!(error.code, ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE),
                    "Golden fixture schema error should use CBKP* code, got {:?}", error.code
                );
                return;
            }
        };

        // Test golden fixture data patterns
        let golden_test_data = create_golden_fixture_test_data();

        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::String);

        let decode_result = decode_record(&schema, &golden_test_data, &options);

        match decode_result {
            Ok(json_data) => {
                // Validate golden fixture output format
                assert!(json_data.is_object(), "Golden fixture should produce object");

                // Validate required sections exist
                assert!(json_data["HEADER-SECTION"].is_object(), "Header section should exist");
                assert!(json_data["DATA-SECTION"].is_object(), "Data section should exist");
                assert!(json_data["FLAGS-SECTION"].is_object(), "Flags section should exist");

                // Validate REDEFINES handling
                if json_data["OPTION-FLAGS"].is_object() {
                    let option_flags = &json_data["OPTION-FLAGS"];
                    assert!(option_flags["OPTION-A"].is_string(), "REDEFINES option A should exist");
                    assert!(option_flags["OPTION-B"].is_string(), "REDEFINES option B should exist");
                }

                // Test deterministic output (golden fixture requirement)
                let second_decode = decode_record(&schema, &golden_test_data, &options);
                match second_decode {
                    Ok(second_json) => {
                        validate_json_consistency(&json_data, &second_json);
                    }
                    Err(error) => {
                        assert!(
                            is_valid_error_code(&error.code),
                            "Second decode error should use valid code: {:?}", error.code
                        );
                    }
                }
            }
            Err(error) => {
                // Golden fixture errors should use appropriate codes
                assert!(
                    is_valid_error_code(&error.code),
                    "Golden fixture error should use valid code: {:?}", error.code
                );
            }
        }
    }

    /// AC10: Memory safety integration testing
    /// Tests that panic elimination preserves memory safety guarantees
    #[test] // AC:33:INTEGRATION:MEMORY_SAFETY_PRESERVATION
    fn test_memory_safety_integration() {
        let memory_test_copybook = r#"
        01 MEMORY-TEST-RECORD.
            05 LARGE-BUFFER PIC X(1000).
            05 NUMERIC-ARRAY OCCURS 100 TIMES.
                10 ARRAY-ITEM PIC S9(9)V99 COMP-3.
        "#;

        let schema = match parse_copybook(memory_test_copybook) {
            Ok(s) => s,
            Err(error) => {
                assert!(
                    matches!(error.code, ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE),
                    "Memory test schema error should use CBKP* code, got {:?}", error.code
                );
                return;
            }
        };

        // Test memory safety with large data
        let large_test_data = create_large_memory_test_data(1000 + 100 * 6); // Buffer + array size

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        // Test multiple iterations to stress memory management
        for iteration in 0..50 {
            let decode_result = decode_record(&schema, &large_test_data, &options);

            match decode_result {
                Ok(json_data) => {
                    // Validate memory safety is maintained
                    assert!(json_data.is_object(), "Iteration {} should produce object", iteration);

                    // Validate large buffer handling
                    assert!(json_data["LARGE-BUFFER"].is_string(), "Large buffer should be string");

                    // Validate array handling
                    if json_data["NUMERIC-ARRAY"].is_array() {
                        let array = json_data["NUMERIC-ARRAY"].as_array().unwrap();
                        assert_eq!(array.len(), 100, "Array should have 100 items");
                    }
                }
                Err(error) => {
                    // Memory test errors should use appropriate codes
                    assert!(
                        is_valid_error_code(&error.code),
                        "Memory test error iteration {} should use valid code: {:?}", iteration, error.code
                    );
                    break; // Stop on error
                }
            }
        }
    }

    // Helper functions for integration testing

    fn create_enterprise_test_data() -> Vec<u8> {
        // Create realistic enterprise test data
        let mut data = Vec::new();

        // CUSTOMER-ID (10 digits)
        data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7]);

        // RECORD-TYPE (3 chars)
        data.extend_from_slice(&[0xC3, 0xE4, 0xE2]); // "CUS"

        // STATUS-FLAGS
        data.extend_from_slice(&[0xE8, 0xD5]); // "YN"

        // PERSONAL-INFO (20 + 30 + 8 chars)
        data.extend_from_slice(&[0x40; 58]); // Padded with spaces

        // ADDRESS-INFO (40 + 25 + 2 + 5 chars)
        data.extend_from_slice(&[0x40; 72]); // Padded with spaces

        // ACCOUNT-SECTION (assume 1 occurrence)
        data.extend_from_slice(&[0xF0; 12]); // Account number
        data.extend_from_slice(&[0x40; 10]); // Account type
        data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x9C]); // Balance (COMP-3)
        data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF4, 0xF0, 0xF1, 0xF0, 0xF1]); // Date

        data
    }

    fn create_multi_record_test_data(record_count: usize) -> Vec<u8> {
        let mut data = Vec::new();

        for i in 0..record_count {
            // TRANSACTION-ID (15 digits)
            let id_bytes = format!("{:015}", i).bytes()
                .map(|b| b - b'0' + 0xF0) // Convert ASCII to EBCDIC digits
                .collect::<Vec<u8>>();
            data.extend_from_slice(&id_bytes);

            // TIMESTAMP (14 digits)
            data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF4, 0xF0, 0xF1, 0xF0, 0xF1, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6]);

            // AMOUNT (COMP-3)
            data.extend_from_slice(&[0x12, 0x34, 0x56, 0x7C]); // $123456.78

            // CURRENCY (3 chars)
            data.extend_from_slice(&[0xE4, 0xE2, 0xC4]); // "USD"

            // DESCRIPTION (50 chars)
            data.extend_from_slice(&[0x40; 50]); // Padded with spaces
        }

        data
    }

    fn create_complex_enterprise_data() -> Vec<u8> {
        // Create complex enterprise data with realistic patterns
        let mut data = Vec::new();

        // CLAIM-HEADER section
        data.extend_from_slice(&[0xF1; 12]); // Claim number
        data.extend_from_slice(&[0x40; 15]); // Policy number
        data.extend_from_slice(&[0x40; 5]);  // Claim type
        data.extend_from_slice(&[0xF0, 0xF1]); // Status code (01)

        // CLAIMANT-INFO section
        data.extend_from_slice(&[0xF0; 10]); // Claimant ID
        data.extend_from_slice(&[0x40; 40]); // Name
        data.extend_from_slice(&[0xF0; 10]); // Phone
        data.extend_from_slice(&[0x40; 50]); // Email
        data.extend_from_slice(&[0x40; 112]); // Address (50+30+2+9+padding)

        // FINANCIAL-DATA section
        data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x3C]); // Claim amount
        data.extend_from_slice(&[0x50, 0x0C]); // Deductible
        data.extend_from_slice(&[0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C]); // Max coverage
        data.extend_from_slice(&[0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0C]); // Remaining

        // INCIDENT-DETAILS (1 occurrence based on status-code = 01)
        data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF4, 0xF0, 0xF1, 0xF0, 0xF1]); // Date
        data.extend_from_slice(&[0x40; 20]); // Type
        data.extend_from_slice(&[0x10, 0x00, 0x0C]); // Amount
        data.extend_from_slice(&[0x40; 100]); // Description

        data
    }

    fn create_golden_fixture_test_data() -> Vec<u8> {
        let mut data = Vec::new();

        // HEADER-SECTION
        data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF4, 0xF0, 0xF1, 0xF0, 0xF1]); // Record ID
        data.extend_from_slice(&[0xF0, 0xF1]); // Version

        // DATA-SECTION
        data.extend_from_slice(&[0x40; 20]); // String field
        data.extend_from_slice(&[0xF0; 10]); // Numeric field
        data.extend_from_slice(&[0x12, 0x34, 0x5C]); // Decimal field

        // FLAGS-SECTION
        data.extend_from_slice(&[0xE8, 0xD5, 0xC1, 0x40, 0x40, 0x40, 0x40, 0x40]); // Status flags

        data
    }

    fn create_large_memory_test_data(size: usize) -> Vec<u8> {
        vec![0x40; size] // Fill with EBCDIC spaces
    }

    fn validate_json_consistency(first: &JsonValue, second: &JsonValue) {
        // Basic consistency check for deterministic behavior
        assert_eq!(
            first.to_string().len(),
            second.to_string().len(),
            "JSON output should have consistent length"
        );

        // More detailed comparison could be added here
        if first.is_object() && second.is_object() {
            let first_obj = first.as_object().unwrap();
            let second_obj = second.as_object().unwrap();

            assert_eq!(
                first_obj.keys().count(),
                second_obj.keys().count(),
                "JSON objects should have same number of keys"
            );
        }
    }

    fn is_valid_error_code(code: &ErrorCode) -> bool {
        // Validate that error code follows copybook-rs taxonomy
        let code_str = code.to_string();
        code_str.starts_with("CBKP") ||
        code_str.starts_with("CBKS") ||
        code_str.starts_with("CBKD") ||
        code_str.starts_with("CBKC") ||
        code_str.starts_with("CBKE") ||
        code_str.starts_with("CBKF")
    }
}
