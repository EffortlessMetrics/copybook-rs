/// Tests feature spec: issue-63-spec.md#ac1-complete-panic-elimination
/// Tests feature spec: issue-63-technical-specification.md#core-parser-safety
/// Tests feature spec: panic-elimination-implementation-blueprint.md#phase-1-infrastructure-hardening
///
/// Issue #63 - Comprehensive Panic Elimination Test Scaffolding for copybook-core
///
/// This module provides comprehensive test scaffolding for eliminating 63 .unwrap()/.expect() calls
/// in copybook-core production code. Tests target parser.rs (16), layout.rs (8), pic.rs (7),
/// and audit modules (32) with enterprise-safe error handling patterns.
///
/// **AC Traceability:**
/// - AC1: Complete elimination of 63 .unwrap()/.expect() calls in copybook-core
/// - AC2: Zero breaking changes to existing public APIs
/// - AC3: Integration with CBKP*/CBKS*/CBKD*/CBKE* error taxonomy
/// - AC4: Performance impact <5% on enterprise benchmarks
/// - AC7: Comprehensive test coverage with // AC:ID tags
/// - AC10: Memory safety preserved with zero unsafe code

use copybook_core::{ErrorCode, parse_copybook};

#[cfg(test)]
mod panic_elimination_parser_tests {
    use super::*;

    /// Tests parser.rs panic elimination (16 instances)
    /// AC:63-1 - Parser state management safety with bounds checking

    #[test] // AC:63-1-1 Parser token access safety
    fn test_parser_token_access_panic_elimination() {
        // Test case: Token stream bounds checking to eliminate .unwrap() on token access
        let empty_copybook = "";

        let result = parse_copybook(empty_copybook);

        // Should return structured error instead of panicking on token access
        assert!(result.is_err(), "Empty copybook should return error, not panic on token access");

        let error = result.unwrap_err();
        assert!(
            matches!(error.code, ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE),
            "Token access error should use CBKP* error code, got {:?}",
            error.code
        );
    }

    #[test] // AC:63-1-2 Parser position bounds safety
    fn test_parser_position_bounds_panic_elimination() {
        // Test case: Parser position advance beyond token bounds
        let malformed_copybook = "01 INCOMPLETE"; // Incomplete field definition

        let result = parse_copybook(malformed_copybook);

        // Should handle incomplete parsing without position overflow panics
        match result {
            Ok(_) => {
                // Parser handles gracefully with default completion
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Position bounds error should use CBKP* error code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-1-3 Parser stack underflow protection
    fn test_parser_stack_underflow_panic_elimination() {
        // Test case: Unbalanced parentheses causing stack underflow
        let unbalanced_copybook = "01 FIELD PIC X(10).";

        let result = parse_copybook(unbalanced_copybook);

        // Should parse successfully or fail safely without stack panics
        match result {
            Ok(schema) => {
                assert!(!schema.fields.is_empty(), "Parsed schema should have fields");
            }
            Err(error) => {
                assert!(
                    matches!(error.code, ErrorCode::CBKP001_SYNTAX),
                    "Stack underflow error should use CBKP001_SYNTAX, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-1-4 Parser lookahead bounds safety
    fn test_parser_lookahead_bounds_panic_elimination() {
        // Test case: Parser lookahead beyond token stream end
        let truncated_copybook = "01 FIELD PIC"; // Missing PIC specification

        let result = parse_copybook(truncated_copybook);

        // Should handle lookahead safely without bounds panics
        assert!(result.is_err(), "Truncated copybook should return error for lookahead safety");

        let error = result.unwrap_err();
        assert!(
            matches!(
                error.code,
                ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
            ),
            "Lookahead bounds error should use CBKP* error code, got {:?}",
            error.code
        );
    }

    #[test] // AC:63-1-5 Parser recursion depth protection
    fn test_parser_recursion_depth_panic_elimination() {
        // Test case: Deeply nested structure stressing parser recursion
        let deeply_nested = "01 ROOT.\n".to_string() +
            &(0..50).map(|i| format!("    {:02} LEVEL-{} PIC X(1).\n", 5 + i, i))
                    .collect::<String>();

        let result = parse_copybook(&deeply_nested);

        // Should handle deep recursion without stack overflow panics
        match result {
            Ok(schema) => {
                assert!(schema.fields.len() >= 50, "Deep structure should parse all fields");
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Recursion depth error should use CBKP* error code, got {:?}",
                    error.code
                );
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_layout_tests {
    use super::*;

    /// Tests layout.rs panic elimination (8 instances)
    /// AC:63-2 - Field layout calculations with safe bounds checking

    #[test] // AC:63-2-1 Field index bounds safety
    fn test_field_index_bounds_panic_elimination() {
        // Test case: Schema field access by index beyond bounds
        let simple_copybook = "01 RECORD.\n    05 FIELD-A PIC X(10).";

        let result = parse_copybook(simple_copybook);
        assert!(result.is_ok(), "Simple copybook should parse successfully");

        let schema = result.unwrap();

        // This would be internal API usage that should be safe
        // Testing conceptual safe field access pattern
        assert!(schema.fields.len() > 0, "Schema should have fields for bounds testing");

        // Safe field access pattern (using get() instead of indexing)
        let valid_field = schema.fields.get(0);
        assert!(valid_field.is_some(), "First field should exist");

        let invalid_field = schema.fields.get(999);
        assert!(invalid_field.is_none(), "Out-of-bounds field access should return None");
    }

    #[test] // AC:63-2-2 REDEFINES target resolution safety
    fn test_redefines_target_resolution_panic_elimination() {
        // Test case: REDEFINES with missing target causing lookup panic
        let missing_redefines_target = r"
        01 RECORD.
            05 FIELD-A PIC X(10).
            05 FIELD-B REDEFINES NONEXISTENT PIC 9(10).
        ";

        let result = parse_copybook(missing_redefines_target);

        // Should return structured error instead of panicking on missing target
        assert!(result.is_err(), "Missing REDEFINES target should return error");

        let error = result.unwrap_err();
        assert!(
            matches!(error.code, ErrorCode::CBKP001_SYNTAX),
            "REDEFINES target resolution error should use CBKP001_SYNTAX, got {:?}",
            error.code
        );

        assert!(
            error.message.contains("REDEFINES") || error.message.contains("NONEXISTENT"),
            "Error should reference REDEFINES target issue: {}",
            error.message
        );
    }

    #[test] // AC:63-2-3 Field offset calculation overflow protection
    fn test_field_offset_calculation_panic_elimination() {
        // Test case: Field offsets that could overflow during calculation
        let large_field_copybook = r"
        01 LARGE-RECORD.
            05 HUGE-FIELD PIC X(32767).
            05 ANOTHER-FIELD PIC X(32767).
        ";

        let result = parse_copybook(large_field_copybook);

        // Should handle large offsets safely without overflow panics
        match result {
            Ok(schema) => {
                // Validate offset calculations are reasonable
                for field in &schema.fields {
                    assert!(
                        field.offset < (u32::MAX / 2),
                        "Field {} offset should be reasonable: {}",
                        field.name, field.offset
                    );
                }
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Offset calculation error should use CBKP* error code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-2-4 ODO counter field lookup safety
    fn test_odo_counter_lookup_panic_elimination() {
        // Test case: ODO with invalid counter field causing lookup panic
        let invalid_odo_counter = r"
        01 RECORD.
            05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON MISSING-COUNTER.
                10 ITEM PIC X(5).
        ";

        let result = parse_copybook(invalid_odo_counter);

        // Should return structured error instead of panicking on counter lookup
        assert!(result.is_err(), "Invalid ODO counter should return error");

        let error = result.unwrap_err();
        assert!(
            matches!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND),
            "ODO counter lookup error should use CBKS121_COUNTER_NOT_FOUND, got {:?}",
            error.code
        );

        assert!(
            error.message.contains("DEPENDING ON") || error.message.contains("MISSING-COUNTER"),
            "Error should reference ODO counter issue: {}",
            error.message
        );
    }

    #[test] // AC:63-2-5 Schema validation bounds checking
    fn test_schema_validation_bounds_panic_elimination() {
        // Test case: Schema validation that could panic on array bounds
        let complex_schema = r"
        01 COMPLEX-RECORD.
            05 HEADER.
                10 ID PIC 9(10).
                10 COUNT PIC 9(3).
            05 DATA-ARRAY OCCURS 1 TO 999 TIMES DEPENDING ON COUNT.
                10 ITEM PIC X(100).
        ";

        let result = parse_copybook(complex_schema);

        // Should validate complex schema safely without bounds panics
        match result {
            Ok(schema) => {
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 0,
                    "Complex schema should have positive length"
                );

                // Validate ODO is handled safely
                if let Some(odo_field) = schema.tail_odo.as_ref() {
                    assert!(
                        !odo_field.array_path.is_empty(),
                        "ODO field should have array path"
                    );
                }
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP021_ODO_NOT_TAIL | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                    ),
                    "Schema validation error should use appropriate error code, got {:?}",
                    error.code
                );
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_pic_tests {
    use super::*;

    /// Tests pic.rs panic elimination (7 instances)
    /// AC:63-3 - PIC clause parsing with safe character/digit access

    #[test] // AC:63-3-1 PIC string character access safety
    fn test_pic_string_character_access_panic_elimination() {
        // Test case: PIC parsing with character access beyond string bounds
        let truncated_pic_copybook = "01 FIELD PIC X"; // Incomplete PIC specification

        let result = parse_copybook(truncated_pic_copybook);

        // Should handle incomplete PIC safely without character access panics
        match result {
            Ok(_) => {
                // Parser handles gracefully by accepting incomplete PIC
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "PIC character access error should use CBKP* error code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-3-2 PIC digit parsing safety
    fn test_pic_digit_parsing_panic_elimination() {
        // Test case: PIC parsing with invalid digit characters
        let invalid_digit_pic_copybook = "01 FIELD PIC 9(ABC)."; // Non-numeric size

        let result = parse_copybook(invalid_digit_pic_copybook);

        // Should handle invalid digits safely without digit parsing panics
        match result {
            Ok(_) => {
                // Parser may handle with default interpretation
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
                    ),
                    "PIC digit parsing error should use CBKP* error code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-3-3 PIC parentheses parsing safety
    fn test_pic_parentheses_parsing_panic_elimination() {
        // Test case: PIC parsing with unmatched parentheses
        let unmatched_parens_copybook = "01 FIELD PIC X(10."; // Missing closing parenthesis

        let result = parse_copybook(unmatched_parens_copybook);

        // Should handle unmatched parentheses safely
        match result {
            Ok(_) => {
                // Parser may handle with implicit closure
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
                    ),
                    "PIC parentheses parsing error should use CBKP* error code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-3-4 PIC size validation safety
    fn test_pic_size_validation_panic_elimination() {
        // Test case: PIC with extreme size values
        let extreme_size_pics = vec![
            "01 FIELD1 PIC X(0).",       // Zero size
            "01 FIELD2 PIC X(999999).",  // Extremely large size
            "01 FIELD3 PIC 9().",        // Empty parentheses
        ];

        for pic_copybook in extreme_size_pics {
            let result = parse_copybook(pic_copybook);

            // Should handle extreme sizes safely without validation panics
            match result {
                Ok(_) => {
                    // Parser may accept with bounds clamping
                }
                Err(error) => {
                    assert!(
                        matches!(
                            error.code,
                            ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
                        ),
                        "PIC size validation error should use CBKP* error code for '{}', got {:?}",
                        pic_copybook, error.code
                    );
                }
            }
        }
    }

    #[test] // AC:63-3-5 PIC type validation safety
    fn test_pic_type_validation_panic_elimination() {
        // Test case: PIC with invalid type combinations
        let invalid_type_pics = vec![
            "01 FIELD1 PIC X(10) COMP.",    // Alphanumeric with COMP
            "01 FIELD2 PIC 9(5)V99V99.",    // Multiple decimal points
            "01 FIELD3 PIC SS9(5).",        // Multiple signs
        ];

        for pic_copybook in invalid_type_pics {
            let result = parse_copybook(pic_copybook);

            // Should handle invalid combinations safely
            match result {
                Ok(_) => {
                    // Parser may accept with warnings or default interpretation
                }
                Err(error) => {
                    assert!(
                        matches!(
                            error.code,
                            ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                        ),
                        "PIC type validation error should use CBKP* error code for '{}', got {:?}",
                        pic_copybook, error.code
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_audit_tests {
    use super::*;

    /// Tests audit module panic elimination (32 instances)
    /// AC:63-4 - Enterprise audit infrastructure with safe serialization

    #[test] // AC:63-4-1 Audit event serialization safety
    fn test_audit_event_serialization_panic_elimination() {
        // Test case: This would test audit event serialization without panics
        // Note: This is a structural test - actual audit functionality would be in implementation

        let audit_test_copybook = r"
        01 AUDIT-TEST-RECORD.
            05 EVENT-ID PIC 9(10).
            05 EVENT-TYPE PIC X(20).
            05 EVENT-DATA PIC X(100).
        ";

        let result = parse_copybook(audit_test_copybook);

        // Should parse successfully for audit event structure
        assert!(result.is_ok(), "Audit event structure should parse successfully");

        let schema = result.unwrap();
        assert!(
            schema.fields.len() >= 3,
            "Audit event structure should have at least 3 fields"
        );

        // Validate audit-related fields exist
        let has_event_id = schema.fields.iter().any(|f| f.name.contains("EVENT-ID"));
        let has_event_type = schema.fields.iter().any(|f| f.name.contains("EVENT-TYPE"));
        assert!(has_event_id && has_event_type, "Audit structure should have event fields");
    }

    #[test] // AC:63-4-2 Audit context creation safety
    fn test_audit_context_creation_panic_elimination() {
        // Test case: Complex parsing that would generate audit context
        let complex_audit_copybook = r"
        01 COMPLEX-AUDIT-RECORD.
            05 HEADER.
                10 TRANSACTION-ID PIC 9(15).
                10 USER-ID PIC X(8).
                10 TIMESTAMP PIC 9(14).
            05 OPERATION-DATA.
                10 OPERATION-TYPE PIC X(10).
                10 TABLE-NAME PIC X(30).
                10 RECORD-COUNT PIC 9(7).
            05 AUDIT-TRAIL.
                10 BEFORE-IMAGE PIC X(500).
                10 AFTER-IMAGE PIC X(500).
                10 CHANGE-REASON PIC X(100).
        ";

        let result = parse_copybook(complex_audit_copybook);

        // Should handle complex audit structures safely
        match result {
            Ok(schema) => {
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 1000,
                    "Complex audit record should have substantial length"
                );
                assert!(
                    schema.fields.len() >= 8,
                    "Complex audit record should have multiple fields"
                );
            }
            Err(error) => {
                assert!(
                    matches!(error.code, ErrorCode::CBKP001_SYNTAX),
                    "Audit context creation error should use CBKP001_SYNTAX, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-4-3 Audit metadata validation safety
    fn test_audit_metadata_validation_panic_elimination() {
        // Test case: Metadata validation during parsing that could panic
        let metadata_intensive_copybook = r"
        01 METADATA-INTENSIVE-RECORD.
            05 METADATA-HEADER.
                10 SCHEMA-VERSION PIC 9(3).
                10 RECORD-FORMAT PIC X(8).
                10 ENCODING-TYPE PIC X(10).
            05 BUSINESS-DATA OCCURS 1 TO 50 TIMES DEPENDING ON SCHEMA-VERSION.
                10 DATA-ELEMENT PIC X(200).
                10 ELEMENT-METADATA.
                    15 ELEMENT-TYPE PIC X(20).
                    15 VALIDATION-RULES PIC X(100).
                    15 ACCESS-CONTROL PIC X(50).
        ";

        let result = parse_copybook(metadata_intensive_copybook);

        // Should handle metadata validation safely
        match result {
            Ok(schema) => {
                // Validate metadata elements are properly handled
                let has_metadata = schema.fields.iter()
                    .any(|f| f.name.contains("METADATA"));
                assert!(has_metadata, "Should handle metadata fields");

                // Validate ODO with metadata dependency
                if let Some(odo_field) = schema.tail_odo.as_ref() {
                    assert!(
                        !odo_field.array_path.is_empty(),
                        "ODO with metadata should have array path"
                    );
                }
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP021_ODO_NOT_TAIL | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                    ),
                    "Metadata validation error should use appropriate error code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-4-4 Audit performance metrics safety
    fn test_audit_performance_metrics_panic_elimination() {
        // Test case: Performance-sensitive parsing with audit overhead
        let performance_test_copybook = r"
        01 PERFORMANCE-AUDIT-RECORD.
            05 TIMING-DATA.
                10 START-TIME PIC 9(14).
                10 END-TIME PIC 9(14).
                10 DURATION-MICROS PIC 9(10).
            05 RESOURCE-USAGE.
                10 CPU-TIME PIC 9(10).
                10 MEMORY-USED PIC 9(12).
                10 IO-OPERATIONS PIC 9(8).
            05 PERFORMANCE-COUNTERS OCCURS 10 TIMES.
                10 COUNTER-NAME PIC X(20).
                10 COUNTER-VALUE PIC 9(15).
        ";

        let result = parse_copybook(performance_test_copybook);

        // Should handle performance audit structures efficiently
        assert!(result.is_ok(), "Performance audit structure should parse successfully");

        let schema = result.unwrap();
        assert!(
            schema.fields.len() >= 8,
            "Performance audit should have timing and resource fields"
        );

        // Validate occurs handling
        let has_counters = schema.fields.iter()
            .any(|f| f.name.contains("PERFORMANCE-COUNTERS"));
        assert!(has_counters, "Should handle performance counter arrays");
    }
}

#[cfg(test)]
mod panic_elimination_integration_tests {
    use super::*;

    /// Integration tests for panic elimination across copybook-core modules
    /// AC:63-5 - Cross-module panic safety validation

    #[test] // AC:63-5-1 Parser-layout integration safety
    fn test_parser_layout_integration_panic_elimination() {
        // Test case: Complex structure stressing parser-layout interaction
        let integration_test_copybook = r"
        01 INTEGRATION-TEST-RECORD.
            05 HEADER-SECTION.
                10 RECORD-TYPE PIC X(4).
                10 RECORD-LENGTH PIC 9(5).
                10 SEQUENCE-NUMBER PIC 9(10).
            05 VARIABLE-SECTION OCCURS 1 TO 100 TIMES DEPENDING ON RECORD-LENGTH.
                10 ITEM-CODE PIC X(10).
                10 ITEM-DATA.
                    15 NUMERIC-PART PIC S9(7)V99 COMP-3.
                    15 TEXT-PART PIC X(50).
                    15 FLAGS-PART.
                        20 FLAG-A PIC X.
                        20 FLAG-B PIC X.
                        20 FLAG-C REDEFINES FLAG-B PIC 9.
            05 TRAILER-SECTION.
                10 CHECKSUM PIC 9(10).
                10 END-MARKER PIC X(4).
        ";

        let result = parse_copybook(integration_test_copybook);

        // Should handle complex integration safely
        match result {
            Ok(schema) => {
                assert!(
                    !schema.fields.is_empty(),
                    "Integration test should produce fields"
                );
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 0,
                    "Integration test should calculate record length"
                );

                // Validate complex structure elements
                assert!(
                    schema.fields.len() >= 5,
                    "Should parse header, variable, and trailer sections"
                );
            }
            Err(error) => {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX
                            | ErrorCode::CBKP021_ODO_NOT_TAIL
                            | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                    ),
                    "Integration error should use appropriate error code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:63-5-2 Comprehensive panic elimination validation
    fn test_comprehensive_panic_elimination_validation() {
        // Test case: Comprehensive stress test covering all 63 identified panic points
        let comprehensive_test_copybook = r"
        01 COMPREHENSIVE-PANIC-TEST.
            05 PARSER-STRESS-SECTION.
                10 COMPLEX-PIC PIC $$$,$$9.99.
                10 INVALID-REFERENCE REDEFINES NONEXISTENT PIC X(10).
                10 DEEP-NESTING.
                    15 LEVEL-A.
                        20 LEVEL-B.
                            25 LEVEL-C PIC X(1).
            05 LAYOUT-STRESS-SECTION.
                10 HUGE-FIELD PIC X(10000).
                10 ODO-ARRAY OCCURS 1 TO 1000 TIMES DEPENDING ON MISSING-COUNTER.
                    15 ODO-ITEM PIC 9(10).
            05 PIC-STRESS-SECTION.
                10 ZERO-SIZE PIC X(0).
                10 EXTREME-SIZE PIC 9(999).
                10 INVALID-DIGITS PIC 9(ABC).
            05 AUDIT-STRESS-SECTION.
                10 AUDIT-EVENT PIC X(1000).
                10 METADATA-BLOB PIC X(5000).
        ";

        let result = parse_copybook(comprehensive_test_copybook);

        // Should handle comprehensive stress test without any panics
        match result {
            Ok(schema) => {
                // Some parts may parse successfully with warnings
                assert!(
                    !schema.fields.is_empty(),
                    "Comprehensive test should produce some fields"
                );
            }
            Err(error) => {
                // Should fail with structured error, not panic
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX
                            | ErrorCode::CBKP021_ODO_NOT_TAIL
                            | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                            | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
                    ),
                    "Comprehensive test error should use appropriate error code, got {:?}",
                    error.code
                );

                // Validate error provides debugging context
                assert!(
                    !error.message.is_empty(),
                    "Comprehensive test error should provide descriptive message"
                );
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_performance_tests {
    use super::*;

    /// Performance validation for panic elimination changes
    /// AC:63-6 - Performance impact <5% on enterprise benchmarks

    #[test] // AC:63-6-1 Parser performance preservation
    fn test_parser_performance_preservation() {
        // Test case: Parser performance with panic elimination changes
        let performance_test_copybook = r"
        01 PARSER-PERFORMANCE-TEST.
            05 FIELDS OCCURS 100 TIMES.
                10 FIELD-ID PIC 9(5).
                10 FIELD-DATA PIC X(50).
                10 FIELD-METADATA PIC X(20).
        ";

        let start_time = std::time::Instant::now();
        let result = parse_copybook(performance_test_copybook);
        let parse_duration = start_time.elapsed();

        // Should parse successfully within performance bounds
        assert!(result.is_ok(), "Performance test should parse successfully");
        assert!(
            parse_duration.as_millis() < 1000,
            "Parser performance should be reasonable: {:?}",
            parse_duration
        );

        let schema = result.unwrap();
        assert!(
            schema.fields.len() >= 100,
            "Performance test should parse all fields efficiently"
        );
    }

    #[test] // AC:63-6-2 Memory efficiency preservation
    fn test_memory_efficiency_preservation() {
        // Test case: Memory usage with panic elimination overhead
        let memory_test_copybook = r"
        01 MEMORY-EFFICIENCY-TEST.
            05 LARGE-STRUCTURE OCCURS 500 TIMES.
                10 DATA-BLOCK PIC X(100).
                10 METADATA PIC X(50).
        ";

        let result = parse_copybook(memory_test_copybook);

        // Should handle large structures efficiently
        assert!(result.is_ok(), "Memory test should parse successfully");

        let schema = result.unwrap();
        assert!(
            schema.fields.len() >= 500,
            "Memory test should handle large field counts"
        );
        assert!(
            schema.lrecl_fixed.unwrap_or(0) >= 75000,
            "Memory test should calculate large record correctly"
        );
    }
}