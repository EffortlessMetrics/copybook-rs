#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used)]

/// Tests feature spec: panic-elimination-implementation-blueprint.md#phase-2-performance-hotspot-elimination
/// Issue #33 - Hotspot Module Unit Tests
///
/// This module provides comprehensive unit testing for panic elimination in performance-critical
/// modules. Targets identified hotspots: parser.rs (17 panics), layout.rs (9 panics),
/// pic.rs (8 panics), and validates panic-safe error handling with structured error codes.
#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod hotspot_parser_safety {
    use copybook_core::{ErrorCode, parse_copybook};

    /// Tests parser.rs hotspot panic elimination (17 occurrences target)
    /// Validates panic-safe parser state management and error handling

    #[test] // AC:33:HOTSPOT:PARSER_STACK_UNDERFLOW
    fn test_parser_stack_underflow_safety() {
        // Test case: Malformed copybook that causes parser stack issues
        let malformed_copybook = r"
        01 INCOMPLETE-RECORD.
            05 FIELD-A PIC X(10).
            05 FIELD-B
        "; // Missing PIC clause and incomplete structure

        let result = parse_copybook(malformed_copybook);

        // Should return structured error instead of panicking
        assert!(
            result.is_err(),
            "Malformed copybook should return error, not panic"
        );

        let error = result.unwrap_err();

        // Validate error uses appropriate CBKP* code for parser errors
        assert!(
            matches!(
                error.code,
                ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
            ),
            "Parser stack error should use CBKP* code, got {:?}",
            error.code
        );

        // Validate error provides meaningful context
        assert!(
            error.message.contains("FIELD-B")
                || error.message.contains("incomplete")
                || error.message.contains("syntax"),
            "Parser error should provide descriptive context: {}",
            error.message
        );
    }

    #[test] // AC:33:HOTSPOT:PARSER_TOKEN_BOUNDS
    fn test_parser_token_bounds_safety() {
        // Test case: Token buffer bounds checking
        let edge_case_copybook = "01 A."; // Minimal valid copybook

        let result = parse_copybook(edge_case_copybook);

        // Should handle minimal input without bounds errors
        match result {
            Ok(_) => {
                // Successful parsing is acceptable
            }
            Err(error) => {
                // Should use appropriate error code, not panic
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Token bounds error should use CBKP* code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:HOTSPOT:PARSER_FIELD_SEQUENCE
    fn test_parser_field_sequence_safety() {
        // Test case: Complex field sequence that stresses parser state
        let complex_sequence = r"
        01 COMPLEX-RECORD.
            05 FIELD-A PIC X(10).
            05 FIELD-B REDEFINES FIELD-A.
                10 SUB-A PIC X(5).
                10 SUB-B PIC X(5).
            05 FIELD-C OCCURS 10 TIMES.
                10 ITEM-A PIC 9(5).
                10 ITEM-B PIC X(10).
        ";

        let result = parse_copybook(complex_sequence);

        // Complex sequences should be handled safely
        match result {
            Ok(schema) => {
                // Validate successful parsing produces valid schema
                assert!(!schema.fields.is_empty(), "Schema should contain fields");
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 0,
                    "Schema should have positive length"
                );
            }
            Err(error) => {
                // Complex parsing errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX
                            | ErrorCode::CBKP021_ODO_NOT_TAIL
                            | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                    ),
                    "Complex sequence error should use appropriate code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:HOTSPOT:PARSER_LEXER_INTEGRATION
    fn test_parser_lexer_integration_safety() {
        // Test case: Lexer-parser boundary error handling
        let invalid_syntax_copybook = "01 PIC X(10)."; // Missing field name

        let result = parse_copybook(invalid_syntax_copybook);

        // Either succeeds (parser handles gracefully) or fails appropriately
        match result {
            Ok(_) => {
                // Parser handles invalid input gracefully - this is acceptable
            }
            Err(error) => {
                // Parser correctly rejects invalid input - this is also acceptable
                // Should handle lexer-parser boundary errors safely with appropriate codes
                assert!(
                    matches!(error.code, ErrorCode::CBKP001_SYNTAX),
                    "Lexer-parser integration error should use CBKP001_SYNTAX, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:HOTSPOT:PARSER_NESTED_STRUCTURE
    fn test_parser_nested_structure_safety() {
        // Test case: Deeply nested structure that stresses parser stack
        let deeply_nested = r"
        01 DEEP-RECORD.
            05 LEVEL-1.
                10 LEVEL-2.
                    15 LEVEL-3.
                        20 LEVEL-4.
                            25 LEVEL-5 PIC X(10).
                            25 LEVEL-5B PIC 9(5).
                        20 LEVEL-4B PIC X(5).
                    15 LEVEL-3B PIC 9(3).
                10 LEVEL-2B PIC X(8).
            05 LEVEL-1B PIC 9(10).
        ";

        let result = parse_copybook(deeply_nested);

        // Deep nesting should be handled without stack overflow
        match result {
            Ok(schema) => {
                // Validate deep structure is correctly parsed
                assert!(
                    !schema.fields.is_empty(),
                    "Deep structure should parse to fields"
                );

                // Validate nested access works
                if let Some(level_1) = schema.fields.iter().find(|f| f.name == "LEVEL-1") {
                    assert_eq!(level_1.level, 5, "Top level should be correct");
                }
            }
            Err(error) => {
                // Deep nesting errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Deep nesting error should use CBKP* code, got {:?}",
                    error.code
                );
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod hotspot_layout_safety {
    use copybook_core::{ErrorCode, parse_copybook};

    /// Tests layout.rs hotspot panic elimination (9 occurrences target)
    /// Validates panic-safe schema resolution and field layout computation

    #[test] // AC:33:HOTSPOT:LAYOUT_REDEFINES_RESOLUTION
    fn test_layout_redefines_resolution_safety() {
        // Test case: REDEFINES with missing target
        let missing_redefines_target = r"
        01 RECORD-WITH-BAD-REDEFINES.
            05 FIELD-A PIC X(10).
            05 FIELD-B REDEFINES NONEXISTENT-FIELD PIC 9(10).
        ";

        let result = parse_copybook(missing_redefines_target);

        assert!(
            result.is_err(),
            "Missing REDEFINES target should return error"
        );

        let error = result.unwrap_err();

        // Should use schema validation error code
        assert!(
            matches!(error.code, ErrorCode::CBKP001_SYNTAX),
            "REDEFINES resolution error should use CBKP001_SYNTAX, got {:?}",
            error.code
        );

        assert!(
            error.message.contains("REDEFINES") || error.message.contains("NONEXISTENT"),
            "Error should reference REDEFINES issue: {}",
            error.message
        );
    }

    #[test] // AC:33:HOTSPOT:LAYOUT_ODO_VALIDATION
    fn test_layout_odo_validation_safety() {
        // Test case: ODO with invalid counter reference
        let invalid_odo_counter = r"
        01 RECORD-WITH-BAD-ODO.
            05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON MISSING-COUNTER.
                10 ITEM PIC X(5).
        ";

        let result = parse_copybook(invalid_odo_counter);

        assert!(result.is_err(), "Invalid ODO counter should return error");

        let error = result.unwrap_err();

        // Should use counter validation error code
        assert!(
            matches!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND),
            "ODO counter error should use CBKS121_COUNTER_NOT_FOUND, got {:?}",
            error.code
        );

        assert!(
            error.message.contains("ODO")
                || error.message.contains("DEPENDING ON")
                || error.message.contains("MISSING-COUNTER"),
            "Error should reference ODO counter issue: {}",
            error.message
        );
    }

    #[test] // AC:33:HOTSPOT:LAYOUT_FIELD_OFFSET_CALCULATION
    fn test_layout_field_offset_calculation_safety() {
        // Test case: Field layout that could cause offset calculation errors
        let complex_layout = r"
        01 COMPLEX-LAYOUT.
            05 FIXED-PART.
                10 HEADER PIC X(10).
                10 COUNT PIC 9(3).
            05 VARIABLE-PART OCCURS 1 TO 999 TIMES DEPENDING ON COUNT.
                10 ITEM-ID PIC 9(10).
                10 ITEM-DATA PIC X(50).
            05 TRAILER PIC X(20).
        ";

        let result = parse_copybook(complex_layout);

        // Complex layout should be handled safely
        match result {
            Ok(schema) => {
                // Validate layout calculation succeeded
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 0,
                    "Complex layout should have positive length"
                );
                assert!(
                    !schema.fields.is_empty(),
                    "Complex layout should have fields"
                );

                // Validate ODO is positioned correctly
                if let Some(odo_field) = schema.tail_odo.as_ref() {
                    assert!(
                        !odo_field.array_path.is_empty(),
                        "ODO field should have array path"
                    );
                }
            }
            Err(error) => {
                // Layout calculation errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP021_ODO_NOT_TAIL
                            | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                            | ErrorCode::CBKP001_SYNTAX
                    ),
                    "Layout calculation error should use appropriate code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:HOTSPOT:LAYOUT_MEMORY_LAYOUT_CONSISTENCY
    fn test_layout_memory_layout_consistency_safety() {
        // Test case: Memory layout that could cause alignment issues
        let alignment_test = r"
        01 ALIGNMENT-TEST.
            05 CHAR-FIELD PIC X(1).
            05 COMP-FIELD PIC S9(9) COMP.
            05 PACKED-FIELD PIC S9(7)V99 COMP-3.
            05 CHAR-FIELD2 PIC X(3).
        ";

        let result = parse_copybook(alignment_test);

        // Memory layout should be calculated safely
        match result {
            Ok(schema) => {
                // Validate consistent memory layout
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 0,
                    "Alignment test should have positive length"
                );

                // Validate all fields have reasonable offsets
                for field in &schema.fields {
                    // Validate field has reasonable offset and length
                    assert!(
                        field.offset < 1000,
                        "Field {} offset too large: {}",
                        field.name,
                        field.offset
                    );
                    assert!(
                        field.len > 0,
                        "Field {} should have positive length",
                        field.name
                    );
                }
            }
            Err(error) => {
                // Memory layout errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Memory layout error should use CBKP* code, got {:?}",
                    error.code
                );
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod hotspot_pic_safety {
    use copybook_core::{ErrorCode, parse_copybook};

    /// Tests pic.rs hotspot panic elimination (8 occurrences target)
    /// Validates panic-safe PIC clause parsing and validation

    #[test] // AC:33:HOTSPOT:PIC_CLAUSE_PARSING
    fn test_pic_clause_parsing_safety() {
        // Test case: Invalid PIC clause syntax
        let invalid_pic_clauses = vec![
            "01 FIELD1 PIC X(INVALID).", // Invalid size specifier
            "01 FIELD2 PIC Z(10.5).",    // Invalid decimal size
            "01 FIELD3 PIC S9().",       // Empty parentheses
            "01 FIELD4 PIC X(0).",       // Zero size
            "01 FIELD5 PIC 9(99999).",   // Excessive size
        ];

        for invalid_pic in invalid_pic_clauses {
            let result = parse_copybook(invalid_pic);

            // Either fails appropriately or handles gracefully
            match result {
                Ok(_) => {
                    // Parser handles potentially invalid PIC gracefully - this is acceptable
                }
                Err(error) => {
                    // Should use PIC parsing error code
                    assert!(
                        matches!(
                            error.code,
                            ErrorCode::CBKP001_SYNTAX
                                | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                                | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
                        ),
                        "PIC parsing error should use CBKP* code for '{}', got {:?}",
                        invalid_pic,
                        error.code
                    );
                }
            }
        }
    }

    #[test] // AC:33:HOTSPOT:PIC_NUMERIC_VALIDATION
    fn test_pic_numeric_validation_safety() {
        // Test case: Invalid numeric PIC patterns
        let invalid_numeric_pics = vec![
            "01 FIELD1 PIC S9(5)V99V99.", // Multiple decimal points
            "01 FIELD2 PIC SS9(5).",      // Multiple signs
            "01 FIELD3 PIC 9(5)S.",       // Sign in wrong position
            "01 FIELD4 PIC 9(0)V99.",     // Zero integer part
            "01 FIELD5 PIC 9(18)V99.",    // Excessive precision
        ];

        for invalid_pic in invalid_numeric_pics {
            let result = parse_copybook(invalid_pic);

            // Invalid numeric PIC should be detected safely
            if let Err(error) = result {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Numeric PIC validation error should use CBKP* code for '{}', got {:?}",
                    invalid_pic,
                    error.code
                );
            }
            // Some invalid patterns might be accepted with warnings - that's also acceptable
        }
    }

    #[test] // AC:33:HOTSPOT:PIC_COMP_USAGE_VALIDATION
    fn test_pic_comp_usage_validation_safety() {
        // Test case: Invalid COMP usage combinations
        let invalid_comp_pics = vec![
            "01 FIELD1 PIC X(10) COMP.",  // COMP with alphanumeric
            "01 FIELD2 PIC X(5) COMP-3.", // COMP-3 with alphanumeric
            "01 FIELD3 PIC S9(20) COMP.", // COMP with excessive precision
            "01 FIELD4 PIC 9(0) COMP-3.", // COMP-3 with zero precision
        ];

        for invalid_comp in invalid_comp_pics {
            let result = parse_copybook(invalid_comp);

            // Invalid COMP usage should be validated safely
            if let Err(error) = result {
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "COMP usage validation error should use CBKP* code for '{}', got {:?}",
                    invalid_comp,
                    error.code
                );
            }
            // Some combinations might be accepted - implementation dependent
        }
    }

    #[test] // AC:33:HOTSPOT:PIC_PATTERN_COMPLEXITY
    fn test_pic_pattern_complexity_safety() {
        // Test case: Complex PIC patterns that stress parser
        let complex_pic_patterns = vec![
            "01 FIELD1 PIC X(50).",            // Large text field
            "01 FIELD2 PIC S9(15)V99 COMP-3.", // High precision decimal
            "01 FIELD3 PIC Z(10).99.",         // Zero suppression with decimal
            "01 FIELD4 PIC +(10).99.",         // Sign editing
            "01 FIELD5 PIC $$$,$$9.99.",       // Currency formatting
        ];

        for complex_pic in complex_pic_patterns {
            let result = parse_copybook(complex_pic);

            // Complex patterns should be handled safely
            match result {
                Ok(schema) => {
                    // Successful parsing is good
                    assert!(
                        !schema.fields.is_empty(),
                        "Complex PIC should produce fields"
                    );
                }
                Err(error) => {
                    // Errors should use appropriate codes
                    assert!(
                        matches!(
                            error.code,
                            ErrorCode::CBKP001_SYNTAX
                                | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                                | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
                        ),
                        "Complex PIC error should use CBKP* code for '{}', got {:?}",
                        complex_pic,
                        error.code
                    );
                }
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod hotspot_integration_safety {
    use copybook_core::{ErrorCode, parse_copybook};

    /// Integration tests for hotspot module interactions
    /// Validates panic-safe behavior across module boundaries

    #[test] // AC:33:HOTSPOT:PARSER_LAYOUT_INTEGRATION
    fn test_parser_layout_integration_safety() {
        // Test case: Parser-layout integration stress test
        let integration_test = r"
        01 INTEGRATION-RECORD.
            05 HEADER-SECTION.
                10 ID PIC 9(10).
                10 TYPE PIC X(5).
            05 VARIABLE-SECTION OCCURS 1 TO 100 TIMES DEPENDING ON ID.
                10 ITEM-CODE PIC X(10).
                10 ITEM-VALUE PIC S9(7)V99 COMP-3.
                10 ITEM-FLAGS.
                    15 FLAG-A PIC X.
                    15 FLAG-B PIC X.
                    15 FLAG-C REDEFINES FLAG-B PIC 9.
        ";

        let result = parse_copybook(integration_test);

        // Integration should work safely across modules
        match result {
            Ok(schema) => {
                // Validate integration produces coherent schema
                assert!(
                    !schema.fields.is_empty(),
                    "Integration should produce fields"
                );
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 0,
                    "Integration should calculate length"
                );

                // Validate ODO integration
                if let Some(odo_field) = schema.tail_odo.as_ref() {
                    assert!(
                        !odo_field.array_path.is_empty(),
                        "ODO field should have array path"
                    );
                }
            }
            Err(error) => {
                // Integration errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX
                            | ErrorCode::CBKP021_ODO_NOT_TAIL
                            | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                    ),
                    "Integration error should use appropriate code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:HOTSPOT:PIC_LAYOUT_INTEGRATION
    fn test_pic_layout_integration_safety() {
        // Test case: PIC-layout integration for memory calculation
        let pic_layout_test = r"
        01 PIC-LAYOUT-TEST.
            05 MIXED-TYPES.
                10 CHAR-FIELD PIC X(10).
                10 NUM-FIELD PIC 9(8).
                10 COMP-FIELD PIC S9(9) COMP.
                10 PACKED-FIELD PIC S9(7)V99 COMP-3.
                10 EDITED-FIELD PIC Z(5).99.
        ";

        let result = parse_copybook(pic_layout_test);

        // PIC-layout integration should calculate memory correctly
        match result {
            Ok(schema) => {
                // Validate memory layout calculation
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) >= 30,
                    "Mixed types should have reasonable length"
                );

                // Validate individual field calculations
                for field in &schema.fields {
                    // Validate field has reasonable length
                    assert!(
                        field.len > 0,
                        "Field {} should have positive length",
                        field.name
                    );
                    assert!(
                        field.len < 1000,
                        "Field {} length should be reasonable",
                        field.name
                    );
                }
            }
            Err(error) => {
                // PIC-layout integration errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX
                            | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                            | ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC
                    ),
                    "PIC-layout integration error should use CBKP* code, got {:?}",
                    error.code
                );
            }
        }
    }

    #[test] // AC:33:HOTSPOT:COMPREHENSIVE_HOTSPOT_STRESS
    fn test_comprehensive_hotspot_stress() {
        // Test case: Comprehensive stress test covering all hotspots
        let stress_test_copybook = r"
        01 COMPREHENSIVE-STRESS-TEST.
            05 HEADER-COMPLEX.
                10 ID-FIELD PIC 9(10).
                10 TYPE-INDICATOR PIC X(3).
                10 FLAGS.
                    15 STATUS-FLAG PIC X.
                    15 PRIORITY-FLAG PIC 9.
                    15 ERROR-FLAG REDEFINES PRIORITY-FLAG PIC X.
            05 DATA-ARRAY OCCURS 1 TO 50 TIMES DEPENDING ON ID-FIELD.
                10 ITEM-HEADER.
                    15 ITEM-ID PIC 9(8).
                    15 ITEM-TYPE PIC X(5).
                10 ITEM-DATA.
                    15 NUMERIC-VALUE PIC S9(9)V99 COMP-3.
                    15 TEXT-VALUE PIC X(20).
                    15 BINARY-VALUE PIC S9(9) COMP.
                10 ITEM-METADATA.
                    15 CREATED-DATE PIC 9(8).
                    15 MODIFIED-DATE PIC 9(8).
                    15 CHECKSUM PIC 9(10).
            05 TRAILER-SECTION.
                10 TOTAL-COUNT PIC 9(5).
                10 RECORD-CHECKSUM PIC 9(15).
        ";

        let result = parse_copybook(stress_test_copybook);

        // Comprehensive stress test should complete safely
        match result {
            Ok(schema) => {
                // Validate comprehensive parsing success
                assert!(
                    !schema.fields.is_empty(),
                    "Stress test should produce fields"
                );
                assert!(
                    schema.lrecl_fixed.unwrap_or(0) > 0,
                    "Stress test should calculate length"
                );

                // Validate complex structure elements
                assert!(
                    schema.fields.len() >= 3,
                    "Should have header, array, trailer sections"
                );

                // Validate ODO handling
                if let Some(odo_field) = schema.tail_odo.as_ref() {
                    assert!(
                        !odo_field.array_path.is_empty(),
                        "ODO should have array path"
                    );
                }
            }
            Err(error) => {
                // Stress test errors should use appropriate codes
                assert!(
                    matches!(
                        error.code,
                        ErrorCode::CBKP001_SYNTAX
                            | ErrorCode::CBKP021_ODO_NOT_TAIL
                            | ErrorCode::CBKS121_COUNTER_NOT_FOUND
                            | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
                    ),
                    "Stress test error should use appropriate code, got {:?}",
                    error.code
                );

                // Validate error provides useful debugging information
                assert!(
                    !error.message.is_empty(),
                    "Stress test error should provide descriptive message"
                );
            }
        }
    }
}
