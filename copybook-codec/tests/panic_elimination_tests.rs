#![allow(clippy::expect_used)] // Test code validates production code doesn't panic
#![allow(clippy::unwrap_used)] // Test infrastructure for panic elimination validation
#![allow(clippy::uninlined_format_args)] // Test output formatting for clarity
#![allow(clippy::too_many_lines)] // Test scaffolding requires comprehensive coverage
#![allow(clippy::doc_markdown)] // Test documentation may reference code patterns
#![allow(clippy::out_of_bounds_indexing)] // Test data construction with known bounds
#![allow(clippy::single_match_else)] // Test logic flows for readability
#![allow(clippy::useless_vec)] // Test data structures for clarity
#![allow(clippy::unreadable_literal)] // Test constants for specific values

/// Tests feature spec: issue-63-spec.md#ac1-complete-panic-elimination
/// Tests feature spec: issue-63-technical-specification.md#numeric-conversion-safety
/// Tests feature spec: panic-elimination-implementation-blueprint.md#phase-2-performance-hotspot-elimination
///
/// Issue #63 - Comprehensive Panic Elimination Test Scaffolding for copybook-codec
///
/// This module provides comprehensive test scaffolding for eliminating 134 `.unwrap()`/`.expect()` calls
/// in copybook-codec production code. Tests target `numeric.rs` (21), `zoned_overpunch.rs` (24),
/// `record.rs` (32), `memory.rs` (11), `iterator.rs` (11), and other modules (35) with enterprise-safe
/// data processing patterns.
///
/// **AC Traceability:**
/// - AC1: Complete elimination of 134 `.unwrap()`/`.expect()` calls in copybook-codec
/// - AC2: Zero breaking changes to existing public APIs
/// - AC3: Integration with CBKD*/CBKE* error taxonomy for data processing
/// - AC4: Performance impact <5% maintaining 2.33+ GiB/s DISPLAY, 168+ MiB/s COMP-3
/// - AC7: Comprehensive test coverage for data encoding/decoding paths
/// - AC10: Memory safety preserved in numeric conversion algorithms
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, decode_record};
use copybook_core::parse_copybook;

#[cfg(test)]
mod panic_elimination_numeric_tests {
    use super::*;

    /// Tests numeric.rs panic elimination (21 instances)
    /// AC:63-7 - Numeric conversion with safe digit processing and bounds checking

    #[test] // AC:63-7-1 COMP-3 nibble extraction safety
    fn test_comp3_nibble_extraction_panic_elimination() {
        // Test case: COMP-3 data with invalid nibbles causing extraction panics
        let comp3_schema_copybook = "01 RECORD.\n    05 COMP3-FIELD PIC S9(5)V99 COMP-3.";
        let schema = parse_copybook(comp3_schema_copybook)
            .expect("Schema should parse in panic elimination test");

        // Invalid COMP-3 data with bad nibbles (0xFF)
        let invalid_comp3_data = vec![0xFF, 0xFF, 0xFF, 0xFF]; // All invalid nibbles

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &invalid_comp3_data, &options);

        // Should return structured error instead of panicking on nibble extraction
        assert!(
            result.is_err(),
            "Invalid COMP-3 nibbles should return error"
        );

        if let Err(error) = result {
            assert!(
                error.to_string().contains("COMP-3")
                    || error.to_string().contains("nibble")
                    || error.to_string().contains("invalid"),
                "Error should reference COMP-3 nibble issue: {error}"
            );
        }
    }

    #[test] // AC:63-7-2 Decimal digit validation safety
    fn test_decimal_digit_validation_panic_elimination() {
        // Test case: Decimal digit conversion with invalid characters
        let decimal_schema_copybook = "01 RECORD.\n    05 DECIMAL-FIELD PIC 9(10).";
        let schema = parse_copybook(decimal_schema_copybook)
            .expect("Schema should parse in panic elimination test");

        // Invalid decimal data with non-numeric characters
        let invalid_decimal_data = b"ABC123XYZ\x00"; // Mixed alphanumeric

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, invalid_decimal_data, &options);

        // Should handle invalid decimal digits safely without character conversion panics
        match result {
            Ok(_) => {
                // Decoder may handle with default interpretation or conversion
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("decimal")
                        || error.to_string().contains("digit")
                        || error.to_string().contains("numeric"),
                    "Decimal validation error should reference digit issue: {error}"
                );
            }
        }
    }

    #[test] // AC:63-7-3 Binary integer overflow protection
    fn test_binary_integer_overflow_panic_elimination() {
        // Test case: Binary integer with overflow values
        let binary_schema_copybook = "01 RECORD.\n    05 BINARY-FIELD PIC S9(9) COMP.";
        let schema = parse_copybook(binary_schema_copybook).expect("Schema should parse");

        // Binary data that could cause integer overflow (max values)
        let overflow_binary_data = vec![0xFF, 0xFF, 0xFF, 0xFF]; // Maximum binary values

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &overflow_binary_data, &options);

        // Should handle potential overflow safely without integer panics
        match result {
            Ok(value) => {
                // Should produce valid JSON value within reasonable bounds
                if let Some(obj) = value.as_object()
                    && let Some(field_value) = obj.get("BINARY-FIELD")
                {
                    assert!(
                        field_value.is_number(),
                        "Binary field should produce numeric value"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("binary")
                        || error.to_string().contains("overflow")
                        || error.to_string().contains("integer"),
                    "Binary overflow error should reference overflow issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-7-4 Numeric precision bounds safety
    fn test_numeric_precision_bounds_panic_elimination() {
        // Test case: Numeric fields with extreme precision values
        let precision_schema_copybook = "01 RECORD.\n    05 HIGH-PRECISION PIC S9(15)V99 COMP-3.";
        let schema = parse_copybook(precision_schema_copybook).expect("Schema should parse");

        // High precision COMP-3 data at maximum bounds
        let high_precision_data = vec![0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C]; // Maximum positive

        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::Lossless);
        let result = decode_record(&schema, &high_precision_data, &options);

        // Should handle high precision safely without bounds panics
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object()
                    && let Some(field_value) = obj.get("HIGH-PRECISION")
                {
                    // Should produce valid numeric representation
                    assert!(
                        field_value.is_string() || field_value.is_number(),
                        "High precision field should produce valid value"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("precision")
                        || error.to_string().contains("bounds")
                        || error.to_string().contains("COMP-3"),
                    "Precision bounds error should reference precision issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-7-5 Numeric conversion edge cases
    fn test_numeric_conversion_edge_cases_panic_elimination() {
        // Test case: Edge cases in numeric conversion that could panic
        let edge_case_schema_copybook = r"
        01 RECORD.
            05 ZERO-LENGTH PIC 9.
            05 SIGNED-ZERO PIC S9 COMP-3.
            05 MAX-DECIMAL PIC 9(18).
        ";
        let schema = parse_copybook(edge_case_schema_copybook).expect("Schema should parse");

        // Edge case data: zero values, maximum values, minimal data
        let edge_case_data = vec![
            0x00, 0x0C, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99,
        ]; // Mixed edge cases

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &edge_case_data, &options);

        // Should handle edge cases safely without conversion panics
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    // Validate all fields produce reasonable values
                    for (field_name, field_value) in obj {
                        assert!(
                            field_value.is_number() || field_value.is_string(),
                            "Field {} should produce valid value type",
                            field_name
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("conversion")
                        || error.to_string().contains("numeric"),
                    "Numeric conversion error should reference conversion issue: {}",
                    error
                );
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_zoned_overpunch_tests {
    use super::*;

    /// Tests zoned_overpunch.rs panic elimination (24 instances)
    /// AC:63-8 - Zoned overpunch processing with safe character mapping

    #[test] // AC:63-8-1 Overpunch character lookup safety
    fn test_overpunch_character_lookup_panic_elimination() {
        // Test case: Zoned overpunch with invalid character lookups
        let zoned_schema_copybook = "01 RECORD.\n    05 ZONED-FIELD PIC S9(5).";
        let schema = parse_copybook(zoned_schema_copybook).expect("Schema should parse");

        // Invalid overpunch characters that could cause lookup panics
        let invalid_overpunch_data = b"\xFF\xFF\xFF\xFF\x80"; // Invalid EBCDIC overpunch chars

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, invalid_overpunch_data, &options);

        // Should handle invalid overpunch characters safely
        match result {
            Ok(value) => {
                // May interpret with default handling
                if let Some(obj) = value.as_object()
                    && let Some(field_value) = obj.get("ZONED-FIELD")
                {
                    assert!(
                        field_value.is_number() || field_value.is_string(),
                        "Zoned field should produce valid value"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("zoned")
                        || error.to_string().contains("overpunch")
                        || error.to_string().contains("character"),
                    "Overpunch lookup error should reference overpunch issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-8-2 Sign encoding validation safety
    fn test_sign_encoding_validation_panic_elimination() {
        // Test case: Sign encoding with invalid sign patterns
        let sign_schema_copybook = "01 RECORD.\n    05 SIGNED-FIELD PIC S9(8).";
        let schema = parse_copybook(sign_schema_copybook).expect("Schema should parse");

        // Invalid sign encoding patterns
        let invalid_sign_data = b"1234567\x00"; // Invalid sign in last position

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, invalid_sign_data, &options);

        // Should handle invalid sign encoding safely
        match result {
            Ok(value) => {
                // May interpret with default sign handling
                if let Some(obj) = value.as_object()
                    && let Some(field_value) = obj.get("SIGNED-FIELD")
                {
                    assert!(
                        field_value.is_number() || field_value.is_string(),
                        "Signed field should produce valid value"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("sign")
                        || error.to_string().contains("encoding")
                        || error.to_string().contains("zoned"),
                    "Sign encoding error should reference sign issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-8-3 EBCDIC character mapping safety
    fn test_ebcdic_character_mapping_panic_elimination() {
        // Test case: EBCDIC to ASCII mapping with unmappable characters
        let ebcdic_schema_copybook = "01 RECORD.\n    05 EBCDIC-FIELD PIC S9(10).";
        let schema = parse_copybook(ebcdic_schema_copybook).expect("Schema should parse");

        // EBCDIC data with characters that may not map to ASCII
        let unmappable_ebcdic_data = b"\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09"; // Control characters

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, unmappable_ebcdic_data, &options);

        // Should handle unmappable EBCDIC characters safely
        match result {
            Ok(value) => {
                // Should provide some reasonable interpretation
                if let Some(obj) = value.as_object() {
                    assert!(
                        obj.contains_key("EBCDIC-FIELD"),
                        "Should contain EBCDIC field"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("EBCDIC")
                        || error.to_string().contains("character")
                        || error.to_string().contains("mapping"),
                    "EBCDIC mapping error should reference mapping issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-8-4 Overpunch digit bounds safety
    fn test_overpunch_digit_bounds_panic_elimination() {
        // Test case: Overpunch processing with out-of-bounds digit values
        let digit_bounds_schema_copybook = "01 RECORD.\n    05 OVERPUNCH-FIELD PIC S9(6).";
        let schema = parse_copybook(digit_bounds_schema_copybook).expect("Schema should parse");

        // Data with digits that could be out of overpunch table bounds
        let bounds_test_data = b"99999\xFF"; // Last character out of range

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, bounds_test_data, &options);

        // Should handle out-of-bounds digit access safely
        match result {
            Ok(value) => {
                // Should handle with bounds checking
                if let Some(obj) = value.as_object()
                    && let Some(field_value) = obj.get("OVERPUNCH-FIELD")
                {
                    assert!(
                        field_value.is_number() || field_value.is_string(),
                        "Overpunch field should produce valid value"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("overpunch")
                        || error.to_string().contains("bounds")
                        || error.to_string().contains("digit"),
                    "Digit bounds error should reference bounds issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-8-5 Encoding validation edge cases
    fn test_encoding_validation_edge_cases_panic_elimination() {
        // Test case: Edge cases in overpunch encoding/decoding
        let edge_case_schema_copybook = r"
        01 RECORD.
            05 POSITIVE-OVERPUNCH PIC S9(3).
            05 NEGATIVE-OVERPUNCH PIC S9(3).
            05 ZERO-OVERPUNCH PIC S9(1).
        ";
        let schema = parse_copybook(edge_case_schema_copybook).expect("Schema should parse");

        // Edge case overpunch data: positive, negative, zero values
        let edge_case_data = b"12}45{0"; // Mixed overpunch signs

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, edge_case_data, &options);

        // Should handle encoding edge cases safely
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    // Validate all fields produce reasonable values
                    for (field_name, field_value) in obj {
                        assert!(
                            field_value.is_number() || field_value.is_string(),
                            "Field {} should produce valid value",
                            field_name
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("encoding")
                        || error.to_string().contains("overpunch"),
                    "Encoding validation error should reference encoding issue: {}",
                    error
                );
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_record_tests {
    use super::*;

    /// Tests record.rs panic elimination (32 instances)
    /// AC:63-9 - Record processing with safe field extraction and bounds checking

    #[test] // AC:63-9-1 Record bounds checking safety
    fn test_record_bounds_checking_panic_elimination() {
        // Test case: Record field extraction beyond record boundaries
        let bounds_schema_copybook = r"
        01 RECORD.
            05 FIELD-A PIC X(10).
            05 FIELD-B PIC X(20).
            05 FIELD-C PIC 9(5).
        ";
        let schema = parse_copybook(bounds_schema_copybook).expect("Schema should parse");

        // Record data shorter than expected field layout
        let short_record_data = b"SHORT"; // Only 5 bytes, but schema expects ~35

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, short_record_data, &options);

        // Should return structured error instead of panicking on bounds access
        assert!(result.is_err(), "Short record should return bounds error");

        let error = result.unwrap_err();
        assert!(
            error.to_string().contains("record")
                || error.to_string().contains("short")
                || error.to_string().contains("bounds"),
            "Record bounds error should reference record issue: {}",
            error
        );
    }

    #[test] // AC:63-9-2 Field offset calculation safety
    fn test_field_offset_calculation_panic_elimination() {
        // Test case: Field offset calculations that could overflow
        let large_offset_schema_copybook = r"
        01 RECORD.
            05 LARGE-FIELD-A PIC X(1000).
            05 LARGE-FIELD-B PIC X(2000).
            05 LARGE-FIELD-C PIC X(5000).
        ";
        let schema = parse_copybook(large_offset_schema_copybook).expect("Schema should parse");

        // Create appropriately sized record data
        let large_record_data = vec![b'A'; 8000]; // 8KB record

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &large_record_data, &options);

        // Should handle large offsets safely without overflow panics
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(obj.len() >= 3, "Large record should produce all fields");

                    // Validate field values are reasonable
                    for (field_name, field_value) in obj {
                        assert!(
                            field_value.is_string(),
                            "Field {} should be string value",
                            field_name
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("offset")
                        || error.to_string().contains("calculation")
                        || error.to_string().contains("record"),
                    "Offset calculation error should reference offset issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-9-3 Variable length record safety
    fn test_variable_length_record_panic_elimination() {
        // Test case: Variable length records with ODO arrays
        let variable_schema_copybook = r"
        01 RECORD.
            05 HEADER.
                10 COUNT PIC 9(3).
            05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON COUNT.
                10 ITEM PIC X(10).
        ";
        let schema = parse_copybook(variable_schema_copybook).expect("Schema should parse");

        // Variable record with extreme count value
        let mut variable_record_data = b"999".to_vec();
        variable_record_data.extend(vec![b'X'; 1000]); // Count=999, but limited data

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &variable_record_data, &options);

        // Should handle variable length safely without array bounds panics
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    // Should handle count validation
                    if let Some(count_value) = obj.get("COUNT") {
                        assert!(
                            count_value.is_number() || count_value.is_string(),
                            "Count field should be valid"
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("variable")
                        || error.to_string().contains("ODO")
                        || error.to_string().contains("count"),
                    "Variable length error should reference variable length issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-9-4 Field type validation safety
    fn test_field_type_validation_panic_elimination() {
        // Test case: Field type mismatches that could cause processing panics
        let mixed_types_schema_copybook = r"
        01 RECORD.
            05 TEXT-FIELD PIC X(10).
            05 NUMERIC-FIELD PIC 9(5).
            05 COMP-FIELD PIC S9(9) COMP.
            05 COMP3-FIELD PIC S9(7)V99 COMP-3.
        ";
        let schema = parse_copybook(mixed_types_schema_copybook).expect("Schema should parse");

        // Mixed type data that could confuse type processing
        let mixed_data = b"TEXT12345\xFF\xFF\xFF\xFF\x12\x34\x5C"; // Mixed type patterns

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, mixed_data, &options);

        // Should handle mixed types safely without type processing panics
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    // Validate all fields produce appropriate types
                    if let Some(text_field) = obj.get("TEXT-FIELD") {
                        assert!(text_field.is_string(), "Text field should be string");
                    }
                    if let Some(numeric_field) = obj.get("NUMERIC-FIELD") {
                        assert!(
                            numeric_field.is_number() || numeric_field.is_string(),
                            "Numeric field should be number or string"
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("type")
                        || error.to_string().contains("field")
                        || error.to_string().contains("validation"),
                    "Type validation error should reference type issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-9-5 Record layout consistency safety
    fn test_record_layout_consistency_panic_elimination() {
        // Test case: Complex record layouts that stress layout consistency
        let complex_layout_schema_copybook = r"
        01 COMPLEX-RECORD.
            05 HEADER-SECTION.
                10 RECORD-TYPE PIC X(4).
                10 RECORD-LENGTH PIC 9(5).
            05 DATA-SECTION.
                10 PRIMARY-KEY PIC 9(10).
                10 SECONDARY-KEY PIC X(20).
                10 FLAGS.
                    15 FLAG-A PIC X.
                    15 FLAG-B PIC X.
                    15 FLAG-C REDEFINES FLAG-B PIC 9.
                10 METADATA PIC X(100).
            05 TRAILER-SECTION.
                10 CHECKSUM PIC 9(8).
                10 END-MARKER PIC X(4).
        ";
        let schema = parse_copybook(complex_layout_schema_copybook).expect("Schema should parse");

        // Complex record data with potential layout issues
        let complex_data = vec![b'C'; 200]; // Sufficient data for complex layout

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &complex_data, &options);

        // Should handle complex layouts safely without consistency panics
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(
                        obj.len() >= 7,
                        "Complex record should produce all major fields"
                    );

                    // Validate nested structure handling
                    for (field_name, field_value) in obj {
                        assert!(
                            field_value.is_string()
                                || field_value.is_number()
                                || field_value.is_object(),
                            "Field {} should have valid value type",
                            field_name
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("layout")
                        || error.to_string().contains("consistency")
                        || error.to_string().contains("record"),
                    "Layout consistency error should reference layout issue: {}",
                    error
                );
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_memory_tests {
    use super::*;

    /// Tests memory.rs panic elimination (11 instances)
    /// AC:63-10 - Memory management with safe buffer operations

    #[test] // AC:63-10-1 Buffer allocation bounds safety
    fn test_buffer_allocation_bounds_panic_elimination() {
        // Test case: Memory buffer allocations with extreme sizes
        let large_buffer_schema_copybook = "01 RECORD.\n    05 LARGE-FIELD PIC X(10000).";
        let schema = parse_copybook(large_buffer_schema_copybook).expect("Schema should parse");

        // Large record requiring significant buffer allocation
        let large_record_data = vec![b'L'; 10000];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &large_record_data, &options);

        // Should handle large buffer allocations safely
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object()
                    && let Some(field_value) = obj.get("LARGE-FIELD")
                {
                    assert!(
                        field_value.is_string(),
                        "Large field should be string value"
                    );
                    if let Some(string_val) = field_value.as_str() {
                        assert!(
                            string_val.len() <= 10000,
                            "Large field should have reasonable size"
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("buffer")
                        || error.to_string().contains("allocation")
                        || error.to_string().contains("memory"),
                    "Buffer allocation error should reference memory issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-10-2 Scratch buffer management safety
    fn test_scratch_buffer_management_panic_elimination() {
        // Test case: Multiple field processing requiring scratch buffer management
        let multi_field_schema_copybook = r"
        01 RECORD.
            05 FIELD-01 PIC X(100).
            05 FIELD-02 PIC X(100).
            05 FIELD-03 PIC X(100).
            05 FIELD-04 PIC X(100).
            05 FIELD-05 PIC X(100).
        ";
        let schema = parse_copybook(multi_field_schema_copybook).expect("Schema should parse");

        // Multi-field record data
        let multi_field_data = vec![b'M'; 500];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &multi_field_data, &options);

        // Should handle scratch buffer management safely
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(
                        obj.len() >= 5,
                        "Multi-field record should produce all fields"
                    );

                    // Validate all fields processed correctly with buffer management
                    for i in 1..=5 {
                        let field_name = format!("FIELD-{:02}", i);
                        assert!(
                            obj.contains_key(&field_name),
                            "Should contain field {}",
                            field_name
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("buffer")
                        || error.to_string().contains("scratch")
                        || error.to_string().contains("memory"),
                    "Scratch buffer error should reference buffer issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-10-3 Memory reuse efficiency safety
    fn test_memory_reuse_efficiency_panic_elimination() {
        // Test case: Repeated record processing testing memory reuse
        let reuse_schema_copybook = "01 RECORD.\n    05 REUSE-FIELD PIC X(50).";
        let schema = parse_copybook(reuse_schema_copybook).expect("Schema should parse");

        let reuse_record_data = vec![b'R'; 50];
        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        // Process multiple records to test memory reuse
        for i in 0..10 {
            let result = decode_record(&schema, &reuse_record_data, &options);

            match result {
                Ok(value) => {
                    if let Some(obj) = value.as_object() {
                        assert!(
                            obj.contains_key("REUSE-FIELD"),
                            "Iteration {} should contain reuse field",
                            i
                        );
                    }
                }
                Err(error) => {
                    assert!(
                        error.to_string().contains("reuse")
                            || error.to_string().contains("memory")
                            || error.to_string().contains("buffer"),
                        "Memory reuse error should reference reuse issue: {}",
                        error
                    );
                }
            }
        }
    }

    #[test] // AC:63-10-4 Buffer overflow protection
    fn test_buffer_overflow_protection_panic_elimination() {
        // Test case: Potential buffer overflow scenarios
        let overflow_schema_copybook = "01 RECORD.\n    05 OVERFLOW-FIELD PIC X(1000).";
        let schema = parse_copybook(overflow_schema_copybook).expect("Schema should parse");

        // Record data that could cause buffer overflow (oversized)
        let overflow_record_data = vec![b'O'; 2000]; // Larger than expected

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &overflow_record_data, &options);

        // Should handle potential overflow safely
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object()
                    && let Some(field_value) = obj.get("OVERFLOW-FIELD")
                {
                    assert!(
                        field_value.is_string(),
                        "Overflow field should be string value"
                    );
                    // Should truncate safely to expected size
                    if let Some(string_val) = field_value.as_str() {
                        assert!(
                            string_val.len() <= 1000,
                            "Overflow field should be bounded to schema size"
                        );
                    }
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("overflow")
                        || error.to_string().contains("buffer")
                        || error.to_string().contains("bounds"),
                    "Buffer overflow error should reference overflow issue: {}",
                    error
                );
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_iterator_tests {
    use super::*;

    /// Tests iterator.rs panic elimination (11 instances)
    /// AC:63-11 - Iterator processing with safe advancement and bounds checking

    #[test] // AC:63-11-1 Iterator bounds checking safety
    fn test_iterator_bounds_checking_panic_elimination() {
        // Test case: Iterator advancement beyond data bounds
        let iterator_schema_copybook = "01 RECORD.\n    05 ITER-FIELD PIC X(20).";
        let schema = parse_copybook(iterator_schema_copybook).expect("Schema should parse");

        // Minimal data for iterator testing
        let iterator_data = vec![b'I'; 20];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        // Test single record processing (simulating iterator usage)
        let result = decode_record(&schema, &iterator_data, &options);

        // Should handle iterator bounds safely
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(
                        obj.contains_key("ITER-FIELD"),
                        "Iterator record should contain field"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("iterator")
                        || error.to_string().contains("bounds")
                        || error.to_string().contains("advancement"),
                    "Iterator bounds error should reference iterator issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-11-2 Record streaming safety
    fn test_record_streaming_panic_elimination() {
        // Test case: Streaming multiple records with potential state issues
        let streaming_schema_copybook = r"
        01 RECORD.
            05 STREAM-ID PIC 9(5).
            05 STREAM-DATA PIC X(15).
        ";
        let schema = parse_copybook(streaming_schema_copybook).expect("Schema should parse");

        // Multiple concatenated records for streaming simulation
        let streaming_data = b"12345STREAMDATA001754321STREAMDATA002";

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        // Process first record (streaming simulation)
        let first_record_data = &streaming_data[0..20];
        let result1 = decode_record(&schema, first_record_data, &options);

        // Should handle streaming record processing safely
        match result1 {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(
                        obj.contains_key("STREAM-ID") && obj.contains_key("STREAM-DATA"),
                        "Streaming record should contain all fields"
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("streaming")
                        || error.to_string().contains("record")
                        || error.to_string().contains("state"),
                    "Streaming error should reference streaming issue: {}",
                    error
                );
            }
        }

        // Process second record (continuing stream simulation)
        if streaming_data.len() >= 40 {
            let second_record_data = &streaming_data[20..40];
            let result2 = decode_record(&schema, second_record_data, &options);

            match result2 {
                Ok(value) => {
                    if let Some(obj) = value.as_object() {
                        assert!(
                            obj.contains_key("STREAM-ID") && obj.contains_key("STREAM-DATA"),
                            "Second streaming record should contain all fields"
                        );
                    }
                }
                Err(_) => {
                    // Second record processing may fail - that's acceptable for streaming test
                }
            }
        }
    }

    #[test] // AC:63-11-3 Iterator state consistency safety
    fn test_iterator_state_consistency_panic_elimination() {
        // Test case: Iterator state consistency across multiple operations
        let state_schema_copybook = r"
        01 RECORD.
            05 STATE-COUNTER PIC 9(3).
            05 STATE-FLAGS PIC X(5).
            05 STATE-DATA PIC X(12).
        ";
        let schema = parse_copybook(state_schema_copybook).expect("Schema should parse");

        // State test records with different patterns
        let state_test_records = vec![
            vec![b'0'; 20], // Zero pattern
            vec![b'1'; 20], // One pattern
            vec![b'2'; 20], // Two pattern
        ];

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        // Process multiple records testing state consistency
        for (i, record_data) in state_test_records.iter().enumerate() {
            let result = decode_record(&schema, record_data, &options);

            match result {
                Ok(value) => {
                    if let Some(obj) = value.as_object() {
                        assert!(
                            obj.len() >= 3,
                            "State record {} should contain all fields",
                            i
                        );
                    }
                }
                Err(error) => {
                    assert!(
                        error.to_string().contains("state")
                            || error.to_string().contains("consistency")
                            || error.to_string().contains("iterator"),
                        "State consistency error should reference state issue: {}",
                        error
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod panic_elimination_performance_tests {
    use super::*;

    /// Performance validation for codec panic elimination changes
    /// AC:63-12 - Performance impact <5% maintaining enterprise throughput targets

    #[test] // AC:63-12-1 DISPLAY format performance preservation
    fn test_display_format_performance_preservation() {
        // Test case: DISPLAY format processing performance with panic elimination
        let display_schema_copybook = r"
        01 DISPLAY-PERFORMANCE-RECORD.
            05 DISPLAY-FIELDS OCCURS 100 TIMES.
                10 DISPLAY-ID PIC 9(10).
                10 DISPLAY-TEXT PIC X(50).
                10 DISPLAY-AMOUNT PIC 9(8)V99.
        ";
        let schema = parse_copybook(display_schema_copybook).expect("Schema should parse");

        // Large DISPLAY record for performance testing
        let display_record_data = vec![b'D'; 6200]; // 100 * 62 bytes per occurrence

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        let start_time = std::time::Instant::now();
        let result = decode_record(&schema, &display_record_data, &options);
        let decode_duration = start_time.elapsed();

        // Should maintain DISPLAY processing performance
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(
                        obj.contains_key("DISPLAY-FIELDS"),
                        "Should contain DISPLAY fields array"
                    );
                }

                // Performance should be reasonable (target: >2.33 GiB/s throughput)
                assert!(
                    decode_duration.as_millis() < 100,
                    "DISPLAY processing should be fast: {:?}",
                    decode_duration
                );
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("DISPLAY")
                        || error.to_string().contains("performance"),
                    "DISPLAY performance error should reference DISPLAY issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-12-2 COMP-3 format performance preservation
    fn test_comp3_format_performance_preservation() {
        // Test case: COMP-3 format processing performance with panic elimination
        let comp3_schema_copybook = r"
        01 COMP3-PERFORMANCE-RECORD.
            05 COMP3-FIELDS OCCURS 50 TIMES.
                10 COMP3-ID PIC 9(8) COMP-3.
                10 COMP3-AMOUNT PIC S9(11)V99 COMP-3.
                10 COMP3-RATE PIC S9(5)V9999 COMP-3.
        ";
        let schema = parse_copybook(comp3_schema_copybook).expect("Schema should parse");

        // COMP-3 record with valid packed decimal data
        let mut comp3_record_data = Vec::new();
        for _ in 0..50 {
            // Valid COMP-3 data patterns
            comp3_record_data.extend_from_slice(&[0x12, 0x34, 0x56, 0x7C]); // ID: 1234567 positive
            comp3_record_data.extend_from_slice(&[0x98, 0x76, 0x54, 0x32, 0x10, 0x9C]); // Amount: 987654321.09 positive
            comp3_record_data.extend_from_slice(&[0x12, 0x34, 0x5C]); // Rate: 123.45 positive
        }

        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_json_number_mode(JsonNumberMode::Lossless);

        let start_time = std::time::Instant::now();
        let result = decode_record(&schema, &comp3_record_data, &options);
        let decode_duration = start_time.elapsed();

        // Should maintain COMP-3 processing performance
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(
                        obj.contains_key("COMP3-FIELDS"),
                        "Should contain COMP-3 fields array"
                    );
                }

                // Performance should be reasonable (target: >168 MiB/s throughput)
                assert!(
                    decode_duration.as_millis() < 50,
                    "COMP-3 processing should be fast: {:?}",
                    decode_duration
                );
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("COMP-3")
                        || error.to_string().contains("performance"),
                    "COMP-3 performance error should reference COMP-3 issue: {}",
                    error
                );
            }
        }
    }

    #[test] // AC:63-12-3 Memory efficiency preservation
    fn test_memory_efficiency_preservation() {
        // Test case: Memory usage efficiency with panic elimination overhead
        let memory_schema_copybook = r"
        01 MEMORY-EFFICIENCY-RECORD.
            05 LARGE-TEXT-FIELD PIC X(1000).
            05 NUMERIC-FIELDS OCCURS 100 TIMES.
                10 NUMERIC-VALUE PIC 9(10).
        ";
        let schema = parse_copybook(memory_schema_copybook).expect("Schema should parse");

        // Large record for memory efficiency testing
        let memory_record_data = vec![b'M'; 2000]; // 2KB record

        let options = DecodeOptions::new().with_codepage(Codepage::CP037);
        let result = decode_record(&schema, &memory_record_data, &options);

        // Should maintain memory efficiency
        match result {
            Ok(value) => {
                if let Some(obj) = value.as_object() {
                    assert!(obj.len() >= 2, "Memory test should produce major fields");

                    // Validate memory usage is reasonable
                    let value_size = serde_json::to_string(&value).map(|s| s.len()).unwrap_or(0);

                    assert!(
                        value_size < 10000, // Should be under 10KB serialized
                        "Memory usage should be efficient: {} bytes",
                        value_size
                    );
                }
            }
            Err(error) => {
                assert!(
                    error.to_string().contains("memory")
                        || error.to_string().contains("efficiency"),
                    "Memory efficiency error should reference memory issue: {}",
                    error
                );
            }
        }
    }
}
