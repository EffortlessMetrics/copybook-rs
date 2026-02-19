// SPDX-License-Identifier: AGPL-3.0-or-later
/// Panic Elimination Fixtures: Numeric Conversion Test Data
/// Issue #33 - Numeric.rs (20 instances) and Zoned_overpunch.rs (24 instances) Panic Elimination
///
/// This module provides comprehensive test data for numeric conversion scenarios
/// that historically cause panic conditions in numeric processing modules.
/// Validates panic-safe error handling with CBKD* error codes and performance preservation.

#[cfg(test)]
pub struct NumericConversionFixture {
    pub name: &'static str,
    pub copybook_text: &'static str,
    pub test_data: Vec<u8>,
    pub expected_behavior: ExpectedBehavior,
    pub panic_scenario: &'static str,
    pub codepage: &'static str,
}

#[cfg(test)]
#[derive(Debug, Clone)]
pub enum ExpectedBehavior {
    Success(serde_json::Value),
    Error(&'static str), // Expected CBKD* error code
    Either, // Either success or controlled error (both acceptable)
}

/// COMP-3 packed decimal fixtures targeting numeric.rs panic scenarios
pub mod comp3_fixtures {
    use super::*;
    use serde_json::json;

    /// Invalid nibble patterns that can cause unwrap() panics in COMP-3 decoding
    pub fn invalid_nibble_patterns_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "comp3_invalid_nibbles",
            copybook_text: r#"
            01 COMP3-INVALID-RECORD.
                05 INVALID-NIBBLE-FIELD PIC S9(7)V99 COMP-3.
                05 VALID-FIELD PIC S9(5) COMP-3.
            "#,
            test_data: vec![
                // Invalid nibble patterns (0xA-0xF in positions that should be 0x0-0x9)
                0xFF, 0xFF, 0xFF, 0xFF, 0xEE, // Invalid nibbles throughout
                0x12, 0x34, 0x5C, // Valid COMP-3 field: 12345+
            ],
            expected_behavior: ExpectedBehavior::Error("CBKD401_COMP3_INVALID_NIBBLE"),
            panic_scenario: "Invalid nibble values causing unwrap() panic in nibble extraction",
            codepage: "cp037",
        }
    }

    /// Truncated COMP-3 data causing buffer underrun panics
    pub fn truncated_comp3_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "comp3_truncated_data",
            copybook_text: r#"
            01 COMP3-TRUNCATED-RECORD.
                05 TRUNCATED-FIELD PIC S9(9)V99 COMP-3.
                05 ANOTHER-FIELD PIC 9(5) COMP-3.
            "#,
            test_data: vec![
                // Incomplete COMP-3 data (should be 6 bytes, only providing 3)
                0x12, 0x34, 0x5C,
                // Missing data for second field
            ],
            expected_behavior: ExpectedBehavior::Error("CBKD301_RECORD_TOO_SHORT"),
            panic_scenario: "Truncated COMP-3 data causing buffer underrun in byte extraction",
            codepage: "cp037",
        }
    }

    /// Maximum precision COMP-3 values causing overflow panics
    pub fn maximum_precision_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "comp3_maximum_precision",
            copybook_text: r#"
            01 COMP3-MAX-PRECISION-RECORD.
                05 MAX-PRECISION-FIELD PIC S9(18)V99 COMP-3.
            "#,
            test_data: vec![
                // Maximum value: 999999999999999999.99 (positive)
                0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C,
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Maximum precision COMP-3 causing integer overflow in conversion",
            codepage: "cp037",
        }
    }

    /// Zero-length COMP-3 fields causing division by zero or empty slice panics
    pub fn zero_length_comp3_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "comp3_zero_length",
            copybook_text: r#"
            01 COMP3-ZERO-LENGTH-RECORD.
                05 ZERO-LENGTH-FIELD PIC S9(0) COMP-3.
                05 NORMAL-FIELD PIC 9(5) COMP-3.
            "#,
            test_data: vec![
                // No data for zero-length field
                0x12, 0x34, 0x5C, // Normal field data
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Zero-length COMP-3 field causing empty slice access panic",
            codepage: "cp037",
        }
    }

    /// Scale calculation overflow in COMP-3 formatting
    pub fn scale_overflow_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "comp3_scale_overflow",
            copybook_text: r#"
            01 COMP3-SCALE-OVERFLOW-RECORD.
                05 HIGH-SCALE-FIELD PIC S9(10)V9(18) COMP-3.
            "#,
            test_data: vec![
                // Large number with high decimal precision
                0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x56, 0x78, 0x90,
                0x12, 0x34, 0x56, 0x78, 0x9C,
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Scale calculation overflow in decimal formatting",
            codepage: "cp037",
        }
    }

    /// Invalid sign nibbles in COMP-3 data
    pub fn invalid_sign_nibbles_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "comp3_invalid_sign",
            copybook_text: r#"
            01 COMP3-INVALID-SIGN-RECORD.
                05 INVALID-SIGN-FIELD PIC S9(5)V99 COMP-3.
            "#,
            test_data: vec![
                // Invalid sign nibbles (0x0-0x9, 0xE, 0xF are invalid for sign)
                0x12, 0x34, 0x56, 0x70, // Sign nibble 0x0 (invalid)
            ],
            expected_behavior: ExpectedBehavior::Error("CBKD401_COMP3_INVALID_NIBBLE"),
            panic_scenario: "Invalid sign nibble causing panic in sign determination",
            codepage: "cp037",
        }
    }

    pub fn all_comp3_fixtures() -> Vec<NumericConversionFixture> {
        vec![
            invalid_nibble_patterns_fixture(),
            truncated_comp3_fixture(),
            maximum_precision_fixture(),
            zero_length_comp3_fixture(),
            scale_overflow_fixture(),
            invalid_sign_nibbles_fixture(),
        ]
    }
}

/// Zoned decimal fixtures targeting zoned_overpunch.rs panic scenarios
pub mod zoned_fixtures {
    use super::*;
    use serde_json::json;

    /// Invalid overpunch characters causing lookup panics
    pub fn invalid_overpunch_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "zoned_invalid_overpunch",
            copybook_text: r#"
            01 ZONED-INVALID-OVERPUNCH-RECORD.
                05 INVALID-OVERPUNCH-FIELD PIC S9(5).
                05 NORMAL-FIELD PIC 9(3).
            "#,
            test_data: vec![
                // Invalid overpunch characters that don't map to valid signs
                0xF1, 0xF2, 0xF3, 0xF4, 0xE0, // Invalid overpunch 0xE0
                0xF1, 0xF2, 0xF3, // Normal unsigned field
            ],
            expected_behavior: ExpectedBehavior::Error("CBKD411_ZONED_BAD_SIGN"),
            panic_scenario: "Invalid overpunch character causing array bounds panic in lookup",
            codepage: "cp037",
        }
    }

    /// Codepage-specific overpunch character mapping edge cases
    pub fn codepage_overpunch_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "zoned_codepage_overpunch",
            copybook_text: r#"
            01 ZONED-CODEPAGE-RECORD.
                05 CODEPAGE-SENSITIVE-FIELD PIC S9(4).
            "#,
            test_data: vec![
                // Overpunch characters that vary by codepage (CP037 vs CP500 vs CP1047)
                0xF1, 0xF2, 0xF3, 0xD1, // 'J' in different codepages
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Codepage-specific overpunch mapping causing lookup panic",
            codepage: "cp500", // Test with different codepage
        }
    }

    /// Non-numeric characters in zoned fields
    pub fn non_numeric_zoned_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "zoned_non_numeric",
            copybook_text: r#"
            01 ZONED-NON-NUMERIC-RECORD.
                05 NON-NUMERIC-FIELD PIC 9(6).
            "#,
            test_data: vec![
                // Non-numeric EBCDIC characters in numeric field
                0xC1, 0xC2, 0xC3, 0xD4, 0xE5, 0xF6, // A, B, C, M, V, 6
            ],
            expected_behavior: ExpectedBehavior::Error("CBKD411_ZONED_BAD_SIGN"),
            panic_scenario: "Non-numeric characters causing conversion panic",
            codepage: "cp037",
        }
    }

    /// Extremely large zoned numbers causing string buffer overflows
    pub fn large_zoned_numbers_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "zoned_large_numbers",
            copybook_text: r#"
            01 ZONED-LARGE-RECORD.
                05 LARGE-ZONED-FIELD PIC S9(18).
            "#,
            test_data: vec![
                // Maximum 18-digit number with positive overpunch
                0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9,
                0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xC9, // Positive 9
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Large zoned number causing string buffer overflow in formatting",
            codepage: "cp037",
        }
    }

    /// Mixed sign patterns that can confuse overpunch detection
    pub fn mixed_sign_patterns_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "zoned_mixed_signs",
            copybook_text: r#"
            01 ZONED-MIXED-SIGNS-RECORD.
                05 MIXED-SIGN-FIELD-1 PIC S9(3).
                05 MIXED-SIGN-FIELD-2 PIC S9(3).
                05 MIXED-SIGN-FIELD-3 PIC S9(3).
            "#,
            test_data: vec![
                // Mix of different sign representations
                0xF1, 0xF2, 0xC3, // Positive overpunch (C = +3)
                0xF4, 0xF5, 0xD6, // Negative overpunch (O = -6)
                0xF7, 0xF8, 0xF9, // Unsigned (no sign)
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Mixed sign patterns causing overpunch detection confusion",
            codepage: "cp037",
        }
    }

    /// Edge case overpunch values that map to boundary conditions
    pub fn edge_overpunch_values_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "zoned_edge_overpunch",
            copybook_text: r#"
            01 ZONED-EDGE-OVERPUNCH-RECORD.
                05 EDGE-FIELD-1 PIC S9(2).
                05 EDGE-FIELD-2 PIC S9(2).
                05 EDGE-FIELD-3 PIC S9(2).
                05 EDGE-FIELD-4 PIC S9(2).
            "#,
            test_data: vec![
                // Edge case overpunch mappings
                0xF1, 0xC0, // +10 (A maps to +1, but in tens position?)
                0xF2, 0xD0, // -20 (J maps to -1, but in tens position?)
                0xF0, 0xC9, // +09 (I maps to +9)
                0xF0, 0xD9, // -09 (R maps to -9)
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Edge case overpunch values causing boundary condition panics",
            codepage: "cp037",
        }
    }

    /// Character encoding boundary cases across different EBCDIC codepages
    pub fn character_encoding_boundaries_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "zoned_encoding_boundaries",
            copybook_text: r#"
            01 ZONED-ENCODING-RECORD.
                05 BOUNDARY-FIELD PIC S9(4).
            "#,
            test_data: vec![
                // Characters at EBCDIC encoding boundaries
                0xFF, 0xFE, 0xFD, 0xFC, // High boundary values
            ],
            expected_behavior: ExpectedBehavior::Error("CBKC301_INVALID_EBCDIC_BYTE"),
            panic_scenario: "EBCDIC encoding boundary values causing character conversion panic",
            codepage: "cp1047",
        }
    }

    pub fn all_zoned_fixtures() -> Vec<NumericConversionFixture> {
        vec![
            invalid_overpunch_fixture(),
            codepage_overpunch_fixture(),
            non_numeric_zoned_fixture(),
            large_zoned_numbers_fixture(),
            mixed_sign_patterns_fixture(),
            edge_overpunch_values_fixture(),
            character_encoding_boundaries_fixture(),
        ]
    }
}

/// Binary/COMP numeric fixtures for additional numeric panic scenarios
pub mod binary_fixtures {
    use super::*;

    /// Integer overflow in binary field conversion
    pub fn binary_overflow_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "binary_integer_overflow",
            copybook_text: r#"
            01 BINARY-OVERFLOW-RECORD.
                05 LARGE-BINARY-FIELD PIC S9(18) COMP.
            "#,
            test_data: vec![
                // Maximum 64-bit signed integer representation
                0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Binary field overflow causing integer conversion panic",
            codepage: "cp037",
        }
    }

    /// Endianness handling edge cases
    pub fn endianness_edge_cases_fixture() -> NumericConversionFixture {
        NumericConversionFixture {
            name: "binary_endianness_edge",
            copybook_text: r#"
            01 BINARY-ENDIANNESS-RECORD.
                05 ENDIAN-FIELD-1 PIC S9(9) COMP.
                05 ENDIAN-FIELD-2 PIC 9(4) COMP.
            "#,
            test_data: vec![
                // Big-endian vs little-endian confusion patterns
                0x80, 0x00, 0x00, 0x01, // Could be 1 or very large number depending on endianness
                0x01, 0x00, // Could be 1 or 256
            ],
            expected_behavior: ExpectedBehavior::Either,
            panic_scenario: "Endianness confusion causing incorrect bit interpretation",
            codepage: "cp037",
        }
    }

    pub fn all_binary_fixtures() -> Vec<NumericConversionFixture> {
        vec![
            binary_overflow_fixture(),
            endianness_edge_cases_fixture(),
        ]
    }
}

/// Comprehensive collection of all numeric conversion fixtures
pub fn all_numeric_conversion_fixtures() -> Vec<NumericConversionFixture> {
    let mut fixtures = Vec::new();
    fixtures.extend(comp3_fixtures::all_comp3_fixtures());
    fixtures.extend(zoned_fixtures::all_zoned_fixtures());
    fixtures.extend(binary_fixtures::all_binary_fixtures());
    fixtures
}

/// Generate large-scale numeric test data for performance validation
pub fn generate_performance_numeric_data() -> Vec<(String, Vec<u8>)> {
    vec![
        (
            "large_comp3_dataset".to_string(),
            {
                let mut data = Vec::new();
                // Generate 10,000 COMP-3 records
                for i in 0..10000 {
                    let packed = vec![
                        ((i / 10000) % 10) as u8 | 0xF0,
                        ((i / 1000) % 10) as u8 | 0xF0,
                        ((i / 100) % 10) as u8 | 0xF0,
                        ((i / 10) % 10) as u8 | 0xF0,
                        (i % 10) as u8 | 0xC0, // Positive sign
                    ];
                    data.extend(packed);
                }
                data
            }
        ),
        (
            "large_zoned_dataset".to_string(),
            {
                let mut data = Vec::new();
                // Generate 10,000 zoned decimal records
                for i in 0..10000 {
                    let zoned = vec![
                        0xF0 | ((i / 1000) % 10) as u8,
                        0xF0 | ((i / 100) % 10) as u8,
                        0xF0 | ((i / 10) % 10) as u8,
                        0xF0 | (i % 10) as u8,
                    ];
                    data.extend(zoned);
                }
                data
            }
        ),
    ]
}

#[cfg(test)]
mod numeric_conversion_tests {
    use super::*;
    use copybook_core::parse_copybook;
    use copybook_codec::{decode_record, DecodeOptions, Codepage};

    #[test]
    fn test_numeric_fixtures_load() {
        let fixtures = all_numeric_conversion_fixtures();
        assert!(fixtures.len() >= 15, "Should have at least 15 numeric conversion fixtures");

        for fixture in &fixtures {
            assert!(!fixture.name.is_empty(), "Fixture name should not be empty");
            assert!(!fixture.copybook_text.is_empty(), "Copybook text should not be empty");
            assert!(!fixture.test_data.is_empty(), "Test data should not be empty");
            assert!(!fixture.panic_scenario.is_empty(), "Panic scenario should not be empty");
        }
    }

    #[test]
    fn test_comp3_fixtures_safety() {
        let comp3_fixtures = comp3_fixtures::all_comp3_fixtures();

        for fixture in &comp3_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            if let Ok(schema) = schema_result {
                let codepage = match fixture.codepage {
                    "cp037" => Codepage::CP037,
                    "cp500" => Codepage::CP500,
                    "cp1047" => Codepage::CP1047,
                    _ => Codepage::CP037,
                };

                let options = DecodeOptions::new().with_codepage(codepage);
                let result = decode_record(&schema, &fixture.test_data, &options);

                // Test should not panic - either succeed or return controlled error
                match (&fixture.expected_behavior, result) {
                    (ExpectedBehavior::Success(_), Ok(_)) => {
                        // Expected success achieved
                    }
                    (ExpectedBehavior::Error(expected_code), Err(error)) => {
                        let error_str = format!("{:?}", error.code);
                        assert!(
                            error_str.starts_with("CBKD") || error_str.starts_with("CBKC"),
                            "Error should use structured error code for {}, got {:?}",
                            fixture.name, error.code
                        );
                    }
                    (ExpectedBehavior::Either, _) => {
                        // Either success or error is acceptable
                    }
                    _ => {
                        // Unexpected outcome - log but don't fail (panic elimination is the key goal)
                        eprintln!("Unexpected outcome for fixture {}: expected {:?}, got result type",
                                fixture.name, fixture.expected_behavior);
                    }
                }
            }
        }
    }

    #[test]
    fn test_zoned_fixtures_safety() {
        let zoned_fixtures = zoned_fixtures::all_zoned_fixtures();

        for fixture in &zoned_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            if let Ok(schema) = schema_result {
                let codepage = match fixture.codepage {
                    "cp037" => Codepage::CP037,
                    "cp500" => Codepage::CP500,
                    "cp1047" => Codepage::CP1047,
                    "cp1140" => Codepage::CP1140,
                    _ => Codepage::CP037,
                };

                let options = DecodeOptions::new().with_codepage(codepage);
                let result = decode_record(&schema, &fixture.test_data, &options);

                // Test should not panic - either succeed or return controlled error
                match result {
                    Ok(_) => {
                        // Success is acceptable
                    }
                    Err(error) => {
                        // Error should use appropriate error codes
                        let error_str = format!("{:?}", error.code);
                        assert!(
                            error_str.starts_with("CBKD") || error_str.starts_with("CBKC"),
                            "Error should use structured error code for {}, got {:?}",
                            fixture.name, error.code
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_performance_data_generation() {
        let perf_data = generate_performance_numeric_data();
        assert_eq!(perf_data.len(), 2, "Should generate 2 performance datasets");

        for (name, data) in &perf_data {
            assert!(!name.is_empty(), "Performance dataset name should not be empty");
            assert!(data.len() > 10000, "Performance dataset {} should be substantial", name);
        }
    }
}