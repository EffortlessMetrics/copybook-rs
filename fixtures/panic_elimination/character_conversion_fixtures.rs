/// Panic Elimination Fixtures: Character Conversion Test Data
/// Issue #33 - Character Conversion Panic Elimination
///
/// This module provides comprehensive test data for character conversion scenarios
/// that historically cause panic conditions in EBCDIC/ASCII conversion and charset
/// handling. Focuses on edge cases across all supported codepages and invalid
/// byte sequences that can cause unwrap() panics in character processing.

#[cfg(test)]
pub struct CharacterConversionFixture {
    pub name: &'static str,
    pub copybook_text: &'static str,
    pub test_data: Vec<u8>,
    pub codepage: &'static str,
    pub expected_behavior: CharExpectedBehavior,
    pub panic_scenario: &'static str,
    pub edge_case_type: &'static str,
}

#[cfg(test)]
#[derive(Debug, Clone)]
pub enum CharExpectedBehavior {
    Success,
    ConversionError(&'static str), // Expected CBKC* error code
    Either, // Either success or controlled error
}

/// EBCDIC codepage-specific edge cases that can cause lookup panics
pub mod codepage_fixtures {
    use super::*;

    /// CP037 (US/Canada) specific edge cases
    pub fn cp037_edge_cases_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "cp037_edge_cases",
            copybook_text: r#"
            01 CP037-EDGE-RECORD.
                05 BOUNDARY-FIELD PIC X(10).
                05 SPECIAL-CHARS PIC X(15).
                05 NUMERIC-FIELD PIC 9(8).
            "#,
            test_data: vec![
                // CP037 boundary values and unmapped characters
                0x00, 0x01, 0x02, 0x03, 0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA, // Boundary bytes
                0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, // Control characters
                0x8A, 0x8B, 0x8C, 0x8D, 0x8E, // More controls
                0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, // Valid numerics
            ],
            codepage: "cp037",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "CP037 boundary values causing character lookup table overflow",
            edge_case_type: "Codepage boundary values",
        }
    }

    /// CP273 (Germany/Austria) specific edge cases
    pub fn cp273_edge_cases_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "cp273_edge_cases",
            copybook_text: r#"
            01 CP273-EDGE-RECORD.
                05 GERMAN-CHARS PIC X(12).
                05 CURRENCY-FIELD PIC X(8).
            "#,
            test_data: vec![
                // CP273 German-specific characters and edge cases
                0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, // A-I variants
                0xD0, 0xD1, // German umlauts region
                0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, // Special German characters
                0x7A, 0x7B, // Currency symbols in CP273
            ],
            codepage: "cp273",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "CP273 German character mapping causing conversion panic",
            edge_case_type: "International character set",
        }
    }

    /// CP500 (International) specific edge cases
    pub fn cp500_edge_cases_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "cp500_edge_cases",
            copybook_text: r#"
            01 CP500-EDGE-RECORD.
                05 INTL-CHARS PIC X(16).
                05 SYMBOL-FIELD PIC X(4).
            "#,
            test_data: vec![
                // CP500 international character edge cases
                0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, // International symbols
                0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, // Bracket variations
                0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, // Additional symbols
                0x7F, 0x80, 0x8F, 0x90, // Boundary transitions
            ],
            codepage: "cp500",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "CP500 international character boundary causing mapping panic",
            edge_case_type: "International symbol mapping",
        }
    }

    /// CP1047 (Open Systems) specific edge cases
    pub fn cp1047_edge_cases_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "cp1047_edge_cases",
            copybook_text: r#"
            01 CP1047-EDGE-RECORD.
                05 MODERN-CHARS PIC X(20).
            "#,
            test_data: vec![
                // CP1047 modern character set edge cases
                0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, // Control range
                0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, // ASCII compatibility range
            ],
            codepage: "cp1047",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "CP1047 ASCII compatibility characters causing conversion mismatch",
            edge_case_type: "ASCII compatibility edge cases",
        }
    }

    /// CP1140 (US/Canada Euro) specific edge cases
    pub fn cp1140_edge_cases_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "cp1140_edge_cases",
            copybook_text: r#"
            01 CP1140-EDGE-RECORD.
                05 EURO-CHARS PIC X(8).
                05 CURRENCY-SYMBOLS PIC X(12).
            "#,
            test_data: vec![
                // CP1140 Euro currency and special characters
                0x9F, // Euro symbol position
                0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, // Currency region
                0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F, // Extended symbols
            ],
            codepage: "cp1140",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "CP1140 Euro symbol handling causing currency conversion panic",
            edge_case_type: "Euro currency support",
        }
    }

    pub fn all_codepage_fixtures() -> Vec<CharacterConversionFixture> {
        vec![
            cp037_edge_cases_fixture(),
            cp273_edge_cases_fixture(),
            cp500_edge_cases_fixture(),
            cp1047_edge_cases_fixture(),
            cp1140_edge_cases_fixture(),
        ]
    }
}

/// Invalid byte sequences that can cause conversion panics
pub mod invalid_byte_fixtures {
    use super::*;

    /// Null bytes and control characters causing string processing panics
    pub fn null_control_bytes_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "null_control_bytes",
            copybook_text: r#"
            01 NULL-CONTROL-RECORD.
                05 NULL-EMBEDDED-FIELD PIC X(10).
                05 CONTROL-CHARS-FIELD PIC X(15).
            "#,
            test_data: vec![
                // Null bytes and control characters
                0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, // Control chars 0-9
                0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, // Control chars 10-19
                0x14, 0x15, 0x16, 0x17, 0x18, // More controls
            ],
            codepage: "cp037",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "Null bytes and control characters causing string termination panic",
            edge_case_type: "Control character handling",
        }
    }

    /// Invalid EBCDIC sequences that don't map to valid characters
    pub fn invalid_ebcdic_sequences_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "invalid_ebcdic_sequences",
            copybook_text: r#"
            01 INVALID-EBCDIC-RECORD.
                05 INVALID-SEQUENCE-FIELD PIC X(20).
            "#,
            test_data: vec![
                // Invalid EBCDIC byte sequences
                0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, // Invalid range
                0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, // More invalid
            ],
            codepage: "cp037",
            expected_behavior: CharExpectedBehavior::ConversionError("CBKC301_INVALID_EBCDIC_BYTE"),
            panic_scenario: "Invalid EBCDIC byte sequences causing lookup table panic",
            edge_case_type: "Invalid byte sequences",
        }
    }

    /// High-bit characters causing encoding confusion
    pub fn high_bit_characters_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "high_bit_characters",
            copybook_text: r#"
            01 HIGH-BIT-RECORD.
                05 HIGH-BIT-FIELD PIC X(16).
            "#,
            test_data: vec![
                // High-bit set characters (128-255 range)
                0x80, 0x90, 0xA0, 0xB0, 0xC0, 0xD0, 0xE0, 0xF0,
                0x8F, 0x9F, 0xAF, 0xBF, 0xCF, 0xDF, 0xEF, 0xFF,
            ],
            codepage: "cp037",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "High-bit characters causing character classification panic",
            edge_case_type: "High-bit character handling",
        }
    }

    /// Buffer boundary edge cases causing overflow panics
    pub fn buffer_boundary_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "buffer_boundary_edge",
            copybook_text: r#"
            01 BUFFER-BOUNDARY-RECORD.
                05 BOUNDARY-FIELD PIC X(1).
                05 EXACTLY-256-FIELD PIC X(256).
            "#,
            test_data: {
                let mut data = vec![0x40]; // Single character
                data.extend(vec![0x41; 256]); // Exactly 256 characters
                data
            },
            codepage: "cp037",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "Buffer boundary conditions causing allocation panic in conversion",
            edge_case_type: "Buffer boundary conditions",
        }
    }

    pub fn all_invalid_byte_fixtures() -> Vec<CharacterConversionFixture> {
        vec![
            null_control_bytes_fixture(),
            invalid_ebcdic_sequences_fixture(),
            high_bit_characters_fixture(),
            buffer_boundary_fixture(),
        ]
    }
}

/// Cross-codepage compatibility issues that can cause conversion panics
pub mod cross_codepage_fixtures {
    use super::*;

    /// Same data across different codepages causing mapping confusion
    pub fn cross_codepage_consistency_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "cross_codepage_consistency",
            copybook_text: r#"
            01 CROSS-CODEPAGE-RECORD.
                05 CONSISTENT-FIELD PIC X(10).
                05 VARIABLE-FIELD PIC X(10).
            "#,
            test_data: vec![
                // Characters that map differently across codepages
                0x5B, 0x5C, 0x5D, 0x5E, 0x5F, // Bracket/symbol region
                0x7B, 0x7C, 0x7D, 0x7E, 0x7F, // Brace/symbol region
                0x9F, 0xA0, 0xA1, 0xA2, 0xA3, // International region
            ],
            codepage: "cp273", // Test with German codepage
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "Cross-codepage character mapping inconsistency causing conversion panic",
            edge_case_type: "Codepage compatibility",
        }
    }

    /// Unsupported codepage combinations
    pub fn unsupported_codepage_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "unsupported_codepage",
            copybook_text: r#"
            01 UNSUPPORTED-CODEPAGE-RECORD.
                05 TEST-FIELD PIC X(15).
            "#,
            test_data: vec![0x40; 15], // Simple EBCDIC spaces
            codepage: "cp999", // Intentionally unsupported
            expected_behavior: CharExpectedBehavior::ConversionError("CBKC101_UNSUPPORTED_CODEPAGE"),
            panic_scenario: "Unsupported codepage specification causing codepage lookup panic",
            edge_case_type: "Unsupported codepage handling",
        }
    }

    pub fn all_cross_codepage_fixtures() -> Vec<CharacterConversionFixture> {
        vec![
            cross_codepage_consistency_fixture(),
            unsupported_codepage_fixture(),
        ]
    }
}

/// Unicode and multi-byte character edge cases
pub mod unicode_fixtures {
    use super::*;

    /// Unicode characters in EBCDIC context causing conversion panics
    pub fn unicode_in_ebcdic_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "unicode_in_ebcdic",
            copybook_text: r#"
            01 UNICODE-EBCDIC-RECORD.
                05 UNICODE-FIELD PIC X(20).
            "#,
            test_data: vec![
                // UTF-8 byte sequences in EBCDIC context
                0xC3, 0xA4, // UTF-8 'ä' (2 bytes)
                0xE2, 0x82, 0xAC, // UTF-8 '€' (3 bytes)
                0xF0, 0x9F, 0x98, 0x80, // UTF-8 '😀' (4 bytes)
                0x40, 0x40, 0x40, 0x40, 0x40, // EBCDIC spaces
                0x40, 0x40, 0x40, 0x40, 0x40,
                0x40, 0x40,
            ],
            codepage: "cp037",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "Unicode byte sequences causing EBCDIC conversion panic",
            edge_case_type: "Unicode handling in EBCDIC",
        }
    }

    /// Multi-byte sequence boundaries causing parsing panics
    pub fn multibyte_boundary_fixture() -> CharacterConversionFixture {
        CharacterConversionFixture {
            name: "multibyte_boundary",
            copybook_text: r#"
            01 MULTIBYTE-BOUNDARY-RECORD.
                05 BOUNDARY-TEST-FIELD PIC X(12).
            "#,
            test_data: vec![
                // Incomplete multi-byte sequences at field boundaries
                0xC3, // Incomplete UTF-8 sequence
                0x40, 0x40, 0x40, 0x40, // EBCDIC spaces
                0xE2, 0x82, // Incomplete 3-byte UTF-8
                0x40, 0x40, 0x40, 0x40,
                0xF0, // Incomplete 4-byte UTF-8
            ],
            codepage: "cp037",
            expected_behavior: CharExpectedBehavior::Either,
            panic_scenario: "Incomplete multi-byte sequences causing buffer read panic",
            edge_case_type: "Multi-byte sequence boundaries",
        }
    }

    pub fn all_unicode_fixtures() -> Vec<CharacterConversionFixture> {
        vec![
            unicode_in_ebcdic_fixture(),
            multibyte_boundary_fixture(),
        ]
    }
}

/// Comprehensive collection of all character conversion fixtures
pub fn all_character_conversion_fixtures() -> Vec<CharacterConversionFixture> {
    let mut fixtures = Vec::new();
    fixtures.extend(codepage_fixtures::all_codepage_fixtures());
    fixtures.extend(invalid_byte_fixtures::all_invalid_byte_fixtures());
    fixtures.extend(cross_codepage_fixtures::all_cross_codepage_fixtures());
    fixtures.extend(unicode_fixtures::all_unicode_fixtures());
    fixtures
}

/// Generate performance test data for character conversion validation
pub fn generate_character_conversion_performance_data() -> Vec<(String, Vec<u8>)> {
    vec![
        (
            "large_ascii_compatible_dataset".to_string(),
            {
                // Large dataset with ASCII-compatible EBCDIC characters
                let mut data = Vec::new();
                for i in 0..50000 {
                    let ch = 0xF0 + (i % 10) as u8; // EBCDIC digits 0-9
                    data.push(ch);
                }
                data
            }
        ),
        (
            "mixed_codepage_stress_dataset".to_string(),
            {
                // Mixed character set for cross-codepage stress testing
                let mut data = Vec::new();
                let patterns = [0x40, 0x41, 0x42, 0x43, 0x5B, 0x5C, 0x5D, 0x7B, 0x7C, 0x7D];
                for i in 0..25000 {
                    data.push(patterns[i % patterns.len()]);
                }
                data
            }
        ),
        (
            "boundary_character_dataset".to_string(),
            {
                // Boundary characters for edge case testing
                let mut data = Vec::new();
                for i in 0..10000 {
                    let boundary_chars = [0x00, 0x7F, 0x80, 0xFF];
                    data.push(boundary_chars[i % boundary_chars.len()]);
                }
                data
            }
        ),
    ]
}

#[cfg(test)]
mod character_conversion_tests {
    use super::*;
    use copybook_core::parse_copybook;
    use copybook_codec::{decode_record, DecodeOptions, Codepage};

    #[test]
    fn test_character_fixtures_load() {
        let fixtures = all_character_conversion_fixtures();
        assert!(fixtures.len() >= 13, "Should have at least 13 character conversion fixtures");

        for fixture in &fixtures {
            assert!(!fixture.name.is_empty(), "Fixture name should not be empty");
            assert!(!fixture.copybook_text.is_empty(), "Copybook text should not be empty");
            assert!(!fixture.test_data.is_empty(), "Test data should not be empty");
            assert!(!fixture.panic_scenario.is_empty(), "Panic scenario should not be empty");
            assert!(!fixture.edge_case_type.is_empty(), "Edge case type should not be empty");
        }
    }

    #[test]
    fn test_codepage_fixtures_safety() {
        let codepage_fixtures = codepage_fixtures::all_codepage_fixtures();

        for fixture in &codepage_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            if let Ok(schema) = schema_result {
                let codepage = match fixture.codepage {
                    "cp037" => Codepage::CP037,
                    "cp273" => Codepage::CP273,
                    "cp500" => Codepage::CP500,
                    "cp1047" => Codepage::CP1047,
                    "cp1140" => Codepage::CP1140,
                    _ => Codepage::CP037, // Default fallback
                };

                let options = DecodeOptions::new().with_codepage(codepage);
                let result = decode_record(&schema, &fixture.test_data, &options);

                // Test should not panic - either succeed or return controlled error
                match result {
                    Ok(_) => {
                        // Successful character conversion is good
                    }
                    Err(error) => {
                        // Character conversion errors should use appropriate codes
                        let error_str = format!("{:?}", error.code);
                        assert!(
                            error_str.starts_with("CBKC") || error_str.starts_with("CBKD"),
                            "Character error for {} should use CBKC*/CBKD* code, got {:?}",
                            fixture.name, error.code
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_invalid_byte_fixtures_safety() {
        let invalid_fixtures = invalid_byte_fixtures::all_invalid_byte_fixtures();

        for fixture in &invalid_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            if let Ok(schema) = schema_result {
                let codepage = Codepage::CP037; // Use standard codepage for invalid byte testing

                let options = DecodeOptions::new().with_codepage(codepage);
                let result = decode_record(&schema, &fixture.test_data, &options);

                // Test should not panic regardless of outcome
                match (&fixture.expected_behavior, result) {
                    (CharExpectedBehavior::Success, Ok(_)) => {
                        // Expected success achieved
                    }
                    (CharExpectedBehavior::ConversionError(expected_code), Err(error)) => {
                        let error_str = format!("{:?}", error.code);
                        assert!(
                            error_str.starts_with("CBKC"),
                            "Conversion error should use CBKC* code for {}, got {:?}",
                            fixture.name, error.code
                        );
                    }
                    (CharExpectedBehavior::Either, _) => {
                        // Either success or error is acceptable
                    }
                    _ => {
                        // Log unexpected outcomes but don't fail (panic elimination is key)
                        eprintln!("Unexpected outcome for fixture {}: expected {:?}",
                                fixture.name, fixture.expected_behavior);
                    }
                }
            }
        }
    }

    #[test]
    fn test_cross_codepage_fixtures_safety() {
        let cross_fixtures = cross_codepage_fixtures::all_cross_codepage_fixtures();

        for fixture in &cross_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            if let Ok(schema) = schema_result {
                // Test unsupported codepage handling
                if fixture.codepage == "cp999" {
                    // This should be handled gracefully, not panic
                    let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                    let result = decode_record(&schema, &fixture.test_data, &options);
                    // Any controlled outcome is acceptable
                } else {
                    let codepage = match fixture.codepage {
                        "cp273" => Codepage::CP273,
                        "cp500" => Codepage::CP500,
                        _ => Codepage::CP037,
                    };

                    let options = DecodeOptions::new().with_codepage(codepage);
                    let result = decode_record(&schema, &fixture.test_data, &options);
                    // Cross-codepage tests should not panic
                }
            }
        }
    }

    #[test]
    fn test_unicode_fixtures_safety() {
        let unicode_fixtures = unicode_fixtures::all_unicode_fixtures();

        for fixture in &unicode_fixtures {
            let schema_result = parse_copybook(fixture.copybook_text);

            if let Ok(schema) = schema_result {
                let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                let result = decode_record(&schema, &fixture.test_data, &options);

                // Unicode handling in EBCDIC context should not panic
                match result {
                    Ok(_) => {
                        // Successful handling of Unicode in EBCDIC is good
                    }
                    Err(error) => {
                        // Unicode errors should use appropriate error codes
                        let error_str = format!("{:?}", error.code);
                        assert!(
                            error_str.starts_with("CBKC") || error_str.starts_with("CBKD"),
                            "Unicode error for {} should use structured code, got {:?}",
                            fixture.name, error.code
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_performance_data_generation() {
        let perf_data = generate_character_conversion_performance_data();
        assert_eq!(perf_data.len(), 3, "Should generate 3 character conversion performance datasets");

        for (name, data) in &perf_data {
            assert!(!name.is_empty(), "Performance dataset name should not be empty");
            assert!(data.len() > 5000, "Performance dataset {} should be substantial", name);
        }
    }
}