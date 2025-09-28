//! Data Encoding Test Fixtures for copybook-codec Panic Elimination
//!
//! This module provides comprehensive test fixtures for eliminating 134 .unwrap()/.expect() calls
//! in copybook-codec production code. Tests target numeric.rs (21), zoned_overpunch.rs (24),
//! record.rs (32), memory.rs (11), iterator.rs (11), and other modules (35) with enterprise-safe
//! data processing patterns.
//!
//! **Issue #63 - AC Traceability:**
//! - AC1: Complete elimination of 134 .unwrap()/.expect() calls in copybook-codec
//! - AC2: Zero breaking changes to existing public APIs
//! - AC3: Integration with CBKD*/CBKE* error taxonomy for data processing
//! - AC4: Performance impact <5% maintaining 2.33+ GiB/s DISPLAY, 168+ MiB/s COMP-3
//! - AC7: Comprehensive test coverage for data encoding/decoding paths
//! - AC10: Memory safety preserved in numeric conversion algorithms

use std::sync::LazyLock;

/// COMP-3 Packed Decimal Edge Cases
pub struct Comp3TestFixture {
    pub copybook_text: &'static str,
    pub binary_data: Vec<u8>,
    pub description: &'static str,
    pub expected_error_pattern: Option<&'static str>,
    pub panic_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// DISPLAY Format Edge Cases with Character Encoding
pub struct DisplayTestFixture {
    pub copybook_text: &'static str,
    pub binary_data: Vec<u8>,
    pub description: &'static str,
    pub codepage: &'static str,
    pub panic_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// Zoned Overpunch Edge Cases
pub struct ZonedOverpunchFixture {
    pub copybook_text: &'static str,
    pub binary_data: Vec<u8>,
    pub description: &'static str,
    pub overpunch_char: Option<u8>,
    pub expected_sign: Option<&'static str>,
    pub ac_tag: &'static str,
}

/// Binary Integer Edge Cases
pub struct BinaryIntegerFixture {
    pub copybook_text: &'static str,
    pub binary_data: Vec<u8>,
    pub description: &'static str,
    pub endianness: &'static str,
    pub overflow_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// Record Processing Edge Cases
pub struct RecordProcessingFixture {
    pub copybook_text: &'static str,
    pub binary_data: Vec<u8>,
    pub description: &'static str,
    pub record_format: &'static str,
    pub processing_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// Memory Management Edge Cases
pub struct MemoryManagementFixture {
    pub copybook_text: &'static str,
    pub data_size_mb: usize,
    pub description: &'static str,
    pub memory_scenario: &'static str,
    pub target_memory_limit_mb: usize,
    pub ac_tag: &'static str,
}

/// COMP-3 Packed Decimal Test Data - AC:63-7 (21 instances)
pub static COMP3_EDGE_CASES: LazyLock<Vec<Comp3TestFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-7-1 - COMP-3 nibble extraction safety
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(5)V99 COMP-3.",
            binary_data: vec![0xFF, 0xFF, 0xFF, 0xFF], // All invalid nibbles
            description: "Invalid COMP-3 nibbles causing extraction panics",
            expected_error_pattern: Some("invalid|nibble|COMP-3"),
            panic_scenario: "COMP-3 nibble extraction with invalid data",
            ac_tag: "AC:63-7-1",
        },
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(3) COMP-3.",
            binary_data: vec![0xAB, 0xCD], // Invalid nibbles
            description: "COMP-3 data with invalid nibble combinations",
            expected_error_pattern: Some("invalid|nibble"),
            panic_scenario: "COMP-3 nibble validation with non-decimal nibbles",
            ac_tag: "AC:63-7-1",
        },
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(7)V99 COMP-3.",
            binary_data: vec![0x12, 0x34, 0x56, 0x7A, 0x8F], // Invalid sign nibble
            description: "COMP-3 with invalid sign nibble",
            expected_error_pattern: Some("sign|invalid"),
            panic_scenario: "COMP-3 sign nibble validation",
            ac_tag: "AC:63-7-1",
        },

        // AC:63-7-2 - Decimal digit validation safety
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC 9(1) COMP-3.",
            binary_data: vec![], // Empty data
            description: "COMP-3 field with no data causing bounds panic",
            expected_error_pattern: Some("truncated|insufficient"),
            panic_scenario: "COMP-3 bounds checking with empty data",
            ac_tag: "AC:63-7-2",
        },
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(15)V99 COMP-3.",
            binary_data: vec![0x12], // Insufficient data for large field
            description: "COMP-3 field with insufficient data for specified size",
            expected_error_pattern: Some("truncated|insufficient"),
            panic_scenario: "COMP-3 data length validation",
            ac_tag: "AC:63-7-2",
        },

        // AC:63-7-3 - Binary integer overflow protection
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(18)V99 COMP-3.",
            binary_data: vec![0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C], // Maximum positive value
            description: "COMP-3 maximum precision causing overflow",
            expected_error_pattern: None, // Should handle gracefully
            panic_scenario: "COMP-3 maximum precision bounds",
            ac_tag: "AC:63-7-3",
        },
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(18)V99 COMP-3.",
            binary_data: vec![0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9D], // Maximum negative value
            description: "COMP-3 maximum negative value",
            expected_error_pattern: None, // Should handle gracefully
            panic_scenario: "COMP-3 negative maximum bounds",
            ac_tag: "AC:63-7-3",
        },

        // AC:63-7-4 - Numeric precision bounds safety
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(1)V99999 COMP-3.",
            binary_data: vec![0x12, 0x34, 0x5C], // High decimal precision
            description: "COMP-3 with extreme decimal precision",
            expected_error_pattern: None, // Should handle gracefully
            panic_scenario: "COMP-3 decimal precision bounds",
            ac_tag: "AC:63-7-4",
        },

        // AC:63-7-5 - Zero and special values
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(5) COMP-3.",
            binary_data: vec![0x00, 0x00, 0x0C], // Positive zero
            description: "COMP-3 positive zero value",
            expected_error_pattern: None,
            panic_scenario: "COMP-3 zero value handling",
            ac_tag: "AC:63-7-5",
        },
        Comp3TestFixture {
            copybook_text: "01 RECORD.\n    05 COMP3-FIELD PIC S9(5) COMP-3.",
            binary_data: vec![0x00, 0x00, 0x0D], // Negative zero
            description: "COMP-3 negative zero value",
            expected_error_pattern: None,
            panic_scenario: "COMP-3 negative zero handling",
            ac_tag: "AC:63-7-5",
        },
    ]
});

/// DISPLAY Format Test Data with Character Encoding Edge Cases
pub static DISPLAY_EDGE_CASES: LazyLock<Vec<DisplayTestFixture>> = LazyLock::new(|| {
    vec![
        // Character encoding edge cases
        DisplayTestFixture {
            copybook_text: "01 RECORD.\n    05 DISPLAY-FIELD PIC 9(10).",
            binary_data: b"ABC123XYZ\x00".to_vec(), // Mixed alphanumeric in numeric field
            description: "DISPLAY numeric field with non-numeric characters",
            codepage: "CP037",
            panic_scenario: "DISPLAY character validation in numeric context",
            ac_tag: "AC:63-8-1",
        },
        DisplayTestFixture {
            copybook_text: "01 RECORD.\n    05 DISPLAY-FIELD PIC X(10).",
            binary_data: vec![0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09], // Control characters
            description: "DISPLAY field with control characters",
            codepage: "CP037",
            panic_scenario: "DISPLAY control character handling",
            ac_tag: "AC:63-8-1",
        },
        DisplayTestFixture {
            copybook_text: "01 RECORD.\n    05 DISPLAY-FIELD PIC X(5).",
            binary_data: vec![0xFF, 0xFE, 0xFD, 0xFC, 0xFB], // High-byte values
            description: "DISPLAY field with high-byte characters",
            codepage: "CP037",
            panic_scenario: "DISPLAY high-byte character conversion",
            ac_tag: "AC:63-8-1",
        },

        // Truncation and padding edge cases
        DisplayTestFixture {
            copybook_text: "01 RECORD.\n    05 DISPLAY-FIELD PIC X(10).",
            binary_data: b"SHORT".to_vec(), // Data shorter than field
            description: "DISPLAY field with insufficient data",
            codepage: "CP037",
            panic_scenario: "DISPLAY field truncation handling",
            ac_tag: "AC:63-8-2",
        },
        DisplayTestFixture {
            copybook_text: "01 RECORD.\n    05 DISPLAY-FIELD PIC X(5).",
            binary_data: b"TOOLONGDATA".to_vec(), // Data longer than field
            description: "DISPLAY field with excess data",
            codepage: "CP037",
            panic_scenario: "DISPLAY field overflow handling",
            ac_tag: "AC:63-8-2",
        },

        // EBCDIC conversion edge cases
        DisplayTestFixture {
            copybook_text: "01 RECORD.\n    05 DISPLAY-FIELD PIC X(10).",
            binary_data: vec![0x40, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50, 0x5A, 0x5B], // Special EBCDIC chars
            description: "DISPLAY field with special EBCDIC characters",
            codepage: "CP037",
            panic_scenario: "EBCDIC special character conversion",
            ac_tag: "AC:63-8-3",
        },
        DisplayTestFixture {
            copybook_text: "01 RECORD.\n    05 DISPLAY-FIELD PIC X(10).",
            binary_data: vec![0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, 0x9A, 0x9B, 0x9C, 0x9D], // Extended EBCDIC
            description: "DISPLAY field with extended EBCDIC characters",
            codepage: "CP500", // Different codepage
            panic_scenario: "Extended EBCDIC character conversion",
            ac_tag: "AC:63-8-3",
        },
    ]
});

/// Zoned Overpunch Test Data - AC:63-8 (24 instances)
pub static ZONED_OVERPUNCH_CASES: LazyLock<Vec<ZonedOverpunchFixture>> = LazyLock::new(|| {
    vec![
        // Valid overpunch characters
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(5).",
            binary_data: b"1234{".to_vec(), // Positive overpunch
            description: "Zoned decimal with positive overpunch character",
            overpunch_char: Some(b'{'),
            expected_sign: Some("positive"),
            ac_tag: "AC:63-8-1",
        },
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(5).",
            binary_data: b"1234}".to_vec(), // Negative overpunch
            description: "Zoned decimal with negative overpunch character",
            overpunch_char: Some(b'}'),
            expected_sign: Some("negative"),
            ac_tag: "AC:63-8-1",
        },

        // Invalid overpunch characters
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(5).",
            binary_data: b"1234@".to_vec(), // Invalid overpunch
            description: "Zoned decimal with invalid overpunch character",
            overpunch_char: Some(b'@'),
            expected_sign: None,
            ac_tag: "AC:63-8-2",
        },
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(5).",
            binary_data: b"1234\xFF".to_vec(), // Non-ASCII overpunch
            description: "Zoned decimal with non-ASCII overpunch character",
            overpunch_char: Some(0xFF),
            expected_sign: None,
            ac_tag: "AC:63-8-2",
        },

        // EBCDIC overpunch characters
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(3).",
            binary_data: vec![0xF1, 0xF2, 0xC3], // EBCDIC "12C" (positive 123 in EBCDIC)
            description: "EBCDIC zoned decimal with positive overpunch",
            overpunch_char: Some(0xC3),
            expected_sign: Some("positive"),
            ac_tag: "AC:63-8-3",
        },
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(3).",
            binary_data: vec![0xF1, 0xF2, 0xD3], // EBCDIC "12L" (negative 123 in EBCDIC)
            description: "EBCDIC zoned decimal with negative overpunch",
            overpunch_char: Some(0xD3),
            expected_sign: Some("negative"),
            ac_tag: "AC:63-8-3",
        },

        // Edge cases with boundaries
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(1).",
            binary_data: b"{".to_vec(), // Single character positive
            description: "Single digit zoned decimal with overpunch",
            overpunch_char: Some(b'{'),
            expected_sign: Some("positive"),
            ac_tag: "AC:63-8-4",
        },
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(1).",
            binary_data: b"}".to_vec(), // Single character negative
            description: "Single digit zoned decimal with negative overpunch",
            overpunch_char: Some(b'}'),
            expected_sign: Some("negative"),
            ac_tag: "AC:63-8-4",
        },

        // Missing overpunch (should default to positive)
        ZonedOverpunchFixture {
            copybook_text: "01 RECORD.\n    05 ZONED-FIELD PIC S9(5).",
            binary_data: b"12345".to_vec(), // No overpunch character
            description: "Zoned decimal without overpunch (unsigned data)",
            overpunch_char: None,
            expected_sign: Some("positive"), // Default to positive
            ac_tag: "AC:63-8-5",
        },
    ]
});

/// Binary Integer Test Data - AC:63-7-3
pub static BINARY_INTEGER_CASES: LazyLock<Vec<BinaryIntegerFixture>> = LazyLock::new(|| {
    vec![
        // Integer overflow edge cases
        BinaryIntegerFixture {
            copybook_text: "01 RECORD.\n    05 BINARY-FIELD PIC S9(9) COMP.",
            binary_data: vec![0xFF, 0xFF, 0xFF, 0xFF], // Maximum 32-bit value
            description: "Binary integer at maximum 32-bit bounds",
            endianness: "big",
            overflow_scenario: "Maximum positive 32-bit value",
            ac_tag: "AC:63-7-3",
        },
        BinaryIntegerFixture {
            copybook_text: "01 RECORD.\n    05 BINARY-FIELD PIC S9(9) COMP.",
            binary_data: vec![0x80, 0x00, 0x00, 0x00], // Minimum 32-bit value
            description: "Binary integer at minimum 32-bit bounds",
            endianness: "big",
            overflow_scenario: "Minimum negative 32-bit value",
            ac_tag: "AC:63-7-3",
        },
        BinaryIntegerFixture {
            copybook_text: "01 RECORD.\n    05 BINARY-FIELD PIC S9(18) COMP.",
            binary_data: vec![0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF], // Maximum 64-bit value
            description: "Binary integer at maximum 64-bit bounds",
            endianness: "big",
            overflow_scenario: "Maximum positive 64-bit value",
            ac_tag: "AC:63-7-3",
        },

        // Endianness edge cases
        BinaryIntegerFixture {
            copybook_text: "01 RECORD.\n    05 BINARY-FIELD PIC 9(5) COMP.",
            binary_data: vec![0x12, 0x34], // Little-endian interpretation
            description: "Binary integer with little-endian data",
            endianness: "little",
            overflow_scenario: "Endianness conversion",
            ac_tag: "AC:63-7-4",
        },

        // Truncated binary data
        BinaryIntegerFixture {
            copybook_text: "01 RECORD.\n    05 BINARY-FIELD PIC S9(9) COMP.",
            binary_data: vec![0x12], // Insufficient bytes for 32-bit
            description: "Binary integer with insufficient data",
            endianness: "big",
            overflow_scenario: "Truncated binary data",
            ac_tag: "AC:63-7-5",
        },
    ]
});

/// Record Processing Test Data
pub static RECORD_PROCESSING_CASES: LazyLock<Vec<RecordProcessingFixture>> = LazyLock::new(|| {
    vec![
        // Fixed-length record edge cases
        RecordProcessingFixture {
            copybook_text: r"
            01 RECORD.
                05 FIELD-1 PIC X(10).
                05 FIELD-2 PIC 9(5).
                05 FIELD-3 PIC S9(7)V99 COMP-3.
            ",
            binary_data: vec![0x40; 10], // Partial record data
            description: "Fixed-length record with insufficient data",
            record_format: "fixed",
            processing_scenario: "Truncated fixed-length record",
            ac_tag: "AC:63-9-1",
        },
        RecordProcessingFixture {
            copybook_text: r"
            01 RECORD.
                05 FIELD-1 PIC X(5).
            ",
            binary_data: vec![0x40; 100], // Excess data
            description: "Fixed-length record with excess data",
            record_format: "fixed",
            processing_scenario: "Oversized fixed-length record",
            ac_tag: "AC:63-9-1",
        },

        // Variable-length record edge cases
        RecordProcessingFixture {
            copybook_text: r"
            01 RECORD.
                05 LENGTH-FIELD PIC 9(4) COMP.
                05 DATA-FIELD PIC X(100).
            ",
            binary_data: vec![0x00, 0x00, 0x00, 0x50], // RDW indicating 80 bytes but insufficient data
            description: "Variable-length record with invalid RDW",
            record_format: "rdw",
            processing_scenario: "Invalid RDW length field",
            ac_tag: "AC:63-9-2",
        },

        // ODO record edge cases
        RecordProcessingFixture {
            copybook_text: r"
            01 RECORD.
                05 COUNT PIC 9(3).
                05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON COUNT.
                    10 ITEM PIC X(10).
            ",
            binary_data: b"999".to_vec(), // Count exceeds maximum
            description: "ODO record with count exceeding maximum",
            record_format: "fixed",
            processing_scenario: "ODO count validation",
            ac_tag: "AC:63-9-3",
        },
        RecordProcessingFixture {
            copybook_text: r"
            01 RECORD.
                05 COUNT PIC 9(3).
                05 ARRAY-DATA OCCURS 1 TO 100 TIMES DEPENDING ON COUNT.
                    10 ITEM PIC X(10).
            ",
            binary_data: b"000".to_vec(), // Count below minimum
            description: "ODO record with count below minimum",
            record_format: "fixed",
            processing_scenario: "ODO minimum count validation",
            ac_tag: "AC:63-9-3",
        },
    ]
});

/// Memory Management Test Data - AC:63-10 (11 instances)
pub static MEMORY_MANAGEMENT_CASES: LazyLock<Vec<MemoryManagementFixture>> = LazyLock::new(|| {
    vec![
        // Large data processing
        MemoryManagementFixture {
            copybook_text: r"
            01 LARGE-RECORD.
                05 DATA-BLOCK PIC X(1000).
            ",
            data_size_mb: 100, // 100MB of data
            description: "Large data processing within memory limits",
            memory_scenario: "Bulk data processing with memory constraints",
            target_memory_limit_mb: 256,
            ac_tag: "AC:63-10-1",
        },
        MemoryManagementFixture {
            copybook_text: r"
            01 PERFORMANCE-RECORD.
                05 FIELDS OCCURS 1000 TIMES.
                    10 FIELD-DATA PIC X(100).
            ",
            data_size_mb: 500, // 500MB of data
            description: "High-volume record processing",
            memory_scenario: "Enterprise-scale data processing",
            target_memory_limit_mb: 256,
            ac_tag: "AC:63-10-2",
        },

        // Streaming data edge cases
        MemoryManagementFixture {
            copybook_text: r"
            01 STREAMING-RECORD.
                05 HEADER PIC X(100).
                05 PAYLOAD PIC X(10000).
            ",
            data_size_mb: 1000, // 1GB of streaming data
            description: "Streaming data processing with memory bounds",
            memory_scenario: "Continuous data stream processing",
            target_memory_limit_mb: 256,
            ac_tag: "AC:63-10-3",
        },

        // Memory fragmentation scenarios
        MemoryManagementFixture {
            copybook_text: r"
            01 FRAGMENTED-RECORD.
                05 SMALL-FIELDS OCCURS 10000 TIMES.
                    10 TINY-FIELD PIC X(1).
            ",
            data_size_mb: 50,
            description: "Memory fragmentation with many small fields",
            memory_scenario: "Memory fragmentation stress test",
            target_memory_limit_mb: 256,
            ac_tag: "AC:63-10-4",
        },
    ]
});

/// Enterprise Performance Test Data
pub static ENTERPRISE_PERFORMANCE_CASES: LazyLock<Vec<RecordProcessingFixture>> = LazyLock::new(|| {
    vec![
        // High-throughput DISPLAY processing (target: 2.33+ GiB/s)
        RecordProcessingFixture {
            copybook_text: r"
            01 DISPLAY-PERFORMANCE-RECORD.
                05 FIELDS OCCURS 100 TIMES.
                    10 DISPLAY-FIELD PIC X(50).
            ",
            binary_data: vec![0x40; 5000], // 5KB record with EBCDIC spaces
            description: "High-throughput DISPLAY field processing",
            record_format: "fixed",
            processing_scenario: "DISPLAY performance validation (2.33+ GiB/s target)",
            ac_tag: "AC:63-11-1",
        },

        // High-throughput COMP-3 processing (target: 168+ MiB/s)
        RecordProcessingFixture {
            copybook_text: r"
            01 COMP3-PERFORMANCE-RECORD.
                05 COMP3-FIELDS OCCURS 50 TIMES.
                    10 AMOUNT PIC S9(13)V99 COMP-3.
            ",
            binary_data: {
                let mut data = Vec::new();
                for _ in 0..50 {
                    data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x5C]); // Valid COMP-3
                }
                data
            },
            description: "High-throughput COMP-3 field processing",
            record_format: "fixed",
            processing_scenario: "COMP-3 performance validation (168+ MiB/s target)",
            ac_tag: "AC:63-11-2",
        },
    ]
});

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::parse_copybook;
    use copybook_codec::{DecodeOptions, Codepage, decode_record};

    #[test]
    fn test_comp3_fixtures_load() {
        assert!(!COMP3_EDGE_CASES.is_empty(), "COMP-3 edge cases should be loaded");
        assert!(COMP3_EDGE_CASES.len() >= 10, "Should have comprehensive COMP-3 edge cases");

        for fixture in COMP3_EDGE_CASES.iter() {
            assert!(!fixture.description.is_empty(), "Fixture should have description");
            assert!(!fixture.panic_scenario.is_empty(), "Fixture should have panic scenario");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_display_fixtures_load() {
        assert!(!DISPLAY_EDGE_CASES.is_empty(), "DISPLAY edge cases should be loaded");

        for fixture in DISPLAY_EDGE_CASES.iter() {
            assert!(!fixture.copybook_text.trim().is_empty(), "Fixture should have copybook");
            assert!(!fixture.binary_data.is_empty(), "Fixture should have test data");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_zoned_overpunch_fixtures_load() {
        assert!(!ZONED_OVERPUNCH_CASES.is_empty(), "Zoned overpunch cases should be loaded");

        for fixture in ZONED_OVERPUNCH_CASES.iter() {
            assert!(!fixture.description.is_empty(), "Fixture should have description");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_binary_integer_fixtures_load() {
        assert!(!BINARY_INTEGER_CASES.is_empty(), "Binary integer cases should be loaded");

        for fixture in BINARY_INTEGER_CASES.iter() {
            assert!(!fixture.overflow_scenario.is_empty(), "Fixture should have overflow scenario");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_fixture_decoding_safety() {
        // Test that COMP-3 fixtures can be decoded without panicking
        for fixture in COMP3_EDGE_CASES.iter() {
            let parse_result = parse_copybook(fixture.copybook_text);
            if let Ok(schema) = parse_result {
                let options = DecodeOptions::new().with_codepage(Codepage::CP037);
                let decode_result = std::panic::catch_unwind(|| {
                    decode_record(&schema, &fixture.binary_data, &options)
                });

                assert!(decode_result.is_ok(),
                       "COMP-3 fixture should not panic during decoding: {} - {}",
                       fixture.ac_tag, fixture.description);
            }
        }

        // Test DISPLAY fixtures
        for fixture in DISPLAY_EDGE_CASES.iter() {
            let parse_result = parse_copybook(fixture.copybook_text);
            if let Ok(schema) = parse_result {
                let codepage = match fixture.codepage {
                    "CP037" => Codepage::CP037,
                    "CP500" => Codepage::CP500,
                    _ => Codepage::CP037,
                };
                let options = DecodeOptions::new().with_codepage(codepage);
                let decode_result = std::panic::catch_unwind(|| {
                    decode_record(&schema, &fixture.binary_data, &options)
                });

                assert!(decode_result.is_ok(),
                       "DISPLAY fixture should not panic during decoding: {} - {}",
                       fixture.ac_tag, fixture.description);
            }
        }
    }

    #[test]
    fn test_memory_management_fixtures() {
        assert!(!MEMORY_MANAGEMENT_CASES.is_empty(), "Memory management cases should be loaded");

        for fixture in MEMORY_MANAGEMENT_CASES.iter() {
            assert!(fixture.data_size_mb > 0, "Fixture should have positive data size");
            assert!(fixture.target_memory_limit_mb > 0, "Fixture should have memory limit");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_enterprise_performance_fixtures() {
        assert!(!ENTERPRISE_PERFORMANCE_CASES.is_empty(), "Enterprise performance cases should be loaded");

        for fixture in ENTERPRISE_PERFORMANCE_CASES.iter() {
            assert!(!fixture.binary_data.is_empty(), "Performance fixture should have test data");
            assert!(fixture.processing_scenario.contains("performance") ||
                   fixture.processing_scenario.contains("GiB/s") ||
                   fixture.processing_scenario.contains("MiB/s"),
                   "Performance fixture should reference performance targets");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }
}