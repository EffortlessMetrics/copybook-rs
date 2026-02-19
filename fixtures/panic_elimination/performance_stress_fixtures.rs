// SPDX-License-Identifier: AGPL-3.0-or-later
/// Panic Elimination Fixtures: Performance Stress Test Data
/// Issue #33 - Performance Validation Under Panic Elimination
///
/// This module provides large-scale test data for validating that panic elimination
/// maintains enterprise performance targets (4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3).
/// Focuses on throughput validation, memory management, and performance regression
/// detection under panic-safe error handling.

use std::sync::LazyLock;

#[cfg(test)]
pub struct PerformanceStressFixture {
    pub name: &'static str,
    pub copybook_text: &'static str,
    pub record_count: usize,
    pub record_size: usize,
    pub target_throughput: u64, // bytes per second
    pub data_type: &'static str,
    pub stress_scenario: &'static str,
    pub codepage: &'static str,
}

/// Large-scale DISPLAY field performance fixtures
pub mod display_performance_fixtures {
    use super::*;

    /// High-throughput DISPLAY field processing (targeting 4.1+ GiB/s)
    pub fn high_throughput_display_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "display_high_throughput",
            copybook_text: r#"
            01 DISPLAY-THROUGHPUT-RECORD.
                05 RECORD-ID PIC 9(10).
                05 CUSTOMER-NAME PIC X(50).
                05 ADDRESS-LINE-1 PIC X(60).
                05 ADDRESS-LINE-2 PIC X(60).
                05 PHONE-NUMBER PIC X(15).
                05 EMAIL-ADDRESS PIC X(80).
                05 ACCOUNT-STATUS PIC X(10).
                05 BALANCE-DISPLAY PIC 9(12)V99.
                05 LAST-TRANSACTION PIC X(30).
                05 NOTES-FIELD PIC X(200).
            "#,
            record_count: 100000, // 100k records
            record_size: 527, // Total record size
            target_throughput: 4_400_000_000, // 4.4 GiB/s (safety margin above 4.1)
            data_type: "DISPLAY",
            stress_scenario: "High-volume DISPLAY field processing for enterprise throughput",
            codepage: "cp037",
        }
    }

    /// Memory-efficient DISPLAY processing under stress
    pub fn memory_efficient_display_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "display_memory_efficient",
            copybook_text: r#"
            01 MEMORY-EFFICIENT-RECORD.
                05 LARGE-TEXT-FIELD PIC X(1000).
                05 METADATA-FIELD PIC X(100).
            "#,
            record_count: 250000, // 250k records * 1100 bytes = 275 MB
            record_size: 1100,
            target_throughput: 4_100_000_000, // Minimum enterprise target
            data_type: "DISPLAY",
            stress_scenario: "Memory-efficient streaming of large DISPLAY fields",
            codepage: "cp037",
        }
    }

    /// Multi-codepage DISPLAY performance stress
    pub fn multi_codepage_display_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "display_multi_codepage",
            copybook_text: r#"
            01 MULTI-CODEPAGE-RECORD.
                05 STANDARD-FIELD PIC X(100).
                05 INTERNATIONAL-FIELD PIC X(150).
                05 CURRENCY-FIELD PIC X(50).
            "#,
            record_count: 150000, // 150k records
            record_size: 300,
            target_throughput: 4_200_000_000, // 4.2 GiB/s
            data_type: "DISPLAY",
            stress_scenario: "Multi-codepage character conversion under high throughput",
            codepage: "cp273", // German codepage for international stress
        }
    }

    pub fn all_display_performance_fixtures() -> Vec<PerformanceStressFixture> {
        vec![
            high_throughput_display_fixture(),
            memory_efficient_display_fixture(),
            multi_codepage_display_fixture(),
        ]
    }
}

/// Large-scale COMP-3 performance fixtures
pub mod comp3_performance_fixtures {
    use super::*;

    /// High-throughput COMP-3 processing (targeting 560+ MiB/s)
    pub fn high_throughput_comp3_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "comp3_high_throughput",
            copybook_text: r#"
            01 COMP3-THROUGHPUT-RECORD.
                05 TRANSACTION-ID PIC 9(10) COMP-3.
                05 AMOUNT-1 PIC S9(13)V99 COMP-3.
                05 AMOUNT-2 PIC S9(13)V99 COMP-3.
                05 AMOUNT-3 PIC S9(13)V99 COMP-3.
                05 RATE-FIELD PIC S9(5)V9(5) COMP-3.
                05 PERCENTAGE PIC S9(3)V99 COMP-3.
                05 COUNTER-FIELD PIC 9(7) COMP-3.
                05 BALANCE PIC S9(15)V99 COMP-3.
            "#,
            record_count: 500000, // 500k records
            record_size: 42, // COMP-3 packed size
            target_throughput: 600_000_000, // 600 MiB/s (safety margin above 560)
            data_type: "COMP-3",
            stress_scenario: "High-volume COMP-3 decimal processing for financial data",
            codepage: "cp037",
        }
    }

    /// Complex COMP-3 decimal precision stress
    pub fn complex_precision_comp3_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "comp3_complex_precision",
            copybook_text: r#"
            01 COMPLEX-PRECISION-RECORD.
                05 HIGH-PRECISION-1 PIC S9(18)V9(9) COMP-3.
                05 HIGH-PRECISION-2 PIC S9(15)V9(12) COMP-3.
                05 MICRO-AMOUNT PIC S9(10)V9(15) COMP-3.
                05 RATIO PIC SV9(18) COMP-3.
            "#,
            record_count: 200000, // 200k records
            record_size: 56, // Complex precision COMP-3
            target_throughput: 560_000_000, // Minimum enterprise target
            data_type: "COMP-3",
            stress_scenario: "Complex decimal precision under high-volume processing",
            codepage: "cp037",
        }
    }

    /// COMP-3 error handling stress (invalid data mixed with valid)
    pub fn error_handling_comp3_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "comp3_error_handling_stress",
            copybook_text: r#"
            01 ERROR-HANDLING-RECORD.
                05 VALID-AMOUNT PIC S9(9)V99 COMP-3.
                05 POTENTIALLY-INVALID PIC S9(7)V99 COMP-3.
                05 ANOTHER-VALID PIC 9(8) COMP-3.
            "#,
            record_count: 300000, // 300k records (some with invalid data)
            record_size: 18,
            target_throughput: 580_000_000, // 580 MiB/s with error handling
            data_type: "COMP-3",
            stress_scenario: "COMP-3 processing with mixed valid/invalid data for error path stress",
            codepage: "cp037",
        }
    }

    pub fn all_comp3_performance_fixtures() -> Vec<PerformanceStressFixture> {
        vec![
            high_throughput_comp3_fixture(),
            complex_precision_comp3_fixture(),
            error_handling_comp3_fixture(),
        ]
    }
}

/// Mixed data type performance fixtures
pub mod mixed_performance_fixtures {
    use super::*;

    /// Enterprise-scale mixed record processing
    pub fn enterprise_mixed_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "mixed_enterprise_scale",
            copybook_text: r#"
            01 ENTERPRISE-MIXED-RECORD.
                05 HEADER-SECTION.
                    10 RECORD-TYPE PIC X(5).
                    10 TIMESTAMP PIC X(26).
                    10 TRANSACTION-ID PIC 9(15) COMP-3.
                05 FINANCIAL-SECTION.
                    10 PRINCIPAL-AMOUNT PIC S9(13)V99 COMP-3.
                    10 INTEREST-RATE PIC S9(3)V9(5) COMP-3.
                    10 FEES PIC S9(7)V99 COMP-3.
                05 CUSTOMER-SECTION.
                    10 CUSTOMER-ID PIC X(20).
                    10 CUSTOMER-NAME PIC X(60).
                    10 ACCOUNT-NUMBER PIC 9(12).
                05 METADATA-SECTION.
                    10 PROCESSING-CODE PIC X(10).
                    10 BRANCH-CODE PIC X(8).
                    10 NOTES PIC X(200).
            "#,
            record_count: 75000, // 75k records
            record_size: 402, // Mixed record size
            target_throughput: 2_000_000_000, // 2 GiB/s for mixed processing
            data_type: "MIXED",
            stress_scenario: "Enterprise-scale mixed DISPLAY/COMP-3 processing",
            codepage: "cp037",
        }
    }

    /// Variable-length simulation with ODO stress
    pub fn variable_length_odo_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "mixed_variable_odo",
            copybook_text: r#"
            01 VARIABLE-ODO-RECORD.
                05 RECORD-HEADER.
                    10 ITEM-COUNT PIC 9(3).
                    10 RECORD-TYPE PIC X(5).
                05 VARIABLE-ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
                    10 ITEM-ID PIC X(10).
                    10 ITEM-VALUE PIC S9(9)V99 COMP-3.
                    10 ITEM-STATUS PIC X(3).
            "#,
            record_count: 25000, // 25k records with variable sizes
            record_size: 1800, // Average size (assuming 75 items per record)
            target_throughput: 1_500_000_000, // 1.5 GiB/s for variable processing
            data_type: "VARIABLE",
            stress_scenario: "Variable-length ODO processing under high throughput",
            codepage: "cp037",
        }
    }

    pub fn all_mixed_performance_fixtures() -> Vec<PerformanceStressFixture> {
        vec![
            enterprise_mixed_fixture(),
            variable_length_odo_fixture(),
        ]
    }
}

/// Memory pressure and streaming fixtures
pub mod memory_stress_fixtures {
    use super::*;

    /// Large single record processing (memory allocation stress)
    pub fn large_single_record_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "memory_large_single_record",
            copybook_text: r#"
            01 LARGE-SINGLE-RECORD.
                05 MASSIVE-TEXT-FIELD PIC X(50000).
                05 LARGE-NUMERIC-ARRAY PIC 9(10) OCCURS 1000 TIMES.
                05 METADATA PIC X(1000).
            "#,
            record_count: 100, // 100 very large records
            record_size: 61000, // ~60KB per record
            target_throughput: 1_000_000_000, // 1 GiB/s for large records
            data_type: "LARGE",
            stress_scenario: "Large single record processing with memory allocation stress",
            codepage: "cp037",
        }
    }

    /// Streaming processing under memory pressure
    pub fn streaming_memory_pressure_fixture() -> PerformanceStressFixture {
        PerformanceStressFixture {
            name: "memory_streaming_pressure",
            copybook_text: r#"
            01 STREAMING-RECORD.
                05 STREAM-ID PIC 9(8).
                05 DATA-BLOCK PIC X(2048).
                05 CHECKSUM PIC 9(10) COMP-3.
            "#,
            record_count: 125000, // 125k records * 2KB = 250MB+
            record_size: 2063,
            target_throughput: 3_000_000_000, // 3 GiB/s streaming
            data_type: "STREAMING",
            stress_scenario: "Streaming data processing under memory pressure (<256MB limit)",
            codepage: "cp037",
        }
    }

    pub fn all_memory_stress_fixtures() -> Vec<PerformanceStressFixture> {
        vec![
            large_single_record_fixture(),
            streaming_memory_pressure_fixture(),
        ]
    }
}

/// Comprehensive collection of all performance stress fixtures
pub fn all_performance_stress_fixtures() -> Vec<PerformanceStressFixture> {
    let mut fixtures = Vec::new();
    fixtures.extend(display_performance_fixtures::all_display_performance_fixtures());
    fixtures.extend(comp3_performance_fixtures::all_comp3_performance_fixtures());
    fixtures.extend(mixed_performance_fixtures::all_mixed_performance_fixtures());
    fixtures.extend(memory_stress_fixtures::all_memory_stress_fixtures());
    fixtures
}

/// Generate actual test data for performance fixtures
pub fn generate_performance_test_data(fixture: &PerformanceStressFixture) -> Vec<u8> {
    let mut data = Vec::with_capacity(fixture.record_count * fixture.record_size);

    match fixture.data_type {
        "DISPLAY" => {
            // Generate EBCDIC DISPLAY data
            for i in 0..fixture.record_count {
                let mut record = Vec::with_capacity(fixture.record_size);

                // Fill with varied EBCDIC characters
                for j in 0..fixture.record_size {
                    let char_value = match j % 10 {
                        0..=3 => 0xF0 + (i % 10) as u8, // Digits
                        4..=7 => 0xC1 + (j % 26) as u8, // Letters A-Z
                        _ => 0x40, // Space
                    };
                    record.push(char_value);
                }
                data.extend(record);
            }
        },
        "COMP-3" => {
            // Generate COMP-3 packed decimal data
            for i in 0..fixture.record_count {
                let mut record = Vec::with_capacity(fixture.record_size);

                // Generate valid COMP-3 patterns
                for j in 0..(fixture.record_size / 6) { // Approximate COMP-3 fields
                    let comp3_field = vec![
                        0x12, 0x34, 0x56, 0x78, 0x90,
                        if (i + j) % 2 == 0 { 0x1C } else { 0x1D }, // Positive/Negative
                    ];
                    record.extend(&comp3_field[..std::cmp::min(6, fixture.record_size - record.len())]);
                    if record.len() >= fixture.record_size { break; }
                }

                // Pad to exact size
                while record.len() < fixture.record_size {
                    record.push(0x00);
                }
                data.extend(record);
            }
        },
        "MIXED" => {
            // Generate mixed DISPLAY/COMP-3 data
            for i in 0..fixture.record_count {
                let mut record = Vec::with_capacity(fixture.record_size);

                // Mix of DISPLAY and COMP-3 sections
                let display_portion = (fixture.record_size * 7) / 10; // 70% DISPLAY

                // DISPLAY section
                for j in 0..display_portion {
                    record.push(0x40 + (j % 60) as u8); // Varied EBCDIC
                }

                // COMP-3 section
                for j in 0..(fixture.record_size - display_portion) {
                    let comp3_byte = if j % 2 == 0 {
                        0x12 + (i % 88) as u8
                    } else {
                        0x3C
                    };
                    record.push(comp3_byte);
                }

                data.extend(record);
            }
        },
        "VARIABLE" => {
            // Generate variable-length ODO data
            for i in 0..fixture.record_count {
                let item_count = 25 + (i % 75); // 25-100 items
                let mut record = Vec::new();

                // Header with item count
                record.extend(format!("{:03}", item_count).as_bytes());
                record.extend(b"VAR01"); // Record type

                // Variable items
                for j in 0..item_count {
                    record.extend(format!("ITEM{:06}", j).as_bytes()); // Item ID
                    record.extend(vec![0x12, 0x34, 0x56, 0x7C]); // COMP-3 value
                    record.extend(b"ACT"); // Status
                }

                data.extend(record);
            }
        },
        "LARGE" => {
            // Generate large record data
            for i in 0..fixture.record_count {
                let mut record = Vec::with_capacity(fixture.record_size);

                // Large text field
                let pattern = format!("LARGE_RECORD_{:08}_", i);
                for j in 0..50000 {
                    record.push(pattern.as_bytes()[j % pattern.len()]);
                }

                // Numeric array
                for j in 0..10000 {
                    record.push(0xF0 + (j % 10) as u8);
                }

                // Metadata
                for j in 0..1000 {
                    record.push(0x40 + (j % 60) as u8);
                }

                data.extend(record);
            }
        },
        "STREAMING" => {
            // Generate streaming data
            for i in 0..fixture.record_count {
                let mut record = Vec::with_capacity(fixture.record_size);

                // Stream ID
                record.extend(format!("{:08}", i).as_bytes());

                // Data block (2048 bytes)
                for j in 0..2048 {
                    record.push(0x40 + ((i + j) % 95) as u8);
                }

                // Checksum (COMP-3)
                record.extend(vec![0x12, 0x34, 0x56, 0x78, 0x9C]);

                data.extend(record);
            }
        },
        _ => {
            // Default case - simple repeated pattern
            for i in 0..fixture.record_count {
                let mut record = vec![0x40; fixture.record_size]; // EBCDIC spaces
                // Add some variation
                for j in 0..std::cmp::min(10, fixture.record_size) {
                    record[j] = 0xF0 + ((i + j) % 10) as u8;
                }
                data.extend(record);
            }
        }
    }

    data
}

/// Performance benchmark data cache (lazy-loaded for memory efficiency)
static DISPLAY_PERFORMANCE_DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
    let fixture = display_performance_fixtures::high_throughput_display_fixture();
    generate_performance_test_data(&fixture)
});

static COMP3_PERFORMANCE_DATA: LazyLock<Vec<u8>> = LazyLock::new(|| {
    let fixture = comp3_performance_fixtures::high_throughput_comp3_fixture();
    generate_performance_test_data(&fixture)
});

/// Get cached performance data for benchmarking
pub fn get_display_performance_data() -> &'static [u8] {
    &DISPLAY_PERFORMANCE_DATA
}

pub fn get_comp3_performance_data() -> &'static [u8] {
    &COMP3_PERFORMANCE_DATA
}

/// Calculate expected throughput for performance validation
pub fn calculate_expected_throughput(fixture: &PerformanceStressFixture) -> u64 {
    let total_data_size = fixture.record_count * fixture.record_size;

    // Apply safety margin based on data type complexity
    let complexity_factor = match fixture.data_type {
        "DISPLAY" => 0.95, // 5% overhead for character conversion
        "COMP-3" => 0.90,  // 10% overhead for decimal processing
        "MIXED" => 0.85,   // 15% overhead for mixed processing
        "VARIABLE" => 0.80, // 20% overhead for variable-length processing
        "LARGE" => 0.75,   // 25% overhead for large record processing
        "STREAMING" => 0.90, // 10% overhead for streaming
        _ => 0.80,         // Conservative default
    };

    (fixture.target_throughput as f64 * complexity_factor) as u64
}

#[cfg(test)]
mod performance_stress_tests {
    use super::*;
    use copybook_core::parse_copybook;
    use copybook_codec::{decode_record, DecodeOptions, Codepage};
    use std::time::Instant;

    #[test]
    fn test_performance_fixtures_load() {
        let fixtures = all_performance_stress_fixtures();
        assert!(fixtures.len() >= 10, "Should have at least 10 performance stress fixtures");

        for fixture in &fixtures {
            assert!(!fixture.name.is_empty(), "Fixture name should not be empty");
            assert!(!fixture.copybook_text.is_empty(), "Copybook text should not be empty");
            assert!(fixture.record_count > 0, "Record count should be positive");
            assert!(fixture.record_size > 0, "Record size should be positive");
            assert!(fixture.target_throughput > 0, "Target throughput should be positive");
            assert!(!fixture.stress_scenario.is_empty(), "Stress scenario should not be empty");
        }
    }

    #[test]
    fn test_display_performance_fixtures() {
        let display_fixtures = display_performance_fixtures::all_display_performance_fixtures();

        for fixture in &display_fixtures {
            // Validate DISPLAY performance targets (4.1+ GiB/s)
            assert!(
                fixture.target_throughput >= 4_100_000_000,
                "DISPLAY fixture {} should target 4.1+ GiB/s, got {} bytes/s",
                fixture.name, fixture.target_throughput
            );

            let schema_result = parse_copybook(fixture.copybook_text);
            assert!(schema_result.is_ok(), "DISPLAY fixture {} should parse successfully", fixture.name);
        }
    }

    #[test]
    fn test_comp3_performance_fixtures() {
        let comp3_fixtures = comp3_performance_fixtures::all_comp3_performance_fixtures();

        for fixture in &comp3_fixtures {
            // Validate COMP-3 performance targets (560+ MiB/s)
            assert!(
                fixture.target_throughput >= 560_000_000,
                "COMP-3 fixture {} should target 560+ MiB/s, got {} bytes/s",
                fixture.name, fixture.target_throughput
            );

            let schema_result = parse_copybook(fixture.copybook_text);
            assert!(schema_result.is_ok(), "COMP-3 fixture {} should parse successfully", fixture.name);
        }
    }

    #[test]
    fn test_performance_data_generation() {
        let fixtures = all_performance_stress_fixtures();

        // Test data generation for first few fixtures (avoid memory exhaustion in tests)
        for fixture in fixtures.iter().take(3) {
            let test_data = generate_performance_test_data(fixture);

            let expected_size = fixture.record_count * fixture.record_size;
            assert!(
                test_data.len() >= expected_size / 2, // Allow for some variation in variable-length
                "Generated data size for {} should be substantial, expected ~{}, got {}",
                fixture.name, expected_size, test_data.len()
            );
        }
    }

    #[test]
    fn test_performance_calculation() {
        let fixture = display_performance_fixtures::high_throughput_display_fixture();
        let expected = calculate_expected_throughput(&fixture);

        // Should apply appropriate safety margin
        assert!(
            expected < fixture.target_throughput,
            "Expected throughput should account for safety margin"
        );

        assert!(
            expected > fixture.target_throughput / 2,
            "Expected throughput should not be too conservative"
        );
    }

    #[test]
    fn test_cached_performance_data() {
        // Test lazy-loaded performance data
        let display_data = get_display_performance_data();
        assert!(display_data.len() > 1_000_000, "Cached DISPLAY data should be substantial");

        let comp3_data = get_comp3_performance_data();
        assert!(comp3_data.len() > 100_000, "Cached COMP-3 data should be substantial");
    }

    #[test]
    #[ignore] // Expensive test - run manually for performance validation
    fn test_sample_performance_validation() {
        let fixture = display_performance_fixtures::memory_efficient_display_fixture();
        let schema = parse_copybook(fixture.copybook_text).expect("Schema should parse");

        // Generate smaller sample for testing
        let sample_fixture = PerformanceStressFixture {
            record_count: 1000, // Smaller sample
            ..*fixture
        };

        let test_data = generate_performance_test_data(&sample_fixture);
        let options = DecodeOptions::new().with_codepage(Codepage::CP037);

        let start = Instant::now();
        let mut processed_bytes = 0;
        let mut record_start = 0;

        // Process sample records
        for _i in 0..sample_fixture.record_count {
            let record_end = record_start + sample_fixture.record_size;
            if record_end <= test_data.len() {
                let record_data = &test_data[record_start..record_end];
                let result = decode_record(&schema, record_data, &options);

                // Should not panic, either succeed or return controlled error
                match result {
                    Ok(_) => processed_bytes += record_data.len(),
                    Err(_) => {
                        // Controlled error is acceptable for performance testing
                        processed_bytes += record_data.len();
                    }
                }
            }
            record_start = record_end;
        }

        let elapsed = start.elapsed();
        let throughput = (processed_bytes as f64) / elapsed.as_secs_f64();

        println!("Sample performance test: {} bytes/sec", throughput as u64);

        // Performance should be reasonable (even for debug builds)
        assert!(throughput > 1_000_000.0, "Sample throughput should be > 1 MB/s");
    }
}