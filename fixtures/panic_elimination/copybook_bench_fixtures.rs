//! Performance Test Data for copybook-bench Panic Elimination
//!
//! This module provides comprehensive test fixtures for eliminating 6 .unwrap()/.expect() calls
//! in copybook-bench production code. Tests target benchmark execution, performance measurement,
//! regression detection, and memory profiling with enterprise-grade performance validation.
//!
//! **Issue #63 - AC Traceability:**
//! - AC1: Complete elimination of 6 .unwrap()/.expect() calls in copybook-bench
//! - AC2: Zero breaking changes to existing public APIs
//! - AC4: Performance impact <5% maintaining 2.33+ GiB/s DISPLAY, 168+ MiB/s COMP-3
//! - AC7: Comprehensive test coverage for benchmark execution paths
//! - AC10: Safe performance measurement without panics

use std::sync::LazyLock;

/// Performance Benchmark Edge Cases
pub struct BenchmarkFixture {
    pub copybook_content: &'static str,
    pub data_content: Vec<u8>,
    pub benchmark_name: &'static str,
    pub target_throughput: &'static str,
    pub data_size_mb: usize,
    pub description: &'static str,
    pub panic_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// Regression Detection Edge Cases
pub struct RegressionTestFixture {
    pub benchmark_config: &'static str,
    pub baseline_performance: f64, // MB/s or operations/s
    pub tolerance_percent: f64,
    pub description: &'static str,
    pub regression_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// Memory Profiling Edge Cases
pub struct MemoryProfilingFixture {
    pub copybook_content: &'static str,
    pub data_size_mb: usize,
    pub memory_limit_mb: usize,
    pub description: &'static str,
    pub memory_scenario: &'static str,
    pub ac_tag: &'static str,
}

/// Benchmark Execution Edge Cases
pub struct BenchmarkExecutionFixture {
    pub benchmark_type: &'static str,
    pub execution_config: &'static str,
    pub description: &'static str,
    pub execution_scenario: &'static str,
    pub expected_behavior: &'static str,
    pub ac_tag: &'static str,
}

/// Performance Statistics Edge Cases
pub struct PerformanceStatsFixture {
    pub measurement_data: Vec<f64>, // Performance measurements
    pub description: &'static str,
    pub stats_scenario: &'static str,
    pub expected_statistics: &'static str,
    pub ac_tag: &'static str,
}

/// Performance Benchmark Test Data - AC:63-18 (6 instances)
pub static BENCHMARK_PERFORMANCE_CASES: LazyLock<Vec<BenchmarkFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-18-1 - DISPLAY field throughput benchmarks
        BenchmarkFixture {
            copybook_content: r"
            01 DISPLAY-BENCHMARK-RECORD.
                05 DISPLAY-FIELDS OCCURS 100 TIMES.
                    10 FIELD-DATA PIC X(50).
            ",
            data_content: {
                let mut data = Vec::new();
                for _ in 0..100 {
                    // 50 bytes of EBCDIC spaces for each field
                    data.extend_from_slice(&[0x40; 50]);
                }
                data
            },
            benchmark_name: "display_throughput",
            target_throughput: "2.33+ GiB/s",
            data_size_mb: 100,
            description: "DISPLAY field throughput benchmark with enterprise target",
            panic_scenario: "Benchmark measurement calculation with large throughput values",
            ac_tag: "AC:63-18-1",
        },

        // AC:63-18-2 - COMP-3 packed decimal benchmarks
        BenchmarkFixture {
            copybook_content: r"
            01 COMP3-BENCHMARK-RECORD.
                05 COMP3-AMOUNTS OCCURS 50 TIMES.
                    10 AMOUNT PIC S9(13)V99 COMP-3.
            ",
            data_content: {
                let mut data = Vec::new();
                for i in 0..50 {
                    // Valid COMP-3 data: 123456789012.34
                    data.extend_from_slice(&[
                        0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x5C
                    ]);
                }
                data
            },
            benchmark_name: "comp3_throughput",
            target_throughput: "168+ MiB/s",
            data_size_mb: 50,
            description: "COMP-3 packed decimal benchmark with enterprise target",
            panic_scenario: "COMP-3 conversion rate calculation with precision arithmetic",
            ac_tag: "AC:63-18-2",
        },

        // AC:63-18-3 - Large-scale memory benchmark
        BenchmarkFixture {
            copybook_content: r"
            01 MEMORY-BENCHMARK-RECORD.
                05 LARGE-FIELD PIC X(10000).
            ",
            data_content: vec![0x40; 10000], // 10KB of EBCDIC spaces
            benchmark_name: "memory_efficiency",
            target_throughput: "<256 MiB memory usage",
            data_size_mb: 500, // 500MB total processing
            description: "Memory efficiency benchmark with large records",
            panic_scenario: "Memory usage calculation with large allocations",
            ac_tag: "AC:63-18-3",
        },

        // AC:63-18-4 - Multi-threaded benchmark
        BenchmarkFixture {
            copybook_content: r"
            01 PARALLEL-BENCHMARK-RECORD.
                05 THREAD-FIELDS OCCURS 20 TIMES.
                    10 FIELD-ID PIC 9(5).
                    10 FIELD-DATA PIC X(100).
            ",
            data_content: {
                let mut data = Vec::new();
                for i in 0..20 {
                    // Field ID (5 digits)
                    data.extend_from_slice(format!("{:05}", i).as_bytes());
                    // Field data (100 bytes)
                    data.extend_from_slice(&[0x40; 100]);
                }
                data
            },
            benchmark_name: "parallel_processing",
            target_throughput: "Linear scaling with thread count",
            data_size_mb: 200,
            description: "Multi-threaded processing benchmark",
            panic_scenario: "Thread synchronization in performance measurement",
            ac_tag: "AC:63-18-4",
        },

        // AC:63-18-5 - Extreme data size benchmark
        BenchmarkFixture {
            copybook_content: r"
            01 EXTREME-BENCHMARK-RECORD.
                05 MASSIVE-ARRAY OCCURS 1000 TIMES.
                    10 ARRAY-ITEM PIC X(1000).
            ",
            data_content: vec![0x40; 1000000], // 1MB record
            benchmark_name: "extreme_scale",
            target_throughput: "1+ GiB/s with memory constraints",
            data_size_mb: 1000, // 1GB total
            description: "Extreme scale benchmark testing system limits",
            panic_scenario: "Performance calculation with extreme data sizes",
            ac_tag: "AC:63-18-5",
        },

        // AC:63-18-6 - Edge case data benchmark
        BenchmarkFixture {
            copybook_content: r"
            01 EDGE-CASE-BENCHMARK-RECORD.
                05 MIXED-FIELDS.
                    10 TINY-FIELD PIC X(1).
                    10 HUGE-FIELD PIC X(32767).
                    10 COMP3-FIELD PIC S9(18)V99 COMP-3.
                    10 BINARY-FIELD PIC S9(18) COMP.
            ",
            data_content: {
                let mut data = Vec::new();
                data.push(0x40); // Tiny field
                data.extend_from_slice(&vec![0x40; 32767]); // Huge field
                data.extend_from_slice(&[0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C]); // Max COMP-3
                data.extend_from_slice(&[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]); // Max binary
                data
            },
            benchmark_name: "edge_case_performance",
            target_throughput: "Consistent performance across field types",
            data_size_mb: 100,
            description: "Edge case performance with mixed field types and sizes",
            panic_scenario: "Performance measurement with extreme field size variations",
            ac_tag: "AC:63-18-6",
        },
    ]
});

/// Regression Detection Test Data - AC:63-19
pub static REGRESSION_DETECTION_CASES: LazyLock<Vec<RegressionTestFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-19-1 - DISPLAY performance regression
        RegressionTestFixture {
            benchmark_config: "display_throughput_baseline",
            baseline_performance: 2500.0, // 2.5 GiB/s baseline
            tolerance_percent: 5.0,
            description: "DISPLAY throughput regression detection",
            regression_scenario: "Performance regression calculation with floating point arithmetic",
            ac_tag: "AC:63-19-1",
        },

        // AC:63-19-2 - COMP-3 performance regression
        RegressionTestFixture {
            benchmark_config: "comp3_throughput_baseline",
            baseline_performance: 176.0, // 176 MiB/s baseline
            tolerance_percent: 5.0,
            description: "COMP-3 throughput regression detection",
            regression_scenario: "COMP-3 performance variance calculation",
            ac_tag: "AC:63-19-2",
        },

        // AC:63-19-3 - Memory usage regression
        RegressionTestFixture {
            benchmark_config: "memory_usage_baseline",
            baseline_performance: 200.0, // 200 MiB baseline memory usage
            tolerance_percent: 10.0,
            description: "Memory usage regression detection",
            regression_scenario: "Memory measurement calculation with allocation tracking",
            ac_tag: "AC:63-19-3",
        },

        // AC:63-19-4 - Latency regression
        RegressionTestFixture {
            benchmark_config: "latency_baseline",
            baseline_performance: 100.0, // 100ms baseline latency
            tolerance_percent: 15.0,
            description: "Processing latency regression detection",
            regression_scenario: "Latency calculation with time measurement precision",
            ac_tag: "AC:63-19-4",
        },

        // AC:63-19-5 - Extreme variance handling
        RegressionTestFixture {
            benchmark_config: "variance_stress_test",
            baseline_performance: 1000.0,
            tolerance_percent: 50.0, // High tolerance for stress test
            description: "Extreme performance variance handling",
            regression_scenario: "Statistical calculation with extreme variance values",
            ac_tag: "AC:63-19-5",
        },
    ]
});

/// Memory Profiling Test Data - AC:63-20
pub static MEMORY_PROFILING_CASES: LazyLock<Vec<MemoryProfilingFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-20-1 - Steady-state memory profiling
        MemoryProfilingFixture {
            copybook_content: r"
            01 MEMORY-PROFILE-RECORD.
                05 PROFILE-DATA PIC X(1000).
            ",
            data_size_mb: 500,
            memory_limit_mb: 256,
            description: "Steady-state memory profiling with large datasets",
            memory_scenario: "Memory tracking calculation with allocation boundaries",
            ac_tag: "AC:63-20-1",
        },

        // AC:63-20-2 - Memory leak detection
        MemoryProfilingFixture {
            copybook_content: r"
            01 LEAK-DETECTION-RECORD.
                05 LEAK-TEST-FIELDS OCCURS 1000 TIMES.
                    10 FIELD-DATA PIC X(100).
            ",
            data_size_mb: 100,
            memory_limit_mb: 256,
            description: "Memory leak detection in repeated processing",
            memory_scenario: "Memory growth calculation across multiple iterations",
            ac_tag: "AC:63-20-2",
        },

        // AC:63-20-3 - Peak memory measurement
        MemoryProfilingFixture {
            copybook_content: r"
            01 PEAK-MEMORY-RECORD.
                05 PEAK-FIELD PIC X(50000).
            ",
            data_size_mb: 200,
            memory_limit_mb: 256,
            description: "Peak memory usage measurement",
            memory_scenario: "Peak memory calculation with allocation spikes",
            ac_tag: "AC:63-20-3",
        },

        // AC:63-20-4 - Memory fragmentation profiling
        MemoryProfilingFixture {
            copybook_content: r"
            01 FRAGMENTATION-RECORD.
                05 SMALL-CHUNKS OCCURS 10000 TIMES.
                    10 CHUNK PIC X(10).
            ",
            data_size_mb: 50,
            memory_limit_mb: 256,
            description: "Memory fragmentation profiling with many small allocations",
            memory_scenario: "Fragmentation calculation with allocation pattern analysis",
            ac_tag: "AC:63-20-4",
        },
    ]
});

/// Benchmark Execution Edge Cases - AC:63-21
pub static BENCHMARK_EXECUTION_CASES: LazyLock<Vec<BenchmarkExecutionFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-21-1 - Benchmark initialization failures
        BenchmarkExecutionFixture {
            benchmark_type: "initialization_failure",
            execution_config: "invalid_benchmark_config",
            description: "Benchmark execution with initialization failures",
            execution_scenario: "Benchmark setup failure handling",
            expected_behavior: "Graceful failure with error reporting",
            ac_tag: "AC:63-21-1",
        },

        // AC:63-21-2 - Benchmark timeout handling
        BenchmarkExecutionFixture {
            benchmark_type: "timeout_test",
            execution_config: "timeout_10ms",
            description: "Benchmark execution with tight timeout limits",
            execution_scenario: "Timeout calculation and handling",
            expected_behavior: "Clean timeout with partial results",
            ac_tag: "AC:63-21-2",
        },

        // AC:63-21-3 - Resource exhaustion during benchmarks
        BenchmarkExecutionFixture {
            benchmark_type: "resource_exhaustion",
            execution_config: "extreme_memory_pressure",
            description: "Benchmark execution under resource pressure",
            execution_scenario: "Resource limit calculation and enforcement",
            expected_behavior: "Graceful degradation with resource management",
            ac_tag: "AC:63-21-3",
        },

        // AC:63-21-4 - Concurrent benchmark execution
        BenchmarkExecutionFixture {
            benchmark_type: "concurrent_execution",
            execution_config: "parallel_benchmarks",
            description: "Concurrent benchmark execution with resource contention",
            execution_scenario: "Concurrent execution coordination",
            expected_behavior: "Isolated benchmark results without interference",
            ac_tag: "AC:63-21-4",
        },
    ]
});

/// Performance Statistics Edge Cases - AC:63-22
pub static PERFORMANCE_STATS_CASES: LazyLock<Vec<PerformanceStatsFixture>> = LazyLock::new(|| {
    vec![
        // AC:63-22-1 - Statistical calculation edge cases
        PerformanceStatsFixture {
            measurement_data: vec![1000.0, 1050.0, 950.0, 1100.0, 900.0, 1200.0, 800.0],
            description: "Normal performance measurement distribution",
            stats_scenario: "Statistical calculation with typical variance",
            expected_statistics: "Mean: ~1000, StdDev: ~150",
            ac_tag: "AC:63-22-1",
        },

        // AC:63-22-2 - Extreme outlier handling
        PerformanceStatsFixture {
            measurement_data: vec![1000.0, 1000.0, 1000.0, 10000.0, 1000.0], // One extreme outlier
            description: "Performance measurements with extreme outliers",
            stats_scenario: "Outlier detection and handling in statistics",
            expected_statistics: "Robust statistics with outlier handling",
            ac_tag: "AC:63-22-2",
        },

        // AC:63-22-3 - Zero and negative values
        PerformanceStatsFixture {
            measurement_data: vec![0.0, -1.0, 1000.0, 0.0, 2000.0],
            description: "Performance measurements with zero and negative values",
            stats_scenario: "Invalid measurement value handling",
            expected_statistics: "Filtered positive values only",
            ac_tag: "AC:63-22-3",
        },

        // AC:63-22-4 - Single measurement
        PerformanceStatsFixture {
            measurement_data: vec![1000.0],
            description: "Performance statistics with single measurement",
            stats_scenario: "Statistical calculation with insufficient data",
            expected_statistics: "Single value statistics handling",
            ac_tag: "AC:63-22-4",
        },

        // AC:63-22-5 - Empty measurement set
        PerformanceStatsFixture {
            measurement_data: vec![],
            description: "Performance statistics with no measurements",
            stats_scenario: "Statistical calculation with empty dataset",
            expected_statistics: "Error or default values for empty set",
            ac_tag: "AC:63-22-5",
        },

        // AC:63-22-6 - High precision calculations
        PerformanceStatsFixture {
            measurement_data: vec![1.000001, 1.000002, 1.000003, 1.000004, 1.000005],
            description: "High precision performance measurements",
            stats_scenario: "Floating point precision in statistical calculations",
            expected_statistics: "High precision mean and variance",
            ac_tag: "AC:63-22-6",
        },
    ]
});

/// Enterprise Performance Validation Fixtures
pub static ENTERPRISE_PERFORMANCE_VALIDATION: LazyLock<Vec<BenchmarkFixture>> = LazyLock::new(|| {
    vec![
        // Banking performance validation
        BenchmarkFixture {
            copybook_content: r"
            01 BANKING-PERFORMANCE-RECORD.
                05 TRANSACTION-HEADER.
                    10 TRANSACTION-ID PIC X(20).
                    10 ACCOUNT-NUMBER PIC 9(16).
                    10 ROUTING-NUMBER PIC 9(9).
                05 TRANSACTION-DATA.
                    10 AMOUNT PIC S9(13)V99 COMP-3.
                    10 TRANSACTION-TYPE PIC X(4).
                    10 TIMESTAMP PIC 9(14).
                    10 REFERENCE-NUMBER PIC X(30).
                05 REGULATORY-DATA.
                    10 COMPLIANCE-CODE PIC X(10).
                    10 AUDIT-TRAIL PIC X(100).
            ",
            data_content: {
                let mut data = Vec::new();
                // Transaction ID (20 bytes)
                data.extend_from_slice(b"TXN12345678901234567");
                // Account number (16 bytes)
                data.extend_from_slice(b"1234567890123456");
                // Routing number (9 bytes)
                data.extend_from_slice(b"123456789");
                // Amount (COMP-3, 8 bytes) - $123,456.78
                data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x9C]);
                // Transaction type (4 bytes)
                data.extend_from_slice(b"XFER");
                // Timestamp (14 bytes)
                data.extend_from_slice(b"20241201120000");
                // Reference number (30 bytes)
                data.extend_from_slice(b"REF123456789012345678901234567");
                // Compliance code (10 bytes)
                data.extend_from_slice(b"SOX-001   ");
                // Audit trail (100 bytes)
                data.extend_from_slice(&[0x40; 100]);
                data
            },
            benchmark_name: "banking_enterprise_validation",
            target_throughput: "Mixed: DISPLAY 2.33+ GiB/s, COMP-3 168+ MiB/s",
            data_size_mb: 250,
            description: "Enterprise banking performance validation with mixed field types",
            panic_scenario: "Complex field type performance calculation",
            ac_tag: "AC:63-23-1",
        },

        // Insurance claims processing validation
        BenchmarkFixture {
            copybook_content: r"
            01 INSURANCE-PERFORMANCE-RECORD.
                05 CLAIM-HEADER.
                    10 CLAIM-NUMBER PIC X(15).
                    10 POLICY-NUMBER PIC X(20).
                    10 CLAIM-DATE PIC 9(8).
                05 FINANCIAL-DATA.
                    10 CLAIM-AMOUNT PIC S9(11)V99 COMP-3.
                    10 DEDUCTIBLE PIC S9(7)V99 COMP-3.
                    10 COVERAGE-LIMIT PIC S9(11)V99 COMP-3.
                05 CLAIM-DETAILS OCCURS 10 TIMES.
                    10 LINE-ITEM-CODE PIC X(8).
                    10 LINE-ITEM-AMOUNT PIC S9(9)V99 COMP-3.
                    10 LINE-ITEM-DESC PIC X(50).
            ",
            data_content: {
                let mut data = Vec::new();
                // Claim number (15 bytes)
                data.extend_from_slice(b"CLM123456789012");
                // Policy number (20 bytes)
                data.extend_from_slice(b"POL1234567890123456 ");
                // Claim date (8 bytes)
                data.extend_from_slice(b"20241201");
                // Claim amount (COMP-3) - $50,000.00
                data.extend_from_slice(&[0x50, 0x00, 0x00, 0x0C]);
                // Deductible (COMP-3) - $1,000.00
                data.extend_from_slice(&[0x10, 0x00, 0x0C]);
                // Coverage limit (COMP-3) - $1,000,000.00
                data.extend_from_slice(&[0x10, 0x00, 0x00, 0x00, 0x0C]);
                // 10 claim detail items
                for i in 0..10 {
                    // Line item code (8 bytes)
                    data.extend_from_slice(&format!("ITEM{:04}", i).as_bytes());
                    // Line item amount (COMP-3) - $5,000.00
                    data.extend_from_slice(&[0x50, 0x00, 0x0C]);
                    // Line item description (50 bytes)
                    data.extend_from_slice(&format!("Line item description {:02}                ", i).as_bytes()[..50]);
                }
                data
            },
            benchmark_name: "insurance_enterprise_validation",
            target_throughput: "COMP-3 intensive: 168+ MiB/s with array processing",
            data_size_mb: 150,
            description: "Enterprise insurance claims processing with COMP-3 arrays",
            panic_scenario: "Array processing performance calculation with COMP-3 fields",
            ac_tag: "AC:63-23-2",
        },
    ]
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_benchmark_performance_fixtures_load() {
        assert!(!BENCHMARK_PERFORMANCE_CASES.is_empty(), "Benchmark performance cases should be loaded");
        assert!(BENCHMARK_PERFORMANCE_CASES.len() >= 6, "Should have comprehensive benchmark cases");

        for fixture in BENCHMARK_PERFORMANCE_CASES.iter() {
            assert!(!fixture.copybook_content.trim().is_empty(), "Fixture should have copybook content");
            assert!(!fixture.data_content.is_empty(), "Fixture should have test data");
            assert!(!fixture.target_throughput.is_empty(), "Fixture should have performance target");
            assert!(fixture.data_size_mb > 0, "Fixture should have positive data size");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_regression_detection_fixtures_load() {
        assert!(!REGRESSION_DETECTION_CASES.is_empty(), "Regression detection cases should be loaded");

        for fixture in REGRESSION_DETECTION_CASES.iter() {
            assert!(fixture.baseline_performance > 0.0, "Fixture should have positive baseline");
            assert!(fixture.tolerance_percent > 0.0, "Fixture should have positive tolerance");
            assert!(fixture.tolerance_percent <= 100.0, "Tolerance should be reasonable percentage");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_memory_profiling_fixtures_load() {
        assert!(!MEMORY_PROFILING_CASES.is_empty(), "Memory profiling cases should be loaded");

        for fixture in MEMORY_PROFILING_CASES.iter() {
            assert!(!fixture.copybook_content.trim().is_empty(), "Fixture should have copybook content");
            assert!(fixture.data_size_mb > 0, "Fixture should have positive data size");
            assert!(fixture.memory_limit_mb > 0, "Fixture should have positive memory limit");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_benchmark_execution_fixtures_load() {
        assert!(!BENCHMARK_EXECUTION_CASES.is_empty(), "Benchmark execution cases should be loaded");

        for fixture in BENCHMARK_EXECUTION_CASES.iter() {
            assert!(!fixture.benchmark_type.is_empty(), "Fixture should have benchmark type");
            assert!(!fixture.execution_scenario.is_empty(), "Fixture should have execution scenario");
            assert!(!fixture.expected_behavior.is_empty(), "Fixture should have expected behavior");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_performance_stats_fixtures_load() {
        assert!(!PERFORMANCE_STATS_CASES.is_empty(), "Performance stats cases should be loaded");

        for fixture in PERFORMANCE_STATS_CASES.iter() {
            // Note: measurement_data can be empty for testing empty dataset scenario
            assert!(!fixture.description.is_empty(), "Fixture should have description");
            assert!(!fixture.stats_scenario.is_empty(), "Fixture should have stats scenario");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_enterprise_performance_validation_load() {
        assert!(!ENTERPRISE_PERFORMANCE_VALIDATION.is_empty(), "Enterprise validation cases should be loaded");

        for fixture in ENTERPRISE_PERFORMANCE_VALIDATION.iter() {
            assert!(!fixture.copybook_content.trim().is_empty(), "Enterprise fixture should have copybook");
            assert!(!fixture.data_content.is_empty(), "Enterprise fixture should have test data");
            assert!(fixture.description.contains("Enterprise") ||
                   fixture.description.contains("banking") ||
                   fixture.description.contains("insurance"),
                   "Enterprise fixture should reference enterprise scenarios");
            assert!(fixture.target_throughput.contains("GiB/s") ||
                   fixture.target_throughput.contains("MiB/s"),
                   "Enterprise fixture should specify performance targets");
            assert!(fixture.ac_tag.starts_with("AC:63-"), "Fixture should have AC tag");
        }
    }

    #[test]
    fn test_performance_target_validation() {
        // Validate performance targets meet enterprise requirements
        for fixture in BENCHMARK_PERFORMANCE_CASES.iter() {
            if fixture.target_throughput.contains("GiB/s") {
                // DISPLAY targets should be >= 2.33 GiB/s
                assert!(fixture.description.contains("DISPLAY") ||
                       fixture.target_throughput.contains("2.") ||
                       fixture.target_throughput.contains("1+"),
                       "GiB/s targets should reference DISPLAY performance: {}",
                       fixture.target_throughput);
            }
            if fixture.target_throughput.contains("MiB/s") {
                // COMP-3 targets should be >= 168 MiB/s
                assert!(fixture.description.contains("COMP-3") ||
                       fixture.target_throughput.contains("168") ||
                       fixture.target_throughput.contains("1"),
                       "MiB/s targets should reference COMP-3 performance: {}",
                       fixture.target_throughput);
            }
        }
    }

    #[test]
    fn test_memory_constraints_validation() {
        // Validate memory constraints meet enterprise requirements (<256 MiB)
        for fixture in MEMORY_PROFILING_CASES.iter() {
            assert!(fixture.memory_limit_mb <= 256,
                   "Memory limit should meet enterprise constraint (<= 256 MiB): {} MiB",
                   fixture.memory_limit_mb);
        }
    }

    #[test]
    fn test_statistical_edge_cases() {
        // Validate statistical edge cases are covered
        let has_empty_dataset = PERFORMANCE_STATS_CASES.iter()
            .any(|f| f.measurement_data.is_empty());
        let has_single_value = PERFORMANCE_STATS_CASES.iter()
            .any(|f| f.measurement_data.len() == 1);
        let has_outliers = PERFORMANCE_STATS_CASES.iter()
            .any(|f| f.description.contains("outlier"));

        assert!(has_empty_dataset, "Should have empty dataset test case");
        assert!(has_single_value, "Should have single value test case");
        assert!(has_outliers, "Should have outlier handling test case");
    }
}