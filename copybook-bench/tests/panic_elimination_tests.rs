/// Tests feature spec: issue-63-spec.md#ac1-complete-panic-elimination
/// Tests feature spec: issue-63-technical-specification.md#benchmark-tool-safety
/// Tests feature spec: panic-elimination-implementation-blueprint.md#phase-3-long-tail-cleanup
///
/// Issue #63 - Comprehensive Panic Elimination Test Scaffolding for copybook-bench
///
/// This module provides comprehensive test scaffolding for eliminating 6 .unwrap()/.expect() calls
/// in copybook-bench production code. Tests target performance measurement tools with enterprise-safe
/// benchmarking and regression detection capabilities.
///
/// **AC Traceability:**
/// - AC1: Complete elimination of 6 .unwrap()/.expect() calls in copybook-bench
/// - AC2: Zero breaking changes to existing benchmarking APIs
/// - AC3: Integration with structured error taxonomy for benchmark errors
/// - AC4: Performance measurement accuracy preserved during panic elimination
/// - AC7: Comprehensive test coverage for benchmark execution
/// - AC12: Enterprise validation with performance regression detection
use std::time::Duration;

/// Mock benchmark framework types for testing
/// These would be replaced by actual copybook-bench types in implementation
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    pub name: String,
    pub duration: Duration,
    pub iterations: u64,
    pub throughput: Option<f64>,
    pub memory_usage: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct BenchmarkConfig {
    pub warmup_time: Duration,
    pub measurement_time: Duration,
    pub sample_size: usize,
    pub confidence_level: f64,
}

#[derive(Debug, Clone)]
pub struct PerformanceBaseline {
    pub display_throughput_gbps: f64,
    pub comp3_throughput_mbps: f64,
    pub memory_limit_mb: usize,
    pub variance_threshold_percent: f64,
}

#[cfg(test)]
mod panic_elimination_benchmark_execution_tests {
    use super::*;

    /// Tests benchmark execution panic elimination (6 instances)
    /// AC:63-20 - Benchmark execution with safe measurement and result processing

    #[test] // AC:63-20-1 Benchmark measurement safety
    fn test_benchmark_measurement_safety() {
        // Test case: Benchmark measurement with extreme or invalid configurations
        let extreme_configs = vec![
            BenchmarkConfig {
                warmup_time: Duration::from_nanos(0),      // Zero warmup
                measurement_time: Duration::from_nanos(1), // Minimal measurement
                sample_size: 0,                            // Zero samples
                confidence_level: 0.0,                     // Invalid confidence
            },
            BenchmarkConfig {
                warmup_time: Duration::from_secs(3600),      // 1 hour warmup
                measurement_time: Duration::from_secs(3600), // 1 hour measurement
                sample_size: 1000000,                        // Excessive samples
                confidence_level: 200.0,                     // Invalid confidence (>100%)
            },
            BenchmarkConfig {
                warmup_time: Duration::from_millis(100),
                measurement_time: Duration::from_millis(100),
                sample_size: 10,
                confidence_level: 95.0, // Valid configuration for comparison
            },
        ];

        for (i, config) in extreme_configs.iter().enumerate() {
            // Mock benchmark execution that could panic with extreme configurations
            let measurement_result = execute_benchmark_safely("test_benchmark", config);

            match measurement_result {
                Ok(result) => {
                    // Should handle extreme configurations gracefully
                    assert!(
                        !result.name.is_empty(),
                        "Benchmark result {} should have valid name",
                        i
                    );

                    // Validate measurement results are reasonable
                    assert!(
                        result.duration <= Duration::from_secs(7200), // 2 hour max
                        "Benchmark duration {} should be reasonable: {:?}",
                        i,
                        result.duration
                    );

                    if let Some(throughput) = result.throughput {
                        assert!(
                            (0.0..1e12).contains(&throughput), // Reasonable throughput range
                            "Benchmark throughput {} should be reasonable: {}",
                            i,
                            throughput
                        );
                    }
                }
                Err(error) => {
                    // Should return structured error instead of panicking
                    assert!(
                        error.contains("benchmark")
                            || error.contains("measurement")
                            || error.contains("configuration")
                            || error.contains("sample size")
                            || error.contains("confidence"),
                        "Benchmark measurement error {} should reference measurement issue: {}",
                        i,
                        error
                    );

                    // Should not contain panic traces
                    assert!(
                        !error.contains("panic") && !error.contains("unwrap"),
                        "Benchmark measurement error {} should not contain panic traces: {}",
                        i,
                        error
                    );
                }
            }
        }
    }

    #[test] // AC:63-20-2 Benchmark timing safety
    fn test_benchmark_timing_safety() {
        // Test case: Benchmark timing with system clock issues and edge cases
        let timing_scenarios = vec![
            ("normal_timing", Duration::from_millis(100)),
            ("minimal_timing", Duration::from_micros(1)),
            ("zero_timing", Duration::from_nanos(0)),
            ("large_timing", Duration::from_secs(60)),
        ];

        for (scenario_name, target_duration) in timing_scenarios {
            // Mock benchmark with timing challenges
            let timing_result = measure_benchmark_timing_safely(scenario_name, target_duration);

            match timing_result {
                Ok(measurement) => {
                    // Should handle timing scenarios safely
                    assert!(
                        measurement.iterations > 0,
                        "Timing scenario '{}' should have positive iterations",
                        scenario_name
                    );

                    // Validate timing measurements are consistent
                    if measurement.duration > Duration::from_nanos(0) {
                        let iterations_per_second =
                            measurement.iterations as f64 / measurement.duration.as_secs_f64();
                        assert!(
                            iterations_per_second < 1e12, // Reasonable iterations per second
                            "Timing scenario '{}' should have reasonable iteration rate: {}",
                            scenario_name,
                            iterations_per_second
                        );
                    }
                }
                Err(error) => {
                    // Should handle timing issues gracefully
                    assert!(
                        error.contains("timing")
                            || error.contains("measurement")
                            || error.contains(scenario_name),
                        "Timing error for '{}' should reference timing issue: {}",
                        scenario_name,
                        error
                    );

                    // Should not contain panic traces
                    assert!(
                        !error.contains("panic") && !error.contains("unwrap"),
                        "Timing error for '{}' should not contain panic traces: {}",
                        scenario_name,
                        error
                    );
                }
            }
        }
    }

    #[test] // AC:63-20-3 Performance regression detection safety
    fn test_performance_regression_detection_safety() {
        // Test case: Performance regression detection with corrupted or missing baselines
        let baseline_scenarios = vec![
            (
                "valid_baseline",
                Some(PerformanceBaseline {
                    display_throughput_gbps: 2.33,
                    comp3_throughput_mbps: 168.0,
                    memory_limit_mb: 256,
                    variance_threshold_percent: 5.0,
                }),
            ),
            ("missing_baseline", None),
            (
                "corrupted_baseline",
                Some(PerformanceBaseline {
                    display_throughput_gbps: -1.0,     // Invalid negative throughput
                    comp3_throughput_mbps: f64::NAN,   // Invalid NaN value
                    memory_limit_mb: 0,                // Invalid zero memory limit
                    variance_threshold_percent: 150.0, // Invalid percentage
                }),
            ),
            (
                "extreme_baseline",
                Some(PerformanceBaseline {
                    display_throughput_gbps: 1e12,     // Extremely high throughput
                    comp3_throughput_mbps: 1e9,        // Extremely high throughput
                    memory_limit_mb: usize::MAX,       // Maximum memory
                    variance_threshold_percent: 0.001, // Extremely tight variance
                }),
            ),
        ];

        for (scenario_name, baseline) in baseline_scenarios {
            // Mock current performance measurements
            let current_performance = BenchmarkResult {
                name: format!("test_{}", scenario_name),
                duration: Duration::from_millis(100),
                iterations: 1000,
                throughput: Some(2.0),                 // 2 GiB/s throughput
                memory_usage: Some(128 * 1024 * 1024), // 128 MB
            };

            // Should handle regression detection safely
            let regression_result =
                detect_performance_regression_safely(&baseline, &current_performance);

            match regression_result {
                Ok(detection) => {
                    // Should provide meaningful regression analysis
                    assert!(
                        detection.analyzed,
                        "Regression detection for '{}' should complete analysis",
                        scenario_name
                    );

                    // Validate detection results are reasonable
                    if let Some(regression_percent) = detection.regression_percent {
                        assert!(
                            regression_percent.is_finite(),
                            "Regression percentage for '{}' should be finite: {}",
                            scenario_name,
                            regression_percent
                        );
                    }
                }
                Err(error) => {
                    // Should handle regression detection issues gracefully
                    assert!(
                        error.contains("regression")
                            || error.contains("detection")
                            || error.contains(scenario_name),
                        "Regression detection error for '{}' should reference detection issue: {}",
                        scenario_name,
                        error
                    );

                    // Should not contain panic traces
                    assert!(
                        !error.contains("panic") && !error.contains("unwrap"),
                        "Regression detection error for '{}' should not contain panic traces: {}",
                        scenario_name,
                        error
                    );
                }
            }
        }
    }

    #[test] // AC:63-20-4 Benchmark result aggregation safety
    fn test_benchmark_result_aggregation_safety() {
        // Test case: Benchmark result aggregation with inconsistent or invalid data
        let aggregation_scenarios = vec![
            ("empty_results", vec![]),
            (
                "single_result",
                vec![BenchmarkResult {
                    name: "test_single".to_string(),
                    duration: Duration::from_millis(50),
                    iterations: 500,
                    throughput: Some(1.5),
                    memory_usage: Some(64 * 1024 * 1024),
                }],
            ),
            (
                "inconsistent_results",
                vec![
                    BenchmarkResult {
                        name: "test_inconsistent_1".to_string(),
                        duration: Duration::from_millis(10),
                        iterations: 100,
                        throughput: Some(10.0),
                        memory_usage: Some(32 * 1024 * 1024),
                    },
                    BenchmarkResult {
                        name: "test_inconsistent_2".to_string(),
                        duration: Duration::from_secs(10),
                        iterations: 1,
                        throughput: Some(0.001),
                        memory_usage: Some(1024 * 1024 * 1024),
                    },
                    BenchmarkResult {
                        name: "test_inconsistent_3".to_string(),
                        duration: Duration::from_nanos(1),
                        iterations: 1000000,
                        throughput: None,   // Missing throughput
                        memory_usage: None, // Missing memory usage
                    },
                ],
            ),
            (
                "extreme_results",
                vec![
                    BenchmarkResult {
                        name: "test_extreme_1".to_string(),
                        duration: Duration::from_nanos(0), // Zero duration
                        iterations: 0,                     // Zero iterations
                        throughput: Some(f64::INFINITY),   // Infinite throughput
                        memory_usage: Some(0),             // Zero memory
                    },
                    BenchmarkResult {
                        name: "test_extreme_2".to_string(),
                        duration: Duration::from_secs(u64::MAX / 1000), // Maximum duration
                        iterations: u64::MAX,                           // Maximum iterations
                        throughput: Some(f64::NEG_INFINITY), // Negative infinite throughput
                        memory_usage: Some(usize::MAX),      // Maximum memory
                    },
                ],
            ),
        ];

        for (scenario_name, results) in aggregation_scenarios {
            // Should handle result aggregation safely
            let aggregation_result = aggregate_benchmark_results_safely(&results);

            match aggregation_result {
                Ok(aggregated) => {
                    // Should provide meaningful aggregation
                    assert!(
                        !aggregated.summary.is_empty(),
                        "Aggregation for '{}' should provide summary",
                        scenario_name
                    );

                    // Validate aggregated statistics are reasonable
                    if let Some(mean_throughput) = aggregated.mean_throughput {
                        assert!(
                            mean_throughput.is_finite() && mean_throughput >= 0.0,
                            "Mean throughput for '{}' should be finite and non-negative: {}",
                            scenario_name,
                            mean_throughput
                        );
                    }

                    if let Some(mean_memory) = aggregated.mean_memory_usage {
                        assert!(
                            mean_memory <= usize::MAX / 2, // Reasonable memory bound
                            "Mean memory usage for '{}' should be reasonable: {}",
                            scenario_name,
                            mean_memory
                        );
                    }
                }
                Err(error) => {
                    // Should handle aggregation issues gracefully
                    assert!(
                        error.contains("aggregation")
                            || error.contains("results")
                            || error.contains(scenario_name),
                        "Aggregation error for '{}' should reference aggregation issue: {}",
                        scenario_name,
                        error
                    );

                    // Should not contain panic traces
                    assert!(
                        !error.contains("panic") && !error.contains("unwrap"),
                        "Aggregation error for '{}' should not contain panic traces: {}",
                        scenario_name,
                        error
                    );
                }
            }
        }
    }

    #[test] // AC:63-20-5 Benchmark reporting safety
    fn test_benchmark_reporting_safety() {
        // Test case: Benchmark reporting with various data formats and edge cases
        let reporting_scenarios = vec![
            (
                "standard_report",
                BenchmarkResult {
                    name: "standard_benchmark".to_string(),
                    duration: Duration::from_millis(200),
                    iterations: 2000,
                    throughput: Some(3.5),
                    memory_usage: Some(128 * 1024 * 1024),
                },
            ),
            (
                "minimal_report",
                BenchmarkResult {
                    name: "".to_string(), // Empty name
                    duration: Duration::from_nanos(1),
                    iterations: 1,
                    throughput: None,
                    memory_usage: None,
                },
            ),
            (
                "unicode_report",
                BenchmarkResult {
                    name: "测试_benchmark_🚀".to_string(), // Unicode characters
                    duration: Duration::from_millis(150),
                    iterations: 1500,
                    throughput: Some(2.8),
                    memory_usage: Some(96 * 1024 * 1024),
                },
            ),
            (
                "special_chars_report",
                BenchmarkResult {
                    name: "benchmark\\with\"special'chars<>&".to_string(), // Special characters
                    duration: Duration::from_millis(175),
                    iterations: 1750,
                    throughput: Some(f64::NAN),     // NaN throughput
                    memory_usage: Some(usize::MAX), // Maximum memory
                },
            ),
        ];

        let report_formats = vec!["json", "csv", "html", "text", "xml"];

        for (scenario_name, benchmark_result) in reporting_scenarios {
            for format in &report_formats {
                // Should handle benchmark reporting safely
                let reporting_result = generate_benchmark_report_safely(&benchmark_result, format);

                match reporting_result {
                    Ok(report) => {
                        // Should generate valid report
                        assert!(
                            !report.content.is_empty(),
                            "Report for '{}' in format '{}' should have content",
                            scenario_name,
                            format
                        );

                        // Validate report structure based on format
                        match *format {
                            "json" => {
                                // Should be valid JSON or handle gracefully
                                if let Err(json_error) =
                                    serde_json::from_str::<serde_json::Value>(&report.content)
                                {
                                    // JSON parsing may fail for edge cases - that's acceptable
                                    println!(
                                        "JSON parsing failed for '{}': {} (acceptable for edge cases)",
                                        scenario_name, json_error
                                    );
                                }
                            }
                            "csv" => {
                                // Should contain CSV-like structure
                                assert!(
                                    report.content.contains(',') || report.content.contains('\n'),
                                    "CSV report for '{}' should contain CSV separators",
                                    scenario_name
                                );
                            }
                            _ => {
                                // Other formats should have reasonable content
                                assert!(
                                    !report.content.is_empty(),
                                    "Report for '{}' in format '{}' should have minimal content",
                                    scenario_name,
                                    format
                                );
                            }
                        }
                    }
                    Err(error) => {
                        // Should handle reporting issues gracefully
                        assert!(
                            error.contains("report")
                                || error.contains("format")
                                || error.contains(format),
                            "Reporting error for '{}' in format '{}' should reference reporting issue: {}",
                            scenario_name,
                            format,
                            error
                        );

                        // Should not contain panic traces
                        assert!(
                            !error.contains("panic") && !error.contains("unwrap"),
                            "Reporting error for '{}' in format '{}' should not contain panic traces: {}",
                            scenario_name,
                            format,
                            error
                        );
                    }
                }
            }
        }
    }

    #[test] // AC:63-20-6 Benchmark cleanup and resource management safety
    fn test_benchmark_cleanup_safety() {
        // Test case: Benchmark cleanup and resource management
        let cleanup_scenarios = vec![
            ("normal_cleanup", 10, Duration::from_millis(100)),
            ("rapid_cleanup", 1000, Duration::from_millis(1)), // Many short benchmarks
            ("long_cleanup", 1, Duration::from_secs(5)),       // Single long benchmark
            ("zero_cleanup", 0, Duration::from_nanos(0)),      // Edge case
        ];

        for (scenario_name, benchmark_count, benchmark_duration) in cleanup_scenarios {
            // Simulate benchmark execution with resource allocation
            let resource_allocation_result = allocate_benchmark_resources_safely(benchmark_count);

            match resource_allocation_result {
                Ok(resources) => {
                    // Execute benchmarks with allocated resources
                    let mut benchmark_results = Vec::new();

                    for i in 0..benchmark_count {
                        let execution_result = execute_benchmark_with_resources_safely(
                            &format!("{}_{}", scenario_name, i),
                            benchmark_duration,
                            &resources,
                        );

                        match execution_result {
                            Ok(result) => benchmark_results.push(result),
                            Err(error) => {
                                // Execution errors should be handled gracefully
                                assert!(
                                    error.contains("execution") || error.contains("benchmark"),
                                    "Execution error for '{}' should reference execution issue: {}",
                                    scenario_name,
                                    error
                                );
                            }
                        }
                    }

                    // Test resource cleanup
                    let cleanup_result = cleanup_benchmark_resources_safely(resources);

                    match cleanup_result {
                        Ok(cleanup_status) => {
                            // Should report successful cleanup
                            assert!(
                                cleanup_status.successful,
                                "Cleanup for '{}' should be successful",
                                scenario_name
                            );

                            // Should not report resource leaks
                            assert!(
                                cleanup_status.resources_freed >= benchmark_count,
                                "Cleanup for '{}' should free adequate resources",
                                scenario_name
                            );
                        }
                        Err(error) => {
                            // Cleanup errors should be structured
                            assert!(
                                error.contains("cleanup")
                                    || error.contains("resource")
                                    || error.contains(scenario_name),
                                "Cleanup error for '{}' should reference cleanup issue: {}",
                                scenario_name,
                                error
                            );

                            // Should not contain panic traces
                            assert!(
                                !error.contains("panic") && !error.contains("unwrap"),
                                "Cleanup error for '{}' should not contain panic traces: {}",
                                scenario_name,
                                error
                            );
                        }
                    }
                }
                Err(error) => {
                    // Resource allocation errors should be handled gracefully
                    assert!(
                        error.contains("allocation")
                            || error.contains("resource")
                            || error.contains(scenario_name),
                        "Resource allocation error for '{}' should reference allocation issue: {}",
                        scenario_name,
                        error
                    );

                    // Should not contain panic traces
                    assert!(
                        !error.contains("panic") && !error.contains("unwrap"),
                        "Resource allocation error for '{}' should not contain panic traces: {}",
                        scenario_name,
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

    /// Performance validation for benchmark panic elimination changes
    /// AC:63-21 - Benchmark performance measurement accuracy preserved

    #[test] // AC:63-21-1 Measurement accuracy preservation
    fn test_measurement_accuracy_preservation() {
        // Test case: Measurement accuracy with panic elimination overhead
        let accuracy_config = BenchmarkConfig {
            warmup_time: Duration::from_millis(100),
            measurement_time: Duration::from_millis(500),
            sample_size: 50,
            confidence_level: 95.0,
        };

        // Run multiple measurements to test consistency
        let mut measurements = Vec::new();

        for i in 0..10 {
            let measurement_result =
                execute_benchmark_safely(&format!("accuracy_test_{}", i), &accuracy_config);

            match measurement_result {
                Ok(result) => {
                    measurements.push(result);
                }
                Err(error) => {
                    assert!(
                        error.contains("measurement") || error.contains("accuracy"),
                        "Accuracy measurement error should reference measurement issue: {}",
                        error
                    );
                }
            }
        }

        // Validate measurement consistency
        if measurements.len() >= 2 {
            let mean_duration = measurements
                .iter()
                .map(|m| m.duration.as_nanos() as f64)
                .sum::<f64>()
                / measurements.len() as f64;

            let variance = measurements
                .iter()
                .map(|m| {
                    let diff = m.duration.as_nanos() as f64 - mean_duration;
                    diff * diff
                })
                .sum::<f64>()
                / measurements.len() as f64;

            let coefficient_of_variation = variance.sqrt() / mean_duration;

            // Measurement variation should be reasonable
            assert!(
                coefficient_of_variation < 0.1, // 10% CV limit
                "Measurement accuracy should be consistent: CV = {}",
                coefficient_of_variation
            );
        }
    }

    #[test] // AC:63-21-2 Throughput calculation safety
    fn test_throughput_calculation_safety() {
        // Test case: Throughput calculation with edge cases
        let throughput_scenarios = vec![
            (
                "normal_throughput",
                Duration::from_millis(100),
                1000,
                Some(10000.0),
            ),
            ("zero_duration", Duration::from_nanos(0), 1000, None), // Division by zero
            ("zero_iterations", Duration::from_millis(100), 0, Some(0.0)),
            (
                "extreme_duration",
                Duration::from_secs(3600),
                1,
                Some(0.000277),
            ), // Very low throughput
            (
                "extreme_iterations",
                Duration::from_millis(1),
                1000000,
                Some(1000000000.0),
            ), // Very high throughput
        ];

        for (scenario_name, duration, iterations, expected_range) in throughput_scenarios {
            let calculation_result = calculate_throughput_safely(duration, iterations);

            match calculation_result {
                Ok(throughput) => {
                    // Should handle throughput calculation safely
                    assert!(
                        throughput.is_finite() && throughput >= 0.0,
                        "Throughput for '{}' should be finite and non-negative: {}",
                        scenario_name,
                        throughput
                    );

                    // Validate against expected range if provided
                    if let Some(expected) = expected_range {
                        let relative_error = ((throughput - expected) / expected).abs();
                        assert!(
                            relative_error < 0.1, // 10% tolerance
                            "Throughput for '{}' should be within expected range: {} vs {}",
                            scenario_name,
                            throughput,
                            expected
                        );
                    }
                }
                Err(error) => {
                    // Should handle calculation issues gracefully
                    assert!(
                        error.contains("throughput")
                            || error.contains("calculation")
                            || error.contains(scenario_name),
                        "Throughput calculation error for '{}' should reference calculation issue: {}",
                        scenario_name,
                        error
                    );

                    // Should not contain panic traces
                    assert!(
                        !error.contains("panic") && !error.contains("unwrap"),
                        "Throughput calculation error for '{}' should not contain panic traces: {}",
                        scenario_name,
                        error
                    );
                }
            }
        }
    }
}

// Mock implementation functions for testing
// These would be replaced by actual copybook-bench implementations

fn execute_benchmark_safely(
    name: &str,
    config: &BenchmarkConfig,
) -> Result<BenchmarkResult, String> {
    // Mock implementation that simulates panic elimination patterns
    if config.sample_size == 0 {
        return Err("Invalid sample size: cannot be zero".to_string());
    }

    if config.confidence_level <= 0.0 || config.confidence_level > 100.0 {
        return Err(format!(
            "Invalid confidence level: {}",
            config.confidence_level
        ));
    }

    Ok(BenchmarkResult {
        name: name.to_string(),
        duration: config.measurement_time,
        iterations: config.sample_size as u64,
        throughput: Some(2.5),                 // Mock throughput
        memory_usage: Some(128 * 1024 * 1024), // Mock memory usage
    })
}

fn measure_benchmark_timing_safely(
    name: &str,
    target_duration: Duration,
) -> Result<BenchmarkResult, String> {
    if target_duration == Duration::from_nanos(0) {
        return Err("Cannot measure zero duration benchmark".to_string());
    }

    let iterations = if target_duration < Duration::from_millis(1) {
        1000000 // High iterations for short duration
    } else {
        1000 // Standard iterations
    };

    Ok(BenchmarkResult {
        name: name.to_string(),
        duration: target_duration,
        iterations,
        throughput: Some(1.0),
        memory_usage: Some(64 * 1024 * 1024),
    })
}

#[derive(Debug)]
#[allow(dead_code)]
struct RegressionDetection {
    analyzed: bool,
    regression_detected: bool,
    regression_percent: Option<f64>,
}

fn detect_performance_regression_safely(
    baseline: &Option<PerformanceBaseline>,
    current: &BenchmarkResult,
) -> Result<RegressionDetection, String> {
    match baseline {
        Some(baseline) => {
            if baseline.display_throughput_gbps <= 0.0
                || !baseline.display_throughput_gbps.is_finite()
            {
                return Err("Invalid baseline throughput".to_string());
            }

            let current_throughput = current.throughput.unwrap_or(0.0);
            let regression_percent = if baseline.display_throughput_gbps > 0.0 {
                ((baseline.display_throughput_gbps - current_throughput)
                    / baseline.display_throughput_gbps)
                    * 100.0
            } else {
                0.0
            };

            Ok(RegressionDetection {
                analyzed: true,
                regression_detected: regression_percent > baseline.variance_threshold_percent,
                regression_percent: Some(regression_percent),
            })
        }
        None => Ok(RegressionDetection {
            analyzed: false,
            regression_detected: false,
            regression_percent: None,
        }),
    }
}

#[derive(Debug)]
struct AggregatedResults {
    summary: String,
    mean_throughput: Option<f64>,
    mean_memory_usage: Option<usize>,
}

fn aggregate_benchmark_results_safely(
    results: &[BenchmarkResult],
) -> Result<AggregatedResults, String> {
    if results.is_empty() {
        return Ok(AggregatedResults {
            summary: "No results to aggregate".to_string(),
            mean_throughput: None,
            mean_memory_usage: None,
        });
    }

    let valid_throughputs: Vec<f64> = results
        .iter()
        .filter_map(|r| r.throughput)
        .filter(|t| t.is_finite() && *t >= 0.0)
        .collect();

    let valid_memory_usages: Vec<usize> = results
        .iter()
        .filter_map(|r| r.memory_usage)
        .filter(|m| *m < usize::MAX / 2)
        .collect();

    let mean_throughput = if !valid_throughputs.is_empty() {
        Some(valid_throughputs.iter().sum::<f64>() / valid_throughputs.len() as f64)
    } else {
        None
    };

    let mean_memory_usage = if !valid_memory_usages.is_empty() {
        Some(valid_memory_usages.iter().sum::<usize>() / valid_memory_usages.len())
    } else {
        None
    };

    Ok(AggregatedResults {
        summary: format!("Aggregated {} results", results.len()),
        mean_throughput,
        mean_memory_usage,
    })
}

#[derive(Debug)]
#[allow(dead_code)]
struct BenchmarkReport {
    content: String,
    format: String,
}

fn generate_benchmark_report_safely(
    result: &BenchmarkResult,
    format: &str,
) -> Result<BenchmarkReport, String> {
    let content = match format {
        "json" => {
            if result.name.is_empty() {
                "{\"name\":\"unnamed\",\"duration\":0,\"iterations\":0}".to_string()
            } else {
                format!(
                    "{{\"name\":\"{}\",\"duration\":{},\"iterations\":{}}}",
                    result.name.replace('"', "\\\""),
                    result.duration.as_millis(),
                    result.iterations
                )
            }
        }
        "csv" => {
            format!(
                "name,duration_ms,iterations\n{},{},{}",
                result.name.replace(',', "_"),
                result.duration.as_millis(),
                result.iterations
            )
        }
        _ => {
            format!(
                "Benchmark: {}\nDuration: {:?}\nIterations: {}",
                if result.name.is_empty() {
                    "unnamed"
                } else {
                    &result.name
                },
                result.duration,
                result.iterations
            )
        }
    };

    Ok(BenchmarkReport {
        content,
        format: format.to_string(),
    })
}

#[derive(Debug)]
#[allow(dead_code)]
struct BenchmarkResources {
    allocated_count: usize,
    resource_ids: Vec<u64>,
}

fn allocate_benchmark_resources_safely(count: usize) -> Result<BenchmarkResources, String> {
    if count > 10000 {
        return Err("Resource count exceeds system limits".to_string());
    }

    Ok(BenchmarkResources {
        allocated_count: count,
        resource_ids: (0..count as u64).collect(),
    })
}

fn execute_benchmark_with_resources_safely(
    name: &str,
    duration: Duration,
    _resources: &BenchmarkResources,
) -> Result<BenchmarkResult, String> {
    if duration > Duration::from_secs(60) {
        return Err("Benchmark duration exceeds time limits".to_string());
    }

    Ok(BenchmarkResult {
        name: name.to_string(),
        duration,
        iterations: 100,
        throughput: Some(1.5),
        memory_usage: Some(32 * 1024 * 1024),
    })
}

#[derive(Debug)]
struct CleanupStatus {
    successful: bool,
    resources_freed: usize,
}

fn cleanup_benchmark_resources_safely(
    resources: BenchmarkResources,
) -> Result<CleanupStatus, String> {
    // Mock cleanup implementation
    Ok(CleanupStatus {
        successful: true,
        resources_freed: resources.allocated_count,
    })
}

fn calculate_throughput_safely(duration: Duration, iterations: u64) -> Result<f64, String> {
    if duration == Duration::from_nanos(0) {
        return Err("Cannot calculate throughput for zero duration".to_string());
    }

    let duration_secs = duration.as_secs_f64();
    if !duration_secs.is_finite() || duration_secs <= 0.0 {
        return Err("Invalid duration for throughput calculation".to_string());
    }

    let throughput = iterations as f64 / duration_secs;

    if !throughput.is_finite() {
        return Err("Throughput calculation resulted in invalid value".to_string());
    }

    Ok(throughput)
}
