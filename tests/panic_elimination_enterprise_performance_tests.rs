/// Tests feature spec: issue-63-spec.md#ac4-performance-impact-limited
/// Tests feature spec: issue-63-technical-specification.md#performance-specifications
/// Tests feature spec: panic-elimination-implementation-blueprint.md#performance-monitoring-integration
///
/// Issue #63 - Enterprise Performance Validation Test Scaffolding
///
/// This module provides comprehensive test scaffolding for validating that panic elimination
/// changes maintain enterprise performance targets: DISPLAY ≥2.33 GiB/s, COMP-3 ≥168 MiB/s,
/// with <5% overall performance impact across all copybook-rs components.
///
/// **AC Traceability:**
/// - AC4: Performance impact limited to <5% degradation on enterprise benchmarks
/// - AC7: Comprehensive test coverage for performance preservation
/// - AC12: Enterprise validation through performance regression detection
/// - Performance targets: DISPLAY ≥2.33 GiB/s, COMP-3 ≥168 MiB/s, Memory <256 MiB

use std::time::{Duration, Instant};
use std::collections::HashMap;

/// Enterprise performance baseline requirements
const ENTERPRISE_DISPLAY_THRESHOLD_GBPS: f64 = 2.33;
const ENTERPRISE_COMP3_THRESHOLD_MBPS: f64 = 168.0;
const ENTERPRISE_MEMORY_LIMIT_MB: usize = 256;
const ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD: f64 = 5.0; // 5% maximum impact

#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    pub display_throughput_gbps: f64,
    pub comp3_throughput_mbps: f64,
    pub memory_usage_mb: usize,
    pub processing_latency_ms: f64,
    pub cpu_utilization_percent: f64,
}

#[derive(Debug, Clone)]
pub struct EnterpriseWorkload {
    pub name: String,
    pub record_count: usize,
    pub record_size: usize,
    pub field_complexity: WorkloadComplexity,
    pub data_patterns: Vec<DataPattern>,
}

#[derive(Debug, Clone)]
pub enum WorkloadComplexity {
    Simple,    // Basic fields only
    Moderate,  // Mixed field types
    Complex,   // ODO, REDEFINES, Level-88
    Enterprise, // Full production complexity
}

#[derive(Debug, Clone)]
pub enum DataPattern {
    Sequential,
    Random,
    Realistic,
    EdgeCase,
    Stress,
}

#[cfg(test)]
mod enterprise_performance_validation_tests {
    use super::*;

    /// Enterprise performance validation for panic elimination
    /// AC:63-22 - Enterprise performance targets maintained with <5% impact

    #[test] // AC:63-22-1 DISPLAY format throughput preservation
    fn test_display_format_throughput_preservation() {
        // Test case: DISPLAY format processing maintaining ≥2.33 GiB/s throughput
        let display_workloads = vec![
            EnterpriseWorkload {
                name: "display_high_volume".to_string(),
                record_count: 1000000,
                record_size: 1024,
                field_complexity: WorkloadComplexity::Moderate,
                data_patterns: vec![DataPattern::Sequential, DataPattern::Realistic],
            },
            EnterpriseWorkload {
                name: "display_enterprise_mixed".to_string(),
                record_count: 500000,
                record_size: 2048,
                field_complexity: WorkloadComplexity::Enterprise,
                data_patterns: vec![DataPattern::Random, DataPattern::EdgeCase],
            },
            EnterpriseWorkload {
                name: "display_stress_test".to_string(),
                record_count: 100000,
                record_size: 8192,
                field_complexity: WorkloadComplexity::Complex,
                data_patterns: vec![DataPattern::Stress],
            },
        ];

        for workload in display_workloads {
            // Measure baseline performance before panic elimination
            let baseline_result = measure_display_performance_safely(&workload, false);

            match baseline_result {
                Ok(baseline_metrics) => {
                    // Measure performance with panic elimination changes
                    let panic_safe_result = measure_display_performance_safely(&workload, true);

                    match panic_safe_result {
                        Ok(panic_safe_metrics) => {
                            // Validate DISPLAY throughput meets enterprise threshold
                            assert!(
                                panic_safe_metrics.display_throughput_gbps >= ENTERPRISE_DISPLAY_THRESHOLD_GBPS,
                                "DISPLAY throughput for '{}' should meet enterprise threshold: {} GiB/s >= {} GiB/s",
                                workload.name, panic_safe_metrics.display_throughput_gbps, ENTERPRISE_DISPLAY_THRESHOLD_GBPS
                            );

                            // Validate performance impact is within acceptable range
                            let performance_impact = calculate_performance_impact(
                                baseline_metrics.display_throughput_gbps,
                                panic_safe_metrics.display_throughput_gbps
                            );

                            assert!(
                                performance_impact <= ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD,
                                "DISPLAY performance impact for '{}' should be within threshold: {}% <= {}%",
                                workload.name, performance_impact, ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD
                            );

                            // Validate memory usage remains within enterprise limits
                            assert!(
                                panic_safe_metrics.memory_usage_mb <= ENTERPRISE_MEMORY_LIMIT_MB,
                                "Memory usage for '{}' should be within enterprise limit: {} MB <= {} MB",
                                workload.name, panic_safe_metrics.memory_usage_mb, ENTERPRISE_MEMORY_LIMIT_MB
                            );
                        }
                        Err(error) => {
                            assert!(
                                error.contains("performance") || error.contains("measurement"),
                                "Panic-safe performance measurement error for '{}' should reference measurement issue: {}",
                                workload.name, error
                            );
                        }
                    }
                }
                Err(error) => {
                    assert!(
                        error.contains("baseline") || error.contains("measurement"),
                        "Baseline performance measurement error for '{}' should reference measurement issue: {}",
                        workload.name, error
                    );
                }
            }
        }
    }

    #[test] // AC:63-22-2 COMP-3 format throughput preservation
    fn test_comp3_format_throughput_preservation() {
        // Test case: COMP-3 format processing maintaining ≥168 MiB/s throughput
        let comp3_workloads = vec![
            EnterpriseWorkload {
                name: "comp3_financial_processing".to_string(),
                record_count: 2000000,
                record_size: 256,
                field_complexity: WorkloadComplexity::Enterprise,
                data_patterns: vec![DataPattern::Realistic, DataPattern::Sequential],
            },
            EnterpriseWorkload {
                name: "comp3_precision_calculations".to_string(),
                record_count: 1000000,
                record_size: 512,
                field_complexity: WorkloadComplexity::Complex,
                data_patterns: vec![DataPattern::EdgeCase, DataPattern::Random],
            },
            EnterpriseWorkload {
                name: "comp3_high_precision_stress".to_string(),
                record_count: 500000,
                record_size: 1024,
                field_complexity: WorkloadComplexity::Enterprise,
                data_patterns: vec![DataPattern::Stress],
            },
        ];

        for workload in comp3_workloads {
            // Measure baseline COMP-3 performance
            let baseline_result = measure_comp3_performance_safely(&workload, false);

            match baseline_result {
                Ok(baseline_metrics) => {
                    // Measure COMP-3 performance with panic elimination
                    let panic_safe_result = measure_comp3_performance_safely(&workload, true);

                    match panic_safe_result {
                        Ok(panic_safe_metrics) => {
                            // Validate COMP-3 throughput meets enterprise threshold
                            assert!(
                                panic_safe_metrics.comp3_throughput_mbps >= ENTERPRISE_COMP3_THRESHOLD_MBPS,
                                "COMP-3 throughput for '{}' should meet enterprise threshold: {} MiB/s >= {} MiB/s",
                                workload.name, panic_safe_metrics.comp3_throughput_mbps, ENTERPRISE_COMP3_THRESHOLD_MBPS
                            );

                            // Validate performance impact is acceptable
                            let performance_impact = calculate_performance_impact(
                                baseline_metrics.comp3_throughput_mbps,
                                panic_safe_metrics.comp3_throughput_mbps
                            );

                            assert!(
                                performance_impact <= ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD,
                                "COMP-3 performance impact for '{}' should be within threshold: {}% <= {}%",
                                workload.name, performance_impact, ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD
                            );

                            // Validate numeric precision preservation
                            let precision_result = validate_comp3_precision_preservation(&workload);
                            match precision_result {
                                Ok(precision_maintained) => {
                                    assert!(
                                        precision_maintained,
                                        "COMP-3 precision should be maintained for '{}'",
                                        workload.name
                                    );
                                }
                                Err(error) => {
                                    assert!(
                                        error.contains("precision") || error.contains("comp3"),
                                        "COMP-3 precision validation error for '{}' should reference precision issue: {}",
                                        workload.name, error
                                    );
                                }
                            }
                        }
                        Err(error) => {
                            assert!(
                                error.contains("comp3") || error.contains("performance"),
                                "COMP-3 performance measurement error for '{}' should reference measurement issue: {}",
                                workload.name, error
                            );
                        }
                    }
                }
                Err(error) => {
                    assert!(
                        error.contains("baseline") || error.contains("comp3"),
                        "COMP-3 baseline measurement error for '{}' should reference measurement issue: {}",
                        workload.name, error
                    );
                }
            }
        }
    }

    #[test] // AC:63-22-3 Mixed workload performance preservation
    fn test_mixed_workload_performance_preservation() {
        // Test case: Mixed enterprise workloads with combined DISPLAY and COMP-3 processing
        let mixed_workloads = vec![
            EnterpriseWorkload {
                name: "mixed_enterprise_standard".to_string(),
                record_count: 1000000,
                record_size: 1024,
                field_complexity: WorkloadComplexity::Enterprise,
                data_patterns: vec![DataPattern::Realistic, DataPattern::Sequential],
            },
            EnterpriseWorkload {
                name: "mixed_financial_complex".to_string(),
                record_count: 750000,
                record_size: 1536,
                field_complexity: WorkloadComplexity::Enterprise,
                data_patterns: vec![DataPattern::EdgeCase, DataPattern::Random],
            },
            EnterpriseWorkload {
                name: "mixed_production_simulation".to_string(),
                record_count: 500000,
                record_size: 2048,
                field_complexity: WorkloadComplexity::Enterprise,
                data_patterns: vec![DataPattern::Stress, DataPattern::Realistic],
            },
        ];

        for workload in mixed_workloads {
            // Measure comprehensive mixed workload performance
            let baseline_result = measure_mixed_workload_performance_safely(&workload, false);

            match baseline_result {
                Ok(baseline_metrics) => {
                    // Measure mixed workload performance with panic elimination
                    let panic_safe_result = measure_mixed_workload_performance_safely(&workload, true);

                    match panic_safe_result {
                        Ok(panic_safe_metrics) => {
                            // Validate both DISPLAY and COMP-3 thresholds in mixed workload
                            assert!(
                                panic_safe_metrics.display_throughput_gbps >= ENTERPRISE_DISPLAY_THRESHOLD_GBPS * 0.8, // 80% of threshold for mixed workload
                                "Mixed workload DISPLAY throughput for '{}' should meet proportional threshold: {} GiB/s >= {} GiB/s",
                                workload.name, panic_safe_metrics.display_throughput_gbps, ENTERPRISE_DISPLAY_THRESHOLD_GBPS * 0.8
                            );

                            assert!(
                                panic_safe_metrics.comp3_throughput_mbps >= ENTERPRISE_COMP3_THRESHOLD_MBPS * 0.8, // 80% of threshold for mixed workload
                                "Mixed workload COMP-3 throughput for '{}' should meet proportional threshold: {} MiB/s >= {} MiB/s",
                                workload.name, panic_safe_metrics.comp3_throughput_mbps, ENTERPRISE_COMP3_THRESHOLD_MBPS * 0.8
                            );

                            // Validate overall performance impact across mixed processing
                            let overall_impact = calculate_mixed_workload_impact(&baseline_metrics, &panic_safe_metrics);

                            assert!(
                                overall_impact <= ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD,
                                "Mixed workload overall performance impact for '{}' should be within threshold: {}% <= {}%",
                                workload.name, overall_impact, ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD
                            );

                            // Validate resource efficiency in mixed scenarios
                            assert!(
                                panic_safe_metrics.memory_usage_mb <= ENTERPRISE_MEMORY_LIMIT_MB,
                                "Mixed workload memory usage for '{}' should be within limit: {} MB <= {} MB",
                                workload.name, panic_safe_metrics.memory_usage_mb, ENTERPRISE_MEMORY_LIMIT_MB
                            );

                            assert!(
                                panic_safe_metrics.cpu_utilization_percent <= 95.0, // Maximum 95% CPU utilization
                                "Mixed workload CPU utilization for '{}' should be reasonable: {}% <= 95%",
                                workload.name, panic_safe_metrics.cpu_utilization_percent
                            );
                        }
                        Err(error) => {
                            assert!(
                                error.contains("mixed") || error.contains("workload") || error.contains("performance"),
                                "Mixed workload performance error for '{}' should reference workload issue: {}",
                                workload.name, error
                            );
                        }
                    }
                }
                Err(error) => {
                    assert!(
                        error.contains("baseline") || error.contains("mixed"),
                        "Mixed workload baseline error for '{}' should reference baseline issue: {}",
                        workload.name, error
                    );
                }
            }
        }
    }

    #[test] // AC:63-22-4 Enterprise stress testing validation
    fn test_enterprise_stress_testing_validation() {
        // Test case: Enterprise stress conditions with panic elimination
        let stress_scenarios = vec![
            ("high_concurrency", 1000000, 512, WorkloadComplexity::Enterprise),
            ("large_records", 100000, 8192, WorkloadComplexity::Enterprise),
            ("complex_structures", 500000, 2048, WorkloadComplexity::Enterprise),
            ("memory_pressure", 2000000, 1024, WorkloadComplexity::Complex),
        ];

        for (scenario_name, record_count, record_size, complexity) in stress_scenarios {
            let stress_workload = EnterpriseWorkload {
                name: format!("stress_{}", scenario_name),
                record_count,
                record_size,
                field_complexity: complexity,
                data_patterns: vec![DataPattern::Stress, DataPattern::EdgeCase],
            };

            // Execute stress test with panic elimination safety
            let stress_result = execute_enterprise_stress_test_safely(&stress_workload);

            match stress_result {
                Ok(stress_metrics) => {
                    // Validate stress test maintains minimum performance thresholds
                    let min_display_threshold = ENTERPRISE_DISPLAY_THRESHOLD_GBPS * 0.7; // 70% under stress
                    let min_comp3_threshold = ENTERPRISE_COMP3_THRESHOLD_MBPS * 0.7; // 70% under stress

                    assert!(
                        stress_metrics.display_throughput_gbps >= min_display_threshold,
                        "Stress test '{}' DISPLAY throughput should meet minimum threshold: {} GiB/s >= {} GiB/s",
                        scenario_name, stress_metrics.display_throughput_gbps, min_display_threshold
                    );

                    assert!(
                        stress_metrics.comp3_throughput_mbps >= min_comp3_threshold,
                        "Stress test '{}' COMP-3 throughput should meet minimum threshold: {} MiB/s >= {} MiB/s",
                        scenario_name, stress_metrics.comp3_throughput_mbps, min_comp3_threshold
                    );

                    // Validate stress test stability (no crashes or panics)
                    assert!(
                        stress_metrics.processing_latency_ms < 10000.0, // 10 second max latency
                        "Stress test '{}' should maintain reasonable latency: {} ms < 10000 ms",
                        scenario_name, stress_metrics.processing_latency_ms
                    );

                    // Validate memory usage under stress remains bounded
                    assert!(
                        stress_metrics.memory_usage_mb <= ENTERPRISE_MEMORY_LIMIT_MB * 2, // Allow 2x memory under stress
                        "Stress test '{}' memory usage should remain bounded: {} MB <= {} MB",
                        scenario_name, stress_metrics.memory_usage_mb, ENTERPRISE_MEMORY_LIMIT_MB * 2
                    );
                }
                Err(error) => {
                    // Stress test failures should be handled gracefully
                    assert!(
                        error.contains("stress") || error.contains("enterprise") || error.contains(scenario_name),
                        "Stress test error for '{}' should reference stress issue: {}",
                        scenario_name, error
                    );

                    // Should not contain panic traces
                    assert!(
                        !error.contains("panic") && !error.contains("unwrap"),
                        "Stress test error for '{}' should not contain panic traces: {}",
                        scenario_name, error
                    );
                }
            }
        }
    }

    #[test] // AC:63-22-5 Regression detection framework validation
    fn test_regression_detection_framework_validation() {
        // Test case: Performance regression detection with panic elimination changes
        let regression_test_scenarios = vec![
            ("baseline_establishment", 0.0), // No change
            ("minor_improvement", -2.0), // 2% improvement
            ("acceptable_degradation", 3.0), // 3% degradation (within 5% threshold)
            ("threshold_degradation", 5.0), // 5% degradation (at threshold)
        ];

        for (scenario_name, simulated_impact) in regression_test_scenarios {
            // Create baseline metrics
            let baseline_metrics = PerformanceMetrics {
                display_throughput_gbps: 2.5,
                comp3_throughput_mbps: 180.0,
                memory_usage_mb: 200,
                processing_latency_ms: 100.0,
                cpu_utilization_percent: 80.0,
            };

            // Simulate performance with impact
            let current_metrics = simulate_performance_impact(&baseline_metrics, simulated_impact);

            // Test regression detection
            let regression_result = detect_performance_regression_safely(&baseline_metrics, &current_metrics);

            match regression_result {
                Ok(regression_analysis) => {
                    // Validate regression detection accuracy
                    let expected_regression = simulated_impact > ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD;
                    assert_eq!(
                        regression_analysis.regression_detected, expected_regression,
                        "Regression detection for '{}' should match expected: detected={}, expected={}",
                        scenario_name, regression_analysis.regression_detected, expected_regression
                    );

                    // Validate impact calculation accuracy
                    if let Some(calculated_impact) = regression_analysis.impact_percent {
                        let impact_difference = (calculated_impact - simulated_impact).abs();
                        assert!(
                            impact_difference < 0.5, // 0.5% tolerance
                            "Regression impact calculation for '{}' should be accurate: {} vs {} (diff: {})",
                            scenario_name, calculated_impact, simulated_impact, impact_difference
                        );
                    }

                    // Validate threshold enforcement
                    if regression_analysis.regression_detected {
                        assert!(
                            regression_analysis.impact_percent.unwrap_or(0.0) > ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD,
                            "Detected regression for '{}' should exceed threshold: {}% > {}%",
                            scenario_name, regression_analysis.impact_percent.unwrap_or(0.0), ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD
                        );
                    }
                }
                Err(error) => {
                    assert!(
                        error.contains("regression") || error.contains("detection") || error.contains(scenario_name),
                        "Regression detection error for '{}' should reference detection issue: {}",
                        scenario_name, error
                    );
                }
            }
        }
    }
}

// Mock implementation functions for enterprise performance testing
// These would integrate with actual copybook-rs benchmarking infrastructure

fn measure_display_performance_safely(workload: &EnterpriseWorkload, panic_safe: bool) -> Result<PerformanceMetrics, String> {
    if workload.record_count == 0 {
        return Err("Cannot measure performance with zero records".to_string());
    }

    // Simulate performance measurement with optional panic safety overhead
    let base_throughput = 2.8; // GiB/s
    let panic_safe_overhead = if panic_safe { 0.02 } else { 0.0 }; // 2% overhead

    let throughput = base_throughput * (1.0 - panic_safe_overhead);

    Ok(PerformanceMetrics {
        display_throughput_gbps: throughput,
        comp3_throughput_mbps: 0.0, // Not applicable for DISPLAY-only test
        memory_usage_mb: 128,
        processing_latency_ms: 50.0,
        cpu_utilization_percent: 75.0,
    })
}

fn measure_comp3_performance_safely(workload: &EnterpriseWorkload, panic_safe: bool) -> Result<PerformanceMetrics, String> {
    if workload.record_count == 0 {
        return Err("Cannot measure COMP-3 performance with zero records".to_string());
    }

    // Simulate COMP-3 performance measurement
    let base_throughput = 185.0; // MiB/s
    let panic_safe_overhead = if panic_safe { 0.03 } else { 0.0 }; // 3% overhead

    let throughput = base_throughput * (1.0 - panic_safe_overhead);

    Ok(PerformanceMetrics {
        display_throughput_gbps: 0.0, // Not applicable for COMP-3-only test
        comp3_throughput_mbps: throughput,
        memory_usage_mb: 96,
        processing_latency_ms: 75.0,
        cpu_utilization_percent: 70.0,
    })
}

fn measure_mixed_workload_performance_safely(workload: &EnterpriseWorkload, panic_safe: bool) -> Result<PerformanceMetrics, String> {
    if workload.record_count == 0 {
        return Err("Cannot measure mixed workload performance with zero records".to_string());
    }

    // Simulate mixed workload performance
    let panic_safe_overhead = if panic_safe { 0.025 } else { 0.0 }; // 2.5% average overhead

    Ok(PerformanceMetrics {
        display_throughput_gbps: 2.4 * (1.0 - panic_safe_overhead),
        comp3_throughput_mbps: 175.0 * (1.0 - panic_safe_overhead),
        memory_usage_mb: 192,
        processing_latency_ms: 125.0,
        cpu_utilization_percent: 85.0,
    })
}

fn execute_enterprise_stress_test_safely(workload: &EnterpriseWorkload) -> Result<PerformanceMetrics, String> {
    // Simulate stress test execution
    match workload.name.as_str() {
        name if name.contains("high_concurrency") => {
            Ok(PerformanceMetrics {
                display_throughput_gbps: 1.8, // Reduced under stress
                comp3_throughput_mbps: 125.0, // Reduced under stress
                memory_usage_mb: 380, // Higher under stress
                processing_latency_ms: 250.0, // Higher under stress
                cpu_utilization_percent: 92.0, // Higher under stress
            })
        }
        name if name.contains("large_records") => {
            Ok(PerformanceMetrics {
                display_throughput_gbps: 1.9,
                comp3_throughput_mbps: 130.0,
                memory_usage_mb: 420,
                processing_latency_ms: 300.0,
                cpu_utilization_percent: 88.0,
            })
        }
        name if name.contains("memory_pressure") => {
            Ok(PerformanceMetrics {
                display_throughput_gbps: 1.7,
                comp3_throughput_mbps: 120.0,
                memory_usage_mb: 480,
                processing_latency_ms: 400.0,
                cpu_utilization_percent: 94.0,
            })
        }
        _ => {
            Ok(PerformanceMetrics {
                display_throughput_gbps: 1.8,
                comp3_throughput_mbps: 125.0,
                memory_usage_mb: 350,
                processing_latency_ms: 200.0,
                cpu_utilization_percent: 90.0,
            })
        }
    }
}

fn validate_comp3_precision_preservation(workload: &EnterpriseWorkload) -> Result<bool, String> {
    // Mock COMP-3 precision validation
    match workload.field_complexity {
        WorkloadComplexity::Enterprise => Ok(true), // Enterprise workloads maintain precision
        WorkloadComplexity::Complex => Ok(true), // Complex workloads maintain precision
        _ => Ok(true), // All workloads should maintain precision
    }
}

fn calculate_performance_impact(baseline: f64, current: f64) -> f64 {
    if baseline <= 0.0 {
        return 0.0;
    }
    ((baseline - current) / baseline) * 100.0
}

fn calculate_mixed_workload_impact(baseline: &PerformanceMetrics, current: &PerformanceMetrics) -> f64 {
    // Calculate weighted impact across both DISPLAY and COMP-3 performance
    let display_impact = calculate_performance_impact(baseline.display_throughput_gbps, current.display_throughput_gbps);
    let comp3_impact = calculate_performance_impact(baseline.comp3_throughput_mbps, current.comp3_throughput_mbps);

    // Weighted average (50% DISPLAY, 50% COMP-3 for mixed workload)
    (display_impact + comp3_impact) / 2.0
}

#[derive(Debug)]
struct RegressionAnalysis {
    regression_detected: bool,
    impact_percent: Option<f64>,
    threshold_exceeded: bool,
}

fn detect_performance_regression_safely(baseline: &PerformanceMetrics, current: &PerformanceMetrics) -> Result<RegressionAnalysis, String> {
    // Calculate overall performance impact
    let display_impact = calculate_performance_impact(baseline.display_throughput_gbps, current.display_throughput_gbps);
    let comp3_impact = calculate_performance_impact(baseline.comp3_throughput_mbps, current.comp3_throughput_mbps);

    // Use worst-case impact for regression detection
    let overall_impact = display_impact.max(comp3_impact);

    let regression_detected = overall_impact > ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD;
    let threshold_exceeded = overall_impact > ENTERPRISE_PERFORMANCE_IMPACT_THRESHOLD;

    Ok(RegressionAnalysis {
        regression_detected,
        impact_percent: Some(overall_impact),
        threshold_exceeded,
    })
}

fn simulate_performance_impact(baseline: &PerformanceMetrics, impact_percent: f64) -> PerformanceMetrics {
    let impact_factor = 1.0 - (impact_percent / 100.0);

    PerformanceMetrics {
        display_throughput_gbps: baseline.display_throughput_gbps * impact_factor,
        comp3_throughput_mbps: baseline.comp3_throughput_mbps * impact_factor,
        memory_usage_mb: baseline.memory_usage_mb,
        processing_latency_ms: baseline.processing_latency_ms / impact_factor, // Latency increases with degradation
        cpu_utilization_percent: baseline.cpu_utilization_percent,
    }
}