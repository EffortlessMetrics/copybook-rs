//! AC1: Regression Detection Validation Tests
//!
//! Tests validation of existing regression detection functionality from Issue #52.
//!
//! **Status**: Validates existing implementation (`baseline.rs:check_regression`)
//!
//! Specification: docs/issue-49-tdd-handoff-package.md#ac1-regression-detection
//! Traceability: docs/issue-49-traceability-matrix.md#ac1

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::bool_to_int_with_if, clippy::doc_markdown)]

use copybook_bench::baseline::BaselineStore;
use copybook_bench::reporting::PerformanceReport;

/// AC1: Test PASS status with no performance change
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that identical performance returns empty regression list (PASS).
#[test]
fn test_regression_pass_no_change() {
    // AC1
    let mut store = BaselineStore::new();

    // Create baseline
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(2.50);
    baseline.comp3_mibs = Some(172.0);
    baseline.commit = "baseline-commit".to_string();
    store.promote_baseline(&baseline, "main", "baseline-commit");

    // Test identical performance
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(2.50); // 0% change
    current.comp3_mibs = Some(172.0); // 0% change
    current.commit = "current-commit".to_string();

    let regressions = store.check_regression(&current, 5.0);
    assert!(
        regressions.is_empty(),
        "Expected no regressions for identical performance"
    );

    // TODO: Validate exit code (0)
    // TODO: Test with small variations (<1%)
}

/// AC1: Test WARNING status with 5-10% regression
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that 5-10% performance degradation triggers WARNING status
/// but does not fail CI (exit code 0).
#[test]
fn test_regression_warning_threshold() {
    // AC1
    let mut store = BaselineStore::new();

    // Create baseline
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline-commit");

    // Test WARNING threshold (7% regression)
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(93.0); // 7% regression (WARNING)
    current.comp3_mibs = Some(100.0);

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(regressions.len(), 1, "Expected 1 regression warning");
    assert!(
        regressions[0].contains("DISPLAY regression"),
        "Expected DISPLAY regression message"
    );
    assert!(
        regressions[0].contains("7.00%"),
        "Expected 7% regression percentage"
    );

    // TODO: Validate exit code (0 - WARNING does not fail CI)
    // TODO: Test both DISPLAY and COMP-3 warnings simultaneously
}

/// AC1: Test FAILURE status with >10% regression
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that >10% performance degradation triggers FAILURE status
/// and fails CI (exit code 1).
#[test]
fn test_regression_failure_threshold() {
    // AC1
    let mut store = BaselineStore::new();

    // Create baseline
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline-commit");

    // Test FAILURE threshold (15% regression)
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(85.0); // 15% regression (FAILURE)
    current.comp3_mibs = Some(100.0);

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(regressions.len(), 1, "Expected 1 regression failure");
    assert!(
        regressions[0].contains("DISPLAY regression"),
        "Expected DISPLAY regression message"
    );
    assert!(
        regressions[0].contains("15.00%"),
        "Expected 15% regression percentage"
    );

    // TODO: Validate exit code (1 - FAILURE fails CI)
    // TODO: Test PR blocking behavior
}

/// AC1: Test NEUTRAL status with missing baseline
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that missing baseline returns empty regression list (NEUTRAL)
/// and does not fail CI (first-time PRs).
#[test]
fn test_missing_baseline_neutral() {
    // AC1
    let store = BaselineStore::new(); // Empty store, no baseline

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(2.50);
    current.comp3_mibs = Some(172.0);

    let regressions = store.check_regression(&current, 5.0);
    assert!(
        regressions.is_empty(),
        "Expected no regressions for missing baseline (NEUTRAL)"
    );

    // TODO: Validate exit code (0 - NEUTRAL does not fail CI)
    // TODO: Test PR comment generation for NEUTRAL status
}

/// AC1: Test regression calculation accuracy
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates regression percentage calculation formula:
/// regression_pct = (baseline - current) / baseline * 100.0
#[test]
fn test_regression_calculation_accuracy() {
    // AC1
    let mut store = BaselineStore::new();

    // Create baseline with known values
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(200.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test exact 10% regression
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(180.0); // Exactly 10% slower
    current.comp3_mibs = Some(90.0); // Exactly 10% slower

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(
        regressions.len(),
        2,
        "Expected 2 regressions (DISPLAY + COMP-3)"
    );

    // Validate DISPLAY regression message
    assert!(
        regressions[0].contains("10.00%"),
        "Expected exact 10.00% regression"
    );

    // Validate COMP-3 regression message
    assert!(
        regressions[1].contains("10.00%"),
        "Expected exact 10.00% regression"
    );

    // TODO: Test edge cases (0.01% regression)
    // TODO: Validate floating-point precision
}

/// AC1: Test threshold boundary conditions
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates behavior at exact threshold boundaries (5.0%, 10.0%).
#[test]
fn test_threshold_boundary_conditions() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test exactly 5% regression (boundary)
    let mut at_boundary = PerformanceReport::new();
    at_boundary.display_gibs = Some(95.0); // Exactly 5%

    let regressions = store.check_regression(&at_boundary, 5.0);
    assert!(
        regressions.is_empty(),
        "Expected no regression at exactly 5% threshold"
    );

    // Test just over 5% regression
    let mut over_boundary = PerformanceReport::new();
    over_boundary.display_gibs = Some(94.9); // 5.1% regression

    let regressions = store.check_regression(&over_boundary, 5.0);
    assert_eq!(
        regressions.len(),
        1,
        "Expected regression just over 5% threshold"
    );

    // TODO: Test 10% boundary
    // TODO: Test floating-point precision at boundaries
}

/// AC1: Test regression message formatting
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that regression messages contain all required information:
/// - Metric name (DISPLAY/COMP-3)
/// - Regression percentage
/// - Current value
/// - Baseline value
/// - Units (GiB/s / MiB/s)
#[test]
fn test_regression_message_formatting() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "baseline");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(85.0);
    current.comp3_mibs = Some(400.0);

    let regressions = store.check_regression(&current, 5.0);

    // Validate DISPLAY message format
    let display_msg = &regressions[0];
    assert!(
        display_msg.contains("DISPLAY regression"),
        "Message must contain metric name"
    );
    assert!(
        display_msg.contains("15.00%"),
        "Message must contain percentage"
    );
    assert!(
        display_msg.contains("85.00"),
        "Message must contain current value"
    );
    assert!(
        display_msg.contains("100.00"),
        "Message must contain baseline value"
    );
    assert!(display_msg.contains("GiB/s"), "Message must contain units");

    // Validate COMP-3 message format
    let comp3_msg = &regressions[1];
    assert!(
        comp3_msg.contains("COMP-3 regression"),
        "Message must contain metric name"
    );
    assert!(
        comp3_msg.contains("20.00%"),
        "Message must contain percentage"
    );
    assert!(
        comp3_msg.contains("400"),
        "Message must contain current value"
    );
    assert!(
        comp3_msg.contains("500"),
        "Message must contain baseline value"
    );
    assert!(comp3_msg.contains("MiB/s"), "Message must contain units");

    // TODO: Test message format consistency
    // TODO: Validate PR comment formatting
}

/// AC1: Test DISPLAY-only regression
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that only DISPLAY regressions are reported when COMP-3 is stable.
#[test]
fn test_display_only_regression() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(85.0); // 15% regression
    current.comp3_mibs = Some(100.0); // No change

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(regressions.len(), 1, "Expected only DISPLAY regression");
    assert!(
        regressions[0].contains("DISPLAY"),
        "Expected DISPLAY regression"
    );

    // TODO: Test COMP-3-only regression
    // TODO: Test both metrics regressing
}

/// AC1: Test COMP-3-only regression
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that only COMP-3 regressions are reported when DISPLAY is stable.
#[test]
fn test_comp3_only_regression() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(100.0); // No change
    current.comp3_mibs = Some(80.0); // 20% regression

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(regressions.len(), 1, "Expected only COMP-3 regression");
    assert!(
        regressions[0].contains("COMP-3"),
        "Expected COMP-3 regression"
    );

    // TODO: Test both metrics regressing simultaneously
}

/// AC1: Test missing performance metrics
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates behavior when performance metrics are missing (None).
#[test]
fn test_missing_performance_metrics() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test with missing DISPLAY metric
    let mut missing_display = PerformanceReport::new();
    missing_display.display_gibs = None;
    missing_display.comp3_mibs = Some(100.0);

    let regressions = store.check_regression(&missing_display, 5.0);
    assert!(
        regressions.is_empty() || !regressions[0].contains("DISPLAY"),
        "Expected no DISPLAY regression when metric is missing"
    );

    // TODO: Test missing COMP-3 metric
    // TODO: Test both metrics missing
    // TODO: Validate error handling for missing metrics
}

/// AC1: Test performance improvement (negative regression)
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that performance improvements do not trigger regressions.
#[test]
fn test_performance_improvement() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test improvement
    let mut improved = PerformanceReport::new();
    improved.display_gibs = Some(110.0); // 10% improvement
    improved.comp3_mibs = Some(120.0); // 20% improvement

    let regressions = store.check_regression(&improved, 5.0);
    assert!(
        regressions.is_empty(),
        "Performance improvements should not trigger regressions"
    );

    // TODO: Test large improvements (>50%)
    // TODO: Validate promotion of improved baselines
}

/// AC1: Test WARNING threshold at 5.5% (specific example from task)
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that 5.5% regression triggers WARNING status.
#[test]
fn test_warning_threshold_5_percent() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test 5.5% regression (just over 5% threshold)
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(94.5); // 5.5% regression
    current.comp3_mibs = Some(100.0);

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(regressions.len(), 1, "Expected 1 WARNING at 5.5%");
    assert!(
        regressions[0].contains("DISPLAY regression"),
        "Expected DISPLAY regression"
    );
    assert!(
        regressions[0].contains("5.50%"),
        "Expected 5.50% regression percentage"
    );
}

/// AC1: Test FAILURE threshold at 12% (specific example from task)
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that 12% regression triggers FAILURE status.
#[test]
fn test_failure_threshold_10_percent() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test 12% regression (exceeds 10% FAILURE threshold)
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(88.0); // 12% regression
    current.comp3_mibs = Some(100.0);

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(regressions.len(), 1, "Expected 1 FAILURE at 12%");
    assert!(
        regressions[0].contains("DISPLAY regression"),
        "Expected DISPLAY regression"
    );
    assert!(
        regressions[0].contains("12.00%"),
        "Expected 12.00% regression percentage"
    );
}

/// AC1: Test multiple metric regression (both DISPLAY and COMP-3)
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates behavior when both metrics regress simultaneously.
#[test]
fn test_multiple_metric_regression() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test both metrics regressing
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(93.0); // 7% WARNING
    current.comp3_mibs = Some(88.0); // 12% FAILURE

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(
        regressions.len(),
        2,
        "Expected 2 regressions (DISPLAY + COMP-3)"
    );

    // Verify DISPLAY warning
    assert!(
        regressions[0].contains("DISPLAY regression"),
        "Expected DISPLAY regression first"
    );
    assert!(
        regressions[0].contains("7.00%"),
        "Expected 7% DISPLAY regression"
    );

    // Verify COMP-3 failure
    assert!(
        regressions[1].contains("COMP-3 regression"),
        "Expected COMP-3 regression second"
    );
    assert!(
        regressions[1].contains("12.00%"),
        "Expected 12% COMP-3 regression"
    );
}

/// AC1: Test zero baseline protection
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that zero baseline values do not cause division by zero.
#[test]
fn test_zero_baseline_protection() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(0.0); // Zero baseline
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(100.0);
    current.comp3_mibs = Some(100.0);

    // Should not panic or fail
    let _regressions = store.check_regression(&current, 5.0);
    // May contain regression (infinite %) but should not crash
    // This is a safety test - exact behavior with zero baseline is implementation-defined
    // Test passes if we reach this point without panic
}

/// AC1: Test very small regression (<0.1%)
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that very small regressions do not trigger warnings.
#[test]
fn test_very_small_regression() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test 0.05% regression (well below threshold)
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(99.95); // 0.05% regression
    current.comp3_mibs = Some(99.99); // 0.01% regression

    let regressions = store.check_regression(&current, 5.0);
    assert!(
        regressions.is_empty(),
        "Very small regressions should not trigger warnings"
    );
}

/// AC1: Test very large regression (>50%)
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates that very large regressions are properly detected and reported.
#[test]
fn test_very_large_regression() {
    // AC1
    let mut store = BaselineStore::new();

    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    baseline.comp3_mibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "baseline");

    // Test 75% regression
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(25.0); // 75% regression
    current.comp3_mibs = Some(100.0);

    let regressions = store.check_regression(&current, 5.0);
    assert_eq!(regressions.len(), 1, "Expected 1 regression");
    assert!(
        regressions[0].contains("75.00%"),
        "Expected 75% regression percentage"
    );
}

/// AC1: Test enterprise performance context (from AC2 baseline)
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#regression-detection-algorithm
///
/// Validates regression detection using actual enterprise baseline values:
/// - DISPLAY: 2.33 GiB/s baseline (from AC2)
/// - COMP-3: 172 MiB/s baseline (from AC2)
/// - 5% threshold: DISPLAY 2.21 GiB/s, COMP-3 163 MiB/s
/// - 10% threshold: DISPLAY 2.10 GiB/s, COMP-3 155 MiB/s
#[test]
fn test_enterprise_performance_context() {
    // AC1
    let mut store = BaselineStore::new();

    // Use AC2 canonical baseline from baseline_reference.json
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(2.33); // GiB/s from AC2
    baseline.comp3_mibs = Some(172.0); // MiB/s from AC2
    store.promote_baseline(&baseline, "main", "dcb9885");

    // Test at 5% threshold boundary (WARNING) - should trigger regression
    let mut at_warning = PerformanceReport::new();
    at_warning.display_gibs = Some(2.21); // ~5.15% regression
    at_warning.comp3_mibs = Some(163.0); // ~5.23% regression

    let regressions = store.check_regression(&at_warning, 5.0);
    assert_eq!(
        regressions.len(),
        2,
        "Expected both metrics to trigger at 5% threshold boundary: {regressions:?}"
    );

    // Test at 10% threshold boundary (FAILURE)
    let mut at_failure = PerformanceReport::new();
    at_failure.display_gibs = Some(2.05); // ~12.02% regression
    at_failure.comp3_mibs = Some(150.0); // ~12.79% regression

    let regressions = store.check_regression(&at_failure, 5.0);
    assert_eq!(
        regressions.len(),
        2,
        "Expected both metrics to trigger at 10% threshold"
    );

    // Verify these match the fixture data
    assert!(
        regressions[0].contains("12.") || regressions[0].contains("12.0"),
        "Expected ~12% DISPLAY regression"
    );
    assert!(
        regressions[1].contains("12.") || regressions[1].contains("13."),
        "Expected ~12-13% COMP-3 regression"
    );
}
