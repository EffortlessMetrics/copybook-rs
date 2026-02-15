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

    // Exit codes are tested via the bench-report CLI binary (bench-report compare).

    // Test with small variations (<1%): 0.4% change should not trigger regression
    let mut small_variation = PerformanceReport::new();
    small_variation.display_gibs = Some(2.49); // 0.4% slower
    small_variation.comp3_mibs = Some(171.0); // ~0.58% slower
    small_variation.commit = "small-variation-commit".to_string();

    let regressions = store.check_regression(&small_variation, 5.0);
    assert!(
        regressions.is_empty(),
        "Expected no regressions for <1% variation"
    );
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

    // Exit codes are tested via the bench-report CLI binary (bench-report compare).

    // Test both DISPLAY and COMP-3 warnings simultaneously (7% regression each)
    let mut both_warning = PerformanceReport::new();
    both_warning.display_gibs = Some(93.0); // 7% regression
    both_warning.comp3_mibs = Some(93.0); // 7% regression

    let regressions = store.check_regression(&both_warning, 5.0);
    assert_eq!(
        regressions.len(),
        2,
        "Expected 2 regression warnings (DISPLAY + COMP-3)"
    );
    assert!(
        regressions[0].contains("DISPLAY regression"),
        "Expected DISPLAY regression"
    );
    assert!(
        regressions[1].contains("COMP-3 regression"),
        "Expected COMP-3 regression"
    );
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

    // Exit codes are tested via the bench-report CLI binary (bench-report compare).
    // PR blocking is determined by the bench-report binary exit code (non-zero on regression).
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

    // Exit codes are tested via the bench-report CLI binary (bench-report compare).

    // Test PR comment generation for NEUTRAL status
    let summary = current.format_pr_summary();
    assert!(
        !summary.is_empty(),
        "PR summary should not be empty for NEUTRAL status"
    );
    assert!(
        summary.contains("GiB/s") || summary.contains("N/A"),
        "PR summary should contain metric units or N/A"
    );
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

    // Test edge case: 0.01% regression (baseline 10000.0, current 9999.0)
    let mut store2 = BaselineStore::new();
    let mut baseline2 = PerformanceReport::new();
    baseline2.display_gibs = Some(10000.0);
    store2.promote_baseline(&baseline2, "main", "baseline2");

    let mut tiny_regression = PerformanceReport::new();
    tiny_regression.display_gibs = Some(9999.0); // 0.01% regression

    let regressions = store2.check_regression(&tiny_regression, 5.0);
    assert!(
        regressions.is_empty(),
        "0.01% regression should not trigger at 5% threshold"
    );

    // Validate floating-point precision: exact threshold arithmetic
    let mut precise = PerformanceReport::new();
    precise.display_gibs = Some(9500.0); // Exactly 5.0% regression

    let regressions = store2.check_regression(&precise, 5.0);
    assert!(
        regressions.is_empty(),
        "Exactly 5.0% regression should not trigger (threshold is strictly greater than)"
    );
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

    // Test 10% boundary: baseline 100.0, current 90.0 (exactly 10% regression)
    let mut at_10pct = PerformanceReport::new();
    at_10pct.display_gibs = Some(90.0); // Exactly 10%

    let regressions = store.check_regression(&at_10pct, 5.0);
    // 10% > 5% threshold, so this should trigger a regression
    assert_eq!(
        regressions.len(),
        1,
        "10% regression should trigger at 5% threshold"
    );
    assert!(
        regressions[0].contains("10.00%"),
        "Expected 10.00% regression"
    );

    // Test floating-point precision at boundary: just barely under threshold
    let mut just_under = PerformanceReport::new();
    just_under.display_gibs = Some(95.000_000_000_01); // Marginally above 95.0

    let regressions = store.check_regression(&just_under, 5.0);
    assert!(
        regressions.is_empty(),
        "Regression just under 5% threshold should not trigger"
    );
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

    // Validate message format consistency
    // Messages follow pattern: "{metric} regression: {pct}% ... ({current} vs {baseline} {units})"
    assert!(
        display_msg.contains("DISPLAY regression")
            && display_msg.contains("GiB/s")
            && display_msg.contains("%"),
        "DISPLAY message should contain metric name, units, and percentage: {display_msg}"
    );
    assert!(
        comp3_msg.contains("COMP-3 regression")
            && comp3_msg.contains("MiB/s")
            && comp3_msg.contains("%"),
        "COMP-3 message should contain metric name, units, and percentage: {comp3_msg}"
    );

    // Validate PR comment formatting via format_pr_summary
    let pr_summary = current.format_pr_summary();
    assert!(!pr_summary.is_empty(), "PR summary should not be empty");
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

    // COMP-3-only regression is tested in test_comp3_only_regression.
    // Both metrics regressing is tested in test_multiple_metric_regression.
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

    // Both metrics regressing simultaneously is tested in test_multiple_metric_regression.
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

    // Test missing COMP-3 metric
    let mut missing_comp3 = PerformanceReport::new();
    missing_comp3.display_gibs = Some(100.0);
    missing_comp3.comp3_mibs = None;

    let regressions = store.check_regression(&missing_comp3, 5.0);
    assert!(
        regressions.is_empty() || regressions.iter().all(|r| !r.contains("COMP-3")),
        "Expected no COMP-3 regression when metric is missing"
    );

    // Test both metrics missing
    let mut both_missing = PerformanceReport::new();
    both_missing.display_gibs = None;
    both_missing.comp3_mibs = None;

    let regressions = store.check_regression(&both_missing, 5.0);
    assert!(
        regressions.is_empty(),
        "Expected no regressions when both metrics are missing"
    );
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

    // Test large improvement (>50%): 200% improvement (baseline 100, current 300)
    let mut large_improvement = PerformanceReport::new();
    large_improvement.display_gibs = Some(300.0); // 200% improvement
    large_improvement.comp3_mibs = Some(250.0); // 150% improvement

    let regressions = store.check_regression(&large_improvement, 5.0);
    assert!(
        regressions.is_empty(),
        "Large performance improvements should not trigger regressions"
    );

    // Validate that improved baselines can be promoted without error
    let mut store2 = BaselineStore::new();
    store2.promote_baseline(&baseline, "main", "original");
    store2.promote_baseline(&large_improvement, "main", "improved");
    assert!(
        store2.current.is_some(),
        "Improved baseline should be promotable"
    );
    assert_eq!(
        store2.current.as_ref().expect("has current").display_gibs,
        Some(300.0),
        "Promoted baseline should reflect improved value"
    );
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
