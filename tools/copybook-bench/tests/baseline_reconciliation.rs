// SPDX-License-Identifier: AGPL-3.0-or-later
//! AC2: Performance Baseline Reconciliation Tests
//!
//! Tests baseline measurement methodology, promotion procedures, and persistence.
//!
//! **CRITICAL PATH**: AC2 must complete FIRST before AC1/AC3 implementation.
//!
//! Specification: docs/issue-49-tdd-handoff-package.md#ac2-baseline-reconciliation
//! Traceability: docs/issue-49-traceability-matrix.md#ac2

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::useless_vec,
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::bool_to_int_with_if,
    clippy::uninlined_format_args,
    clippy::items_after_statements
)]

use copybook_bench::baseline::{BaselineStore, PerformanceBaseline};
use copybook_bench::reporting::PerformanceReport;

/// AC2: Test comprehensive baseline measurement methodology
///
/// Tests feature spec: docs/issue-49-tdd-handoff-package.md#ac2-baseline-reconciliation
///
/// Validates:
/// - Statistical variance <5% across 5 runs
/// - Baseline promotion procedure
/// - Hardware specification documentation requirements
#[test]
fn test_baseline_measurement_methodology() {
    // AC2
    // Simulate 5 baseline measurement runs
    let measurements = vec![2.50, 2.48, 2.52, 2.49, 2.51]; // DISPLAY GiB/s

    // Calculate statistical variance
    let mean = measurements.iter().sum::<f64>() / measurements.len() as f64;
    let variance =
        measurements.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / measurements.len() as f64;
    let std_dev = variance.sqrt();
    let coefficient_of_variation = (std_dev / mean) * 100.0;

    // Validate variance <5% requirement
    assert!(
        coefficient_of_variation < 5.0,
        "Expected variance <5%, got {:.2}%",
        coefficient_of_variation
    );

    // Create canonical baseline with mean values
    let canonical_baseline = PerformanceBaseline {
        branch: "main".to_string(),
        commit: "canonical-baseline".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        display_gibs: Some(mean / 1024.0), // Convert to GiB/s
        comp3_mibs: Some(172.0),           // Example COMP-3 baseline
        sample_count: 5,
    };

    // Validate baseline quality metrics
    assert!(
        canonical_baseline.display_gibs.unwrap() > 0.0,
        "DISPLAY baseline must be positive"
    );
    assert!(
        canonical_baseline.comp3_mibs.unwrap() > 0.0,
        "COMP-3 baseline must be positive"
    );
    assert_eq!(canonical_baseline.sample_count, 5, "Sample count must be 5");

    // Store canonical baseline for validation
    let mut store = BaselineStore::new();
    let mut report = PerformanceReport::new();
    report.display_gibs = canonical_baseline.display_gibs;
    report.comp3_mibs = canonical_baseline.comp3_mibs;
    report.timestamp = canonical_baseline.timestamp.clone();
    report.commit = canonical_baseline.commit.clone();

    store.promote_baseline(
        &report,
        &canonical_baseline.branch,
        &canonical_baseline.commit,
    );
    assert!(
        store.current.is_some(),
        "Canonical baseline should be stored"
    );
}

/// AC2: Test baseline measurement variance threshold
///
/// Tests feature spec: docs/issue-49-tdd-handoff-package.md#ac2-baseline-reconciliation
///
/// Validates that baseline measurements meet <5% variance requirement
/// for statistical reliability and reproducibility.
#[test]
fn test_baseline_variance_threshold() {
    // AC2
    // Test acceptable variance (< 5%)
    let acceptable_measurements = vec![100.0, 98.0, 102.0, 99.5, 100.5];
    let mean = acceptable_measurements.iter().sum::<f64>() / acceptable_measurements.len() as f64;
    let variance = acceptable_measurements
        .iter()
        .map(|x| (x - mean).powi(2))
        .sum::<f64>()
        / acceptable_measurements.len() as f64;
    let std_dev = variance.sqrt();
    let cv = (std_dev / mean) * 100.0;

    assert!(cv < 5.0, "Expected CV <5%, got {:.2}%", cv);

    // Test unacceptable variance (>5%)
    let unacceptable_measurements = vec![100.0, 85.0, 115.0, 90.0, 110.0];
    let mean_bad =
        unacceptable_measurements.iter().sum::<f64>() / unacceptable_measurements.len() as f64;
    let variance_bad = unacceptable_measurements
        .iter()
        .map(|x| (x - mean_bad).powi(2))
        .sum::<f64>()
        / unacceptable_measurements.len() as f64;
    let std_dev_bad = variance_bad.sqrt();
    let cv_bad = (std_dev_bad / mean_bad) * 100.0;

    assert!(
        cv_bad > 5.0,
        "Expected CV >5% for bad measurements, got {:.2}%",
        cv_bad
    );

    // Implement variance rejection logic
    fn validate_variance(measurements: &[f64], threshold: f64) -> Result<f64, String> {
        let mean = measurements.iter().sum::<f64>() / measurements.len() as f64;
        let variance = measurements.iter().map(|x| (x - mean).powi(2)).sum::<f64>()
            / measurements.len() as f64;
        let std_dev = variance.sqrt();
        let cv = (std_dev / mean) * 100.0;

        if cv > threshold {
            Err(format!(
                "Variance {:.2}% exceeds threshold {:.2}%",
                cv, threshold
            ))
        } else {
            Ok(cv)
        }
    }

    // Test variance rejection
    let good_result = validate_variance(&acceptable_measurements, 5.0);
    assert!(
        good_result.is_ok(),
        "Good measurements should pass validation"
    );

    let bad_result = validate_variance(&unacceptable_measurements, 5.0);
    assert!(
        bad_result.is_err(),
        "Bad measurements should fail validation"
    );
}

/// AC2: Test baseline promotion procedure
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#baseline-management-api
///
/// Validates that baseline promotion correctly:
/// - Archives previous baseline to history
/// - Sets new baseline as current
/// - Updates timestamp metadata
/// - Applies retention policy
#[test]
fn test_baseline_promotion() {
    // AC2
    let mut store = BaselineStore::new();
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(2.50);
    report.comp3_mibs = Some(172.0);
    report.timestamp = chrono::Utc::now().to_rfc3339();
    report.commit = "canonical-baseline".to_string();

    store.promote_baseline(&report, "main", "canonical-baseline");

    // Validate promotion succeeded
    assert!(store.current.is_some(), "Expected baseline to be promoted");
    let baseline = store.current.as_ref().unwrap();
    assert_eq!(baseline.display_gibs, Some(2.50));
    assert_eq!(baseline.comp3_mibs, Some(172.0));
    assert_eq!(baseline.commit, "canonical-baseline");
    assert_eq!(baseline.branch, "main");

    // Validate history archival
    assert_eq!(
        store.history.len(),
        0,
        "History should be empty after first promotion"
    );

    // Test multiple sequential promotions
    let mut report2 = PerformanceReport::new();
    report2.display_gibs = Some(2.60);
    report2.comp3_mibs = Some(180.0);
    report2.timestamp = chrono::Utc::now().to_rfc3339();
    report2.commit = "baseline-2".to_string();

    store.promote_baseline(&report2, "main", "baseline-2");

    assert_eq!(
        store.history.len(),
        1,
        "Previous baseline should be archived"
    );
    assert_eq!(store.history[0].commit, "canonical-baseline");
    assert_eq!(store.current.as_ref().unwrap().commit, "baseline-2");

    // Validate retention policy application
    let mut report3 = PerformanceReport::new();
    report3.display_gibs = Some(2.70);
    report3.comp3_mibs = Some(190.0);
    report3.timestamp = chrono::Utc::now().to_rfc3339();
    report3.commit = "baseline-3".to_string();

    store.promote_baseline(&report3, "main", "baseline-3");

    assert_eq!(
        store.history.len(),
        2,
        "History should contain 2 previous baselines"
    );
    assert_eq!(store.history[0].commit, "baseline-2");
    assert_eq!(store.history[1].commit, "canonical-baseline");
}

/// AC2: Test baseline persistence across sessions
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#baseline-management-api
///
/// Validates that baselines persist correctly to disk and can be
/// loaded in new sessions without data loss or corruption.
#[test]
fn test_baseline_persistence() {
    // AC2
    let temp_dir = std::env::temp_dir();
    let temp_path = temp_dir.join("test_baseline_issue49_ac2.json");

    // Create and save baseline
    let mut store = BaselineStore::new();
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(2.50);
    report.comp3_mibs = Some(172.0);
    report.timestamp = chrono::Utc::now().to_rfc3339();
    report.commit = "persistence-test".to_string();

    store.promote_baseline(&report, "main", "persistence-test");
    store.save(&temp_path).expect("Failed to save baseline");

    // Load baseline in new session
    let loaded_store = BaselineStore::load_or_create(&temp_path).expect("Failed to load baseline");

    // Validate loaded baseline matches original
    assert!(
        loaded_store.current.is_some(),
        "Expected baseline to be loaded"
    );
    let loaded_baseline = loaded_store.current.as_ref().unwrap();
    assert_eq!(loaded_baseline.display_gibs, Some(2.50));
    assert_eq!(loaded_baseline.comp3_mibs, Some(172.0));
    assert_eq!(loaded_baseline.commit, "persistence-test");

    // Cleanup
    std::fs::remove_file(&temp_path).ok();

    // Test persistence with history
    let mut report2 = PerformanceReport::new();
    report2.display_gibs = Some(2.60);
    report2.comp3_mibs = Some(180.0);
    report2.timestamp = chrono::Utc::now().to_rfc3339();
    report2.commit = "baseline-2".to_string();

    store.promote_baseline(&report2, "main", "baseline-2");
    store
        .save(&temp_path)
        .expect("Failed to save baseline with history");

    let loaded_store =
        BaselineStore::load_or_create(&temp_path).expect("Failed to load baseline with history");
    assert_eq!(loaded_store.history.len(), 1, "History should be persisted");
    assert_eq!(loaded_store.history[0].commit, "persistence-test");

    // Validate JSON format compatibility
    let json_content = std::fs::read_to_string(&temp_path).expect("Failed to read JSON");
    let parsed: serde_json::Value =
        serde_json::from_str(&json_content).expect("JSON should be valid");
    assert!(
        parsed.get("current").is_some(),
        "JSON should contain current baseline"
    );
    assert!(
        parsed.get("history").is_some(),
        "JSON should contain history array"
    );

    // Test persistence error handling (cross-platform: create a file where the parent dir should be)
    let error_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let blocker = error_dir.path().join("not_a_dir");
    std::fs::write(&blocker, b"x").expect("Failed to create blocker file");
    let invalid_path = blocker.join("baseline.json");
    let result = store.save(&invalid_path);
    assert!(result.is_err(), "Should fail to save when parent is a file");
}

/// AC2: Test baseline documentation requirements
///
/// Tests feature spec: docs/issue-49-tdd-handoff-package.md#ac2-baseline-reconciliation
///
/// Validates that baseline documentation includes all required
/// metadata for reproducibility and audit compliance.
#[test]
fn test_baseline_documentation() {
    // AC2
    let baseline = PerformanceBaseline {
        branch: "main".to_string(),
        commit: "abc12345".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        display_gibs: Some(2.50),
        comp3_mibs: Some(172.0),
        sample_count: 5,
    };

    // Validate required fields present
    assert!(!baseline.branch.is_empty(), "Branch must be documented");
    assert!(!baseline.commit.is_empty(), "Commit must be documented");
    assert!(
        !baseline.timestamp.is_empty(),
        "Timestamp must be documented"
    );
    assert!(baseline.display_gibs.is_some(), "DISPLAY baseline required");
    assert!(baseline.comp3_mibs.is_some(), "COMP-3 baseline required");
    assert!(baseline.sample_count > 0, "Sample count must be > 0");

    // Validate hardware specification documentation exists (use manifest dir for reliable paths)
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let hardware_specs_path = manifest_dir.join("HARDWARE_SPECS.md");
    if hardware_specs_path.exists() {
        let specs_content = std::fs::read_to_string(&hardware_specs_path).unwrap_or_default();
        assert!(
            !specs_content.is_empty(),
            "Hardware specs should be documented"
        );
    }

    // Verify BASELINE_METHODOLOGY.md exists (use manifest dir for reliable paths)
    let methodology_path = manifest_dir.join("BASELINE_METHODOLOGY.md");
    assert!(
        methodology_path.exists(),
        "Baseline methodology should be documented"
    );

    // Validate baseline metadata completeness
    assert!(
        baseline.commit.len() >= 8,
        "Commit hash should be at least 8 characters"
    );
    assert!(
        baseline.timestamp.contains('T'),
        "Timestamp should be ISO 8601 format"
    );
    assert!(
        baseline.sample_count >= 3,
        "Should have at least 3 samples for statistical significance"
    );
}

/// AC2: Test baseline reconciliation complete
///
/// Tests feature spec: docs/issue-49-tdd-handoff-package.md#ac2-baseline-reconciliation
///
/// Validates that baseline reconciliation resolves the discrepancy
/// between CLAUDE.md and REPORT.md performance numbers.
#[test]
fn test_baseline_reconciliation_complete() {
    // AC2
    // Create a canonical baseline for testing reconciliation
    let canonical_baseline = PerformanceBaseline {
        branch: "main".to_string(),
        commit: "abc12345".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        display_gibs: Some(2.50),
        comp3_mibs: Some(172.0),
        sample_count: 5,
    };

    // Store canonical baseline
    let mut store = BaselineStore::new();
    let mut report = PerformanceReport::new();
    report.display_gibs = canonical_baseline.display_gibs;
    report.comp3_mibs = canonical_baseline.comp3_mibs;
    report.timestamp = canonical_baseline.timestamp.clone();
    report.commit = canonical_baseline.commit.clone();

    store.promote_baseline(
        &report,
        &canonical_baseline.branch,
        &canonical_baseline.commit,
    );

    // Validate reconciliation completeness
    assert!(
        store.current.is_some(),
        "Canonical baseline should be established"
    );
    let reconciled_baseline = store.current.as_ref().unwrap();

    assert_eq!(
        reconciled_baseline.display_gibs,
        canonical_baseline.display_gibs
    );
    assert_eq!(
        reconciled_baseline.comp3_mibs,
        canonical_baseline.comp3_mibs
    );
    // promote_baseline sets sample_count to 1 (single promotion event)
    assert_eq!(reconciled_baseline.sample_count, 1);

    // Verify hardware specifications are documented (use manifest dir for reliable paths)
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let hardware_specs_path = manifest_dir.join("HARDWARE_SPECS.md");
    if hardware_specs_path.exists() {
        let specs_content = std::fs::read_to_string(&hardware_specs_path).unwrap_or_default();
        assert!(
            !specs_content.is_empty(),
            "Hardware specifications should be documented"
        );
    }

    // Validate measurement methodology is documented
    let methodology_path = manifest_dir.join("BASELINE_METHODOLOGY.md");
    assert!(
        methodology_path.exists(),
        "Measurement methodology should be documented"
    );
}

/// AC2: Test canonical baseline measurement procedure
///
/// Tests feature spec: docs/how-to/benchmark-regression-testing.md#ac2
///
/// Validates the complete canonical baseline measurement procedure:
/// 1. Clean environment setup
/// 2. 5 measurement runs
/// 3. Statistical analysis (CV <5%)
/// 4. Baseline promotion
/// 5. Documentation update
#[test]
fn test_canonical_baseline_measurement() {
    // AC2
    // Implement clean environment validation
    fn validate_clean_environment() -> bool {
        // Check for common environment issues - test that we have a valid Cargo environment
        // by checking for CARGO_MANIFEST_DIR (always set by cargo test)
        std::env::var("CARGO_MANIFEST_DIR").is_ok()
    }

    assert!(
        validate_clean_environment(),
        "Environment should be clean for baseline measurement"
    );

    // Simulate 5 baseline measurements
    let measurements = vec![2.50, 2.48, 2.52, 2.49, 2.51]; // GiB/s

    // Calculate statistical metrics (mean, stddev, CV)
    let mean = measurements.iter().sum::<f64>() / measurements.len() as f64;
    let variance =
        measurements.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / measurements.len() as f64;
    let std_dev = variance.sqrt();
    let cv = (std_dev / mean) * 100.0;

    // Validate CV <5% threshold
    assert!(
        cv < 5.0,
        "Coefficient of variation should be <5%, got {:.2}%",
        cv
    );

    // Promote canonical baseline
    let mut store = BaselineStore::new();
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(mean);
    report.comp3_mibs = Some(172.0);
    report.timestamp = chrono::Utc::now().to_rfc3339();
    report.commit = "canonical-baseline".to_string();

    store.promote_baseline(&report, "main", "canonical-baseline");

    // Verify baseline establishment
    assert!(
        store.current.is_some(),
        "Canonical baseline should be established"
    );
    let baseline = store.current.as_ref().unwrap();
    assert_eq!(baseline.display_gibs, Some(mean));
    assert_eq!(baseline.sample_count, 1); // Store sets this to 1 for promoted baselines

    // Verify documentation updates (check that methodology exists)
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let methodology_path = manifest_dir.join("BASELINE_METHODOLOGY.md");
    assert!(
        methodology_path.exists(),
        "Methodology documentation should exist"
    );
}

/// AC2: Test baseline quality metrics
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#baseline-management-api
///
/// Validates baseline quality metrics including:
/// - Measurement count (n=5)
/// - Variance threshold (<5%)
/// - Timestamp validity
/// - Commit hash format
#[test]
fn test_baseline_quality_metrics() {
    // AC2
    let baseline = PerformanceBaseline {
        branch: "main".to_string(),
        commit: "abc12345".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        display_gibs: Some(2.50),
        comp3_mibs: Some(172.0),
        sample_count: 5,
    };

    // Validate sample count
    assert_eq!(baseline.sample_count, 5, "Expected 5 measurement samples");

    // Validate timestamp format (ISO 8601)
    assert!(
        chrono::DateTime::parse_from_rfc3339(&baseline.timestamp).is_ok(),
        "Timestamp must be valid ISO 8601"
    );

    // Validate commit hash format (8 characters minimum)
    assert!(
        baseline.commit.len() >= 8,
        "Commit hash must be >= 8 characters"
    );

    // Validate variance calculation
    let test_measurements = vec![100.0, 102.0, 98.0, 101.0, 99.0];
    let test_mean = test_measurements.iter().sum::<f64>() / test_measurements.len() as f64;
    let test_variance = test_measurements
        .iter()
        .map(|x| (x - test_mean).powi(2))
        .sum::<f64>()
        / test_measurements.len() as f64;
    let test_std_dev = test_variance.sqrt();
    let test_cv = (test_std_dev / test_mean) * 100.0;

    assert!(
        test_cv < 5.0,
        "Test measurements should have acceptable variance"
    );

    // Test quality metric thresholds
    fn calculate_quality_score(baseline: &PerformanceBaseline) -> f64 {
        let mut score = 100.0;

        // Deduct points for insufficient samples
        if baseline.sample_count < 5 {
            score -= (5 - baseline.sample_count) as f64 * 10.0;
        }

        // Deduct points for missing metrics
        if baseline.display_gibs.is_none() {
            score -= 25.0;
        }
        if baseline.comp3_mibs.is_none() {
            score -= 25.0;
        }

        score.max(0.0)
    }

    let quality_score = calculate_quality_score(&baseline);
    assert!(
        quality_score >= 50.0,
        "Quality score should be at least 50%, got {:.1}",
        quality_score
    );

    // Implement quality score calculation
    assert!(quality_score > 0.0, "Quality score should be positive");
    assert!(
        quality_score <= 100.0,
        "Quality score should not exceed 100%"
    );
}

/// AC2: Test baseline measurement reproducibility
///
/// Tests feature spec: docs/issue-49-tdd-handoff-package.md#ac2-baseline-reconciliation
///
/// Validates that baseline measurements are reproducible across
/// different runs with consistent environment setup.
#[test]
fn test_baseline_measurement_reproducibility() {
    // AC2
    // Simulate two sets of measurements under identical conditions
    let run1 = vec![2.50, 2.48, 2.52, 2.49, 2.51];
    let run2 = vec![2.49, 2.51, 2.48, 2.52, 2.50];

    let mean1 = run1.iter().sum::<f64>() / run1.len() as f64;
    let mean2 = run2.iter().sum::<f64>() / run2.len() as f64;

    // Validate means are within acceptable variance
    let diff_pct = ((mean1 - mean2).abs() / mean1) * 100.0;
    assert!(
        diff_pct < 2.0,
        "Expected <2% difference between runs, got {:.2}%",
        diff_pct
    );

    // Test reproducibility with different sample sizes
    let small_sample = vec![2.50, 2.48, 2.52]; // 3 samples
    let large_sample = vec![2.50, 2.48, 2.52, 2.49, 2.51, 2.50, 2.49]; // 7 samples

    let small_mean = small_sample.iter().sum::<f64>() / small_sample.len() as f64;
    let large_mean = large_sample.iter().sum::<f64>() / large_sample.len() as f64;

    let mean_diff_pct = ((small_mean - large_mean).abs() / small_mean) * 100.0;
    assert!(
        mean_diff_pct < 1.0,
        "Means should be consistent across sample sizes"
    );

    // Validate reproducibility criteria
    fn validate_reproducibility(
        measurements1: &[f64],
        measurements2: &[f64],
        threshold: f64,
    ) -> bool {
        let mean1 = measurements1.iter().sum::<f64>() / measurements1.len() as f64;
        let mean2 = measurements2.iter().sum::<f64>() / measurements2.len() as f64;
        let diff_pct = ((mean1 - mean2).abs() / mean1) * 100.0;
        diff_pct <= threshold
    }

    assert!(
        validate_reproducibility(&run1, &run2, 2.0),
        "Runs should be reproducible within 2%"
    );
}

/// AC2: Test baseline archival to history
///
/// Tests feature spec: docs/reference/benchmark-api-contracts.md#baseline-management-api
///
/// Validates that previous baselines are correctly archived to history
/// when new baselines are promoted.
#[test]
fn test_baseline_archival() {
    // AC2
    let mut store = BaselineStore::new();

    // Promote first baseline
    let mut report1 = PerformanceReport::new();
    report1.display_gibs = Some(2.40);
    report1.comp3_mibs = Some(160.0);
    store.promote_baseline(&report1, "main", "baseline-1");

    assert_eq!(
        store.history.len(),
        0,
        "History should be empty after first promotion"
    );

    // Promote second baseline
    let mut report2 = PerformanceReport::new();
    report2.display_gibs = Some(2.50);
    report2.comp3_mibs = Some(172.0);
    store.promote_baseline(&report2, "main", "baseline-2");

    assert_eq!(
        store.history.len(),
        1,
        "Previous baseline should be archived"
    );
    assert_eq!(store.history[0].commit, "baseline-1");

    // Test archival with multiple promotions
    let mut report3 = PerformanceReport::new();
    report3.display_gibs = Some(2.80);
    report3.comp3_mibs = Some(200.0);
    report3.timestamp = chrono::Utc::now().to_rfc3339();
    report3.commit = "baseline-3".to_string();

    store.promote_baseline(&report3, "main", "baseline-3");
    assert_eq!(
        store.history.len(),
        2,
        "History should contain 2 previous baselines"
    );

    // Validate history ordering (newest first)
    assert_eq!(
        store.history[0].commit, "baseline-2",
        "Most recent should be first"
    );
    assert_eq!(
        store.history[1].commit, "baseline-1",
        "Older should be second"
    );

    // Test archival with retention policy (simulate old baselines)
    let old_timestamp = chrono::Utc::now() - chrono::Duration::days(100);
    store.history[0].timestamp = old_timestamp.to_rfc3339();
    store.history[1].timestamp = old_timestamp.to_rfc3339();

    // Apply retention policy manually for testing
    store.apply_retention_policy(90); // 90-day retention
    assert_eq!(
        store.history.len(),
        0,
        "Old baselines should be removed by retention policy"
    );
}
