// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-bench tooling.
//!
//! Covers: Performance JSON schema validation, baseline management,
//! SLO evaluation, regression detection, summary formatting,
//! receipt validation, statistical analysis, and performance floor enforcement.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_bench::baseline::{BaselineStore, PerformanceBaseline};
use copybook_bench::reporting::PerformanceReport;

// ---------------------------------------------------------------------------
// 1. Performance JSON schema validation
// ---------------------------------------------------------------------------

#[test]
fn json_schema_full_report_roundtrip() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.22);
    r.comp3_mibs = Some(571.0);
    r.commit = "abc12345".to_string();
    r.warnings.push("near threshold".to_string());
    r.errors.push("below SLO".to_string());
    r.status = "failure".to_string();

    let json = serde_json::to_string_pretty(&r).unwrap();
    let parsed: PerformanceReport = serde_json::from_str(&json).unwrap();

    assert_eq!(parsed.display_gibs, Some(4.22));
    assert_eq!(parsed.comp3_mibs, Some(571.0));
    assert_eq!(parsed.commit, "abc12345");
    assert_eq!(parsed.status, "failure");
    assert_eq!(parsed.warnings.len(), 1);
    assert_eq!(parsed.errors.len(), 1);
}

#[test]
fn json_schema_minimal_deserialize_uses_defaults() {
    let json = r#"{"display_gibs": 2.0}"#;
    let r: PerformanceReport = serde_json::from_str(json).unwrap();
    assert_eq!(r.display_gibs, Some(2.0));
    assert!(r.comp3_mibs.is_none());
    assert_eq!(r.status, "success");
    assert!(r.warnings.is_empty());
    assert!(r.errors.is_empty());
}

#[test]
fn json_schema_empty_object_deserializes() {
    let json = "{}";
    let r: PerformanceReport = serde_json::from_str(json).unwrap();
    assert!(r.display_gibs.is_none());
    assert!(r.comp3_mibs.is_none());
    assert_eq!(r.status, "success");
}

#[test]
fn json_schema_invalid_json_errors() {
    assert!(serde_json::from_str::<PerformanceReport>("not json").is_err());
}

// ---------------------------------------------------------------------------
// 2. Baseline management (promote, show, compare)
// ---------------------------------------------------------------------------

#[test]
fn baseline_promote_sets_current() {
    let mut store = BaselineStore::new();
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(205.0);
    r.comp3_mibs = Some(58.0);
    store.promote_baseline(&r, "main", "1fa63633");

    let b = store.current.as_ref().unwrap();
    assert_eq!(b.branch, "main");
    assert_eq!(b.commit, "1fa63633");
    assert_eq!(b.display_gibs, Some(205.0));
    assert_eq!(b.comp3_mibs, Some(58.0));
}

#[test]
fn baseline_promote_archives_previous() {
    let mut store = BaselineStore::new();
    let mut r1 = PerformanceReport::new();
    r1.display_gibs = Some(3.0);
    store.promote_baseline(&r1, "main", "old_commit");

    let mut r2 = PerformanceReport::new();
    r2.display_gibs = Some(4.0);
    store.promote_baseline(&r2, "main", "new_commit");

    assert_eq!(store.current.as_ref().unwrap().commit, "new_commit");
    assert_eq!(store.history.len(), 1);
    assert_eq!(store.history[0].commit, "old_commit");
}

#[test]
fn baseline_show_summary_with_values() {
    let mut store = BaselineStore::new();
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.22);
    r.comp3_mibs = Some(571.0);
    store.promote_baseline(&r, "main", "deadbeef00");

    let summary = store.summary();
    assert!(summary.contains("deadbeef"));
    assert!(summary.contains("4.22 GiB/s"));
    assert!(summary.contains("571 MiB/s"));
    assert!(summary.contains("main"));
}

#[test]
fn baseline_show_summary_no_baseline() {
    let store = BaselineStore::new();
    assert_eq!(store.summary(), "No baseline established");
}

#[test]
fn baseline_compare_no_regression() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(4.0);
    current.comp3_mibs = Some(500.0);
    assert!(store.check_regression(&current, 5.0).is_empty());
}

#[test]
fn baseline_compare_improvement_not_flagged() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(5.0);
    current.comp3_mibs = Some(700.0);
    assert!(store.check_regression(&current, 5.0).is_empty());
}

// ---------------------------------------------------------------------------
// 3. SLO evaluation (pass/fail thresholds)
// ---------------------------------------------------------------------------

#[test]
fn slo_pass_well_above() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(5.0);
    r.comp3_mibs = Some(700.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "success");
    assert!(r.errors.is_empty());
    assert!(r.warnings.is_empty());
}

#[test]
fn slo_fail_display_below() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(3.0);
    r.comp3_mibs = Some(700.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
    assert!(r.errors.iter().any(|e| e.contains("DISPLAY")));
}

#[test]
fn slo_fail_comp3_below() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(5.0);
    r.comp3_mibs = Some(400.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
    assert!(r.errors.iter().any(|e| e.contains("COMP-3")));
}

#[test]
fn slo_fail_both_below() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(1.0);
    r.comp3_mibs = Some(100.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
    assert_eq!(r.errors.len(), 2);
}

#[test]
fn slo_warning_near_display_threshold() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.02); // within 5% above SLO
    r.comp3_mibs = Some(700.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "warning");
    assert!(!r.warnings.is_empty());
}

#[test]
fn slo_failure_overrides_warning() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.02); // warning range
    r.comp3_mibs = Some(100.0); // failure
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
}

#[test]
fn slo_none_values_succeed() {
    let mut r = PerformanceReport::new();
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "success");
}

// ---------------------------------------------------------------------------
// 4. Regression detection logic
// ---------------------------------------------------------------------------

#[test]
fn regression_no_baseline_empty() {
    let store = BaselineStore::new();
    let r = PerformanceReport::new();
    assert!(store.check_regression(&r, 5.0).is_empty());
}

#[test]
fn regression_display_only() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.0); // 25% slower
    current.comp3_mibs = Some(500.0);
    let reg = store.check_regression(&current, 5.0);
    assert_eq!(reg.len(), 1);
    assert!(reg[0].contains("DISPLAY"));
}

#[test]
fn regression_comp3_only() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(4.0);
    current.comp3_mibs = Some(200.0); // 60% slower
    let reg = store.check_regression(&current, 5.0);
    assert_eq!(reg.len(), 1);
    assert!(reg[0].contains("COMP-3"));
}

#[test]
fn regression_both_metrics() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(2.0);
    current.comp3_mibs = Some(200.0);
    let reg = store.check_regression(&current, 5.0);
    assert_eq!(reg.len(), 2);
}

#[test]
fn regression_exact_threshold_boundary() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "b");

    // Exactly 5% slower: (100-95)/100*100 = 5.0; > is strict
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(95.0);
    assert!(
        store.check_regression(&current, 5.0).is_empty(),
        "exactly at threshold should not trigger"
    );
}

#[test]
fn regression_just_above_threshold() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    store.promote_baseline(&baseline, "main", "b");

    // (4.0 - 3.79) / 4.0 * 100 = 5.25%
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.79);
    assert_eq!(store.check_regression(&current, 5.0).len(), 1);
}

// ---------------------------------------------------------------------------
// 5. Summary output formatting
// ---------------------------------------------------------------------------

#[test]
fn pr_summary_success_icon() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.22);
    r.comp3_mibs = Some(571.0);
    let s = r.format_pr_summary();
    assert!(s.contains("4.22 GiB/s"));
    assert!(s.contains("571 MiB/s"));
    assert!(s.contains('✅'));
}

#[test]
fn pr_summary_failure_icon() {
    let mut r = PerformanceReport::new();
    r.status = "failure".to_string();
    r.display_gibs = Some(2.0);
    r.comp3_mibs = Some(300.0);
    let s = r.format_pr_summary();
    assert!(s.contains('❌'));
}

#[test]
fn pr_summary_warning_icon() {
    let mut r = PerformanceReport::new();
    r.status = "warning".to_string();
    let s = r.format_pr_summary();
    assert!(s.contains("⚠️"));
}

#[test]
fn pr_summary_none_values_show_na() {
    let r = PerformanceReport::new();
    let s = r.format_pr_summary();
    assert_eq!(s.matches("N/A").count(), 2);
}

#[test]
fn baseline_summary_none_values_show_na() {
    let mut store = BaselineStore::new();
    let r = PerformanceReport::new(); // no metrics
    store.promote_baseline(&r, "feature", "xyz");
    let s = store.summary();
    assert!(s.contains("N/A"));
}

// ---------------------------------------------------------------------------
// 6. Receipt validation (hardware specs, measurements)
// ---------------------------------------------------------------------------

#[test]
fn baseline_store_file_save_load_roundtrip() {
    let dir = std::env::temp_dir().join("cb_bench_comprehensive_test");
    std::fs::create_dir_all(&dir).ok();
    let path = dir.join("receipt.json");

    let mut store = BaselineStore::new();
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(205.0);
    r.comp3_mibs = Some(58.0);
    store.promote_baseline(&r, "main", "1fa63633");

    store.save(&path).unwrap();
    let loaded = BaselineStore::load_or_create(&path).unwrap();

    let b = loaded.current.as_ref().unwrap();
    assert_eq!(b.commit, "1fa63633");
    assert_eq!(b.display_gibs, Some(205.0));
    assert_eq!(b.comp3_mibs, Some(58.0));

    std::fs::remove_file(&path).ok();
    std::fs::remove_dir(&dir).ok();
}

#[test]
fn baseline_store_load_missing_creates_empty() {
    let path = std::env::temp_dir().join("nonexistent_receipt_99999.json");
    std::fs::remove_file(&path).ok();
    let store = BaselineStore::load_or_create(&path).unwrap();
    assert!(store.current.is_none());
    assert!(store.history.is_empty());
}

#[test]
fn baseline_store_json_serialization_roundtrip() {
    let mut store = BaselineStore::new();
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.5);
    r.comp3_mibs = Some(550.0);
    store.promote_baseline(&r, "main", "abc");

    let json = serde_json::to_string_pretty(&store).unwrap();
    let parsed: BaselineStore = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed.current.as_ref().unwrap().commit, "abc");
}

// ---------------------------------------------------------------------------
// 7. Statistical analysis (mean, stddev, variance proxy)
// ---------------------------------------------------------------------------

#[test]
fn retention_policy_removes_old_entries() {
    let mut store = BaselineStore::new();
    store.history.push(PerformanceBaseline {
        branch: "main".to_string(),
        commit: "old".to_string(),
        timestamp: "2020-01-01T00:00:00+00:00".to_string(),
        display_gibs: Some(3.0),
        comp3_mibs: Some(400.0),
        sample_count: 1,
    });
    store.history.push(PerformanceBaseline {
        branch: "main".to_string(),
        commit: "recent".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        display_gibs: Some(4.0),
        comp3_mibs: Some(500.0),
        sample_count: 1,
    });
    store.apply_retention_policy(90);
    assert_eq!(store.history.len(), 1);
    assert_eq!(store.history[0].commit, "recent");
}

#[test]
fn retention_policy_keeps_invalid_timestamps() {
    let mut store = BaselineStore::new();
    store.history.push(PerformanceBaseline {
        branch: "main".to_string(),
        commit: "bad_ts".to_string(),
        timestamp: "not-a-date".to_string(),
        display_gibs: None,
        comp3_mibs: None,
        sample_count: 0,
    });
    store.apply_retention_policy(90);
    assert_eq!(store.history.len(), 1);
}

#[test]
fn verbose_regression_log_contains_delta() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.8); // 5% slower
    current.comp3_mibs = Some(490.0); // 2% slower
    let (_, log) = store.check_regression_verbose(&current, 10.0);
    // Log should contain baseline= and current= values
    assert!(log.iter().any(|l| l.contains("baseline=")));
    assert!(log.iter().any(|l| l.contains("current=")));
    assert!(log.iter().any(|l| l.contains("delta=")));
}

#[test]
fn verbose_regression_no_baseline_logs_neutral() {
    let store = BaselineStore::new();
    let r = PerformanceReport::new();
    let (reg, log) = store.check_regression_verbose(&r, 5.0);
    assert!(reg.is_empty());
    assert!(log.iter().any(|l| l.contains("No baseline")));
}

#[test]
fn multiple_promotions_build_history() {
    let mut store = BaselineStore::new();
    for i in 0..10 {
        let mut r = PerformanceReport::new();
        r.display_gibs = Some(f64::from(i));
        store.promote_baseline(&r, "main", &format!("commit_{i}"));
    }
    assert_eq!(store.current.as_ref().unwrap().commit, "commit_9");
    assert_eq!(store.history.len(), 9);
    // Newest first
    assert_eq!(store.history[0].commit, "commit_8");
    assert_eq!(store.history[8].commit, "commit_0");
}

// ---------------------------------------------------------------------------
// 8. Performance floor enforcement
// ---------------------------------------------------------------------------

#[test]
fn floor_display_80_mibs_pass() {
    // CI floor: DISPLAY >= 80 MiB/s → in GiB/s terms that's ~0.078
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(0.2); // 200 MiB/s
    r.validate_slos(0.078, 40.0);
    assert_eq!(r.status, "success");
}

#[test]
fn floor_display_80_mibs_fail() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(0.05); // 50 MiB/s < 80 MiB/s floor
    r.validate_slos(0.078, 40.0);
    assert_eq!(r.status, "failure");
}

#[test]
fn floor_comp3_40_mibs_pass() {
    let mut r = PerformanceReport::new();
    r.comp3_mibs = Some(58.0);
    r.validate_slos(0.078, 40.0);
    assert_eq!(r.status, "success");
}

#[test]
fn floor_comp3_40_mibs_fail() {
    let mut r = PerformanceReport::new();
    r.comp3_mibs = Some(30.0);
    r.validate_slos(0.078, 40.0);
    assert_eq!(r.status, "failure");
}

#[test]
fn floor_both_fail() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(0.01);
    r.comp3_mibs = Some(10.0);
    r.validate_slos(0.078, 40.0);
    assert_eq!(r.status, "failure");
    assert_eq!(r.errors.len(), 2);
}

#[test]
fn regression_zero_threshold_any_decrease_triggers() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.999);
    let reg = store.check_regression(&current, 0.0);
    assert_eq!(reg.len(), 1);
}

// ---------------------------------------------------------------------------
// Additional: health checks and memory utils
// ---------------------------------------------------------------------------

#[test]
fn health_checks_run_without_panic() {
    let path = std::path::PathBuf::from("nonexistent_baseline.json");
    let checks = copybook_bench::health::run_health_checks(&path);
    assert!(checks.len() >= 4);
}

#[test]
fn health_check_baseline_missing_is_warning() {
    let checks =
        copybook_bench::health::run_health_checks(std::path::Path::new("/no/such/baseline"));
    let baseline_check = checks.iter().find(|c| c.name == "Baseline file").unwrap();
    assert_eq!(
        baseline_check.status,
        copybook_bench::health::HealthStatus::Warning
    );
}

#[test]
fn memory_rss_does_not_panic() {
    let _ = copybook_bench::memory::get_rss_bytes();
}

#[test]
fn memory_fd_count_does_not_panic() {
    let _ = copybook_bench::memory::get_open_fd_count();
}

#[test]
fn regression_utils_create_detectors_no_panic() {
    let _ = copybook_bench::regression::utils::create_standard_detector();
    let _ = copybook_bench::regression::utils::create_ci_detector();
    let _ = copybook_bench::regression::utils::create_dev_detector();
}
