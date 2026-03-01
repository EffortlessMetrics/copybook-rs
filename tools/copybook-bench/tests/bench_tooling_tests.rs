// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-bench tooling
//!
//! Covers: Benchmark report parsing, performance JSON validation, baseline comparison,
//! summary generation, threshold checking, SLO validation, health checks, memory utils,
//! and regression detection infrastructure.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_bench::baseline::{BaselineStore, PerformanceBaseline};
use copybook_bench::reporting::PerformanceReport;

// ---------------------------------------------------------------------------
// PerformanceReport construction & defaults
// ---------------------------------------------------------------------------

#[test]
fn test_report_new_defaults() {
    let r = PerformanceReport::new();
    assert_eq!(r.status, "success");
    assert!(r.display_gibs.is_none());
    assert!(r.comp3_mibs.is_none());
    assert!(r.warnings.is_empty());
    assert!(r.errors.is_empty());
    assert!(!r.timestamp.is_empty());
    assert_eq!(r.commit, "unknown");
}

#[test]
fn test_report_default_trait() {
    let r = PerformanceReport::default();
    assert_eq!(r.status, "success");
}

// ---------------------------------------------------------------------------
// SLO validation
// ---------------------------------------------------------------------------

#[test]
fn test_slo_pass_well_above_threshold() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(5.0);
    r.comp3_mibs = Some(700.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "success");
    assert!(r.errors.is_empty());
    assert!(r.warnings.is_empty());
}

#[test]
fn test_slo_failure_display_below() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(3.0);
    r.comp3_mibs = Some(700.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
    assert!(r.errors.iter().any(|e| e.contains("DISPLAY")));
}

#[test]
fn test_slo_failure_comp3_below() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(5.0);
    r.comp3_mibs = Some(400.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
    assert!(r.errors.iter().any(|e| e.contains("COMP-3")));
}

#[test]
fn test_slo_failure_both_below() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(1.0);
    r.comp3_mibs = Some(100.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
    assert_eq!(r.errors.len(), 2);
}

#[test]
fn test_slo_warning_near_threshold() {
    let mut r = PerformanceReport::new();
    // 4.02 is within 5% above 4.0 SLO → warning
    r.display_gibs = Some(4.02);
    r.comp3_mibs = Some(700.0);
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "warning");
    assert!(!r.warnings.is_empty());
}

#[test]
fn test_slo_failure_overrides_warning() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.02); // warning range
    r.comp3_mibs = Some(100.0); // failure
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "failure");
}

#[test]
fn test_slo_none_values_no_errors() {
    let mut r = PerformanceReport::new();
    r.validate_slos(4.0, 500.0);
    assert_eq!(r.status, "success");
    assert!(r.errors.is_empty());
}

// ---------------------------------------------------------------------------
// PR summary format
// ---------------------------------------------------------------------------

#[test]
fn test_pr_summary_success() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.22);
    r.comp3_mibs = Some(571.0);
    let summary = r.format_pr_summary();
    assert!(summary.contains("4.22 GiB/s"));
    assert!(summary.contains("571 MiB/s"));
    assert!(summary.contains("✅"));
}

#[test]
fn test_pr_summary_failure() {
    let mut r = PerformanceReport::new();
    r.status = "failure".to_string();
    r.display_gibs = Some(2.0);
    r.comp3_mibs = Some(300.0);
    let summary = r.format_pr_summary();
    assert!(summary.contains("❌"));
}

#[test]
fn test_pr_summary_warning() {
    let mut r = PerformanceReport::new();
    r.status = "warning".to_string();
    let summary = r.format_pr_summary();
    assert!(summary.contains("⚠️"));
}

#[test]
fn test_pr_summary_none_values() {
    let r = PerformanceReport::new();
    let summary = r.format_pr_summary();
    assert!(summary.contains("N/A"));
}

// ---------------------------------------------------------------------------
// Performance report JSON serialization
// ---------------------------------------------------------------------------

#[test]
fn test_report_json_roundtrip() {
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.5);
    r.comp3_mibs = Some(600.0);
    r.commit = "abc123".to_string();
    r.warnings.push("test warning".to_string());

    let json = serde_json::to_string(&r).unwrap();
    let parsed: PerformanceReport = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed.display_gibs, Some(4.5));
    assert_eq!(parsed.comp3_mibs, Some(600.0));
    assert_eq!(parsed.commit, "abc123");
    assert_eq!(parsed.warnings.len(), 1);
}

#[test]
fn test_report_deserialize_minimal_json() {
    let json = r#"{"display_gibs": 3.0, "comp3_mibs": 400.0}"#;
    let parsed: PerformanceReport = serde_json::from_str(json).unwrap();
    assert_eq!(parsed.display_gibs, Some(3.0));
    assert_eq!(parsed.comp3_mibs, Some(400.0));
    assert_eq!(parsed.status, "success"); // default
    assert!(parsed.warnings.is_empty());
}

#[test]
fn test_report_deserialize_invalid_json() {
    let result = serde_json::from_str::<PerformanceReport>("not json");
    assert!(result.is_err());
}

// ---------------------------------------------------------------------------
// BaselineStore construction
// ---------------------------------------------------------------------------

#[test]
fn test_baseline_store_new_empty() {
    let store = BaselineStore::new();
    assert!(store.current.is_none());
    assert!(store.history.is_empty());
    assert!(!store.updated.is_empty());
}

#[test]
fn test_baseline_store_default_trait() {
    let store = BaselineStore::default();
    assert!(store.current.is_none());
}

// ---------------------------------------------------------------------------
// Baseline promotion
// ---------------------------------------------------------------------------

#[test]
fn test_promote_baseline_sets_current() {
    let mut store = BaselineStore::new();
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(4.0);
    report.comp3_mibs = Some(500.0);

    store.promote_baseline(&report, "main", "commit1");

    let b = store.current.as_ref().unwrap();
    assert_eq!(b.branch, "main");
    assert_eq!(b.commit, "commit1");
    assert_eq!(b.display_gibs, Some(4.0));
    assert_eq!(b.comp3_mibs, Some(500.0));
    assert_eq!(b.sample_count, 1);
}

#[test]
fn test_promote_baseline_archives_previous() {
    let mut store = BaselineStore::new();

    let mut r1 = PerformanceReport::new();
    r1.display_gibs = Some(3.0);
    store.promote_baseline(&r1, "main", "commit_old");

    let mut r2 = PerformanceReport::new();
    r2.display_gibs = Some(4.0);
    store.promote_baseline(&r2, "main", "commit_new");

    assert_eq!(store.current.as_ref().unwrap().commit, "commit_new");
    assert_eq!(store.history.len(), 1);
    assert_eq!(store.history[0].commit, "commit_old");
}

#[test]
fn test_promote_baseline_multiple_archives() {
    let mut store = BaselineStore::new();
    for i in 0..5 {
        let mut r = PerformanceReport::new();
        r.display_gibs = Some(f64::from(i));
        store.promote_baseline(&r, "main", &format!("commit_{i}"));
    }
    assert_eq!(store.current.as_ref().unwrap().commit, "commit_4");
    assert_eq!(store.history.len(), 4);
    // History should be newest-first
    assert_eq!(store.history[0].commit, "commit_3");
}

// ---------------------------------------------------------------------------
// Regression detection
// ---------------------------------------------------------------------------

#[test]
fn test_regression_no_baseline_returns_empty() {
    let store = BaselineStore::new();
    let report = PerformanceReport::new();
    let regressions = store.check_regression(&report, 5.0);
    assert!(regressions.is_empty());
}

#[test]
fn test_regression_pass_identical() {
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
fn test_regression_pass_small_variation() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.85); // 3.75% slower - under 5% threshold
    current.comp3_mibs = Some(480.0); // 4% slower - under 5% threshold

    assert!(store.check_regression(&current, 5.0).is_empty());
}

#[test]
fn test_regression_detected_display_only() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.0); // 25% slower
    current.comp3_mibs = Some(500.0); // same

    let reg = store.check_regression(&current, 5.0);
    assert_eq!(reg.len(), 1);
    assert!(reg[0].contains("DISPLAY"));
}

#[test]
fn test_regression_detected_both() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(2.0); // 50% slower
    current.comp3_mibs = Some(200.0); // 60% slower

    let reg = store.check_regression(&current, 5.0);
    assert_eq!(reg.len(), 2);
}

#[test]
fn test_regression_improvement_not_flagged() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(5.0); // 25% faster
    current.comp3_mibs = Some(600.0); // 20% faster

    assert!(store.check_regression(&current, 5.0).is_empty());
}

// ---------------------------------------------------------------------------
// Verbose regression detection
// ---------------------------------------------------------------------------

#[test]
fn test_verbose_regression_no_baseline() {
    let store = BaselineStore::new();
    let report = PerformanceReport::new();
    let (reg, log) = store.check_regression_verbose(&report, 5.0);
    assert!(reg.is_empty());
    assert!(log.iter().any(|l| l.contains("No baseline")));
}

#[test]
fn test_verbose_regression_pass_has_log() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(4.0);
    current.comp3_mibs = Some(500.0);

    let (reg, log) = store.check_regression_verbose(&current, 5.0);
    assert!(reg.is_empty());
    assert!(log.iter().any(|l| l.contains("PASS")));
}

#[test]
fn test_verbose_regression_failure_has_log() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    baseline.comp3_mibs = Some(500.0);
    store.promote_baseline(&baseline, "main", "b");

    let mut current = PerformanceReport::new();
    current.display_gibs = Some(2.0);
    current.comp3_mibs = Some(200.0);

    let (reg, log) = store.check_regression_verbose(&current, 5.0);
    assert_eq!(reg.len(), 2);
    assert!(log.iter().any(|l| l.contains("FAILURE")));
}

// ---------------------------------------------------------------------------
// Baseline summary
// ---------------------------------------------------------------------------

#[test]
fn test_summary_no_baseline() {
    let store = BaselineStore::new();
    assert_eq!(store.summary(), "No baseline established");
}

#[test]
fn test_summary_with_baseline() {
    let mut store = BaselineStore::new();
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.22);
    r.comp3_mibs = Some(571.0);
    store.promote_baseline(&r, "main", "abcdef12345");

    let summary = store.summary();
    assert!(summary.contains("abcdef12"));
    assert!(summary.contains("4.22 GiB/s"));
    assert!(summary.contains("571 MiB/s"));
    assert!(summary.contains("main"));
}

#[test]
fn test_summary_with_none_values() {
    let mut store = BaselineStore::new();
    let r = PerformanceReport::new(); // no display/comp3
    store.promote_baseline(&r, "feature", "xyz");

    let summary = store.summary();
    assert!(summary.contains("N/A"));
}

// ---------------------------------------------------------------------------
// Retention policy
// ---------------------------------------------------------------------------

#[test]
fn test_retention_policy_removes_old_entries() {
    let mut store = BaselineStore::new();

    // Insert history entries with old timestamps
    let old_baseline = PerformanceBaseline {
        branch: "main".to_string(),
        commit: "old".to_string(),
        timestamp: "2020-01-01T00:00:00+00:00".to_string(),
        display_gibs: Some(3.0),
        comp3_mibs: Some(400.0),
        sample_count: 1,
    };
    store.history.push(old_baseline);

    let recent_baseline = PerformanceBaseline {
        branch: "main".to_string(),
        commit: "recent".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        display_gibs: Some(4.0),
        comp3_mibs: Some(500.0),
        sample_count: 1,
    };
    store.history.push(recent_baseline);

    store.apply_retention_policy(90);
    // Old entry should be removed, recent should remain
    assert_eq!(store.history.len(), 1);
    assert_eq!(store.history[0].commit, "recent");
}

#[test]
fn test_retention_policy_keeps_invalid_timestamps() {
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
    // Invalid timestamps are kept for safety
    assert_eq!(store.history.len(), 1);
}

// ---------------------------------------------------------------------------
// BaselineStore file persistence
// ---------------------------------------------------------------------------

#[test]
fn test_baseline_store_save_load_roundtrip() {
    let dir = std::env::temp_dir().join("copybook_bench_test");
    std::fs::create_dir_all(&dir).ok();
    let path = dir.join("test_baseline.json");

    let mut store = BaselineStore::new();
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.0);
    r.comp3_mibs = Some(500.0);
    store.promote_baseline(&r, "main", "test_commit");

    store.save(&path).unwrap();
    let loaded = BaselineStore::load_or_create(&path).unwrap();

    assert!(loaded.current.is_some());
    assert_eq!(loaded.current.as_ref().unwrap().commit, "test_commit");
    assert_eq!(loaded.current.as_ref().unwrap().display_gibs, Some(4.0));

    // Cleanup
    std::fs::remove_file(&path).ok();
    std::fs::remove_dir(&dir).ok();
}

#[test]
fn test_baseline_store_load_missing_creates_new() {
    let path = std::env::temp_dir().join("nonexistent_baseline_12345.json");
    // Ensure it doesn't exist
    std::fs::remove_file(&path).ok();

    let store = BaselineStore::load_or_create(&path).unwrap();
    assert!(store.current.is_none());
    assert!(store.history.is_empty());
}

// ---------------------------------------------------------------------------
// Threshold boundary tests
// ---------------------------------------------------------------------------

#[test]
fn test_regression_exactly_at_threshold_boundary() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(100.0);
    store.promote_baseline(&baseline, "main", "b");

    // 5% slower: 100.0 → 95.0; regression_pct = (100-95)/100*100 = 5.0 exactly
    // Because 100.0 - 95.0 = 5.0 exactly in IEEE 754, no rounding occurs.
    // check_regression uses strict >, so 5.0 > 5.0 is false → no regression
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(95.0);

    let reg = store.check_regression(&current, 5.0);
    assert!(
        reg.is_empty(),
        "exactly at threshold should not trigger regression"
    );
}

#[test]
fn test_regression_just_above_threshold() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    store.promote_baseline(&baseline, "main", "b");

    // Just over 5%: (4.0 - 3.79) / 4.0 * 100 = 5.25%
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.79);

    let reg = store.check_regression(&current, 5.0);
    assert_eq!(reg.len(), 1);
}

#[test]
fn test_regression_with_zero_threshold() {
    let mut store = BaselineStore::new();
    let mut baseline = PerformanceReport::new();
    baseline.display_gibs = Some(4.0);
    store.promote_baseline(&baseline, "main", "b");

    // Any decrease with 0% threshold should trigger
    let mut current = PerformanceReport::new();
    current.display_gibs = Some(3.99);
    let reg = store.check_regression(&current, 0.0);
    assert_eq!(reg.len(), 1);
}

// ---------------------------------------------------------------------------
// Health checks
// ---------------------------------------------------------------------------

#[test]
fn test_health_checks_run_without_panic() {
    let path = std::path::PathBuf::from("nonexistent_baseline.json");
    let checks = copybook_bench::health::run_health_checks(&path);
    assert!(checks.len() >= 4);
}

#[test]
fn test_health_check_baseline_missing_is_warning() {
    let checks = copybook_bench::health::run_health_checks(std::path::Path::new("/no/such/file"));
    let baseline_check = checks.iter().find(|c| c.name == "Baseline file").unwrap();
    assert_eq!(
        baseline_check.status,
        copybook_bench::health::HealthStatus::Warning
    );
}

// ---------------------------------------------------------------------------
// Memory utilities
// ---------------------------------------------------------------------------

#[test]
fn test_memory_rss_does_not_panic() {
    // On non-Linux, returns None; on Linux, returns Some(usize)
    let _rss = copybook_bench::memory::get_rss_bytes();
}

#[test]
fn test_memory_fd_count_does_not_panic() {
    let _fd = copybook_bench::memory::get_open_fd_count();
}

// ---------------------------------------------------------------------------
// Regression infrastructure types
// ---------------------------------------------------------------------------

#[test]
fn test_create_standard_detector_no_panic() {
    let _detector = copybook_bench::regression::utils::create_standard_detector();
}

#[test]
fn test_create_ci_detector_no_panic() {
    let _detector = copybook_bench::regression::utils::create_ci_detector();
}

#[test]
fn test_create_dev_detector_no_panic() {
    let _detector = copybook_bench::regression::utils::create_dev_detector();
}

// ---------------------------------------------------------------------------
// BaselineStore JSON serialization
// ---------------------------------------------------------------------------

#[test]
fn test_baseline_store_json_roundtrip() {
    let mut store = BaselineStore::new();
    let mut r = PerformanceReport::new();
    r.display_gibs = Some(4.5);
    r.comp3_mibs = Some(550.0);
    store.promote_baseline(&r, "main", "commit_abc");

    let json = serde_json::to_string_pretty(&store).unwrap();
    let parsed: BaselineStore = serde_json::from_str(&json).unwrap();

    assert!(parsed.current.is_some());
    assert_eq!(parsed.current.as_ref().unwrap().commit, "commit_abc");
    assert_eq!(parsed.current.as_ref().unwrap().display_gibs, Some(4.5));
}

#[test]
fn test_baseline_store_empty_json_roundtrip() {
    let store = BaselineStore::new();
    let json = serde_json::to_string(&store).unwrap();
    let parsed: BaselineStore = serde_json::from_str(&json).unwrap();
    assert!(parsed.current.is_none());
    assert!(parsed.history.is_empty());
}
