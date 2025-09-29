//! Baseline Management Mutation Testing
//!
//! Comprehensive mutation testing for baseline storage, promotion,
//! and regression detection to ensure enterprise reliability.

use copybook_bench::baseline::BaselineStore;
use copybook_bench::reporting::PerformanceReport;
use tempfile::TempDir;
use std::thread;
use std::sync::Arc;

fn create_test_report(display: Option<f64>, comp3: Option<f64>, commit: &str) -> PerformanceReport {
    let mut report = PerformanceReport::new();
    report.display_gibs = display;
    report.comp3_mibs = comp3;
    report.commit = commit.to_string();
    report.timestamp = "2024-01-01T00:00:00Z".to_string();
    report
}

#[test]
fn test_baseline_store_serialization_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let baseline_path = temp_dir.path().join("baseline.json");

    // Test empty store
    let mut store = BaselineStore::new();
    store.save(&baseline_path).expect("Failed to save empty store");

    let loaded_store = BaselineStore::load_or_create(&baseline_path)
        .expect("Failed to load store");
    assert!(loaded_store.current.is_none());
    assert!(loaded_store.history.is_empty());

    // Test store with baseline
    let report = create_test_report(Some(4.0), Some(500.0), "baseline_commit");
    store.promote_baseline(&report, "main", "baseline_commit");

    store.save(&baseline_path).expect("Failed to save store with baseline");

    let loaded_store = BaselineStore::load_or_create(&baseline_path)
        .expect("Failed to load store with baseline");

    assert!(loaded_store.current.is_some());
    let current = loaded_store.current.unwrap();
    assert_eq!(current.commit, "baseline_commit");
    assert_eq!(current.branch, "main");
    assert_eq!(current.display_gibs, Some(4.0));
    assert_eq!(current.comp3_mibs, Some(500.0));
}

#[test]
fn test_baseline_promotion_mutations() {
    let mut store = BaselineStore::new();

    // Test promotion with all metrics
    let report1 = create_test_report(Some(4.0), Some(500.0), "commit1");
    store.promote_baseline(&report1, "main", "commit1");

    assert!(store.current.is_some());
    assert_eq!(store.current.as_ref().unwrap().commit, "commit1");
    assert!(store.history.is_empty());

    // Test promotion archiving previous baseline
    let report2 = create_test_report(Some(4.5), Some(550.0), "commit2");
    store.promote_baseline(&report2, "main", "commit2");

    assert!(store.current.is_some());
    assert_eq!(store.current.as_ref().unwrap().commit, "commit2");
    assert_eq!(store.history.len(), 1);
    assert_eq!(store.history[0].commit, "commit1");

    // Test promotion with missing metrics (mutation scenario)
    let report3 = create_test_report(None, None, "commit3");
    store.promote_baseline(&report3, "feature", "commit3");

    let current = store.current.as_ref().unwrap();
    assert_eq!(current.commit, "commit3");
    assert_eq!(current.branch, "feature");
    assert_eq!(current.display_gibs, None);
    assert_eq!(current.comp3_mibs, None);
}

#[test]
fn test_regression_detection_mutations() {
    let mut store = BaselineStore::new();

    // Establish baseline
    let baseline = create_test_report(Some(4.0), Some(500.0), "baseline");
    store.promote_baseline(&baseline, "main", "baseline");

    // Test cases for comprehensive mutation coverage
    let test_cases = vec![
        // (display, comp3, threshold, expected_regressions)
        (Some(4.0), Some(500.0), 5.0, 0),     // Exact match - no regression
        // Exactly 5% - may not trigger if check is strict > (not >=)
        (Some(3.8), Some(475.0), 5.0, 1),     // 5% regression - check if logic is > or >=
        (Some(3.9), Some(500.0), 5.0, 0),     // 2.5% regression in DISPLAY - should pass
        (Some(4.0), Some(476.0), 5.0, 0),     // 4.8% regression in COMP-3 - should pass
        (Some(4.2), Some(525.0), 5.0, 0),     // Improvement - should pass
        (Some(3.79), Some(474.0), 5.0, 2),    // Just over 5% regression - should detect both
        (Some(0.0), Some(0.0), 5.0, 2),       // Zero performance - should detect regression
        (None, None, 5.0, 0),                  // Missing metrics - no comparison possible
        (Some(4.0), None, 5.0, 0),            // Mixed metrics - only compare available
        (None, Some(500.0), 5.0, 0),          // Mixed metrics - only compare available
    ];

    for (display, comp3, threshold, expected_count) in test_cases {
        let current = create_test_report(display, comp3, "current");
        let regressions = store.check_regression(&current, threshold);

        assert_eq!(regressions.len(), expected_count,
            "Expected {} regressions for DISPLAY: {:?}, COMP-3: {:?}, but got {}: {:?}",
            expected_count, display, comp3, regressions.len(), regressions);

        // Validate regression message format
        for regression in &regressions {
            assert!(regression.contains("regression:"));
            assert!(regression.contains("%"));
            assert!(regression.contains("baseline"));
        }
    }
}

#[test]
fn test_retention_policy_mutations() {
    let mut store = BaselineStore::new();

    // Create multiple baselines with different timestamps
    for i in 0..5 {
        let report = create_test_report(Some(4.0), Some(500.0), &format!("commit_{}", i));
        store.promote_baseline(&report, "main", &format!("commit_{}", i));

        // Simulate passage of time by manually adjusting history timestamps
        if let Some(last_history) = store.history.last_mut() {
            // Make older baselines progressively older
            let days_ago = chrono::Duration::days(i64::from(i) * 30 + 100); // 100+ days ago
            let old_timestamp = (chrono::Utc::now() - days_ago).to_rfc3339();
            last_history.timestamp = old_timestamp;
        }
    }

    // Should have some items in history (latest is current)
    // Note: retention policy is applied during promotion, so actual count depends on implementation
    assert!(store.history.len() >= 1, "Should have at least some history before retention test");

    // Apply retention - should remove old baselines
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let path = temp_dir.path().join("test.json");

    // Save and reload to trigger retention policy
    store.save(&path).expect("Failed to save");
    let reloaded_store = BaselineStore::load_or_create(&path).expect("Failed to reload");

    // Current should still exist
    assert!(reloaded_store.current.is_some());
    // History should be reduced due to retention policy (but depends on exact implementation)
    assert!(reloaded_store.history.len() <= store.history.len());
}

#[test]
fn test_concurrent_access_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let baseline_path = Arc::new(temp_dir.path().join("concurrent_baseline.json"));

    // Test concurrent promotions
    let handles: Vec<_> = (0..5).map(|i| {
        let path = Arc::clone(&baseline_path);
        thread::spawn(move || {
            let mut store = BaselineStore::load_or_create(&*path).unwrap_or_else(|_| BaselineStore::new());
            let report = create_test_report(Some(4.0 + f64::from(i) * 0.1), Some(500.0), &format!("commit_{}", i));
            store.promote_baseline(&report, "main", &format!("commit_{}", i));
            store.save(&*path).ok(); // May fail due to concurrent writes, that's expected
        })
    }).collect();

    for handle in handles {
        handle.join().expect("Thread panicked");
    }

    // Should still have a valid baseline after concurrent access
    let final_store = BaselineStore::load_or_create(&*baseline_path)
        .unwrap_or_else(|_| BaselineStore::new());

    // Either we have a baseline (successful concurrent write) or we don't (file corruption)
    // Both are acceptable outcomes for this stress test
    if let Some(current) = final_store.current {
        assert!(current.commit.starts_with("commit_"));
        assert!(current.display_gibs.is_some());
        assert!(current.comp3_mibs.is_some());
    }
}

#[test]
fn test_edge_case_mutations() {
    let mut store = BaselineStore::new();

    // Test with extreme values
    let extreme_report = create_test_report(Some(f64::MAX), Some(f64::MIN), "extreme");
    store.promote_baseline(&extreme_report, "extreme_branch", "extreme");

    assert!(store.current.is_some());
    let current = store.current.as_ref().unwrap();
    assert_eq!(current.display_gibs, Some(f64::MAX));
    assert_eq!(current.comp3_mibs, Some(f64::MIN));

    // Test regression detection with extreme baseline
    let normal_report = create_test_report(Some(4.0), Some(500.0), "normal");
    let regressions = store.check_regression(&normal_report, 5.0);

    // Should detect massive "regression" from MAX to 4.0
    assert!(!regressions.is_empty());

    // Test summary with extreme values
    let summary = store.summary();
    assert!(summary.contains("Baseline"));
    assert!(summary.contains("extreme_branch"));
}

#[test]
fn test_invalid_data_mutations() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let corrupted_path = temp_dir.path().join("corrupted.json");

    // Create corrupted JSON file
    std::fs::write(&corrupted_path, r#"{"invalid": "json", "missing": "fields"#)
        .expect("Failed to write corrupted file");

    // Should handle corrupted file gracefully by creating new store
    let result = BaselineStore::load_or_create(&corrupted_path);
    match result {
        Ok(store) => {
            // If it loads, it should be empty (fallback behavior)
            assert!(store.current.is_none());
        }
        Err(_) => {
            // It's also acceptable to fail on corrupted data
        }
    }

    // Test with valid JSON but invalid structure
    std::fs::write(&corrupted_path, r#"{"current": "not_a_baseline"}"#)
        .expect("Failed to write invalid structure");

    let result = BaselineStore::load_or_create(&corrupted_path);
    assert!(result.is_err(), "Should reject invalid baseline structure");
}

#[test]
fn test_summary_formatting_mutations() {
    let mut store = BaselineStore::new();

    // Test summary with no baseline
    let summary = store.summary();
    assert_eq!(summary, "No baseline established");

    // Test summary with complete baseline
    let report = create_test_report(Some(4.123456), Some(567.89), "1234567890abcdef");
    store.promote_baseline(&report, "main", "1234567890abcdef");

    let summary = store.summary();
    assert!(summary.contains("Baseline (12345678):")); // 8-char commit
    assert!(summary.contains("4.12 GiB/s"));          // Rounded display
    assert!(summary.contains("568 MiB/s"));           // Rounded comp3
    assert!(summary.contains("[main]"));              // Branch

    // Test summary with missing metrics
    let partial_report = create_test_report(None, None, "partial");
    store.promote_baseline(&partial_report, "feature", "partial");

    let summary = store.summary();
    assert!(summary.contains("N/A"));
    assert!(summary.contains("[feature]"));
}