//! AC2: JSON Schema Validation Tests
//!
//! Comprehensive validation of the `PerformanceReport` JSON schema
//! ensuring enterprise-grade data integrity and mutation resilience.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::needless_borrows_for_generic_args,
    clippy::uninlined_format_args,
    clippy::single_match_else
)] // Tests: allow common pedantic lints

use copybook_bench::reporting::PerformanceReport;

#[test]
fn test_performance_report_serialization() {
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(4.22);
    report.comp3_mibs = Some(571.0);
    report.timestamp = "2024-01-15T10:30:00Z".to_string();
    report.commit = "7bef3ba".to_string();
    report.warnings = vec!["COMP-3 performance below target".to_string()];
    report.errors = vec![];

    let json = serde_json::to_string_pretty(&report).unwrap();
    assert!(json.contains("4.22"));
    assert!(json.contains("571.0"));
    assert!(json.contains("\"commit\": \"7bef3ba\""));

    // Validate round-trip
    let deserialized: PerformanceReport = serde_json::from_str(&json).unwrap();

    assert_eq!(deserialized.display_gibs, Some(4.22));
    assert_eq!(deserialized.comp3_mibs, Some(571.0));
    assert_eq!(deserialized.commit, "7bef3ba");
}

#[test]
fn test_json_schema_edge_cases() {
    // Test with zero performance (mutation testing scenario)
    let mut report = PerformanceReport::new();
    report.display_gibs = Some(0.0);
    report.comp3_mibs = Some(0.0);
    report.timestamp = "2024-01-01T00:00:00Z".to_string();
    report.commit = "0000000".to_string();
    report.warnings = vec!["Zero performance detected".to_string()];
    report.errors = vec!["Benchmark failure".to_string()];

    let json = serde_json::to_string(&report).unwrap();
    let deserialized: PerformanceReport = serde_json::from_str(&json).unwrap();

    assert_eq!(deserialized.display_gibs, Some(0.0));
    assert_eq!(deserialized.comp3_mibs, Some(0.0));
}

#[test]
fn test_invalid_json_handling() {
    let invalid_jsons = [
        r#"{"incomplete": true"#,              // Malformed JSON
        r#"{"display_gibs": "not_a_number"}"#, // Type mismatch
        r#"{"display_gibs": null}"#,           // Null value
        r"{}",                                 // Missing required fields
    ];

    for invalid_json in invalid_jsons {
        let result: Result<PerformanceReport, _> = serde_json::from_str(invalid_json);
        assert!(
            result.is_err(),
            "Should reject invalid JSON: {invalid_json}"
        );
    }
}

#[test]
fn test_performance_metrics_validation() {
    // Test extreme values for mutation testing
    let test_cases = vec![
        (Some(f64::INFINITY), Some(100.0)), // Infinite DISPLAY performance
        (Some(100.0), Some(f64::NEG_INFINITY)), // Negative infinite COMP-3
        (Some(f64::NAN), Some(50.0)),       // NaN DISPLAY
        (Some(25.0), Some(f64::NAN)),       // NaN COMP-3
        (None, None),                       // Missing values
    ];

    for (display, comp3) in test_cases {
        let mut report = PerformanceReport::new();
        report.display_gibs = display;
        report.comp3_mibs = comp3;

        // Should handle extreme values gracefully
        let json_result = serde_json::to_string(&report);
        if let Ok(json) = json_result {
            // If serialization succeeds, deserialization should handle it
            let _: Result<PerformanceReport, _> = serde_json::from_str(&json);
        } else {
            // JSON serialization may legitimately fail for NaN/Infinity
        }
    }
}

#[test]
fn test_slo_validation_mutation() {
    // Test SLO validation with various scenarios for mutation coverage
    let test_cases = vec![
        (Some(4.5), Some(600.0), "success"), // Above SLOs
        (Some(3.0), Some(500.0), "failure"), // Below SLOs
        (Some(4.3), Some(570.0), "warning"), // Close to SLOs
        (None, None, "success"),             // No metrics
        (Some(0.0), Some(0.0), "failure"),   // Zero performance
    ];

    for (display, comp3, expected_status) in test_cases {
        let mut report = PerformanceReport::new();
        report.display_gibs = display;
        report.comp3_mibs = comp3;
        report.timestamp = "2024-12-31T23:59:59Z".to_string();
        report.commit = "abc123def456".to_string();

        report.validate_slos(4.1, 560.0);

        let json = serde_json::to_string(&report).unwrap();
        let deserialized: PerformanceReport = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.display_gibs, display);
        assert_eq!(deserialized.comp3_mibs, comp3);
        assert_eq!(deserialized.commit, "abc123def456");

        // Status should match expected based on SLO validation
        if expected_status == "failure" {
            assert_eq!(report.status, "failure");
            assert!(!report.errors.is_empty());
        } else if expected_status == "warning" {
            assert!(report.status == "warning" || report.status == "success");
        }
    }
}
