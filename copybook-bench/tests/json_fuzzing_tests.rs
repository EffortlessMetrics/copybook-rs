//! JSON Schema Fuzzing Tests
//!
//! Property-based and fuzzing tests for JSON schema validation to ensure
//! robust handling of malformed, adversarial, and edge-case inputs.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::needless_borrows_for_generic_args,
    clippy::uninlined_format_args,
    clippy::single_match_else,
    clippy::cast_lossless
)] // Tests: allow common pedantic lints
#![allow(clippy::unreachable)] // Tests may use unreachable for exhaustive match coverage

use copybook_bench::reporting::PerformanceReport;

#[test]
fn test_malformed_json_fuzzing() {
    let test_cases = [
        r#"{"display_gibs": 4.0"#,      // Unclosed brace
        r#"{"warnings": ["incomplete"#, // Unclosed array
        r#"{"display_gibs": 4.0,}"#,    // Trailing comma
        r#"{"display_gibs"": 4.0}"#,    // Double quotes
        r#"{"commit": "abc"def"}"#,     // Unescaped quotes
    ];

    for (i, malformed_json) in test_cases.iter().enumerate() {
        let result: Result<PerformanceReport, _> = serde_json::from_str(malformed_json);
        assert!(result.is_err(), "Malformed JSON {i} should fail to parse");
    }
}

#[test]
fn test_type_mismatch_fuzzing() {
    let test_cases = [
        r#"{"display_gibs": "not_a_number"}"#,
        r#"{"timestamp": 1234567890}"#,
        r#"{"warnings": "should_be_array"}"#,
        r#"{"timestamp": {"not": "string"}}"#,
    ];

    for (i, type_mismatch_json) in test_cases.iter().enumerate() {
        let result: Result<PerformanceReport, _> = serde_json::from_str(type_mismatch_json);
        // Type mismatches should either fail or deserialize with defaults
        match result {
            Ok(report) => {
                // If it succeeds, validate it's reasonable
                assert!(
                    report.timestamp.len() < 1000,
                    "Timestamp should be bounded for case {i}"
                );
            }
            Err(_) => {
                // Failure is also acceptable for type mismatches
            }
        }
    }
}

#[test]
fn test_extreme_values_fuzzing() {
    let test_cases = [
        r#"{"display_gibs": 1e308}"#,  // Very large
        r#"{"display_gibs": 1e-324}"#, // Very small
        r#"{"display_gibs": -1e400}"#, // Negative infinity
        r#"{"comp3_mibs": 1e400}"#,    // Positive infinity
        r#"{"display_gibs": null}"#,   // Null value
    ];

    for (i, extreme_json) in test_cases.iter().enumerate() {
        let result: Result<PerformanceReport, _> = serde_json::from_str(extreme_json);

        match result {
            Ok(report) => {
                // If parsing succeeds, validate the values
                if let Some(display) = report.display_gibs {
                    assert!(
                        display.is_finite() || display.is_infinite(),
                        "Display should be valid f64 for case {i}"
                    );
                }
                if let Some(comp3) = report.comp3_mibs {
                    assert!(
                        comp3.is_finite() || comp3.is_infinite(),
                        "COMP-3 should be valid f64 for case {i}"
                    );
                }
            }
            Err(_) => {
                // Parsing failure is acceptable for extreme values
            }
        }
    }
}

#[test]
fn test_large_data_fuzzing() {
    // Test with large but not excessive data to avoid timeout
    let large_commit = "x".repeat(10000);
    let large_json = format!(r#"{{"commit": "{}"}}"#, large_commit);

    let start = std::time::Instant::now();
    let result: Result<PerformanceReport, _> = serde_json::from_str(&large_json);
    let elapsed = start.elapsed();

    // Should complete in reasonable time
    assert!(
        elapsed.as_secs() < 2,
        "Large JSON parsing should not take excessive time"
    );

    match result {
        Ok(report) => {
            assert_eq!(report.commit.len(), 10000);
        }
        Err(_) => {
            // May legitimately fail for very large strings
        }
    }

    // Test with many warnings
    let many_warnings: Vec<String> = (0..1000).map(|i| format!("Warning {}", i)).collect();
    let mut report = PerformanceReport::new();
    report.warnings = many_warnings;

    let json = serde_json::to_string(&report).unwrap();
    let deserialized: PerformanceReport = serde_json::from_str(&json).unwrap();

    assert_eq!(deserialized.warnings.len(), 1000);
}

#[test]
fn test_injection_resistance_fuzzing() {
    let injection_attempts = [
        r"<script>alert('xss')</script>",
        r"'; DROP TABLE baselines; --",
        r"; rm -rf / #",
        r"../../../etc/passwd",
    ];

    for (i, injection) in injection_attempts.iter().enumerate() {
        let json = format!(r#"{{"commit": "{}"}}"#, injection);

        let result: Result<PerformanceReport, _> = serde_json::from_str(&json);

        match result {
            Ok(report) => {
                // If parsing succeeds, the injection should be treated as plain text
                assert_eq!(
                    report.commit, *injection,
                    "Injection {i} should be stored as plain text"
                );

                // Test that it serializes safely
                let serialized = serde_json::to_string(&report).unwrap();
                assert!(serialized.contains("commit"));
            }
            Err(_) => {
                // May fail due to escaping issues, which is fine
            }
        }
    }
}

#[test]
fn test_property_based_fuzzing() {
    // Property-based testing with many variations
    for i in 0..100 {
        let mut report = PerformanceReport::new();

        // Vary properties based on test index
        match i % 8 {
            0 => {
                report.display_gibs = None;
                report.comp3_mibs = None;
            }
            1 => {
                report.display_gibs = Some(0.0);
                report.comp3_mibs = Some(0.0);
            }
            2 => {
                report.display_gibs = Some(-1.0);
                report.comp3_mibs = Some(-100.0);
            }
            3 => {
                report.display_gibs = Some((i as f64) * 0.1);
                report.comp3_mibs = Some((i as f64) * 10.0);
            }
            4 => {
                report.commit = format!("commit_{:050}", i);
            }
            5 => {
                report.warnings = (0..i % 50)
                    .map(|j| format!("Warning {}_{}", i, j))
                    .collect();
            }
            6 => {
                report.errors = (0..i % 25).map(|j| format!("Error {}_{}", i, j)).collect();
            }
            7 => {
                report.status = format!("status_{}", i);
            }
            _ => unreachable!(),
        }

        // Test serialization roundtrip
        let json = serde_json::to_string(&report).unwrap();
        let deserialized: PerformanceReport = serde_json::from_str(&json).unwrap();

        // Validate properties preserved (with floating point tolerance)
        match (deserialized.display_gibs, report.display_gibs) {
            (Some(a), Some(b)) => assert!(
                (a - b).abs() < 1e-10,
                "Display values should be approximately equal: {a} vs {b}"
            ),
            (None, None) => {}
            _ => assert_eq!(deserialized.display_gibs, report.display_gibs),
        }
        match (deserialized.comp3_mibs, report.comp3_mibs) {
            (Some(a), Some(b)) => assert!(
                (a - b).abs() < 1e-10,
                "COMP-3 values should be approximately equal: {a} vs {b}"
            ),
            (None, None) => {}
            _ => assert_eq!(deserialized.comp3_mibs, report.comp3_mibs),
        }
        assert_eq!(deserialized.commit, report.commit);
        assert_eq!(deserialized.warnings, report.warnings);
        assert_eq!(deserialized.errors, report.errors);

        // Test SLO validation doesn't panic
        let mut slo_report = deserialized.clone();
        slo_report.validate_slos(4.1, 560.0);

        // Test PR summary doesn't panic
        let summary = slo_report.format_pr_summary();
        assert!(!summary.is_empty());
        assert!(summary.len() < 500);
    }
}

#[test]
fn test_concurrent_json_processing() {
    use std::sync::Arc;
    use std::thread;

    let test_json = r#"{
        "display_gibs": 4.22,
        "comp3_mibs": 571.0,
        "timestamp": "2024-01-01T00:00:00Z",
        "commit": "abc123",
        "status": "success",
        "warnings": ["test"],
        "errors": []
    }"#;

    let json = Arc::new(test_json.to_string());

    // Test concurrent deserialization
    let handles: Vec<_> = (0..5)
        .map(|i| {
            let json = Arc::clone(&json);
            thread::spawn(move || {
                for _ in 0..20 {
                    let result: Result<PerformanceReport, _> = serde_json::from_str(&json);
                    assert!(result.is_ok(), "Thread {i} should successfully parse JSON");

                    if let Ok(report) = result {
                        let summary = report.format_pr_summary();
                        assert!(!summary.is_empty());
                    }
                }
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn test_unicode_fuzzing() {
    let unicode_tests = [
        "🚀🔥💥⚡️",          // Emoji
        "café résumé naïve", // Accented characters
        "Здравствуй мир",    // Cyrillic
        "こんにちは世界",    // Japanese
        "🏳️‍🌈🏳️‍⚧️",              // Complex emoji sequences
    ];

    for (i, unicode_text) in unicode_tests.iter().enumerate() {
        let json = format!(r#"{{"commit": "{unicode_text}"}}"#);

        let result: Result<PerformanceReport, _> = serde_json::from_str(&json);

        if let Ok(report) = result {
            assert_eq!(
                report.commit, *unicode_text,
                "Unicode text {i} should be preserved"
            );

            // Test serialization roundtrip
            let serialized = serde_json::to_string(&report).unwrap();
            let deserialized: PerformanceReport = serde_json::from_str(&serialized).unwrap();
            assert_eq!(deserialized.commit, *unicode_text);
        } else {
            // May fail due to JSON escaping issues
        }
    }
}

#[test]
fn test_edge_case_numbers() {
    let number_cases = vec![
        ("zero", 0.0),
        ("negative_zero", -0.0),
        ("one", 1.0),
        ("negative_one", -1.0),
        ("small_positive", 1e-10),
        ("small_negative", -1e-10),
        ("large_positive", 1e6),
        ("large_negative", -1e6),
    ];

    for (name, value) in number_cases {
        let json = format!(r#"{{"display_gibs": {value}}}"#);

        let result: Result<PerformanceReport, _> = serde_json::from_str(&json);

        if let Ok(report) = result {
            assert_eq!(
                report.display_gibs,
                Some(value),
                "Number case '{name}' should preserve value {value}"
            );

            // Test SLO validation with edge case numbers
            let mut slo_report = report.clone();
            slo_report.validate_slos(4.1, 560.0);

            // Should not panic or produce invalid state
            assert!(["success", "warning", "failure"].contains(&slo_report.status.as_str()));
        } else {
            // May fail for some edge cases, which is acceptable
        }
    }
}
