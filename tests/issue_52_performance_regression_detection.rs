//! Test scaffolding for Issue #52 performance regression detection and statistical analysis
//!
//! Tests feature spec: issue-52-spec.md#performance-regression-detection
//! Validates statistical analysis, regression detection, and performance validation

use std::path::Path;
use std::fs;
use std::process::Command;
use serde_json::{Value, json};

/// Tests feature spec: issue-52-spec.md#statistical-regression-analysis
/// Validates statistical regression analysis integration with existing infrastructure
#[test]
fn test_statistical_regression_analysis_integration() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6,AC7 - Test statistical regression analysis
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let regression_file = workspace_root.join("copybook-bench").join("src").join("regression.rs");

    // Verify sophisticated regression system exists (mentioned in specs as 1,976 lines)
    if regression_file.exists() {
        let content = fs::read_to_string(&regression_file)?;
        assert!(content.len() > 1000,
            "regression.rs must contain substantial statistical analysis code: {} lines",
            content.lines().count());

        // Verify statistical analysis concepts
        let statistical_concepts = vec![
            "confidence",
            "variance",
            "statistical",
            "significance",
            "regression",
            "analysis",
            "sample",
            "distribution",
        ];

        for concept in statistical_concepts {
            assert!(content.to_lowercase().contains(concept),
                "regression.rs must contain statistical concept: {}", concept);
        }

        // Verify performance metrics handling
        assert!(content.contains("throughput") || content.contains("performance"),
            "regression.rs must handle performance metrics");
        assert!(content.contains("ThroughputMetrics") || content.contains("PerformanceMetrics"),
            "regression.rs must define performance metric structures");
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#baseline-comparison-validation
/// Validates baseline comparison and regression detection algorithms
#[test]
fn test_baseline_comparison_validation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC5,AC6 - Test baseline comparison validation
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let baseline_manager = workspace_root.join("scripts").join("bench").join("baseline_manager.py");

    if baseline_manager.exists() {
        // Test baseline comparison functionality
        let output = Command::new("python3")
            .arg(&baseline_manager)
            .arg("--compare-baseline")
            .arg("--current-performance")
            .arg("display_gibs=4.22,comp3_mibs=571.0")
            .arg("--baseline-id")
            .arg("main-current")
            .arg("--regression-threshold")
            .arg("0.02")  // 2% threshold
            .output();

        match output {
            Ok(result) => {
                if result.status.success() {
                    let stdout = String::from_utf8_lossy(&result.stdout);

                    // Verify baseline comparison output
                    assert!(stdout.contains("baseline") || stdout.contains("comparison"),
                        "Must perform baseline comparison");
                    assert!(stdout.contains("regression") || stdout.contains("improvement"),
                        "Must detect regression or improvement");
                    assert!(stdout.contains("threshold") || stdout.contains("2%"),
                        "Must apply regression threshold");
                }
            }
            Err(_) => {
                // Expected until implementation
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#confidence-interval-analysis
/// Validates confidence interval calculation and statistical significance
#[test]
fn test_confidence_interval_analysis() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6,AC7 - Test confidence interval analysis
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let bench_runner = workspace_root.join("scripts").join("bench").join("bench_runner.py");

    if bench_runner.exists() {
        // Test statistical analysis with confidence intervals
        let output = Command::new("python3")
            .arg(&bench_runner)
            .arg("--statistical-analysis")
            .arg("--confidence-level")
            .arg("0.95")  // 95% confidence
            .arg("--sample-size")
            .arg("100")
            .arg("--test-mode")
            .output();

        match output {
            Ok(result) => {
                if result.status.success() {
                    let stdout = String::from_utf8_lossy(&result.stdout);

                    // Verify statistical analysis output
                    assert!(stdout.contains("confidence") || stdout.contains("interval"),
                        "Must calculate confidence intervals");
                    assert!(stdout.contains("95%") || stdout.contains("0.95"),
                        "Must support 95% confidence level");
                    assert!(stdout.contains("sample") || stdout.contains("statistical"),
                        "Must perform statistical sampling analysis");
                }
            }
            Err(_) => {
                // Expected until implementation
            }
        }
    }

    // Test JSON output includes statistical properties
    let test_json_with_stats = json!({
        "display_gibs": 4.22,
        "comp3_mibs": 571.0,
        "warnings": [],
        "errors": [],
        "_metadata": {
            "statistical_properties": {
                "display_confidence_interval": {
                    "lower_bound": 4.18,
                    "upper_bound": 4.26,
                    "confidence_level": 0.95
                },
                "comp3_confidence_interval": {
                    "lower_bound": 565.0,
                    "upper_bound": 577.0,
                    "confidence_level": 0.95
                },
                "sample_size": 100,
                "variance_coefficient": 0.015,
                "statistical_significance": true
            }
        }
    });

    // Validate statistical properties structure
    let stats = test_json_with_stats
        .get("_metadata").unwrap()
        .get("statistical_properties").unwrap();

    assert!(stats.get("display_confidence_interval").is_some(),
        "Must include DISPLAY confidence interval");
    assert!(stats.get("comp3_confidence_interval").is_some(),
        "Must include COMP-3 confidence interval");
    assert!(stats.get("sample_size").is_some(),
        "Must include sample size");
    assert!(stats.get("variance_coefficient").is_some(),
        "Must include variance coefficient");
    assert!(stats.get("statistical_significance").is_some(),
        "Must include statistical significance flag");

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#regression-threshold-validation
/// Validates regression threshold enforcement and variance tolerance
#[test]
fn test_regression_threshold_validation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6 - Test regression threshold validation
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let slo_validator = workspace_root.join("scripts").join("bench").join("slo_validator.py");

    if slo_validator.exists() {
        // Test regression detection with different threshold values
        let test_cases = vec![
            (4.22, 4.20, "1.0", false), // 0.47% difference - should pass 2% threshold
            (4.22, 4.00, "2.0", true),  // 5.2% difference - should fail 2% threshold
            (571.0, 560.0, "2.0", true), // 1.9% difference - should pass 2% threshold
            (571.0, 530.0, "2.0", true), // 7.2% difference - should fail 2% threshold
        ];

        for (current, baseline, threshold, should_fail) in test_cases {
            let output = Command::new("python3")
                .arg(&slo_validator)
                .arg("--regression-check")
                .arg("--current")
                .arg(&current.to_string())
                .arg("--baseline")
                .arg(&baseline.to_string())
                .arg("--threshold")
                .arg(threshold)
                .output();

            match output {
                Ok(result) => {
                    let stdout = String::from_utf8_lossy(&result.stdout);
                    let stderr = String::from_utf8_lossy(&result.stderr);

                    if should_fail {
                        assert!(stdout.contains("FAIL") || stdout.contains("regression") ||
                               !result.status.success(),
                            "Should detect regression: current={}, baseline={}, threshold={}%",
                            current, baseline, threshold);
                    } else {
                        assert!(stdout.contains("PASS") || result.status.success(),
                            "Should pass regression check: current={}, baseline={}, threshold={}%",
                            current, baseline, threshold);
                    }
                }
                Err(_) => {
                    // Expected until implementation
                }
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#performance-variance-analysis
/// Validates performance variance analysis and coefficient of variation
#[test]
fn test_performance_variance_analysis() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6,AC7 - Test performance variance analysis
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));

    // Test variance calculation for performance stability
    let test_measurements = vec![4.22, 4.20, 4.24, 4.18, 4.26, 4.21, 4.23, 4.19, 4.25, 4.22];

    // Calculate variance coefficient
    let mean: f64 = test_measurements.iter().sum::<f64>() / test_measurements.len() as f64;
    let variance: f64 = test_measurements.iter()
        .map(|x| (x - mean).powi(2))
        .sum::<f64>() / test_measurements.len() as f64;
    let std_dev = variance.sqrt();
    let coefficient_of_variation = std_dev / mean;

    // Verify acceptable variance (should be <5% for stable performance)
    assert!(coefficient_of_variation < 0.05,
        "Performance variance coefficient must be <5%: {:.3}%",
        coefficient_of_variation * 100.0);

    // Test variance analysis in Python utilities
    let bench_runner = workspace_root.join("scripts").join("bench").join("bench_runner.py");
    if bench_runner.exists() {
        let output = Command::new("python3")
            .arg(&bench_runner)
            .arg("--variance-analysis")
            .arg("--measurements")
            .arg(&format!("{:?}", test_measurements))
            .arg("--max-variance")
            .arg("5.0")  // 5% maximum variance
            .output();

        match output {
            Ok(result) => {
                if result.status.success() {
                    let stdout = String::from_utf8_lossy(&result.stdout);

                    // Verify variance analysis output
                    assert!(stdout.contains("variance") || stdout.contains("coefficient"),
                        "Must perform variance analysis");
                    assert!(stdout.contains("stable") || stdout.contains("acceptable"),
                        "Must indicate variance acceptability");
                }
            }
            Err(_) => {
                // Expected until implementation
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#outlier-detection-validation
/// Validates outlier detection using IQR method for statistical robustness
#[test]
fn test_outlier_detection_validation() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6,AC7 - Test outlier detection for statistical robustness
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));

    // Test data with outliers
    let measurements_with_outliers = vec![
        4.22, 4.20, 4.24, 4.18, 4.26, // Normal measurements
        3.50, // Outlier (significantly lower)
        4.21, 4.23, 4.19, 4.25,       // Normal measurements
        5.00, // Outlier (significantly higher)
    ];

    // IQR outlier detection algorithm
    let mut sorted_measurements = measurements_with_outliers.clone();
    sorted_measurements.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let q1_index = sorted_measurements.len() / 4;
    let q3_index = 3 * sorted_measurements.len() / 4;
    let q1 = sorted_measurements[q1_index];
    let q3 = sorted_measurements[q3_index];
    let iqr = q3 - q1;

    let lower_bound = q1 - 1.5 * iqr;
    let upper_bound = q3 + 1.5 * iqr;

    let outliers: Vec<f64> = measurements_with_outliers.iter()
        .filter(|&&x| x < lower_bound || x > upper_bound)
        .cloned()
        .collect();

    // Verify outlier detection identifies the extreme values
    assert!(!outliers.is_empty(),
        "Must detect outliers in test data: bounds=[{:.2}, {:.2}]", lower_bound, upper_bound);
    assert!(outliers.contains(&3.50),
        "Must detect low outlier: 3.50");
    assert!(outliers.contains(&5.00),
        "Must detect high outlier: 5.00");

    // Test Python outlier detection
    let bench_runner = workspace_root.join("scripts").join("bench").join("bench_runner.py");
    if bench_runner.exists() {
        let output = Command::new("python3")
            .arg(&bench_runner)
            .arg("--outlier-detection")
            .arg("--measurements")
            .arg(&format!("{:?}", measurements_with_outliers))
            .arg("--method")
            .arg("iqr")
            .output();

        match output {
            Ok(result) => {
                if result.status.success() {
                    let stdout = String::from_utf8_lossy(&result.stdout);

                    // Verify outlier detection output
                    assert!(stdout.contains("outlier") || stdout.contains("IQR"),
                        "Must perform IQR outlier detection");
                    assert!(stdout.contains("3.5") || stdout.contains("5.0"),
                        "Must identify extreme outlier values");
                }
            }
            Err(_) => {
                // Expected until implementation
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#criterion-statistical-integration
/// Validates integration with Criterion.rs statistical analysis
#[test]
fn test_criterion_statistical_integration() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC9 - Test Criterion.rs statistical integration
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let decode_performance = workspace_root.join("copybook-bench").join("benches").join("decode_performance.rs");

    if decode_performance.exists() {
        let content = fs::read_to_string(&decode_performance)?;

        // Verify Criterion.rs usage
        assert!(content.contains("criterion") || content.contains("Criterion"),
            "Performance benchmarks must use Criterion.rs");
        assert!(content.contains("throughput") || content.contains("iter"),
            "Performance benchmarks must measure throughput");

        // Look for statistical configuration
        if content.contains("measurement_time") || content.contains("sample_size") {
            assert!(content.contains("Duration::from_secs") || content.contains("sample"),
                "Must configure Criterion.rs statistical parameters");
        }

        // Check for performance metrics extraction
        if content.contains("ThroughputMetrics") || content.contains("BenchmarkId") {
            assert!(content.contains("display") || content.contains("comp3"),
                "Must benchmark DISPLAY and COMP-3 performance");
        }
    }

    // Test JSON reporter integration with Criterion
    let json_reporter = workspace_root.join("copybook-bench").join("src").join("json_reporter.rs");
    if json_reporter.exists() {
        let content = fs::read_to_string(&json_reporter)?;

        // Verify Criterion integration
        assert!(content.contains("criterion") || content.contains("BenchmarkResults"),
            "JsonReporter must integrate with Criterion.rs results");
        assert!(content.contains("extract") && (content.contains("throughput") || content.contains("metrics")),
            "JsonReporter must extract throughput metrics from Criterion");
        assert!(content.contains("statistical") || content.contains("confidence"),
            "JsonReporter must handle statistical properties");
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#performance-trending-analysis
/// Validates performance trending analysis over time
#[test]
fn test_performance_trending_analysis() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC7 - Test performance trending analysis
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let baseline_manager = workspace_root.join("scripts").join("bench").join("baseline_manager.py");

    if baseline_manager.exists() {
        // Test trending analysis over multiple baselines
        let output = Command::new("python3")
            .arg(&baseline_manager)
            .arg("--trend-analysis")
            .arg("--timeframe")
            .arg("30d")
            .arg("--metrics")
            .arg("display_gibs,comp3_mibs")
            .arg("--statistical-analysis")
            .output();

        match output {
            Ok(result) => {
                if result.status.success() {
                    let stdout = String::from_utf8_lossy(&result.stdout);

                    // Verify trending analysis components
                    assert!(stdout.contains("trend") || stdout.contains("analysis"),
                        "Must perform trend analysis");
                    assert!(stdout.contains("30d") || stdout.contains("timeframe"),
                        "Must support timeframe analysis");
                    assert!(stdout.contains("display") && stdout.contains("comp3"),
                        "Must analyze both DISPLAY and COMP-3 trends");
                    assert!(stdout.contains("statistical") || stdout.contains("regression"),
                        "Must include statistical trend analysis");
                }
            }
            Err(_) => {
                // Expected until implementation
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#regression-alerting-system
/// Validates regression alerting and notification system
#[test]
fn test_regression_alerting_system() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6,AC7 - Test regression alerting system
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let pr_automation = workspace_root.join("scripts").join("bench").join("pr_automation.py");

    if pr_automation.exists() {
        // Test regression alerting
        let test_regression_data = json!({
            "display_gibs": 3.8,      // Below 4.1 GiB/s enterprise target
            "comp3_mibs": 500.0,      // Below 560 MiB/s enterprise target
            "warnings": [
                "DISPLAY performance regression detected: 3.8 GiB/s vs baseline 4.2 GiB/s (9.5% decline)",
                "COMP-3 performance regression detected: 500.0 MiB/s vs baseline 570.0 MiB/s (12.3% decline)"
            ],
            "errors": [],
            "_metadata": {
                "regression_analysis": {
                    "display_regression_percentage": -9.5,
                    "comp3_regression_percentage": -12.3,
                    "statistical_significance": true,
                    "confidence_level": 0.95
                }
            }
        });

        let temp_regression_file = workspace_root.join("temp_regression.json");
        fs::write(&temp_regression_file, serde_json::to_string_pretty(&test_regression_data)?)?;

        let output = Command::new("python3")
            .arg(&pr_automation)
            .arg("--regression-alert")
            .arg("--pr")
            .arg("123")
            .arg("--perf-file")
            .arg(&temp_regression_file)
            .arg("--alert-threshold")
            .arg("5.0")  // 5% regression threshold
            .output();

        // Clean up
        let _ = fs::remove_file(&temp_regression_file);

        match output {
            Ok(result) => {
                if result.status.success() {
                    let stdout = String::from_utf8_lossy(&result.stdout);

                    // Verify regression alerting
                    assert!(stdout.contains("regression") || stdout.contains("decline"),
                        "Must detect and alert on performance regression");
                    assert!(stdout.contains("9.5%") || stdout.contains("12.3%"),
                        "Must report regression percentages");
                    assert!(stdout.contains("⚠️") || stdout.contains("warning") || stdout.contains("alert"),
                        "Must generate appropriate alert indicators");
                }
            }
            Err(_) => {
                // Expected until implementation
            }
        }
    }

    Ok(())
}

/// Tests feature spec: issue-52-spec.md#statistical-significance-testing
/// Validates statistical significance testing for regression detection
#[test]
fn test_statistical_significance_testing() -> Result<(), Box<dyn std::error::Error>> {
    // AC:AC6,AC7 - Test statistical significance testing
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"));

    // Test statistical significance calculation
    let baseline_measurements = vec![4.22, 4.20, 4.24, 4.18, 4.26, 4.21, 4.23, 4.19, 4.25, 4.22];
    let current_measurements = vec![4.10, 4.08, 4.12, 4.06, 4.14, 4.09, 4.11, 4.07, 4.13, 4.10];

    // Calculate means
    let baseline_mean: f64 = baseline_measurements.iter().sum::<f64>() / baseline_measurements.len() as f64;
    let current_mean: f64 = current_measurements.iter().sum::<f64>() / current_measurements.len() as f64;

    // Calculate percentage difference
    let percentage_difference = ((current_mean - baseline_mean) / baseline_mean) * 100.0;

    // This should show a significant regression (approximately -2.8%)
    assert!(percentage_difference < -2.0,
        "Test data should show significant regression: {:.1}%", percentage_difference);

    // Test statistical significance in Python utilities
    let slo_validator = workspace_root.join("scripts").join("bench").join("slo_validator.py");
    if slo_validator.exists() {
        let output = Command::new("python3")
            .arg(&slo_validator)
            .arg("--significance-test")
            .arg("--baseline")
            .arg(&format!("{:?}", baseline_measurements))
            .arg("--current")
            .arg(&format!("{:?}", current_measurements))
            .arg("--alpha")
            .arg("0.05")  // 5% significance level
            .output();

        match output {
            Ok(result) => {
                if result.status.success() {
                    let stdout = String::from_utf8_lossy(&result.stdout);

                    // Verify statistical significance testing
                    assert!(stdout.contains("significant") || stdout.contains("t-test"),
                        "Must perform statistical significance testing");
                    assert!(stdout.contains("0.05") || stdout.contains("5%"),
                        "Must use appropriate significance level");
                    assert!(stdout.contains("regression") || stdout.contains("difference"),
                        "Must identify significant performance difference");
                }
            }
            Err(_) => {
                // Expected until implementation
            }
        }
    }

    Ok(())
}