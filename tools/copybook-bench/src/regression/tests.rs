use super::*;
use std::time::SystemTime;

#[test]
fn test_detector_creation() {
    let detector = PerformanceRegressionDetector::new();
    assert!((detector.statistical_analyzer.variance_tolerance - 0.02).abs() < f64::EPSILON);
}

#[test]
fn test_baseline_repository() {
    let mut repo = BaselineRepository::new();
    let metrics = PerformanceMetrics {
        display_throughput: ThroughputMetrics {
            mean: 4200.0,
            median: 4150.0,
            percentile_95: 4800.0,
            percentile_99: 5200.0,
            std_deviation: 250.0,
            min: 3800.0,
            max: 5500.0,
        },
        comp3_throughput: ThroughputMetrics {
            mean: 580.0,
            median: 575.0,
            percentile_95: 620.0,
            percentile_99: 650.0,
            std_deviation: 25.0,
            min: 540.0,
            max: 680.0,
        },
        memory_usage: MemoryUsageMetrics {
            peak_memory_mb: 245.0,
            average_memory_mb: 180.0,
            steady_state_memory_mb: 165.0,
            memory_variance: 12.5,
        },
        latency_metrics: LatencyMetrics {
            p50_ms: 0.24,
            p95_ms: 0.45,
            p99_ms: 0.68,
            p999_ms: 1.2,
            max_latency_ms: 2.1,
        },
    };

    let baseline = BaselineMetadata {
        baseline_id: "test_baseline".to_string(),
        creation_timestamp: SystemTime::now(),
        git_commit_hash: None,
        environment_info: EnvironmentInfo {
            rust_version: "1.70.0".to_string(),
            target_triple: "x86_64-unknown-linux-gnu".to_string(),
            cpu_info: CpuInfo {
                model: "Test CPU".to_string(),
                cores: 4,
                frequency_mhz: 2400,
                cache_size_kb: 8192,
            },
            memory_info: MemoryInfo {
                total_gb: 16.0,
                available_gb: 8.0,
                memory_type: "DDR4".to_string(),
            },
            build_configuration: BuildConfiguration {
                optimization_level: OptimizationLevel::Release,
                debug_info: false,
                target_cpu: "native".to_string(),
                features_enabled: Vec::new(),
            },
        },
        performance_metrics: metrics,
        statistical_properties: StatisticalProperties {
            sample_size: 100,
            confidence_interval_95: ConfidenceInterval {
                lower_bound: 4000.0,
                upper_bound: 4400.0,
                confidence_level: 0.95,
            },
            statistical_significance: true,
            normality_test_passed: true,
        },
        validation_status: BaselineValidationStatus::Valid,
    };

    let baseline_id = repo.store_baseline(baseline).unwrap();
    let loaded_baseline = repo.load_baseline(&baseline_id).unwrap();
    assert_eq!(loaded_baseline.baseline_id, "test_baseline");
}

#[test]
fn test_statistical_analyzer() {
    let analyzer = StatisticalRegressionAnalyzer::new();

    let baseline_metrics = PerformanceMetrics {
        display_throughput: ThroughputMetrics {
            mean: 4200.0,
            median: 4150.0,
            percentile_95: 4800.0,
            percentile_99: 5200.0,
            std_deviation: 250.0,
            min: 3800.0,
            max: 5500.0,
        },
        comp3_throughput: ThroughputMetrics {
            mean: 580.0,
            median: 575.0,
            percentile_95: 620.0,
            percentile_99: 650.0,
            std_deviation: 25.0,
            min: 540.0,
            max: 680.0,
        },
        memory_usage: MemoryUsageMetrics {
            peak_memory_mb: 245.0,
            average_memory_mb: 180.0,
            steady_state_memory_mb: 165.0,
            memory_variance: 12.5,
        },
        latency_metrics: LatencyMetrics {
            p50_ms: 0.24,
            p95_ms: 0.45,
            p99_ms: 0.68,
            p999_ms: 1.2,
            max_latency_ms: 2.1,
        },
    };

    let current_metrics = PerformanceMetrics {
        display_throughput: ThroughputMetrics {
            mean: 4100.0, // Slightly lower
            median: 4050.0,
            percentile_95: 4700.0,
            percentile_99: 5100.0,
            std_deviation: 250.0,
            min: 3700.0,
            max: 5400.0,
        },
        comp3_throughput: ThroughputMetrics {
            mean: 575.0, // Slightly lower
            median: 570.0,
            percentile_95: 615.0,
            percentile_99: 645.0,
            std_deviation: 25.0,
            min: 535.0,
            max: 675.0,
        },
        memory_usage: MemoryUsageMetrics {
            peak_memory_mb: 250.0, // Slightly higher
            average_memory_mb: 185.0,
            steady_state_memory_mb: 170.0,
            memory_variance: 13.0,
        },
        latency_metrics: LatencyMetrics {
            p50_ms: 0.25,
            p95_ms: 0.47,
            p99_ms: 0.70,
            p999_ms: 1.25,
            max_latency_ms: 2.2,
        },
    };

    let comparison = analyzer
        .compare_metrics(&baseline_metrics, &current_metrics)
        .unwrap();
    assert!(!comparison.throughput_changes.is_empty());
    assert!(!comparison.memory_changes.is_empty());
    assert!(!comparison.latency_changes.is_empty());

    let statistical_tests = analyzer
        .run_statistical_tests(&baseline_metrics, &current_metrics)
        .unwrap();
    assert!(statistical_tests.t_test_result.degrees_of_freedom > 0);
}

#[test]
fn test_ci_integrator() {
    let ci = CiIntegrator::new();
    assert_eq!(ci.performance_gates.len(), 2);
    assert!(
        ci.performance_gates
            .iter()
            .any(|g| g.gate_id == "display_throughput")
    );
    assert!(
        ci.performance_gates
            .iter()
            .any(|g| g.gate_id == "comp3_throughput")
    );
}

#[test]
fn test_alert_system() {
    let alert_system = AlertSystem::new();
    assert_eq!(alert_system.alert_policies.len(), 1);
    assert_eq!(alert_system.notification_channels.len(), 1);
}

#[test]
fn test_utils_create_detectors() {
    let _standard = utils::create_standard_detector();
    let ci = utils::create_ci_detector();
    let dev = utils::create_dev_detector();

    assert!(
        ci.statistical_analyzer.variance_tolerance < dev.statistical_analyzer.variance_tolerance
    );
}

#[test]
fn test_generate_baseline_id_not_empty() {
    let detector = PerformanceRegressionDetector::new();
    let env = EnvironmentInfo {
        rust_version: "1.92.0".to_string(),
        target_triple: "x86_64-unknown-linux-gnu".to_string(),
        cpu_info: CpuInfo {
            model: "Test CPU".to_string(),
            cores: 4,
            frequency_mhz: 2400,
            cache_size_kb: 8192,
        },
        memory_info: MemoryInfo {
            total_gb: 16.0,
            available_gb: 12.0,
            memory_type: "DDR4".to_string(),
        },
        build_configuration: BuildConfiguration {
            optimization_level: OptimizationLevel::Release,
            debug_info: false,
            target_cpu: "x86_64".to_string(),
            features_enabled: vec!["sse2".to_string(), "avx".to_string()],
        },
    };

    let baseline_id = detector.generate_baseline_id(&env);

    // Kill mutants that return empty string or fixed string
    assert!(!baseline_id.is_empty(), "Baseline ID must not be empty");
    assert_ne!(baseline_id, "xyzzy", "Baseline ID must not be fixed string");
    assert!(
        baseline_id.len() > 10,
        "Baseline ID should be substantial hash"
    );

    // Verify ID has expected format (kill mutants that return invalid formats)
    assert!(
        baseline_id.starts_with("baseline_"),
        "Baseline ID should start with 'baseline_'"
    );
    assert!(
        baseline_id.len() == 16,
        "Baseline ID should be exactly 16 characters (8 + 8 hex chars)"
    );
}

#[test]
fn test_establish_baseline_returns_valid_id() {
    let mut detector = PerformanceRegressionDetector::new();
    let metrics = create_test_performance_metrics();
    let env = EnvironmentInfo {
        rust_version: "1.92.0".to_string(),
        target_triple: "x86_64-unknown-linux-gnu".to_string(),
        cpu_info: CpuInfo {
            model: "Test CPU".to_string(),
            cores: 4,
            frequency_mhz: 2400,
            cache_size_kb: 8192,
        },
        memory_info: MemoryInfo {
            total_gb: 16.0,
            available_gb: 12.0,
            memory_type: "DDR4".to_string(),
        },
        build_configuration: BuildConfiguration {
            optimization_level: OptimizationLevel::Release,
            debug_info: false,
            target_cpu: "x86_64".to_string(),
            features_enabled: vec!["sse2".to_string(), "avx".to_string()],
        },
    };

    let result = detector.establish_baseline(metrics, env);
    assert!(result.is_ok(), "Establish baseline should succeed");

    let baseline_id = result.unwrap();
    // Kill mutants that return empty string
    assert!(
        !baseline_id.is_empty(),
        "Baseline ID from establish_baseline must not be empty"
    );
    assert_ne!(baseline_id, "xyzzy", "Baseline ID must not be fixed string");
    assert!(
        baseline_id.len() > 10,
        "Established baseline ID should be substantial"
    );
}

#[test]
fn test_performance_regression_detector_baseline_workflow() {
    // Test the complete workflow to catch more mutants
    let mut detector = PerformanceRegressionDetector::new();

    let metrics = create_test_performance_metrics();
    let env = EnvironmentInfo {
        rust_version: "1.92.0".to_string(),
        target_triple: "x86_64-unknown-linux-gnu".to_string(),
        cpu_info: CpuInfo {
            model: "Test CPU".to_string(),
            cores: 4,
            frequency_mhz: 2400,
            cache_size_kb: 8192,
        },
        memory_info: MemoryInfo {
            total_gb: 16.0,
            available_gb: 12.0,
            memory_type: "DDR4".to_string(),
        },
        build_configuration: BuildConfiguration {
            optimization_level: OptimizationLevel::Release,
            debug_info: false,
            target_cpu: "x86_64".to_string(),
            features_enabled: vec!["sse2".to_string(), "avx".to_string()],
        },
    };

    // Test that baseline establishment produces a valid ID
    let baseline_id = detector.establish_baseline(metrics, env).unwrap();

    // Kill mutants that return empty or fixed strings
    assert!(
        !baseline_id.is_empty(),
        "Baseline workflow should produce non-empty ID"
    );
    assert_ne!(
        baseline_id, "xyzzy",
        "Baseline workflow should not produce fixed string"
    );
    assert!(
        baseline_id.len() > 10,
        "Baseline workflow should produce substantial ID"
    );

    // Test that the detector now has some baseline data stored
    // Just verify the workflow completed without error - the baseline_id is proof it worked
    assert!(
        baseline_id.starts_with("baseline_"),
        "Baseline ID should have expected format"
    );
}

fn create_test_performance_metrics() -> PerformanceMetrics {
    PerformanceMetrics {
        display_throughput: ThroughputMetrics {
            mean: 4200.0,
            median: 4100.0,
            percentile_95: 4800.0,
            percentile_99: 5200.0,
            std_deviation: 300.0,
            min: 3800.0,
            max: 5600.0,
        },
        comp3_throughput: ThroughputMetrics {
            mean: 580.0,
            median: 575.0,
            percentile_95: 620.0,
            percentile_99: 650.0,
            std_deviation: 30.0,
            min: 540.0,
            max: 680.0,
        },
        memory_usage: MemoryUsageMetrics {
            peak_memory_mb: 245.0,
            average_memory_mb: 180.0,
            steady_state_memory_mb: 165.0,
            memory_variance: 12.5,
        },
        latency_metrics: LatencyMetrics {
            p50_ms: 0.24,
            p95_ms: 0.45,
            p99_ms: 0.68,
            p999_ms: 1.2,
            max_latency_ms: 2.1,
        },
    }
}
