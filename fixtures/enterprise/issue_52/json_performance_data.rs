//! JSON Performance Data Fixtures for Issue #52 Machine-Readable Benchmark Reporting
//!
//! Provides realistic test data for perf.json schema validation and Python utility testing
//! Supports comprehensive testing of copybook-rs performance metrics and enterprise compliance

use serde_json::{Value, Map};
use std::collections::HashMap;

/// Performance data fixture representing current copybook-rs achievement levels
/// DISPLAY: 4.22+ GiB/s (52x enterprise floor), COMP-3: 571+ MiB/s (14x enterprise floor)
#[derive(Debug, Clone)]
pub struct PerformanceFixture {
    pub display_gibs: f64,
    pub comp3_mibs: f64,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
    pub metadata: HashMap<String, Value>,
}

impl PerformanceFixture {
    pub fn new(display_gibs: f64, comp3_mibs: f64) -> Self {
        Self {
            display_gibs,
            comp3_mibs,
            warnings: Vec::new(),
            errors: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    pub fn with_warnings(mut self, warnings: Vec<String>) -> Self {
        self.warnings = warnings;
        self
    }

    pub fn with_errors(mut self, errors: Vec<String>) -> Self {
        self.errors = errors;
        self
    }

    pub fn with_metadata(mut self, key: &str, value: Value) -> Self {
        self.metadata.insert(key.to_string(), value);
        self
    }

    pub fn to_json(&self) -> serde_json::Result<Value> {
        let mut map = Map::new();
        map.insert("display_gibs".to_string(), Value::Number(
            serde_json::Number::from_f64(self.display_gibs).unwrap()
        ));
        map.insert("comp3_mibs".to_string(), Value::Number(
            serde_json::Number::from_f64(self.comp3_mibs).unwrap()
        ));
        map.insert("warnings".to_string(), Value::Array(
            self.warnings.iter().map(|w| Value::String(w.clone())).collect()
        ));
        map.insert("errors".to_string(), Value::Array(
            self.errors.iter().map(|e| Value::String(e.clone())).collect()
        ));

        // Add metadata if present
        for (key, value) in &self.metadata {
            map.insert(key.clone(), value.clone());
        }

        Ok(Value::Object(map))
    }
}

/// Create current achievement performance fixture (exceeds all targets)
pub fn create_current_achievement_fixture() -> PerformanceFixture {
    PerformanceFixture::new(4.22, 571.0)
        .with_metadata("test_scenario", Value::String("current_achievement".to_string()))
        .with_metadata("safety_margin_display", Value::Number(serde_json::Number::from(52)))
        .with_metadata("safety_margin_comp3", Value::Number(serde_json::Number::from(14)))
        .with_metadata("enterprise_compliant", Value::Bool(true))
        .with_metadata("variance_percent", Value::Number(serde_json::Number::from_f64(1.2).unwrap()))
}

/// Create high performance fixture (exceeds current levels)
pub fn create_high_performance_fixture() -> PerformanceFixture {
    PerformanceFixture::new(5.1, 650.0)
        .with_metadata("test_scenario", Value::String("high_performance".to_string()))
        .with_metadata("safety_margin_display", Value::Number(serde_json::Number::from(63)))
        .with_metadata("safety_margin_comp3", Value::Number(serde_json::Number::from(16)))
        .with_metadata("enterprise_compliant", Value::Bool(true))
        .with_metadata("variance_percent", Value::Number(serde_json::Number::from_f64(0.8).unwrap()))
}

/// Create performance floor scenario (just above enterprise minimums)
pub fn create_performance_floor_fixture() -> PerformanceFixture {
    PerformanceFixture::new(0.085, 42.0)
        .with_warnings(vec![
            "DISPLAY throughput near 80 MB/s enterprise floor".to_string(),
            "COMP-3 throughput near 40 MB/s enterprise floor".to_string(),
            "Performance monitoring recommended".to_string(),
        ])
        .with_metadata("test_scenario", Value::String("performance_floor".to_string()))
        .with_metadata("safety_margin_display", Value::Number(serde_json::Number::from(1)))
        .with_metadata("safety_margin_comp3", Value::Number(serde_json::Number::from(1)))
        .with_metadata("enterprise_compliant", Value::Bool(true))
        .with_metadata("variance_percent", Value::Number(serde_json::Number::from_f64(4.8).unwrap()))
}

/// Create performance violation scenario (below enterprise floors)
pub fn create_performance_violation_fixture() -> PerformanceFixture {
    PerformanceFixture::new(0.065, 35.0)
        .with_errors(vec![
            "DISPLAY throughput below 80 MB/s enterprise floor".to_string(),
            "COMP-3 throughput below 40 MB/s enterprise floor".to_string(),
            "Enterprise compliance violation detected".to_string(),
        ])
        .with_warnings(vec![
            "Performance regression detected".to_string(),
            "Immediate investigation required".to_string(),
        ])
        .with_metadata("test_scenario", Value::String("performance_violation".to_string()))
        .with_metadata("safety_margin_display", Value::Number(serde_json::Number::from(-1)))
        .with_metadata("safety_margin_comp3", Value::Number(serde_json::Number::from(-1)))
        .with_metadata("enterprise_compliant", Value::Bool(false))
        .with_metadata("variance_percent", Value::Number(serde_json::Number::from_f64(15.2).unwrap()))
}

/// Create high variance scenario (performance within range but unstable)
pub fn create_high_variance_fixture() -> PerformanceFixture {
    PerformanceFixture::new(4.18, 568.0)
        .with_warnings(vec![
            "Performance variance above 5% tolerance".to_string(),
            "Consider performance tuning".to_string(),
        ])
        .with_metadata("test_scenario", Value::String("high_variance".to_string()))
        .with_metadata("safety_margin_display", Value::Number(serde_json::Number::from(51)))
        .with_metadata("safety_margin_comp3", Value::Number(serde_json::Number::from(14)))
        .with_metadata("enterprise_compliant", Value::Bool(true))
        .with_metadata("variance_percent", Value::Number(serde_json::Number::from_f64(6.7).unwrap()))
}

/// Create timeout scenario (benchmark execution timeout)
pub fn create_timeout_scenario_fixture() -> PerformanceFixture {
    PerformanceFixture::new(0.0, 0.0)
        .with_errors(vec![
            "Benchmark execution timeout exceeded".to_string(),
            "Unable to measure performance within time limit".to_string(),
            "CI/CD pipeline intervention required".to_string(),
        ])
        .with_metadata("test_scenario", Value::String("timeout_scenario".to_string()))
        .with_metadata("timeout_seconds", Value::Number(serde_json::Number::from(1800)))
        .with_metadata("enterprise_compliant", Value::Bool(false))
        .with_metadata("execution_status", Value::String("timeout".to_string()))
}

/// Create mixed workload fixture (enterprise processing scenario)
pub fn create_mixed_workload_fixture() -> PerformanceFixture {
    PerformanceFixture::new(3.85, 545.0)
        .with_metadata("test_scenario", Value::String("mixed_workload".to_string()))
        .with_metadata("display_fields", Value::Number(serde_json::Number::from(15)))
        .with_metadata("comp3_fields", Value::Number(serde_json::Number::from(8)))
        .with_metadata("binary_fields", Value::Number(serde_json::Number::from(3)))
        .with_metadata("record_count", Value::Number(serde_json::Number::from(1000000)))
        .with_metadata("compliance_overhead_percent", Value::Number(serde_json::Number::from_f64(4.7).unwrap()))
        .with_metadata("enterprise_compliant", Value::Bool(true))
        .with_metadata("variance_percent", Value::Number(serde_json::Number::from_f64(2.1).unwrap()))
}

/// Create enterprise compliance fixture (regulatory processing)
pub fn create_enterprise_compliance_fixture() -> PerformanceFixture {
    PerformanceFixture::new(4.12, 589.0)
        .with_warnings(vec![
            "HIPAA compliance overhead detected".to_string(),
            "Audit logging impact measured".to_string(),
        ])
        .with_metadata("test_scenario", Value::String("enterprise_compliance".to_string()))
        .with_metadata("compliance_profiles", Value::Array(vec![
            Value::String("HIPAA".to_string()),
            Value::String("SOX".to_string()),
            Value::String("PCI_DSS".to_string()),
        ]))
        .with_metadata("audit_overhead_percent", Value::Number(serde_json::Number::from_f64(3.2).unwrap()))
        .with_metadata("phi_fields_processed", Value::Number(serde_json::Number::from(127)))
        .with_metadata("enterprise_compliant", Value::Bool(true))
        .with_metadata("variance_percent", Value::Number(serde_json::Number::from_f64(1.8).unwrap()))
}

/// Create all performance test fixtures for comprehensive testing
pub fn create_all_performance_fixtures() -> HashMap<String, PerformanceFixture> {
    let mut fixtures = HashMap::new();

    fixtures.insert("current_achievement".to_string(), create_current_achievement_fixture());
    fixtures.insert("high_performance".to_string(), create_high_performance_fixture());
    fixtures.insert("performance_floor".to_string(), create_performance_floor_fixture());
    fixtures.insert("performance_violation".to_string(), create_performance_violation_fixture());
    fixtures.insert("high_variance".to_string(), create_high_variance_fixture());
    fixtures.insert("timeout_scenario".to_string(), create_timeout_scenario_fixture());
    fixtures.insert("mixed_workload".to_string(), create_mixed_workload_fixture());
    fixtures.insert("enterprise_compliance".to_string(), create_enterprise_compliance_fixture());

    fixtures
}

/// Generate sample perf.json files for Python utility testing
pub fn generate_sample_perf_json_files() -> HashMap<String, String> {
    let fixtures = create_all_performance_fixtures();
    let mut json_files = HashMap::new();

    for (name, fixture) in fixtures {
        if let Ok(json_value) = fixture.to_json() {
            if let Ok(json_string) = serde_json::to_string_pretty(&json_value) {
                json_files.insert(format!("{}.json", name), json_string);
            }
        }
    }

    json_files
}

/// Create edge case performance fixtures for comprehensive testing
pub fn create_edge_case_fixtures() -> Vec<PerformanceFixture> {
    vec![
        // Extremely high performance (stress test)
        PerformanceFixture::new(10.5, 1200.0)
            .with_metadata("test_scenario", Value::String("extreme_high_performance".to_string())),

        // Very low but valid performance
        PerformanceFixture::new(0.081, 40.1)
            .with_warnings(vec!["Borderline enterprise compliance".to_string()])
            .with_metadata("test_scenario", Value::String("borderline_compliance".to_string())),

        // Single metric failure
        PerformanceFixture::new(4.5, 25.0)
            .with_errors(vec!["COMP-3 throughput below enterprise floor".to_string()])
            .with_metadata("test_scenario", Value::String("single_metric_failure".to_string())),

        // Memory pressure scenario
        PerformanceFixture::new(3.2, 480.0)
            .with_warnings(vec!["Memory usage approaching 256 MiB limit".to_string()])
            .with_metadata("test_scenario", Value::String("memory_pressure".to_string()))
            .with_metadata("memory_usage_mb", Value::Number(serde_json::Number::from(248))),

        // CI/CD pipeline scenario
        PerformanceFixture::new(4.0, 550.0)
            .with_metadata("test_scenario", Value::String("cicd_pipeline".to_string()))
            .with_metadata("pipeline_stage", Value::String("performance_gate".to_string()))
            .with_metadata("commit_sha", Value::String("abc123def456".to_string())),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_current_achievement_fixture() {
        let fixture = create_current_achievement_fixture();
        assert_eq!(fixture.display_gibs, 4.22);
        assert_eq!(fixture.comp3_mibs, 571.0);
        assert!(fixture.warnings.is_empty());
        assert!(fixture.errors.is_empty());

        let json = fixture.to_json().unwrap();
        assert!(json["enterprise_compliant"].as_bool().unwrap());
    }

    #[test]
    fn test_performance_violation_fixture() {
        let fixture = create_performance_violation_fixture();
        assert_eq!(fixture.display_gibs, 0.065);
        assert_eq!(fixture.comp3_mibs, 35.0);
        assert!(!fixture.warnings.is_empty());
        assert!(!fixture.errors.is_empty());

        let json = fixture.to_json().unwrap();
        assert!(!json["enterprise_compliant"].as_bool().unwrap());
    }

    #[test]
    fn test_timeout_scenario_fixture() {
        let fixture = create_timeout_scenario_fixture();
        assert_eq!(fixture.display_gibs, 0.0);
        assert_eq!(fixture.comp3_mibs, 0.0);
        assert!(!fixture.errors.is_empty());
        assert!(fixture.errors.iter().any(|e| e.contains("timeout")));
    }

    #[test]
    fn test_all_fixtures_json_serialization() {
        let fixtures = create_all_performance_fixtures();
        for (name, fixture) in fixtures {
            let json = fixture.to_json().unwrap();
            assert!(json.is_object(), "Fixture {} must serialize to JSON object", name);

            let obj = json.as_object().unwrap();
            assert!(obj.contains_key("display_gibs"));
            assert!(obj.contains_key("comp3_mibs"));
            assert!(obj.contains_key("warnings"));
            assert!(obj.contains_key("errors"));
        }
    }

    #[test]
    fn test_sample_json_file_generation() {
        let json_files = generate_sample_perf_json_files();
        assert!(!json_files.is_empty());

        for (filename, content) in json_files {
            assert!(filename.ends_with(".json"));
            assert!(!content.trim().is_empty());

            // Verify JSON is valid
            let _: Value = serde_json::from_str(&content).unwrap();
        }
    }

    #[test]
    fn test_edge_case_fixtures() {
        let edge_cases = create_edge_case_fixtures();
        assert!(!edge_cases.is_empty());

        for fixture in edge_cases {
            let json = fixture.to_json().unwrap();
            assert!(json.is_object());
            assert!(json["display_gibs"].is_number());
            assert!(json["comp3_mibs"].is_number());
        }
    }
}