// SPDX-License-Identifier: AGPL-3.0-or-later
//! Issue #52 Machine-Readable Benchmark Reporting Test Fixtures
//!
//! Comprehensive fixture collection for testing Python utilities implementation
//! and supporting the machine-readable benchmark reporting infrastructure

use serde_json::Value;
use std::collections::HashMap;
use std::sync::LazyLock;

pub mod json_performance_data;
pub mod python_utility_outputs;
pub mod enterprise_audit_fixtures;
pub mod cicd_integration_fixtures;
pub mod cobol_processing_integration;

#[cfg(test)]
pub mod validation_tests;

pub use json_performance_data::*;
pub use python_utility_outputs::*;
pub use enterprise_audit_fixtures::*;
pub use cicd_integration_fixtures::*;
pub use cobol_processing_integration::*;

/// Centralized fixture loading utilities for Issue #52 test infrastructure
pub struct Issue52FixtureLoader {
    performance_fixtures: HashMap<String, PerformanceFixture>,
    python_outputs: HashMap<String, HashMap<String, Value>>,
    audit_fixtures: HashMap<String, Vec<AuditEvent>>,
    cicd_fixtures: HashMap<String, HashMap<String, Value>>,
    cobol_fixtures: HashMap<String, CopybookPerformanceFixture>,
}

impl Issue52FixtureLoader {
    /// Create new fixture loader with all Issue #52 test data
    pub fn new() -> Self {
        Self {
            performance_fixtures: create_all_performance_fixtures(),
            python_outputs: create_all_python_utility_outputs(),
            audit_fixtures: create_comprehensive_audit_fixtures(),
            cicd_fixtures: create_comprehensive_cicd_fixtures(),
            cobol_fixtures: create_comprehensive_cobol_fixtures(),
        }
    }

    /// Load performance fixture by name
    pub fn load_performance_fixture(&self, name: &str) -> Option<&PerformanceFixture> {
        self.performance_fixtures.get(name)
    }

    /// Load all performance fixtures
    pub fn load_all_performance_fixtures(&self) -> &HashMap<String, PerformanceFixture> {
        &self.performance_fixtures
    }

    /// Load Python utility output by utility and scenario name
    pub fn load_python_output(&self, utility: &str, scenario: &str) -> Option<&Value> {
        self.python_outputs.get(utility)?.get(scenario)
    }

    /// Load all Python utility outputs
    pub fn load_all_python_outputs(&self) -> &HashMap<String, HashMap<String, Value>> {
        &self.python_outputs
    }

    /// Load audit fixtures by category
    pub fn load_audit_fixtures(&self, category: &str) -> Option<&Vec<AuditEvent>> {
        self.audit_fixtures.get(category)
    }

    /// Load all audit fixtures
    pub fn load_all_audit_fixtures(&self) -> &HashMap<String, Vec<AuditEvent>> {
        &self.audit_fixtures
    }

    /// Load CI/CD integration fixtures by category
    pub fn load_cicd_fixtures(&self, category: &str) -> Option<&HashMap<String, Value>> {
        self.cicd_fixtures.get(category)
    }

    /// Load all CI/CD integration fixtures
    pub fn load_all_cicd_fixtures(&self) -> &HashMap<String, HashMap<String, Value>> {
        &self.cicd_fixtures
    }

    /// Load COBOL processing fixture by name
    pub fn load_cobol_fixture(&self, name: &str) -> Option<&CopybookPerformanceFixture> {
        self.cobol_fixtures.get(name)
    }

    /// Load all COBOL processing fixtures
    pub fn load_all_cobol_fixtures(&self) -> &HashMap<String, CopybookPerformanceFixture> {
        &self.cobol_fixtures
    }

    /// Generate sample perf.json content for testing
    pub fn generate_perf_json(&self, scenario: &str) -> Option<String> {
        let fixture = self.load_performance_fixture(scenario)?;
        let json_value = fixture.to_json().ok()?;
        serde_json::to_string_pretty(&json_value).ok()
    }

    /// Generate comprehensive test report with all fixture statistics
    pub fn generate_fixture_report(&self) -> HashMap<String, Value> {
        let mut report = HashMap::new();

        report.insert("performance_fixtures".to_string(), serde_json::json!({
            "count": self.performance_fixtures.len(),
            "scenarios": self.performance_fixtures.keys().collect::<Vec<_>>(),
            "coverage": {
                "current_achievement": self.performance_fixtures.contains_key("current_achievement"),
                "performance_violations": self.performance_fixtures.contains_key("performance_violation"),
                "enterprise_compliance": self.performance_fixtures.contains_key("enterprise_compliance"),
                "timeout_scenarios": self.performance_fixtures.contains_key("timeout_scenario")
            }
        }));

        report.insert("python_utility_outputs".to_string(), serde_json::json!({
            "utilities_count": self.python_outputs.len(),
            "utilities": self.python_outputs.keys().collect::<Vec<_>>(),
            "total_scenarios": self.python_outputs.values().map(|v| v.len()).sum::<usize>(),
            "coverage": {
                "bench_runner": self.python_outputs.contains_key("bench_runner"),
                "json_processor": self.python_outputs.contains_key("json_processor"),
                "pr_automation": self.python_outputs.contains_key("pr_automation"),
                "baseline_manager": self.python_outputs.contains_key("baseline_manager"),
                "audit_generator": self.python_outputs.contains_key("audit_generator"),
                "slo_validator": self.python_outputs.contains_key("slo_validator")
            }
        }));

        report.insert("audit_fixtures".to_string(), serde_json::json!({
            "categories_count": self.audit_fixtures.len(),
            "categories": self.audit_fixtures.keys().collect::<Vec<_>>(),
            "total_events": self.audit_fixtures.values().map(|v| v.len()).sum::<usize>(),
            "compliance_coverage": {
                "sox": self.audit_fixtures.contains_key("sox_compliance"),
                "hipaa": self.audit_fixtures.contains_key("hipaa_compliance"),
                "pci_dss": self.audit_fixtures.contains_key("pci_dss_compliance"),
                "gdpr": self.audit_fixtures.contains_key("gdpr_compliance"),
                "iso27001": self.audit_fixtures.contains_key("iso27001_compliance")
            }
        }));

        report.insert("cicd_fixtures".to_string(), serde_json::json!({
            "categories_count": self.cicd_fixtures.len(),
            "categories": self.cicd_fixtures.keys().collect::<Vec<_>>(),
            "workflow_coverage": {
                "performance_validation": self.cicd_fixtures.contains_key("performance_workflows"),
                "baseline_promotion": self.cicd_fixtures.contains_key("baseline_workflows"),
                "pr_comments": self.cicd_fixtures.contains_key("pr_comments"),
                "artifacts": self.cicd_fixtures.contains_key("artifacts")
            }
        }));

        report.insert("cobol_fixtures".to_string(), serde_json::json!({
            "copybooks_count": self.cobol_fixtures.len(),
            "copybooks": self.cobol_fixtures.keys().collect::<Vec<_>>(),
            "processing_coverage": {
                "display_heavy": self.cobol_fixtures.values().any(|f| f.display_fields > f.comp3_fields),
                "comp3_heavy": self.cobol_fixtures.values().any(|f| f.comp3_fields > f.display_fields),
                "odo_structures": self.cobol_fixtures.values().any(|f| f.odo_fields > 0),
                "level_88_conditions": self.cobol_fixtures.values().any(|f| f.level_88_count > 0),
                "redefines_structures": self.cobol_fixtures.values().any(|f| f.redefines_count > 0)
            },
            "compliance_profiles": self.cobol_fixtures.values()
                .map(|f| f.compliance_profile.clone())
                .collect::<std::collections::HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
        }));

        report.insert("fixture_summary".to_string(), serde_json::json!({
            "total_categories": 5,
            "total_fixtures": self.performance_fixtures.len() +
                             self.python_outputs.values().map(|v| v.len()).sum::<usize>() +
                             self.audit_fixtures.values().map(|v| v.len()).sum::<usize>() +
                             self.cicd_fixtures.values().map(|v| v.len()).sum::<usize>() +
                             self.cobol_fixtures.len(),
            "enterprise_ready": true,
            "issue_52_coverage": "comprehensive",
            "test_scaffolding_support": "full"
        }));

        report
    }
}

impl Default for Issue52FixtureLoader {
    fn default() -> Self {
        Self::new()
    }
}

/// Global fixture loader instance for test infrastructure
pub static ISSUE_52_FIXTURES: LazyLock<Issue52FixtureLoader> = LazyLock::new(Issue52FixtureLoader::new);

/// Convenience function to load performance fixture
pub fn load_performance_fixture(name: &str) -> Option<&'static PerformanceFixture> {
    ISSUE_52_FIXTURES.load_performance_fixture(name)
}

/// Convenience function to load Python utility output
pub fn load_python_output(utility: &str, scenario: &str) -> Option<&'static Value> {
    ISSUE_52_FIXTURES.load_python_output(utility, scenario)
}

/// Convenience function to load audit fixtures
pub fn load_audit_fixtures(category: &str) -> Option<&'static Vec<AuditEvent>> {
    ISSUE_52_FIXTURES.load_audit_fixtures(category)
}

/// Convenience function to load CI/CD fixtures
pub fn load_cicd_fixtures(category: &str) -> Option<&'static HashMap<String, Value>> {
    ISSUE_52_FIXTURES.load_cicd_fixtures(category)
}

/// Convenience function to load COBOL fixture
pub fn load_cobol_fixture(name: &str) -> Option<&'static CopybookPerformanceFixture> {
    ISSUE_52_FIXTURES.load_cobol_fixture(name)
}

/// Generate sample perf.json for testing
pub fn generate_perf_json(scenario: &str) -> Option<String> {
    ISSUE_52_FIXTURES.generate_perf_json(scenario)
}

/// Generate comprehensive fixture coverage report
pub fn generate_fixture_report() -> HashMap<String, Value> {
    ISSUE_52_FIXTURES.generate_fixture_report()
}

/// Test helper functions for Issue #52 test infrastructure
pub mod test_helpers {
    use super::*;

    /// Validate that a performance fixture meets enterprise requirements
    pub fn validate_enterprise_performance(fixture: &PerformanceFixture) -> bool {
        fixture.display_gibs >= 0.0745 && // Enterprise floor: 80 MB/s = ~0.0745 GiB/s
        fixture.comp3_mibs >= 40.0 &&     // Enterprise floor: 40 MB/s
        fixture.to_json().is_ok()
    }

    /// Validate that a Python utility output has required structure
    pub fn validate_python_output_structure(output: &Value) -> bool {
        output.is_object() &&
        output.get("status").is_some() &&
        output.get("timestamp").is_some()
    }

    /// Validate that an audit event meets compliance requirements
    pub fn validate_audit_event_compliance(event: &AuditEvent) -> bool {
        !event.event_id.is_empty() &&
        !event.compliance_profile.is_empty() &&
        !event.description.is_empty() &&
        event.to_json().is_ok()
    }

    /// Validate that a COBOL fixture has realistic performance expectations
    pub fn validate_cobol_performance_expectation(fixture: &CopybookPerformanceFixture) -> bool {
        fixture.expected_performance.display_throughput_gibs > 0.0 &&
        fixture.expected_performance.comp3_throughput_mibs > 0.0 &&
        fixture.expected_performance.memory_usage_mb > 0 &&
        fixture.expected_performance.parsing_duration_ms > 0
    }

    /// Get all test scenarios for comprehensive test coverage validation
    pub fn get_all_test_scenarios() -> Vec<String> {
        let loader = &*ISSUE_52_FIXTURES;
        let mut scenarios = Vec::new();

        // Performance scenarios
        scenarios.extend(loader.load_all_performance_fixtures().keys().cloned());

        // Python utility scenarios
        for (utility, outputs) in loader.load_all_python_outputs() {
            for scenario in outputs.keys() {
                scenarios.push(format!("{}:{}", utility, scenario));
            }
        }

        // Audit scenarios
        scenarios.extend(loader.load_all_audit_fixtures().keys().cloned());

        // CI/CD scenarios
        scenarios.extend(loader.load_all_cicd_fixtures().keys().cloned());

        // COBOL scenarios
        scenarios.extend(loader.load_all_cobol_fixtures().keys().cloned());

        scenarios.sort();
        scenarios
    }

    /// Validate comprehensive fixture coverage for Issue #52
    pub fn validate_issue_52_coverage() -> bool {
        let loader = &*ISSUE_52_FIXTURES;

        // Must have core performance scenarios
        let has_performance_scenarios = loader.load_performance_fixture("current_achievement").is_some() &&
                                       loader.load_performance_fixture("performance_violation").is_some() &&
                                       loader.load_performance_fixture("enterprise_compliance").is_some();

        // Must have all Python utility outputs
        let has_python_utilities = loader.load_python_output("bench_runner", "success_execution").is_some() &&
                                   loader.load_python_output("json_processor", "valid_json_processing").is_some() &&
                                   loader.load_python_output("pr_automation", "pr_comment_success").is_some() &&
                                   loader.load_python_output("baseline_manager", "baseline_promotion_success").is_some() &&
                                   loader.load_python_output("audit_generator", "audit_report_success").is_some() &&
                                   loader.load_python_output("slo_validator", "slo_validation_success").is_some();

        // Must have compliance audit fixtures
        let has_compliance_audits = loader.load_audit_fixtures("sox_compliance").is_some() &&
                                   loader.load_audit_fixtures("hipaa_compliance").is_some() &&
                                   loader.load_audit_fixtures("pci_dss_compliance").is_some();

        // Must have CI/CD integration fixtures
        let has_cicd_integration = loader.load_cicd_fixtures("performance_workflows").is_some() &&
                                  loader.load_cicd_fixtures("baseline_workflows").is_some() &&
                                  loader.load_cicd_fixtures("pr_comments").is_some();

        // Must have COBOL processing fixtures
        let has_cobol_processing = loader.load_cobol_fixture("enterprise_customer").is_some() &&
                                  loader.load_cobol_fixture("financial_transaction").is_some() &&
                                  loader.load_cobol_fixture("healthcare_patient").is_some();

        has_performance_scenarios &&
        has_python_utilities &&
        has_compliance_audits &&
        has_cicd_integration &&
        has_cobol_processing
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixture_loader_initialization() {
        let loader = Issue52FixtureLoader::new();
        assert!(!loader.performance_fixtures.is_empty());
        assert!(!loader.python_outputs.is_empty());
        assert!(!loader.audit_fixtures.is_empty());
        assert!(!loader.cicd_fixtures.is_empty());
        assert!(!loader.cobol_fixtures.is_empty());
    }

    #[test]
    fn test_performance_fixture_loading() {
        let fixture = load_performance_fixture("current_achievement");
        assert!(fixture.is_some());

        let fixture = fixture.unwrap();
        assert_eq!(fixture.display_gibs, 4.22);
        assert_eq!(fixture.comp3_mibs, 571.0);
        assert!(fixture.errors.is_empty());
    }

    #[test]
    fn test_python_utility_output_loading() {
        let output = load_python_output("bench_runner", "success_execution");
        assert!(output.is_some());

        let output = output.unwrap();
        assert_eq!(output["status"], "success");
        assert!(output["benchmark_results"]["display_gibs"].is_number());
    }

    #[test]
    fn test_audit_fixtures_loading() {
        let fixtures = load_audit_fixtures("sox_compliance");
        assert!(fixtures.is_some());

        let fixtures = fixtures.unwrap();
        assert!(!fixtures.is_empty());
        assert!(fixtures.iter().all(|f| f.compliance_profile == "SOX"));
    }

    #[test]
    fn test_cicd_fixtures_loading() {
        let fixtures = load_cicd_fixtures("performance_workflows");
        assert!(fixtures.is_some());

        let fixtures = fixtures.unwrap();
        assert!(fixtures.contains_key("main_branch_success"));
        assert!(fixtures.contains_key("regression_detected"));
    }

    #[test]
    fn test_cobol_fixture_loading() {
        let fixture = load_cobol_fixture("enterprise_customer");
        assert!(fixture.is_some());

        let fixture = fixture.unwrap();
        assert_eq!(fixture.name, "enterprise_customer_record");
        assert!(fixture.expected_performance.enterprise_compliant);
    }

    #[test]
    fn test_perf_json_generation() {
        let json = generate_perf_json("current_achievement");
        assert!(json.is_some());

        let json = json.unwrap();
        assert!(json.contains("display_gibs"));
        assert!(json.contains("comp3_mibs"));
        assert!(json.contains("4.22"));
    }

    #[test]
    fn test_fixture_report_generation() {
        let report = generate_fixture_report();
        assert!(report.contains_key("performance_fixtures"));
        assert!(report.contains_key("python_utility_outputs"));
        assert!(report.contains_key("audit_fixtures"));
        assert!(report.contains_key("cicd_fixtures"));
        assert!(report.contains_key("cobol_fixtures"));
        assert!(report.contains_key("fixture_summary"));

        let summary = &report["fixture_summary"];
        assert_eq!(summary["enterprise_ready"], true);
        assert_eq!(summary["issue_52_coverage"], "comprehensive");
    }

    #[test]
    fn test_enterprise_performance_validation() {
        let fixture = load_performance_fixture("current_achievement").unwrap();
        assert!(test_helpers::validate_enterprise_performance(fixture));

        let floor_fixture = load_performance_fixture("performance_floor").unwrap();
        assert!(test_helpers::validate_enterprise_performance(floor_fixture));

        let violation_fixture = load_performance_fixture("performance_violation").unwrap();
        assert!(!test_helpers::validate_enterprise_performance(violation_fixture));
    }

    #[test]
    fn test_comprehensive_issue_52_coverage() {
        assert!(test_helpers::validate_issue_52_coverage());
    }

    #[test]
    fn test_all_test_scenarios() {
        let scenarios = test_helpers::get_all_test_scenarios();
        assert!(!scenarios.is_empty());
        assert!(scenarios.len() > 50); // Should have substantial coverage

        // Verify key scenarios are present
        assert!(scenarios.contains(&"current_achievement".to_string()));
        assert!(scenarios.contains(&"enterprise_customer".to_string()));
        assert!(scenarios.iter().any(|s| s.starts_with("bench_runner:")));
    }

    #[test]
    fn test_global_fixture_loader() {
        // Test that global loader is properly initialized
        let fixture = ISSUE_52_FIXTURES.load_performance_fixture("current_achievement");
        assert!(fixture.is_some());

        let output = ISSUE_52_FIXTURES.load_python_output("json_processor", "valid_json_processing");
        assert!(output.is_some());
    }
}