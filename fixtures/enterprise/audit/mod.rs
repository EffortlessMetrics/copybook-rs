// SPDX-License-Identifier: AGPL-3.0-or-later
//! Enterprise Audit System Test Fixtures Module
//!
//! Comprehensive test fixture loading utilities for copybook-rs Enterprise Audit System.
//! Provides deterministic data generation, realistic COBOL processing scenarios,
//! and integration with copybook-rs workspace testing patterns.

use std::sync::LazyLock;
use std::collections::HashMap;
use std::path::PathBuf;
use std::fs;

pub mod audit_context_fixtures;
pub mod audit_test_data;
pub mod enterprise_cobol_data;

pub use audit_context_fixtures::*;
pub use audit_test_data::*;
pub use enterprise_cobol_data::*;

/// Lazily-loaded enterprise COBOL test data for performance validation
pub static ENTERPRISE_COBOL_DATA: LazyLock<HashMap<String, Vec<u8>>> = LazyLock::new(|| {
    generate_mixed_enterprise_workload()
});

/// Lazily-loaded SIEM integration test events
pub static SIEM_INTEGRATION_EVENTS: LazyLock<HashMap<String, serde_json::Value>> = LazyLock::new(|| {
    create_siem_integration_events().into_iter()
        .map(|(k, v)| (k, serde_json::Value::Array(v)))
        .collect()
});

/// Lazily-loaded performance audit baselines
pub static PERFORMANCE_BASELINES: LazyLock<HashMap<String, serde_json::Value>> = LazyLock::new(|| {
    create_performance_audit_data()
});

/// Get path to enterprise audit fixtures directory
pub fn enterprise_audit_fixtures_dir() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("fixtures");
    path.push("enterprise");
    path.push("audit");
    path
}

/// Load COBOL copybook fixture by name
pub fn load_copybook_fixture(name: &str) -> Result<String, std::io::Error> {
    let mut path = enterprise_audit_fixtures_dir();
    path.push(format!("{}.cpy", name));
    fs::read_to_string(path)
}

/// Load binary data fixture by name
pub fn load_binary_data_fixture(name: &str) -> Result<Vec<u8>, std::io::Error> {
    if let Some(data) = ENTERPRISE_COBOL_DATA.get(name) {
        Ok(data.clone())
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("Binary data fixture '{}' not found", name)
        ))
    }
}

/// Create deterministic audit test environment
/// Ensures reproducible test results across different environments
pub struct AuditTestEnvironment {
    pub temp_dir: tempfile::TempDir,
    pub audit_log_path: PathBuf,
    pub performance_baseline_path: PathBuf,
    pub compliance_report_path: PathBuf,
}

impl AuditTestEnvironment {
    /// Create new test environment with temporary directories
    pub fn new() -> Result<Self, std::io::Error> {
        let temp_dir = tempfile::tempdir()?;
        let mut audit_log_path = temp_dir.path().to_path_buf();
        audit_log_path.push("audit.jsonl");

        let mut performance_baseline_path = temp_dir.path().to_path_buf();
        performance_baseline_path.push("performance_baseline.json");

        let mut compliance_report_path = temp_dir.path().to_path_buf();
        compliance_report_path.push("compliance_report.json");

        Ok(Self {
            temp_dir,
            audit_log_path,
            performance_baseline_path,
            compliance_report_path,
        })
    }

    /// Write performance baseline data to file
    pub fn write_performance_baseline(&self) -> Result<(), std::io::Error> {
        let baseline_data = serde_json::to_string_pretty(&*PERFORMANCE_BASELINES)?;
        fs::write(&self.performance_baseline_path, baseline_data)?;
        Ok(())
    }

    /// Write COBOL test data to file
    pub fn write_cobol_test_data(&self, data_type: &str, filename: &str) -> Result<PathBuf, std::io::Error> {
        if let Some(data) = ENTERPRISE_COBOL_DATA.get(data_type) {
            let mut file_path = self.temp_dir.path().to_path_buf();
            file_path.push(filename);
            fs::write(&file_path, data)?;
            Ok(file_path)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("COBOL test data type '{}' not found", data_type)
            ))
        }
    }

    /// Write copybook fixture to file
    pub fn write_copybook_fixture(&self, fixture_name: &str, filename: &str) -> Result<PathBuf, std::io::Error> {
        let copybook_content = load_copybook_fixture(fixture_name)?;
        let mut file_path = self.temp_dir.path().to_path_buf();
        file_path.push(filename);
        fs::write(&file_path, copybook_content)?;
        Ok(file_path)
    }
}

/// Audit fixture validation utilities
pub struct AuditFixtureValidator;

impl AuditFixtureValidator {
    /// Validate performance data meets enterprise requirements
    pub fn validate_performance_data(data_type: &str) -> bool {
        if let Some(data) = ENTERPRISE_COBOL_DATA.get(data_type) {
            match data_type {
                "display_heavy_processing" => {
                    // Should have enough data for 4.1+ GiB/s throughput testing
                    data.len() >= 50_000_000 // 50+ MB
                },
                "comp3_heavy_processing" => {
                    // Should have enough data for 560+ MiB/s throughput testing
                    data.len() >= 16_000_000 // 16+ MB
                },
                "hipaa_phi_processing" => {
                    // Should have realistic PHI data volume
                    data.len() >= 400_000 // 400+ KB
                },
                "sox_financial_processing" => {
                    // Should have realistic financial transaction volume
                    data.len() >= 1_500_000 // 1.5+ MB
                },
                _ => false,
            }
        } else {
            false
        }
    }

    /// Validate audit context has required compliance settings
    pub fn validate_audit_context_compliance(context: &copybook_core::audit::AuditContext, profile: copybook_core::audit::ComplianceProfile) -> bool {
        match profile {
            copybook_core::audit::ComplianceProfile::HIPAA => {
                context.requires_compliance(profile) &&
                context.metadata.contains_key("minimum_necessary_justification") &&
                matches!(context.security.classification, copybook_core::audit::context::SecurityClassification::PHI)
            },
            copybook_core::audit::ComplianceProfile::SOX => {
                context.requires_compliance(profile) &&
                matches!(context.security.classification, copybook_core::audit::context::SecurityClassification::MaterialTransaction)
            },
            copybook_core::audit::ComplianceProfile::GDPR => {
                context.requires_compliance(profile) &&
                context.metadata.contains_key("gdpr_legal_basis")
            },
            copybook_core::audit::ComplianceProfile::PciDss => {
                context.requires_compliance(profile) &&
                context.metadata.contains_key("cardholder_data_present")
            },
        }
    }

    /// Validate CEF log format compliance
    pub fn validate_cef_format(cef_log: &str) -> bool {
        cef_log.starts_with("CEF:0|Copybook-rs|") &&
        cef_log.contains("|Enterprise Audit|") &&
        cef_log.contains("|0.3.1|")
    }
}

/// Performance test configuration for enterprise scenarios
#[derive(Debug, Clone)]
pub struct PerformanceTestConfig {
    pub display_target_gbps: f64,
    pub comp3_target_mbps: f64,
    pub max_overhead_percent: f64,
    pub max_memory_mb: usize,
    pub min_test_duration_seconds: f64,
}

impl Default for PerformanceTestConfig {
    fn default() -> Self {
        Self {
            display_target_gbps: 4.1,
            comp3_target_mbps: 560.0,
            max_overhead_percent: 5.0,
            max_memory_mb: 256,
            min_test_duration_seconds: 1.0,
        }
    }
}

impl PerformanceTestConfig {
    /// Enterprise performance configuration with higher targets
    pub fn enterprise() -> Self {
        Self {
            display_target_gbps: 4.5,
            comp3_target_mbps: 600.0,
            max_overhead_percent: 3.0,
            max_memory_mb: 512,
            min_test_duration_seconds: 5.0,
        }
    }

    /// Development performance configuration with relaxed targets
    pub fn development() -> Self {
        Self {
            display_target_gbps: 2.0,
            comp3_target_mbps: 300.0,
            max_overhead_percent: 10.0,
            max_memory_mb: 128,
            min_test_duration_seconds: 0.5,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use copybook_core::audit::ComplianceProfile;

    #[test]
    fn test_enterprise_cobol_data_loading() {
        assert!(ENTERPRISE_COBOL_DATA.contains_key("display_heavy_processing"));
        assert!(ENTERPRISE_COBOL_DATA.contains_key("comp3_heavy_processing"));
        assert!(ENTERPRISE_COBOL_DATA.contains_key("hipaa_phi_processing"));
        assert!(ENTERPRISE_COBOL_DATA.contains_key("sox_financial_processing"));
    }

    #[test]
    fn test_audit_test_environment() {
        let env = AuditTestEnvironment::new().expect("Should create test environment");
        assert!(env.temp_dir.path().exists());
        assert!(!env.audit_log_path.exists()); // File not created yet

        env.write_performance_baseline().expect("Should write baseline");
        assert!(env.performance_baseline_path.exists());
    }

    #[test]
    fn test_performance_data_validation() {
        assert!(AuditFixtureValidator::validate_performance_data("display_heavy_processing"));
        assert!(AuditFixtureValidator::validate_performance_data("comp3_heavy_processing"));
        assert!(!AuditFixtureValidator::validate_performance_data("nonexistent_data"));
    }

    #[test]
    fn test_hipaa_context_validation() {
        let context = create_hipaa_test_context();
        assert!(AuditFixtureValidator::validate_audit_context_compliance(&context, ComplianceProfile::HIPAA));
    }

    #[test]
    fn test_cef_format_validation() {
        let cef_samples = create_cef_log_samples();
        for sample in cef_samples {
            assert!(AuditFixtureValidator::validate_cef_format(&sample));
        }
    }

    #[test]
    fn test_performance_test_configs() {
        let default_config = PerformanceTestConfig::default();
        assert_eq!(default_config.display_target_gbps, 4.1);

        let enterprise_config = PerformanceTestConfig::enterprise();
        assert!(enterprise_config.display_target_gbps > default_config.display_target_gbps);

        let dev_config = PerformanceTestConfig::development();
        assert!(dev_config.max_overhead_percent > default_config.max_overhead_percent);
    }

    #[test]
    fn test_fixture_directory_access() {
        let fixtures_dir = enterprise_audit_fixtures_dir();
        assert!(fixtures_dir.to_string_lossy().contains("fixtures/enterprise/audit"));
    }

    #[test]
    fn test_siem_integration_events_loading() {
        assert!(SIEM_INTEGRATION_EVENTS.contains_key("splunk_hec"));
        assert!(SIEM_INTEGRATION_EVENTS.contains_key("qradar_leef"));
        assert!(SIEM_INTEGRATION_EVENTS.contains_key("elastic_ecs"));
    }
}