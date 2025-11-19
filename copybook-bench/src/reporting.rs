//! Machine-readable benchmark reporting for Issue #52
//!
//! Provides standardized JSON schema and result processing for copybook-rs
//! performance monitoring with enterprise audit capabilities.

use serde::{Deserialize, Serialize};

/// Machine-readable benchmark report format for Issue #52 AC2
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PerformanceReport {
    /// DISPLAY throughput in GiB/s
    pub display_gibs: Option<f64>,
    /// COMP-3 throughput in MiB/s
    pub comp3_mibs: Option<f64>,
    /// ISO 8601 timestamp
    #[serde(default)]
    pub timestamp: String,
    /// Git commit hash (short)
    #[serde(default)]
    pub commit: String,
    /// Overall status: success, failure, warning
    #[serde(default = "default_status")]
    pub status: String,
    /// Performance warnings
    #[serde(default)]
    pub warnings: Vec<String>,
    /// Performance errors
    #[serde(default)]
    pub errors: Vec<String>,
}

fn default_status() -> String {
    "success".to_string()
}

impl PerformanceReport {
    /// Create new report with defaults
    #[must_use]
    pub fn new() -> Self {
        Self {
            display_gibs: None,
            comp3_mibs: None,
            timestamp: chrono::Utc::now().to_rfc3339(),
            commit: "unknown".to_string(),
            status: "success".to_string(),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Validate against SLO thresholds
    pub fn validate_slos(&mut self, display_slo: f64, comp3_slo: f64) {
        if let Some(display) = self.display_gibs {
            if display < display_slo {
                self.status = "failure".to_string();
                self.errors.push(format!(
                    "DISPLAY throughput {display:.2} GiB/s below SLO {display_slo:.1} GiB/s"
                ));
            } else if display < display_slo * 1.05 {
                if self.status != "failure" {
                    self.status = "warning".to_string();
                }
                self.warnings.push(format!(
                    "DISPLAY throughput {display:.2} GiB/s close to SLO threshold"
                ));
            }
        }

        if let Some(comp3) = self.comp3_mibs {
            if comp3 < comp3_slo {
                self.status = "failure".to_string();
                self.errors.push(format!(
                    "COMP-3 throughput {comp3:.1} MiB/s below SLO {comp3_slo:.0} MiB/s"
                ));
            } else if comp3 < comp3_slo * 1.05 {
                if self.status != "failure" {
                    self.status = "warning".to_string();
                }
                self.warnings.push(format!(
                    "COMP-3 throughput {comp3:.1} MiB/s close to SLO threshold"
                ));
            }
        }
    }

    /// Format one-liner for PR comments (AC4)
    #[must_use]
    pub fn format_pr_summary(&self) -> String {
        let display = self
            .display_gibs
            .map_or_else(|| "N/A".to_string(), |v| format!("{v:.2} GiB/s"));
        let comp3 = self
            .comp3_mibs
            .map_or_else(|| "N/A".to_string(), |v| format!("{v:.0} MiB/s"));

        let status_icon = match self.status.as_str() {
            "failure" => "❌",
            "warning" => "⚠️",
            _ => "✅",
        };

        format!("DISPLAY: {display}, COMP-3: {comp3} {status_icon}")
    }
}

impl Default for PerformanceReport {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_report_creation() {
        let report = PerformanceReport::new();
        assert_eq!(report.status, "success");
        assert!(report.warnings.is_empty());
        assert!(report.errors.is_empty());
    }

    #[test]
    fn test_slo_validation_pass() {
        let mut report = PerformanceReport::new();
        report.display_gibs = Some(4.5);
        report.comp3_mibs = Some(600.0);

        report.validate_slos(4.1, 560.0);
        assert_eq!(report.status, "success");
        assert!(report.errors.is_empty());
    }

    #[test]
    fn test_slo_validation_failure() {
        let mut report = PerformanceReport::new();
        report.display_gibs = Some(3.0);
        report.comp3_mibs = Some(500.0);

        report.validate_slos(4.1, 560.0);
        assert_eq!(report.status, "failure");
        assert!(!report.errors.is_empty());
    }

    #[test]
    fn test_pr_summary_format() {
        let mut report = PerformanceReport::new();
        report.display_gibs = Some(4.22);
        report.comp3_mibs = Some(571.0);

        let summary = report.format_pr_summary();
        assert_eq!(summary, "DISPLAY: 4.22 GiB/s, COMP-3: 571 MiB/s ✅");
    }
}
