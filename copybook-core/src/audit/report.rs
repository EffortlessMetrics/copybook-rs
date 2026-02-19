//! Audit Reporting System
//!
//! Generates comprehensive audit reports for compliance, performance,
//! and security monitoring in copybook-rs enterprise operations.

use serde::{Deserialize, Serialize};

use super::event::AuditSeverity;
use super::{AuditEvent, AuditEventType, AuditResult, ComplianceResult};

/// Comprehensive audit report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditReport {
    pub report_id: String,
    pub report_type: ReportType,
    pub operation_id: String,
    pub created_at: String,
    pub summary: AuditSummary,
}

/// Audit report types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReportType {
    Comprehensive,
    Compliance,
    Performance,
    Security,
}

/// Audit summary information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditSummary {
    pub total_events: u64,
    pub compliance_status: String,
    pub performance_status: String,
    pub security_status: String,
}

/// Compliance-specific audit report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceReport {
    pub report_id: String,
    pub operation_id: String,
    pub compliance_result: ComplianceResult,
    pub recommendations: Vec<super::compliance::ComplianceRecommendation>,
    pub next_review_date: String,
    pub created_at: String,
}

/// Performance audit report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceReport {
    pub report_id: String,
    pub baseline_comparison: String,
    pub metrics: super::performance::ThroughputMetrics,
    pub created_at: String,
}

/// Security audit report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityReport {
    pub report_id: String,
    pub security_events: u64,
    pub threat_level: String,
    pub created_at: String,
}

/// Report generator for various audit report types
pub struct ReportGenerator;

impl ReportGenerator {
    pub fn new() -> Self {
        Self
    }

    /// Generate a comprehensive audit report from a collection of audit events
    pub fn generate_audit_report(
        &self,
        events: &[AuditEvent],
        report_type: ReportType,
    ) -> AuditResult<AuditReport> {
        let total_events = events.len() as u64;

        // Aggregate compliance status from events
        let compliance_status = self.aggregate_compliance_status(events);

        // Aggregate performance status from events
        let performance_status = self.aggregate_performance_status(events);

        // Aggregate security status from events
        let security_status = self.aggregate_security_status(events);

        // Determine operation_id (use first event's correlation_id or generate new one)
        let operation_id = events
            .first()
            .map_or_else(super::generate_audit_id, |e| e.correlation_id().to_string());

        Ok(AuditReport {
            report_id: super::generate_audit_id(),
            report_type,
            operation_id,
            created_at: chrono::Utc::now().to_rfc3339(),
            summary: AuditSummary {
                total_events,
                compliance_status,
                performance_status,
                security_status,
            },
        })
    }

    /// Generate a compliance-specific report
    pub fn generate_compliance_report(
        &self,
        result: &ComplianceResult,
        operation_id: &str,
    ) -> AuditResult<ComplianceReport> {
        // Extract recommendations from compliance result (empty for now, as ComplianceResult doesn't contain them)
        let recommendations = Vec::new();

        // Calculate next review date (90 days from now as default)
        let next_review_date = (chrono::Utc::now() + chrono::Duration::days(90)).to_rfc3339();

        Ok(ComplianceReport {
            report_id: super::generate_audit_id(),
            operation_id: operation_id.to_string(),
            compliance_result: result.clone(),
            recommendations,
            next_review_date,
            created_at: chrono::Utc::now().to_rfc3339(),
        })
    }

    /// Generate a performance audit report
    pub fn generate_performance_report(
        &self,
        metrics: &super::performance::ThroughputMetrics,
        baseline_comparison: &str,
        _operation_id: &str,
    ) -> AuditResult<PerformanceReport> {
        Ok(PerformanceReport {
            report_id: super::generate_audit_id(),
            baseline_comparison: baseline_comparison.to_string(),
            metrics: metrics.clone(),
            created_at: chrono::Utc::now().to_rfc3339(),
        })
    }

    /// Generate a security audit report
    pub fn generate_security_report(
        &self,
        events_count: usize,
        threat_level: &str,
        _operation_id: &str,
    ) -> AuditResult<SecurityReport> {
        Ok(SecurityReport {
            report_id: super::generate_audit_id(),
            security_events: events_count as u64,
            threat_level: threat_level.to_string(),
            created_at: chrono::Utc::now().to_rfc3339(),
        })
    }

    /// Format an audit report in the specified output format
    pub fn format_report(
        &self,
        report: &AuditReport,
        format: &ReportFormat,
    ) -> AuditResult<String> {
        match format {
            ReportFormat::Json => serde_json::to_string_pretty(report).map_err(|e| e.into()),
            ReportFormat::Csv => self.format_as_csv(report),
            ReportFormat::Xml => self.format_as_xml(report),
            ReportFormat::Html => self.format_as_html(report),
            ReportFormat::Pdf => Err(super::AuditError::Configuration {
                message: "PDF export is planned for v1.0. Use HTML or JSON format instead."
                    .to_string(),
            }),
        }
    }

    // Helper methods for aggregating event data

    fn aggregate_compliance_status(&self, events: &[AuditEvent]) -> String {
        let compliance_events: Vec<_> = events
            .iter()
            .filter(|e| matches!(e.event_type, AuditEventType::ComplianceCheck))
            .collect();

        if compliance_events.is_empty() {
            return "Not Evaluated".to_string();
        }

        // Check for any compliance failures
        let has_failures = compliance_events.iter().any(|e| {
            matches!(
                e.payload,
                super::AuditPayload::ComplianceCheck {
                    remediation_required: true,
                    ..
                }
            )
        });

        if has_failures {
            "Non-Compliant".to_string()
        } else {
            "Compliant".to_string()
        }
    }

    fn aggregate_performance_status(&self, events: &[AuditEvent]) -> String {
        let perf_events: Vec<_> = events
            .iter()
            .filter(|e| matches!(e.event_type, AuditEventType::PerformanceMeasurement))
            .collect();

        if perf_events.is_empty() {
            return "Not Measured".to_string();
        }

        // Check for regressions
        let has_regressions = perf_events.iter().any(|e| {
            matches!(
                e.payload,
                super::AuditPayload::PerformanceMeasurement {
                    regression_detected: true,
                    ..
                }
            )
        });

        if has_regressions {
            "Regression Detected".to_string()
        } else {
            "Within Baseline".to_string()
        }
    }

    fn aggregate_security_status(&self, events: &[AuditEvent]) -> String {
        let security_events: Vec<_> = events
            .iter()
            .filter(|e| matches!(e.event_type, AuditEventType::SecurityEvent))
            .collect();

        if security_events.is_empty() {
            return "No Incidents".to_string();
        }

        // Count high-severity security events
        let high_severity_count = security_events
            .iter()
            .filter(|e| matches!(e.severity, AuditSeverity::High | AuditSeverity::Critical))
            .count();

        if high_severity_count > 0 {
            format!("{} Critical Incidents", high_severity_count)
        } else {
            format!("{} Low-Severity Incidents", security_events.len())
        }
    }

    fn format_as_html(&self, report: &AuditReport) -> AuditResult<String> {
        let compliance_badge_class = match report.summary.compliance_status.as_str() {
            "Compliant" => "badge-green",
            "Non-Compliant" => "badge-red",
            _ => "badge-yellow",
        };
        let performance_badge_class = match report.summary.performance_status.as_str() {
            s if s.contains("Regression") => "badge-red",
            "Not Measured" => "badge-yellow",
            _ => "badge-green",
        };
        let security_badge_class = match report.summary.security_status.as_str() {
            "No Incidents" => "badge-green",
            s if s.contains("Critical") => "badge-red",
            _ => "badge-yellow",
        };

        let html = format!(
            r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Audit Report - {report_id}</title>
<style>
  body {{ font-family: Arial, sans-serif; margin: 2rem; color: #333; background: #f9f9f9; }}
  h1 {{ color: #1a1a2e; border-bottom: 2px solid #4a4a8a; padding-bottom: 0.5rem; }}
  h2 {{ color: #4a4a8a; }}
  .meta-table {{ border-collapse: collapse; width: 100%; max-width: 600px; margin-bottom: 1.5rem; }}
  .meta-table td {{ padding: 0.4rem 0.8rem; border: 1px solid #ddd; }}
  .meta-table td:first-child {{ font-weight: bold; background: #f0f0f0; width: 40%; }}
  .summary-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem; margin-bottom: 1.5rem; }}
  .summary-card {{ background: #fff; border-radius: 8px; padding: 1rem; box-shadow: 0 1px 4px rgba(0,0,0,0.1); }}
  .summary-card h3 {{ margin: 0 0 0.5rem 0; font-size: 0.9rem; color: #666; text-transform: uppercase; }}
  .badge {{ display: inline-block; padding: 0.25rem 0.75rem; border-radius: 4px; font-weight: bold; font-size: 0.9rem; }}
  .badge-green {{ background: #d4edda; color: #155724; }}
  .badge-yellow {{ background: #fff3cd; color: #856404; }}
  .badge-red {{ background: #f8d7da; color: #721c24; }}
  .total-events {{ font-size: 2rem; font-weight: bold; color: #4a4a8a; }}
</style>
</head>
<body>
<h1>Audit Report</h1>
<h2>Report Metadata</h2>
<table class="meta-table">
  <tr><td>Report ID</td><td>{report_id}</td></tr>
  <tr><td>Report Type</td><td>{report_type:?}</td></tr>
  <tr><td>Operation ID</td><td>{operation_id}</td></tr>
  <tr><td>Created At</td><td>{created_at}</td></tr>
</table>
<h2>Summary</h2>
<div class="summary-grid">
  <div class="summary-card">
    <h3>Total Events</h3>
    <div class="total-events">{total_events}</div>
  </div>
  <div class="summary-card">
    <h3>Compliance Status</h3>
    <span class="badge {compliance_badge_class}">{compliance_status}</span>
  </div>
  <div class="summary-card">
    <h3>Performance Status</h3>
    <span class="badge {performance_badge_class}">{performance_status}</span>
  </div>
  <div class="summary-card">
    <h3>Security Status</h3>
    <span class="badge {security_badge_class}">{security_status}</span>
  </div>
</div>
</body>
</html>
"#,
            report_id = report.report_id,
            report_type = report.report_type,
            operation_id = report.operation_id,
            created_at = report.created_at,
            total_events = report.summary.total_events,
            compliance_status = report.summary.compliance_status,
            performance_status = report.summary.performance_status,
            security_status = report.summary.security_status,
            compliance_badge_class = compliance_badge_class,
            performance_badge_class = performance_badge_class,
            security_badge_class = security_badge_class,
        );
        Ok(html)
    }

    fn format_as_csv(&self, report: &AuditReport) -> AuditResult<String> {
        let mut csv = String::new();

        // Header
        csv.push_str("Field,Value\n");

        // Report metadata
        csv.push_str(&format!("Report ID,{}\n", report.report_id));
        csv.push_str(&format!("Report Type,{:?}\n", report.report_type));
        csv.push_str(&format!("Operation ID,{}\n", report.operation_id));
        csv.push_str(&format!("Created At,{}\n", report.created_at));

        // Summary data
        csv.push_str(&format!("Total Events,{}\n", report.summary.total_events));
        csv.push_str(&format!(
            "Compliance Status,{}\n",
            report.summary.compliance_status
        ));
        csv.push_str(&format!(
            "Performance Status,{}\n",
            report.summary.performance_status
        ));
        csv.push_str(&format!(
            "Security Status,{}\n",
            report.summary.security_status
        ));

        Ok(csv)
    }

    fn format_as_xml(&self, report: &AuditReport) -> AuditResult<String> {
        let mut xml = String::new();

        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xml.push_str("<AuditReport>\n");

        xml.push_str(&format!("  <ReportId>{}</ReportId>\n", report.report_id));
        xml.push_str(&format!(
            "  <ReportType>{:?}</ReportType>\n",
            report.report_type
        ));
        xml.push_str(&format!(
            "  <OperationId>{}</OperationId>\n",
            report.operation_id
        ));
        xml.push_str(&format!("  <CreatedAt>{}</CreatedAt>\n", report.created_at));

        xml.push_str("  <Summary>\n");
        xml.push_str(&format!(
            "    <TotalEvents>{}</TotalEvents>\n",
            report.summary.total_events
        ));
        xml.push_str(&format!(
            "    <ComplianceStatus>{}</ComplianceStatus>\n",
            report.summary.compliance_status
        ));
        xml.push_str(&format!(
            "    <PerformanceStatus>{}</PerformanceStatus>\n",
            report.summary.performance_status
        ));
        xml.push_str(&format!(
            "    <SecurityStatus>{}</SecurityStatus>\n",
            report.summary.security_status
        ));
        xml.push_str("  </Summary>\n");

        xml.push_str("</AuditReport>\n");

        Ok(xml)
    }
}

impl Default for ReportGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Report output formats
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReportFormat {
    Json,
    Pdf,
    Html,
    Csv,
    Xml,
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;
    use crate::audit::{AuditContext, AuditPayload, event::ParseResult};

    #[test]
    fn test_generate_audit_report_empty_events() {
        let generator = ReportGenerator::new();
        let events = vec![];

        let report = generator
            .generate_audit_report(&events, ReportType::Comprehensive)
            .expect("Should generate report for empty events");

        assert_eq!(report.summary.total_events, 0);
        assert_eq!(report.summary.compliance_status, "Not Evaluated");
        assert_eq!(report.summary.performance_status, "Not Measured");
        assert_eq!(report.summary.security_status, "No Incidents");
    }

    #[test]
    fn test_generate_audit_report_with_events() {
        let generator = ReportGenerator::new();
        let context = AuditContext::new();

        let payload = AuditPayload::CopybookParse {
            copybook_path: "test.cpy".to_string(),
            schema_fingerprint: "abc123".to_string(),
            parse_result: ParseResult::Success,
            parsing_duration_ms: 100,
            field_count: 10,
            level_88_count: 2,
            error_count: 0,
            warnings: vec![],
        };

        let event = super::super::AuditEvent::new(
            super::super::AuditEventType::CopybookParse,
            context,
            payload,
        );

        let events = vec![event];

        let report = generator
            .generate_audit_report(&events, ReportType::Comprehensive)
            .expect("Should generate report");

        assert_eq!(report.summary.total_events, 1);
        assert!(report.report_id.starts_with("audit-"));
        assert!(!report.created_at.is_empty());
    }

    #[test]
    fn test_generate_compliance_report() {
        let generator = ReportGenerator::new();

        let compliance_result = super::super::ComplianceResult {
            status: super::super::compliance::ComplianceStatus::Compliant,
            violations: vec![],
            warnings: vec![],
            validated_profiles: vec![],
            validation_timestamp: chrono::Utc::now().to_rfc3339(),
        };

        let report = generator
            .generate_compliance_report(&compliance_result, "test-op-123")
            .expect("Should generate compliance report");

        assert_eq!(report.operation_id, "test-op-123");
        assert!(report.report_id.starts_with("audit-"));
        assert!(!report.next_review_date.is_empty());
        assert!(report.recommendations.is_empty());
    }

    #[test]
    fn test_generate_performance_report() {
        let generator = ReportGenerator::new();

        let metrics = super::super::performance::ThroughputMetrics {
            display_throughput: 205_000_000,
            comp3_throughput: 58_000_000,
            record_rate: 10_000,
            peak_memory_mb: 256,
        };

        let report = generator
            .generate_performance_report(&metrics, "Within baseline", "test-op-456")
            .expect("Should generate performance report");

        assert_eq!(report.baseline_comparison, "Within baseline");
        assert_eq!(report.metrics.display_throughput, 205_000_000);
        assert!(report.report_id.starts_with("audit-"));
    }

    #[test]
    fn test_generate_security_report() {
        let generator = ReportGenerator::new();

        let report = generator
            .generate_security_report(5, "High", "test-op-789")
            .expect("Should generate security report");

        assert_eq!(report.security_events, 5);
        assert_eq!(report.threat_level, "High");
        assert!(report.report_id.starts_with("audit-"));
    }

    #[test]
    fn test_format_report_json() {
        let generator = ReportGenerator::new();

        let report = AuditReport {
            report_id: "test-report-001".to_string(),
            report_type: ReportType::Comprehensive,
            operation_id: "test-op-001".to_string(),
            created_at: "2025-01-11T00:00:00Z".to_string(),
            summary: AuditSummary {
                total_events: 10,
                compliance_status: "Compliant".to_string(),
                performance_status: "Within Baseline".to_string(),
                security_status: "No Incidents".to_string(),
            },
        };

        let json_output = generator
            .format_report(&report, &ReportFormat::Json)
            .expect("Should format as JSON");

        assert!(json_output.contains("test-report-001"));
        assert!(json_output.contains("Compliant"));
    }

    #[test]
    fn test_format_report_csv() {
        let generator = ReportGenerator::new();

        let report = AuditReport {
            report_id: "test-report-002".to_string(),
            report_type: ReportType::Performance,
            operation_id: "test-op-002".to_string(),
            created_at: "2025-01-11T00:00:00Z".to_string(),
            summary: AuditSummary {
                total_events: 5,
                compliance_status: "Not Evaluated".to_string(),
                performance_status: "Regression Detected".to_string(),
                security_status: "No Incidents".to_string(),
            },
        };

        let csv_output = generator
            .format_report(&report, &ReportFormat::Csv)
            .expect("Should format as CSV");

        assert!(csv_output.contains("Field,Value"));
        assert!(csv_output.contains("test-report-002"));
        assert!(csv_output.contains("Regression Detected"));
    }

    #[test]
    fn test_format_report_xml() {
        let generator = ReportGenerator::new();

        let report = AuditReport {
            report_id: "test-report-003".to_string(),
            report_type: ReportType::Security,
            operation_id: "test-op-003".to_string(),
            created_at: "2025-01-11T00:00:00Z".to_string(),
            summary: AuditSummary {
                total_events: 3,
                compliance_status: "Not Evaluated".to_string(),
                performance_status: "Not Measured".to_string(),
                security_status: "2 Critical Incidents".to_string(),
            },
        };

        let xml_output = generator
            .format_report(&report, &ReportFormat::Xml)
            .expect("Should format as XML");

        assert!(xml_output.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
        assert!(xml_output.contains("<AuditReport>"));
        assert!(xml_output.contains("test-report-003"));
        assert!(xml_output.contains("2 Critical Incidents"));
        assert!(xml_output.contains("</AuditReport>"));
    }

    #[test]
    fn test_format_report_pdf_unsupported() {
        let generator = ReportGenerator::new();

        let report = AuditReport {
            report_id: "test-report-004".to_string(),
            report_type: ReportType::Comprehensive,
            operation_id: "test-op-004".to_string(),
            created_at: "2025-01-11T00:00:00Z".to_string(),
            summary: AuditSummary {
                total_events: 0,
                compliance_status: "Not Evaluated".to_string(),
                performance_status: "Not Measured".to_string(),
                security_status: "No Incidents".to_string(),
            },
        };

        let result = generator.format_report(&report, &ReportFormat::Pdf);
        assert!(result.is_err());
        let err_msg = format!("{}", result.unwrap_err());
        assert!(
            err_msg.contains("v1.0"),
            "PDF error should mention v1.0 plan"
        );
    }

    #[test]
    fn test_format_report_html() {
        let generator = ReportGenerator::new();

        let report = AuditReport {
            report_id: "test-report-html-001".to_string(),
            report_type: ReportType::Comprehensive,
            operation_id: "test-op-html-001".to_string(),
            created_at: "2025-01-11T00:00:00Z".to_string(),
            summary: AuditSummary {
                total_events: 42,
                compliance_status: "Compliant".to_string(),
                performance_status: "Within Baseline".to_string(),
                security_status: "No Incidents".to_string(),
            },
        };

        let html = generator
            .format_report(&report, &ReportFormat::Html)
            .expect("Should generate HTML report");

        assert!(
            html.contains("<!DOCTYPE html>"),
            "Should be a valid HTML document"
        );
        assert!(html.contains("<html"), "Should have html tag");
        assert!(html.contains("</html>"), "Should close html tag");
        assert!(
            html.contains("test-report-html-001"),
            "Should include report ID"
        );
        assert!(
            html.contains("test-op-html-001"),
            "Should include operation ID"
        );
        assert!(html.contains("42"), "Should include total events count");
        assert!(
            html.contains("Compliant"),
            "Should include compliance status"
        );
        assert!(
            html.contains("badge-green"),
            "Should use green badge for Compliant"
        );
        assert!(
            html.contains("No Incidents"),
            "Should include security status"
        );
        assert!(html.contains("<style>"), "Should embed CSS");
        assert!(html.contains("Audit Report"), "Should have report heading");
    }

    #[test]
    fn test_aggregate_compliance_status_with_failures() {
        let generator = ReportGenerator::new();
        let context = AuditContext::new();

        let payload = super::super::AuditPayload::ComplianceCheck {
            compliance_framework: "SOX".to_string(),
            validation_result: super::super::event::ComplianceValidationResult::NonCompliant,
            violations: vec![],
            remediation_required: true,
            next_review_date: None,
        };

        let event = super::super::AuditEvent::new(
            super::super::AuditEventType::ComplianceCheck,
            context,
            payload,
        );

        let events = vec![event];
        let status = generator.aggregate_compliance_status(&events);

        assert_eq!(status, "Non-Compliant");
    }

    #[test]
    fn test_aggregate_performance_status_with_regression() {
        let generator = ReportGenerator::new();
        let context = AuditContext::new();

        let payload = super::super::AuditPayload::PerformanceMeasurement {
            measurement_type: super::super::event::PerformanceMeasurementType::Throughput,
            baseline_id: Some("baseline-001".to_string()),
            metrics: super::super::event::PerformanceMetrics {
                throughput_bytes_per_sec: 50_000_000,
                latency_ms: 100,
                cpu_usage_percent: 50.0,
                memory_usage_mb: 256,
                io_operations: 1000,
            },
            comparison_result: Some(super::super::event::ComparisonResult::BelowBaseline),
            regression_detected: true,
        };

        let event = super::super::AuditEvent::new(
            super::super::AuditEventType::PerformanceMeasurement,
            context,
            payload,
        );

        let events = vec![event];
        let status = generator.aggregate_performance_status(&events);

        assert_eq!(status, "Regression Detected");
    }

    #[test]
    fn test_aggregate_security_status_with_critical_incidents() {
        let generator = ReportGenerator::new();
        let context = AuditContext::new();

        let payload = super::super::AuditPayload::SecurityEvent {
            security_event_type: super::super::event::SecurityEventType::DataBreach,
            severity: "Critical".to_string(),
            affected_resources: vec![],
            threat_indicators: vec![],
            remediation_actions: vec![],
            incident_id: Some("INC-001".to_string()),
        };

        let event = super::super::AuditEvent::new(
            super::super::AuditEventType::SecurityEvent,
            context,
            payload,
        )
        .with_severity(AuditSeverity::Critical);

        let events = vec![event];
        let status = generator.aggregate_security_status(&events);

        assert_eq!(status, "1 Critical Incidents");
    }
}
