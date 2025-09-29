//! Enterprise Audit and Compliance Test Fixtures for Issue #52
//!
//! Provides comprehensive audit data for regulatory compliance validation
//! Supports SOX, HIPAA, PCI-DSS, GDPR, and ISO 27001 compliance testing

use serde_json::{Value, Map};
use std::collections::HashMap;

/// Enterprise audit event representing regulatory compliance activities
#[derive(Debug, Clone)]
pub struct AuditEvent {
    pub event_id: String,
    pub event_type: String,
    pub timestamp: String,
    pub compliance_profile: String,
    pub severity: String,
    pub description: String,
    pub metadata: HashMap<String, Value>,
}

impl AuditEvent {
    pub fn new(event_type: &str, compliance_profile: &str, description: &str) -> Self {
        use std::time::{SystemTime, UNIX_EPOCH};
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();

        Self {
            event_id: format!("AUDIT-{}-{:08x}", timestamp, timestamp as u32),
            event_type: event_type.to_string(),
            timestamp: "2024-09-25T10:30:00Z".to_string(), // Fixed for deterministic tests
            compliance_profile: compliance_profile.to_string(),
            severity: "info".to_string(),
            description: description.to_string(),
            metadata: HashMap::new(),
        }
    }

    pub fn with_severity(mut self, severity: &str) -> Self {
        self.severity = severity.to_string();
        self
    }

    pub fn with_metadata(mut self, key: &str, value: Value) -> Self {
        self.metadata.insert(key.to_string(), value);
        self
    }

    pub fn to_json(&self) -> serde_json::Result<Value> {
        let mut map = Map::new();
        map.insert("event_id".to_string(), Value::String(self.event_id.clone()));
        map.insert("event_type".to_string(), Value::String(self.event_type.clone()));
        map.insert("timestamp".to_string(), Value::String(self.timestamp.clone()));
        map.insert("compliance_profile".to_string(), Value::String(self.compliance_profile.clone()));
        map.insert("severity".to_string(), Value::String(self.severity.clone()));
        map.insert("description".to_string(), Value::String(self.description.clone()));

        for (key, value) in &self.metadata {
            map.insert(key.clone(), value.clone());
        }

        Ok(Value::Object(map))
    }
}

/// Create SOX compliance audit fixtures
pub fn create_sox_compliance_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "financial_control_validation",
            "SOX",
            "Material transaction processing controls validated"
        )
        .with_severity("info")
        .with_metadata("transaction_materiality", Value::String("material".to_string()))
        .with_metadata("control_effectiveness", Value::String("effective".to_string()))
        .with_metadata("segregation_of_duties", Value::Bool(true))
        .with_metadata("approval_workflow", Value::Bool(true))
        .with_metadata("audit_trail_complete", Value::Bool(true)),

        AuditEvent::new(
            "performance_floor_compliance",
            "SOX",
            "Performance metrics exceed regulatory floors for financial processing"
        )
        .with_severity("info")
        .with_metadata("display_performance_gibs", Value::Number(serde_json::Number::from_f64(4.22).unwrap()))
        .with_metadata("comp3_performance_mibs", Value::Number(serde_json::Number::from_f64(571.0).unwrap()))
        .with_metadata("financial_floor_display", Value::Number(serde_json::Number::from_f64(0.0745).unwrap()))
        .with_metadata("financial_floor_comp3", Value::Number(serde_json::Number::from(40)))
        .with_metadata("safety_margin_display", Value::Number(serde_json::Number::from(52)))
        .with_metadata("safety_margin_comp3", Value::Number(serde_json::Number::from(14))),

        AuditEvent::new(
            "sox_section_404_compliance",
            "SOX",
            "Section 404 internal controls assessment completed"
        )
        .with_severity("info")
        .with_metadata("management_assessment", Value::String("effective".to_string()))
        .with_metadata("external_auditor_opinion", Value::String("unqualified".to_string()))
        .with_metadata("material_weaknesses", Value::Number(serde_json::Number::from(0)))
        .with_metadata("significant_deficiencies", Value::Number(serde_json::Number::from(0)))
        .with_metadata("remediation_required", Value::Bool(false)),
    ]
}

/// Create HIPAA compliance audit fixtures
pub fn create_hipaa_compliance_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "phi_processing_controls",
            "HIPAA",
            "Protected Health Information processing controls validated"
        )
        .with_severity("info")
        .with_metadata("phi_fields_processed", Value::Number(serde_json::Number::from(127)))
        .with_metadata("encryption_status", Value::String("aes_256".to_string()))
        .with_metadata("access_logging", Value::Bool(true))
        .with_metadata("minimum_necessary_standard", Value::Bool(true))
        .with_metadata("authorization_valid", Value::Bool(true)),

        AuditEvent::new(
            "hipaa_security_rule_compliance",
            "HIPAA",
            "Security Rule §164.312 technical safeguards implemented"
        )
        .with_severity("info")
        .with_metadata("access_control", Value::Bool(true))
        .with_metadata("audit_controls", Value::Bool(true))
        .with_metadata("integrity", Value::Bool(true))
        .with_metadata("person_authentication", Value::Bool(true))
        .with_metadata("transmission_security", Value::Bool(true))
        .with_metadata("performance_impact_percent", Value::Number(serde_json::Number::from_f64(3.2).unwrap())),

        AuditEvent::new(
            "phi_performance_validation",
            "HIPAA",
            "PHI processing performance meets healthcare compliance requirements"
        )
        .with_severity("info")
        .with_metadata("healthcare_throughput_gibs", Value::Number(serde_json::Number::from_f64(4.12).unwrap()))
        .with_metadata("compliance_overhead_percent", Value::Number(serde_json::Number::from_f64(4.7).unwrap()))
        .with_metadata("phi_encryption_latency_ms", Value::Number(serde_json::Number::from_f64(0.5).unwrap()))
        .with_metadata("audit_logging_overhead_percent", Value::Number(serde_json::Number::from_f64(1.3).unwrap())),
    ]
}

/// Create PCI-DSS compliance audit fixtures
pub fn create_pci_dss_compliance_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "cardholder_data_protection",
            "PCI_DSS",
            "Cardholder data environment protection validated"
        )
        .with_severity("info")
        .with_metadata("pci_dss_level", Value::String("level_1".to_string()))
        .with_metadata("cardholder_data_encryption", Value::Bool(true))
        .with_metadata("key_management", Value::String("compliant".to_string()))
        .with_metadata("network_segmentation", Value::Bool(true))
        .with_metadata("vulnerability_scanning", Value::String("quarterly".to_string())),

        AuditEvent::new(
            "payment_processing_performance",
            "PCI_DSS",
            "Payment processing performance maintains compliance standards"
        )
        .with_severity("info")
        .with_metadata("payment_throughput_tps", Value::Number(serde_json::Number::from(15000)))
        .with_metadata("encryption_overhead_percent", Value::Number(serde_json::Number::from_f64(2.1).unwrap()))
        .with_metadata("tokenization_enabled", Value::Bool(true))
        .with_metadata("secure_transmission", Value::Bool(true)),
    ]
}

/// Create GDPR compliance audit fixtures
pub fn create_gdpr_compliance_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "data_protection_rights",
            "GDPR",
            "Data subject rights implementation validated"
        )
        .with_severity("info")
        .with_metadata("right_to_access", Value::Bool(true))
        .with_metadata("right_to_rectification", Value::Bool(true))
        .with_metadata("right_to_erasure", Value::Bool(true))
        .with_metadata("right_to_portability", Value::Bool(true))
        .with_metadata("consent_management", Value::Bool(true)),

        AuditEvent::new(
            "gdpr_performance_impact",
            "GDPR",
            "GDPR compliance controls maintain acceptable performance"
        )
        .with_severity("info")
        .with_metadata("data_processing_gibs", Value::Number(serde_json::Number::from_f64(4.08).unwrap()))
        .with_metadata("privacy_overhead_percent", Value::Number(serde_json::Number::from_f64(3.8).unwrap()))
        .with_metadata("consent_validation_ms", Value::Number(serde_json::Number::from_f64(12.5).unwrap()))
        .with_metadata("data_lineage_tracking", Value::Bool(true)),
    ]
}

/// Create ISO 27001 compliance audit fixtures
pub fn create_iso27001_compliance_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "information_security_management",
            "ISO_27001",
            "Information Security Management System (ISMS) compliance validated"
        )
        .with_severity("info")
        .with_metadata("isms_effectiveness", Value::String("effective".to_string()))
        .with_metadata("security_controls", Value::Number(serde_json::Number::from(114)))
        .with_metadata("controls_implemented", Value::Number(serde_json::Number::from(114)))
        .with_metadata("risk_assessment_current", Value::Bool(true))
        .with_metadata("management_review_current", Value::Bool(true)),

        AuditEvent::new(
            "security_performance_balance",
            "ISO_27001",
            "Security controls maintain performance without degradation"
        )
        .with_severity("info")
        .with_metadata("secure_processing_gibs", Value::Number(serde_json::Number::from_f64(4.15).unwrap()))
        .with_metadata("security_overhead_percent", Value::Number(serde_json::Number::from_f64(2.8).unwrap()))
        .with_metadata("incident_response_time_ms", Value::Number(serde_json::Number::from(50)))
        .with_metadata("monitoring_coverage_percent", Value::Number(serde_json::Number::from(100))),
    ]
}

/// Create comprehensive compliance violation scenarios
pub fn create_compliance_violation_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "performance_floor_violation",
            "SOX",
            "Financial processing performance below regulatory floor"
        )
        .with_severity("critical")
        .with_metadata("actual_display_gibs", Value::Number(serde_json::Number::from_f64(0.065).unwrap()))
        .with_metadata("required_display_gibs", Value::Number(serde_json::Number::from_f64(0.0745).unwrap()))
        .with_metadata("violation_magnitude_percent", Value::Number(serde_json::Number::from_f64(12.7).unwrap()))
        .with_metadata("remediation_required", Value::Bool(true))
        .with_metadata("escalation_level", Value::String("immediate".to_string())),

        AuditEvent::new(
            "phi_encryption_failure",
            "HIPAA",
            "PHI processing encryption controls failed validation"
        )
        .with_severity("high")
        .with_metadata("phi_exposure_risk", Value::String("high".to_string()))
        .with_metadata("affected_records", Value::Number(serde_json::Number::from(15000)))
        .with_metadata("notification_required", Value::Bool(true))
        .with_metadata("breach_risk_assessment", Value::String("required".to_string()))
        .with_metadata("remediation_timeline_hours", Value::Number(serde_json::Number::from(24))),

        AuditEvent::new(
            "cardholder_data_exposure",
            "PCI_DSS",
            "Potential cardholder data exposure detected"
        )
        .with_severity("critical")
        .with_metadata("exposure_scope", Value::String("limited".to_string()))
        .with_metadata("payment_brands_notification", Value::Bool(true))
        .with_metadata("forensic_investigation", Value::Bool(true))
        .with_metadata("compliance_status", Value::String("suspended".to_string()))
        .with_metadata("restoration_timeline_hours", Value::Number(serde_json::Number::from(72))),
    ]
}

/// Create audit trail integrity fixtures
pub fn create_audit_trail_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "audit_trail_integrity_check",
            "ENTERPRISE",
            "Audit trail integrity validation completed"
        )
        .with_severity("info")
        .with_metadata("total_events", Value::Number(serde_json::Number::from(1247)))
        .with_metadata("integrity_hash_valid", Value::Bool(true))
        .with_metadata("chain_continuity", Value::Bool(true))
        .with_metadata("tamper_evidence", Value::String("none_detected".to_string()))
        .with_metadata("validation_timestamp", Value::String("2024-09-25T10:30:00Z".to_string())),

        AuditEvent::new(
            "performance_audit_trail",
            "ENTERPRISE",
            "Performance measurement audit trail maintained"
        )
        .with_severity("info")
        .with_metadata("measurements_recorded", Value::Number(serde_json::Number::from(156)))
        .with_metadata("baseline_promotions", Value::Number(serde_json::Number::from(3)))
        .with_metadata("regression_detections", Value::Number(serde_json::Number::from(0)))
        .with_metadata("compliance_checks", Value::Number(serde_json::Number::from(89)))
        .with_metadata("trail_completeness", Value::Number(serde_json::Number::from(100))),
    ]
}

/// Create risk assessment audit fixtures
pub fn create_risk_assessment_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "performance_risk_assessment",
            "ENTERPRISE",
            "Performance degradation risk assessment completed"
        )
        .with_severity("info")
        .with_metadata("risk_level", Value::String("minimal".to_string()))
        .with_metadata("performance_stability", Value::Number(serde_json::Number::from_f64(98.7).unwrap()))
        .with_metadata("enterprise_readiness", Value::String("production_ready".to_string()))
        .with_metadata("deployment_risk", Value::String("low".to_string()))
        .with_metadata("monitoring_coverage", Value::Number(serde_json::Number::from(100))),

        AuditEvent::new(
            "compliance_risk_assessment",
            "ENTERPRISE",
            "Regulatory compliance risk assessment completed"
        )
        .with_severity("info")
        .with_metadata("overall_compliance_score", Value::Number(serde_json::Number::from_f64(97.8).unwrap()))
        .with_metadata("sox_risk", Value::String("low".to_string()))
        .with_metadata("hipaa_risk", Value::String("low".to_string()))
        .with_metadata("pci_dss_risk", Value::String("low".to_string()))
        .with_metadata("gdpr_risk", Value::String("low".to_string()))
        .with_metadata("iso27001_risk", Value::String("low".to_string())),
    ]
}

/// Create business continuity planning fixtures
pub fn create_business_continuity_fixtures() -> Vec<AuditEvent> {
    vec![
        AuditEvent::new(
            "disaster_recovery_validation",
            "ENTERPRISE",
            "Disaster recovery procedures validated for performance systems"
        )
        .with_severity("info")
        .with_metadata("recovery_time_objective_hours", Value::Number(serde_json::Number::from(4)))
        .with_metadata("recovery_point_objective_hours", Value::Number(serde_json::Number::from(1)))
        .with_metadata("backup_systems_tested", Value::Bool(true))
        .with_metadata("failover_procedures_validated", Value::Bool(true))
        .with_metadata("data_integrity_verified", Value::Bool(true)),

        AuditEvent::new(
            "performance_continuity_plan",
            "ENTERPRISE",
            "Performance monitoring continuity plan validated"
        )
        .with_severity("info")
        .with_metadata("monitoring_redundancy", Value::Bool(true))
        .with_metadata("alert_escalation_tested", Value::Bool(true))
        .with_metadata("baseline_backup_current", Value::Bool(true))
        .with_metadata("emergency_contacts_verified", Value::Bool(true))
        .with_metadata("communication_plan_tested", Value::Bool(true)),
    ]
}

/// Create comprehensive enterprise audit fixture collection
pub fn create_comprehensive_audit_fixtures() -> HashMap<String, Vec<AuditEvent>> {
    let mut fixtures = HashMap::new();

    fixtures.insert("sox_compliance".to_string(), create_sox_compliance_fixtures());
    fixtures.insert("hipaa_compliance".to_string(), create_hipaa_compliance_fixtures());
    fixtures.insert("pci_dss_compliance".to_string(), create_pci_dss_compliance_fixtures());
    fixtures.insert("gdpr_compliance".to_string(), create_gdpr_compliance_fixtures());
    fixtures.insert("iso27001_compliance".to_string(), create_iso27001_compliance_fixtures());
    fixtures.insert("compliance_violations".to_string(), create_compliance_violation_fixtures());
    fixtures.insert("audit_trail".to_string(), create_audit_trail_fixtures());
    fixtures.insert("risk_assessment".to_string(), create_risk_assessment_fixtures());
    fixtures.insert("business_continuity".to_string(), create_business_continuity_fixtures());

    fixtures
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sox_compliance_fixtures() {
        let fixtures = create_sox_compliance_fixtures();
        assert!(!fixtures.is_empty());

        for fixture in fixtures {
            assert_eq!(fixture.compliance_profile, "SOX");
            assert!(fixture.to_json().is_ok());
        }
    }

    #[test]
    fn test_hipaa_compliance_fixtures() {
        let fixtures = create_hipaa_compliance_fixtures();
        assert!(!fixtures.is_empty());

        for fixture in fixtures {
            assert_eq!(fixture.compliance_profile, "HIPAA");
            assert!(fixture.metadata.contains_key("phi_fields_processed") ||
                   fixture.metadata.contains_key("healthcare_throughput_gibs"));
        }
    }

    #[test]
    fn test_compliance_violation_fixtures() {
        let fixtures = create_compliance_violation_fixtures();
        assert!(!fixtures.is_empty());

        for fixture in fixtures {
            assert!(fixture.severity == "critical" || fixture.severity == "high");
            assert!(fixture.metadata.contains_key("remediation_required") ||
                   fixture.metadata.contains_key("notification_required"));
        }
    }

    #[test]
    fn test_comprehensive_audit_fixtures() {
        let all_fixtures = create_comprehensive_audit_fixtures();
        assert!(!all_fixtures.is_empty());

        let expected_categories = vec![
            "sox_compliance", "hipaa_compliance", "pci_dss_compliance",
            "gdpr_compliance", "iso27001_compliance", "compliance_violations",
            "audit_trail", "risk_assessment", "business_continuity"
        ];

        for category in expected_categories {
            assert!(all_fixtures.contains_key(category));
            assert!(!all_fixtures[category].is_empty());
        }
    }

    #[test]
    fn test_audit_event_json_serialization() {
        let event = AuditEvent::new("test_event", "TEST", "Test description")
            .with_severity("info")
            .with_metadata("test_key", Value::String("test_value".to_string()));

        let json = event.to_json().unwrap();
        assert!(json.is_object());

        let obj = json.as_object().unwrap();
        assert!(obj.contains_key("event_id"));
        assert!(obj.contains_key("event_type"));
        assert!(obj.contains_key("compliance_profile"));
        assert!(obj.contains_key("test_key"));
    }
}