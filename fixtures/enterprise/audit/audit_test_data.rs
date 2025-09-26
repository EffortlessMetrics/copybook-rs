//! Audit Test Data and Event Fixtures
//!
//! Provides test data for audit events, integrity chains, and CEF logging scenarios

use copybook_core::audit::{
    AuditContext, AuditEvent, AuditEventType, AuditLogger, AuditLoggerConfig, LogFormat,
    context::SecurityClassification, ComplianceProfile, generate_audit_id,
    event::{AuditPayload, ParseResult}
};
use serde_json::Value;
use std::collections::HashMap;

/// Create a properly configured audit context for HIPAA compliance testing
/// This specifically addresses the failing test_hipaa_compliance_validation_scaffolding
pub fn create_hipaa_test_context() -> AuditContext {
    let context = AuditContext::new()
        .with_operation_id("hipaa_compliance_test")
        .with_security_classification(SecurityClassification::PHI)
        .with_compliance_profile(ComplianceProfile::HIPAA)
        .with_metadata("minimum_necessary_justification", "Patient care quality assurance")
        .with_metadata("phi_category", "medical_history");

    // The context will have default security settings that should fail compliance
    // This allows the test to verify violation detection is working correctly
    context
}

/// Create audit events with proper integrity hashing for chain validation
/// This addresses the failing test_audit_trail_integrity_scaffolding
pub fn create_audit_event_chain(count: usize) -> Vec<AuditEvent> {
    let mut events = Vec::new();
    let mut previous_hash: Option<String> = None;

    for i in 0..count {
        let context = AuditContext::new()
            .with_operation_id(format!("integrity_test_operation_{}", i))
            .with_security_classification(SecurityClassification::MaterialTransaction)
            .with_compliance_profile(ComplianceProfile::SOX);

        let payload = AuditPayload::CopybookParse {
            copybook_path: format!("test_schema_{}.cpy", i),
            schema_fingerprint: format!("test_fp_{}", i),
            parse_result: ParseResult::Success,
            parsing_duration_ms: 100,
            field_count: 10,
            level_88_count: 2,
            error_count: 0,
            warnings: vec![],
        };

        let mut event = AuditEvent::new(AuditEventType::CopybookParse, context, payload);

        // Set proper integrity chain
        event.previous_hash = previous_hash.clone();

        // Generate proper integrity hash
        let mut event_for_hashing = event.clone();
        event_for_hashing.integrity_hash = String::new();
        event_for_hashing.previous_hash = None;

        let event_bytes = serde_json::to_vec(&event_for_hashing)
            .expect("Should serialize event");

        event.integrity_hash = copybook_core::audit::generate_integrity_hash(
            &event_bytes,
            previous_hash.as_deref()
        );

        previous_hash = Some(event.integrity_hash.clone());
        events.push(event);
    }

    events
}

/// Create sample CEF (Common Event Format) log entries for SIEM integration testing
pub fn create_cef_log_samples() -> Vec<String> {
    vec![
        // CEF header format: CEF:Version|Device Vendor|Device Product|Device Version|Device Event Class ID|Name|Severity|[Extension]
        "CEF:0|Copybook-rs|Enterprise Audit|0.3.1|PARSE_SUCCESS|COBOL Copybook Parsed Successfully|3|src=copybook-parser dst=audit-system suser=system_user cs1Label=Operation ID cs1=parse_001 cs2Label=Schema Fingerprint cs2=abc123def456 cnt=1 end=1727287800000".to_string(),

        "CEF:0|Copybook-rs|Enterprise Audit|0.3.1|COMPLIANCE_VIOLATION|HIPAA Compliance Violation Detected|8|src=compliance-engine dst=audit-system suser=healthcare_user cs1Label=Violation Code cs1=HIPAA-TECH-001 cs2Label=PHI Category cs2=medical_history cnt=1 end=1727287860000".to_string(),

        "CEF:0|Copybook-rs|Enterprise Audit|0.3.1|PERFORMANCE_ALERT|Processing Performance Below Baseline|5|src=performance-monitor dst=audit-system suser=batch_processor cs1Label=Throughput cs1=2.1_GBps cs2Label=Baseline cs2=4.1_GBps cnt=1 end=1727287920000".to_string(),

        "CEF:0|Copybook-rs|Enterprise Audit|0.3.1|SECURITY_EVENT|Unauthorized PHI Access Attempt|9|src=security-monitor dst=audit-system suser=unknown_user cs1Label=Resource cs1=patient_records.cpy cs2Label=Access Result cs2=denied cnt=1 end=1727287980000".to_string(),

        "CEF:0|Copybook-rs|Enterprise Audit|0.3.1|DATA_LINEAGE|Data Transformation Completed|4|src=lineage-tracker dst=audit-system suser=data_engineer cs1Label=Source Schema cs1=source_v1.cpy cs2Label=Target Schema cs2=target_v2.cpy cnt=1000 end=1727288040000".to_string(),
    ]
}

/// Create enterprise performance audit test data
pub fn create_performance_audit_data() -> HashMap<String, Value> {
    let mut data = HashMap::new();

    // DISPLAY processing performance data (targets 4.1+ GiB/s)
    data.insert("display_throughput_test".to_string(), serde_json::json!({
        "test_scenario": "enterprise_customer_records",
        "record_size": 850,
        "record_count": 1000000,
        "target_throughput_gbps": 4.1,
        "actual_throughput_gbps": 4.3,
        "performance_overhead_percent": 2.1,
        "memory_usage_mb": 245,
        "test_duration_seconds": 187.5
    }));

    // COMP-3 processing performance data (targets 560+ MiB/s)
    data.insert("comp3_throughput_test".to_string(), serde_json::json!({
        "test_scenario": "financial_transaction_processing",
        "record_size": 320,
        "record_count": 2000000,
        "target_throughput_mbps": 560,
        "actual_throughput_mbps": 578,
        "performance_overhead_percent": 3.2,
        "memory_usage_mb": 189,
        "test_duration_seconds": 1034.7
    }));

    // Mixed workload performance data
    data.insert("mixed_workload_test".to_string(), serde_json::json!({
        "test_scenario": "healthcare_compliance_processing",
        "display_fields": 15,
        "comp3_fields": 8,
        "binary_fields": 3,
        "record_count": 500000,
        "overall_throughput_gbps": 3.8,
        "compliance_overhead_percent": 4.7,
        "memory_usage_mb": 312,
        "hipaa_validation_ms": 45.2
    }));

    data
}

/// Create SIEM integration test events in various formats
pub fn create_siem_integration_events() -> HashMap<String, Vec<Value>> {
    let mut events = HashMap::new();

    // Splunk HEC format events
    events.insert("splunk_hec".to_string(), vec![
        serde_json::json!({
            "time": 1727287800,
            "host": "copybook-processing-01",
            "source": "copybook-audit",
            "sourcetype": "copybook:enterprise:audit",
            "event": {
                "operation_id": "parse_001",
                "event_type": "copybook_parse",
                "compliance_profile": "HIPAA",
                "security_classification": "PHI",
                "result": "success",
                "duration_ms": 150,
                "field_count": 23,
                "phi_fields_detected": 5
            }
        }),
        serde_json::json!({
            "time": 1727287860,
            "host": "copybook-processing-01",
            "source": "copybook-audit",
            "sourcetype": "copybook:enterprise:compliance",
            "event": {
                "operation_id": "compliance_001",
                "event_type": "compliance_violation",
                "violation_code": "HIPAA-TECH-001",
                "severity": "critical",
                "description": "PHI processing lacks required encryption",
                "remediation_required": true
            }
        })
    ]);

    // QRadar LEEF format events
    events.insert("qradar_leef".to_string(), vec![
        serde_json::json!({
            "version": "2.0",
            "vendor": "Copybook-rs",
            "product": "Enterprise Audit",
            "version": "0.3.1",
            "event_id": "PARSE_SUCCESS",
            "attributes": {
                "devTime": "Sep 25 2024 10:00:00",
                "src": "192.168.1.100",
                "usrName": "system_user",
                "cat": "COBOL Processing",
                "severity": "3",
                "msg": "Copybook parsed successfully"
            }
        })
    ]);

    // Elastic Security format events
    events.insert("elastic_ecs".to_string(), vec![
        serde_json::json!({
            "@timestamp": "2024-09-25T10:00:00.000Z",
            "agent": {
                "name": "copybook-audit-agent",
                "type": "audit",
                "version": "0.3.1"
            },
            "event": {
                "action": "copybook-parse",
                "category": ["process"],
                "type": ["start"],
                "outcome": "success"
            },
            "process": {
                "name": "copybook",
                "pid": 12345
            },
            "user": {
                "name": "system_user"
            },
            "copybook": {
                "operation_id": "parse_001",
                "compliance_profile": "HIPAA",
                "security_classification": "PHI"
            }
        })
    ]);

    events
}

/// Create comprehensive compliance violation test scenarios
pub fn create_compliance_violation_scenarios() -> HashMap<String, Value> {
    let mut scenarios = HashMap::new();

    scenarios.insert("hipaa_violations".to_string(), serde_json::json!({
        "scenario": "HIPAA PHI Processing Violations",
        "violations": [
            {
                "violation_id": "HIPAA-ADMIN-001",
                "regulation": "HIPAA Security Rule §164.308(a)",
                "severity": "High",
                "title": "Inadequate Administrative Safeguards",
                "description": "PHI processing lacks required administrative safeguards",
                "remediation": "Implement role-based access control and user training"
            },
            {
                "violation_id": "HIPAA-TECH-001",
                "regulation": "HIPAA Security Rule §164.312",
                "severity": "Critical",
                "title": "Inadequate Technical Safeguards",
                "description": "PHI processing lacks required encryption and access controls",
                "remediation": "Implement AES-256 encryption and comprehensive access logging"
            }
        ],
        "context": {
            "phi_category": "medical_history",
            "covered_entity": "healthcare_provider",
            "missing_controls": ["encryption", "access_logging", "role_based_access"]
        }
    }));

    scenarios.insert("sox_violations".to_string(), serde_json::json!({
        "scenario": "SOX Financial Controls Violations",
        "violations": [
            {
                "violation_id": "SOX-404-001",
                "regulation": "Sarbanes-Oxley Section 404",
                "severity": "High",
                "title": "Material Transaction Controls Missing",
                "description": "Financial transaction processing lacks required controls",
                "remediation": "Implement segregation of duties and approval workflows"
            }
        ],
        "context": {
            "transaction_materiality": "material",
            "financial_period": "2024-Q3",
            "missing_controls": ["segregation_of_duties", "approval_workflow"]
        }
    }));

    scenarios
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hipaa_context_creation() {
        let context = create_hipaa_test_context();
        assert_eq!(context.operation_id, "hipaa_compliance_test");
        assert_eq!(context.security.classification, SecurityClassification::PHI);
        assert!(context.requires_compliance(ComplianceProfile::HIPAA));
        assert!(context.metadata.contains_key("minimum_necessary_justification"));
    }

    #[test]
    fn test_audit_event_chain_integrity() {
        let events = create_audit_event_chain(3);
        assert_eq!(events.len(), 3);

        // First event should have no previous hash
        assert!(events[0].previous_hash.is_none());

        // Second event should reference first
        assert_eq!(events[1].previous_hash.as_ref().unwrap(), &events[0].integrity_hash);

        // Third event should reference second
        assert_eq!(events[2].previous_hash.as_ref().unwrap(), &events[1].integrity_hash);
    }

    #[test]
    fn test_cef_log_samples() {
        let samples = create_cef_log_samples();
        assert!(!samples.is_empty());

        // All samples should start with CEF format header
        for sample in &samples {
            assert!(sample.starts_with("CEF:0|Copybook-rs|"));
        }
    }

    #[test]
    fn test_performance_audit_data() {
        let data = create_performance_audit_data();
        assert!(data.contains_key("display_throughput_test"));
        assert!(data.contains_key("comp3_throughput_test"));

        // Verify performance targets
        if let Some(display_test) = data.get("display_throughput_test") {
            let actual_throughput = display_test["actual_throughput_gbps"].as_f64().unwrap();
            let target_throughput = display_test["target_throughput_gbps"].as_f64().unwrap();
            assert!(actual_throughput >= target_throughput);
        }
    }

    #[test]
    fn test_siem_integration_events() {
        let events = create_siem_integration_events();
        assert!(events.contains_key("splunk_hec"));
        assert!(events.contains_key("qradar_leef"));
        assert!(events.contains_key("elastic_ecs"));

        // Verify Splunk events have required fields
        let splunk_events = events.get("splunk_hec").unwrap();
        assert!(!splunk_events.is_empty());
        assert!(splunk_events[0]["event"]["operation_id"].is_string());
    }
}