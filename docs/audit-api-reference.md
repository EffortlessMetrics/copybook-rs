<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Enterprise Audit API Reference

## Overview

This document provides comprehensive API reference for the copybook-rs enterprise audit system, including core structures, configuration options, and usage examples for regulatory compliance and enterprise monitoring.

## Core API Types

### AuditContext

Primary context object for all audit operations.

```rust
use copybook_core::audit::{AuditContext, EnvironmentContext, SecurityContext, ProcessingConfig};

// Create audit context for SOX-compliant financial data processing
let audit_context = AuditContext::new()
    .with_operation_id("financial_data_transform_20241225_001")
    .with_user("system_user")
    .with_environment(EnvironmentContext::current())
    .with_compliance_profiles(&[ComplianceProfile::SOX, ComplianceProfile::GDPR])
    .with_security_classification(SecurityClassification::Confidential);
```

### AuditEvent

Core audit event structure for all audit trail entries.

```rust
use copybook_core::audit::{AuditEvent, AuditEventType, AuditPayload};

// Create parsing audit event
let parse_event = AuditEvent::new(
    AuditEventType::CopybookParse,
    audit_context.clone(),
    AuditPayload::CopybookParse {
        copybook_path: "customer-schema.cpy".to_string(),
        schema_fingerprint: "a1b2c3d4e5f6...".to_string(),
        parse_result: ParseResult::Success,
        parsing_duration_ms: 45,
        field_count: 127,
        level_88_count: 23,
    }
);
```

### ComplianceEngine

Enterprise compliance validation engine.

```rust
use copybook_core::audit::{ComplianceEngine, ComplianceProfile, ComplianceResult};

// Initialize compliance engine with multiple profiles
let compliance_engine = ComplianceEngine::new()
    .with_profiles(&[
        ComplianceProfile::SOX,
        ComplianceProfile::HIPAA,
        ComplianceProfile::GDPR
    ]);

// Validate processing operation for compliance
let compliance_result = compliance_engine
    .validate_processing_operation(&processing_context)
    .await?;

match compliance_result {
    ComplianceResult::Compliant => println!("Processing meets all compliance requirements"),
    ComplianceResult::NonCompliant(violations) => {
        for violation in violations {
            eprintln!("Compliance violation: {}", violation.description);
        }
    }
}
```

## Enterprise Integration APIs

### SIEM Integration

Export audit events to enterprise SIEM systems.

```rust
use copybook_core::audit::{SiemExporter, SiemFormat, SiemConfig};

// Configure SIEM export for Splunk
let siem_config = SiemConfig::new()
    .with_format(SiemFormat::CEF)
    .with_endpoint("https://splunk.company.com:8088/services/collector")
    .with_token("your-hec-token")
    .with_source_type("copybook_audit")
    .with_batch_size(100);

let siem_exporter = SiemExporter::new(siem_config)?;

// Export audit events to SIEM
siem_exporter.export_events(&audit_events).await?;
```

### Performance Monitoring Integration

Integration with enterprise monitoring platforms.

```rust
use copybook_core::audit::{PerformanceMonitor, MetricsExporter, PrometheusConfig};

// Configure Prometheus metrics export
let prometheus_config = PrometheusConfig::new()
    .with_endpoint("http://prometheus:9090")
    .with_job_name("copybook_processing")
    .with_instance("prod-node-01");

let performance_monitor = PerformanceMonitor::new()
    .with_metrics_exporter(MetricsExporter::Prometheus(prometheus_config));

// Record processing metrics
performance_monitor
    .record_processing_metrics(&ProcessingMetrics {
        records_processed: 50000,
        processing_duration_ms: 2340,
        peak_memory_mb: 128,
        throughput_records_per_sec: 21367,
    })
    .await?;
```

## Compliance-Specific APIs

### SOX Compliance

Sarbanes-Oxley compliance features for financial data processing.

```rust
use copybook_core::audit::{SoxCompliance, FinancialDataClassification, InternalControl};

// Configure SOX compliance validation
let sox_compliance = SoxCompliance::new()
    .with_financial_data_classification(FinancialDataClassification::MaterialTransaction)
    .with_internal_controls(&[
        InternalControl::DataIntegrityValidation,
        InternalControl::AccessControlValidation,
        InternalControl::ChangeManagementTracking,
        InternalControl::AuditTrailIntegrity
    ])
    .with_retention_period_years(7);

// Validate SOX compliance for financial processing
let sox_result = sox_compliance
    .validate_financial_processing(&processing_operation)
    .await?;

if sox_result.is_compliant() {
    println!("Processing meets SOX compliance requirements");
} else {
    for control_failure in sox_result.control_failures() {
        eprintln!("SOX control failure: {}", control_failure.description);
    }
}
```

### HIPAA Compliance

Healthcare data processing compliance.

```rust
use copybook_core::audit::{HipaaCompliance, PhiClassification, MinimumNecessary};

// Configure HIPAA compliance for PHI processing
let hipaa_compliance = HipaaCompliance::new()
    .with_phi_classification(PhiClassification::ProtectedHealthInformation)
    .with_minimum_necessary_validation(MinimumNecessary::Strict)
    .with_access_logging_required(true)
    .with_encryption_requirements(EncryptionRequirements::AES256);

// Validate HIPAA compliance
let hipaa_result = hipaa_compliance
    .validate_phi_processing(&processing_context)
    .await?;
```

### GDPR Compliance

European data protection compliance.

```rust
use copybook_core::audit::{GdprCompliance, LegalBasis, ProcessingPurpose};

// Configure GDPR compliance
let gdpr_compliance = GdprCompliance::new()
    .with_legal_basis(LegalBasis::LegitimateInterest)
    .with_processing_purpose(ProcessingPurpose::DataAnalytics)
    .with_data_subject_rights_enabled(true)
    .with_cross_border_transfer_monitoring(true);

// Record GDPR processing activity
gdpr_compliance
    .record_processing_activity(&GdprProcessingRecord {
        data_categories: vec![DataCategory::PersonalIdentifiers, DataCategory::FinancialData],
        recipients: vec![Recipient::InternalAnalytics, Recipient::ExternalAuditor],
        retention_period: RetentionPeriod::Years(5),
        automated_decision_making: false,
    })
    .await?;
```

## Data Lineage APIs

### Field-Level Lineage Tracking

Track data transformations at the field level.

```rust
use copybook_core::audit::{LineageTracker, FieldLineage, TransformationType};

// Initialize lineage tracker
let lineage_tracker = LineageTracker::new()
    .with_source_system("mainframe_cobol_system")
    .with_target_system("modern_data_warehouse");

// Record field-level transformation
lineage_tracker
    .record_field_transformation(&FieldTransformation {
        source_field: FieldLineage {
            field_path: "CUSTOMER-RECORD.CUSTOMER-ID",
            system_id: "mainframe_cobol_system",
            schema_version: "v2.1",
            data_type: "PIC 9(10)",
        },
        target_field: FieldLineage {
            field_path: "customers.customer_id",
            system_id: "modern_data_warehouse",
            schema_version: "v1.0",
            data_type: "BIGINT",
        },
        transformation: TransformationType::DirectMapping,
        quality_score: 1.0,
    })
    .await?;
```

### Impact Analysis

Analyze the impact of schema or processing changes.

```rust
use copybook_core::audit::{ImpactAnalyzer, ChangeType, ImpactAssessment};

// Analyze impact of schema changes
let impact_analyzer = ImpactAnalyzer::new()
    .with_lineage_database(&lineage_db);

let impact_assessment = impact_analyzer
    .analyze_schema_change(&SchemaChange {
        change_type: ChangeType::FieldDataTypeChange,
        field_path: "CUSTOMER-RECORD.ACCOUNT-BALANCE",
        old_type: "PIC S9(13)V99 COMP-3",
        new_type: "PIC S9(15)V99 COMP-3",
    })
    .await?;

println!("Impact assessment:");
println!("  Affected downstream systems: {}", impact_assessment.affected_systems.len());
println!("  Risk level: {:?}", impact_assessment.risk_level);
println!("  Recommended actions: {:?}", impact_assessment.recommended_actions);
```

## Performance Audit APIs

### Baseline Management

Manage performance baselines and regression detection.

```rust
use copybook_core::audit::{BaselineManager, PerformanceBaseline, RegressionDetector};

// Create performance baseline
let baseline_manager = BaselineManager::new()
    .with_storage_backend(StorageBackend::JsonFile("baselines.json"));

// Record new baseline
let baseline = PerformanceBaseline {
    baseline_id: "customer_processing_v2_1".to_string(),
    throughput: ThroughputMetrics {
        display_throughput: 4_200_000_000, // 4.2 GiB/s
        comp3_throughput: 580_000_000,     // 580 MiB/s
        record_rate: 50_000,               // 50k records/sec
        peak_memory_mb: 128,
    },
    created_at: chrono::Utc::now().to_rfc3339(),
};

baseline_manager.record_baseline(baseline).await?;

// Detect performance regression
let regression_detector = RegressionDetector::new()
    .with_threshold_percent(5.0); // 5% degradation threshold

let current_metrics = ThroughputMetrics { /* current performance */ };
let regression_result = regression_detector
    .detect_regression(&baseline, &current_metrics)?;

if let Some(regression) = regression_result {
    eprintln!("Performance regression detected: {}", regression.description);
}
```

### Resource Monitoring

Monitor system resource utilization during processing.

```rust
use copybook_core::audit::{ResourceMonitor, ResourceMetrics, AlertThreshold};

// Configure resource monitoring
let resource_monitor = ResourceMonitor::new()
    .with_alert_thresholds(&[
        AlertThreshold::new("memory_usage_mb", 512.0),
        AlertThreshold::new("cpu_usage_percent", 80.0),
    ]);

// Start monitoring session
let monitoring_session = resource_monitor
    .start_session("customer_data_processing")
    .await?;

// Processing operations occur here...

// Stop monitoring and get metrics
let resource_metrics = monitoring_session.stop().await?;

println!("Resource utilization:");
println!("  Peak memory: {} MB", resource_metrics.peak_memory_mb);
println!("  Average CPU: {:.1}%", resource_metrics.average_cpu_percent);
println!("  I/O operations: {}", resource_metrics.io_operations);
```

## Security Audit APIs

### Access Control Auditing

Monitor and audit data access patterns.

```rust
use copybook_core::audit::{AccessAuditor, AccessEvent, AccessResult};

// Configure access auditing
let access_auditor = AccessAuditor::new()
    .with_anomaly_detection(true)
    .with_failed_access_monitoring(true);

// Record data access event
access_auditor
    .record_access(&AccessEvent {
        user_id: "system_processor",
        resource_type: ResourceType::CopybookSchema,
        resource_id: "customer_schema_v2_1",
        access_type: AccessType::Read,
        source_ip: "10.0.1.100".parse()?,
        user_agent: Some("copybook-cli/0.3.1".to_string()),
        result: AccessResult::Success,
    })
    .await?;

// Detect access anomalies
let anomalies = access_auditor
    .detect_access_anomalies(chrono::Duration::hours(24))
    .await?;

for anomaly in anomalies {
    println!("Access anomaly detected: {}", anomaly.description);
}
```

### Data Security Monitoring

Monitor data security controls and encryption status.

```rust
use copybook_core::audit::{SecurityMonitor, EncryptionStatus, DataClassification};

// Monitor data security
let security_monitor = SecurityMonitor::new()
    .with_encryption_monitoring(true)
    .with_data_classification_enforcement(true);

// Validate security controls for processing operation
let security_validation = security_monitor
    .validate_processing_security(&ProcessingOperation {
        data_classification: DataClassification::Confidential,
        encryption_at_rest: EncryptionStatus::AES256,
        encryption_in_transit: EncryptionStatus::TLS12,
        access_controls: vec![AccessControl::RoleBasedAccess, AccessControl::MultiFactorAuth],
    })
    .await?;

if !security_validation.is_compliant() {
    for violation in security_validation.violations() {
        eprintln!("Security violation: {}", violation.description);
    }
}
```

## Configuration APIs

### Audit Configuration Management

Manage comprehensive audit configuration.

```rust
use copybook_core::audit::{AuditConfig, RetentionPolicy, OutputFormat};

// Load audit configuration from file
let audit_config = AuditConfig::from_file("audit-config.yaml")?;

// Or create programmatically
let audit_config = AuditConfig::new()
    .with_enabled(true)
    .with_output_format(OutputFormat::StructuredJson)
    .with_retention_policy(RetentionPolicy::new()
        .with_default_retention_days(2555) // 7 years
        .with_compliance_retention(ComplianceProfile::SOX, 2555)
        .with_compliance_retention(ComplianceProfile::HIPAA, 2190) // 6 years
    )
    .with_integrity_validation(true)
    .with_cryptographic_signing(true);

// Apply configuration
let audit_system = AuditSystem::with_config(audit_config)?;
```

## Error Handling

All audit APIs use structured error handling with the copybook-rs error taxonomy:

```rust
use copybook_core::audit::{AuditError, AuditErrorCode};

match audit_operation().await {
    Ok(result) => println!("Audit operation successful"),
    Err(AuditError { code: AuditErrorCode::ComplianceViolation, message, context }) => {
        eprintln!("Compliance violation: {}", message);
        if let Some(context) = context {
            eprintln!("Violation context: {:?}", context);
        }
    }
    Err(AuditError { code: AuditErrorCode::SecurityValidationFailed, message, .. }) => {
        eprintln!("Security validation failed: {}", message);
    }
    Err(e) => eprintln!("Audit error: {}", e),
}
```

## Integration Examples

### Complete Enterprise Processing Workflow

```rust
use copybook_core::audit::*;

async fn enterprise_processing_workflow() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize enterprise audit system
    let audit_config = AuditConfig::from_file("enterprise-audit.yaml")?;
    let audit_system = AuditSystem::with_config(audit_config)?;

    // Create processing context with compliance requirements
    let audit_context = AuditContext::new()
        .with_operation_id("customer_data_migration_batch_001")
        .with_user("migration_service")
        .with_compliance_profiles(&[ComplianceProfile::SOX, ComplianceProfile::GDPR])
        .with_security_classification(SecurityClassification::Confidential);

    // Initialize compliance validation
    let compliance_engine = ComplianceEngine::new()
        .with_profiles(&[ComplianceProfile::SOX, ComplianceProfile::GDPR]);

    // Parse copybook with audit trail
    let copybook_text = std::fs::read_to_string("customer-schema.cpy")?;
    let schema = parse_copybook_with_audit(&copybook_text, &ParseOptions::default(), audit_context.clone())?;

    // Validate compliance before processing
    let compliance_result = compliance_engine
        .validate_processing_operation(&audit_context)
        .await?;

    if !compliance_result.is_compliant() {
        return Err("Processing does not meet compliance requirements".into());
    }

    // Process data with comprehensive auditing
    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_audit_context(audit_context.clone());

    let processing_result = decode_file_to_jsonl_with_audit(
        &schema,
        "customer-data.bin",
        "customer-data.jsonl",
        &decode_options
    ).await?;

    // Generate comprehensive audit report
    let audit_report = audit_system
        .generate_comprehensive_report(&audit_context)
        .await?;

    // Export to enterprise systems
    audit_system
        .export_to_siem(&audit_report)
        .await?;

    audit_system
        .export_metrics_to_monitoring(&processing_result.performance_metrics)
        .await?;

    println!("Enterprise processing completed with full audit trail");
    Ok(())
}
```

This comprehensive API reference provides enterprise-grade audit capabilities for regulatory compliance, security monitoring, performance tracking, and complete data lineage in copybook-rs operations.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
