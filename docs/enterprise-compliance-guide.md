<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Enterprise Compliance Guide

## Overview

This guide provides comprehensive information on using copybook-rs for enterprise compliance requirements, including SOX, HIPAA, GDPR, and other regulatory frameworks. It covers configuration, validation, audit trail management, and best practices for regulated industries.

## Supported Compliance Frameworks

### 1. SOX (Sarbanes-Oxley Act) - Financial Services
- **Data Integrity Controls**: Cryptographic validation of all financial data processing
- **Audit Trail Requirements**: Complete audit trail with 7-year retention
- **Access Control Monitoring**: Comprehensive access logging and anomaly detection
- **Change Management**: Full audit trail of schema and processing changes

### 2. HIPAA (Health Insurance Portability and Accountability Act) - Healthcare
- **PHI Protection**: Protected Health Information processing controls
- **Access Logging**: Detailed logging of all PHI access and processing
- **Minimum Necessary**: Validation that only necessary data is processed
- **Breach Detection**: Automated detection of unauthorized access patterns

### 3. GDPR (General Data Protection Regulation) - Data Protection
- **Processing Records**: Comprehensive records of personal data processing
- **Legal Basis Tracking**: Link processing to legal basis and consent
- **Data Subject Rights**: Support for access requests and right to erasure
- **Cross-Border Monitoring**: Track international data transfers

### 4. PCI DSS (Payment Card Industry) - Payment Processing
- **Cardholder Data Protection**: Secure processing of payment card data
- **Encryption Requirements**: End-to-end encryption validation
- **Access Control**: Strict access controls for cardholder data environments
- **Vulnerability Management**: Security control monitoring and validation

## Configuration for Compliance

### SOX Compliance Configuration

```yaml
# sox-compliance.yaml
audit:
  enabled: true
  compliance:
    sox:
      enabled: true
      validation_level: strict
      retention_years: 7

      # SOX-specific controls
      internal_controls:
        - data_integrity_validation
        - access_control_validation
        - change_management_tracking
        - audit_trail_integrity
        - segregation_of_duties

      # Financial data classification
      data_classification:
        - material_transactions
        - financial_statements
        - internal_controls_documentation

      # Reporting requirements
      reporting:
        quarterly_compliance_report: true
        executive_certification: required
        external_auditor_access: enabled

  # Cryptographic integrity for audit trails
  integrity:
    cryptographic_signing: true
    hash_algorithm: SHA-256
    signature_algorithm: RSA-2048

  # Access control auditing
  access_control:
    monitoring_enabled: true
    failed_access_logging: true
    privilege_escalation_detection: true
    segregation_of_duties_validation: true
```

### HIPAA Compliance Configuration

```yaml
# hipaa-compliance.yaml
audit:
  enabled: true
  compliance:
    hipaa:
      enabled: true
      validation_level: strict
      retention_years: 6

      # HIPAA-specific controls
      safeguards:
        - administrative_safeguards
        - physical_safeguards
        - technical_safeguards

      # PHI handling requirements
      phi_protection:
        minimum_necessary: strict
        access_logging: comprehensive
        encryption_required: true
        de_identification_validation: true

      # Breach notification requirements
      breach_detection:
        unauthorized_access_monitoring: true
        data_integrity_monitoring: true
        notification_automation: true

  # Security requirements
  security:
    encryption:
      at_rest: AES-256
      in_transit: TLS-1.2
    access_control:
      unique_user_identification: required
      automatic_logoff: enabled
      encryption_decryption: controlled

  # Audit trail requirements
  audit_trail:
    comprehensive_logging: true
    tamper_protection: enabled
    regular_review: quarterly
```

### GDPR Compliance Configuration

```yaml
# gdpr-compliance.yaml
audit:
  enabled: true
  compliance:
    gdpr:
      enabled: true
      validation_level: strict

      # GDPR principles
      principles:
        - lawfulness_fairness_transparency
        - purpose_limitation
        - data_minimisation
        - accuracy
        - storage_limitation
        - integrity_confidentiality
        - accountability

      # Legal basis tracking
      legal_basis:
        default: legitimate_interest
        consent_tracking: enabled
        vital_interests_documentation: required

      # Data subject rights
      data_subject_rights:
        right_to_access: enabled
        right_to_rectification: enabled
        right_to_erasure: enabled
        right_to_portability: enabled
        right_to_object: enabled

      # Cross-border transfer monitoring
      international_transfers:
        adequacy_decision_validation: true
        standard_contractual_clauses: monitored
        binding_corporate_rules: validated

  # Processing activity records
  processing_records:
    comprehensive_recording: true
    controller_processor_identification: required
    processing_purpose_tracking: detailed
    data_category_classification: automatic
```

## Compliance Implementation Examples

### SOX-Compliant Financial Data Processing

```rust
use copybook_core::audit::*;

async fn sox_compliant_financial_processing() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize SOX compliance engine
    let sox_compliance = SoxCompliance::new()
        .with_financial_data_classification(FinancialDataClassification::MaterialTransaction)
        .with_internal_controls(&[
            InternalControl::DataIntegrityValidation,
            InternalControl::AccessControlValidation,
            InternalControl::ChangeManagementTracking,
            InternalControl::AuditTrailIntegrity,
            InternalControl::SegregationOfDuties,
        ])
        .with_retention_period_years(7)
        .with_quarterly_reporting(true);

    // Create audit context with SOX requirements
    let audit_context = AuditContext::new()
        .with_operation_id("quarterly_financial_processing_q4_2024")
        .with_user("financial_reporting_system")
        .with_compliance_profiles(&[ComplianceProfile::SOX])
        .with_security_classification(SecurityClassification::MaterialTransaction)
        .with_segregation_of_duties_validation(true);

    // Validate SOX compliance before processing
    let compliance_validation = sox_compliance
        .validate_processing_operation(&audit_context)
        .await?;

    if !compliance_validation.is_compliant() {
        let violations: Vec<_> = compliance_validation
            .violations()
            .iter()
            .map(|v| format!("SOX-{}: {}", v.control_id, v.description))
            .collect();

        return Err(format!("SOX compliance violations: {}", violations.join(", ")).into());
    }

    // Process financial data with comprehensive auditing
    let processing_options = ProcessingOptions::new()
        .with_data_integrity_validation(DataIntegrityLevel::Cryptographic)
        .with_access_control_enforcement(AccessControlLevel::Strict)
        .with_audit_trail_level(AuditTrailLevel::Comprehensive)
        .with_change_management_tracking(true);

    let financial_schema = parse_copybook_with_sox_validation(
        &financial_copybook_text,
        &ParseOptions::default(),
        audit_context.clone()
    ).await?;

    let processing_result = process_financial_data_with_sox_compliance(
        &financial_schema,
        "quarterly_financial_data.bin",
        "sox_compliant_output.jsonl",
        &processing_options,
        &sox_compliance
    ).await?;

    // Generate SOX compliance report
    let sox_report = sox_compliance
        .generate_compliance_report(&processing_result)
        .await?;

    // Executive certification requirement
    let executive_certification = sox_compliance
        .prepare_executive_certification(&sox_report)
        .await?;

    println!("SOX-compliant financial processing completed");
    println!("Compliance report: {:?}", sox_report.summary);
    println!("Executive certification required: {:?}", executive_certification);

    Ok(())
}
```

### HIPAA-Compliant Healthcare Data Processing

```rust
use copybook_core::audit::*;

async fn hipaa_compliant_healthcare_processing() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize HIPAA compliance engine
    let hipaa_compliance = HipaaCompliance::new()
        .with_phi_classification(PhiClassification::ProtectedHealthInformation)
        .with_minimum_necessary_validation(MinimumNecessary::Strict)
        .with_safeguards(&[
            HipaaSafeguard::Administrative,
            HipaaSafeguard::Physical,
            HipaaSafeguard::Technical,
        ])
        .with_breach_notification(BreachNotification::Automatic)
        .with_retention_period_years(6);

    // Create HIPAA-compliant audit context
    let audit_context = AuditContext::new()
        .with_operation_id("patient_records_processing_20241225")
        .with_user("healthcare_data_processor")
        .with_compliance_profiles(&[ComplianceProfile::HIPAA])
        .with_security_classification(SecurityClassification::PHI)
        .with_minimum_necessary_justification("Quality assurance for patient care outcomes");

    // Validate HIPAA compliance
    let hipaa_validation = hipaa_compliance
        .validate_phi_processing(&audit_context)
        .await?;

    if !hipaa_validation.is_compliant() {
        return Err("HIPAA compliance validation failed".into());
    }

    // Process PHI with HIPAA safeguards
    let healthcare_options = ProcessingOptions::new()
        .with_encryption_at_rest(EncryptionStandard::AES256)
        .with_encryption_in_transit(EncryptionStandard::TLS12)
        .with_access_logging(AccessLogging::Comprehensive)
        .with_integrity_monitoring(IntegrityMonitoring::Continuous)
        .with_automatic_logoff(Duration::minutes(15));

    let healthcare_schema = parse_copybook_with_hipaa_validation(
        &patient_records_copybook,
        &ParseOptions::hipaa_compliant(),
        audit_context.clone()
    ).await?;

    let processing_result = process_phi_data_with_hipaa_compliance(
        &healthcare_schema,
        "patient_records.bin",
        "hipaa_compliant_output.jsonl",
        &healthcare_options,
        &hipaa_compliance
    ).await?;

    // Monitor for potential breaches
    let breach_monitoring = hipaa_compliance
        .monitor_for_breaches(&processing_result)
        .await?;

    if breach_monitoring.potential_breach_detected() {
        let breach_report = hipaa_compliance
            .generate_breach_report(&breach_monitoring)
            .await?;

        eprintln!("Potential HIPAA breach detected: {:?}", breach_report);

        // Automatic breach notification as required
        hipaa_compliance
            .initiate_breach_notification_process(&breach_report)
            .await?;
    }

    // Generate HIPAA compliance documentation
    let hipaa_documentation = hipaa_compliance
        .generate_compliance_documentation(&processing_result)
        .await?;

    println!("HIPAA-compliant healthcare processing completed");
    println!("PHI records processed: {}", processing_result.records_processed);
    println!("Compliance status: {:?}", hipaa_documentation.compliance_status);

    Ok(())
}
```

### GDPR-Compliant Personal Data Processing

```rust
use copybook_core::audit::*;

async fn gdpr_compliant_personal_data_processing() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize GDPR compliance engine
    let gdpr_compliance = GdprCompliance::new()
        .with_legal_basis(LegalBasis::LegitimateInterest)
        .with_processing_purpose(ProcessingPurpose::CustomerAnalytics)
        .with_data_subject_rights_enabled(true)
        .with_cross_border_transfer_monitoring(true)
        .with_controller_details(ControllerDetails {
            name: "ACME Corporation".to_string(),
            contact: "dpo@acme.com".to_string(),
            representative: Some("EU Representative Ltd.".to_string()),
        });

    // Create GDPR processing record
    let processing_record = GdprProcessingRecord::new()
        .with_controller("ACME Corporation")
        .with_processor("Data Processing Service Ltd.")
        .with_processing_purposes(&[ProcessingPurpose::CustomerAnalytics])
        .with_legal_basis(LegalBasis::LegitimateInterest)
        .with_data_categories(&[
            DataCategory::PersonalIdentifiers,
            DataCategory::ContactInformation,
            DataCategory::TransactionHistory,
        ])
        .with_recipients(&[
            Recipient::InternalAnalytics,
            Recipient::ExternalAuditor,
        ])
        .with_retention_period(RetentionPeriod::Years(5))
        .with_international_transfers(vec![
            InternationalTransfer {
                recipient_country: "United States".to_string(),
                adequacy_decision: AdequacyDecision::None,
                safeguards: TransferSafeguards::StandardContractualClauses,
            }
        ]);

    // Create audit context with GDPR requirements
    let audit_context = AuditContext::new()
        .with_operation_id("customer_analytics_processing_20241225")
        .with_user("analytics_service")
        .with_compliance_profiles(&[ComplianceProfile::GDPR])
        .with_security_classification(SecurityClassification::PersonalData)
        .with_gdpr_processing_record(processing_record);

    // Validate GDPR compliance
    let gdpr_validation = gdpr_compliance
        .validate_processing_activity(&audit_context)
        .await?;

    if !gdpr_validation.is_compliant() {
        return Err("GDPR compliance validation failed".into());
    }

    // Process personal data with GDPR safeguards
    let gdpr_options = ProcessingOptions::new()
        .with_data_minimization(DataMinimization::Strict)
        .with_purpose_limitation(PurposeLimitation::Enforced)
        .with_accuracy_validation(AccuracyValidation::Automatic)
        .with_storage_limitation(StorageLimitation::EnforceRetentionPeriod)
        .with_integrity_confidentiality(IntegrityConfidentiality::Advanced);

    let customer_schema = parse_copybook_with_gdpr_validation(
        &customer_data_copybook,
        &ParseOptions::gdpr_compliant(),
        audit_context.clone()
    ).await?;

    let processing_result = process_personal_data_with_gdpr_compliance(
        &customer_schema,
        "customer_personal_data.bin",
        "gdpr_compliant_analytics.jsonl",
        &gdpr_options,
        &gdpr_compliance
    ).await?;

    // Handle data subject rights requests
    let data_subject_rights_handler = DataSubjectRightsHandler::new()
        .with_gdpr_compliance(&gdpr_compliance);

    // Example: Process a data subject access request
    if let Some(access_request) = pending_access_requests().await? {
        let access_response = data_subject_rights_handler
            .handle_access_request(&access_request, &processing_result)
            .await?;

        println!("Data subject access request processed: {:?}", access_response);
    }

    // Generate GDPR processing activity documentation
    let gdpr_documentation = gdpr_compliance
        .generate_processing_activity_documentation(&processing_result)
        .await?;

    // Monitor cross-border transfers
    let transfer_monitoring = gdpr_compliance
        .monitor_international_transfers(&processing_result)
        .await?;

    println!("GDPR-compliant personal data processing completed");
    println!("Personal data records processed: {}", processing_result.records_processed);
    println!("Cross-border transfers: {:?}", transfer_monitoring.transfers_summary);

    Ok(())
}
```

## Best Practices for Compliance

### 1. Configuration Management

```yaml
# Use environment-specific configurations
# production-compliance.yaml
audit:
  enabled: true
  environment: production

  compliance:
    # Enable multiple frameworks as required
    sox: { enabled: true, validation_level: strict }
    hipaa: { enabled: true, validation_level: strict }
    gdpr: { enabled: true, validation_level: strict }
    pci_dss: { enabled: false }  # Enable as needed

  # Comprehensive logging
  logging:
    level: comprehensive
    structured_format: true
    sensitive_data_masking: true

  # Retention policies aligned with regulatory requirements
  retention:
    default_days: 2555  # 7 years (longest requirement)
    compliance_specific:
      sox: 2555       # 7 years
      hipaa: 2190     # 6 years
      gdpr: 1825      # 5 years (or data subject consent period)
```

### 2. Regular Compliance Validation

```bash
# Daily compliance validation
copybook audit validate \
  --compliance sox,hipaa,gdpr \
  --report-format detailed \
  --output compliance-report-$(date +%Y%m%d).json

# Weekly comprehensive audit
copybook audit report \
  --comprehensive \
  --compliance-summary \
  --performance-analysis \
  --security-assessment \
  --output weekly-audit-$(date +%Y%m%d).json
```

### 3. Automated Compliance Monitoring

```rust
use copybook_core::audit::*;

// Set up automated compliance monitoring
async fn setup_compliance_monitoring() -> Result<(), Box<dyn std::error::Error>> {
    let compliance_monitor = ComplianceMonitor::new()
        .with_frameworks(&[
            ComplianceFramework::SOX,
            ComplianceFramework::HIPAA,
            ComplianceFramework::GDPR,
        ])
        .with_monitoring_interval(Duration::hours(1))
        .with_alert_thresholds(&[
            AlertThreshold::new("compliance_violation", 0),
            AlertThreshold::new("security_control_failure", 0),
            AlertThreshold::new("audit_trail_integrity_failure", 0),
        ])
        .with_notification_channels(&[
            NotificationChannel::Email("compliance@company.com".to_string()),
            NotificationChannel::Slack("#compliance-alerts".to_string()),
            NotificationChannel::PagerDuty("compliance-team".to_string()),
        ]);

    // Start continuous monitoring
    compliance_monitor.start_monitoring().await?;

    Ok(())
}
```

## Compliance Reporting

### Automated Report Generation

```rust
use copybook_core::audit::*;

async fn generate_compliance_reports() -> Result<(), Box<dyn std::error::Error>> {
    let report_generator = ComplianceReportGenerator::new();

    // Generate SOX quarterly report
    let sox_report = report_generator
        .generate_sox_quarterly_report(Quarter::Q4, Year::Y2024)
        .await?;

    // Generate HIPAA annual security assessment
    let hipaa_report = report_generator
        .generate_hipaa_security_assessment(Year::Y2024)
        .await?;

    // Generate GDPR processing activity report
    let gdpr_report = report_generator
        .generate_gdpr_processing_activity_report(Year::Y2024)
        .await?;

    // Export reports to required formats
    report_generator
        .export_report(&sox_report, ReportFormat::PDF, "sox-q4-2024.pdf")
        .await?;

    report_generator
        .export_report(&hipaa_report, ReportFormat::JSON, "hipaa-assessment-2024.json")
        .await?;

    report_generator
        .export_report(&gdpr_report, ReportFormat::XML, "gdpr-activity-2024.xml")
        .await?;

    Ok(())
}
```

## Troubleshooting Compliance Issues

### Common Compliance Violations

1. **Data Retention Policy Violations**
   ```
   Error: AUDIT_RETENTION_VIOLATION
   Description: Data retained beyond required retention period
   Solution: Review and update retention policies, implement automated cleanup
   ```

2. **Access Control Violations**
   ```
   Error: ACCESS_CONTROL_VIOLATION
   Description: Unauthorized access to regulated data
   Solution: Review access permissions, implement stricter controls
   ```

3. **Audit Trail Integrity Failures**
   ```
   Error: AUDIT_TRAIL_INTEGRITY_FAILURE
   Description: Audit trail tampering detected
   Solution: Investigate security breach, restore from backup, review security controls
   ```

### Compliance Validation Commands

```bash
# Validate SOX compliance
copybook audit validate --compliance sox --strict

# Check HIPAA safeguards implementation
copybook audit security --framework hipaa --comprehensive

# Verify GDPR data subject rights capabilities
copybook audit rights --framework gdpr --test-all-rights

# Comprehensive compliance health check
copybook audit health-check --all-frameworks --detailed-report
```

This comprehensive compliance guide ensures copybook-rs meets enterprise regulatory requirements while maintaining high performance and usability standards.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
