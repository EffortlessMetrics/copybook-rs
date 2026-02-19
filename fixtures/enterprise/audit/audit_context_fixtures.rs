// SPDX-License-Identifier: AGPL-3.0-or-later
//! Enterprise Audit Context Test Fixtures
//!
//! Provides realistic audit context test data for all compliance frameworks
//! including proper security configurations, metadata, and compliance requirements.

use copybook_core::audit::{
    AuditContext,
    context::{
        SecurityClassification, EnvironmentContext, ProcessingConfig, SecurityContext,
        EncryptionStandard, AccessControlRequirements, AuditRequirements,
        EncryptionRequirements, NetworkSecurityContext, KeyManagementRequirements,
        ProcessingMode
    },
    ComplianceProfile
};
use std::collections::HashMap;

/// Create audit context fixture for HIPAA compliance testing
/// Includes proper PHI security configurations to satisfy validation requirements
pub fn create_hipaa_compliant_context() -> AuditContext {
    AuditContext::new()
        .with_operation_id("hipaa_test_operation")
        .with_user("healthcare_provider_user")
        .with_security_classification(SecurityClassification::PHI)
        .with_compliance_profile(ComplianceProfile::HIPAA)
        .with_metadata("minimum_necessary_justification", "Patient care quality assurance")
        .with_metadata("phi_category", "medical_history")
        .with_metadata("data_classification", "phi")
        .with_metadata("access_purpose", "treatment")
        .with_metadata("covered_entity", "healthcare_provider")
}

/// Create audit context fixture for SOX compliance testing
/// Includes material transaction controls and financial data protections
pub fn create_sox_compliant_context() -> AuditContext {
    let mut metadata = HashMap::new();
    metadata.insert("business_unit".to_string(), "financial_services".to_string());
    metadata.insert("data_classification".to_string(), "confidential".to_string());
    metadata.insert("transaction_materiality".to_string(), "material".to_string());
    metadata.insert("financial_period".to_string(), "2024-Q3".to_string());
    metadata.insert("sox_section".to_string(), "404".to_string());

    AuditContext {
        operation_id: "sox_financial_processing".to_string(),
        user: Some("financial_analyst".to_string()),
        environment: EnvironmentContext {
            deployment_stage: "production".to_string(),
            region: "us-east-1".to_string(),
            compliance_zone: Some("sox_controlled_zone".to_string()),
        },
        processing: ProcessingConfig {
            batch_size: Some(10000),
            parallel_processing: true,
            memory_limit_mb: Some(2048),
            timeout_seconds: Some(1800),
        },
        security: SecurityContext {
            classification: SecurityClassification::MaterialTransaction,
            encryption: copybook_core::audit::context::EncryptionConfig {
                at_rest: EncryptionStandard::AES256,
                in_transit: EncryptionStandard::AES256,
                key_management: "enterprise_kms".to_string(),
            },
            access_control: AccessControlConfig {
                role_based_access: true,
                authorization_required: true,
                multi_factor_auth: true,
                access_logging: true,
            },
            audit_requirements: AuditRequirements {
                comprehensive_logging: true,
                retention_years: 7, // SOX requires 7-year retention
                immutable_trail: true,
                real_time_monitoring: true,
            },
        },
        compliance_profiles: vec![ComplianceProfile::SOX],
        metadata,
        created_at: chrono::Utc::now().to_rfc3339(),
    }
}

/// Create audit context fixture for GDPR compliance testing
/// Includes personal data processing controls and legal basis requirements
pub fn create_gdpr_compliant_context() -> AuditContext {
    let mut metadata = HashMap::new();
    metadata.insert("gdpr_legal_basis".to_string(), "legitimate_interest".to_string());
    metadata.insert("data_subject_category".to_string(), "customers".to_string());
    metadata.insert("processing_purpose".to_string(), "service_delivery".to_string());
    metadata.insert("data_controller".to_string(), "primary".to_string());
    metadata.insert("cross_border_transfer".to_string(), "eu_to_uk".to_string());

    AuditContext {
        operation_id: "gdpr_data_processing".to_string(),
        user: Some("data_processor".to_string()),
        environment: EnvironmentContext {
            deployment_stage: "production".to_string(),
            region: "eu-west-1".to_string(),
            compliance_zone: Some("gdpr_compliant_zone".to_string()),
        },
        processing: ProcessingConfig {
            batch_size: Some(5000),
            parallel_processing: true,
            memory_limit_mb: Some(1024),
            timeout_seconds: Some(900),
        },
        security: SecurityContext {
            classification: SecurityClassification::PersonalData,
            encryption: copybook_core::audit::context::EncryptionConfig {
                at_rest: EncryptionStandard::AES256,
                in_transit: EncryptionStandard::AES256,
                key_management: "gdpr_compliant_kms".to_string(),
            },
            access_control: AccessControlConfig {
                role_based_access: true,
                authorization_required: true,
                multi_factor_auth: false, // Not required for all GDPR processing
                access_logging: true,
            },
            audit_requirements: AuditRequirements {
                comprehensive_logging: true,
                retention_years: 3, // Varies by jurisdiction
                immutable_trail: true,
                real_time_monitoring: false,
            },
        },
        compliance_profiles: vec![ComplianceProfile::GDPR],
        metadata,
        created_at: chrono::Utc::now().to_rfc3339(),
    }
}

/// Create audit context fixture for PCI DSS compliance testing
/// Includes cardholder data protections and security controls
pub fn create_pci_dss_compliant_context() -> AuditContext {
    let mut metadata = HashMap::new();
    metadata.insert("cardholder_data_present".to_string(), "true".to_string());
    metadata.insert("pci_dss_level".to_string(), "level_1".to_string());
    metadata.insert("merchant_category".to_string(), "retail".to_string());
    metadata.insert("payment_processor".to_string(), "certified_provider".to_string());

    AuditContext {
        operation_id: "pci_payment_processing".to_string(),
        user: Some("payment_processor".to_string()),
        environment: EnvironmentContext {
            deployment_stage: "production".to_string(),
            region: "us-central-1".to_string(),
            compliance_zone: Some("pci_dss_compliant_zone".to_string()),
        },
        processing: ProcessingConfig {
            batch_size: Some(2000),
            parallel_processing: true,
            memory_limit_mb: Some(1024),
            timeout_seconds: Some(600),
        },
        security: SecurityContext {
            classification: SecurityClassification::CardholderData,
            encryption: copybook_core::audit::context::EncryptionConfig {
                at_rest: EncryptionStandard::AES256,
                in_transit: EncryptionStandard::AES256,
                key_management: "pci_compliant_hsm".to_string(),
            },
            access_control: AccessControlConfig {
                role_based_access: true,
                authorization_required: true,
                multi_factor_auth: true,
                access_logging: true,
            },
            audit_requirements: AuditRequirements {
                comprehensive_logging: true,
                retention_years: 1, // PCI DSS minimum 1 year
                immutable_trail: true,
                real_time_monitoring: true,
            },
        },
        compliance_profiles: vec![ComplianceProfile::PciDss],
        metadata,
        created_at: chrono::Utc::now().to_rfc3339(),
    }
}

/// Create multi-framework compliance context for testing complex scenarios
/// Tests interaction between multiple compliance requirements
pub fn create_multi_compliance_context() -> AuditContext {
    let mut metadata = HashMap::new();
    metadata.insert("business_unit".to_string(), "healthcare_finance".to_string());
    metadata.insert("data_classification".to_string(), "highly_sensitive".to_string());
    metadata.insert("sox_section".to_string(), "404".to_string());
    metadata.insert("minimum_necessary_justification".to_string(), "Financial audit of healthcare transactions".to_string());
    metadata.insert("phi_category".to_string(), "billing_information".to_string());
    metadata.insert("gdpr_legal_basis".to_string(), "contract".to_string());

    AuditContext {
        operation_id: "multi_compliance_operation".to_string(),
        user: Some("compliance_auditor".to_string()),
        environment: EnvironmentContext {
            deployment_stage: "production".to_string(),
            region: "us-east-1".to_string(),
            compliance_zone: Some("multi_framework_compliant_zone".to_string()),
        },
        processing: ProcessingConfig {
            batch_size: Some(1000),
            parallel_processing: false, // Conservative for multi-compliance
            memory_limit_mb: Some(512),
            timeout_seconds: Some(1200),
        },
        security: SecurityContext {
            classification: SecurityClassification::PHI, // Highest sensitivity
            encryption: copybook_core::audit::context::EncryptionConfig {
                at_rest: EncryptionStandard::AES256,
                in_transit: EncryptionStandard::AES256,
                key_management: "fips_140_2_level_3_hsm".to_string(),
            },
            access_control: AccessControlConfig {
                role_based_access: true,
                authorization_required: true,
                multi_factor_auth: true,
                access_logging: true,
            },
            audit_requirements: AuditRequirements {
                comprehensive_logging: true,
                retention_years: 7, // Max of SOX (7) and HIPAA (6)
                immutable_trail: true,
                real_time_monitoring: true,
            },
        },
        compliance_profiles: vec![
            ComplianceProfile::SOX,
            ComplianceProfile::HIPAA,
            ComplianceProfile::GDPR
        ],
        metadata,
        created_at: chrono::Utc::now().to_rfc3339(),
    }
}

/// Create non-compliant context for testing violation detection
/// Missing security controls to trigger compliance violations
pub fn create_non_compliant_context() -> AuditContext {
    let mut metadata = HashMap::new();
    metadata.insert("phi_category".to_string(), "medical_history".to_string());
    // Missing minimum_necessary_justification for HIPAA

    AuditContext {
        operation_id: "non_compliant_test".to_string(),
        user: Some("unauthorized_user".to_string()),
        environment: EnvironmentContext {
            deployment_stage: "development".to_string(),
            region: "us-west-1".to_string(),
            compliance_zone: None, // No compliance zone
        },
        processing: ProcessingConfig {
            batch_size: Some(100),
            parallel_processing: true,
            memory_limit_mb: None,
            timeout_seconds: None,
        },
        security: SecurityContext {
            classification: SecurityClassification::PHI,
            encryption: copybook_core::audit::context::EncryptionConfig {
                at_rest: EncryptionStandard::None, // No encryption
                in_transit: EncryptionStandard::None,
                key_management: "none".to_string(),
            },
            access_control: AccessControlConfig {
                role_based_access: false, // No RBAC
                authorization_required: false, // No auth
                multi_factor_auth: false,
                access_logging: false, // No logging
            },
            audit_requirements: AuditRequirements {
                comprehensive_logging: false,
                retention_years: 0,
                immutable_trail: false,
                real_time_monitoring: false,
            },
        },
        compliance_profiles: vec![ComplianceProfile::HIPAA], // Claims HIPAA but not compliant
        metadata,
        created_at: chrono::Utc::now().to_rfc3339(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hipaa_context_fixture() {
        let context = create_hipaa_compliant_context();
        assert_eq!(context.security.classification, SecurityClassification::PHI);
        assert!(context.metadata.contains_key("minimum_necessary_justification"));
        assert!(context.compliance_profiles.contains(&ComplianceProfile::HIPAA));
    }

    #[test]
    fn test_sox_context_fixture() {
        let context = create_sox_compliant_context();
        assert_eq!(context.security.classification, SecurityClassification::MaterialTransaction);
        assert_eq!(context.security.audit_requirements.retention_years, 7);
        assert!(context.compliance_profiles.contains(&ComplianceProfile::SOX));
    }

    #[test]
    fn test_multi_compliance_context() {
        let context = create_multi_compliance_context();
        assert_eq!(context.compliance_profiles.len(), 3);
        assert!(context.compliance_profiles.contains(&ComplianceProfile::SOX));
        assert!(context.compliance_profiles.contains(&ComplianceProfile::HIPAA));
        assert!(context.compliance_profiles.contains(&ComplianceProfile::GDPR));
    }

    #[test]
    fn test_non_compliant_context() {
        let context = create_non_compliant_context();
        assert_eq!(context.security.encryption.at_rest, EncryptionStandard::None);
        assert!(!context.security.access_control.role_based_access);
        assert!(!context.metadata.contains_key("minimum_necessary_justification"));
    }
}