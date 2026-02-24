// SPDX-License-Identifier: AGPL-3.0-or-later
//! Enterprise Compliance Engine
//!
//! Provides comprehensive compliance validation for regulatory frameworks
//! including SOX, HIPAA, GDPR, and PCI DSS with automated validation
//! and remediation guidance.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::context::SecurityClassification;
use super::{AuditContext, AuditResult};
use crate::Field;

/// Enterprise compliance engine for regulatory validation
pub struct ComplianceEngine {
    profiles: Vec<ComplianceProfile>,
    validators: HashMap<ComplianceProfile, Box<dyn ComplianceValidator>>,
    config: ComplianceConfig,
}

impl ComplianceEngine {
    /// Create a new compliance engine with a given configuration
    pub fn new(config: ComplianceConfig) -> Self {
        Self {
            profiles: Vec::new(),
            validators: HashMap::new(),
            config,
        }
    }

    /// Add compliance profiles to validate against
    #[must_use]
    pub fn with_profiles(mut self, profiles: &[ComplianceProfile]) -> Self {
        self.profiles = profiles.to_vec();

        // Initialize validators for each profile with its specific config
        for profile in profiles {
            let validator: Box<dyn ComplianceValidator> = match profile {
                ComplianceProfile::SOX => Box::new(SoxValidator::new(self.config.sox.clone())),
                ComplianceProfile::HIPAA => {
                    Box::new(HipaaValidator::new(self.config.hipaa.clone()))
                }
                ComplianceProfile::GDPR => Box::new(GdprValidator::new(self.config.gdpr.clone())),
                ComplianceProfile::PciDss => {
                    Box::new(PciDssValidator::new(self.config.pci_dss.clone()))
                }
            };
            self.validators.insert(*profile, validator);
        }

        self
    }

    /// Validate processing operation against all configured compliance profiles
    pub async fn validate_processing_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceResult> {
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        for profile in &self.profiles {
            if let Some(validator) = self.validators.get(profile) {
                match validator.validate_operation(context).await {
                    Ok(validation_result) => {
                        violations.extend(validation_result.violations);
                        warnings.extend(validation_result.warnings);
                    }
                    Err(e) => {
                        // Add a critical violation when a validator fails completely
                        violations.push(ComplianceViolation {
                            violation_id: format!("{:?}-VALIDATOR-FAILURE", profile),
                            regulation: format!("{:?} Compliance Framework", profile),
                            severity: ComplianceSeverity::Critical,
                            title: "Compliance Validator System Failure".to_string(),
                            description: format!(
                                "Critical failure in {:?} compliance validator: {}. \n\
                                Compliance status cannot be determined for this framework.",
                                profile, e
                            ),
                            remediation: format!(
                                "Investigate and resolve {:?} validator system issues. \n\
                                Review audit logs and system health. \n\
                                Consider manual compliance review until validator is restored.",
                                profile
                            ),
                            reference_url: None,
                        });

                        // Add warning about degraded compliance monitoring
                        warnings.push(ComplianceWarning {
                            warning_id: format!("{:?}-DEGRADED-MONITORING", profile),
                            title: "Degraded Compliance Monitoring".to_string(),
                            description: format!(
                                "Compliance monitoring for {:?} framework is degraded due to validator failure",
                                profile
                            ),
                            recommendation: "Restore validator functionality to ensure complete compliance coverage".to_string(),
                        });
                    }
                }
            }
        }

        let compliance_status = if violations.is_empty() {
            if warnings.is_empty() {
                ComplianceStatus::Compliant
            } else {
                ComplianceStatus::CompliantWithWarnings
            }
        } else {
            ComplianceStatus::NonCompliant
        };

        Ok(ComplianceResult {
            status: compliance_status,
            violations,
            warnings,
            validated_profiles: self.profiles.clone(),
            validation_timestamp: chrono::Utc::now().to_rfc3339(),
        })
    }

    /// Generate compliance report for audit purposes
    pub async fn generate_compliance_report(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceReport> {
        let validation_result = self.validate_processing_operation(context).await?;

        // Generate recommendations with fallback handling
        let recommendations = match self.generate_recommendations(context).await {
            Ok(recs) => recs,
            Err(e) => {
                // Provide fallback recommendation when recommendation generation fails
                vec![ComplianceRecommendation {
                    recommendation_id: "SYSTEM-FALLBACK-REC".to_string(),
                    priority: RecommendationPriority::Critical,
                    title: "Restore Compliance Recommendation System".to_string(),
                    description: format!(
                        "The compliance recommendation system failed: {}. \n\
                        Manual compliance analysis is required.",
                        e
                    ),
                    implementation_effort: "Immediate".to_string(),
                    compliance_benefit:
                        "Restores automated compliance guidance and recommendations".to_string(),
                }]
            }
        };

        Ok(ComplianceReport {
            report_id: super::generate_audit_id(),
            operation_id: context.operation_id.clone(),
            compliance_result: validation_result,
            recommendations,
            next_review_date: self.calculate_next_review_date(),
            created_at: chrono::Utc::now().to_rfc3339(),
        })
    }

    async fn generate_recommendations(
        &self,
        context: &AuditContext,
    ) -> AuditResult<Vec<ComplianceRecommendation>> {
        let mut recommendations = Vec::new();

        for profile in &self.profiles {
            if let Some(validator) = self.validators.get(profile) {
                match validator.generate_recommendations(context).await {
                    Ok(profile_recommendations) => {
                        recommendations.extend(profile_recommendations);
                    }
                    Err(e) => {
                        // Add fallback recommendation when validator fails
                        recommendations.push(ComplianceRecommendation {
                            recommendation_id: format!("{:?}-FALLBACK-REC", profile),
                            priority: RecommendationPriority::Critical,
                            title: format!("Restore {:?} Compliance Monitoring", profile),
                            description: format!(
                                "The {:?} compliance validator failed to generate recommendations due to: {}. \n\
                                Manual compliance review is recommended until validator is restored.",
                                profile, e
                            ),
                            implementation_effort: "Immediate".to_string(),
                            compliance_benefit: format!(
                                "Restores automated {:?} compliance monitoring and recommendation generation",
                                profile
                            ),
                        });
                    }
                }
            }
        }

        Ok(recommendations)
    }

    fn calculate_next_review_date(&self) -> String {
        let review_interval = self.config.review_interval_days.unwrap_or(90);
        let next_review = chrono::Utc::now() + chrono::Duration::days(review_interval as i64);
        next_review.to_rfc3339()
    }
}

impl Default for ComplianceEngine {
    fn default() -> Self {
        Self::new(ComplianceConfig::default())
    }
}

/// Compliance profiles for different regulatory frameworks
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ComplianceProfile {
    /// Sarbanes-Oxley Act (Financial Services)
    SOX,
    /// Health Insurance Portability and Accountability Act
    HIPAA,
    /// General Data Protection Regulation
    GDPR,
    /// Payment Card Industry Data Security Standard
    PciDss,
}

/// Compliance validation trait for different regulatory frameworks
#[async_trait::async_trait]
pub trait ComplianceValidator: Send + Sync {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult>;
    async fn generate_recommendations(
        &self,
        context: &AuditContext,
    ) -> AuditResult<Vec<ComplianceRecommendation>>;
}

/// SOX compliance validator
#[derive(Default)]
pub struct SoxValidator {
    config: SoxConfig,
}

impl SoxValidator {
    pub fn new(config: SoxConfig) -> Self {
        Self { config }
    }

    fn validate_financial_data_controls(&self, context: &AuditContext) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // SOX Section 302: Financial data integrity controls
        if !self.has_adequate_data_integrity_controls(context) {
            violations.push(ComplianceViolation {
                violation_id: "SOX-302-001".to_string(),
                regulation: "SOX Section 302".to_string(),
                severity: ComplianceSeverity::High,
                title: "Inadequate Financial Data Integrity Controls".to_string(),
                description: "Processing lacks cryptographic validation required for financial data integrity".to_string(),
                remediation: "Implement SHA-256 integrity validation for all financial data processing operations".to_string(),
                reference_url: Some("https://www.sec.gov/rules/final/33-8238.htm".to_string()),
            });
        }

        // SOX Section 404: Internal controls assessment
        if !self.has_segregation_of_duties(context) {
            violations.push(ComplianceViolation {
                violation_id: "SOX-404-001".to_string(),
                regulation: "SOX Section 404".to_string(),
                severity: ComplianceSeverity::High,
                title: "Segregation of Duties Violation".to_string(),
                description: "Single user has both processing and validation permissions".to_string(),
                remediation: "Implement role-based access control with segregation of duties for financial data processing".to_string(),
                reference_url: Some("https://www.sec.gov/rules/final/33-8238.htm".to_string()),
            });
        }

        violations
    }

    fn has_adequate_data_integrity_controls(&self, context: &AuditContext) -> bool {
        // Check if cryptographic integrity is enabled for financial data
        context.security.audit_requirements.integrity_protection
            && matches!(
                context.security.classification,
                SecurityClassification::MaterialTransaction
            )
    }

    fn has_segregation_of_duties(&self, context: &AuditContext) -> bool {
        context.security.access_control.segregation_of_duties
    }
}

#[async_trait::async_trait]
impl ComplianceValidator for SoxValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        if self.config.financial_data_validation {
            violations.extend(self.validate_financial_data_controls(context));
        }

        // Check audit trail requirements
        if context.security.audit_requirements.retention_days < 2555 {
            warnings.push(ComplianceWarning {
                warning_id: "SOX-AUDIT-001".to_string(),
                title: "Audit Retention Below Recommended".to_string(),
                description:
                    "Audit retention period is less than 7 years recommended for SOX compliance"
                        .to_string(),
                recommendation: "Set audit retention to at least 2555 days (7 years)".to_string(),
            });
        }

        Ok(ComplianceValidationResult {
            violations,
            warnings,
        })
    }

    async fn generate_recommendations(
        &self,
        context: &AuditContext,
    ) -> AuditResult<Vec<ComplianceRecommendation>> {
        let mut recommendations = Vec::new();

        if self.config.executive_certification_required
            && matches!(
                context.security.classification,
                SecurityClassification::MaterialTransaction
            )
        {
            recommendations.push(ComplianceRecommendation {
                recommendation_id: "SOX-REC-001".to_string(),
                priority: RecommendationPriority::High,
                title: "Implement Executive Certification Process".to_string(),
                description:
                    "Establish automated executive certification for financial data processing"
                        .to_string(),
                implementation_effort: "2-3 weeks".to_string(),
                compliance_benefit:
                    "Ensures SOX Section 302 compliance for executive accountability".to_string(),
            });
        }

        if self.config.quarterly_reporting {
            recommendations.push(ComplianceRecommendation {
                recommendation_id: "SOX-REC-002".to_string(),
                priority: RecommendationPriority::Medium,
                title: "Quarterly Compliance Reporting".to_string(),
                description: "Implement automated quarterly compliance reporting for SOX audits"
                    .to_string(),
                implementation_effort: "1-2 weeks".to_string(),
                compliance_benefit: "Streamlines SOX audit process and reduces compliance burden"
                    .to_string(),
            });
        }

        Ok(recommendations)
    }
}

/// HIPAA compliance validator
#[derive(Default)]
pub struct HipaaValidator {
    config: HipaaConfig,
}

impl HipaaValidator {
    pub fn new(config: HipaaConfig) -> Self {
        Self { config }
    }

    fn validate_phi_protection(&self, context: &AuditContext) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // HIPAA Security Rule: Administrative Safeguards
        if !self.has_adequate_administrative_safeguards(context) {
            violations.push(ComplianceViolation {
                violation_id: "HIPAA-ADMIN-001".to_string(),
                regulation: "HIPAA Security Rule §164.308(a)".to_string(),
                severity: ComplianceSeverity::High,
                title: "Inadequate Administrative Safeguards".to_string(),
                description: "PHI processing lacks required administrative safeguards".to_string(),
                remediation:
                    "Implement role-based access control and user training for PHI handling"
                        .to_string(),
                reference_url: Some(
                    "https://www.hhs.gov/hipaa/for-professionals/security/index.html".to_string(),
                ),
            });
        }

        // HIPAA Security Rule: Technical Safeguards
        if self.config.phi_encryption_required && !self.has_adequate_technical_safeguards(context) {
            violations.push(ComplianceViolation {
                violation_id: "HIPAA-TECH-001".to_string(),
                regulation: "HIPAA Security Rule §164.312".to_string(),
                severity: ComplianceSeverity::Critical,
                title: "Inadequate Technical Safeguards".to_string(),
                description: "PHI processing lacks required encryption and access controls"
                    .to_string(),
                remediation: "Implement AES-256 encryption and comprehensive access logging"
                    .to_string(),
                reference_url: Some(
                    "https://www.hhs.gov/hipaa/for-professionals/security/index.html".to_string(),
                ),
            });
        }

        violations
    }

    fn has_adequate_administrative_safeguards(&self, context: &AuditContext) -> bool {
        context.security.access_control.role_based_access &&
        context.security.access_control.authorization_required &&
        context.security.access_control.multi_factor_authentication && // PHI requires MFA
        context.metadata.contains_key("phi_access_justification") // PHI access must be justified
    }

    fn has_adequate_technical_safeguards(&self, context: &AuditContext) -> bool {
        matches!(
            context.security.encryption.at_rest,
            super::context::EncryptionStandard::AES256
        ) && context.security.audit_requirements.comprehensive_logging
    }
}

#[async_trait::async_trait]
impl ComplianceValidator for HipaaValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        // Only validate if PHI is involved
        if matches!(context.security.classification, SecurityClassification::PHI) {
            violations.extend(self.validate_phi_protection(context));

            // Check minimum necessary requirement
            if self.config.minimum_necessary_enforcement
                && !context
                    .metadata
                    .contains_key("minimum_necessary_justification")
            {
                warnings.push(ComplianceWarning {
                    warning_id: "HIPAA-MIN-001".to_string(),
                    title: "Minimum Necessary Justification Missing".to_string(),
                    description: "PHI processing should include minimum necessary justification"
                        .to_string(),
                    recommendation: "Add minimum necessary justification to audit context metadata"
                        .to_string(),
                });
            }
        }

        Ok(ComplianceValidationResult {
            violations,
            warnings,
        })
    }

    async fn generate_recommendations(
        &self,
        context: &AuditContext,
    ) -> AuditResult<Vec<ComplianceRecommendation>> {
        let mut recommendations = Vec::new();

        if self.config.breach_notification_automation
            && matches!(context.security.classification, SecurityClassification::PHI)
        {
            recommendations.push(ComplianceRecommendation {
                recommendation_id: "HIPAA-REC-001".to_string(),
                priority: RecommendationPriority::High,
                title: "Implement Breach Detection Automation".to_string(),
                description:
                    "Automated monitoring for potential PHI breaches with notification workflows"
                        .to_string(),
                implementation_effort: "3-4 weeks".to_string(),
                compliance_benefit: "Ensures HIPAA Breach Notification Rule compliance".to_string(),
            });
        }

        Ok(recommendations)
    }
}

/// GDPR compliance validator
#[derive(Default)]
pub struct GdprValidator {
    config: GdprConfig,
}

impl GdprValidator {
    pub fn new(config: GdprConfig) -> Self {
        Self { config }
    }

    fn validate_data_protection_principles(
        &self,
        context: &AuditContext,
    ) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // GDPR Article 5: Principles of data processing
        if self.config.legal_basis_validation && !self.has_legal_basis_documentation(context) {
            violations.push(ComplianceViolation {
                violation_id: "GDPR-ART5-001".to_string(),
                regulation: "GDPR Article 5(1)(a)".to_string(),
                severity: ComplianceSeverity::High,
                title: "Legal Basis Not Documented".to_string(),
                description: "Personal data processing lacks documented legal basis".to_string(),
                remediation: "Document legal basis for processing in audit context metadata"
                    .to_string(),
                reference_url: Some(
                    "https://gdpr.eu/article-5-how-to-process-personal-data/".to_string(),
                ),
            });
        }

        // GDPR Article 25: Data protection by design
        if !self.has_data_minimization_controls(context) {
            violations.push(ComplianceViolation {
                violation_id: "GDPR-ART25-001".to_string(),
                regulation: "GDPR Article 25".to_string(),
                severity: ComplianceSeverity::Medium,
                title: "Data Minimization Controls Missing".to_string(),
                description: "Processing does not implement data minimization by design"
                    .to_string(),
                remediation:
                    "Implement data minimization controls to process only necessary personal data"
                        .to_string(),
                reference_url: Some(
                    "https://gdpr.eu/article-25-data-protection-by-design/".to_string(),
                ),
            });
        }

        violations
    }

    fn has_legal_basis_documentation(&self, context: &AuditContext) -> bool {
        context.metadata.contains_key("gdpr_legal_basis")
            || context.metadata.contains_key("processing_purpose")
    }

    fn has_data_minimization_controls(&self, context: &AuditContext) -> bool {
        // Check if processing configuration indicates data minimization
        context.processing_config.batch_size.is_some() // Simple check for controlled processing
    }
}

#[async_trait::async_trait]
impl ComplianceValidator for GdprValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        violations.extend(self.validate_data_protection_principles(context));

        // Check data subject rights support
        if !context.metadata.contains_key("data_subject_rights_enabled") {
            warnings.push(ComplianceWarning {
                warning_id: "GDPR-DSR-001".to_string(),
                title: "Data Subject Rights Support Not Indicated".to_string(),
                description:
                    "Processing should support data subject rights (access, rectification, erasure)"
                        .to_string(),
                recommendation: "Implement data subject rights handling capabilities".to_string(),
            });
        }

        Ok(ComplianceValidationResult {
            violations,
            warnings,
        })
    }

    async fn generate_recommendations(
        &self,
        _context: &AuditContext,
    ) -> AuditResult<Vec<ComplianceRecommendation>> {
        let mut recommendations = Vec::new();
        if self.config.data_subject_rights_automation {
            recommendations.push(ComplianceRecommendation {
                recommendation_id: "GDPR-REC-001".to_string(),
                priority: RecommendationPriority::High,
                title: "Implement Data Subject Rights Portal".to_string(),
                description:
                    "Automated portal for data subject access, rectification, and erasure requests"
                        .to_string(),
                implementation_effort: "4-6 weeks".to_string(),
                compliance_benefit:
                    "Ensures GDPR Articles 15-17 compliance for data subject rights".to_string(),
            });
        }
        Ok(recommendations)
    }
}

/// PCI DSS compliance validator
#[derive(Default)]
pub struct PciDssValidator {
    config: PciDssConfig,
}

impl PciDssValidator {
    pub fn new(config: PciDssConfig) -> Self {
        Self { config }
    }

    /// Validate PCI DSS Requirement 3: Protect stored cardholder data
    fn validate_cardholder_data_protection(
        &self,
        context: &AuditContext,
    ) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // PCI DSS Requirement 3.1: Keep cardholder data storage to a minimum
        if self.config.cardholder_data_validation
            && context.metadata.contains_key("stores_full_pan")
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-3.1-001".to_string(),
                regulation: "PCI DSS Requirement 3.1".to_string(),
                severity: ComplianceSeverity::Critical,
                title: "Full PAN Storage Detected".to_string(),
                description: "Full Primary Account Number (PAN) is being stored in violation of PCI DSS data minimization requirements".to_string(),
                remediation: "Implement PAN truncation (display only first 6 and last 4 digits) or use tokenization".to_string(),
                reference_url: Some("https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string()),
            });
        }

        // PCI DSS Requirement 3.2: Render PAN unreadable
        if self.config.cardholder_data_validation && !context.security.encryption.at_rest_encrypted
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-3.2-001".to_string(),
                regulation: "PCI DSS Requirement 3.2".to_string(),
                severity: ComplianceSeverity::Critical,
                title: "Cardholder Data Not Encrypted at Rest".to_string(),
                description: "Cardholder data is stored without encryption at rest".to_string(),
                remediation: "Implement strong cryptography (AES-256 or equivalent) for all cardholder data at rest".to_string(),
                reference_url: Some("https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string()),
            });
        }

        // PCI DSS Requirement 3.4: Render PAN unreadable when displayed
        if self.config.cardholder_data_validation
            && context.metadata.contains_key("displays_full_pan")
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-3.4-001".to_string(),
                regulation: "PCI DSS Requirement 3.4".to_string(),
                severity: ComplianceSeverity::High,
                title: "Full PAN Displayed in Logs/UI".to_string(),
                description: "Full PAN is displayed in logs or user interface in violation of PCI DSS masking requirements".to_string(),
                remediation: "Implement PAN masking to display only first 6 and last 4 digits (e.g., XXXXXX123456XXXX)".to_string(),
                reference_url: Some("https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string()),
            });
        }

        // PCI DSS Requirement 3.5: Protect cryptographic keys
        if self.config.cardholder_data_validation
            && context.security.encryption.at_rest_encrypted
            && !context.security.audit_requirements.key_management
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-3.5-001".to_string(),
                regulation: "PCI DSS Requirement 3.5".to_string(),
                severity: ComplianceSeverity::Critical,
                title: "Inadequate Key Management".to_string(),
                description: "Cryptographic keys are not properly managed according to PCI DSS requirements".to_string(),
                remediation: "Implement secure key generation, distribution, storage, rotation, and destruction processes".to_string(),
                reference_url: Some("https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string()),
            });
        }

        violations
    }

    /// Validate PCI DSS Requirement 4: Encrypt transmission of cardholder data
    fn validate_transmission_encryption(&self, context: &AuditContext) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // PCI DSS Requirement 4.1: Use strong cryptography
        if self.config.cardholder_data_validation
            && context.metadata.contains_key("transmits_cardholder_data")
            && !context.security.encryption.transit_encrypted
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-4.1-001".to_string(),
                regulation: "PCI DSS Requirement 4.1".to_string(),
                severity: ComplianceSeverity::Critical,
                title: "Unencrypted Cardholder Data Transmission".to_string(),
                description: "Cardholder data is transmitted over open networks without encryption".to_string(),
                remediation: "Implement TLS 1.2 or higher with strong cipher suites for all cardholder data transmission".to_string(),
                reference_url: Some("https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string()),
            });
        }

        violations
    }

    /// Validate PCI DSS Requirement 7: Restrict access to cardholder data
    fn validate_access_controls(&self, context: &AuditContext) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // PCI DSS Requirement 7.1: Limit access based on need-to-know
        if self.config.cardholder_data_validation
            && !context.security.access_control.role_based_access
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-7.1-001".to_string(),
                regulation: "PCI DSS Requirement 7.1".to_string(),
                severity: ComplianceSeverity::High,
                title: "Inadequate Access Control".to_string(),
                description:
                    "Access to cardholder data is not restricted based on business need-to-know"
                        .to_string(),
                remediation: "Implement role-based access control with least privilege principles"
                    .to_string(),
                reference_url: Some(
                    "https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string(),
                ),
            });
        }

        // PCI DSS Requirement 8.2: Identify and authenticate access
        if self.config.mfa_required && !context.security.access_control.multi_factor_authentication
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-8.2-001".to_string(),
                regulation: "PCI DSS Requirement 8.2".to_string(),
                severity: ComplianceSeverity::High,
                title: "Missing Multi-Factor Authentication".to_string(),
                description:
                    "Multi-factor authentication is not required for access to cardholder data"
                        .to_string(),
                remediation: "Implement MFA for all access to cardholder data environments"
                    .to_string(),
                reference_url: Some(
                    "https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string(),
                ),
            });
        }

        violations
    }

    /// Validate PCI DSS Requirement 10: Track and monitor all access
    fn validate_audit_logging(&self, context: &AuditContext) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // PCI DSS Requirement 10.1: Implement audit trails
        if self.config.cardholder_data_validation
            && !context.security.audit_requirements.comprehensive_logging
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-10.1-001".to_string(),
                regulation: "PCI DSS Requirement 10.1".to_string(),
                severity: ComplianceSeverity::High,
                title: "Inadequate Audit Trails".to_string(),
                description: "Comprehensive audit trails for all access to cardholder data are not implemented".to_string(),
                remediation: "Implement audit trails that record all access to cardholder data, including user identity, timestamp, and action".to_string(),
                reference_url: Some("https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string()),
            });
        }

        // PCI DSS Requirement 10.5.1: Secure audit trails
        if self.config.cardholder_data_validation
            && !context.security.audit_requirements.integrity_protection
        {
            violations.push(ComplianceViolation {
                violation_id: "PCI-10.5.1-001".to_string(),
                regulation: "PCI DSS Requirement 10.5.1".to_string(),
                severity: ComplianceSeverity::High,
                title: "Audit Trail Integrity Not Protected".to_string(),
                description: "Audit trails are not protected against tampering or unauthorized modification".to_string(),
                remediation: "Implement cryptographic integrity checks and immutable logging for audit trails".to_string(),
                reference_url: Some("https://www.pcisecuritystandards.org/documents/PCI_DSS_v4-0.pdf".to_string()),
            });
        }

        violations
    }
}

#[async_trait::async_trait]
impl ComplianceValidator for PciDssValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        // Only validate if cardholder data is involved
        if self.config.cardholder_data_validation
            && (context.metadata.contains_key("has_cardholder_data")
                || context.metadata.contains_key("stores_full_pan")
                || context.metadata.contains_key("transmits_cardholder_data"))
        {
            violations.extend(self.validate_cardholder_data_protection(context));
            violations.extend(self.validate_transmission_encryption(context));
            violations.extend(self.validate_access_controls(context));
            violations.extend(self.validate_audit_logging(context));

            // PCI DSS Requirement 12: Maintain information security policy
            if !context.metadata.contains_key("pci_security_policy") {
                warnings.push(ComplianceWarning {
                    warning_id: "PCI-12-001".to_string(),
                    title: "Security Policy Not Documented".to_string(),
                    description: "PCI DSS security policy documentation is not referenced".to_string(),
                    recommendation: "Ensure PCI DSS information security policy is documented and communicated to all personnel".to_string(),
                });
            }
        }

        Ok(ComplianceValidationResult {
            violations,
            warnings,
        })
    }

    async fn generate_recommendations(
        &self,
        context: &AuditContext,
    ) -> AuditResult<Vec<ComplianceRecommendation>> {
        let mut recommendations = Vec::new();

        if self.config.cardholder_data_validation
            && (context.metadata.contains_key("has_cardholder_data")
                || context.metadata.contains_key("stores_full_pan"))
        {
            recommendations.push(ComplianceRecommendation {
                recommendation_id: "PCI-REC-001".to_string(),
                priority: RecommendationPriority::Critical,
                title: "Implement Tokenization for Cardholder Data".to_string(),
                description: "Replace sensitive cardholder data with non-sensitive equivalents (tokens) to reduce PCI DSS scope".to_string(),
                implementation_effort: "4-6 weeks".to_string(),
                compliance_benefit: "Significantly reduces PCI DSS compliance scope and risk exposure".to_string(),
            });

            recommendations.push(ComplianceRecommendation {
                recommendation_id: "PCI-REC-002".to_string(),
                priority: RecommendationPriority::High,
                title: "Implement Continuous Compliance Monitoring".to_string(),
                description: "Deploy automated monitoring for PCI DSS compliance with real-time violation detection".to_string(),
                implementation_effort: "3-4 weeks".to_string(),
                compliance_benefit: "Ensures ongoing PCI DSS compliance and reduces audit preparation time".to_string(),
            });
        }

        Ok(recommendations)
    }
}

// Configuration structures

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceConfig {
    pub review_interval_days: Option<u32>,
    pub sox: SoxConfig,
    pub hipaa: HipaaConfig,
    pub gdpr: GdprConfig,
    pub pci_dss: PciDssConfig,
}

impl Default for ComplianceConfig {
    fn default() -> Self {
        Self {
            review_interval_days: Some(90),
            sox: SoxConfig::default(),
            hipaa: HipaaConfig::default(),
            gdpr: GdprConfig::default(),
            pci_dss: PciDssConfig::default(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoxConfig {
    pub financial_data_validation: bool,
    pub executive_certification_required: bool,
    pub quarterly_reporting: bool,
}

impl Default for SoxConfig {
    fn default() -> Self {
        Self {
            financial_data_validation: true,
            executive_certification_required: true,
            quarterly_reporting: true,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HipaaConfig {
    pub phi_encryption_required: bool,
    pub breach_notification_automation: bool,
    pub minimum_necessary_enforcement: bool,
}

impl Default for HipaaConfig {
    fn default() -> Self {
        Self {
            phi_encryption_required: true,
            breach_notification_automation: true,
            minimum_necessary_enforcement: true,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GdprConfig {
    pub legal_basis_validation: bool,
    pub data_subject_rights_automation: bool,
}

impl Default for GdprConfig {
    fn default() -> Self {
        Self {
            legal_basis_validation: true,
            data_subject_rights_automation: true,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PciDssConfig {
    pub cardholder_data_validation: bool,
    pub mfa_required: bool,
}

impl Default for PciDssConfig {
    fn default() -> Self {
        Self {
            cardholder_data_validation: true,
            mfa_required: true,
        }
    }
}

// Result structures

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceResult {
    pub status: ComplianceStatus,
    pub violations: Vec<ComplianceViolation>,
    pub warnings: Vec<ComplianceWarning>,
    pub validated_profiles: Vec<ComplianceProfile>,
    pub validation_timestamp: String,
}

impl ComplianceResult {
    pub fn is_compliant(&self) -> bool {
        matches!(
            self.status,
            ComplianceStatus::Compliant | ComplianceStatus::CompliantWithWarnings
        )
    }

    pub fn has_critical_violations(&self) -> bool {
        self.violations
            .iter()
            .any(|v| matches!(v.severity, ComplianceSeverity::Critical))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComplianceStatus {
    Compliant,
    CompliantWithWarnings,
    NonCompliant,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceViolation {
    pub violation_id: String,
    pub regulation: String,
    pub severity: ComplianceSeverity,
    pub title: String,
    pub description: String,
    pub remediation: String,
    pub reference_url: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComplianceSeverity {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceWarning {
    pub warning_id: String,
    pub title: String,
    pub description: String,
    pub recommendation: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceValidationResult {
    pub violations: Vec<ComplianceViolation>,
    pub warnings: Vec<ComplianceWarning>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceReport {
    pub report_id: String,
    pub operation_id: String,
    pub compliance_result: ComplianceResult,
    pub recommendations: Vec<ComplianceRecommendation>,
    pub next_review_date: String,
    pub created_at: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceRecommendation {
    pub recommendation_id: String,
    pub priority: RecommendationPriority,
    pub title: String,
    pub description: String,
    pub implementation_effort: String,
    pub compliance_benefit: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RecommendationPriority {
    Low,
    Medium,
    High,
    Critical,
}

/// Patterns per compliance profile for sensitive field detection
fn sensitive_patterns_for_profile(profile: ComplianceProfile) -> &'static [&'static str] {
    match profile {
        ComplianceProfile::SOX => &["ACCOUNT", "BALANCE", "TRANSACTION", "LEDGER", "REVENUE"],
        ComplianceProfile::HIPAA => &["SSN", "DOB", "PATIENT", "DIAGNOSIS", "INSURANCE", "MEDICAL"],
        ComplianceProfile::PciDss => &["CARD", "PAN", "CVV", "EXPIRY", "CVC"],
        ComplianceProfile::GDPR => &[
            "NAME",
            "ADDRESS",
            "EMAIL",
            "PHONE",
            "NATIONAL-ID",
            "PASSPORT",
        ],
    }
}

fn check_field_against_profiles(
    field: &Field,
    profiles: &[ComplianceProfile],
    warnings: &mut Vec<ComplianceWarning>,
) {
    let upper_name = field.name.to_ascii_uppercase();
    for &profile in profiles {
        for &pattern in sensitive_patterns_for_profile(profile) {
            if upper_name.contains(pattern) {
                warnings.push(ComplianceWarning {
                    warning_id: format!("{:?}-FIELD-{}", profile, field.name),
                    title: format!("Sensitive field detected ({profile:?})"),
                    description: format!(
                        "Field '{}' matches {profile:?} sensitive pattern '{pattern}'",
                        field.name,
                    ),
                    recommendation: format!(
                        "Review field '{}' to ensure {profile:?} compliance requirements are met",
                        field.name,
                    ),
                });
                // Only emit one warning per (field, profile) pair
                break;
            }
        }
    }
    // Recurse into children
    for child in &field.children {
        check_field_against_profiles(child, profiles, warnings);
    }
}

/// Detect fields in a schema that may contain regulated data under the given compliance profiles.
///
/// Recursively inspects all fields (including group children) and emits a [`ComplianceWarning`]
/// for each field name that matches a profile-specific sensitive-data pattern.
pub fn detect_sensitive_fields(
    fields: &[Field],
    profiles: &[ComplianceProfile],
) -> Vec<ComplianceWarning> {
    let mut warnings = Vec::new();
    for field in fields {
        check_field_against_profiles(field, profiles, &mut warnings);
    }
    warnings
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn test_compliance_engine_creation() {
        let engine = ComplianceEngine::default()
            .with_profiles(&[ComplianceProfile::SOX, ComplianceProfile::HIPAA]);

        assert_eq!(engine.profiles.len(), 2);
        assert!(engine.profiles.contains(&ComplianceProfile::SOX));
        assert!(engine.profiles.contains(&ComplianceProfile::HIPAA));
    }

    #[tokio::test]
    async fn test_sox_compliance_validation() {
        let context = AuditContext::new()
            .with_security_classification(SecurityClassification::MaterialTransaction);

        let sox_validator = SoxValidator::new(SoxConfig::default());
        let result = sox_validator
            .validate_operation(&context)
            .await
            .expect("SOX validation should not fail in test environment");

        // Should have violations due to missing controls in default context
        assert!(!result.violations.is_empty());
    }

    #[tokio::test]
    async fn test_hipaa_compliance_validation() {
        let context = AuditContext::new().with_security_classification(SecurityClassification::PHI);

        let hipaa_validator = HipaaValidator::new(HipaaConfig::default());
        let result = hipaa_validator
            .validate_operation(&context)
            .await
            .expect("HIPAA validation should not fail in test environment");

        // Should have violations due to missing PHI protections in default context
        assert!(!result.violations.is_empty());
    }

    #[tokio::test]
    async fn test_pci_dss_compliance_validation() {
        let mut context = AuditContext::new()
            .with_metadata("has_cardholder_data", "true")
            .with_metadata("stores_full_pan", "true");

        // Set up context with violations
        context.security.encryption.at_rest_encrypted = false;
        context.security.access_control.multi_factor_authentication = false;

        let pci_validator = PciDssValidator::new(PciDssConfig::default());
        let result = pci_validator
            .validate_operation(&context)
            .await
            .expect("PCI DSS validation should not fail in test environment");

        // Should have violations due to missing PCI DSS protections
        assert!(!result.violations.is_empty());

        // Check for specific violations
        let has_pan_storage_violation = result
            .violations
            .iter()
            .any(|v| v.violation_id == "PCI-3.1-001");
        assert!(
            has_pan_storage_violation,
            "Should detect full PAN storage violation"
        );

        let has_encryption_violation = result
            .violations
            .iter()
            .any(|v| v.violation_id == "PCI-3.2-001");
        assert!(
            has_encryption_violation,
            "Should detect encryption violation"
        );

        let has_mfa_violation = result
            .violations
            .iter()
            .any(|v| v.violation_id == "PCI-8.2-001");
        assert!(has_mfa_violation, "Should detect MFA violation");
    }

    #[tokio::test]
    async fn test_pci_dss_compliant_context() {
        let mut context = AuditContext::new()
            .with_metadata("has_cardholder_data", "true")
            .with_metadata("pci_security_policy", "documented");

        // Set up context with proper protections
        context.security.encryption.at_rest_encrypted = true;
        context.security.encryption.transit_encrypted = true;
        context.security.access_control.multi_factor_authentication = true;
        context.security.access_control.role_based_access = true;
        context.security.audit_requirements.comprehensive_logging = true;
        context.security.audit_requirements.integrity_protection = true;
        context.security.audit_requirements.key_management = true;

        let pci_validator = PciDssValidator::new(PciDssConfig::default());
        let result = pci_validator
            .validate_operation(&context)
            .await
            .expect("PCI DSS validation should not fail in test environment");

        // Should have no violations with proper protections
        assert!(
            result.violations.is_empty(),
            "Should be compliant with proper protections"
        );
    }

    #[tokio::test]
    async fn test_pci_dss_recommendations() {
        let context = AuditContext::new()
            .with_metadata("has_cardholder_data", "true")
            .with_metadata("stores_full_pan", "true");

        let pci_validator = PciDssValidator::new(PciDssConfig::default());
        let recommendations = pci_validator
            .generate_recommendations(&context)
            .await
            .expect("PCI DSS recommendations should not fail in test environment");

        // Should have recommendations for cardholder data
        assert!(!recommendations.is_empty());

        let has_tokenization_rec = recommendations
            .iter()
            .any(|r| r.recommendation_id == "PCI-REC-001");
        assert!(has_tokenization_rec, "Should recommend tokenization");

        let has_monitoring_rec = recommendations
            .iter()
            .any(|r| r.recommendation_id == "PCI-REC-002");
        assert!(has_monitoring_rec, "Should recommend continuous monitoring");
    }

    #[test]
    fn test_compliance_result_status() {
        let compliant_result = ComplianceResult {
            status: ComplianceStatus::Compliant,
            violations: Vec::new(),
            warnings: Vec::new(),
            validated_profiles: vec![ComplianceProfile::SOX],
            validation_timestamp: chrono::Utc::now().to_rfc3339(),
        };

        assert!(compliant_result.is_compliant());
        assert!(!compliant_result.has_critical_violations());

        let non_compliant_result = ComplianceResult {
            status: ComplianceStatus::NonCompliant,
            violations: vec![ComplianceViolation {
                violation_id: "TEST-001".to_string(),
                regulation: "Test Regulation".to_string(),
                severity: ComplianceSeverity::Critical,
                title: "Test Violation".to_string(),
                description: "Test Description".to_string(),
                remediation: "Test Remediation".to_string(),
                reference_url: None,
            }],
            warnings: Vec::new(),
            validated_profiles: vec![ComplianceProfile::SOX],
            validation_timestamp: chrono::Utc::now().to_rfc3339(),
        };

        assert!(!non_compliant_result.is_compliant());
        assert!(non_compliant_result.has_critical_violations());
    }

    #[test]
    fn test_calculate_next_review_date_arithmetic() {
        let engine = ComplianceEngine::default();

        // Test default 90-day calculation
        let review_date = engine.calculate_next_review_date();
        let current_time = chrono::Utc::now();

        // Parse the returned date to verify arithmetic
        let parsed_review_date = chrono::DateTime::parse_from_rfc3339(&review_date)
            .expect("Review date should be valid RFC3339 format in test");
        let actual_timestamp = parsed_review_date.timestamp();

        // Should be approximately 90 days (allow small variance for execution time)
        let days_diff = (actual_timestamp - current_time.timestamp()) / (24 * 3600);
        assert!(
            days_diff >= 89,
            "Review date should be at least 89 days from now"
        );
        assert!(
            days_diff <= 91,
            "Review date should be at most 91 days from now"
        );

        // Test custom review interval arithmetic
        let custom_config = ComplianceConfig {
            review_interval_days: Some(365), // 1 year
            ..Default::default()
        };
        let custom_engine = ComplianceEngine::new(custom_config);
        let custom_review_date = custom_engine.calculate_next_review_date();

        let parsed_custom_date = chrono::DateTime::parse_from_rfc3339(&custom_review_date)
            .expect("Custom review date should be valid RFC3339 format in test");
        let custom_days_diff =
            (parsed_custom_date.timestamp() - current_time.timestamp()) / (24 * 3600);
        assert!(
            custom_days_diff >= 364,
            "Custom review date should be at least 364 days from now"
        );
        assert!(
            custom_days_diff <= 366,
            "Custom review date should be at most 366 days from now"
        );
    }

    #[tokio::test]
    async fn test_sox_retention_calculation_logic() {
        // Test the specific arithmetic operation in line 237: retention_days < 2555
        // Create a context with retention exactly at threshold (2555)
        let mut context_compliant = AuditContext::new()
            .with_security_classification(SecurityClassification::MaterialTransaction);
        context_compliant.security.audit_requirements.retention_days = 2555; // Exactly the threshold

        let sox_validator = SoxValidator::new(SoxConfig::default());
        let result = sox_validator
            .validate_operation(&context_compliant)
            .await
            .expect("SOX validation should not fail in test with compliant context");

        // At exactly 2555 days, should not trigger warning (not < 2555)
        let retention_warnings: Vec<_> = result
            .warnings
            .iter()
            .filter(|w| w.warning_id == "SOX-AUDIT-001")
            .collect();
        assert!(
            retention_warnings.is_empty(),
            "Should not warn at exactly 2555 days"
        );

        // Test just below threshold
        let mut context_warning = AuditContext::new()
            .with_security_classification(SecurityClassification::MaterialTransaction);
        context_warning.security.audit_requirements.retention_days = 2554; // Just below threshold

        let result_warning = sox_validator
            .validate_operation(&context_warning)
            .await
            .expect("SOX validation should not fail in test with warning context");

        // Should trigger warning when < 2555
        let retention_warnings: Vec<_> = result_warning
            .warnings
            .iter()
            .filter(|w| w.warning_id == "SOX-AUDIT-001")
            .collect();
        assert!(
            !retention_warnings.is_empty(),
            "Should warn when retention < 2555 days"
        );

        // Test well above threshold
        let mut context_good = AuditContext::new()
            .with_security_classification(SecurityClassification::MaterialTransaction);
        context_good.security.audit_requirements.retention_days = 3000; // Well above threshold

        let result_good = sox_validator
            .validate_operation(&context_good)
            .await
            .expect("SOX validation should not fail in test with good context");

        // Should not trigger warning when > 2555
        let retention_warnings_good: Vec<_> = result_good
            .warnings
            .iter()
            .filter(|w| w.warning_id == "SOX-AUDIT-001")
            .collect();
        assert!(
            retention_warnings_good.is_empty(),
            "Should not warn when retention > 2555 days"
        );
    }

    #[test]
    fn test_compliance_status_determination_logic() {
        // Test the arithmetic/logic for determining ComplianceStatus

        // Case 1: Empty violations and warnings -> Compliant
        let mut violations = Vec::new();
        let mut warnings = Vec::new();

        let status = if violations.is_empty() {
            if warnings.is_empty() {
                ComplianceStatus::Compliant
            } else {
                ComplianceStatus::CompliantWithWarnings
            }
        } else {
            ComplianceStatus::NonCompliant
        };
        assert_eq!(status, ComplianceStatus::Compliant);

        // Case 2: No violations but has warnings -> CompliantWithWarnings
        warnings.push(ComplianceWarning {
            warning_id: "TEST-001".to_string(),
            title: "Test Warning".to_string(),
            description: "Test".to_string(),
            recommendation: "Test".to_string(),
        });

        let status = if violations.is_empty() {
            if warnings.is_empty() {
                ComplianceStatus::Compliant
            } else {
                ComplianceStatus::CompliantWithWarnings
            }
        } else {
            ComplianceStatus::NonCompliant
        };
        assert_eq!(status, ComplianceStatus::CompliantWithWarnings);

        // Case 3: Has violations -> NonCompliant
        violations.push(ComplianceViolation {
            violation_id: "TEST-001".to_string(),
            regulation: "Test".to_string(),
            severity: ComplianceSeverity::Medium,
            title: "Test".to_string(),
            description: "Test".to_string(),
            remediation: "Test".to_string(),
            reference_url: None,
        });

        let status = if violations.is_empty() {
            if warnings.is_empty() {
                ComplianceStatus::Compliant
            } else {
                ComplianceStatus::CompliantWithWarnings
            }
        } else {
            ComplianceStatus::NonCompliant
        };
        assert_eq!(status, ComplianceStatus::NonCompliant);
    }

    // Helper to create a minimal leaf Field for testing
    fn make_field(name: &str) -> crate::Field {
        crate::Field {
            path: name.to_string(),
            name: name.to_string(),
            level: 5,
            kind: crate::FieldKind::Alphanum { len: 10 },
            offset: 0,
            len: 10,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            resolved_renames: None,
            children: Vec::new(),
        }
    }

    fn make_group(name: &str, children: Vec<crate::Field>) -> crate::Field {
        crate::Field {
            path: name.to_string(),
            name: name.to_string(),
            level: 1,
            kind: crate::FieldKind::Group,
            offset: 0,
            len: 0,
            redefines_of: None,
            occurs: None,
            sync_padding: None,
            synchronized: false,
            blank_when_zero: false,
            resolved_renames: None,
            children,
        }
    }

    #[test]
    fn test_detect_sensitive_fields_empty() {
        let warnings = detect_sensitive_fields(&[], &[ComplianceProfile::SOX]);
        assert!(warnings.is_empty(), "No fields means no warnings");
    }

    #[test]
    fn test_detect_sensitive_fields_sox_match() {
        let fields = vec![make_field("ACCOUNT-NUMBER"), make_field("CUSTOMER-NAME")];
        let warnings = detect_sensitive_fields(&fields, &[ComplianceProfile::SOX]);
        assert_eq!(warnings.len(), 1, "Only ACCOUNT-NUMBER should match SOX");
        assert!(warnings[0].warning_id.contains("SOX"));
        assert!(warnings[0].description.contains("ACCOUNT-NUMBER"));
    }

    #[test]
    fn test_detect_sensitive_fields_hipaa_match() {
        let fields = vec![make_field("PATIENT-ID"), make_field("DIAGNOSIS-CODE")];
        let warnings = detect_sensitive_fields(&fields, &[ComplianceProfile::HIPAA]);
        assert_eq!(warnings.len(), 2, "Both fields should match HIPAA");
        let ids: Vec<_> = warnings.iter().map(|w| &w.warning_id).collect();
        assert!(ids.iter().any(|id| id.contains("PATIENT")));
        assert!(ids.iter().any(|id| id.contains("DIAGNOSIS")));
    }

    #[test]
    fn test_detect_sensitive_fields_pci_match() {
        let fields = vec![make_field("CARD-NUM"), make_field("CVV-VALUE")];
        let warnings = detect_sensitive_fields(&fields, &[ComplianceProfile::PciDss]);
        assert_eq!(warnings.len(), 2, "Both fields should match PCI DSS");
    }

    #[test]
    fn test_detect_sensitive_fields_gdpr_match() {
        let fields = vec![make_field("CUSTOMER-EMAIL"), make_field("HOME-ADDRESS")];
        let warnings = detect_sensitive_fields(&fields, &[ComplianceProfile::GDPR]);
        assert_eq!(warnings.len(), 2, "Both fields should match GDPR");
    }

    #[test]
    fn test_detect_sensitive_fields_no_match() {
        let fields = vec![make_field("RECORD-TYPE"), make_field("FILLER")];
        let warnings = detect_sensitive_fields(
            &fields,
            &[
                ComplianceProfile::SOX,
                ComplianceProfile::HIPAA,
                ComplianceProfile::PciDss,
                ComplianceProfile::GDPR,
            ],
        );
        assert!(
            warnings.is_empty(),
            "Generic fields should not match any profile"
        );
    }

    #[test]
    fn test_detect_sensitive_fields_recurses_into_children() {
        let child = make_field("SSN-FIELD");
        let group = make_group("CUSTOMER-RECORD", vec![child]);
        let warnings = detect_sensitive_fields(&[group], &[ComplianceProfile::HIPAA]);
        assert_eq!(warnings.len(), 1, "Should find SSN in nested child");
        assert!(warnings[0].description.contains("SSN-FIELD"));
    }

    #[test]
    fn test_detect_sensitive_fields_multiple_profiles() {
        let fields = vec![make_field("BALANCE"), make_field("PATIENT-NAME")];
        let warnings =
            detect_sensitive_fields(&fields, &[ComplianceProfile::SOX, ComplianceProfile::HIPAA]);
        // BALANCE matches SOX, PATIENT-NAME matches HIPAA
        assert_eq!(warnings.len(), 2);
    }

    #[test]
    fn test_detect_sensitive_fields_case_insensitive() {
        let fields = vec![make_field("account-balance")];
        let warnings = detect_sensitive_fields(&fields, &[ComplianceProfile::SOX]);
        // "account-balance" uppercased contains both "ACCOUNT" and "BALANCE"
        // but we break after the first match per (field, profile)
        assert_eq!(warnings.len(), 1, "Should match case-insensitively");
    }
}
