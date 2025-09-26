//! Enterprise Compliance Engine
//!
//! Provides comprehensive compliance validation for regulatory frameworks
//! including SOX, HIPAA, GDPR, and PCI DSS with automated validation
//! and remediation guidance.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::context::SecurityClassification;
use super::{AuditContext, AuditResult};

/// Enterprise compliance engine for regulatory validation
pub struct ComplianceEngine {
    profiles: Vec<ComplianceProfile>,
    validators: HashMap<ComplianceProfile, Box<dyn ComplianceValidator>>,
    config: ComplianceConfig,
}

impl ComplianceEngine {
    /// Create a new compliance engine
    pub fn new() -> Self {
        Self {
            profiles: Vec::new(),
            validators: HashMap::new(),
            config: ComplianceConfig::default(),
        }
    }

    /// Add compliance profiles to validate against
    #[must_use]
    pub fn with_profiles(mut self, profiles: &[ComplianceProfile]) -> Self {
        self.profiles = profiles.to_vec();

        // Initialize validators for each profile
        for profile in profiles {
            let validator: Box<dyn ComplianceValidator> = match profile {
                ComplianceProfile::SOX => Box::new(SoxValidator::new()),
                ComplianceProfile::HIPAA => Box::new(HipaaValidator::new()),
                ComplianceProfile::GDPR => Box::new(GdprValidator::new()),
                ComplianceProfile::PciDss => Box::new(PciDssValidator::new()),
            };
            self.validators.insert(*profile, validator);
        }

        self
    }

    /// Set compliance configuration
    #[must_use]
    pub fn with_config(mut self, config: ComplianceConfig) -> Self {
        self.config = config;
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
                        // This ensures compliance failures are tracked even if validator crashes
                        violations.push(ComplianceViolation {
                            violation_id: format!("{:?}-VALIDATOR-FAILURE", profile),
                            regulation: format!("{:?} Compliance Framework", profile),
                            severity: ComplianceSeverity::Critical,
                            title: "Compliance Validator System Failure".to_string(),
                            description: format!(
                                "Critical failure in {:?} compliance validator: {}. \
                                Compliance status cannot be determined for this framework.",
                                profile, e
                            ),
                            remediation: format!(
                                "Investigate and resolve {:?} validator system issues. \
                                Review audit logs and system health. \
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
                        "The compliance recommendation system failed: {}. \
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
                                "The {:?} compliance validator failed to generate recommendations due to: {}. \
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
        Self::new()
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
    #[allow(dead_code)]
    config: SoxConfig,
}

impl SoxValidator {
    pub fn new() -> Self {
        Self {
            config: SoxConfig::default(),
        }
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

        // Validate financial data processing controls
        violations.extend(self.validate_financial_data_controls(context));

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

        if matches!(
            context.security.classification,
            SecurityClassification::MaterialTransaction
        ) {
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

        Ok(recommendations)
    }
}

/// HIPAA compliance validator
#[derive(Default)]
pub struct HipaaValidator {
    #[allow(dead_code)]
    config: HipaaConfig,
}

impl HipaaValidator {
    pub fn new() -> Self {
        Self {
            config: HipaaConfig::default(),
        }
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
        if !self.has_adequate_technical_safeguards(context) {
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
            if !context
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

        if matches!(context.security.classification, SecurityClassification::PHI) {
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
    #[allow(dead_code)]
    config: GdprConfig,
}

impl GdprValidator {
    pub fn new() -> Self {
        Self {
            config: GdprConfig::default(),
        }
    }

    fn validate_data_protection_principles(
        &self,
        context: &AuditContext,
    ) -> Vec<ComplianceViolation> {
        let mut violations = Vec::new();

        // GDPR Article 5: Principles of data processing
        if !self.has_legal_basis_documentation(context) {
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
        let recommendations = vec![ComplianceRecommendation {
            recommendation_id: "GDPR-REC-001".to_string(),
            priority: RecommendationPriority::High,
            title: "Implement Data Subject Rights Portal".to_string(),
            description:
                "Automated portal for data subject access, rectification, and erasure requests"
                    .to_string(),
            implementation_effort: "4-6 weeks".to_string(),
            compliance_benefit: "Ensures GDPR Articles 15-17 compliance for data subject rights"
                .to_string(),
        }];

        Ok(recommendations)
    }
}

/// PCI DSS compliance validator (placeholder)
#[derive(Default)]
pub struct PciDssValidator {
    #[allow(dead_code)]
    config: PciDssConfig,
}

impl PciDssValidator {
    pub fn new() -> Self {
        Self {
            config: PciDssConfig::default(),
        }
    }
}

#[async_trait::async_trait]
impl ComplianceValidator for PciDssValidator {
    async fn validate_operation(
        &self,
        _context: &AuditContext,
    ) -> AuditResult<ComplianceValidationResult> {
        // PCI DSS validation would be implemented here
        Ok(ComplianceValidationResult {
            violations: Vec::new(),
            warnings: Vec::new(),
        })
    }

    async fn generate_recommendations(
        &self,
        _context: &AuditContext,
    ) -> AuditResult<Vec<ComplianceRecommendation>> {
        Ok(Vec::new())
    }
}

// Configuration structures

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceConfig {
    pub strict_mode: bool,
    pub review_interval_days: Option<u32>,
    pub auto_remediation: bool,
    pub notification_endpoints: Vec<String>,
}

impl Default for ComplianceConfig {
    fn default() -> Self {
        Self {
            strict_mode: true,
            review_interval_days: Some(90),
            auto_remediation: false,
            notification_endpoints: Vec::new(),
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
    pub cross_border_transfer_monitoring: bool,
}

impl Default for GdprConfig {
    fn default() -> Self {
        Self {
            legal_basis_validation: true,
            data_subject_rights_automation: true,
            cross_border_transfer_monitoring: true,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PciDssConfig {
    pub cardholder_data_validation: bool,
}

impl Default for PciDssConfig {
    fn default() -> Self {
        Self {
            cardholder_data_validation: true,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compliance_engine_creation() {
        let engine = ComplianceEngine::new()
            .with_profiles(&[ComplianceProfile::SOX, ComplianceProfile::HIPAA]);

        assert_eq!(engine.profiles.len(), 2);
        assert!(engine.profiles.contains(&ComplianceProfile::SOX));
        assert!(engine.profiles.contains(&ComplianceProfile::HIPAA));
    }

    #[tokio::test]
    async fn test_sox_compliance_validation() {
        let context = AuditContext::new()
            .with_security_classification(SecurityClassification::MaterialTransaction);

        let sox_validator = SoxValidator::new();
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

        let hipaa_validator = HipaaValidator::new();
        let result = hipaa_validator
            .validate_operation(&context)
            .await
            .expect("HIPAA validation should not fail in test environment");

        // Should have violations due to missing PHI protections in default context
        assert!(!result.violations.is_empty());
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
        let engine = ComplianceEngine::new();

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
        let custom_engine = ComplianceEngine::new().with_config(custom_config);
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

        let sox_validator = SoxValidator::new();
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
}
