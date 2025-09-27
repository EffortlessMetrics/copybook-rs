# ADR-004: Multi-Framework Compliance Engine Architecture

## Status
Accepted

## Context
The copybook-rs Enterprise Audit System must support multiple regulatory compliance frameworks simultaneously (SOX, HIPAA, GDPR, PCI DSS) while maintaining performance and extensibility for future regulatory requirements. Each framework has distinct requirements, validation rules, and reporting obligations that must be handled consistently and efficiently.

### Regulatory Framework Requirements

#### SOX (Sarbanes-Oxley Act)
- **Section 302**: CEO/CFO certification of financial data integrity
- **Section 404**: Internal control assessment and documentation
- **Data Integrity**: Cryptographic validation of financial data processing
- **Access Controls**: Segregation of duties and authorization trails
- **Change Management**: Documentation of all processing changes
- **Retention**: 7-year audit trail retention requirement

#### HIPAA (Health Insurance Portability and Accountability Act)
- **Security Rule**: Administrative, physical, and technical safeguards
- **Privacy Rule**: Minimum necessary principle and access controls
- **Breach Notification**: Detection and reporting of unauthorized access
- **Audit Controls**: Hardware, software, and procedural mechanisms
- **Integrity**: PHI must not be improperly altered or destroyed
- **Access Management**: Unique user identification and access control

#### GDPR (General Data Protection Regulation)
- **Article 32**: Security of processing technical measures
- **Article 30**: Records of processing activities
- **Article 33**: Breach notification requirements
- **Data Minimization**: Process only necessary personal data
- **Consent Management**: Legal basis tracking and consent records
- **Subject Rights**: Access, portability, erasure, and rectification

#### PCI DSS (Payment Card Industry Data Security Standard)
- **Requirement 3**: Protect stored cardholder data
- **Requirement 7**: Restrict access by business need-to-know
- **Requirement 8**: Identify and authenticate access to system components
- **Requirement 10**: Track and monitor all access to network resources
- **Requirement 11**: Regularly test security systems and processes

### Technical Constraints
- Must integrate with existing audit event system (ADR-001)
- Performance impact <2% for compliance validation operations
- Support for real-time compliance monitoring and alerting
- Extensible architecture for future regulatory frameworks
- Enterprise configuration management for policy customization

## Decision
We will implement a modular compliance engine architecture using trait-based validation, rule engine pattern, and pluggable compliance providers with centralized policy management.

### 1. Compliance Engine Architecture
```rust
pub struct ComplianceEngine {
    validators: HashMap<ComplianceProfile, Box<dyn ComplianceValidator>>,
    rule_engine: ComplianceRuleEngine,
    policy_manager: PolicyManager,
    violation_tracker: ViolationTracker,
    remediation_engine: RemediationEngine,
}
```

### 2. Trait-Based Validation System
Each compliance framework implements a common validation trait:
```rust
#[async_trait]
pub trait ComplianceValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
        event: &AuditEvent,
    ) -> Result<ComplianceResult, ComplianceError>;

    fn get_requirements(&self) -> Vec<ComplianceRequirement>;
    fn get_framework_info(&self) -> FrameworkInfo;
    async fn generate_recommendations(&self, violations: &[ComplianceViolation]) -> Vec<ComplianceRecommendation>;
}
```

### 3. Rule Engine Pattern
Declarative rule definitions with dynamic evaluation:
- Rules defined in configuration files or policy management systems
- Runtime rule evaluation with performance optimization
- Custom rule creation for organization-specific requirements
- Rule versioning and change management

### 4. Pluggable Compliance Providers
Framework-specific implementations as separate modules:
- `sox_validator`: SOX compliance validation
- `hipaa_validator`: HIPAA compliance validation
- `gdpr_validator`: GDPR compliance validation
- `pci_validator`: PCI DSS compliance validation
- `custom_validator`: Organization-specific compliance rules

### 5. Centralized Policy Management
Enterprise policy configuration and management:
- Policy templates for different industries and use cases
- Dynamic policy updates without system restart
- Policy inheritance and override capabilities
- Audit trail for policy changes

## Implementation Architecture

### Core Compliance Engine
```rust
#[derive(Debug)]
pub struct ComplianceEngine {
    validators: HashMap<ComplianceProfile, Box<dyn ComplianceValidator>>,
    rule_engine: ComplianceRuleEngine,
    policy_manager: PolicyManager,
    violation_tracker: ViolationTracker,
    remediation_engine: RemediationEngine,
    metrics: Arc<Mutex<ComplianceMetrics>>,
}

impl ComplianceEngine {
    pub fn new() -> Self {
        let mut validators: HashMap<ComplianceProfile, Box<dyn ComplianceValidator>> = HashMap::new();

        // Register framework validators
        validators.insert(ComplianceProfile::SOX, Box::new(SoxValidator::new()));
        validators.insert(ComplianceProfile::HIPAA, Box::new(HipaaValidator::new()));
        validators.insert(ComplianceProfile::GDPR, Box::new(GdprValidator::new()));
        validators.insert(ComplianceProfile::PciDss, Box::new(PciValidator::new()));

        Self {
            validators,
            rule_engine: ComplianceRuleEngine::new(),
            policy_manager: PolicyManager::new(),
            violation_tracker: ViolationTracker::new(),
            remediation_engine: RemediationEngine::new(),
            metrics: Arc::new(Mutex::new(ComplianceMetrics::default())),
        }
    }

    pub async fn validate_processing_operation(
        &self,
        context: &AuditContext,
    ) -> Result<ComplianceResult, ComplianceError> {
        let mut combined_result = ComplianceResult::new();

        // Validate against each required compliance framework
        for profile in &context.compliance_profiles {
            if let Some(validator) = self.validators.get(profile) {
                let start_time = Instant::now();

                match validator.validate_operation(context, &AuditEvent::default()).await {
                    Ok(result) => {
                        combined_result.merge(result);
                        self.update_validation_metrics(profile, start_time.elapsed(), true).await;
                    }
                    Err(e) => {
                        combined_result.add_error(profile.clone(), e);
                        self.update_validation_metrics(profile, start_time.elapsed(), false).await;
                    }
                }
            }
        }

        // Track violations for trending and remediation
        if !combined_result.violations.is_empty() {
            self.violation_tracker.record_violations(&combined_result.violations).await;
        }

        Ok(combined_result)
    }

    pub async fn generate_compliance_report(
        &self,
        context: &AuditContext,
    ) -> Result<ComplianceReport, ComplianceError> {
        let mut report = ComplianceReport {
            report_id: generate_report_id(),
            operation_id: context.operation_id.clone(),
            generated_at: SystemTime::now(),
            compliance_profiles: context.compliance_profiles.clone(),
            overall_status: ComplianceStatus::Compliant,
            framework_results: HashMap::new(),
            recommendations: Vec::new(),
            remediation_actions: Vec::new(),
        };

        // Generate framework-specific assessments
        for profile in &context.compliance_profiles {
            if let Some(validator) = self.validators.get(profile) {
                let framework_assessment = self.assess_framework_compliance(
                    validator.as_ref(),
                    context,
                ).await?;

                if !framework_assessment.is_compliant {
                    report.overall_status = ComplianceStatus::NonCompliant;
                }

                report.framework_results.insert(profile.clone(), framework_assessment);
            }
        }

        // Generate recommendations
        if report.overall_status == ComplianceStatus::NonCompliant {
            report.recommendations = self.generate_compliance_recommendations(&report).await?;
            report.remediation_actions = self.remediation_engine
                .generate_remediation_plan(&report.recommendations).await?;
        }

        Ok(report)
    }
}
```

### SOX Compliance Validator
```rust
#[derive(Debug)]
pub struct SoxValidator {
    requirements: Vec<SoxRequirement>,
    control_framework: SoxControlFramework,
}

#[async_trait]
impl ComplianceValidator for SoxValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
        event: &AuditEvent,
    ) -> Result<ComplianceResult, ComplianceError> {
        let mut result = ComplianceResult::new();
        result.profile = ComplianceProfile::SOX;

        // SOX Section 302 - Management Certification Requirements
        if !self.validate_data_integrity_controls(context, event) {
            result.add_violation(ComplianceViolation {
                violation_id: "SOX-302-001".to_string(),
                regulation: "SOX Section 302".to_string(),
                title: "Data Integrity Controls Missing".to_string(),
                description: "Financial data processing lacks cryptographic integrity validation required for management certification.".to_string(),
                severity: ViolationSeverity::High,
                affected_scope: vec![context.operation_id.clone()],
                business_impact: BusinessImpact::High,
                remediation: "Implement SHA-256 hash validation for all financial data processing operations.".to_string(),
                remediation_effort: RemediationEffort {
                    estimated_hours: 8,
                    required_resources: vec!["Security Engineer".to_string()],
                    complexity: ComplexityLevel::Medium,
                    requires_approval: false,
                },
                detected_at: SystemTime::now(),
                remediation_due: Some(SystemTime::now() + Duration::from_secs(7 * 24 * 3600)), // 7 days
                risk_level: RiskLevel::High,
            });
        }

        // SOX Section 404 - Internal Control Assessment
        if !self.validate_internal_controls(context) {
            result.add_violation(ComplianceViolation {
                violation_id: "SOX-404-001".to_string(),
                regulation: "SOX Section 404".to_string(),
                title: "Internal Control Documentation Insufficient".to_string(),
                description: "Processing operation lacks documented internal controls required for Section 404 assessment.".to_string(),
                severity: ViolationSeverity::Medium,
                affected_scope: vec![context.operation_id.clone()],
                business_impact: BusinessImpact::Medium,
                remediation: "Document internal control procedures and establish audit trail requirements.".to_string(),
                remediation_effort: RemediationEffort {
                    estimated_hours: 16,
                    required_resources: vec!["Compliance Officer".to_string(), "Process Owner".to_string()],
                    complexity: ComplexityLevel::Medium,
                    requires_approval: true,
                },
                detected_at: SystemTime::now(),
                remediation_due: Some(SystemTime::now() + Duration::from_secs(30 * 24 * 3600)), // 30 days
                risk_level: RiskLevel::Medium,
            });
        }

        // Access Control Validation
        if !self.validate_segregation_of_duties(context) {
            result.add_violation(self.create_segregation_violation(context));
        }

        // Change Management Validation
        if !self.validate_change_management(context, event) {
            result.add_violation(self.create_change_management_violation(context));
        }

        Ok(result)
    }

    fn get_requirements(&self) -> Vec<ComplianceRequirement> {
        vec![
            ComplianceRequirement {
                requirement_id: "SOX-302".to_string(),
                title: "Management Assessment of Internal Controls".to_string(),
                description: "CEO and CFO must certify the accuracy of financial statements and adequacy of internal controls.".to_string(),
                mandatory: true,
                controls: vec![
                    "Data integrity validation".to_string(),
                    "Processing audit trails".to_string(),
                    "Access control documentation".to_string(),
                ],
            },
            ComplianceRequirement {
                requirement_id: "SOX-404".to_string(),
                title: "Internal Control Over Financial Reporting".to_string(),
                description: "Management must assess and report on internal control effectiveness.".to_string(),
                mandatory: true,
                controls: vec![
                    "Control documentation".to_string(),
                    "Control testing".to_string(),
                    "Deficiency remediation".to_string(),
                ],
            },
        ]
    }

    async fn generate_recommendations(
        &self,
        violations: &[ComplianceViolation],
    ) -> Vec<ComplianceRecommendation> {
        let mut recommendations = Vec::new();

        for violation in violations {
            match violation.violation_id.as_str() {
                "SOX-302-001" => {
                    recommendations.push(ComplianceRecommendation {
                        recommendation_id: "SOX-REC-001".to_string(),
                        title: "Implement Cryptographic Data Integrity".to_string(),
                        description: "Deploy SHA-256 hash validation for all financial data processing to ensure data integrity for management certification.".to_string(),
                        priority: RecommendationPriority::High,
                        category: RecommendationCategory::TechnicalControl,
                        estimated_effort: ImplementationEffort::Medium,
                        business_impact: BusinessImpactLevel::High,
                        implementation_steps: vec![
                            "Configure cryptographic integrity validation".to_string(),
                            "Implement hash chain verification".to_string(),
                            "Document integrity control procedures".to_string(),
                            "Train operations team on validation procedures".to_string(),
                        ],
                    });
                }
                "SOX-404-001" => {
                    recommendations.push(ComplianceRecommendation {
                        recommendation_id: "SOX-REC-002".to_string(),
                        title: "Enhance Internal Control Documentation".to_string(),
                        description: "Develop comprehensive documentation of internal controls over financial reporting processes.".to_string(),
                        priority: RecommendationPriority::Medium,
                        category: RecommendationCategory::ProcessImprovement,
                        estimated_effort: ImplementationEffort::High,
                        business_impact: BusinessImpactLevel::Medium,
                        implementation_steps: vec![
                            "Document current control procedures".to_string(),
                            "Identify control gaps and weaknesses".to_string(),
                            "Design enhanced control framework".to_string(),
                            "Implement control testing procedures".to_string(),
                        ],
                    });
                }
                _ => {}
            }
        }

        recommendations
    }
}

impl SoxValidator {
    fn validate_data_integrity_controls(&self, context: &AuditContext, event: &AuditEvent) -> bool {
        // Check for cryptographic integrity
        if event.integrity_hash.is_empty() {
            return false;
        }

        // Check for material transaction classification
        if context.security.classification == SecurityClassification::MaterialTransaction {
            // Additional controls required for material transactions
            if context.security.permissions.is_empty() {
                return false;
            }
        }

        // Check for proper authorization
        if context.user.is_none() {
            return false;
        }

        true
    }

    fn validate_internal_controls(&self, context: &AuditContext) -> bool {
        // Check for documented procedures
        if !context.metadata.contains_key("control_procedure_id") {
            return false;
        }

        // Check for control testing evidence
        if !context.metadata.contains_key("control_test_date") {
            return false;
        }

        // Check for segregation of duties
        if !self.validate_segregation_of_duties(context) {
            return false;
        }

        true
    }

    fn validate_segregation_of_duties(&self, context: &AuditContext) -> bool {
        // Check for dual control on sensitive operations
        if context.security.classification == SecurityClassification::MaterialTransaction {
            // Material transactions require dual authorization
            if !context.metadata.contains_key("dual_authorization") {
                return false;
            }
        }

        // Check for role-based access controls
        if context.security.permissions.iter().any(|p| matches!(p, Permission::SystemAdministrator)) {
            // Admin users should not process financial transactions
            if context.operation_id.contains("financial") {
                return false;
            }
        }

        true
    }

    fn validate_change_management(&self, context: &AuditContext, event: &AuditEvent) -> bool {
        match event.event_type {
            AuditEventType::ConfigurationChange => {
                // Configuration changes require approval
                if !context.metadata.contains_key("change_approval_id") {
                    return false;
                }

                // Changes require business justification
                if !context.metadata.contains_key("business_justification") {
                    return false;
                }
            }
            _ => {}
        }

        true
    }
}
```

### HIPAA Compliance Validator
```rust
#[derive(Debug)]
pub struct HipaaValidator {
    safeguards: HipaaSafeguards,
    phi_detector: PhiDetector,
    access_controls: HipaaAccessControls,
}

#[async_trait]
impl ComplianceValidator for HipaaValidator {
    async fn validate_operation(
        &self,
        context: &AuditContext,
        event: &AuditEvent,
    ) -> Result<ComplianceResult, ComplianceError> {
        let mut result = ComplianceResult::new();
        result.profile = ComplianceProfile::HIPAA;

        // Administrative Safeguards Validation
        if !self.validate_administrative_safeguards(context) {
            result.add_violation(ComplianceViolation {
                violation_id: "HIPAA-ADMIN-001".to_string(),
                regulation: "HIPAA Security Rule § 164.308(a)(1)".to_string(),
                title: "Administrative Safeguards Missing".to_string(),
                description: "Required administrative safeguards for PHI processing not implemented.".to_string(),
                severity: ViolationSeverity::High,
                affected_scope: vec![context.operation_id.clone()],
                business_impact: BusinessImpact::Critical,
                remediation: "Implement assigned security responsibility, workforce training, and access management procedures.".to_string(),
                remediation_effort: RemediationEffort {
                    estimated_hours: 40,
                    required_resources: vec!["HIPAA Security Officer".to_string(), "IT Security".to_string()],
                    complexity: ComplexityLevel::High,
                    requires_approval: true,
                },
                detected_at: SystemTime::now(),
                remediation_due: Some(SystemTime::now() + Duration::from_secs(60 * 24 * 3600)), // 60 days
                risk_level: RiskLevel::Critical,
            });
        }

        // Technical Safeguards Validation
        if !self.validate_technical_safeguards(context, event) {
            result.add_violation(self.create_technical_safeguards_violation(context));
        }

        // PHI Minimum Necessary Validation
        if context.security.classification == SecurityClassification::PHI {
            if !self.validate_minimum_necessary(context) {
                result.add_warning(ComplianceWarning {
                    warning_id: "HIPAA-MIN-001".to_string(),
                    title: "Minimum Necessary Assessment Recommended".to_string(),
                    description: "PHI access should be evaluated for minimum necessary compliance.".to_string(),
                    recommendation: "Document business justification for PHI access scope.".to_string(),
                });
            }
        }

        // Breach Detection Validation
        if !self.validate_breach_detection_capabilities(context) {
            result.add_violation(self.create_breach_detection_violation());
        }

        Ok(result)
    }

    fn get_requirements(&self) -> Vec<ComplianceRequirement> {
        vec![
            ComplianceRequirement {
                requirement_id: "HIPAA-SECURITY".to_string(),
                title: "HIPAA Security Rule Compliance".to_string(),
                description: "Implement administrative, physical, and technical safeguards for PHI.".to_string(),
                mandatory: true,
                controls: vec![
                    "Assigned security responsibility".to_string(),
                    "Workforce access controls".to_string(),
                    "Access audit controls".to_string(),
                    "Integrity validation".to_string(),
                    "Transmission security".to_string(),
                ],
            },
        ]
    }
}

impl HipaaValidator {
    fn validate_administrative_safeguards(&self, context: &AuditContext) -> bool {
        // Check for assigned security responsibility
        if !context.metadata.contains_key("security_officer") {
            return false;
        }

        // Check for workforce training
        if context.user.is_some() && !context.metadata.contains_key("hipaa_training_date") {
            return false;
        }

        // Check for access management procedures
        if context.security.classification == SecurityClassification::PHI {
            if !context.metadata.contains_key("access_authorization_date") {
                return false;
            }
        }

        true
    }

    fn validate_technical_safeguards(&self, context: &AuditContext, event: &AuditEvent) -> bool {
        // Check for unique user identification
        if context.user.is_none() {
            return false;
        }

        // Check for audit controls
        if event.integrity_hash.is_empty() {
            return false;
        }

        // Check for integrity controls
        if context.security.classification == SecurityClassification::PHI {
            if event.digital_signature.is_none() {
                return false; // PHI requires stronger integrity controls
            }
        }

        // Check for transmission security
        if !context.metadata.contains_key("transmission_security") {
            return false;
        }

        true
    }

    fn validate_minimum_necessary(&self, context: &AuditContext) -> bool {
        // Check for minimum necessary justification
        context.metadata.contains_key("minimum_necessary_justification")
    }

    fn validate_breach_detection_capabilities(&self, context: &AuditContext) -> bool {
        // Check for breach detection monitoring
        context.metadata.contains_key("breach_monitoring_enabled")
    }
}
```

### Rule Engine Implementation
```rust
#[derive(Debug)]
pub struct ComplianceRuleEngine {
    rules: HashMap<ComplianceProfile, Vec<ComplianceRule>>,
    rule_evaluator: RuleEvaluator,
    rule_cache: LruCache<String, RuleEvaluationResult>,
}

impl ComplianceRuleEngine {
    pub async fn evaluate_rules(
        &self,
        profile: ComplianceProfile,
        context: &AuditContext,
        event: &AuditEvent,
    ) -> Result<RuleEvaluationResult, ComplianceError> {
        let cache_key = format!("{}:{}:{}", profile, context.operation_id, event.event_id);

        if let Some(cached_result) = self.rule_cache.get(&cache_key) {
            return Ok(cached_result.clone());
        }

        let mut evaluation_result = RuleEvaluationResult::new();

        if let Some(rules) = self.rules.get(&profile) {
            for rule in rules {
                let rule_result = self.rule_evaluator.evaluate_rule(rule, context, event).await?;
                evaluation_result.add_rule_result(rule_result);
            }
        }

        self.rule_cache.put(cache_key, evaluation_result.clone());
        Ok(evaluation_result)
    }

    pub fn load_rules_from_config(&mut self, config: &ComplianceRuleConfig) -> Result<(), ComplianceError> {
        for (profile, rule_definitions) in &config.rule_definitions {
            let mut profile_rules = Vec::new();

            for rule_def in rule_definitions {
                let rule = ComplianceRule::from_definition(rule_def)?;
                profile_rules.push(rule);
            }

            self.rules.insert(profile.clone(), profile_rules);
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ComplianceRule {
    pub rule_id: String,
    pub name: String,
    pub description: String,
    pub condition: RuleCondition,
    pub action: RuleAction,
    pub severity: ViolationSeverity,
}

#[derive(Debug, Clone)]
pub enum RuleCondition {
    And(Vec<RuleCondition>),
    Or(Vec<RuleCondition>),
    Not(Box<RuleCondition>),
    FieldEquals { field: String, value: String },
    FieldContains { field: String, value: String },
    FieldMissing { field: String },
    SecurityClassification { classification: SecurityClassification },
    UserRole { role: String },
    EventType { event_type: AuditEventType },
}

#[derive(Debug, Clone)]
pub enum RuleAction {
    CreateViolation { violation_template: ViolationTemplate },
    CreateWarning { warning_template: WarningTemplate },
    RequireApproval { approval_type: ApprovalType },
    TriggerAlert { alert_template: AlertTemplate },
}
```

## Configuration Management

### Policy Configuration
```yaml
# Multi-framework compliance configuration
compliance:
  enabled_frameworks:
    - SOX
    - HIPAA
    - GDPR
    - PCI

  sox:
    validation_level: "strict"
    material_transaction_threshold: 5000000  # $5M
    dual_control_required: true
    change_approval_required: true
    retention_years: 7

    controls:
      data_integrity:
        enabled: true
        cryptographic_validation: true
        hash_algorithm: "SHA256"

      access_controls:
        segregation_of_duties: true
        role_based_access: true
        dual_authorization: true

  hipaa:
    phi_detection: true
    minimum_necessary_enforcement: "warn"  # strict, warn, disabled
    breach_threshold: 500  # Number of records
    technical_safeguards:
      unique_user_identification: true
      automatic_logoff: 30  # minutes
      encryption_at_rest: true
      encryption_in_transit: true

  gdpr:
    personal_data_detection: true
    consent_tracking: true
    data_minimization_checks: true
    breach_notification_hours: 72

    lawful_basis_validation:
      - "consent"
      - "contract"
      - "legal_obligation"
      - "legitimate_interest"

  pci:
    cardholder_data_detection: true
    encryption_requirements:
      minimum_key_length: 256
      approved_algorithms: ["AES"]

    access_controls:
      need_to_know_basis: true
      role_based_restrictions: true

  custom_rules:
    - rule_id: "FINANCE-001"
      name: "High Value Transaction Validation"
      condition:
        and:
          - field_equals: { field: "data_classification", value: "MaterialTransaction" }
          - field_contains: { field: "operation_id", value: "financial" }
      action:
        create_violation:
          title: "High Value Transaction Requires Enhanced Controls"
          severity: "High"
```

## Performance Optimization

### Validation Performance
```rust
impl ComplianceEngine {
    async fn validate_with_performance_optimization(
        &self,
        context: &AuditContext,
    ) -> Result<ComplianceResult, ComplianceError> {
        // Parallel validation of multiple frameworks
        let validation_futures: Vec<_> = context.compliance_profiles
            .iter()
            .filter_map(|profile| {
                self.validators.get(profile).map(|validator| {
                    let context = context.clone();
                    async move {
                        validator.validate_operation(&context, &AuditEvent::default()).await
                    }
                })
            })
            .collect();

        // Execute validations in parallel
        let validation_results = futures::future::join_all(validation_futures).await;

        // Combine results
        let mut combined_result = ComplianceResult::new();
        for result in validation_results {
            match result {
                Ok(validation_result) => {
                    combined_result.merge(validation_result);
                }
                Err(e) => {
                    // Log error but continue with other validations
                    warn!("Compliance validation error: {}", e);
                }
            }
        }

        Ok(combined_result)
    }
}
```

### Caching Strategy
```rust
pub struct ComplianceCache {
    validation_cache: LruCache<String, ComplianceResult>,
    rule_cache: LruCache<String, RuleEvaluationResult>,
    policy_cache: Arc<RwLock<PolicyCache>>,
}

impl ComplianceCache {
    pub async fn get_or_validate(
        &self,
        cache_key: &str,
        validator: impl Fn() -> Future<Output = Result<ComplianceResult, ComplianceError>>,
    ) -> Result<ComplianceResult, ComplianceError> {
        if let Some(cached_result) = self.validation_cache.get(cache_key) {
            return Ok(cached_result.clone());
        }

        let result = validator().await?;
        self.validation_cache.put(cache_key.to_string(), result.clone());
        Ok(result)
    }
}
```

## Testing and Validation

### Compliance Testing Framework
```rust
#[cfg(test)]
mod compliance_tests {
    use super::*;

    #[tokio::test]
    async fn test_sox_compliance_validation() {
        let context = AuditContext::new()
            .with_security_classification(SecurityClassification::MaterialTransaction)
            .with_compliance_profile(ComplianceProfile::SOX)
            .with_metadata("control_procedure_id", "FIN-CTRL-001");

        let engine = ComplianceEngine::new();
        let result = engine.validate_processing_operation(&context).await.unwrap();

        assert!(!result.is_compliant());
        assert!(!result.violations.is_empty());

        // Verify SOX-specific violations
        let sox_violations: Vec<_> = result.violations
            .iter()
            .filter(|v| v.regulation.contains("SOX"))
            .collect();
        assert!(!sox_violations.is_empty());
    }

    #[tokio::test]
    async fn test_multi_framework_validation() {
        let context = AuditContext::new()
            .with_compliance_profile(ComplianceProfile::SOX)
            .with_compliance_profile(ComplianceProfile::GDPR)
            .with_security_classification(SecurityClassification::Confidential);

        let engine = ComplianceEngine::new();
        let result = engine.validate_processing_operation(&context).await.unwrap();

        assert_eq!(result.validated_profiles.len(), 2);
        assert!(result.validated_profiles.contains(&ComplianceProfile::SOX));
        assert!(result.validated_profiles.contains(&ComplianceProfile::GDPR));
    }

    #[tokio::test]
    async fn test_compliance_performance() {
        let context = AuditContext::new()
            .with_compliance_profile(ComplianceProfile::SOX);

        let engine = ComplianceEngine::new();
        let start_time = Instant::now();

        for _ in 0..1000 {
            let _result = engine.validate_processing_operation(&context).await.unwrap();
        }

        let duration = start_time.elapsed();
        let avg_validation_time = duration.as_micros() / 1000;

        assert!(avg_validation_time < 2000, "Compliance validation too slow: {}μs", avg_validation_time);
    }
}
```

## Alternatives Considered

### Alternative 1: Single Monolithic Validator
**Approach**: Single validation class handling all compliance frameworks
**Rejected Because**:
- Poor separation of concerns
- Difficult to maintain and extend
- Performance impact of running all validations
- Complex testing and validation

### Alternative 2: External Compliance Service
**Approach**: Call external service for compliance validation
**Rejected Because**:
- Network latency and reliability concerns
- Security risks of sending audit data externally
- Additional infrastructure complexity
- Performance impact of network calls

### Alternative 3: Configuration-Only Rules
**Approach**: Define all compliance rules in configuration files
**Rejected Because**:
- Limited expressiveness for complex validation logic
- Difficulty in implementing framework-specific algorithms
- Performance overhead of rule interpretation
- Limited support for custom business logic

## Related Decisions
- ADR-001: Audit Event Schema Design
- ADR-002: Cryptographic Integrity Implementation
- ADR-003: Performance Optimization Strategy
- ADR-005: Enterprise Integration Patterns

## References
- [SOX Section 404 - Management Assessment of Internal Controls](https://www.sox-online.com/act_section_404.html)
- [HIPAA Security Rule](https://www.hhs.gov/hipaa/for-professionals/security/index.html)
- [GDPR Articles 30-34](https://gdpr-info.eu/chapter-4/)
- [PCI DSS Requirements](https://www.pcisecuritystandards.org/pci_security/)