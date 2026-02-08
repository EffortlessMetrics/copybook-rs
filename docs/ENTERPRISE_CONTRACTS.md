# Enterprise Contracts: Audit, Compliance, and Security Monitoring

**Phase 2.1 - Enterprise Feature Definition**

This document defines the contracts for enterprise features in copybook-rs. These contracts specify the minimum viable implementation for each feature, ensuring incremental delivery with safe defaults.

> **Status**: Design Phase - Implementation pending
> **Version**: 1.0.0
> **Last Updated**: 2026-02-07

---

## Table of Contents

1. [Audit System Contract](#audit-system-contract)
2. [Compliance Validation Contracts](#compliance-validation-contracts)
3. [Security Monitoring Contract](#security-monitoring-contract)
4. [Feature Flag Governance](#feature-flag-governance)
5. [Test Integration Requirements](#test-integration-requirements)
6. [Performance Impact Assessment](#performance-impact-assessment)

---

## Audit System Contract

### Overview

The audit system provides a tamper-evident, cryptographically chained audit trail for all copybook-rs operations. It must be zero-cost when disabled and have minimal overhead when enabled.

### 1. Event Schema (Versioned)

#### Core Audit Event Structure

```rust
/// Versioned audit event structure
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AuditEvent {
    /// Unique audit event identifier (format: audit-{timestamp}-{random_hex})
    pub event_id: String,

    /// Event schema version for backward compatibility
    pub event_version: AuditEventVersion,

    /// ISO 8601 timestamp with nanosecond precision
    pub timestamp: String,

    /// Classification of audit event
    pub event_type: AuditEventType,

    /// Operation context and metadata
    pub context: AuditContext,

    /// Event-specific payload data
    pub payload: AuditPayload,

    /// SHA-256 integrity hash for tamper detection
    pub integrity_hash: String,

    /// Previous event hash for chain validation
    pub previous_hash: Option<String>,

    /// Event severity level for monitoring and alerting
    pub severity: AuditSeverity,
}

/// Audit event schema version
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum AuditEventVersion {
    /// Initial version (v1.0.0)
    V1_0_0,
}

impl AuditEventVersion {
    /// Get current event version
    pub fn current() -> Self {
        Self::V1_0_0
    }

    /// Serialize version string
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::V1_0_0 => "1.0.0",
        }
    }
}
```

#### Event Types

```rust
/// Comprehensive audit event type taxonomy
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum AuditEventType {
    /// Copybook parsing operations
    CopybookParse,

    /// Data validation and verification
    DataValidation,

    /// Data transformation (decode/encode)
    DataTransformation,

    /// Performance measurement events
    PerformanceMeasurement,

    /// Compliance validation events
    ComplianceCheck,

    /// Security-relevant events
    SecurityEvent,

    /// Data lineage tracking
    LineageTracking,

    /// Error and exception events
    ErrorEvent,

    /// Access control events
    AccessEvent,

    /// Configuration change events
    ConfigurationChange,
}
```

#### Event Payloads

```rust
/// Audit event payload containing event-specific data
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum AuditPayload {
    CopybookParse {
        copybook_path: String,
        schema_fingerprint: String,
        parse_result: ParseResult,
        parsing_duration_ms: u64,
        field_count: usize,
        level_88_count: usize,
        error_count: usize,
        warnings: Vec<String>,
    },

    DataValidation {
        input_file: String,
        validation_result: ValidationResult,
        validation_duration_ms: u64,
        records_validated: u64,
        errors_found: u64,
        error_details: Vec<ValidationError>,
        validation_rules: Vec<String>,
    },

    DataTransformation {
        operation: TransformationOperation,
        input_file: String,
        output_file: String,
        transformation_result: TransformationResult,
        processing_duration_ms: u64,
        records_processed: u64,
        bytes_processed: u64,
        throughput_bytes_per_sec: u64,
        memory_usage_mb: u64,
    },

    PerformanceMeasurement {
        measurement_type: PerformanceMeasurementType,
        baseline_id: Option<String>,
        metrics: PerformanceMetrics,
        comparison_result: Option<ComparisonResult>,
        regression_detected: bool,
    },

    ComplianceCheck {
        compliance_framework: String,
        validation_result: ComplianceValidationResult,
        violations: Vec<ComplianceViolationDetail>,
        remediation_required: bool,
        next_review_date: Option<String>,
    },

    SecurityEvent {
        security_event_type: SecurityEventType,
        severity: String,
        affected_resources: Vec<String>,
        threat_indicators: Vec<String>,
        remediation_actions: Vec<String>,
        incident_id: Option<String>,
    },

    LineageTracking {
        source_system: String,
        target_system: String,
        field_mappings: Vec<FieldMapping>,
        transformation_rules: Vec<TransformationRule>,
        quality_score: f64,
        impact_assessment: Option<ImpactAssessmentSummary>,
    },

    ErrorEvent {
        error_code: String,
        error_message: String,
        error_category: String,
        stack_trace: Option<String>,
        context_information: HashMap<String, String>,
        recovery_actions: Vec<String>,
        user_impact: UserImpactLevel,
    },

    AccessEvent {
        access_type: AccessType,
        resource_type: String,
        resource_id: String,
        access_result: AccessResult,
        user_id: String,
        source_ip: Option<String>,
        user_agent: Option<String>,
        session_id: Option<String>,
    },

    ConfigurationChange {
        component: String,
        change_type: ConfigurationChangeType,
        old_configuration: Option<String>,
        new_configuration: String,
        change_reason: String,
        approved_by: Option<String>,
        rollback_available: bool,
    },
}
```

### 2. Emission Points

#### Required Emission Points

| Operation | Event Type | Required | Notes |
|-----------|-----------|----------|-------|
| Copybook parse start | CopybookParse | Yes | With lightweight context |
| Copybook parse end | CopybookParse | Yes | With full result |
| Data validation | DataValidation | Yes | Per batch |
| Decode operation | DataTransformation | Yes | Per file |
| Encode operation | DataTransformation | Yes | Per file |
| Validation failure | ErrorEvent | Yes | Always |
| Compliance check | ComplianceCheck | When enabled | Per compliance profile |
| Security event | SecurityEvent | When enabled | On security-relevant operations |
| Configuration change | ConfigurationChange | Yes | On config modifications |

#### Emission Strategy

```rust
/// Emission strategy for audit events
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum EmissionStrategy {
    /// Emit immediately (blocking)
    Blocking,

    /// Emit to buffer, flush periodically
    Buffered,

    /// Emit to background channel (async)
    Async,

    /// No emission (feature disabled)
    Disabled,
}

impl EmissionStrategy {
    /// Determine emission strategy based on feature flag and context
    pub fn from_context(severity: AuditSeverity, feature_enabled: bool) -> Self {
        if !feature_enabled {
            return Self::Disabled;
        }

        match severity {
            AuditSeverity::Critical | AuditSeverity::High => Self::Blocking,
            AuditSeverity::Medium => Self::Buffered,
            AuditSeverity::Low | AuditSeverity::Info => Self::Async,
        }
    }
}
```

### 3. Redaction Policy

#### Redaction Rules

```rust
/// Redaction policy for audit payloads
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionPolicy {
    /// Fields to redact completely (replace with [REDACTED])
    pub redact_fields: Vec<String>,

    /// Fields to redact partially (show first N characters)
    pub partial_redact_fields: Vec<PartialRedactionRule>,

    /// Regular expressions for pattern-based redaction
    pub pattern_redactions: Vec<PatternRedactionRule>,

    /// Default redaction for unknown fields
    pub default_redaction: DefaultRedactionPolicy,
}

/// Partial redaction rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PartialRedactionRule {
    /// Field name pattern
    pub field_pattern: String,

    /// Number of characters to show
    pub visible_chars: usize,

    /// Character to use for redaction
    pub redaction_char: char,
}

/// Pattern-based redaction rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternRedactionRule {
    /// Regex pattern to match
    pub pattern: String,

    /// Replacement string
    pub replacement: String,

    /// Description of what this pattern redacts
    pub description: String,
}

/// Default redaction policy
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum DefaultRedactionPolicy {
    /// Keep all fields (no default redaction)
    Keep,

    /// Redact all unknown fields
    Redact,

    /// Partially redact all unknown fields
    PartialRedact,
}
```

#### Default Redaction Settings

```rust
impl Default for RedactionPolicy {
    fn default() -> Self {
        Self {
            redact_fields: vec![
                "password".to_string(),
                "secret".to_string(),
                "token".to_string(),
                "api_key".to_string(),
                "private_key".to_string(),
                "credit_card".to_string(),
                "ssn".to_string(),
                "phi".to_string(),  // Protected Health Information
            ],
            partial_redact_fields: vec![
                PartialRedactionRule {
                    field_pattern: "email".to_string(),
                    visible_chars: 2,
                    redaction_char: '*',
                },
                PartialRedactionRule {
                    field_pattern: "phone".to_string(),
                    visible_chars: 3,
                    redaction_char: '*',
                },
            ],
            pattern_redactions: vec![
                PatternRedactionRule {
                    pattern: r"\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b".to_string(),
                    replacement: "[CARD_NUMBER_REDACTED]".to_string(),
                    description: "Credit card numbers".to_string(),
                },
                PatternRedactionRule {
                    pattern: r"\b\d{3}-\d{2}-\d{4}\b".to_string(),
                    replacement: "[SSN_REDACTED]".to_string(),
                    description: "Social Security Numbers".to_string(),
                },
            ],
            default_redaction: DefaultRedactionPolicy::Keep,
        }
    }
}
```

### 4. Implementation Shape

#### AuditSink Trait

```rust
/// Trait for audit event sinks
pub trait AuditSink: Send + Sync {
    /// Emit an audit event
    fn emit(&self, event: AuditEvent) -> Result<(), AuditError>;

    /// Flush any buffered events
    fn flush(&self) -> Result<(), AuditError>;

    /// Get sink configuration
    fn config(&self) -> &AuditSinkConfig;
}

/// Audit sink configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditSinkConfig {
    /// Sink type
    pub sink_type: AuditSinkType,

    /// Buffer size for async sinks
    pub buffer_size: usize,

    /// Flush interval in milliseconds
    pub flush_interval_ms: u64,

    /// Maximum retry attempts for failed writes
    pub max_retries: u32,

    /// Redaction policy
    pub redaction: RedactionPolicy,
}

/// Audit sink types
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum AuditSinkType {
    /// No-op sink (disabled)
    Noop,

    /// File-based sink (JSON Lines)
    FileJsonl,

    /// File-based sink (CEF format)
    FileCef,

    /// Syslog sink
    Syslog,

    /// HTTP endpoint sink
    Http,

    /// Kafka topic sink
    Kafka,
}
```

#### NoopAuditSink Implementation

```rust
/// No-op audit sink for zero-cost when audit is disabled
#[derive(Debug, Clone)]
pub struct NoopAuditSink;

impl NoopAuditSink {
    pub fn new() -> Self {
        Self
    }
}

impl Default for NoopAuditSink {
    fn default() -> Self {
        Self::new()
    }
}

impl AuditSink for NoopAuditSink {
    #[inline]
    fn emit(&self, _event: AuditEvent) -> Result<(), AuditError> {
        Ok(())
    }

    #[inline]
    fn flush(&self) -> Result<(), AuditError> {
        Ok(())
    }

    fn config(&self) -> &AuditSinkConfig {
        static CONFIG: AuditSinkConfig = AuditSinkConfig {
            sink_type: AuditSinkType::Noop,
            buffer_size: 0,
            flush_interval_ms: 0,
            max_retries: 0,
            redaction: RedactionPolicy::default(),
        };
        &CONFIG
    }
}
```

#### AuditConfig Structure

```rust
/// Audit system configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditConfig {
    /// Whether audit is enabled
    pub enabled: bool,

    /// Audit sink
    pub sink: Box<dyn AuditSink>,

    /// Redaction policy
    pub redaction: RedactionPolicy,

    /// Emission strategy
    pub emission_strategy: EmissionStrategy,

    /// Event types to emit (empty = all)
    pub event_filter: Vec<AuditEventType>,

    /// Minimum severity to emit
    pub min_severity: AuditSeverity,

    /// Retention policy
    pub retention: RetentionPolicy,
}

impl Default for AuditConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            sink: Box::new(NoopAuditSink::new()),
            redaction: RedactionPolicy::default(),
            emission_strategy: EmissionStrategy::Disabled,
            event_filter: Vec::new(),
            min_severity: AuditSeverity::Info,
            retention: RetentionPolicy::default(),
        }
    }
}
```

### 5. CLI Integration

#### Proposed CLI Flags

```bash
# Enable audit system
copybook parse --audit

# Specify audit output file
copybook parse --audit --audit-jsonl /var/log/copybook/audit.jsonl

# Specify audit format
copybook parse --audit --audit-format jsonl|cef|syslog

# Configure redaction level
copybook parse --audit --redaction-level strict|moderate|none

# Set minimum severity to emit
copybook parse --audit --min-severity info|low|medium|high|critical

# Filter event types
copybook parse --audit --event-types CopybookParse,DataTransformation
```

#### Runtime Enable/Disable

```rust
/// Audit configuration from CLI
#[derive(Parser, Debug, Clone)]
pub struct AuditCliOpts {
    /// Enable audit system
    #[arg(long)]
    pub audit: bool,

    /// Audit output file (JSON Lines format)
    #[arg(long)]
    pub audit_jsonl: Option<PathBuf>,

    /// Audit output format
    #[arg(long, value_enum, default_value = "jsonl")]
    pub audit_format: AuditFormat,

    /// Redaction level
    #[arg(long, value_enum, default_value = "moderate")]
    pub redaction_level: RedactionLevel,

    /// Minimum severity to emit
    #[arg(long, value_enum, default_value = "info")]
    pub min_severity: AuditSeverity,

    /// Event types to emit (comma-separated)
    #[arg(long, value_delimiter = ',')]
    pub event_types: Vec<AuditEventType>,
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub enum AuditFormat {
    Jsonl,
    Cef,
    Syslog,
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq)]
pub enum RedactionLevel {
    None,
    Moderate,
    Strict,
}
```

---

## Compliance Validation Contracts

### Overview

Compliance validation provides enforceable policy sets for regulatory frameworks. The focus is on "checkbox compliance theater" avoidance - only include controls that are mechanically enforceable.

### 1. Policy Sets

#### SOX (Sarbanes-Oxley Act)

```rust
/// SOX compliance policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoxPolicy {
    /// Enable financial data validation
    pub financial_data_validation: bool,

    /// Require executive certification
    pub executive_certification_required: bool,

    /// Minimum audit retention in days (SOX requires 7 years = 2555 days)
    pub min_retention_days: u32,

    /// Require segregation of duties
    pub require_segregation_of_duties: bool,

    /// Require cryptographic integrity for financial data
    pub require_cryptographic_integrity: bool,

    /// Enable quarterly compliance reporting
    pub quarterly_reporting: bool,
}

impl Default for SoxPolicy {
    fn default() -> Self {
        Self {
            financial_data_validation: true,
            executive_certification_required: true,
            min_retention_days: 2555,  // 7 years
            require_segregation_of_duties: true,
            require_cryptographic_integrity: true,
            quarterly_reporting: true,
        }
    }
}
```

#### HIPAA (Health Insurance Portability and Accountability Act)

```rust
/// HIPAA compliance policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HipaaPolicy {
    /// Enable PHI protection
    pub phi_protection_enabled: bool,

    /// Minimum necessary enforcement level
    pub minimum_necessary_level: MinimumNecessaryLevel,

    /// Require encryption at rest
    pub require_encryption_at_rest: bool,

    /// Require encryption in transit
    pub require_encryption_in_transit: bool,

    /// Enable access logging
    pub access_logging_enabled: bool,

    /// Minimum audit retention in days (HIPAA requires 6 years = 2190 days)
    pub min_retention_days: u32,

    /// Enable breach detection
    pub breach_detection_enabled: bool,
}

impl Default for HipaaPolicy {
    fn default() -> Self {
        Self {
            phi_protection_enabled: true,
            minimum_necessary_level: MinimumNecessaryLevel::Strict,
            require_encryption_at_rest: true,
            require_encryption_in_transit: true,
            access_logging_enabled: true,
            min_retention_days: 2190,  // 6 years
            breach_detection_enabled: true,
        }
    }
}

/// Minimum necessary enforcement level
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum MinimumNecessaryLevel {
    /// No enforcement (not recommended)
    None,

    /// Warn when processing unnecessary data
    Warn,

    /// Block processing of unnecessary data
    Strict,
}
```

#### GDPR (General Data Protection Regulation)

```rust
/// GDPR compliance policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GdprPolicy {
    /// Enable data minimization
    pub data_minimization_enabled: bool,

    /// Require legal basis documentation
    pub require_legal_basis: bool,

    /// Enable data subject rights tracking
    pub enable_subject_rights: bool,

    /// Track cross-border transfers
    pub track_cross_border_transfers: bool,

    /// Default legal basis
    pub default_legal_basis: LegalBasis,

    /// Enable right to be forgotten support
    pub enable_right_to_erasure: bool,
}

impl Default for GdprPolicy {
    fn default() -> Self {
        Self {
            data_minimization_enabled: true,
            require_legal_basis: true,
            enable_subject_rights: true,
            track_cross_border_transfers: true,
            default_legal_basis: LegalBasis::LegitimateInterest,
            enable_right_to_erasure: true,
        }
    }
}

/// Legal basis for processing
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum LegalBasis {
    Consent,
    Contract,
    LegalObligation,
    VitalInterests,
    PublicTask,
    LegitimateInterest,
}
```

#### PCI DSS (Payment Card Industry Data Security Standard)

```rust
/// PCI DSS compliance policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PciDssPolicy {
    /// Enable cardholder data protection
    pub cardholder_data_protection: bool,

    /// Require encryption at rest
    pub require_encryption_at_rest: bool,

    /// Require encryption in transit
    pub require_encryption_in_transit: bool,

    /// Require access logging
    pub require_access_logging: bool,

    /// Enable vulnerability scanning integration
    pub enable_vulnerability_scanning: bool,

    /// Require multi-factor authentication for admin access
    pub require_mfa_for_admin: bool,
}

impl Default for PciDssPolicy {
    fn default() -> Self {
        Self {
            cardholder_data_protection: true,
            require_encryption_at_rest: true,
            require_encryption_in_transit: true,
            require_access_logging: true,
            enable_vulnerability_scanning: true,
            require_mfa_for_admin: true,
        }
    }
}
```

### 2. Enforceable Controls

#### Log/Audit Redaction Rules

```rust
/// Enforceable control: Audit redaction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditRedactionControl {
    /// Require redaction of sensitive data in audit logs
    pub required: bool,

    /// Fields that must be redacted
    pub required_redactions: Vec<String>,

    /// Pattern-based redactions
    pub pattern_redactions: Vec<PatternRedactionRule>,

    /// Action on violation
    pub violation_action: ViolationAction,
}

impl Default for AuditRedactionControl {
    fn default() -> Self {
        Self {
            required: true,
            required_redactions: vec![
                "password".to_string(),
                "secret".to_string(),
                "token".to_string(),
                "api_key".to_string(),
                "credit_card".to_string(),
                "ssn".to_string(),
            ],
            pattern_redactions: vec![
                PatternRedactionRule {
                    pattern: r"\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b".to_string(),
                    replacement: "[CARD_NUMBER_REDACTED]".to_string(),
                    description: "Credit card numbers".to_string(),
                },
            ],
            violation_action: ViolationAction::Block,
        }
    }
}

/// Action on policy violation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ViolationAction {
    /// Allow operation with warning
    Warn,

    /// Block operation
    Block,

    /// Allow operation but log violation
    Log,
}
```

#### Deterministic Output Requirements

```rust
/// Enforceable control: Deterministic output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeterministicOutputControl {
    /// Require deterministic output for validation
    pub required: bool,

    /// Seed for deterministic operations
    pub seed: Option<u64>,

    /// Validate output determinism
    pub validate_determinism: bool,

    /// Action on non-deterministic output
    pub violation_action: ViolationAction,
}

impl Default for DeterministicOutputControl {
    fn default() -> Self {
        Self {
            required: true,
            seed: None,
            validate_determinism: true,
            violation_action: ViolationAction::Block,
        }
    }
}
```

#### Input Data Emission Restrictions

```rust
/// Enforceable control: Input data emission
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputDataEmissionControl {
    /// Restrict emitting input bytes in error messages
    pub restrict_input_emission: bool,

    /// Maximum bytes to emit in error messages
    pub max_error_bytes: usize,

    /// Allow partial emission with truncation
    pub allow_partial_emission: bool,

    /// Action on violation
    pub violation_action: ViolationAction,
}

impl Default for InputDataEmissionControl {
    fn default() -> Self {
        Self {
            restrict_input_emission: true,
            max_error_bytes: 64,
            allow_partial_emission: true,
            violation_action: ViolationAction::Block,
        }
    }
}
```

#### Configuration Hardening

```rust
/// Enforceable control: Configuration hardening
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigurationHardeningControl {
    /// Reject unsafe configurations
    pub reject_unsafe_configs: bool,

    /// Minimum encryption standard
    pub min_encryption_standard: EncryptionStandard,

    /// Require audit trail for config changes
    pub require_config_audit: bool,

    /// Require approval for config changes
    pub require_config_approval: bool,

    /// Action on unsafe config
    pub violation_action: ViolationAction,
}

impl Default for ConfigurationHardeningControl {
    fn default() -> Self {
        Self {
            reject_unsafe_configs: true,
            min_encryption_standard: EncryptionStandard::AES256,
            require_config_audit: true,
            require_config_approval: false,  // Optional for most deployments
            violation_action: ViolationAction::Block,
        }
    }
}

/// Encryption standards
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum EncryptionStandard {
    None,
    AES128,
    AES256,
    ChaCha20,
}
```

### 3. Implementation Shape

#### Policy Structure

```rust
/// Comprehensive compliance policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policy {
    /// Policy identifier
    pub policy_id: String,

    /// Policy name
    pub name: String,

    /// Policy version
    pub version: String,

    /// SOX policy
    pub sox: Option<SoxPolicy>,

    /// HIPAA policy
    pub hipaa: Option<HipaaPolicy>,

    /// GDPR policy
    pub gdpr: Option<GdprPolicy>,

    /// PCI DSS policy
    pub pci_dss: Option<PciDssPolicy>,

    /// Audit redaction control
    pub audit_redaction: AuditRedactionControl,

    /// Deterministic output control
    pub deterministic_output: DeterministicOutputControl,

    /// Input data emission control
    pub input_data_emission: InputDataEmissionControl,

    /// Configuration hardening control
    pub config_hardening: ConfigurationHardeningControl,

    /// Policy metadata
    pub metadata: PolicyMetadata,
}

/// Policy metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyMetadata {
    /// Policy owner
    pub owner: String,

    /// Creation timestamp
    pub created_at: String,

    /// Last modified timestamp
    pub modified_at: String,

    /// Effective date
    pub effective_date: String,

    /// Expiration date (optional)
    pub expiration_date: Option<String>,
}
```

#### PolicySet Presets

```rust
/// Pre-configured policy sets
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum PolicySet {
    /// SOX compliance preset
    Sox,

    /// HIPAA compliance preset
    Hipaa,

    /// GDPR compliance preset
    Gdpr,

    /// PCI DSS compliance preset
    PciDss,

    /// Combined SOX + HIPAA
    SoxHipaa,

    /// Combined SOX + PCI DSS
    SoxPciDss,

    /// Combined HIPAA + GDPR
    HipaaGdpr,

    /// All frameworks
    All,
}

impl PolicySet {
    /// Create policy from preset
    pub fn to_policy(self) -> Policy {
        match self {
            Self::Sox => Policy {
                policy_id: "policy-sox-v1".to_string(),
                name: "SOX Compliance Policy".to_string(),
                version: "1.0.0".to_string(),
                sox: Some(SoxPolicy::default()),
                hipaa: None,
                gdpr: None,
                pci_dss: None,
                audit_redaction: AuditRedactionControl::default(),
                deterministic_output: DeterministicOutputControl::default(),
                input_data_emission: InputDataEmissionControl::default(),
                config_hardening: ConfigurationHardeningControl::default(),
                metadata: PolicyMetadata::default(),
            },
            Self::Hipaa => Policy {
                policy_id: "policy-hipaa-v1".to_string(),
                name: "HIPAA Compliance Policy".to_string(),
                version: "1.0.0".to_string(),
                sox: None,
                hipaa: Some(HipaaPolicy::default()),
                gdpr: None,
                pci_dss: None,
                audit_redaction: AuditRedactionControl::default(),
                deterministic_output: DeterministicOutputControl::default(),
                input_data_emission: InputDataEmissionControl::default(),
                config_hardening: ConfigurationHardeningControl::default(),
                metadata: PolicyMetadata::default(),
            },
            Self::Gdpr => Policy {
                policy_id: "policy-gdpr-v1".to_string(),
                name: "GDPR Compliance Policy".to_string(),
                version: "1.0.0".to_string(),
                sox: None,
                hipaa: None,
                gdpr: Some(GdprPolicy::default()),
                pci_dss: None,
                audit_redaction: AuditRedactionControl::default(),
                deterministic_output: DeterministicOutputControl::default(),
                input_data_emission: InputDataEmissionControl::default(),
                config_hardening: ConfigurationHardeningControl::default(),
                metadata: PolicyMetadata::default(),
            },
            Self::PciDss => Policy {
                policy_id: "policy-pcidss-v1".to_string(),
                name: "PCI DSS Compliance Policy".to_string(),
                version: "1.0.0".to_string(),
                sox: None,
                hipaa: None,
                gdpr: None,
                pci_dss: Some(PciDssPolicy::default()),
                audit_redaction: AuditRedactionControl::default(),
                deterministic_output: DeterministicOutputControl::default(),
                input_data_emission: InputDataEmissionControl::default(),
                config_hardening: ConfigurationHardeningControl::default(),
                metadata: PolicyMetadata::default(),
            },
            Self::SoxHipaa => {
                let mut policy = Self::Sox.to_policy();
                policy.hipaa = Some(HipaaPolicy::default());
                policy.policy_id = "policy-sox-hipaa-v1".to_string();
                policy.name = "SOX + HIPAA Compliance Policy".to_string();
                policy
            }
            Self::SoxPciDss => {
                let mut policy = Self::Sox.to_policy();
                policy.pci_dss = Some(PciDssPolicy::default());
                policy.policy_id = "policy-sox-pcidss-v1".to_string();
                policy.name = "SOX + PCI DSS Compliance Policy".to_string();
                policy
            }
            Self::HipaaGdpr => {
                let mut policy = Self::Hipaa.to_policy();
                policy.gdpr = Some(GdprPolicy::default());
                policy.policy_id = "policy-hipaa-gdpr-v1".to_string();
                policy.name = "HIPAA + GDPR Compliance Policy".to_string();
                policy
            }
            Self::All => {
                let mut policy = Self::Sox.to_policy();
                policy.hipaa = Some(HipaaPolicy::default());
                policy.gdpr = Some(GdprPolicy::default());
                policy.pci_dss = Some(PciDssPolicy::default());
                policy.policy_id = "policy-all-v1".to_string();
                policy.name = "All Compliance Frameworks Policy".to_string();
                policy
            }
        }
    }
}
```

#### Validation Entry Point

```rust
/// Validate configuration against policy
pub fn validate_policy(
    config: &AuditConfig,
    policy: &Policy,
) -> Result<(), ErrorCode> {
    // Validate audit redaction
    if policy.audit_redaction.required {
        for field in &policy.audit_redaction.required_redactions {
            if !config.redaction.redact_fields.contains(field) {
                return Err(ErrorCode::CBKA001_BASELINE_ERROR);
            }
        }
    }

    // Validate deterministic output
    if policy.deterministic_output.required {
        // Check if determinism is enabled
        // Implementation depends on config structure
    }

    // Validate input data emission
    if policy.input_data_emission.restrict_input_emission {
        // Check if error messages are properly redacted
    }

    // Validate configuration hardening
    if policy.config_hardening.reject_unsafe_configs {
        // Check for unsafe configurations
    }

    Ok(())
}
```

---

## Security Monitoring Contract

### Overview

Security monitoring extends the existing telemetry system with low-cardinality counters and structured error events. The focus is on adding security-relevant signals without introducing high cardinality metrics.

### 1. Low-Cardinality Counters

#### Error Code Frequencies

```rust
/// Error code frequency counter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorFrequencyCounter {
    /// Error code family (e.g., CBKP, CBKS, CBKR)
    pub family: String,

    /// Specific error code
    pub code: String,

    /// Count of occurrences
    pub count: u64,

    /// First occurrence timestamp
    pub first_seen: String,

    /// Last occurrence timestamp
    pub last_seen: String,
}

/// Error frequency metrics
pub struct ErrorFrequencyMetrics {
    /// Error frequencies by family
    pub by_family: HashMap<String, u64>,

    /// Error frequencies by specific code
    pub by_code: HashMap<String, ErrorFrequencyCounter>,

    /// Total error count
    pub total_errors: u64,

    /// Errors in last hour
    pub errors_last_hour: u64,

    /// Errors in last 24 hours
    pub errors_last_day: u64,
}
```

#### Decode Failures by Class

```rust
/// Decode failure classification
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum DecodeFailureClass {
    /// Invalid record format
    InvalidRecordFormat,

    /// Data validation failure
    DataValidationFailure,

    /// Encoding conversion failure
    EncodingConversionFailure,

    /// ODO bounds violation
    OdoBoundsViolation,

    /// REDEFINES mismatch
    RedefinesMismatch,

    /// Level-88 validation failure
    Level88ValidationFailure,

    /// Memory allocation failure
    MemoryAllocationFailure,

    /// Other failure
    Other,
}

/// Decode failure counter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecodeFailureCounter {
    /// Failure class
    pub class: DecodeFailureClass,

    /// Count of occurrences
    pub count: u64,

    /// First occurrence timestamp
    pub first_seen: String,

    /// Last occurrence timestamp
    pub last_seen: String,
}

/// Decode failure metrics
pub struct DecodeFailureMetrics {
    /// Failure counts by class
    pub by_class: HashMap<DecodeFailureClass, DecodeFailureCounter>,

    /// Total decode failures
    pub total_failures: u64,

    /// Failures in last hour
    pub failures_last_hour: u64,

    /// Failures in last 24 hours
    pub failures_last_day: u64,

    /// Failure rate (failures per 1000 records)
    pub failure_rate: f64,
}
```

#### Feature Flag State

```rust
/// Feature flag state counter (low cardinality - only track enabled/disabled)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeatureFlagStateCounter {
    /// Feature flag name
    pub flag_name: String,

    /// Flag state (enabled/disabled)
    pub enabled: bool,

    /// Count of operations with this flag state
    pub count: u64,

    /// First timestamp with this state
    pub first_seen: String,

    /// Last timestamp with this state
    pub last_seen: String,
}
```

### 2. Metric Schema

#### Naming Convention

```rust
/// Metric naming convention
///
/// Format: copybook_<category>_<metric>_<unit>
///
/// Examples:
/// - copybook_decode_records_total
/// - copybook_decode_bytes_total
/// - copybook_decode_errors_total
/// - copybook_decode_seconds
/// - copybook_throughput_mibps
///
/// Labels (low cardinality only):
/// - family: Error family (CBKP, CBKS, CBKR, etc.)
/// - operation: Operation type (parse, decode, encode)
/// - status: Operation status (success, failure)
///
/// High cardinality labels are NOT allowed:
/// - NO: copybook_decode_bytes_total{file_name="..."}
/// - NO: copybook_decode_errors_total{error_message="..."}
/// - NO: copybook_decode_records_total{copybook_path="..."}
```

#### Cardinality Guards

```rust
/// Cardinality guard configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CardinalityGuardConfig {
    /// Maximum unique label values per metric
    pub max_label_values_per_metric: usize,

    /// Maximum total unique label combinations
    pub max_total_label_combinations: usize,

    /// Action when cardinality limit exceeded
    pub overflow_action: CardinalityOverflowAction,

    /// Metrics to monitor for cardinality
    pub monitored_metrics: Vec<String>,
}

impl Default for CardinalityGuardConfig {
    fn default() -> Self {
        Self {
            max_label_values_per_metric: 50,
            max_total_label_combinations: 1000,
            overflow_action: CardinalityOverflowAction::Drop,
            monitored_metrics: vec![
                "copybook_decode_errors_total".to_string(),
                "copybook_decode_records_total".to_string(),
            ],
        }
    }
}

/// Action when cardinality limit exceeded
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum CardinalityOverflowAction {
    /// Drop new metrics beyond limit
    Drop,

    /// Aggregate new metrics into "other" bucket
    Aggregate,

    /// Log warning and drop
    WarnAndDrop,

    /// Panic (for development/testing only)
    Panic,
}
```

#### Label Restrictions

```rust
/// Allowed label values (whitelist)
pub const ALLOWED_ERROR_FAMILY_LABELS: &[&str] = &[
    "CBKP",  // Parse errors
    "CBKS",  // Schema errors
    "CBKR",  // Record format errors
    "CBKC",  // Character conversion errors
    "CBKD",  // Data decoding errors
    "CBKE",  // Encoding errors
    "CBKF",  // File format errors
    "CBKI",  // Infrastructure errors
    "CBKA",  // Audit errors
];

pub const ALLOWED_OPERATION_LABELS: &[&str] = &[
    "parse",
    "decode",
    "encode",
    "validate",
    "transform",
];

pub const ALLOWED_STATUS_LABELS: &[&str] = &[
    "success",
    "failure",
    "partial",
];
```

### 3. Structured Error Events

#### Error Event Schema

```rust
/// Structured error event for security monitoring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityErrorEvent {
    /// Unique event identifier
    pub event_id: String,

    /// Timestamp
    pub timestamp: String,

    /// Error code
    pub error_code: String,

    /// Error family
    pub error_family: String,

    /// Error message (redacted)
    pub error_message: String,

    /// Operation that failed
    pub operation: String,

    /// Error severity
    pub severity: ErrorSeverity,

    /// User impact level
    pub user_impact: UserImpactLevel,

    /// Whether error is security-relevant
    pub security_relevant: bool,

    /// Error context (low cardinality)
    pub context: ErrorContext,

    /// Recovery action taken
    pub recovery_action: Option<String>,
}

/// Error context (low cardinality only)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorContext {
    /// Copybook name (if applicable)
    pub copybook_name: Option<String>,

    /// Record format (if applicable)
    pub record_format: Option<String>,

    /// Codepage (if applicable)
    pub codepage: Option<String>,

    /// Error location (module/function)
    pub location: String,
}
```

#### Error Severity Levels

```rust
/// Error severity for security monitoring
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorSeverity {
    /// Informational (no impact)
    Info,

    /// Low (minor impact, no data loss)
    Low,

    /// Medium (some impact, possible data loss)
    Medium,

    /// High (significant impact, data loss likely)
    High,

    /// Critical (system-wide impact, security breach)
    Critical,
}

/// User impact level
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum UserImpactLevel {
    /// No impact to users
    None,

    /// Low impact (minor inconvenience)
    Low,

    /// Medium impact (some users affected)
    Medium,

    /// High impact (many users affected)
    High,
}
```

#### Security-Relevant Error Detection

```rust
/// Security-relevant error detection
pub fn is_security_relevant(error_code: &ErrorCode) -> bool {
    matches!(
        error_code,
        // Audit errors
        ErrorCode::CBKA001_BASELINE_ERROR
        // Schema errors (potential injection)
        | ErrorCode::CBKP001_SYNTAX
        | ErrorCode::CBKP011_UNSUPPORTED_CLAUSE
        // Data validation errors (potential data corruption)
        | ErrorCode::CBKD001_INVALID_DATA
        // Access control errors
        | ErrorCode::CBKI001_INVALID_STATE
    )
}
```

---

## Feature Flag Governance

### Overview

Feature flags provide runtime control over enterprise features. Each flag has defined ownership, default states, kill-switch behavior, and cleanup milestones.

### 1. Audit System

| Property | Value |
|-----------|-------|
| **Flag Key** | `audit_system` |
| **Owner** | Platform Team |
| **Default (dev)** | Disabled |
| **Default (CI)** | Disabled |
| **Default (release)** | Disabled |
| **Kill-switch behavior** | Graceful degradation to NoopAuditSink |
| **Cleanup milestone** | v1.2.0 (promote to always-on) |
| **Dependencies** | None |

### 2. SOX Compliance

| Property | Value |
|-----------|-------|
| **Flag Key** | `sox_compliance` |
| **Owner** | Compliance Team |
| **Default (dev)** | Disabled |
| **Default (CI)** | Disabled |
| **Default (release)** | Disabled |
| **Kill-switch behavior** | Skip SOX validation, emit warning |
| **Cleanup milestone** | v1.5.0 (promote to always-on for financial customers) |
| **Dependencies** | `audit_system` |

### 3. HIPAA Compliance

| Property | Value |
|-----------|-------|
| **Flag Key** | `hipaa_compliance` |
| **Owner** | Compliance Team |
| **Default (dev)** | Disabled |
| **Default (CI)** | Disabled |
| **Default (release)** | Disabled |
| **Kill-switch behavior** | Skip HIPAA validation, emit warning |
| **Cleanup milestone** | v1.5.0 (promote to always-on for healthcare customers) |
| **Dependencies** | `audit_system` |

### 4. GDPR Compliance

| Property | Value |
|-----------|-------|
| **Flag Key** | `gdpr_compliance` |
| **Owner** | Compliance Team |
| **Default (dev)** | Disabled |
| **Default (CI)** | Disabled |
| **Default (release)** | Disabled |
| **Kill-switch behavior** | Skip GDPR validation, emit warning |
| **Cleanup milestone** | v1.5.0 (promote to always-on for EU customers) |
| **Dependencies** | `audit_system` |

### 5. PCI DSS Compliance

| Property | Value |
|-----------|-------|
| **Flag Key** | `pci_dss_compliance` |
| **Owner** | Compliance Team |
| **Default (dev)** | Disabled |
| **Default (CI)** | Disabled |
| **Default (release)** | Disabled |
| **Kill-switch behavior** | Skip PCI DSS validation, emit warning |
| **Cleanup milestone** | v1.5.0 (promote to always-on for payment customers) |
| **Dependencies** | `audit_system` |

### 6. Security Monitoring

| Property | Value |
|-----------|-------|
| **Flag Key** | `security_monitoring` |
| **Owner** | Security Team |
| **Default (dev)** | Disabled |
| **Default (CI)** | Disabled |
| **Default (release)** | Disabled |
| **Kill-switch behavior** | Stop emitting security metrics, continue normal operation |
| **Cleanup milestone** | v1.3.0 (promote to always-on) |
| **Dependencies** | `audit_system` |

---

## Test Integration Requirements

### 1. Behavior Driven Development (BDD)

#### Required BDD Scenarios

```gherkin
Feature: Audit System
  Scenario: Audit event is emitted for copybook parse
    Given audit system is enabled
    And copybook file "test.cpy" exists
    When copybook is parsed
    Then an audit event of type "CopybookParse" is emitted
    And the event contains the copybook path
    And the event contains the parse result

  Scenario: Audit event is redacted according to policy
    Given audit system is enabled
    And redaction policy requires redaction of "password" fields
    When an audit event contains a "password" field
    Then the "password" field is replaced with "[REDACTED]"

  Scenario: Audit system gracefully degrades when disabled
    Given audit system is disabled
    When copybook is parsed
    Then parsing completes successfully
    And no audit event is emitted
```

### 2. Property Testing

#### Required Property Tests

```rust
#[proptest]
fn audit_event_integrity_hash_is_deterministic(
    event_type in any::<AuditEventType>(),
    payload in any::<AuditPayload>(),
) {
    let context = AuditContext::new();
    let event1 = AuditEvent::new(event_type, context.clone(), payload.clone());
    let event2 = AuditEvent::new(event_type, context, payload);

    prop_assert_eq!(event1.integrity_hash, event2.integrity_hash);
}

#[proptest]
fn audit_chain_validation_preserves_integrity(
    events in prop::collection::vec(
        any::<AuditEvent>(),
        1..100
    ),
) {
    // Build chain with proper previous_hash links
    let chained_events = build_audit_chain(events);

    prop_assert!(validate_audit_chain(&chained_events).is_ok());
}
```

### 3. Fuzzing

#### Required Fuzz Targets

```rust
// Fuzz audit event parsing
fuzz_target!(|data: &[u8]| {
    if let Ok(event_json) = std::str::from_utf8(data) {
        let _ = serde_json::from_str::<AuditEvent>(event_json);
    }
});

// Fuzz redaction policy application
fuzz_target!(|data: &[u8]| {
    if let Ok(payload_json) = std::str::from_utf8(data) {
        if let Ok(payload) = serde_json::from_str::<AuditPayload>(payload_json) {
            let policy = RedactionPolicy::default();
            let _ = apply_redaction(&payload, &policy);
        }
    }
});
```

### 4. Mutation Testing

#### Required Mutation Thresholds

| Module | Threshold | Rationale |
|--------|-----------|-----------|
| Audit event creation | 80% | Critical for audit integrity |
| Redaction logic | 90% | Critical for data protection |
| Compliance validation | 75% | Important for regulatory compliance |
| Security monitoring | 75% | Important for threat detection |

---

## Performance Impact Assessment

### 1. Audit System Overhead

| Operation | Baseline (no audit) | With Audit (async) | With Audit (blocking) |
|-----------|---------------------|-------------------|----------------------|
| Copybook parse | 100% | 101-102% | 105-110% |
| Decode (1M records) | 100% | 102-105% | 110-120% |
| Encode (1M records) | 100% | 102-105% | 110-120% |

**Target**: < 5% overhead with async emission, < 10% with blocking emission.

### 2. Compliance Validation Overhead

| Operation | Baseline (no compliance) | With SOX | With HIPAA | With GDPR | With PCI DSS |
|-----------|---------------------------|----------|------------|-----------|--------------|
| Copybook parse | 100% | 101% | 101% | 101% | 101% |
| Decode (1M records) | 100% | 101% | 101% | 101% | 101% |

**Target**: < 2% overhead per compliance framework.

### 3. Security Monitoring Overhead

| Operation | Baseline (no monitoring) | With Monitoring |
|-----------|---------------------------|-----------------|
| Decode (1M records) | 100% | 101-102% |

**Target**: < 2% overhead.

### 4. Memory Impact

| Component | Baseline | With Enterprise Features |
|-----------|----------|------------------------|
| Binary size | 10 MB | 12 MB (+20%) |
| Runtime memory | 50 MB | 55 MB (+10%) |

**Target**: < 25% increase in binary size, < 15% increase in runtime memory.

---

## Implementation Recommendations

### Phase 2.2: Audit System Implementation

1. Implement `AuditSink` trait and `NoopAuditSink`
2. Implement `AuditConfig` with feature flag integration
3. Implement redaction policy with default rules
4. Implement CLI integration for audit flags
5. Add BDD scenarios for audit emission
6. Add property tests for event integrity
7. Add fuzz targets for event parsing

### Phase 2.3: Compliance Validation Implementation

1. Implement `Policy` and `PolicySet` structures
2. Implement SOX validator with enforceable controls
3. Implement HIPAA validator with enforceable controls
4. Implement GDPR validator with enforceable controls
5. Implement PCI DSS validator with enforceable controls
6. Add BDD scenarios for compliance validation
7. Add property tests for policy enforcement

### Phase 2.4: Security Monitoring Implementation

1. Implement error frequency counters
2. Implement decode failure classification
3. Implement low-cardinality metric guards
4. Implement structured error events
5. Add security-relevant error detection
6. Add BDD scenarios for security monitoring
7. Add fuzz targets for error event parsing

### Phase 2.5: Feature Flag Integration

1. Implement feature flag defaults for enterprise features
2. Implement kill-switch behavior for each feature
3. Add feature flag validation on startup
4. Add feature flag documentation
5. Add feature flag tests

---

## Appendix: Error Code Extensions

### New Audit Error Codes

| Code | Description | Severity |
|------|-------------|----------|
| `CBKA001_BASELINE_ERROR` | Audit baseline error | High |
| `CBKA002_REDACATION_FAILURE` | Redaction policy failure | High |
| `CBKA003_INTEGRITY_VIOLATION` | Audit trail integrity violation | Critical |
| `CBKA004_SINK_FAILURE` | Audit sink write failure | Medium |
| `CBKA005_CARDINALITY_EXCEEDED` | Metric cardinality limit exceeded | Low |

---

**Document Status**: Draft - Pending Review
**Next Review**: After Phase 2.2 Implementation
**Approval Required**: Platform Team, Compliance Team, Security Team
