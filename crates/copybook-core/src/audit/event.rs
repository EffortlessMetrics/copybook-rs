// SPDX-License-Identifier: AGPL-3.0-or-later
//! Audit Event System
//!
//! Defines the core audit event structure and payload types for comprehensive
//! enterprise audit trail generation and validation.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::{AuditContext, generate_audit_id, generate_integrity_hash};

/// Core audit event structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEvent {
    /// Unique audit event identifier
    pub event_id: String,

    /// Event timestamp (ISO 8601 with nanosecond precision)
    pub timestamp: String,

    /// Event type classification
    pub event_type: AuditEventType,

    /// Operation context information
    pub context: AuditContext,

    /// Audit event payload data
    pub payload: AuditPayload,

    /// Cryptographic integrity hash
    pub integrity_hash: String,

    /// Previous event hash for chain integrity
    pub previous_hash: Option<String>,

    /// Event severity level
    pub severity: AuditSeverity,

    /// Event source component
    pub source: String,

    /// Event version for schema evolution
    pub event_version: String,
}

impl AuditEvent {
    /// Create a new audit event
    pub fn new(event_type: AuditEventType, context: AuditContext, payload: AuditPayload) -> Self {
        let event_id = generate_audit_id();
        let timestamp = chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Nanos, true);
        let severity = payload.default_severity();
        let source = Self::determine_source(event_type);

        let mut event = Self {
            event_id,
            timestamp,
            event_type,
            context,
            payload,
            integrity_hash: String::new(), // Will be set below
            previous_hash: None,           // Will be set by audit logger
            severity,
            source,
            event_version: "1.0.0".to_string(),
        };

        // Generate integrity hash (exclude hash field from calculation)
        let mut event_for_hash = event.clone();
        event_for_hash.integrity_hash = String::new();
        let event_bytes = match serde_json::to_vec(&event_for_hash) {
            Ok(bytes) => bytes,
            Err(e) => {
                // Fallback to a minimal serializable representation on serialization failure
                let fallback_data = format!(
                    "{{\"event_id\":\"{}\",\"timestamp\":\"{}\",\"event_type\":\"{:?}\",\"serialization_error\":\"{}\"}}",
                    event.event_id, event.timestamp, event.event_type, e
                );
                fallback_data.into_bytes()
            }
        };
        event.integrity_hash = generate_integrity_hash(&event_bytes, None);

        event
    }

    /// Create event with specific severity
    #[must_use]
    pub fn with_severity(mut self, severity: AuditSeverity) -> Self {
        self.severity = severity;

        // Regenerate integrity hash with updated severity (exclude hash field from calculation)
        let mut event_for_hash = self.clone();
        event_for_hash.integrity_hash = String::new();
        let event_bytes = match serde_json::to_vec(&event_for_hash) {
            Ok(bytes) => bytes,
            Err(e) => {
                // Fallback to a minimal serializable representation on serialization failure
                let fallback_data = format!(
                    "{{\"event_id\":\"{}\",\"timestamp\":\"{}\",\"event_type\":\"{:?}\",\"severity\":\"{:?}\",\"serialization_error\":\"{}\"}}",
                    self.event_id, self.timestamp, self.event_type, self.severity, e
                );
                fallback_data.into_bytes()
            }
        };
        self.integrity_hash = generate_integrity_hash(&event_bytes, None);

        self
    }

    /// Set the previous event hash for chain integrity
    #[must_use]
    pub fn with_previous_hash(mut self, previous_hash: String) -> Self {
        self.previous_hash = Some(previous_hash);

        // Regenerate integrity hash with previous hash (exclude hash field from calculation)
        let mut event_for_hash = self.clone();
        event_for_hash.integrity_hash = String::new();
        event_for_hash.previous_hash = None; // Also exclude previous_hash from serialization
        let event_bytes = match serde_json::to_vec(&event_for_hash) {
            Ok(bytes) => bytes,
            Err(e) => {
                // Fallback to a minimal serializable representation on serialization failure
                let fallback_data = format!(
                    "{{\"event_id\":\"{}\",\"timestamp\":\"{}\",\"event_type\":\"{:?}\",\"serialization_error\":\"{}\"}}",
                    self.event_id, self.timestamp, self.event_type, e
                );
                fallback_data.into_bytes()
            }
        };
        self.integrity_hash = generate_integrity_hash(&event_bytes, self.previous_hash.as_deref());

        self
    }

    /// Determine the source component based on event type
    fn determine_source(event_type: AuditEventType) -> String {
        match event_type {
            AuditEventType::CopybookParse => "copybook-core::parser",
            AuditEventType::DataValidation => "copybook-codec::validator",
            AuditEventType::DataTransformation => "copybook-codec::transformer",
            AuditEventType::PerformanceMeasurement => "copybook-bench::performance",
            AuditEventType::ComplianceCheck => "copybook-core::compliance",
            AuditEventType::SecurityEvent => "copybook-core::security",
            AuditEventType::LineageTracking => "copybook-core::lineage",
            AuditEventType::ErrorEvent => "copybook-core::error",
            AuditEventType::AccessEvent => "copybook-core::access",
            AuditEventType::ConfigurationChange => "copybook-core::config",
        }
        .to_string()
    }

    /// Check if this event requires immediate attention
    pub fn requires_immediate_attention(&self) -> bool {
        matches!(self.severity, AuditSeverity::High | AuditSeverity::Critical)
    }

    /// Get event correlation ID from context
    pub fn correlation_id(&self) -> &str {
        &self.context.operation_id
    }
}

/// Audit event type classification
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

/// Audit event severity levels
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum AuditSeverity {
    /// Informational event with no action required
    Info,
    /// Low-severity event requiring awareness
    Low,
    /// Medium-severity event requiring investigation
    Medium,
    /// High-severity event requiring prompt attention
    High,
    /// Critical event requiring immediate response
    Critical,
}

impl std::fmt::Display for AuditEventType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CopybookParse => f.write_str("CopybookParse"),
            Self::DataValidation => f.write_str("DataValidation"),
            Self::DataTransformation => f.write_str("DataTransformation"),
            Self::PerformanceMeasurement => f.write_str("PerformanceMeasurement"),
            Self::ComplianceCheck => f.write_str("ComplianceCheck"),
            Self::SecurityEvent => f.write_str("SecurityEvent"),
            Self::LineageTracking => f.write_str("LineageTracking"),
            Self::ErrorEvent => f.write_str("ErrorEvent"),
            Self::AccessEvent => f.write_str("AccessEvent"),
            Self::ConfigurationChange => f.write_str("ConfigurationChange"),
        }
    }
}

impl std::fmt::Display for AuditSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Info => f.write_str("Info"),
            Self::Low => f.write_str("Low"),
            Self::Medium => f.write_str("Medium"),
            Self::High => f.write_str("High"),
            Self::Critical => f.write_str("Critical"),
        }
    }
}

impl AuditSeverity {
    /// Return the CEF-spec numeric severity (0-10 scale).
    #[must_use]
    pub fn cef_numeric(self) -> u8 {
        match self {
            Self::Info => 1,
            Self::Low => 3,
            Self::Medium => 5,
            Self::High => 7,
            Self::Critical => 10,
        }
    }
}

/// Audit event payload containing event-specific data
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum AuditPayload {
    /// Copybook parsing operation payload
    CopybookParse {
        /// File path of the parsed copybook
        copybook_path: String,
        /// SHA-based fingerprint of the parsed schema
        schema_fingerprint: String,
        /// Outcome of the parse operation
        parse_result: ParseResult,
        /// Wall-clock parsing duration in milliseconds
        parsing_duration_ms: u64,
        /// Total number of fields parsed
        field_count: usize,
        /// Number of Level-88 condition fields parsed
        level_88_count: usize,
        /// Number of errors encountered during parsing
        error_count: usize,
        /// Non-fatal warning messages emitted during parsing
        warnings: Vec<String>,
    },

    /// Data validation operation payload
    DataValidation {
        /// Path of the input file being validated
        input_file: String,
        /// Outcome of the validation operation
        validation_result: ValidationResult,
        /// Wall-clock validation duration in milliseconds
        validation_duration_ms: u64,
        /// Total number of records validated
        records_validated: u64,
        /// Number of validation errors found
        errors_found: u64,
        /// Detailed list of validation errors
        error_details: Vec<ValidationError>,
        /// Names of the validation rules applied
        validation_rules: Vec<String>,
    },

    /// Data transformation (decode/encode) operation payload
    DataTransformation {
        /// Type of transformation operation performed
        operation: TransformationOperation,
        /// Path of the input file
        input_file: String,
        /// Path of the output file
        output_file: String,
        /// Outcome of the transformation operation
        transformation_result: TransformationResult,
        /// Wall-clock processing duration in milliseconds
        processing_duration_ms: u64,
        /// Total number of records processed
        records_processed: u64,
        /// Total number of bytes processed
        bytes_processed: u64,
        /// Measured throughput in bytes per second
        throughput_bytes_per_sec: u64,
        /// Peak memory usage in megabytes
        memory_usage_mb: u64,
    },

    /// Performance measurement event payload
    PerformanceMeasurement {
        /// Category of the performance measurement
        measurement_type: PerformanceMeasurementType,
        /// Identifier of the baseline used for comparison
        baseline_id: Option<String>,
        /// Collected performance metrics
        metrics: PerformanceMetrics,
        /// Result of comparing metrics against the baseline
        comparison_result: Option<ComparisonResult>,
        /// Whether a performance regression was detected
        regression_detected: bool,
    },

    /// Compliance check operation payload
    ComplianceCheck {
        /// Name of the compliance framework evaluated
        compliance_framework: String,
        /// Outcome of the compliance validation
        validation_result: ComplianceValidationResult,
        /// Detailed list of compliance violations found
        violations: Vec<ComplianceViolationDetail>,
        /// Whether remediation actions are required
        remediation_required: bool,
        /// Scheduled date for the next compliance review
        next_review_date: Option<String>,
    },

    /// Security-relevant event payload
    SecurityEvent {
        /// Classification of the security event
        security_event_type: SecurityEventType,
        /// Severity level of the security event
        severity: String,
        /// Resources affected by the security event
        affected_resources: Vec<String>,
        /// Indicators of the detected threat
        threat_indicators: Vec<String>,
        /// Recommended remediation actions
        remediation_actions: Vec<String>,
        /// Associated incident tracking identifier
        incident_id: Option<String>,
    },

    /// Data lineage tracking payload
    LineageTracking {
        /// Originating system of the data
        source_system: String,
        /// Destination system for the data
        target_system: String,
        /// Field-level source-to-target mappings
        field_mappings: Vec<FieldMapping>,
        /// Transformation rules applied during lineage
        transformation_rules: Vec<TransformationRule>,
        /// Data quality score (0.0 to 1.0)
        quality_score: f64,
        /// Summary of downstream impact assessment
        impact_assessment: Option<ImpactAssessmentSummary>,
    },

    /// Error and exception event payload
    ErrorEvent {
        /// Structured error code identifier
        error_code: String,
        /// Human-readable error description
        error_message: String,
        /// Category grouping for the error
        error_category: String,
        /// Optional stack trace for debugging
        stack_trace: Option<String>,
        /// Additional key-value context for the error
        context_information: HashMap<String, String>,
        /// Suggested recovery actions
        recovery_actions: Vec<String>,
        /// Assessed user impact level
        user_impact: UserImpactLevel,
    },

    /// Access control event payload
    AccessEvent {
        /// Type of access attempted
        access_type: AccessType,
        /// Kind of resource being accessed
        resource_type: String,
        /// Identifier of the accessed resource
        resource_id: String,
        /// Outcome of the access attempt
        access_result: AccessResult,
        /// Identifier of the user performing the access
        user_id: String,
        /// IP address of the access origin
        source_ip: Option<String>,
        /// User agent string of the client
        user_agent: Option<String>,
        /// Session identifier for the access
        session_id: Option<String>,
    },

    /// Configuration change event payload
    ConfigurationChange {
        /// Component whose configuration changed
        component: String,
        /// Type of configuration change performed
        change_type: ConfigurationChangeType,
        /// Previous configuration value before the change
        old_configuration: Option<String>,
        /// New configuration value after the change
        new_configuration: String,
        /// Reason or justification for the change
        change_reason: String,
        /// Identifier of the approver, if applicable
        approved_by: Option<String>,
        /// Whether the change can be rolled back
        rollback_available: bool,
    },
}

impl AuditPayload {
    /// Get the default severity for this payload type
    pub fn default_severity(&self) -> AuditSeverity {
        match self {
            AuditPayload::CopybookParse { parse_result, .. } => match parse_result {
                ParseResult::Success => AuditSeverity::Info,
                ParseResult::SuccessWithWarnings => AuditSeverity::Low,
                ParseResult::Failed => AuditSeverity::High,
            },
            AuditPayload::DataValidation {
                validation_result, ..
            } => match validation_result {
                ValidationResult::Valid => AuditSeverity::Info,
                ValidationResult::ValidWithWarnings => AuditSeverity::Low,
                ValidationResult::Invalid => AuditSeverity::Medium,
            },
            AuditPayload::DataTransformation {
                transformation_result,
                ..
            } => match transformation_result {
                TransformationResult::Success => AuditSeverity::Info,
                TransformationResult::PartialSuccess => AuditSeverity::Medium,
                TransformationResult::Failed => AuditSeverity::High,
            },
            AuditPayload::PerformanceMeasurement {
                regression_detected,
                ..
            } => {
                if *regression_detected {
                    AuditSeverity::Medium
                } else {
                    AuditSeverity::Info
                }
            }
            AuditPayload::ComplianceCheck {
                remediation_required,
                ..
            } => {
                if *remediation_required {
                    AuditSeverity::High
                } else {
                    AuditSeverity::Info
                }
            }
            AuditPayload::SecurityEvent { .. } => AuditSeverity::High,
            AuditPayload::LineageTracking { .. } => AuditSeverity::Info,
            AuditPayload::ErrorEvent { user_impact, .. } => match user_impact {
                UserImpactLevel::None => AuditSeverity::Low,
                UserImpactLevel::Low => AuditSeverity::Medium,
                UserImpactLevel::Medium => AuditSeverity::High,
                UserImpactLevel::High => AuditSeverity::Critical,
            },
            AuditPayload::AccessEvent { access_result, .. } => match access_result {
                AccessResult::Success => AuditSeverity::Info,
                AccessResult::Denied => AuditSeverity::Medium,
                AccessResult::Failed => AuditSeverity::High,
            },
            AuditPayload::ConfigurationChange { .. } => AuditSeverity::Medium,
        }
    }
}

// Supporting data structures for audit payloads

/// Outcome of a copybook parse operation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ParseResult {
    /// Parse completed successfully with no issues
    Success,
    /// Parse completed with non-fatal warnings
    SuccessWithWarnings,
    /// Parse failed with errors
    Failed,
}

/// Outcome of a data validation operation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidationResult {
    /// Data is fully valid
    Valid,
    /// Data is valid but has non-fatal warnings
    ValidWithWarnings,
    /// Data is invalid
    Invalid,
}

/// Details of a single validation error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    /// Structured error code identifier
    pub error_code: String,
    /// Dot-separated path to the field with the error
    pub field_path: Option<String>,
    /// Zero-based index of the record containing the error
    pub record_index: Option<u64>,
    /// Byte offset within the record where the error occurred
    pub byte_offset: Option<u64>,
    /// Human-readable error description
    pub message: String,
}

/// Type of data transformation operation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TransformationOperation {
    /// Binary-to-JSON decode operation
    Decode,
    /// JSON-to-binary encode operation
    Encode,
    /// Data validation operation
    Validate,
    /// Format conversion operation
    Convert,
}

/// Outcome of a data transformation operation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TransformationResult {
    /// Transformation completed successfully
    Success,
    /// Transformation completed with some records failing
    PartialSuccess,
    /// Transformation failed entirely
    Failed,
}

/// Category of performance measurement
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PerformanceMeasurementType {
    /// Throughput measurement in bytes per second
    Throughput,
    /// Latency measurement in milliseconds
    Latency,
    /// CPU and memory resource utilization measurement
    ResourceUtilization,
    /// Baseline establishment measurement
    Baseline,
}

/// Collected performance metrics for a measurement event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Measured throughput in bytes per second
    pub throughput_bytes_per_sec: u64,
    /// Measured latency in milliseconds
    pub latency_ms: u64,
    /// CPU usage as a percentage (0.0 to 100.0)
    pub cpu_usage_percent: f64,
    /// Memory usage in megabytes
    pub memory_usage_mb: u64,
    /// Total number of I/O operations performed
    pub io_operations: u64,
}

/// Result of comparing metrics against a performance baseline
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComparisonResult {
    /// Metrics exceed the baseline (improvement)
    BetterThanBaseline,
    /// Metrics are within acceptable baseline tolerance
    WithinBaseline,
    /// Metrics are below the baseline (minor regression)
    BelowBaseline,
    /// Metrics show a significant regression beyond tolerance
    SignificantRegression,
}

/// Outcome of a compliance validation check
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComplianceValidationResult {
    /// Fully compliant with the framework requirements
    Compliant,
    /// Non-compliant with one or more requirements
    NonCompliant,
    /// Requires manual review to determine compliance
    RequiresReview,
}

/// Details of a single compliance violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceViolationDetail {
    /// Unique identifier for the violation
    pub violation_id: String,
    /// Regulation or standard that was violated
    pub regulation: String,
    /// Severity level of the violation
    pub severity: String,
    /// Human-readable description of the violation
    pub description: String,
    /// Recommended remediation steps
    pub remediation: String,
}

/// Classification of a security event
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum SecurityEventType {
    /// Access attempt without valid credentials
    UnauthorizedAccess,
    /// Failed authentication attempt
    AuthenticationFailure,
    /// Denied authorization for a requested operation
    AuthorizationFailure,
    /// Detected or confirmed data breach
    DataBreach,
    /// Data or system integrity violation detected
    IntegrityViolation,
    /// Unauthorized configuration modification detected
    ConfigurationTampering,
    /// Activity matching suspicious behavior patterns
    SuspiciousActivity,
}

/// Mapping between source and target fields in data lineage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldMapping {
    /// Name of the source field
    pub source_field: String,
    /// Name of the target field
    pub target_field: String,
    /// Description of the transformation applied
    pub transformation: String,
    /// Confidence score for the mapping (0.0 to 1.0)
    pub confidence_score: f64,
}

/// Rule applied during a data transformation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformationRule {
    /// Unique identifier for the rule
    pub rule_id: String,
    /// Category or type of the rule
    pub rule_type: String,
    /// Human-readable description of the rule
    pub description: String,
    /// Key-value parameters for the rule
    pub parameters: HashMap<String, String>,
}

/// Summary of a downstream impact assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactAssessmentSummary {
    /// Number of downstream systems affected
    pub affected_systems: u32,
    /// Overall risk level of the impact
    pub risk_level: String,
    /// Description of the estimated impact
    pub estimated_impact: String,
}

/// Level of impact on end users
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum UserImpactLevel {
    /// No user impact
    None,
    /// Minor user impact with workaround available
    Low,
    /// Moderate user impact affecting some workflows
    Medium,
    /// Severe user impact blocking critical workflows
    High,
}

/// Type of resource access attempted
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AccessType {
    /// Read access to a resource
    Read,
    /// Write or modify access to a resource
    Write,
    /// Execute or invoke access to a resource
    Execute,
    /// Delete access to a resource
    Delete,
    /// Administrative access to a resource
    Admin,
}

/// Outcome of a resource access attempt
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AccessResult {
    /// Access was granted successfully
    Success,
    /// Access was denied by policy
    Denied,
    /// Access failed due to an error
    Failed,
}

/// Type of configuration change performed
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConfigurationChangeType {
    /// New configuration created
    Create,
    /// Existing configuration updated
    Update,
    /// Configuration deleted
    Delete,
    /// Configuration imported from external source
    Import,
    /// Configuration exported to external destination
    Export,
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::audit::AuditContext;

    #[test]
    fn test_audit_event_creation() {
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

        let event = AuditEvent::new(AuditEventType::CopybookParse, context, payload);

        assert!(event.event_id.starts_with("audit-"));
        assert!(!event.timestamp.is_empty());
        assert_eq!(event.event_type, AuditEventType::CopybookParse);
        assert_eq!(event.severity, AuditSeverity::Info);
        assert!(!event.integrity_hash.is_empty());
    }

    #[test]
    fn test_event_severity_override() {
        let context = AuditContext::new();
        let payload = AuditPayload::ErrorEvent {
            error_code: "TEST001".to_string(),
            error_message: "Test error".to_string(),
            error_category: "test".to_string(),
            stack_trace: None,
            context_information: HashMap::new(),
            recovery_actions: vec![],
            user_impact: UserImpactLevel::None,
        };

        let event = AuditEvent::new(AuditEventType::ErrorEvent, context, payload)
            .with_severity(AuditSeverity::Critical);

        assert_eq!(event.severity, AuditSeverity::Critical);
    }

    #[test]
    fn test_payload_default_severity() {
        let success_payload = AuditPayload::CopybookParse {
            copybook_path: "test.cpy".to_string(),
            schema_fingerprint: "abc123".to_string(),
            parse_result: ParseResult::Success,
            parsing_duration_ms: 100,
            field_count: 10,
            level_88_count: 2,
            error_count: 0,
            warnings: vec![],
        };

        assert_eq!(success_payload.default_severity(), AuditSeverity::Info);

        let failed_payload = AuditPayload::CopybookParse {
            copybook_path: "test.cpy".to_string(),
            schema_fingerprint: "abc123".to_string(),
            parse_result: ParseResult::Failed,
            parsing_duration_ms: 100,
            field_count: 0,
            level_88_count: 0,
            error_count: 5,
            warnings: vec![],
        };

        assert_eq!(failed_payload.default_severity(), AuditSeverity::High);
    }

    #[test]
    fn test_event_requires_immediate_attention() {
        let context = AuditContext::new();

        let low_priority_payload = AuditPayload::CopybookParse {
            copybook_path: "test.cpy".to_string(),
            schema_fingerprint: "abc123".to_string(),
            parse_result: ParseResult::Success,
            parsing_duration_ms: 100,
            field_count: 10,
            level_88_count: 2,
            error_count: 0,
            warnings: vec![],
        };

        let low_event = AuditEvent::new(
            AuditEventType::CopybookParse,
            context.clone(),
            low_priority_payload,
        );

        assert!(!low_event.requires_immediate_attention());

        let high_priority_payload = AuditPayload::SecurityEvent {
            security_event_type: SecurityEventType::DataBreach,
            severity: "Critical".to_string(),
            affected_resources: vec!["customer_data".to_string()],
            threat_indicators: vec![],
            remediation_actions: vec![],
            incident_id: Some("INC-001".to_string()),
        };

        let high_event = AuditEvent::new(
            AuditEventType::SecurityEvent,
            context,
            high_priority_payload,
        );

        assert!(high_event.requires_immediate_attention());
    }

    #[test]
    fn test_audit_severity_display() {
        assert_eq!(AuditSeverity::Info.to_string(), "Info");
        assert_eq!(AuditSeverity::Low.to_string(), "Low");
        assert_eq!(AuditSeverity::Medium.to_string(), "Medium");
        assert_eq!(AuditSeverity::High.to_string(), "High");
        assert_eq!(AuditSeverity::Critical.to_string(), "Critical");
    }

    #[test]
    fn test_audit_severity_cef_numeric() {
        assert_eq!(AuditSeverity::Info.cef_numeric(), 1);
        assert_eq!(AuditSeverity::Low.cef_numeric(), 3);
        assert_eq!(AuditSeverity::Medium.cef_numeric(), 5);
        assert_eq!(AuditSeverity::High.cef_numeric(), 7);
        assert_eq!(AuditSeverity::Critical.cef_numeric(), 10);
    }

    #[test]
    fn test_audit_event_type_display() {
        assert_eq!(AuditEventType::SecurityEvent.to_string(), "SecurityEvent");
        assert_eq!(AuditEventType::CopybookParse.to_string(), "CopybookParse");
        assert_eq!(
            AuditEventType::DataTransformation.to_string(),
            "DataTransformation"
        );
    }
}
