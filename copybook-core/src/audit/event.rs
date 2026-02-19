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
    Info,
    Low,
    Medium,
    High,
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ParseResult {
    Success,
    SuccessWithWarnings,
    Failed,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidationResult {
    Valid,
    ValidWithWarnings,
    Invalid,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    pub error_code: String,
    pub field_path: Option<String>,
    pub record_index: Option<u64>,
    pub byte_offset: Option<u64>,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TransformationOperation {
    Decode,
    Encode,
    Validate,
    Convert,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TransformationResult {
    Success,
    PartialSuccess,
    Failed,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PerformanceMeasurementType {
    Throughput,
    Latency,
    ResourceUtilization,
    Baseline,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    pub throughput_bytes_per_sec: u64,
    pub latency_ms: u64,
    pub cpu_usage_percent: f64,
    pub memory_usage_mb: u64,
    pub io_operations: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComparisonResult {
    BetterThanBaseline,
    WithinBaseline,
    BelowBaseline,
    SignificantRegression,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ComplianceValidationResult {
    Compliant,
    NonCompliant,
    RequiresReview,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceViolationDetail {
    pub violation_id: String,
    pub regulation: String,
    pub severity: String,
    pub description: String,
    pub remediation: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum SecurityEventType {
    UnauthorizedAccess,
    AuthenticationFailure,
    AuthorizationFailure,
    DataBreach,
    IntegrityViolation,
    ConfigurationTampering,
    SuspiciousActivity,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldMapping {
    pub source_field: String,
    pub target_field: String,
    pub transformation: String,
    pub confidence_score: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformationRule {
    pub rule_id: String,
    pub rule_type: String,
    pub description: String,
    pub parameters: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactAssessmentSummary {
    pub affected_systems: u32,
    pub risk_level: String,
    pub estimated_impact: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum UserImpactLevel {
    None,
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AccessType {
    Read,
    Write,
    Execute,
    Delete,
    Admin,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AccessResult {
    Success,
    Denied,
    Failed,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ConfigurationChangeType {
    Create,
    Update,
    Delete,
    Import,
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
