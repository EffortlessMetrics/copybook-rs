// SPDX-License-Identifier: AGPL-3.0-or-later
//! Enterprise Audit System for copybook-rs
//!
//! **EXPERIMENTAL**: This module is experimental and subject to breaking changes.
//! It is disabled by default and must be explicitly enabled via the `audit` feature flag.
//!
//! ## Status
//!
//! This audit system is under active development and should be considered unstable.
//! The API may change significantly between releases, and production use is not
//! recommended without thorough validation in your specific environment.
//!
//! ## Feature Flag
//!
//! To enable the audit system, add the `audit` feature to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! copybook-core = { version = "0.4", features = ["audit"] }
//! ```
//!
//! **Performance Note**: The audit system is disabled by default because it adds
//! overhead to parsing and processing operations. Only enable it when you require
//! comprehensive audit capabilities for compliance or security monitoring.
//!
//! ## Capabilities
//!
//! When enabled, this module provides comprehensive audit capabilities for enterprise
//! mainframe data processing, including:
//!
//! - **Regulatory Compliance**: SOX, HIPAA, GDPR, PCI-DSS compliance tracking
//! - **Security Monitoring**: Access control validation and security event auditing
//! - **Performance Tracking**: Throughput metrics and regression detection
//! - **Data Lineage**: Complete field-level transformation and impact tracking
//! - **Audit Trail Integrity**: Cryptographic hash chains for tamper detection
//!
//! ## Stability Guarantees
//!
//! **None**. This is experimental software:
//!
//! - APIs may change without notice between minor versions
//! - Serialization formats may be incompatible across releases
//! - Performance characteristics are not yet optimized
//! - Error handling patterns may evolve
//!
//! Use at your own risk and validate thoroughly before production deployment.

pub mod compliance;
pub mod context;
pub mod event;
pub mod lineage;
pub mod logger;
pub mod performance;
pub mod report;
pub mod security;

pub use compliance::{
    ComplianceConfig, ComplianceEngine, ComplianceProfile, ComplianceResult, ComplianceViolation,
};
pub use context::{AuditContext, EnvironmentContext, ProcessingConfig, SecurityContext};
pub use event::{AuditEvent, AuditEventType, AuditPayload, AuditSeverity};
pub use lineage::{
    FieldLineage, ImpactAnalyzer, ImpactAssessment, LineageRecord, LineageTracker, RiskLevel,
    TransformationType,
};
pub use logger::{AuditLogger, AuditLoggerConfig, LogFormat, RetentionPolicy};
pub use performance::{
    BaselineManager, PerformanceAuditor, PerformanceBaseline, RegressionDetector, ResourceMetrics,
    ThroughputMetrics,
};
pub use report::{
    AuditReport, ComplianceReport, PerformanceReport, ReportFormat, ReportGenerator, SecurityReport,
};
pub use security::{
    AccessAuditor, AccessEvent, AccessResult, EncryptionConfig, EventMonitor, SecurityAuditor,
    SecurityMonitor, SecurityValidation, SecurityViolation,
};

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Enterprise audit system error types
#[derive(Debug, thiserror::Error)]
pub enum AuditError {
    #[error("Compliance violation: {message}")]
    ComplianceViolation {
        code: String,
        message: String,
        context: Option<HashMap<String, String>>,
    },

    #[error("Security validation failed: {message}")]
    SecurityValidationFailed {
        message: String,
        severity: SecuritySeverity,
    },

    #[error("Performance regression detected: {message}")]
    PerformanceRegression {
        message: String,
        baseline_deviation: f64,
    },

    #[error("Audit trail integrity failure: {message}")]
    AuditTrailIntegrity {
        message: String,
        expected_hash: String,
        actual_hash: String,
    },

    #[error("Configuration error: {message}")]
    Configuration { message: String },

    #[error("I/O error in audit system: {source}")]
    Io {
        #[from]
        source: std::io::Error,
    },

    #[error("Serialization error: {source}")]
    Serialization {
        #[from]
        source: serde_json::Error,
    },
}

/// Security severity levels for audit events
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum SecuritySeverity {
    Low,
    Medium,
    High,
    Critical,
}

/// Audit result type
pub type AuditResult<T> = std::result::Result<T, AuditError>;

/// Guard flag consumed by hardened AC paths in the enterprise audit surface.
///
/// Accepted values (case-insensitive): `1`, `true`, `yes`, `on`.
pub const AUDIT_ENTERPRISE_RELEASE_GATE_ENV: &str = "COPYBOOK_AUDIT_ENTERPRISE_READY";
pub const AUDIT_ENTERPRISE_RELEASE_GATE_VALUES: [&str; 4] = ["1", "true", "yes", "on"];

#[inline]
pub fn is_audit_enterprise_release_gate_enabled() -> bool {
    env::var(AUDIT_ENTERPRISE_RELEASE_GATE_ENV).is_ok_and(|value| {
        AUDIT_ENTERPRISE_RELEASE_GATE_VALUES
            .iter()
            .any(|allowed| allowed.eq_ignore_ascii_case(value.trim()))
    })
}

pub fn require_audit_release_gate(ac: &str) -> AuditResult<()> {
    if is_audit_enterprise_release_gate_enabled() {
        return Ok(());
    }

    Err(AuditError::Configuration {
        message: format!(
            "AC({}): enterprise hardening gate is blocked. Set {} to 1/true/yes/on to run this audit path.",
            ac, AUDIT_ENTERPRISE_RELEASE_GATE_ENV
        ),
    })
}

/// AC10 health-mode guardrail: health checks must have execution evidence.
pub const AC10_HEALTH_EXECUTION_EVIDENCE_ERROR: &str =
    "AC10: audit health checks selected but no audit events were available for execution";

pub fn require_health_execution_evidence(
    checks_requested: bool,
    audit_trail_events: usize,
    audit_log_events: usize,
) -> AuditResult<()> {
    if checks_requested && audit_trail_events == 0 && audit_log_events == 0 {
        return Err(AuditError::Configuration {
            message: AC10_HEALTH_EXECUTION_EVIDENCE_ERROR.to_string(),
        });
    }

    Ok(())
}

/// Generate a unique audit event identifier
pub fn generate_audit_id() -> String {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::ZERO)
        .as_nanos();

    let random_bytes = rand::random::<[u8; 8]>();
    let random_hex = hex::encode(random_bytes);

    format!("audit-{}-{}", timestamp, random_hex)
}

/// Generate a stable lineage identifier from a source digest.
#[inline]
pub fn derive_lineage_id_from_fingerprint(fingerprint: &str) -> String {
    if fingerprint.is_empty() {
        return generate_lineage_id();
    }

    format!("lineage-{fingerprint}")
}

/// Generate a random lineage identifier
#[inline]
pub fn generate_lineage_id() -> String {
    format!("lineage-{}", generate_audit_id())
}

// Thread-local counter for lightweight ID generation
thread_local! {
    static ID_COUNTER: std::cell::RefCell<u64> = const { std::cell::RefCell::new(0) };
}

/// Generate a lightweight audit ID for performance-critical operations
/// Uses thread-local counter instead of expensive timestamp and crypto
#[inline]
pub fn generate_lightweight_audit_id() -> String {
    ID_COUNTER.with(|counter| {
        let mut counter = counter.borrow_mut();
        *counter = counter.wrapping_add(1);
        format!("perf-audit-{}", *counter)
    })
}

/// Generate a cryptographic hash for audit trail integrity
pub fn generate_integrity_hash(data: &[u8], previous_hash: Option<&str>) -> String {
    use sha2::{Digest, Sha256};

    let mut hasher = Sha256::new();
    hasher.update(data);

    if let Some(prev) = previous_hash {
        hasher.update(prev.as_bytes());
    }

    format!("{:x}", hasher.finalize())
}

/// Validate audit trail integrity chain
pub fn validate_audit_chain(events: &[AuditEvent]) -> AuditResult<bool> {
    if events.is_empty() {
        return Ok(true);
    }

    for (i, event) in events.iter().enumerate() {
        if event.event_id.trim().is_empty() {
            return Err(AuditError::AuditTrailIntegrity {
                message: "Audit event is missing event_id".to_string(),
                expected_hash: "non-empty event_id".to_string(),
                actual_hash: String::new(),
            });
        }

        if event.integrity_hash.trim().is_empty() {
            return Err(AuditError::AuditTrailIntegrity {
                message: format!("Integrity hash missing for event {}", event.event_id),
                expected_hash: "non-empty value".to_string(),
                actual_hash: "<empty>".to_string(),
            });
        }

        let expected_previous_hash = if i == 0 {
            None
        } else {
            Some(events[i - 1].integrity_hash.as_str())
        };

        // Serialize event without integrity_hash for validation
        let mut event_for_validation = event.clone();
        event_for_validation.integrity_hash = String::new();
        event_for_validation.previous_hash = None;

        let event_bytes = serde_json::to_vec(&event_for_validation)?;
        let expected_hash = generate_integrity_hash(&event_bytes, expected_previous_hash);

        if event.integrity_hash != expected_hash {
            return Err(AuditError::AuditTrailIntegrity {
                message: format!("Integrity hash mismatch for event {}", event.event_id),
                expected_hash,
                actual_hash: event.integrity_hash.clone(),
            });
        }

        if event.previous_hash.as_deref() != expected_previous_hash {
            return Err(AuditError::AuditTrailIntegrity {
                message: format!("Previous hash mismatch for event {}", event.event_id),
                expected_hash: expected_previous_hash.unwrap_or("None").to_string(),
                actual_hash: event.previous_hash.as_deref().unwrap_or("None").to_string(),
            });
        }
    }

    Ok(true)
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_audit_id() {
        let id1 = generate_audit_id();
        let id2 = generate_audit_id();

        assert!(id1.starts_with("audit-"));
        assert!(id2.starts_with("audit-"));
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_generate_lineage_id() {
        let id = generate_lineage_id();

        assert!(id.starts_with("lineage-audit-"));
    }

    #[test]
    fn test_derive_lineage_id_from_fingerprint() {
        let fingerprint = "abc123";
        let lineage_id = derive_lineage_id_from_fingerprint(fingerprint);

        assert_eq!(lineage_id, "lineage-abc123");
    }

    #[test]
    fn test_generate_integrity_hash() {
        let data = b"test data";
        let hash1 = generate_integrity_hash(data, None);
        let hash2 = generate_integrity_hash(data, Some("previous"));

        assert_eq!(hash1.len(), 64); // SHA-256 produces 64 character hex string
        assert_ne!(hash1, hash2); // Including previous hash should change result
    }

    #[test]
    fn test_empty_audit_chain_validation() {
        let events = vec![];
        assert!(validate_audit_chain(&events).expect("Failed to validate audit chain"));
    }

    #[test]
    fn test_require_health_execution_evidence_requires_events() {
        assert!(
            require_health_execution_evidence(false, 0, 0).is_ok(),
            "No checks requested should not require events"
        );

        assert!(
            require_health_execution_evidence(true, 0, 0).is_err(),
            "Checks requested with no events should fail"
        );

        assert!(
            require_health_execution_evidence(true, 1, 0).is_ok(),
            "Health trail events should satisfy execution evidence"
        );

        assert!(
            require_health_execution_evidence(true, 0, 1).is_ok(),
            "Audit log events should satisfy execution evidence"
        );
    }

    fn make_audit_event_for_chain(id_suffix: &str) -> AuditEvent {
        let context = AuditContext::new().with_operation_id(format!("audit-chain-{id_suffix}"));
        let payload = AuditPayload::PerformanceMeasurement {
            measurement_type: PerformanceMeasurementType::Throughput,
            baseline_id: Some(format!("baseline-{id_suffix}")),
            metrics: PerformanceMetrics {
                throughput_bytes_per_sec: 1024,
                latency_ms: 5,
                cpu_usage_percent: 2.0,
                memory_usage_mb: 16,
                io_operations: 42,
            },
            comparison_result: Some(ComparisonResult::WithinBaseline),
            regression_detected: false,
        };

        AuditEvent::new(AuditEventType::PerformanceMeasurement, context, payload)
    }

    fn make_audit_chain() -> Vec<AuditEvent> {
        let first = make_audit_event_for_chain("one");
        let second = make_audit_event_for_chain("two").with_previous_hash(first.integrity_hash.clone());

        vec![first, second]
    }

    #[test]
    fn test_validate_audit_chain_rejects_missing_event_id() {
        let mut events = make_audit_chain();
        events[0].event_id.clear();

        let err = validate_audit_chain(&events);
        assert!(matches!(err, Err(AuditError::AuditTrailIntegrity { .. })));
    }

    #[test]
    fn test_validate_audit_chain_rejects_missing_integrity_hash() {
        let mut events = make_audit_chain();
        events[0].integrity_hash = String::new();

        let err = validate_audit_chain(&events);
        assert!(matches!(err, Err(AuditError::AuditTrailIntegrity { .. })));
    }

    #[test]
    fn test_validate_audit_chain_rejects_broken_previous_hash() {
        let mut events = make_audit_chain();
        events[1].previous_hash = Some("tampered-link".to_string());

        let err = validate_audit_chain(&events);
        assert!(matches!(err, Err(AuditError::AuditTrailIntegrity { .. })));
    }

    #[test]
    fn test_ac9_gate_status_reflects_env() {
        let expected = if is_audit_enterprise_release_gate_enabled() {
            "production-gated-enabled"
        } else {
            "production-gated-blocked"
        };

        assert_eq!(ac9_monitoring_gate_status(), expected);
    }

    #[test]
    fn test_require_audit_release_gate_error_path_matches_gate_state() {
        let gate_enabled = is_audit_enterprise_release_gate_enabled();
        let result = require_audit_release_gate("AC5");

        if gate_enabled {
            assert!(result.is_ok());
        } else {
            assert!(matches!(result, Err(AuditError::Configuration { .. })));
        }
    }
}
