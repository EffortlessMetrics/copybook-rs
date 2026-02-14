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

pub use compliance::{ComplianceEngine, ComplianceProfile, ComplianceResult, ComplianceViolation, ComplianceConfig};
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
    AccessAuditor, AccessEvent, AccessResult, SecurityAuditor, SecurityMonitor, SecurityValidation,
    SecurityViolation,
};

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
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
}
