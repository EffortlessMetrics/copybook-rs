// SPDX-License-Identifier: AGPL-3.0-or-later
//! Security Audit System
//!
//! Provides comprehensive security monitoring, access control auditing,
//! and threat detection for copybook-rs enterprise operations.

use std::collections::{HashMap, HashSet};
use std::net::IpAddr;
use std::str::FromStr;

use serde::{Deserialize, Serialize};

use super::{
    AuditError, AuditResult, SecuritySeverity, AUDIT_ENTERPRISE_RELEASE_GATE_ENV,
    generate_audit_id, is_audit_enterprise_release_gate_enabled, require_audit_release_gate,
};

/// AC9 status helper for CLI-facing metadata and diagnostics.
#[must_use]
pub fn ac9_monitoring_gate_status() -> &'static str {
    if is_audit_enterprise_release_gate_enabled() {
        "production-gated-enabled"
    } else {
        "production-gated-blocked"
    }
}

/// AC9 status helper for environment-variable-aware reporting.
#[must_use]
pub const fn ac9_monitoring_gate_env() -> &'static str {
    AUDIT_ENTERPRISE_RELEASE_GATE_ENV
}

// AC5 status: deterministic, non-silent policy checks suitable for gated
// production use while retaining compatibility for existing behavior.

/// Security audit system for monitoring and validation
pub struct SecurityAuditor;

impl SecurityAuditor {
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    fn validate_access_event(event: &AccessEvent) -> AuditResult<()> {
        let user_id = event.user_id.trim();
        let resource_type = event.resource_type.trim();
        let resource_id = event.resource_id.trim();
        let access_type = event.access_type.trim();

        if user_id.is_empty()
            || resource_type.is_empty()
            || resource_id.is_empty()
            || access_type.is_empty()
        {
            return Err(AuditError::SecurityValidationFailed {
                message: "Access event must include user_id, resource_type, resource_id, and access_type"
                    .to_string(),
                severity: SecuritySeverity::High,
            });
        }

        if let Some(source_ip) = &event.source_ip {
            if source_ip.trim().is_empty() {
                return Err(AuditError::SecurityValidationFailed {
                    message: "Invalid source_ip format: empty".to_string(),
                    severity: SecuritySeverity::Medium,
                });
            }

            if IpAddr::from_str(source_ip)
                .is_err()
            {
                return Err(AuditError::SecurityValidationFailed {
                    message: format!("Invalid source_ip format: {source_ip}"),
                    severity: SecuritySeverity::Medium,
                });
            }
        }

        if let Some(user_agent) = &event.user_agent
            && user_agent.trim().is_empty()
        {
            return Err(AuditError::SecurityValidationFailed {
                message: "user_agent must be non-empty when provided".to_string(),
                severity: SecuritySeverity::Medium,
            });
        }

        if let Some(timestamp) = &event.timestamp {
            let timestamp = timestamp.trim();

            if timestamp.is_empty() {
                return Err(AuditError::SecurityValidationFailed {
                    message: "timestamp is required to be RFC3339 when provided"
                        .to_string(),
                    severity: SecuritySeverity::High,
                });
            }

            if chrono::DateTime::parse_from_rfc3339(timestamp).is_err() {
                return Err(AuditError::SecurityValidationFailed {
                    message: format!("timestamp must be RFC3339: {timestamp}"),
                    severity: SecuritySeverity::High,
                });
            }
        }

        Ok(())
    }

    /// Audit an access event and validate for security violations
    pub fn audit_access_event(&self, event: &AccessEvent) -> AuditResult<SecurityValidation> {
        require_audit_release_gate("AC5")?;

        Self::validate_access_event(event)?;

        let mut violations = Vec::new();

        // Check for failed access attempts
        if matches!(event.result, AccessResult::Failed | AccessResult::Denied) {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "medium".to_string(),
                description: format!(
                    "Access {:?} for user {} to {} {}",
                    event.result, event.user_id, event.resource_type, event.resource_id
                ),
            });
        }

        // Check for suspicious source IP patterns
        if let Some(source_ip) = &event.source_ip
            && IpAddr::from_str(source_ip)
                .is_ok_and(|ip| ip.is_loopback() || ip.is_unspecified())
        {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "low".to_string(),
                description: format!("Suspicious source IP detected: {}", source_ip),
            });
        }

        let is_compliant = violations.is_empty();

        Ok(SecurityValidation {
            is_compliant,
            violations,
        })
    }

    /// Check if security severity requires immediate attention
    #[must_use]
    pub fn check_sensitive_operation(&self, severity: &str) -> bool {
        matches!(
            severity.trim().to_ascii_lowercase().as_str(),
            "critical" | "high"
        )
    }

    /// Validate encryption configuration against security policy
    #[must_use]
    pub fn validate_encryption_config(&self, config: &EncryptionConfig) -> Vec<SecurityViolation> {
        let mut violations = Vec::new();
        let at_rest_algorithm = config.at_rest_algorithm.trim();
        let in_transit_protocol = config.in_transit_protocol.trim();

        if at_rest_algorithm.is_empty() {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "high".to_string(),
                description: "At-rest encryption algorithm is required".to_string(),
            });
        }

        // Check at-rest algorithm: must be AES-256 or higher
        let at_rest_upper = at_rest_algorithm.to_ascii_uppercase();
        let at_rest_ok = at_rest_upper.contains("AES-256")
            || at_rest_upper.contains("AES256");
        if !at_rest_ok {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "high".to_string(),
                description: format!(
                    "At-rest encryption algorithm '{}' does not meet AES-256 minimum requirement",
                    config.at_rest_algorithm
                ),
            });
        }

        // Check in-transit protocol: must be TLS 1.2+
        if in_transit_protocol.is_empty() {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "high".to_string(),
                description: "In-transit protocol is required".to_string(),
            });
        }

        // Check key rotation: 0 disables key hygiene enforcement
        if config.key_rotation_days == 0 {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "medium".to_string(),
                description: "Key rotation period of 0 days disables configured rotation policy"
                    .to_string(),
            });
        }

        let in_transit_upper = in_transit_protocol.to_ascii_uppercase();
        let tls_ok = in_transit_upper.contains("TLS 1.2")
            || in_transit_upper.contains("TLS1.2")
            || in_transit_upper.contains("TLS 1.3")
            || in_transit_upper.contains("TLS1.3")
            || in_transit_upper.contains("TLSV1.2")
            || in_transit_upper.contains("TLSV1.3");
        if !tls_ok {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "high".to_string(),
                description: format!(
                    "In-transit protocol '{}' does not meet TLS 1.2+ minimum requirement",
                    config.in_transit_protocol
                ),
            });
        }

        // Check key rotation: must be <= 90 days
        if config.key_rotation_days > 90 {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "medium".to_string(),
                description: format!(
                    "Key rotation period of {} days exceeds the maximum allowed 90 days",
                    config.key_rotation_days
                ),
            });
        }

        violations
    }
}

impl Default for SecurityAuditor {
    fn default() -> Self {
        Self::new()
    }
}

/// Encryption configuration for security validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EncryptionConfig {
    /// Algorithm used for at-rest encryption (e.g., "AES-256-GCM")
    pub at_rest_algorithm: String,
    /// Protocol used for in-transit encryption (e.g., "TLS 1.3")
    pub in_transit_protocol: String,
    /// Maximum number of days before key rotation is required
    pub key_rotation_days: u32,
}

/// Access control events for audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccessEvent {
    pub user_id: String,
    pub resource_type: String,
    pub resource_id: String,
    pub access_type: String,
    pub source_ip: Option<String>,
    pub user_agent: Option<String>,
    pub result: AccessResult,
    /// Optional RFC3339 timestamp for time-based anomaly detection
    pub timestamp: Option<String>,
}

/// Access attempt results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AccessResult {
    Success,
    Denied,
    Failed,
}

/// Security validation results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityValidation {
    pub is_compliant: bool,
    pub violations: Vec<SecurityViolation>,
}

/// Security violations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityViolation {
    pub violation_id: String,
    pub severity: String,
    pub description: String,
}

/// Access auditing system
pub struct AccessAuditor;

impl AccessAuditor {
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Create an access record with default values
    #[must_use]
    pub fn create_access_record(
        &self,
        user_id: &str,
        resource_type: &str,
        resource_id: &str,
        access_type: &str,
    ) -> AccessEvent {
        AccessEvent {
            user_id: user_id.to_string(),
            resource_type: resource_type.to_string(),
            resource_id: resource_id.to_string(),
            access_type: access_type.to_string(),
            source_ip: None,
            user_agent: None,
            result: AccessResult::Success,
            timestamp: None,
        }
    }

    /// Audit access patterns and detect suspicious behavior
    #[must_use]
    pub fn audit_access_pattern(&self, events: &[AccessEvent]) -> Vec<SecurityViolation> {
        let mut violations = Vec::new();
        const DENIED_ACCESS_WINDOW_SECS: i64 = 300;
        const DENIED_ACCESS_WARNING_THRESHOLD: usize = 3;
        const DENIED_ACCESS_HIGH_THRESHOLD: usize = 5;
        const DENY_LOOP_WINDOW_SECS: i64 = 120;
        const DENY_LOOP_WARNING_THRESHOLD: usize = 4;
        const DENY_LOOP_HIGH_THRESHOLD: usize = 6;
        const USER_RESOURCE_BURST_WINDOW_SECS: i64 = 120;
        const USER_RESOURCE_BURST_WARNING_THRESHOLD: usize = 6;
        const USER_RESOURCE_BURST_HIGH_THRESHOLD: usize = 10;

        // Count failures by user
        let mut user_failures: HashMap<String, usize> = HashMap::new();
        let mut denied_by_source_and_bucket: HashMap<(String, Option<i64>), usize> =
            HashMap::new();
        let mut denied_loop_by_user_resource_source: HashMap<(String, String, String, i64), usize> =
            HashMap::new();
        let mut user_resource_burst_by_window: HashMap<(String, i64), HashSet<String>> =
            HashMap::new();

        for event in events {
            let user_id = event.user_id.trim();
            let resource_id = event.resource_id.trim();

            if user_id.is_empty()
                || event.resource_type.trim().is_empty()
                || resource_id.is_empty()
                || event.access_type.trim().is_empty()
            {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: "high".to_string(),
                    description: "Correlation input rejected: access event must include user_id, resource_type, resource_id, and access_type".to_string(),
                });
                continue;
            }

            if matches!(event.result, AccessResult::Failed | AccessResult::Denied) {
                *user_failures.entry(user_id.to_string()).or_insert(0) += 1;
            }

            if let AccessResult::Denied = event.result {
                let Some(source_ip) = event
                    .source_ip
                    .as_deref()
                    .map(str::trim)
                    .filter(|value| !value.is_empty())
                else {
                    violations.push(SecurityViolation {
                        violation_id: generate_audit_id(),
                        severity: "high".to_string(),
                        description: format!(
                            "Correlation input rejected: denied access for user {user_id} to {resource_id} is missing source_ip"
                        ),
                    });
                    continue;
                };

                let Some(timestamp) = event.timestamp.as_deref().map(str::trim).filter(|value| !value.is_empty()) else {
                    violations.push(SecurityViolation {
                        violation_id: generate_audit_id(),
                        severity: "high".to_string(),
                        description: format!(
                            "Deny-loop correlation rejected: denied access for user {user_id} to {resource_id} is missing timestamp"
                        ),
                    });
                    continue;
                };

                let Some(deny_bucket) =
                    chrono::DateTime::parse_from_rfc3339(timestamp)
                        .ok()
                        .map(|timestamp| timestamp.timestamp() / DENY_LOOP_WINDOW_SECS)
                else {
                    violations.push(SecurityViolation {
                        violation_id: generate_audit_id(),
                        severity: "high".to_string(),
                        description: format!(
                            "Deny-loop correlation rejected: denied access for user {user_id} to {resource_id} has invalid RFC3339 timestamp"
                        ),
                    });
                    continue;
                };

                *denied_loop_by_user_resource_source
                    .entry((user_id.to_string(), resource_id.to_string(), source_ip.to_string(), deny_bucket))
                    .or_insert(0) += 1;

                let bucket = event
                    .timestamp
                    .as_deref()
                    .and_then(|timestamp| {
                        chrono::DateTime::parse_from_rfc3339(timestamp)
                            .ok()
                            .map(|timestamp| timestamp.timestamp() / DENIED_ACCESS_WINDOW_SECS)
                    });

                *denied_by_source_and_bucket
                    .entry((source_ip.to_string(), bucket))
                    .or_insert(0) += 1;
            }

            let Some(timestamp) = event.timestamp.as_deref().map(str::trim).filter(|value| !value.is_empty()) else {
                continue;
            };

            let Some(burst_bucket) = chrono::DateTime::parse_from_rfc3339(timestamp)
                .ok()
                .map(|timestamp| timestamp.timestamp() / USER_RESOURCE_BURST_WINDOW_SECS)
            else {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: "high".to_string(),
                    description: format!(
                        "User-resource burst correlation rejected: invalid timestamp for user {user_id} and resource {resource_id}"
                    ),
                });
                continue;
            };

            user_resource_burst_by_window
                .entry((user_id.to_string(), burst_bucket))
                .or_default()
                .insert(resource_id.to_string());
        }

        // Generate violations for users with multiple failures
        for (user_id, failure_count) in user_failures {
            if failure_count >= 3 {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: if failure_count >= 5 {
                        "high".to_string()
                    } else {
                        "medium".to_string()
                    },
                    description: format!(
                        "User {} has {} failed access attempts - possible brute force attack",
                        user_id, failure_count
                    ),
                });
            }
        }

        for ((source_ip, bucket), denied_count) in denied_by_source_and_bucket {
            if denied_count >= DENIED_ACCESS_WARNING_THRESHOLD {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: if denied_count >= DENIED_ACCESS_HIGH_THRESHOLD {
                        "high".to_string()
                    } else {
                        "medium".to_string()
                    },
                    description: if bucket.is_some() {
                        format!(
                            "Source IP {source_ip} has {denied_count} denied access attempts in a 5-minute window",
                        )
                    } else {
                        format!(
                            "Source IP {source_ip} has {denied_count} denied access attempts without a valid timestamp",
                        )
                    },
                });
            }
        }

        for ((user_id, resource_id, source_ip, denied_count), _bucket) in
            denied_loop_by_user_resource_source
        {
            if denied_count >= DENY_LOOP_WARNING_THRESHOLD {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: if denied_count >= DENY_LOOP_HIGH_THRESHOLD {
                        "high".to_string()
                    } else {
                        "medium".to_string()
                    },
                    description: format!(
                        "Deny-loop correlation detected: user {user_id} denied {denied_count} times for resource {resource_id} from source {source_ip} in a 2-minute window"
                    ),
                });
            }
        }

        for ((user_id, _bucket), resources) in user_resource_burst_by_window {
            let resource_count = resources.len();
            if resource_count >= USER_RESOURCE_BURST_WARNING_THRESHOLD {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: if resource_count >= USER_RESOURCE_BURST_HIGH_THRESHOLD {
                        "high".to_string()
                    } else {
                        "medium".to_string()
                    },
                    description: format!(
                        "User-resource burst correlation detected: user {user_id} accessed {resource_count} distinct resources in a 2-minute window"
                    ),
                });
            }
        }

        violations
    }

    /// Detect anomalies in a time window using frequency and pattern analysis.
    ///
    /// Checks for:
    /// - Rapid-fire access: >10 events from same user in window
    /// - Multi-resource scanning: same user accessing >5 distinct resources
    ///
    /// The `window_secs` parameter is reserved for future time-filtering when
    /// events carry timestamps; currently all provided events are analysed.
    #[must_use]
    pub fn detect_anomalies(
        &self,
        events: &[AccessEvent],
        _window_secs: u64,
    ) -> Vec<SecurityViolation> {
        let mut violations = Vec::new();

        // Aggregate per-user counts and distinct resources
        let mut user_event_count: HashMap<&str, usize> = HashMap::new();
        let mut user_resources: HashMap<&str, HashSet<&str>> = HashMap::new();

        for event in events {
            *user_event_count.entry(event.user_id.as_str()).or_insert(0) += 1;

            user_resources
                .entry(event.user_id.as_str())
                .or_default()
                .insert(event.resource_id.as_str());
        }

        // Rapid-fire access: >10 events per user
        for (user_id, count) in &user_event_count {
            if *count > 10 {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: "high".to_string(),
                    description: format!(
                        "Anomaly: user {} generated {} events in the monitoring window (threshold: 10)",
                        user_id, count
                    ),
                });
            }
        }

        // Multi-resource scanning: >5 distinct resources per user
        for (user_id, resources) in &user_resources {
            if resources.len() > 5 {
                violations.push(SecurityViolation {
                    violation_id: generate_audit_id(),
                    severity: "high".to_string(),
                    description: format!(
                        "Anomaly: user {} accessed {} distinct resources in the monitoring window (threshold: 5)",
                        user_id,
                        resources.len()
                    ),
                });
            }
        }

        violations
    }
}

impl Default for AccessAuditor {
    fn default() -> Self {
        Self::new()
    }
}

/// Map a severity string to a numeric value for SIEM formats.
fn severity_to_num(severity: &str) -> u8 {
    match severity {
        "critical" => 10,
        "high" => 7,
        "medium" => 5,
        "low" => 3,
        _ => 5,
    }
}

/// Security monitoring system
pub struct SecurityMonitor;

impl SecurityMonitor {
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Detect threats based on failure count thresholds
    #[must_use]
    pub fn detect_threats(&self, failed_count: usize) -> Vec<SecurityViolation> {
        let mut violations = Vec::new();

        // Threshold-based threat detection
        if failed_count >= 10 {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "critical".to_string(),
                description: format!(
                    "Critical: {} failed operations detected - possible security breach or system compromise",
                    failed_count
                ),
            });
        } else if failed_count >= 5 {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "high".to_string(),
                description: format!(
                    "High: {} failed operations detected - potential security incident",
                    failed_count
                ),
            });
        } else if failed_count >= 3 {
            violations.push(SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "medium".to_string(),
                description: format!(
                    "Medium: {} failed operations detected - monitoring required",
                    failed_count
                ),
            });
        }

        violations
    }

    /// Generate a security alert string from a violation
    #[must_use]
    pub fn generate_security_alert(&self, violation: &SecurityViolation) -> String {
        format!(
            "[SECURITY ALERT] {} | Severity: {} | {}",
            violation.violation_id, violation.severity, violation.description
        )
    }

    /// Format a violation as ArcSight CEF (Common Event Format).
    ///
    /// Format: `CEF:0|Vendor|Product|Version|SignatureID|Name|Severity|Extensions`
    #[must_use]
    pub fn format_cef(&self, violation: &SecurityViolation) -> String {
        let severity_num = severity_to_num(&violation.severity);
        format!(
            "CEF:0|copybook-rs|SecurityMonitor|1.0|{}|{}|{}|",
            violation.violation_id, violation.description, severity_num
        )
    }

    /// Format a violation as IBM QRadar LEEF (Log Event Extended Format).
    ///
    /// Format: `LEEF:2.0|Vendor|Product|Version|EventID|key=value\t...`
    #[must_use]
    pub fn format_leef(&self, violation: &SecurityViolation) -> String {
        format!(
            "LEEF:2.0|copybook-rs|SecurityMonitor|1.0|{}|severity={}\tdescription={}",
            violation.violation_id, violation.severity, violation.description
        )
    }

    /// Format a violation as a Syslog RFC 5424 message.
    ///
    /// Format: `<Priority>VERSION TIMESTAMP HOSTNAME APP-NAME PROCID MSGID STRUCTURED-DATA MSG`
    #[must_use]
    pub fn format_syslog(&self, violation: &SecurityViolation) -> String {
        // RFC 5424 priority = (facility * 8) + severity_level
        // Using facility 16 (local0) and severity 4 (warning) as a reasonable default.
        let priority = 16 * 8 + 4; // 132
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        format!(
            "<{}>1 {} copybook-rs SecurityMonitor - {} - {}",
            priority, timestamp, violation.violation_id, violation.description
        )
    }
}

impl Default for SecurityMonitor {
    fn default() -> Self {
        Self::new()
    }
}

/// Real-time event monitor that tracks rolling access events and emits alerts
/// when a configurable threshold is breached.
pub struct EventMonitor {
    /// Number of events that must accumulate before an alert is emitted
    pub alert_threshold: usize,
    /// Events seen since last reset
    events: Vec<AccessEvent>,
    /// Alerts that have been emitted and are awaiting consumption
    alerts: Vec<SecurityViolation>,
}

impl EventMonitor {
    /// Create a new `EventMonitor` with the given alert threshold.
    #[must_use]
    pub fn new(alert_threshold: usize) -> Self {
        Self {
            alert_threshold,
            events: Vec::new(),
            alerts: Vec::new(),
        }
    }

    /// Process a single event. Returns `Some(SecurityViolation)` if the alert
    /// threshold has just been crossed, and adds the violation to
    /// `pending_alerts`. Returns `None` otherwise.
    pub fn process_event(&mut self, event: &AccessEvent) -> Option<SecurityViolation> {
        self.events.push(event.clone());

        if self.events.len() == self.alert_threshold + 1 {
            let violation = SecurityViolation {
                violation_id: generate_audit_id(),
                severity: "high".to_string(),
                description: format!(
                    "EventMonitor: alert threshold of {} events breached ({} events received)",
                    self.alert_threshold,
                    self.events.len()
                ),
            };
            self.alerts.push(violation.clone());
            return Some(violation);
        }

        None
    }

    /// Return pending alerts that have not yet been consumed.
    #[must_use]
    pub fn pending_alerts(&self) -> &[SecurityViolation] {
        &self.alerts
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;
    use std::{env, sync::Mutex};

    static AUDIT_ENTERPRISE_GATE_MUTEX: Mutex<()> = Mutex::new(());

    fn with_audit_gate_value<T>(value: Option<&str>, operation: impl FnOnce() -> T) -> T {
        let _guard = AUDIT_ENTERPRISE_GATE_MUTEX.lock().unwrap();
        let previous = env::var_os(AUDIT_ENTERPRISE_RELEASE_GATE_ENV);

        match value {
            Some(enabled) => env::set_var(AUDIT_ENTERPRISE_RELEASE_GATE_ENV, enabled),
            None => env::remove_var(AUDIT_ENTERPRISE_RELEASE_GATE_ENV),
        }

        let result = operation();

        match previous {
            Some(previous_value) => env::set_var(AUDIT_ENTERPRISE_RELEASE_GATE_ENV, previous_value),
            None => env::remove_var(AUDIT_ENTERPRISE_RELEASE_GATE_ENV),
        }

        result
    }

    fn with_audit_gate_enabled<T>(operation: impl FnOnce() -> T) -> T {
        with_audit_gate_value(Some("1"), operation)
    }

    fn with_audit_gate_disabled<T>(operation: impl FnOnce() -> T) -> T {
        with_audit_gate_value(None, operation)
    }

    fn audit_access_event_with_gate(
        auditor: &SecurityAuditor,
        event: &AccessEvent,
    ) -> AuditResult<SecurityValidation> {
        with_audit_gate_enabled(|| auditor.audit_access_event(event))
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    fn make_event(user_id: &str, resource_id: &str, result: AccessResult) -> AccessEvent {
        AccessEvent {
            user_id: user_id.to_string(),
            resource_type: "database".to_string(),
            resource_id: resource_id.to_string(),
            access_type: "read".to_string(),
            source_ip: None,
            user_agent: None,
            result,
            timestamp: None,
        }
    }

    fn make_event_with_timestamp_and_source(
        user_id: &str,
        resource_id: &str,
        result: AccessResult,
        source_ip: Option<&str>,
        timestamp: &str,
    ) -> AccessEvent {
        AccessEvent {
            user_id: user_id.to_string(),
            resource_type: "database".to_string(),
            resource_id: resource_id.to_string(),
            access_type: "read".to_string(),
            source_ip: source_ip.map(str::to_string),
            user_agent: None,
            result,
            timestamp: Some(timestamp.to_string()),
        }
    }

    #[test]
    fn test_validate_access_event_rejects_missing_required_fields() {
        let mut event = make_event("user123", "prod-db", AccessResult::Success);
        event.user_id = String::new();

        let err = SecurityAuditor::validate_access_event(&event)
            .expect_err("event should fail when required fields are missing");
        assert!(matches!(err, AuditError::SecurityValidationFailed { .. }));
    }

    #[test]
    fn test_validate_access_event_rejects_invalid_source_ip() {
        let mut event = make_event("user123", "prod-db", AccessResult::Success);
        event.source_ip = Some("999.999.999.999".to_string());

        let err = SecurityAuditor::validate_access_event(&event)
            .expect_err("event should fail for invalid source_ip");
        assert!(matches!(err, AuditError::SecurityValidationFailed { .. }));
    }

    #[test]
    fn test_validate_access_event_rejects_empty_user_agent_when_present() {
        let mut event = make_event("user123", "prod-db", AccessResult::Success);
        event.user_agent = Some("   ".to_string());

        let err = SecurityAuditor::validate_access_event(&event)
            .expect_err("event should fail for empty user_agent");
        assert!(matches!(err, AuditError::SecurityValidationFailed { .. }));
    }

    #[test]
    fn test_validate_access_event_rejects_invalid_timestamp() {
        let mut event = make_event("user123", "prod-db", AccessResult::Success);
        event.timestamp = Some("invalid-timestamp".to_string());

        let err = SecurityAuditor::validate_access_event(&event)
            .expect_err("event should fail for invalid timestamp");
        assert!(matches!(
            err,
            AuditError::SecurityValidationFailed {
                message: _,
                severity: SecuritySeverity::High,
            }
        ));
    }

    #[test]
    fn test_security_auditor_audit_access_event_requires_enterprise_gate() {
        let auditor = SecurityAuditor::new();
        let event = AccessEvent {
            user_id: "gate-check".to_string(),
            resource_type: "database".to_string(),
            resource_id: "prod-db".to_string(),
            access_type: "read".to_string(),
            source_ip: None,
            user_agent: None,
            result: AccessResult::Success,
            timestamp: None,
        };

        let err = with_audit_gate_disabled(|| auditor.audit_access_event(&event))
            .expect_err("audit should fail when enterprise gate is disabled");
        assert!(err.to_string().contains("enterprise"));
        assert!(err.to_string().contains("AC5"));
    }

    // -------------------------------------------------------------------------
    // SecurityAuditor – existing tests (ported, timestamp field added)
    // -------------------------------------------------------------------------

    #[test]
    fn test_security_auditor_audit_access_event_success() {
        let auditor = SecurityAuditor::new();
        let event = AccessEvent {
            user_id: "user123".to_string(),
            resource_type: "database".to_string(),
            resource_id: "prod-db".to_string(),
            access_type: "read".to_string(),
            source_ip: Some("192.168.1.1".to_string()),
            user_agent: Some("Mozilla/5.0".to_string()),
            result: AccessResult::Success,
            timestamp: None,
        };

        let validation = audit_access_event_with_gate(&auditor, &event)
            .expect("Audit should succeed");

        assert!(validation.is_compliant);
        assert!(validation.violations.is_empty());
    }

    #[test]
    fn test_security_auditor_audit_access_event_failed() {
        let auditor = SecurityAuditor::new();
        let event = AccessEvent {
            user_id: "user123".to_string(),
            resource_type: "database".to_string(),
            resource_id: "prod-db".to_string(),
            access_type: "write".to_string(),
            source_ip: Some("192.168.1.1".to_string()),
            user_agent: None,
            result: AccessResult::Failed,
            timestamp: None,
        };

        let validation = audit_access_event_with_gate(&auditor, &event)
            .expect("Audit should succeed");

        assert!(!validation.is_compliant);
        assert_eq!(validation.violations.len(), 1);
        assert_eq!(validation.violations[0].severity, "medium");
    }

    #[test]
    fn test_security_auditor_audit_access_event_denied() {
        let auditor = SecurityAuditor::new();
        let event = AccessEvent {
            user_id: "user456".to_string(),
            resource_type: "file".to_string(),
            resource_id: "secret.txt".to_string(),
            access_type: "read".to_string(),
            source_ip: None,
            user_agent: None,
            result: AccessResult::Denied,
            timestamp: None,
        };

        let validation = audit_access_event_with_gate(&auditor, &event)
            .expect("Audit should succeed");

        assert!(!validation.is_compliant);
        assert_eq!(validation.violations.len(), 1);
        assert!(
            validation.violations[0]
                .description
                .contains("Access Denied")
        );
    }

    #[test]
    fn test_security_auditor_suspicious_ip() {
        let auditor = SecurityAuditor::new();
        let event = AccessEvent {
            user_id: "user123".to_string(),
            resource_type: "database".to_string(),
            resource_id: "prod-db".to_string(),
            access_type: "read".to_string(),
            source_ip: Some("0.0.0.0".to_string()),
            user_agent: None,
            result: AccessResult::Success,
            timestamp: None,
        };

        let validation = audit_access_event_with_gate(&auditor, &event)
            .expect("Audit should succeed");

        assert!(!validation.is_compliant);
        assert_eq!(validation.violations.len(), 1);
        assert_eq!(validation.violations[0].severity, "low");
        assert!(
            validation.violations[0]
                .description
                .contains("Suspicious source IP")
        );
    }

    #[test]
    fn test_security_auditor_check_sensitive_operation() {
        let auditor = SecurityAuditor::new();

        assert!(auditor.check_sensitive_operation("critical"));
        assert!(auditor.check_sensitive_operation("high"));
        assert!(auditor.check_sensitive_operation("Critical"));
        assert!(!auditor.check_sensitive_operation("medium"));
        assert!(!auditor.check_sensitive_operation("low"));
        assert!(!auditor.check_sensitive_operation("unknown"));
    }

    // -------------------------------------------------------------------------
    // EncryptionConfig / validate_encryption_config
    // -------------------------------------------------------------------------

    #[test]
    fn test_validate_encryption_config_compliant() {
        let auditor = SecurityAuditor::new();
        let config = EncryptionConfig {
            at_rest_algorithm: "AES-256-GCM".to_string(),
            in_transit_protocol: "TLS 1.3".to_string(),
            key_rotation_days: 30,
        };
        let violations = auditor.validate_encryption_config(&config);
        assert!(
            violations.is_empty(),
            "Expected no violations, got: {:?}",
            violations
        );
    }

    #[test]
    fn test_validate_encryption_config_rejects_missing_policy_fields() {
        let auditor = SecurityAuditor::new();
        let config = EncryptionConfig {
            at_rest_algorithm: "   ".to_string(),
            in_transit_protocol: "   ".to_string(),
            key_rotation_days: 0,
        };
        let violations = auditor.validate_encryption_config(&config);
        assert!(violations.iter().any(|v| v.description.contains("required")));
    }

    #[test]
    fn test_validate_encryption_config_weak_at_rest() {
        let auditor = SecurityAuditor::new();
        let config = EncryptionConfig {
            at_rest_algorithm: "AES-128-CBC".to_string(),
            in_transit_protocol: "TLS 1.2".to_string(),
            key_rotation_days: 60,
        };
        let violations = auditor.validate_encryption_config(&config);
        assert_eq!(violations.len(), 1);
        assert!(
            violations[0]
                .description
                .contains("at-rest encryption algorithm")
                || violations[0].description.contains("At-rest")
        );
        assert_eq!(violations[0].severity, "high");
    }

    #[test]
    fn test_validate_encryption_config_weak_in_transit() {
        let auditor = SecurityAuditor::new();
        let config = EncryptionConfig {
            at_rest_algorithm: "AES-256-GCM".to_string(),
            in_transit_protocol: "TLS 1.0".to_string(),
            key_rotation_days: 30,
        };
        let violations = auditor.validate_encryption_config(&config);
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "high");
        assert!(
            violations[0].description.contains("TLS 1.2+")
                || violations[0].description.contains("in-transit")
                || violations[0].description.contains("In-transit")
        );
    }

    #[test]
    fn test_validate_encryption_config_key_rotation_too_long() {
        let auditor = SecurityAuditor::new();
        let config = EncryptionConfig {
            at_rest_algorithm: "AES-256-GCM".to_string(),
            in_transit_protocol: "TLS 1.2".to_string(),
            key_rotation_days: 120,
        };
        let violations = auditor.validate_encryption_config(&config);
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "medium");
        assert!(
            violations[0].description.contains("90 days")
                || violations[0].description.contains("key rotation")
        );
    }

    #[test]
    fn test_validate_encryption_config_multiple_violations() {
        let auditor = SecurityAuditor::new();
        let config = EncryptionConfig {
            at_rest_algorithm: "DES".to_string(),
            in_transit_protocol: "SSL 3.0".to_string(),
            key_rotation_days: 365,
        };
        let violations = auditor.validate_encryption_config(&config);
        assert_eq!(
            violations.len(),
            3,
            "Expected 3 violations, got: {:?}",
            violations
        );
    }

    #[test]
    fn test_validate_encryption_config_exactly_90_days() {
        let auditor = SecurityAuditor::new();
        let config = EncryptionConfig {
            at_rest_algorithm: "AES-256-GCM".to_string(),
            in_transit_protocol: "TLSv1.2".to_string(),
            key_rotation_days: 90,
        };
        let violations = auditor.validate_encryption_config(&config);
        assert!(
            violations.is_empty(),
            "90-day rotation should be compliant: {:?}",
            violations
        );
    }

    // -------------------------------------------------------------------------
    // AccessAuditor – existing tests (ported with timestamp field)
    // -------------------------------------------------------------------------

    #[test]
    fn test_access_auditor_create_access_record() {
        let auditor = AccessAuditor::new();
        let event = auditor.create_access_record("user123", "database", "prod-db", "read");

        assert_eq!(event.user_id, "user123");
        assert_eq!(event.resource_type, "database");
        assert_eq!(event.resource_id, "prod-db");
        assert_eq!(event.access_type, "read");
        assert!(event.source_ip.is_none());
        assert!(event.user_agent.is_none());
        assert!(event.timestamp.is_none());
        assert!(matches!(event.result, AccessResult::Success));
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_no_violations() {
        let auditor = AccessAuditor::new();
        let events = vec![
            make_event("user123", "prod-db", AccessResult::Success),
            make_event("user456", "data.txt", AccessResult::Success),
        ];

        let violations = auditor.audit_access_pattern(&events);
        assert!(violations.is_empty());
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_medium_threat() {
        let auditor = AccessAuditor::new();
        let events = vec![
            make_event("user123", "prod-db", AccessResult::Failed),
            make_event("user123", "prod-db", AccessResult::Failed),
            make_event("user123", "prod-db", AccessResult::Denied),
        ];

        let violations = auditor.audit_access_pattern(&events);
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "medium");
        assert!(
            violations[0]
                .description
                .contains("3 failed access attempts")
        );
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_high_threat() {
        let auditor = AccessAuditor::new();
        let events: Vec<_> = (0..5)
            .map(|_| make_event("attacker", "prod-db", AccessResult::Failed))
            .collect();

        let violations = auditor.audit_access_pattern(&events);
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "high");
        assert!(
            violations[0]
                .description
                .contains("5 failed access attempts")
        );
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_denied_source_ip_window() {
        let auditor = AccessAuditor::new();
        let events = vec![
            make_event_with_timestamp_and_source(
                "alice",
                "resource-a",
                AccessResult::Denied,
                Some("198.51.100.10"),
                "2026-02-28T10:00:00Z",
            ),
            make_event_with_timestamp_and_source(
                "bob",
                "resource-b",
                AccessResult::Denied,
                Some("198.51.100.10"),
                "2026-02-28T10:01:00Z",
            ),
            make_event_with_timestamp_and_source(
                "carol",
                "resource-c",
                AccessResult::Denied,
                Some("198.51.100.10"),
                "2026-02-28T10:02:00Z",
            ),
        ];

        let violations = auditor.audit_access_pattern(&events);
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "medium");
        assert!(
            violations[0]
                .description
                .contains("Source IP 198.51.100.10 has 3 denied access attempts in a 5-minute window")
        );
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_deny_loop_correlation_medium() {
        let auditor = AccessAuditor::new();
        let events = vec![
            make_event_with_timestamp_and_source(
                "attacker",
                "resource-alpha",
                AccessResult::Denied,
                Some("198.51.100.10"),
                "2026-02-28T10:00:00Z",
            ),
            make_event_with_timestamp_and_source(
                "attacker",
                "resource-alpha",
                AccessResult::Denied,
                Some("198.51.100.10"),
                "2026-02-28T10:00:20Z",
            ),
            make_event_with_timestamp_and_source(
                "attacker",
                "resource-alpha",
                AccessResult::Denied,
                Some("198.51.100.10"),
                "2026-02-28T10:00:40Z",
            ),
            make_event_with_timestamp_and_source(
                "attacker",
                "resource-alpha",
                AccessResult::Denied,
                Some("198.51.100.10"),
                "2026-02-28T10:01:00Z",
            ),
        ];

        let violations = auditor.audit_access_pattern(&events);
        assert!(violations
            .iter()
            .any(|v| v.description.contains("Deny-loop correlation detected")));
        assert!(
            violations
                .iter()
                .any(|v| v.description.contains("4 times"))
        );
        assert!(
            violations
                .iter()
                .any(|v| v.severity == "medium" && v.description.contains("Deny-loop correlation detected"))
        );
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_deny_loop_correlation_high() {
        let auditor = AccessAuditor::new();
        let mut events = Vec::new();
        for i in 0..6 {
            events.push(make_event_with_timestamp_and_source(
                "attacker",
                "resource-alpha",
                AccessResult::Denied,
                Some("198.51.100.10"),
                &format!("2026-02-28T10:00:{:02}Z", i * 10),
            ));
        }

        let violations = auditor.audit_access_pattern(&events);
        assert!(
            violations
                .iter()
                .any(|v| v.severity == "high" && v.description.contains("Deny-loop correlation detected"))
        );
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_user_resource_burst_correlation() {
        let auditor = AccessAuditor::new();
        let mut events = Vec::new();
        for i in 0..7 {
            events.push(make_event_with_timestamp_and_source(
                "analyst",
                &format!("resource-{i}"),
                AccessResult::Success,
                Some("203.0.113.20"),
                &format!("2026-02-28T10:05:{:02}Z", i),
            ));
        }

        let violations = auditor.audit_access_pattern(&events);
        assert!(
            violations
                .iter()
                .any(|v| v.severity == "medium" && v.description.contains("User-resource burst correlation detected"))
        );
    }

    #[test]
    fn test_access_auditor_audit_access_pattern_rejects_malformed_correlation_inputs() {
        let auditor = AccessAuditor::new();
        let events = vec![
            AccessEvent {
                user_id: "".to_string(),
                resource_type: "database".to_string(),
                resource_id: "resource-x".to_string(),
                access_type: "read".to_string(),
                source_ip: None,
                user_agent: None,
                result: AccessResult::Denied,
                timestamp: Some("not-a-timestamp".to_string()),
            },
            make_event_with_timestamp_and_source(
                "attacker",
                "resource-y",
                AccessResult::Denied,
                None,
                "2026-02-28T10:10:00Z",
            ),
        ];

        let violations = auditor.audit_access_pattern(&events);
        assert!(
            violations
                .iter()
                .any(|v| v.description
                    .contains("Correlation input rejected: access event must include user_id, resource_type, resource_id, and access_type"))
        );
        assert!(
            violations
                .iter()
                .any(|v| v.description.contains("is missing source_ip"))
        );
    }

    // -------------------------------------------------------------------------
    // AccessAuditor – detect_anomalies
    // -------------------------------------------------------------------------

    #[test]
    fn test_detect_anomalies_no_violations() {
        let auditor = AccessAuditor::new();
        // 5 events, single user, single resource — well under thresholds
        let events: Vec<_> = (0..5)
            .map(|_| make_event("alice", "db-1", AccessResult::Success))
            .collect();
        let violations = auditor.detect_anomalies(&events, 60);
        assert!(violations.is_empty());
    }

    #[test]
    fn test_detect_anomalies_rapid_fire() {
        let auditor = AccessAuditor::new();
        // 11 events from same user — exceeds the 10-event threshold
        let events: Vec<_> = (0..11)
            .map(|_| make_event("rapid-user", "db-1", AccessResult::Success))
            .collect();
        let violations = auditor.detect_anomalies(&events, 60);
        // At minimum one rapid-fire violation
        assert!(!violations.is_empty());
        assert!(
            violations
                .iter()
                .any(|v| v.description.contains("rapid-user"))
        );
    }

    #[test]
    fn test_detect_anomalies_multi_resource_scanning() {
        let auditor = AccessAuditor::new();
        // 6 distinct resources — exceeds the 5-resource threshold
        let events: Vec<_> = (0..6)
            .map(|i| make_event("scanner", &format!("resource-{}", i), AccessResult::Success))
            .collect();
        let violations = auditor.detect_anomalies(&events, 60);
        assert!(!violations.is_empty());
        assert!(violations.iter().any(|v| v.description.contains("scanner")));
    }

    #[test]
    fn test_detect_anomalies_both_triggers() {
        let auditor = AccessAuditor::new();
        // 11 events across 6 resources — triggers both anomaly types
        let events: Vec<_> = (0..11)
            .map(|i| make_event("attacker", &format!("res-{}", i % 6), AccessResult::Success))
            .collect();
        let violations = auditor.detect_anomalies(&events, 300);
        assert!(
            violations.len() >= 2,
            "Expected at least 2 violations, got: {}",
            violations.len()
        );
    }

    #[test]
    fn test_detect_anomalies_multi_user_no_single_exceeds_threshold() {
        let auditor = AccessAuditor::new();
        // 5 users × 2 events each = 10 events total, but no single user exceeds threshold
        let events: Vec<_> = (0..5)
            .flat_map(|u| {
                (0..2)
                    .map(move |_| make_event(&format!("user-{}", u), "db-1", AccessResult::Success))
            })
            .collect();
        let violations = auditor.detect_anomalies(&events, 60);
        assert!(violations.is_empty());
    }

    // -------------------------------------------------------------------------
    // SecurityMonitor – existing tests (unchanged)
    // -------------------------------------------------------------------------

    #[test]
    fn test_security_monitor_detect_threats_critical() {
        let monitor = SecurityMonitor::new();
        let violations = monitor.detect_threats(10);

        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "critical");
        assert!(violations[0].description.contains("10 failed operations"));
    }

    #[test]
    fn test_security_monitor_detect_threats_high() {
        let monitor = SecurityMonitor::new();
        let violations = monitor.detect_threats(7);

        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "high");
        assert!(violations[0].description.contains("7 failed operations"));
    }

    #[test]
    fn test_security_monitor_detect_threats_medium() {
        let monitor = SecurityMonitor::new();
        let violations = monitor.detect_threats(3);

        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].severity, "medium");
        assert!(violations[0].description.contains("3 failed operations"));
    }

    #[test]
    fn test_security_monitor_detect_threats_no_violations() {
        let monitor = SecurityMonitor::new();
        let violations = monitor.detect_threats(2);

        assert!(violations.is_empty());
    }

    #[test]
    fn test_security_monitor_generate_security_alert() {
        let monitor = SecurityMonitor::new();
        let violation = SecurityViolation {
            violation_id: "test-violation-123".to_string(),
            severity: "high".to_string(),
            description: "Test security violation".to_string(),
        };

        let alert = monitor.generate_security_alert(&violation);

        assert!(alert.contains("[SECURITY ALERT]"));
        assert!(alert.contains("test-violation-123"));
        assert!(alert.contains("Severity: high"));
        assert!(alert.contains("Test security violation"));
    }

    // -------------------------------------------------------------------------
    // SecurityMonitor – SIEM format methods
    // -------------------------------------------------------------------------

    fn sample_violation() -> SecurityViolation {
        SecurityViolation {
            violation_id: "viol-001".to_string(),
            severity: "high".to_string(),
            description: "Suspicious access pattern detected".to_string(),
        }
    }

    #[test]
    fn test_format_cef_structure() {
        let monitor = SecurityMonitor::new();
        let v = sample_violation();
        let cef = monitor.format_cef(&v);

        assert!(
            cef.starts_with("CEF:0|copybook-rs|SecurityMonitor|1.0|"),
            "CEF header missing: {}",
            cef
        );
        assert!(cef.contains("viol-001"), "violation_id missing: {}", cef);
        assert!(
            cef.contains("Suspicious access pattern detected"),
            "description missing: {}",
            cef
        );
        // severity_num for "high" is 7
        assert!(cef.contains('7'), "severity number missing: {}", cef);
    }

    #[test]
    fn test_format_cef_severity_mapping() {
        let monitor = SecurityMonitor::new();
        for (sev, expected_num) in [
            ("critical", "10"),
            ("high", "7"),
            ("medium", "5"),
            ("low", "3"),
            ("unknown", "5"),
        ] {
            let v = SecurityViolation {
                violation_id: "x".to_string(),
                severity: sev.to_string(),
                description: "d".to_string(),
            };
            let cef = monitor.format_cef(&v);
            assert!(
                cef.contains(expected_num),
                "severity {} should map to {}: {}",
                sev,
                expected_num,
                cef
            );
        }
    }

    #[test]
    fn test_format_leef_structure() {
        let monitor = SecurityMonitor::new();
        let v = sample_violation();
        let leef = monitor.format_leef(&v);

        assert!(
            leef.starts_with("LEEF:2.0|copybook-rs|SecurityMonitor|1.0|"),
            "LEEF header missing: {}",
            leef
        );
        assert!(leef.contains("viol-001"), "violation_id missing: {}", leef);
        assert!(
            leef.contains("severity=high"),
            "severity field missing: {}",
            leef
        );
        assert!(
            leef.contains("description=Suspicious access pattern detected"),
            "description field missing: {}",
            leef
        );
        // LEEF key-value pairs are tab-separated
        assert!(
            leef.contains('\t'),
            "LEEF should use tab separator: {}",
            leef
        );
    }

    #[test]
    fn test_format_syslog_structure() {
        let monitor = SecurityMonitor::new();
        let v = sample_violation();
        let syslog = monitor.format_syslog(&v);

        // Must start with <priority>version
        assert!(
            syslog.starts_with('<'),
            "Syslog should start with <priority>: {}",
            syslog
        );
        assert!(
            syslog.contains("copybook-rs"),
            "hostname/app missing: {}",
            syslog
        );
        assert!(
            syslog.contains("SecurityMonitor"),
            "app-name missing: {}",
            syslog
        );
        assert!(
            syslog.contains("viol-001"),
            "violation_id missing: {}",
            syslog
        );
        assert!(
            syslog.contains("Suspicious access pattern detected"),
            "description missing: {}",
            syslog
        );
    }

    #[test]
    fn test_format_syslog_priority() {
        let monitor = SecurityMonitor::new();
        let v = sample_violation();
        let syslog = monitor.format_syslog(&v);
        // Priority <132> = facility 16 * 8 + severity 4
        assert!(syslog.starts_with("<132>"), "Wrong priority: {}", syslog);
    }

    // -------------------------------------------------------------------------
    // EventMonitor
    // -------------------------------------------------------------------------

    #[test]
    fn test_event_monitor_no_alert_below_threshold() {
        let mut monitor = EventMonitor::new(5);
        // Send exactly 5 events — threshold not crossed
        for i in 0..5 {
            let event = make_event("alice", &format!("res-{}", i), AccessResult::Success);
            let alert = monitor.process_event(&event);
            assert!(
                alert.is_none(),
                "Should not alert before threshold: i={}",
                i
            );
        }
        assert!(monitor.pending_alerts().is_empty());
    }

    #[test]
    fn test_event_monitor_alert_at_threshold_plus_one() {
        let mut monitor = EventMonitor::new(5);
        // Events 0..5 (five events) — no alert
        for i in 0..5 {
            let event = make_event("alice", &format!("res-{}", i), AccessResult::Success);
            monitor.process_event(&event);
        }
        // 6th event crosses the threshold
        let event = make_event("alice", "res-5", AccessResult::Success);
        let alert = monitor.process_event(&event);
        assert!(alert.is_some(), "Should alert when threshold is crossed");
        assert_eq!(monitor.pending_alerts().len(), 1);
    }

    #[test]
    fn test_event_monitor_alert_contains_threshold_info() {
        let mut monitor = EventMonitor::new(5);
        for i in 0..=5 {
            let event = make_event("alice", &format!("res-{}", i), AccessResult::Success);
            monitor.process_event(&event);
        }
        let alerts = monitor.pending_alerts();
        assert_eq!(alerts.len(), 1);
        assert!(
            alerts[0].description.contains('5'),
            "Description should mention threshold: {}",
            alerts[0].description
        );
        assert_eq!(alerts[0].severity, "high");
    }

    #[test]
    fn test_event_monitor_only_one_alert_per_threshold_crossing() {
        let mut monitor = EventMonitor::new(3);
        // Cross threshold at event 4 (index 3)
        for i in 0..10 {
            let event = make_event("alice", &format!("res-{}", i), AccessResult::Success);
            monitor.process_event(&event);
        }
        // Only one alert should have been emitted (the crossing)
        assert_eq!(monitor.pending_alerts().len(), 1);
    }

    #[test]
    fn test_event_monitor_default_threshold() {
        // Default threshold documented as 5
        let monitor = EventMonitor::new(5);
        assert_eq!(monitor.alert_threshold, 5);
    }

    #[test]
    fn test_event_monitor_pending_alerts_initially_empty() {
        let monitor = EventMonitor::new(5);
        assert!(monitor.pending_alerts().is_empty());
    }
}
