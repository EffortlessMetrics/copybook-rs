// SPDX-License-Identifier: AGPL-3.0-or-later
//! Audit Logger Framework
//!
//! Provides structured logging and audit trail management with cryptographic
//! integrity, enterprise format integration, and automated retention policies.

use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::fs::{File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use super::{AuditError, AuditEvent, AuditResult, validate_audit_chain};

/// Enterprise audit logger with cryptographic integrity and retention management
pub struct AuditLogger {
    config: AuditLoggerConfig,
    event_buffer: Arc<Mutex<VecDeque<AuditEvent>>>,
    file_writer: Option<Arc<Mutex<BufWriter<File>>>>,
}

impl AuditLogger {
    /// Create a new audit logger with configuration
    pub fn new(config: AuditLoggerConfig) -> AuditResult<Self> {
        let file_writer = if let Some(ref log_file) = config.log_file_path {
            let file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(log_file)?;
            Some(Arc::new(Mutex::new(BufWriter::new(file))))
        } else {
            None
        };

        Ok(Self {
            config,
            event_buffer: Arc::new(Mutex::new(VecDeque::new())),
            file_writer,
        })
    }

    /// Log an audit event with integrity chain validation
    pub fn log_event(&self, mut event: AuditEvent) -> AuditResult<()> {
        // Get the previous event hash for chain integrity
        if let Some(previous_hash) = self.get_last_event_hash()? {
            event = event.with_previous_hash(previous_hash);
        }

        // Add to buffer
        {
            let mut buffer = self
                .event_buffer
                .lock()
                .map_err(|_| AuditError::Configuration {
                    message: "Failed to acquire buffer lock".to_string(),
                })?;

            buffer.push_back(event.clone());

            // Enforce buffer size limit
            while buffer.len() > self.config.buffer_size {
                buffer.pop_front();
            }
        }

        // Write to file if configured
        if let Some(ref writer) = self.file_writer {
            self.write_event_to_file(writer, &event)?;
        }

        // Send to external systems if configured
        if !self.config.external_endpoints.is_empty() {
            self.send_to_external_systems(&event)?;
        }

        Ok(())
    }

    /// Get the last event hash for chain integrity
    fn get_last_event_hash(&self) -> AuditResult<Option<String>> {
        let buffer = self
            .event_buffer
            .lock()
            .map_err(|_| AuditError::Configuration {
                message: "Failed to acquire buffer lock".to_string(),
            })?;

        Ok(buffer.back().map(|event| event.integrity_hash.clone()))
    }

    /// Write event to log file
    fn write_event_to_file(
        &self,
        writer: &Arc<Mutex<BufWriter<File>>>,
        event: &AuditEvent,
    ) -> AuditResult<()> {
        let formatted_event = match self.config.format {
            LogFormat::Json => serde_json::to_string(event)?,
            LogFormat::JsonLines => format!("{}\n", serde_json::to_string(event)?),
            LogFormat::CEF => self.format_as_cef(event)?,
            LogFormat::Syslog => self.format_as_syslog(event)?,
        };

        let mut file_writer = writer.lock().map_err(|_| AuditError::Configuration {
            message: "Failed to acquire file writer lock".to_string(),
        })?;

        file_writer.write_all(formatted_event.as_bytes())?;
        file_writer.flush()?;

        Ok(())
    }

    /// Format event as Common Event Format (CEF) for SIEM integration
    fn format_as_cef(&self, event: &AuditEvent) -> AuditResult<String> {
        // CEF format: CEF:Version|Device Vendor|Device Product|Device Version|Device Event Class ID|Name|Severity|[Extension]
        let event_class_id = format!("{:?}", event.event_type);
        let event_name = match &event.payload {
            super::AuditPayload::SecurityEvent { .. } => "SecurityEvent",
            super::AuditPayload::ComplianceCheck { .. } => "ComplianceEvent",
            super::AuditPayload::PerformanceMeasurement { .. } => "PerformanceEvent",
            super::AuditPayload::CopybookParse { .. } => "CopybookEvent",
            super::AuditPayload::DataValidation { .. } => "ValidationEvent",
            super::AuditPayload::DataTransformation { .. } => "TransformationEvent",
            super::AuditPayload::LineageTracking { .. } => "LineageEvent",
            super::AuditPayload::ErrorEvent { .. } => "ErrorEvent",
            super::AuditPayload::AccessEvent { .. } => "AccessEvent",
            super::AuditPayload::ConfigurationChange { .. } => "ConfigurationEvent",
        };

        let cef_event = format!(
            "CEF:0|copybook-rs|Enterprise Audit|{}|{}|{}|{}|src={} cs1Label=Operation cs1={} cs2Label=Context cs2={} cn1Label=Severity cn1={}\n",
            env!("CARGO_PKG_VERSION"),
            event_class_id,
            event_name,
            self.map_severity_to_cef(event.severity),
            event.source,
            event.context.operation_id,
            serde_json::to_string(&event.context).unwrap_or_default(),
            event.severity as u32,
        );

        Ok(cef_event)
    }

    /// Format event as Syslog format
    fn format_as_syslog(&self, event: &AuditEvent) -> AuditResult<String> {
        // RFC5424 Syslog format
        let priority = self.map_severity_to_syslog_priority(event.severity);
        let hostname = &event.context.environment.hostname;
        let app_name = "copybook-audit";
        let process_id = event.context.environment.process_id;

        let syslog_event = format!(
            "<{}>{} {} {} {} - - {}\n",
            priority,
            event.timestamp,
            hostname,
            app_name,
            process_id,
            serde_json::to_string(event)?,
        );

        Ok(syslog_event)
    }

    /// Map audit severity to CEF severity
    fn map_severity_to_cef(&self, severity: super::event::AuditSeverity) -> u32 {
        match severity {
            super::event::AuditSeverity::Info => 2,
            super::event::AuditSeverity::Low => 3,
            super::event::AuditSeverity::Medium => 6,
            super::event::AuditSeverity::High => 8,
            super::event::AuditSeverity::Critical => 10,
        }
    }

    /// Map audit severity to Syslog priority
    fn map_severity_to_syslog_priority(&self, severity: super::event::AuditSeverity) -> u32 {
        // Facility 16 (local0) + Severity
        let facility = 16 * 8; // local0 = 16, shifted left 3 bits
        let sev = match severity {
            super::event::AuditSeverity::Info => 6,     // info
            super::event::AuditSeverity::Low => 5,      // notice
            super::event::AuditSeverity::Medium => 4,   // warning
            super::event::AuditSeverity::High => 3,     // err
            super::event::AuditSeverity::Critical => 2, // crit
        };

        facility + sev
    }

    /// Send audit events to external systems
    fn send_to_external_systems(&self, event: &AuditEvent) -> AuditResult<()> {
        for endpoint in &self.config.external_endpoints {
            if let Err(e) = self.send_to_endpoint(endpoint, event) {
                eprintln!("Failed to send audit event to {:?}: {}", endpoint, e);
                // Continue with other endpoints rather than failing entirely
            }
        }
        Ok(())
    }

    /// Send audit event to specific endpoint
    fn send_to_endpoint(&self, endpoint: &ExternalEndpoint, event: &AuditEvent) -> AuditResult<()> {
        match endpoint {
            ExternalEndpoint::Http { url, headers } => self.send_http(url, headers, event),
            ExternalEndpoint::Syslog { host, port } => self.send_syslog(host, *port, event),
            ExternalEndpoint::Kafka { brokers, topic } => self.send_kafka(brokers, topic, event),
        }
    }

    /// Send audit event via HTTP
    fn send_http(
        &self,
        url: &str,
        _headers: &std::collections::HashMap<String, String>,
        event: &AuditEvent,
    ) -> AuditResult<()> {
        // HTTP client implementation would go here
        // For now, just log the attempt
        println!(
            "Would send audit event {} to HTTP endpoint {}",
            event.event_id, url
        );
        Ok(())
    }

    /// Send audit event via Syslog
    fn send_syslog(&self, host: &str, port: u16, event: &AuditEvent) -> AuditResult<()> {
        // Syslog client implementation would go here
        println!(
            "Would send audit event {} to Syslog {}:{}",
            event.event_id, host, port
        );
        Ok(())
    }

    /// Send audit event via Kafka
    fn send_kafka(&self, brokers: &[String], topic: &str, event: &AuditEvent) -> AuditResult<()> {
        // Kafka client implementation would go here
        println!(
            "Would send audit event {} to Kafka topic {} via brokers {:?}",
            event.event_id, topic, brokers
        );
        Ok(())
    }

    /// Validate audit trail integrity
    pub fn validate_integrity(&self) -> AuditResult<bool> {
        let buffer = self
            .event_buffer
            .lock()
            .map_err(|_| AuditError::Configuration {
                message: "Failed to acquire buffer lock".to_string(),
            })?;

        let events: Vec<AuditEvent> = buffer.iter().cloned().collect();
        validate_audit_chain(&events)
    }

    /// Get audit events within time range
    pub fn get_events_in_range(
        &self,
        start_time: chrono::DateTime<chrono::Utc>,
        end_time: chrono::DateTime<chrono::Utc>,
    ) -> AuditResult<Vec<AuditEvent>> {
        let buffer = self
            .event_buffer
            .lock()
            .map_err(|_| AuditError::Configuration {
                message: "Failed to acquire buffer lock".to_string(),
            })?;

        let events = buffer
            .iter()
            .filter(|event| {
                if let Ok(event_time) = chrono::DateTime::parse_from_rfc3339(&event.timestamp) {
                    let event_utc = event_time.with_timezone(&chrono::Utc);
                    event_utc >= start_time && event_utc <= end_time
                } else {
                    false
                }
            })
            .cloned()
            .collect();

        Ok(events)
    }

    /// Perform log rotation based on retention policy
    pub fn rotate_logs(&self) -> AuditResult<()> {
        if let Some(ref retention) = self.config.retention_policy {
            self.enforce_retention_policy(retention)?;
        }

        if let Some(ref log_file) = self.config.log_file_path {
            self.rotate_log_file(log_file)?;
        }

        Ok(())
    }

    /// Enforce retention policy
    fn enforce_retention_policy(&self, retention: &RetentionPolicy) -> AuditResult<()> {
        let cutoff_time =
            chrono::Utc::now() - chrono::Duration::days(retention.retention_days as i64);

        let mut buffer = self
            .event_buffer
            .lock()
            .map_err(|_| AuditError::Configuration {
                message: "Failed to acquire buffer lock".to_string(),
            })?;

        // Remove events older than retention period
        buffer.retain(|event| {
            if let Ok(event_time) = chrono::DateTime::parse_from_rfc3339(&event.timestamp) {
                event_time.with_timezone(&chrono::Utc) >= cutoff_time
            } else {
                true // Keep events with invalid timestamps for manual review
            }
        });

        Ok(())
    }

    /// Rotate log file
    fn rotate_log_file(&self, log_file: &Path) -> AuditResult<()> {
        // Simple rotation: rename current file with timestamp suffix
        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
        let rotated_name = format!("{}.{}", log_file.display(), timestamp);

        if log_file.exists() {
            std::fs::rename(log_file, &rotated_name)?;
        }

        // Recreate the log file
        if let Some(ref writer) = self.file_writer {
            let new_file = File::create(log_file)?;
            let mut writer_guard = writer.lock().map_err(|_| AuditError::Configuration {
                message: "Failed to acquire file writer lock".to_string(),
            })?;
            *writer_guard = BufWriter::new(new_file);
        }

        Ok(())
    }
}

/// Audit logger configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditLoggerConfig {
    /// Log file path (None for memory-only logging)
    pub log_file_path: Option<PathBuf>,

    /// Log format for output
    pub format: LogFormat,

    /// Buffer size for in-memory events
    pub buffer_size: usize,

    /// Retention policy for log rotation
    pub retention_policy: Option<RetentionPolicy>,

    /// External endpoints for audit event forwarding
    pub external_endpoints: Vec<ExternalEndpoint>,

    /// Enable real-time event forwarding
    pub real_time_forwarding: bool,

    /// Batch size for event forwarding
    pub batch_size: usize,

    /// Flush interval in seconds
    pub flush_interval_seconds: u64,
}

impl Default for AuditLoggerConfig {
    fn default() -> Self {
        Self {
            log_file_path: Some(PathBuf::from("audit.jsonl")),
            format: LogFormat::JsonLines,
            buffer_size: 10000,
            retention_policy: Some(RetentionPolicy::default()),
            external_endpoints: Vec::new(),
            real_time_forwarding: false,
            batch_size: 100,
            flush_interval_seconds: 60,
        }
    }
}

/// Log output formats
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum LogFormat {
    /// Standard JSON format
    Json,
    /// JSON Lines format (one JSON object per line)
    JsonLines,
    /// Common Event Format (CEF) for SIEM integration
    CEF,
    /// Syslog format (RFC5424)
    Syslog,
}

/// Log retention policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetentionPolicy {
    /// Number of days to retain audit logs
    pub retention_days: u32,

    /// Maximum log file size in MB before rotation
    pub max_file_size_mb: u64,

    /// Maximum number of rotated files to keep
    pub max_rotated_files: u32,

    /// Compress rotated files
    pub compress_rotated: bool,

    /// Archive location for long-term storage
    pub archive_path: Option<PathBuf>,
}

impl Default for RetentionPolicy {
    fn default() -> Self {
        Self {
            retention_days: 2555, // 7 years for SOX compliance
            max_file_size_mb: 100,
            max_rotated_files: 50,
            compress_rotated: true,
            archive_path: None,
        }
    }
}

/// External endpoints for audit event forwarding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExternalEndpoint {
    Http {
        url: String,
        headers: std::collections::HashMap<String, String>,
    },
    Syslog {
        host: String,
        port: u16,
    },
    Kafka {
        brokers: Vec<String>,
        topic: String,
    },
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;
    use crate::audit::event::{ParseResult, SecurityEventType};
    use crate::audit::{AuditContext, AuditEvent, AuditEventType, AuditPayload};
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_audit_logger_creation() {
        let config = AuditLoggerConfig::default();
        let logger = AuditLogger::new(config).expect("Failed to create audit logger");

        // Should be able to create logger with default config
        assert!(
            logger
                .event_buffer
                .lock()
                .expect("Lock should work")
                .is_empty()
        );
    }

    #[tokio::test]
    async fn test_event_logging() {
        let dir = tempdir().expect("Failed to create temp directory");
        let log_path = dir.path().join("test_audit.jsonl");

        let config = AuditLoggerConfig {
            log_file_path: Some(log_path.clone()),
            format: LogFormat::JsonLines,
            buffer_size: 100,
            ..Default::default()
        };

        let logger = AuditLogger::new(config).expect("Failed to create audit logger");

        // Create a test audit event
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

        // Log the event
        logger.log_event(event).expect("Failed to log event");

        // Verify event was buffered
        let buffer = logger.event_buffer.lock().expect("Lock should work");
        assert_eq!(buffer.len(), 1);

        // Verify event was written to file
        assert!(log_path.exists());
        let file_content = std::fs::read_to_string(&log_path).expect("Failed to read log file");
        assert!(file_content.contains("CopybookParse"));
    }

    #[tokio::test]
    async fn test_integrity_validation() {
        let config = AuditLoggerConfig::default();
        let logger = AuditLogger::new(config).expect("Failed to create audit logger");

        // Add multiple events to establish chain
        let context = AuditContext::new();

        for i in 0..3 {
            let payload = AuditPayload::CopybookParse {
                copybook_path: format!("test{}.cpy", i),
                schema_fingerprint: format!("abc{}", i),
                parse_result: ParseResult::Success,
                parsing_duration_ms: 100,
                field_count: 10,
                level_88_count: 2,
                error_count: 0,
                warnings: vec![],
            };

            let event = AuditEvent::new(AuditEventType::CopybookParse, context.clone(), payload);

            logger.log_event(event).expect("Failed to log event");
        }

        // Validate chain integrity
        let is_valid = logger
            .validate_integrity()
            .expect("Failed to validate integrity");
        assert!(is_valid);
    }

    #[test]
    fn test_cef_formatting() {
        let config = AuditLoggerConfig {
            format: LogFormat::CEF,
            ..Default::default()
        };

        let logger = AuditLogger::new(config).expect("Failed to create audit logger");

        let context = AuditContext::new();
        let payload = AuditPayload::SecurityEvent {
            security_event_type: SecurityEventType::UnauthorizedAccess,
            severity: "High".to_string(),
            affected_resources: vec!["customer_data".to_string()],
            threat_indicators: vec![],
            remediation_actions: vec![],
            incident_id: Some("INC-001".to_string()),
        };

        let event = AuditEvent::new(AuditEventType::SecurityEvent, context, payload);

        let cef_format = logger
            .format_as_cef(&event)
            .expect("Failed to format as CEF");
        assert!(cef_format.starts_with("CEF:0|copybook-rs|Enterprise Audit|"));
        assert!(cef_format.contains("SecurityEvent"));
    }

    #[test]
    fn test_retention_policy_default() {
        let policy = RetentionPolicy::default();
        assert_eq!(policy.retention_days, 2555); // 7 years for SOX compliance
        assert!(policy.compress_rotated);
        assert_eq!(policy.max_rotated_files, 50);
    }
}
