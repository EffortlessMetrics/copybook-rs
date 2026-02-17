//! Enterprise Audit Command Implementation
//!
//! Provides comprehensive audit capabilities through CLI commands including
//! compliance validation, performance assessment, security auditing, and
//! complete data lineage reporting.

use crate::exit_codes::ExitCode;
use crate::{write_stderr_line, write_stdout_line};
use crate::utils::atomic_write;
use chrono::{self, DateTime, Duration as ChronoDuration, Utc};
use clap::{Parser, Subcommand};
use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RecordFormat, RawMode, RunSummary,
};
use copybook_core::audit::{
    AccessAuditor, AccessEvent, AccessResult, AuditContext, AuditEvent, AuditEventType, AuditLogger,
    AuditLoggerConfig, AuditPayload, BaselineManager, ComplianceConfig, ComplianceEngine,
    ComplianceProfile, ComplianceResult, FieldLineage, ImpactAnalyzer, LogFormat, PerformanceAuditor,
    PerformanceBaseline, ResourceMetrics, SecurityAuditor, SecurityMonitor, SecurityViolation,
    ThroughputMetrics, TransformationType, RiskLevel,
};
use copybook_core::{
    audit::{
        self as audit_core,
        context::SecurityClassification,
        PerformanceMeasurementType,
        PerformanceMetrics,
        SecurityEventType,
        ComparisonResult,
    },
    Field, FieldKind, Schema,
};
use serde::{Deserialize, Serialize};
use serde_json::{self, Value};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::{self, ErrorKind, BufRead, BufReader, Cursor, Read, Write as IoWrite};
use std::path::Path;
use std::path::PathBuf;
use std::{fs, str};

type AuditResult<T> = Result<T, Box<dyn std::error::Error + Send + Sync>>;

/// Enterprise audit command with comprehensive regulatory compliance
#[derive(Parser)]
#[command(
    about = "Enterprise audit for regulatory compliance and security monitoring",
    long_about = "Comprehensive audit capabilities for copybook-rs including SOX, HIPAA, \
                   GDPR compliance validation, performance auditing, security monitoring, \
                   and data lineage tracking for enterprise governance."
)]
pub struct AuditCommand {
    #[command(subcommand)]
    pub command: AuditSubcommand,

    /// Enable verbose audit logging
    #[arg(short, long)]
    pub verbose: bool,

    /// Audit output file (JSON format)
    #[arg(long)]
    pub audit_output: Option<PathBuf>,

    /// Enable real-time audit monitoring
    #[arg(long)]
    pub monitor: bool,
}

#[derive(Subcommand)]
pub enum AuditSubcommand {
    /// Generate comprehensive audit reports
    #[command(
        about = "Generate comprehensive audit reports for enterprise compliance",
        long_about = "Creates detailed audit reports covering compliance status, \
                      performance metrics, security assessment, and data lineage \
                      for regulatory requirements and enterprise governance."
    )]
    Report {
        /// Copybook file path for analysis
        copybook: PathBuf,

        /// Data file path for processing audit
        data_file: Option<PathBuf>,

        /// Output report file
        #[arg(short, long)]
        output: PathBuf,

        /// Report format
        #[arg(long, value_enum, default_value = "json")]
        format: OutputFormat,

        /// Compliance frameworks to include in report
        #[arg(long)]
        compliance: Option<String>,

        /// Include performance analysis
        #[arg(long)]
        include_performance: bool,

        /// Include security assessment
        #[arg(long)]
        include_security: bool,

        /// Include data lineage
        #[arg(long)]
        include_lineage: bool,

        /// Include remediation recommendations
        #[arg(long)]
        include_recommendations: bool,
    },

    /// Validate compliance against regulatory frameworks
    #[command(
        about = "Validate operations against regulatory compliance requirements",
        long_about = "Validates copybook parsing and data processing operations \
                      against SOX, HIPAA, GDPR, and PCI DSS compliance requirements \
                      with detailed violation reporting and remediation guidance."
    )]
    Validate {
        /// Copybook file path
        copybook: PathBuf,

        /// Data file path for validation
        data_file: Option<PathBuf>,

        /// Compliance frameworks to validate against (comma-separated)
        #[arg(long)]
        compliance: String,

        /// Record format
        #[arg(long)]
        format: Option<RecordFormat>,

        /// Character encoding
        #[arg(long, default_value = "cp037")]
        codepage: Codepage,

        /// Enable strict compliance validation
        #[arg(long)]
        strict: bool,

        /// Output validation report
        #[arg(short, long)]
        output: PathBuf,

        /// Auto-remediation mode
        #[arg(long)]
        auto_remediate: bool,

        /// Report compliance violations in detail
        #[arg(long)]
        report_violations: bool,

        /// Include remediation recommendations
        #[arg(long)]
        include_recommendations: bool,
    },

    /// Analyze data lineage and transformation impact
    #[command(
        about = "Analyze data lineage and transformation impact",
        long_about = "Tracks field-level data lineage, transformation rules, \
                      and analyzes the impact of schema or processing changes \
                      on downstream systems and data consumers."
    )]
    Lineage {
        /// Source copybook file
        source_copybook: PathBuf,

        /// Target copybook file (optional)
        target_copybook: Option<PathBuf>,

        /// Source system identifier
        #[arg(long)]
        source_system: String,

        /// Target system identifier
        #[arg(long)]
        target_system: Option<String>,

        /// Output lineage report
        #[arg(short, long)]
        output: PathBuf,

        /// Source copybook for comparison
        #[arg(long)]
        source: Option<PathBuf>,

        /// Target format for lineage report
        #[arg(long, default_value = "json")]
        target_format: String,

        /// Enable field-level lineage tracking
        #[arg(long)]
        field_level: bool,

        /// Include transformation details
        #[arg(long)]
        transformation_details: bool,

        /// Include data quality metrics
        #[arg(long)]
        quality_metrics: bool,

        /// Include impact analysis
        #[arg(long)]
        impact_analysis: bool,

        /// Transformation confidence threshold
        #[arg(long, default_value = "0.8")]
        confidence_threshold: f64,
    },

    /// Run performance audit and baseline validation
    #[command(
        about = "Run performance audit and baseline validation",
        long_about = "Validates processing performance against enterprise baselines, \
                      detects performance regressions, and generates performance \
                      audit reports with recommendations for optimization."
    )]
    Performance {
        /// Copybook file path
        copybook: PathBuf,

        /// Test data file path
        data_file: Option<PathBuf>,

        /// Record format
        #[arg(long)]
        format: Option<RecordFormat>,

        /// Character encoding
        #[arg(long, default_value = "cp037")]
        codepage: Codepage,

        /// Output performance report
        #[arg(short, long)]
        output: PathBuf,

        /// Establish new performance baseline
        #[arg(long)]
        establish_baseline: bool,

        /// Baseline file path
        #[arg(long)]
        baseline_file: Option<PathBuf>,

        /// Validate against existing baseline
        #[arg(long)]
        validate_against_baseline: Option<PathBuf>,

        /// Target DISPLAY throughput in GiB/s
        #[arg(long)]
        target_display_gbps: Option<f64>,

        /// Target COMP-3 throughput in MiB/s
        #[arg(long)]
        target_comp3_mbps: Option<f64>,

        /// Maximum overhead percentage
        #[arg(long)]
        max_overhead_percent: Option<f64>,

        /// Include regression analysis
        #[arg(long)]
        include_regression_analysis: bool,

        /// Performance test iterations
        #[arg(long, default_value = "10")]
        iterations: u32,
    },

    /// Run security audit and access pattern analysis
    #[command(
        about = "Run security audit and access pattern analysis",
        long_about = "Performs comprehensive security auditing including access \
                      control validation, encryption status checking, and \
                      anomaly detection for copybook processing operations."
    )]
    Security {
        /// Copybook file path
        copybook: PathBuf,

        /// Data classification level
        #[arg(long, value_enum)]
        classification: Option<DataClassification>,

        /// Output security report
        #[arg(short, long)]
        output: PathBuf,

        /// Access log file for analysis
        #[arg(long)]
        access_log: Option<PathBuf>,

        /// Enable anomaly detection
        #[arg(long)]
        detect_anomalies: bool,

        /// Validate encryption status
        #[arg(long)]
        validate_encryption: bool,

        /// Check access patterns
        #[arg(long)]
        check_access_patterns: bool,

        /// SIEM format for export
        #[arg(long)]
        siem_format: Option<String>,

        /// SIEM vendor integration
        #[arg(long)]
        siem_vendor: Option<String>,

        /// Export events file path
        #[arg(long)]
        export_events: Option<PathBuf>,

        /// Enable real-time monitoring
        #[arg(long)]
        real_time_monitoring: bool,

        /// Security validation depth
        #[arg(long, value_enum, default_value = "standard")]
        validation_depth: ValidationDepth,

        /// Include threat assessment
        #[arg(long)]
        threat_assessment: bool,
    },

    /// Monitor audit trail health and integrity
    #[command(
        about = "Monitor audit trail health and integrity",
        long_about = "Validates audit trail integrity using cryptographic hashes, \
                      checks for audit trail tampering, and monitors audit \
                      system health including retention compliance."
    )]
    Health {
        /// Audit trail file path
        audit_trail: Option<PathBuf>,

        /// Audit log file path
        #[arg(long)]
        audit_log: Option<PathBuf>,

        /// Validate integrity chain
        #[arg(long)]
        validate_integrity: bool,

        /// Validate chain integrity
        #[arg(long)]
        validate_chain_integrity: bool,

        /// Check cryptographic hashes
        #[arg(long)]
        check_cryptographic_hashes: bool,

        /// Verify timestamps
        #[arg(long)]
        verify_timestamps: bool,

        /// Check retention compliance
        #[arg(long)]
        check_retention: bool,

        /// Output health report
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Enable detailed diagnostics
        #[arg(long)]
        detailed_diagnostics: bool,

        /// Health check interval in minutes
        #[arg(long, default_value = "60")]
        check_interval: u32,

        /// Enable continuous monitoring
        #[arg(long)]
        continuous: bool,
    },
}

#[derive(clap::ValueEnum, Clone)]
pub enum ReportType {
    Comprehensive,
    Compliance,
    Performance,
    Security,
    Lineage,
}

#[derive(clap::ValueEnum, Clone)]
pub enum OutputFormat {
    Json,
    Html,
    Pdf,
    Csv,
}

#[derive(clap::ValueEnum, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum ComplianceFramework {
    SOX,
    HIPAA,
    GDPR,
    PCI,
}

impl From<ComplianceFramework> for ComplianceProfile {
    #[inline]
    fn from(framework: ComplianceFramework) -> Self {
        match framework {
            ComplianceFramework::SOX => ComplianceProfile::SOX,
            ComplianceFramework::HIPAA => ComplianceProfile::HIPAA,
            ComplianceFramework::GDPR => ComplianceProfile::GDPR,
            ComplianceFramework::PCI => ComplianceProfile::PciDss,
        }
    }
}

#[derive(clap::ValueEnum, Clone, Debug, serde::Serialize)]
#[allow(clippy::upper_case_acronyms)]
pub enum DataClassification {
    Public,
    Internal,
    Confidential,
    MaterialTransaction,
    PHI,
}

#[derive(clap::ValueEnum, Clone, Copy, Debug, serde::Serialize)]
pub enum ValidationDepth {
    Basic,
    Standard,
    Comprehensive,
    Forensic,
}

/// Run the audit command with comprehensive enterprise capabilities
#[allow(clippy::too_many_lines)]
pub async fn run(
    audit_command: AuditCommand,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    // Initialize audit context
    let audit_context = AuditContext::new()
        .with_operation_id("cli_audit_operation")
        .with_user("cli_user");

    // Initialize audit logger if requested
    let _audit_logger = if audit_command.audit_output.is_some() {
        let config = AuditLoggerConfig {
            log_file_path: audit_command.audit_output.clone(),
            ..Default::default()
        };
        Some(AuditLogger::new(config)?)
    } else {
        None
    };

    match audit_command.command {
        AuditSubcommand::Report {
            copybook,
            data_file,
            output,
            format,
            compliance,
        include_performance,
        include_security,
        include_lineage,
        include_recommendations,
        } => {
            run_audit_report(
            &copybook,
            data_file.as_deref(),
            &output,
            format,
            compliance.as_deref(),
            include_performance,
            include_security,
            include_lineage,
            include_recommendations,
            audit_context,
            )
            .await
        }

        AuditSubcommand::Validate {
            copybook,
            data_file,
            compliance,
            format,
            codepage,
            strict,
            output,
            auto_remediate,
            report_violations,
            include_recommendations,
        } => {
            run_compliance_validation(
                &compliance,
                &copybook,
                data_file.as_deref(),
                format,
                codepage,
                strict,
                &output,
                auto_remediate,
                report_violations,
                include_recommendations,
                audit_context,
            )
            .await
        }

        AuditSubcommand::Lineage {
            source_copybook,
            target_copybook,
            source_system,
            target_system,
            output,
            source,
            target_format,
            field_level,
            transformation_details,
            quality_metrics,
            impact_analysis,
            confidence_threshold,
        } => run_lineage_analysis(
            &source_copybook,
            target_copybook.as_deref(),
            &source_system,
            target_system.as_deref(),
            &output,
            source.as_deref(),
            &target_format,
            field_level,
            transformation_details,
            quality_metrics,
            impact_analysis,
            confidence_threshold,
            audit_context,
        ),

        AuditSubcommand::Performance {
            copybook,
            data_file,
            format,
            codepage,
            output,
            establish_baseline,
            baseline_file,
            validate_against_baseline,
            target_display_gbps,
            target_comp3_mbps,
            max_overhead_percent,
            include_regression_analysis,
            iterations,
        } => run_performance_audit(
            &copybook,
            data_file.as_deref(),
            format,
            codepage,
            &output,
            establish_baseline,
            baseline_file.as_deref(),
            validate_against_baseline.as_deref(),
            target_display_gbps,
            target_comp3_mbps,
            max_overhead_percent,
            include_regression_analysis,
            iterations,
            audit_context,
        ),

        AuditSubcommand::Security {
            copybook,
            classification,
            output,
            access_log,
            detect_anomalies,
            validate_encryption,
            check_access_patterns,
            siem_format,
            siem_vendor,
            export_events,
            real_time_monitoring,
            validation_depth,
            threat_assessment,
        } => run_security_audit(
            &copybook,
            classification,
            &output,
            access_log.as_deref(),
            detect_anomalies,
            validate_encryption,
            check_access_patterns,
            siem_format.as_deref(),
            siem_vendor.as_deref(),
            export_events.as_deref(),
            real_time_monitoring,
            validation_depth,
            threat_assessment,
            audit_context,
        ),

        AuditSubcommand::Health {
            audit_trail,
            audit_log,
            validate_integrity,
            validate_chain_integrity,
            check_cryptographic_hashes,
            verify_timestamps,
            check_retention,
            output,
            detailed_diagnostics,
            check_interval,
            continuous,
        } => run_audit_health_check(
            audit_trail.as_deref(),
            audit_log.as_deref(),
            validate_integrity,
            validate_chain_integrity,
            check_cryptographic_hashes,
            verify_timestamps,
            check_retention,
            output.as_deref(),
            detailed_diagnostics,
            check_interval,
            continuous,
            audit_context,
        ),
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
#[derive(Deserialize)]
struct RawAccessEvent {
    #[serde(default)]
    user: Option<String>,
    #[serde(default)]
    user_id: Option<String>,
    #[serde(default)]
    action: Option<String>,
    #[serde(default)]
    access_type: Option<String>,
    #[serde(default)]
    resource_type: Option<String>,
    #[serde(default)]
    resource: Option<String>,
    #[serde(default)]
    resource_id: Option<String>,
    #[serde(default)]
    result: Option<String>,
    #[serde(default)]
    status: Option<String>,
    #[serde(default)]
    source_ip: Option<String>,
    #[serde(default)]
    user_agent: Option<String>,
    #[serde(default)]
    source_ip_address: Option<String>,
}

#[derive(Deserialize)]
struct RawHealthEvent {
    #[serde(default)]
    event_id: Option<String>,
    #[serde(default)]
    timestamp: Option<String>,
    #[serde(default)]
    source: Option<String>,
    #[serde(default)]
    event_type: Option<String>,
    #[serde(default)]
    integrity_hash: Option<String>,
    #[serde(default)]
    previous_hash: Option<String>,
}

#[derive(Serialize, Deserialize)]
struct HealthEventRecord {
    event_id: String,
    timestamp: String,
    source: String,
    event_type: String,
    integrity_hash: String,
    previous_hash: Option<String>,
}

#[derive(Deserialize)]
struct ReportSectionFile {
    #[serde(default)]
    audit_report: Option<Value>,
    #[serde(default)]
    compliance_validation: Option<Value>,
    #[serde(default)]
    lineage_analysis: Option<Value>,
    #[serde(default)]
    performance_audit: Option<Value>,
    #[serde(default)]
    security_audit: Option<Value>,
    #[serde(default)]
    audit_health: Option<Value>,
}

fn parse_health_events(path: &Path) -> AuditResult<(Vec<HealthEventRecord>, Vec<String>)> {
    let content = fs::read_to_string(path)?;
    let mut parse_issues = Vec::new();

    if content.trim().is_empty() {
        return Ok((Vec::new(), parse_issues));
    }

    if let Ok(mut events) = serde_json::from_str::<Vec<RawHealthEvent>>(&content) {
        let mut parsed = Vec::with_capacity(events.len());
        for (index, raw_event) in events.drain(..).enumerate() {
            match normalize_health_event(raw_event) {
                Ok(event) => parsed.push(event),
                Err(err) => parse_issues.push(format!(
                    "entry {}: failed to normalize audit health event: {err}",
                    index + 1
                )),
            }
        }
        return Ok((parsed, parse_issues));
    }

    let mut parsed = Vec::new();
    for (line_number, raw_line) in content.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }

        match serde_json::from_str::<RawHealthEvent>(line) {
            Ok(raw_event) => match normalize_health_event(raw_event) {
                Ok(event) => parsed.push(event),
                Err(err) => parse_issues.push(format!(
                    "line {}: failed to normalize audit health event: {err}",
                    line_number + 1
                )),
            },
            Err(err) => parse_issues.push(format!(
                "line {}: invalid health event JSON: {err}",
                line_number + 1
            )),
        }
    }

    Ok((parsed, parse_issues))
}

fn parse_audit_events_for_health(
    path: &Path,
) -> AuditResult<(Vec<AuditEvent>, Vec<String>)> {
    let content = fs::read_to_string(path)?;
    let mut parse_issues = Vec::new();

    if content.trim().is_empty() {
        return Ok((Vec::new(), parse_issues));
    }

    if let Ok(events) = serde_json::from_str::<Vec<AuditEvent>>(&content) {
        return Ok((events, parse_issues));
    }

    let mut events = Vec::new();
    for (line_number, raw_line) in content.lines().enumerate() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }

        match serde_json::from_str::<AuditEvent>(line) {
            Ok(event) => events.push(event),
            Err(err) => parse_issues.push(format!(
                "line {}: not a parseable AuditEvent JSON object: {err}",
                line_number + 1
            )),
        }
    }

    Ok((events, parse_issues))
}

fn normalize_health_event(raw: RawHealthEvent) -> Result<HealthEventRecord, String> {
    let event_id = raw
        .event_id
        .unwrap_or_else(|| format!("health-event-{}", generate_random_suffix()));
    let timestamp = raw.timestamp.unwrap_or_else(|| chrono::Utc::now().to_rfc3339());
    let source = raw.source.unwrap_or_else(|| "copybook-core".to_string());
    let event_type = raw.event_type.unwrap_or_else(|| "Unknown".to_string());
    let integrity_hash = raw.integrity_hash.unwrap_or_else(|| "".to_string());

    Ok(HealthEventRecord {
        event_id,
        timestamp,
        source,
        event_type,
        integrity_hash,
        previous_hash: raw.previous_hash,
    })
}

fn generate_random_suffix() -> String {
    chrono::Utc::now()
        .timestamp_nanos_opt()
        .map_or_else(|| "fallback".to_string(), |value| value.to_string())
}

#[inline]
fn bytes_to_gbps(bytes_per_second: f64) -> f64 {
    bytes_per_second / (1024.0_f64 * 1024.0_f64 * 1024.0_f64)
}

#[inline]
fn bytes_to_mbps(bytes_per_second: f64) -> f64 {
    bytes_per_second / (1024.0_f64 * 1024.0_f64)
}

fn combine_exit_code(current: ExitCode, candidate: ExitCode) -> ExitCode {
    if candidate.precedence() >= current.precedence() {
        candidate
    } else {
        current
    }
}

fn sidecar_path(output: &Path, suffix: &str) -> PathBuf {
    let stem = output.file_stem().and_then(|s| s.to_str()).unwrap_or("audit");
    let ext = output.extension().and_then(|s| s.to_str()).unwrap_or("json");
    let file_name = format!("{stem}.{suffix}.{ext}");
    output.parent().unwrap_or_else(|| Path::new(".")).join(file_name)
}

fn parse_compliance_profile(raw: &str) -> Option<ComplianceProfile> {
    match raw.to_ascii_lowercase().as_str() {
        "sox" => Some(ComplianceProfile::SOX),
        "hipaa" => Some(ComplianceProfile::HIPAA),
        "gdpr" => Some(ComplianceProfile::GDPR),
        "pci" | "pcidss" | "pci_dss" | "pci-dss" => Some(ComplianceProfile::PciDss),
        _ => None,
    }
}

fn parse_compliance_profiles(
    profiles: &str,
) -> Result<(Vec<ComplianceProfile>, Vec<String>), io::Error> {
    let mut parsed = Vec::new();
    let mut names = Vec::new();
    let mut seen = HashSet::new();

    for raw in profiles.split(',').map(str::trim).filter(|item| !item.is_empty()) {
        if let Some(profile) = parse_compliance_profile(raw) {
            if seen.insert(profile) {
                names.push(raw.to_string());
                parsed.push(profile);
            }
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("invalid compliance profile: '{raw}'"),
            ));
        }
    }

    Ok((parsed, names))
}

fn parse_copybook_schema(path: &Path) -> AuditResult<Schema> {
    let copybook_text = fs::read_to_string(path)?;
    Ok(copybook_core::parse_copybook(&copybook_text)?)
}

fn collect_leaf_fields<'a>(fields: &'a [Field], out: &mut Vec<&'a Field>) {
    for field in fields {
        if field.children.is_empty() {
            out.push(field);
        } else {
            collect_leaf_fields(&field.children, out);
        }
    }
}

fn map_field_type_name(field: &Field) -> String {
    match &field.kind {
        copybook_core::FieldKind::Alphanum { len } => format!("alphanum[{len}]"),
        copybook_core::FieldKind::ZonedDecimal { digits, .. } => format!("zoned-decimal[{digits}]"),
        copybook_core::FieldKind::BinaryInt { bits, .. } => format!("binary-int[{bits}]"),
        copybook_core::FieldKind::PackedDecimal { digits, .. } => {
            format!("packed-decimal[{digits}]")
        }
        copybook_core::FieldKind::Group => "group".to_string(),
        copybook_core::FieldKind::Condition { .. } => "condition".to_string(),
        copybook_core::FieldKind::Renames { .. } => "renames".to_string(),
        copybook_core::FieldKind::EditedNumeric { width, .. } => format!("edited-numeric[{width}]"),
        copybook_core::FieldKind::FloatSingle => "float-single".to_string(),
        copybook_core::FieldKind::FloatDouble => "float-double".to_string(),
    }
}

fn estimate_schema_bytes(schema: &Schema) -> (u64, u64, u64) {
    let mut display_bytes = 0u64;
    let mut comp3_bytes = 0u64;
    let mut total_bytes = 0u64;

    let mut fields = Vec::new();
    collect_leaf_fields(&schema.fields, &mut fields);
    for field in fields {
        let (display, comp3) = match &field.kind {
            FieldKind::Alphanum { len } => (*len as u64, 0),
            FieldKind::PackedDecimal { digits, .. } => (0, u64::from(*digits) / 2 + 1),
            FieldKind::BinaryInt { bits, .. } => (0, u64::from(*bits) / 8),
            FieldKind::ZonedDecimal { digits, .. } => (0, (u64::from(*digits) + 1) / 2),
            FieldKind::EditedNumeric { width, .. } => (*width as u64, 0),
            FieldKind::FloatSingle => (0, 4),
            FieldKind::FloatDouble => (0, 8),
            _ => (0, 0),
        };

        display_bytes += display;
        comp3_bytes += comp3;
        total_bytes += display + comp3;
    }

    if total_bytes == 0 {
        total_bytes = schema.lrecl_fixed.unwrap_or(128) as u64;
        display_bytes = (total_bytes * 80) / 100;
        comp3_bytes = total_bytes - display_bytes;
    }

    (display_bytes, comp3_bytes, total_bytes)
}

fn parse_access_result(raw: Option<&str>) -> AccessResult {
    match raw.unwrap_or_default().to_ascii_lowercase().as_str() {
        "deny" | "denied" | "forbidden" | "failure" | "failed" => AccessResult::Denied,
        "error" | "invalid" | "blocked" | "blocked" => AccessResult::Failed,
        _ => AccessResult::Success,
    }
}

fn parse_access_events(path: &Path) -> AuditResult<(Vec<AccessEvent>, Vec<String>)> {
    let file = fs::File::open(path)?;
    let reader = BufReader::new(file);
    let mut events = Vec::new();
    let mut parse_issues = Vec::new();

    for (line_number, line_result) in reader.lines().enumerate() {
        let line = match line_result {
            Ok(value) => value,
            Err(err) => {
                parse_issues.push(format!("line {}: failed to read access event line: {err}", line_number + 1));
                continue;
            }
        };
        if line.trim().is_empty() {
            continue;
        }

        match serde_json::from_str::<RawAccessEvent>(&line) {
            Ok(raw) => {
                let user_id = raw
                    .user
                    .or(raw.user_id)
                    .unwrap_or_else(|| "unknown-user".to_string());
                let access_type = raw
                    .action
                    .or(raw.access_type)
                    .unwrap_or_else(|| "read".to_string());
                let resource_type = raw.resource_type.unwrap_or_else(|| "resource".to_string());
                let resource_id = raw.resource.or(raw.resource_id).unwrap_or_else(|| "generic".to_string());
                let result = parse_access_result(raw.result.as_deref().or(raw.status.as_deref()));

                events.push(AccessEvent {
                    user_id,
                    resource_type,
                    resource_id,
                    access_type,
                    source_ip: raw.source_ip.or(raw.source_ip_address),
                    user_agent: raw.user_agent,
                    result,
                });
            }
            Err(err) => {
                parse_issues.push(format!(
                    "line {}: invalid access event JSON: {err}",
                    line_number + 1
                ));
            }
        }
    }

    Ok((events, parse_issues))
}

fn transformation_label(transformation: &TransformationType) -> &'static str {
    match transformation {
        TransformationType::DirectMapping => "direct_mapping",
        TransformationType::TypeConversion => "type_conversion",
        TransformationType::Aggregation => "aggregation",
        TransformationType::Calculation => "calculation",
        TransformationType::Lookup => "lookup",
        TransformationType::Filter => "filter",
    }
}

fn risk_level_rank(level: &RiskLevel) -> u8 {
    match level {
        RiskLevel::Low => 1,
        RiskLevel::Medium => 2,
        RiskLevel::High => 3,
        RiskLevel::Critical => 4,
    }
}

fn security_classification(
    classification: Option<DataClassification>,
) -> copybook_core::audit::context::SecurityClassification {
    match classification {
        Some(DataClassification::Public) => copybook_core::audit::context::SecurityClassification::Public,
        Some(DataClassification::Internal) => {
            copybook_core::audit::context::SecurityClassification::Internal
        }
        Some(DataClassification::Confidential) => {
            copybook_core::audit::context::SecurityClassification::Confidential
        }
        Some(DataClassification::MaterialTransaction) => {
            copybook_core::audit::context::SecurityClassification::MaterialTransaction
        }
        Some(DataClassification::PHI) => copybook_core::audit::context::SecurityClassification::PHI,
        None => copybook_core::audit::context::SecurityClassification::Internal,
    }
}

fn is_sensitive_field_name(name: &str) -> bool {
    let candidate = name.to_ascii_uppercase();
    candidate.contains("SSN")
        || candidate.contains("SOCIAL")
        || candidate.contains("PASSWORD")
        || candidate.contains("CREDIT")
        || candidate.contains("CARD")
        || candidate.contains("ACCOUNT")
        || candidate.contains("DOB")
        || candidate.contains("DOB")
        || candidate.contains("PIN")
        || candidate.contains("ENCRYPT")
        || candidate.contains("SECRET")
}

fn collect_sensitive_fields(schema: &Schema) -> Vec<String> {
    let mut sensitive = Vec::new();
    let mut leaf_fields = Vec::new();
    collect_leaf_fields(&schema.fields, &mut leaf_fields);

    for field in leaf_fields {
        if is_sensitive_field_name(&field.name) {
            sensitive.push(field.name.clone());
        }
    }

    sensitive
}

fn generate_synthetic_benchmark_data(schema: &Schema, records: usize) -> Vec<u8> {
    let (_, _, record_size) = estimate_schema_bytes(schema);
    let record_size = record_size.max(128);
    let total_records = records.max(1);
    vec![0xF0; record_size as usize * total_records]
}

fn clamp_u64(value: f64) -> u64 {
    if !value.is_finite() || value <= 0.0 {
        0
    } else if value >= u64::MAX as f64 {
        u64::MAX
    } else {
        value.round() as u64
    }
}

fn report_section_from_file(path: &Path) -> AuditResult<ReportSectionFile> {
    let content = fs::read_to_string(path)?;
    serde_json::from_str::<ReportSectionFile>(&content).map_err(|err| err.into())
}

async fn run_audit_report(
    copybook: &std::path::Path,
    data_file: Option<&std::path::Path>,
    output: &std::path::Path,
    format: OutputFormat,
    compliance: Option<&str>,
    include_performance: bool,
    include_security: bool,
    include_lineage: bool,
    include_recommendations: bool,
    audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    let mut overall_code = ExitCode::Ok;
    if !matches!(format, OutputFormat::Json) {
        write_stderr_line("Report format for audit report is currently limited to json")?;
        return Ok(ExitCode::Data);
    }

    write_stdout_line("Generating comprehensive audit report...")?;

    let mut sections = BTreeMap::new();
    let schema = parse_copybook_schema(copybook)?;
    let mut status_messages = Vec::new();

    status_messages.push(format!(
        "copybook_fields: {}",
        schema.fields.len()
    ));

    if let Some(compliance_profiles) = compliance {
        let sidecar = sidecar_path(output, "compliance");
        let sidecar = sidecar.as_path();
        let code = run_compliance_validation(
            compliance_profiles,
            copybook,
            data_file,
            None,
            Codepage::CP037,
            true,
            sidecar,
            false,
            true,
            include_recommendations,
            audit_context.clone(),
        )
        .await?;
        overall_code = combine_exit_code(overall_code, code);
        let compliance_file = report_section_from_file(sidecar)?;
        if let Some(section) = compliance_file.compliance_validation {
            sections.insert("compliance_validation".to_string(), section);
        }
    }

    if include_performance {
        let sidecar = sidecar_path(output, "performance");
        let sidecar = sidecar.as_path();
        let code = run_performance_audit(
            copybook,
            data_file,
            Some(RecordFormat::Fixed),
            Codepage::CP037,
            sidecar,
            false,
            None,
            None,
            None,
            None,
            None,
            false,
            1,
            audit_context.clone(),
        )?;
        overall_code = combine_exit_code(overall_code, code);
        let performance_file = report_section_from_file(sidecar)?;
        if let Some(section) = performance_file.performance_audit {
            sections.insert("performance_audit".to_string(), section);
        }
    }

    if include_security {
        let sidecar = sidecar_path(output, "security");
        let sidecar = sidecar.as_path();
        let code = run_security_audit(
            copybook,
            None,
            sidecar,
            None,
            true,
            true,
            false,
            None,
            None,
            None,
            false,
            ValidationDepth::Standard,
            include_recommendations,
            audit_context.clone(),
        )?;
        overall_code = combine_exit_code(overall_code, code);
        let security_file = report_section_from_file(sidecar)?;
        if let Some(section) = security_file.security_audit {
            sections.insert("security_audit".to_string(), section);
        }
    }

    if include_lineage {
        let sidecar = sidecar_path(output, "lineage");
        let sidecar = sidecar.as_path();
        let code = run_lineage_analysis(
            copybook,
            None,
            "audit-report-source",
            Some("audit-report-target"),
            sidecar,
            Some(copybook),
            "json",
            true,
            true,
            true,
            true,
            0.8,
            audit_context,
        )?;
        overall_code = combine_exit_code(overall_code, code);
        let lineage_file = report_section_from_file(sidecar)?;
        if let Some(section) = lineage_file.lineage_analysis {
            sections.insert("lineage_analysis".to_string(), section);
        }
    }

    let report = serde_json::json!({
        "audit_report": {
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "copybook": copybook.display(),
            "status_code": format!("{} ({})", overall_code.as_i32(), overall_code),
            "status": if overall_code == ExitCode::Ok { "pass" } else { "fail" },
            "sections": sections,
            "summary": {
                "copybook_path": copybook.display().to_string(),
                "diagnostic_messages": status_messages,
                "include_recommendations": include_recommendations,
            }
        }
    });

    atomic_write(output, |writer| {
        let body = serde_json::to_vec_pretty(&report)?;
        writer.write_all(&body)?;
        writer.write_all(b"\n")?;
        Ok(())
    })?;

    write_stdout_line(&format!(
        "Comprehensive audit report written to {}",
        output.display()
    ))?;
    Ok(overall_code)
}

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
async fn run_compliance_validation(
    compliance: &str,
    copybook: &std::path::Path,
    _data_file: Option<&std::path::Path>,
    _format: Option<RecordFormat>,
    codepage: Codepage,
    strict: bool,
    output: &std::path::Path,
    auto_remediate: bool,
    _report_violations: bool,
    include_recommendations: bool,
    audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync + 'static>> {
    write_stdout_line("Running compliance validation...")?;

    let (profiles, framework_names) = parse_compliance_profiles(compliance)?;
    let mut compliance_config = ComplianceConfig::default();
    compliance_config.strict_mode = strict;
    compliance_config.auto_remediation = auto_remediate;

    let mut schema = parse_copybook_schema(copybook)?;
    if schema.lrecl_fixed.is_none() {
        schema.calculate_fingerprint();
    }

    let mut validation_context = audit_context.clone();
    validation_context = validation_context.with_compliance_profiles(&profiles);
    validation_context = validation_context
        .with_metadata("operation", "compliance_validation".to_string())
        .with_metadata("codepage", format!("{codepage}"));

    let engine = ComplianceEngine::new(compliance_config).with_profiles(&profiles);
    let validation_result = engine.validate_processing_operation(&validation_context).await?;
    let recommendations = if include_recommendations {
        engine.generate_recommendations(&validation_context).await.unwrap_or_default()
    } else {
        Vec::new()
    };

    let status = if validation_result.is_compliant() {
        "compliant"
    } else {
        "non_compliant"
    };

    let report = serde_json::json!({
        "compliance_validation": {
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "copybook": copybook.display().to_string(),
            "frameworks": framework_names,
            "status": status,
            "profiles": validation_result.validated_profiles.iter().map(|p| format!("{p:?}")).collect::<Vec<_>>(),
            "violation_count": validation_result.violations.len(),
            "violations": validation_result.violations.iter().map(|v| serde_json::json!({
                "id": v.violation_id,
                "title": v.title,
                "regulation": v.regulation,
                "severity": format!("{:?}", v.severity),
                "description": v.description,
                "remediation": v.remediation,
                "reference_url": v.reference_url,
            })).collect::<Vec<_>>(),
            "warnings": validation_result.warnings.iter().map(|w| serde_json::json!({
                "id": w.warning_id,
                "title": w.title,
                "description": w.description,
                "recommendation": w.recommendation,
            })).collect::<Vec<_>>(),
            "recommendations": recommendations.iter().map(|r| serde_json::json!({
                "id": r.recommendation_id,
                "title": r.title,
                "description": r.description,
                "priority": format!("{:?}", r.priority),
            })).collect::<Vec<_>>(),
            "validation_timestamp": validation_result.validation_timestamp,
            "strict_mode": strict,
            "auto_remediation": auto_remediate,
            "codepage": format!("{codepage}"),
        }
    });

    atomic_write(output, |writer| {
        let body = serde_json::to_vec_pretty(&report)?;
        writer.write_all(&body)?;
        writer.write_all(b"\n")?;
        Ok(())
    })?;

    if validation_result.is_compliant() {
        write_stdout_line("✅ Compliance validation passed")?;
        Ok(ExitCode::Ok)
    } else {
        write_stdout_line("⚠️  Compliance violations detected")?;
        for violation in &validation_result.violations {
            write_stdout_line(&format!(
                "   {} - {}: {}",
                violation.violation_id, violation.title, violation.description
            ))?;
        }
        Ok(ExitCode::Encode)
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
fn run_lineage_analysis(
    source_copybook: &std::path::Path,
    target_copybook: Option<&std::path::Path>,
    source_system: &str,
    target_system: Option<&str>,
    output: &std::path::Path,
    source: Option<&std::path::Path>,
    target_format: &str,
    field_level: bool,
    transformation_details: bool,
    quality_metrics: bool,
    impact_analysis: bool,
    confidence_threshold: f64,
    audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    return run_lineage_analysis_impl(
        source_copybook,
        target_copybook,
        source_system,
        target_system,
        output,
        source,
        target_format,
        field_level,
        transformation_details,
        quality_metrics,
        impact_analysis,
        confidence_threshold,
        audit_context,
    );

    write_stdout_line("Analyzing data lineage...")?;

    let source_path = source.unwrap_or(source_copybook);
    let target_path = target_copybook.unwrap_or(source_copybook);
    let resolved_target_system = target_system.unwrap_or("target-system");
    let source_schema = parse_copybook_schema(source_path)?;
    let target_schema = parse_copybook_schema(target_path)?;

    let mut source_fields = Vec::new();
    let mut target_fields = Vec::new();
    collect_leaf_fields(&source_schema.fields, &mut source_fields);
    collect_leaf_fields(&target_schema.fields, &mut target_fields);

    let source_fingerprint = if source_schema.fingerprint.is_empty() {
        "unknown".to_string()
    } else {
        source_schema.fingerprint.clone()
    };
    let target_fingerprint = if target_schema.fingerprint.is_empty() {
        "unknown".to_string()
    } else {
        target_schema.fingerprint.clone()
    };

    let tracker = LineageTracker::new()
        .with_source_system(source_system)
        .with_target_system(resolved_target_system);
    let impact_assessor = ImpactAnalyzer::new();

    let mut lineage_records = Vec::new();
    let mut field_mappings = Vec::new();
    let mut transformation_rules = Vec::new();
    let mut impact_reports = Vec::new();
    let mut mapping_issues = Vec::new();

    let max_records = source_fields.len().max(target_fields.len());
    let confidence_threshold = confidence_threshold.clamp(0.0, 1.0);
    for index in 0..max_records {
        let source_field = match source_fields.get(index) {
            Some(field) => field,
            None => {
                mapping_issues.push(format!("Missing source field at index {index}"));
                continue;
            }
        };
        let target_field = target_fields.get(index).unwrap_or(source_field);

        let source_type = map_field_type_name(source_field);
        let target_type = map_field_type_name(target_field);
        let source_path = source_field.path.clone();
        let target_path = target_field.path.clone();

        let transformation = if source_path.eq_ignore_ascii_case(&target_path) && source_type == target_type {
            TransformationType::DirectMapping
        } else {
            TransformationType::TypeConversion
        };
        let confidence = if source_path.eq_ignore_ascii_case(&target_path) {
            confidence_threshold
        } else {
            confidence_threshold * 0.75
        };

        let source_info = FieldLineage {
            field_path: source_path.clone(),
            system_id: source_system.to_string(),
            schema_version: source_fingerprint.clone(),
            data_type: source_type.clone(),
        };
        let target_info = FieldLineage {
            field_path: target_path.clone(),
            system_id: resolved_target_system.to_string(),
            schema_version: target_fingerprint.clone(),
            data_type: target_type.clone(),
        };

        let record = tracker.track_transformation(
            source_info,
            target_info,
            transformation.clone(),
            confidence,
        )?;
        lineage_records.push(record);

        field_mappings.push(serde_json::json!({
            "source_field": source_path,
            "target_field": target_path,
            "source_type": source_type,
            "target_type": target_type,
            "transformation": format!("{transformation:?}"),
            "quality_score": confidence,
        }));

        if transformation_details {
            transformation_rules.push(serde_json::json!({
                "rule_id": format!("TRANS-{index:03}"),
                "rule_type": format!("{transformation:?}"),
                "description": format!("Mapped {source_path} -> {target_path}"),
                "confidence": confidence,
                "target_format": target_format,
            }));
        }

        if impact_analysis {
            let assessment = impact_assessor.assess_field_impact(&source_path, &lineage_records);
            impact_reports.push(serde_json::json!({
                "field_path": source_path,
                "affected_systems": assessment.affected_systems,
                "risk_level": format!("{:?}", assessment.risk_level),
                "estimated_impact": assessment.estimated_impact,
                "remediation": impact_assessor.estimate_remediation_effort(&assessment),
            }));
        }
    }

    let chain_valid = tracker.validate_lineage_chain(&lineage_records).unwrap_or(false);
    let avg_quality = if lineage_records.is_empty() {
        0.0
    } else {
        lineage_records.iter().map(|record| record.quality_score).sum::<f64>() / lineage_records.len() as f64
    };

    let payload = AuditPayload::LineageTracking {
        source_system: source_system.to_string(),
        target_system: resolved_target_system.to_string(),
        field_mappings: field_mappings
            .iter()
            .filter_map(|value| serde_json::from_value::<audit_core::event::FieldMapping>(value.clone()).ok())
            .map(|value| audit_core::event::FieldMapping {
                source_field: value.source_field,
                target_field: value.target_field,
                transformation: value.transformation,
                confidence_score: value.confidence_score,
            })
            .collect(),
        transformation_rules: transformation_rules
            .iter()
            .filter_map(|value| serde_json::from_value::<audit_core::event::TransformationRule>(value.clone()).ok())
            .map(|value| audit_core::event::TransformationRule {
                rule_id: value.rule_id,
                rule_type: value.rule_type,
                description: value.description,
                parameters: value.parameters,
            })
            .collect(),
        quality_score: avg_quality,
        impact_assessment: if impact_analysis {
            Some(audit_core::event::ImpactAssessmentSummary {
                affected_systems: impact_reports.len() as u32,
                risk_level: if impact_reports.is_empty() {
                    "low".to_string()
                } else if impact_reports.iter().any(|entry| entry["risk_level"].as_str() == Some("Critical")) {
                    "critical".to_string()
                } else if impact_reports.iter().any(|entry| entry["risk_level"].as_str() == Some("High")) {
                    "high".to_string()
                } else if impact_reports.iter().any(|entry| entry["risk_level"].as_str() == Some("Medium")) {
                    "medium".to_string()
                } else {
                    "low".to_string()
                },
                estimated_impact: if impact_reports.is_empty() {
                    "No impact analysis".to_string()
                } else {
                    format!("{} affected path(s)", impact_reports.len())
                },
            })
        } else {
            None
        },
    };
    let event = AuditEvent::new(
        AuditEventType::LineageTracking,
        audit_context.with_metadata("operation", "lineage_analysis".to_string()),
        payload,
    );

    let mut status = "pass";
    let mut exit_code = ExitCode::Ok;
    if !chain_valid || !mapping_issues.is_empty() {
        status = "fail";
        exit_code = ExitCode::Data;
    }
    if !source_schema.lrecl_fixed.is_some_and(|value| value > 0) {
        mapping_issues.push("source schema LRECL is not set".to_string());
    }

    let report = serde_json::json!({
        "lineage_analysis": {
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "source_system": source_system,
            "target_system": resolved_target_system,
            "source_copybook": source_path.display().to_string(),
            "target_copybook": target_path.display().to_string(),
            "source_fingerprint": source_fingerprint,
            "target_fingerprint": target_fingerprint,
            "source_schema_fields": source_fields.len(),
            "target_schema_fields": target_fields.len(),
            "target_format": target_format,
            "field_level": field_level,
            "transformation_details": transformation_details,
            "quality_metrics": quality_metrics,
            "impact_analysis": impact_analysis,
            "confidence_threshold": confidence_threshold,
            "lineage_record_count": lineage_records.len(),
            "avg_quality_score": avg_quality,
            "chain_valid": chain_valid,
            "status": status,
            "status_code": format!("{} ({})", exit_code.as_i32(), exit_code),
            "audit_event_id": event.event_id,
            "audit_event_integrity": event.integrity_hash,
            "mapping_issues": mapping_issues,
            "field_mappings": if field_level { field_mappings } else { serde_json::json!([]) },
            "transformation_rules": transformation_rules,
            "impact_summary": impact_reports,
            "source_root_path": source_copybook.display().to_string(),
            "target_root_path": target_copybook.display().to_string(),
            "source_override": source.map(|path| path.display().to_string()),
        }
    });

    atomic_write(output, |writer| {
        let body = serde_json::to_vec_pretty(&report)?;
        writer.write_all(&body)?;
        writer.write_all(b"\n")?;
        Ok(())
    })?;

    write_stdout_line(&format!(
        "Lineage analysis written to {}",
        output.display()
    ))?;
    Ok(exit_code)
}

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
fn run_lineage_analysis_impl(
    source_copybook: &std::path::Path,
    target_copybook: Option<&std::path::Path>,
    source_system: &str,
    target_system: Option<&str>,
    output: &std::path::Path,
    source: Option<&std::path::Path>,
    target_format: &str,
    field_level: bool,
    transformation_details: bool,
    quality_metrics: bool,
    impact_analysis: bool,
    confidence_threshold: f64,
    audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    write_stdout_line("Analyzing data lineage (implemented)...")?;

    let source_path = source.unwrap_or(source_copybook);
    let target_path = target_copybook.unwrap_or(source_copybook);
    let resolved_target_system = target_system.unwrap_or("target-system");
    let source_schema = parse_copybook_schema(source_path)?;
    let target_schema = parse_copybook_schema(target_path)?;

    let mut source_fields = Vec::new();
    let mut target_fields = Vec::new();
    collect_leaf_fields(&source_schema.fields, &mut source_fields);
    collect_leaf_fields(&target_schema.fields, &mut target_fields);

    let source_fingerprint = if source_schema.fingerprint.is_empty() {
        "unknown".to_string()
    } else {
        source_schema.fingerprint.clone()
    };
    let target_fingerprint = if target_schema.fingerprint.is_empty() {
        "unknown".to_string()
    } else {
        target_schema.fingerprint.clone()
    };

    if source_fields.is_empty() {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "source schema has no leaf fields").into());
    }

    let tracker = LineageTracker::new()
        .with_source_system(source_system)
        .with_target_system(resolved_target_system);
    let impact_assessor = ImpactAnalyzer::new();

    let max_records = source_fields.len().max(target_fields.len());
    let normalized_threshold = confidence_threshold.clamp(0.0, 1.0);

    let mut lineage_records = Vec::new();
    let mut field_mappings = Vec::new();
    let mut transformation_rules = Vec::new();
    let mut impact_reports: Vec<serde_json::Value> = Vec::new();
    let mut mapping_issues = Vec::new();

    for index in 0..max_records {
        let source_field = source_fields.get(index).or(target_fields.get(index)).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("no source or target field at index {index}"),
            )
        })?;
        let target_field = target_fields.get(index).unwrap_or(source_field);

        let source_type = map_field_type_name(source_field);
        let target_type = map_field_type_name(target_field);
        let source_path = source_field.path.clone();
        let target_path = target_field.path.clone();

        let transformation = if source_path.eq_ignore_ascii_case(&target_path) {
            if source_type == target_type {
                TransformationType::DirectMapping
            } else {
                TransformationType::TypeConversion
            }
        } else {
            TransformationType::Calculation
        };

        let mut confidence = if source_path.eq_ignore_ascii_case(&target_path) {
            normalized_threshold
        } else {
            normalized_threshold * 0.75
        };
        confidence = confidence.clamp(0.0, 1.0);

        let source_info = FieldLineage {
            field_path: source_path.clone(),
            system_id: source_system.to_string(),
            schema_version: source_fingerprint.clone(),
            data_type: source_type.clone(),
        };
        let target_info = FieldLineage {
            field_path: target_path.clone(),
            system_id: resolved_target_system.to_string(),
            schema_version: target_fingerprint.clone(),
            data_type: target_type.clone(),
        };

        let record = tracker.track_transformation(
            source_info,
            target_info,
            transformation.clone(),
            confidence,
        )?;
        lineage_records.push(record);

        field_mappings.push(audit_core::event::FieldMapping {
            source_field: source_path.clone(),
            target_field: target_path.clone(),
            transformation: format!("{transformation:?}"),
            confidence_score: confidence,
        });

        if transformation_details {
            let mut parameters = HashMap::new();
            parameters.insert("index".to_string(), index.to_string());
            parameters.insert("source_type".to_string(), source_type);
            parameters.insert("target_type".to_string(), target_type);
            parameters.insert("source_system".to_string(), source_system.to_string());
            parameters.insert("target_system".to_string(), resolved_target_system.to_string());
            parameters.insert("target_format".to_string(), target_format.to_string());

            transformation_rules.push(audit_core::event::TransformationRule {
                rule_id: format!("TRANS-{index:03}"),
                rule_type: transformation_label(&transformation).to_string(),
                description: format!("Mapped {source_path} -> {target_path}"),
                parameters,
            });
        }

        if impact_analysis {
            let assessment = impact_assessor.assess_field_impact(&source_path, &lineage_records);
            impact_reports.push(serde_json::json!({
                "field_path": source_path,
                "affected_systems": assessment.affected_systems,
                "risk_level": format!("{:?}", assessment.risk_level),
                "estimated_impact": assessment.estimated_impact,
                "remediation": impact_assessor.estimate_remediation_effort(&assessment),
            }));
        }

        if confidence < normalized_threshold * 0.5 {
            mapping_issues.push(format!(
                "Low confidence mapping detected for {source_path} -> {target_path}: {confidence:.2}"
            ));
        }
    }

    let chain_records: Vec<_> = lineage_records
        .iter()
        .enumerate()
        .map(|(index, record)| {
            let mut cloned = record.clone();
            if index > 0 {
                cloned.source_field = lineage_records[index - 1].target_field.clone();
            }
            cloned
        })
        .collect();
    let chain_valid = if chain_records.len() <= 1 {
        true
    } else {
        tracker.validate_lineage_chain(&chain_records).unwrap_or(false)
    };

    if !chain_valid {
        mapping_issues.push("Lineage chain continuity is non-linear and requires manual review".to_string());
    }

    let avg_quality = if lineage_records.is_empty() {
        0.0
    } else {
        lineage_records.iter().map(|record| record.quality_score).sum::<f64>()
            / lineage_records.len() as f64
    };

    let quality_ok = avg_quality >= normalized_threshold;
    if !quality_ok {
        mapping_issues.push(format!(
            "Lineage quality average {avg_quality:.2} below required threshold {normalized_threshold:.2}"
        ));
    }

    let impact_summary = if impact_analysis {
        let mut highest_risk = RiskLevel::Low;
        let mut affected_systems = HashSet::new();
        for report in &impact_reports {
            if let Some(systems) = report["affected_systems"].as_array() {
                for entry in systems {
                    if let Some(system) = entry.as_str() {
                        affected_systems.insert(system.to_string());
                    }
                }
            }
            if let Some(level) = report["risk_level"].as_str() {
                let level = match level {
                    "Medium" => RiskLevel::Medium,
                    "High" => RiskLevel::High,
                    "Critical" => RiskLevel::Critical,
                    _ => RiskLevel::Low,
                };
                if risk_level_rank(&level) > risk_level_rank(&highest_risk) {
                    highest_risk = level;
                }
            }
        }
        Some(audit_core::event::ImpactAssessmentSummary {
            affected_systems: affected_systems.len() as u32,
            risk_level: format!("{highest_risk:?}").to_lowercase(),
            estimated_impact: if impact_reports.is_empty() {
                "No impact analysis".to_string()
            } else {
                format!("{} impacted field mapping(s)", impact_reports.len())
            },
        })
    } else {
        None
    };

    let payload = AuditPayload::LineageTracking {
        source_system: source_system.to_string(),
        target_system: resolved_target_system.to_string(),
        field_mappings,
        transformation_rules,
        quality_score: avg_quality,
        impact_assessment: impact_summary,
    };
    let event = AuditEvent::new(
        AuditEventType::LineageTracking,
        audit_context
            .with_metadata("operation", "lineage_analysis".to_string())
            .with_metadata("target_format", target_format.to_string()),
        payload,
    );

    let mut status = "pass";
    let mut exit_code = ExitCode::Ok;
    if !quality_metrics {
        status = "pass";
    }
    if !chain_valid || !mapping_issues.is_empty() || !quality_ok {
        status = if mapping_issues.iter().any(|entry| entry.contains("Low confidence")) {
            "warn"
        } else {
            "pass"
        };
        if !chain_valid || !mapping_issues.is_empty() {
            exit_code = if mapping_issues.iter().any(|entry| entry.contains("LRECL")) {
                ExitCode::Encode
            } else {
                ExitCode::Data
            };
        }
    }

    if !source_schema.lrecl_fixed.is_some_and(|value| value > 0) {
        mapping_issues.push("source schema LRECL is not set".to_string());
        if exit_code == ExitCode::Ok {
            exit_code = ExitCode::Encode;
        }
    }
    if !target_schema.lrecl_fixed.is_some_and(|value| value > 0) {
        mapping_issues.push("target schema LRECL is not set".to_string());
        if exit_code == ExitCode::Ok {
            exit_code = ExitCode::Encode;
        }
    }

    let report = serde_json::json!({
        "lineage_analysis": {
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "source_system": source_system,
            "target_system": resolved_target_system,
            "source_copybook": source_path.display().to_string(),
            "target_copybook": target_path.display().to_string(),
            "source_fingerprint": source_fingerprint,
            "target_fingerprint": target_fingerprint,
            "source_schema_fields": source_fields.len(),
            "target_schema_fields": target_fields.len(),
            "target_format": target_format,
            "field_level": field_level,
            "transformation_details": transformation_details,
            "quality_metrics": quality_metrics,
            "impact_analysis": impact_analysis,
            "confidence_threshold": normalized_threshold,
            "lineage_record_count": lineage_records.len(),
            "avg_quality_score": avg_quality,
            "chain_valid": chain_valid,
            "quality_ok": quality_ok,
            "field_mappings": if field_level { serde_json::to_value(payload.field_mappings).unwrap_or_default() } else { serde_json::Value::Array(Vec::new()) },
            "transformation_rules": serde_json::to_value(payload.transformation_rules).unwrap_or_default(),
            "impact_summary": impact_reports,
            "audit_event_id": event.event_id,
            "audit_event_integrity": event.integrity_hash,
            "mapping_issues": mapping_issues,
            "status": status,
            "status_code": format!("{} ({})", exit_code.as_i32(), exit_code),
            "source_root_path": source_copybook.display().to_string(),
            "target_root_path": target_copybook.map_or_else(|| source_copybook.display().to_string(), |path| path.display().to_string()),
            "source_override": source.map(|path| path.display().to_string()),
        }
    });

    atomic_write(output, |writer| {
        let body = serde_json::to_vec_pretty(&report)?;
        writer.write_all(&body)?;
        writer.write_all(b"\n")?;
        Ok(())
    })?;

    write_stdout_line(&format!("Lineage analysis written to {}", output.display()))?;
    Ok(exit_code)
}

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
fn run_performance_audit(
    _copybook: &std::path::Path,
    _data_file: Option<&std::path::Path>,
    _format: Option<RecordFormat>,
    _codepage: Codepage,
    _output: &std::path::Path,
    _establish_baseline: bool,
    _baseline_file: Option<&std::path::Path>,
    _validate_against_baseline: Option<&std::path::Path>,
    _target_display_gbps: Option<f64>,
    _target_comp3_mbps: Option<f64>,
    _max_overhead_percent: Option<f64>,
    _include_regression_analysis: bool,
    _iterations: u32,
    _audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    return run_performance_audit_impl(
        _copybook,
        _data_file,
        _format,
        _codepage,
        _output,
        _establish_baseline,
        _baseline_file,
        _validate_against_baseline,
        _target_display_gbps,
        _target_comp3_mbps,
        _max_overhead_percent,
        _include_regression_analysis,
        _iterations,
        _audit_context,
    );
}

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
fn run_performance_audit_impl(
    copybook: &std::path::Path,
    data_file: Option<&std::path::Path>,
    format: Option<RecordFormat>,
    codepage: Codepage,
    output: &std::path::Path,
    establish_baseline: bool,
    baseline_file: Option<&std::path::Path>,
    validate_against_baseline: Option<&std::path::Path>,
    target_display_gbps: Option<f64>,
    target_comp3_mbps: Option<f64>,
    max_overhead_percent: Option<f64>,
    include_regression_analysis: bool,
    iterations: u32,
    audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    write_stdout_line("Running performance audit (implemented)...")?;

    let schema = parse_copybook_schema(copybook)?;
    let decode_format = format.unwrap_or(RecordFormat::Fixed);
    let run_iterations = iterations.max(1);

    let decode_options = DecodeOptions {
        format: decode_format,
        codepage,
        strict_mode: false,
        ..DecodeOptions::default()
    };

    let synthetic_records = 512usize.max(run_iterations as usize * 16);
    let synthetic_data = generate_synthetic_benchmark_data(&schema, synthetic_records);
    let mut run_summaries = Vec::new();
    let mut parse_issues = Vec::new();

    for iteration in 0..run_iterations {
        let mut output_buffer = Cursor::new(Vec::new());
        let summary = if let Some(path) = data_file {
            let reader = fs::File::open(path).map_err(io::Error::from)?;
            decode_file_to_jsonl(&schema, reader, &mut output_buffer, &decode_options)?
        } else {
            decode_file_to_jsonl(
                &schema,
                Cursor::new(synthetic_data.as_slice()),
                &mut output_buffer,
                &decode_options,
            )?
        };

        run_summaries.push(summary);
        if iteration == 0 {
            parse_issues.push(format!("iteration {} completed: {} bytes", iteration + 1, output_buffer.position()));
        }
    }

    let run_count = run_summaries.len() as f64;
    if run_count == 0.0 {
        return Err(io::Error::other("no benchmark runs were executed").into());
    }

    let avg_processed_records: f64 =
        run_summaries.iter().map(|summary| summary.records_processed as f64).sum::<f64>() / run_count;
    let avg_throughput_mbps: f64 =
        run_summaries.iter().map(|summary| summary.throughput_mbps).sum::<f64>() / run_count;
    let avg_processing_ms: f64 =
        run_summaries.iter().map(|summary| summary.processing_time_ms as f64).sum::<f64>() / run_count;
    let avg_errors: f64 =
        run_summaries.iter().map(|summary| summary.records_with_errors as f64).sum::<f64>() / run_count;

    let avg_throughput_bytes = avg_throughput_mbps * 1024.0 * 1024.0;
    let record_rate = if avg_processing_ms > 0.0 {
        avg_processed_records / (avg_processing_ms / 1000.0)
    } else {
        0.0
    };
    let peak_memory_mb = run_summaries
        .iter()
        .filter_map(|summary| summary.peak_memory_bytes)
        .map(|bytes| bytes / (1024 * 1024))
        .max()
        .unwrap_or(0);

    let (display_schema_bytes, comp3_schema_bytes, total_bytes) = estimate_schema_bytes(&schema);
    let display_ratio = if total_bytes == 0 {
        0.8_f64
    } else {
        display_schema_bytes as f64 / total_bytes as f64
    };
    let comp3_ratio = if total_bytes == 0 {
        0.2_f64
    } else {
        comp3_schema_bytes as f64 / total_bytes as f64
    };

    let avg_display_throughput_bps = avg_throughput_bytes * display_ratio;
    let avg_comp3_throughput_bps = avg_throughput_bytes * comp3_ratio;

    let display_throughput_gbps = bytes_to_gbps(avg_display_throughput_bps);
    let comp3_throughput_mbps = bytes_to_mbps(avg_comp3_throughput_bps);

    let mut baseline_id = None;
    let mut baseline_throughput = None;
    let mut comparison_result = ComparisonResult::WithinBaseline;
    let mut regressions = Vec::new();
    let mut overhead = 0.0;

    let threshold_percent = max_overhead_percent.unwrap_or(5.0);
    if let Some(reference_path) = validate_against_baseline {
        let baseline_manager = BaselineManager::new(reference_path);
        let baseline = baseline_manager.load_baseline()?;
        baseline_id = Some(baseline.baseline_id.clone());
        baseline_throughput = Some(baseline.throughput.clone());

        let current_metrics = ThroughputMetrics {
            display_throughput: clamp_u64(avg_display_throughput_bps),
            comp3_throughput: clamp_u64(avg_comp3_throughput_bps),
            record_rate: clamp_u64(record_rate),
            peak_memory_mb,
        };
        let auditor = PerformanceAuditor::new(reference_path, threshold_percent);
        regressions.extend(auditor.audit(&current_metrics)?);

        if regressions.is_empty() {
            if avg_display_throughput_bps >= baseline.throughput.display_throughput as f64 * 0.99
                && avg_comp3_throughput_bps >= baseline.throughput.comp3_throughput as f64 * 0.99
            {
                comparison_result = ComparisonResult::WithinBaseline;
            } else {
                comparison_result = ComparisonResult::BelowBaseline;
            }
        } else {
            comparison_result = ComparisonResult::SignificantRegression;
        }

        if baseline.throughput.display_throughput > 0 {
            let d = baseline.throughput.display_throughput as f64;
            overhead = ((d - avg_display_throughput_bps) / d) * 100.0;
        }
        if baseline.throughput.comp3_throughput > 0 {
            let d = baseline.throughput.comp3_throughput as f64;
            let component_overhead = ((d - avg_comp3_throughput_bps) / d) * 100.0;
            if component_overhead > overhead {
                overhead = component_overhead;
            }
        }

        if !include_regression_analysis && !regressions.is_empty() {
            regressions.truncate(1);
        }
    }

    if establish_baseline {
        let target = baseline_file
            .map(|path| path.to_path_buf())
            .unwrap_or_else(|| sidecar_path(output, \"baseline\"));
        let baseline_manager = BaselineManager::new(&target);

        let baseline = PerformanceBaseline {
            baseline_id: format!("baseline-{}", generate_random_suffix()),
            throughput: ThroughputMetrics {
                display_throughput: clamp_u64(avg_display_throughput_bps),
                comp3_throughput: clamp_u64(avg_comp3_throughput_bps),
                record_rate: clamp_u64(record_rate),
                peak_memory_mb,
            },
            resources: ResourceMetrics {
                cpu_usage_percent: 0.0,
                memory_usage_mb: peak_memory_mb,
                io_operations: 0,
                network_bytes: 0,
            },
            created_at: chrono::Utc::now().to_rfc3339(),
        };
        baseline_manager.save_baseline(&baseline)?;
        baseline_id = Some(baseline.baseline_id.clone());
        parse_issues.push(format!(
            \"Baseline established at {} (display {:.3} GB/s, comp3 {:.2} MB/s)\",
            target.display(),
            display_throughput_gbps,
            comp3_throughput_mbps
        ));
    }

    if let Some(target_display) = target_display_gbps {
        if display_throughput_gbps < target_display {
            regressions.push(format!(
                \"display throughput {:.3} GB/s below target {:.3} GB/s\",
                display_throughput_gbps, target_display
            ));
        }
    }
    if let Some(target_comp3) = target_comp3_mbps {
        if comp3_throughput_mbps < target_comp3 {
            regressions.push(format!(
                \"comp3 throughput {:.2} MB/s below target {:.2} MB/s\",
                comp3_throughput_mbps, target_comp3
            ));
        }
    }

    if comparison_result == ComparisonResult::WithinBaseline && !regressions.is_empty() {
        comparison_result = ComparisonResult::BelowBaseline;
    }
    if comparison_result == ComparisonResult::WithinBaseline
        && validate_against_baseline.is_some()
        && threshold_percent > 0.0
    {
        comparison_result = ComparisonResult::BetterThanBaseline;
    }

    let regression_detected = !regressions.is_empty();
    let (status_code, status) = if regression_detected || avg_errors > 0.0 {
        (ExitCode::Data, \"warn\")
    } else {
        (ExitCode::Ok, \"pass\")
    };

    let metrics = PerformanceMetrics {
        throughput_bytes_per_sec: clamp_u64(avg_throughput_bytes),
        latency_ms: avg_processing_ms.round() as u64,
        cpu_usage_percent: 0.0,
        memory_usage_mb: peak_memory_mb,
        io_operations: 0,
    };

    let payload = AuditPayload::PerformanceMeasurement {
        measurement_type: PerformanceMeasurementType::Throughput,
        baseline_id,
        metrics,
        comparison_result: Some(comparison_result),
        regression_detected,
    };
    let event = AuditEvent::new(
        AuditEventType::PerformanceMeasurement,
        audit_context
            .with_metadata(\"operation\", \"performance_audit\".to_string())
            .with_metadata(\"iterations\", run_iterations.to_string()),
        payload,
    );

    let report = serde_json::json!({
        "performance_audit": {
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "copybook": copybook.display().to_string(),
            "data_file": data_file.map(|path| path.display().to_string()),
            "format": decode_format,
            "codepage": format!(\"{:?}\", codepage),
            "iterations": run_iterations,
            "iterations_executed": run_summaries.len(),
            "display_throughput_gbps": display_throughput_gbps,
            "comp3_throughput_mbps": comp3_throughput_mbps,
            "throughput_bps": clamp_u64(avg_throughput_bytes),
            "avg_processing_ms": avg_processing_ms,
            "avg_records_processed": avg_processed_records,
            "avg_records_with_errors": avg_errors,
            "record_rate": record_rate,
            "peak_memory_mb": peak_memory_mb,
            "quality_ratio_display": display_ratio,
            "quality_ratio_comp3": comp3_ratio,
            "overhead_percent": overhead,
            "comparison": format!(\"{:?}\", comparison_result),
            "regressions": regressions,
            "parse_issues": parse_issues,
            "include_regression_analysis": include_regression_analysis,
            "target_display_gbps": target_display_gbps,
            "target_comp3_mbps": target_comp3_mbps,
            "max_overhead_percent": max_overhead_percent,
            "status": status,
            "status_code": format!(\"{} ({})\", status_code.as_i32(), status_code),
            "audit_event_id": event.event_id,
            "audit_event_integrity": event.integrity_hash,
        }
    });

    atomic_write(output, |writer| {
        let body = serde_json::to_vec_pretty(&report)?;
        writer.write_all(&body)?;
        writer.write_all(b\"\\n\")?;
        Ok(())
    })?;

    write_stdout_line(&format!(
        \"Performance audit written to {} ({} iterations)\",
        output.display(),
        run_iterations
    ))?;
    Ok(status_code)
}

#[allow(
    clippy::too_many_arguments,
    clippy::fn_params_excessive_bools,
    clippy::used_underscore_binding
)]
fn run_security_audit(
    _copybook: &std::path::Path,
    _classification: Option<DataClassification>,
    output: &std::path::Path,
    _access_log: Option<&std::path::Path>,
    _detect_anomalies: bool,
    _validate_encryption: bool,
    _check_access_patterns: bool,
    siem_format: Option<&str>,
    siem_vendor: Option<&str>,
    export_events: Option<&std::path::Path>,
    real_time_monitoring: bool,
    validation_depth: ValidationDepth,
    threat_assessment: bool,
    _audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    return run_security_audit_impl(
        _copybook,
        _classification,
        output,
        _access_log,
        _detect_anomalies,
        _validate_encryption,
        _check_access_patterns,
        siem_format,
        siem_vendor,
        export_events,
        real_time_monitoring,
        validation_depth,
        threat_assessment,
        _audit_context,
    );
}

fn run_security_audit_impl(
    copybook: &std::path::Path,
    classification: Option<DataClassification>,
    output: &std::path::Path,
    access_log: Option<&std::path::Path>,
    detect_anomalies: bool,
    validate_encryption: bool,
    check_access_patterns: bool,
    siem_format: Option<&str>,
    siem_vendor: Option<&str>,
    export_events: Option<&std::path::Path>,
    real_time_monitoring: bool,
    validation_depth: ValidationDepth,
    threat_assessment: bool,
    audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    write_stdout_line("Running security audit (implemented)...")?;

    let schema = parse_copybook_schema(copybook)?;
    let sensitive_fields = collect_sensitive_fields(&schema);
    let mut parse_issues = Vec::new();
    let mut access_events = Vec::new();
    let mut access_event_violations = Vec::new();
    let mut pattern_violations = Vec::new();
    let mut anomaly_violations = Vec::new();
    let mut access_failures = 0u64;

    if let Some(access_log_path) = access_log {
        let (events, issues) = parse_access_events(access_log_path)?;
        parse_issues.extend(issues);
        access_events = events;

        let access_auditor = AccessAuditor::new();
        let security_auditor = SecurityAuditor::new();
        let security_monitor = SecurityMonitor::new();

        for event in &access_events {
            let validation = security_auditor.audit_access_event(event)?;
            if !validation.is_compliant {
                access_event_violations.extend(validation.violations);
            }

            if matches!(event.result, AccessResult::Failed | AccessResult::Denied) {
                access_failures += 1;
            }
        }

        if check_access_patterns {
            pattern_violations.extend(access_auditor.audit_access_pattern(&access_events));
        }

        if detect_anomalies {
            anomaly_violations.extend(security_monitor.detect_threats(access_failures as usize));
        }
    }

    let mut affected_resources = Vec::new();
    let mut threat_indicators = Vec::new();
    let mut remediation_actions = Vec::new();

    for event in &access_events {
        let key = format!("{}:{}", event.resource_type, event.resource_id);
        if !affected_resources.contains(&key) {
            affected_resources.push(key);
        }
    }

    let mut all_violations = Vec::new();
    all_violations.extend(access_event_violations);
    all_violations.extend(pattern_violations);
    if threat_assessment || detect_anomalies {
        all_violations.extend(anomaly_violations.clone());
    }

    for violation in &all_violations {
        threat_indicators.push(violation.description.clone());
        remediation_actions.push(format!(
            "Address violation {}: {}",
            violation.violation_id, violation.description
        ));
    }

    let mut sensitive_encryption_risk = None;
    if validate_encryption {
        let unencrypted_sensitive_count = sensitive_fields
            .iter()
            .filter(|field| !field.contains("ENC") && !field.contains("CRYPT"))
            .count();

        if unencrypted_sensitive_count > 0 {
            sensitive_encryption_risk = Some(format!(
                "Detected {unencrypted_sensitive_count} sensitive fields without explicit encryption metadata"
            ));
            threat_indicators.push(sensitive_encryption_risk.clone().unwrap_or_default());
            remediation_actions.push("Review encryption strategy for sensitive fields".to_string());
            all_violations.push(SecurityViolation {
                violation_id: format!("SEC-{}", generate_random_suffix()),
                severity: if matches!(
                    validation_depth,
                    ValidationDepth::Forensic | ValidationDepth::Comprehensive
                ) {
                    "critical".to_string()
                } else {
                    "medium".to_string()
                },
                description: sensitive_encryption_risk.clone().unwrap_or_else(|| {
                    "Sensitive field encryption status requires manual review".to_string()
                }),
            });
        }
    }

    if validation_depth == ValidationDepth::Forensic || validation_depth == ValidationDepth::Comprehensive {
        for event in &access_events {
            if event.source_ip.is_none() {
                threat_indicators.push(format!(
                    "Forensic mode flagged missing source IP for user {} on resource {}",
                    event.user_id, event.resource_id
                ));
            }
            if event.user_agent.is_none() {
                threat_indicators.push(format!(
                    "Forensic mode flagged missing user-agent for user {} on resource {}",
                    event.user_id, event.resource_id
                ));
            }
        }
    }

    let mut highest_severity = "low".to_string();
    let mut critical_count = 0u32;
    let mut high_count = 0u32;
    let mut medium_count = 0u32;

    for violation in &all_violations {
        match violation.severity.as_str() {
            "critical" => {
                critical_count += 1;
                highest_severity = "critical".to_string();
            }
            "high" if highest_severity != "critical" => {
                high_count += 1;
                highest_severity = "high".to_string();
            }
            "medium" if !matches!(highest_severity.as_str(), "high" | "critical") => {
                medium_count += 1;
                highest_severity = "medium".to_string();
            }
            "low" if highest_severity == "low" => {}
            _ => {}
        }
    }

    let sec_event_type = if critical_count > 0 {
        SecurityEventType::DataBreach
    } else if high_count > 0 {
        SecurityEventType::AuthorizationFailure
    } else {
        SecurityEventType::SuspiciousActivity
    };

    let status = if critical_count > 0 {
        "fail"
    } else if high_count > 0 || medium_count > 0 {
        "warn"
    } else {
        "pass"
    };

    let exit_code = if critical_count > 0 {
        ExitCode::Encode
    } else if high_count > 0 || medium_count > 0 || !parse_issues.is_empty() {
        ExitCode::Data
    } else {
        ExitCode::Ok
    };

    if remediation_actions.is_empty() {
        remediation_actions.push("No remediation required".to_string());
    }

    let payload = AuditPayload::SecurityEvent {
        security_event_type: if all_violations.is_empty() {
            SecurityEventType::UnauthorizedAccess
        } else {
            sec_event_type
        },
        severity: highest_severity.clone(),
        affected_resources: affected_resources.clone(),
        threat_indicators: threat_indicators.clone(),
        remediation_actions: remediation_actions.clone(),
        incident_id: if critical_count > 0 {
            Some(format!("SEC-INC-{}", generate_random_suffix()))
        } else {
            None
        },
    };

    let security_context = audit_context
        .with_metadata("operation", "security_audit".to_string())
        .with_metadata("classification", format!("{classification:?}"))
        .with_metadata("validation_depth", format!("{validation_depth:?}"));
    let security_context = security_context
        .with_security_classification(security_classification(classification));
    let event = AuditEvent::new(
        AuditEventType::SecurityEvent,
        security_context,
        payload,
    );

    let mut report = serde_json::json!({
        "security_audit": {
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "copybook": copybook.display().to_string(),
            "classification": format!("{classification:?}"),
            "security_classification": format!("{:?}", security_classification(classification)),
            "sensitive_fields": sensitive_fields,
            "sensitive_field_count": sensitive_fields.len(),
            "access_log": access_log.map(|path| path.display().to_string()),
            "access_event_count": access_events.len(),
            "parse_issues": parse_issues,
            "detect_anomalies": detect_anomalies,
            "validate_encryption": validate_encryption,
            "check_access_patterns": check_access_patterns,
            "validation_depth": format!("{validation_depth:?}"),
            "threat_assessment": threat_assessment,
            "real_time_monitoring": real_time_monitoring,
            "siem_vendor": siem_vendor,
            "sensitive_encryption_risk": sensitive_encryption_risk,
            "threat_count": all_violations.len(),
            "threat_indicators": threat_indicators,
            "access_failures": access_failures,
            "violations": all_violations.iter().map(|violation| serde_json::json!({
                "violation_id": violation.violation_id,
                "severity": violation.severity,
                "description": violation.description,
            })).collect::<Vec<_>>(),
            "affected_resources": affected_resources,
            "audit_event_id": event.event_id,
            "audit_event_integrity": event.integrity_hash,
            "status": status,
            "status_code": format!("{} ({})", exit_code.as_i32(), exit_code),
        }
    });

    if let Some(events_path) = export_events {
        let siem_export = match siem_format.unwrap_or("json").to_ascii_lowercase().as_str() {
            "cef" => {
                vec![event]
                    .into_iter()
                    .map(|event| {
                        format!(
                            "CEF:0|copybook-rs|Audit|1.0|{}|Security Event|{}|src={} event={} resources={}\n",
                            event.event_type as i32,
                            event.severity as usize,
                            event.source,
                            event.context.operation_id
                        )
                    })
                    .collect::<String>()
            }
            "leef" => {
                vec![event]
                    .into_iter()
                    .map(|event| {
                        format!(
                            "LEEF:2.0|copybook-rs|Audit|1.0|security|devTime={} src={} event={} sev={}\n",
                            event.timestamp, event.source, event.event_id, event.severity as usize
                        )
                    })
                    .collect::<String>()
            }
            "json" => serde_json::to_string_pretty(&event)?,
            _ => serde_json::to_string_pretty(&event)?,
        };
        fs::write(events_path, siem_export)?;
        report["security_audit"]["siem_exported_path"] =
            serde_json::Value::String(events_path.display().to_string());
    } else if siem_format.is_some() {
        report["security_audit"]["siem_export_note"] =
            serde_json::Value::String("siem_format provided without export_events path".to_string());
    }

    atomic_write(output, |writer| {
        let body = serde_json::to_vec_pretty(&report)?;
        writer.write_all(&body)?;
        writer.write_all(b"\n")?;
        Ok(())
    })?;

    write_stdout_line(&format!(
        "Security audit written to {} ({} findings)",
        output.display(),
        all_violations.len()
    ))?;
    Ok(exit_code)
}

#[allow(clippy::too_many_arguments, clippy::fn_params_excessive_bools)]
fn run_audit_health_check(
    audit_trail: Option<&std::path::Path>,
    audit_log: Option<&std::path::Path>,
    validate_integrity: bool,
    validate_chain_integrity: bool,
    check_cryptographic_hashes: bool,
    verify_timestamps: bool,
    check_retention: bool,
    output: Option<&std::path::Path>,
    detailed_diagnostics: bool,
    check_interval: u32,
    continuous: bool,
    _audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    return run_audit_health_check_impl(
        audit_trail,
        audit_log,
        validate_integrity,
        validate_chain_integrity,
        check_cryptographic_hashes,
        verify_timestamps,
        check_retention,
        output,
        detailed_diagnostics,
        check_interval,
        continuous,
        _audit_context,
    );
}

fn run_audit_health_check_impl(
    audit_trail: Option<&std::path::Path>,
    audit_log: Option<&std::path::Path>,
    validate_integrity: bool,
    validate_chain_integrity: bool,
    check_cryptographic_hashes: bool,
    verify_timestamps: bool,
    check_retention: bool,
    output: Option<&std::path::Path>,
    detailed_diagnostics: bool,
    check_interval: u32,
    continuous: bool,
    audit_context: AuditContext,
) -> Result<ExitCode, Box<dyn std::error::Error + Send + Sync>> {
    write_stdout_line("Running audit health check (implemented)...")?;

    if audit_trail.is_none() && audit_log.is_none() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "one of --audit-trail or --audit-log is required",
        )
        .into());
    }

    let mut parse_issues = Vec::new();
    let mut diagnostics = Vec::new();
    let mut health_events = Vec::new();
    let mut security_events: Vec<AuditEvent> = Vec::new();

    let mut chain_integrity_valid = true;
    let mut hash_chain_valid = true;
    let mut timestamps_valid = true;
    let mut retention_compliant = true;
    let mut checks_executed = 0u32;
    let mut checks_failed = 0u32;

    let now = chrono::Utc::now();
    let retention_limit = if check_interval == 0 {
        None
    } else {
        Some(now - ChronoDuration::minutes(i64::from(check_interval)))
    };

    let perform_chain_validation = validate_integrity || validate_chain_integrity;
    let perform_hash_validation = validate_integrity || check_cryptographic_hashes;

    if let Some(audit_trail_path) = audit_trail {
        let (records, issues) = parse_health_events(audit_trail_path)?;
        parse_issues.extend(issues);
        health_events = records;

        if perform_chain_validation {
            checks_executed += 1;
            let chain_valid = health_events
                .windows(2)
                .all(|records| {
                    records.get(1).is_some_and(|next| {
                        next.previous_hash
                            .as_deref()
                            .is_none_or(|previous| previous == records[0].integrity_hash.as_str())
                    })
                });
            if !chain_valid {
                chain_integrity_valid = false;
                checks_failed += 1;
                diagnostics.push("audit_trail chain continuity broken".to_string());
            }
            if chain_integrity_valid {
                diagnostics.push("audit_trail chain continuity valid".to_string());
            }
        }

        if perform_hash_validation {
            checks_executed += 1;
            let hashes_present = health_events
                .iter()
                .all(|record| !record.integrity_hash.is_empty());
            if !hashes_present {
                hash_chain_valid = false;
                checks_failed += 1;
                diagnostics.push("audit_trail contains empty or missing integrity hash values".to_string());
            }
        }

        if verify_timestamps {
            checks_executed += 1;
            let timestamps = health_events
                .iter()
                .map(|record| DateTime::parse_from_rfc3339(&record.timestamp).map(|value| value.with_timezone(&chrono::Utc)))
                .collect::<Vec<_>>();
            for parsed in &timestamps {
                if parsed.is_err() {
                    timestamps_valid = false;
                    checks_failed += 1;
                    diagnostics.push("audit_trail timestamp parsing failed for one or more records".to_string());
                    break;
                }
            }
            if timestamps_valid {
                if let Some(window) = retention_limit {
                    let too_old = timestamps.iter().any(|entry| {
                        entry
                            .as_ref()
                            .is_ok_and(|timestamp| timestamp < &window)
                    });
                    if too_old {
                        retention_compliant = false;
                    }
                }
                diagnostics.push("audit_trail timestamps parsed successfully".to_string());
            }
        }

        if check_retention {
            checks_executed += 1;
            if let Some(window) = retention_limit {
                let old_records = health_events.iter().filter(|record| {
                    DateTime::parse_from_rfc3339(&record.timestamp)
                        .map(|timestamp| timestamp.with_timezone(&chrono::Utc) < window)
                        .unwrap_or(false)
                });
                if old_records.clone().count() > 0 {
                    retention_compliant = false;
                    checks_failed += 1;
                    diagnostics.push(format!(
                        "audit_trail has {} record(s) older than {} minute retention window",
                        old_records.count(),
                        check_interval
                    ));
                }
            }
        }
    }

    if let Some(audit_log_path) = audit_log {
        let (events, issues) = parse_audit_events_for_health(audit_log_path)?;
        parse_issues.extend(issues);
        security_events = events;

        if perform_chain_validation {
            checks_executed += 1;
            match audit_core::validate_audit_chain(&security_events) {
                Ok(true) => diagnostics.push("audit_log chain integrity validated".to_string()),
                Ok(false) | Err(_) => {
                    chain_integrity_valid = false;
                    checks_failed += 1;
                    diagnostics.push("audit_log chain integrity validation failed".to_string());
                }
            }
        }

        if perform_hash_validation {
            checks_executed += 1;
            let mut valid = true;
            for window in security_events.windows(2) {
                let previous = &window[0];
                let current = &window[1];
                if current.previous_hash.as_deref() != Some(previous.integrity_hash.as_str()) {
                    valid = false;
                    break;
                }
            }
            hash_chain_valid &= valid;
            if !valid {
                checks_failed += 1;
                diagnostics.push("audit_log previous_hash linkage broken".to_string());
            }
        }

        if verify_timestamps {
            checks_executed += 1;
            let mut last = None;
            for event in &security_events {
                let parsed = match DateTime::parse_from_rfc3339(&event.timestamp) {
                    Ok(value) => value.with_timezone(&chrono::Utc),
                    Err(_) => {
                        timestamps_valid = false;
                        checks_failed += 1;
                        diagnostics.push("audit_log timestamp parsing failed".to_string());
                        break;
                    }
                };
                if let Some(prev) = last {
                    if parsed < prev {
                        timestamps_valid = false;
                        checks_failed += 1;
                        diagnostics.push("audit_log timestamp order is not monotonic".to_string());
                        break;
                    }
                }
                last = Some(parsed);
            }
        }

        if check_retention {
            checks_executed += 1;
            if let Some(window) = retention_limit {
                let mut old_records = 0u32;
                for event in &security_events {
                    let parsed = DateTime::parse_from_rfc3339(&event.timestamp)
                        .map(|timestamp| timestamp.with_timezone(&chrono::Utc));
                    if parsed.is_ok_and(|timestamp| timestamp < window) {
                        old_records += 1;
                    }
                }
                if old_records > 0 {
                    retention_compliant = false;
                    checks_failed += 1;
                    diagnostics.push(format!(
                        "audit_log has {old_records} record(s) outside configured retention window ({} min)",
                        check_interval
                    ));
                }
            }
        }
    }

    if perform_chain_validation || perform_hash_validation || verify_timestamps || check_retention {
        checks_executed = checks_executed.max(1);
    } else if diagnostics.is_empty() {
        diagnostics.push("no checks selected".to_string());
    }

    if !detailed_diagnostics {
        diagnostics.retain(|message| !message.is_empty());
    }

    if continuous {
        diagnostics.push(
            "continuous monitoring mode requested (one-shot validation only in this command)".to_string(),
        );
    }

    let overall_health_score = if checks_executed == 0 {
        0
    } else {
        let failed_penalty = (checks_failed * 100) / checks_executed;
        (100u32.saturating_sub(failed_penalty)).min(100)
    };

    let status_code = if checks_failed > 0 || !parse_issues.is_empty() {
        ExitCode::Data
    } else {
        ExitCode::Ok
    };

    let status = if checks_failed > 0 {
        "warn"
    } else if parse_issues.is_empty() {
        "pass"
    } else {
        "warn"
    };

    let health_payload = AuditPayload::ErrorEvent {
        error_code: if status == "warn" {
            "AUDIT_HEALTH_WARNING".to_string()
        } else {
            "AUDIT_HEALTH_OK".to_string()
        },
        error_message: format!(
            "Audit trail health score {:.0}/100, parse_issues {}, diagnostics {}",
            overall_health_score,
            parse_issues.len(),
            diagnostics.len()
        ),
        error_category: "audit_health".to_string(),
        stack_trace: None,
        context_information: {
            let mut map = HashMap::new();
            map.insert(
                "audit_trail".to_string(),
                audit_trail
                    .map(|path| path.display().to_string())
                    .unwrap_or_else(|| "none".to_string()),
            );
            map.insert(
                "audit_log".to_string(),
                audit_log
                    .map(|path| path.display().to_string())
                    .unwrap_or_else(|| "none".to_string()),
            );
            map.insert(
                "checks_executed".to_string(),
                checks_executed.to_string(),
            );
            map.insert(
                "checks_failed".to_string(),
                checks_failed.to_string(),
            );
            map.insert(
                "overall_health_score".to_string(),
                overall_health_score.to_string(),
            );
            map
        },
        recovery_actions: diagnostics.clone(),
        user_impact: if status == "pass" {
            copybook_core::audit::UserImpactLevel::None
        } else {
            copybook_core::audit::UserImpactLevel::Medium
        },
    };

    let health_context = audit_context
        .with_metadata("operation", "audit_health_check".to_string())
        .with_metadata("validate_integrity", format!("{validate_integrity}"))
        .with_metadata("validate_chain_integrity", format!("{validate_chain_integrity}"))
        .with_metadata("check_cryptographic_hashes", format!("{check_cryptographic_hashes}"));
    let health_event = AuditEvent::new(
        AuditEventType::ErrorEvent,
        health_context,
        health_payload,
    );

    let report = serde_json::json!({
        "audit_health": {
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "audit_trail": audit_trail.map(|path| path.display().to_string()),
            "audit_log": audit_log.map(|path| path.display().to_string()),
            "validate_integrity": validate_integrity,
            "validate_chain_integrity": validate_chain_integrity,
            "check_cryptographic_hashes": check_cryptographic_hashes,
            "verify_timestamps": verify_timestamps,
            "check_retention": check_retention,
            "detailed_diagnostics": detailed_diagnostics,
            "check_interval_minutes": check_interval,
            "continuous": continuous,
            "health_status": {
                "overall": status,
                "overall_health_score": overall_health_score,
                "chain_integrity_valid": chain_integrity_valid,
                "hash_chain_valid": hash_chain_valid,
                "timestamps_valid": timestamps_valid,
                "retention_compliant": retention_compliant,
                "checks_executed": checks_executed,
                "checks_failed": checks_failed,
            },
            "hash_verification_results": diagnostics,
            "chain_verification": {
                "audit_trail_events": health_events.len(),
                "audit_log_events": security_events.len(),
                "chain_integrity_valid": chain_integrity_valid,
                "hash_chain_valid": hash_chain_valid,
            },
            "parse_issues": parse_issues,
            "audit_event_id": health_event.event_id,
            "audit_event_integrity": health_event.integrity_hash,
            "status": status,
            "status_code": format!("{} ({})", status_code.as_i32(), status_code),
        }
    });

    if let Some(output_path) = output {
        atomic_write(output_path, |writer| {
            let body = serde_json::to_vec_pretty(&report)?;
            writer.write_all(&body)?;
            writer.write_all(b"\n")?;
            Ok(())
        })?;
        write_stdout_line(&format!(
            "Audit health report written to {} (status {})",
            output_path.display(),
            status
        ))?;
    } else {
        write_stdout_line(&format!(
            "Audit health check completed (status {}, score {overall_health_score})"
        ))?;
    }

    Ok(status_code)
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_compliance_framework_conversion() {
        assert_eq!(
            ComplianceProfile::from(ComplianceFramework::SOX),
            ComplianceProfile::SOX
        );
        assert_eq!(
            ComplianceProfile::from(ComplianceFramework::HIPAA),
            ComplianceProfile::HIPAA
        );
        assert_eq!(
            ComplianceProfile::from(ComplianceFramework::GDPR),
            ComplianceProfile::GDPR
        );
        assert_eq!(
            ComplianceProfile::from(ComplianceFramework::PCI),
            ComplianceProfile::PciDss
        );
    }

    #[tokio::test]
    async fn test_compliance_validation() -> anyhow::Result<()> {
        use std::io::Write;
        use tempfile::{NamedTempFile, tempdir};

        let compliance = "sox";

        // Create temporary copybook file
        let mut temp_file = NamedTempFile::new()?;
        writeln!(
            temp_file,
            r"       01 TEST-RECORD.
           05 TEST-FIELD           PIC X(10)."
        )?;
        let copybook_path = temp_file.path().to_path_buf();

        let temp_dir = tempdir()?;
        let output_path = temp_dir.path().join("compliance.json");
        let audit_context = AuditContext::new();

        let exit_code = run_compliance_validation(
            compliance,
            &copybook_path,
            None,
            None,
            Codepage::CP037,
            false,
            &output_path,
            false,
            false,
            false,
            audit_context,
        )
        .await
        .map_err(|err| anyhow::Error::msg(err.to_string()))?;

        assert_eq!(exit_code, ExitCode::Encode); // Compliance failure
        Ok(())
    }
}
