// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive CLI Audit Command Tests
//!
//! Tests feature spec: enterprise-audit-system-spec.md#cli-integration
//! API contracts: audit-api-reference.md#cli-commands
//!
//! Tests all 6 audit subcommands with comprehensive scenarios:
//! - `copybook audit report` - Generate comprehensive audit reports (AC7)
//! - `copybook audit validate` - Run compliance validation checks (AC7)
//! - `copybook audit lineage` - Generate data lineage reports (AC7)
//! - `copybook audit performance` - Performance audit and baseline validation (AC7)
//! - `copybook audit security` - Security audit and access pattern analysis (AC7)
//! - `copybook audit health` - Audit trail integrity and health checking (AC7)
//!
//! Note: These tests require the `audit` feature to be enabled during compilation.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "audit")]

mod test_utils;

use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;
use tempfile::tempdir;
use test_utils::TestResult;

/// Tests feature spec: enterprise-audit-system-spec.md#cli-integration
/// Test audit report command with comprehensive enterprise scenarios (AC7)
#[test]
fn test_audit_report_comprehensive() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let copybook_file = temp_dir.path().join("financial_sox.cpy");
    let output_file = temp_dir.path().join("audit_report.json");

    // Create test copybook fixture
    fs::write(
        &copybook_file,
        r"       01 FINANCIAL-TRANSACTION.
           05 ACCOUNT-ID           PIC 9(12) COMP-3.
           05 TRANSACTION-AMOUNT   PIC S9(13)V99 COMP-3.
           05 AUDIT-TRAIL-REF      PIC X(32).
    ",
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("report")
        .arg("--compliance")
        .arg("sox")
        .arg("--format")
        .arg("json")
        .arg("--output")
        .arg(&output_file)
        .arg("--include-recommendations")
        .arg("--include-lineage")
        .arg(&copybook_file);

    // Report may return non-zero when compliance violations are detected,
    // but should still produce the report file.
    let _ = cmd.output()?;

    assert!(output_file.exists(), "audit report should be generated");
    let report: Value = serde_json::from_str(&fs::read_to_string(&output_file)?)?;
    assert!(
        report.get("audit_report").is_some(),
        "expected top-level audit_report section"
    );

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#compliance-validation
/// Test audit validate command with multiple compliance frameworks (AC3, AC7)
#[test]
fn test_audit_validate_multi_compliance() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let copybook_file = temp_dir.path().join("healthcare_hipaa.cpy");
    let validation_report = temp_dir.path().join("validation_report.json");

    // Create HIPAA test copybook fixture
    fs::write(
        &copybook_file,
        r"
       01 PATIENT-RECORD.
           05 PATIENT-ID           PIC 9(10) COMP-3.
           05 SSN-LAST-FOUR        PIC 9(4) COMP.
           05 PHI-CATEGORY         PIC X(2).
               88 DEMOGRAPHIC-INFO VALUE 'DM'.
               88 MEDICAL-HISTORY  VALUE 'MH'.
           05 CONSENT-STATUS       PIC X(1).
               88 CONSENT-OBTAINED VALUE 'Y'.
    ",
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("validate")
        .arg("--compliance")
        .arg("hipaa,gdpr")
        .arg("--strict")
        .arg("--output")
        .arg(&validation_report)
        .arg("--report-violations")
        .arg("--include-recommendations")
        .arg(&copybook_file);

    // Compliance validation may return non-zero when violations are found,
    // but should still produce a report.
    let _ = cmd.output()?;

    assert!(
        validation_report.exists(),
        "validation report should be generated"
    );
    let report: Value = serde_json::from_str(&fs::read_to_string(&validation_report)?)?;
    assert!(
        report.get("compliance_validation").is_some(),
        "expected top-level compliance_validation section"
    );

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#data-lineage
/// Test audit lineage command with field-level tracking (AC6, AC7)
#[test]
fn test_audit_lineage_field_level() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let source_copybook = temp_dir.path().join("source_schema.cpy");
    let lineage_report = temp_dir.path().join("lineage_report.json");

    // Create source copybook for lineage tracking
    fs::write(
        &source_copybook,
        r"
       01 SOURCE-RECORD.
           05 CUSTOMER-ID          PIC 9(8) COMP-3.
           05 TRANSACTION-AMOUNT   PIC S9(11)V99 COMP-3.
           05 PROCESSING-DATE      PIC 9(8) COMP-3.
           05 STATUS-CODE          PIC X(2).
               88 ACTIVE-STATUS    VALUE 'AC'.
               88 INACTIVE-STATUS  VALUE 'IN'.
    ",
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("lineage")
        .arg("--source-system")
        .arg("test-system")
        .arg("--source")
        .arg(&source_copybook)
        .arg("--target-format")
        .arg("json")
        .arg("--field-level")
        .arg("--transformation-details")
        .arg("--quality-metrics")
        .arg("--output")
        .arg(&lineage_report)
        .arg(&source_copybook);

    let _ = cmd.output()?;

    assert!(
        lineage_report.exists(),
        "lineage report should be generated"
    );
    let report: Value = serde_json::from_str(&fs::read_to_string(&lineage_report)?)?;
    let lineage = report
        .get("lineage_analysis")
        .and_then(|v| v.as_object())
        .expect("expected lineage_analysis object");
    assert_eq!(
        lineage.get("source_system").and_then(Value::as_str),
        Some("test-system")
    );
    assert_eq!(
        lineage.get("field_level").and_then(Value::as_bool),
        Some(true)
    );

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#performance-audit
/// Test audit performance command with baseline validation (AC4, AC7, AC11)
#[test]
fn test_audit_performance_baseline() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let performance_copybook = temp_dir.path().join("performance_test.cpy");
    let performance_report = temp_dir.path().join("performance_report.json");

    // Create performance test copybook
    fs::write(
        &performance_copybook,
        r"
       01 PERFORMANCE-RECORD.
           05 DISPLAY-HEAVY-SECTION.
               10 CUSTOMER-NAME        PIC X(100).
               10 ADDRESS-LINE-1       PIC X(100).
               10 DESCRIPTION          PIC X(500).
           05 COMP3-HEAVY-SECTION.
               10 ACCOUNT-BALANCE      PIC S9(13)V99 COMP-3.
               10 TRANSACTION-AMOUNT   PIC S9(11)V99 COMP-3.
               10 INTEREST-RATE        PIC S9(3)V9999 COMP-3.
    ",
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("performance")
        .arg("--output")
        .arg(&performance_report)
        .arg("--iterations")
        .arg("1")
        .arg(&performance_copybook);

    // Performance audit may return non-zero (warn) depending on decode error rate;
    // it should still emit a report.
    let _ = cmd.output()?;

    assert!(
        performance_report.exists(),
        "performance report should be generated"
    );
    let report: Value = serde_json::from_str(&fs::read_to_string(&performance_report)?)?;
    assert!(
        report.get("performance_audit").is_some(),
        "expected top-level performance_audit section"
    );

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#security-audit
/// Test audit security command with access pattern analysis (AC5, AC7)
#[test]
fn test_audit_security_comprehensive() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let security_copybook = temp_dir.path().join("security_test.cpy");
    let security_report = temp_dir.path().join("security_report.json");
    let access_log = temp_dir.path().join("access_log.jsonl");

    // Create security test copybook with sensitive data
    fs::write(
        &security_copybook,
        r"
       01 SENSITIVE-RECORD.
           05 SSN                  PIC 9(9) COMP-3.
               88 VALID-SSN        VALUE 100000000 THRU 999999999.
           05 CREDIT-CARD-NUMBER   PIC 9(16) COMP-3.
           05 ACCOUNT-PASSWORD     PIC X(64).
           05 ENCRYPTION-STATUS    PIC X(1).
               88 ENCRYPTED        VALUE 'Y'.
               88 UNENCRYPTED      VALUE 'N'.
    ",
    )?;

    // Create mock access log
    fs::write(&access_log, "
{\"timestamp\": \"2024-09-25T10:00:00Z\", \"user\": \"test_user\", \"action\": \"read\", \"resource\": \"sensitive_record\"}
{\"timestamp\": \"2024-09-25T10:01:00Z\", \"user\": \"test_user\", \"action\": \"write\", \"resource\": \"sensitive_record\"}
    ")?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("security")
        .arg("--access-log")
        .arg(&access_log)
        .arg("--detect-anomalies")
        .arg("--validate-encryption")
        .arg("--check-access-patterns")
        .arg("--output")
        .arg(&security_report)
        .arg(&security_copybook);

    // Security audit may return non-zero when findings are detected,
    // but should still produce the report file.
    let _ = cmd.output()?;

    assert!(
        security_report.exists(),
        "security report should be generated"
    );
    let report: Value = serde_json::from_str(&fs::read_to_string(&security_report)?)?;
    assert!(
        report.get("security_audit").is_some(),
        "expected top-level security_audit section"
    );

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#audit-trail-integrity
/// Test audit health command with integrity validation (AC10, AC7)
#[test]
fn test_audit_health_integrity() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let audit_log = temp_dir.path().join("audit_trail.jsonl");
    let health_report = temp_dir.path().join("health_report.json");

    // Create mock audit trail (valid JSONL — one JSON object per line)
    fs::write(
        &audit_log,
        "{\"event_id\":\"audit-001\",\"timestamp\":\"2024-09-25T10:00:00Z\",\"integrity_hash\":\"a1b2c3d4\",\"previous_hash\":null}\n\
         {\"event_id\":\"audit-002\",\"timestamp\":\"2024-09-25T10:01:00Z\",\"integrity_hash\":\"e5f6g7h8\",\"previous_hash\":\"a1b2c3d4\"}\n\
         {\"event_id\":\"audit-003\",\"timestamp\":\"2024-09-25T10:02:00Z\",\"integrity_hash\":\"i9j0k1l2\",\"previous_hash\":\"e5f6g7h8\"}\n",
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("health")
        .arg("--audit-log")
        .arg(&audit_log)
        .arg("--validate-chain-integrity")
        .arg("--check-cryptographic-hashes")
        .arg("--verify-timestamps")
        .arg("--output")
        .arg(&health_report)
        .arg("--detailed-diagnostics");

    // Health check may return non-zero (warn) when issues are detected,
    // but should still produce the report file.
    let _ = cmd.output()?;

    assert!(health_report.exists(), "health report should be generated");
    let report: Value = serde_json::from_str(&fs::read_to_string(&health_report)?)?;
    assert!(
        report.get("audit_health").is_some(),
        "expected top-level audit_health section"
    );

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#cli-integration
/// Test audit command error handling and validation
#[test]
fn test_audit_command_error_handling() -> TestResult<()> {
    // Test missing arguments
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit").arg("validate");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("required arguments"));

    // Test invalid compliance profile
    let temp_dir = tempdir()?;
    let copybook_file = temp_dir.path().join("test.cpy");
    fs::write(
        &copybook_file,
        "01 TEST-RECORD.\n   05 TEST-FIELD PIC X(10).",
    )?;

    let output_file = temp_dir.path().join("validation_output.json");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("validate")
        .arg("--compliance")
        .arg("invalid_profile")
        .arg("--output")
        .arg(&output_file)
        .arg(&copybook_file);

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("invalid compliance profile"));

    // Test nonexistent file
    let output_file2 = temp_dir.path().join("report_output.json");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("report")
        .arg("--output")
        .arg(&output_file2)
        .arg("nonexistent_file.cpy");

    // Platform-agnostic file not found error check
    // Unix: "No such file"
    // Windows: "The system cannot find the file specified"
    cmd.assert().failure().stderr(
        predicate::str::contains("No such file")
            .or(predicate::str::contains("cannot find the file")),
    );
    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#enterprise-integration
/// Test audit command with SIEM integration options (AC8)
#[test]
fn test_audit_siem_integration() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let copybook_file = temp_dir.path().join("siem_test.cpy");
    let siem_output = temp_dir.path().join("siem_events.cef");

    fs::write(
        &copybook_file,
        r"
       01 SECURITY-EVENT-RECORD.
           05 EVENT-ID             PIC 9(12) COMP-3.
           05 SECURITY-LEVEL       PIC X(1).
               88 HIGH-SECURITY    VALUE 'H'.
               88 MEDIUM-SECURITY  VALUE 'M'.
               88 LOW-SECURITY     VALUE 'L'.
           05 THREAT-INDICATOR     PIC X(50).
    ",
    )?;

    let security_report = temp_dir.path().join("security_report.json");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("security")
        .arg("--siem-format")
        .arg("cef")
        .arg("--siem-vendor")
        .arg("splunk")
        .arg("--export-events")
        .arg(&siem_output)
        .arg("--real-time-monitoring")
        .arg("--output")
        .arg(&security_report)
        .arg(&copybook_file);

    // Security audit may return non-zero when findings are detected,
    // but should still produce the report and SIEM export files.
    let _ = cmd.output()?;

    assert!(
        security_report.exists(),
        "security report should be generated"
    );
    let report: Value = serde_json::from_str(&fs::read_to_string(&security_report)?)?;
    let sec = report
        .get("security_audit")
        .and_then(|v| v.as_object())
        .expect("expected security_audit object");
    assert!(
        sec.get("siem_exported_path").is_some(),
        "expected siem_exported_path when --export-events is set"
    );

    // SIEM export file should exist
    assert!(siem_output.exists(), "SIEM events file should be generated");

    Ok(())
}
