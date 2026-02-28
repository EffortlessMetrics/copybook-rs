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
use std::io::{self, Cursor};
use tempfile::tempdir;
use test_utils::{fixture_path, TestResult};

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
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
        .arg(&copybook_file);

    let output = cmd.output()?;
    let code = output
        .status
        .code()
        .ok_or_else(|| "audit report command exited without code".to_string())?;
    assert!(
        code == 0 || code == 2 || code == 3 || code == 4,
        "unexpected audit report exit code {code}",
    );

    assert!(output_file.exists());
    let report_content = fs::read_to_string(&output_file)?;
    let report: Value = serde_json::from_str(&report_content)?;
    let audit_report = report
        .get("audit_report")
        .ok_or_else(|| "missing audit_report section".to_string())?;
    let sections = audit_report
        .get("sections")
        .ok_or_else(|| "missing report sections".to_string())?;
    assert!(sections.get("compliance_validation").is_some());
    assert!(sections.get("lineage_analysis").is_some());
    let compliance = sections["compliance_validation"]["compliance_validation"]
        .as_object()
        .ok_or_else(|| "missing compliance section body".to_string())?;
    assert!(compliance.get("frameworks").is_some());
    let lineage = sections["lineage_analysis"]["lineage_analysis"]
        .as_object()
        .ok_or_else(|| "missing lineage section body".to_string())?;
    assert!(lineage.get("source_lineage_id").is_some());
    assert!(lineage.get("target_lineage_id").is_some());
    assert!(
        lineage["source_root_path"].as_str().is_some(),
        "source root path missing"
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
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
        .arg("--output")
        .arg(&validation_report)
        .arg("--report-violations")
        .arg("--include-recommendations")
        .arg(&copybook_file);

    let output = cmd.output()?;
    let code = output
        .status
        .code()
        .ok_or_else(|| "audit validate command exited without code".to_string())?;
    assert!(
        code == 0 || code == 2 || code == 3 || code == 4,
        "unexpected audit validate exit code {code}",
    );

    // Verify validation report generated
    if validation_report.exists() {
        let report_content = fs::read_to_string(&validation_report)?;
        let report: Value = serde_json::from_str(&report_content)?;
        let validation = report
            .get("compliance_validation")
            .ok_or_else(|| "missing compliance_validation section".to_string())?;
        assert_eq!(validation["strict_mode"].as_bool(), Some(true));
        assert!(validation["frameworks"].as_array().is_some());
        assert!(validation["violation_count"].is_number());
        assert!(validation["status"].as_str().is_some());
    }
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
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
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

    let output = cmd.output()?;
    let code = output
        .status
        .code()
        .ok_or_else(|| "audit lineage command exited without code".to_string())?;
    assert!(
        code == 0 || code == 2 || code == 3 || code == 4,
        "unexpected audit lineage exit code {code}",
    );

    assert!(lineage_report.exists());
    let lineage_content = fs::read_to_string(&lineage_report)?;
    let report: Value = serde_json::from_str(&lineage_content)?;
    let lineage = report
        .get("lineage_analysis")
        .ok_or_else(|| "missing lineage_analysis section".to_string())?;
    assert_eq!(lineage["field_level"].as_bool(), Some(true));
    assert_eq!(lineage["transformation_details"].as_bool(), Some(true));
    assert_eq!(lineage["quality_metrics"].as_bool(), Some(true));
    assert!(lineage["field_mappings"].is_array());
    assert!(lineage["lineage_record_count"].is_number());
    Ok(())
}

#[test]
fn test_audit_lineage_ids_match_parser_and_codec_for_fixture_inputs() -> TestResult<()> {
    let source_copybook = fixture_path("copybooks/simple.cpy")?;
    let target_copybook = fixture_path("copybooks/complex.cpy")?;

    let temp_dir = tempdir()?;
    let lineage_report = temp_dir.path().join("lineage.json");

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1");
    cmd.arg("audit")
        .arg("lineage")
        .arg(&source_copybook)
        .arg(&target_copybook)
        .arg("--source-system")
        .arg("fixture-source")
        .arg("--target-system")
        .arg("fixture-target")
        .arg("--field-level")
        .arg("--transformation-details")
        .arg("--quality-metrics")
        .arg("--impact-analysis")
        .arg("--output")
        .arg(&lineage_report)
        .arg("--target-format")
        .arg("json");

    cmd.assert().success();

    let report_text = fs::read_to_string(&lineage_report)?;
    let report: Value = serde_json::from_str(&report_text)?;
    let analysis = report
        .get("lineage_analysis")
        .ok_or_else(|| "lineage report missing lineage_analysis section")?;

    let source_lineage_from_report = analysis["source_lineage_id"]
        .as_str()
        .ok_or_else(|| "source_lineage_id missing")?;
    let target_lineage_from_report = analysis["target_lineage_id"]
        .as_str()
        .ok_or_else(|| "target_lineage_id missing")?;
    let source_codec_lineage_from_report = analysis["source_codec_lineage_id"]
        .as_str()
        .ok_or_else(|| "source_codec_lineage_id missing")?;
    let target_codec_lineage_from_report = analysis["target_codec_lineage_id"]
        .as_str()
        .ok_or_else(|| "target_codec_lineage_id missing")?;

    let source_text = fs::read_to_string(&source_copybook)?;
    let target_text = fs::read_to_string(&target_copybook)?;
    let (source_schema, source_metadata) = copybook_core::parser::parse_with_audit_lineage_metadata(
        &source_text,
        &copybook_core::ParseOptions::default(),
    )?;
    let (target_schema, target_metadata) = copybook_core::parser::parse_with_audit_lineage_metadata(
        &target_text,
        &copybook_core::ParseOptions::default(),
    )?;

    let source_codec_summary = {
        let mut sink = io::sink();
        copybook_codec::decode_file_to_jsonl(
            &source_schema,
            Cursor::new(Vec::<u8>::new()),
            &mut sink,
            &copybook_codec::DecodeOptions::default(),
        )?
    };
    let target_codec_summary = {
        let mut sink = io::sink();
        copybook_codec::decode_file_to_jsonl(
            &target_schema,
            Cursor::new(Vec::<u8>::new()),
            &mut sink,
            &copybook_codec::DecodeOptions::default(),
        )?
    };

    assert_eq!(source_lineage_from_report, source_metadata.lineage_id);
    assert_eq!(target_lineage_from_report, target_metadata.lineage_id);
    assert_eq!(
        source_codec_lineage_from_report,
        source_codec_summary.lineage_id(),
    );
    assert_eq!(
        target_codec_lineage_from_report,
        target_codec_summary.lineage_id(),
    );
    assert_eq!(source_codec_summary.lineage_id(), source_metadata.lineage_id);
    assert_eq!(target_codec_summary.lineage_id(), target_metadata.lineage_id);
    assert_ne!(source_lineage_from_report, target_lineage_from_report);

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#performance-audit
/// Test audit performance command with baseline validation (AC4, AC7, AC11)
#[test]
fn test_audit_performance_baseline() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let performance_copybook = temp_dir.path().join("performance_test.cpy");
    let baseline_file = temp_dir.path().join("performance_baseline.json");
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

    // Test baseline establishment
    let mut baseline_cmd = cargo_bin_cmd!("copybook");
    baseline_cmd
        .arg("audit")
        .arg("performance")
        .arg("--establish-baseline")
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
        .arg("--baseline-file")
        .arg(&baseline_file)
        .arg("--target-display-gbps")
        .arg("4.1")
        .arg("--target-comp3-mbps")
        .arg("560")
        .arg("--output")
        .arg(&performance_report)
        .arg(&performance_copybook);

    let baseline_output = baseline_cmd.output()?;
    let baseline_code = baseline_output
        .status
        .code()
        .ok_or_else(|| "audit performance baseline command exited without code".to_string())?;
    assert!(
        baseline_code == 0 || baseline_code == 2 || baseline_code == 3 || baseline_code == 4,
        "unexpected performance baseline exit code {baseline_code}",
    );
    assert!(baseline_file.exists());
    let baseline_report_content = fs::read_to_string(&performance_report)?;
    let baseline_report: Value = serde_json::from_str(&baseline_report_content)?;
    let baseline_payload = baseline_report
        .get("performance_audit")
        .ok_or_else(|| "missing performance_audit section".to_string())?;
    assert!(baseline_payload["comparison"].as_str().is_some());
    assert!(baseline_payload["status"].as_str().is_some());

    // Test performance validation against baseline
    let mut validation_cmd = cargo_bin_cmd!("copybook");
    validation_cmd
        .arg("audit")
        .arg("performance")
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
        .arg("--validate-against-baseline")
        .arg(&baseline_file)
        .arg("--max-overhead-percent")
        .arg("5.0") // AC11: <5% overhead
        .arg("--output")
        .arg(&performance_report)
        .arg("--include-regression-analysis")
        .arg(&performance_copybook);

    let validation_output = validation_cmd.output()?;
    let validation_code = validation_output
        .status
        .code()
        .ok_or_else(|| "audit performance validation command exited without code".to_string())?;
    assert!(
        validation_code == 0 || validation_code == 2 || validation_code == 3 || validation_code == 4,
        "unexpected performance validation exit code {validation_code}",
    );

    // Verify performance report generated
    if performance_report.exists() {
        let report_content = fs::read_to_string(&performance_report)?;
        let report: Value = serde_json::from_str(&report_content)?;
        let payload = report
            .get("performance_audit")
            .ok_or_else(|| "missing performance_audit section".to_string())?;
        assert!(payload["throughput_bps"].is_number());
        assert!(payload["iterations"].is_number());
        assert!(payload["baseline_id"].is_string());
        assert!(payload["status_code"].as_str().is_some());
    }
    Ok(())
}

#[test]
fn test_audit_performance_requires_objective_flags() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let performance_copybook = temp_dir.path().join("performance_objective_test.cpy");
    let performance_report = temp_dir.path().join("performance_objective_report.json");

    fs::write(
        &performance_copybook,
        r"
       01 PERFORMANCE-RECORD.
           05 FIELD                PIC X(32).
       ",
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1");
    cmd.arg("audit")
        .arg("performance")
        .arg("--output")
        .arg(&performance_report)
        .arg(&performance_copybook);

    cmd.assert().failure().stderr(
        predicate::str::contains("performance audit requires AC11 objective").or(
            predicate::str::contains("AC11 objective"),
        ),
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
    fs::write(
        &access_log,
        r#"
{"timestamp":"2024-09-25T10:00:00Z","user":"test_user","resource_type":"record","resource":"sensitive_record","action":"read","result":"success"}
{"timestamp":"2024-09-25T10:01:00Z","user":"test_user","resource_type":"record","resource":"sensitive_record","action":"write","result":"denied"}
        "#,
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("security")
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
        .arg("--access-log")
        .arg(&access_log)
        .arg("--detect-anomalies")
        .arg("--validate-encryption")
        .arg("--check-access-patterns")
        .arg("--output")
        .arg(&security_report)
        .arg(&security_copybook);

    let output = cmd.output()?;
    let code = output
        .status
        .code()
        .ok_or_else(|| "audit security command exited without code".to_string())?;
    assert!(
        code == 0 || code == 2 || code == 3 || code == 4,
        "unexpected security exit code {code}",
    );

    assert!(security_report.exists());
    let report_content = fs::read_to_string(&security_report)?;
    let report: Value = serde_json::from_str(&report_content)?;
    let security = report
        .get("security_audit")
        .ok_or_else(|| "missing security_audit section".to_string())?;
    assert!(security["sensitive_fields"].is_array());
    assert!(security["access_event_count"].is_number());
    assert_eq!(security["real_time_monitoring"].as_bool(), Some(false));
    assert!(security["status"].as_str().is_some());
    Ok(())
}

/// Ensures enterprise gate is enforced for security audit flows.
#[test]
fn test_audit_security_requires_enterprise_gate_env() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let security_copybook = temp_dir.path().join("security_gate_test.cpy");
    let security_report = temp_dir.path().join("security_gate_report.json");

    fs::write(
        &security_copybook,
        r#"
           01 SECURITY-GATE-RECORD.
               05 USER-ID            PIC X(20).
               05 ACCOUNT-BALANCE    PIC 9(11)V99 COMP-3.
        "#,
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.env_remove("COPYBOOK_AUDIT_ENTERPRISE_READY");
    cmd.arg("audit")
        .arg("security")
        .arg("--validate-encryption")
        .arg("--output")
        .arg(&security_report)
        .arg(&security_copybook);

    cmd.assert().failure().stderr(
        predicate::str::contains("enterprise hardening gate is blocked")
            .or(predicate::str::contains("Permission denied")),
    );

    Ok(())
}

#[test]
fn test_audit_security_monitoring_status_reports_ac9_enabled() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let security_copybook = temp_dir.path().join("security_monitoring_enabled.cpy");
    let security_report = temp_dir.path().join("security_monitoring_report.json");

    fs::write(
        &security_copybook,
        r#"
           01 MONITORING-RECORD.
               05 USER-ID            PIC X(20).
               05 ACCOUNT-BALANCE    PIC 9(11)V99 COMP-3.
        "#,
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1");
    cmd.arg("audit")
        .arg("security")
        .arg("--real-time-monitoring")
        .arg("--threat-assessment")
        .arg("--output")
        .arg(&security_report)
        .arg(&security_copybook);

    cmd.assert().success();

    let report_content = fs::read_to_string(&security_report)?;
    let report: Value = serde_json::from_str(&report_content)?;
    assert_eq!(
        report["security_audit"]["ac9_monitoring"]["requested"]
            .as_bool(),
        Some(true),
    );
    assert_eq!(
        report["security_audit"]["ac9_monitoring"]["status"]
            .as_str(),
        Some("production-gated-enabled"),
    );

    Ok(())
}

/// Tests feature spec: enterprise-audit-system-spec.md#audit-trail-integrity
/// Test audit health command with integrity validation (AC10, AC7)
#[test]
fn test_audit_health_integrity() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let audit_trail = temp_dir.path().join("audit_trail.jsonl");
    let health_report = temp_dir.path().join("health_report.json");

    // Create mock audit trail
    fs::write(
        &audit_trail,
        r#"
{"event_id":"audit-001","timestamp":"2024-09-25T10:00:00Z","source":"cli","event_type":"copybook_parse","integrity_hash":"a1b2c3d4","previous_hash":null}
{"event_id":"audit-002","timestamp":"2024-09-25T10:01:00Z","source":"cli","event_type":"copybook_parse","integrity_hash":"e5f6g7h8","previous_hash":"a1b2c3d4"}
{"event_id":"audit-003","timestamp":"2024-09-25T10:02:00Z","source":"cli","event_type":"copybook_parse","integrity_hash":"i9j0k1l2","previous_hash":"a1b2c3d4"}
        "#,
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("health")
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
        .arg(&audit_trail)
        .arg("--validate-chain-integrity")
        .arg("--verify-timestamps")
        .arg("--output")
        .arg(&health_report)
        .arg("--detailed-diagnostics");

    let output = cmd.output()?;
    let code = output
        .status
        .code()
        .ok_or_else(|| "audit health command exited without code".to_string())?;
    assert!(
        code == 0 || code == 2 || code == 3 || code == 4,
        "unexpected audit health exit code {code}",
    );

    // Verify health report generated
    assert!(health_report.exists());
    let report_content = fs::read_to_string(&health_report)?;
    let report: Value = serde_json::from_str(&report_content)?;
    let audit_health = report
        .get("audit_health")
        .ok_or_else(|| "missing audit_health section".to_string())?;
    let health_status = audit_health
        .get("health_status")
        .ok_or_else(|| "missing health_status section".to_string())?;
    assert!(health_status["overall"].as_str().is_some());
    assert!(health_status["overall_health_score"].as_u64().is_some());
    assert!(audit_health["chain_verification"]["chain_integrity_valid"].as_bool().is_some());
    assert!(audit_health["hash_verification_results"].is_array());

    Ok(())
}

#[test]
fn test_audit_health_requires_check_selector() -> TestResult<()> {
    let temp_dir = tempdir()?;
    let audit_log = temp_dir.path().join("audit_trail.jsonl");
    let health_report = temp_dir.path().join("health_check_report.json");

    fs::write(
        &audit_log,
        r#"
{"event_id": "audit-001", "timestamp": "2024-09-25T10:00:00Z", "integrity_hash": "a1b2c3d4", "previous_hash": null}
{"event_id": "audit-002", "timestamp": "2024-09-25T10:01:00Z", "integrity_hash": "e5f6g7h8", "previous_hash": "a1b2c3d4"}
{"event_id": "audit-003", "timestamp": "2024-09-25T10:02:00Z", "integrity_hash": "i9j0k1l2", "previous_hash": "e5f6g7h8"}
        "#,
    )?;

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("audit")
        .arg("health")
        .arg("--audit-log")
        .arg(&audit_log)
        .arg("--output")
        .arg(&health_report);

    cmd.assert().failure().stderr(
        predicate::str::contains("audit health checks are required for AC10").or(
            predicate::str::contains("AC10"),
        ),
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
        .env("COPYBOOK_AUDIT_ENTERPRISE_READY", "1")
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

    let output = cmd.output()?;
    let code = output
        .status
        .code()
        .ok_or_else(|| "audit siem command exited without code".to_string())?;
    assert!(
        code == 0 || code == 2 || code == 3 || code == 4,
        "unexpected SIEM audit exit code {code}",
    );

    assert!(security_report.exists());
    let security_content = fs::read_to_string(&security_report)?;
    let security: Value = serde_json::from_str(&security_content)?;
    let report = security
        .get("security_audit")
        .ok_or_else(|| "missing security_audit section".to_string())?;
    assert!(report["siem_exported_path"].is_string());
    assert_eq!(report["real_time_monitoring"].as_bool(), Some(true));
    assert_eq!(
        report["ac9_monitoring"]["status"].as_str(),
        Some("production-gated-enabled"),
    );
    assert!(siem_output.exists());
    let cef_content = fs::read_to_string(&siem_output)?;
    assert!(cef_content.contains("CEF:0|copybook-rs|Audit|1.0"));
    Ok(())
}
