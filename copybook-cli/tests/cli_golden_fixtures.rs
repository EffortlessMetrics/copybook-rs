//! CLI integration tests using golden fixtures
//!
//! These tests verify that the CLI commands work correctly with real fixtures

mod test_utils;

use assert_cmd::Command;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;
use tempfile::TempDir;
use test_utils::{TestResult, copybook_cmd, fixture_path, path_to_str, require_some};

/// Test parse command with golden fixture
#[test]
fn test_cli_parse_simple() -> TestResult<()> {
    let mut cmd = Command::cargo_bin("copybook")?;
    let copybook = fixture_path("copybooks/simple.cpy")?;
    cmd.arg("parse").arg(&copybook);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-RECORD"))
        .stdout(predicate::str::contains("CUSTOMER-ID"))
        .stdout(predicate::str::contains("ACCOUNT-BALANCE"));

    Ok(())
}

/// Test inspect command with golden fixture
#[test]
fn test_cli_inspect_simple() -> TestResult<()> {
    let mut cmd = Command::cargo_bin("copybook")?;
    let copybook = fixture_path("copybooks/simple.cpy")?;
    cmd.arg("inspect").arg(&copybook);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-RECORD"))
        .stdout(predicate::str::contains("9(6)"))
        .stdout(predicate::str::contains("COMP-3"));

    Ok(())
}

/// Test verify command with valid data
#[test]
fn test_cli_verify_valid_data() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let data_file = temp_dir.path().join("test_data.bin");

    fs::write(&data_file, vec![0u8; 100])?; // Create 100 bytes of test data

    let copybook = fixture_path("copybooks/simple.cpy")?;
    let copybook_str = path_to_str(&copybook)?;
    let data_str = path_to_str(&data_file)?;
    let mut cmd = copybook_cmd(&["verify", copybook_str, data_str])?;

    let output = cmd.output()?;
    let exit_code = require_some(
        output.status.code(),
        "verify command terminated without exit code",
    )?;
    assert!(exit_code <= 3, "Should not be a fatal error (code 4)");

    Ok(())
}

/// Test verify command with JSON report schema validation
#[test]
fn test_cli_verify_report_schema() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let data_file = temp_dir.path().join("test_data.bin");
    let report_file = temp_dir.path().join("verify_report.json");

    fs::write(&data_file, vec![0u8; 100])?;

    let copybook = fixture_path("copybooks/simple.cpy")?;
    let copybook_str = path_to_str(&copybook)?;
    let data_str = path_to_str(&data_file)?;
    let mut cmd = copybook_cmd(&["verify", copybook_str, data_str])?;
    cmd.arg("--report").arg(&report_file);

    let output = cmd.output()?;
    let exit_code = require_some(
        output.status.code(),
        "verify command terminated without exit code",
    )?;
    assert!(exit_code <= 3);

    assert!(report_file.exists(), "Report file should be generated");

    let report_content = fs::read_to_string(&report_file)?;
    let report: Value = serde_json::from_str(&report_content)?;
    validate_verify_report_schema(&report)?;

    Ok(())
}

/// Helper function to validate `VerifyReport` JSON structure
fn validate_verify_report_schema(report: &Value) -> TestResult<()> {
    let obj = require_some(report.as_object(), "Report should be a JSON object")?;

    assert!(obj.contains_key("report_version"), "Missing report_version");
    assert!(
        obj["report_version"].is_u64(),
        "report_version should be integer"
    );
    let report_version = require_some(
        obj["report_version"].as_u64(),
        "report_version should be integer",
    )?;
    assert_eq!(report_version, 1, "report_version should be 1");

    assert!(
        obj.contains_key("schema_fingerprint"),
        "Missing schema_fingerprint"
    );
    assert!(
        obj["schema_fingerprint"].is_string(),
        "schema_fingerprint should be string"
    );
    let fingerprint = require_some(
        obj["schema_fingerprint"].as_str(),
        "schema_fingerprint should be string",
    )?;
    assert_eq!(
        fingerprint.len(),
        32,
        "schema_fingerprint should be 32 hex chars"
    );
    assert!(
        fingerprint.chars().all(|c| c.is_ascii_hexdigit()),
        "schema_fingerprint should be hex"
    );

    assert!(obj.contains_key("record_format"), "Missing record_format");
    assert!(
        obj["record_format"].is_string(),
        "record_format should be string"
    );
    let format = require_some(
        obj["record_format"].as_str(),
        "record_format should be string",
    )?;
    assert!(
        matches!(format, "fixed" | "rdw"),
        "record_format should be 'fixed' or 'rdw'"
    );

    assert!(obj.contains_key("file"), "Missing file");
    assert!(obj["file"].is_string(), "file should be string");
    let file_name = require_some(obj["file"].as_str(), "file should be string")?;
    assert!(!file_name.is_empty(), "file should not be empty");

    assert!(
        obj.contains_key("file_size_bytes"),
        "Missing file_size_bytes"
    );
    assert!(
        obj["file_size_bytes"].is_u64(),
        "file_size_bytes should be integer"
    );

    assert!(obj.contains_key("cli_opts"), "Missing cli_opts");
    validate_cli_opts(&obj["cli_opts"])?;

    assert!(obj.contains_key("records_total"), "Missing records_total");
    assert!(
        obj["records_total"].is_u64(),
        "records_total should be integer"
    );

    assert!(obj.contains_key("errors_total"), "Missing errors_total");
    assert!(
        obj["errors_total"].is_u64(),
        "errors_total should be integer"
    );

    assert!(obj.contains_key("truncated"), "Missing truncated");
    assert!(obj["truncated"].is_boolean(), "truncated should be boolean");

    assert!(obj.contains_key("errors"), "Missing errors");
    assert!(obj["errors"].is_array(), "errors should be array");
    for error in require_some(obj["errors"].as_array(), "errors should be array")? {
        validate_verify_error(error)?;
    }

    assert!(obj.contains_key("sample"), "Missing sample");
    assert!(obj["sample"].is_array(), "sample should be array");
    for sample in require_some(obj["sample"].as_array(), "sample should be array")? {
        validate_verify_sample(sample)?;
    }

    Ok(())
}

/// Validate `cli_opts` structure
fn validate_cli_opts(cli_opts: &Value) -> TestResult<()> {
    let obj = require_some(cli_opts.as_object(), "cli_opts should be object")?;

    assert!(obj.contains_key("codepage"), "Missing codepage");
    assert!(obj["codepage"].is_string(), "codepage should be string");

    assert!(obj.contains_key("strict"), "Missing strict");
    assert!(obj["strict"].is_boolean(), "strict should be boolean");

    assert!(obj.contains_key("max_errors"), "Missing max_errors");
    assert!(obj["max_errors"].is_u64(), "max_errors should be integer");
    let max_errors = require_some(obj["max_errors"].as_u64(), "max_errors should be integer")?;
    assert!(max_errors >= 1, "max_errors should be >= 1");

    assert!(obj.contains_key("sample"), "Missing sample");
    assert!(obj["sample"].is_u64(), "sample should be integer");

    assert!(
        obj.contains_key("strict_comments"),
        "Missing strict_comments"
    );
    assert!(
        obj["strict_comments"].is_boolean(),
        "strict_comments should be boolean"
    );

    Ok(())
}

/// Validate verify error structure
fn validate_verify_error(error: &Value) -> TestResult<()> {
    let obj = require_some(error.as_object(), "error should be object")?;

    assert!(obj.contains_key("index"), "Missing error.index");
    assert!(obj["index"].is_u64(), "error.index should be integer");

    assert!(obj.contains_key("code"), "Missing error.code");
    assert!(obj["code"].is_string(), "error.code should be string");
    let code = require_some(obj["code"].as_str(), "error.code should be string")?;
    assert!(code.starts_with("CBK"), "error.code should start with CBK");

    assert!(obj.contains_key("field"), "Missing error.field");
    assert!(obj.contains_key("offset"), "Missing error.offset");

    assert!(obj.contains_key("msg"), "Missing error.msg");
    assert!(obj["msg"].is_string(), "error.msg should be string");
    let message = require_some(obj["msg"].as_str(), "error.msg should be string")?;
    assert!(!message.is_empty(), "error.msg should not be empty");

    assert!(obj.contains_key("hex"), "Missing error.hex");
    if obj["hex"].is_string() {
        let hex = require_some(obj["hex"].as_str(), "error.hex should be string")?;
        assert!(
            hex.chars().all(|c| c.is_ascii_hexdigit()),
            "error.hex should be hex chars"
        );
    }

    Ok(())
}

/// Validate verify sample structure
fn validate_verify_sample(sample: &Value) -> TestResult<()> {
    let obj = require_some(sample.as_object(), "sample should be object")?;

    assert!(obj.contains_key("index"), "Missing sample.index");
    assert!(obj["index"].is_u64(), "sample.index should be integer");

    assert!(obj.contains_key("hex"), "Missing sample.hex");
    assert!(obj["hex"].is_string(), "sample.hex should be string");
    let hex = require_some(obj["hex"].as_str(), "sample.hex should be string")?;
    if !hex.starts_with("TODO:") {
        assert!(
            hex.chars().all(|c| c.is_ascii_hexdigit()),
            "sample.hex should be hex chars or TODO placeholder"
        );
    }

    Ok(())
}

/// Test encode command with COMP-3 fixture
#[test]
fn test_cli_encode_comp3() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let output_file = temp_dir.path().join("encoded.bin");

    let mut cmd = Command::cargo_bin("copybook")?;
    let copybook = fixture_path("copybooks/comp3_test.cpy")?;
    let data = fixture_path("data/comp3_test.jsonl")?;
    cmd.arg("encode")
        .arg(&copybook)
        .arg(&data)
        .arg("--output")
        .arg(&output_file)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Encode Summary"))
        .stdout(predicate::str::contains("Records processed: 2"));

    assert!(output_file.exists());
    let encoded_data = fs::read(&output_file)?;
    assert!(!encoded_data.is_empty());

    Ok(())
}

/// Test decode command round-trip with COMP-3 fixture
#[test]
fn test_cli_decode_comp3_roundtrip() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let encoded_file = temp_dir.path().join("encoded.bin");
    let decoded_file = temp_dir.path().join("decoded.jsonl");

    let copybook = fixture_path("copybooks/comp3_test.cpy")?;
    let data = fixture_path("data/comp3_test.jsonl")?;

    let mut encode_cmd = Command::cargo_bin("copybook")?;
    encode_cmd
        .arg("encode")
        .arg(&copybook)
        .arg(&data)
        .arg("--output")
        .arg(&encoded_file)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");
    encode_cmd.assert().success();

    let mut decode_cmd = Command::cargo_bin("copybook")?;
    decode_cmd
        .arg("decode")
        .arg(&copybook)
        .arg(&encoded_file)
        .arg("--output")
        .arg(&decoded_file)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");

    decode_cmd
        .assert()
        .success()
        .stdout(predicate::str::contains("Records processed: 2"));

    let decoded_content = fs::read_to_string(&decoded_file)?;
    assert!(decoded_content.contains("RECORD-ID"));
    assert!(decoded_content.contains("POSITIVE-AMOUNT"));
    assert!(decoded_content.contains("123.45"));

    Ok(())
}

/// Test encode fail-fast behavior
#[test]
fn test_cli_encode_fail_fast() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let bad_jsonl = temp_dir.path().join("bad.jsonl");
    let output_file = temp_dir.path().join("output.bin");

    // Create invalid JSONL that should fail encoding - provide invalid data for numeric field
    fs::write(
        &bad_jsonl,
        r#"{"CUSTOMER-ID": "not-a-number", "ACCOUNT-BALANCE": "invalid-decimal"}"#,
    )?;

    let mut cmd = Command::cargo_bin("copybook")?;
    let copybook = fixture_path("copybooks/simple.cpy")?;
    cmd.arg("encode")
        .arg(&copybook)
        .arg(&bad_jsonl)
        .arg("--output")
        .arg(&output_file)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--fail-fast");

    // Should fail with detailed error message
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Fatal error"));

    Ok(())
}

/// Test help messages are correct
#[test]
fn test_cli_help_messages() -> TestResult<()> {
    let mut cmd = Command::cargo_bin("copybook")?;
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Modern COBOL copybook parser"))
        .stdout(predicate::str::contains("parse"))
        .stdout(predicate::str::contains("verify"))
        .stdout(predicate::str::contains("encode"))
        .stdout(predicate::str::contains("decode"));

    // Test subcommand help
    let mut verify_cmd = Command::cargo_bin("copybook")?;
    verify_cmd.arg("verify").arg("--help");
    verify_cmd
        .assert()
        .success()
        .stdout(predicate::str::contains("Verify data file structure"))
        .stdout(predicate::str::contains("--strict"))
        .stdout(predicate::str::contains("--max-errors"));

    Ok(())
}

/// Test that inline comments work by default (should succeed)
#[test]
fn test_cli_strict_comments_allowed_by_default() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let copybook_file = temp_dir.path().join("test_inline.cpy");

    // Create a copybook with inline comments
    fs::write(
        &copybook_file,
        r"01 CUSTOMER-ID PIC X(10). *> This should be allowed by default
        ",
    )?;

    let mut cmd = Command::cargo_bin("copybook")?;
    cmd.arg("inspect").arg(&copybook_file);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-ID"));

    Ok(())
}

/// Test that inline comments are rejected with --strict-comments flag
#[test]
fn test_cli_strict_comments_flag_rejects_inline_comments() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let copybook_file = temp_dir.path().join("test_inline.cpy");

    // Create a copybook with inline comments
    fs::write(
        &copybook_file,
        r"01 CUSTOMER-ID PIC X(10). *> This should be rejected
        ",
    )?;

    let mut cmd = Command::cargo_bin("copybook")?;
    cmd.arg("inspect")
        .arg(&copybook_file)
        .arg("--strict-comments");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("CBKP001_SYNTAX"))
        .stderr(predicate::str::contains(
            "Inline comments (*>) are not allowed in strict mode",
        ));

    Ok(())
}

/// Test that stdin ("-") works with inline comments by default
#[test]
fn test_cli_strict_comments_stdin_path() -> TestResult<()> {
    let mut cmd = Command::cargo_bin("copybook")?;
    cmd.arg("inspect")
        .arg("-") // stdin
        .write_stdin("01 A PIC X(5). *> inline\n");
    cmd.assert().success().stdout(predicate::str::contains("A"));

    Ok(())
}

/// Test that stdin ("-") rejects inline comments with --strict-comments
#[test]
fn test_cli_strict_comments_stdin_rejected() -> TestResult<()> {
    let mut cmd = Command::cargo_bin("copybook")?;
    cmd.arg("inspect")
        .arg("-")
        .arg("--strict-comments")
        .write_stdin("01 A PIC X(5). *> inline\n");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("CBKP001_SYNTAX"));

    Ok(())
}
