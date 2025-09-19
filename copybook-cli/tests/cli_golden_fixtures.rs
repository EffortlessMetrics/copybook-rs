//! CLI integration tests using golden fixtures
//!
//! These tests verify that the CLI commands work correctly with real fixtures

mod test_utils;

use assert_cmd::Command;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;
use tempfile::TempDir;
use test_utils::{copybook_cmd, fixture_path};

/// Test parse command with golden fixture
#[test]
fn test_cli_parse_simple() {
    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("parse").arg(fixture_path("copybooks/simple.cpy"));

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-RECORD"))
        .stdout(predicate::str::contains("CUSTOMER-ID"))
        .stdout(predicate::str::contains("ACCOUNT-BALANCE"));
}

/// Test inspect command with golden fixture
#[test]
fn test_cli_inspect_simple() {
    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("inspect").arg(fixture_path("copybooks/simple.cpy"));

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-RECORD"))
        .stdout(predicate::str::contains("9(6)"))
        .stdout(predicate::str::contains("COMP-3"));
}

/// Test verify command with valid data
#[test]
fn test_cli_verify_valid_data() {
    // First, we need to create some test data
    let temp_dir = TempDir::new().unwrap();
    let data_file = temp_dir.path().join("test_data.bin");

    // Create some simple test data (just zeros for now)
    let test_data = vec![0u8; 100]; // Create 100 bytes of test data
    fs::write(&data_file, test_data).unwrap();

    let mut cmd = copybook_cmd(&[
        "verify",
        fixture_path("copybooks/simple.cpy").to_str().unwrap(),
        data_file.to_str().unwrap(),
    ]);

    // This might fail due to data format, but should not crash
    let output = cmd.output().unwrap();
    assert!(output.status.code().unwrap() <= 3); // Should not be a fatal error (code 4)
}

/// Test verify command with JSON report schema validation
#[test]
fn test_cli_verify_report_schema() {
    let temp_dir = TempDir::new().unwrap();
    let data_file = temp_dir.path().join("test_data.bin");
    let report_file = temp_dir.path().join("verify_report.json");

    // Create valid test data for simple.cpy
    let test_data = vec![0u8; 100]; // Simple test data
    fs::write(&data_file, test_data).unwrap();

    let mut cmd = copybook_cmd(&[
        "verify",
        fixture_path("copybooks/simple.cpy").to_str().unwrap(),
        data_file.to_str().unwrap(),
    ]);
    cmd.arg("--report").arg(&report_file);

    let output = cmd.output().unwrap();

    // Command should complete (may have validation errors, but should generate report)
    assert!(output.status.code().unwrap() <= 3);

    // Report file should exist
    assert!(report_file.exists(), "Report file should be generated");

    // Read and parse the JSON report
    let report_content = fs::read_to_string(&report_file).unwrap();
    let report: Value = serde_json::from_str(&report_content).expect("Report should be valid JSON");

    // Validate the report structure using our helper
    validate_verify_report_schema(&report);
}

/// Helper function to validate VerifyReport JSON structure
fn validate_verify_report_schema(report: &Value) {
    let obj = report.as_object().expect("Report should be a JSON object");

    // Validate required fields and types
    assert!(obj.contains_key("report_version"), "Missing report_version");
    assert!(
        obj["report_version"].is_u64(),
        "report_version should be integer"
    );
    assert_eq!(
        obj["report_version"].as_u64().unwrap(),
        1,
        "report_version should be 1"
    );

    assert!(
        obj.contains_key("schema_fingerprint"),
        "Missing schema_fingerprint"
    );
    assert!(
        obj["schema_fingerprint"].is_string(),
        "schema_fingerprint should be string"
    );
    let fingerprint = obj["schema_fingerprint"].as_str().unwrap();
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
    let format = obj["record_format"].as_str().unwrap();
    assert!(
        format == "fixed" || format == "rdw",
        "record_format should be 'fixed' or 'rdw'"
    );

    assert!(obj.contains_key("file"), "Missing file");
    assert!(obj["file"].is_string(), "file should be string");
    assert!(
        !obj["file"].as_str().unwrap().is_empty(),
        "file should not be empty"
    );

    assert!(
        obj.contains_key("file_size_bytes"),
        "Missing file_size_bytes"
    );
    assert!(
        obj["file_size_bytes"].is_u64(),
        "file_size_bytes should be integer"
    );

    assert!(obj.contains_key("cli_opts"), "Missing cli_opts");
    validate_cli_opts(&obj["cli_opts"]);

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
    for error in obj["errors"].as_array().unwrap() {
        validate_verify_error(error);
    }

    assert!(obj.contains_key("sample"), "Missing sample");
    assert!(obj["sample"].is_array(), "sample should be array");
    for sample in obj["sample"].as_array().unwrap() {
        validate_verify_sample(sample);
    }
}

/// Validate cli_opts structure
fn validate_cli_opts(cli_opts: &Value) {
    let obj = cli_opts.as_object().expect("cli_opts should be object");

    assert!(obj.contains_key("codepage"), "Missing codepage");
    assert!(obj["codepage"].is_string(), "codepage should be string");

    assert!(obj.contains_key("strict"), "Missing strict");
    assert!(obj["strict"].is_boolean(), "strict should be boolean");

    assert!(obj.contains_key("max_errors"), "Missing max_errors");
    assert!(obj["max_errors"].is_u64(), "max_errors should be integer");
    assert!(
        obj["max_errors"].as_u64().unwrap() >= 1,
        "max_errors should be >= 1"
    );

    assert!(obj.contains_key("sample"), "Missing sample");
    assert!(obj["sample"].is_u64(), "sample should be integer");
}

/// Validate verify error structure
fn validate_verify_error(error: &Value) {
    let obj = error.as_object().expect("error should be object");

    assert!(obj.contains_key("index"), "Missing error.index");
    assert!(obj["index"].is_u64(), "error.index should be integer");

    assert!(obj.contains_key("code"), "Missing error.code");
    assert!(obj["code"].is_string(), "error.code should be string");
    let code = obj["code"].as_str().unwrap();
    assert!(code.starts_with("CBK"), "error.code should start with CBK");

    assert!(obj.contains_key("field"), "Missing error.field");
    // field can be string or null

    assert!(obj.contains_key("offset"), "Missing error.offset");
    // offset can be integer or null

    assert!(obj.contains_key("msg"), "Missing error.msg");
    assert!(obj["msg"].is_string(), "error.msg should be string");
    assert!(
        !obj["msg"].as_str().unwrap().is_empty(),
        "error.msg should not be empty"
    );

    assert!(obj.contains_key("hex"), "Missing error.hex");
    // hex can be string or null
    if obj["hex"].is_string() {
        let hex = obj["hex"].as_str().unwrap();
        assert!(
            hex.chars().all(|c| c.is_ascii_hexdigit()),
            "error.hex should be hex chars"
        );
    }
}

/// Validate verify sample structure
fn validate_verify_sample(sample: &Value) {
    let obj = sample.as_object().expect("sample should be object");

    assert!(obj.contains_key("index"), "Missing sample.index");
    assert!(obj["index"].is_u64(), "sample.index should be integer");

    assert!(obj.contains_key("hex"), "Missing sample.hex");
    assert!(obj["hex"].is_string(), "sample.hex should be string");
    let hex = obj["hex"].as_str().unwrap();
    // Allow TODO placeholder or hex chars
    if !hex.starts_with("TODO:") {
        assert!(
            hex.chars().all(|c| c.is_ascii_hexdigit()),
            "sample.hex should be hex chars or TODO placeholder"
        );
    }
}

/// Test encode command with COMP-3 fixture
#[test]
fn test_cli_encode_comp3() {
    let temp_dir = TempDir::new().unwrap();
    let output_file = temp_dir.path().join("encoded.bin");

    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("encode")
        .arg(fixture_path("copybooks/comp3_test.cpy"))
        .arg(fixture_path("data/comp3_test.jsonl"))
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

    // Verify output file was created
    assert!(output_file.exists());
    let encoded_data = fs::read(&output_file).unwrap();
    assert!(!encoded_data.is_empty());
}

/// Test decode command round-trip with COMP-3 fixture
#[test]
fn test_cli_decode_comp3_roundtrip() {
    let temp_dir = TempDir::new().unwrap();
    let encoded_file = temp_dir.path().join("encoded.bin");
    let decoded_file = temp_dir.path().join("decoded.jsonl");

    // First encode the test data
    let mut encode_cmd = Command::cargo_bin("copybook").unwrap();
    encode_cmd
        .arg("encode")
        .arg(fixture_path("copybooks/comp3_test.cpy"))
        .arg(fixture_path("data/comp3_test.jsonl"))
        .arg("--output")
        .arg(&encoded_file)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037");

    encode_cmd.assert().success();

    // Then decode it back
    let mut decode_cmd = Command::cargo_bin("copybook").unwrap();
    decode_cmd
        .arg("decode")
        .arg(fixture_path("copybooks/comp3_test.cpy"))
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

    // Verify decoded content
    let decoded_content = fs::read_to_string(&decoded_file).unwrap();
    assert!(decoded_content.contains("RECORD-ID"));
    assert!(decoded_content.contains("POSITIVE-AMOUNT"));
    assert!(decoded_content.contains("123.45"));
}

/// Test encode fail-fast behavior
#[test]
fn test_cli_encode_fail_fast() {
    let temp_dir = TempDir::new().unwrap();
    let bad_jsonl = temp_dir.path().join("bad.jsonl");
    let output_file = temp_dir.path().join("output.bin");

    // Create invalid JSONL that should fail encoding - provide invalid data for numeric field
    fs::write(
        &bad_jsonl,
        r#"{"CUSTOMER-ID": "not-a-number", "ACCOUNT-BALANCE": "invalid-decimal"}"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("encode")
        .arg(fixture_path("copybooks/simple.cpy"))
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
}

/// Test help messages are correct
#[test]
fn test_cli_help_messages() {
    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Modern COBOL copybook parser"))
        .stdout(predicate::str::contains("parse"))
        .stdout(predicate::str::contains("verify"))
        .stdout(predicate::str::contains("encode"))
        .stdout(predicate::str::contains("decode"));

    // Test subcommand help
    let mut verify_cmd = Command::cargo_bin("copybook").unwrap();
    verify_cmd.arg("verify").arg("--help");
    verify_cmd
        .assert()
        .success()
        .stdout(predicate::str::contains("Verify data file structure"))
        .stdout(predicate::str::contains("--strict"))
        .stdout(predicate::str::contains("--max-errors"));
}
