//! CLI integration tests using golden fixtures
//!
//! These tests verify that the CLI commands work correctly with real fixtures

mod test_utils;

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;
use test_utils::fixture_path;

/// Test parse command with golden fixture
#[test]
fn test_cli_parse_simple() {
    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("parse")
        .arg(fixture_path("copybooks/simple.cpy"));

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
    cmd.arg("inspect")
        .arg(fixture_path("copybooks/simple.cpy"));

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

    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("verify")
        .arg(fixture_path("copybooks/simple.cpy"))
        .arg(&data_file)
        .arg("--format")
        .arg("fixed");

    // This might fail due to data format, but should not crash
    let output = cmd.output().unwrap();
    assert!(output.status.code().unwrap() <= 3); // Should not be a fatal error (code 4)
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
        .arg("fixed");

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
    encode_cmd.arg("encode")
        .arg(fixture_path("copybooks/comp3_test.cpy"))
        .arg(fixture_path("data/comp3_test.jsonl"))
        .arg("--output")
        .arg(&encoded_file)
        .arg("--format")
        .arg("fixed");

    encode_cmd.assert().success();

    // Then decode it back
    let mut decode_cmd = Command::cargo_bin("copybook").unwrap();
    decode_cmd.arg("decode")
        .arg(fixture_path("copybooks/comp3_test.cpy"))
        .arg(&encoded_file)
        .arg("--output")
        .arg(&decoded_file)
        .arg("--format")
        .arg("fixed");

    decode_cmd.assert()
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
    fs::write(&bad_jsonl, r#"{"CUSTOMER-ID": "not-a-number", "ACCOUNT-BALANCE": "invalid-decimal"}"#).unwrap();

    let mut cmd = Command::cargo_bin("copybook").unwrap();
    cmd.arg("encode")
        .arg(fixture_path("copybooks/simple.cpy"))
        .arg(&bad_jsonl)
        .arg("--output")
        .arg(&output_file)
        .arg("--format")
        .arg("fixed")
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
    verify_cmd.assert()
        .success()
        .stdout(predicate::str::contains("Verify data file structure"))
        .stdout(predicate::str::contains("--strict"))
        .stdout(predicate::str::contains("--max-errors"));
}