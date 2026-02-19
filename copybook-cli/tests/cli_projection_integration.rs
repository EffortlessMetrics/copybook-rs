// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI integration tests for field projection
//!
//! Tests the --select flag for decode, encode, and verify commands

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod test_utils;

use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;
use tempfile::TempDir;
use test_utils::TestResult;

/// Test decode with simple field selection
#[test]
fn test_cli_decode_with_select_simple_fields() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    // Create a simple copybook
    let copybook_path = temp_dir.path().join("test.cpy");
    let copybook_text = r#"
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID      PIC 9(6).
           05  CUSTOMER-NAME    PIC X(30).
           05  BALANCE          PIC S9(7)V99 COMP-3.
    "#;
    fs::write(&copybook_path, copybook_text)?;

    // Create test data (6 bytes for ID, 30 bytes for name, 5 bytes for COMP-3)
    let data_path = temp_dir.path().join("data.bin");
    let mut data = vec![];
    data.extend_from_slice(b"000123"); // CUSTOMER-ID
    data.extend_from_slice(b"John Doe                      "); // CUSTOMER-NAME (30 bytes)
    data.extend_from_slice(&[0x00, 0x01, 0x23, 0x45, 0x6C]); // BALANCE (COMP-3)
    fs::write(&data_path, data)?;

    // Decode with field selection
    let output_path = temp_dir.path().join("output.jsonl");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("decode")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("CUSTOMER-ID,BALANCE");

    cmd.assert().success();

    // Verify output contains only selected fields
    let output_content = fs::read_to_string(&output_path)?;
    let json: Value = serde_json::from_str(output_content.lines().next().unwrap())?;

    // Should include selected fields in the output map
    let record = json.get("fields").unwrap();
    assert!(record.get("CUSTOMER-ID").is_some());
    assert!(record.get("BALANCE").is_some());
    // CUSTOMER-NAME should not be present
    assert!(record.get("CUSTOMER-NAME").is_none());

    Ok(())
}

/// Test decode with comma-separated field selection
#[test]
fn test_cli_decode_with_select_comma_separated() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    // Create a simple copybook
    let copybook_path = temp_dir.path().join("test.cpy");
    let copybook_text = r#"
       01  RECORD.
           05  FIELD-A      PIC X(5).
           05  FIELD-B      PIC X(5).
           05  FIELD-C      PIC X(5).
    "#;
    fs::write(&copybook_path, copybook_text)?;

    // Create test data
    let data_path = temp_dir.path().join("data.bin");
    fs::write(&data_path, b"AAAAABBBBBCCCCC")?;

    // Decode with comma-separated selection
    let output_path = temp_dir.path().join("output.jsonl");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("decode")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("FIELD-A,FIELD-C");

    cmd.assert().success();

    // Verify output
    let output_content = fs::read_to_string(&output_path)?;
    let json: Value = serde_json::from_str(output_content.lines().next().unwrap())?;

    let record = json.get("fields").unwrap();
    assert!(record.get("FIELD-A").is_some());
    assert!(record.get("FIELD-B").is_none());
    assert!(record.get("FIELD-C").is_some());

    Ok(())
}

/// Test decode with multiple --select flags
#[test]
fn test_cli_decode_with_select_multiple_flags() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    // Create a simple copybook
    let copybook_path = temp_dir.path().join("test.cpy");
    let copybook_text = r#"
       01  RECORD.
           05  FIELD-A      PIC X(5).
           05  FIELD-B      PIC X(5).
    "#;
    fs::write(&copybook_path, copybook_text)?;

    // Create test data
    let data_path = temp_dir.path().join("data.bin");
    fs::write(&data_path, b"AAAAABBBBB")?;

    // Decode with multiple --select flags
    let output_path = temp_dir.path().join("output.jsonl");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("decode")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("FIELD-A")
        .arg("--select")
        .arg("FIELD-B");

    cmd.assert().success();

    // Verify both fields are present
    let output_content = fs::read_to_string(&output_path)?;
    let json: Value = serde_json::from_str(output_content.lines().next().unwrap())?;

    let record = json.get("fields").unwrap();
    assert!(record.get("FIELD-A").is_some());
    assert!(record.get("FIELD-B").is_some());

    Ok(())
}

/// Test decode with invalid field name
#[test]
fn test_cli_decode_with_select_invalid_field() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    // Create a simple copybook
    let copybook_path = temp_dir.path().join("test.cpy");
    let copybook_text = r#"
       01  RECORD.
           05  FIELD-A      PIC X(5).
    "#;
    fs::write(&copybook_path, copybook_text)?;

    // Create test data
    let data_path = temp_dir.path().join("data.bin");
    fs::write(&data_path, b"AAAAA")?;

    // Try to decode with nonexistent field
    let output_path = temp_dir.path().join("output.jsonl");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("decode")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("NONEXISTENT-FIELD");

    // Should fail with error about field not found
    cmd.assert().failure().stderr(predicate::str::contains(
        "CBKS703_PROJECTION_FIELD_NOT_FOUND",
    ));

    Ok(())
}

/// Test encode with field projection
#[test]
fn test_cli_encode_with_projection() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    // Create a simple copybook
    let copybook_path = temp_dir.path().join("test.cpy");
    let copybook_text = r#"
       01  RECORD.
           05  FIELD-A      PIC X(5).
           05  FIELD-B      PIC X(5).
    "#;
    fs::write(&copybook_path, copybook_text)?;

    // Create JSON input (only FIELD-A)
    let input_path = temp_dir.path().join("input.jsonl");
    let json_input = r#"{"RECORD":{"FIELD-A":"HELLO"}}"#;
    fs::write(&input_path, json_input)?;

    // Encode with field selection (only validate FIELD-A)
    let output_path = temp_dir.path().join("output.bin");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("encode")
        .arg(&copybook_path)
        .arg(&input_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("FIELD-A");

    // Should succeed (only validating FIELD-A)
    cmd.assert().success();

    Ok(())
}

/// Test verify with field projection
#[test]
fn test_cli_verify_with_projection() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    // Create a simple copybook
    let copybook_path = temp_dir.path().join("test.cpy");
    let copybook_text = r#"
       01  RECORD.
           05  FIELD-A      PIC 9(5).
           05  FIELD-B      PIC X(5).
    "#;
    fs::write(&copybook_path, copybook_text)?;

    // Create test data with invalid numeric field (FIELD-A has non-digits)
    let data_path = temp_dir.path().join("data.bin");
    fs::write(&data_path, b"AAAAABBBBB")?;

    // Verify with projection (only check FIELD-B)
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("verify")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("FIELD-B");

    // Should pass since we're only validating FIELD-B
    cmd.assert().success();

    Ok(())
}

/// Test decode with group selection includes all children
#[test]
fn test_cli_decode_group_selection_includes_children() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    // Create a copybook with nested groups
    let copybook_path = temp_dir.path().join("test.cpy");
    let copybook_text = r#"
       01  RECORD.
           05  GROUP-A.
               10  FIELD-A1     PIC X(5).
               10  FIELD-A2     PIC X(5).
           05  GROUP-B.
               10  FIELD-B1     PIC X(5).
    "#;
    fs::write(&copybook_path, copybook_text)?;

    // Create test data
    let data_path = temp_dir.path().join("data.bin");
    fs::write(&data_path, b"AAAA1AAAA2BBBB1")?;

    // Decode with group selection
    let output_path = temp_dir.path().join("output.jsonl");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("decode")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("GROUP-A");

    cmd.assert().success();

    // Verify output includes all children of GROUP-A
    let output_content = fs::read_to_string(&output_path)?;
    let json: Value = serde_json::from_str(output_content.lines().next().unwrap())?;

    let record = json.get("fields").unwrap();
    assert!(record.get("FIELD-A1").is_some());
    assert!(record.get("FIELD-A2").is_some());
    // GROUP-B fields should not be present
    assert!(record.get("FIELD-B1").is_none());

    Ok(())
}
