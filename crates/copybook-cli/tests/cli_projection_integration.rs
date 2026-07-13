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

fn encode_rdw_record(payload: &[u8]) -> Vec<u8> {
    let rdw_len = u16::try_from(payload.len()).expect("payload length must fit in u16");
    let mut record = Vec::with_capacity(payload.len() + 4);
    record.extend_from_slice(&rdw_len.to_be_bytes());
    record.extend_from_slice(&[0x00, 0x00]);
    record.extend_from_slice(payload);
    record
}

fn parse_first_projection(json_lines: &str) -> TestResult<Value> {
    let line = json_lines.lines().next().ok_or("decode output was empty")?;
    Ok(serde_json::from_str(line)?)
}

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
    let stderr = cmd.assert().failure().get_output().stderr.clone();
    let stderr = String::from_utf8_lossy(&stderr);
    let has_exact_code = stderr
        .split(&[
            ' ', '\n', '\r', '\t', '[', ']', '(', ')', '{', '}', ':', ',', '.', ';',
        ])
        .any(|token| token == "CBKS703_PROJECTION_FIELD_NOT_FOUND");
    assert!(
        has_exact_code,
        "expected exact CBKS703_PROJECTION_FIELD_NOT_FOUND token, got: {stderr}"
    );

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

    let fields = json.get("fields").unwrap();
    // With hierarchical nesting, children live under their group object
    let group_a = fields.get("GROUP-A").unwrap();
    assert!(group_a.get("FIELD-A1").is_some());
    assert!(group_a.get("FIELD-A2").is_some());
    // GROUP-B fields should not be present
    assert!(fields.get("GROUP-B").is_none());

    Ok(())
}

#[test]
fn test_cli_decode_with_rdw_single_select_field() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let copybook_path = temp_dir.path().join("test.cpy");
    fs::write(
        &copybook_path,
        r#"
           01  RECORD.
               05  FIELD-A PIC X(3).
               05  FIELD-B PIC X(3).
        "#,
    )?;

    let input = encode_rdw_record(b"AAABBB");
    let data_path = temp_dir.path().join("data.bin");
    fs::write(&data_path, input)?;

    let output_path = temp_dir.path().join("output.jsonl");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("decode")
        .arg(&copybook_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("FIELD-B");

    cmd.assert().success();

    let json = parse_first_projection(&fs::read_to_string(output_path)?)?;
    let record = json.get("fields").ok_or("missing fields object")?;
    assert!(record.get("FIELD-A").is_none());
    assert_eq!(
        record.get("FIELD-B"),
        Some(&Value::String("BBB".to_string()))
    );

    Ok(())
}

#[test]
fn test_cli_encode_with_projection_rdw_roundtrip_selected_fields() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let copybook_path = temp_dir.path().join("test.cpy");
    fs::write(
        &copybook_path,
        r#"
           01  CUSTOMER-RECORD.
               05  CUSTOMER-ID      PIC X(3).
               05  CUSTOMER-NAME    PIC X(2).
        "#,
    )?;

    let input_path = temp_dir.path().join("input.jsonl");
    fs::write(&input_path, r#"{"CUSTOMER-ID":"123","CUSTOMER-NAME":"AB"}"#)?;

    let encoded_path = temp_dir.path().join("encoded.bin");
    let mut encode = cargo_bin_cmd!("copybook");
    encode
        .arg("encode")
        .arg(&copybook_path)
        .arg(&input_path)
        .arg("--output")
        .arg(&encoded_path)
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("CUSTOMER-ID");

    encode.assert().success();

    let encoded = fs::read(&encoded_path)?;
    assert!(
        encoded.len() >= 4,
        "encoded RDW record must include RDW header"
    );
    assert_eq!(&encoded[2..4], &[0x00, 0x00]);
    let declared_len = usize::from(u16::from_be_bytes([encoded[0], encoded[1]]));
    assert_eq!(declared_len, encoded.len() - 4);

    let output_path = temp_dir.path().join("output.jsonl");
    let mut decode = cargo_bin_cmd!("copybook");
    decode
        .arg("decode")
        .arg(&copybook_path)
        .arg(&encoded_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("CUSTOMER-ID");

    decode.assert().success();

    let json = parse_first_projection(&fs::read_to_string(output_path)?)?;
    let fields = json.get("fields").ok_or("missing fields object")?;
    assert_eq!(
        fields.get("CUSTOMER-ID"),
        Some(&Value::String("123".to_string()))
    );
    assert!(fields.get("CUSTOMER-NAME").is_none());

    Ok(())
}

#[test]
fn test_cli_encode_with_flat_projection_input_shape() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let copybook_path = temp_dir.path().join("test.cpy");
    fs::write(
        &copybook_path,
        r#"
           01  RECORD.
               05 FIELD-A PIC X(3).
               05 FIELD-B PIC X(3).
        "#,
    )?;

    let input_path = temp_dir.path().join("input.jsonl");
    fs::write(&input_path, r#"{"FIELD-A":"ABC","FIELD-B":"DEF"}"#)?;

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

    cmd.assert().success();
    let output = fs::read(&output_path)?;
    assert_eq!(output.len(), 6);

    let decoded_path = temp_dir.path().join("decoded.jsonl");
    let mut decode_cmd = cargo_bin_cmd!("copybook");
    decode_cmd
        .arg("decode")
        .arg(&copybook_path)
        .arg(&output_path)
        .arg("--output")
        .arg(&decoded_path)
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("FIELD-A");

    decode_cmd.assert().success();

    let decoded = parse_first_projection(&fs::read_to_string(decoded_path)?)?;
    let fields = decoded.get("fields").ok_or("missing fields object")?;
    assert_eq!(
        fields.get("FIELD-A"),
        Some(&Value::String("ABC".to_string()))
    );
    assert!(
        fields.get("FIELD-B").is_none()
            || fields.get("FIELD-B") == Some(&Value::String("DEF".to_string()))
            || fields.get("FIELD-B") == Some(&Value::String("".to_string())),
        "unexpected projection-fill output for unselected FIELD-B: {:?}",
        fields.get("FIELD-B")
    );

    Ok(())
}

#[test]
fn test_cli_decode_with_rdw_zero_reserved_bytes() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let copybook_path = temp_dir.path().join("test.cpy");
    fs::write(
        &copybook_path,
        r#"
           01  RECORD.
               05  FIELD PIC X(3).
        "#,
    )?;

    let output = encode_rdw_record(b"ABC");
    let input_path = temp_dir.path().join("data.bin");
    fs::write(&input_path, &output)?;
    assert_eq!(&output[2..4], &[0x00, 0x00]);

    let output_path = temp_dir.path().join("decoded.jsonl");
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.arg("decode")
        .arg(&copybook_path)
        .arg(&input_path)
        .arg("--output")
        .arg(&output_path)
        .arg("--format")
        .arg("rdw")
        .arg("--codepage")
        .arg("ascii")
        .arg("--select")
        .arg("FIELD");

    cmd.assert().success();
    let lines = fs::read_to_string(output_path)?;
    let json: Value = serde_json::from_str(lines.lines().next().ok_or("decode output was empty")?)?;
    let fields = json.get("fields").ok_or("missing fields object")?;
    assert_eq!(fields.get("FIELD"), Some(&Value::String("ABC".to_string())));

    Ok(())
}

#[test]
fn test_cli_projection_unknown_field_returns_cbks703() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let copybook_path = temp_dir.path().join("test.cpy");
    fs::write(
        &copybook_path,
        r#"
           01  RECORD.
              05 FIELD PIC X(3).
        "#,
    )?;

    let input_path = temp_dir.path().join("input.jsonl");
    fs::write(&input_path, r#"{"RECORD":{"FIELD":"ABC"}}"#)?;
    let output_path = temp_dir.path().join("output.jsonl");

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
        .arg("DOES_NOT_EXIST");

    cmd.assert().failure().stderr(predicate::str::contains(
        "CBKS703_PROJECTION_FIELD_NOT_FOUND",
    ));

    Ok(())
}
