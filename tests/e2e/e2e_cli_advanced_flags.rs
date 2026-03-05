// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for advanced CLI flags: `--float-format`, `--json-number native`,
//! `--bwz-encode`, `--use-raw`, `--emit-raw field`, and `--zoned-encoding-override`.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use serde_json::Value;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

/// Write copybook and data to temp dir, return (dir, `cpy_path`, `data_path`).
fn setup(cpy_text: &str, data: &[u8]) -> (TempDir, std::path::PathBuf, std::path::PathBuf) {
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let data_path = dir.path().join("data.bin");
    std::fs::write(&cpy, cpy_text).unwrap();
    std::fs::write(&data_path, data).unwrap();
    (dir, cpy, data_path)
}

// =========================================================================
// --float-format tests (COMP-1 / COMP-2 IEEE big-endian)
// =========================================================================

/// Copybook with COMP-1 (4-byte IEEE float) and COMP-2 (8-byte IEEE double).
const FLOAT_CPY: &str = "\
       01  REC.
           05  RATE      COMP-1.
           05  BALANCE   COMP-2.
";

#[test]
fn decode_float_format_ieee_be_comp1_comp2() {
    // IEEE-754 big-endian: COMP-1 42.0 = 0x42280000, COMP-2 1234.5 = 0x40934A0000000000
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0x42, 0x28, 0x00, 0x00]); // COMP-1: 42.0
    data.extend_from_slice(&[0x40, 0x93, 0x4A, 0x00, 0x00, 0x00, 0x00, 0x00]); // COMP-2: 1234.5

    let (dir, cpy, data_path) = setup(FLOAT_CPY, &data);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--float-format",
            "ieee-be",
            "--json-number",
            "native",
        ])
        .arg(&cpy)
        .arg(&data_path)
        .arg("--output")
        .arg(&out)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let v: Value = serde_json::from_str(content.trim()).unwrap();
    let rate = v["fields"]["RATE"].as_f64().unwrap();
    let balance = v["fields"]["BALANCE"].as_f64().unwrap();
    assert!((rate - 42.0).abs() < 0.01, "COMP-1 42.0, got {rate}");
    assert!(
        (balance - 1234.5).abs() < 0.01,
        "COMP-2 1234.5, got {balance}"
    );
}

// =========================================================================
// --json-number native tests
// =========================================================================

/// Simple ASCII copybook for number mode testing.
const NUMBER_CPY: &str = "\
       01  REC.
           05  QTY      PIC 9(5).
           05  PRICE    PIC 9(3)V99.
";

#[test]
fn decode_json_number_native_accepted_and_produces_output() {
    // QTY=00042, PRICE=12345 (123.45 with implied decimal V99)
    let data = b"0004212345";
    let (dir, cpy, data_path) = setup(NUMBER_CPY, data);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--json-number",
            "native",
        ])
        .arg(&cpy)
        .arg(&data_path)
        .arg("--output")
        .arg(&out)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let v: Value = serde_json::from_str(content.trim()).unwrap();
    // Native mode returns JSON numbers (not strings)
    assert!(
        v["fields"]["QTY"].is_number(),
        "QTY should be a JSON number in native mode, got: {}",
        v["fields"]["QTY"]
    );
    assert_eq!(v["fields"]["QTY"], serde_json::json!(42));
}

#[test]
fn decode_json_number_lossless_produces_json_strings() {
    let data = b"0004212345";
    let (dir, cpy, data_path) = setup(NUMBER_CPY, data);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--json-number",
            "lossless",
        ])
        .arg(&cpy)
        .arg(&data_path)
        .arg("--output")
        .arg(&out)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let v: Value = serde_json::from_str(content.trim()).unwrap();
    // In lossless mode, decimal values should be strings to preserve precision
    assert!(
        v["fields"]["PRICE"].is_string(),
        "PRICE should be JSON string in lossless mode, got: {}",
        v["fields"]["PRICE"]
    );
}

// =========================================================================
// --emit-raw field tests
// =========================================================================

const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME     PIC X(5).
           05  CODE     PIC 9(3).
";

#[test]
fn decode_emit_raw_field_accepted_without_error() {
    let data = b"HELLO123";
    let (dir, cpy, data_path) = setup(SIMPLE_CPY, data);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-raw",
            "field",
        ])
        .arg(&cpy)
        .arg(&data_path)
        .arg("--output")
        .arg(&out)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let v: Value = serde_json::from_str(content.trim()).unwrap();
    // Verify decode succeeded with field-level raw mode
    assert_eq!(v["fields"]["NAME"].as_str().unwrap(), "HELLO");
    assert_eq!(v["fields"]["CODE"].as_str().unwrap(), "123");
}

// =========================================================================
// --bwz-encode tests (BLANK WHEN ZERO)
// =========================================================================

const BWZ_CPY: &str = "\
       01  REC.
           05  AMOUNT   PIC 9(5) BLANK WHEN ZERO.
";

#[test]
fn encode_bwz_encode_flag_accepted() {
    // The --bwz-encode flag is accepted by the CLI. Verify encode succeeds.
    let jsonl = r#"{"fields":{"AMOUNT":"0"}}"#;
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let jsonl_path = dir.path().join("input.jsonl");
    let out = dir.path().join("output.bin");
    std::fs::write(&cpy, BWZ_CPY).unwrap();
    std::fs::write(&jsonl_path, format!("{jsonl}\n")).unwrap();

    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--bwz-encode",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&jsonl_path)
        .assert()
        .success();

    let encoded = std::fs::read(&out).unwrap();
    assert_eq!(encoded.len(), 5, "AMOUNT field should be 5 bytes");
}

#[test]
fn encode_bwz_encode_non_zero_unchanged() {
    let jsonl = r#"{"fields":{"AMOUNT":"42"}}"#;
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let jsonl_path = dir.path().join("input.jsonl");
    let out = dir.path().join("output.bin");
    std::fs::write(&cpy, BWZ_CPY).unwrap();
    std::fs::write(&jsonl_path, format!("{jsonl}\n")).unwrap();

    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--bwz-encode",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&jsonl_path)
        .assert()
        .success();

    let encoded = std::fs::read(&out).unwrap();
    // Non-zero values should still encode normally
    assert_eq!(
        encoded, b"00042",
        "Non-zero should encode normally, got: {encoded:?}",
    );
}

// =========================================================================
// --use-raw round-trip tests
// =========================================================================

#[test]
fn encode_use_raw_round_trip_preserves_bytes() {
    // Decode with --emit-raw record, then encode with --use-raw
    let original_data = b"HELLO123";
    let (dir, cpy, data_path) = setup(SIMPLE_CPY, original_data);
    let decoded_jsonl = dir.path().join("decoded.jsonl");
    let re_encoded = dir.path().join("re_encoded.bin");

    // Step 1: Decode with raw capture
    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-raw",
            "record",
        ])
        .arg(&cpy)
        .arg(&data_path)
        .arg("--output")
        .arg(&decoded_jsonl)
        .assert()
        .success();

    // Step 2: Encode with --use-raw to restore original bytes
    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--use-raw",
            "--output",
        ])
        .arg(&re_encoded)
        .arg(&cpy)
        .arg(&decoded_jsonl)
        .assert()
        .success();

    let result = std::fs::read(&re_encoded).unwrap();
    assert_eq!(
        result, original_data,
        "use-raw round-trip should preserve original bytes"
    );
}

// =========================================================================
// --zoned-encoding-override tests (encode side)
// =========================================================================

const ZONED_CPY: &str = "\
       01  REC.
           05  NUM      PIC 9(3).
";

#[test]
fn encode_zoned_encoding_override_ebcdic() {
    let jsonl = r#"{"fields":{"NUM":"123"}}"#;
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let jsonl_path = dir.path().join("input.jsonl");
    let out = dir.path().join("output.bin");
    std::fs::write(&cpy, ZONED_CPY).unwrap();
    std::fs::write(&jsonl_path, format!("{jsonl}\n")).unwrap();

    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--zoned-encoding-override",
            "ebcdic",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&jsonl_path)
        .assert()
        .success();

    let encoded = std::fs::read(&out).unwrap();
    // EBCDIC zoned digits: 1=0xF1, 2=0xF2, 3=0xF3
    assert_eq!(
        encoded,
        &[0xF1, 0xF2, 0xF3],
        "EBCDIC zoned override should produce F-zone digits, got: {encoded:02X?}",
    );
}

#[test]
fn encode_zoned_encoding_override_ascii() {
    let jsonl = r#"{"fields":{"NUM":"123"}}"#;
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let jsonl_path = dir.path().join("input.jsonl");
    let out = dir.path().join("output.bin");
    std::fs::write(&cpy, ZONED_CPY).unwrap();
    std::fs::write(&jsonl_path, format!("{jsonl}\n")).unwrap();

    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--zoned-encoding-override",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&jsonl_path)
        .assert()
        .success();

    let encoded = std::fs::read(&out).unwrap();
    // ASCII zoned digits: 1=0x31, 2=0x32, 3=0x33
    assert_eq!(
        encoded, b"123",
        "ASCII zoned override should produce ASCII digits, got: {encoded:02X?}",
    );
}

// =========================================================================
// --coerce-numbers (encode flag)
// =========================================================================

#[test]
fn encode_coerce_numbers_flag_accepted() {
    // The --coerce-numbers flag is accepted by the CLI. Test with string input (normal path).
    let jsonl = r#"{"fields":{"NUM":"123"}}"#;
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let jsonl_path = dir.path().join("input.jsonl");
    let out = dir.path().join("output.bin");
    std::fs::write(&cpy, ZONED_CPY).unwrap();
    std::fs::write(&jsonl_path, format!("{jsonl}\n")).unwrap();

    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--coerce-numbers",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&jsonl_path)
        .assert()
        .success();

    let encoded = std::fs::read(&out).unwrap();
    assert_eq!(
        encoded, b"123",
        "coerce-numbers flag should be accepted, got: {encoded:02X?}",
    );
}
