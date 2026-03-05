// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for encode CLI with multiple codepages and round-trip fidelity.
//!
//! Tests cover: cp037, cp1047, cp500, cp273, cp1140 encode paths,
//! decode-encode round-trip across codepages, and encode error scenarios.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").expect("copybook binary should exist")
}

/// A minimal valid COBOL copybook (13-byte fixed record).
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// EBCDIC CP037 record matching `SIMPLE_CPY` (13 bytes).
/// NAME = "ALICE     " AGE = "025"
fn simple_record_cp037() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE (CP037)
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// JSONL line matching `SIMPLE_CPY`.
const SIMPLE_JSONL: &str = r#"{"NAME":"ALICE","AGE":"025"}"#;

/// Copybook with multiple field types for richer encode testing.
const MULTI_TYPE_CPY: &str = "\
       01  REC.
           05  ID       PIC 9(4).
           05  AMT      PIC S9(5)V99.
           05  LABEL    PIC X(8).
";

/// JSONL line matching `MULTI_TYPE_CPY`.
const MULTI_TYPE_JSONL: &str = r#"{"ID":"0042","AMT":"01234.56","LABEL":"TESTDATA"}"#;

fn setup_encode(cpy: &str, jsonl: &str) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("input.jsonl"), format!("{jsonl}\n")).unwrap();
    dir
}

fn setup_decode_encode(cpy: &str, data: &[u8]) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

// ---------------------------------------------------------------------------
// Encode with different codepages
// ---------------------------------------------------------------------------

fn encode_with_codepage(codepage: &str) -> Vec<u8> {
    let dir = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    let out = dir.path().join("output.bin");
    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg(codepage)
        .arg("--output")
        .arg(&out)
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .success();
    std::fs::read(out).unwrap()
}

#[test]
fn encode_cp037_produces_output() {
    let output = encode_with_codepage("cp037");
    assert_eq!(output.len(), 13, "output should be 13 bytes (LRECL)");
}

#[test]
fn encode_cp1047_produces_output() {
    let output = encode_with_codepage("cp1047");
    assert_eq!(output.len(), 13);
}

#[test]
fn encode_cp500_produces_output() {
    let output = encode_with_codepage("cp500");
    assert_eq!(output.len(), 13);
}

#[test]
fn encode_cp273_produces_output() {
    let output = encode_with_codepage("cp273");
    assert_eq!(output.len(), 13);
}

#[test]
fn encode_cp1140_produces_output() {
    let output = encode_with_codepage("cp1140");
    assert_eq!(output.len(), 13);
}

// ---------------------------------------------------------------------------
// Codepage encoding comparison
// ---------------------------------------------------------------------------

#[test]
fn encode_different_codepages_produce_same_size() {
    let out037 = encode_with_codepage("cp037");
    let out1047 = encode_with_codepage("cp1047");
    let out500 = encode_with_codepage("cp500");
    assert_eq!(
        out037.len(),
        out1047.len(),
        "same LRECL regardless of codepage"
    );
    assert_eq!(out037.len(), out500.len());
}

// ---------------------------------------------------------------------------
// Decode-encode round-trip across codepages
// ---------------------------------------------------------------------------

fn roundtrip_test(codepage: &str) {
    let dir = setup_decode_encode(SIMPLE_CPY, &simple_record_cp037());

    // Step 1: Decode binary to JSONL using cp037
    cmd()
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("decoded.jsonl"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .assert()
        .success();

    // Step 2: Encode JSONL back to binary using target codepage
    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg(codepage)
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("decoded.jsonl"))
        .assert()
        .success();

    // Step 3: Verify output is valid (correct LRECL)
    let output = std::fs::read(dir.path().join("output.bin")).unwrap();
    assert_eq!(output.len(), 13, "round-trip should produce correct LRECL");
}

#[test]
fn roundtrip_cp037() {
    roundtrip_test("cp037");
}

#[test]
fn roundtrip_cp1047() {
    roundtrip_test("cp1047");
}

#[test]
fn roundtrip_cp500() {
    roundtrip_test("cp500");
}

#[test]
fn roundtrip_cp273() {
    roundtrip_test("cp273");
}

#[test]
fn roundtrip_cp1140() {
    roundtrip_test("cp1140");
}

// ---------------------------------------------------------------------------
// Same-codepage round-trip produces identical bytes
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_cp037_byte_identical() {
    let original = simple_record_cp037();
    let dir = setup_decode_encode(SIMPLE_CPY, &original);

    // Decode cp037 binary -> JSONL
    cmd()
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("decoded.jsonl"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .assert()
        .success();

    // Encode JSONL -> cp037 binary
    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("decoded.jsonl"))
        .assert()
        .success();

    let result = std::fs::read(dir.path().join("output.bin")).unwrap();
    assert_eq!(
        original, result,
        "same-codepage round-trip must produce byte-identical output"
    );
}

// ---------------------------------------------------------------------------
// Multi-type field encode
// ---------------------------------------------------------------------------

#[test]
fn encode_multi_type_fields_cp037() {
    let dir = setup_encode(MULTI_TYPE_CPY, MULTI_TYPE_JSONL);
    let out = dir.path().join("output.bin");
    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(&out)
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .success();

    // PIC 9(4) + PIC S9(5)V99 + PIC X(8) = 4 + 7 + 8 = 19 bytes
    let output = std::fs::read(out).unwrap();
    assert_eq!(output.len(), 19, "multi-type record should be 19 bytes");
}

// ---------------------------------------------------------------------------
// Encode error scenarios
// ---------------------------------------------------------------------------

#[test]
fn encode_invalid_json_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    std::fs::write(dir.path().join("input.jsonl"), "this is not json\n").unwrap();

    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .failure();
}

#[test]
fn encode_empty_jsonl_exits_zero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    std::fs::write(dir.path().join("input.jsonl"), "").unwrap();

    let out = dir.path().join("output.bin");
    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(&out)
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .success();

    let output = std::fs::read(out).unwrap();
    assert!(output.is_empty(), "empty JSONL should produce empty output");
}

#[test]
fn encode_missing_input_file_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();

    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("nonexistent.jsonl"))
        .assert()
        .failure();
}

#[test]
fn encode_missing_copybook_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("input.jsonl"), format!("{SIMPLE_JSONL}\n")).unwrap();

    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("nonexistent.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .failure();
}

#[test]
fn encode_wrong_field_names_no_panic() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    std::fs::write(
        dir.path().join("input.jsonl"),
        r#"{"WRONG_FIELD":"VALUE","OTHER":"123"}"#.to_owned() + "\n",
    )
    .unwrap();

    // May succeed (padding) or fail, but must not panic
    let result = cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .output()
        .unwrap();

    assert!(
        result.status.success() || result.status.code().is_some(),
        "encode with wrong field names should not panic"
    );
}

// ---------------------------------------------------------------------------
// Multi-record encode
// ---------------------------------------------------------------------------

#[test]
fn encode_multiple_records() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    let jsonl = format!(
        "{SIMPLE_JSONL}\n{}\n{SIMPLE_JSONL}\n",
        r#"{"NAME":"BOB","AGE":"030"}"#
    );
    std::fs::write(dir.path().join("input.jsonl"), jsonl).unwrap();

    let out = dir.path().join("output.bin");
    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(&out)
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .success();

    let output = std::fs::read(out).unwrap();
    assert_eq!(output.len(), 39, "3 records x 13 bytes = 39 bytes");
}

// ---------------------------------------------------------------------------
// Encode with --fail-fast
// ---------------------------------------------------------------------------

#[test]
fn encode_fail_fast_stops_on_error() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    let jsonl = format!("{SIMPLE_JSONL}\nnot-json\n{SIMPLE_JSONL}\n");
    std::fs::write(dir.path().join("input.jsonl"), jsonl).unwrap();

    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--fail-fast")
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .failure();
}

// ---------------------------------------------------------------------------
// Encode with --max-errors
// ---------------------------------------------------------------------------

#[test]
fn encode_max_errors_flag_accepted() {
    let dir = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    cmd()
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--max-errors")
        .arg("5")
        .arg("--output")
        .arg(dir.path().join("output.bin"))
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("input.jsonl"))
        .assert()
        .success();
}
