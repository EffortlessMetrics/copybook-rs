// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `determinism` CLI subcommand flags and output formats.
//!
//! Covers: decode/encode/round-trip subcommands, --output human/json,
//! --max-diffs, exit codes (0/2/3), missing file errors, codepage variants,
//! --emit-meta, and structured JSON output validation.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").expect("copybook binary should exist")
}

/// Minimal copybook (13-byte fixed record).
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// EBCDIC CP037 record: NAME="ALICE     " AGE="025" (13 bytes).
fn simple_record_cp037() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// JSONL matching `SIMPLE_CPY`.
const SIMPLE_JSONL: &str = r#"{"NAME":"ALICE","AGE":"025"}"#;

fn setup_decode(cpy: &str, data: &[u8]) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

fn setup_encode(cpy: &str, jsonl: &str) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("input.jsonl"), format!("{jsonl}\n")).unwrap();
    dir
}

fn cpy_path(dir: &TempDir) -> PathBuf {
    dir.path().join("schema.cpy")
}

fn data_path(dir: &TempDir) -> PathBuf {
    dir.path().join("data.bin")
}

fn jsonl_path(dir: &TempDir) -> PathBuf {
    dir.path().join("input.jsonl")
}

// ---------------------------------------------------------------------------
// determinism decode: exit code 0 (deterministic)
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_exits_zero() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// determinism encode: exit code 0
// ---------------------------------------------------------------------------

#[test]
fn determinism_encode_exits_zero() {
    let dir = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    cmd()
        .arg("determinism")
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(cpy_path(&dir))
        .arg(jsonl_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// determinism round-trip: exit code 0
// ---------------------------------------------------------------------------

#[test]
fn determinism_roundtrip_exits_zero() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("determinism")
        .arg("round-trip")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// --output human (default)
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_human_output_contains_deterministic() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let output = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("human")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}{stderr}");
    let lower = combined.to_lowercase();
    assert!(
        lower.contains("deterministic") || lower.contains("match"),
        "human output should indicate determinism: {combined}"
    );
}

// ---------------------------------------------------------------------------
// --output json
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_json_output_is_valid_json() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let output = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    assert!(output.status.success(), "should succeed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // JSON output should parse as valid JSON
    let parsed: serde_json::Value =
        serde_json::from_str(stdout.trim()).expect("JSON output should be valid JSON");
    assert!(parsed.is_object(), "JSON output should be an object");
}

#[test]
fn determinism_decode_json_contains_deterministic_field() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let output = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);
    let parsed: serde_json::Value = serde_json::from_str(stdout.trim()).unwrap();
    // Should have a deterministic/match field
    let has_deterministic = parsed.get("deterministic").is_some()
        || parsed.get("is_deterministic").is_some()
        || parsed.get("match").is_some()
        || parsed.get("result").is_some();
    assert!(
        has_deterministic,
        "JSON should have deterministic result field: {parsed}"
    );
}

#[test]
fn determinism_encode_json_output_is_valid_json() {
    let dir = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    let output = cmd()
        .arg("determinism")
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir))
        .arg(jsonl_path(&dir))
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let parsed: serde_json::Value =
        serde_json::from_str(stdout.trim()).expect("encode JSON output should be valid");
    assert!(parsed.is_object());
}

#[test]
fn determinism_roundtrip_json_output_is_valid_json() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let output = cmd()
        .arg("determinism")
        .arg("round-trip")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let parsed: serde_json::Value =
        serde_json::from_str(stdout.trim()).expect("round-trip JSON output should be valid");
    assert!(parsed.is_object());
}

// ---------------------------------------------------------------------------
// --max-diffs flag accepted
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_max_diffs_accepted() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--max-diffs")
        .arg("10")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn determinism_encode_max_diffs_accepted() {
    let dir = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    cmd()
        .arg("determinism")
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--max-diffs")
        .arg("5")
        .arg(cpy_path(&dir))
        .arg(jsonl_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// --emit-meta flag accepted
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_emit_meta_accepted() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--emit-meta")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// --json-number mode accepted
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_json_number_lossless() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--json-number")
        .arg("lossless")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn determinism_decode_json_number_native() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--json-number")
        .arg("native")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// Error exit codes: missing files → exit code 3
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_missing_copybook_exits_nonzero() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let result = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(dir.path().join("nonexistent.cpy"))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    assert!(!result.status.success(), "missing copybook should fail");
}

#[test]
fn determinism_decode_missing_data_exits_nonzero() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let result = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(cpy_path(&dir))
        .arg(dir.path().join("nonexistent.bin"))
        .output()
        .unwrap();

    assert!(!result.status.success(), "missing data file should fail");
}

#[test]
fn determinism_encode_missing_jsonl_exits_nonzero() {
    let dir = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    let result = cmd()
        .arg("determinism")
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(cpy_path(&dir))
        .arg(dir.path().join("nonexistent.jsonl"))
        .output()
        .unwrap();

    assert!(!result.status.success(), "missing JSONL file should fail");
}

// ---------------------------------------------------------------------------
// Codepage variants
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_cp1047() {
    // cp1047 record: construct from scratch
    // ALICE in cp1047: same EBCDIC as cp037 for basic letters
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    // Use cp037 data but test that the command runs with cp1047 flag
    // (data may decode differently, but command should not crash)
    let result = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp1047")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    // Should succeed (command runs) — determinism check compares two runs
    assert!(
        result.status.success(),
        "determinism decode with cp1047 should succeed: {}",
        String::from_utf8_lossy(&result.stderr)
    );
}

#[test]
fn determinism_encode_cp500() {
    let dir = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    cmd()
        .arg("determinism")
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp500")
        .arg(cpy_path(&dir))
        .arg(jsonl_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// JSON output hash fields
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_json_has_hash_fields() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let output = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let parsed: serde_json::Value = serde_json::from_str(stdout.trim()).unwrap();
    // JSON output should contain hash-related info
    let json_str = parsed.to_string().to_lowercase();
    assert!(
        json_str.contains("hash") || json_str.contains("sha") || json_str.contains("digest"),
        "JSON output should contain hash information: {parsed}"
    );
}

// ---------------------------------------------------------------------------
// Determinism with multi-type schema
// ---------------------------------------------------------------------------

const MULTI_TYPE_CPY: &str = "\
       01  REC.
           05  ID       PIC 9(4).
           05  AMT      PIC S9(5)V99.
           05  LABEL    PIC X(8).
";

fn multi_type_record_cp037() -> Vec<u8> {
    // ID=0042 (F0F0F4F2), AMT=+01234.56 (F0F1F2F3F4F5C6), LABEL=TESTDATA
    vec![
        0xF0, 0xF0, 0xF4, 0xF2, // 0042
        0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xC6, // +0123456
        0xE3, 0xC5, 0xE2, 0xE3, 0xC4, 0xC1, 0xE3, 0xC1, // TESTDATA
    ]
}

#[test]
fn determinism_decode_multi_type_exits_zero() {
    let dir = setup_decode(MULTI_TYPE_CPY, &multi_type_record_cp037());
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn determinism_roundtrip_multi_type_exits_zero() {
    let dir = setup_decode(MULTI_TYPE_CPY, &multi_type_record_cp037());
    cmd()
        .arg("determinism")
        .arg("round-trip")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// Determinism JSON output with --emit-meta
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_emit_meta_json_valid() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let output = cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--emit-meta")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let _parsed: serde_json::Value =
        serde_json::from_str(stdout.trim()).expect("emit-meta JSON should be valid");
}

// ---------------------------------------------------------------------------
// max-diffs = 0 still succeeds on deterministic data
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_max_diffs_zero_succeeds() {
    let dir = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--max-diffs")
        .arg("0")
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// All three subcommands accept --output json
// ---------------------------------------------------------------------------

#[test]
fn determinism_all_subcommands_accept_output_json() {
    let dir_decode = setup_decode(SIMPLE_CPY, &simple_record_cp037());
    let dir_encode = setup_encode(SIMPLE_CPY, SIMPLE_JSONL);
    let dir_rt = setup_decode(SIMPLE_CPY, &simple_record_cp037());

    // decode
    cmd()
        .arg("determinism")
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir_decode))
        .arg(data_path(&dir_decode))
        .assert()
        .success();

    // encode
    cmd()
        .arg("determinism")
        .arg("encode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir_encode))
        .arg(jsonl_path(&dir_encode))
        .assert()
        .success();

    // round-trip
    cmd()
        .arg("determinism")
        .arg("round-trip")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg("json")
        .arg(cpy_path(&dir_rt))
        .arg(data_path(&dir_rt))
        .assert()
        .success();
}
