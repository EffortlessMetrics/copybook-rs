// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for `--format text`: line-delimited fixed-width records.
//!
//! Oracle: JRecord 0.93.2 `Cobol2Csv -IFS Text` (see
//! `docs/evidence/differential-breadth/README.md`, lane 2) plus POSIX line
//! semantics. Terminator handling follows the oracle (LF/CRLF stripped,
//! final unterminated line accepted); line-length policy is strict where
//! the oracle is lenient: short/long lines are classified errors naming
//! expected and actual lengths, never silent pads or truncations.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use serde_json::Value;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

/// Two `PIC X(4)` fields: one 8-byte line per record.
const LINES_CPY: &str = "\
       01  L-REC.\n\
           05  L-A           PIC X(4).\n\
           05  L-B           PIC X(4).\n";

fn setup(cpy_text: &str, data: &[u8]) -> (TempDir, std::path::PathBuf, std::path::PathBuf) {
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let data_path = dir.path().join("data.txt");
    std::fs::write(&cpy, cpy_text).unwrap();
    std::fs::write(&data_path, data).unwrap();
    (dir, cpy, data_path)
}

fn decode_text(
    dir: &TempDir,
    cpy: &std::path::Path,
    data: &std::path::Path,
) -> assert_cmd::assert::Assert {
    decode_text_with_raw(dir, cpy, data, false)
}

fn decode_text_with_raw(
    dir: &TempDir,
    cpy: &std::path::Path,
    data: &std::path::Path,
    raw: bool,
) -> assert_cmd::assert::Assert {
    let out = dir.path().join("out.jsonl");
    let mut binding = cmd();
    let command = binding.args(["decode", "--format", "text", "--codepage", "ascii"]);
    if raw {
        command.arg("--emit-raw").arg("record");
    }
    command
        .arg("--output")
        .arg(&out)
        .arg(cpy)
        .arg(data)
        .assert()
}

fn read_records(dir: &TempDir) -> Vec<Value> {
    let text = std::fs::read_to_string(dir.path().join("out.jsonl")).unwrap();
    text.lines()
        .filter(|line| !line.trim().is_empty() || line.contains('{'))
        .filter_map(|line| serde_json::from_str::<Value>(line).ok())
        .filter(|value| value.get("schema").is_some())
        .collect()
}

#[test]
fn text_decodes_line_delimited_fixed_records() {
    let (_dir_guard, cpy, data) = setup(LINES_CPY, b"ABCD1234\nEFGH5678\n");
    let dir = _dir_guard;
    decode_text(&dir, &cpy, &data).success();
    let records = read_records(&dir);
    assert_eq!(records.len(), 2, "expected two records, got {records:?}");
    assert_eq!(records[0]["L-A"], Value::String("ABCD".to_string()));
    assert_eq!(records[0]["L-B"], Value::String("1234".to_string()));
    assert_eq!(records[1]["L-A"], Value::String("EFGH".to_string()));
    assert_eq!(records[1]["L-B"], Value::String("5678".to_string()));
}

#[test]
fn text_terminators_are_framing_not_payload() {
    let (_dir_guard, cpy, data) = setup(LINES_CPY, b"ABCD1234\n");
    let dir = _dir_guard;
    decode_text_with_raw(&dir, &cpy, &data, true).success();
    let records = read_records(&dir);
    assert_eq!(records.len(), 1);
    // Raw capture holds the 8 payload bytes only, no LF.
    let raw = records[0]["raw_b64"].as_str().unwrap();
    assert_eq!(raw, "QUJDRDEyMzQ=");
}

#[test]
fn text_accepts_final_line_without_terminator() {
    // JRecord -IFS Text emits the row; copybook-rs accepts it too.
    let (_dir_guard, cpy, data) = setup(LINES_CPY, b"ABCD1234\nEFGH5678");
    let dir = _dir_guard;
    decode_text(&dir, &cpy, &data).success();
    let records = read_records(&dir);
    assert_eq!(records.len(), 2, "expected two records, got {records:?}");
    assert_eq!(records[1]["L-A"], Value::String("EFGH".to_string()));
}

#[test]
fn text_strips_crlf_terminators() {
    let (_dir_guard, cpy, data) = setup(LINES_CPY, b"ABCD1234\r\nEFGH5678\r\n");
    let dir = _dir_guard;
    decode_text(&dir, &cpy, &data).success();
    let records = read_records(&dir);
    assert_eq!(records.len(), 2, "expected two records, got {records:?}");
    assert_eq!(records[0]["L-B"], Value::String("1234".to_string()));
}

#[test]
fn text_short_line_is_classified_error_naming_lengths() {
    // JRecord pads leniently; copybook-rs rejects naming expected/actual.
    let (_dir_guard, cpy, data) = setup(LINES_CPY, b"ABCD12\n");
    let dir = _dir_guard;
    let assertion = decode_text(&dir, &cpy, &data);
    let output = assertion.get_output();
    assert!(!output.status.success(), "short line must fail");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("8") && stderr.contains('6'),
        "error must name expected (8) and actual (6) lengths: {stderr}"
    );
}

#[test]
fn text_long_line_is_classified_error_not_truncation() {
    // JRecord truncates leniently; copybook-rs rejects.
    let (_dir_guard, cpy, data) = setup(LINES_CPY, b"ABCD12345\n");
    let dir = _dir_guard;
    let assertion = decode_text(&dir, &cpy, &data);
    assert!(
        !assertion.get_output().status.success(),
        "long line must fail"
    );
}

// =========================================================================
// Encode symmetry
// =========================================================================

fn encode_text(
    dir: &TempDir,
    cpy: &std::path::Path,
    input: &std::path::Path,
    terminator: Option<&str>,
) -> (assert_cmd::assert::Assert, Vec<u8>) {
    let out = dir.path().join("enc.bin");
    let mut binding = cmd();
    let command = binding.args(["encode", "--format", "text", "--codepage", "ascii"]);
    if let Some(term) = terminator {
        command.arg("--text-terminator").arg(term);
    }
    let assertion = command
        .arg("--output")
        .arg(&out)
        .arg(cpy)
        .arg(input)
        .assert();
    let bytes = std::fs::read(&out).unwrap_or_default();
    (assertion, bytes)
}

#[test]
fn text_encode_emits_lf_terminated_lines() {
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let input = dir.path().join("rows.jsonl");
    std::fs::write(&cpy, LINES_CPY).unwrap();
    std::fs::write(&input, "{\"L-A\": \"ABCD\", \"L-B\": \"1234\"}\n").unwrap();
    let (assertion, bytes) = encode_text(&dir, &cpy, &input, None);
    assertion.success();
    assert_eq!(bytes, b"ABCD1234\n");
}

#[test]
fn text_encode_crlf_terminator_is_configured() {
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let input = dir.path().join("rows.jsonl");
    std::fs::write(&cpy, LINES_CPY).unwrap();
    std::fs::write(&input, "{\"L-A\": \"ABCD\", \"L-B\": \"1234\"}\n").unwrap();
    let (assertion, bytes) = encode_text(&dir, &cpy, &input, Some("crlf"));
    assertion.success();
    assert_eq!(bytes, b"ABCD1234\r\n");
}

#[test]
fn text_encode_decode_round_trip() {
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let input = dir.path().join("rows.jsonl");
    std::fs::write(&cpy, LINES_CPY).unwrap();
    std::fs::write(
        &input,
        "{\"L-A\": \"ABCD\", \"L-B\": \"1234\"}\n{\"L-A\": \"EFGH\", \"L-B\": \"5678\"}\n",
    )
    .unwrap();
    let (assertion, _) = encode_text(&dir, &cpy, &input, None);
    assertion.success();
    let encoded = dir.path().join("enc.bin");
    decode_text(&dir, &cpy, &encoded).success();
    let records = read_records(&dir);
    assert_eq!(records.len(), 2);
    assert_eq!(records[0]["L-A"], Value::String("ABCD".to_string()));
    assert_eq!(records[1]["L-B"], Value::String("5678".to_string()));
}
