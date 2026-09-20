// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for strict fixed-block tails (`fixed-blocked-retained`, #1161).
//!
//! Policy: exact multiples decode, short tails stay classified errors
//! naming expected and actual lengths (the JRecord NUL-pad leniency is the
//! explicitly rejected pole), and encode emits exact multiples only: a
//! short `--use-raw` capture is refused with `CBKE532` instead of writing
//! a tail no fixed decoder accepts.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use base64::Engine as _;
use serde_json::Value;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

fn workspace_path(rel: &str) -> std::path::PathBuf {
    // copybook-e2e lives at tests/e2e; fixtures live at the workspace root.
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(rel)
}

#[test]
fn decode_short_tail_names_expected_and_actual_lengths() {
    // Two 14-byte records plus a 6-byte tail.
    let full = std::fs::read(workspace_path("fixtures/corpus/mini_fixed.bin")).unwrap();
    assert_eq!(full.len(), 28);
    let dir = TempDir::new().unwrap();
    let data = dir.path().join("short.bin");
    std::fs::write(&data, &full[..20]).unwrap();
    let out = dir.path().join("out.jsonl");

    let assertion = cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(&data)
        .assert();
    let output = assertion.get_output();
    assert!(!output.status.success(), "short tail must fail");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("CBKR101"),
        "short tail stays classified: {stderr}"
    );
    assert!(
        stderr.contains("14") && stderr.contains('6'),
        "error names expected (14) and actual (6) lengths: {stderr}"
    );
}

#[test]
fn decode_exact_multiple_succeeds() {
    let dir = TempDir::new().unwrap();
    let out = dir.path().join("out.jsonl");
    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(workspace_path("fixtures/corpus/mini_fixed.bin"))
        .assert()
        .success();
    let text = std::fs::read_to_string(&out).unwrap();
    assert_eq!(text.lines().count(), 2);
}

#[test]
fn decode_empty_file_yields_zero_records() {
    let dir = TempDir::new().unwrap();
    let data = dir.path().join("empty.bin");
    std::fs::write(&data, []).unwrap();
    let out = dir.path().join("out.jsonl");
    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(&data)
        .assert()
        .success();
}

#[test]
fn encode_short_raw_capture_is_refused_not_emitted() {
    // Capture one exact record, truncate its raw bytes, replay with --use-raw.
    let dir = TempDir::new().unwrap();
    let decoded = dir.path().join("dec.jsonl");
    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-raw",
            "record",
            "--output",
        ])
        .arg(&decoded)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(workspace_path("fixtures/corpus/mini_fixed.bin"))
        .assert()
        .success();
    let first: Value = serde_json::from_str(
        std::fs::read_to_string(&decoded)
            .unwrap()
            .lines()
            .next()
            .unwrap(),
    )
    .unwrap();
    let mut short = first.clone();
    let raw = base64::engine::general_purpose::STANDARD
        .decode(short["raw_b64"].as_str().unwrap())
        .unwrap();
    assert_eq!(raw.len(), 14);
    short["raw_b64"] = Value::String(base64::engine::general_purpose::STANDARD.encode(&raw[..10]));
    let input = dir.path().join("short.jsonl");
    std::fs::write(&input, serde_json::to_string(&short).unwrap()).unwrap();

    let out = dir.path().join("out.bin");
    let assertion = cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--use-raw",
            "--output",
        ])
        .arg(&out)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(&input)
        .assert();
    let output = assertion.get_output();
    assert!(!output.status.success(), "short raw must fail");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("CBKE532"),
        "refusal names the code: {stderr}"
    );
    assert!(
        stderr.contains("10") && stderr.contains("14"),
        "refusal names captured (10) and layout (14) lengths: {stderr}"
    );
}

#[test]
fn encode_exact_raw_capture_round_trips_byte_identical() {
    let dir = TempDir::new().unwrap();
    let decoded = dir.path().join("dec.jsonl");
    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-raw",
            "record",
            "--output",
        ])
        .arg(&decoded)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(workspace_path("fixtures/corpus/mini_fixed.bin"))
        .assert()
        .success();
    let out = dir.path().join("out.bin");
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
        .arg(&out)
        .arg(workspace_path("fixtures/corpus/mini.cpy"))
        .arg(&decoded)
        .assert()
        .success();
    let original = std::fs::read(workspace_path("fixtures/corpus/mini_fixed.bin")).unwrap();
    let replayed = std::fs::read(&out).unwrap();
    assert_eq!(replayed, original);
}
