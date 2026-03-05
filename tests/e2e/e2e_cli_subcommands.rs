// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive E2E tests for every CLI subcommand using `assert_cmd`.
//!
//! Tests cover: parse, inspect, decode, encode, verify, determinism,
//! version/help, error paths, and exit codes.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use std::path::PathBuf;
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

/// A copybook with a numeric field that has decimals (for lossless checks).
const NUMERIC_CPY: &str = "\
       01  REC.
           05  ID     PIC 9(4).
           05  AMT    PIC 9(5)V99.
           05  LABEL  PIC X(6).
";

/// EBCDIC CP037 record matching `SIMPLE_CPY` (13 bytes).
/// NAME = "ALICE     " AGE = "025"
fn simple_record() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// Two concatenated records for multi-record tests.
fn two_records() -> Vec<u8> {
    let mut data = simple_record();
    // BOB       030
    data.extend_from_slice(&[
        0xC2, 0xD6, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xF0, 0xF3, 0xF0,
    ]);
    data
}

/// EBCDIC CP037 record matching `NUMERIC_CPY` (17 bytes).
/// ID = "0001", AMT = "0012345" (123.45), LABEL = "ITEM  "
fn numeric_record() -> Vec<u8> {
    vec![
        0xF0, 0xF0, 0xF0, 0xF1, // 0001
        0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, // 0012345
        0xC9, 0xE3, 0xC5, 0xD4, 0x40, 0x40, // ITEM
    ]
}

/// Prepare a temp dir with `SIMPLE_CPY` and one EBCDIC record.
fn setup_simple() -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    std::fs::write(dir.path().join("data.bin"), simple_record()).unwrap();
    dir
}

/// Prepare a temp dir with `SIMPLE_CPY` and two EBCDIC records.
fn setup_two_records() -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    std::fs::write(dir.path().join("data.bin"), two_records()).unwrap();
    dir
}

/// Prepare a temp dir with `NUMERIC_CPY` and the numeric record.
fn setup_numeric() -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), NUMERIC_CPY).unwrap();
    std::fs::write(dir.path().join("data.bin"), numeric_record()).unwrap();
    dir
}

fn p(dir: &TempDir, name: &str) -> PathBuf {
    dir.path().join(name)
}

/// Decode the simple record to JSONL and return the path.
fn decode_to_jsonl(dir: &TempDir) -> PathBuf {
    let out = p(dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(dir, "schema.cpy"))
        .arg(p(dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
    out
}

// =========================================================================
// 1. PARSE: Parse copybook to JSON, verify structure
// =========================================================================

#[test]
fn parse_produces_valid_json_schema() {
    let dir = setup_simple();
    cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success()
        .stdout(predicate::str::contains("NAME"))
        .stdout(predicate::str::contains("AGE"));
}

#[test]
fn parse_output_is_valid_json() {
    let dir = setup_simple();
    let assert = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success();

    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let _json: serde_json::Value =
        serde_json::from_str(&stdout).expect("parse output must be valid JSON");
}

// =========================================================================
// 2. PARSE with --dialect flags
// =========================================================================

#[test]
fn parse_with_dialect_normative() {
    let dir = setup_simple();
    cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .args(["--dialect", "n"])
        .assert()
        .success();
}

#[test]
fn parse_with_dialect_zero_tolerant() {
    let dir = setup_simple();
    cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .args(["--dialect", "0"])
        .assert()
        .success();
}

#[test]
fn parse_with_dialect_one_tolerant() {
    let dir = setup_simple();
    cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .args(["--dialect", "1"])
        .assert()
        .success();
}

// =========================================================================
// 3. INSPECT: output format (human-readable)
// =========================================================================

#[test]
fn inspect_shows_human_readable_layout() {
    let dir = setup_simple();
    cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success()
        .stdout(predicate::str::contains("NAME"))
        .stdout(predicate::str::contains("AGE"));
}

// =========================================================================
// 4. INSPECT: shows field offsets and sizes
// =========================================================================

#[test]
fn inspect_shows_field_offsets_and_sizes() {
    let dir = setup_simple();
    let assert = cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success();

    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    // Inspect should show size info (10 for NAME, 3 for AGE, or 13 for REC)
    assert!(
        stdout.contains("10") || stdout.contains("13") || stdout.contains("PIC"),
        "inspect should show field sizes or PIC clauses, got:\n{stdout}"
    );
}

// =========================================================================
// 5. DECODE: Fixed-format decode to JSONL
// =========================================================================

#[test]
fn decode_fixed_format_to_jsonl() {
    let dir = setup_simple();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    let jsonl = std::fs::read_to_string(&out).unwrap();
    assert!(!jsonl.is_empty());
    for line in jsonl.lines() {
        let val: serde_json::Value = serde_json::from_str(line).expect("valid JSON line");
        assert!(val.to_string().contains("NAME"));
        assert!(val.to_string().contains("AGE"));
    }
}

#[test]
fn decode_multiple_records_produces_correct_line_count() {
    let dir = setup_two_records();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    let jsonl = std::fs::read_to_string(&out).unwrap();
    assert_eq!(jsonl.lines().count(), 2, "should decode exactly 2 records");
}

// =========================================================================
// 6. DECODE with --select (field projection)
// =========================================================================

#[test]
fn decode_with_select_field_projection() {
    let dir = setup_simple();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "NAME",
        ])
        .assert()
        .success();

    let jsonl = std::fs::read_to_string(&out).unwrap();
    assert!(!jsonl.is_empty(), "projected output should not be empty");
    for line in jsonl.lines() {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        let text = val.to_string();
        assert!(
            text.contains("NAME"),
            "projected output should contain NAME"
        );
    }
}

// =========================================================================
// 7. DECODE with --emit-meta
// =========================================================================

#[test]
fn decode_with_emit_meta() {
    let dir = setup_simple();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--emit-meta"])
        .assert()
        .success();

    let jsonl = std::fs::read_to_string(&out).unwrap();
    assert!(
        jsonl.contains("__record_index") || jsonl.contains("__length") || jsonl.contains("schema"),
        "emit-meta output should contain metadata fields, got: {jsonl}"
    );
}

// =========================================================================
// 8. DECODE with --json-number lossless
// =========================================================================

#[test]
fn decode_with_json_number_lossless() {
    let dir = setup_numeric();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--json-number",
            "lossless",
        ])
        .assert()
        .success();

    let jsonl = std::fs::read_to_string(&out).unwrap();
    assert!(!jsonl.is_empty());
    // In lossless mode, the numeric value should be preserved as a string or exact number
    let first: serde_json::Value = serde_json::from_str(jsonl.lines().next().unwrap()).unwrap();
    let text = first.to_string();
    assert!(
        text.contains("AMT") || text.contains("ID"),
        "lossless output should contain field data"
    );
}

// =========================================================================
// 9. DECODE with --threads N
// =========================================================================

#[test]
fn decode_with_threads() {
    let dir = setup_two_records();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--threads", "2"])
        .assert()
        .success();

    let jsonl = std::fs::read_to_string(&out).unwrap();
    assert_eq!(
        jsonl.lines().count(),
        2,
        "threaded decode should produce 2 records"
    );
}

// =========================================================================
// 10. ENCODE: Encode JSONL to binary
// =========================================================================

#[test]
fn encode_jsonl_to_binary() {
    let dir = setup_simple();
    let jsonl = decode_to_jsonl(&dir);
    let out = p(&dir, "encoded.bin");

    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    let encoded = std::fs::read(&out).unwrap();
    assert_eq!(encoded.len(), 13, "encoded record should be 13 bytes");
    assert_eq!(
        encoded,
        simple_record(),
        "round-trip should produce identical bytes"
    );
}

// =========================================================================
// 11. ENCODE with --fail-fast
// =========================================================================

#[test]
fn encode_with_fail_fast_bad_input() {
    let dir = setup_simple();
    let bad_jsonl = p(&dir, "bad.jsonl");
    std::fs::write(&bad_jsonl, "NOT JSON\n").unwrap();
    let out = p(&dir, "encoded.bin");

    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&bad_jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .assert()
        .failure();
}

#[test]
fn encode_with_fail_fast_false() {
    let dir = setup_simple();
    let jsonl = decode_to_jsonl(&dir);
    let out = p(&dir, "encoded.bin");

    // --fail-fast defaults to true for encode; just test encode succeeds without it
    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
}

// =========================================================================
// 12. ENCODE with --max-errors N
// =========================================================================

#[test]
fn encode_with_max_errors() {
    let dir = setup_simple();
    let jsonl = decode_to_jsonl(&dir);
    let out = p(&dir, "encoded.bin");

    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--max-errors",
            "5",
        ])
        .assert()
        .success();
}

// =========================================================================
// 13. VERIFY: valid data exits 0
// =========================================================================

#[test]
fn verify_valid_data_exits_zero() {
    let dir = setup_simple();

    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
}

#[test]
fn verify_with_report_produces_file() {
    let dir = setup_simple();
    let report = p(&dir, "report.json");

    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .arg("--report")
        .arg(&report)
        .assert()
        .success();

    assert!(report.exists(), "report file should be written");
    let contents = std::fs::read_to_string(&report).unwrap();
    assert!(!contents.is_empty(), "report should not be empty");
}

// =========================================================================
// 14. VERIFY: invalid data exits non-zero
// =========================================================================

#[test]
fn verify_nonexistent_data_exits_nonzero() {
    let dir = setup_simple();

    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg("nonexistent_data_file_99999.bin")
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .failure();
}

// =========================================================================
// 15. DETERMINISM DECODE: exits 0 for deterministic data
// =========================================================================

#[test]
fn determinism_decode_exits_zero() {
    let dir = setup_simple();

    cmd()
        .args(["determinism", "decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success()
        .stdout(
            predicate::str::contains("deterministic")
                .or(predicate::str::contains("Deterministic"))
                .or(predicate::str::contains("DETERMINISTIC"))
                .or(predicate::str::contains("PASS"))
                .or(predicate::str::contains("match")),
        );
}

#[test]
fn determinism_decode_json_output() {
    let dir = setup_simple();

    let assert = cmd()
        .args(["determinism", "decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            "json",
        ])
        .assert()
        .success();

    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let _json: serde_json::Value =
        serde_json::from_str(&stdout).expect("determinism JSON output must be valid JSON");
}

// =========================================================================
// 16. DETERMINISM ROUND-TRIP: exits 0
// =========================================================================

#[test]
fn determinism_round_trip_exits_zero() {
    let dir = setup_simple();

    cmd()
        .args(["determinism", "round-trip"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
}

#[test]
fn determinism_encode_exits_zero() {
    let dir = setup_simple();
    let jsonl = decode_to_jsonl(&dir);

    cmd()
        .args(["determinism", "encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
}

// =========================================================================
// 17. VERSION and HELP flags
// =========================================================================

#[test]
fn version_flag_exits_zero() {
    cmd()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("copybook"));
}

#[test]
fn help_flag_exits_zero_and_lists_subcommands() {
    cmd()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("parse"))
        .stdout(predicate::str::contains("inspect"))
        .stdout(predicate::str::contains("decode"))
        .stdout(predicate::str::contains("encode"))
        .stdout(predicate::str::contains("verify"))
        .stdout(predicate::str::contains("determinism"));
}

#[test]
fn parse_help_exits_zero() {
    cmd()
        .args(["parse", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("parse")));
}

#[test]
fn decode_help_exits_zero() {
    cmd()
        .args(["decode", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("decode")));
}

#[test]
fn encode_help_exits_zero() {
    cmd()
        .args(["encode", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("encode")));
}

#[test]
fn verify_help_exits_zero() {
    cmd()
        .args(["verify", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("verify")));
}

#[test]
fn determinism_help_exits_zero() {
    cmd()
        .args(["determinism", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("determinism")));
}

// =========================================================================
// 18. ERROR PATHS: missing file, bad format, invalid codepage
// =========================================================================

#[test]
fn parse_missing_file_exits_nonzero() {
    cmd()
        .args(["parse", "nonexistent_file_99999.cpy"])
        .assert()
        .failure();
}

#[test]
fn parse_invalid_syntax_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), b"THIS IS NOT VALID COBOL !!!").unwrap();

    cmd()
        .args(["parse"])
        .arg(p(&dir, "bad.cpy"))
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("CBK")
                .or(predicate::str::contains("error"))
                .or(predicate::str::contains("Error")),
        );
}

#[test]
fn decode_missing_format_flag_exits_nonzero() {
    let dir = setup_simple();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        // Intentionally omit --format
        .assert()
        .failure();
}

#[test]
fn decode_nonexistent_input_exits_nonzero() {
    let dir = setup_simple();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg("nonexistent_input_99999.bin")
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .failure();
}

#[test]
fn decode_truncated_data_with_fail_fast_exits_nonzero() {
    let dir = setup_simple();
    // Write only 5 bytes; record requires 13
    std::fs::write(dir.path().join("data.bin"), [0xF0; 5]).unwrap();
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .assert()
        .failure();
}

#[test]
fn encode_bad_jsonl_exits_nonzero() {
    let dir = setup_simple();
    let bad_jsonl = p(&dir, "bad.jsonl");
    std::fs::write(&bad_jsonl, "THIS IS NOT JSON\n").unwrap();
    let out = p(&dir, "encoded.bin");

    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&bad_jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .failure();
}

#[test]
fn encode_nonexistent_schema_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    let jsonl = p(&dir, "input.jsonl");
    std::fs::write(&jsonl, r#"{"NAME":"X"}"#).unwrap();
    let out = p(&dir, "encoded.bin");

    cmd()
        .args(["encode"])
        .arg("nonexistent_schema_99999.cpy")
        .arg(&jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .failure();
}

// =========================================================================
// 19. EXIT CODES: verify expected exit codes
// =========================================================================

#[test]
fn unknown_subcommand_exits_nonzero() {
    cmd().arg("not-a-real-command").assert().failure();
}

#[test]
fn parse_missing_args_exits_nonzero() {
    cmd().arg("parse").assert().failure();
}

#[test]
fn decode_missing_args_exits_nonzero() {
    cmd().arg("decode").assert().failure();
}

#[test]
fn encode_missing_args_exits_nonzero() {
    cmd().arg("encode").assert().failure();
}

#[test]
fn verify_missing_args_exits_nonzero() {
    cmd().arg("verify").assert().failure();
}

#[test]
fn determinism_missing_args_exits_nonzero() {
    cmd().args(["determinism", "decode"]).assert().failure();
}

#[test]
fn exit_codes_are_in_valid_range() {
    let scenarios: &[&[&str]] = &[&["--help"], &["--version"], &["not-a-real-command"]];

    for args in scenarios {
        let output = cmd().args(args.iter()).output().expect("should run");
        let code = output.status.code().expect("exit code");
        assert!(
            (0..=5).contains(&code),
            "args {args:?} exited with {code}, expected 0..=5"
        );
    }
}

// =========================================================================
// 20. Error paths: no panic on any error
// =========================================================================

#[test]
fn error_paths_never_panic() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), b"NOT COBOL").unwrap();

    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "bad.cpy"))
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("panicked at"),
        "CLI should not panic on bad input: {stderr}"
    );
    assert!(
        !stderr.contains("stack backtrace"),
        "CLI should not emit backtraces: {stderr}"
    );
}

// =========================================================================
// 21. Full pipeline: decode → encode → verify round-trip
// =========================================================================

#[test]
fn full_pipeline_decode_encode_verify() {
    let dir = setup_two_records();

    // Decode
    let jsonl = p(&dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&jsonl)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    // Encode
    let encoded = p(&dir, "encoded.bin");
    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .arg("--output")
        .arg(&encoded)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    // Verify
    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&encoded)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    // Binary fidelity
    let original = std::fs::read(p(&dir, "data.bin")).unwrap();
    let re_encoded = std::fs::read(&encoded).unwrap();
    assert_eq!(
        original, re_encoded,
        "round-trip must produce identical bytes"
    );
}

// =========================================================================
// 22. Parse to output file
// =========================================================================

#[test]
fn parse_to_output_file() {
    let dir = setup_simple();
    let out = p(&dir, "schema.json");

    cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .arg("-o")
        .arg(&out)
        .assert()
        .success();

    let contents = std::fs::read_to_string(&out).unwrap();
    let _json: serde_json::Value =
        serde_json::from_str(&contents).expect("output file must be valid JSON");
}

// =========================================================================
// 23. Decode to stdout
// =========================================================================

#[test]
fn decode_to_stdout() {
    let dir = setup_simple();

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--output", "-", "--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success()
        .stdout(predicate::str::contains("NAME"));
}

// =========================================================================
// 24. Encode to stdout
// =========================================================================

#[test]
fn encode_to_stdout() {
    let dir = setup_simple();
    let jsonl = decode_to_jsonl(&dir);

    let assert = cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .args(["--output", "-", "--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    assert_eq!(
        assert.get_output().stdout.len(),
        13,
        "stdout should contain 13 binary bytes"
    );
}

// =========================================================================
// 25. Inspect with dialect flag
// =========================================================================

#[test]
fn inspect_with_dialect_flag() {
    let dir = setup_simple();

    for dialect in &["n", "0", "1"] {
        cmd()
            .args(["inspect"])
            .arg(p(&dir, "schema.cpy"))
            .args(["--dialect", dialect])
            .assert()
            .success();
    }
}
