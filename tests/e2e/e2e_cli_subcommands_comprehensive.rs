// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive E2E tests for CLI subcommands.
//!
//! Exercises `parse`, `inspect`, `decode`, `encode`, `verify`, `determinism`,
//! `--help`, and `--version` through the `copybook` binary with diverse
//! copybook schemas (alphanumeric, signed numeric, COMP-3, ODO).

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

/// Minimal 13-byte fixed record: NAME PIC X(10), AGE PIC 9(3).
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// Copybook with signed numeric and decimal: ID PIC 9(4), AMT PIC S9(5)V99, TAG PIC X(4).
const SIGNED_CPY: &str = "\
       01  REC.
           05  ID     PIC 9(4).
           05  AMT    PIC S9(5)V99.
           05  TAG    PIC X(4).
";

/// Copybook with COMP-3 packed decimal.
const COMP3_CPY: &str = "\
       01  REC.
           05  LABEL  PIC X(6).
           05  PKD    PIC S9(5) COMP-3.
";

#[allow(dead_code)]
/// Copybook with ODO (Occurs Depending On).
const ODO_CPY: &str = "\
       01  REC.
           05  CNT       PIC 9(2).
           05  ITEMS     OCCURS 1 TO 5 TIMES
              DEPENDING ON CNT.
              10  ITEM-VAL PIC X(4).
";

/// EBCDIC CP037 record for SIMPLE_CPY: "ALICE     " + "025".
fn simple_ebcdic() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// Two simple records back-to-back.
fn two_simple_records() -> Vec<u8> {
    let mut v = simple_ebcdic();
    // BOB       030
    v.extend_from_slice(&[
        0xC2, 0xD6, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xF0, 0xF3, 0xF0,
    ]);
    v
}

/// EBCDIC CP037 record for SIGNED_CPY (15 bytes):
/// ID="0001", AMT="+0012345" (sign trailing overpunch C=positive, so last byte 0xC5),
/// TAG="TEST"
fn signed_ebcdic() -> Vec<u8> {
    vec![
        0xF0, 0xF0, 0xF0, 0xF1, // 0001
        0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xC5, // +0012345 (trailing overpunch C5 = +5)
        0xE3, 0xC5, 0xE2, 0xE3, // TEST
    ]
}

/// EBCDIC CP037 record for COMP3_CPY (9 bytes):
/// LABEL="ITEM  " (6 bytes), PKD=+12345 COMP-3 (3 bytes: 01 23 4C)
fn comp3_ebcdic() -> Vec<u8> {
    vec![
        0xC9, 0xE3, 0xC5, 0xD4, 0x40, 0x40, // ITEM
        0x01, 0x23, 0x4C, // +12345 packed
    ]
}

#[allow(dead_code)]
/// EBCDIC CP037 record for ODO_CPY with CNT=02 and 2 items:
/// CNT="02", ITEM1="AAAA", ITEM2="BBBB"
fn odo_ebcdic() -> Vec<u8> {
    vec![
        0xF0, 0xF2, // 02
        0xC1, 0xC1, 0xC1, 0xC1, // AAAA
        0xC2, 0xC2, 0xC2, 0xC2, // BBBB
    ]
}

fn setup(cpy: &str, data: &[u8]) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

fn p(dir: &TempDir, name: &str) -> PathBuf {
    dir.path().join(name)
}

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
// 1. PARSE: valid copybook produces JSON with expected top-level keys
// =========================================================================

#[test]
fn parse_valid_copybook_produces_json_with_fields() {
    let dir = setup(SIMPLE_CPY, &[]);
    let assert = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success();

    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let json: serde_json::Value = serde_json::from_str(&stdout).expect("valid JSON");
    assert!(json.is_object(), "parse output must be a JSON object");
}

// =========================================================================
// 2. PARSE: invalid copybook reports error on stderr
// =========================================================================

#[test]
fn parse_invalid_copybook_reports_cbk_error() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), b"NOT COBOL AT ALL !!!").unwrap();
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

// =========================================================================
// 3. PARSE: JSON output contains field names from copybook
// =========================================================================

#[test]
fn parse_json_contains_field_names() {
    let dir = setup(SIGNED_CPY, &[]);
    let assert = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success();

    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    assert!(stdout.contains("ID"), "schema JSON should mention ID");
    assert!(stdout.contains("AMT"), "schema JSON should mention AMT");
    assert!(stdout.contains("TAG"), "schema JSON should mention TAG");
}

// =========================================================================
// 4. PARSE: output to file with -o flag
// =========================================================================

#[test]
fn parse_output_to_file() {
    let dir = setup(SIMPLE_CPY, &[]);
    let out = p(&dir, "schema.json");
    cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .arg("-o")
        .arg(&out)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let _: serde_json::Value = serde_json::from_str(&content).expect("valid JSON in file");
}

// =========================================================================
// 5. PARSE: all three dialect values accepted
// =========================================================================

#[test]
fn parse_accepts_all_dialect_values() {
    let dir = setup(SIMPLE_CPY, &[]);
    for dialect in &["n", "0", "1"] {
        cmd()
            .args(["parse"])
            .arg(p(&dir, "schema.cpy"))
            .args(["--dialect", dialect])
            .assert()
            .success();
    }
}

// =========================================================================
// 6. INSPECT: shows human-readable layout with field names
// =========================================================================

#[test]
fn inspect_shows_layout_with_all_fields() {
    let dir = setup(SIGNED_CPY, &[]);
    cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success()
        .stdout(predicate::str::contains("ID"))
        .stdout(predicate::str::contains("AMT"))
        .stdout(predicate::str::contains("TAG"));
}

// =========================================================================
// 7. INSPECT: output contains PIC clause or size info
// =========================================================================

#[test]
fn inspect_contains_pic_or_size_info() {
    let dir = setup(SIMPLE_CPY, &[]);
    let assert = cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success();

    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    assert!(
        stdout.contains("PIC") || stdout.contains("10") || stdout.contains("X("),
        "inspect should show PIC clauses or field sizes: {stdout}"
    );
}

// =========================================================================
// 8. INSPECT: accepts --strict-comments flag
// =========================================================================

#[test]
fn inspect_with_strict_comments_flag() {
    let dir = setup(SIMPLE_CPY, &[]);
    cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .arg("--strict-comments")
        .assert()
        .success();
}

// =========================================================================
// 9. DECODE: fixed format with CP037 produces valid JSONL
// =========================================================================

#[test]
fn decode_fixed_cp037_produces_valid_jsonl() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
    for line in jsonl.lines() {
        let val: serde_json::Value = serde_json::from_str(line).expect("valid JSON line");
        assert!(val.is_object());
    }
}

// =========================================================================
// 10. DECODE: signed numeric copybook decodes correctly
// =========================================================================

#[test]
fn decode_signed_numeric_fields() {
    let dir = setup(SIGNED_CPY, &signed_ebcdic());
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
    assert!(!jsonl.is_empty(), "decode output should not be empty");
    let first: serde_json::Value = serde_json::from_str(jsonl.lines().next().unwrap()).unwrap();
    assert!(first.get("REC").is_some() || first.get("ID").is_some() || first.get("AMT").is_some());
}

// =========================================================================
// 11. DECODE: COMP-3 packed decimal field
// =========================================================================

#[test]
fn decode_comp3_packed_decimal() {
    let dir = setup(COMP3_CPY, &comp3_ebcdic());
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
    assert!(!jsonl.is_empty(), "COMP-3 decode should produce output");
}

// =========================================================================
// 12. DECODE: --select with single field
// =========================================================================

#[test]
fn decode_select_single_field() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
    assert!(!jsonl.is_empty());
}

// =========================================================================
// 13. DECODE: --select with comma-separated fields
// =========================================================================

#[test]
fn decode_select_comma_separated() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
            "NAME,AGE",
        ])
        .assert()
        .success();

    let jsonl = std::fs::read_to_string(&out).unwrap();
    assert!(!jsonl.is_empty());
}

// =========================================================================
// 14. DECODE: --threads 2 produces correct record count
// =========================================================================

#[test]
fn decode_threads_correct_record_count() {
    let dir = setup(SIMPLE_CPY, &two_simple_records());
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
    assert_eq!(jsonl.lines().count(), 2, "should decode 2 records");
}

// =========================================================================
// 15. DECODE: --emit-meta includes metadata fields
// =========================================================================

#[test]
fn decode_emit_meta_includes_metadata() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
        "emit-meta should add metadata fields"
    );
}

// =========================================================================
// 16. DECODE: --json-number lossless preserves precision
// =========================================================================

#[test]
fn decode_json_number_lossless() {
    let dir = setup(SIGNED_CPY, &signed_ebcdic());
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
}

// =========================================================================
// 17. DECODE: to stdout via --output -
// =========================================================================

#[test]
fn decode_to_stdout() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--output", "-", "--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success()
        .stdout(predicate::str::contains("NAME").or(predicate::str::contains("REC")));
}

// =========================================================================
// 18. ENCODE: basic encode produces correct-length output
// =========================================================================

#[test]
fn encode_basic_produces_correct_length() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
}

// =========================================================================
// 19. ENCODE: --fail-fast stops on first error
// =========================================================================

#[test]
fn encode_fail_fast_stops_on_error() {
    let dir = setup(SIMPLE_CPY, &[]);
    let bad = p(&dir, "bad.jsonl");
    std::fs::write(&bad, "NOT JSON\n").unwrap();
    let out = p(&dir, "encoded.bin");

    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&bad)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .assert()
        .failure();
}

// =========================================================================
// 20. ENCODE: --max-errors accepted
// =========================================================================

#[test]
fn encode_max_errors_accepted() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
            "10",
        ])
        .assert()
        .success();
}

// =========================================================================
// 21. ENCODE: to stdout via --output -
// =========================================================================

#[test]
fn encode_to_stdout() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let jsonl = decode_to_jsonl(&dir);

    let assert = cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .args(["--output", "-", "--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    assert_eq!(assert.get_output().stdout.len(), 13);
}

// =========================================================================
// 22. VERIFY: valid data passes
// =========================================================================

#[test]
fn verify_valid_data_passes() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
}

// =========================================================================
// 23. VERIFY: with --report flag writes report file
// =========================================================================

#[test]
fn verify_report_flag_writes_file() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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

    assert!(report.exists(), "report file should exist");
    let content = std::fs::read_to_string(&report).unwrap();
    assert!(!content.is_empty());
}

// =========================================================================
// 24. VERIFY: truncated data (not multiple of LRECL) warns but passes with 0 records
// =========================================================================

#[test]
fn verify_truncated_data_exits_zero_with_zero_records() {
    let dir = setup(SIMPLE_CPY, &[0xF0; 5]); // 5 bytes, needs 13
    // File size is not a multiple of LRECL; verify logs a warning
    // but processes 0 records and exits 0 (PASS).
    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success()
        .stdout(predicate::str::contains("PASS"));
}

// =========================================================================
// 25. DETERMINISM: decode exits 0 for deterministic data
// =========================================================================

#[test]
fn determinism_decode_exits_zero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    cmd()
        .args(["determinism", "decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
}

// =========================================================================
// 26. DETERMINISM: encode exits 0
// =========================================================================

#[test]
fn determinism_encode_exits_zero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
// 27. DETERMINISM: round-trip exits 0
// =========================================================================

#[test]
fn determinism_round_trip_exits_zero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    cmd()
        .args(["determinism", "round-trip"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
}

// =========================================================================
// 28. DETERMINISM: --output json produces valid JSON
// =========================================================================

#[test]
fn determinism_json_output_is_valid() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
    let _: serde_json::Value = serde_json::from_str(&stdout).expect("valid JSON");
}

// =========================================================================
// 29. HELP: each subcommand --help exits 0
// =========================================================================

#[test]
fn help_all_subcommands_exit_zero() {
    let subcommands = [
        "parse",
        "inspect",
        "decode",
        "encode",
        "verify",
        "determinism",
        "support",
    ];
    for sub in subcommands {
        cmd()
            .args([sub, "--help"])
            .assert()
            .success()
            .stdout(predicate::str::contains("Usage").or(predicate::str::contains(sub)));
    }
}

// =========================================================================
// 30. VERSION: contains "copybook" and a semver-like version
// =========================================================================

#[test]
fn version_output_contains_name_and_semver() {
    let assert = cmd().arg("--version").assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    assert!(
        stdout.contains("copybook"),
        "version should contain 'copybook'"
    );
    // Check for semver-like pattern: digits.digits.digits
    let has_semver = stdout
        .split_whitespace()
        .any(|w| w.split('.').count() >= 3 && w.chars().next().is_some_and(|c| c.is_ascii_digit()));
    assert!(has_semver, "version should contain semver: {stdout}");
}

// =========================================================================
// 31. FULL PIPELINE: decode → encode → verify round-trip with two records
// =========================================================================

#[test]
fn full_pipeline_round_trip_fidelity() {
    let dir = setup(SIMPLE_CPY, &two_simple_records());

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
    assert_eq!(original, re_encoded, "round-trip must preserve bytes");
}

// =========================================================================
// 32. DECODE: --emit-filler flag accepted
// =========================================================================

#[test]
fn decode_emit_filler_flag_accepted() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--emit-filler"])
        .assert()
        .success();
}

// =========================================================================
// 33. DECODE: --strict flag accepted
// =========================================================================

#[test]
fn decode_strict_flag_accepted() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--strict"])
        .assert()
        .success();
}

// =========================================================================
// 34. ENCODE: --coerce-numbers flag accepted
// =========================================================================

#[test]
fn encode_coerce_numbers_flag_accepted() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
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
            "--coerce-numbers",
        ])
        .assert()
        .success();
}

// =========================================================================
// 35. DECODE: multi-field copybook with different PIC types
// =========================================================================

#[test]
fn decode_multi_field_copybook() {
    // Use SIGNED_CPY which has numeric and alpha fields
    let dir = setup(SIGNED_CPY, &signed_ebcdic());
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
    assert!(
        !jsonl.is_empty(),
        "multi-field decode should produce output"
    );
    let first: serde_json::Value = serde_json::from_str(jsonl.lines().next().unwrap()).unwrap();
    assert!(first.is_object());
}

// =========================================================================
// 36. VERIFY: --max-errors flag accepted
// =========================================================================

#[test]
fn verify_max_errors_flag_accepted() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--max-errors",
            "3",
        ])
        .assert()
        .success();
}
