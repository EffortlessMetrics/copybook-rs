// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI output snapshot tests — verify structural properties of every
//! subcommand's stdout/stderr without relying on `insta`.
//!
//! Each test writes a temporary copybook (and optionally a data file),
//! invokes the `copybook` binary, and asserts that the output contains
//! expected substrings and structural markers.

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

/// A minimal COBOL copybook (13-byte record).
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// A copybook with numeric + decimal fields (17-byte record).
const NUMERIC_CPY: &str = "\
       01  REC.
           05  ID     PIC 9(4).
           05  AMT    PIC 9(5)V99.
           05  LABEL  PIC X(6).
";

/// A copybook with a group and nested fields.
const GROUP_CPY: &str = "\
       01  CUSTOMER.
           05  CUST-ID    PIC 9(6).
           05  CUST-INFO.
               10  FIRST-NAME  PIC X(15).
               10  LAST-NAME   PIC X(15).
           05  BALANCE    PIC S9(7)V99.
";

/// EBCDIC CP037 record for SIMPLE_CPY: NAME="ALICE     " AGE="025"
fn simple_record() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// Two concatenated SIMPLE_CPY records.
fn two_records() -> Vec<u8> {
    let mut data = simple_record();
    // BOB       030
    data.extend_from_slice(&[
        0xC2, 0xD6, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xF0, 0xF3, 0xF0,
    ]);
    data
}

/// EBCDIC CP037 record for NUMERIC_CPY: ID="0001" AMT="0012345" LABEL="ITEM  "
fn numeric_record() -> Vec<u8> {
    vec![
        0xF0, 0xF0, 0xF0, 0xF1, // 0001
        0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, // 0012345
        0xC9, 0xE3, 0xC5, 0xD4, 0x40, 0x40, // ITEM
    ]
}

fn setup(cpy: &str) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    dir
}

fn setup_with_data(cpy: &str, data: &[u8]) -> TempDir {
    let dir = setup(cpy);
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

fn p(dir: &TempDir, name: &str) -> PathBuf {
    dir.path().join(name)
}

// =========================================================================
// 1. PARSE command — JSON output structure
// =========================================================================

#[test]
fn parse_output_contains_field_names() {
    let dir = setup(SIMPLE_CPY);
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
    let dir = setup(SIMPLE_CPY);
    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let _: serde_json::Value = serde_json::from_str(&stdout).expect("valid JSON");
}

#[test]
fn parse_output_contains_field_kind() {
    let dir = setup(SIMPLE_CPY);
    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Schema JSON should contain field kind info (Alphanum, Numeric, Group, etc.)
    assert!(
        stdout.contains("Alphanum") || stdout.contains("Numeric") || stdout.contains("kind"),
        "Parse output should contain field kind info, got: {stdout}"
    );
}

#[test]
fn parse_numeric_copybook_contains_decimal_info() {
    let dir = setup(NUMERIC_CPY);
    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("AMT"), "Should contain AMT field");
    assert!(stdout.contains("ID"), "Should contain ID field");
    assert!(stdout.contains("LABEL"), "Should contain LABEL field");
}

#[test]
fn parse_group_copybook_shows_nested_fields() {
    let dir = setup(GROUP_CPY);
    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("CUST-ID"), "Missing CUST-ID");
    assert!(stdout.contains("FIRST-NAME"), "Missing FIRST-NAME");
    assert!(stdout.contains("LAST-NAME"), "Missing LAST-NAME");
    assert!(stdout.contains("BALANCE"), "Missing BALANCE");
}

#[test]
fn parse_output_contains_record_path() {
    let dir = setup(SIMPLE_CPY);
    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // The schema JSON uses "path" field for fully qualified names
    assert!(
        stdout.contains("\"path\""),
        "Should contain path field in schema JSON"
    );
}

// =========================================================================
// 2. INSPECT command — human-readable layout
// =========================================================================

#[test]
fn inspect_output_lists_field_names() {
    let dir = setup(SIMPLE_CPY);
    cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .assert()
        .success()
        .stdout(predicate::str::contains("NAME"))
        .stdout(predicate::str::contains("AGE"));
}

#[test]
fn inspect_output_shows_numeric_field_info() {
    let dir = setup(NUMERIC_CPY);
    let output = cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .output()
        .unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("ID"), "Missing ID field");
    assert!(stdout.contains("AMT"), "Missing AMT field");
    assert!(stdout.contains("LABEL"), "Missing LABEL field");
}

#[test]
fn inspect_group_copybook_shows_hierarchy() {
    let dir = setup(GROUP_CPY);
    let output = cmd()
        .args(["inspect"])
        .arg(p(&dir, "schema.cpy"))
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "inspect failed: stderr={stderr}, stdout={stdout}"
    );
    // Inspect uses fully qualified paths like CUSTOMER.CUST-INFO.FIRST-NAME
    assert!(stdout.contains("CUST-ID"), "Missing CUST-ID in: {stdout}");
    assert!(
        stdout.contains("FIRST-NAME"),
        "Missing FIRST-NAME in: {stdout}"
    );
}

// =========================================================================
// 3. DECODE command — JSONL output format
// =========================================================================

#[test]
fn decode_single_record_contains_field_values() {
    let dir = setup_with_data(SIMPLE_CPY, &simple_record());
    let out = p(&dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
    let content = std::fs::read_to_string(&out).unwrap();
    assert!(content.contains("ALICE"), "Missing decoded NAME value");
    assert!(
        content.contains("25") || content.contains("025"),
        "Missing decoded AGE value"
    );
}

#[test]
fn decode_two_records_produces_two_jsonl_lines() {
    let dir = setup_with_data(SIMPLE_CPY, &two_records());
    let out = p(&dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
    let content = std::fs::read_to_string(&out).unwrap();
    let lines: Vec<&str> = content.lines().collect();
    assert!(
        lines.len() >= 2,
        "Expected at least 2 JSONL lines, got {}",
        lines.len()
    );
    assert!(
        lines[0].contains("ALICE"),
        "First line should contain ALICE"
    );
    assert!(lines[1].contains("BOB"), "Second line should contain BOB");
}

#[test]
fn decode_output_lines_are_valid_json() {
    let dir = setup_with_data(SIMPLE_CPY, &simple_record());
    let out = p(&dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
    let content = std::fs::read_to_string(&out).unwrap();
    for (i, line) in content.lines().enumerate() {
        let _: serde_json::Value =
            serde_json::from_str(line).unwrap_or_else(|e| panic!("Line {i} not valid JSON: {e}"));
    }
}

#[test]
fn decode_numeric_record_contains_expected_values() {
    let dir = setup_with_data(NUMERIC_CPY, &numeric_record());
    let out = p(&dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
    let content = std::fs::read_to_string(&out).unwrap();
    assert!(
        content.contains("ITEM") || content.contains("item"),
        "Missing LABEL value"
    );
}

#[test]
fn decode_jsonl_field_names_match_copybook() {
    let dir = setup_with_data(SIMPLE_CPY, &simple_record());
    let out = p(&dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();
    let content = std::fs::read_to_string(&out).unwrap();
    let first_line = content.lines().next().expect("at least one line");
    let json: serde_json::Value = serde_json::from_str(first_line).unwrap();
    let obj = json.as_object().expect("JSON object");
    // The top-level object should contain the field names from the copybook
    let all_keys: Vec<String> = obj
        .keys()
        .flat_map(|k| {
            let mut keys = vec![k.clone()];
            if let Some(inner) = obj[k].as_object() {
                keys.extend(inner.keys().cloned());
            }
            keys
        })
        .collect();
    let keys_str = all_keys.join(",");
    assert!(
        keys_str.contains("NAME"),
        "Missing NAME key, got: {keys_str}"
    );
    assert!(keys_str.contains("AGE"), "Missing AGE key, got: {keys_str}");
}

// =========================================================================
// 4. Error output format — verify error codes appear in stderr
// =========================================================================

#[test]
fn error_on_missing_file_shows_error() {
    cmd().args(["parse", "nonexistent.cpy"]).assert().failure();
}

#[test]
fn error_on_invalid_copybook_shows_message() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), "THIS IS NOT COBOL").unwrap();
    let output = cmd()
        .args(["parse"])
        .arg(dir.path().join("bad.cpy"))
        .output()
        .unwrap();
    // Should either fail or produce a diagnostic — not silently succeed with garbage
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{stderr}{stdout}");
    // Accept either an error message, error code, or empty schema
    assert!(
        !output.status.success()
            || combined.contains("error")
            || combined.contains("Error")
            || combined.contains("CBK")
            || combined.contains("fields"),
        "Invalid copybook should produce diagnostic output, got: stdout={stdout}, stderr={stderr}"
    );
}

#[test]
fn decode_with_wrong_lrecl_shows_error() {
    // Create a copybook expecting 13 bytes, but data file has 5 bytes (not divisible)
    let dir = setup_with_data(SIMPLE_CPY, &[0x40; 5]);
    let out = p(&dir, "out.jsonl");
    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{stderr}{stdout}");
    // Should indicate a truncation or record-length issue
    assert!(
        !output.status.success()
            || combined.contains("truncat")
            || combined.contains("CBK")
            || combined.contains("error")
            || combined.contains("Error")
            || combined.contains("record"),
        "Truncated data should produce diagnostic, got: {combined}"
    );
}

// =========================================================================
// 5. Help text — verify subcommands listed
// =========================================================================

#[test]
fn help_flag_shows_subcommands() {
    cmd()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("parse"))
        .stdout(predicate::str::contains("inspect"))
        .stdout(predicate::str::contains("decode"));
}

#[test]
fn help_lists_encode_subcommand() {
    cmd()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("encode"));
}

#[test]
fn parse_help_shows_usage() {
    cmd()
        .args(["parse", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage"));
}

#[test]
fn decode_help_shows_format_option() {
    cmd()
        .args(["decode", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("format"));
}

#[test]
fn encode_help_shows_codepage_option() {
    cmd()
        .args(["encode", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("codepage"));
}

// =========================================================================
// 6. Version output
// =========================================================================

#[test]
fn version_flag_shows_version() {
    cmd()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("copybook"));
}

#[test]
fn version_output_contains_semver_pattern() {
    let output = cmd().arg("--version").output().unwrap();
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should contain a version like X.Y.Z
    let has_version = regex::Regex::new(r"\d+\.\d+\.\d+")
        .unwrap()
        .is_match(&stdout);
    assert!(
        has_version,
        "Version output should contain semver: {stdout}"
    );
}

// =========================================================================
// 7. Output determinism — same input → same output across runs
// =========================================================================

#[test]
fn parse_output_deterministic_across_runs() {
    let dir = setup(SIMPLE_CPY);
    let path = p(&dir, "schema.cpy");

    let first = cmd().args(["parse"]).arg(&path).output().unwrap();
    assert!(first.status.success());

    for _ in 0..5 {
        let next = cmd().args(["parse"]).arg(&path).output().unwrap();
        assert!(next.status.success());
        assert_eq!(
            first.stdout, next.stdout,
            "parse output should be deterministic"
        );
    }
}

#[test]
fn inspect_output_deterministic_across_runs() {
    let dir = setup(SIMPLE_CPY);
    let path = p(&dir, "schema.cpy");

    let first = cmd().args(["inspect"]).arg(&path).output().unwrap();
    assert!(first.status.success());

    for _ in 0..5 {
        let next = cmd().args(["inspect"]).arg(&path).output().unwrap();
        assert!(next.status.success());
        assert_eq!(
            first.stdout, next.stdout,
            "inspect output should be deterministic"
        );
    }
}

#[test]
fn decode_output_deterministic_across_runs() {
    let dir = setup_with_data(SIMPLE_CPY, &two_records());

    let mut outputs = Vec::new();
    for i in 0..5 {
        let out = p(&dir, &format!("out_{i}.jsonl"));
        cmd()
            .args(["decode"])
            .arg(p(&dir, "schema.cpy"))
            .arg(p(&dir, "data.bin"))
            .arg("--output")
            .arg(&out)
            .args(["--format", "fixed", "--codepage", "cp037"])
            .assert()
            .success();
        outputs.push(std::fs::read_to_string(&out).unwrap());
    }

    let first = &outputs[0];
    for (i, output) in outputs.iter().enumerate().skip(1) {
        assert_eq!(first, output, "decode output diverged on run {i}");
    }
}
