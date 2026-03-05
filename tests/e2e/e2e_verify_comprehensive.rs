// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive E2E tests for the `verify` CLI subcommand.
//!
//! Tests cover: report JSON structure, `--max-errors`, `--sample`, `--strict`,
//! multi-record validation, corrupt data detection, dialect interaction,
//! field projection, codepage variants, and exit code contracts.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use serde_json::Value;
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

/// EBCDIC CP037 record matching `SIMPLE_CPY` (13 bytes).
/// NAME = "ALICE     " AGE = "025"
fn simple_record() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// Create N copies of a record for multi-record tests.
fn repeat_record(base: &[u8], n: usize) -> Vec<u8> {
    base.repeat(n)
}

/// Prepare a temp dir with a copybook and data file.
fn setup(cpy: &str, data: &[u8]) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

fn cpy_path(dir: &TempDir) -> PathBuf {
    dir.path().join("schema.cpy")
}

fn data_path(dir: &TempDir) -> PathBuf {
    dir.path().join("data.bin")
}

fn report_path(dir: &TempDir) -> PathBuf {
    dir.path().join("report.json")
}

// ---------------------------------------------------------------------------
// Report JSON structure tests
// ---------------------------------------------------------------------------

#[test]
fn verify_report_has_required_fields() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();

    // Verify all top-level fields exist
    assert!(
        report.get("report_version").is_some(),
        "missing report_version"
    );
    assert!(
        report.get("schema_fingerprint").is_some(),
        "missing schema_fingerprint"
    );
    assert!(
        report.get("record_format").is_some(),
        "missing record_format"
    );
    assert!(report.get("file").is_some(), "missing file");
    assert!(
        report.get("file_size_bytes").is_some(),
        "missing file_size_bytes"
    );
    assert!(report.get("cli_opts").is_some(), "missing cli_opts");
    assert!(
        report.get("records_total").is_some(),
        "missing records_total"
    );
    assert!(report.get("errors_total").is_some(), "missing errors_total");
    assert!(report.get("truncated").is_some(), "missing truncated");
    assert!(report.get("errors").is_some(), "missing errors");
    assert!(report.get("sample").is_some(), "missing sample");
}

#[test]
fn verify_report_version_is_one() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert_eq!(report["report_version"], 1, "report version must be 1");
}

#[test]
fn verify_report_valid_data_counts() {
    let data = repeat_record(&simple_record(), 5);
    let dir = setup(SIMPLE_CPY, &data);
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert_eq!(report["records_total"], 5);
    assert_eq!(report["errors_total"], 0);
    assert_eq!(report["truncated"], false);
    assert!(report["errors"].as_array().unwrap().is_empty());
}

#[test]
fn verify_report_file_size_matches() {
    let data = simple_record();
    let expected_size = data.len() as u64;
    let dir = setup(SIMPLE_CPY, &data);
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert_eq!(report["file_size_bytes"], expected_size);
}

#[test]
fn verify_report_record_format_lowercase() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert_eq!(report["record_format"], "fixed");
}

#[test]
fn verify_report_cli_opts_echo() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--max-errors",
            "20",
            "--sample",
            "3",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();

    let cli = &report["cli_opts"];
    assert_eq!(cli["max_errors"], 20);
    assert_eq!(cli["sample"], 3);
    assert_eq!(cli["strict"], false);
    assert_eq!(cli["strict_comments"], false);
}

#[test]
fn verify_report_schema_fingerprint_is_md5_hex() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    let fp = report["schema_fingerprint"].as_str().unwrap();
    // MD5 hex is 32 hex characters
    assert_eq!(fp.len(), 32, "fingerprint should be 32-char MD5 hex");
    assert!(
        fp.chars().all(|c| c.is_ascii_hexdigit()),
        "fingerprint should only contain hex chars"
    );
}

// ---------------------------------------------------------------------------
// Exit code tests
// ---------------------------------------------------------------------------

#[test]
fn verify_valid_data_exits_zero() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success()
        .stdout(predicate::str::contains("PASS"));
}

#[test]
fn verify_empty_data_exits_zero() {
    let dir = setup(SIMPLE_CPY, &[]);
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success()
        .stdout(predicate::str::contains("Records Total: 0"));
}

#[test]
fn verify_nonexistent_file_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(cpy_path(&dir))
        .arg(dir.path().join("nonexistent.bin"))
        .assert()
        .failure();
}

#[test]
fn verify_nonexistent_copybook_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(dir.path().join("nonexistent.cpy"))
        .arg(data_path(&dir))
        .assert()
        .failure();
}

// ---------------------------------------------------------------------------
// Corrupt data detection
// ---------------------------------------------------------------------------

/// COMP-3 copybook with a packed decimal field.
const COMP3_CPY: &str = "\
       01  REC.
           05  ID     PIC 9(4).
           05  AMT    PIC 9(5)V99 COMP-3.
           05  LABEL  PIC X(3).
";

#[test]
fn verify_comp3_invalid_nibble_detected() {
    // COMP-3 PIC 9(5)V99 = 4 bytes packed. Use invalid nibbles (0xFF).
    // Record: ID(4) + AMT(4 COMP-3) + LABEL(3) = 11 bytes
    let record = vec![
        0xF0, 0xF0, 0xF0, 0xF1, // ID = "0001"
        0xFF, 0xFF, 0xFF, 0xFF, // AMT = invalid COMP-3
        0xC1, 0xC2, 0xC3, // LABEL = "ABC"
    ];
    // Ensure the invalid data is detected
    let dir = setup(COMP3_CPY, &record);
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3); // ExitCode::Encode = 3

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert!(report["errors_total"].as_u64().unwrap() > 0);
    assert!(!report["errors"].as_array().unwrap().is_empty());
}

#[test]
fn verify_truncated_record_detected() {
    // Send 10 bytes when LRECL is 13
    let dir = setup(SIMPLE_CPY, &simple_record()[..10]);

    // The CLI should warn about non-LRECL-aligned file but still exit
    // The verify should detect the truncation
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .stdout(predicate::str::contains("Records Total: 0"));
}

// ---------------------------------------------------------------------------
// `--max-errors` behavior
// ---------------------------------------------------------------------------

#[test]
fn verify_max_errors_limits_reported_errors() {
    // Create data with many invalid COMP-3 records
    let bad_comp3_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, // ID = "0001"
        0xFF, 0xFF, 0xFF, 0xFF, // AMT = invalid COMP-3
        0xC1, 0xC2, 0xC3, // LABEL = "ABC"
    ];
    let data = repeat_record(&bad_comp3_record, 20);
    let dir = setup(COMP3_CPY, &data);

    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--max-errors",
            "5",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3);

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();

    // errors array should have at most 5 entries
    let errors = report["errors"].as_array().unwrap();
    assert!(
        errors.len() <= 5,
        "errors array should be capped at max_errors=5"
    );

    // but errors_total should count all of them
    assert!(
        report["errors_total"].as_u64().unwrap() >= 5,
        "errors_total should count beyond max_errors"
    );

    // truncated should be true
    assert_eq!(report["truncated"], true, "truncated flag should be set");
}

#[test]
fn verify_max_errors_one() {
    let bad_comp3_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, 0xFF, 0xFF, 0xFF, 0xFF, 0xC1, 0xC2, 0xC3,
    ];
    let data = repeat_record(&bad_comp3_record, 10);
    let dir = setup(COMP3_CPY, &data);

    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--max-errors",
            "1",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3);

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    let errors = report["errors"].as_array().unwrap();
    assert_eq!(
        errors.len(),
        1,
        "only 1 error should be in array with max-errors=1"
    );
    assert_eq!(report["truncated"], true);
}

// ---------------------------------------------------------------------------
// `--sample` behavior
// ---------------------------------------------------------------------------

#[test]
fn verify_sample_limits_sample_records() {
    let bad_comp3_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, 0xFF, 0xFF, 0xFF, 0xFF, 0xC1, 0xC2, 0xC3,
    ];
    let data = repeat_record(&bad_comp3_record, 10);
    let dir = setup(COMP3_CPY, &data);

    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--sample",
            "2",
            "--max-errors",
            "100",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3);

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    let samples = report["sample"].as_array().unwrap();
    assert!(
        samples.len() <= 2,
        "sample array should be limited to --sample=2, got {}",
        samples.len()
    );
}

// ---------------------------------------------------------------------------
// Error entry structure
// ---------------------------------------------------------------------------

#[test]
fn verify_error_entries_have_required_fields() {
    let bad_comp3_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, 0xFF, 0xFF, 0xFF, 0xFF, 0xC1, 0xC2, 0xC3,
    ];
    let dir = setup(COMP3_CPY, &bad_comp3_record);

    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3);

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    let errors = report["errors"].as_array().unwrap();
    assert!(!errors.is_empty(), "should have errors");

    let err = &errors[0];
    assert!(err.get("index").is_some(), "error should have index");
    assert!(err.get("code").is_some(), "error should have code");
    assert!(err.get("msg").is_some(), "error should have msg");
    // field and hex may be null but should exist
    assert!(
        err.get("field").is_some(),
        "error should have field (even if null)"
    );
    assert!(
        err.get("hex").is_some(),
        "error should have hex (even if null)"
    );
}

#[test]
fn verify_error_code_is_cbk_prefixed() {
    let bad_comp3_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, 0xFF, 0xFF, 0xFF, 0xFF, 0xC1, 0xC2, 0xC3,
    ];
    let dir = setup(COMP3_CPY, &bad_comp3_record);

    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3);

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    let errors = report["errors"].as_array().unwrap();
    for err in errors {
        let code = err["code"].as_str().unwrap();
        assert!(
            code.starts_with("Cbk") || code.starts_with("CBK"),
            "error code should start with CBK prefix, got: {code}"
        );
    }
}

// ---------------------------------------------------------------------------
// Multi-record mixed valid/invalid
// ---------------------------------------------------------------------------

#[test]
fn verify_mixed_records_counts_errors_correctly() {
    // 3 valid records + 2 invalid COMP-3 records
    let good_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, // ID = "0001"
        0x12, 0x34, 0x56, 0x7F, // AMT = valid unsigned COMP-3 12345.67
        0xC1, 0xC2, 0xC3, // LABEL = "ABC"
    ];
    let bad_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF2, // ID = "0002"
        0xFF, 0xFF, 0xFF, 0xFF, // AMT = invalid COMP-3
        0xC1, 0xC2, 0xC3, // LABEL = "ABC"
    ];
    let mut data = Vec::new();
    data.extend_from_slice(&good_record);
    data.extend_from_slice(&good_record);
    data.extend_from_slice(&bad_record);
    data.extend_from_slice(&good_record);
    data.extend_from_slice(&bad_record);

    let dir = setup(COMP3_CPY, &data);
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3);

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert_eq!(report["records_total"], 5, "should process all 5 records");
    assert_eq!(report["errors_total"], 2, "should detect 2 errors");
}

// ---------------------------------------------------------------------------
// Stdout summary output
// ---------------------------------------------------------------------------

#[test]
fn verify_stdout_summary_contains_pass() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success()
        .stdout(predicate::str::contains("Verification Summary"))
        .stdout(predicate::str::contains("PASS"));
}

#[test]
fn verify_stdout_summary_shows_record_count() {
    let data = repeat_record(&simple_record(), 3);
    let dir = setup(SIMPLE_CPY, &data);
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success()
        .stdout(predicate::str::contains("Records Total: 3"));
}

#[test]
fn verify_stdout_summary_shows_errors() {
    let bad_comp3_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, 0xFF, 0xFF, 0xFF, 0xFF, 0xC1, 0xC2, 0xC3,
    ];
    let dir = setup(COMP3_CPY, &bad_comp3_record);
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp037"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3)
        .stdout(predicate::str::contains("Errors:"));
}

// ---------------------------------------------------------------------------
// Codepage variants
// ---------------------------------------------------------------------------

#[test]
fn verify_codepage_cp1047() {
    // CP1047 has the same uppercase letters A-Z at the same positions as CP037
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp1047"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn verify_codepage_cp500() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args(["verify", "--format", "fixed", "--codepage", "cp500"])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// Dialect interaction
// ---------------------------------------------------------------------------

/// ODO copybook for dialect tests.
const ODO_CPY: &str = "\
       01  REC.
           05  CNT    PIC 9(3).
           05  ITEMS  OCCURS 1 TO 5 DEPENDING ON CNT
                      PIC X(4).
";

#[test]
fn verify_with_dialect_normative() {
    // CNT=002 with 2 ITEMS (valid for normative with min=1)
    // Record: CNT(3) + ITEMS(2*4=8) = 11 bytes, but LRECL for ODO = max = 3 + 5*4 = 23 bytes
    let mut record = vec![0xF0, 0xF0, 0xF2]; // CNT = "002"
    // 5 ITEMS slots (LRECL = max): fill with EBCDIC data
    for _ in 0..5 {
        record.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // "ABCD"
    }
    assert_eq!(record.len(), 23);

    let dir = setup(ODO_CPY, &record);
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "n",
        ])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn verify_with_dialect_zero_tolerant() {
    // Same record but using dialect 0
    let mut record = vec![0xF0, 0xF0, 0xF2]; // CNT = "002"
    for _ in 0..5 {
        record.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]);
    }

    let dir = setup(ODO_CPY, &record);
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "0",
        ])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// `--strict` mode
// ---------------------------------------------------------------------------

#[test]
fn verify_strict_flag_accepted() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--strict",
        ])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn verify_strict_reflected_in_report() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--strict",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert_eq!(report["cli_opts"]["strict"], true);
}

// ---------------------------------------------------------------------------
// `--strict-comments` mode
// ---------------------------------------------------------------------------

#[test]
fn verify_strict_comments_flag_accepted() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--strict-comments",
        ])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn verify_strict_comments_reflected_in_report() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--strict-comments",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    assert_eq!(report["cli_opts"]["strict_comments"], true);
}

// ---------------------------------------------------------------------------
// `--select` (field projection)
// ---------------------------------------------------------------------------

#[test]
fn verify_with_select_valid_field() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "NAME",
        ])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .success();
}

#[test]
fn verify_with_select_nonexistent_field() {
    let dir = setup(SIMPLE_CPY, &simple_record());
    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "NONEXISTENT",
        ])
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .failure();
}

// ---------------------------------------------------------------------------
// Sample entry structure
// ---------------------------------------------------------------------------

#[test]
fn verify_sample_entries_have_hex_and_index() {
    let bad_comp3_record: Vec<u8> = vec![
        0xF0, 0xF0, 0xF0, 0xF1, 0xFF, 0xFF, 0xFF, 0xFF, 0xC1, 0xC2, 0xC3,
    ];
    let dir = setup(COMP3_CPY, &bad_comp3_record);

    cmd()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--sample",
            "5",
            "--report",
        ])
        .arg(report_path(&dir))
        .arg(cpy_path(&dir))
        .arg(data_path(&dir))
        .assert()
        .code(3);

    let report: Value =
        serde_json::from_str(&std::fs::read_to_string(report_path(&dir)).unwrap()).unwrap();
    let samples = report["sample"].as_array().unwrap();
    if !samples.is_empty() {
        let s = &samples[0];
        assert!(s.get("index").is_some(), "sample should have index");
        assert!(s.get("hex").is_some(), "sample should have hex");
        let hex = s["hex"].as_str().unwrap();
        // Hex should be uppercase hex characters
        assert!(
            hex.chars().all(|c| c.is_ascii_hexdigit()),
            "hex should be hex chars"
        );
    }
}

// ---------------------------------------------------------------------------
// Schema fingerprint determinism
// ---------------------------------------------------------------------------

#[test]
fn verify_schema_fingerprint_is_deterministic() {
    let dir = setup(SIMPLE_CPY, &simple_record());

    // Run verify twice
    for i in 0..2 {
        let rp = dir.path().join(format!("report_{i}.json"));
        cmd()
            .args([
                "verify",
                "--format",
                "fixed",
                "--codepage",
                "cp037",
                "--report",
            ])
            .arg(&rp)
            .arg(cpy_path(&dir))
            .arg(data_path(&dir))
            .assert()
            .success();
    }

    let r1: Value =
        serde_json::from_str(&std::fs::read_to_string(dir.path().join("report_0.json")).unwrap())
            .unwrap();
    let r2: Value =
        serde_json::from_str(&std::fs::read_to_string(dir.path().join("report_1.json")).unwrap())
            .unwrap();

    assert_eq!(
        r1["schema_fingerprint"], r2["schema_fingerprint"],
        "schema fingerprint must be deterministic across runs"
    );
}
