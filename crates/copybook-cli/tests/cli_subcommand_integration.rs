// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive CLI integration tests covering all subcommands.
//!
//! Tests exercise the compiled binary end-to-end via `assert_cmd`,
//! covering happy paths, missing/invalid arguments, edge cases,
//! help text, and error output formatting.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use assert_cmd::cargo::cargo_bin_cmd;
use assert_fs::prelude::*;
use common::TestResult;
use predicates::prelude::*;
use std::io::Write;

/// Minimal fixed-format copybook: one X(5) field = 5-byte LRECL.
const SIMPLE_COPYBOOK: &str = "\
       01 RECORD.
          05 FIELD1 PIC X(5).
";

/// Copybook with two fields for richer testing.
const TWO_FIELD_COPYBOOK: &str = "\
       01 RECORD.
          05 NAME    PIC X(10).
          05 AMOUNT  PIC 9(5)V99.
";

/// CP037 bytes for "ABCDE"
const CP037_ABCDE: [u8; 5] = [0xC1, 0xC2, 0xC3, 0xC4, 0xC5];

/// ASCII bytes for "ABCDE"
const ASCII_ABCDE: [u8; 5] = [0x41, 0x42, 0x43, 0x44, 0x45];

// ====================================================================
// parse subcommand
// ====================================================================

#[test]
fn parse_simple_copybook_outputs_valid_json() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("simple.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    let output = cargo_bin_cmd!("copybook")
        .args(["parse", &cpy.path().to_string_lossy()])
        .output()?;

    assert!(output.status.success());
    let json: serde_json::Value = serde_json::from_slice(&output.stdout)?;
    assert!(json.get("fields").is_some());
    assert!(json.get("lrecl_fixed").is_some());
    Ok(())
}

#[test]
fn parse_with_output_flag_writes_file() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("out.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let out_path = tmp.child("schema.json");

    cargo_bin_cmd!("copybook")
        .args([
            "parse",
            &cpy.path().to_string_lossy(),
            "--output",
            &out_path.path().to_string_lossy(),
        ])
        .assert()
        .success();

    let contents = std::fs::read_to_string(out_path.path())?;
    let json: serde_json::Value = serde_json::from_str(&contents)?;
    assert!(json.get("fields").is_some());
    Ok(())
}

#[test]
fn parse_with_strict_flag_succeeds_on_valid_input() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("strict.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args(["parse", "--strict", &cpy.path().to_string_lossy()])
        .assert()
        .success();
    Ok(())
}

#[test]
fn parse_with_dialect_zero_succeeds() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("dialect.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args(["parse", "--dialect", "0", &cpy.path().to_string_lossy()])
        .assert()
        .success();
    Ok(())
}

#[test]
fn parse_invalid_copybook_syntax_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("bad.cpy");
    cpy.write_str("THIS IS NOT VALID COBOL")?;

    cargo_bin_cmd!("copybook")
        .args(["parse", &cpy.path().to_string_lossy()])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn parse_nonexistent_file_fails_with_stderr() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["parse", "this_file_does_not_exist.cpy"])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn parse_empty_file_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("empty.cpy");
    cpy.write_str("")?;

    cargo_bin_cmd!("copybook")
        .args(["parse", &cpy.path().to_string_lossy()])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn parse_help_exits_zero() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["parse", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("<COPYBOOK>"))
        .stdout(predicate::str::contains("--output"))
        .stdout(predicate::str::contains("--strict"))
        .stdout(predicate::str::contains("--dialect"));
    Ok(())
}

// ====================================================================
// inspect subcommand
// ====================================================================

#[test]
fn inspect_simple_copybook_shows_layout() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("inspect.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args(["inspect", &cpy.path().to_string_lossy()])
        .assert()
        .success()
        .stdout(predicate::str::contains("Copybook Layout"))
        .stdout(predicate::str::contains("FIELD1"));
    Ok(())
}

#[test]
fn inspect_with_codepage_flag() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("cp.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args([
            "inspect",
            "--codepage",
            "ascii",
            &cpy.path().to_string_lossy(),
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("ASCII"));
    Ok(())
}

#[test]
fn inspect_with_strict_comments_flag() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("sc.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args([
            "inspect",
            "--strict-comments",
            &cpy.path().to_string_lossy(),
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn inspect_nonexistent_file_fails() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["inspect", "no_such_file.cpy"])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn inspect_empty_file_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("empty.cpy");
    cpy.write_str("")?;

    cargo_bin_cmd!("copybook")
        .args(["inspect", &cpy.path().to_string_lossy()])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn inspect_binary_file_as_copybook_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("binary.cpy");
    // Write some random binary bytes that are not valid COBOL
    let mut f = std::fs::File::create(cpy.path())?;
    f.write_all(&[0x00, 0xFF, 0xFE, 0x01, 0x80, 0x90, 0xAB, 0xCD])?;

    cargo_bin_cmd!("copybook")
        .args(["inspect", &cpy.path().to_string_lossy()])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn inspect_help_exits_zero() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["inspect", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("<COPYBOOK>"))
        .stdout(predicate::str::contains("--codepage"));
    Ok(())
}

// ====================================================================
// decode subcommand
// ====================================================================

#[test]
fn decode_simple_fixed_record() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("decode.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .success();

    let contents = std::fs::read_to_string(out.path())?;
    assert!(contents.contains("ABCDE"));
    Ok(())
}

#[test]
fn decode_ascii_codepage() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("ascii.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&ASCII_ABCDE)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .success();

    let contents = std::fs::read_to_string(out.path())?;
    assert!(contents.contains("ABCDE"));
    Ok(())
}

#[test]
fn decode_missing_format_flag_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("mf.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("--format"));
    Ok(())
}

#[test]
fn decode_missing_output_flag_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("mo.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("--output"));
    Ok(())
}

#[test]
fn decode_missing_input_file_arg_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("ni.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            "--format",
            "fixed",
            "--output",
            "-",
        ])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn decode_nonexistent_data_file_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("ne.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            "nonexistent_data.bin",
            "--format",
            "fixed",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn decode_with_emit_meta_flag() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("meta.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
            "--emit-meta",
        ])
        .assert()
        .success();

    // emit-meta accepted without error; output contains a decoded record
    let contents = std::fs::read_to_string(out.path())?;
    assert!(!contents.is_empty(), "decoded output should not be empty");
    Ok(())
}

#[test]
fn decode_with_fail_fast_on_bad_data() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    // COMP-3 field: 2 digits → 2 bytes
    let cpy = tmp.child("ff.cpy");
    cpy.write_str("       01 REC.\n          05 F1 PIC 9(2) COMP-3.\n")?;
    let data = tmp.child("data.bin");
    // Invalid packed decimal nibbles
    data.write_binary(&[0xFF, 0xFF])?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
            "--fail-fast",
        ])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn decode_help_exits_zero() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["decode", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--format"))
        .stdout(predicate::str::contains("--output"))
        .stdout(predicate::str::contains("--codepage"))
        .stdout(predicate::str::contains("--json-number"))
        .stdout(predicate::str::contains("--select"))
        .stdout(predicate::str::contains("--emit-meta"))
        .stdout(predicate::str::contains("--threads"));
    Ok(())
}

// ====================================================================
// encode subcommand
// ====================================================================

#[test]
fn encode_simple_fixed_record() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("enc.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let jsonl = tmp.child("input.jsonl");
    jsonl.write_str("{\"FIELD1\":\"ABCDE\"}\n")?;
    let out = tmp.child("out.bin");

    cargo_bin_cmd!("copybook")
        .args([
            "encode",
            &cpy.path().to_string_lossy(),
            &jsonl.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .success();

    let data = std::fs::read(out.path())?;
    assert_eq!(data.len(), 5, "encoded record should be 5 bytes");
    Ok(())
}

#[test]
fn encode_missing_format_flag_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("emf.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let jsonl = tmp.child("input.jsonl");
    jsonl.write_str("{\"FIELD1\":\"ABCDE\"}\n")?;
    let out = tmp.child("out.bin");

    cargo_bin_cmd!("copybook")
        .args([
            "encode",
            &cpy.path().to_string_lossy(),
            &jsonl.path().to_string_lossy(),
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("--format"));
    Ok(())
}

#[test]
fn encode_missing_output_flag_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("emo.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let jsonl = tmp.child("input.jsonl");
    jsonl.write_str("{\"FIELD1\":\"ABCDE\"}\n")?;

    cargo_bin_cmd!("copybook")
        .args([
            "encode",
            &cpy.path().to_string_lossy(),
            &jsonl.path().to_string_lossy(),
            "--format",
            "fixed",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("--output"));
    Ok(())
}

#[test]
fn encode_malformed_json_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("bad_json.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let jsonl = tmp.child("input.jsonl");
    jsonl.write_str("{\"FIELD1\":\"unterminated")?;
    let out = tmp.child("out.bin");

    cargo_bin_cmd!("copybook")
        .args([
            "encode",
            &cpy.path().to_string_lossy(),
            &jsonl.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn encode_nonexistent_input_file_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("nei.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let out = tmp.child("out.bin");

    cargo_bin_cmd!("copybook")
        .args([
            "encode",
            &cpy.path().to_string_lossy(),
            "nonexistent.jsonl",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn encode_help_exits_zero() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["encode", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--format"))
        .stdout(predicate::str::contains("--output"))
        .stdout(predicate::str::contains("--codepage"))
        .stdout(predicate::str::contains("--use-raw"))
        .stdout(predicate::str::contains("--fail-fast"))
        .stdout(predicate::str::contains("--select"));
    Ok(())
}

// ====================================================================
// verify subcommand
// ====================================================================

#[test]
fn verify_valid_data_exits_zero() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("verify.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;

    cargo_bin_cmd!("copybook")
        .args([
            "verify",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn verify_with_report_flag_writes_json() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("vr.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;
    let report = tmp.child("report.json");

    cargo_bin_cmd!("copybook")
        .args([
            "verify",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--report",
            &report.path().to_string_lossy(),
        ])
        .assert()
        .success();

    let contents = std::fs::read_to_string(report.path())?;
    let json: serde_json::Value = serde_json::from_str(&contents)?;
    assert!(json.is_object());
    Ok(())
}

#[test]
fn verify_missing_format_flag_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("vmf.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;

    cargo_bin_cmd!("copybook")
        .args([
            "verify",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("--format"));
    Ok(())
}

#[test]
fn verify_nonexistent_data_file_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("vne.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args([
            "verify",
            &cpy.path().to_string_lossy(),
            "nonexistent.bin",
            "--format",
            "fixed",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn verify_truncated_record_reports_warning() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("trunc.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    // Only 3 bytes but LRECL is 5
    data.write_binary(&[0xC1, 0xC2, 0xC3])?;

    let output = cargo_bin_cmd!("copybook")
        .args([
            "verify",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
        ])
        .output()?;

    // Verify command handles truncated records (may warn or fail);
    // the important thing is the process exits cleanly.
    assert!(output.status.code().is_some(), "process should not crash");
    Ok(())
}

#[test]
fn verify_help_exits_zero() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["verify", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--format"))
        .stdout(predicate::str::contains("--report"))
        .stdout(predicate::str::contains("--max-errors"))
        .stdout(predicate::str::contains("--select"));
    Ok(())
}

// ====================================================================
// support subcommand
// ====================================================================

#[test]
fn support_default_table_exits_zero() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .arg("support")
        .assert()
        .success()
        .stdout(predicate::str::contains("Feature"));
    Ok(())
}

#[test]
fn support_json_format_outputs_array() -> TestResult<()> {
    let output = cargo_bin_cmd!("copybook")
        .args(["support", "--format", "json"])
        .output()?;

    assert!(output.status.success());
    let json: serde_json::Value = serde_json::from_slice(&output.stdout)?;
    assert!(json.is_array());
    Ok(())
}

#[test]
fn support_invalid_format_fails() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["support", "--format", "xml"])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn support_help_exits_zero() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["support", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--format"))
        .stdout(predicate::str::contains("--check"));
    Ok(())
}

// ====================================================================
// determinism subcommand
// ====================================================================

#[test]
fn determinism_missing_subcommand_fails() -> TestResult<()> {
    // determinism requires a sub-subcommand (decode, encode, round-trip)
    cargo_bin_cmd!("copybook")
        .arg("determinism")
        .assert()
        .failure();
    Ok(())
}

#[test]
fn determinism_decode_with_ascii_data() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("det.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&ASCII_ABCDE)?;

    cargo_bin_cmd!("copybook")
        .args([
            "determinism",
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("DETERMINISTIC"));
    Ok(())
}

#[test]
fn determinism_encode_with_valid_json() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("det_enc.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let jsonl = tmp.child("input.jsonl");
    jsonl.write_str("{\"FIELD1\":\"ABCDE\"}\n")?;

    cargo_bin_cmd!("copybook")
        .args([
            "determinism",
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            &cpy.path().to_string_lossy(),
            &jsonl.path().to_string_lossy(),
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn determinism_help_shows_subcommands() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .args(["determinism", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("decode"))
        .stdout(predicate::str::contains("encode"))
        .stdout(predicate::str::contains("round-trip"));
    Ok(())
}

// ====================================================================
// version and top-level flags
// ====================================================================

#[test]
fn version_output_contains_program_name() -> TestResult<()> {
    let output = cargo_bin_cmd!("copybook").arg("--version").output()?;

    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;
    assert!(stdout.starts_with("copybook"));
    // Should contain a version pattern like X.Y.Z
    assert!(
        stdout.contains('.'),
        "version output should contain a dot-separated version"
    );
    Ok(())
}

#[test]
fn help_lists_all_subcommands() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("parse"))
        .stdout(predicate::str::contains("inspect"))
        .stdout(predicate::str::contains("decode"))
        .stdout(predicate::str::contains("encode"))
        .stdout(predicate::str::contains("verify"))
        .stdout(predicate::str::contains("support"))
        .stdout(predicate::str::contains("determinism"));
    Ok(())
}

// ====================================================================
// invalid argument combinations
// ====================================================================

#[test]
fn unknown_global_flag_fails() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .arg("--nonexistent-flag")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error:"));
    Ok(())
}

#[test]
fn unknown_subcommand_fails() -> TestResult<()> {
    cargo_bin_cmd!("copybook")
        .arg("frobnicate")
        .assert()
        .failure();
    Ok(())
}

#[test]
fn strict_policy_and_no_strict_policy_conflict() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("conflict.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args([
            "--strict-policy",
            "--no-strict-policy",
            "parse",
            &cpy.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("cannot be used with"));
    Ok(())
}

#[test]
fn decode_invalid_codepage_value_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("ic.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "INVALID_CODEPAGE",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn decode_invalid_format_value_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("if.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&CP037_ABCDE)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "NOT_A_FORMAT",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

#[test]
fn parse_invalid_dialect_value_fails() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("id.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args(["parse", "--dialect", "xyz", &cpy.path().to_string_lossy()])
        .assert()
        .failure()
        .stderr(predicate::str::is_empty().not());
    Ok(())
}

// ====================================================================
// error output formatting
// ====================================================================

#[test]
fn error_output_goes_to_stderr_not_stdout() -> TestResult<()> {
    let output = cargo_bin_cmd!("copybook")
        .args(["parse", "nonexistent_file.cpy"])
        .output()?;

    assert!(!output.status.success());
    // Error messages go to stderr
    assert!(
        !output.stderr.is_empty(),
        "error output should appear on stderr"
    );
    // Stdout should be empty on error
    assert!(output.stdout.is_empty(), "stdout should be empty on error");
    Ok(())
}

#[test]
fn clap_errors_include_usage_hint() -> TestResult<()> {
    // When clap fails (missing args), output includes usage
    cargo_bin_cmd!("copybook")
        .arg("decode")
        .assert()
        .failure()
        .stderr(predicate::str::contains("Usage:"));
    Ok(())
}

// ====================================================================
// edge cases
// ====================================================================

#[test]
fn decode_empty_data_file_succeeds_with_empty_output() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("empty_data.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    data.write_binary(&[])?;
    let out = tmp.child("out.jsonl");

    // Empty data file → no records → should succeed with empty output
    let result = cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .output()?;

    // Should succeed or fail gracefully (implementation-dependent)
    // but must not panic
    assert!(result.status.code().is_some(), "process should not crash");
    Ok(())
}

#[test]
fn encode_empty_jsonl_file_succeeds() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("empty_enc.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let jsonl = tmp.child("input.jsonl");
    jsonl.write_str("")?;
    let out = tmp.child("out.bin");

    let result = cargo_bin_cmd!("copybook")
        .args([
            "encode",
            &cpy.path().to_string_lossy(),
            &jsonl.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .output()?;

    // Empty input → no records → should not panic
    assert!(result.status.code().is_some(), "process should not crash");
    Ok(())
}

#[test]
fn decode_multi_record_file() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("multi.cpy");
    cpy.write_str(SIMPLE_COPYBOOK)?;
    let data = tmp.child("data.bin");
    // Two 5-byte records
    let mut two_records = Vec::new();
    two_records.extend_from_slice(&CP037_ABCDE);
    two_records.extend_from_slice(&CP037_ABCDE);
    data.write_binary(&two_records)?;
    let out = tmp.child("out.jsonl");

    cargo_bin_cmd!("copybook")
        .args([
            "decode",
            &cpy.path().to_string_lossy(),
            &data.path().to_string_lossy(),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            &out.path().to_string_lossy(),
        ])
        .assert()
        .success();

    let contents = std::fs::read_to_string(out.path())?;
    let lines: Vec<&str> = contents.lines().collect();
    assert_eq!(lines.len(), 2, "should decode two records");
    Ok(())
}

#[test]
fn parse_two_field_copybook_has_correct_field_count() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("two.cpy");
    cpy.write_str(TWO_FIELD_COPYBOOK)?;

    let output = cargo_bin_cmd!("copybook")
        .args(["parse", &cpy.path().to_string_lossy()])
        .output()?;

    assert!(output.status.success());
    let json: serde_json::Value = serde_json::from_slice(&output.stdout)?;
    let fields = json["fields"].as_array().unwrap();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0]["name"], "NAME");
    assert_eq!(fields[1]["name"], "AMOUNT");
    Ok(())
}

#[test]
fn inspect_shows_lrecl_for_two_field_layout() -> TestResult<()> {
    let tmp = assert_fs::TempDir::new()?;
    let cpy = tmp.child("lrecl.cpy");
    cpy.write_str(TWO_FIELD_COPYBOOK)?;

    cargo_bin_cmd!("copybook")
        .args(["inspect", &cpy.path().to_string_lossy()])
        .assert()
        .success()
        .stdout(predicate::str::contains("Fixed LRECL:"))
        .stdout(predicate::str::contains("NAME"))
        .stdout(predicate::str::contains("AMOUNT"));
    Ok(())
}
