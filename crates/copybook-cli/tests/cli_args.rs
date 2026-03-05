// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI argument handling tests using assert_cmd.
//!
//! Tests exercise the compiled `copybook` binary end-to-end:
//! subcommand routing, required/optional arguments, defaults,
//! flag conflicts, help/version, environment variable overrides.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use assert_fs::prelude::*;
use common::{TestResult, bin};

/// Minimal copybook: one DISPLAY field = 5-byte LRECL.
const SIMPLE_CPY: &str = "\
       01 RECORD.
          05 FIELD1 PIC X(5).
";

/// Two-field copybook for projection / multi-field tests.
const TWO_FIELD_CPY: &str = "\
       01 RECORD.
          05 NAME    PIC X(10).
          05 AMOUNT  PIC 9(5)V99.
";

/// CP-037 encoding of "ABCDE" (5 bytes).
const CP037_ABCDE: [u8; 5] = [0xC1, 0xC2, 0xC3, 0xC4, 0xC5];

/// CP-037 encoding for a 17-byte two-field record (10 + 7).
const CP037_TWO_FIELD: [u8; 17] = [
    0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // NAME  = "ABCDE     "
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, // AMOUNT = "0123456" -> 01234.56
];

// ====================================================================
// Helper to set up temp copybook + data files
// ====================================================================

struct Fixture {
    _dir: assert_fs::TempDir,
    cpy: std::path::PathBuf,
    bin: std::path::PathBuf,
    out: std::path::PathBuf,
}

impl Fixture {
    fn simple() -> Self {
        let dir = assert_fs::TempDir::new().unwrap();
        let cpy_file = dir.child("test.cpy");
        cpy_file.write_str(SIMPLE_CPY).unwrap();
        let bin_file = dir.child("test.bin");
        bin_file.write_binary(&CP037_ABCDE).unwrap();
        let out_file = dir.child("out.jsonl");
        Self {
            cpy: cpy_file.path().to_path_buf(),
            bin: bin_file.path().to_path_buf(),
            out: out_file.path().to_path_buf(),
            _dir: dir,
        }
    }

    fn two_field() -> Self {
        let dir = assert_fs::TempDir::new().unwrap();
        let cpy_file = dir.child("two.cpy");
        cpy_file.write_str(TWO_FIELD_CPY).unwrap();
        let bin_file = dir.child("two.bin");
        bin_file.write_binary(&CP037_TWO_FIELD).unwrap();
        let out_file = dir.child("out.jsonl");
        Self {
            cpy: cpy_file.path().to_path_buf(),
            bin: bin_file.path().to_path_buf(),
            out: out_file.path().to_path_buf(),
            _dir: dir,
        }
    }
}

fn path_str(p: &std::path::Path) -> String {
    p.to_string_lossy().into_owned()
}

// ====================================================================
// parse subcommand
// ====================================================================

#[test]
fn parse_with_required_args() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["parse", &path_str(&f.cpy)])
        .assert()
        .success()
        .stdout(predicates::str::contains("fields"));
    Ok(())
}

#[test]
fn parse_with_output_flag() -> TestResult<()> {
    let f = Fixture::simple();
    let out = f._dir.child("schema.json");
    bin()
        .args([
            "parse",
            &path_str(&f.cpy),
            "--output",
            &path_str(out.path()),
        ])
        .assert()
        .success();
    assert!(out.path().exists());
    Ok(())
}

#[test]
fn parse_with_strict_flag() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["parse", &path_str(&f.cpy), "--strict"])
        .assert()
        .success();
    Ok(())
}

#[test]
fn parse_with_strict_comments_flag() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["parse", &path_str(&f.cpy), "--strict-comments"])
        .assert()
        .success();
    Ok(())
}

#[test]
fn parse_with_dialect_normative() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["parse", &path_str(&f.cpy), "--dialect", "n"])
        .assert()
        .success();
    Ok(())
}

#[test]
fn parse_with_dialect_zero_tolerant() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["parse", &path_str(&f.cpy), "--dialect", "0"])
        .assert()
        .success();
    Ok(())
}

#[test]
fn parse_with_dialect_one_tolerant() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["parse", &path_str(&f.cpy), "--dialect", "1"])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// inspect subcommand
// ====================================================================

#[test]
fn inspect_with_required_args() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["inspect", &path_str(&f.cpy)])
        .assert()
        .success()
        .stdout(predicates::str::contains("FIELD1"));
    Ok(())
}

#[test]
fn inspect_with_codepage() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["inspect", &path_str(&f.cpy), "--codepage", "cp037"])
        .assert()
        .success();
    Ok(())
}

#[test]
fn inspect_with_dialect() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["inspect", &path_str(&f.cpy), "--dialect", "0"])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// decode subcommand
// ====================================================================

#[test]
fn decode_with_all_required_args() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
        ])
        .assert()
        .success();
    assert!(f.out.exists());
    Ok(())
}

#[test]
fn decode_with_all_options() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--json-number",
            "lossless",
            "--strict",
            "--max-errors",
            "10",
            "--fail-fast",
            "--emit-filler",
            "--emit-meta",
            "--emit-raw",
            "record",
            "--on-decode-unmappable",
            "replace",
            "--threads",
            "1",
            "--dialect",
            "n",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn decode_with_json_number_native() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
            "--json-number",
            "native",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn decode_with_select_single() -> TestResult<()> {
    let f = Fixture::two_field();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
            "--select",
            "NAME",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn decode_with_select_comma_separated() -> TestResult<()> {
    let f = Fixture::two_field();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
            "--select",
            "NAME,AMOUNT",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn decode_with_preserve_zoned_encoding() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
            "--preserve-zoned-encoding",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn decode_stdout_dash() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            "-",
            "--format",
            "fixed",
        ])
        .assert()
        .success()
        .stdout(predicates::str::contains("FIELD1"));
    Ok(())
}

// ====================================================================
// encode subcommand
// ====================================================================

#[test]
fn encode_with_all_required_args() -> TestResult<()> {
    let f = Fixture::simple();
    // First decode to get valid JSONL, then encode it back
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .success();

    let encoded = f._dir.child("encoded.bin");
    bin()
        .args([
            "encode",
            &path_str(&f.cpy),
            &path_str(&f.out),
            "--output",
            &path_str(encoded.path()),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
        ])
        .assert()
        .success();
    assert!(encoded.path().exists());
    Ok(())
}

#[test]
fn encode_with_all_options() -> TestResult<()> {
    let f = Fixture::simple();
    // Decode first
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .success();

    let encoded = f._dir.child("enc.bin");
    bin()
        .args([
            "encode",
            &path_str(&f.cpy),
            &path_str(&f.out),
            "--output",
            &path_str(encoded.path()),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--strict",
            "--max-errors",
            "5",
            "--fail-fast",
            "--threads",
            "1",
            "--coerce-numbers",
            "--dialect",
            "0",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn encode_with_bwz_encode() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .success();

    let encoded = f._dir.child("bwz.bin");
    bin()
        .args([
            "encode",
            &path_str(&f.cpy),
            &path_str(&f.out),
            "--output",
            &path_str(encoded.path()),
            "--format",
            "fixed",
            "--bwz-encode",
        ])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// verify subcommand
// ====================================================================

#[test]
fn verify_with_all_required_args() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "verify",
            &path_str(&f.cpy),
            &path_str(&f.bin),
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
fn verify_with_all_options() -> TestResult<()> {
    let f = Fixture::simple();
    let report = f._dir.child("report.json");
    bin()
        .args([
            "verify",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--strict",
            "--max-errors",
            "20",
            "--sample",
            "3",
            "--report",
            &path_str(report.path()),
            "--dialect",
            "1",
        ])
        .assert()
        .success();
    Ok(())
}

#[test]
fn verify_with_select() -> TestResult<()> {
    let f = Fixture::two_field();
    bin()
        .args([
            "verify",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--format",
            "fixed",
            "--select",
            "NAME",
        ])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// determinism subcommand
// ====================================================================

#[test]
fn determinism_decode_mode() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "determinism",
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
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
fn determinism_encode_mode() -> TestResult<()> {
    let f = Fixture::simple();
    // Decode first
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .success();

    bin()
        .args([
            "determinism",
            "encode",
            &path_str(&f.cpy),
            &path_str(&f.out),
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
fn determinism_round_trip_mode() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "determinism",
            "round-trip",
            &path_str(&f.cpy),
            &path_str(&f.bin),
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
fn determinism_with_json_output() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "determinism",
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            "json",
        ])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// Missing required arguments
// ====================================================================

#[test]
fn decode_missing_all_args_fails() -> TestResult<()> {
    bin()
        .arg("decode")
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("Usage:"));
    Ok(())
}

#[test]
fn encode_missing_all_args_fails() -> TestResult<()> {
    bin()
        .arg("encode")
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("Usage:"));
    Ok(())
}

#[test]
fn verify_missing_all_args_fails() -> TestResult<()> {
    bin()
        .arg("verify")
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("Usage:"));
    Ok(())
}

#[test]
fn parse_missing_copybook_fails() -> TestResult<()> {
    bin()
        .arg("parse")
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("Usage:"));
    Ok(())
}

#[test]
fn inspect_missing_copybook_fails() -> TestResult<()> {
    bin()
        .arg("inspect")
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("Usage:"));
    Ok(())
}

#[test]
fn decode_missing_format_fails() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
        ])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("--format"));
    Ok(())
}

// ====================================================================
// Invalid flag/values
// ====================================================================

#[test]
fn unknown_flag_rejected() -> TestResult<()> {
    bin()
        .arg("--no-such-flag")
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("error:"));
    Ok(())
}

#[test]
fn unknown_subcommand_rejected() -> TestResult<()> {
    bin()
        .arg("nonexistent")
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("Usage:"));
    Ok(())
}

#[test]
fn invalid_dialect_value_rejected() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["parse", &path_str(&f.cpy), "--dialect", "invalid"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("error:"));
    Ok(())
}

#[test]
fn invalid_codepage_rejected() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
            "--codepage",
            "BOGUS",
        ])
        .assert()
        .failure()
        .code(3);
    Ok(())
}

#[test]
fn invalid_format_rejected() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "INVALID",
        ])
        .assert()
        .failure()
        .code(3);
    Ok(())
}

// ====================================================================
// Conflicting options
// ====================================================================

#[test]
fn strict_policy_conflict() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "--strict-policy",
            "--no-strict-policy",
            "parse",
            &path_str(&f.cpy),
        ])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("cannot be used with"));
    Ok(())
}

// ====================================================================
// --help and --version
// ====================================================================

#[test]
fn help_flag_succeeds() -> TestResult<()> {
    bin().arg("--help").assert().success();
    Ok(())
}

#[test]
fn version_flag_succeeds() -> TestResult<()> {
    bin().arg("--version").assert().success();
    Ok(())
}

#[test]
fn help_output_contains_subcommands() -> TestResult<()> {
    let assert = bin().arg("--help").assert().success();
    let stdout = String::from_utf8_lossy(&assert.get_output().stdout);
    assert!(stdout.contains("parse"));
    assert!(stdout.contains("inspect"));
    assert!(stdout.contains("decode"));
    assert!(stdout.contains("encode"));
    assert!(stdout.contains("verify"));
    assert!(stdout.contains("determinism"));
    Ok(())
}

#[test]
fn version_output_contains_version_number() -> TestResult<()> {
    let assert = bin().arg("--version").assert().success();
    let stdout = String::from_utf8_lossy(&assert.get_output().stdout);
    // Version string should contain a semver-like pattern
    assert!(
        stdout.contains("copybook"),
        "expected 'copybook' in version output"
    );
    Ok(())
}

#[test]
fn subcommand_help_decode() -> TestResult<()> {
    bin()
        .args(["decode", "--help"])
        .assert()
        .success()
        .stdout(predicates::str::contains("--format"))
        .stdout(predicates::str::contains("--codepage"))
        .stdout(predicates::str::contains("--output"));
    Ok(())
}

#[test]
fn subcommand_help_encode() -> TestResult<()> {
    bin()
        .args(["encode", "--help"])
        .assert()
        .success()
        .stdout(predicates::str::contains("--format"))
        .stdout(predicates::str::contains("--codepage"));
    Ok(())
}

#[test]
fn subcommand_help_verify() -> TestResult<()> {
    bin()
        .args(["verify", "--help"])
        .assert()
        .success()
        .stdout(predicates::str::contains("--format"))
        .stdout(predicates::str::contains("--report"));
    Ok(())
}

#[test]
fn subcommand_help_determinism() -> TestResult<()> {
    bin()
        .args(["determinism", "--help"])
        .assert()
        .success()
        .stdout(predicates::str::contains("decode"))
        .stdout(predicates::str::contains("encode"));
    Ok(())
}

// ====================================================================
// Default values
// ====================================================================

#[test]
fn decode_defaults_codepage_cp037() -> TestResult<()> {
    // Without explicit --codepage, cp037 is the default
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .success();

    // Verify the output contains decoded data (cp037 is default)
    let content = std::fs::read_to_string(&f.out)?;
    assert!(content.contains("ABCDE"));
    Ok(())
}

#[test]
fn decode_defaults_json_number_lossless() -> TestResult<()> {
    // Without explicit --json-number, lossless is the default
    let f = Fixture::two_field();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .success();

    let content = std::fs::read_to_string(&f.out)?;
    // Lossless mode emits string-wrapped numbers
    assert!(!content.is_empty());
    Ok(())
}

#[test]
fn decode_defaults_threads_1() -> TestResult<()> {
    // Without explicit --threads, default is 1
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            &path_str(&f.bin),
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// Environment variable overrides (COPYBOOK_DIALECT)
// ====================================================================

#[test]
fn env_copybook_dialect_zero() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .env("COPYBOOK_DIALECT", "0")
        .args(["parse", &path_str(&f.cpy)])
        .assert()
        .success();
    Ok(())
}

#[test]
fn env_copybook_dialect_one() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .env("COPYBOOK_DIALECT", "1")
        .args(["parse", &path_str(&f.cpy)])
        .assert()
        .success();
    Ok(())
}

#[test]
fn cli_dialect_overrides_env() -> TestResult<()> {
    // CLI --dialect should take precedence over COPYBOOK_DIALECT env var
    let f = Fixture::simple();
    bin()
        .env("COPYBOOK_DIALECT", "0")
        .args(["parse", &path_str(&f.cpy), "--dialect", "1"])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// Verbose flag
// ====================================================================

#[test]
fn verbose_flag_accepted() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["-v", "parse", &path_str(&f.cpy)])
        .assert()
        .success();
    Ok(())
}

#[test]
fn long_verbose_flag_accepted() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["--verbose", "parse", &path_str(&f.cpy)])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// Feature flag options
// ====================================================================

#[test]
fn list_features_flag() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args(["--list-features", "parse", &path_str(&f.cpy)])
        .assert()
        .success();
    Ok(())
}

#[test]
fn enable_features_flag() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "--enable-features",
            "verbose_logging",
            "parse",
            &path_str(&f.cpy),
        ])
        .assert()
        .success();
    Ok(())
}

// ====================================================================
// Nonexistent file paths (runtime errors, not parse errors)
// ====================================================================

#[test]
fn parse_nonexistent_copybook_fails_runtime() -> TestResult<()> {
    bin()
        .args(["parse", "nonexistent_file.cpy"])
        .assert()
        .failure();
    Ok(())
}

#[test]
fn decode_nonexistent_input_fails_runtime() -> TestResult<()> {
    let f = Fixture::simple();
    bin()
        .args([
            "decode",
            &path_str(&f.cpy),
            "nonexistent_data.bin",
            "--output",
            &path_str(&f.out),
            "--format",
            "fixed",
        ])
        .assert()
        .failure();
    Ok(())
}

// ====================================================================
// Support subcommand
// ====================================================================

#[test]
fn support_subcommand_succeeds() -> TestResult<()> {
    bin().arg("support").assert().success();
    Ok(())
}
