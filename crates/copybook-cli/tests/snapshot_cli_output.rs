// SPDX-License-Identifier: AGPL-3.0-or-later
//! Snapshot tests for CLI output stability.
//!
//! These tests verify that CLI commands produce stable, expected output
//! formats. They catch accidental changes to JSON structure, help text
//! phrasing, and human-readable inspect output.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod test_utils;

use assert_cmd::cargo::cargo_bin_cmd;
use assert_fs::prelude::*;
use test_utils::{TestResult, path_to_str};

// ---------------------------------------------------------------------------
// parse: JSON output structure is stable
// ---------------------------------------------------------------------------

#[test]
fn snapshot_parse_json_output_structure() -> TestResult<()> {
    // Validates that `parse` produces a JSON object with the expected
    // top-level keys: fields, lrecl_fixed, tail_odo, fingerprint.
    let cpy = "\
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID   PIC X(10).
          05 BALANCE       PIC S9(7)V99 COMP-3.
";
    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("simple.cpy");
    f.write_str(cpy)?;

    let output = cargo_bin_cmd!("copybook")
        .args(["parse", path_to_str(f.path())?])
        .output()?;
    assert!(output.status.success());

    let json: serde_json::Value = serde_json::from_slice(&output.stdout)?;

    // Top-level keys must exist
    assert!(json.get("fields").is_some(), "missing 'fields' key");
    assert!(
        json.get("lrecl_fixed").is_some(),
        "missing 'lrecl_fixed' key"
    );
    assert!(json.get("tail_odo").is_some(), "missing 'tail_odo' key");
    assert!(
        json.get("fingerprint").is_some(),
        "missing 'fingerprint' key"
    );

    // fields array contains the parsed fields (01-level children inlined)
    let fields = json["fields"].as_array().unwrap();
    assert_eq!(fields.len(), 2);

    // Verify field types in output
    assert_eq!(fields[0]["name"], "CUSTOMER-ID");
    assert_eq!(fields[0]["kind"]["Alphanum"]["len"], 10);
    assert_eq!(fields[1]["name"], "BALANCE");
    assert_eq!(fields[1]["kind"]["PackedDecimal"]["digits"], 9);
    assert_eq!(fields[1]["kind"]["PackedDecimal"]["scale"], 2);
    assert_eq!(fields[1]["kind"]["PackedDecimal"]["signed"], true);

    Ok(())
}

// ---------------------------------------------------------------------------
// inspect: human-readable layout contains expected sections
// ---------------------------------------------------------------------------

#[test]
fn snapshot_inspect_output_format() -> TestResult<()> {
    // Validates that `inspect` produces human-readable output with
    // a header, column labels, and field rows in the expected format.
    let cpy = "\
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID   PIC X(10).
          05 BALANCE       PIC S9(7)V99 COMP-3.
";
    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("inspect.cpy");
    f.write_str(cpy)?;

    let output = cargo_bin_cmd!("copybook")
        .args(["inspect", path_to_str(f.path())?])
        .output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    // Header section
    assert!(
        stdout.contains("Copybook Layout"),
        "missing 'Copybook Layout' header"
    );
    assert!(stdout.contains("Codepage:"), "missing 'Codepage:' line");
    assert!(
        stdout.contains("Fixed LRECL:"),
        "missing 'Fixed LRECL:' line"
    );

    // Column headers
    assert!(
        stdout.contains("Field Path"),
        "missing 'Field Path' column header"
    );
    assert!(stdout.contains("Offset"), "missing 'Offset' column header");
    assert!(stdout.contains("Length"), "missing 'Length' column header");
    assert!(stdout.contains("Type"), "missing 'Type' column header");

    // Field rows
    assert!(stdout.contains("CUSTOMER-ID"), "missing CUSTOMER-ID field");
    assert!(stdout.contains("X(10)"), "missing X(10) type annotation");
    assert!(stdout.contains("BALANCE"), "missing BALANCE field");
    assert!(stdout.contains("COMP-3"), "missing COMP-3 type annotation");

    Ok(())
}

// ---------------------------------------------------------------------------
// help text: subcommand names and descriptions are stable
// ---------------------------------------------------------------------------

#[test]
fn snapshot_main_help_lists_all_subcommands() -> TestResult<()> {
    // Validates that --help lists all expected subcommands
    let output = cargo_bin_cmd!("copybook").arg("--help").output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    let expected_subcommands = [
        "parse",
        "inspect",
        "decode",
        "encode",
        "verify",
        "support",
        "determinism",
        "help",
    ];
    for cmd in &expected_subcommands {
        assert!(
            stdout.contains(cmd),
            "missing subcommand '{cmd}' in --help output"
        );
    }

    Ok(())
}

#[test]
fn snapshot_parse_help_shows_expected_options() -> TestResult<()> {
    // Validates that `parse --help` mentions key options
    let output = cargo_bin_cmd!("copybook")
        .args(["parse", "--help"])
        .output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    assert!(stdout.contains("<COPYBOOK>"), "missing COPYBOOK argument");
    assert!(stdout.contains("--output"), "missing --output option");
    assert!(stdout.contains("--dialect"), "missing --dialect option");
    assert!(stdout.contains("--strict"), "missing --strict option");

    Ok(())
}

#[test]
fn snapshot_inspect_help_shows_expected_options() -> TestResult<()> {
    // Validates that `inspect --help` mentions key options
    let output = cargo_bin_cmd!("copybook")
        .args(["inspect", "--help"])
        .output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    assert!(stdout.contains("<COPYBOOK>"), "missing COPYBOOK argument");
    assert!(stdout.contains("--codepage"), "missing --codepage option");
    assert!(stdout.contains("--dialect"), "missing --dialect option");

    Ok(())
}

#[test]
fn snapshot_decode_help_shows_expected_options() -> TestResult<()> {
    // Validates that `decode --help` mentions key options
    let output = cargo_bin_cmd!("copybook")
        .args(["decode", "--help"])
        .output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    assert!(stdout.contains("--format"), "missing --format option");
    assert!(stdout.contains("--codepage"), "missing --codepage option");
    assert!(stdout.contains("--output"), "missing --output option");

    Ok(())
}

// ---------------------------------------------------------------------------
// parse: error on invalid input produces structured error exit
// ---------------------------------------------------------------------------

#[test]
fn snapshot_parse_error_on_empty_file() -> TestResult<()> {
    // An empty copybook should produce a non-zero exit code
    let tmp = assert_fs::TempDir::new()?;
    let f = tmp.child("empty.cpy");
    f.write_str("")?;

    cargo_bin_cmd!("copybook")
        .args(["parse", path_to_str(f.path())?])
        .assert()
        .failure();

    Ok(())
}

#[test]
fn snapshot_parse_error_on_missing_file() -> TestResult<()> {
    // A non-existent file should produce a non-zero exit code
    cargo_bin_cmd!("copybook")
        .args(["parse", "nonexistent_file.cpy"])
        .assert()
        .failure();

    Ok(())
}

// ---------------------------------------------------------------------------
// version: output format is stable
// ---------------------------------------------------------------------------

#[test]
fn snapshot_version_output_format() -> TestResult<()> {
    // Validates that --version produces a line starting with "copybook"
    let output = cargo_bin_cmd!("copybook").arg("--version").output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    assert!(
        stdout.starts_with("copybook"),
        "version output should start with 'copybook', got: {stdout}"
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// support: table output format
// ---------------------------------------------------------------------------

#[test]
fn snapshot_support_command_table_output() -> TestResult<()> {
    // Validates that `copybook support` produces human-readable table output
    let output = cargo_bin_cmd!("copybook").arg("support").output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    // Table output should contain header-like content and feature identifiers
    assert!(
        !stdout.is_empty(),
        "support command should produce non-empty output"
    );
    // Should contain at least one known feature category
    assert!(
        stdout.contains("DISPLAY") || stdout.contains("COMP") || stdout.contains("PIC"),
        "support table should mention COBOL data types, got: {stdout}"
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// support: JSON output format
// ---------------------------------------------------------------------------

#[test]
fn snapshot_support_command_json_output() -> TestResult<()> {
    // Validates that `copybook support --format json` produces valid JSON
    let output = cargo_bin_cmd!("copybook")
        .args(["support", "--format", "json"])
        .output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    let parsed: serde_json::Value = serde_json::from_str(stdout.trim())
        .unwrap_or_else(|e| panic!("support JSON should be valid: {e}\nGot: {stdout}"));
    assert!(
        parsed.is_object() || parsed.is_array(),
        "support JSON should be an object or array"
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// decode --help: mentions --emit-meta flag
// ---------------------------------------------------------------------------

#[test]
fn snapshot_decode_help_shows_emit_meta() -> TestResult<()> {
    // Validates that decode subcommand help includes --emit-meta flag
    let output = cargo_bin_cmd!("copybook")
        .args(["decode", "--help"])
        .output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    assert!(
        stdout.contains("--emit-meta"),
        "decode help should mention --emit-meta flag, got: {stdout}"
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// decode --help: mentions --select flag
// ---------------------------------------------------------------------------

#[test]
fn snapshot_decode_help_shows_select() -> TestResult<()> {
    // Validates that decode subcommand help includes --select flag
    let output = cargo_bin_cmd!("copybook")
        .args(["decode", "--help"])
        .output()?;
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;

    assert!(
        stdout.contains("--select"),
        "decode help should mention --select flag, got: {stdout}"
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// parse --dialect: flag is accepted without error
// ---------------------------------------------------------------------------

#[test]
fn snapshot_parse_dialect_flag_accepted() -> TestResult<()> {
    // Validates that --dialect flag is accepted by the parse command.
    // Uses a minimal valid copybook so parsing succeeds.
    let temp = assert_fs::TempDir::new()?;
    let cpy_file = temp.child("dialect_test.cpy");
    cpy_file.write_str(
        "\
       01 DIALECT-RECORD.
          05 COUNTER PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 DEPENDING ON COUNTER
             PIC X(5).
",
    )?;

    for dialect_value in &["n", "0", "1"] {
        let output = cargo_bin_cmd!("copybook")
            .args([
                "parse",
                path_to_str(cpy_file.path())?,
                "--dialect",
                dialect_value,
            ])
            .output()?;
        assert!(
            output.status.success(),
            "parse --dialect {dialect_value} should succeed, stderr: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}
