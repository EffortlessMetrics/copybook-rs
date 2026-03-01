// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for determinism CLI commands
//!
//! These tests verify the CLI wiring and argument parsing for determinism validation
//! commands. They use real copybook fixtures and invoke the compiled binary via
//! `assert_cmd` to validate end-to-end behavior.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;
use std::fs;
use std::io::Write;
use std::path::Path;
use tempfile::tempdir;

/// Minimal fixed-format copybook: one X(5) field.
const SIMPLE_COPYBOOK: &str = r"
       01 RECORD.
          05 FIELD PIC X(5).
";

/// CP037 bytes for "ABCDE" (A–E)
const SIMPLE_DATA_CP037: [u8; 5] = [0xC1, 0xC2, 0xC3, 0xC4, 0xC5];

/// Helper: write a small schema + data pair into a temp dir.
fn write_simple_fixture(dir: &Path) -> (String, String) {
    let copybook_path = dir.join("schema.cpy");
    let data_path = dir.join("data.bin");

    fs::write(&copybook_path, SIMPLE_COPYBOOK).expect("write copybook");
    fs::write(&data_path, SIMPLE_DATA_CP037).expect("write data");

    (
        copybook_path.to_string_lossy().into_owned(),
        data_path.to_string_lossy().into_owned(),
    )
}

/// Helper: write a one-line JSON object for encode tests.
fn write_simple_json(dir: &Path) -> String {
    let json_path = dir.join("input.jsonl");
    let mut f = fs::File::create(&json_path).expect("create jsonl");
    // determinism CLI currently reads only the first line
    writeln!(f, r#"{{"FIELD":"ABCDE"}}"#).expect("write json line");
    json_path.to_string_lossy().into_owned()
}

#[test]
fn determinism_decode_deterministic_exit_ok() {
    let tmp = tempdir().expect("tempdir");
    let (copybook, data) = write_simple_fixture(tmp.path());

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "determinism",
        "decode",
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        &copybook,
        &data,
    ]);

    cmd.assert().success();
}

#[test]
fn determinism_decode_json_output_is_well_formed() {
    let tmp = tempdir().expect("tempdir");
    let (copybook, data) = write_simple_fixture(tmp.path());

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "determinism",
        "decode",
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        "--output",
        "json",
        &copybook,
        &data,
    ]);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains(r#""is_deterministic": true"#));
}

#[test]
fn determinism_encode_deterministic_exit_ok() {
    let tmp = tempdir().expect("tempdir");
    let (copybook, _) = write_simple_fixture(tmp.path());
    let json_path = write_simple_json(tmp.path());

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "determinism",
        "encode",
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        &copybook,
        &json_path,
    ]);

    cmd.assert().success();
}

#[test]
fn determinism_round_trip_deterministic_exit_ok() {
    let tmp = tempdir().expect("tempdir");
    let (copybook, data) = write_simple_fixture(tmp.path());

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "determinism",
        "round-trip",
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        &copybook,
        &data,
    ]);

    cmd.assert().success();
}

#[test]
fn determinism_decode_human_output_contains_verdict() {
    let tmp = tempdir().expect("tempdir");
    let (copybook, data) = write_simple_fixture(tmp.path());

    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args([
        "determinism",
        "decode",
        "--format",
        "fixed",
        "--codepage",
        "cp037",
        &copybook,
        &data,
    ]);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("✅ DETERMINISTIC"))
        .stdout(predicate::str::contains("Round 1 hash:"))
        .stdout(predicate::str::contains("Round 2 hash:"));
}

#[test]
fn determinism_help_shows_exit_codes() {
    let mut cmd = cargo_bin_cmd!("copybook");
    cmd.args(["determinism", "--help"]);

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Exit codes:"))
        .stdout(predicate::str::contains("0 = deterministic"))
        .stdout(predicate::str::contains("2 = non-deterministic"));
}
