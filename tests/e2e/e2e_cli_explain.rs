// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `explain` CLI subcommand.
//!
//! Validates that known stable error identities render in both text and
//! JSON formats (full and short codes, case-insensitively), and that
//! unknown identities exit 3 with guidance.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;

fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

#[test]
fn explain_full_code_text() {
    cmd()
        .args(["explain", "CBKE501_JSON_TYPE_MISMATCH"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKE501_JSON_TYPE_MISMATCH"))
        .stdout(predicate::str::contains("Fix:"));
}

#[test]
fn explain_short_code_resolves() {
    cmd()
        .args(["explain", "cbkd411"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKD411_ZONED_BAD_SIGN"));
}

#[test]
fn explain_reserved_code_is_explained() {
    // Reserved identities are still documented taxonomy: explain them.
    cmd()
        .args(["explain", "CBKD431"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKD431_FLOAT_NAN"));
}

#[test]
fn explain_json_format_is_machine_readable() {
    let assert = cmd()
        .args(["explain", "CBKF104_RDW_SUSPECT_ASCII", "--format", "json"])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("explain --format json should produce valid JSON: {e}"));
    assert_eq!(parsed["code"], "CBKF104_RDW_SUSPECT_ASCII");
    assert_eq!(parsed["family"], "CBKF");
    assert!(
        parsed["resolution"].as_str().is_some_and(|s| !s.is_empty()),
        "JSON explanation must carry a resolution"
    );
}

#[test]
fn explain_unknown_identity_exits_3() {
    cmd()
        .args(["explain", "CBKX000_MISSING"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("unknown error identity"));
}
