// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `explain` CLI subcommand.
//!
//! Validates that known stable error identities render in both text and
//! JSON formats (full and short codes, case-insensitively), and that
//! unknown identities exit 3 with guidance.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use std::io::Write as _;

fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

fn workspace_path(rel: &str) -> std::path::PathBuf {
    // copybook-e2e lives at tests/e2e; fixtures live at the workspace root.
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(rel)
}

fn write_temp_file(dir: &tempfile::TempDir, name: &str, contents: &[u8]) -> std::path::PathBuf {
    let path = dir.path().join(name);
    let mut file = std::fs::File::create(&path).expect("create temp file");
    file.write_all(contents).expect("write temp file");
    path
}

/// COMP-3 fixture with an invalid sign nibble in DECIMAL-AMOUNT.
fn corrupted_comp3(dir: &tempfile::TempDir) -> (std::path::PathBuf, std::path::PathBuf) {
    let copybook = workspace_path("fixtures/copybooks/comp3_test.cpy");
    let mut data =
        std::fs::read(workspace_path("fixtures/data/comp3_test.bin")).expect("read fixture");
    data[14] = (data[14] & 0xF0) | 0x07;
    let data = write_temp_file(dir, "bad.bin", &data);
    (copybook, data)
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
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("explain --format json should produce valid JSON");
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
