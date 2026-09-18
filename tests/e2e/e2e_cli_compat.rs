// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `compat` CLI subcommand.
//!
//! Validates identical copybooks report compatible (exit 0), a head
//! copybook that newly refuses input reports incompatible (exit 3) under
//! both policies, JSON output carries the machine-readable verdict, and an
//! unreadable base is inconclusive (exit 3).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use std::io::Write as _;

fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

fn write_temp_file(dir: &tempfile::TempDir, name: &str, contents: &str) -> std::path::PathBuf {
    let path = dir.path().join(name);
    let mut file = std::fs::File::create(&path).expect("create temp file");
    file.write_all(contents.as_bytes())
        .expect("write temp file");
    path
}

const BASE_COPYBOOK: &str = "       01  REC.\n           05  FLD PIC X(10).\n";

// Nested ODO is deliberately rejected (CBKP022); a head copybook that adds
// it regresses every base-supported scenario it touches.
const HEAD_COPYBOOK: &str = "       01  REC.\n           05  N PIC 9(2).\n           05  OUTER OCCURS 2 TIMES.\n               10 INNER OCCURS DEPENDING ON N PIC X(5).\n";

#[test]
fn compat_identical_copybooks_are_compatible() {
    let dir = tempfile::tempdir().expect("tempdir");
    let base = write_temp_file(&dir, "base.cpy", BASE_COPYBOOK);
    cmd()
        .args(["compat"])
        .arg(&base)
        .arg(&base)
        .assert()
        .success()
        .stdout(predicate::str::contains("compatible"))
        .stdout(predicate::str::contains("no scenario changed"));
}

#[test]
fn compat_regressed_head_is_incompatible() {
    let dir = tempfile::tempdir().expect("tempdir");
    let base = write_temp_file(&dir, "base.cpy", BASE_COPYBOOK);
    let head = write_temp_file(&dir, "head.cpy", HEAD_COPYBOOK);
    cmd()
        .args(["compat"])
        .arg(&base)
        .arg(&head)
        .assert()
        .failure()
        .code(3)
        .stdout(predicate::str::contains("incompatible"));
}

#[test]
fn compat_fail_on_any_json_reports_machine_verdict() {
    let dir = tempfile::tempdir().expect("tempdir");
    let base = write_temp_file(&dir, "base.cpy", BASE_COPYBOOK);
    let head = write_temp_file(&dir, "head.cpy", HEAD_COPYBOOK);
    let assert = cmd()
        .args(["compat"])
        .arg(&base)
        .arg(&head)
        .args(["--fail-on", "any", "--format", "json"])
        .assert()
        .failure()
        .code(3);
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("compat --format json should produce valid JSON");
    assert_eq!(parsed["verdict"], "incompatible");
    assert_eq!(parsed["fail_on"], "any");
    let changes = parsed["changes"].as_array().expect("changes array");
    assert!(
        !changes.is_empty(),
        "incompatible verdict must name changes"
    );
    assert!(
        changes.iter().any(|change| change["breaking"] == true),
        "nested-ODO head must contain a breaking change, got: {parsed}"
    );
}

#[test]
fn compat_unreadable_base_exits_4() {
    let dir = tempfile::tempdir().expect("tempdir");
    let head = write_temp_file(&dir, "head.cpy", BASE_COPYBOOK);
    cmd()
        .args(["compat"])
        .arg(dir.path().join("missing.cpy"))
        .arg(&head)
        .assert()
        .failure()
        .code(4);
}
