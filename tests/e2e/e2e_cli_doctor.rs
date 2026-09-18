// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `doctor` CLI subcommand.
//!
//! Validates the healthy demo pair reports healthy (exit 0) with a next
//! command, a garbage copybook fails with its stable identity and fix
//! (exit 3), a truncated file fails framing, copybook-only mode passes,
//! and JSON output is machine-readable.

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

#[test]
fn doctor_healthy_demo_pair() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains("healthy"))
        .stdout(predicate::str::contains("trial-decode"))
        .stdout(predicate::str::contains("next: copybook decode"));
}

#[test]
fn doctor_garbage_copybook_names_code_and_fix() {
    let dir = tempfile::tempdir().expect("tempdir");
    let bad = write_temp_file(&dir, "bad.cpy", b"THIS IS NOT A COPYBOOK ((( ");
    let data = workspace_path("fixtures/data/simple.bin");
    cmd()
        .args(["doctor"])
        .arg(&bad)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stdout(predicate::str::contains("CBKP"))
        .stdout(predicate::str::contains("fix:"));
}

#[test]
fn doctor_truncated_file_fails_framing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let data = workspace_path("fixtures/data/simple.bin");
    let bytes = std::fs::read(&data).expect("read fixture");
    let truncated = write_temp_file(&dir, "short.bin", &bytes[..30]);
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&truncated)
        .assert()
        .failure()
        .stdout(predicate::str::contains("format-probe"));
}

#[test]
fn doctor_copybook_only_mode_passes() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    cmd()
        .args(["doctor"])
        .arg(&copybook)
        .assert()
        .success()
        .stdout(predicate::str::contains("copybook-only diagnosis"));
}

#[test]
fn doctor_json_report_is_machine_readable() {
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let assert = cmd()
        .args(["doctor"])
        .arg(&copybook)
        .arg(&data)
        .arg("--json")
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("doctor --json should produce valid JSON: {e}"));
    assert_eq!(parsed["verdict"], "healthy");
    let findings = parsed["findings"].as_array().expect("findings array");
    assert!(
        findings.iter().any(|f| f["check"] == "trial-decode"),
        "report must include trial-decode, got: {parsed}"
    );
}
