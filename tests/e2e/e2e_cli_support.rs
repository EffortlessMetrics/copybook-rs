// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `support` CLI subcommand.
//!
//! Validates that the support matrix displays correctly in both table
//! and JSON formats, that `--check` looks up individual features, and
//! that `--status` filtering works.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;

fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

// =========================================================================
// 1. Default table output
// =========================================================================

#[test]
fn support_table_default() {
    cmd()
        .args(["support"])
        .assert()
        .success()
        .stdout(predicate::str::contains("COBOL Feature Support Matrix"))
        .stdout(predicate::str::contains("Feature"))
        .stdout(predicate::str::contains("Status"));
}

// =========================================================================
// 2. JSON format output
// =========================================================================

#[test]
fn support_json_format() {
    let assert = cmd()
        .args(["support", "--format", "json"])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    // Should be valid JSON
    let parsed: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("support --format json should produce valid JSON: {e}"));
    // Should be an array or object containing feature entries
    assert!(
        parsed.is_array() || parsed.is_object(),
        "JSON output should be array or object, got: {parsed}"
    );
}

// =========================================================================
// 3. Table format explicit
// =========================================================================

#[test]
fn support_table_format_explicit() {
    cmd()
        .args(["support", "--format", "table"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Feature"));
}

// =========================================================================
// 4. Filter by supported status
// =========================================================================

#[test]
fn support_filter_supported() {
    cmd()
        .args(["support", "--status", "supported"])
        .assert()
        .success()
        // The table spells the status the way `--status` and the JSON `status`
        // field do, so the three surfaces agree.
        .stdout(predicate::str::contains("supported"))
        .stdout(predicate::str::contains("partial").not());
}

// =========================================================================
// 5. Filter by partial status
// =========================================================================

#[test]
fn support_filter_partial() {
    cmd()
        .args(["support", "--status", "partial"])
        .assert()
        .success()
        .stdout(predicate::str::contains("partial"));
}

// =========================================================================
// 6. Invalid format rejected
// =========================================================================

#[test]
fn support_invalid_format_rejected() {
    cmd()
        .args(["support", "--format", "xml"])
        .assert()
        .failure();
}

// =========================================================================
// 7. With governance metadata
// =========================================================================

#[test]
fn support_with_governance() {
    cmd()
        .args(["support", "--with-governance"])
        .assert()
        .success();
}

// =========================================================================
// 8. JSON with governance
// =========================================================================

#[test]
fn support_json_with_governance() {
    let assert = cmd()
        .args(["support", "--format", "json", "--with-governance"])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let _parsed: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("JSON with governance should be valid: {e}"));
}

// =========================================================================
// 9. The table lists the IDs its own footer tells you to use
// =========================================================================

#[test]
fn support_table_lists_checkable_ids() {
    let assert = cmd().args(["support"]).assert().success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();

    assert!(
        stdout.contains("ID"),
        "table needs an ID column, got:\n{stdout}"
    );
    for id in ["level-88", "occurs-depending", "edited-pic", "nested-odo"] {
        assert!(
            stdout.contains(id),
            "table should list the {id} identifier, got:\n{stdout}"
        );
    }
}

#[test]
fn support_table_ids_are_accepted_by_check() {
    // Every identifier the table prints must be one `--check` resolves,
    // otherwise the footer sends users to a command that rejects them.
    let assert = cmd()
        .args(["support", "--format", "json"])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let features: serde_json::Value = serde_json::from_str(&stdout).unwrap();

    for feature in features.as_array().expect("json array") {
        let id = feature["id"].as_str().expect("string id");
        let output = cmd()
            .args(["support", "--check", id])
            .assert()
            .get_output()
            .clone();
        let combined = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            !combined.contains("unknown feature ID"),
            "--check rejected the advertised id {id}"
        );
    }
}

// =========================================================================
// 10. An unknown identifier names the ones that work
// =========================================================================

#[test]
fn support_check_unknown_id_lists_known_ids() {
    cmd()
        .args(["support", "--check", "no-such-feature"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("unknown feature ID"))
        .stderr(predicate::str::contains("Known feature IDs:"))
        .stderr(predicate::str::contains("level-88"));
}

// =========================================================================
// 11. A closed pipe is not a panic
// =========================================================================

#[test]
fn support_survives_a_closed_stdout() {
    // `copybook support | head` closes stdout early. `println!` panics there;
    // the command must use the pipe-safe writer instead.
    use std::process::{Command as StdCommand, Stdio};

    let mut child = StdCommand::new(assert_cmd::cargo::cargo_bin("copybook"))
        .args(["support"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn copybook support");

    drop(child.stdout.take());
    let output = child.wait_with_output().expect("wait for copybook support");
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !stderr.contains("panicked at"),
        "closed stdout must not panic, stderr:\n{stderr}"
    );
}
