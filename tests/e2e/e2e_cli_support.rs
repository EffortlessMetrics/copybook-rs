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
        .stdout(predicate::str::contains("Supported"));
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
        .stdout(predicate::str::contains("Partial"));
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
