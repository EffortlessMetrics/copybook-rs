// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `inspect` CLI subcommand.
//!
//! Covers flags and modes not tested in `e2e_cli_subcommands.rs`:
//! `--codepage`, `--strict`, `--strict-comments`, and stdin input.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

const SIMPLE_CPY: &str = "\
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID      PIC 9(5).
           05  CUSTOMER-NAME    PIC X(20).
           05  BALANCE          PIC S9(7)V99.
";

/// Copybook with inline comment (COBOL-2002 style).
const INLINE_COMMENT_CPY: &str = "\
       01  REC.
           05  FIELD-A   PIC X(10). *> inline comment here
           05  FIELD-B   PIC 9(5).
";

fn write_cpy(dir: &TempDir, content: &str) -> std::path::PathBuf {
    let path = dir.path().join("schema.cpy");
    std::fs::write(&path, content).unwrap();
    path
}

// =========================================================================
// 1. --codepage cp037 (explicit, same as default)
// =========================================================================

#[test]
fn inspect_codepage_cp037() {
    let dir = TempDir::new().unwrap();
    let cpy = write_cpy(&dir, SIMPLE_CPY);

    cmd()
        .args(["inspect"])
        .arg(&cpy)
        .args(["--codepage", "cp037"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-ID"));
}

// =========================================================================
// 2. --codepage cp1047
// =========================================================================

#[test]
fn inspect_codepage_cp1047() {
    let dir = TempDir::new().unwrap();
    let cpy = write_cpy(&dir, SIMPLE_CPY);

    cmd()
        .args(["inspect"])
        .arg(&cpy)
        .args(["--codepage", "cp1047"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-NAME"));
}

// =========================================================================
// 3. --codepage cp500
// =========================================================================

#[test]
fn inspect_codepage_cp500() {
    let dir = TempDir::new().unwrap();
    let cpy = write_cpy(&dir, SIMPLE_CPY);

    cmd()
        .args(["inspect"])
        .arg(&cpy)
        .args(["--codepage", "cp500"])
        .assert()
        .success()
        .stdout(predicate::str::contains("BALANCE"));
}

// =========================================================================
// 4. --strict flag
// =========================================================================

#[test]
fn inspect_strict_mode() {
    let dir = TempDir::new().unwrap();
    let cpy = write_cpy(&dir, SIMPLE_CPY);

    cmd()
        .args(["inspect"])
        .arg(&cpy)
        .args(["--strict"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-ID"));
}

// =========================================================================
// 5. --strict-comments disables inline comments
// =========================================================================

#[test]
fn inspect_strict_comments_rejects_inline() {
    let dir = TempDir::new().unwrap();
    let cpy = write_cpy(&dir, INLINE_COMMENT_CPY);

    // Without --strict-comments, inline comments are accepted
    cmd().args(["inspect"]).arg(&cpy).assert().success();

    // With --strict-comments, inline comments should cause failure
    cmd()
        .args(["inspect"])
        .arg(&cpy)
        .args(["--strict-comments"])
        .assert()
        .failure();
}

// =========================================================================
// 6. stdin input via pipe
// =========================================================================

#[test]
fn inspect_stdin_input() {
    cmd()
        .args(["inspect", "-"])
        .write_stdin(SIMPLE_CPY)
        .assert()
        .success()
        .stdout(predicate::str::contains("CUSTOMER-ID"));
}

// =========================================================================
// 7. Invalid codepage rejected
// =========================================================================

#[test]
fn inspect_invalid_codepage_rejected() {
    let dir = TempDir::new().unwrap();
    let cpy = write_cpy(&dir, SIMPLE_CPY);

    cmd()
        .args(["inspect"])
        .arg(&cpy)
        .args(["--codepage", "invalid"])
        .assert()
        .failure();
}

// =========================================================================
// 8. All supported codepages
// =========================================================================

#[test]
fn inspect_all_codepages() {
    let dir = TempDir::new().unwrap();
    let cpy = write_cpy(&dir, SIMPLE_CPY);

    for cp in &["cp037", "cp273", "cp500", "cp1047", "cp1140"] {
        cmd()
            .args(["inspect"])
            .arg(&cpy)
            .args(["--codepage", cp])
            .assert()
            .success();
    }
}
