// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for decode/encode `--strict-comments` and `--emit-filler` flags.
//!
//! These flags affect parsing behavior (inline comments) and output
//! completeness (FILLER fields), respectively.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

/// Copybook with inline comment (*> syntax, COBOL-2002).
const INLINE_COMMENT_CPY: &str = "\
       01  REC.
           05  FIELD-A   PIC X(10). *> inline comment
           05  FIELD-B   PIC 9(5).
";

/// Copybook with a FILLER field.
const FILLER_CPY: &str = "\
       01  REC.
           05  NAME     PIC X(10).
           05  FILLER   PIC X(5).
           05  CODE     PIC 9(3).
";

/// Build ASCII test data for a 15-byte record (NAME=10, FIELD-B=5).
fn make_inline_data() -> Vec<u8> {
    let mut data = Vec::new();
    data.extend_from_slice(b"ABCDEFGHIJ"); // FIELD-A: 10 bytes
    data.extend_from_slice(b"12345"); // FIELD-B: 5 bytes
    data
}

/// Build ASCII test data for an 18-byte record (NAME=10, FILLER=5, CODE=3).
fn make_filler_data() -> Vec<u8> {
    let mut data = Vec::new();
    data.extend_from_slice(b"JOHN SMITH"); // NAME: 10 bytes
    data.extend_from_slice(b"     "); // FILLER: 5 bytes
    data.extend_from_slice(b"001"); // CODE: 3 bytes
    data
}

fn setup(cpy_text: &str, data: &[u8]) -> (TempDir, std::path::PathBuf, std::path::PathBuf) {
    let dir = TempDir::new().unwrap();
    let cpy_path = dir.path().join("schema.cpy");
    let data_path = dir.path().join("data.bin");
    std::fs::write(&cpy_path, cpy_text).unwrap();
    std::fs::write(&data_path, data).unwrap();
    (dir, cpy_path, data_path)
}

// =========================================================================
// 1. decode without --strict-comments accepts inline comments
// =========================================================================

#[test]
fn decode_accepts_inline_comments_by_default() {
    let (dir, cpy, data) = setup(INLINE_COMMENT_CPY, &make_inline_data());
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode"])
        .arg(&cpy)
        .arg(&data)
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "ascii"])
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    assert!(
        content.contains("FIELD-A") || content.contains("FIELD_A"),
        "Output should contain field names"
    );
}

// =========================================================================
// 2. decode with --strict-comments rejects inline comments
// =========================================================================

#[test]
fn decode_strict_comments_rejects_inline() {
    let (dir, cpy, data) = setup(INLINE_COMMENT_CPY, &make_inline_data());
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode"])
        .arg(&cpy)
        .arg(&data)
        .args(["--output"])
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--strict-comments",
        ])
        .assert()
        .failure();
}

// =========================================================================
// 3. encode without --strict-comments accepts inline comments
// =========================================================================

#[test]
fn encode_accepts_inline_comments_by_default() {
    let (dir, cpy, _data) = setup(INLINE_COMMENT_CPY, &make_inline_data());

    // Create JSONL input
    let jsonl_path = dir.path().join("input.jsonl");
    std::fs::write(&jsonl_path, r#"{"FIELD-A":"ABCDEFGHIJ","FIELD-B":12345}"#).unwrap();
    let out = dir.path().join("out.bin");

    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&jsonl_path)
        .assert()
        .success();
}

// =========================================================================
// 4. encode with --strict-comments rejects inline comments
// =========================================================================

#[test]
fn encode_strict_comments_rejects_inline() {
    let (dir, cpy, _data) = setup(INLINE_COMMENT_CPY, &make_inline_data());

    let jsonl_path = dir.path().join("input.jsonl");
    std::fs::write(&jsonl_path, r#"{"FIELD-A":"ABCDEFGHIJ","FIELD-B":12345}"#).unwrap();
    let out = dir.path().join("out.bin");

    cmd()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--strict-comments",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&jsonl_path)
        .assert()
        .failure();
}

// =========================================================================
// 5. decode with --emit-filler includes FILLER fields
// =========================================================================

#[test]
fn decode_emit_filler_includes_filler_fields() {
    let (dir, cpy, data) = setup(FILLER_CPY, &make_filler_data());
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode"])
        .arg(&cpy)
        .arg(&data)
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "ascii", "--emit-filler"])
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    // When --emit-filler is set, FILLER fields should appear in output
    // (with auto-generated names like _filler_XXXXXXXX)
    assert!(
        content.contains("filler") || content.contains("FILLER"),
        "Output with --emit-filler should include FILLER fields, got: {content}"
    );
}

// =========================================================================
// 6. decode without --emit-filler omits FILLER fields
// =========================================================================

#[test]
fn decode_without_emit_filler_omits_filler() {
    let (dir, cpy, data) = setup(FILLER_CPY, &make_filler_data());
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode"])
        .arg(&cpy)
        .arg(&data)
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "ascii"])
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    assert!(
        content.contains("NAME") || content.contains("CODE"),
        "Output should contain regular field names"
    );
    assert!(
        !content.contains("filler") && !content.contains("FILLER"),
        "Output without --emit-filler should NOT include FILLER fields, got: {content}"
    );
}
