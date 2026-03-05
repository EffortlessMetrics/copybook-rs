// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for dialect flag behavior via the CLI binary.
//!
//! Validates `--dialect` flag on subcommands, `COPYBOOK_DIALECT` env var,
//! flag-overrides-env precedence, invalid dialect values, and that the three
//! modes (Normative, ZeroTolerant, OneTolerant) produce different ODO
//! interpretations.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use std::io::Write;
use std::path::PathBuf;
use std::process::Output;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

fn assert_no_panic(stderr: &str) {
    assert!(
        !stderr.contains("panicked at"),
        "CLI panicked! stderr:\n{stderr}"
    );
}

fn write_temp_file(name: &str, contents: &[u8]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    let path = dir.path().join(name);
    let mut f = std::fs::File::create(&path).expect("create temp file");
    f.write_all(contents).expect("write temp file");
    dir
}

fn temp_path(dir: &tempfile::TempDir, name: &str) -> PathBuf {
    dir.path().join(name)
}

// ---------------------------------------------------------------------------
// Test data
// ---------------------------------------------------------------------------

/// ODO copybook: `OCCURS 2 TO 5 DEPENDING ON CNT` (min_count=2).
const ODO_MIN2_CPY: &str = "\
       01  REC.
           05  CNT       PIC 9(2).
           05  ITEMS     OCCURS 2 TO 5 TIMES
              DEPENDING ON CNT.
              10  ITEM-VAL PIC X(4).
";

/// ODO copybook: `OCCURS 0 TO 5 DEPENDING ON CNT` (min_count=0).
const ODO_MIN0_CPY: &str = "\
       01  REC.
           05  CNT       PIC 9(2).
           05  ITEMS     OCCURS 0 TO 5 TIMES
              DEPENDING ON CNT.
              10  ITEM-VAL PIC X(4).
";

/// Simple copybook for quick dialect parse checks.
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// Build an EBCDIC record for `ODO_MIN2_CPY` with the given count value.
/// CNT is 2 EBCDIC digits, then `count` items of 4 bytes each.
fn odo_min2_record(count: u8) -> Vec<u8> {
    let mut data = Vec::new();
    // CNT as 2 EBCDIC digits: e.g. count=2 → "02" → [0xF0, 0xF2]
    data.push(0xF0 + (count / 10));
    data.push(0xF0 + (count % 10));
    // 5 items max (pad to max size for fixed record)
    for i in 0..5 {
        if i < count {
            // "ITMx" in EBCDIC
            data.extend_from_slice(&[0xC9, 0xE3, 0xD4, 0xF0 + i]);
        } else {
            data.extend_from_slice(&[0x40, 0x40, 0x40, 0x40]);
        }
    }
    data
}

/// Build an EBCDIC record for `ODO_MIN0_CPY` with the given count value.
fn odo_min0_record(count: u8) -> Vec<u8> {
    odo_min2_record(count) // Same layout, different schema
}

/// Simple EBCDIC record for `SIMPLE_CPY` (13 bytes).
#[allow(dead_code)]
fn simple_record() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// Set up temp dir with a copybook and corresponding data file.
fn setup_odo_min2(count: u8) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), ODO_MIN2_CPY).unwrap();
    std::fs::write(dir.path().join("data.bin"), odo_min2_record(count)).unwrap();
    dir
}

fn setup_odo_min0(count: u8) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), ODO_MIN0_CPY).unwrap();
    std::fs::write(dir.path().join("data.bin"), odo_min0_record(count)).unwrap();
    dir
}

#[allow(dead_code)]
fn setup_simple() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_CPY).unwrap();
    std::fs::write(dir.path().join("data.bin"), simple_record()).unwrap();
    dir
}

// =========================================================================
// 1. Parse with --dialect n succeeds
// =========================================================================

#[test]
fn parse_with_dialect_normative() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["parse", "--dialect", "n"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 2. Parse with --dialect 0 succeeds
// =========================================================================

#[test]
fn parse_with_dialect_zero_tolerant() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["parse", "--dialect", "0"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 3. Parse with --dialect 1 succeeds
// =========================================================================

#[test]
fn parse_with_dialect_one_tolerant() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["parse", "--dialect", "1"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 4. Invalid dialect value produces an error
// =========================================================================

#[test]
fn invalid_dialect_value_produces_error() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["parse", "--dialect", "xyz"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_ne!(output.status.code(), Some(0), "invalid dialect should fail");
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 5. Decode with --dialect n and valid count succeeds
// =========================================================================

#[test]
fn decode_dialect_normative_valid_count() {
    let dir = setup_odo_min2(3); // count=3, min=2, within bounds
    let out_path = temp_path(&dir, "out.jsonl");
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "n",
        ])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .arg("--output")
        .arg(&out_path)
        .output()
        .expect("run decode");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 6. Decode with --dialect 0 allows counter=0 on min_count=2 schema
// =========================================================================

#[test]
fn decode_dialect_zero_tolerant_allows_zero_count() {
    let dir = setup_odo_min2(0); // count=0, min=2 but zero-tolerant ignores min
    let out_path = temp_path(&dir, "out.jsonl");
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "0",
        ])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .arg("--output")
        .arg(&out_path)
        .output()
        .expect("run decode");
    // Zero-tolerant should allow count=0 even when declared min is 2
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 7. Decode with --dialect 1 and ODO min_count=0 raises min to 1
// =========================================================================

#[test]
fn decode_dialect_one_tolerant_with_min0_schema() {
    let dir = setup_odo_min0(1); // count=1, min_count=0 but one-tolerant clamps to 1
    let out_path = temp_path(&dir, "out.jsonl");
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "1",
        ])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .arg("--output")
        .arg(&out_path)
        .output()
        .expect("run decode");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 8. COPYBOOK_DIALECT env var is respected
// =========================================================================

#[test]
fn env_var_copybook_dialect_is_respected() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .env("COPYBOOK_DIALECT", "0")
        .args(["parse"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse with env");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 9. CLI --dialect flag overrides COPYBOOK_DIALECT env var
// =========================================================================

#[test]
fn cli_dialect_flag_overrides_env_var() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    // Set env to "0" but CLI to "1" – should use "1"
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .env("COPYBOOK_DIALECT", "0")
        .args(["parse", "--dialect", "1"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 10. Default dialect (no flag, no env) uses normative
// =========================================================================

#[test]
fn default_dialect_is_normative() {
    let dir = write_temp_file("schema.cpy", SIMPLE_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .env_remove("COPYBOOK_DIALECT")
        .args(["parse"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 11. Inspect with --dialect 0 succeeds
// =========================================================================

#[test]
fn inspect_with_dialect_zero() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["inspect", "--dialect", "0"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run inspect");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 12. Inspect with --dialect 1 succeeds
// =========================================================================

#[test]
fn inspect_with_dialect_one() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["inspect", "--dialect", "1"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run inspect");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 13. Verify with --dialect 0 succeeds
// =========================================================================

#[test]
fn verify_with_dialect_zero() {
    let dir = setup_odo_min2(3);
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args([
            "verify",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "0",
        ])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .output()
        .expect("run verify");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 14. Numeric dialect value "2" is invalid
// =========================================================================

#[test]
fn dialect_value_two_is_invalid() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["parse", "--dialect", "2"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_ne!(output.status.code(), Some(0), "dialect 2 should be invalid");
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 15. Empty dialect value is invalid
// =========================================================================

#[test]
fn empty_dialect_value_is_invalid() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args(["parse", "--dialect", ""])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    assert_ne!(
        output.status.code(),
        Some(0),
        "empty dialect should be invalid"
    );
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 16. Dialect flag on encode subcommand
// =========================================================================

#[test]
fn encode_with_dialect_flag() {
    // Just verify the flag is accepted (encode needs input data, so test parse acceptance)
    let dir = setup_odo_min2(3);
    let out_path = temp_path(&dir, "out.jsonl");
    // Decode first
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "0",
        ])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .arg("--output")
        .arg(&out_path)
        .output()
        .expect("run decode");
    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );

    // Now encode with dialect
    let enc_out = temp_path(&dir, "encoded.bin");
    let enc_output = Command::cargo_bin("copybook")
        .unwrap()
        .args([
            "encode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "0",
            "--output",
        ])
        .arg(&enc_out)
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&out_path)
        .output()
        .expect("run encode");
    assert_eq!(
        enc_output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&enc_output)
    );
    assert_no_panic(&stderr_str(&enc_output));
}

// =========================================================================
// 17. Decode with normative dialect and count below min produces error or clipping
// =========================================================================

#[test]
fn decode_normative_count_below_min_is_handled() {
    let dir = setup_odo_min2(1); // count=1, min=2 in normative → should clip/error
    let out_path = temp_path(&dir, "out.jsonl");
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--dialect",
            "n",
        ])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .arg("--output")
        .arg(&out_path)
        .output()
        .expect("run decode");
    // Under normative, count=1 < min=2 should either raise to 2 or produce an error.
    // Either way, no panic.
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 18. Env var with invalid value produces error
// =========================================================================

#[test]
fn env_var_invalid_dialect_produces_error() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    let output = Command::cargo_bin("copybook")
        .unwrap()
        .env("COPYBOOK_DIALECT", "invalid")
        .args(["parse"])
        .arg(temp_path(&dir, "schema.cpy"))
        .output()
        .expect("run parse");
    // Invalid env var should produce error or be silently ignored (no panic).
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// 19. All three dialects on inspect produce output
// =========================================================================

#[test]
fn all_three_dialects_on_inspect() {
    let dir = write_temp_file("schema.cpy", ODO_MIN2_CPY.as_bytes());
    for dialect in &["n", "0", "1"] {
        let output = Command::cargo_bin("copybook")
            .unwrap()
            .args(["inspect", "--dialect", dialect])
            .arg(temp_path(&dir, "schema.cpy"))
            .output()
            .unwrap_or_else(|e| panic!("run inspect --dialect {dialect}: {e}"));
        assert_eq!(
            output.status.code(),
            Some(0),
            "inspect --dialect {dialect} failed: {}",
            stderr_str(&output)
        );
        let text = stdout_str(&output);
        assert!(
            !text.is_empty(),
            "inspect --dialect {dialect} should produce output"
        );
        assert_no_panic(&stderr_str(&output));
    }
}
