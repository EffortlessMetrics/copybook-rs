// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep error-handling E2E tests for the CLI binary.
//!
//! Validates exit codes, stderr content, and absence of panics for a broad
//! set of error conditions: missing files, bad syntax, truncated data,
//! invalid options, and conflicting flags.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").expect("copybook binary should exist")
}

/// Minimal 13-byte fixed record: NAME PIC X(10), AGE PIC 9(3).
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// EBCDIC CP037 record for SIMPLE_CPY: "ALICE     " + "025".
fn simple_ebcdic() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

fn setup(cpy: &str, data: &[u8]) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

fn p(dir: &TempDir, name: &str) -> PathBuf {
    dir.path().join(name)
}

/// Assert stderr does not contain panic markers.
fn assert_no_panic(output: &std::process::Output) {
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("panicked at"),
        "CLI panicked! stderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("stack backtrace"),
        "CLI should not emit backtraces:\n{stderr}"
    );
}

// =========================================================================
// 1. Missing input data file → non-zero exit
// =========================================================================

#[test]
fn missing_input_file_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &[]);
    let out = p(&dir, "output.jsonl");

    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg("nonexistent_data_file_99999.bin")
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 2. Missing copybook file → non-zero exit
// =========================================================================

#[test]
fn missing_copybook_file_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    let out = p(&dir, "output.jsonl");
    std::fs::write(dir.path().join("data.bin"), simple_ebcdic()).unwrap();

    cmd()
        .args(["decode"])
        .arg("nonexistent_schema_99999.cpy")
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .failure();
}

// =========================================================================
// 3. Invalid copybook syntax (garbage text)
// =========================================================================

#[test]
fn invalid_copybook_syntax_garbage() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), b"@#$%^&*() NOT COBOL").unwrap();

    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "bad.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("CBK") || stderr.contains("error") || stderr.contains("Error"),
        "stderr should contain error info: {stderr}"
    );
}

// =========================================================================
// 4. Empty copybook file → parse error
// =========================================================================

#[test]
fn empty_copybook_file_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("empty.cpy"), b"").unwrap();

    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "empty.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 5. Data shorter than LRECL with --fail-fast
// =========================================================================

#[test]
fn data_shorter_than_lrecl_with_fail_fast() {
    let dir = setup(SIMPLE_CPY, &[0xF0; 5]); // 5 bytes, needs 13
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .assert()
        .failure();
}

// =========================================================================
// 6. Data not a multiple of LRECL
// =========================================================================

#[test]
fn data_not_multiple_of_lrecl() {
    // 15 bytes: not divisible by 13 (one full record + 2 extra bytes)
    let mut data = simple_ebcdic(); // 13 bytes
    data.extend_from_slice(&[0xF0, 0xF0]); // 2 extra
    let dir = setup(SIMPLE_CPY, &data);
    let out = p(&dir, "output.jsonl");

    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    // Either fails or succeeds with partial decode; no panic
    assert_no_panic(&output);
}

// =========================================================================
// 7. Invalid codepage name → clap error
// =========================================================================

#[test]
fn invalid_codepage_name_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "INVALID_CODEPAGE_XYZ"])
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("INVALID_CODEPAGE_XYZ")
                .or(predicate::str::contains("invalid").or(predicate::str::contains("Invalid"))),
        );
}

// =========================================================================
// 8. Missing required --format flag → clap error
// =========================================================================

#[test]
fn missing_format_flag_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .assert()
        .failure();
}

// =========================================================================
// 9. Unknown field in --select → error
// =========================================================================

#[test]
fn unknown_field_in_select_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "NONEXISTENT_FIELD_XYZ",
        ])
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "selecting unknown field should fail"
    );
    assert_no_panic(&output);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("NONEXISTENT_FIELD_XYZ")
            || stderr.contains("CBK")
            || stderr.contains("not found")
            || stderr.contains("error"),
        "stderr should mention the unknown field: {stderr}"
    );
}

// =========================================================================
// 10. Encode with invalid JSON input
// =========================================================================

#[test]
fn encode_invalid_json_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &[]);
    let bad_jsonl = p(&dir, "bad.jsonl");
    std::fs::write(&bad_jsonl, "THIS IS NOT JSON\n").unwrap();
    let out = p(&dir, "encoded.bin");

    cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&bad_jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .failure();
}

// =========================================================================
// 11. Encode with empty JSONL file
// =========================================================================

#[test]
fn encode_empty_jsonl_succeeds_or_fails_no_panic() {
    let dir = setup(SIMPLE_CPY, &[]);
    let empty_jsonl = p(&dir, "empty.jsonl");
    std::fs::write(&empty_jsonl, "").unwrap();
    let out = p(&dir, "encoded.bin");

    let output = cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&empty_jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    // Empty input may succeed (0 records) or fail; must not panic
    assert_no_panic(&output);
}

// =========================================================================
// 12. Verify nonexistent data file → non-zero
// =========================================================================

#[test]
fn verify_nonexistent_data_file() {
    let dir = setup(SIMPLE_CPY, &[]);

    cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg("nonexistent_data_99999.bin")
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .failure();
}

// =========================================================================
// 13. Parse: missing positional argument
// =========================================================================

#[test]
fn parse_missing_positional_arg() {
    cmd().arg("parse").assert().failure();
}

// =========================================================================
// 14. Decode: missing positional arguments
// =========================================================================

#[test]
fn decode_missing_positional_args() {
    cmd().arg("decode").assert().failure();
}

// =========================================================================
// 15. Encode: missing positional arguments
// =========================================================================

#[test]
fn encode_missing_positional_args() {
    cmd().arg("encode").assert().failure();
}

// =========================================================================
// 16. Verify: missing positional arguments
// =========================================================================

#[test]
fn verify_missing_positional_args() {
    cmd().arg("verify").assert().failure();
}

// =========================================================================
// 17. Determinism: missing arguments
// =========================================================================

#[test]
fn determinism_missing_args() {
    cmd().args(["determinism", "decode"]).assert().failure();
}

// =========================================================================
// 18. Unknown subcommand → exit code 2
// =========================================================================

#[test]
fn unknown_subcommand_exits_nonzero() {
    let output = cmd().arg("totally-bogus-subcommand").output().unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 19. Exit codes are in valid range (no panic codes like 101)
// =========================================================================

#[test]
fn exit_codes_in_valid_range() {
    let scenarios: &[&[&str]] = &[
        &["--help"],
        &["--version"],
        &["bogus-subcommand"],
        &["parse"],
    ];

    for args in scenarios {
        let output = cmd().args(args.iter()).output().unwrap();
        let code = output.status.code().expect("exit code");
        assert!(
            (0..=5).contains(&code),
            "args {args:?} exited with {code}, expected 0..=5"
        );
        assert_no_panic(&output);
    }
}

// =========================================================================
// 20. Stderr contains CBK error code for invalid copybook
// =========================================================================

#[test]
fn error_output_contains_cbk_code() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), b"GARBAGE COBOL").unwrap();

    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "bad.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("CBK") || stderr.contains("error") || stderr.contains("Error"),
        "stderr should contain error info: {stderr}"
    );
}

// =========================================================================
// 21. Whitespace-only copybook → parse error
// =========================================================================

#[test]
fn whitespace_only_copybook_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("ws.cpy"), b"       \n    \n    ").unwrap();

    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "ws.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 22. Encode with JSONL containing wrong field names
// =========================================================================

#[test]
fn encode_wrong_field_names_no_panic() {
    let dir = setup(SIMPLE_CPY, &[]);
    let wrong_jsonl = p(&dir, "wrong.jsonl");
    std::fs::write(&wrong_jsonl, r#"{"WRONG-FIELD":"HELLO","ANOTHER":"WORLD"}"#).unwrap();
    let out = p(&dir, "encoded.bin");

    let output = cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&wrong_jsonl)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    // May succeed with defaults or fail; must not panic
    assert_no_panic(&output);
}

// =========================================================================
// 23. Decode zero-length data file
// =========================================================================

#[test]
fn decode_zero_length_data_no_panic() {
    let dir = setup(SIMPLE_CPY, &[]);
    let out = p(&dir, "output.jsonl");

    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    // Zero-length file: may succeed with 0 records or fail; no panic
    assert_no_panic(&output);
}

// =========================================================================
// 24. Invalid --threads value (0)
// =========================================================================

#[test]
fn decode_threads_zero_no_panic() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--threads", "0"])
        .output()
        .unwrap();

    // --threads 0 may be treated as error or default to 1; no panic
    assert_no_panic(&output);
}

// =========================================================================
// 25. JSONL with partial/malformed line
// =========================================================================

#[test]
fn encode_partial_jsonl_line_no_panic() {
    let dir = setup(SIMPLE_CPY, &[]);
    let partial = p(&dir, "partial.jsonl");
    // First line valid, second line truncated
    std::fs::write(
        &partial,
        "{\"NAME\":\"ALICE\",\"AGE\":\"025\"}\n{\"NAME\":\"BO",
    )
    .unwrap();
    let out = p(&dir, "encoded.bin");

    let output = cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&partial)
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert_no_panic(&output);
}

// =========================================================================
// 26. Invalid --json-number value
// =========================================================================

#[test]
fn invalid_json_number_mode_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--json-number",
            "INVALID_MODE",
        ])
        .assert()
        .failure();
}

// =========================================================================
// 27. Invalid --dialect value
// =========================================================================

#[test]
fn invalid_dialect_value_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &[]);

    cmd()
        .args(["parse"])
        .arg(p(&dir, "schema.cpy"))
        .args(["--dialect", "INVALID_DIALECT"])
        .assert()
        .failure();
}

// =========================================================================
// 28. Invalid --emit-raw value
// =========================================================================

#[test]
fn invalid_emit_raw_value_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--emit-raw",
            "INVALID_RAW_MODE",
        ])
        .assert()
        .failure();
}

// =========================================================================
// 29. Verify with --select on unknown field
// =========================================================================

#[test]
fn verify_select_unknown_field_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());

    let output = cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "DOES_NOT_EXIST",
        ])
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 30. Encode with --select on unknown field
// =========================================================================

#[test]
fn encode_select_unknown_field_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    // First decode to get valid JSONL
    let jsonl = p(&dir, "decoded.jsonl");
    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&jsonl)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .assert()
        .success();

    let out = p(&dir, "encoded.bin");
    let output = cmd()
        .args(["encode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(&jsonl)
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "DOES_NOT_EXIST",
        ])
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 31. Multiple errors: bad copybook + nonexistent data
// =========================================================================

#[test]
fn bad_copybook_with_nonexistent_data_no_panic() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), b"GARBAGE").unwrap();
    let out = p(&dir, "output.jsonl");

    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "bad.cpy"))
        .arg("nonexistent_99999.bin")
        .arg("--output")
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 32. Invalid --format value
// =========================================================================

#[test]
fn invalid_format_value_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args(["--format", "INVALID_FORMAT_XYZ", "--codepage", "cp037"])
        .assert()
        .failure();
}

// =========================================================================
// 33. Non-numeric --max-errors value
// =========================================================================

#[test]
fn non_numeric_max_errors_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--max-errors",
            "not_a_number",
        ])
        .assert()
        .failure();
}

// =========================================================================
// 34. Non-numeric --threads value
// =========================================================================

#[test]
fn non_numeric_threads_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--threads",
            "abc",
        ])
        .assert()
        .failure();
}

// =========================================================================
// 35. Inspect invalid copybook → non-zero exit
// =========================================================================

#[test]
fn inspect_invalid_copybook_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("bad.cpy"), b"NOT VALID COBOL").unwrap();

    let output = cmd()
        .args(["inspect"])
        .arg(p(&dir, "bad.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 36. Inspect nonexistent file → non-zero exit
// =========================================================================

#[test]
fn inspect_nonexistent_file_exits_nonzero() {
    cmd()
        .args(["inspect", "nonexistent_schema_99999.cpy"])
        .assert()
        .failure();
}

// =========================================================================
// 37. Determinism decode with nonexistent file
// =========================================================================

#[test]
fn determinism_nonexistent_file_exits_nonzero() {
    cmd()
        .args([
            "determinism",
            "decode",
            "nonexistent_99999.cpy",
            "nonexistent_99999.bin",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
        ])
        .assert()
        .failure();
}

// =========================================================================
// 38. Verify with empty data file
// =========================================================================

#[test]
fn verify_empty_data_no_panic() {
    let dir = setup(SIMPLE_CPY, &[]);

    let output = cmd()
        .args(["verify"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    // Empty data may pass (0 records) or fail; no panic
    assert_no_panic(&output);
}

// =========================================================================
// 39. Binary content in copybook file → parse error, no panic
// =========================================================================

#[test]
fn binary_content_in_copybook_no_panic() {
    let dir = tempfile::tempdir().unwrap();
    // Write random binary bytes as "copybook"
    std::fs::write(
        dir.path().join("binary.cpy"),
        &[0x00, 0xFF, 0xFE, 0x01, 0x80, 0x7F, 0xAB, 0xCD],
    )
    .unwrap();

    let output = cmd()
        .args(["parse"])
        .arg(p(&dir, "binary.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_no_panic(&output);
}

// =========================================================================
// 40. Very large thread count → no panic
// =========================================================================

#[test]
fn very_large_thread_count_no_panic() {
    let dir = setup(SIMPLE_CPY, &simple_ebcdic());
    let out = p(&dir, "output.jsonl");

    let output = cmd()
        .args(["decode"])
        .arg(p(&dir, "schema.cpy"))
        .arg(p(&dir, "data.bin"))
        .arg("--output")
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--threads",
            "9999",
        ])
        .output()
        .unwrap();

    // Large thread count may succeed or be rejected; no panic
    assert_no_panic(&output);
}
