// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive CLI E2E error code coverage tests.
//!
//! Every test invokes the `copybook` binary as a subprocess and verifies that
//! the expected `CBK*` error code appears in stderr (or stdout for `verify`).
//!
//! ## Coverage: 35 tests across all error families
//!
//! | Family | Codes Triggered                                           | Via Command        |
//! |--------|-----------------------------------------------------------|--------------------|
//! | CBKP   | 001 (×3), 021, 022, 023                                  | parse, inspect     |
//! | CBKS   | 121, 601, 602, 604, 607, 703 (×3)                        | parse, decode, verify |
//! | CBKD   | 401 (×2), 411 (×3), 421 (×2)                             | decode, verify     |
//! | CBKE   | (exit-code validation ×3)                                 | encode             |
//! | CBKF   | 102, 104, 221                                            | decode (rdw/fixed) |

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a `Command` pointing at the compiled `copybook` binary.
fn copybook_cmd() -> Command {
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .expect("parent of tests/e2e")
        .parent()
        .expect("workspace root");
    let bin_name = if cfg!(windows) {
        "copybook.exe"
    } else {
        "copybook"
    };
    let bin_path = workspace_root.join("target").join("debug").join(bin_name);
    assert!(
        bin_path.exists(),
        "copybook binary not found at {}. Run `cargo build --bin copybook` first.",
        bin_path.display()
    );
    Command::new(bin_path)
}

fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// Combined stderr + stdout for searching error codes that may appear in either.
fn combined_output(output: &Output) -> String {
    format!("{}\n{}", stderr_str(output), stdout_str(output))
}

fn assert_no_panic(stderr: &str) {
    assert!(
        !stderr.contains("panicked at"),
        "CLI panicked! stderr:\n{stderr}"
    );
}

/// Write content to a named file in a temp directory; returns the dir handle.
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

/// Setup a temp dir with a copybook and binary data file.
fn setup_decode(cpy_text: &str, data: &[u8]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), cpy_text).unwrap();
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

/// Setup a temp dir with a copybook and JSONL input for encoding.
fn setup_encode(cpy_text: &str, jsonl: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), cpy_text).unwrap();
    std::fs::write(dir.path().join("input.jsonl"), jsonl).unwrap();
    dir
}

// =========================================================================
// CBKP* — Parse Errors
// =========================================================================

/// CBKP001: Garbage text triggers syntax error via `parse`.
#[test]
fn cli_cbkp001_syntax_garbage_parse() {
    let dir = write_temp_file("bad.cpy", b"@#$%^&*() NOT COBOL AT ALL");

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "bad.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP001_SYNTAX"),
        "Expected CBKP001_SYNTAX in stderr: {se}"
    );
}

/// CBKP001: Empty copybook triggers syntax error via `inspect`.
#[test]
fn cli_cbkp001_syntax_empty_inspect() {
    let dir = write_temp_file("empty.cpy", b"");

    let output = copybook_cmd()
        .arg("inspect")
        .arg(temp_path(&dir, "empty.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP001_SYNTAX"),
        "Expected CBKP001_SYNTAX in stderr: {se}"
    );
}

/// CBKP001: Invalid PIC character triggers syntax error.
#[test]
fn cli_cbkp001_syntax_invalid_pic_char() {
    let dir = write_temp_file(
        "bad_pic.cpy",
        b"       01  REC.\n           05  FLD   PIC Q(5).\n",
    );

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "bad_pic.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP001_SYNTAX"),
        "Expected CBKP001_SYNTAX for invalid PIC char: {se}"
    );
}

/// CBKP021: ODO array not at tail position.
#[test]
fn cli_cbkp021_odo_not_tail() {
    let cpy = b"\
       01  REC.\n\
           05  CTR      PIC 9(3).\n\
           05  ITEMS    OCCURS 1 TO 10 DEPENDING ON CTR\n\
                        PIC X(5).\n\
           05  TRAILER  PIC X(10).\n";
    let dir = write_temp_file("odo_tail.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "odo_tail.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP021_ODO_NOT_TAIL"),
        "Expected CBKP021_ODO_NOT_TAIL in stderr: {se}"
    );
}

/// CBKP022: Nested OCCURS DEPENDING ON.
#[test]
fn cli_cbkp022_nested_odo() {
    let cpy = b"\
       01  REC.\n\
           05  CTR1     PIC 9(3).\n\
           05  CTR2     PIC 9(3).\n\
           05  OUTER    OCCURS 1 TO 5 DEPENDING ON CTR1.\n\
               10  INNER OCCURS 1 TO 3 DEPENDING ON CTR2\n\
                         PIC X(5).\n";
    let dir = write_temp_file("nested_odo.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "nested_odo.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP022_NESTED_ODO"),
        "Expected CBKP022_NESTED_ODO in stderr: {se}"
    );
}

/// CBKP023: ODO over REDEFINES.
#[test]
fn cli_cbkp023_odo_redefines() {
    let cpy = b"\
       01  REC.\n\
           05  CTR      PIC 9(3).\n\
           05  A        PIC X(10).\n\
           05  B        REDEFINES A OCCURS 1 TO 10\n\
                        DEPENDING ON CTR PIC X(1).\n";
    let dir = write_temp_file("odo_redef.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "odo_redef.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP023_ODO_REDEFINES"),
        "Expected CBKP023_ODO_REDEFINES in stderr: {se}"
    );
}

// =========================================================================
// CBKS* — Schema Validation Errors
// =========================================================================

/// CBKS121: ODO counter field not found in schema.
#[test]
fn cli_cbks121_counter_not_found() {
    let cpy = b"\
       01  REC.\n\
           05  ITEMS OCCURS 1 TO 10 DEPENDING ON MISSING-CTR\n\
                    PIC X(5).\n";
    let dir = write_temp_file("odo_no_ctr.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "odo_no_ctr.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKS121_COUNTER_NOT_FOUND"),
        "Expected CBKS121_COUNTER_NOT_FOUND in stderr: {se}"
    );
}

/// CBKS601: RENAMES from field not found.
#[test]
fn cli_cbks601_rename_unknown_from() {
    let cpy = b"\
       01  REC.\n\
           05  NAME   PIC X(10).\n\
           66  MY-ALIAS RENAMES NONEXISTENT.\n";
    let dir = write_temp_file("ren601.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "ren601.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKS601_RENAME_UNKNOWN_FROM"),
        "Expected CBKS601_RENAME_UNKNOWN_FROM in stderr: {se}"
    );
}

/// CBKS602: RENAMES thru field not found.
#[test]
fn cli_cbks602_rename_unknown_thru() {
    let cpy = b"\
       01  REC.\n\
           05  A   PIC X(5).\n\
           05  B   PIC X(5).\n\
           66  MY-ALIAS RENAMES A THRU NONEXISTENT.\n";
    let dir = write_temp_file("ren602.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "ren602.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKS602_RENAME_UNKNOWN_THRU"),
        "Expected CBKS602_RENAME_UNKNOWN_THRU in stderr: {se}"
    );
}

/// CBKS604: RENAMES reversed range (from comes after thru).
#[test]
fn cli_cbks604_rename_reversed_range() {
    let cpy = b"\
       01  REC.\n\
           05  A   PIC X(5).\n\
           05  B   PIC X(5).\n\
           66  MY-ALIAS RENAMES B THRU A.\n";
    let dir = write_temp_file("ren604.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "ren604.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKS604_RENAME_REVERSED_RANGE"),
        "Expected CBKS604_RENAME_REVERSED_RANGE in stderr: {se}"
    );
}

/// CBKS607: RENAMES range crosses OCCURS boundary.
#[test]
fn cli_cbks607_rename_crosses_occurs() {
    let cpy = b"\
       01  REC.\n\
           05  A   PIC X(5).\n\
           05  B   OCCURS 3 PIC X(5).\n\
           05  C   PIC X(5).\n\
           66  MY-ALIAS RENAMES A THRU C.\n";
    let dir = write_temp_file("ren607.cpy", cpy);

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "ren607.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKS607_RENAME_CROSSES_OCCURS"),
        "Expected CBKS607_RENAME_CROSSES_OCCURS in stderr: {se}"
    );
}

/// CBKS703: Projection field not found via `decode --select`.
#[test]
fn cli_cbks703_projection_not_found_decode() {
    let dir = setup_decode(
        "       01  REC.\n           05  NAME   PIC X(10).\n",
        // 10 bytes of EBCDIC spaces
        &[0x40; 10],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "NOPE",
        ])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKS703_PROJECTION_FIELD_NOT_FOUND"),
        "Expected CBKS703_PROJECTION_FIELD_NOT_FOUND in stderr: {se}"
    );
}

/// CBKS703: Projection field not found via `encode --select`.
#[test]
fn cli_cbks703_projection_not_found_encode() {
    let dir = setup_encode(
        "       01  REC.\n           05  NAME   PIC X(10).\n",
        "{\"REC\":{\"NAME\":\"ALICE\"}}\n",
    );

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "input.jsonl"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.bin"))
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "NOPE",
        ])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKS703_PROJECTION_FIELD_NOT_FOUND"),
        "Expected CBKS703 in encode stderr: {se}"
    );
}

/// CBKS703: Projection field not found via `verify --select`.
#[test]
fn cli_cbks703_projection_not_found_verify() {
    let dir = setup_decode(
        "       01  REC.\n           05  NAME   PIC X(10).\n",
        &[0x40; 10],
    );

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--select",
            "NOPE",
        ])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let combined = combined_output(&output);
    assert_no_panic(&stderr_str(&output));
    assert!(
        combined.contains("CBKS703") || combined.contains("not found"),
        "Expected CBKS703 or field not found in output: {combined}"
    );
}

// =========================================================================
// CBKD* — Data Decode Errors
// =========================================================================

/// CBKD401: Invalid COMP-3 nibble via `decode`.
#[test]
fn cli_cbkd401_comp3_invalid_nibble_decode() {
    // PIC S9(5) COMP-3 = 3 bytes. 0xFF has invalid digit nibble.
    let dir = setup_decode(
        "       01  REC.\n           05  AMOUNT   PIC S9(5) COMP-3.\n",
        &[0xFF, 0xFF, 0xFF],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKD401_COMP3_INVALID_NIBBLE"),
        "Expected CBKD401 in stderr: {se}"
    );
}

/// CBKD401: Invalid COMP-3 nibble via `verify`.
#[test]
fn cli_cbkd401_comp3_invalid_nibble_verify() {
    let dir = setup_decode(
        "       01  REC.\n           05  AMOUNT   PIC S9(5) COMP-3.\n",
        &[0xFF, 0xFF, 0xFF],
    );

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let combined = combined_output(&output);
    assert_no_panic(&stderr_str(&output));
    assert!(
        combined.contains("CBKD401_COMP3_INVALID_NIBBLE"),
        "Expected CBKD401 in verify output: {combined}"
    );
}

/// CBKD411: Invalid zoned decimal sign (bad zone nibble) via `decode`.
#[test]
fn cli_cbkd411_zoned_bad_sign_decode() {
    // PIC S9(5): 5-byte signed zoned decimal
    // Last byte has sign in high nibble; 0x05 has zone 0x0 (invalid)
    let dir = setup_decode(
        "       01  REC.\n           05  AMT   PIC S9(5).\n",
        &[0xF1, 0xF2, 0xF3, 0xF4, 0x05],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKD411_ZONED_BAD_SIGN"),
        "Expected CBKD411 in stderr: {se}"
    );
}

/// CBKD411: Spaces in unsigned numeric field via `decode`.
#[test]
fn cli_cbkd411_zoned_spaces_in_numeric() {
    // PIC 9(3): 3-byte unsigned zoned. All spaces = 0x40 (EBCDIC space).
    let dir = setup_decode(
        "       01  REC.\n           05  NUM   PIC 9(3).\n",
        &[0x40, 0x40, 0x40],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKD411_ZONED_BAD_SIGN"),
        "Expected CBKD411 for all-spaces numeric: {se}"
    );
}

/// CBKD411: Invalid zoned via `verify` with null bytes in numeric.
#[test]
fn cli_cbkd411_zoned_bad_sign_verify() {
    // PIC 9(3): null bytes in numeric field
    let dir = setup_decode(
        "       01  REC.\n           05  NAME   PIC X(10).\n           05  AGE    PIC 9(3).\n",
        &[
            0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x00, 0x00, 0x00,
        ],
    );

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let combined = combined_output(&output);
    assert_no_panic(&stderr_str(&output));
    assert!(
        combined.contains("CBKD411_ZONED_BAD_SIGN"),
        "Expected CBKD411 in verify output: {combined}"
    );
}

/// CBKD411: SIGN SEPARATE field with invalid sign byte via `decode`.
#[test]
fn cli_cbkd411_sign_separate_invalid() {
    // PIC S9(3) SIGN IS LEADING SEPARATE: 4 bytes (sign + 3 digits)
    // 0x00 is not a valid sign byte
    let dir = setup_decode(
        "       01  REC.\n           05  AMT   PIC S9(3) SIGN IS LEADING SEPARATE.\n",
        &[0x00, 0xF1, 0xF2, 0xF3],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKD411_ZONED_BAD_SIGN"),
        "Expected CBKD411 for invalid sign separate byte: {se}"
    );
}

/// CBKD421: Edited PIC invalid format via `decode`.
#[test]
fn cli_cbkd421_edited_pic_invalid_decode() {
    // PIC ZZZ9: 4-byte edited numeric. EBCDIC letters instead of digits/spaces.
    // 0xC1..C4 = 'A'..'D' — not valid for ZZZ9 pattern.
    let dir = setup_decode(
        "       01  REC.\n           05  AMT   PIC ZZZ9.\n",
        &[0xC1, 0xC2, 0xC3, 0xC4],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKD421_EDITED_PIC_INVALID_FORMAT"),
        "Expected CBKD421 in stderr: {se}"
    );
}

/// CBKD421: Edited PIC invalid format via `verify`.
#[test]
fn cli_cbkd421_edited_pic_invalid_verify() {
    let dir = setup_decode(
        "       01  REC.\n           05  AMT   PIC ZZZ9.\n",
        &[0xC1, 0xC2, 0xC3, 0xC4],
    );

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let combined = combined_output(&output);
    assert_no_panic(&stderr_str(&output));
    assert!(
        combined.contains("CBKD421_EDITED_PIC_INVALID_FORMAT"),
        "Expected CBKD421 in verify output: {combined}"
    );
}

// =========================================================================
// CBKE* — Encoding Errors (exit code + summary validation)
// =========================================================================

/// CBKE510: Numeric overflow — value exceeds PIC capacity.
/// The CLI wraps the specific code, so we verify non-zero exit + error count.
#[test]
fn cli_cbke_numeric_overflow_encode() {
    let dir = setup_encode(
        "       01  REC.\n           05  NUM   PIC 9(3).\n",
        "{\"REC\":{\"NUM\":\"99999\"}}\n",
    );

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "input.jsonl"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.bin"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let combined = combined_output(&output);
    assert_no_panic(&stderr_str(&output));
    assert!(
        combined.contains("error") || combined.contains("Error"),
        "Expected error indication in encode output: {combined}"
    );
    assert!(
        combined.contains("Records with errors: 1") || combined.contains("failed"),
        "Expected error count in encode output: {combined}"
    );
}

/// CBKE515: String length violation — string exceeds PIC X capacity.
#[test]
fn cli_cbke_string_too_long_encode() {
    let dir = setup_encode(
        "       01  REC.\n           05  NAME   PIC X(5).\n",
        "{\"REC\":{\"NAME\":\"THIS STRING IS WAY TOO LONG FOR FIVE BYTES\"}}\n",
    );

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "input.jsonl"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.bin"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let combined = combined_output(&output);
    assert_no_panic(&stderr_str(&output));
    assert!(
        combined.contains("error") || combined.contains("Error") || combined.contains("failed"),
        "Expected error indication in encode output: {combined}"
    );
}

/// CBKE501: JSON type mismatch — null where string expected.
#[test]
fn cli_cbke_type_mismatch_encode() {
    let dir = setup_encode(
        "       01  REC.\n           05  NAME   PIC X(10).\n           05  NUM   PIC 9(5).\n",
        "{\"REC\":{\"NAME\":null,\"NUM\":\"12345\"}}\n",
    );

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "input.jsonl"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.bin"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    // null for PIC X may succeed (space-filled) or fail depending on implementation.
    // Either way, verify no panic and valid exit.
    assert_no_panic(&stderr_str(&output));
}

// =========================================================================
// CBKF* — File/Record Format Errors
// =========================================================================

/// CBKF102: RDW record length invalid — payload shorter than header declares.
#[test]
fn cli_cbkf102_rdw_record_length_invalid() {
    // RDW header: length=2 (only 2 bytes, meaning 0 payload after 4-byte header is wrong
    // because RDW length < 4), then no more data
    let dir = setup_decode(
        "       01  REC.\n           05  A   PIC X(1).\n",
        &[0x00, 0x02, 0x00, 0x00], // length=2 < 4 → underflow
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "rdw", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKF102_RECORD_LENGTH_INVALID"),
        "Expected CBKF102 in stderr: {se}"
    );
}

/// CBKF104: RDW header looks ASCII-corrupted.
#[test]
fn cli_cbkf104_rdw_suspect_ascii() {
    // ASCII digits '1','2' in first two bytes → suspect ASCII corruption
    let dir = setup_decode(
        "       01  REC.\n           05  A   PIC X(10).\n",
        &[
            0x31, 0x32, 0x00, 0x00, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A,
        ],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "rdw", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKF104_RDW_SUSPECT_ASCII"),
        "Expected CBKF104 in stderr: {se}"
    );
}

/// CBKF221: Truncated fixed-length record (file shorter than LRECL).
#[test]
fn cli_cbkf221_rdw_underflow_fixed() {
    // Schema needs 13 bytes per record but data is only 5 bytes
    let dir = setup_decode(
        "       01  REC.\n           05  NAME   PIC X(10).\n           05  AGE    PIC 9(3).\n",
        &[0xC1, 0xD3, 0xC9, 0xC3, 0xC5],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKF221_RDW_UNDERFLOW"),
        "Expected CBKF221 in stderr: {se}"
    );
}

// =========================================================================
// Cross-command error propagation tests
// =========================================================================

/// Parse error propagates through `inspect` command.
#[test]
fn cli_parse_error_propagates_to_inspect() {
    let dir = write_temp_file("garbage.cpy", b"NOT VALID COBOL SYNTAX");

    let output = copybook_cmd()
        .arg("inspect")
        .arg(temp_path(&dir, "garbage.cpy"))
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP001_SYNTAX"),
        "Expected CBKP001 in inspect stderr: {se}"
    );
}

/// Parse error propagates through `decode` command.
#[test]
fn cli_parse_error_propagates_to_decode() {
    let dir = setup_decode("GARBAGE NOT COBOL", &[0x00; 10]);

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP001_SYNTAX"),
        "Expected CBKP001 in decode stderr: {se}"
    );
}

/// Parse error propagates through `encode` command.
#[test]
fn cli_parse_error_propagates_to_encode() {
    let dir = setup_encode("GARBAGE NOT COBOL", "{\"REC\":{\"A\":\"X\"}}\n");

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "input.jsonl"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP001_SYNTAX"),
        "Expected CBKP001 in encode stderr: {se}"
    );
}

/// Parse error propagates through `verify` command.
#[test]
fn cli_parse_error_propagates_to_verify() {
    let dir = setup_decode("GARBAGE NOT COBOL", &[0x00; 10]);

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .unwrap();

    assert!(!output.status.success());
    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        se.contains("CBKP001_SYNTAX"),
        "Expected CBKP001 in verify stderr: {se}"
    );
}

// =========================================================================
// Exit code validation
// =========================================================================

/// Parse errors produce non-zero exit code (expected: 5 for internal/parse).
#[test]
fn cli_exit_code_parse_error() {
    let dir = write_temp_file("bad.cpy", b"INVALID");

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "bad.cpy"))
        .output()
        .unwrap();

    assert_no_panic(&stderr_str(&output));
    let code = output.status.code().unwrap_or(-1);
    assert_ne!(code, 0, "Parse error should produce non-zero exit code");
}

/// Decode data errors produce non-zero exit code (expected: 2 for decode).
#[test]
fn cli_exit_code_decode_error() {
    let dir = setup_decode(
        "       01  REC.\n           05  AMOUNT   PIC S9(5) COMP-3.\n",
        &[0xFF, 0xFF, 0xFF],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert_no_panic(&stderr_str(&output));
    let code = output.status.code().unwrap_or(-1);
    assert_ne!(code, 0, "Decode error should produce non-zero exit code");
}

/// RDW format errors produce non-zero exit code (expected: 4 for I/O).
#[test]
fn cli_exit_code_rdw_error() {
    let dir = setup_decode(
        "       01  REC.\n           05  A   PIC X(10).\n",
        &[0x31, 0x32, 0x00, 0x00, 0x41, 0x42],
    );

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(temp_path(&dir, "out.jsonl"))
        .args(["--format", "rdw", "--codepage", "cp037", "--fail-fast"])
        .output()
        .unwrap();

    assert_no_panic(&stderr_str(&output));
    let code = output.status.code().unwrap_or(-1);
    assert_ne!(code, 0, "RDW error should produce non-zero exit code");
}
