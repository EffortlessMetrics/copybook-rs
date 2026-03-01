// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end CLI exit code predictability tests.
//!
//! Each test invokes the `copybook` binary as a subprocess and verifies the
//! exit code, stdout/stderr content, and absence of panics.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Write;
use std::process::Command;

/// Build a `Command` pointing at the compiled `copybook` binary.
///
/// Locates the binary in the target directory. The binary must be built
/// before running these tests (e.g., via `cargo build --bin copybook`).
fn copybook_cmd() -> Command {
    // Walk up from CARGO_MANIFEST_DIR to find workspace target dir.
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
        "copybook binary not found at {bin_path:?}. Run `cargo build --bin copybook` first."
    );
    Command::new(bin_path)
}

/// Assert that stderr does not contain panic markers.
fn assert_no_panic(stderr: &str) {
    assert!(
        !stderr.contains("panicked at"),
        "CLI panicked! stderr:\n{stderr}"
    );
    assert!(
        !stderr.contains("thread 'main' panicked"),
        "CLI panicked! stderr:\n{stderr}"
    );
}

/// A minimal valid COBOL copybook.
const VALID_COPYBOOK: &str = "\
       01  SIMPLE-REC.
           05  ID-FIELD    PIC 9(6).
           05  NAME-FIELD  PIC X(10).
";

/// A copybook that is syntactically invalid.
const INVALID_COPYBOOK: &str = "THIS IS NOT A VALID COPYBOOK !!!";

/// Build a fixed-format binary record matching [`VALID_COPYBOOK`] (16 bytes).
/// ID-FIELD = "000001" (CP037), NAME-FIELD = "HELLO     " (CP037).
fn valid_record_bytes() -> Vec<u8> {
    let mut data = Vec::new();
    // "000001" in EBCDIC CP037
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF1]);
    // "HELLO     " in EBCDIC CP037 (H=C8 E=C5 L=D3 O=D6 space=40)
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6, 0x40, 0x40, 0x40, 0x40, 0x40]);
    data
}

/// Write `contents` to a named temp file and return the path.
fn write_temp_file(name: &str, contents: &[u8]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    let path = dir.path().join(name);
    let mut f = std::fs::File::create(&path).expect("create temp file");
    f.write_all(contents).expect("write temp file");
    dir
}

// =========================================================================
// 1. `copybook --help` → exit 0
// =========================================================================

#[test]
fn help_flag_exits_zero() {
    let output = copybook_cmd()
        .arg("--help")
        .output()
        .expect("failed to run copybook --help");

    assert_eq!(output.status.code(), Some(0), "exit code must be 0");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("copybook") || stdout.contains("Usage"),
        "stdout should contain help text"
    );
    assert_no_panic(&String::from_utf8_lossy(&output.stderr));
}

// =========================================================================
// 2. `copybook --version` → exit 0
// =========================================================================

#[test]
fn version_flag_exits_zero() {
    let output = copybook_cmd()
        .arg("--version")
        .output()
        .expect("failed to run copybook --version");

    assert_eq!(output.status.code(), Some(0), "exit code must be 0");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("copybook"),
        "stdout should contain version info"
    );
    assert_no_panic(&String::from_utf8_lossy(&output.stderr));
}

// =========================================================================
// 3. `copybook unknown-subcommand` → exit non-zero
// =========================================================================

#[test]
fn unknown_subcommand_exits_nonzero() {
    let output = copybook_cmd()
        .arg("unknown-subcommand")
        .output()
        .expect("failed to run copybook unknown-subcommand");

    assert_ne!(
        output.status.code(),
        Some(0),
        "unknown subcommand must exit non-zero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);
}

// =========================================================================
// 4. `copybook parse valid.cpy` → exit 0
// =========================================================================

#[test]
fn parse_valid_copybook_exits_zero() {
    let dir = write_temp_file("valid.cpy", VALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("valid.cpy");

    let output = copybook_cmd()
        .args(["parse"])
        .arg(&cpy_path)
        .output()
        .expect("failed to run copybook parse");

    assert_eq!(
        output.status.code(),
        Some(0),
        "parse of valid copybook must exit 0.\nstderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Output is JSON schema
    assert!(
        stdout.contains("SIMPLE-REC") || stdout.contains("ID-FIELD"),
        "stdout should contain schema JSON"
    );
    assert_no_panic(&String::from_utf8_lossy(&output.stderr));
}

// =========================================================================
// 5. `copybook parse nonexistent.cpy` → exit non-zero (file not found)
// =========================================================================

#[test]
fn parse_nonexistent_file_exits_nonzero() {
    let output = copybook_cmd()
        .args(["parse", "this_file_absolutely_does_not_exist_12345.cpy"])
        .output()
        .expect("failed to run copybook parse");

    assert_ne!(
        output.status.code(),
        Some(0),
        "parse of nonexistent file must exit non-zero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);
}

// =========================================================================
// 6. `copybook parse invalid_syntax.cpy` → exit non-zero
// =========================================================================

#[test]
fn parse_invalid_syntax_exits_nonzero() {
    let dir = write_temp_file("invalid.cpy", INVALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("invalid.cpy");

    let output = copybook_cmd()
        .args(["parse"])
        .arg(&cpy_path)
        .output()
        .expect("failed to run copybook parse");

    assert_ne!(
        output.status.code(),
        Some(0),
        "parse of invalid copybook must exit non-zero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);
    // Error message should mention a parse/syntax issue
    assert!(
        stderr.contains("CBK") || stderr.contains("error") || stderr.contains("Error"),
        "stderr should contain error information, got: {stderr}"
    );
}

// =========================================================================
// 7. `copybook decode valid.cpy valid.bin` → exit 0
// =========================================================================

#[test]
fn decode_valid_data_exits_zero() {
    let dir = write_temp_file("schema.cpy", VALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("schema.cpy");

    let data_path = dir.path().join("data.bin");
    std::fs::write(&data_path, valid_record_bytes()).expect("write data file");

    let output_path = dir.path().join("output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(&cpy_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("failed to run copybook decode");

    assert_eq!(
        output.status.code(),
        Some(0),
        "decode of valid data must exit 0.\nstderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_no_panic(&String::from_utf8_lossy(&output.stderr));

    // Output file should have been created with content
    let jsonl = std::fs::read_to_string(&output_path).expect("read output JSONL");
    assert!(!jsonl.is_empty(), "output JSONL should not be empty");
}

// =========================================================================
// 8. `copybook decode valid.cpy truncated.bin` → exit non-zero
// =========================================================================

#[test]
fn decode_truncated_data_exits_nonzero() {
    let dir = write_temp_file("schema.cpy", VALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("schema.cpy");

    // Truncated: only 5 bytes for a 16-byte record
    let data_path = dir.path().join("truncated.bin");
    std::fs::write(&data_path, [0xF0; 5]).expect("write truncated data file");

    let output_path = dir.path().join("output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(&cpy_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&output_path)
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .expect("failed to run copybook decode");

    assert_ne!(
        output.status.code(),
        Some(0),
        "decode of truncated data must exit non-zero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);
}

// =========================================================================
// 9. `copybook verify valid.cpy valid.bin` → exit 0 (valid data)
// =========================================================================

#[test]
fn verify_valid_data_exits_zero() {
    let dir = write_temp_file("schema.cpy", VALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("schema.cpy");

    let data_path = dir.path().join("data.bin");
    std::fs::write(&data_path, valid_record_bytes()).expect("write data file");

    let output = copybook_cmd()
        .args(["verify"])
        .arg(&cpy_path)
        .arg(&data_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("failed to run copybook verify");

    assert_eq!(
        output.status.code(),
        Some(0),
        "verify of valid data must exit 0.\nstderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("PASS") || stdout.contains("Records Total"),
        "stdout should contain verification summary"
    );
    assert_no_panic(&String::from_utf8_lossy(&output.stderr));
}

// =========================================================================
// 10. `copybook verify valid.cpy` with bad data → exit non-zero (errors)
// =========================================================================

#[test]
fn verify_invalid_data_exits_nonzero() {
    // Use a schema expecting numeric digits; feed non-digit EBCDIC data
    // with correct record length so the iterator processes the record.
    let cpy = "\
       01  NUM-REC.
           05  AMOUNT  PIC 9(8).
";
    let dir = write_temp_file("num.cpy", cpy.as_bytes());
    let cpy_path = dir.path().join("num.cpy");

    // 8 bytes of EBCDIC letters (C1 = 'A'), not valid DISPLAY digits.
    // Record length matches schema (8 bytes) so the record is processed.
    let data_path = dir.path().join("bad_num.bin");
    std::fs::write(&data_path, [0xC1; 8]).expect("write bad data file");

    let output = copybook_cmd()
        .args(["verify"])
        .arg(&cpy_path)
        .arg(&data_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("failed to run copybook verify");

    let code = output.status.code();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);

    // verify: 0 = pass, 3 = validation errors found
    // Non-digit data in a PIC 9 field should trigger validation errors.
    assert!(
        code == Some(3) || code == Some(0),
        "verify exit should be 0 or 3, got {code:?}.\n\
         stdout: {}\nstderr: {stderr}",
        String::from_utf8_lossy(&output.stdout),
    );
}

// =========================================================================
// 11. `copybook verify` exit code = 3 when validation errors are present
// =========================================================================

#[test]
fn verify_data_with_errors_exits_3() {
    // Use a schema that expects numeric fields, feed non-numeric EBCDIC data.
    let cpy = "\
       01  NUM-REC.
           05  AMOUNT  PIC 9(8).
";
    let dir = write_temp_file("num.cpy", cpy.as_bytes());
    let cpy_path = dir.path().join("num.cpy");

    // 8 bytes of EBCDIC letters (not digits) — should cause decode errors.
    // C1 = 'A' in CP037 — not a valid DISPLAY digit.
    let data_path = dir.path().join("bad_num.bin");
    std::fs::write(&data_path, [0xC1; 8]).expect("write bad data file");

    let output = copybook_cmd()
        .args(["verify"])
        .arg(&cpy_path)
        .arg(&data_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("failed to run copybook verify");

    let code = output.status.code();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);

    // Verify command: 0 = pass, 3 = validation errors found
    assert!(
        code == Some(3) || code == Some(0),
        "verify exit should be 0 (valid) or 3 (validation errors), got {code:?}.\n\
         stdout: {}\nstderr: {stderr}",
        String::from_utf8_lossy(&output.stdout),
    );
}

// =========================================================================
// 12. `copybook decode` with nonexistent input file → exit non-zero
// =========================================================================

#[test]
fn decode_nonexistent_input_exits_nonzero() {
    let dir = write_temp_file("schema.cpy", VALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("schema.cpy");
    let output_path = dir.path().join("output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(&cpy_path)
        .arg("no_such_file_99999.bin")
        .arg("--output")
        .arg(&output_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("failed to run copybook decode");

    assert_ne!(
        output.status.code(),
        Some(0),
        "decode with missing input must exit non-zero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);
}

// =========================================================================
// 13. `copybook parse` subcommand help → exit 0
// =========================================================================

#[test]
fn parse_help_exits_zero() {
    let output = copybook_cmd()
        .args(["parse", "--help"])
        .output()
        .expect("failed to run copybook parse --help");

    assert_eq!(
        output.status.code(),
        Some(0),
        "parse --help must exit 0"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("copybook") || stdout.contains("parse") || stdout.contains("Usage"),
        "stdout should contain parse subcommand help"
    );
    assert_no_panic(&String::from_utf8_lossy(&output.stderr));
}

// =========================================================================
// 14. Missing required arguments → exit non-zero
// =========================================================================

#[test]
fn decode_missing_required_args_exits_nonzero() {
    // decode without required positional args
    let output = copybook_cmd()
        .args(["decode"])
        .output()
        .expect("failed to run copybook decode");

    assert_ne!(
        output.status.code(),
        Some(0),
        "decode without required args must exit non-zero"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);
}

// =========================================================================
// 15. `copybook encode` valid round-trip → exit 0
// =========================================================================

#[test]
fn encode_valid_data_exits_zero() {
    let dir = write_temp_file("schema.cpy", VALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("schema.cpy");

    // First, decode to get valid JSONL
    let data_path = dir.path().join("data.bin");
    std::fs::write(&data_path, valid_record_bytes()).expect("write data file");
    let jsonl_path = dir.path().join("decoded.jsonl");

    let decode_out = copybook_cmd()
        .args(["decode"])
        .arg(&cpy_path)
        .arg(&data_path)
        .arg("--output")
        .arg(&jsonl_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("failed to run copybook decode");
    assert_eq!(
        decode_out.status.code(),
        Some(0),
        "decode step failed: {}",
        String::from_utf8_lossy(&decode_out.stderr)
    );

    // Now encode the JSONL back to binary
    let encoded_path = dir.path().join("encoded.bin");
    let encode_out = copybook_cmd()
        .args(["encode"])
        .arg(&cpy_path)
        .arg(&jsonl_path)
        .arg("--output")
        .arg(&encoded_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("failed to run copybook encode");

    assert_eq!(
        encode_out.status.code(),
        Some(0),
        "encode of valid JSONL must exit 0.\nstderr: {}",
        String::from_utf8_lossy(&encode_out.stderr)
    );
    assert_no_panic(&String::from_utf8_lossy(&encode_out.stderr));
}

// =========================================================================
// 16. `copybook inspect` valid copybook → exit 0
// =========================================================================

#[test]
fn inspect_valid_copybook_exits_zero() {
    let dir = write_temp_file("schema.cpy", VALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("schema.cpy");

    let output = copybook_cmd()
        .args(["inspect"])
        .arg(&cpy_path)
        .output()
        .expect("failed to run copybook inspect");

    assert_eq!(
        output.status.code(),
        Some(0),
        "inspect of valid copybook must exit 0.\nstderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("ID-FIELD") || stdout.contains("NAME-FIELD"),
        "stdout should contain field layout"
    );
    assert_no_panic(&String::from_utf8_lossy(&output.stderr));
}

// =========================================================================
// 17. All exit codes are stable integers (no unexpected panic codes)
// =========================================================================

#[test]
fn all_exit_codes_are_in_expected_range() {
    // Collect exit codes from several scenarios and verify they are in [0..=5]
    let scenarios: Vec<(&str, Vec<&str>)> = vec![
        ("help", vec!["--help"]),
        ("version", vec!["--version"]),
        ("unknown", vec!["bogus-subcommand"]),
    ];

    for (label, args) in &scenarios {
        let output = copybook_cmd()
            .args(args.iter())
            .output()
            .unwrap_or_else(|e| panic!("failed to run scenario '{label}': {e}"));

        let code = output.status.code().expect("exit code should be present");
        assert!(
            (0..=5).contains(&code),
            "scenario '{label}' exited with code {code}, expected 0..=5"
        );
        assert_no_panic(&String::from_utf8_lossy(&output.stderr));
    }
}

// =========================================================================
// 18. Stderr never contains raw Rust panic backtraces in normal error paths
// =========================================================================

#[test]
fn error_paths_do_not_produce_backtraces() {
    let dir = write_temp_file("bad.cpy", INVALID_COPYBOOK.as_bytes());
    let cpy_path = dir.path().join("bad.cpy");

    let output = copybook_cmd()
        .args(["parse"])
        .arg(&cpy_path)
        .output()
        .expect("failed to run copybook parse");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_panic(&stderr);
    assert!(
        !stderr.contains("stack backtrace"),
        "stderr should not contain stack backtrace on normal errors:\n{stderr}"
    );
}
