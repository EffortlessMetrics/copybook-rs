// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive E2E integration tests for the copybook CLI binary.
//!
//! Every test invokes the `copybook` binary as a subprocess via `std::process::Command`,
//! exercising all six subcommands: parse, inspect, decode, encode, verify, determinism.
//! Tests validate stdout/stderr content, exit codes, and absence of panics.

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

fn stdout_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr_str(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

/// Write bytes to a named file inside a temp directory; return the dir (keeps it alive).
fn write_temp_file(name: &str, contents: &[u8]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    let path = dir.path().join(name);
    let mut f = std::fs::File::create(&path).expect("create temp file");
    f.write_all(contents).expect("write temp file");
    dir
}

/// Return the path to a named file inside a temp dir.
fn temp_path(dir: &tempfile::TempDir, name: &str) -> PathBuf {
    dir.path().join(name)
}

// ---------------------------------------------------------------------------
// Test data constants
// ---------------------------------------------------------------------------

/// A minimal valid COBOL copybook (13-byte record: 10 + 3).
const SIMPLE_COPYBOOK: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// EBCDIC CP037 record matching SIMPLE_COPYBOOK (13 bytes).
/// NAME = "ALICE     " (A=C1 L=D3 I=C9 C=C3 E=C5 + 5×space=40)
/// AGE  = "025"       (0=F0 2=F2 5=F5)
fn simple_ebcdic_record() -> Vec<u8> {
    let mut data = Vec::new();
    // "ALICE     " in CP037
    data.extend_from_slice(&[0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40]);
    // "025" in CP037
    data.extend_from_slice(&[0xF0, 0xF2, 0xF5]);
    data
}

/// Two EBCDIC records concatenated for multi-record tests.
fn two_ebcdic_records() -> Vec<u8> {
    let mut data = simple_ebcdic_record();
    // Second record: "BOB       " + "030"
    // B=C2 O=D6 B=C2 + 7×space=40
    data.extend_from_slice(&[0xC2, 0xD6, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40]);
    data.extend_from_slice(&[0xF0, 0xF3, 0xF0]);
    data
}

/// Prepare a temp dir with the simple copybook and one EBCDIC record.
fn setup_simple() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_COPYBOOK).unwrap();
    std::fs::write(dir.path().join("data.bin"), simple_ebcdic_record()).unwrap();
    dir
}

/// Prepare a temp dir with the simple copybook and two EBCDIC records.
fn setup_two_records() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temp dir");
    std::fs::write(dir.path().join("schema.cpy"), SIMPLE_COPYBOOK).unwrap();
    std::fs::write(dir.path().join("data.bin"), two_ebcdic_records()).unwrap();
    dir
}

// =========================================================================
// 1. PARSE subcommand
// =========================================================================

#[test]
fn parse_valid_copybook_produces_json_schema() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .arg("parse")
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

    let stdout = stdout_str(&output);
    // Parse produces JSON schema; verify it contains field names.
    let json: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("parse stdout is not valid JSON: {e}\n---\n{stdout}"));
    let text = json.to_string();
    assert!(text.contains("NAME"), "schema should contain NAME field");
    assert!(text.contains("AGE"), "schema should contain AGE field");
}

#[test]
fn parse_valid_copybook_to_output_file() {
    let dir = setup_simple();
    let out_path = temp_path(&dir, "schema.json");

    let output = copybook_cmd()
        .args(["parse"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg("-o")
        .arg(&out_path)
        .output()
        .expect("run parse with -o");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    let contents = std::fs::read_to_string(&out_path).expect("read output file");
    let _json: serde_json::Value =
        serde_json::from_str(&contents).expect("output file should be valid JSON");
}

#[test]
fn parse_nonexistent_file_exits_nonzero() {
    let output = copybook_cmd()
        .args(["parse", "does_not_exist_12345.cpy"])
        .output()
        .expect("run parse");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn parse_invalid_syntax_exits_nonzero() {
    let dir = write_temp_file("bad.cpy", b"THIS IS NOT VALID COBOL !!!");

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "bad.cpy"))
        .output()
        .expect("run parse");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
    let se = stderr_str(&output);
    assert!(
        se.contains("CBK") || se.contains("error") || se.contains("Error"),
        "stderr should contain an error indicator, got: {se}"
    );
}

#[test]
fn parse_missing_args_exits_nonzero() {
    let output = copybook_cmd()
        .arg("parse")
        .output()
        .expect("run parse without args");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn parse_help_exits_zero_with_usage() {
    let output = copybook_cmd()
        .args(["parse", "--help"])
        .output()
        .expect("run parse --help");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(
        so.contains("parse") || so.contains("Parse") || so.contains("Usage"),
        "help output should describe the parse command"
    );
}

// =========================================================================
// 2. INSPECT subcommand
// =========================================================================

#[test]
fn inspect_valid_copybook_shows_layout() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .arg("inspect")
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

    let so = stdout_str(&output);
    assert!(so.contains("NAME"), "inspect output should mention NAME");
    assert!(so.contains("AGE"), "inspect output should mention AGE");
    // Inspect shows offsets/sizes; check for numeric info
    assert!(
        so.contains("10") || so.contains("13") || so.contains("PIC"),
        "inspect should show field sizes or PIC clauses"
    );
}

#[test]
fn inspect_invalid_file_exits_nonzero() {
    let dir = write_temp_file("bad.cpy", b"NOT COBOL");

    let output = copybook_cmd()
        .arg("inspect")
        .arg(temp_path(&dir, "bad.cpy"))
        .output()
        .expect("run inspect");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn inspect_missing_args_exits_nonzero() {
    let output = copybook_cmd()
        .arg("inspect")
        .output()
        .expect("run inspect without args");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn inspect_help_exits_zero() {
    let output = copybook_cmd()
        .args(["inspect", "--help"])
        .output()
        .expect("run inspect --help");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(
        so.contains("inspect") || so.contains("Inspect") || so.contains("Usage"),
        "help output should describe inspect command"
    );
}

// =========================================================================
// 3. DECODE subcommand
// =========================================================================

#[test]
fn decode_single_record_to_jsonl() {
    let dir = setup_simple();
    let out = temp_path(&dir, "output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run decode");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));

    let jsonl = std::fs::read_to_string(&out).expect("read output jsonl");
    assert!(!jsonl.is_empty(), "JSONL output should not be empty");

    // Each line should be valid JSON
    for line in jsonl.lines() {
        let val: serde_json::Value = serde_json::from_str(line)
            .unwrap_or_else(|e| panic!("JSONL line is not valid JSON: {e}\n{line}"));
        let text = val.to_string();
        assert!(text.contains("NAME"), "record should have NAME field");
        assert!(text.contains("AGE"), "record should have AGE field");
    }
}

#[test]
fn decode_multiple_records() {
    let dir = setup_two_records();
    let out = temp_path(&dir, "output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run decode multiple records");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    let jsonl = std::fs::read_to_string(&out).expect("read output");
    let lines: Vec<&str> = jsonl.lines().collect();
    assert_eq!(lines.len(), 2, "should decode exactly 2 records");
}

#[test]
fn decode_to_stdout() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output", "-", "--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run decode to stdout");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    let so = stdout_str(&output);
    assert!(so.contains("NAME"), "stdout should contain decoded JSON");
}

#[test]
fn decode_with_lossless_number_mode() {
    let dir = setup_simple();
    let out = temp_path(&dir, "output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(&out)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--json-number",
            "lossless",
        ])
        .output()
        .expect("run decode with lossless");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    let jsonl = std::fs::read_to_string(&out).expect("read output");
    assert!(!jsonl.is_empty());
}

#[test]
fn decode_truncated_data_exits_nonzero() {
    let dir = setup_simple();
    // Overwrite data with a truncated 5-byte payload (record is 13 bytes)
    std::fs::write(dir.path().join("data.bin"), [0xF0; 5]).unwrap();
    let out = temp_path(&dir, "output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--fail-fast"])
        .output()
        .expect("run decode truncated");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn decode_nonexistent_input_exits_nonzero() {
    let dir = setup_simple();
    let out = temp_path(&dir, "output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg("nonexistent_file_99999.bin")
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run decode nonexistent input");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn decode_missing_required_args_exits_nonzero() {
    let output = copybook_cmd()
        .arg("decode")
        .output()
        .expect("run decode without args");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn decode_missing_format_flag_exits_nonzero() {
    let dir = setup_simple();
    let out = temp_path(&dir, "output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(&out)
        // Intentionally omit --format
        .output()
        .expect("run decode without --format");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn decode_help_exits_zero() {
    let output = copybook_cmd()
        .args(["decode", "--help"])
        .output()
        .expect("run decode --help");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(
        so.contains("decode") || so.contains("Decode") || so.contains("Usage"),
        "help output should describe decode command"
    );
}

// =========================================================================
// 4. ENCODE subcommand
// =========================================================================

/// Helper: decode a record to produce valid JSONL, return the JSONL file path.
fn decode_to_jsonl(dir: &tempfile::TempDir) -> PathBuf {
    let jsonl_path = dir.path().join("decoded.jsonl");
    let output = copybook_cmd()
        .args(["decode"])
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .args(["--output"])
        .arg(&jsonl_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("decode for encode setup");
    assert_eq!(
        output.status.code(),
        Some(0),
        "decode setup failed: {}",
        stderr_str(&output)
    );
    jsonl_path
}

#[test]
fn encode_valid_jsonl_to_binary() {
    let dir = setup_simple();
    let jsonl_path = decode_to_jsonl(&dir);
    let encoded_path = temp_path(&dir, "encoded.bin");

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&jsonl_path)
        .args(["--output"])
        .arg(&encoded_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run encode");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));

    let encoded = std::fs::read(&encoded_path).expect("read encoded binary");
    assert!(!encoded.is_empty(), "encoded binary should not be empty");
    assert_eq!(
        encoded.len(),
        13,
        "encoded record should be 13 bytes (10 + 3)"
    );
}

#[test]
fn encode_round_trip_binary_fidelity() {
    let dir = setup_simple();
    let jsonl_path = decode_to_jsonl(&dir);
    let encoded_path = temp_path(&dir, "encoded.bin");

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&jsonl_path)
        .args(["--output"])
        .arg(&encoded_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run encode for round-trip");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );

    let original = simple_ebcdic_record();
    let encoded = std::fs::read(&encoded_path).expect("read encoded");
    assert_eq!(
        original, encoded,
        "round-trip binary should match original EBCDIC data"
    );
}

#[test]
fn encode_invalid_jsonl_exits_nonzero() {
    let dir = setup_simple();
    let bad_jsonl = dir.path().join("bad.jsonl");
    std::fs::write(&bad_jsonl, "THIS IS NOT JSON\n").unwrap();
    let encoded_path = temp_path(&dir, "encoded.bin");

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&bad_jsonl)
        .args(["--output"])
        .arg(&encoded_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run encode with bad jsonl");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn encode_nonexistent_input_exits_nonzero() {
    let dir = setup_simple();
    let encoded_path = temp_path(&dir, "encoded.bin");

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg("nonexistent_99999.jsonl")
        .args(["--output"])
        .arg(&encoded_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run encode nonexistent input");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn encode_missing_required_args_exits_nonzero() {
    let output = copybook_cmd()
        .arg("encode")
        .output()
        .expect("run encode without args");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn encode_help_exits_zero() {
    let output = copybook_cmd()
        .args(["encode", "--help"])
        .output()
        .expect("run encode --help");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(
        so.contains("encode") || so.contains("Encode") || so.contains("Usage"),
        "help output should describe encode command"
    );
}

#[test]
fn encode_to_stdout() {
    let dir = setup_simple();
    let jsonl_path = decode_to_jsonl(&dir);

    let output = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&jsonl_path)
        .args(["--output", "-", "--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run encode to stdout");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_eq!(
        output.stdout.len(),
        13,
        "stdout should contain 13 binary bytes"
    );
}

// =========================================================================
// 5. VERIFY subcommand
// =========================================================================

#[test]
fn verify_valid_data_exits_zero() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run verify");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));

    let so = stdout_str(&output);
    assert!(
        so.contains("PASS") || so.contains("Records Total") || so.contains("valid"),
        "verify should report success: {so}"
    );
}

#[test]
fn verify_truncated_data_reports_zero_records() {
    let dir = setup_simple();
    // 5 bytes is not a complete record (13 needed); verify processes 0 records.
    std::fs::write(dir.path().join("data.bin"), [0xF0; 5]).unwrap();

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run verify truncated");

    assert_no_panic(&stderr_str(&output));
    // Truncated data yields 0 complete records; verify exits 0 with a warning.
    let so = stdout_str(&output);
    assert!(
        so.contains("Records Total: 0") || so.contains("PASS"),
        "verify of truncated data should process 0 records: {so}"
    );
}

#[test]
fn verify_nonexistent_data_exits_nonzero() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg("nonexistent_99999.bin")
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run verify nonexistent");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn verify_missing_required_args_exits_nonzero() {
    let output = copybook_cmd()
        .arg("verify")
        .output()
        .expect("run verify without args");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn verify_help_exits_zero() {
    let output = copybook_cmd()
        .args(["verify", "--help"])
        .output()
        .expect("run verify --help");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(
        so.contains("verify") || so.contains("Verify") || so.contains("Usage"),
        "help output should describe verify command"
    );
}

#[test]
fn verify_with_report_flag() {
    let dir = setup_simple();
    let report_path = temp_path(&dir, "report.json");

    let output = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .arg("--report")
        .arg(&report_path)
        .output()
        .expect("run verify with --report");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));

    // Report file should have been written
    assert!(
        report_path.exists(),
        "report file should exist at {report_path:?}"
    );
    let report = std::fs::read_to_string(&report_path).expect("read report");
    assert!(!report.is_empty(), "report should not be empty");
}

// =========================================================================
// 6. DETERMINISM subcommand
// =========================================================================

#[test]
fn determinism_decode_exits_zero_for_valid_data() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .args(["determinism", "decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run determinism decode");

    assert_eq!(
        output.status.code(),
        Some(0),
        "determinism decode should pass for valid data. stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
    let so = stdout_str(&output);
    assert!(
        so.contains("deterministic")
            || so.contains("Deterministic")
            || so.contains("PASS")
            || so.contains("✅")
            || so.contains("match"),
        "determinism output should indicate a pass: {so}"
    );
}

#[test]
fn determinism_encode_exits_zero_for_valid_data() {
    let dir = setup_simple();
    let jsonl_path = decode_to_jsonl(&dir);

    let output = copybook_cmd()
        .args(["determinism", "encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&jsonl_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run determinism encode");

    assert_eq!(
        output.status.code(),
        Some(0),
        "determinism encode should pass. stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn determinism_round_trip_exits_zero_for_valid_data() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .args(["determinism", "round-trip"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("run determinism round-trip");

    assert_eq!(
        output.status.code(),
        Some(0),
        "determinism round-trip should pass. stderr: {}",
        stderr_str(&output)
    );
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn determinism_json_output_format() {
    let dir = setup_simple();

    let output = copybook_cmd()
        .args(["determinism", "decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--output",
            "json",
        ])
        .output()
        .expect("run determinism decode --output json");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    let so = stdout_str(&output);
    // JSON output should be parseable
    let _json: serde_json::Value = serde_json::from_str(&so)
        .unwrap_or_else(|e| panic!("determinism JSON output is not valid JSON: {e}\n{so}"));
}

#[test]
fn determinism_missing_args_exits_nonzero() {
    let output = copybook_cmd()
        .args(["determinism", "decode"])
        .output()
        .expect("run determinism decode without args");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn determinism_help_exits_zero() {
    let output = copybook_cmd()
        .args(["determinism", "--help"])
        .output()
        .expect("run determinism --help");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(
        so.contains("determinism") || so.contains("Determinism") || so.contains("Usage"),
        "help output should describe determinism command"
    );
}

#[test]
fn determinism_decode_help_exits_zero() {
    let output = copybook_cmd()
        .args(["determinism", "decode", "--help"])
        .output()
        .expect("run determinism decode --help");

    assert_eq!(output.status.code(), Some(0));
}

// =========================================================================
// 7. Cross-cutting concerns
// =========================================================================

#[test]
fn global_help_exits_zero() {
    let output = copybook_cmd().arg("--help").output().expect("run --help");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(so.contains("parse"), "help should list parse subcommand");
    assert!(
        so.contains("inspect"),
        "help should list inspect subcommand"
    );
    assert!(so.contains("decode"), "help should list decode subcommand");
    assert!(so.contains("encode"), "help should list encode subcommand");
    assert!(so.contains("verify"), "help should list verify subcommand");
    assert!(
        so.contains("determinism"),
        "help should list determinism subcommand"
    );
}

#[test]
fn version_flag_exits_zero() {
    let output = copybook_cmd()
        .arg("--version")
        .output()
        .expect("run --version");

    assert_eq!(output.status.code(), Some(0));
    let so = stdout_str(&output);
    assert!(
        so.contains("copybook"),
        "version output should contain 'copybook'"
    );
}

#[test]
fn unknown_subcommand_exits_nonzero() {
    let output = copybook_cmd()
        .arg("not-a-real-command")
        .output()
        .expect("run unknown subcommand");

    assert_ne!(output.status.code(), Some(0));
    assert_no_panic(&stderr_str(&output));
}

#[test]
fn error_paths_never_produce_backtraces() {
    let dir = write_temp_file("bad.cpy", b"NOT COBOL");

    let output = copybook_cmd()
        .arg("parse")
        .arg(temp_path(&dir, "bad.cpy"))
        .output()
        .expect("run parse bad file");

    let se = stderr_str(&output);
    assert_no_panic(&se);
    assert!(
        !se.contains("stack backtrace"),
        "error paths should not produce stack backtraces: {se}"
    );
}

#[test]
fn exit_codes_are_in_expected_range() {
    // Exit codes should be in 0..=5 per the ExitCode enum.
    let scenarios: &[(&str, &[&str])] = &[
        ("help", &["--help"]),
        ("version", &["--version"]),
        ("unknown", &["bogus-command"]),
    ];

    for (label, args) in scenarios {
        let output = copybook_cmd()
            .args(args.iter())
            .output()
            .unwrap_or_else(|e| panic!("failed to run scenario '{label}': {e}"));

        let code = output.status.code().expect("exit code should be present");
        assert!(
            (0..=5).contains(&code),
            "scenario '{label}' exited with {code}, expected 0..=5"
        );
    }
}

// =========================================================================
// 8. Full pipeline (decode → encode → verify) integration
// =========================================================================

#[test]
fn full_pipeline_decode_encode_verify() {
    let dir = setup_two_records();

    // Step 1: Decode binary → JSONL
    let jsonl_path = temp_path(&dir, "decoded.jsonl");
    let decode_out = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(&jsonl_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("pipeline: decode");
    assert_eq!(
        decode_out.status.code(),
        Some(0),
        "pipeline decode failed: {}",
        stderr_str(&decode_out)
    );

    // Step 2: Encode JSONL → binary
    let encoded_path = temp_path(&dir, "encoded.bin");
    let encode_out = copybook_cmd()
        .args(["encode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&jsonl_path)
        .args(["--output"])
        .arg(&encoded_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("pipeline: encode");
    assert_eq!(
        encode_out.status.code(),
        Some(0),
        "pipeline encode failed: {}",
        stderr_str(&encode_out)
    );

    // Step 3: Verify re-encoded binary
    let verify_out = copybook_cmd()
        .args(["verify"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(&encoded_path)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .output()
        .expect("pipeline: verify");
    assert_eq!(
        verify_out.status.code(),
        Some(0),
        "pipeline verify failed: {}",
        stderr_str(&verify_out)
    );

    // Step 4: Compare original and re-encoded binaries
    let original = std::fs::read(temp_path(&dir, "data.bin")).unwrap();
    let encoded = std::fs::read(&encoded_path).unwrap();
    assert_eq!(
        original, encoded,
        "full pipeline round-trip must produce identical binary"
    );
}

// =========================================================================
// 9. Dialect flag integration
// =========================================================================

#[test]
fn parse_with_dialect_flag() {
    let dir = setup_simple();

    for dialect in &["n", "0", "1"] {
        let output = copybook_cmd()
            .args(["parse"])
            .arg(temp_path(&dir, "schema.cpy"))
            .args(["--dialect", dialect])
            .output()
            .unwrap_or_else(|e| panic!("parse --dialect {dialect} failed: {e}"));

        assert_eq!(
            output.status.code(),
            Some(0),
            "parse --dialect {dialect} should exit 0. stderr: {}",
            stderr_str(&output)
        );
    }
}

#[test]
fn decode_with_emit_meta() {
    let dir = setup_simple();
    let out = temp_path(&dir, "output.jsonl");

    let output = copybook_cmd()
        .args(["decode"])
        .arg(temp_path(&dir, "schema.cpy"))
        .arg(temp_path(&dir, "data.bin"))
        .args(["--output"])
        .arg(&out)
        .args(["--format", "fixed", "--codepage", "cp037", "--emit-meta"])
        .output()
        .expect("run decode with --emit-meta");

    assert_eq!(
        output.status.code(),
        Some(0),
        "stderr: {}",
        stderr_str(&output)
    );
    let jsonl = std::fs::read_to_string(&out).expect("read output");
    // With --emit-meta, JSON output should contain metadata fields like __record_index, __length
    assert!(
        jsonl.contains("__record_index") || jsonl.contains("__length") || jsonl.contains("schema"),
        "emit-meta output should contain metadata fields, got: {jsonl}"
    );
}
