// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for the `explain` CLI subcommand.
//!
//! Validates that known stable error identities render in both text and
//! JSON formats (full and short codes, case-insensitively), and that
//! unknown identities exit 3 with guidance.

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

/// COMP-3 fixture with an invalid sign nibble in DECIMAL-AMOUNT.
fn corrupted_comp3(dir: &tempfile::TempDir) -> (std::path::PathBuf, std::path::PathBuf) {
    let copybook = workspace_path("fixtures/copybooks/comp3_test.cpy");
    let mut data =
        std::fs::read(workspace_path("fixtures/data/comp3_test.bin")).expect("read fixture");
    data[14] = (data[14] & 0xF0) | 0x07;
    let data = write_temp_file(dir, "bad.bin", &data);
    (copybook, data)
}

const RDW_COPYBOOK: &str = "       01  REC.\n           05  FLD PIC X(10).\n";

/// RDW fixture whose declared length (100) overruns the 10-byte payload.
fn truncated_rdw(dir: &tempfile::TempDir) -> (std::path::PathBuf, std::path::PathBuf) {
    let copybook = write_temp_file(dir, "rdw.cpy", RDW_COPYBOOK.as_bytes());
    let mut data = vec![0x00, 0x64, 0x00, 0x00];
    data.extend_from_slice(b"XXXXXXXXXX");
    let data = write_temp_file(dir, "bad.bin", &data);
    (copybook, data)
}

#[test]
fn explain_full_code_text() {
    cmd()
        .args(["explain", "CBKE501_JSON_TYPE_MISMATCH"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKE501_JSON_TYPE_MISMATCH"))
        .stdout(predicate::str::contains("Fix:"));
}

#[test]
fn explain_short_code_resolves() {
    cmd()
        .args(["explain", "cbkd411"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKD411_ZONED_BAD_SIGN"));
}

#[test]
fn explain_reserved_code_is_explained() {
    // Reserved identities are still documented taxonomy: explain them.
    cmd()
        .args(["explain", "CBKD431"])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKD431_FLOAT_NAN"));
}

#[test]
fn explain_json_format_is_machine_readable() {
    let assert = cmd()
        .args(["explain", "CBKF104_RDW_SUSPECT_ASCII", "--format", "json"])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("explain --format json should produce valid JSON");
    assert_eq!(parsed["code"], "CBKF104_RDW_SUSPECT_ASCII");
    assert_eq!(parsed["family"], "CBKF");
    assert!(
        parsed["resolution"].as_str().is_some_and(|s| !s.is_empty()),
        "JSON explanation must carry a resolution"
    );
}

#[test]
fn explain_unknown_identity_exits_3() {
    cmd()
        .args(["explain", "CBKX000_MISSING"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("unknown error identity"));
}

#[test]
fn explain_occurrence_text_names_field_and_bytes() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = corrupted_comp3(&dir);
    cmd()
        .args([
            "explain",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "fixed",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKD401_COMP3_INVALID_NIBBLE"))
        .stdout(predicate::str::contains("Record 1"))
        .stdout(predicate::str::contains("DECIMAL-AMOUNT"))
        .stdout(predicate::str::contains("Field bytes: 10..15"))
        .stdout(predicate::str::contains("S9(7)V99 COMP-3"))
        .stdout(predicate::str::contains("Next: copybook doctor"));
}

#[test]
fn explain_occurrence_json_is_machine_readable() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = corrupted_comp3(&dir);
    let assert = cmd()
        .args([
            "explain",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "fixed",
            "--format",
            "json",
        ])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("occurrence --format json is valid JSON");
    assert_eq!(parsed["code"], "CBKD401_COMP3_INVALID_NIBBLE");
    assert_eq!(parsed["occurrence"]["record"], 1);
    assert_eq!(
        parsed["occurrence"]["field_bytes"],
        serde_json::json!([10, 15])
    );
    assert!(
        parsed["occurrence"]["representation"]
            .as_str()
            .is_some_and(|s| s.contains("COMP-3")),
        "occurrence JSON must carry the field representation"
    );
}

#[test]
fn explain_occurrence_code_filter_selects_identity() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = corrupted_comp3(&dir);
    cmd()
        .args([
            "explain",
            "cbkd401",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "fixed",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKD401_COMP3_INVALID_NIBBLE"));
}

#[test]
fn explain_occurrence_clean_record_says_so() {
    let copybook = workspace_path("fixtures/copybooks/comp3_test.cpy");
    let data = workspace_path("fixtures/data/comp3_test.bin");
    cmd()
        .args([
            "explain",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "fixed",
            "--record",
            "1",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("decoded without errors"));
}

#[test]
fn explain_occurrence_beyond_end_reports_scope() {
    let copybook = workspace_path("fixtures/copybooks/comp3_test.cpy");
    let data = workspace_path("fixtures/data/comp3_test.bin");
    cmd()
        .args([
            "explain",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "fixed",
            "--record",
            "9999",
        ])
        .assert()
        .failure()
        .code(3)
        .stdout(predicate::str::contains("does not exist"));
}

#[test]
fn explain_occurrence_needs_record_format() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = corrupted_comp3(&dir);
    cmd()
        .args([
            "explain",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
        ])
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("--record-format"));
}

#[test]
fn explain_occurrence_framing_card_has_no_field_but_next_step() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = truncated_rdw(&dir);
    cmd()
        .args([
            "explain",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "rdw",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("CBKF221_RDW_UNDERFLOW"))
        .stdout(predicate::str::contains("Physical offset: 0"))
        .stdout(predicate::str::contains("Field: unknown"))
        .stdout(predicate::str::contains("Next: copybook doctor"));
}

#[test]
fn explain_occurrence_filter_mismatch_names_seen_identity() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = truncated_rdw(&dir);
    cmd()
        .args([
            "explain",
            "CBKF102",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "rdw",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "failures seen with other identities",
        ))
        .stdout(predicate::str::contains("CBKF221_RDW_UNDERFLOW"));
}

#[test]
fn decode_fatal_framing_failure_prints_explain_hint() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = truncated_rdw(&dir);
    let out = dir.path().join("out.jsonl");
    cmd()
        .args([
            "decode",
            copybook.to_str().expect("copybook path"),
            data.to_str().expect("data path"),
            "-o",
            out.to_str().expect("output path"),
            "--format",
            "rdw",
        ])
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains(
            "Explain a failure: copybook explain CBKF102_RECORD_LENGTH_INVALID",
        ));
}

#[test]
fn decode_failure_points_at_explain() {
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = corrupted_comp3(&dir);
    let out = dir.path().join("out.jsonl");
    cmd()
        .args([
            "decode",
            copybook.to_str().expect("copybook path"),
            data.to_str().expect("data path"),
            "-o",
            out.to_str().expect("output path"),
            "--format",
            "fixed",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Explain a failure: copybook explain CBKD401_COMP3_INVALID_NIBBLE",
        ));
}

#[test]
fn explain_occurrence_target_failed_differently_is_not_clean() {
    // Record 1 fails with CBKD401, but the filter asks for CBKE501: the
    // target must report a filtered scope naming what was seen, never a
    // clean record.
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = corrupted_comp3(&dir);
    let assert = cmd()
        .args([
            "explain",
            "CBKE501",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "fixed",
            "--record",
            "1",
        ])
        .assert()
        .failure()
        .code(3);
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).unwrap();
    assert!(
        stdout.contains("CBKD401_COMP3_INVALID_NIBBLE"),
        "filtered scope must name the seen identity.\nstdout: {stdout}"
    );
    assert!(
        !stdout.contains("decoded without errors"),
        "a record that failed differently is not clean.\nstdout: {stdout}"
    );
}

#[test]
fn explain_occurrence_filtered_scope_json_verdict_is_inconclusive() {
    // The text report names the seen identity; the JSON verdict must agree
    // instead of claiming the scope is clean.
    let dir = tempfile::TempDir::new().expect("temp dir");
    let (copybook, data) = truncated_rdw(&dir);
    let assert = cmd()
        .args([
            "explain",
            "CBKF102",
            "--copybook",
            copybook.to_str().expect("copybook path"),
            "--input",
            data.to_str().expect("data path"),
            "--record-format",
            "rdw",
            "--format",
            "json",
        ])
        .assert()
        .success();
    let stdout = String::from_utf8(assert.get_output().stdout.clone()).expect("stdout utf8");
    let value: serde_json::Value = serde_json::from_str(&stdout).expect("valid json");
    assert_eq!(value["verdict"], "inconclusive");
    assert!(
        value["message"]
            .as_str()
            .unwrap_or_default()
            .contains("CBKF221_RDW_UNDERFLOW"),
        "filtered scope must name the seen identity.\nstdout: {stdout}"
    );
}

#[test]
fn decode_failure_hint_quotes_spaced_paths() {
    // Pasted next commands must survive directories with spaces.
    let outer = tempfile::TempDir::new().expect("temp dir");
    let spaced = outer.path().join("with space");
    std::fs::create_dir(&spaced).expect("spaced dir");
    std::fs::copy(
        workspace_path("fixtures/copybooks/comp3_test.cpy"),
        spaced.join("schema.cpy"),
    )
    .expect("copy copybook");
    let mut data =
        std::fs::read(workspace_path("fixtures/data/comp3_test.bin")).expect("read fixture");
    data[14] = (data[14] & 0xF0) | 0x07;
    let data_path = spaced.join("bad.bin");
    std::fs::write(&data_path, &data).expect("write data");
    let output = cmd()
        .args([
            "decode",
            spaced.join("schema.cpy").to_str().expect("copybook path"),
            data_path.to_str().expect("data path"),
            "-o",
            spaced.join("out.jsonl").to_str().expect("output path"),
            "--format",
            "fixed",
        ])
        .output()
        .expect("run copybook decode");
    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr).expect("stderr utf8");
    let quoted = format!("'{}'", data_path.display());
    assert!(
        stderr.contains(&quoted),
        "hint must quote the spaced input path.\nstderr: {stderr}"
    );
}
