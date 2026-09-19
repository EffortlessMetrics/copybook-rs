// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for `decode --profile` / `verify --profile` consumption.
//!
//! A reviewed interpretation profile (TOML) supplies framing, decode
//! options, dialect, and error budget so `--format` and friends become
//! optional. A flag that disagrees with the profile is a contradiction
//! (exit 3), and `framing.reserved_bytes = "strict"` rejects non-zero
//! RDW reserved bytes without full `--strict` handling.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use predicates::prelude::*;
use std::io::Write as _;

#[allow(deprecated)]
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

/// Fixed/CP037 profile matching `fixtures/copybooks/simple.cpy` +
/// `fixtures/data/simple.bin` (50-byte EBCDIC fixed record).
const FIXED_CP037_PROFILE: &str = "\
schema_version = 1
[source]
dialect = \"normative\"
[framing]
kind = \"fixed\"
reserved_bytes = \"lenient\"
[decode]
codepage = \"cp037\"
unmappable = \"error\"
json_numbers = \"lossless\"
[limits]
maximum_record_length = 32760
maximum_errors = 100
";

/// Simple ASCII copybook: 10 bytes name + 5 bytes amount = 15 bytes.
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME     PIC X(10).
           05  AMOUNT   PIC 9(5).
";

fn rdw_profile(reserved: &str) -> String {
    format!(
        "\
schema_version = 1
[source]
dialect = \"normative\"
[framing]
kind = \"rdw\"
reserved_bytes = \"{reserved}\"
[decode]
codepage = \"ascii\"
unmappable = \"error\"
json_numbers = \"lossless\"
[limits]
maximum_record_length = 32760
maximum_errors = 100
"
    )
}

/// One RDW record with reserved bytes 0x0001: header `00 0F 00 01`
/// (payload length 15) plus a 15-byte ASCII payload.
fn rdw_record_nonzero_reserved() -> Vec<u8> {
    let mut data = vec![0x00, 0x0F, 0x00, 0x01];
    let mut payload = vec![b' '; 15];
    for (i, b) in "ALICE".bytes().enumerate() {
        payload[i] = b;
    }
    for (i, b) in "00100".bytes().enumerate() {
        payload[10 + i] = b;
    }
    data.extend_from_slice(&payload);
    data
}

#[test]
fn decode_profile_supplies_framing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).expect("read decode output");
    assert!(
        content.contains("CUSTOMER-ID") && content.contains("123456"),
        "profile-driven decode must emit CUSTOMER-ID 123456, got: {content}"
    );
}

#[test]
fn decode_profile_equal_flags_agree() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args([
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--max-errors",
            "100",
            "--output",
        ])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success();
}

#[test]
fn decode_profile_format_conflict() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--format", "rdw", "--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("conflicting"))
        .stderr(predicate::str::contains("framing.kind"))
        .stderr(predicate::str::contains("fixed"))
        .stderr(predicate::str::contains("rdw"));
}

#[test]
fn decode_profile_max_errors_conflict() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--max-errors", "5", "--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("100"));
}

#[test]
fn decode_profile_missing_file() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["decode", "--profile", "/nonexistent/x.toml", "--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("profile"));
}

#[test]
fn decode_profile_invalid_toml() {
    let dir = tempfile::tempdir().expect("tempdir");
    let bad = FIXED_CP037_PROFILE.replace("kind = \"fixed\"", "kind = \"oops\"");
    let profile = write_temp_file(&dir, "bad.toml", bad.as_bytes());
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("profile"));
}

#[test]
fn decode_profile_strict_reserved_rejects() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "strict.toml", rdw_profile("strict").as_bytes());
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let data = write_temp_file(&dir, "data.bin", &rdw_record_nonzero_reserved());
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKR211_RDW_RESERVED_NONZERO"));
}

#[test]
fn decode_profile_lenient_reserved_passes() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "lenient.toml", rdw_profile("lenient").as_bytes());
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let data = write_temp_file(&dir, "data.bin", &rdw_record_nonzero_reserved());
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .success();
}

#[test]
fn verify_profile_strict_reserved_reports() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "strict.toml", rdw_profile("strict").as_bytes());
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let data = write_temp_file(&dir, "data.bin", &rdw_record_nonzero_reserved());

    cmd()
        .args(["verify", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stdout(predicate::str::contains("CBKR211_RDW_RESERVED_NONZERO"));
}

#[test]
fn verify_profile_supplies_framing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["verify", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .code(0);
}

#[test]
fn missing_format_without_profile_still_clap_error() {
    // NOTE: this branch maps clap usage errors to ExitCode::Encode (3), not
    // the conventional clap exit 2; the assertion pins actual behavior.
    // See `Cli::try_parse` handling in crates/copybook-cli/src/main.rs.
    let dir = tempfile::tempdir().expect("tempdir");
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["decode", "--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("--format"));
}
