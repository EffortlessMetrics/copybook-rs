// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for `--profile` consumption on `decode`, `verify`, and `encode`.
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

/// ASCII profile for one framing kind, mirroring `product_defaults` with
/// framing and codepage overwritten (#1113 item 4: truthful defaults).
fn ascii_profile(kind: &str) -> String {
    format!(
        "\
schema_version = 1
[source]
dialect = \"normative\"
[framing]
kind = \"{kind}\"
reserved_bytes = \"lenient\"
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

/// A profile-driven decode must emit byte-identical JSONL to the direct
/// flag-driven decode of the same records (#1113 item 4: product-default
/// equivalence executed against current direct behavior).
fn assert_profile_matches_direct(kind: &str, data: &str) {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "profile.toml", ascii_profile(kind).as_bytes());
    let direct_out = dir.path().join("direct.jsonl");
    let profile_out = dir.path().join("profile.jsonl");
    let copybook = workspace_path("fixtures/corpus/mini.cpy");
    let records = workspace_path(data);

    cmd()
        .args([
            "decode",
            "--format",
            kind,
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&direct_out)
        .arg(&copybook)
        .arg(&records)
        .assert()
        .success();
    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&profile_out)
        .arg(&copybook)
        .arg(&records)
        .assert()
        .success();

    let direct = std::fs::read(&direct_out).expect("read direct output");
    let via_profile = std::fs::read(&profile_out).expect("read profile output");
    assert_eq!(
        via_profile, direct,
        "profile-driven {kind} decode must reproduce direct decode byte-for-byte"
    );
}

#[test]
fn decode_profile_matches_direct_fixed() {
    assert_profile_matches_direct("fixed", "fixtures/corpus/mini_fixed.bin");
}

#[test]
fn decode_profile_matches_direct_rdw() {
    assert_profile_matches_direct("rdw", "fixtures/corpus/mini_rdw.bin");
}

#[test]
fn decode_profile_matches_direct_vb() {
    assert_profile_matches_direct("vb", "fixtures/corpus/mini_vb.bin");
}

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
fn decode_profile_dialect_survives_invalid_env() {
    // Precedence is flag > profile > env: when the profile supplies a
    // dialect, an unrelated invalid COPYBOOK_DIALECT must not abort the run
    // (#1126). The environment is only rejected when it is actually
    // consulted.
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let out = dir.path().join("out.jsonl");
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .env("COPYBOOK_DIALECT", "unsupported")
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
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
        .stderr(predicate::str::contains("profile"))
        .stderr(predicate::str::contains("subcode=405"));
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
        .stderr(predicate::str::contains("profile"))
        .stderr(predicate::str::contains("subcode=403"));
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

// =========================================================================
// encode --profile tests: framing, codepage, dialect, and error budget
// come from the profile; --format/--codepage become optional.
// =========================================================================

/// Fixed/ASCII profile matching `SIMPLE_CPY` (15-byte fixed records).
const FIXED_ASCII_PROFILE: &str = "\
schema_version = 1
[source]
dialect = \"normative\"
[framing]
kind = \"fixed\"
reserved_bytes = \"lenient\"
[decode]
codepage = \"ascii\"
unmappable = \"error\"
json_numbers = \"lossless\"
[limits]
maximum_record_length = 32760
maximum_errors = 100
";

/// One JSONL line matching `SIMPLE_CPY`: 10-byte name, 5-byte amount.
const SIMPLE_JSONL: &str = "{\"NAME\":\"ALICE     \",\"AMOUNT\":\"00100\"}\n";

/// Expected 15-byte fixed record for `SIMPLE_JSONL` under ASCII.
fn expected_simple_record() -> Vec<u8> {
    let mut record = vec![b' '; 15];
    for (i, b) in "ALICE".bytes().enumerate() {
        record[i] = b;
    }
    for (i, b) in "00100".bytes().enumerate() {
        record[10 + i] = b;
    }
    record
}

#[test]
fn encode_profile_supplies_framing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let profile = write_temp_file(&dir, "profile.toml", FIXED_ASCII_PROFILE.as_bytes());
    let out = dir.path().join("out.bin");

    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .success();

    let bytes = std::fs::read(&out).expect("read output");
    assert_eq!(bytes, expected_simple_record());
}

#[test]
fn encode_profile_format_conflict() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let profile = write_temp_file(&dir, "profile.toml", FIXED_ASCII_PROFILE.as_bytes());
    let out = dir.path().join("out.bin");

    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .args(["--format", "rdw"])
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("conflicting"))
        .stderr(predicate::str::contains("framing.kind"));
}

#[test]
fn encode_profile_codepage_conflict() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let profile = write_temp_file(&dir, "profile.toml", FIXED_ASCII_PROFILE.as_bytes());
    let out = dir.path().join("out.bin");

    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .args(["--codepage", "cp037"])
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("decode.codepage"));
}

/// Profile with a caller-selected record bound over the 15-byte layout.
fn capped_profile(kind: &str, codepage: &str, cap: u64) -> String {
    format!(
        "\
schema_version = 1
[source]
dialect = \"normative\"
[framing]
kind = \"{kind}\"
reserved_bytes = \"lenient\"
[decode]
codepage = \"{codepage}\"
unmappable = \"error\"
json_numbers = \"lossless\"
[limits]
maximum_record_length = {cap}
maximum_errors = 100
"
    )
}

/// One RDW record with zero reserved bytes: header `00 0F 00 00`
/// (payload length 15) plus the 15-byte ASCII payload.
fn rdw_record_zero_reserved() -> Vec<u8> {
    let mut data = vec![0x00, 0x0F, 0x00, 0x00];
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
fn decode_profile_cap_at_lrecl_succeeds() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.bin", b"ALICE     00100");
    let profile = write_temp_file(
        &dir,
        "profile.toml",
        capped_profile("fixed", "ascii", 15).as_bytes(),
    );
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .success()
        .stdout(predicate::str::contains("Records with errors: 0"));
}

#[test]
fn decode_profile_cap_below_lrecl_fails_without_output() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.bin", b"ALICE     00100");
    let profile = write_temp_file(
        &dir,
        "profile.toml",
        capped_profile("fixed", "ascii", 14).as_bytes(),
    );
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
    assert!(!out.exists(), "pre-execution failure leaves output absent");
}

#[test]
fn decode_profile_rdw_over_cap_fails() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.bin", &rdw_record_zero_reserved());
    let profile = write_temp_file(
        &dir,
        "profile.toml",
        capped_profile("rdw", "ascii", 14).as_bytes(),
    );
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
}

#[test]
fn verify_profile_cap_below_lrecl_fails() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.bin", b"ALICE     00100");
    let profile = write_temp_file(
        &dir,
        "profile.toml",
        capped_profile("fixed", "ascii", 14).as_bytes(),
    );

    cmd()
        .args(["verify", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .assert()
        .failure()
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
}

#[test]
fn encode_missing_format_without_profile() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let out = dir.path().join("out.bin");

    cmd()
        .args(["encode"])
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("--format"));
}

#[test]
fn encode_profile_cap_below_lrecl_fails_without_output() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let profile = write_temp_file(
        &dir,
        "profile.toml",
        capped_profile("fixed", "ascii", 14).as_bytes(),
    );
    let out = dir.path().join("out.bin");

    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
    assert!(!out.exists(), "pre-execution failure leaves output absent");
}

#[test]
fn encode_profile_cap_at_lrecl_succeeds() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let profile = write_temp_file(
        &dir,
        "profile.toml",
        capped_profile("fixed", "ascii", 15).as_bytes(),
    );
    let out = dir.path().join("out.bin");

    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&out)
        .assert()
        .success();
    assert_eq!(
        std::fs::read(&out).expect("read output"),
        expected_simple_record()
    );
}

#[test]
fn encode_profile_rdw_over_cap_lenient_counts_failure() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", b"       01  REC PIC X(8).\n");
    let input = write_temp_file(&dir, "input.jsonl", b"{\"REC\": \"RECORD01\"}\n");
    let profile = write_temp_file(
        &dir,
        "profile.toml",
        capped_profile("rdw", "ascii", 7).as_bytes(),
    );
    let out = dir.path().join("out.bin");

    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--no-fail-fast", "--output"])
        .arg(&out)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
    assert!(
        std::fs::read(&out).expect("read output").is_empty(),
        "over-cap record leaves no bytes behind"
    );
}
