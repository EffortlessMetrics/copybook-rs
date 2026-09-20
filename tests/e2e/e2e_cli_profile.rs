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

/// Profile identical to `FIXED_ASCII_PROFILE` except for one decode-only
/// field value, for proving `encode` ignores decode-named policy (#1120).
fn fixed_ascii_profile_with(field: &str, value: &str) -> String {
    FIXED_ASCII_PROFILE
        .lines()
        .map(|line| {
            if line.starts_with(&format!("{field} = ")) {
                format!("{field} = \"{value}\"")
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
        + "\n"
}

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
    std::fs::write(&out, b"SENTINEL").expect("seed output");

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
    assert_eq!(
        std::fs::read(&out).expect("read output"),
        b"SENTINEL",
        "contradictory flags leave existing output untouched"
    );
}

#[test]
fn encode_profile_codepage_conflict() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let profile = write_temp_file(&dir, "profile.toml", FIXED_ASCII_PROFILE.as_bytes());
    let out = dir.path().join("out.bin");
    std::fs::write(&out, b"SENTINEL").expect("seed output");

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
        .stderr(predicate::str::contains("representation.codepage"));
    assert_eq!(
        std::fs::read(&out).expect("read output"),
        b"SENTINEL",
        "contradictory flags leave existing output untouched"
    );
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

// =========================================================================
// encode ignores decode-named policy (#1120 slice 1: no Encode decision
// is inferred from an unrelated decode-only field).
// =========================================================================

/// Fixed/CP037 profile matching `SIMPLE_CPY`, with a caller-selected
/// write-side unmappable policy (#1120: `[encode].unmappable`).
fn fixed_cp037_encode_profile(policy: &str) -> String {
    format!(
        "\
schema_version = 2
[source]
dialect = \"normative\"
[framing]
kind = \"fixed\"
reserved_bytes = \"lenient\"
[representation]
codepage = \"cp037\"
[decode]
unmappable = \"error\"
json_numbers = \"lossless\"
[encode]
unmappable = \"{policy}\"
[limits]
maximum_record_length = 32760
maximum_errors = 100
"
    )
}

#[test]
fn encode_enforces_profile_unmappable_policy() {
    // 日 (U+65E5) is not representable in CP037: `error` fails the
    // record naming CBKC301, `replace` writes `?` (0x6F), `skip` drops
    // the character. `decode.unmappable` stays `error` throughout, so
    // only `[encode].unmappable` can steer the run.
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(
        &dir,
        "input.jsonl",
        "{\"NAME\":\"A日       \",\"AMOUNT\":\"00100\"}\n".as_bytes(),
    );
    let error_profile = write_temp_file(
        &dir,
        "error.toml",
        fixed_cp037_encode_profile("error").as_bytes(),
    );
    let error_out = dir.path().join("error.bin");
    cmd()
        .args(["encode", "--profile"])
        .arg(&error_profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&error_out)
        .assert()
        .failure()
        .code(5)
        .stderr(predicate::str::contains("CBKC301_INVALID_EBCDIC_BYTE"));

    let replace_profile = write_temp_file(
        &dir,
        "replace.toml",
        fixed_cp037_encode_profile("replace").as_bytes(),
    );
    let replace_out = dir.path().join("replace.bin");
    cmd()
        .args(["encode", "--profile"])
        .arg(&replace_profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&replace_out)
        .assert()
        .success();
    let replaced = std::fs::read(&replace_out).expect("read output");
    assert_eq!(
        &replaced[0..2],
        &[0xC1, 0x6F],
        "A? in CP037, got: {replaced:02X?}"
    );

    let skip_profile = write_temp_file(
        &dir,
        "skip.toml",
        fixed_cp037_encode_profile("skip").as_bytes(),
    );
    let skip_out = dir.path().join("skip.bin");
    cmd()
        .args(["encode", "--profile"])
        .arg(&skip_profile)
        .arg(&cpy)
        .arg(&input)
        .args(["--output"])
        .arg(&skip_out)
        .assert()
        .success();
    let skipped = std::fs::read(&skip_out).expect("read output");
    assert_eq!(&skipped[0..1], &[0xC1], "dropped char, got: {skipped:02X?}");
    assert_ne!(replaced, skipped, "replace and skip must differ");
}

#[test]
fn encode_ignores_decode_unmappable_policy() {
    // `decode.unmappable` still steers nothing on encode: error vs
    // replace there leave the `[encode]` default in force.
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let mut outputs = Vec::new();
    for policy in ["error", "replace"] {
        let profile = write_temp_file(
            &dir,
            &format!("{policy}.toml"),
            fixed_ascii_profile_with("unmappable", policy).as_bytes(),
        );
        let out = dir.path().join(format!("{policy}.bin"));
        cmd()
            .args(["encode", "--profile"])
            .arg(&profile)
            .arg(&cpy)
            .arg(&input)
            .args(["--output"])
            .arg(&out)
            .assert()
            .success();
        outputs.push(std::fs::read(&out).expect("read output"));
    }
    assert_eq!(
        outputs[0], outputs[1],
        "encode output must not depend on decode.unmappable"
    );
}

#[test]
fn encode_ignores_profile_json_numbers() {
    // Same numeric payload under lossless vs native: byte-identical
    // records, proving `decode.json_numbers` steers nothing on encode.
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(&dir, "input.jsonl", SIMPLE_JSONL.as_bytes());
    let mut outputs = Vec::new();
    for mode in ["lossless", "native"] {
        let profile = write_temp_file(
            &dir,
            &format!("{mode}.toml"),
            fixed_ascii_profile_with("json_numbers", mode).as_bytes(),
        );
        let out = dir.path().join(format!("{mode}.bin"));
        cmd()
            .args(["encode", "--profile"])
            .arg(&profile)
            .arg(&cpy)
            .arg(&input)
            .args(["--output"])
            .arg(&out)
            .assert()
            .success();
        outputs.push(std::fs::read(&out).expect("read output"));
    }
    assert_eq!(
        outputs[0], outputs[1],
        "encode output must not depend on decode.json_numbers"
    );
    assert_eq!(
        outputs[0],
        expected_simple_record(),
        "both modes emit the expected record"
    );
}

// =========================================================================
// Encode/projection evidence (#1120 slice 4): equivalent combined
// invocations reproduce profile-only bytes; codepage bytes are pinned
// against the EBCDIC standard, not the product encoder.
// =========================================================================

/// Profile-only encode must match the same run with equal explicit
/// flags: equal values agree (no contradiction) and reproduce
/// profile-only bytes. Provenance follows the flag on agreement by
/// implemented contract; this pins the byte equivalence.
fn assert_encode_combined_matches_profile_only(kind: &str) {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "profile.toml", ascii_profile(kind).as_bytes());
    let alone_out = dir.path().join("alone.bin");
    let combined_out = dir.path().join("combined.bin");
    let copybook = workspace_path("fixtures/corpus/mini.cpy");
    let records = workspace_path("fixtures/corpus/mini.jsonl");

    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&alone_out)
        .arg(&copybook)
        .arg(&records)
        .assert()
        .success();
    cmd()
        .args(["encode", "--profile"])
        .arg(&profile)
        .args(["--format", kind, "--codepage", "ascii", "--output"])
        .arg(&combined_out)
        .arg(&copybook)
        .arg(&records)
        .assert()
        .success();

    let alone = std::fs::read(&alone_out).expect("read profile-only output");
    let combined = std::fs::read(&combined_out).expect("read combined output");
    assert_eq!(
        combined, alone,
        "profile + equal flags must reproduce profile-only {kind} bytes"
    );
}

#[test]
fn encoding_combined_matches_profile_only_fixed() {
    assert_encode_combined_matches_profile_only("fixed");
}

#[test]
fn encoding_combined_matches_profile_only_rdw() {
    assert_encode_combined_matches_profile_only("rdw");
}

#[test]
fn encoding_combined_matches_profile_only_vb() {
    assert_encode_combined_matches_profile_only("vb");
}

#[test]
fn encoding_cp037_cp500_bracket_bytes() {
    // `[` is the classic discriminator: 0xBA in CP037, 0x4A in CP500
    // (EBCDIC standard; capitals and space are invariant). Full-record
    // literals, not encoder output.
    let dir = tempfile::tempdir().expect("tempdir");
    let cpy = write_temp_file(&dir, "schema.cpy", SIMPLE_CPY.as_bytes());
    let input = write_temp_file(
        &dir,
        "input.jsonl",
        "{\"NAME\":\"A[bCDEFGH\",\"AMOUNT\":\"00100\"}\n".as_bytes(),
    );
    let expected_037: Vec<u8> = vec![
        0xC1, 0xBA, 0x82, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0x40, 0xF0, 0xF0, 0xF1, 0xF0, 0xF0,
    ];
    let expected_500: Vec<u8> = vec![
        0xC1, 0x4A, 0x82, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0x40, 0xF0, 0xF0, 0xF1, 0xF0, 0xF0,
    ];
    assert_ne!(expected_037, expected_500, "codepages must discriminate");
    for (codepage, expected) in [("cp037", &expected_037), ("cp500", &expected_500)] {
        let out = dir.path().join(format!("{codepage}.bin"));
        cmd()
            .args([
                "encode",
                "--format",
                "fixed",
                "--codepage",
                codepage,
                "--output",
            ])
            .arg(&out)
            .arg(&cpy)
            .arg(&input)
            .assert()
            .success();
        assert_eq!(
            &std::fs::read(&out).expect("read output"),
            expected,
            "{codepage} must emit the standard bytes"
        );
    }
}

// =========================================================================
// Determinism consumption (#1120 slice 5)
// =========================================================================
// Determinism resolves the same profile layers as the operating commands,
// names the comparison kind, records profile/input/output identities, and
// states the evidence it cannot supply.

/// Profile-driven decode determinism passes and names the comparison kind
/// plus the bound profile fingerprint.
#[test]
fn determinism_decode_profile_passes_with_identity() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains("Comparison: decode"))
        .stdout(predicate::str::contains("Profile: sha256:"))
        .stdout(predicate::str::contains("DETERMINISTIC"))
        .stdout(predicate::str::contains("Limitations:"))
        .stdout(predicate::str::contains(
            "worker count cannot change ordering or verdict",
        ));
}

/// Profile-driven encode determinism proves the write side under the same
/// profile that governs `encode`.
#[test]
fn determinism_encode_profile_passes_with_identity() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let jsonl = dir.path().join("records.jsonl");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&jsonl)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success();

    cmd()
        .args(["determinism", "encode", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&jsonl)
        .assert()
        .success()
        .stdout(predicate::str::contains("Comparison: encode"))
        .stdout(predicate::str::contains("Profile: sha256:"))
        .stdout(predicate::str::contains("DETERMINISTIC"));
}

/// Profile-driven round-trip determinism passes and labels itself internal
/// self-consistency rather than an independent external oracle.
#[test]
fn determinism_round_trip_profile_states_oracle_limitation() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["determinism", "round-trip", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains("Comparison: round-trip"))
        .stdout(predicate::str::contains("DETERMINISTIC"))
        .stdout(predicate::str::contains(
            "not an independent external oracle",
        ));
}

/// Profile-only and profile-plus-equal-flags determinism runs are
/// byte-identical: equal flags confirm intent without changing the run.
#[test]
fn determinism_profile_matches_profile_plus_equal_flags() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    let profile_only = cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .output()
        .expect("profile-only determinism");
    assert_eq!(profile_only.status.code(), Some(0));

    let with_equal_flags = cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .args(["--format", "fixed", "--codepage", "cp037"])
        .arg(&copybook)
        .arg(&data)
        .output()
        .expect("profile-plus-equal-flags determinism");
    assert_eq!(with_equal_flags.status.code(), Some(0));
    assert_eq!(
        with_equal_flags.stdout, profile_only.stdout,
        "equal flags must confirm intent without changing the determinism report"
    );
}

/// The JSON report binds comparison kind, profile fingerprint, input hash,
/// output verdict, and limitations in one machine-readable envelope.
#[test]
fn determinism_json_report_binds_identities() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    let output = cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .args(["--output", "json"])
        .arg(&copybook)
        .arg(&data)
        .output()
        .expect("profile determinism json");
    assert_eq!(output.status.code(), Some(0));
    let stdout = String::from_utf8(output.stdout).expect("utf8 stdout");
    let parsed: serde_json::Value =
        serde_json::from_str(&stdout).expect("determinism JSON report parses");
    assert_eq!(parsed["comparison"], "decode");
    assert_eq!(parsed["profile"]["kind"], "profile");
    let fingerprint = parsed["profile"]["fingerprint"]
        .as_str()
        .expect("fingerprint");
    assert_eq!(fingerprint.len(), 64, "fingerprint is sha256 hex");
    assert!(fingerprint.bytes().all(|byte| byte.is_ascii_hexdigit()));
    let input_hash = parsed["input_hash"].as_str().expect("input hash");
    assert_eq!(input_hash.len(), 64, "input hash is blake3 hex");
    assert_eq!(parsed["result"]["is_deterministic"], true);
    assert_eq!(
        parsed["limitations"].as_array().expect("limitations").len(),
        2
    );
}

/// A reviewed bound equal to the fixed LRECL (50) still passes: the bound
/// is inclusive, matching the operating path.
#[test]
fn determinism_decode_bound_at_lrecl_passes() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile_toml = FIXED_CP037_PROFILE.replace(
        "maximum_record_length = 32760",
        "maximum_record_length = 50",
    );
    let profile = write_temp_file(&dir, "bound50.toml", profile_toml.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success()
        .stdout(predicate::str::contains("DETERMINISTIC"));
}

/// A reviewed bound below the fixed LRECL (49 < 50) rejects the comparison
/// with the same identity and exit code as `decode` with the same profile.
#[test]
fn determinism_decode_bound_below_lrecl_rejects_like_decode() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile_toml = FIXED_CP037_PROFILE.replace(
        "maximum_record_length = 32760",
        "maximum_record_length = 49",
    );
    let profile = write_temp_file(&dir, "bound49.toml", profile_toml.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"))
        .stdout(predicate::str::contains("DETERMINISTIC").not());

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
}

/// A reviewed bound below the encoded payload rejects the write-side
/// comparison with the same identity and exit code as `encode`.
#[test]
fn determinism_encode_bound_rejects_like_encode() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let profile_toml = FIXED_CP037_PROFILE.replace(
        "maximum_record_length = 32760",
        "maximum_record_length = 49",
    );
    let bound = write_temp_file(&dir, "bound49.toml", profile_toml.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");
    let jsonl = dir.path().join("records.jsonl");
    let out = dir.path().join("out.bin");

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&jsonl)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .success();

    cmd()
        .args(["determinism", "encode", "--profile"])
        .arg(&bound)
        .arg(&copybook)
        .arg(&jsonl)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));

    cmd()
        .args(["encode", "--profile"])
        .arg(&bound)
        .args(["--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&jsonl)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
}

/// A strict reserved policy rejects an RDW record with non-zero reserved
/// bytes on the comparison with the same identity and exit code as
/// `decode` with the same profile.
#[test]
fn determinism_rdw_strict_reserved_rejects_like_decode() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "strict.toml", rdw_profile("strict").as_bytes());
    let copybook = write_temp_file(&dir, "rec.cpy", SIMPLE_CPY.as_bytes());
    let data = write_temp_file(&dir, "record.bin", &rdw_record_nonzero_reserved());
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKR211_RDW_RESERVED_NONZERO"))
        .stdout(predicate::str::contains("DETERMINISTIC").not());

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKR211_RDW_RESERVED_NONZERO"));
}

/// A reviewed bound below an RDW declared payload rejects the comparison
/// with the same identity and exit code as `decode`.
#[test]
fn determinism_rdw_declared_over_bound_rejects_like_decode() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile_toml = rdw_profile("lenient")
        .replace("maximum_record_length = 32760", "maximum_record_length = 5");
    let profile = write_temp_file(&dir, "bound5.toml", profile_toml.as_bytes());
    let copybook = write_temp_file(&dir, "rec.cpy", SIMPLE_CPY.as_bytes());
    let mut record = rdw_record_nonzero_reserved();
    record[3] = 0x00;
    let data = write_temp_file(&dir, "record.bin", &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));

    cmd()
        .args(["decode", "--profile"])
        .arg(&profile)
        .args(["--output"])
        .arg(&out)
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(4)
        .stderr(predicate::str::contains("CBKF226_RECORD_BOUND_EXCEEDED"));
}

/// A flag that contradicts the profile fails with exit 3 before any
/// comparison output is produced.
#[test]
fn determinism_profile_format_conflict_fails_before_output() {
    let dir = tempfile::tempdir().expect("tempdir");
    let profile = write_temp_file(&dir, "fixed.toml", FIXED_CP037_PROFILE.as_bytes());
    let copybook = workspace_path("fixtures/copybooks/simple.cpy");
    let data = workspace_path("fixtures/data/simple.bin");

    cmd()
        .args(["determinism", "decode", "--profile"])
        .arg(&profile)
        .args(["--format", "rdw"])
        .arg(&copybook)
        .arg(&data)
        .assert()
        .failure()
        .code(3)
        .stderr(predicate::str::contains("conflicting"))
        .stderr(predicate::str::contains("framing.kind"))
        .stdout(predicate::str::contains("DETERMINISTIC").not());
}
