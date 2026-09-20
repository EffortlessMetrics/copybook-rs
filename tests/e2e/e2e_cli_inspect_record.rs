// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for `inspect` record-specific ownership queries (toward #1122).
//!
//! `--input FILE` with `--record N` answers one ownership query inside the
//! Nth decoded record (1-based, matching decode's `record_index`): ODO
//! tables clamp to that record's actual counts, so a table the static
//! answer reports as `possible` can report `absent` in a zero-count
//! record. Selection failures (contradictory flags, short inputs,
//! unreadable files) fail closed with exit 3 instead of falling back to
//! static bounds.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use std::io::Write as _;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

fn write_temp_file(dir: &tempfile::TempDir, name: &str, contents: &[u8]) -> std::path::PathBuf {
    let path = dir.path().join(name);
    let mut file = std::fs::File::create(&path).expect("create temp file");
    file.write_all(contents).expect("write temp file");
    path
}

/// Fixed ODO copybook: `CNT` controls a 0-to-2 table of 2-byte cells.
/// Maximum extent is 6 bytes, so fixed framing strides by 6.
const ODO_COPYBOOK: &str = "       01  REC.\n           05  CNT           PIC 9(2).\n           05  TBL OCCURS 0 TO 2 TIMES\n               DEPENDING ON CNT.\n               10  VAL         PIC X(2).\n";

/// Two max-padded records: record 1 holds two cells, record 2 holds none.
const ODO_RECORDS: &[u8] = b"02AABB00    ";

fn odo_fixture(dir: &tempfile::TempDir) -> (std::path::PathBuf, std::path::PathBuf) {
    let copybook = write_temp_file(dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    let input = write_temp_file(dir, "odo.bin", ODO_RECORDS);
    (copybook, input)
}

/// A present ODO table answers owned with its actual count echoed.
#[test]
fn inspect_record_field_query_present_odo_reports_guaranteed() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (copybook, input) = odo_fixture(&dir);

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .success()
        .stdout(predicates::str::contains("Record: #1"))
        .stdout(predicates::str::contains("REC.TBL=2"))
        .stdout(predicates::str::contains("State: owned"));
}

/// A zero-count ODO table has no extent in its record: the same path the
/// static answer reports as `possible` reports `absent` here.
#[test]
fn inspect_record_field_query_zero_odo_reports_absent() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (copybook, input) = odo_fixture(&dir);

    // Static first: byte 3 sits in a table occurrence that is possible
    // over repetition bounds.
    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--payload-byte", "3"])
        .assert()
        .success()
        .stdout(predicates::str::contains("State: owned"))
        .stdout(predicates::str::contains("possible"));

    // Record 2 decodes zero cells: the table is absent, not merely possible.
    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "2"])
        .assert()
        .success()
        .stdout(predicates::str::contains("Record: #2"))
        .stdout(predicates::str::contains("REC.TBL=0"))
        .stdout(predicates::str::contains("State: absent"));
}

/// Record byte queries resolve to the true within-occurrence child with
/// guaranteed presence.
#[test]
fn inspect_record_byte_query_resolves_within_occurrence() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (copybook, input) = odo_fixture(&dir);

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--payload-byte", "3"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .success()
        .stdout(predicates::str::contains("REC.TBL.VAL"))
        .stdout(predicates::str::contains("guaranteed"));
}

/// Record JSON answers echo the interpreted record with its counts and
/// carry no local paths.
#[test]
fn inspect_record_query_json_echoes_record_context() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (copybook, input) = odo_fixture(&dir);

    let output = cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL", "--output", "json"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "2"])
        .output()
        .expect("record query json");
    assert_eq!(output.status.code(), Some(0));
    let stdout = String::from_utf8(output.stdout).expect("utf8 stdout");
    let parsed: serde_json::Value = serde_json::from_str(&stdout).expect("answer parses");
    assert_eq!(parsed["state"], "absent");
    assert_eq!(parsed["record"]["index"], 2);
    assert_eq!(parsed["record"]["odo_counts"][0]["table_path"], "REC.TBL");
    assert_eq!(parsed["record"]["odo_counts"][0]["actual"], 0);
    assert_eq!(parsed["record_len"], 6);
    let dir_str = dir.path().to_str().expect("tempdir utf8");
    assert!(
        !stdout.contains(dir_str),
        "machine output leaks a local path"
    );
}

/// Half a record selection is a contradiction, never a silent static answer.
#[test]
fn inspect_record_selection_needs_both_sides() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (copybook, input) = odo_fixture(&dir);

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(&input)
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("--input needs --record"))
        .stderr(predicates::str::contains("subcode=407"));

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL", "--record", "1"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("--record needs --input"))
        .stderr(predicates::str::contains("subcode=407"));

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "0"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("1-based"))
        .stderr(predicates::str::contains("subcode=407"));
}

/// Record queries decode through COPYBOOK: `--manifest` binds no schema.
#[test]
fn inspect_record_selection_refuses_manifest_input() {
    let dir = tempfile::tempdir().expect("tempdir");
    let manifest = write_temp_file(&dir, "bound.manifest.json", b"{}");

    cmd()
        .args(["inspect", "--manifest"])
        .arg(&manifest)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(dir.path().join("odo.bin"))
        .args(["--record", "1"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("decode through COPYBOOK"))
        .stderr(predicates::str::contains("subcode=407"));
}

/// Selecting past the end of the input fails closed with the input length.
#[test]
fn inspect_record_selection_past_end_fails() {
    let dir = tempfile::tempdir().expect("tempdir");
    let (copybook, input) = odo_fixture(&dir);

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "3"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("holds only 2 records"))
        .stderr(predicates::str::contains("subcode=408"));
}

/// A short RDW record clamps container ranges to the payload it holds:
///
/// the 4-byte record below fills one of two ODO cells of a 6-byte maximum,
/// so the record group answers 0..4, never the static 0..6.
#[test]
fn inspect_record_query_short_rdw_record_clamps_ranges() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    // RDW header: 4 payload bytes, zero reserved bytes; then CNT="01" and
    // one cell.
    let input = write_temp_file(
        &dir,
        "short.rdw",
        &[0x00, 0x04, 0x00, 0x00, b'0', b'1', b'A', b'A'],
    );

    cmd()
        .args(["inspect", "--format", "rdw", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--payload-byte", "2"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .success()
        .stdout(predicates::str::contains("Record: #1"))
        .stdout(predicates::str::contains("REC.TBL=1"))
        .stdout(predicates::str::contains("State: owned"))
        .stdout(predicates::str::contains("0..4"));
}

/// Record selection decodes under the reviewed profile: a strict
/// reserved-bytes framing rejects the nonzero RDW header instead of
/// answering over it.
#[test]
fn inspect_record_query_honors_profile_framing_policy() {
    const RDW_STRICT_PROFILE: &str = "schema_version = 1\n[source]\ndialect = \"normative\"\n[framing]\nkind = \"rdw\"\nreserved_bytes = \"strict\"\n[decode]\ncodepage = \"ascii\"\nunmappable = \"error\"\njson_numbers = \"lossless\"\n[limits]\nmaximum_record_length = 32760\nmaximum_errors = 100\n";
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    let profile = write_temp_file(&dir, "rdw-strict.toml", RDW_STRICT_PROFILE.as_bytes());
    // Same short record, but the RDW reserved bytes are nonzero.
    let input = write_temp_file(
        &dir,
        "strict.rdw",
        &[0x00, 0x04, 0x00, 0x01, b'0', b'1', b'A', b'A'],
    );

    cmd()
        .args(["inspect", "--profile"])
        .arg(&profile)
        .arg(&copybook)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("RDW_RESERVED_NONZERO"))
        .stderr(predicates::str::contains("subcode=411"));
}

/// A record that fails to decode still answers: the selector resolves
/// over static bounds with the decoder's refusal attached, plus a runnable
/// pointer at its `explain` occurrence.
#[test]
fn inspect_record_query_undecodable_answers_static_with_note() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    // CNT="XX" is not zoned decimal: the record cannot decode.
    let input = write_temp_file(&dir, "bad.bin", b"XXAABB");

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.CNT"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .success()
        .stdout(predicates::str::contains("State: owned"))
        .stdout(predicates::str::contains("REC.CNT"))
        .stdout(predicates::str::contains("failed to decode"))
        .stdout(predicates::str::contains("CBKD411_ZONED_BAD_SIGN"))
        .stdout(predicates::str::contains("static repetition bounds"))
        .stdout(predicates::str::contains(
            "See: copybook explain CBKD411_ZONED_BAD_SIGN",
        ));
}

/// Byte queries over an undecodable record name the static owner with the
/// refusal attached.
#[test]
fn inspect_record_query_byte_on_undecodable_answers_static() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    let input = write_temp_file(&dir, "bad.bin", b"XXAABB");

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--payload-byte", "0"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .success()
        .stdout(predicates::str::contains("REC.CNT"))
        .stdout(predicates::str::contains("CBKD411_ZONED_BAD_SIGN"));
}

/// The JSON note carries the refusal identity with field and byte location
/// and no local paths; the failure pointer stays human-only.
#[test]
fn inspect_record_query_undecodable_json_carries_note() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    let input = write_temp_file(&dir, "bad.bin", b"XXAABB");

    let output = cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.CNT", "--output", "json"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .output()
        .expect("undecodable record query json");
    assert_eq!(output.status.code(), Some(0));
    let stdout = String::from_utf8(output.stdout).expect("utf8 stdout");
    let parsed: serde_json::Value = serde_json::from_str(&stdout).expect("answer parses");
    assert_eq!(parsed["state"], "owned");
    assert_eq!(parsed["record"]["index"], 1);
    assert_eq!(parsed["decode_note"]["code"], "CBKD411_ZONED_BAD_SIGN");
    assert!(
        parsed["decode_note"]["message"]
            .as_str()
            .expect("message")
            .contains("zone"),
        "note carries the decoder message"
    );
    assert_eq!(parsed["decode_note"]["field_path"], "REC.CNT");
    assert_eq!(parsed["decode_note"]["byte_offset"], 0);
    let dir_str = dir.path().to_str().expect("tempdir utf8");
    assert!(
        !stdout.contains(dir_str),
        "machine output leaks a local path"
    );
}

/// A byte past a short undecodable payload names nothing: the answer is
/// out of range with the refusal attached, not its static owner.
#[test]
fn inspect_record_query_byte_past_short_payload_is_out_of_range() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    // 4-byte RDW payload of a 6-byte maximum; CNT="XX" fails decoding.
    let input = write_temp_file(
        &dir,
        "short-bad.rdw",
        &[0x00, 0x04, 0x00, 0x00, b'X', b'X', b'A', b'A'],
    );

    cmd()
        .args(["inspect", "--format", "rdw", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--payload-byte", "5"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .success()
        .stdout(predicates::str::contains("out of range"))
        .stdout(predicates::str::contains("CBKD411_ZONED_BAD_SIGN"));
}

/// The failure pointer replays the resolved interpretation: strict mode
/// and dialect ride the hint so the occurrence reproduces the failure.
#[test]
fn inspect_record_query_hint_replays_strict_interpretation() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    let input = write_temp_file(&dir, "bad.bin", b"XXAABB");

    let output = cmd()
        .args([
            "inspect",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--strict",
            "--dialect",
            "0",
        ])
        .arg(&copybook)
        .args(["--field", "REC.CNT"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .output()
        .expect("strict failing-record query");
    assert_eq!(output.status.code(), Some(0));
    let stdout = String::from_utf8(output.stdout).expect("utf8 stdout");
    assert!(stdout.contains("--strict"), "hint replays strict mode");
    assert!(
        stdout.contains("--dialect 0"),
        "hint replays the resolved dialect"
    );
}

/// A truncated fixed payload has no bytes to interpret: framing failures
/// still fail closed instead of answering.
#[test]
fn inspect_record_query_truncated_payload_stays_closed() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());
    let input = write_temp_file(&dir, "short.bin", b"01");

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.CNT"])
        .args(["--input"])
        .arg(&input)
        .args(["--record", "1"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("subcode=411"));
}

/// An unreadable record file names the file with its own subcode.
#[test]
fn inspect_record_selection_unreadable_input_fails() {
    let dir = tempfile::tempdir().expect("tempdir");
    let copybook = write_temp_file(&dir, "odo.cpy", ODO_COPYBOOK.as_bytes());

    cmd()
        .args(["inspect", "--format", "fixed", "--codepage", "ascii"])
        .arg(&copybook)
        .args(["--field", "REC.TBL"])
        .args(["--input"])
        .arg(dir.path().join("missing.bin"))
        .args(["--record", "1"])
        .assert()
        .failure()
        .code(3)
        .stderr(predicates::str::contains("cannot read input"))
        .stderr(predicates::str::contains("subcode=411"));
}
