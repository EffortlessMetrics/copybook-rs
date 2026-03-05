// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for decode CLI flags: `--emit-meta`, `--emit-raw`, `--on-decode-unmappable`.
//!
//! These flags control metadata injection, raw data capture, and
//! unmappable-character handling during decode.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use serde_json::Value;
use tempfile::TempDir;

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").unwrap()
}

/// Simple ASCII copybook for basic flag testing.
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME     PIC X(10).
           05  AMOUNT   PIC 9(5).
";

/// Build ASCII record: 10 bytes name + 5 bytes amount = 15 bytes.
fn make_ascii_record(name: &str, amount: &str) -> Vec<u8> {
    let mut rec = vec![b' '; 15];
    for (i, b) in name.bytes().enumerate().take(10) {
        rec[i] = b;
    }
    for (i, b) in amount.bytes().enumerate().take(5) {
        rec[10 + i] = b;
    }
    rec
}

/// Write copybook and data to temp dir, return (dir, cpy_path, data_path).
fn setup(cpy_text: &str, data: &[u8]) -> (TempDir, std::path::PathBuf, std::path::PathBuf) {
    let dir = TempDir::new().unwrap();
    let cpy = dir.path().join("schema.cpy");
    let data_path = dir.path().join("data.bin");
    std::fs::write(&cpy, cpy_text).unwrap();
    std::fs::write(&data_path, data).unwrap();
    (dir, cpy, data_path)
}

// =========================================================================
// --emit-meta tests
// =========================================================================

#[test]
fn decode_emit_meta_includes_metadata_fields() {
    let record = make_ascii_record("ALICE", "00100");
    let (dir, cpy, data) = setup(SIMPLE_CPY, &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-meta",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let json: Value = serde_json::from_str(content.trim()).unwrap();

    // --emit-meta should add schema_fingerprint, offset, and length
    assert!(
        json.get("schema_fingerprint").is_some(),
        "Missing schema_fingerprint: {json}"
    );
    assert!(json.get("length").is_some(), "Missing length: {json}");

    // length should equal the record size (15 bytes)
    assert_eq!(json["length"], 15, "Record length should be 15");
}

#[test]
fn decode_without_emit_meta_omits_metadata() {
    let record = make_ascii_record("BOB", "00200");
    let (dir, cpy, data) = setup(SIMPLE_CPY, &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let json: Value = serde_json::from_str(content.trim()).unwrap();

    // Without --emit-meta, these fields should not be present
    assert!(
        json.get("schema_fingerprint").is_none(),
        "schema_fingerprint should not appear without --emit-meta"
    );
    assert!(
        json.get("offset").is_none(),
        "offset should not appear without --emit-meta"
    );
}

// =========================================================================
// --emit-raw tests
// =========================================================================

#[test]
fn decode_emit_raw_record_includes_base64() {
    let record = make_ascii_record("CAROL", "00300");
    let (dir, cpy, data) = setup(SIMPLE_CPY, &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-raw",
            "record",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let json: Value = serde_json::from_str(content.trim()).unwrap();

    // --emit-raw record should produce raw_b64 field
    let raw = json
        .get("raw_b64")
        .expect("Missing raw_b64 field with --emit-raw record");
    assert!(raw.is_string(), "raw_b64 should be a string");

    // Verify base64 decodes back to our original record
    use base64::Engine;
    let decoded = base64::engine::general_purpose::STANDARD
        .decode(raw.as_str().unwrap())
        .expect("raw_b64 should be valid base64");
    assert_eq!(
        decoded, record,
        "Decoded raw bytes should match input record"
    );
}

#[test]
fn decode_emit_raw_off_omits_base64() {
    let record = make_ascii_record("DAVE", "00400");
    let (dir, cpy, data) = setup(SIMPLE_CPY, &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-raw",
            "off",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let json: Value = serde_json::from_str(content.trim()).unwrap();

    assert!(
        json.get("raw_b64").is_none(),
        "raw_b64 should not appear with --emit-raw off"
    );
    assert!(
        json.get("__raw_b64").is_none(),
        "__raw_b64 should not appear with --emit-raw off"
    );
}

// =========================================================================
// --emit-meta + --emit-raw combined
// =========================================================================

#[test]
fn decode_emit_meta_and_raw_combined() {
    let record = make_ascii_record("EVE", "00500");
    let (dir, cpy, data) = setup(SIMPLE_CPY, &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "ascii",
            "--emit-meta",
            "--emit-raw",
            "record",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let json: Value = serde_json::from_str(content.trim()).unwrap();

    // Both metadata and raw should be present
    assert!(json.get("schema_fingerprint").is_some(), "Missing metadata");
    assert!(json.get("length").is_some(), "Missing length");
    assert!(json.get("raw_b64").is_some(), "Missing raw_b64");
}

// =========================================================================
// --on-decode-unmappable tests (requires EBCDIC codepage; ASCII is pass-through)
// =========================================================================

/// Build a CP037 EBCDIC record: 10 bytes name + 5 bytes amount = 15 bytes.
/// Uses basic EBCDIC encoding for ASCII characters.
fn make_cp037_record(name_bytes: &[u8], amount_bytes: &[u8]) -> Vec<u8> {
    let mut rec = vec![0x40u8; 15]; // 0x40 = EBCDIC space
    for (i, &b) in name_bytes.iter().enumerate().take(10) {
        rec[i] = b;
    }
    for (i, &b) in amount_bytes.iter().enumerate().take(5) {
        rec[10 + i] = b;
    }
    rec
}

#[test]
fn decode_unmappable_replace_produces_replacement_char() {
    // Byte 0x00 in CP037 maps to U+0000 (NUL), which is unmappable
    let record = make_cp037_record(
        &[0x00, 0xC5, 0xD3, 0xD3, 0xD6, 0x40, 0x40, 0x40, 0x40, 0x40], // NUL + "ELLO     "
        &[0xF0, 0xF0, 0xF1, 0xF0, 0xF0],                               // "00100"
    );

    let (dir, cpy, data) = setup(SIMPLE_CPY, &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--on-decode-unmappable",
            "replace",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .success();

    let content = std::fs::read_to_string(&out).unwrap();
    let json: Value = serde_json::from_str(content.trim()).unwrap();
    let name = json["fields"]["NAME"]
        .as_str()
        .or_else(|| json["NAME"].as_str())
        .expect("NAME field should exist");

    // The unmappable byte should be replaced with U+FFFD
    assert!(
        name.contains('\u{FFFD}'),
        "NAME should contain replacement character U+FFFD, got: {name:?}"
    );
}

#[test]
fn decode_unmappable_error_rejects_bad_byte() {
    // Byte 0x00 in CP037 maps to U+0000 (NUL), which is unmappable
    let record = make_cp037_record(
        &[0x00, 0xC5, 0xD3, 0xD3, 0xD6, 0x40, 0x40, 0x40, 0x40, 0x40],
        &[0xF0, 0xF0, 0xF1, 0xF0, 0xF0],
    );

    let (dir, cpy, data) = setup(SIMPLE_CPY, &record);
    let out = dir.path().join("out.jsonl");

    cmd()
        .args([
            "decode",
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--on-decode-unmappable",
            "error",
            "--output",
        ])
        .arg(&out)
        .arg(&cpy)
        .arg(&data)
        .assert()
        .failure();
}
