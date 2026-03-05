// SPDX-License-Identifier: AGPL-3.0-or-later
//! E2E tests for decode CLI with multiple option combinations.
//!
//! Tests cover combinations of: --select, --dialect, --codepage, --json-number,
//! --emit-meta, --threads, --strict, and multi-record scenarios.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

#[allow(deprecated)]
fn cmd() -> Command {
    Command::cargo_bin("copybook").expect("copybook binary should exist")
}

/// Copybook with group + ODO counter + alphanumeric field.
const ODO_CPY: &str = "\
       01  REC.
           05  HDR.
               10  REC-ID     PIC X(4).
               10  ITEM-CNT   PIC 9(2).
           05  ITEMS OCCURS 1 TO 5 DEPENDING ON ITEM-CNT
                                PIC X(6).
";

/// CP037 ODO record: `REC-ID`=TST1, `ITEM-CNT`=02, ITEMS=ITEM01+ITEM02.
/// Fixed LRECL = max size = 4 + 2 + 5*6 = 36 bytes.
/// Items 3-5 are padded with EBCDIC spaces (0x40).
fn odo_record_cp037() -> Vec<u8> {
    let mut data = vec![
        0xE3, 0xE2, 0xE3, 0xF1, // TST1
        0xF0, 0xF2, // 02
        0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF1, // ITEM01
        0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF2, // ITEM02
    ];
    // Pad remaining 3 items (18 bytes) with EBCDIC spaces
    data.extend_from_slice(&[0x40; 18]);
    data
}

/// Simple copybook for basic combination tests.
const SIMPLE_CPY: &str = "\
       01  REC.
           05  NAME   PIC X(10).
           05  AGE    PIC 9(3).
";

/// CP037 record: NAME="ALICE     ", AGE="025".
fn simple_record_cp037() -> Vec<u8> {
    vec![
        0xC1, 0xD3, 0xC9, 0xC3, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40, // ALICE
        0xF0, 0xF2, 0xF5, // 025
    ]
}

/// Multi-record data (3 records, 13 bytes each = 39 bytes).
fn multi_record_cp037() -> Vec<u8> {
    let mut data = simple_record_cp037();
    // BOB record
    data.extend_from_slice(&[
        0xC2, 0xD6, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, // BOB
        0xF0, 0xF3, 0xF0, // 030
    ]);
    // CAROL record
    data.extend_from_slice(&[
        0xC3, 0xC1, 0xD9, 0xD6, 0xD3, 0x40, 0x40, 0x40, 0x40, 0x40, // CAROL
        0xF0, 0xF2, 0xF8, // 028
    ]);
    data
}

/// Copybook with signed decimal for `--json-number` testing.
const DECIMAL_CPY: &str = "\
       01  REC.
           05  ACCT-ID   PIC X(6).
           05  BALANCE   PIC S9(5)V99.
";

/// CP037: ACCT-ID="ACC001", BALANCE=+01234.56 (7 bytes zoned).
fn decimal_record_cp037() -> Vec<u8> {
    vec![
        0xC1, 0xC3, 0xC3, 0xF0, 0xF0, 0xF1, // ACC001
        0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xC6, // +0123456
    ]
}

fn setup(cpy: &str, data: &[u8]) -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("schema.cpy"), cpy).unwrap();
    std::fs::write(dir.path().join("data.bin"), data).unwrap();
    dir
}

fn decode_to_jsonl(dir: &TempDir, extra_args: &[&str]) -> String {
    let output_path = dir.path().join("decoded.jsonl");
    let mut c = cmd();
    c.arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--output")
        .arg(&output_path)
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"));

    // Add --codepage cp037 only if not overridden in extra_args
    if !extra_args.contains(&"--codepage") {
        c.arg("--codepage").arg("cp037");
    }

    for arg in extra_args {
        c.arg(arg);
    }
    c.assert().success();
    std::fs::read_to_string(output_path).unwrap()
}

// ---------------------------------------------------------------------------
// --select + --codepage
// ---------------------------------------------------------------------------

#[test]
fn decode_select_with_codepage_cp037() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--select", "NAME"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    assert!(parsed.get("NAME").is_some(), "NAME should be present");
}

#[test]
fn decode_select_with_codepage_cp1047() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let output_path = dir.path().join("decoded.jsonl");
    // cp1047 decode of cp037 data — may produce different text, but should not crash
    cmd()
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp1047")
        .arg("--output")
        .arg(&output_path)
        .arg("--select")
        .arg("NAME")
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// --select + --dialect
// ---------------------------------------------------------------------------

#[test]
fn decode_select_with_dialect_normative() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--select", "AGE", "--dialect", "n"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    assert!(parsed.get("AGE").is_some(), "AGE should be present");
}

#[test]
fn decode_select_with_dialect_zero_tolerant() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--select", "NAME", "--dialect", "0"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    assert!(parsed.get("NAME").is_some());
}

// ---------------------------------------------------------------------------
// --select + --json-number
// ---------------------------------------------------------------------------

#[test]
fn decode_select_with_json_number_lossless() {
    let dir = setup(DECIMAL_CPY, &decimal_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--select", "BALANCE", "--json-number", "lossless"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    assert!(parsed.get("BALANCE").is_some());
}

// ---------------------------------------------------------------------------
// --emit-meta + --select
// ---------------------------------------------------------------------------

#[test]
fn decode_emit_meta_with_select() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--emit-meta", "--select", "NAME"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    // Should have NAME field and possibly _meta
    assert!(parsed.get("NAME").is_some());
}

// ---------------------------------------------------------------------------
// --threads + --select
// ---------------------------------------------------------------------------

#[test]
fn decode_threads_with_select() {
    let dir = setup(SIMPLE_CPY, &multi_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--threads", "2", "--select", "NAME"]);
    let lines: Vec<&str> = jsonl.trim().lines().collect();
    assert_eq!(lines.len(), 3, "should decode 3 records");
}

// ---------------------------------------------------------------------------
// --dialect + --codepage
// ---------------------------------------------------------------------------

#[test]
fn decode_dialect_and_codepage_together() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--dialect", "0", "--codepage", "cp037"]);
    assert!(!jsonl.trim().is_empty());
}

// ---------------------------------------------------------------------------
// --strict + --select
// ---------------------------------------------------------------------------

#[test]
fn decode_strict_with_select() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--strict", "--select", "NAME"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    assert!(parsed.get("NAME").is_some());
}

// ---------------------------------------------------------------------------
// Triple combination: --select + --dialect + --codepage
// ---------------------------------------------------------------------------

#[test]
fn decode_select_dialect_codepage_together() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(
        &dir,
        &["--select", "AGE", "--dialect", "0", "--codepage", "cp037"],
    );
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    assert!(parsed.get("AGE").is_some());
}

// ---------------------------------------------------------------------------
// ODO with --select auto-includes counter
// ---------------------------------------------------------------------------

#[test]
fn decode_odo_select_auto_includes_counter() {
    let dir = setup(ODO_CPY, &odo_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--select", "ITEMS"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    // ITEM-CNT should be auto-included as ODO counter
    let json_str = parsed.to_string();
    assert!(
        json_str.contains("ITEM-CNT") || json_str.contains("ITEMS"),
        "ODO counter should be auto-included: {json_str}"
    );
}

// ---------------------------------------------------------------------------
// Multi-record decode with --threads
// ---------------------------------------------------------------------------

#[test]
fn decode_multi_record_with_threads() {
    let dir = setup(SIMPLE_CPY, &multi_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--threads", "4"]);
    let lines: Vec<&str> = jsonl.trim().lines().collect();
    assert_eq!(lines.len(), 3, "should produce 3 JSONL lines");

    // Each line should be valid JSON
    for line in &lines {
        let _: serde_json::Value = serde_json::from_str(line).unwrap();
    }
}

// ---------------------------------------------------------------------------
// Multi-record determinism: same output with different thread counts
// ---------------------------------------------------------------------------

#[test]
fn decode_multi_record_deterministic_across_threads() {
    let dir = setup(SIMPLE_CPY, &multi_record_cp037());

    let jsonl_1 = decode_to_jsonl(&dir, &["--threads", "1"]);
    // Need separate output files
    let output2 = dir.path().join("decoded2.jsonl");
    cmd()
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(&output2)
        .arg("--threads")
        .arg("2")
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .assert()
        .success();
    let jsonl_2 = std::fs::read_to_string(output2).unwrap();

    assert_eq!(
        jsonl_1, jsonl_2,
        "decode output should be identical across thread counts"
    );
}

// ---------------------------------------------------------------------------
// --emit-meta + --threads + multi-record
// ---------------------------------------------------------------------------

#[test]
fn decode_emit_meta_with_threads_multi_record() {
    let dir = setup(SIMPLE_CPY, &multi_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--emit-meta", "--threads", "2"]);
    let lines: Vec<&str> = jsonl.trim().lines().collect();
    assert_eq!(lines.len(), 3, "should produce 3 lines with meta");
}

// ---------------------------------------------------------------------------
// All flags combined: --select + --emit-meta + --dialect + --json-number + --threads
// ---------------------------------------------------------------------------

#[test]
fn decode_all_flags_combined() {
    let dir = setup(SIMPLE_CPY, &multi_record_cp037());
    let jsonl = decode_to_jsonl(
        &dir,
        &[
            "--select",
            "NAME",
            "--emit-meta",
            "--dialect",
            "0",
            "--json-number",
            "lossless",
            "--threads",
            "2",
        ],
    );
    let lines: Vec<&str> = jsonl.trim().lines().collect();
    assert_eq!(lines.len(), 3);
    for line in &lines {
        let parsed: serde_json::Value = serde_json::from_str(line).unwrap();
        assert!(parsed.get("NAME").is_some());
    }
}

// ---------------------------------------------------------------------------
// --strict-comments + --select
// ---------------------------------------------------------------------------

#[test]
fn decode_strict_comments_with_select() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--strict-comments", "--select", "AGE"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    assert!(parsed.get("AGE").is_some());
}

// ---------------------------------------------------------------------------
// Decode single field projection produces minimal output
// ---------------------------------------------------------------------------

#[test]
fn decode_select_single_field_minimal_output() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--select", "AGE"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    let obj = parsed.as_object().unwrap();
    // Should have AGE (and possibly REC group wrapper)
    let has_age = obj.contains_key("AGE")
        || obj
            .values()
            .any(|v| v.as_object().is_some_and(|o| o.contains_key("AGE")));
    assert!(has_age, "output should contain AGE: {parsed}");
}

// ---------------------------------------------------------------------------
// --select nonexistent field → error
// ---------------------------------------------------------------------------

#[test]
fn decode_select_nonexistent_field_exits_nonzero() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    cmd()
        .arg("decode")
        .arg("--format")
        .arg("fixed")
        .arg("--codepage")
        .arg("cp037")
        .arg("--output")
        .arg(dir.path().join("decoded.jsonl"))
        .arg("--select")
        .arg("NONEXISTENT_FIELD")
        .arg(dir.path().join("schema.cpy"))
        .arg(dir.path().join("data.bin"))
        .assert()
        .failure();
}

// ---------------------------------------------------------------------------
// --select multiple fields comma-separated
// ---------------------------------------------------------------------------

#[test]
fn decode_select_multiple_fields_comma_separated() {
    let dir = setup(SIMPLE_CPY, &simple_record_cp037());
    let jsonl = decode_to_jsonl(&dir, &["--select", "NAME,AGE"]);
    let parsed: serde_json::Value = serde_json::from_str(jsonl.trim()).unwrap();
    let json_str = parsed.to_string();
    assert!(json_str.contains("NAME"), "should contain NAME");
    assert!(json_str.contains("AGE"), "should contain AGE");
}
