// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI command-context evidence for numeric representations (issue #571).
//!
//! Zoned/DISPLAY and COMP-3 already have CLI coverage (`zoned_encoding_cli_tests.rs`,
//! `cli_golden_fixtures.rs`). This suite fills the gap for the binary and
//! floating-point families, which had **no** dedicated coverage through the
//! compiled `copybook` binary:
//!
//! * `PIC 9 COMP` (binary) decode,
//! * `COMP-1` / `COMP-2` decode and byte-identical decode→encode round-trip,
//! * the `--float-format` flag (`ieee-be` vs `ibm-hex`) end-to-end.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod common;

use assert_fs::prelude::*;
use common::bin;

fn path_str(p: &std::path::Path) -> String {
    p.to_string_lossy().into_owned()
}

/// Decode `bytes` under `copybook` through `copybook decode` and return the
/// parsed first JSONL record.
fn decode_record_json(copybook: &str, bytes: &[u8], extra_args: &[&str]) -> serde_json::Value {
    let dir = assert_fs::TempDir::new().unwrap();
    let cpy = dir.child("num.cpy");
    cpy.write_str(copybook).unwrap();
    let data = dir.child("num.bin");
    data.write_binary(bytes).unwrap();
    let out = dir.child("out.jsonl");

    let mut args = vec![
        "decode".to_string(),
        path_str(cpy.path()),
        path_str(data.path()),
        "--output".to_string(),
        path_str(out.path()),
        "--format".to_string(),
        "fixed".to_string(),
        "--codepage".to_string(),
        "cp037".to_string(),
    ];
    for a in extra_args {
        args.push((*a).to_string());
    }
    bin().args(&args).assert().success();

    let contents = std::fs::read_to_string(out.path()).unwrap();
    let line = contents.lines().next().expect("at least one JSONL record");
    serde_json::from_str(line).expect("valid JSON line")
}

// ====================================================================
// Binary (COMP) decode through the CLI — previously only incidental.
// ====================================================================

#[test]
fn cli_decode_comp_binary() {
    // PIC 9(4) COMP, big-endian 1234 = 0x04D2.
    let json = decode_record_json("01 REC.\n   05 B PIC 9(4) COMP.\n", &[0x04, 0xD2], &[]);
    assert_eq!(
        json.get("B").and_then(serde_json::Value::as_str),
        Some("1234")
    );
}

// ====================================================================
// COMP-1 / COMP-2 decode through the CLI — previously zero coverage.
// ====================================================================

#[test]
fn cli_decode_comp1_ieee() {
    // COMP-1 IEEE-754 single 1.0 = 0x3F800000.
    let json = decode_record_json(
        "01 REC.\n   05 RATE COMP-1.\n",
        &[0x3F, 0x80, 0x00, 0x00],
        &["--float-format", "ieee-be"],
    );
    let v = json
        .get("RATE")
        .and_then(serde_json::Value::as_f64)
        .unwrap();
    assert!(
        (v - 1.0).abs() < f64::EPSILON,
        "COMP-1 should decode to 1.0, got {v}"
    );
}

#[test]
fn cli_decode_comp2_ieee() {
    // COMP-2 IEEE-754 double 1.0 = 0x3FF0000000000000.
    let json = decode_record_json(
        "01 REC.\n   05 RATE COMP-2.\n",
        &[0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        &["--float-format", "ieee-be"],
    );
    let v = json
        .get("RATE")
        .and_then(serde_json::Value::as_f64)
        .unwrap();
    assert!(
        (v - 1.0).abs() < f64::EPSILON,
        "COMP-2 should decode to 1.0, got {v}"
    );
}

#[test]
fn cli_comp1_ibm_hex_float_format() {
    // The --float-format flag must change interpretation: IBM-hex 1.0 = 0x41100000.
    let json = decode_record_json(
        "01 REC.\n   05 RATE COMP-1.\n",
        &[0x41, 0x10, 0x00, 0x00],
        &["--float-format", "ibm-hex"],
    );
    let v = json
        .get("RATE")
        .and_then(serde_json::Value::as_f64)
        .unwrap();
    assert!(
        (v - 1.0).abs() < f64::EPSILON,
        "IBM-hex COMP-1 should decode to 1.0, got {v}"
    );
}

// ====================================================================
// COMP-1 byte-identical decode -> encode round-trip through the CLI.
// ====================================================================

#[test]
fn cli_comp1_roundtrip_byte_identical() {
    let original: [u8; 4] = [0x3F, 0x80, 0x00, 0x00];
    let copybook = "01 REC.\n   05 RATE COMP-1.\n";

    let dir = assert_fs::TempDir::new().unwrap();
    let cpy = dir.child("f.cpy");
    cpy.write_str(copybook).unwrap();
    let bin_in = dir.child("in.bin");
    bin_in.write_binary(&original).unwrap();
    let jsonl = dir.child("mid.jsonl");
    let bin_out = dir.child("out.bin");

    // decode → JSONL
    bin()
        .args([
            "decode",
            &path_str(cpy.path()),
            &path_str(bin_in.path()),
            "--output",
            &path_str(jsonl.path()),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--float-format",
            "ieee-be",
        ])
        .assert()
        .success();

    // encode JSONL → binary
    bin()
        .args([
            "encode",
            &path_str(cpy.path()),
            &path_str(jsonl.path()),
            "--output",
            &path_str(bin_out.path()),
            "--format",
            "fixed",
            "--codepage",
            "cp037",
            "--float-format",
            "ieee-be",
        ])
        .assert()
        .success();

    let roundtripped = std::fs::read(bin_out.path()).unwrap();
    assert_eq!(
        roundtripped,
        original.to_vec(),
        "COMP-1 decode→encode through the CLI must be byte-identical"
    );
}
