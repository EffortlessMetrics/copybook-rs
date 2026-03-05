// SPDX-License-Identifier: AGPL-3.0-or-later
//! Decode determinism proofs: same binary data decoded N times must produce
//! byte-identical JSON output every time.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record};
use copybook_core::parse_copybook;

const ITERATIONS: usize = 5;

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn ebcdic_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn assert_decode_deterministic(copybook: &str, data: &[u8], opts: &DecodeOptions) {
    let schema = parse_copybook(copybook).unwrap();
    let results: Vec<String> = (0..ITERATIONS)
        .map(|_| {
            let json = decode_record(&schema, data, opts).unwrap();
            serde_json::to_string(&json).unwrap()
        })
        .collect();
    for (i, r) in results.iter().enumerate().skip(1) {
        assert_eq!(
            results[0], *r,
            "Decode iteration {i} differs from iteration 0"
        );
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// 1. Simple DISPLAY fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_pic_x_simple() {
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC X(10).",
        b"HELLO     ",
        &decode_opts(),
    );
}

#[test]
fn decode_determinism_pic_x_all_spaces() {
    assert_decode_deterministic("01 REC.\n   05 FLD PIC X(5).", b"     ", &decode_opts());
}

#[test]
fn decode_determinism_pic_9_unsigned() {
    assert_decode_deterministic("01 REC.\n   05 FLD PIC 9(5).", b"12345", &decode_opts());
}

#[test]
fn decode_determinism_pic_9_all_zeros() {
    assert_decode_deterministic("01 REC.\n   05 FLD PIC 9(7).", b"0000000", &decode_opts());
}

#[test]
fn decode_determinism_pic_9v99() {
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC 9(5)V99.",
        b"1234567",
        &decode_opts(),
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// 2. COMP-3 (packed decimal) fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_comp3_positive() {
    // +12345 in COMP-3: 0x12 0x34 0x5C
    let data: &[u8] = &[0x12, 0x34, 0x5C];
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC S9(5) COMP-3.",
        data,
        &ebcdic_decode_opts(),
    );
}

#[test]
fn decode_determinism_comp3_negative() {
    // -98765 in COMP-3: 0x98 0x76 0x5D
    let data: &[u8] = &[0x98, 0x76, 0x5D];
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC S9(5) COMP-3.",
        data,
        &ebcdic_decode_opts(),
    );
}

#[test]
fn decode_determinism_comp3_zero() {
    // +0 in COMP-3 PIC S9(5): 0x00 0x00 0x0C
    let data: &[u8] = &[0x00, 0x00, 0x0C];
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC S9(5) COMP-3.",
        data,
        &ebcdic_decode_opts(),
    );
}

#[test]
fn decode_determinism_comp3_with_scale() {
    // +12345.67 stored as 1234567 in COMP-3: 0x12 0x34 0x56 0x7C
    let data: &[u8] = &[0x12, 0x34, 0x56, 0x7C];
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC S9(5)V99 COMP-3.",
        data,
        &ebcdic_decode_opts(),
    );
}

#[test]
fn decode_determinism_comp3_unsigned() {
    // 12345 unsigned COMP-3: 0x12 0x34 0x5F
    let data: &[u8] = &[0x12, 0x34, 0x5F];
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC 9(5) COMP-3.",
        data,
        &ebcdic_decode_opts(),
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// 3. Multi-field records
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_multi_field_ascii() {
    let copybook = r"
        01 RECORD.
           05 ID     PIC 9(5).
           05 NAME   PIC X(10).
           05 AMOUNT PIC 9(7).
    ";
    assert_decode_deterministic(copybook, b"00042ALICE     0001234", &decode_opts());
}

#[test]
fn decode_determinism_multi_field_mixed_types() {
    let copybook = "\
       01 ORDER.
          05 ORDER-ID   PIC X(8).
          05 ORDER-DATE PIC 9(8).
          05 TOTAL-AMT  PIC S9(7)V99 COMP-3.
          05 STATUS     PIC X(1).";
    // 8 + 8 + 5 + 1 = 22 bytes
    let mut data = Vec::new();
    data.extend_from_slice(b"ORD00001"); // ORDER-ID (ASCII)
    data.extend_from_slice(b"20240115"); // ORDER-DATE
    data.extend_from_slice(&[0x01, 0x23, 0x45, 0x67, 0x0C]); // COMP-3
    data.extend_from_slice(b"A"); // STATUS
    assert_decode_deterministic(copybook, &data, &decode_opts());
}

#[test]
fn decode_determinism_group_with_children() {
    let copybook = "\
       01 REC.
          05 HEADER.
             10 ID   PIC X(5).
             10 DATE PIC 9(8).
          05 BODY.
             10 AMT  PIC 9(7).
             10 DESC PIC X(20).";
    let data = b"ID00120240115000123420 char description";
    assert_decode_deterministic(copybook, data, &decode_opts());
}

#[test]
fn decode_determinism_nested_groups() {
    let copybook = "\
       01 REC.
          05 OUTER.
             10 INNER.
                15 LEAF1 PIC X(4).
                15 LEAF2 PIC 9(3).";
    assert_decode_deterministic(copybook, b"ABCD123", &decode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 4. REDEFINES
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_redefines() {
    let copybook = "\
       01 REC.
          05 DATA-TEXT PIC X(10).
          05 DATA-NUM  REDEFINES DATA-TEXT PIC 9(10).";
    assert_decode_deterministic(copybook, b"0000012345", &decode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 5. OCCURS (fixed array)
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_fixed_occurs() {
    let copybook = "\
       01 REC.
          05 SCORES PIC 9(3) OCCURS 5 TIMES.";
    assert_decode_deterministic(copybook, b"001002003004005", &decode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 6. ODO variable-length
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_odo_min_count() {
    let copybook = "\
       01 REC.
          05 CNT       PIC 9(2).
          05 ENTRIES   OCCURS 1 TO 5
                       DEPENDING ON CNT.
             10 VAL    PIC X(4).";
    // CNT=03 -> 3 entries of 4 bytes each = 2 + 12 = 14 bytes
    assert_decode_deterministic(copybook, b"03AAAABBBBCCCC", &decode_opts());
}

#[test]
fn decode_determinism_odo_single_entry() {
    let copybook = "\
       01 REC.
          05 CNT       PIC 9(2).
          05 ENTRIES   OCCURS 1 TO 10
                       DEPENDING ON CNT.
             10 VAL    PIC X(3).";
    assert_decode_deterministic(copybook, b"01XYZ", &decode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 7. EBCDIC codepage decode determinism
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_ebcdic_zoned_decimal() {
    // EBCDIC zoned decimal "12345" -> 0xF1 0xF2 0xF3 0xF4 0xF5
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_decode_deterministic("01 REC.\n   05 FLD PIC 9(5).", data, &ebcdic_decode_opts());
}

#[test]
fn decode_determinism_ebcdic_signed_zoned() {
    // EBCDIC signed "+12345" -> 0xF1 0xF2 0xF3 0xF4 0xC5
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xC5];
    assert_decode_deterministic("01 REC.\n   05 FLD PIC S9(5).", data, &ebcdic_decode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 8. COMP / BINARY fields
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_comp_binary_16bit() {
    // 16-bit big-endian: 42 = 0x00 0x2A
    let data: &[u8] = &[0x00, 0x2A];
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC 9(4) COMP.",
        data,
        &ebcdic_decode_opts(),
    );
}

#[test]
fn decode_determinism_comp_binary_32bit() {
    // 32-bit big-endian signed: -1 = 0xFF 0xFF 0xFF 0xFF
    let data: &[u8] = &[0xFF, 0xFF, 0xFF, 0xFF];
    assert_decode_deterministic(
        "01 REC.\n   05 FLD PIC S9(9) COMP.",
        data,
        &ebcdic_decode_opts(),
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// 9. JSON string output determinism
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn decode_determinism_json_key_order_stable() {
    let copybook = "\
       01 REC.
          05 Z-FIELD PIC X(3).
          05 A-FIELD PIC 9(2).
          05 M-FIELD PIC X(4).";
    let schema = parse_copybook(copybook).unwrap();
    let data = b"ABC01DEFG";
    let jsons: Vec<String> = (0..ITERATIONS)
        .map(|_| {
            let json = decode_record(&schema, data, &decode_opts()).unwrap();
            serde_json::to_string(&json).unwrap()
        })
        .collect();
    for (i, j) in jsons.iter().enumerate().skip(1) {
        assert_eq!(jsons[0], *j, "JSON key order unstable at iteration {i}");
    }
}
