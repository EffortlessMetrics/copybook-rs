// SPDX-License-Identifier: AGPL-3.0-or-later
//! Encode determinism proofs: same JSON encoded N times must produce
//! byte-identical binary output every time, and round-trip fidelity
//! must be preserved.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, ZonedEncodingFormat,
    decode_record, encode_record,
};
use copybook_core::parse_copybook;

const ITERATIONS: usize = 5;

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

fn ebcdic_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
        .with_preserve_zoned_encoding(true)
}

fn ebcdic_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_zoned_encoding_format(ZonedEncodingFormat::Ebcdic)
}

/// Encode JSON N times and verify byte-identical output.
fn assert_encode_deterministic(copybook: &str, json: &serde_json::Value, opts: &EncodeOptions) {
    let schema = parse_copybook(copybook).unwrap();
    let results: Vec<Vec<u8>> = (0..ITERATIONS)
        .map(|_| encode_record(&schema, json, opts).unwrap())
        .collect();
    for (i, r) in results.iter().enumerate().skip(1) {
        assert_eq!(
            results[0],
            *r,
            "Encode iteration {i} differs from iteration 0\n\
             iter0 ({} bytes): {:02X?}\n\
             iter{i} ({} bytes): {r:02X?}",
            results[0].len(),
            results[0],
            r.len(),
        );
    }
}

/// Full round-trip: decode → encode → compare with original binary.
fn assert_roundtrip(copybook: &str, original: &[u8], dec: &DecodeOptions, enc: &EncodeOptions) {
    let schema = parse_copybook(copybook).unwrap();
    let json = decode_record(&schema, original, dec).unwrap();
    let encoded = encode_record(&schema, &json, enc).unwrap();
    assert_eq!(
        encoded,
        original,
        "Round-trip mismatch:\noriginal ({} bytes): {original:02X?}\n\
         encoded  ({} bytes): {encoded:02X?}",
        original.len(),
        encoded.len(),
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// 1. PIC X encode determinism
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn encode_determinism_pic_x_simple() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").unwrap();
    let data = b"HELLO     ";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic("01 REC.\n   05 FLD PIC X(10).", &json, &ascii_encode_opts());
}

#[test]
fn encode_determinism_pic_x_all_spaces() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC X(5).").unwrap();
    let data = b"     ";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic("01 REC.\n   05 FLD PIC X(5).", &json, &ascii_encode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 2. PIC 9 encode determinism
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn encode_determinism_pic_9_unsigned() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(5).").unwrap();
    let data = b"12345";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic("01 REC.\n   05 FLD PIC 9(5).", &json, &ascii_encode_opts());
}

#[test]
fn encode_determinism_pic_9v99() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(5)V99.").unwrap();
    let data = b"1234567";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic(
        "01 REC.\n   05 FLD PIC 9(5)V99.",
        &json,
        &ascii_encode_opts(),
    );
}

#[test]
fn encode_determinism_pic_9_all_zeros() {
    let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(7).").unwrap();
    let data = b"0000000";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic("01 REC.\n   05 FLD PIC 9(7).", &json, &ascii_encode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 3. COMP-3 encode determinism
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn encode_determinism_comp3_positive() {
    let cpy = "01 REC.\n   05 FLD PIC S9(5) COMP-3.";
    // +12345 in COMP-3: 3 bytes (6 nibbles: 1,2,3,4,5,C)
    let data: &[u8] = &[0x12, 0x34, 0x5C];
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ebcdic_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ebcdic_encode_opts());
}

#[test]
fn encode_determinism_comp3_negative() {
    let cpy = "01 REC.\n   05 FLD PIC S9(5) COMP-3.";
    // -98765 in COMP-3: 3 bytes (6 nibbles: 9,8,7,6,5,D)
    let data: &[u8] = &[0x98, 0x76, 0x5D];
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ebcdic_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ebcdic_encode_opts());
}

#[test]
fn encode_determinism_comp3_with_scale() {
    let cpy = "01 REC.\n   05 FLD PIC S9(5)V99 COMP-3.";
    // +12345.67 in COMP-3: 4 bytes (8 nibbles: 1,2,3,4,5,6,7,C)
    let data: &[u8] = &[0x12, 0x34, 0x56, 0x7C];
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ebcdic_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ebcdic_encode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 4. COMP / BINARY encode determinism
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn encode_determinism_comp_16bit() {
    let cpy = "01 REC.\n   05 FLD PIC 9(4) COMP.";
    let data: &[u8] = &[0x00, 0x2A];
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ebcdic_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ebcdic_encode_opts());
}

#[test]
fn encode_determinism_comp_32bit_signed() {
    let cpy = "01 REC.\n   05 FLD PIC S9(9) COMP.";
    let data: &[u8] = &[0x00, 0x00, 0x01, 0x00];
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ebcdic_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ebcdic_encode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 5. Multi-field encode determinism
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn encode_determinism_multi_field() {
    let cpy = "\
       01 RECORD.
          05 ID     PIC 9(5).
          05 NAME   PIC X(10).
          05 AMOUNT PIC 9(7).";
    let data = b"00042ALICE     0001234";
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ascii_encode_opts());
}

#[test]
fn encode_determinism_group_fields() {
    let cpy = "\
       01 REC.
          05 HDR.
             10 ID   PIC X(5).
             10 DATE PIC 9(8).
          05 AMT PIC 9(7).";
    let data = b"ID001202401150001234";
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ascii_encode_opts());
}

// ═══════════════════════════════════════════════════════════════════════════
// 6. Round-trip: decode → encode → compare original
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn roundtrip_pic_x() {
    assert_roundtrip(
        "01 REC.\n   05 FLD PIC X(10).",
        b"HELLO     ",
        &ascii_decode_opts(),
        &ascii_encode_opts(),
    );
}

#[test]
fn roundtrip_pic_9() {
    assert_roundtrip(
        "01 REC.\n   05 FLD PIC 9(5).",
        b"12345",
        &ascii_decode_opts(),
        &ascii_encode_opts(),
    );
}

#[test]
fn roundtrip_comp3_ebcdic() {
    // +12345 in COMP-3: 3 bytes (6 nibbles: 1,2,3,4,5,C)
    assert_roundtrip(
        "01 REC.\n   05 FLD PIC S9(5) COMP-3.",
        &[0x12, 0x34, 0x5C],
        &ebcdic_decode_opts(),
        &ebcdic_encode_opts(),
    );
}

#[test]
fn roundtrip_multi_field() {
    assert_roundtrip(
        "\
       01 REC.
          05 ID   PIC 9(5).
          05 NAME PIC X(10).
          05 AMT  PIC 9(7).",
        b"00042ALICE     0001234",
        &ascii_decode_opts(),
        &ascii_encode_opts(),
    );
}

#[test]
fn roundtrip_comp_binary() {
    assert_roundtrip(
        "01 REC.\n   05 FLD PIC 9(4) COMP.",
        &[0x00, 0x2A],
        &ebcdic_decode_opts(),
        &ebcdic_encode_opts(),
    );
}

// ═══════════════════════════════════════════════════════════════════════════
// 7. Encode byte identity across iterations
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn encode_determinism_redefines() {
    let cpy = "\
       01 REC.
          05 DATA-TEXT PIC X(10).
          05 DATA-NUM  REDEFINES DATA-TEXT PIC 9(10).";
    // Construct unambiguous JSON: only the primary view is non-null.
    // Encoding with both views would correctly produce CBKE501.
    let json: serde_json::Value = serde_json::json!({
        "DATA-TEXT": "0000012345",
        "DATA-NUM": null
    });
    assert_encode_deterministic(cpy, &json, &ascii_encode_opts());
}

#[test]
fn encode_determinism_fixed_occurs() {
    let cpy = "\
       01 REC.
          05 ITEMS PIC 9(3) OCCURS 5 TIMES.";
    let data = b"001002003004005";
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_encode_deterministic(cpy, &json, &ascii_encode_opts());
}
