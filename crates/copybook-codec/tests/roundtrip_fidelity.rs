// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Comprehensive codec round-trip fidelity tests.
//!
//! Each test proves byte-identical round-trip:
//!   decode(binary) → JSON → encode(JSON) → binary'
//!   where binary == binary'.
//!
//! All tests use CP037 codepage unless otherwise noted.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, ZonedEncodingFormat,
};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
        .with_preserve_zoned_encoding(true)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_zoned_encoding_format(ZonedEncodingFormat::Ebcdic)
}

fn to_ebcdic(text: &str) -> Vec<u8> {
    copybook_charset::utf8_to_ebcdic(text, copybook_charset::Codepage::CP037).unwrap()
}

/// Decode then re-encode; panic if not byte-identical.
fn assert_roundtrip(copybook: &str, original: &[u8]) {
    let schema = parse_copybook(copybook).unwrap();
    let json = copybook_codec::decode_record(&schema, original, &decode_opts()).unwrap();
    let encoded = copybook_codec::encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(
        encoded,
        original,
        "round-trip mismatch for copybook:\n{copybook}\n\
         original ({} bytes): {original:02X?}\n\
         encoded  ({} bytes): {encoded:02X?}",
        original.len(),
        encoded.len(),
    );
}

// =========================================================================
// 1. PIC X(n) — alphanumeric, space-padded
// =========================================================================

#[test]
fn rt_pic_x_simple() {
    // "HELLO     " (10 chars, space-padded) in EBCDIC CP037
    let data = to_ebcdic("HELLO     ");
    assert_roundtrip("01 REC.\n   05 FLD PIC X(10).", &data);
}

#[test]
fn rt_pic_x_all_spaces() {
    // All-space field
    let data = to_ebcdic("     ");
    assert_roundtrip("01 REC.\n   05 FLD PIC X(5).", &data);
}

#[test]
fn rt_pic_x_special_chars() {
    // Mixed alphanumeric with digits and hyphens
    let data = to_ebcdic("A-1 B");
    assert_roundtrip("01 REC.\n   05 FLD PIC X(5).", &data);
}

// =========================================================================
// 2. PIC 9(n) — unsigned display numeric (zoned decimal)
// =========================================================================

#[test]
fn rt_pic_9_simple() {
    // EBCDIC "12345" → 0xF1 F2 F3 F4 F5
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(5).", data);
}

#[test]
fn rt_pic_9_all_zeros() {
    let data: Vec<u8> = vec![0xF0; 7];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(7).", &data);
}

#[test]
fn rt_pic_9_max_value() {
    // "99999" → 0xF9 repeated
    let data: Vec<u8> = vec![0xF9; 5];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(5).", &data);
}

// =========================================================================
// 3. PIC S9(n) — signed display (overpunch)
// =========================================================================

#[test]
fn rt_signed_display_positive() {
    // +123 → F1 F2 C3 (positive overpunch on last nibble)
    let data: &[u8] = &[0xF1, 0xF2, 0xC3];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3).", data);
}

#[test]
fn rt_signed_display_negative() {
    // -456 → F4 F5 D6 (negative overpunch on last nibble)
    let data: &[u8] = &[0xF4, 0xF5, 0xD6];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3).", data);
}

#[test]
fn rt_signed_display_positive_large() {
    // +98765 → F9 F8 F7 F6 C5
    let data: &[u8] = &[0xF9, 0xF8, 0xF7, 0xF6, 0xC5];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(5).", data);
}

// =========================================================================
// 4. PIC S9(n)V9(n) — signed decimal with implied point
// =========================================================================

#[test]
fn rt_implied_decimal_positive() {
    // +12345.67 → F1 F2 F3 F4 F5 F6 C7
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xC7];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(5)V99.", data);
}

#[test]
fn rt_implied_decimal_negative() {
    // -100.53 → F1 F0 F0 F5 D3
    let data: &[u8] = &[0xF1, 0xF0, 0xF0, 0xF5, 0xD3];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3)V99.", data);
}

#[test]
fn rt_implied_decimal_small() {
    // +0.01 → F0 F0 C1
    let data: &[u8] = &[0xF0, 0xF0, 0xC1];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(1)V99.", data);
}

// =========================================================================
// 5. PIC S9(n) COMP-3 — packed decimal
// =========================================================================

#[test]
fn rt_comp3_positive() {
    // +12345 → 0x12 0x34 0x5C
    let data: &[u8] = &[0x12, 0x34, 0x5C];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(5) COMP-3.", data);
}

#[test]
fn rt_comp3_negative() {
    // -678 → 0x67 0x8D
    let data: &[u8] = &[0x67, 0x8D];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3) COMP-3.", data);
}

#[test]
fn rt_comp3_zero() {
    // +0 → 0x00 0x0C
    let data: &[u8] = &[0x00, 0x0C];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3) COMP-3.", data);
}

#[test]
fn rt_comp3_with_decimal() {
    // PIC S9(5)V99 COMP-3 → 7 digits + sign = 8 nibbles = 4 bytes
    // +12345.67 → 0x12 0x34 0x56 0x7C
    let data: &[u8] = &[0x12, 0x34, 0x56, 0x7C];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(5)V99 COMP-3.", data);
}

#[test]
fn rt_comp3_unsigned() {
    // PIC 9(5) COMP-3 → 0x12 0x34 0x5F (unsigned positive = 0xF nibble)
    let data: &[u8] = &[0x12, 0x34, 0x5F];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(5) COMP-3.", data);
}

#[test]
fn rt_comp3_max_digits() {
    // PIC 9(7) COMP-3 → 4 bytes; 9999999 → 0x99 0x99 0x99 0x9F
    let data: &[u8] = &[0x99, 0x99, 0x99, 0x9F];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(7) COMP-3.", data);
}

// =========================================================================
// 6. PIC S9(n) COMP — binary integer (big-endian)
// =========================================================================

#[test]
fn rt_comp_2byte_unsigned() {
    // PIC 9(4) COMP → 2-byte; value 1234 = 0x04D2
    let data: &[u8] = &[0x04, 0xD2];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(4) COMP.", data);
}

#[test]
fn rt_comp_2byte_signed_negative() {
    // PIC S9(4) COMP → 2-byte signed; value -1 = 0xFFFF
    let data: &[u8] = &[0xFF, 0xFF];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(4) COMP.", data);
}

#[test]
fn rt_comp_4byte_positive() {
    // PIC S9(8) COMP → 4-byte signed; value +100 = 0x00000064
    let data: &[u8] = &[0x00, 0x00, 0x00, 0x64];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(8) COMP.", data);
}

#[test]
fn rt_comp_4byte_zero() {
    let data: &[u8] = &[0x00, 0x00, 0x00, 0x00];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(9) COMP.", data);
}

#[test]
fn rt_comp_8byte() {
    // PIC 9(18) COMP → 8-byte; value 1
    let data: &[u8] = &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(18) COMP.", data);
}

// =========================================================================
// 7. PIC S9(n) SIGN SEPARATE LEADING / TRAILING
// =========================================================================

#[test]
fn rt_sign_separate_leading_positive() {
    // SIGN LEADING SEPARATE: '+' (0x4E in EBCDIC CP037) then digits
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3]; // +123
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3) SIGN LEADING SEPARATE.", data);
}

#[test]
fn rt_sign_separate_leading_negative() {
    // '-' (0x60 in EBCDIC CP037) then digits
    let data: &[u8] = &[0x60, 0xF7, 0xF8, 0xF9]; // -789
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3) SIGN LEADING SEPARATE.", data);
}

#[test]
fn rt_sign_separate_trailing_positive() {
    // Digits then '+' (0x4E)
    let data: &[u8] = &[0xF1, 0xF0, 0xF0, 0x4E]; // 100+
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3) SIGN TRAILING SEPARATE.", data);
}

#[test]
fn rt_sign_separate_trailing_negative() {
    // Digits then '-' (0x60)
    let data: &[u8] = &[0xF4, 0xF5, 0xF6, 0x60]; // 456-
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(3) SIGN TRAILING SEPARATE.", data);
}

// =========================================================================
// 8. Mixed record — all types in one record
// =========================================================================

#[test]
fn rt_mixed_record_all_types() {
    let cpy = r"
01 REC.
   05 ALPHA       PIC X(5).
   05 UNSIGNED-NUM PIC 9(3).
   05 SIGNED-NUM  PIC S9(3).
   05 DECIMAL-FLD PIC S9(3)V99.
   05 PACKED-FLD  PIC S9(5) COMP-3.
   05 BINARY-FLD  PIC 9(4) COMP.
";
    let mut data = Vec::new();

    // ALPHA: "ABCDE" in EBCDIC
    data.extend_from_slice(&to_ebcdic("ABCDE"));
    // UNSIGNED-NUM: "042" → F0 F4 F2
    data.extend_from_slice(&[0xF0, 0xF4, 0xF2]);
    // SIGNED-NUM: +123 → F1 F2 C3
    data.extend_from_slice(&[0xF1, 0xF2, 0xC3]);
    // DECIMAL-FLD: +100.53 → F1 F0 F0 F5 C3
    data.extend_from_slice(&[0xF1, 0xF0, 0xF0, 0xF5, 0xC3]);
    // PACKED-FLD: +67891 → 0x06 0x78 0x9C  (S9(5) COMP-3 = 3 bytes)
    data.extend_from_slice(&[0x06, 0x78, 0x9C]);
    // BINARY-FLD: 42 = 0x002A (PIC 9(4) COMP = 2 bytes)
    data.extend_from_slice(&[0x00, 0x2A]);

    assert_roundtrip(cpy, &data);
}

#[test]
fn rt_mixed_record_with_sign_separate() {
    let cpy = r"
01 REC.
   05 NAME        PIC X(8).
   05 AMOUNT      PIC S9(5)V99 COMP-3.
   05 COUNTER     PIC 9(4) COMP.
   05 SIGNED-FLD  PIC S9(3) SIGN TRAILING SEPARATE.
";
    let mut data = Vec::new();

    // NAME: "JOHN    " in EBCDIC (space-padded to 8)
    data.extend_from_slice(&to_ebcdic("JOHN    "));
    // AMOUNT: S9(5)V99 COMP-3 → 7 digits + sign = 4 bytes; +12345.67 → 0x12 0x34 0x56 0x7C
    data.extend_from_slice(&[0x12, 0x34, 0x56, 0x7C]);
    // COUNTER: 9(4) COMP → 2 bytes; 10 = 0x000A
    data.extend_from_slice(&[0x00, 0x0A]);
    // SIGNED-FLD: S9(3) SIGN TRAILING SEPARATE → 4 bytes; 456- → F4 F5 F6 60
    data.extend_from_slice(&[0xF4, 0xF5, 0xF6, 0x60]);

    assert_roundtrip(cpy, &data);
}

// =========================================================================
// 9. OCCURS arrays — decode verification
//
// Note: encode_record and encode_jsonl_to_file currently do not
// round-trip OCCURS groups to byte-identical output. These tests
// verify the decode path produces the correct array structure.
// =========================================================================

#[test]
fn rt_occurs_decode_structure() {
    let cpy = "01 REC.\n   05 VALS PIC 9(3) OCCURS 3 TIMES.";
    let data: &[u8] = &[
        0xF0, 0xF0, 0xF1, // "001"
        0xF0, 0xF1, 0xF0, // "010"
        0xF1, 0xF0, 0xF0, // "100"
    ];

    let schema = parse_copybook(cpy).unwrap();
    let json = copybook_codec::decode_record(&schema, data, &decode_opts()).unwrap();

    let arr = json["VALS"].as_array().expect("VALS should be an array");
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_str(), Some("001"));
    assert_eq!(arr[1].as_str(), Some("010"));
    assert_eq!(arr[2].as_str(), Some("100"));

    // Decode same data again to verify determinism
    let json2 = copybook_codec::decode_record(&schema, data, &decode_opts()).unwrap();
    assert_eq!(json, json2, "OCCURS decode must be deterministic");
}

// =========================================================================
// 10. ODO — decode verification
// =========================================================================

#[test]
fn rt_odo_decode_structure() {
    let cpy = r"
01 REC.
   05 CNT PIC 9(1).
   05 ITEMS PIC X(4) OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
";
    let mut data = Vec::new();
    data.push(0xF2); // EBCDIC '2'
    data.extend_from_slice(&to_ebcdic("AAAA"));
    data.extend_from_slice(&to_ebcdic("BBBB"));

    let schema = parse_copybook(cpy).unwrap();
    let json = copybook_codec::decode_record(&schema, &data, &decode_opts()).unwrap();

    let arr = json["ITEMS"].as_array().expect("ITEMS should be an array");
    assert_eq!(arr.len(), 2);
    assert_eq!(arr[0].as_str(), Some("AAAA"));
    assert_eq!(arr[1].as_str(), Some("BBBB"));
}

// =========================================================================
// Additional round-trip tests — extra coverage for supported types
// =========================================================================

#[test]
fn rt_comp3_large_positive() {
    // PIC S9(9) COMP-3 → 5 bytes; +123456789 → 0x12 0x34 0x56 0x78 0x9C
    let data: &[u8] = &[0x12, 0x34, 0x56, 0x78, 0x9C];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(9) COMP-3.", data);
}

#[test]
fn rt_comp3_large_negative() {
    // PIC S9(9) COMP-3 → 5 bytes; -987654321 → 0x98 0x76 0x54 0x32 0x1D
    let data: &[u8] = &[0x98, 0x76, 0x54, 0x32, 0x1D];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(9) COMP-3.", data);
}

#[test]
fn rt_multiple_alpha_fields() {
    let cpy = r"
01 REC.
   05 FIRST  PIC X(10).
   05 MIDDLE PIC X(1).
   05 LAST   PIC X(15).
";
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic("JOHN      ")); // 10 chars
    data.extend_from_slice(&to_ebcdic("Q")); // 1 char
    data.extend_from_slice(&to_ebcdic("PUBLIC         ")); // 15 chars
    assert_roundtrip(cpy, &data);
}

#[test]
fn rt_mixed_comp_types() {
    let cpy = r"
01 REC.
   05 BINARY2  PIC 9(4) COMP.
   05 BINARY4  PIC S9(8) COMP.
   05 PACKED   PIC S9(5) COMP-3.
";
    let mut data = Vec::new();
    data.extend_from_slice(&[0x03, 0xE8]); // 1000 in 2-byte COMP
    data.extend_from_slice(&[0xFF, 0xFF, 0xFF, 0x9C]); // -100 in 4-byte signed COMP
    data.extend_from_slice(&[0x04, 0x20, 0x0C]); // +42000 in COMP-3
    assert_roundtrip(cpy, &data);
}

#[test]
fn rt_all_sign_types_in_record() {
    let cpy = r"
01 REC.
   05 OVERPUNCH-POS PIC S9(3).
   05 OVERPUNCH-NEG PIC S9(3).
   05 SEP-LEAD      PIC S9(3) SIGN LEADING SEPARATE.
   05 SEP-TRAIL     PIC S9(3) SIGN TRAILING SEPARATE.
";
    let data: &[u8] = &[
        0xF1, 0xF2, 0xC3, // +123 overpunch
        0xF4, 0xF5, 0xD6, // -456 overpunch
        0x4E, 0xF7, 0xF8, 0xF9, // +789 sign leading separate
        0xF1, 0xF1, 0xF1, 0x60, // -111 sign trailing separate
    ];
    assert_roundtrip(cpy, data);
}

#[test]
fn rt_long_alphanumeric_with_padding() {
    // 50-character field with trailing spaces
    let mut text = String::from("ENTERPRISE DATA INTEGRATION");
    while text.len() < 50 {
        text.push(' ');
    }
    let data = to_ebcdic(&text);
    assert_roundtrip("01 REC.\n   05 FLD PIC X(50).", &data);
}

// =========================================================================
// 11. Additional edge cases
// =========================================================================

#[test]
fn rt_comp3_negative_with_decimal() {
    // PIC S9(5)V99 COMP-3 → -12345.67 → 0x12 0x34 0x56 0x7D
    let data: &[u8] = &[0x12, 0x34, 0x56, 0x7D];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(5)V99 COMP-3.", data);
}

#[test]
fn rt_single_byte_alpha() {
    let data = to_ebcdic("X");
    assert_roundtrip("01 REC.\n   05 FLD PIC X(1).", &data);
}

#[test]
fn rt_unsigned_decimal_no_sign() {
    // PIC 9(3)V99 → 5 zoned bytes, "12345" means 123.45
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_roundtrip("01 REC.\n   05 FLD PIC 9(3)V99.", data);
}

#[test]
fn rt_enterprise_customer_record() {
    let cpy = r"
01 CUSTOMER-REC.
   05 CUST-ID       PIC 9(6).
   05 CUST-NAME     PIC X(20).
   05 BALANCE       PIC S9(7)V99 COMP-3.
   05 TXN-COUNT     PIC 9(4) COMP.
";
    let mut data = Vec::new();

    // CUST-ID: "000042" in EBCDIC zoned
    for &d in &[0u8, 0, 0, 0, 4, 2] {
        data.push(0xF0 + d);
    }
    // CUST-NAME: "JOHN DOE" padded to 20 in EBCDIC
    data.extend_from_slice(&to_ebcdic("JOHN DOE            "));
    // BALANCE: S9(7)V99 COMP-3 → 9 digits + sign = 5 bytes; +1234567.89 → 0x12 0x34 0x56 0x78 0x9C
    data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x9C]);
    // TXN-COUNT: 9(4) COMP → 2 bytes; 10 = 0x000A
    data.extend_from_slice(&[0x00, 0x0A]);

    assert_roundtrip(cpy, &data);
}

#[test]
fn rt_comp_signed_4byte_negative() {
    // PIC S9(8) COMP → -1 = 0xFFFFFFFF
    let data: &[u8] = &[0xFF, 0xFF, 0xFF, 0xFF];
    assert_roundtrip("01 REC.\n   05 FLD PIC S9(8) COMP.", data);
}
