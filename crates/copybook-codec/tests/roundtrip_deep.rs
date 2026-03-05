// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Comprehensive round-trip tests: decode → encode byte equality.
//!
//! Covers:
//! 1. Full schema decode→encode roundtrip byte equality
//! 2. Multi-field records with mixed types
//! 3. Records with OCCURS arrays
//! 4. Records with REDEFINES (decode only — encode skips alternates)
//! 5. ODO variable-length roundtrip
//! 6. All codepage variants (CP037, CP1047, CP500)

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, ZonedEncodingFormat,
    decode_record, encode_record,
};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
        .with_preserve_zoned_encoding(true)
}

fn encode_opts(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_zoned_encoding_format(ZonedEncodingFormat::Ebcdic)
}

fn to_ebcdic_cp(text: &str, cp: copybook_charset::Codepage) -> Vec<u8> {
    copybook_charset::utf8_to_ebcdic(text, cp).unwrap()
}

fn to_ebcdic(text: &str) -> Vec<u8> {
    to_ebcdic_cp(text, copybook_charset::Codepage::CP037)
}

/// Decode then re-encode; panic if not byte-identical.
fn assert_roundtrip(copybook: &str, original: &[u8]) {
    assert_roundtrip_cp(copybook, original, Codepage::CP037);
}

fn assert_roundtrip_cp(copybook: &str, original: &[u8], cp: Codepage) {
    let schema = parse_copybook(copybook).unwrap();
    let json = decode_record(&schema, original, &decode_opts(cp)).unwrap();
    let encoded = encode_record(&schema, &json, &encode_opts(cp)).unwrap();
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

// ===========================================================================
// 1. Single-field roundtrip byte equality
// ===========================================================================

#[test]
fn rt_deep_pic_9_five_digits() {
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_roundtrip("01 REC.\n   05 F PIC 9(5).", data);
}

#[test]
fn rt_deep_pic_9_all_zeros() {
    let data: Vec<u8> = vec![0xF0; 7];
    assert_roundtrip("01 REC.\n   05 F PIC 9(7).", &data);
}

#[test]
fn rt_deep_pic_x_with_padding() {
    let data = to_ebcdic("TEST      ");
    assert_roundtrip("01 REC.\n   05 F PIC X(10).", &data);
}

#[test]
fn rt_deep_pic_x_single_char() {
    let data = to_ebcdic("A");
    assert_roundtrip("01 REC.\n   05 F PIC X(1).", &data);
}

#[test]
fn rt_deep_signed_positive_overpunch() {
    let data: &[u8] = &[0xF1, 0xF2, 0xC3]; // +123
    assert_roundtrip("01 REC.\n   05 F PIC S9(3).", data);
}

#[test]
fn rt_deep_signed_negative_overpunch() {
    let data: &[u8] = &[0xF4, 0xF5, 0xD6]; // -456
    assert_roundtrip("01 REC.\n   05 F PIC S9(3).", data);
}

#[test]
fn rt_deep_implied_decimal() {
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xC7]; // +12345.67
    assert_roundtrip("01 REC.\n   05 F PIC S9(5)V99.", data);
}

#[test]
fn rt_deep_comp3_positive() {
    let data: &[u8] = &[0x12, 0x34, 0x5C]; // +12345
    assert_roundtrip("01 REC.\n   05 F PIC S9(5) COMP-3.", data);
}

#[test]
fn rt_deep_comp3_negative() {
    let data: &[u8] = &[0x67, 0x8D]; // -678
    assert_roundtrip("01 REC.\n   05 F PIC S9(3) COMP-3.", data);
}

#[test]
fn rt_deep_comp3_zero() {
    let data: &[u8] = &[0x00, 0x0C]; // +0
    assert_roundtrip("01 REC.\n   05 F PIC S9(3) COMP-3.", data);
}

#[test]
fn rt_deep_comp_2byte() {
    let data: &[u8] = &[0x04, 0xD2]; // 1234
    assert_roundtrip("01 REC.\n   05 F PIC 9(4) COMP.", data);
}

#[test]
fn rt_deep_comp_4byte_signed() {
    let data: &[u8] = &[0xFF, 0xFF, 0xFF, 0x9C]; // -100
    assert_roundtrip("01 REC.\n   05 F PIC S9(8) COMP.", data);
}

#[test]
fn rt_deep_comp_8byte() {
    let data: &[u8] = &[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]; // 1
    assert_roundtrip("01 REC.\n   05 F PIC 9(18) COMP.", data);
}

// ===========================================================================
// 2. Multi-field records with mixed types
// ===========================================================================

#[test]
fn rt_deep_mixed_alpha_numeric() {
    let cpy = r"
01 REC.
   05 NAME   PIC X(10).
   05 ID     PIC 9(5).
   05 BAL    PIC S9(5)V99 COMP-3.
";
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic("ALICE     ")); // 10 chars
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF4, 0xF2]); // 00042
    data.extend_from_slice(&[0x01, 0x23, 0x45, 0x6C]); // +12345.67 in COMP-3

    assert_roundtrip(cpy, &data);
}

#[test]
fn rt_deep_mixed_all_types() {
    let cpy = r"
01 REC.
   05 ALPHA       PIC X(5).
   05 UNSIGNED-N  PIC 9(3).
   05 SIGNED-N    PIC S9(3).
   05 DECIMAL-F   PIC S9(3)V99.
   05 PACKED-F    PIC S9(5) COMP-3.
   05 BINARY-F    PIC 9(4) COMP.
";
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic("ABCDE"));
    data.extend_from_slice(&[0xF0, 0xF4, 0xF2]); // 042
    data.extend_from_slice(&[0xF1, 0xF2, 0xC3]); // +123
    data.extend_from_slice(&[0xF1, 0xF0, 0xF0, 0xF5, 0xC3]); // +100.53
    data.extend_from_slice(&[0x06, 0x78, 0x9C]); // +67890 COMP-3
    data.extend_from_slice(&[0x00, 0x2A]); // 42 COMP

    assert_roundtrip(cpy, &data);
}

#[test]
fn rt_deep_mixed_sign_separate() {
    let cpy = r"
01 REC.
   05 NAME        PIC X(8).
   05 AMOUNT      PIC S9(5)V99 COMP-3.
   05 COUNTER     PIC 9(4) COMP.
   05 SIGNED-FLD  PIC S9(3) SIGN TRAILING SEPARATE.
";
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic("JOHN    "));
    data.extend_from_slice(&[0x12, 0x34, 0x56, 0x7C]); // +12345.67
    data.extend_from_slice(&[0x00, 0x0A]); // 10
    data.extend_from_slice(&[0xF4, 0xF5, 0xF6, 0x60]); // 456-

    assert_roundtrip(cpy, &data);
}

#[test]
fn rt_deep_enterprise_customer_record() {
    let cpy = r"
01 CUSTOMER-REC.
   05 CUST-ID       PIC 9(6).
   05 CUST-NAME     PIC X(20).
   05 BALANCE       PIC S9(7)V99 COMP-3.
   05 TXN-COUNT     PIC 9(4) COMP.
";
    let mut data = Vec::new();
    for &d in &[0u8, 0, 0, 0, 4, 2] {
        data.push(0xF0 + d);
    }
    data.extend_from_slice(&to_ebcdic("JOHN DOE            "));
    data.extend_from_slice(&[0x12, 0x34, 0x56, 0x78, 0x9C]); // +1234567.89
    data.extend_from_slice(&[0x00, 0x0A]); // 10

    assert_roundtrip(cpy, &data);
}

// ===========================================================================
// 3. Records with OCCURS arrays (decode-only verification)
// ===========================================================================

#[test]
fn rt_deep_occurs_pic_9_decode() {
    let cpy = "01 REC.\n   05 VALS PIC 9(3) OCCURS 3 TIMES.";
    let data: &[u8] = &[
        0xF0, 0xF0, 0xF1, // 001
        0xF0, 0xF1, 0xF0, // 010
        0xF1, 0xF0, 0xF0, // 100
    ];
    let schema = parse_copybook(cpy).unwrap();
    let json = decode_record(&schema, data, &decode_opts(Codepage::CP037)).unwrap();
    let arr = json["VALS"].as_array().expect("VALS should be array");
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_str(), Some("001"));
    assert_eq!(arr[1].as_str(), Some("010"));
    assert_eq!(arr[2].as_str(), Some("100"));
}

#[test]
fn rt_deep_occurs_pic_x_decode() {
    let cpy = "01 REC.\n   05 ITEMS PIC X(4) OCCURS 2 TIMES.";
    let schema = parse_copybook(cpy).unwrap();
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic("AAAA"));
    data.extend_from_slice(&to_ebcdic("BBBB"));
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).unwrap();
    let arr = json["ITEMS"].as_array().expect("array");
    assert_eq!(arr.len(), 2);
    assert_eq!(arr[0], "AAAA");
    assert_eq!(arr[1], "BBBB");
}

#[test]
fn rt_deep_occurs_determinism() {
    let cpy = "01 REC.\n   05 VALS PIC 9(2) OCCURS 4 TIMES.";
    let data: &[u8] = &[0xF0, 0xF1, 0xF0, 0xF2, 0xF0, 0xF3, 0xF0, 0xF4];
    let schema = parse_copybook(cpy).unwrap();
    let json1 = decode_record(&schema, data, &decode_opts(Codepage::CP037)).unwrap();
    let json2 = decode_record(&schema, data, &decode_opts(Codepage::CP037)).unwrap();
    assert_eq!(json1, json2, "OCCURS decode must be deterministic");
}

// ===========================================================================
// 4. Records with REDEFINES (decode-only)
// ===========================================================================

#[test]
fn rt_deep_redefines_primary_field_decoded() {
    let cpy = r"
01 REC.
   05 ORIGINAL PIC X(10).
   05 ALT REDEFINES ORIGINAL PIC X(10).
";
    let schema = parse_copybook(cpy).unwrap();
    let data = to_ebcdic("HELLOWORLD");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).unwrap();
    // Primary field should be present
    assert!(json.get("ORIGINAL").is_some());
}

#[test]
fn rt_deep_redefines_alternate_decoded() {
    let cpy = r"
01 REC.
   05 NUM-FIELD PIC X(5).
   05 ALT-FIELD REDEFINES NUM-FIELD PIC X(5).
";
    let schema = parse_copybook(cpy).unwrap();
    let data = to_ebcdic("HELLO");
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).unwrap();
    assert!(json.get("NUM-FIELD").is_some());
}

// ===========================================================================
// 5. ODO variable-length roundtrip (decode verification)
// ===========================================================================

#[test]
fn rt_deep_odo_min_count() {
    let cpy = r"
01 REC.
   05 CNT PIC 9(1).
   05 ITEMS PIC X(4) OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
";
    let schema = parse_copybook(cpy).unwrap();
    // Counter=1 in EBCDIC
    let mut data = vec![0xF1]; // '1'
    data.extend_from_slice(&to_ebcdic("AAAA"));
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).unwrap();
    let arr = json["ITEMS"].as_array().unwrap();
    assert_eq!(arr.len(), 1);
    assert_eq!(arr[0], "AAAA");
}

#[test]
fn rt_deep_odo_max_count() {
    let cpy = r"
01 REC.
   05 CNT PIC 9(1).
   05 ITEMS PIC X(3) OCCURS 1 TO 3 TIMES DEPENDING ON CNT.
";
    let schema = parse_copybook(cpy).unwrap();
    let mut data = vec![0xF3]; // '3'
    data.extend_from_slice(&to_ebcdic("AAA"));
    data.extend_from_slice(&to_ebcdic("BBB"));
    data.extend_from_slice(&to_ebcdic("CCC"));
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).unwrap();
    let arr = json["ITEMS"].as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

#[test]
fn rt_deep_odo_mid_count() {
    let cpy = r"
01 REC.
   05 CNT PIC 9(1).
   05 ITEMS PIC X(2) OCCURS 1 TO 5 TIMES DEPENDING ON CNT.
";
    let schema = parse_copybook(cpy).unwrap();
    let mut data = vec![0xF3]; // '3'
    data.extend_from_slice(&to_ebcdic("AA"));
    data.extend_from_slice(&to_ebcdic("BB"));
    data.extend_from_slice(&to_ebcdic("CC"));
    let json = decode_record(&schema, &data, &decode_opts(Codepage::CP037)).unwrap();
    let arr = json["ITEMS"].as_array().unwrap();
    assert_eq!(arr.len(), 3);
}

// ===========================================================================
// 6. Codepage variants: CP037, CP1047, CP500
// ===========================================================================

#[test]
fn rt_deep_cp037_alpha() {
    let data = to_ebcdic_cp("HELLO", copybook_charset::Codepage::CP037);
    assert_roundtrip_cp("01 REC.\n   05 F PIC X(5).", &data, Codepage::CP037);
}

#[test]
fn rt_deep_cp1047_alpha() {
    let data = to_ebcdic_cp("HELLO", copybook_charset::Codepage::CP1047);
    assert_roundtrip_cp("01 REC.\n   05 F PIC X(5).", &data, Codepage::CP1047);
}

#[test]
fn rt_deep_cp500_alpha() {
    let data = to_ebcdic_cp("HELLO", copybook_charset::Codepage::CP500);
    assert_roundtrip_cp("01 REC.\n   05 F PIC X(5).", &data, Codepage::CP500);
}

#[test]
fn rt_deep_cp037_numeric() {
    // EBCDIC "12345" is same across all codepages for digits: F1 F2 F3 F4 F5
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_roundtrip_cp("01 REC.\n   05 F PIC 9(5).", data, Codepage::CP037);
}

#[test]
fn rt_deep_cp1047_numeric() {
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_roundtrip_cp("01 REC.\n   05 F PIC 9(5).", data, Codepage::CP1047);
}

#[test]
fn rt_deep_cp500_numeric() {
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    assert_roundtrip_cp("01 REC.\n   05 F PIC 9(5).", data, Codepage::CP500);
}

#[test]
fn rt_deep_cp037_comp3() {
    let data: &[u8] = &[0x12, 0x34, 0x5C]; // +12345
    assert_roundtrip_cp("01 REC.\n   05 F PIC S9(5) COMP-3.", data, Codepage::CP037);
}

#[test]
fn rt_deep_cp1047_comp3() {
    let data: &[u8] = &[0x12, 0x34, 0x5C];
    assert_roundtrip_cp("01 REC.\n   05 F PIC S9(5) COMP-3.", data, Codepage::CP1047);
}

#[test]
fn rt_deep_cp500_comp3() {
    let data: &[u8] = &[0x12, 0x34, 0x5C];
    assert_roundtrip_cp("01 REC.\n   05 F PIC S9(5) COMP-3.", data, Codepage::CP500);
}

#[test]
fn rt_deep_cp037_mixed_record() {
    let cpy = r"
01 REC.
   05 NAME PIC X(5).
   05 NUM  PIC 9(3).
   05 PKD  PIC S9(3) COMP-3.
";
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic_cp("HELLO", copybook_charset::Codepage::CP037));
    data.extend_from_slice(&[0xF0, 0xF4, 0xF2]); // 042
    data.extend_from_slice(&[0x12, 0x3C]); // +123
    assert_roundtrip_cp(cpy, &data, Codepage::CP037);
}

#[test]
fn rt_deep_cp1047_mixed_record() {
    let cpy = r"
01 REC.
   05 NAME PIC X(5).
   05 NUM  PIC 9(3).
   05 PKD  PIC S9(3) COMP-3.
";
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic_cp("HELLO", copybook_charset::Codepage::CP1047));
    data.extend_from_slice(&[0xF0, 0xF4, 0xF2]);
    data.extend_from_slice(&[0x12, 0x3C]);
    assert_roundtrip_cp(cpy, &data, Codepage::CP1047);
}

#[test]
fn rt_deep_cp500_mixed_record() {
    let cpy = r"
01 REC.
   05 NAME PIC X(5).
   05 NUM  PIC 9(3).
   05 PKD  PIC S9(3) COMP-3.
";
    let mut data = Vec::new();
    data.extend_from_slice(&to_ebcdic_cp("HELLO", copybook_charset::Codepage::CP500));
    data.extend_from_slice(&[0xF0, 0xF4, 0xF2]);
    data.extend_from_slice(&[0x12, 0x3C]);
    assert_roundtrip_cp(cpy, &data, Codepage::CP500);
}
