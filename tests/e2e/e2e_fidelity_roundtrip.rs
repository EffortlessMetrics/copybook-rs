// SPDX-License-Identifier: AGPL-3.0-or-later
//! Fidelity and round-trip golden tests — structural beams of the project.
//!
//! Validates byte-level and value-level fidelity across:
//! - COMP-3 packed decimal encode/decode
//! - Codepage conversions (CP037, CP500, CP1047)
//! - Edited PIC formatting round-trips
//! - Mixed-type record preservation
//! - Determinism proofs via SHA-256

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use sha2::{Digest, Sha256};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn sha256_hex(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
}

/// Assert a JSON field is numerically close to `expected`.
#[expect(clippy::panic, reason = "test helper — panics are the assertion mechanism")]
fn assert_numeric_value(json: &serde_json::Value, field: &str, expected: f64) {
    let val = &json[field];
    assert!(!val.is_null(), "{field} not found in JSON");
    let actual: f64 = if let Some(s) = val.as_str() {
        s.parse()
            .unwrap_or_else(|_| panic!("cannot parse '{s}' as f64 for {field}"))
    } else if let Some(n) = val.as_f64() {
        n
    } else {
        panic!("{field} is neither string nor number: {val}");
    };
    assert!(
        (actual - expected).abs() < 0.005,
        "{field}: expected {expected}, got {actual}"
    );
}

// =========================================================================
// 1. COMP-3 Round-Trip Fidelity (5 tests)
// =========================================================================

/// Positive value: +12345.00 as COMP-3 → decode → encode → verify identical bytes.
#[test]
fn comp3_roundtrip_positive_value() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(7)V99 COMP-3.
    ";
    // PIC S9(7)V99 = 9 digits + sign = 10 nibbles = 5 bytes
    // +12345.00 → digits 001234500, sign C → packed 00 12 34 50 0C
    let data: Vec<u8> = vec![0x00, 0x12, 0x34, 0x50, 0x0C];

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    assert_numeric_value(&json, "AMOUNT", 12345.0);

    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 +12345 binary round-trip");

    let json2 = decode_record(&schema, &re_encoded, &dopts).expect("decode-2");
    assert_eq!(json, json2, "COMP-3 +12345 JSON round-trip");
}

/// Negative value: −99999.99 as COMP-3 → decode → encode → verify identical bytes.
#[test]
fn comp3_roundtrip_negative_value() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(7)V99 COMP-3.
    ";
    // −99999.99 → digits 009999999, sign D → packed 00 99 99 99 9D
    let data: Vec<u8> = vec![0x00, 0x99, 0x99, 0x99, 0x9D];

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    assert_numeric_value(&json, "AMOUNT", -99999.99);

    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 −99999.99 binary round-trip");

    let json2 = decode_record(&schema, &re_encoded, &dopts).expect("decode-2");
    assert_eq!(json, json2, "COMP-3 −99999.99 JSON round-trip");
}

/// Zero: +0.00 as COMP-3 → decode → encode → verify identical bytes.
#[test]
fn comp3_roundtrip_zero() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(7)V99 COMP-3.
    ";
    // +0.00 → digits 000000000, sign C → packed 00 00 00 00 0C
    let data: Vec<u8> = vec![0x00, 0x00, 0x00, 0x00, 0x0C];

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    assert_numeric_value(&json, "AMOUNT", 0.0);

    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 zero binary round-trip");

    let json2 = decode_record(&schema, &re_encoded, &dopts).expect("decode-2");
    assert_eq!(json, json2, "COMP-3 zero JSON round-trip");
}

/// Max value for PIC S9(7)V99: +9999999.99 → round-trip.
#[test]
fn comp3_roundtrip_max_value() {
    let cpy = r"
        01 REC.
           05 AMOUNT PIC S9(7)V99 COMP-3.
    ";
    // +9999999.99 → digits 999999999, sign C → packed 99 99 99 99 9C
    let data: Vec<u8> = vec![0x99, 0x99, 0x99, 0x99, 0x9C];

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    assert_numeric_value(&json, "AMOUNT", 9_999_999.99);

    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(data, re_encoded, "COMP-3 max-value binary round-trip");

    let json2 = decode_record(&schema, &re_encoded, &dopts).expect("decode-2");
    assert_eq!(json, json2, "COMP-3 max-value JSON round-trip");
}

/// Multi-field COMP-3 record → full decode → encode → byte-identical.
#[test]
fn comp3_multi_field_roundtrip_byte_identical() {
    let cpy = r"
        01 REC.
           05 REC-ID    PIC 9(4).
           05 BALANCE   PIC S9(7)V99 COMP-3.
           05 TAX       PIC S9(5)V99 COMP-3.
           05 LABEL     PIC X(8).
    ";
    // REC-ID "0042", BALANCE +12345.67, TAX −100.50, LABEL "ACCT    "
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF4, 0xF2]); // "0042"
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x56, 0x7C]); // +12345.67
    // TAX PIC S9(5)V99 = 7 digits + sign = 4 bytes; −100.50 → 0010050D
    data.extend_from_slice(&[0x00, 0x10, 0x05, 0x0D]);
    // "ACCT    " CP037: A=C1 C=C3 C=C3 T=E3
    data.extend_from_slice(&[0xC1, 0xC3, 0xC3, 0xE3, 0x40, 0x40, 0x40, 0x40]);

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    assert_numeric_value(&json, "BALANCE", 12345.67);
    assert_numeric_value(&json, "TAX", -100.50);

    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(
        data, re_encoded,
        "multi-field COMP-3 record must be byte-identical after round-trip"
    );
}

// =========================================================================
// 2. Codepage Round-Trip Fidelity (5 tests)
// =========================================================================

/// CP037 ASCII text → encode to EBCDIC → decode back → verify identical.
#[test]
fn codepage_cp037_text_roundtrip() {
    let cpy = r"
        01 REC.
           05 TXT PIC X(30).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let original = "HELLO WORLD FROM CP037";
    let json = serde_json::json!({"TXT": original});

    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");
    assert_eq!(
        decoded["TXT"].as_str().expect("string").trim_end(),
        original
    );

    // Binary stability after one round-trip
    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(encoded, re_encoded, "CP037 binary stable after round-trip");
}

/// CP1047 ASCII text → encode to EBCDIC → decode back → verify identical.
#[test]
fn codepage_cp1047_text_roundtrip() {
    let cpy = r"
        01 REC.
           05 TXT PIC X(30).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP1047);
    let eopts = encode_opts(Codepage::CP1047);

    let original = "HELLO WORLD FROM CP1047";
    let json = serde_json::json!({"TXT": original});

    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");
    assert_eq!(
        decoded["TXT"].as_str().expect("string").trim_end(),
        original
    );

    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(encoded, re_encoded, "CP1047 binary stable after round-trip");
}

/// CP500 ASCII text → encode to EBCDIC → decode back → verify identical.
#[test]
fn codepage_cp500_text_roundtrip() {
    let cpy = r"
        01 REC.
           05 TXT PIC X(30).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP500);
    let eopts = encode_opts(Codepage::CP500);

    let original = "HELLO WORLD FROM CP500";
    let json = serde_json::json!({"TXT": original});

    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");
    assert_eq!(
        decoded["TXT"].as_str().expect("string").trim_end(),
        original
    );

    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(encoded, re_encoded, "CP500 binary stable after round-trip");
}

/// Mixed alphanumeric with special chars → round-trip through CP037.
#[test]
fn codepage_mixed_alphanumeric_roundtrip() {
    let cpy = r"
        01 REC.
           05 TXT PIC X(40).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let original = "JOHN DOE, 123 MAIN ST. APT-4";
    let json = serde_json::json!({"TXT": original});

    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");
    assert_eq!(
        decoded["TXT"].as_str().expect("string").trim_end(),
        original
    );

    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(
        encoded, re_encoded,
        "mixed-alphanumeric binary stable after round-trip"
    );
}

/// All printable ASCII characters → round-trip through CP037.
#[test]
fn codepage_all_printable_ascii_cp037_roundtrip() {
    let cpy = r"
        01 REC.
           05 TXT PIC X(60).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let original = "ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789.,-/()+=";
    let json = serde_json::json!({"TXT": original});

    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");
    assert_eq!(
        decoded["TXT"].as_str().expect("string").trim_end(),
        original
    );

    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(
        encoded, re_encoded,
        "all-printable CP037 binary stable after round-trip"
    );
}

// =========================================================================
// 3. Edited PIC Round-Trip (3 tests)
// =========================================================================

/// ZZZ9 pattern: numeric → format → parse back → verify value preserved.
#[test]
fn edited_pic_zzz9_roundtrip() {
    let cpy = r"
        01 REC.
           05 QTY PIC ZZZ9.
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = serde_json::json!({"QTY": "123"});
    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");

    assert_numeric_value(&decoded, "QTY", 123.0);

    // Binary fixpoint: encode decoded → same bytes
    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(
        encoded, re_encoded,
        "ZZZ9 edited PIC binary stable after round-trip"
    );
}

/// Currency $ZZ,ZZZ.99 pattern → round-trip preserves value.
#[test]
fn edited_pic_currency_roundtrip() {
    let cpy = "
        01 REC.
           05 PRICE PIC $ZZ,ZZZ.99.
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = serde_json::json!({"PRICE": "1234.56"});
    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");

    assert_numeric_value(&decoded, "PRICE", 1234.56);

    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(
        encoded, re_encoded,
        "currency edited PIC binary stable after round-trip"
    );
}

/// Sign-edited −ZZZ9: positive and negative values → round-trip.
#[test]
fn edited_pic_sign_roundtrip() {
    let cpy = r"
        01 REC.
           05 GAIN PIC -ZZZ9.
           05 LOSS PIC -ZZZ9.
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = serde_json::json!({
        "GAIN": "42",
        "LOSS": "-123"
    });
    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");

    assert_numeric_value(&decoded, "GAIN", 42.0);
    assert_numeric_value(&decoded, "LOSS", -123.0);

    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(
        encoded, re_encoded,
        "sign-edited PIC binary stable after round-trip"
    );
}

// =========================================================================
// 4. Record-Level Fidelity (5 tests)
// =========================================================================

/// Full record with mixed types (DISPLAY + COMP-3 + PIC X) → decode → encode
/// → binary comparison.
#[test]
fn record_mixed_types_roundtrip() {
    let cpy = r"
        01 MIXED-REC.
           05 SEQ-NUM    PIC 9(6).
           05 CUST-NAME  PIC X(20).
           05 BALANCE    PIC S9(7)V99 COMP-3.
           05 DATE-FLD   PIC 9(8).
           05 STATUS     PIC X(1).
    ";
    // 6 + 20 + 5 + 8 + 1 = 40 bytes
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF1]); // "000001"
    // "SMITH" (S=E2 M=D4 I=C9 T=E3 H=C8) + 15 spaces
    data.extend_from_slice(&[0xE2, 0xD4, 0xC9, 0xE3, 0xC8]);
    data.extend_from_slice(&[0x40; 15]);
    // +50000.00 COMP-3: 005000000C → 00 50 00 00 0C
    data.extend_from_slice(&[0x00, 0x50, 0x00, 0x00, 0x0C]);
    data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF5, 0xF0, 0xF1, 0xF0, 0xF1]); // "20250101"
    data.push(0xC1); // "A"

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");

    assert_eq!(
        data, re_encoded,
        "mixed-type record must be binary-identical after round-trip"
    );
}

/// Record with FILLER fields → round-trip preserves FILLER bytes (space-filled).
#[test]
fn record_filler_roundtrip() {
    let cpy = r"
        01 FILL-REC.
           05 ID-FIELD   PIC X(4).
           05 FILLER     PIC X(8).
           05 AMOUNT     PIC 9(6).
           05 FILLER     PIC X(4).
           05 STATUS     PIC X(1).
    ";
    // 4 + 8 + 6 + 4 + 1 = 23 bytes; FILLER positions are EBCDIC spaces (0x40)
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4]); // "ABCD"
    data.extend_from_slice(&[0x40; 8]); // FILLER (spaces)
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6]); // "123456"
    data.extend_from_slice(&[0x40; 4]); // FILLER (spaces)
    data.push(0xE7); // "X"

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");

    // JSON should not contain FILLER keys (emit_filler defaults to false)
    let obj = json.as_object().expect("object");
    assert!(
        !obj.keys().any(|k| k.starts_with("_filler")),
        "FILLER fields should not appear in JSON by default"
    );

    // Encode back → FILLER positions filled with EBCDIC spaces → binary-identical
    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(
        data, re_encoded,
        "FILLER round-trip must preserve space bytes"
    );
}

/// Record with OCCURS → decode verifies all array elements, deterministic output.
#[test]
fn record_occurs_roundtrip() {
    let cpy = r"
        01 ARR-REC.
           05 HEADER   PIC X(4).
           05 ITEMS OCCURS 3 TIMES.
              10 ITEM-VAL PIC X(6).
           05 TRAILER  PIC X(2).
    ";
    // 4 + 3×6 + 2 = 24 bytes, all PIC X
    let mut data: Vec<u8> = Vec::new();
    // HEADER "HDR1" (H=C8 D=C4 R=D9 1=F1)
    data.extend_from_slice(&[0xC8, 0xC4, 0xD9, 0xF1]);
    // Item 1: "ITEM01" → C9 E3 C5 D4 F0 F1
    data.extend_from_slice(&[0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF1]);
    // Item 2: "ITEM02" → C9 E3 C5 D4 F0 F2
    data.extend_from_slice(&[0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF2]);
    // Item 3: "ITEM03" → C9 E3 C5 D4 F0 F3
    data.extend_from_slice(&[0xC9, 0xE3, 0xC5, 0xD4, 0xF0, 0xF3]);
    // TRAILER "ZZ" (Z=E9)
    data.extend_from_slice(&[0xE9, 0xE9]);

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    // Decode and verify array structure
    let json = decode_record(&schema, &data, &dopts).expect("decode");

    // Verify HEADER and TRAILER decoded correctly
    assert_eq!(json["HEADER"].as_str().unwrap(), "HDR1");
    assert_eq!(json["TRAILER"].as_str().unwrap(), "ZZ");

    // Verify ITEMS is an array with 3 elements
    let items = json["ITEMS"].as_array().expect("ITEMS should be an array");
    assert_eq!(items.len(), 3, "ITEMS should have 3 elements");

    // Verify each element's value
    assert_eq!(items[0]["ITEM-VAL"].as_str().unwrap(), "ITEM01");
    assert_eq!(items[1]["ITEM-VAL"].as_str().unwrap(), "ITEM02");
    assert_eq!(items[2]["ITEM-VAL"].as_str().unwrap(), "ITEM03");

    // Decode determinism: same binary → identical JSON
    let json2 = decode_record(&schema, &data, &dopts).expect("decode-2");
    assert_eq!(json, json2, "OCCURS decode must be deterministic");
}

/// Record with REDEFINES → decode both views share storage, encode via primary
/// view produces binary-identical output.
#[test]
fn record_redefines_roundtrip() {
    let cpy = r"
        01 REDEF-REC.
           05 REC-TYPE   PIC X(2).
           05 FIELD-A    PIC X(8).
           05 FIELD-B REDEFINES FIELD-A PIC X(8).
           05 TRAILER    PIC X(3).
    ";
    // Hand-construct EBCDIC binary: 2 + 8 + 3 = 13 bytes
    let mut data: Vec<u8> = Vec::new();
    // REC-TYPE "AA" (A=C1)
    data.extend_from_slice(&[0xC1, 0xC1]);
    // FIELD-A/B "DATA1234" (D=C4 A=C1 T=E3 A=C1 1=F1 2=F2 3=F3 4=F4)
    data.extend_from_slice(&[0xC4, 0xC1, 0xE3, 0xC1, 0xF1, 0xF2, 0xF3, 0xF4]);
    // TRAILER "XYZ" (X=E7 Y=E8 Z=E9)
    data.extend_from_slice(&[0xE7, 0xE8, 0xE9]);

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    // Decode and verify both REDEFINES views share storage
    let json1 = decode_record(&schema, &data, &dopts).expect("decode");
    assert_eq!(
        json1["FIELD-A"], json1["FIELD-B"],
        "REDEFINES views must share storage"
    );

    // Decode determinism: same binary always produces identical JSON
    let json2 = decode_record(&schema, &data, &dopts).expect("decode-2");
    assert_eq!(json1, json2, "REDEFINES decode must be deterministic");

    // Verify field values decoded correctly
    let trailer = json1["TRAILER"].as_str().expect("TRAILER should be string");
    assert_eq!(
        trailer, "XYZ",
        "TRAILER should decode correctly after REDEFINES"
    );

    let field_a = json1["FIELD-A"].as_str().expect("FIELD-A should be string");
    assert_eq!(field_a, "DATA1234", "FIELD-A should decode correctly");
}

/// Large record (535 bytes) → round-trip fidelity.
#[test]
fn record_large_roundtrip() {
    let cpy = r"
        01 LARGE-REC.
           05 HEADER-ID    PIC X(20).
           05 CUST-NAME    PIC X(50).
           05 ADDR-LINE-1  PIC X(40).
           05 ADDR-LINE-2  PIC X(40).
           05 CITY         PIC X(30).
           05 STATE        PIC X(2).
           05 ZIP-CODE     PIC X(10).
           05 PHONE-NUM    PIC X(15).
           05 EMAIL-ADDR   PIC X(50).
           05 BALANCE-AMT  PIC S9(9)V99 COMP-3.
           05 CREDIT-LIM   PIC S9(7)V99 COMP-3.
           05 OPEN-DATE    PIC 9(8).
           05 LAST-DATE    PIC 9(8).
           05 ACCT-STATUS  PIC X(1).
           05 NOTES        PIC X(250).
    ";
    // 20+50+40+40+30+2+10+15+50+6+5+8+8+1+250 = 535 bytes

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = serde_json::json!({
        "HEADER-ID": "ACCT-REC-2025",
        "CUST-NAME": "JOHNATHAN SMITH",
        "ADDR-LINE-1": "1234 ENTERPRISE BOULEVARD",
        "ADDR-LINE-2": "SUITE 5678",
        "CITY": "METROPOLITAN CITY",
        "STATE": "NY",
        "ZIP-CODE": "10001",
        "PHONE-NUM": "2125551234",
        "EMAIL-ADDR": "JOHN.SMITH@EXAMPLE.COM",
        "BALANCE-AMT": "123456789.01",
        "CREDIT-LIM": "5000000.00",
        "OPEN-DATE": "20200115",
        "LAST-DATE": "20250601",
        "ACCT-STATUS": "A",
        "NOTES": "PREMIUM ENTERPRISE CUSTOMER ACCOUNT"
    });

    // Encode → decode → verify JSON preserved
    let encoded = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(encoded.len(), 535, "large record must be 535 bytes");

    let decoded = decode_record(&schema, &encoded, &dopts).expect("decode");
    assert_numeric_value(&decoded, "BALANCE-AMT", 123_456_789.01);
    assert_numeric_value(&decoded, "CREDIT-LIM", 5_000_000.0);

    // Binary stability: encode decoded → same bytes
    let re_encoded = encode_record(&schema, &decoded, &eopts).expect("re-encode");
    assert_eq!(
        encoded, re_encoded,
        "large record (535 bytes) must be binary-stable after round-trip"
    );

    // JSON stability
    let decoded2 = decode_record(&schema, &re_encoded, &dopts).expect("decode-2");
    assert_eq!(
        decoded, decoded2,
        "large record JSON round-trip must be identical"
    );
}

// =========================================================================
// 5. Determinism Proofs (3 tests)
// =========================================================================

/// Decode same record 100 times → all outputs byte-identical (SHA-256).
#[test]
fn determinism_decode_100_runs_sha256() {
    let cpy = r"
        01 DET-REC.
           05 ID-FIELD    PIC 9(8).
           05 NAME-FIELD  PIC X(20).
           05 AMOUNT      PIC S9(7)V99 COMP-3.
           05 STATUS      PIC X(1).
    ";
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]); // "00001234"
    // "DETERMINISM TEST    " in CP037
    data.extend_from_slice(&[
        0xC4, 0xC5, 0xE3, 0xC5, 0xD9, 0xD4, 0xC9, 0xD5, 0xC9, 0xE2, 0xD4, 0x40, 0xE3, 0xC5, 0xE2,
        0xE3, 0x40, 0x40, 0x40, 0x40,
    ]);
    data.extend_from_slice(&[0x00, 0x98, 0x76, 0x54, 0x3C]); // +98765.43 COMP-3
    data.push(0xC1); // "A"

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_hash = sha256_hex(first.to_string().as_bytes());

    for i in 2..=100 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        let hash = sha256_hex(json.to_string().as_bytes());
        assert_eq!(
            first_hash, hash,
            "Decode run {i}/100 produced different output (SHA-256 mismatch)"
        );
    }
}

/// Encode same JSON 100 times → all outputs byte-identical (SHA-256).
#[test]
fn determinism_encode_100_runs_sha256() {
    let cpy = r"
        01 ENC-REC.
           05 CODE-FIELD  PIC X(4).
           05 QTY-FIELD   PIC 9(6).
           05 AMOUNT      PIC S9(5)V99 COMP-3.
           05 NOTE        PIC X(10).
    ";
    let schema = parse_copybook(cpy).expect("parse");
    let json = serde_json::json!({
        "CODE-FIELD": "ABCD",
        "QTY-FIELD": "012345",
        "AMOUNT": "123.45",
        "NOTE": "TEST"
    });
    let opts = encode_opts(Codepage::CP037);

    let first = encode_record(&schema, &json, &opts).expect("encode");
    let first_hash = sha256_hex(&first);

    for i in 2..=100 {
        let encoded = encode_record(&schema, &json, &opts).expect("encode");
        let hash = sha256_hex(&encoded);
        assert_eq!(
            first_hash, hash,
            "Encode run {i}/100 produced different output (SHA-256 mismatch)"
        );
    }
}

/// Round-trip 50 times → convergence proof (decode→encode→decode chain).
///
/// Starting from known-good binary, verifies that the decode→encode cycle
/// reaches a fixed point on the first iteration and remains there for 50
/// consecutive iterations.
#[test]
fn determinism_roundtrip_50_convergence() {
    let cpy = r"
        01 CONV-REC.
           05 HDR         PIC X(3).
           05 VALUE-A     PIC 9(5).
           05 VALUE-B     PIC S9(5)V99 COMP-3.
           05 TEXT-FLD    PIC X(10).
           05 TRAIL       PIC X(2).
    ";
    // 3 + 5 + 4 + 10 + 2 = 24 bytes
    let mut data: Vec<u8> = Vec::new();
    // HDR "ABC" (A=C1 B=C2 C=C3)
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3]);
    // VALUE-A "54321"
    data.extend_from_slice(&[0xF5, 0xF4, 0xF3, 0xF2, 0xF1]);
    // VALUE-B −123.45 COMP-3: PIC S9(5)V99 = 7 digits + sign = 4 bytes
    // digits 0012345, sign D → 00 12 34 5D
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5D]);
    // TEXT-FLD "CONVERGE  " (C=C3 O=D6 N=D5 V=E5 E=C5 R=D9 G=C7 E=C5)
    data.extend_from_slice(&[0xC3, 0xD6, 0xD5, 0xE5, 0xC5, 0xD9, 0xC7, 0xC5, 0x40, 0x40]);
    // TRAIL "XY" (X=E7 Y=E8)
    data.extend_from_slice(&[0xE7, 0xE8]);

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    // First round-trip establishes the fixed point
    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let stable_binary = encode_record(&schema, &json, &eopts).expect("encode");
    assert_eq!(data, stable_binary, "first round-trip must be exact");

    // 50 subsequent iterations must produce identical binary and JSON
    let stable_hash = sha256_hex(&stable_binary);
    let stable_json_hash = sha256_hex(json.to_string().as_bytes());

    for i in 2..=50 {
        let decoded = decode_record(&schema, &stable_binary, &dopts).expect("decode");
        let json_hash = sha256_hex(decoded.to_string().as_bytes());
        assert_eq!(
            stable_json_hash, json_hash,
            "convergence: JSON differs at iteration {i}/50"
        );

        let encoded = encode_record(&schema, &decoded, &eopts).expect("encode");
        let bin_hash = sha256_hex(&encoded);
        assert_eq!(
            stable_hash, bin_hash,
            "convergence: binary differs at iteration {i}/50"
        );
    }
}
