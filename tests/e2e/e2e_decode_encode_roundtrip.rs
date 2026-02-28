// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end decode → encode round-trip tests.
//!
//! Validates that decoding binary COBOL data to JSON and encoding it back
//! produces identical (or semantically equivalent) results across multiple
//! copybook layouts and codepages.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    decode_record, encode_record, Codepage, DecodeOptions, EncodeOptions, JsonNumberMode,
    RecordFormat,
};
use copybook_core::parse_copybook;

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

// ---------------------------------------------------------------------------
// 1. Simple DISPLAY-only schema – binary-exact round-trip
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_display_only_cp037() {
    let cpy = r"
        01 SIMPLE-REC.
           05 NUM-FIELD   PIC 9(6).
           05 TXT-FIELD   PIC X(10).
           05 DATE-FIELD  PIC 9(8).
    ";

    // Build EBCDIC CP037 record (24 bytes total)
    let mut data: Vec<u8> = Vec::new();
    // NUM-FIELD "123456"
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6]);
    // TXT-FIELD "HELLO     " (CP037: H=C8 E=C5 L=D3 O=D6 space=40)
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6, 0x40, 0x40, 0x40, 0x40, 0x40]);
    // DATE-FIELD "20240101"
    data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF4, 0xF0, 0xF1, 0xF0, 0xF1]);

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");

    assert_eq!(
        data, re_encoded,
        "Binary round-trip must be identical for DISPLAY-only schema"
    );
}

// ---------------------------------------------------------------------------
// 2. Schema with COMP-3 packed decimal – JSON round-trip
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_comp3_json_equality() {
    let cpy = r"
        01 COMP3-REC.
           05 REC-ID      PIC 9(4).
           05 AMOUNT       PIC S9(7)V99 COMP-3.
           05 LABEL        PIC X(10).
    ";

    // REC-ID "0001", AMOUNT +12345.67, LABEL "TEST      "
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF1]); // "0001"
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x56, 0x7C]); // +12345.67 COMP-3
    // "TEST" in CP037: T=E3 E=C5 S=E2 T=E3 + 6 spaces
    data.extend_from_slice(&[0xE3, 0xC5, 0xE2, 0xE3, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40]);

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json1 = decode_record(&schema, &data, &dopts).expect("decode");
    let re_encoded = encode_record(&schema, &json1, &eopts).expect("encode");
    let json2 = decode_record(&schema, &re_encoded, &dopts).expect("re-decode");

    assert_eq!(
        json1, json2,
        "JSON round-trip (decode→encode→decode) must be identical"
    );
}

// ---------------------------------------------------------------------------
// 3. REDEFINES schema – decode verifies both views are present
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_redefines_decode_both_views() {
    let cpy = r"
        01 REDEF-REC.
           05 REC-TYPE     PIC X(2).
           05 FIELD-A      PIC X(10).
           05 FIELD-B REDEFINES FIELD-A PIC X(10).
           05 TRAILER      PIC X(5).
    ";

    // REC-TYPE "AA", FIELD-A/B "ABCDEFGHIJ", TRAILER "ZZZZZ"
    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC1]); // "AA"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1]); // "ABCDEFGHIJ"
    data.extend_from_slice(&[0xE9, 0xE9, 0xE9, 0xE9, 0xE9]); // "ZZZZZ"

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");

    // Both REDEFINES views decode from the same storage
    assert_eq!(json["FIELD-A"], json["FIELD-B"], "REDEFINES views share storage");
    assert_eq!(json["TRAILER"], "ZZZZZ");

    // Determinism: decode the same data again → identical JSON
    let json2 = decode_record(&schema, &data, &dopts).expect("decode-2");
    assert_eq!(json, json2, "REDEFINES decode must be deterministic");
}

// ---------------------------------------------------------------------------
// 4. Cross-codepage: CP037 vs CP1047 for digit-only data
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_cp1047_digits_only() {
    // Digits F0-F9 are identical in CP037 and CP1047
    let cpy = r"
        01 DIGIT-REC.
           05 FIELD-A  PIC 9(8).
           05 FIELD-B  PIC 9(4).
    ";

    let data: Vec<u8> = vec![
        0xF2, 0xF0, 0xF2, 0xF4, 0xF1, 0xF2, 0xF3, 0xF4, // "20241234"
        0xF5, 0xF6, 0xF7, 0xF8, // "5678"
    ];

    let schema = parse_copybook(cpy).expect("parse");

    for cp in [Codepage::CP037, Codepage::CP1047] {
        let dopts = decode_opts(cp);
        let eopts = encode_opts(cp);

        let json = decode_record(&schema, &data, &dopts).expect("decode");
        let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");

        assert_eq!(
            data, re_encoded,
            "Digit-only round-trip must be binary-exact for {cp:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// 5. Fixture-based round-trip using fixtures/copybooks/simple.cpy + data
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_fixture_simple_cpy() {
    let cpy = r"
        01  CUSTOMER-RECORD.
            05  CUSTOMER-ID         PIC 9(6).
            05  CUSTOMER-NAME       PIC X(30).
            05  ACCOUNT-BALANCE     PIC S9(7)V99 COMP-3.
            05  LAST-ACTIVITY-DATE  PIC 9(8).
            05  STATUS-CODE         PIC X(1).
    ";

    // Data matching fixtures/data/simple.bin (50 bytes)
    let data: Vec<u8> = vec![
        0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, // CUSTOMER-ID "123456"
        0xD1, 0x96, 0x88, 0x95, 0x40, 0xE2, 0x94, 0x89, 0xA3, 0x88, // "John Smith"
        0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, // spaces
        0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, // spaces
        0x00, 0x12, 0x34, 0x56, 0x7C, // ACCOUNT-BALANCE +12345.67 COMP-3
        0xF2, 0xF0, 0xF2, 0xF3, 0xF0, 0xF9, 0xF1, 0xF5, // LAST-ACTIVITY-DATE "20230915"
        0xC1, // STATUS-CODE "A"
    ];

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json1 = decode_record(&schema, &data, &dopts).expect("decode");

    // Verify key fields decoded correctly
    assert!(json1.is_object(), "decoded value should be a JSON object");
    assert_eq!(json1["CUSTOMER-ID"], "123456");
    assert_eq!(json1["LAST-ACTIVITY-DATE"], "20230915");

    // JSON round-trip
    let re_encoded = encode_record(&schema, &json1, &eopts).expect("encode");
    let json2 = decode_record(&schema, &re_encoded, &dopts).expect("re-decode");
    assert_eq!(json1, json2, "JSON round-trip must produce identical output");
}
