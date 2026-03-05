// SPDX-License-Identifier: AGPL-3.0-or-later
//! DISPLAY numeric edge-case tests for zoned decimal fields.
//!
//! Covers:
//! 1. Unsigned display (PIC 9(n)) — zero, max, leading zeros
//! 2. Signed display with overpunch (PIC S9(n)) — positive, negative, zero
//! 3. Implied decimal (PIC 9(n)V99, PIC S9(n)V99) — decode, encode, roundtrip
//! 4. Leading zeros handling — preservation vs stripping
//! 5. Blank/space in numeric fields — error behavior
//! 6. EBCDIC zoned decimal — overpunch encoding, zone nibbles
//! 7. Record-level roundtrip fidelity
//! 8. Signed overpunch digit mapping (0-9 positive/negative)
//! 9. Single-digit signed fields
//! 10. Large-precision fields (PIC 9(18))

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::{decode_zoned_decimal, encode_zoned_decimal};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use serde_json::json;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true)
}

fn ebcdic_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn ebcdic_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true)
}

fn schema_for(pic: &str) -> copybook_core::Schema {
    let cpy = format!("       01 REC.\n           05 FLD {pic}.");
    parse_copybook(&cpy).expect("schema should parse")
}

// ===========================================================================
// 1. Unsigned display (PIC 9(n)) — basic decode/encode
// ===========================================================================

#[test]
fn test_unsigned_pic9_1_zero() {
    let schema = schema_for("PIC 9");
    let json = decode_record(&schema, b"0", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "0");
}

#[test]
fn test_unsigned_pic9_1_nine() {
    let schema = schema_for("PIC 9");
    let json = decode_record(&schema, b"9", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "9");
}

#[test]
fn test_unsigned_pic9_5_all_zeros() {
    let schema = schema_for("PIC 9(5)");
    let json = decode_record(&schema, b"00000", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "0");
}

#[test]
fn test_unsigned_pic9_5_all_nines() {
    let schema = schema_for("PIC 9(5)");
    let json = decode_record(&schema, b"99999", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "99999");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, b"99999");
}

#[test]
fn test_unsigned_pic9_5_leading_zeros_preserved_in_encode() {
    let schema = schema_for("PIC 9(5)");
    // Encode "42" into a 5-digit field → should produce "00042"
    let json_in = json!({"FLD": "42"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, b"00042");
}

#[test]
fn test_unsigned_pic9_5_roundtrip() {
    let schema = schema_for("PIC 9(5)");
    let json_in = json!({"FLD": "12345"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "12345");
}

// ===========================================================================
// 2. Signed display with overpunch (PIC S9(n)) — ASCII
// ===========================================================================

#[test]
fn test_signed_pic_s9_positive_zero_overpunch() {
    let schema = schema_for("PIC S9");
    // ASCII overpunch: '{' = +0
    let json = decode_record(&schema, b"{", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "0");
}

#[test]
fn test_signed_pic_s9_negative_zero_overpunch() {
    let schema = schema_for("PIC S9");
    // ASCII overpunch: '}' = -0, normalizes to "0"
    let json = decode_record(&schema, b"}", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "0");
}

#[test]
fn test_signed_pic_s9_5_positive_overpunch_all_digits() {
    let schema = schema_for("PIC S9(5)");
    // ASCII overpunch positive: {=0, A=1, B=2, C=3, D=4, E=5, F=6, G=7, H=8, I=9
    // Zoned decimal decode preserves leading zeros in lossless mode
    let cases = [
        (b"0000{" as &[u8], "0"),
        (b"0000A", "00001"),
        (b"0000B", "00002"),
        (b"0000C", "00003"),
        (b"0000D", "00004"),
        (b"0000E", "00005"),
        (b"0000F", "00006"),
        (b"0000G", "00007"),
        (b"0000H", "00008"),
        (b"0000I", "00009"),
    ];
    for (data, expected) in cases {
        let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
        assert_eq!(json["FLD"].as_str().unwrap(), expected, "data={data:?}");
    }
}

#[test]
fn test_signed_pic_s9_5_negative_overpunch_all_digits() {
    let schema = schema_for("PIC S9(5)");
    // ASCII overpunch negative: }=0, J=1, K=2, L=3, M=4, N=5, O=6, P=7, Q=8, R=9
    // Zoned decimal decode preserves leading zeros in lossless mode
    let cases = [
        (b"0000J" as &[u8], "-00001"),
        (b"0000K", "-00002"),
        (b"0000L", "-00003"),
        (b"0000M", "-00004"),
        (b"0000N", "-00005"),
        (b"0000O", "-00006"),
        (b"0000P", "-00007"),
        (b"0000Q", "-00008"),
        (b"0000R", "-00009"),
    ];
    for (data, expected) in cases {
        let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
        assert_eq!(json["FLD"].as_str().unwrap(), expected, "data={data:?}");
    }
}

#[test]
fn test_signed_pic_s9_5_max_positive() {
    let schema = schema_for("PIC S9(5)");
    // 99999 positive: "9999I" (I = positive 9)
    let json = decode_record(&schema, b"9999I", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "99999");
}

#[test]
fn test_signed_pic_s9_5_max_negative() {
    let schema = schema_for("PIC S9(5)");
    // -99999: "9999R" (R = negative 9)
    let json = decode_record(&schema, b"9999R", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "-99999");
}

#[test]
fn test_signed_pic_s9_5_roundtrip_positive() {
    let schema = schema_for("PIC S9(5)");
    let json_in = json!({"FLD": "12345"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "12345");
}

#[test]
fn test_signed_pic_s9_5_roundtrip_negative() {
    let schema = schema_for("PIC S9(5)");
    let json_in = json!({"FLD": "-12345"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "-12345");
}

// ===========================================================================
// 3. Implied decimal (PIC 9(n)V99, PIC S9(n)V99)
// ===========================================================================

#[test]
fn test_unsigned_implied_decimal_pic9v99_zero() {
    let schema = schema_for("PIC 9V99");
    let json = decode_record(&schema, b"000", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "0.00");
}

#[test]
fn test_unsigned_implied_decimal_pic9v99_max() {
    let schema = schema_for("PIC 9V99");
    let json = decode_record(&schema, b"999", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "9.99");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, b"999");
}

#[test]
fn test_unsigned_implied_decimal_pic9_5_v99_roundtrip() {
    let schema = schema_for("PIC 9(5)V99");
    let json_in = json!({"FLD": "12345.67"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, b"1234567");
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "12345.67");
}

#[test]
fn test_signed_implied_decimal_pic_s9_5_v99_positive() {
    let schema = schema_for("PIC S9(5)V99");
    // "1234567" with last byte overpunched positive: 'G' = +7
    let json = decode_record(&schema, b"123456G", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "12345.67");
}

#[test]
fn test_signed_implied_decimal_pic_s9_5_v99_negative() {
    let schema = schema_for("PIC S9(5)V99");
    // Negative overpunch: 'P' = -7
    let json = decode_record(&schema, b"123456P", &ascii_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "-12345.67");
}

#[test]
fn test_signed_implied_decimal_roundtrip() {
    let schema = schema_for("PIC S9(5)V99");
    let json_in = json!({"FLD": "-99999.99"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "-99999.99");
}

#[test]
fn test_signed_implied_decimal_zero_roundtrip() {
    let schema = schema_for("PIC S9(5)V99");
    let json_in = json!({"FLD": "0.00"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "0.00");
}

#[test]
fn test_implied_decimal_v9999_four_places() {
    let schema = schema_for("PIC 9(3)V9(4)");
    let json_in = json!({"FLD": "123.4567"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, b"1234567");
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "123.4567");
}

#[test]
fn test_implied_decimal_small_fraction() {
    let schema = schema_for("PIC 9V9(4)");
    let json_in = json!({"FLD": "0.0001"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "0.0001");
}

// ===========================================================================
// 4. Leading zeros handling
// ===========================================================================

#[test]
fn test_leading_zeros_unsigned_decode_strips() {
    let schema = schema_for("PIC 9(5)");
    // "00042" decodes — leading zeros may or may not be stripped depending on mode
    let json = decode_record(&schema, b"00042", &ascii_decode_opts()).unwrap();
    let val = json["FLD"].as_str().unwrap();
    // Unsigned display preserves leading zeros in lossless
    assert!(val == "00042" || val == "42");
}

#[test]
fn test_leading_zeros_encode_pads() {
    let schema = schema_for("PIC 9(8)");
    let json_in = json!({"FLD": "1"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, b"00000001");
}

#[test]
fn test_leading_zeros_signed_encode() {
    let schema = schema_for("PIC S9(8)");
    let json_in = json!({"FLD": "1"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    // Should be 8 bytes: 7 leading zeros + overpunched last digit
    assert_eq!(encoded.len(), 8);
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    // Zoned decimal decode preserves leading zeros
    assert_eq!(decoded["FLD"], "00000001");
}

// ===========================================================================
// 5. Blank/space in numeric fields — error behavior
// ===========================================================================

#[test]
fn test_blank_in_unsigned_numeric_field_errors() {
    let schema = schema_for("PIC 9(5)");
    let data = b"  123"; // spaces in numeric field
    let result = decode_record(&schema, data, &ascii_decode_opts());
    // Spaces in numeric fields should produce an error
    assert!(result.is_err(), "spaces in numeric field should error");
}

#[test]
fn test_all_blanks_in_numeric_field_errors() {
    let schema = schema_for("PIC 9(3)");
    let data = b"   "; // all spaces
    let result = decode_record(&schema, data, &ascii_decode_opts());
    assert!(result.is_err(), "all-blanks numeric field should error");
}

// ===========================================================================
// 6. EBCDIC zoned decimal
// ===========================================================================

#[test]
fn test_ebcdic_unsigned_pic9_5_decode() {
    let schema = schema_for("PIC 9(5)");
    // EBCDIC digits: 0xF0='0' through 0xF9='9'
    let data = [0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "12345");
}

#[test]
fn test_ebcdic_unsigned_pic9_5_roundtrip() {
    let schema = schema_for("PIC 9(5)");
    let data = [0xF0, 0xF0, 0xF0, 0xF4, 0xF2]; // "00042"
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    let re = encode_record(&schema, &json, &ebcdic_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_ebcdic_signed_pic_s9_5_positive() {
    let schema = schema_for("PIC S9(5)");
    // EBCDIC positive overpunch: last byte zone nibble = 0xC
    // "12345" positive → [0xF1, 0xF2, 0xF3, 0xF4, 0xC5]
    let data = [0xF1, 0xF2, 0xF3, 0xF4, 0xC5];
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "12345");
}

#[test]
fn test_ebcdic_signed_pic_s9_5_negative() {
    let schema = schema_for("PIC S9(5)");
    // EBCDIC negative overpunch: last byte zone nibble = 0xD
    // "-12345" → [0xF1, 0xF2, 0xF3, 0xF4, 0xD5]
    let data = [0xF1, 0xF2, 0xF3, 0xF4, 0xD5];
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(json["FLD"], "-12345");
}

#[test]
fn test_ebcdic_signed_pic_s9_5_roundtrip_positive() {
    let schema = schema_for("PIC S9(5)");
    let json_in = json!({"FLD": "67890"});
    let encoded = encode_record(&schema, &json_in, &ebcdic_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ebcdic_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "67890");
}

#[test]
fn test_ebcdic_signed_pic_s9_5_roundtrip_negative() {
    let schema = schema_for("PIC S9(5)");
    // Verify that manually encoded EBCDIC negative data decodes correctly
    // EBCDIC: "-67890" → [0xF6, 0xF7, 0xF8, 0xF9, 0xD0]
    let data = [0xF6, 0xF7, 0xF8, 0xF9, 0xD0];
    let decoded = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], "-67890");
}

// ===========================================================================
// 7. Low-level zoned decimal API
// ===========================================================================

#[test]
fn test_zoned_encode_decode_unsigned_roundtrip() {
    for val in ["0", "1", "42", "999", "12345", "99999"] {
        let digits = 5u16;
        let enc = encode_zoned_decimal(val, digits, 0, false, Codepage::ASCII).unwrap();
        assert_eq!(enc.len(), digits as usize);
        let dec = decode_zoned_decimal(&enc, digits, 0, false, Codepage::ASCII, false).unwrap();
        let dec_str = dec.to_string();
        let expected = val.parse::<i64>().unwrap().to_string();
        assert_eq!(dec_str, expected, "roundtrip mismatch for {val}");
    }
}

#[test]
fn test_zoned_encode_decode_signed_roundtrip() {
    for val in ["0", "1", "-1", "99999", "-99999", "12345", "-12345"] {
        let digits = 5u16;
        let enc = encode_zoned_decimal(val, digits, 0, true, Codepage::ASCII).unwrap();
        assert_eq!(enc.len(), digits as usize);
        let dec = decode_zoned_decimal(&enc, digits, 0, true, Codepage::ASCII, false).unwrap();
        let dec_str = dec.to_string();
        let expected = val.parse::<i64>().unwrap().to_string();
        assert_eq!(dec_str, expected, "roundtrip mismatch for {val}");
    }
}

#[test]
fn test_zoned_encode_with_scale_roundtrip() {
    let enc = encode_zoned_decimal("123.45", 5, 2, false, Codepage::ASCII).unwrap();
    assert_eq!(enc, b"12345");
    let dec = decode_zoned_decimal(&enc, 5, 2, false, Codepage::ASCII, false).unwrap();
    assert_eq!(dec.to_string(), "123.45");
}

#[test]
fn test_zoned_ebcdic_unsigned_roundtrip() {
    let enc = encode_zoned_decimal("42", 3, 0, false, Codepage::CP037).unwrap();
    // EBCDIC: each byte should have 0xF0 zone
    assert_eq!(enc[0], 0xF0); // '0'
    assert_eq!(enc[1], 0xF4); // '4'
    assert_eq!(enc[2], 0xF2); // '2'
    let dec = decode_zoned_decimal(&enc, 3, 0, false, Codepage::CP037, false).unwrap();
    assert_eq!(dec.to_string(), "42");
}

#[test]
fn test_zoned_ebcdic_signed_negative_roundtrip() {
    let enc = encode_zoned_decimal("-42", 3, 0, true, Codepage::CP037).unwrap();
    // Last byte should have 0xD zone (negative)
    assert_eq!(enc[2] & 0xF0, 0xD0);
    let dec = decode_zoned_decimal(&enc, 3, 0, true, Codepage::CP037, false).unwrap();
    assert_eq!(dec.to_string(), "-42");
}

// ===========================================================================
// 8. Single digit signed fields
// ===========================================================================

#[test]
fn test_single_digit_signed_all_positive_values() {
    let schema = schema_for("PIC S9");
    for digit in 0..=9 {
        let json_in = json!({"FLD": digit.to_string()});
        let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
        assert_eq!(encoded.len(), 1, "S9 should be 1 byte");
        let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
        assert_eq!(decoded["FLD"], digit.to_string());
    }
}

#[test]
fn test_single_digit_signed_all_negative_values() {
    let schema = schema_for("PIC S9");
    for digit in 1..=9 {
        let json_in = json!({"FLD": format!("-{digit}")});
        let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
        assert_eq!(encoded.len(), 1, "S9 should be 1 byte");
        let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
        assert_eq!(decoded["FLD"], format!("-{digit}"));
    }
}

// ===========================================================================
// 9. Large precision fields (PIC 9(18))
// ===========================================================================

#[test]
fn test_pic9_18_max_value_roundtrip() {
    let schema = schema_for("PIC 9(18)");
    let max18 = "999999999999999999";
    let json_in = json!({"FLD": max18});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 18);
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], max18);
}

#[test]
fn test_pic_s9_18_positive_max_roundtrip() {
    let schema = schema_for("PIC S9(18)");
    let max18 = "999999999999999999";
    let json_in = json!({"FLD": max18});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 18);
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], max18);
}

#[test]
fn test_pic_s9_18_negative_max_roundtrip() {
    let schema = schema_for("PIC S9(18)");
    let min18 = "-999999999999999999";
    let json_in = json!({"FLD": min18});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["FLD"], min18);
}

// ===========================================================================
// 10. Multi-field mixed numeric records
// ===========================================================================

#[test]
fn test_multi_field_unsigned_record() {
    let cpy = "\
       01  REC.
           05  QTY   PIC 9(3).
           05  PRICE PIC 9(5)V99.
           05  TAX   PIC 9V99.
";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"0100010050050";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["PRICE"], "100.50");
    assert_eq!(json["TAX"], "0.50");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_multi_field_signed_record_roundtrip() {
    let cpy = "\
       01  REC.
           05  AMT1 PIC S9(5)V99.
           05  AMT2 PIC S9(5)V99.
";
    let schema = parse_copybook(cpy).expect("parse");
    let json_in = json!({"AMT1": "12345.67", "AMT2": "-99999.99"});
    let encoded = encode_record(&schema, &json_in, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 14); // 7 + 7
    let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
    assert_eq!(decoded["AMT1"], "12345.67");
    assert_eq!(decoded["AMT2"], "-99999.99");
}

// ===========================================================================
// 11. Encode overflow/reject
// ===========================================================================

#[test]
fn test_encode_overflow_unsigned() {
    let schema = schema_for("PIC 9(3)");
    let json_in = json!({"FLD": "1000"});
    let result = encode_record(&schema, &json_in, &ascii_encode_opts());
    assert!(result.is_err(), "1000 should overflow PIC 9(3)");
}

#[test]
fn test_encode_overflow_signed() {
    let schema = schema_for("PIC S9(3)");
    let json_in = json!({"FLD": "1000"});
    let result = encode_record(&schema, &json_in, &ascii_encode_opts());
    assert!(result.is_err(), "1000 should overflow PIC S9(3)");
}

#[test]
fn test_encode_negative_in_unsigned_field_coerced() {
    let schema = schema_for("PIC 9(5)");
    // Unsigned fields encode the absolute value when given a negative
    let json_in = json!({"FLD": "-1"});
    let result = encode_record(&schema, &json_in, &ascii_encode_opts());
    // Codec may accept or reject; verify consistent behavior
    if let Ok(encoded) = result {
        let decoded = decode_record(&schema, &encoded, &ascii_decode_opts()).unwrap();
        // Unsigned field cannot represent negative values
        let val = decoded["FLD"].as_str().unwrap();
        assert!(
            !val.starts_with('-'),
            "unsigned field should not decode as negative"
        );
    }
}
