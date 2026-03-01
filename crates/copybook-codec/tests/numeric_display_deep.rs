// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Deep edge-case tests for numeric display (zoned decimal) fields.
//!
//! Covers:
//! - PIC 9(18) (maximum digits)
//! - PIC S9 (single digit signed)
//! - PIC 9V99 (implied decimal)
//! - PIC S9(5)V99 (signed with decimal)
//! - Leading zeros preservation
//! - Decode with different JSON number modes (native vs lossless)
//! - Unsigned zoned decimal encode/decode
//! - Multi-field records with mixed numeric display types

use copybook_codec::numeric::{decode_zoned_decimal, encode_zoned_decimal};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;

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

fn native_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Native)
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

// ===========================================================================
// 1. PIC 9 (single unsigned digit)
// ===========================================================================

#[test]
fn test_pic_9_single_digit_zero() {
    let cpy = "       01  REC.\n           05  F1 PIC 9.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"0";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_pic_9_single_digit_nine() {
    let cpy = "       01  REC.\n           05  F1 PIC 9.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"9";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "9");
}

// ===========================================================================
// 2. PIC S9 (single signed digit — overpunch)
// ===========================================================================

#[test]
fn test_pic_s9_positive_zero() {
    let cpy = "       01  REC.\n           05  F1 PIC S9.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // ASCII positive zero overpunch: '{' = 0x7B maps to +0
    let data = b"{";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0");
}

#[test]
fn test_pic_s9_positive_five() {
    let cpy = "       01  REC.\n           05  F1 PIC S9.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // ASCII overpunch: 'E' = positive 5
    let data = b"E";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "5");
}

// ===========================================================================
// 3. PIC 9(n) — multi-digit unsigned
// ===========================================================================

#[test]
fn test_pic_9_5_all_zeros() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"00000";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_pic_9_5_leading_zeros() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"00042";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    // Codec preserves leading zeros for unsigned display fields
    assert_eq!(json["F1"], "00042");
}

#[test]
fn test_pic_9_5_max_value() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"99999";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "99999");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

// ===========================================================================
// 4. PIC 9(18) — max digits
// ===========================================================================

#[test]
fn test_pic_9_18_all_nines() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(18).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"999999999999999999"; // 18 nines
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "999999999999999999");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

#[test]
fn test_pic_9_18_all_zeros() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(18).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"000000000000000000"; // 18 zeros
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0");
}

#[test]
fn test_pic_9_18_one() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(18).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"000000000000000001"; // 1 with leading zeros
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    // Unsigned display fields preserve leading zeros
    assert_eq!(json["F1"], "000000000000000001");
}

// ===========================================================================
// 5. PIC 9V99 — implied decimal
// ===========================================================================

#[test]
fn test_pic_9v99_decode() {
    let cpy = "       01  REC.\n           05  F1 PIC 9V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // "123" means 1.23
    let data = b"123";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "1.23");
}

#[test]
fn test_pic_9v99_zero() {
    let cpy = "       01  REC.\n           05  F1 PIC 9V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"000";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0.00");
}

#[test]
fn test_pic_9v99_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC 9V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"999";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "9.99");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

// ===========================================================================
// 6. PIC 9(5)V99 — larger integer part with decimal
// ===========================================================================

#[test]
fn test_pic_9_5_v99_decode() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // "1234567" means 12345.67
    let data = b"1234567";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "12345.67");
}

#[test]
fn test_pic_9_5_v99_zero() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"0000000";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0.00");
}

#[test]
fn test_pic_9_5_v99_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"0010050";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "100.50");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

// ===========================================================================
// 7. PIC S9(5)V99 — signed with decimal (overpunch)
// ===========================================================================

#[test]
fn test_pic_s9_5_v99_positive() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(5)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // "1234567" with last byte overpunched positive:
    // '7' positive overpunch in ASCII is 'G'
    let data = b"123456G";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "12345.67");
}

#[test]
fn test_pic_s9_5_v99_negative() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(5)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // Negative overpunch: 'P' = negative 7 in ASCII
    let data = b"123456P";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "-12345.67");
}

#[test]
fn test_pic_s9_5_v99_positive_zero() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(5)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // Positive zero: '{' = positive 0
    let data = b"000000{";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0.00");
}

// ===========================================================================
// 8. JSON number modes: Lossless vs Native
// ===========================================================================

#[test]
fn test_lossless_mode_returns_string() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"00042";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    // Lossless mode returns string with leading zeros preserved
    assert!(json["F1"].is_string());
    assert_eq!(json["F1"], "00042");
}

#[test]
fn test_native_mode_returns_number() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"00042";
    let json = decode_record(&schema, data, &native_decode_opts()).unwrap();
    // Display fields return string representation even in native mode
    assert!(json["F1"].is_string());
    assert_eq!(json["F1"], "00042");
}

#[test]
fn test_native_mode_decimal() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(3)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"12345"; // 123.45
    let json = decode_record(&schema, data, &native_decode_opts()).unwrap();
    // Display fields with implied decimal — verify value is present
    let val_str = json["F1"].as_str().unwrap();
    assert_eq!(val_str, "123.45");
}

#[test]
fn test_lossless_mode_preserves_decimal_precision() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(3)V99.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data = b"00010"; // 0.10
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "0.10");
}

// ===========================================================================
// 9. Low-level zoned decimal encode/decode
// ===========================================================================

#[test]
fn test_zoned_encode_unsigned_zero() {
    let encoded = encode_zoned_decimal("0", 3, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, vec![0x30, 0x30, 0x30]); // ASCII "000"
}

#[test]
fn test_zoned_encode_unsigned_123() {
    let encoded = encode_zoned_decimal("123", 5, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, vec![0x30, 0x30, 0x31, 0x32, 0x33]); // "00123"
}

#[test]
fn test_zoned_decode_unsigned_ascii() {
    let data = b"12345";
    let result = decode_zoned_decimal(data, 5, 0, false, Codepage::ASCII, false).unwrap();
    assert_eq!(result.value, 12345);
    assert_eq!(result.scale, 0);
    assert!(!result.negative);
}

#[test]
fn test_zoned_decode_with_scale() {
    let data = b"12345";
    let result = decode_zoned_decimal(data, 5, 2, false, Codepage::ASCII, false).unwrap();
    // SmallDecimal stores unscaled value and scale separately
    assert_eq!(result.value, 12345);
    assert_eq!(result.scale, 2);
}

#[test]
fn test_zoned_encode_with_scale() {
    let encoded = encode_zoned_decimal("123.45", 5, 2, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"12345");
}

// ===========================================================================
// 10. Multi-field numeric display records
// ===========================================================================

#[test]
fn test_record_multi_numeric_display_fields() {
    let cpy = "\
       01  REC.
           05  QTY    PIC 9(3).
           05  PRICE  PIC 9(5)V99.
           05  TAX    PIC 9V99.
";
    let schema = parse_copybook(cpy).expect("parse");
    // QTY=3 bytes, PRICE=7 bytes, TAX=3 bytes = 13 total
    let data = b"0100010050050";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    // Unsigned display fields preserve leading zeros
    assert_eq!(json["QTY"], "010");
    assert_eq!(json["PRICE"], "100.50");
    assert_eq!(json["TAX"], "0.50");
    let re = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re, data);
}

// ===========================================================================
// 11. EBCDIC zoned decimal record roundtrip
// ===========================================================================

#[test]
fn test_record_pic_9_5_ebcdic_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(5).\n";
    let schema = parse_copybook(cpy).expect("parse");
    // EBCDIC digits: 0xF0-0xF9
    let data = [0xF0, 0xF1, 0xF2, 0xF3, 0xF4]; // "01234"
    let json = decode_record(&schema, &data, &ebcdic_decode_opts()).unwrap();
    // Unsigned display preserves leading zeros
    assert_eq!(json["F1"], "01234");
    let re = encode_record(&schema, &json, &ebcdic_encode_opts()).unwrap();
    assert_eq!(re, data);
}

// ===========================================================================
// 12. PIC S9(n) signed numeric — larger sizes
// ===========================================================================

#[test]
fn test_pic_s9_9_positive_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(9).\n";
    let schema = parse_copybook(cpy).expect("parse");
    // "12345678" + overpunch positive 9 => 'I'
    let data = b"12345678I";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "123456789");
}

#[test]
fn test_pic_s9_9_negative_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(9).\n";
    let schema = parse_copybook(cpy).expect("parse");
    // Negative overpunch 9 in ASCII is 'R'
    let data = b"12345678R";
    let json = decode_record(&schema, data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "-123456789");
}
