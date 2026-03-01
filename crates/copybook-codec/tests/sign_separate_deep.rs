// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep tests for SIGN SEPARATE decode, encode, round-trip, codepage, and error handling.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::{
    decode_zoned_decimal_sign_separate, encode_zoned_decimal_sign_separate,
};
use copybook_codec::options::Codepage;
use copybook_codec::{
    DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, ZonedEncodingFormat,
    decode_record, encode_record,
};
use copybook_core::{ErrorCode, SignPlacement, SignSeparateInfo, parse_copybook};

// =============================================================================
// Helpers
// =============================================================================

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(true)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Auto)
}

fn encode_opts(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(cp)
        .with_use_raw(false)
        .with_bwz_encode(false)
        .with_strict_mode(true)
        .with_coerce_numbers(false)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Auto)
}

fn leading() -> SignSeparateInfo {
    SignSeparateInfo {
        placement: SignPlacement::Leading,
    }
}

fn trailing() -> SignSeparateInfo {
    SignSeparateInfo {
        placement: SignPlacement::Trailing,
    }
}

// =============================================================================
// 1. SIGN SEPARATE LEADING positive
// =============================================================================

#[test]
fn test_deep_sign_separate_leading_positive() {
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let result =
        decode_zoned_decimal_sign_separate(data, 5, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "12345");
    assert!(!result.is_negative());
}

// =============================================================================
// 2. SIGN SEPARATE LEADING negative
// =============================================================================

#[test]
fn test_deep_sign_separate_leading_negative() {
    let data: &[u8] = &[0x60, 0xF9, 0xF8, 0xF7, 0xF6, 0xF5];
    let result =
        decode_zoned_decimal_sign_separate(data, 5, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "-98765");
    assert!(result.is_negative());
}

// =============================================================================
// 3. SIGN SEPARATE TRAILING positive
// =============================================================================

#[test]
fn test_deep_sign_separate_trailing_positive() {
    let data: &[u8] = &[0xF5, 0xF4, 0xF3, 0xF2, 0xF1, 0x4E];
    let result =
        decode_zoned_decimal_sign_separate(data, 5, 0, &trailing(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "54321");
    assert!(!result.is_negative());
}

// =============================================================================
// 4. SIGN SEPARATE TRAILING negative
// =============================================================================

#[test]
fn test_deep_sign_separate_trailing_negative() {
    let data: &[u8] = &[0xF1, 0xF1, 0xF1, 0xF1, 0xF1, 0x60];
    let result =
        decode_zoned_decimal_sign_separate(data, 5, 0, &trailing(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "-11111");
    assert!(result.is_negative());
}

// =============================================================================
// 5. SIGN SEPARATE with decimal (V99)
// =============================================================================

#[test]
fn test_deep_sign_separate_v99_leading() {
    // PIC S9(3)V99 SIGN LEADING SEPARATE → 1 sign + 5 digits, scale=2
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let result =
        decode_zoned_decimal_sign_separate(data, 5, 2, &leading(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "123.45");
}

#[test]
fn test_deep_sign_separate_v99_trailing_negative() {
    let data: &[u8] = &[0xF0, 0xF5, 0xF0, 0xF7, 0xF5, 0x60];
    let result =
        decode_zoned_decimal_sign_separate(data, 5, 2, &trailing(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "-50.75");
}

// =============================================================================
// 6. SIGN SEPARATE with S9(5)V99 (full-stack via parse_copybook)
// =============================================================================

#[test]
fn test_deep_sign_separate_s9_5_v99_decode() {
    let copybook = "01 AMT PIC S9(5)V99 SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    // sign(+) + 0 0 1 2 3 4 5 → "123.45" (sign-separate path strips leading zeros for scaled values)
    let data: &[u8] = &[0x4E, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let result = decode_record(&schema, data, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(result["AMT"], serde_json::json!("123.45"));
}

#[test]
fn test_deep_sign_separate_s9_5_v99_negative_decode() {
    let copybook = "01 AMT PIC S9(5)V99 SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: &[u8] = &[0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0x60];
    let result = decode_record(&schema, data, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(result["AMT"], serde_json::json!("-99999.99"));
}

// =============================================================================
// 7. SIGN SEPARATE encode positive
// =============================================================================

#[test]
fn test_deep_sign_separate_encode_positive_leading() {
    let mut buf = vec![0u8; 6];
    encode_zoned_decimal_sign_separate("12345", 5, 0, &leading(), Codepage::CP037, &mut buf)
        .unwrap();
    assert_eq!(buf, [0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5]);
}

#[test]
fn test_deep_sign_separate_encode_positive_trailing() {
    let mut buf = vec![0u8; 6];
    encode_zoned_decimal_sign_separate("12345", 5, 0, &trailing(), Codepage::CP037, &mut buf)
        .unwrap();
    assert_eq!(buf, [0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0x4E]);
}

// =============================================================================
// 8. SIGN SEPARATE encode negative
// =============================================================================

#[test]
fn test_deep_sign_separate_encode_negative_leading() {
    let mut buf = vec![0u8; 6];
    encode_zoned_decimal_sign_separate("-67890", 5, 0, &leading(), Codepage::CP037, &mut buf)
        .unwrap();
    assert_eq!(buf, [0x60, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0]);
}

#[test]
fn test_deep_sign_separate_encode_negative_trailing() {
    let mut buf = vec![0u8; 6];
    encode_zoned_decimal_sign_separate("-67890", 5, 0, &trailing(), Codepage::CP037, &mut buf)
        .unwrap();
    assert_eq!(buf, [0xF6, 0xF7, 0xF8, 0xF9, 0xF0, 0x60]);
}

// =============================================================================
// 9. SIGN SEPARATE round-trip
// =============================================================================

#[test]
fn test_deep_sign_separate_roundtrip_leading_positive() {
    let original: &[u8] = &[0x4E, 0xF5, 0xF5, 0xF5, 0xF5, 0xF5];
    let decimal =
        decode_zoned_decimal_sign_separate(original, 5, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(decimal.to_string(), "55555");

    let mut encoded = vec![0u8; 6];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        5,
        0,
        &leading(),
        Codepage::CP037,
        &mut encoded,
    )
    .unwrap();
    assert_eq!(original, encoded.as_slice());
}

#[test]
fn test_deep_sign_separate_roundtrip_trailing_negative_scale() {
    // 7 digits, scale=2 → value = -01234.56
    // Digits: 0xF0 0xF1 0xF2 0xF3 0xF4 0xF5 0xF6, sign '-' = 0x60
    let original: &[u8] = &[0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0x60];
    let decimal =
        decode_zoned_decimal_sign_separate(original, 7, 2, &trailing(), Codepage::CP037).unwrap();
    assert_eq!(decimal.to_string(), "-1234.56");

    let mut encoded = vec![0u8; 8];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        7,
        2,
        &trailing(),
        Codepage::CP037,
        &mut encoded,
    )
    .unwrap();
    assert_eq!(original, encoded.as_slice());
}

#[test]
fn test_deep_sign_separate_roundtrip_fullstack() {
    let copybook = "01 VAL PIC S9(7) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "1234567" });
    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("1234567"));
}

// =============================================================================
// 10. SIGN SEPARATE with CP037 codepage
// =============================================================================

#[test]
fn test_deep_sign_separate_cp037_leading_roundtrip() {
    let copybook = "01 VAL PIC S9(3) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "-123" });
    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    // CP037: '-' = 0x60, digits 0xF1 0xF2 0xF3
    assert_eq!(encoded, vec![0x60, 0xF1, 0xF2, 0xF3]);

    let decoded = decode_record(&schema, &encoded, &decode_opts(Codepage::CP037)).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("-123"));
}

// =============================================================================
// 11. SIGN SEPARATE with CP1047 codepage
// =============================================================================

#[test]
fn test_deep_sign_separate_cp1047_leading_positive() {
    // CP1047 shares digit encoding 0xF0-0xF9 and '+' = 0x4E, '-' = 0x60
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3];
    let result =
        decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::CP1047).unwrap();
    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_deep_sign_separate_cp1047_trailing_negative() {
    let data: &[u8] = &[0xF4, 0xF5, 0xF6, 0x60];
    let result =
        decode_zoned_decimal_sign_separate(data, 3, 0, &trailing(), Codepage::CP1047).unwrap();
    assert_eq!(result.to_string(), "-456");
    assert!(result.is_negative());
}

#[test]
fn test_deep_sign_separate_cp1047_roundtrip_fullstack() {
    let copybook = "01 VAL PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "-99999" });
    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP1047)).expect("encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts(Codepage::CP1047)).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("-99999"));
}

// =============================================================================
// 12. SIGN SEPARATE zero value
// =============================================================================

#[test]
fn test_deep_sign_separate_zero_leading() {
    let data: &[u8] = &[0x4E, 0xF0, 0xF0, 0xF0];
    let result =
        decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "0");
    assert!(!result.is_negative());
}

#[test]
fn test_deep_sign_separate_negative_zero_normalizes() {
    // -0 should normalize to 0
    let data: &[u8] = &[0x60, 0xF0, 0xF0, 0xF0];
    let result =
        decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "0");
    assert!(!result.is_negative(), "negative zero should normalize");
}

#[test]
fn test_deep_sign_separate_zero_trailing() {
    let data: &[u8] = &[0xF0, 0xF0, 0xF0, 0x4E];
    let result =
        decode_zoned_decimal_sign_separate(data, 3, 0, &trailing(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "0");
}

#[test]
fn test_deep_sign_separate_zero_encode_roundtrip() {
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("0", 3, 0, &leading(), Codepage::CP037, &mut buf).unwrap();
    assert_eq!(buf, [0x4E, 0xF0, 0xF0, 0xF0]);

    let decoded =
        decode_zoned_decimal_sign_separate(&buf, 3, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(decoded.to_string(), "0");
}

// =============================================================================
// 13. SIGN SEPARATE max value
// =============================================================================

#[test]
fn test_deep_sign_separate_max_9_digits() {
    let data: &[u8] = &[0x4E, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9];
    let result =
        decode_zoned_decimal_sign_separate(data, 9, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "999999999");
}

#[test]
fn test_deep_sign_separate_max_negative_9_digits() {
    let data: &[u8] = &[0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0x60];
    let result =
        decode_zoned_decimal_sign_separate(data, 9, 0, &trailing(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "-999999999");
}

// =============================================================================
// 14. SIGN SEPARATE field size = digits + 1
// =============================================================================

#[test]
fn test_deep_sign_separate_field_size_single_digit() {
    // PIC S9 SIGN LEADING SEPARATE → 2 bytes (1 sign + 1 digit)
    let data: &[u8] = &[0x4E, 0xF7];
    let result =
        decode_zoned_decimal_sign_separate(data, 1, 0, &leading(), Codepage::CP037).unwrap();
    assert_eq!(result.to_string(), "7");
}

#[test]
fn test_deep_sign_separate_field_size_equals_digits_plus_one() {
    // PIC S9(4) SIGN TRAILING SEPARATE → 5 bytes (4 digits + 1 sign)
    let copybook = "01 VAL PIC S9(4) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "9999" });
    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(encoded.len(), 5, "field size should be digits(4) + 1(sign)");
    assert_eq!(encoded, vec![0xF9, 0xF9, 0xF9, 0xF9, 0x4E]);
}

#[test]
fn test_deep_sign_separate_field_size_with_scale() {
    // PIC S9(3)V99 SIGN LEADING SEPARATE → 6 bytes (1 sign + 5 digits)
    let copybook = "01 VAL PIC S9(3)V99 SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "123.45" });
    let encoded = encode_record(&schema, &json, &encode_opts(Codepage::CP037)).expect("encode");
    assert_eq!(
        encoded.len(),
        6,
        "field size should be digits(3+2) + 1(sign)"
    );
}

// =============================================================================
// 15. Invalid sign character handling
// =============================================================================

#[test]
fn test_deep_sign_separate_invalid_sign_byte_ebcdic() {
    // 0xFF is not a valid EBCDIC sign byte
    let data: &[u8] = &[0xFF, 0xF1, 0xF2, 0xF3];
    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::CP037);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
}

#[test]
fn test_deep_sign_separate_invalid_sign_byte_ascii() {
    // 0x41 ('A') is not a valid ASCII sign byte
    let data: &[u8] = &[0x41, 0x31, 0x32, 0x33];
    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::ASCII);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
}

#[test]
fn test_deep_sign_separate_invalid_sign_trailing_ebcdic() {
    let data: &[u8] = &[0xF1, 0xF2, 0xF3, 0xAB];
    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &trailing(), Codepage::CP037);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
}

// =============================================================================
// 16. Missing sign byte handling (data too short)
// =============================================================================

#[test]
fn test_deep_sign_separate_missing_sign_byte_leading() {
    // Need 4 bytes (3 digits + 1 sign) but only provide 3
    let data: &[u8] = &[0xF1, 0xF2, 0xF3];
    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::CP037);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT
    );
}

#[test]
fn test_deep_sign_separate_missing_sign_byte_trailing() {
    let data: &[u8] = &[0xF1, 0xF2];
    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &trailing(), Codepage::CP037);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKD301_RECORD_TOO_SHORT
    );
}

#[test]
fn test_deep_sign_separate_empty_data() {
    let data: &[u8] = &[];
    let result = decode_zoned_decimal_sign_separate(data, 1, 0, &leading(), Codepage::CP037);
    assert!(result.is_err());
}

#[test]
fn test_deep_sign_separate_encode_buffer_too_small() {
    let mut buf = vec![0u8; 3]; // Need 4 for 3 digits + 1 sign
    let result =
        encode_zoned_decimal_sign_separate("123", 3, 0, &leading(), Codepage::CP037, &mut buf);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE530_SIGN_SEPARATE_ENCODE_ERROR
    );
}

// =============================================================================
// Additional: ASCII codepage
// =============================================================================

#[test]
fn test_deep_sign_separate_ascii_leading_positive() {
    let data: &[u8] = b"+123";
    let result =
        decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::ASCII).unwrap();
    assert_eq!(result.to_string(), "123");
    assert!(!result.is_negative());
}

#[test]
fn test_deep_sign_separate_ascii_trailing_negative() {
    let data: &[u8] = b"456-";
    let result =
        decode_zoned_decimal_sign_separate(data, 3, 0, &trailing(), Codepage::ASCII).unwrap();
    assert_eq!(result.to_string(), "-456");
    assert!(result.is_negative());
}

#[test]
fn test_deep_sign_separate_ascii_roundtrip() {
    let original: &[u8] = b"-789";
    let decimal =
        decode_zoned_decimal_sign_separate(original, 3, 0, &leading(), Codepage::ASCII).unwrap();
    assert_eq!(decimal.to_string(), "-789");

    let mut encoded = vec![0u8; 4];
    encode_zoned_decimal_sign_separate(
        &decimal.to_string(),
        3,
        0,
        &leading(),
        Codepage::ASCII,
        &mut encoded,
    )
    .unwrap();
    assert_eq!(original, encoded.as_slice());
}

// =============================================================================
// Additional: Invalid digit bytes
// =============================================================================

#[test]
fn test_deep_sign_separate_invalid_digit_byte_ebcdic() {
    // 0xC1 is EBCDIC 'A', not a digit
    let data: &[u8] = &[0x4E, 0xC1, 0xF2, 0xF3];
    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::CP037);
    assert!(result.is_err());
}

#[test]
fn test_deep_sign_separate_invalid_digit_byte_ascii() {
    let data: &[u8] = b"+A23";
    let result = decode_zoned_decimal_sign_separate(data, 3, 0, &leading(), Codepage::ASCII);
    assert!(result.is_err());
}
