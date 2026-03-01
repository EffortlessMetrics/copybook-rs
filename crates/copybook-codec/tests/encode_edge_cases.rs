// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Comprehensive encode edge-case tests for the copybook-codec crate.
//!
//! Covers:
//! 1. PIC X encode: exact length, short input (pad), long input (truncate/reject), empty
//! 2. PIC 9 encode: zero, max digits, leading zeros, overflow
//! 3. COMP-3 encode: positive, negative, zero, max/min value, decimal precision
//! 4. COMP encode: all sizes (halfword, fullword, doubleword), boundary values
//! 5. SIGN SEPARATE: leading/trailing, positive/negative, with decimals
//! 6. Overpunch: encode positive/negative, boundary digits
//! 7. Mixed record encode: multi-field records with all types
//! 8. Error cases: type mismatch, out of range, null values
//! 9. Codepage interaction: encode with different codepages

use copybook_codec::numeric::{
    encode_alphanumeric, encode_binary_int, encode_packed_decimal, encode_zoned_decimal,
    encode_zoned_decimal_sign_separate,
};
use copybook_codec::zoned_overpunch::{ZeroSignPolicy, encode_overpunch_byte};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::{SignPlacement, SignSeparateInfo, parse_copybook};
use serde_json::json;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true)
}

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn ebcdic_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true)
}

fn ebcdic_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

// ===========================================================================
// 1. PIC X encode: exact length, short input, long input, empty string
// ===========================================================================

#[test]
fn test_pic_x_exact_length() {
    let encoded = encode_alphanumeric("HELLO", 5, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"HELLO");
}

#[test]
fn test_pic_x_short_input_padded_with_spaces() {
    let encoded = encode_alphanumeric("AB", 8, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 8);
    assert_eq!(&encoded[..2], b"AB");
    assert!(encoded[2..].iter().all(|&b| b == b' '));
}

#[test]
fn test_pic_x_long_input_rejected() {
    let result = encode_alphanumeric("TOOLONG", 3, Codepage::ASCII);
    assert!(result.is_err(), "should reject text exceeding field length");
}

#[test]
fn test_pic_x_empty_string_all_spaces() {
    let encoded = encode_alphanumeric("", 6, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 6);
    assert!(encoded.iter().all(|&b| b == b' '));
}

#[test]
fn test_pic_x_single_char() {
    let encoded = encode_alphanumeric("Z", 1, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"Z");
}

#[test]
fn test_pic_x_ebcdic_padding_uses_0x40() {
    let encoded = encode_alphanumeric("A", 4, Codepage::CP037).unwrap();
    assert_eq!(encoded.len(), 4);
    // EBCDIC space is 0x40
    assert!(encoded[1..].iter().all(|&b| b == 0x40));
}

// ===========================================================================
// 1b. PIC X encode via record-level API
// ===========================================================================

#[test]
fn test_pic_x_record_exact_length() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME PIC X(5).
    ",
    )
    .unwrap();
    let encoded = encode_record(&schema, &json!({"NAME": "HELLO"}), &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, b"HELLO");
}

#[test]
fn test_pic_x_record_short_input_padded() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME PIC X(10).
    ",
    )
    .unwrap();
    let encoded = encode_record(&schema, &json!({"NAME": "AB"}), &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 10);
    assert_eq!(&encoded[..2], b"AB");
    assert!(encoded[2..].iter().all(|&b| b == b' '));
}

#[test]
fn test_pic_x_record_strict_rejects_overflow() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME PIC X(3).
    ",
    )
    .unwrap();
    let result = encode_record(&schema, &json!({"NAME": "ABCDEFGH"}), &ascii_encode_opts());
    assert!(result.is_err());
}

#[test]
fn test_pic_x_record_empty_string() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME PIC X(4).
    ",
    )
    .unwrap();
    let encoded = encode_record(&schema, &json!({"NAME": ""}), &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 4);
    assert!(encoded.iter().all(|&b| b == b' '));
}

// ===========================================================================
// 2. PIC 9 encode: zero, max digits, leading zeros, overflow
// ===========================================================================

#[test]
fn test_pic9_zero_value() {
    let encoded = encode_zoned_decimal("0", 5, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 5);
    assert!(encoded.iter().all(|&b| b == b'0'));
}

#[test]
fn test_pic9_max_digits_5() {
    let encoded = encode_zoned_decimal("99999", 5, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"99999");
}

#[test]
fn test_pic9_leading_zeros_preserved() {
    let encoded = encode_zoned_decimal("7", 5, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"00007");
}

#[test]
fn test_pic9_overflow_rejected() {
    let result = encode_zoned_decimal("100000", 5, 0, false, Codepage::ASCII);
    assert!(
        result.is_err(),
        "6-digit value should overflow 5-digit field"
    );
}

#[test]
fn test_pic9_signed_positive() {
    let encoded = encode_zoned_decimal("123", 3, 0, true, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 3);
    // Last byte should be overpunch for positive 3
    let (digit, is_neg) =
        copybook_codec::zoned_overpunch::decode_overpunch_byte(encoded[2], Codepage::ASCII)
            .unwrap();
    assert_eq!(digit, 3);
    assert!(!is_neg);
}

#[test]
fn test_pic9_signed_negative() {
    let encoded = encode_zoned_decimal("-456", 3, 0, true, Codepage::ASCII).unwrap();
    let (digit, is_neg) =
        copybook_codec::zoned_overpunch::decode_overpunch_byte(encoded[2], Codepage::ASCII)
            .unwrap();
    assert_eq!(digit, 6);
    assert!(is_neg);
}

#[test]
fn test_pic9_with_decimal_scale() {
    let encoded = encode_zoned_decimal("123.45", 5, 2, false, Codepage::ASCII).unwrap();
    // 12345 stored as zoned (no decimal point in data)
    assert_eq!(encoded.len(), 5);
}

// ===========================================================================
// 3. COMP-3 encode: positive, negative, zero, max/min, decimal precision
// ===========================================================================

#[test]
fn test_comp3_encode_positive() {
    let result = encode_packed_decimal("123", 3, 0, true).unwrap();
    assert_eq!(result, vec![0x12, 0x3C]);
}

#[test]
fn test_comp3_encode_negative() {
    let result = encode_packed_decimal("-456", 3, 0, true).unwrap();
    assert_eq!(result, vec![0x45, 0x6D]);
}

#[test]
fn test_comp3_encode_zero_signed() {
    let result = encode_packed_decimal("0", 1, 0, true).unwrap();
    assert_eq!(result, vec![0x0C]);
}

#[test]
fn test_comp3_encode_zero_unsigned() {
    let result = encode_packed_decimal("0", 1, 0, false).unwrap();
    assert_eq!(result, vec![0x0F]);
}

#[test]
fn test_comp3_encode_max_9_digits() {
    let result = encode_packed_decimal("999999999", 9, 0, true).unwrap();
    // 9 digits = 5 bytes: 0x99 0x99 0x99 0x99 0x9C
    assert_eq!(result, vec![0x99, 0x99, 0x99, 0x99, 0x9C]);
}

#[test]
fn test_comp3_encode_min_negative_9_digits() {
    let result = encode_packed_decimal("-999999999", 9, 0, true).unwrap();
    assert_eq!(result, vec![0x99, 0x99, 0x99, 0x99, 0x9D]);
}

#[test]
fn test_comp3_encode_decimal_precision() {
    let result = encode_packed_decimal("12.34", 4, 2, true).unwrap();
    // 1234 stored as COMP-3: 0x01 0x23 0x4C
    assert_eq!(result, vec![0x01, 0x23, 0x4C]);
}

#[test]
fn test_comp3_encode_negative_decimal() {
    let result = encode_packed_decimal("-99.99", 4, 2, true).unwrap();
    assert_eq!(result, vec![0x09, 0x99, 0x9D]);
}

#[test]
fn test_comp3_encode_overflow_rejected() {
    let result = encode_packed_decimal("1000", 3, 0, true);
    assert!(result.is_err());
}

#[test]
fn test_comp3_encode_single_digit_positive() {
    let result = encode_packed_decimal("9", 1, 0, true).unwrap();
    assert_eq!(result, vec![0x9C]);
}

#[test]
fn test_comp3_encode_even_digit_count() {
    // 2 digits -> 2 bytes (pad nibble + 2 digit nibbles + sign nibble)
    let result = encode_packed_decimal("42", 2, 0, true).unwrap();
    assert_eq!(result, vec![0x04, 0x2C]);
}

// ===========================================================================
// 4. COMP encode: halfword (16), fullword (32), doubleword (64), boundaries
// ===========================================================================

#[test]
fn test_comp_halfword_positive() {
    let result = encode_binary_int(1234, 16, true).unwrap();
    assert_eq!(result, 1234_i16.to_be_bytes().to_vec());
}

#[test]
fn test_comp_halfword_signed_min() {
    let result = encode_binary_int(-32768, 16, true).unwrap();
    assert_eq!(result, (-32768_i16).to_be_bytes().to_vec());
}

#[test]
fn test_comp_halfword_signed_max() {
    let result = encode_binary_int(32767, 16, true).unwrap();
    assert_eq!(result, 32767_i16.to_be_bytes().to_vec());
}

#[test]
fn test_comp_halfword_unsigned_max() {
    let result = encode_binary_int(65535, 16, false).unwrap();
    assert_eq!(result, 65535_u16.to_be_bytes().to_vec());
}

#[test]
fn test_comp_halfword_signed_overflow() {
    assert!(encode_binary_int(32768, 16, true).is_err());
}

#[test]
fn test_comp_halfword_signed_underflow() {
    assert!(encode_binary_int(-32769, 16, true).is_err());
}

#[test]
fn test_comp_halfword_unsigned_negative_rejected() {
    assert!(encode_binary_int(-1, 16, false).is_err());
}

#[test]
fn test_comp_fullword_signed_min() {
    let result = encode_binary_int(i64::from(i32::MIN), 32, true).unwrap();
    assert_eq!(result, i32::MIN.to_be_bytes().to_vec());
}

#[test]
fn test_comp_fullword_signed_max() {
    let result = encode_binary_int(i64::from(i32::MAX), 32, true).unwrap();
    assert_eq!(result, i32::MAX.to_be_bytes().to_vec());
}

#[test]
fn test_comp_fullword_overflow() {
    assert!(encode_binary_int(i64::from(i32::MAX) + 1, 32, true).is_err());
}

#[test]
fn test_comp_fullword_unsigned_max() {
    let result = encode_binary_int(i64::from(u32::MAX), 32, false).unwrap();
    assert_eq!(result, u32::MAX.to_be_bytes().to_vec());
}

#[test]
fn test_comp_doubleword_signed_min() {
    let result = encode_binary_int(i64::MIN, 64, true).unwrap();
    assert_eq!(result, i64::MIN.to_be_bytes().to_vec());
}

#[test]
fn test_comp_doubleword_signed_max() {
    let result = encode_binary_int(i64::MAX, 64, true).unwrap();
    assert_eq!(result, i64::MAX.to_be_bytes().to_vec());
}

#[test]
fn test_comp_doubleword_zero() {
    let result = encode_binary_int(0, 64, true).unwrap();
    assert_eq!(result, vec![0u8; 8]);
}

#[test]
fn test_comp_unsupported_width() {
    assert!(encode_binary_int(0, 24, true).is_err());
    assert!(encode_binary_int(0, 8, true).is_err());
}

// ===========================================================================
// 5. SIGN SEPARATE: leading/trailing, positive/negative, with decimals
// ===========================================================================

#[test]
fn test_sign_separate_leading_positive_ebcdic() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("123", 3, 0, &sign_info, Codepage::CP037, &mut buf).unwrap();
    assert_eq!(buf, vec![0x4E, 0xF1, 0xF2, 0xF3]);
}

#[test]
fn test_sign_separate_leading_negative_ebcdic() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("-789", 3, 0, &sign_info, Codepage::CP037, &mut buf)
        .unwrap();
    assert_eq!(buf, vec![0x60, 0xF7, 0xF8, 0xF9]);
}

#[test]
fn test_sign_separate_trailing_positive_ebcdic() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("456", 3, 0, &sign_info, Codepage::CP037, &mut buf).unwrap();
    assert_eq!(buf, vec![0xF4, 0xF5, 0xF6, 0x4E]);
}

#[test]
fn test_sign_separate_trailing_negative_ebcdic() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("-100", 3, 0, &sign_info, Codepage::CP037, &mut buf)
        .unwrap();
    assert_eq!(buf, vec![0xF1, 0xF0, 0xF0, 0x60]);
}

#[test]
fn test_sign_separate_with_decimal_scale() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buf = vec![0u8; 6];
    encode_zoned_decimal_sign_separate("123.45", 5, 2, &sign_info, Codepage::CP037, &mut buf)
        .unwrap();
    assert_eq!(buf, vec![0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5]);
}

#[test]
fn test_sign_separate_zero_value() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("0", 3, 0, &sign_info, Codepage::CP037, &mut buf).unwrap();
    assert_eq!(buf, vec![0x4E, 0xF0, 0xF0, 0xF0]);
}

#[test]
fn test_sign_separate_ascii_leading_positive() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("999", 3, 0, &sign_info, Codepage::ASCII, &mut buf).unwrap();
    assert_eq!(buf, b"+999".to_vec());
}

#[test]
fn test_sign_separate_ascii_trailing_negative() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let mut buf = vec![0u8; 4];
    encode_zoned_decimal_sign_separate("-001", 3, 0, &sign_info, Codepage::ASCII, &mut buf)
        .unwrap();
    assert_eq!(buf, b"001-".to_vec());
}

#[test]
fn test_sign_separate_buffer_too_small() {
    let sign_info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let mut buf = vec![0u8; 2]; // Too small for 3 digits + sign
    let result =
        encode_zoned_decimal_sign_separate("123", 3, 0, &sign_info, Codepage::CP037, &mut buf);
    assert!(result.is_err());
}

// ===========================================================================
// 6. Overpunch: encode positive/negative, boundary digits
// ===========================================================================

#[test]
fn test_overpunch_ascii_positive_digits() {
    // +0={, +1=A, ..., +9=I
    let expected = [b'{', b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I'];
    for digit in 0..=9 {
        let result =
            encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(
            result, expected[digit as usize],
            "positive digit {digit} mismatch"
        );
    }
}

#[test]
fn test_overpunch_ascii_negative_digits() {
    // -0=}, -1=J, ..., -9=R
    let expected = [b'}', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R'];
    for digit in 0..=9 {
        let result =
            encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(
            result, expected[digit as usize],
            "negative digit {digit} mismatch"
        );
    }
}

#[test]
fn test_overpunch_ebcdic_positive_zone_0x_c() {
    let result =
        encode_overpunch_byte(5, false, Codepage::CP037, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(result, 0xC5, "EBCDIC positive 5 should have 0xC zone");
}

#[test]
fn test_overpunch_ebcdic_negative_zone_0x_d() {
    let result = encode_overpunch_byte(3, true, Codepage::CP037, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(result, 0xD3, "EBCDIC negative 3 should have 0xD zone");
}

#[test]
fn test_overpunch_ebcdic_preferred_zero_policy() {
    let result =
        encode_overpunch_byte(0, false, Codepage::CP037, ZeroSignPolicy::Preferred).unwrap();
    assert_eq!(result, 0xF0, "Preferred zero policy should use 0xF zone");
}

#[test]
fn test_overpunch_invalid_digit_rejected() {
    let result = encode_overpunch_byte(10, false, Codepage::ASCII, ZeroSignPolicy::Positive);
    assert!(result.is_err(), "digit 10 should be rejected");
}

#[test]
fn test_overpunch_boundary_digit_0() {
    let pos = encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(pos, b'{');
    let neg = encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(neg, b'}');
}

#[test]
fn test_overpunch_boundary_digit_9() {
    let pos = encode_overpunch_byte(9, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(pos, b'I');
    let neg = encode_overpunch_byte(9, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(neg, b'R');
}

// ===========================================================================
// 7. Mixed record encode: multi-field records with all types
// ===========================================================================

#[test]
fn test_mixed_record_alpha_and_numeric() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME    PIC X(10).
          05 AMOUNT  PIC 9(5).
    ",
    )
    .unwrap();
    let opts = ascii_encode_opts();
    let data = json!({"NAME": "ALICE", "AMOUNT": "42"});
    let encoded = encode_record(&schema, &data, &opts).unwrap();
    assert_eq!(encoded.len(), 15);
    assert_eq!(&encoded[..5], b"ALICE");
}

#[test]
fn test_mixed_record_alpha_and_comp3_roundtrip() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME    PIC X(8).
          05 BAL     PIC S9(5)V99 COMP-3.
    ",
    )
    .unwrap();
    let enc = ascii_encode_opts();
    let dec = ascii_decode_opts();
    let data = json!({"NAME": "BOB", "BAL": "123.45"});
    let encoded = encode_record(&schema, &data, &enc).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    assert_eq!(decoded["BAL"], "123.45");
    let name = decoded["NAME"].as_str().unwrap();
    assert!(name.starts_with("BOB"));
}

#[test]
fn test_mixed_record_comp_and_pic_x() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 ID      PIC S9(4) COMP.
          05 DESC    PIC X(20).
    ",
    )
    .unwrap();
    let enc = ascii_encode_opts();
    let dec = ascii_decode_opts();
    let data = json!({"ID": "1000", "DESC": "WIDGET"});
    let encoded = encode_record(&schema, &data, &enc).unwrap();
    // COMP S9(4) is halfword (2 bytes) + 20 bytes PIC X
    assert_eq!(encoded.len(), 22);
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    let desc = decoded["DESC"].as_str().unwrap();
    assert!(desc.starts_with("WIDGET"));
}

#[test]
fn test_mixed_record_three_field_roundtrip() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 CODE    PIC X(3).
          05 QTY     PIC S9(7) COMP-3.
          05 PRICE   PIC 9(5)V99.
    ",
    )
    .unwrap();
    let enc = ascii_encode_opts();
    let dec = ascii_decode_opts();
    let data = json!({"CODE": "ABC", "QTY": "500", "PRICE": "199.99"});
    let encoded = encode_record(&schema, &data, &enc).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    assert_eq!(decoded["QTY"], "500");
    assert_eq!(decoded["PRICE"], "199.99");
}

// ===========================================================================
// 8. Error cases: type mismatch, out of range, null values
// ===========================================================================

#[test]
fn test_encode_record_non_object_rejected() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 F PIC X(5).
    ",
    )
    .unwrap();
    let result = encode_record(&schema, &json!("not an object"), &ascii_encode_opts());
    assert!(result.is_err());
    let msg = format!("{}", result.unwrap_err());
    assert!(msg.contains("CBKE501"));
}

#[test]
fn test_encode_record_null_json_rejected() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 F PIC X(5).
    ",
    )
    .unwrap();
    let result = encode_record(&schema, &json!(null), &ascii_encode_opts());
    assert!(result.is_err());
}

#[test]
fn test_encode_comp3_non_numeric_string_rejected() {
    let result = encode_packed_decimal("abc", 3, 0, true);
    assert!(result.is_err());
}

#[test]
fn test_encode_zoned_non_numeric_rejected() {
    let result = encode_zoned_decimal("hello", 5, 0, false, Codepage::ASCII);
    assert!(result.is_err());
}

#[test]
fn test_encode_binary_int_overflow_32bit() {
    let result = encode_binary_int(i64::from(i32::MAX) + 1, 32, true);
    assert!(result.is_err());
}

#[test]
fn test_encode_array_as_record_rejected() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 F PIC X(5).
    ",
    )
    .unwrap();
    let result = encode_record(&schema, &json!([1, 2, 3]), &ascii_encode_opts());
    assert!(result.is_err());
}

// ===========================================================================
// 9. Codepage interaction: encode with different codepages
// ===========================================================================

#[test]
fn test_pic_x_ebcdic_cp037_roundtrip() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME PIC X(6).
    ",
    )
    .unwrap();
    let enc = ebcdic_encode_opts();
    let dec = ebcdic_decode_opts();
    let encoded = encode_record(&schema, &json!({"NAME": "RUST"}), &enc).unwrap();
    assert_eq!(encoded.len(), 6);
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    let name = decoded["NAME"].as_str().unwrap();
    assert!(name.starts_with("RUST"));
}

#[test]
fn test_pic_x_ebcdic_cp500_roundtrip() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME PIC X(5).
    ",
    )
    .unwrap();
    let enc = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP500)
        .with_strict_mode(true);
    let dec = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP500)
        .with_json_number_mode(JsonNumberMode::Lossless);
    let encoded = encode_record(&schema, &json!({"NAME": "COBOL"}), &enc).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    let name = decoded["NAME"].as_str().unwrap();
    assert!(name.starts_with("COBOL"));
}

#[test]
fn test_pic_x_ebcdic_cp1047_roundtrip() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 NAME PIC X(4).
    ",
    )
    .unwrap();
    let enc = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP1047)
        .with_strict_mode(true);
    let dec = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP1047)
        .with_json_number_mode(JsonNumberMode::Lossless);
    let encoded = encode_record(&schema, &json!({"NAME": "TEST"}), &enc).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    let name = decoded["NAME"].as_str().unwrap();
    assert!(name.starts_with("TEST"));
}

#[test]
fn test_zoned_decimal_ebcdic_encodes_with_f_zone() {
    // EBCDIC PIC 9 digits should have 0xF zone for non-sign bytes
    let encoded = encode_zoned_decimal("123", 3, 0, false, Codepage::CP037).unwrap();
    assert_eq!(encoded, vec![0xF1, 0xF2, 0xF3]);
}

#[test]
fn test_comp3_roundtrip_via_record_ebcdic() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 AMT PIC S9(5)V99 COMP-3.
    ",
    )
    .unwrap();
    let enc = ebcdic_encode_opts();
    let dec = ebcdic_decode_opts();
    let data = json!({"AMT": "99999.99"});
    let encoded = encode_record(&schema, &data, &enc).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec).unwrap();
    assert_eq!(decoded["AMT"], "99999.99");
}

// ===========================================================================
// Extra edge cases: fields envelope, nested groups
// ===========================================================================

#[test]
fn test_encode_with_fields_envelope() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 F PIC X(3).
    ",
    )
    .unwrap();
    // encode_record should accept both flat JSON and JSON with "fields" key
    let data = json!({"fields": {"F": "YES"}});
    let encoded = encode_record(&schema, &data, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded, b"YES");
}

#[test]
fn test_encode_nested_group_record() {
    let schema = parse_copybook(
        r"
       01 REC.
          05 GRP.
             10 A PIC X(3).
             10 B PIC 9(2).
    ",
    )
    .unwrap();
    let data = json!({"GRP": {"A": "XY", "B": "5"}});
    let encoded = encode_record(&schema, &data, &ascii_encode_opts()).unwrap();
    assert_eq!(encoded.len(), 5);
}
