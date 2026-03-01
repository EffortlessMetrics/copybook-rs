// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive decode edge-case tests for the copybook-codec crate.
//!
//! Covers:
//! 1. Alphanumeric (PIC X) decode edge cases
//! 2. Numeric display (PIC 9) decode edge cases
//! 3. COMP-3 (packed decimal) decode edge cases
//! 4. COMP (binary integer) decode edge cases
//! 5. SIGN SEPARATE decode edge cases
//! 6. JSON number mode (Lossless vs Native) comparisons

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::{
    decode_binary_int, decode_packed_decimal, decode_zoned_decimal,
    decode_zoned_decimal_sign_separate,
};
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record};
use copybook_core::{SignPlacement, SignSeparateInfo, parse_copybook};

// =============================================================================
// Helpers
// =============================================================================

fn lossless_ascii() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn lossless_ebcdic() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn native_ebcdic() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Native)
}

fn native_ascii() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Native)
}

// =============================================================================
// 1. Alphanumeric (PIC X) decode edge cases
// =============================================================================

#[test]
fn decode_pic_x_trailing_spaces_preserved() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(10).").unwrap();
    // ASCII "HELLO     " (5 chars + 5 spaces) — codec preserves trailing spaces
    let data = b"HELLO     ";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    assert_eq!(result["FLD"], "HELLO     ");
}

#[test]
fn decode_pic_x_all_spaces_preserved() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(8).").unwrap();
    let data = b"        ";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    assert_eq!(result["FLD"], "        ");
}

#[test]
fn decode_pic_x_full_field_no_trimming() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(5).").unwrap();
    let data = b"ABCDE";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    assert_eq!(result["FLD"], "ABCDE");
}

#[test]
fn decode_pic_x_single_character() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X.").unwrap();
    let data = b"Z";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    assert_eq!(result["FLD"], "Z");
}

#[test]
fn decode_pic_x_single_space_preserved() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X.").unwrap();
    let data = b" ";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    assert_eq!(result["FLD"], " ");
}

#[test]
fn decode_pic_x_ebcdic_basic() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(5).").unwrap();
    // EBCDIC "HELLO": H=0xC8, E=0xC5, L=0xD3, L=0xD3, O=0xD6
    let data: [u8; 5] = [0xC8, 0xC5, 0xD3, 0xD3, 0xD6];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "HELLO");
}

#[test]
fn decode_pic_x_ebcdic_trailing_spaces_preserved() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(8).").unwrap();
    // EBCDIC "AB      ": A=0xC1, B=0xC2, space=0x40
    let data: [u8; 8] = [0xC1, 0xC2, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "AB      ");
}

#[test]
fn decode_pic_x_low_values_handling() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(4).").unwrap();
    // Low-values (0x00) — should be decoded (may be replaced/trimmed)
    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &lossless_ascii());
    // Should succeed — low-values are valid data
    assert!(result.is_ok());
}

#[test]
fn decode_pic_x_high_values_handling() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(4).").unwrap();
    // High-values (0xFF) in ASCII mode
    let data: [u8; 4] = [0xFF, 0xFF, 0xFF, 0xFF];
    let result = decode_record(&schema, &data, &lossless_ascii());
    // Should produce a result (may be replacement chars or error depending on policy)
    // The key is it doesn't panic
    let _ = result;
}

#[test]
fn decode_pic_x_mixed_content_with_trailing_space() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(12).").unwrap();
    let data = b"Hello World ";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    // Codec preserves all characters including trailing spaces
    assert_eq!(result["FLD"], "Hello World ");
}

#[test]
fn decode_pic_x_internal_spaces_preserved() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(10).").unwrap();
    let data = b"A  B  C   ";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    // Codec preserves full field content including trailing spaces
    assert_eq!(result["FLD"], "A  B  C   ");
}

// =============================================================================
// 2. Numeric display (PIC 9) decode edge cases
// =============================================================================

#[test]
fn decode_pic9_all_zeros() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(5).").unwrap();
    // EBCDIC "00000": 0xF0 * 5
    let data: [u8; 5] = [0xF0, 0xF0, 0xF0, 0xF0, 0xF0];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "0");
}

#[test]
fn decode_pic9_single_digit() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9.").unwrap();
    let data: [u8; 1] = [0xF7]; // EBCDIC '7'
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "7");
}

#[test]
fn decode_pic9_max_digits_18() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(18).").unwrap();
    // EBCDIC "999999999999999999"
    let data: [u8; 18] = [0xF9; 18];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "999999999999999999");
}

#[test]
fn decode_pic9_with_implied_decimal() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(5)V99.").unwrap();
    // EBCDIC "0012345" → 123.45
    let data: [u8; 7] = [0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "123.45");
}

#[test]
fn decode_pic9_implied_decimal_all_zeros() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(3)V99.").unwrap();
    let data: [u8; 5] = [0xF0, 0xF0, 0xF0, 0xF0, 0xF0];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "0.00");
}

#[test]
fn decode_pic9_signed_overpunch_positive_zero() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9.").unwrap();
    // EBCDIC positive overpunch for 0: 0xC0
    let data: [u8; 1] = [0xC0];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "0");
}

#[test]
fn decode_pic9_signed_overpunch_negative() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(3).").unwrap();
    // EBCDIC: 0xF1 0xF2 + last byte negative overpunch 0xD3 → -123
    let data: [u8; 3] = [0xF1, 0xF2, 0xD3];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "-123");
}

#[test]
fn decode_pic9_signed_with_implied_decimal() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(3)V99.").unwrap();
    // EBCDIC: 0xF1 0xF2 0xF3 0xF4 + last byte positive overpunch 0xC5 → 123.45
    let data: [u8; 5] = [0xF1, 0xF2, 0xF3, 0xF4, 0xC5];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "123.45");
}

#[test]
fn decode_pic9_ascii_unsigned_preserves_leading_zeros() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(4).").unwrap();
    let data = b"0042";
    let result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    // Unsigned display numerics preserve leading zeros in lossless mode
    assert_eq!(result["FLD"], "0042");
}

#[test]
fn decode_zoned_decimal_unit_basic() {
    // Unit-level: 3-digit unsigned zoned decimal in EBCDIC
    let data: [u8; 3] = [0xF1, 0xF2, 0xF3]; // "123"
    let result = decode_zoned_decimal(&data, 3, 0, false, Codepage::CP037, false).unwrap();
    assert_eq!(result.to_string(), "123");
}

#[test]
fn decode_zoned_decimal_unit_with_scale() {
    // Unit-level: 5-digit with scale 2
    let data: [u8; 5] = [0xF0, 0xF1, 0xF2, 0xF3, 0xF4]; // "01234" → 012.34
    let result = decode_zoned_decimal(&data, 5, 2, false, Codepage::CP037, false).unwrap();
    assert_eq!(result.to_string(), "12.34");
}

// =============================================================================
// 3. COMP-3 (packed decimal) decode edge cases
// =============================================================================

#[test]
fn decode_comp3_zero_positive_sign_c() {
    let d = decode_packed_decimal(&[0x0C], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn decode_comp3_zero_negative_sign_d() {
    // Negative zero normalizes to "0"
    let d = decode_packed_decimal(&[0x0D], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn decode_comp3_zero_unsigned_sign_f() {
    let d = decode_packed_decimal(&[0x0F], 1, 0, false).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn decode_comp3_positive_sign_c() {
    let d = decode_packed_decimal(&[0x5C], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "5");
}

#[test]
fn decode_comp3_negative_sign_d() {
    let d = decode_packed_decimal(&[0x5D], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "-5");
}

#[test]
fn decode_comp3_unsigned_sign_f() {
    let d = decode_packed_decimal(&[0x5F], 1, 0, false).unwrap();
    assert_eq!(d.to_string(), "5");
}

#[test]
fn decode_comp3_max_18_digit_value() {
    // S9(18) COMP-3: 999999999999999999
    // 18 digits (even) + sign = 19 nibbles → 10 bytes (pad with leading 0 nibble)
    let data: [u8; 10] = [0x09, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C];
    let d = decode_packed_decimal(&data, 18, 0, true).unwrap();
    assert_eq!(d.to_string(), "999999999999999999");
}

#[test]
fn decode_comp3_max_18_digit_negative() {
    let data: [u8; 10] = [0x09, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9D];
    let d = decode_packed_decimal(&data, 18, 0, true).unwrap();
    assert_eq!(d.to_string(), "-999999999999999999");
}

#[test]
fn decode_comp3_invalid_sign_nibble() {
    // 0x02 is not a valid sign nibble (valid: A-F)
    let data: [u8; 1] = [0x52]; // digit=5, sign=2
    let result = decode_packed_decimal(&data, 1, 0, true);
    assert!(
        result.is_err(),
        "Invalid sign nibble 0x2 should be rejected"
    );
}

#[test]
fn decode_comp3_invalid_digit_nibble() {
    // High nibble 0xA is not a valid BCD digit (0-9 only)
    let data: [u8; 1] = [0xAC]; // digit=A (invalid), sign=C
    let result = decode_packed_decimal(&data, 1, 0, true);
    assert!(
        result.is_err(),
        "Invalid digit nibble 0xA should be rejected"
    );
}

#[test]
fn decode_comp3_odd_digit_count_3() {
    // S9(3) → 3 digits + sign = 4 nibbles = 2 bytes
    // 123C → [0x12, 0x3C]
    let d = decode_packed_decimal(&[0x12, 0x3C], 3, 0, true).unwrap();
    assert_eq!(d.to_string(), "123");
}

#[test]
fn decode_comp3_even_digit_count_4() {
    // S9(4) → 4 digits + sign = 5 nibbles → 3 bytes (pad nibble + 4 digits + sign)
    // 01234C → [0x01, 0x23, 0x4C]
    let d = decode_packed_decimal(&[0x01, 0x23, 0x4C], 4, 0, true).unwrap();
    assert_eq!(d.to_string(), "1234");
}

#[test]
fn decode_comp3_with_decimal_scale() {
    // S9(5)V99: 7 digits (odd) + sign = 8 nibbles = 4 bytes
    // 12345.67 → packed: [0x12, 0x34, 0x56, 0x7C]
    let d = decode_packed_decimal(&[0x12, 0x34, 0x56, 0x7C], 7, 2, true).unwrap();
    assert_eq!(d.to_string(), "12345.67");
}

#[test]
fn decode_comp3_high_level_with_schema() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(5)V99 COMP-3.").unwrap();
    // S9(5)V99: 7 digits (odd) + sign = 8 nibbles = 4 bytes
    // nibbles: 0,1,2,3,4,5,6,C → value 0123456 with scale 2 = 1234.56
    let data: [u8; 4] = [0x01, 0x23, 0x45, 0x6C];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "1234.56");
}

// =============================================================================
// 4. COMP (binary integer) decode edge cases
// =============================================================================

#[test]
fn decode_comp_halfword_signed_positive() {
    // 2-byte signed, value 1234 → 0x04D2
    let val = decode_binary_int(&[0x04, 0xD2], 16, true).unwrap();
    assert_eq!(val, 1234);
}

#[test]
fn decode_comp_halfword_signed_negative() {
    // -1 in 2-byte big-endian signed → 0xFFFF
    let val = decode_binary_int(&[0xFF, 0xFF], 16, true).unwrap();
    assert_eq!(val, -1);
}

#[test]
fn decode_comp_halfword_signed_max() {
    // i16::MAX = 32767 → 0x7FFF
    let val = decode_binary_int(&[0x7F, 0xFF], 16, true).unwrap();
    assert_eq!(val, 32767);
}

#[test]
fn decode_comp_halfword_signed_min() {
    // i16::MIN = -32768 → 0x8000
    let val = decode_binary_int(&[0x80, 0x00], 16, true).unwrap();
    assert_eq!(val, -32768);
}

#[test]
fn decode_comp_halfword_unsigned_max() {
    // u16::MAX = 65535 → 0xFFFF (unsigned)
    let val = decode_binary_int(&[0xFF, 0xFF], 16, false).unwrap();
    assert_eq!(val, 65535);
}

#[test]
fn decode_comp_halfword_zero() {
    let val = decode_binary_int(&[0x00, 0x00], 16, true).unwrap();
    assert_eq!(val, 0);
}

#[test]
fn decode_comp_fullword_signed_positive() {
    // 123456789 → 0x075BCD15
    let val = decode_binary_int(&[0x07, 0x5B, 0xCD, 0x15], 32, true).unwrap();
    assert_eq!(val, 123_456_789);
}

#[test]
fn decode_comp_fullword_signed_max() {
    // i32::MAX = 2147483647 → 0x7FFFFFFF
    let val = decode_binary_int(&[0x7F, 0xFF, 0xFF, 0xFF], 32, true).unwrap();
    assert_eq!(val, 2_147_483_647);
}

#[test]
fn decode_comp_fullword_signed_min() {
    // i32::MIN = -2147483648 → 0x80000000
    let val = decode_binary_int(&[0x80, 0x00, 0x00, 0x00], 32, true).unwrap();
    assert_eq!(val, -2_147_483_648);
}

#[test]
fn decode_comp_fullword_unsigned_max() {
    // u32::MAX = 4294967295 → 0xFFFFFFFF
    let val = decode_binary_int(&[0xFF, 0xFF, 0xFF, 0xFF], 32, false).unwrap();
    assert_eq!(val, 4_294_967_295);
}

#[test]
fn decode_comp_doubleword_signed_positive() {
    // 1 → 0x0000000000000001
    let val =
        decode_binary_int(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], 64, true).unwrap();
    assert_eq!(val, 1);
}

#[test]
fn decode_comp_doubleword_signed_max() {
    // i64::MAX
    let val =
        decode_binary_int(&[0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF], 64, true).unwrap();
    assert_eq!(val, i64::MAX);
}

#[test]
fn decode_comp_doubleword_signed_min() {
    // i64::MIN
    let val =
        decode_binary_int(&[0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], 64, true).unwrap();
    assert_eq!(val, i64::MIN);
}

#[test]
fn decode_comp_big_endian_byte_order() {
    // Verify big-endian: 0x0100 as i16 = 256 (not 1)
    let val = decode_binary_int(&[0x01, 0x00], 16, true).unwrap();
    assert_eq!(val, 256);
}

#[test]
fn decode_comp_high_level_halfword() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(4) COMP.").unwrap();
    let data: [u8; 2] = [0x04, 0xD2]; // 1234
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "1234");
}

#[test]
fn decode_comp_high_level_fullword() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(9) COMP.").unwrap();
    let data: [u8; 4] = [0x80, 0x00, 0x00, 0x00]; // i32::MIN
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "-2147483648");
}

#[test]
fn decode_comp_high_level_doubleword() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(18) COMP.").unwrap();
    let data: [u8; 8] = [0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "9223372036854775807");
}

#[test]
fn decode_comp_high_level_unsigned() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(4) COMP.").unwrap();
    let data: [u8; 2] = [0xFF, 0xFF]; // 65535 unsigned
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "65535");
}

#[test]
fn decode_comp_wrong_data_length() {
    // 16-bit field expects 2 bytes, give 3
    let result = decode_binary_int(&[0x01, 0x02, 0x03], 16, true);
    assert!(result.is_err());
}

// =============================================================================
// 5. SIGN SEPARATE decode edge cases
// =============================================================================

#[test]
fn decode_sign_separate_leading_positive_ebcdic() {
    // EBCDIC '+' = 0x4E, "123" = 0xF1 0xF2 0xF3
    let data: [u8; 4] = [0x4E, 0xF1, 0xF2, 0xF3];
    let info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::CP037).unwrap();
    assert_eq!(d.to_string(), "123");
    assert!(!d.is_negative());
}

#[test]
fn decode_sign_separate_leading_negative_ebcdic() {
    // EBCDIC '-' = 0x60, "456" = 0xF4 0xF5 0xF6
    let data: [u8; 4] = [0x60, 0xF4, 0xF5, 0xF6];
    let info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::CP037).unwrap();
    assert_eq!(d.to_string(), "-456");
    assert!(d.is_negative());
}

#[test]
fn decode_sign_separate_trailing_positive_ebcdic() {
    // "789" + EBCDIC '+' = 0xF7 0xF8 0xF9 0x4E
    let data: [u8; 4] = [0xF7, 0xF8, 0xF9, 0x4E];
    let info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::CP037).unwrap();
    assert_eq!(d.to_string(), "789");
}

#[test]
fn decode_sign_separate_trailing_negative_ebcdic() {
    // "100" + EBCDIC '-' = 0xF1 0xF0 0xF0 0x60
    let data: [u8; 4] = [0xF1, 0xF0, 0xF0, 0x60];
    let info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::CP037).unwrap();
    assert_eq!(d.to_string(), "-100");
}

#[test]
fn decode_sign_separate_with_decimal_places() {
    // EBCDIC '+' + "12345" (scale=2 → 123.45)
    let data: [u8; 6] = [0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 5, 2, &info, Codepage::CP037).unwrap();
    assert_eq!(d.to_string(), "123.45");
}

#[test]
fn decode_sign_separate_trailing_with_decimal() {
    // "99999" + EBCDIC '-' (scale=3 → -99.999)
    let data: [u8; 6] = [0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0x60];
    let info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 5, 3, &info, Codepage::CP037).unwrap();
    assert_eq!(d.to_string(), "-99.999");
}

#[test]
fn decode_sign_separate_ascii_leading_positive() {
    // ASCII '+' = 0x2B, "456" = 0x34 0x35 0x36
    let data: [u8; 4] = [0x2B, 0x34, 0x35, 0x36];
    let info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::ASCII).unwrap();
    assert_eq!(d.to_string(), "456");
}

#[test]
fn decode_sign_separate_ascii_trailing_negative() {
    // "789" + ASCII '-' = 0x37 0x38 0x39 0x2D
    let data: [u8; 4] = [0x37, 0x38, 0x39, 0x2D];
    let info = SignSeparateInfo {
        placement: SignPlacement::Trailing,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::ASCII).unwrap();
    assert_eq!(d.to_string(), "-789");
}

#[test]
fn decode_sign_separate_high_level_leading() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(5) SIGN IS LEADING SEPARATE.").unwrap();
    // EBCDIC '-' + "12345"
    let data: [u8; 6] = [0x60, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "-12345");
}

#[test]
fn decode_sign_separate_high_level_trailing() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(5) SIGN TRAILING SEPARATE.").unwrap();
    // "54321" + EBCDIC '+'
    let data: [u8; 6] = [0xF5, 0xF4, 0xF3, 0xF2, 0xF1, 0x4E];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["FLD"], "54321");
}

#[test]
fn decode_sign_separate_zero_value() {
    let data: [u8; 4] = [0x4E, 0xF0, 0xF0, 0xF0]; // EBCDIC "+000"
    let info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let d = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::CP037).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn decode_sign_separate_data_length_mismatch() {
    // 3-digit field + sign = 4 bytes expected, provide 3
    let data: [u8; 3] = [0x4E, 0xF1, 0xF2];
    let info = SignSeparateInfo {
        placement: SignPlacement::Leading,
    };
    let result = decode_zoned_decimal_sign_separate(&data, 3, 0, &info, Codepage::CP037);
    assert!(result.is_err());
}

// =============================================================================
// 6. JSON number modes: Lossless vs Native
// =============================================================================

#[test]
fn decode_json_lossless_comp3_returns_string() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(5)V99 COMP-3.").unwrap();
    let data: [u8; 4] = [0x12, 0x34, 0x56, 0x7C]; // 12345.67
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    // In lossless mode, numeric values are JSON strings
    assert!(
        result["FLD"].is_string(),
        "Lossless mode should produce string"
    );
    assert_eq!(result["FLD"], "12345.67");
}

#[test]
fn decode_json_native_comp3_returns_string() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(5)V99 COMP-3.").unwrap();
    let data: [u8; 4] = [0x12, 0x34, 0x56, 0x7C]; // 12345.67
    let result = decode_record(&schema, &data, &native_ebcdic()).unwrap();
    // Non-float numeric types return strings even in native mode at record level
    assert!(result["FLD"].is_string());
    assert_eq!(result["FLD"], "12345.67");
}

#[test]
fn decode_json_lossless_integer_returns_string() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(4) COMP.").unwrap();
    let data: [u8; 2] = [0x04, 0xD2]; // 1234
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert!(result["FLD"].is_string());
    assert_eq!(result["FLD"], "1234");
}

#[test]
fn decode_json_native_integer_still_string() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(4) COMP.").unwrap();
    let data: [u8; 2] = [0x04, 0xD2]; // 1234
    let result = decode_record(&schema, &data, &native_ebcdic()).unwrap();
    // COMP integer types return strings even in native mode
    assert!(result["FLD"].is_string());
    assert_eq!(result["FLD"], "1234");
}

#[test]
fn decode_json_lossless_preserves_trailing_zeros() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(3)V99.").unwrap();
    // EBCDIC "10000" → 100.00
    let data: [u8; 5] = [0xF1, 0xF0, 0xF0, 0xF0, 0xF0];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    // Lossless preserves the trailing zeros from the scale
    let s = result["FLD"].as_str().unwrap();
    assert!(
        s.contains(".00"),
        "Lossless should preserve trailing zeros, got: {s}"
    );
}

#[test]
fn decode_json_same_value_both_modes() {
    let schema = parse_copybook("01 REC. 05 FLD PIC S9(4) COMP.").unwrap();
    let data: [u8; 2] = [0x00, 0x2A]; // 42

    let lossless = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    let native = decode_record(&schema, &data, &native_ebcdic()).unwrap();

    // Both modes return strings for COMP integer types
    let lossless_val: i64 = lossless["FLD"].as_str().unwrap().parse().unwrap();
    let native_val: i64 = native["FLD"].as_str().unwrap().parse().unwrap();
    assert_eq!(lossless_val, native_val);
    assert_eq!(native_val, 42);
}

#[test]
fn decode_json_native_display_numeric_returns_string() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(5).").unwrap();
    // EBCDIC "00042"
    let data: [u8; 5] = [0xF0, 0xF0, 0xF0, 0xF4, 0xF2];
    let result = decode_record(&schema, &data, &native_ebcdic()).unwrap();
    // Display numerics return strings even in native mode
    assert!(result["FLD"].is_string());
}

#[test]
fn decode_json_lossless_display_preserves_zeros() {
    let schema = parse_copybook("01 REC. 05 FLD PIC 9(5).").unwrap();
    // EBCDIC "00042"
    let data: [u8; 5] = [0xF0, 0xF0, 0xF0, 0xF4, 0xF2];
    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert!(result["FLD"].is_string());
    // Lossless unsigned display preserves leading zeros
    assert_eq!(result["FLD"], "00042");
}

#[test]
fn decode_json_native_alphanumeric_unchanged() {
    let schema = parse_copybook("01 REC. 05 FLD PIC X(5).").unwrap();
    let data = b"HELLO";
    // Alphanumeric fields are always strings regardless of JSON mode
    let native_result = decode_record(&schema, data, &native_ascii()).unwrap();
    let lossless_result = decode_record(&schema, data, &lossless_ascii()).unwrap();
    assert_eq!(native_result["FLD"], "HELLO");
    assert_eq!(lossless_result["FLD"], "HELLO");
}

// =============================================================================
// 7. Multi-field record decode
// =============================================================================

#[test]
fn decode_multi_field_mixed_types() {
    let schema =
        parse_copybook("01 REC. 05 NAME PIC X(5). 05 AMT PIC S9(5)V99 COMP-3. 05 CODE PIC 9(3).")
            .unwrap();

    // NAME: EBCDIC "ABCDE" = 0xC1 0xC2 0xC3 0xC4 0xC5
    // AMT: COMP-3 +123.45 → 7 digits (S9(5)V99), packed: [0x01, 0x23, 0x45, 0x0C]
    //   Wait: S9(5)V99 = 7 total digits, 7 odd + sign = 8 nibbles = 4 bytes
    //   12345 (integer) + 00 (decimal scaled) → 0012345 → 0x00 0x12 0x34 0x5C
    //   For 123.45 → integer repr 0012345 → [0x00, 0x12, 0x34, 0x5C]
    // CODE: EBCDIC "042" = 0xF0 0xF4 0xF2
    let mut data = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5]); // NAME
    data.extend_from_slice(&[0x00, 0x12, 0x34, 0x5C]); // AMT
    data.extend_from_slice(&[0xF0, 0xF4, 0xF2]); // CODE

    let result = decode_record(&schema, &data, &lossless_ebcdic()).unwrap();
    assert_eq!(result["NAME"], "ABCDE");
    assert_eq!(result["AMT"], "123.45");
    // Unsigned PIC 9 preserves leading zeros
    assert_eq!(result["CODE"], "042");
}
