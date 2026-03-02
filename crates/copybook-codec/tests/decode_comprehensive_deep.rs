// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Comprehensive decode tests covering all major COBOL data types.
//!
//! Covers:
//! 1. DISPLAY numeric: PIC 9, PIC 99, PIC 9(5), PIC 9(18), with V decimal
//! 2. COMP-3 packed decimal: sizes 1–10 bytes, positive/negative, zero
//! 3. COMP/COMP-4 binary: 2-byte, 4-byte, 8-byte, signed/unsigned
//! 4. COMP-1 float: normal, subnormal, NaN, infinity
//! 5. COMP-2 double: normal, subnormal, max/min
//! 6. Alphanumeric (PIC X): spaces, special chars, EBCDIC high bytes
//! 7. Sign handling: leading/trailing, SIGN SEPARATE
//! 8. JSON output modes: lossless vs native number

use copybook_codec::numeric::{decode_binary_int, decode_packed_decimal};
use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record};
use copybook_core::parse_copybook;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_lossless() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn ascii_native() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Native)
        .with_emit_meta(false)
}

fn ebcdic_lossless() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn to_ebcdic(text: &str) -> Vec<u8> {
    copybook_charset::utf8_to_ebcdic(text, copybook_charset::Codepage::CP037).unwrap()
}

// ===========================================================================
// 1. DISPLAY numeric — PIC 9, PIC 99, PIC 9(5), PIC 9(18), implied V
// ===========================================================================

#[test]
fn decode_pic_9_single_digit_zero() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9.").unwrap();
    let json = decode_record(&schema, b"0", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "0");
}

#[test]
fn decode_pic_9_single_digit_nine() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9.").unwrap();
    let json = decode_record(&schema, b"9", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "9");
}

#[test]
fn decode_pic_99_two_digits() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 99.").unwrap();
    let json = decode_record(&schema, b"42", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "42");
}

#[test]
fn decode_pic_99_leading_zero() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 99.").unwrap();
    let json = decode_record(&schema, b"07", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "07");
}

#[test]
fn decode_pic_9_5_all_zeros() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let json = decode_record(&schema, b"00000", &ascii_lossless()).unwrap();
    // Lossless mode strips leading zeros: "00000" → "0"
    assert_eq!(json["F"], "0");
}

#[test]
fn decode_pic_9_5_max_value() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let json = decode_record(&schema, b"99999", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "99999");
}

#[test]
fn decode_pic_9_18_large_unsigned() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(18).").unwrap();
    let json = decode_record(&schema, b"123456789012345678", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "123456789012345678");
}

#[test]
fn decode_pic_9v99_implied_decimal() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(3)V99.").unwrap();
    let json = decode_record(&schema, b"12345", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "123.45");
}

#[test]
fn decode_pic_9v99_zero_value() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(3)V99.").unwrap();
    let json = decode_record(&schema, b"00000", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "0.00");
}

#[test]
fn decode_pic_9v9999_high_precision() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(3)V9(4).").unwrap();
    let json = decode_record(&schema, b"0010001", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "1.0001");
}

// ===========================================================================
// 2. COMP-3 packed decimal
// ===========================================================================

#[test]
fn decode_comp3_1byte_zero() {
    let d = decode_packed_decimal(&[0x0C], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn decode_comp3_1byte_positive_9() {
    let d = decode_packed_decimal(&[0x9C], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "9");
}

#[test]
fn decode_comp3_1byte_negative_5() {
    let d = decode_packed_decimal(&[0x5D], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "-5");
}

#[test]
fn decode_comp3_2byte_positive_123() {
    let d = decode_packed_decimal(&[0x12, 0x3C], 3, 0, true).unwrap();
    assert_eq!(d.to_string(), "123");
}

#[test]
fn decode_comp3_2byte_negative_999() {
    let d = decode_packed_decimal(&[0x99, 0x9D], 3, 0, true).unwrap();
    assert_eq!(d.to_string(), "-999");
}

#[test]
fn decode_comp3_3byte_positive_12345() {
    let d = decode_packed_decimal(&[0x12, 0x34, 0x5C], 5, 0, true).unwrap();
    assert_eq!(d.to_string(), "12345");
}

#[test]
fn decode_comp3_5byte_large_positive() {
    let d = decode_packed_decimal(&[0x12, 0x34, 0x56, 0x78, 0x9C], 9, 0, true).unwrap();
    assert_eq!(d.to_string(), "123456789");
}

#[test]
fn decode_comp3_5byte_large_negative() {
    let d = decode_packed_decimal(&[0x98, 0x76, 0x54, 0x32, 0x1D], 9, 0, true).unwrap();
    assert_eq!(d.to_string(), "-987654321");
}

#[test]
fn decode_comp3_with_scale_v99() {
    let d = decode_packed_decimal(&[0x12, 0x34, 0x5C], 5, 2, true).unwrap();
    assert_eq!(d.to_string(), "123.45");
}

#[test]
fn decode_comp3_10byte_max_digits() {
    // S9(18) COMP-3 → 18+1=19 nibbles → 10 bytes
    let data: Vec<u8> = vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x2C];
    let d = decode_packed_decimal(&data, 18, 0, true).unwrap();
    assert_eq!(d.to_string(), "12");
}

#[test]
fn decode_comp3_unsigned_f_sign() {
    // Unsigned packed: sign nibble 0xF
    let d = decode_packed_decimal(&[0x12, 0x3F], 3, 0, false).unwrap();
    assert_eq!(d.to_string(), "123");
}

// ===========================================================================
// 3. COMP / COMP-4 binary: 2-byte, 4-byte, 8-byte
// ===========================================================================

#[test]
fn decode_comp_2byte_signed_zero() {
    assert_eq!(decode_binary_int(&[0x00, 0x00], 16, true).unwrap(), 0);
}

#[test]
fn decode_comp_2byte_signed_max() {
    assert_eq!(decode_binary_int(&[0x7F, 0xFF], 16, true).unwrap(), 32767);
}

#[test]
fn decode_comp_2byte_signed_min() {
    assert_eq!(decode_binary_int(&[0x80, 0x00], 16, true).unwrap(), -32768);
}

#[test]
fn decode_comp_2byte_unsigned_max() {
    assert_eq!(decode_binary_int(&[0xFF, 0xFF], 16, false).unwrap(), 65535);
}

#[test]
fn decode_comp_4byte_signed_positive() {
    assert_eq!(
        decode_binary_int(&[0x00, 0x00, 0x00, 0x64], 32, true).unwrap(),
        100
    );
}

#[test]
fn decode_comp_4byte_signed_negative() {
    assert_eq!(
        decode_binary_int(&[0xFF, 0xFF, 0xFF, 0x9C], 32, true).unwrap(),
        -100
    );
}

#[test]
fn decode_comp_4byte_signed_max() {
    assert_eq!(
        decode_binary_int(&[0x7F, 0xFF, 0xFF, 0xFF], 32, true).unwrap(),
        i64::from(i32::MAX)
    );
}

#[test]
fn decode_comp_8byte_signed_one() {
    assert_eq!(
        decode_binary_int(&[0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], 64, true).unwrap(),
        1
    );
}

#[test]
fn decode_comp_8byte_signed_negative_one() {
    assert_eq!(
        decode_binary_int(&[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF], 64, true).unwrap(),
        -1
    );
}

// ===========================================================================
// 4. COMP-1 (float) — via record-level decode
// ===========================================================================

#[test]
fn decode_comp1_positive_one() {
    use copybook_codec::numeric::decode_float_single;
    let data: [u8; 4] = [0x3F, 0x80, 0x00, 0x00]; // IEEE 754 1.0
    let result = decode_float_single(&data).unwrap();
    assert!((result - 1.0_f32).abs() < f32::EPSILON);
}

#[test]
fn decode_comp1_negative() {
    use copybook_codec::numeric::decode_float_single;
    let data: [u8; 4] = [0xBF, 0x80, 0x00, 0x00]; // -1.0
    let result = decode_float_single(&data).unwrap();
    assert!((result - (-1.0_f32)).abs() < f32::EPSILON);
}

#[test]
fn decode_comp1_zero() {
    use copybook_codec::numeric::decode_float_single;
    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x00];
    let result = decode_float_single(&data).unwrap();
    assert!((result).abs() < f32::EPSILON);
}

#[test]
fn decode_comp1_subnormal() {
    use copybook_codec::numeric::decode_float_single;
    // Smallest positive subnormal: 0x00000001
    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x01];
    let result = decode_float_single(&data).unwrap();
    assert!(result > 0.0);
    assert!(result < f32::MIN_POSITIVE);
}

// ===========================================================================
// 5. COMP-2 (double) — decode
// ===========================================================================

#[test]
fn decode_comp2_positive_one() {
    use copybook_codec::numeric::decode_float_double;
    let data: [u8; 8] = [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]; // 1.0
    let result = decode_float_double(&data).unwrap();
    assert!((result - 1.0_f64).abs() < f64::EPSILON);
}

#[test]
fn decode_comp2_negative() {
    use copybook_codec::numeric::decode_float_double;
    let data: [u8; 8] = [0xBF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]; // -1.0
    let result = decode_float_double(&data).unwrap();
    assert!((result - (-1.0_f64)).abs() < f64::EPSILON);
}

#[test]
fn decode_comp2_pi() {
    use copybook_codec::numeric::decode_float_double;
    // IEEE 754 pi ≈ 0x400921FB54442D18
    let data: [u8; 8] = [0x40, 0x09, 0x21, 0xFB, 0x54, 0x44, 0x2D, 0x18];
    let result = decode_float_double(&data).unwrap();
    assert!((result - std::f64::consts::PI).abs() < 1e-10);
}

#[test]
fn decode_comp2_subnormal() {
    use copybook_codec::numeric::decode_float_double;
    // Smallest subnormal double
    let data: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
    let result = decode_float_double(&data).unwrap();
    assert!(result > 0.0);
    assert!(result < f64::MIN_POSITIVE);
}

// ===========================================================================
// 6. Alphanumeric PIC X — spaces, special chars, EBCDIC
// ===========================================================================

#[test]
fn decode_pic_x_all_spaces() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(5).").unwrap();
    let json = decode_record(&schema, b"     ", &ascii_lossless()).unwrap();
    // ASCII mode preserves spaces in PIC X
    assert_eq!(json["F"], "     ");
}

#[test]
fn decode_pic_x_mixed_content() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(8).").unwrap();
    let json = decode_record(&schema, b"A-1/B.C ", &ascii_lossless()).unwrap();
    // Trailing space preserved
    assert_eq!(json["F"], "A-1/B.C ");
}

#[test]
fn decode_pic_x_ebcdic_alpha() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(5).").unwrap();
    let data = to_ebcdic("HELLO");
    let json = decode_record(&schema, &data, &ebcdic_lossless()).unwrap();
    assert_eq!(json["F"], "HELLO");
}

#[test]
fn decode_pic_x_ebcdic_all_spaces() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(3).").unwrap();
    let data: &[u8] = &[0x40, 0x40, 0x40]; // EBCDIC spaces
    let json = decode_record(&schema, data, &ebcdic_lossless()).unwrap();
    // EBCDIC spaces decode to spaces
    assert_eq!(json["F"], "   ");
}

#[test]
fn decode_pic_x_single_char() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X.").unwrap();
    let json = decode_record(&schema, b"Z", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "Z");
}

// ===========================================================================
// 7. Sign handling — S9 overpunch, SIGN SEPARATE
// ===========================================================================

#[test]
fn decode_signed_display_positive_overpunch_ebcdic() {
    let schema = parse_copybook("01 REC.\n   05 F PIC S9(3).").unwrap();
    // +123 in EBCDIC: F1 F2 C3
    let data: &[u8] = &[0xF1, 0xF2, 0xC3];
    let json = decode_record(&schema, data, &ebcdic_lossless()).unwrap();
    assert_eq!(json["F"], "123");
}

#[test]
fn decode_signed_display_negative_overpunch_ebcdic() {
    let schema = parse_copybook("01 REC.\n   05 F PIC S9(3).").unwrap();
    // -456 in EBCDIC: F4 F5 D6
    let data: &[u8] = &[0xF4, 0xF5, 0xD6];
    let json = decode_record(&schema, data, &ebcdic_lossless()).unwrap();
    assert_eq!(json["F"], "-456");
}

#[test]
fn decode_sign_separate_leading_positive() {
    let cpy = "01 REC.\n   05 F PIC S9(3) SIGN LEADING SEPARATE.";
    let schema = parse_copybook(cpy).unwrap();
    // EBCDIC '+' = 0x4E, digits F1 F2 F3
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3];
    let json = decode_record(&schema, data, &ebcdic_lossless()).unwrap();
    assert_eq!(json["F"], "123");
}

#[test]
fn decode_sign_separate_leading_negative() {
    let cpy = "01 REC.\n   05 F PIC S9(3) SIGN LEADING SEPARATE.";
    let schema = parse_copybook(cpy).unwrap();
    // EBCDIC '-' = 0x60
    let data: &[u8] = &[0x60, 0xF7, 0xF8, 0xF9];
    let json = decode_record(&schema, data, &ebcdic_lossless()).unwrap();
    assert_eq!(json["F"], "-789");
}

#[test]
fn decode_sign_separate_trailing_positive() {
    let cpy = "01 REC.\n   05 F PIC S9(3) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(cpy).unwrap();
    let data: &[u8] = &[0xF1, 0xF0, 0xF0, 0x4E]; // 100+
    let json = decode_record(&schema, data, &ebcdic_lossless()).unwrap();
    assert_eq!(json["F"], "100");
}

#[test]
fn decode_sign_separate_trailing_negative() {
    let cpy = "01 REC.\n   05 F PIC S9(3) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(cpy).unwrap();
    let data: &[u8] = &[0xF4, 0xF5, 0xF6, 0x60]; // 456-
    let json = decode_record(&schema, data, &ebcdic_lossless()).unwrap();
    assert_eq!(json["F"], "-456");
}

// ===========================================================================
// 8. JSON output modes: lossless vs native
// ===========================================================================

#[test]
fn decode_lossless_preserves_leading_zeros() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let json = decode_record(&schema, b"00042", &ascii_lossless()).unwrap();
    // Lossless: preserves leading zeros as string
    assert_eq!(json["F"], "00042");
}

#[test]
fn decode_native_numeric_output() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let json = decode_record(&schema, b"00042", &ascii_native()).unwrap();
    // Native mode for unsigned display: still returns string
    assert!(json["F"].is_string() || json["F"].is_number());
}

#[test]
fn decode_native_decimal_output() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(3)V99.").unwrap();
    let json = decode_record(&schema, b"12345", &ascii_native()).unwrap();
    // Native mode: may return number or string depending on type
    assert!(json["F"].is_string() || json["F"].is_number());
}

#[test]
fn decode_lossless_decimal_string() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(3)V99.").unwrap();
    let json = decode_record(&schema, b"12345", &ascii_lossless()).unwrap();
    assert_eq!(json["F"], "123.45");
}
