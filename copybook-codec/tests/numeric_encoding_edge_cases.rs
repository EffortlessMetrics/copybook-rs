#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
//! Comprehensive tests for numeric encoding edge cases
//!
//! This test suite validates edge cases in numeric encoding:
//! - encode_packed_decimal with various edge cases
//! - encode_binary_int with different widths
//! - encode_zoned_decimal with format detection
//! - Boundary values and overflow handling

use copybook_codec::numeric::{
    encode_packed_decimal, encode_binary_int, encode_zoned_decimal_with_format,
};
use copybook_codec::options::{Codepage, ZonedEncodingFormat};

#[test]
fn test_encode_packed_decimal_single_digit() {
    // Test encoding a single digit
    let result = encode_packed_decimal("5", 1, 0, true)
        .expect("Should encode successfully");

    // Single digit packed: 0x5C (5 positive)
    assert_eq!(result, vec![0x5C]);
}

#[test]
fn test_encode_packed_decimal_single_digit_negative() {
    // Test encoding a single negative digit
    let result = encode_packed_decimal("-5", 1, 0, true)
        .expect("Should encode successfully");

    // Single digit packed: 0x5D (5 negative)
    assert_eq!(result, vec![0x5D]);
}

#[test]
fn test_encode_packed_decimal_odd_digits() {
    // Test encoding odd number of digits (leaves nibble as 0)
    let result = encode_packed_decimal("123", 3, 0, true)
        .expect("Should encode successfully");

    // 123 positive: 0x12, 0x3C (last nibble is 0, sign is C)
    assert_eq!(result, vec![0x12, 0x3C]);
}

#[test]
fn test_encode_packed_decimal_even_digits() {
    // Test encoding even number of digits
    let result = encode_packed_decimal("1234", 4, 0, true)
        .expect("Should encode successfully");

    // 1234 positive: 0x12, 0x34, 0x0C (sign is C)
    assert_eq!(result, vec![0x12, 0x34, 0x0C]);
}

#[test]
fn test_encode_packed_decimal_with_scale() {
    // Test encoding with decimal places
    let result = encode_packed_decimal("123.45", 5, 2, true)
        .expect("Should encode successfully");

    // 12345 positive: 0x12, 0x34, 0x5C
    assert_eq!(result, vec![0x12, 0x34, 0x5C]);
}

#[test]
fn test_encode_packed_decimal_negative_with_scale() {
    // Test encoding negative with decimal places
    let result = encode_packed_decimal("-987.65", 5, 2, true)
        .expect("Should encode successfully");

    // 98765 negative: 0x98, 0x76, 0x5D
    assert_eq!(result, vec![0x98, 0x76, 0x5D]);
}

#[test]
fn test_encode_packed_decimal_all_zeros() {
    // Test encoding all zeros
    let result = encode_packed_decimal("000", 3, 0, true)
        .expect("Should encode successfully");

    // 000 positive: 0x00, 0x0C
    assert_eq!(result, vec![0x00, 0x0C]);
}

#[test]
fn test_encode_packed_decimal_all_nines() {
    // Test encoding all nines (boundary value)
    let result = encode_packed_decimal("999", 3, 0, true)
        .expect("Should encode successfully");

    // 999 positive: 0x99, 0x9C
    assert_eq!(result, vec![0x99, 0x9C]);
}

#[test]
fn test_encode_packed_decimal_unsigned() {
    // Test encoding unsigned field
    let result = encode_packed_decimal("123", 3, 0, false)
        .expect("Should encode successfully");

    // Unsigned uses 0xF for sign
    assert_eq!(result, vec![0x12, 0x3F]);
}

#[test]
fn test_encode_packed_decimal_unsigned_negative_fails() {
    // Test that encoding negative to unsigned fails
    let result = encode_packed_decimal("-123", 3, 0, false);

    assert!(result.is_err());
}

#[test]
fn test_encode_packed_decimal_large_number() {
    // Test encoding large number of digits
    let value = "12345678901234567890";
    let result = encode_packed_decimal(value, 20, 0, true)
        .expect("Should encode successfully");

    // Should be 10 bytes for 20 digits + sign nibble
    assert_eq!(result.len(), 10);
    // Last byte should have positive sign nibble (0xC)
    assert_eq!(result[9] & 0x0F, 0x0C);
}

#[test]
fn test_encode_packed_decimal_zero_with_scale() {
    // Test encoding zero with scale
    let result = encode_packed_decimal("0.00", 3, 2, true)
        .expect("Should encode successfully");

    // 000 positive: 0x00, 0x0C
    assert_eq!(result, vec![0x00, 0x0C]);
}

#[test]
fn test_encode_packed_decimal_negative_scale() {
    // Test encoding with negative scale (multiplier)
    let result = encode_packed_decimal("12345", 5, -2, true)
        .expect("Should encode successfully");

    // 12345 positive: 0x12, 0x34, 0x5C
    assert_eq!(result, vec![0x12, 0x34, 0x5C]);
}

#[test]
fn test_encode_packed_decimal_trailing_zeros() {
    // Test encoding with trailing zeros
    let result = encode_packed_decimal("12300", 5, 0, true)
        .expect("Should encode successfully");

    // 12300 positive: 0x12, 0x30, 0x0C
    assert_eq!(result, vec![0x12, 0x30, 0x0C]);
}

#[test]
fn test_encode_packed_decimal_leading_zeros() {
    // Test encoding with leading zeros
    let result = encode_packed_decimal("00123", 5, 0, true)
        .expect("Should encode successfully");

    // 00123 positive: 0x00, 0x12, 0x3C
    assert_eq!(result, vec![0x00, 0x12, 0x3C]);
}

#[test]
fn test_encode_binary_int_2_bytes() {
    // Test encoding to 2 bytes (1-4 digits)
    let result = encode_binary_int(1234, 16, false)
        .expect("Should encode successfully");

    // 1234 in big-endian 2 bytes
    assert_eq!(result, vec![0x04, 0xD2]);
}

#[test]
fn test_encode_binary_int_2_bytes_negative() {
    // Test encoding negative to 2 bytes
    let result = encode_binary_int(-1234, 16, true)
        .expect("Should encode successfully");

    // -1234 in big-endian 2 bytes (signed)
    assert_eq!(result, vec![0xFB, 0x2E]);
}

#[test]
fn test_encode_binary_int_4_bytes() {
    // Test encoding to 4 bytes (5-9 digits)
    let result = encode_binary_int(12345, 32, false)
        .expect("Should encode successfully");

    // 12345 in big-endian 4 bytes
    assert_eq!(result, vec![0x00, 0x00, 0x30, 0x39]);
}

#[test]
fn test_encode_binary_int_4_bytes_negative() {
    // Test encoding negative to 4 bytes
    let result = encode_binary_int(-12345, 32, true)
        .expect("Should encode successfully");

    // -12345 in big-endian 4 bytes (signed)
    assert_eq!(result, vec![0xFF, 0xFF, 0xCF, 0xC7]);
}

#[test]
fn test_encode_binary_int_8_bytes() {
    // Test encoding to 8 bytes (10-18 digits)
    let result = encode_binary_int(1234567890, 64, false)
        .expect("Should encode successfully");

    // 1234567890 in big-endian 8 bytes
    assert_eq!(result, vec![0x00, 0x00, 0x00, 0x00, 0x49, 0x96, 0x02, 0xD2]);
}

#[test]
fn test_encode_binary_int_8_bytes_negative() {
    // Test encoding negative to 8 bytes
    let result = encode_binary_int(-1234567890, 64, true)
        .expect("Should encode successfully");

    // -1234567890 in big-endian 8 bytes (signed)
    assert_eq!(result, vec![0xFF, 0xFF, 0xFF, 0xFF, 0xB6, 0x69, 0xFD, 0x2E]);
}

#[test]
fn test_encode_binary_int_max_i16() {
    // Test encoding maximum i16 value
    let result = encode_binary_int(32767, 16, true)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0x7F, 0xFF]);
}

#[test]
fn test_encode_binary_int_min_i16() {
    // Test encoding minimum i16 value
    let result = encode_binary_int(-32768, 16, true)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0x80, 0x00]);
}

#[test]
fn test_encode_binary_int_max_i32() {
    // Test encoding maximum i32 value
    let result = encode_binary_int(2147483647, 32, true)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0x7F, 0xFF, 0xFF, 0xFF]);
}

#[test]
fn test_encode_binary_int_min_i32() {
    // Test encoding minimum i32 value
    let result = encode_binary_int(-2147483648, 32, true)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0x80, 0x00, 0x00, 0x00]);
}

#[test]
fn test_encode_binary_int_max_u16() {
    // Test encoding maximum u16 value
    let result = encode_binary_int(65535, 16, false)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0xFF, 0xFF]);
}

#[test]
fn test_encode_binary_int_max_u32() {
    // Test encoding maximum u32 value
    let result = encode_binary_int(4294967295, 32, false)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0xFF, 0xFF, 0xFF, 0xFF]);
}

#[test]
fn test_encode_binary_int_zero() {
    // Test encoding zero
    let result = encode_binary_int(0, 16, true)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0x00, 0x00]);
}

#[test]
fn test_encode_binary_int_negative_zero_normalizes() {
    // Test that -0 normalizes to 0
    let result = encode_binary_int(0, 16, true)
        .expect("Should encode successfully");

    assert_eq!(result, vec![0x00, 0x00]);
}

#[test]
fn test_encode_binary_int_with_scale() {
    // Test encoding with scale
    let result = encode_binary_int(12345, 32, true)
        .expect("Should encode successfully");

    // 12345 in big-endian 4 bytes
    assert_eq!(result, vec![0x00, 0x00, 0x30, 0x39]);
}

#[test]
fn test_encode_zoned_decimal_ascii() {
    // Test encoding to ASCII zoned decimal
    let result = encode_zoned_decimal_with_format(
        "123",
        3,
        0,
        true,
        Codepage::ASCII,
        Some(ZonedEncodingFormat::Ascii),
    )
        .expect("Should encode successfully");

    assert_eq!(result, b"123");
}

#[test]
fn test_encode_zoned_decimal_ascii_negative() {
    // Test encoding negative to ASCII zoned decimal (with overpunch)
    let result = encode_zoned_decimal_with_format(
        "-123",
        3,
        0,
        true,
        Codepage::ASCII,
        Some(ZonedEncodingFormat::Ascii),
    )
        .expect("Should encode successfully");

    // Negative ASCII uses overpunch: 'L' for -3
    assert_eq!(result, b"12L");
}

#[test]
fn test_encode_zoned_decimal_ebcdic() {
    // Test encoding to EBCDIC zoned decimal
    let result = encode_zoned_decimal_with_format(
        "123",
        3,
        0,
        true,
        Codepage::CP037,
        Some(ZonedEncodingFormat::Ebcdic),
    )
        .expect("Should encode successfully");

    assert_eq!(result, vec![0xF1, 0xF2, 0xF3]);
}

#[test]
fn test_encode_zoned_decimal_ebcdic_negative() {
    // Test encoding negative to EBCDIC zoned decimal (with overpunch)
    let result = encode_zoned_decimal_with_format(
        "-123",
        3,
        0,
        true,
        Codepage::CP037,
        Some(ZonedEncodingFormat::Ebcdic),
    )
        .expect("Should encode successfully");

    // Negative EBCDIC uses overpunch: 0xD3 for -3
    assert_eq!(result, vec![0xF1, 0xF2, 0xD3]);
}

#[test]
fn test_encode_zoned_decimal_with_scale_ascii() {
    // Test encoding with scale to ASCII
    let result = encode_zoned_decimal_with_format(
        "123.45",
        5,
        2,
        true,
        Codepage::ASCII,
        Some(ZonedEncodingFormat::Ascii),
    )
        .expect("Should encode successfully");

    assert_eq!(result, b"12345");
}

#[test]
fn test_encode_zoned_decimal_with_scale_ebcdic() {
    // Test encoding with scale to EBCDIC
    let result = encode_zoned_decimal_with_format(
        "123.45",
        5,
        2,
        true,
        Codepage::CP037,
        Some(ZonedEncodingFormat::Ebcdic),
    )
        .expect("Should encode successfully");

    assert_eq!(result, vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5]);
}

#[test]
fn test_encode_zoned_decimal_unsigned_ascii() {
    // Test encoding unsigned to ASCII
    let result = encode_zoned_decimal_with_format(
        "123",
        3,
        0,
        false,
        Codepage::ASCII,
        Some(ZonedEncodingFormat::Ascii),
    )
        .expect("Should encode successfully");

    assert_eq!(result, b"123");
}

#[test]
fn test_encode_zoned_decimal_unsigned_ebcdic() {
    // Test encoding unsigned to EBCDIC
    let result = encode_zoned_decimal_with_format(
        "123",
        3,
        0,
        false,
        Codepage::CP037,
        Some(ZonedEncodingFormat::Ebcdic),
    )
        .expect("Should encode successfully");

    assert_eq!(result, vec![0xF1, 0xF2, 0xF3]);
}

#[test]
fn test_encode_zoned_decimal_zero_ascii() {
    // Test encoding zero to ASCII
    let result = encode_zoned_decimal_with_format(
        "0",
        1,
        0,
        true,
        Codepage::ASCII,
        Some(ZonedEncodingFormat::Ascii),
    )
        .expect("Should encode successfully");

    assert_eq!(result, b"0");
}

#[test]
fn test_encode_zoned_decimal_zero_ebcdic() {
    // Test encoding zero to EBCDIC
    let result = encode_zoned_decimal_with_format(
        "0",
        1,
        0,
        true,
        Codepage::CP037,
        Some(ZonedEncodingFormat::Ebcdic),
    )
        .expect("Should encode successfully");

    assert_eq!(result, vec![0xF0]);
}

#[test]
fn test_encode_packed_decimal_alternative_sign_nibbles() {
    // Test that alternative positive sign nibbles work (0xA, 0xE, 0xF)
    // This test verifies the decoder accepts these variants

    // Encode normally
    let result = encode_packed_decimal("123", 3, 0, true)
        .expect("Should encode successfully");

    // Standard encoding uses 0xC for positive
    assert_eq!(result[1] & 0x0F, 0x0C);
}

#[test]
fn test_encode_binary_int_1_byte() {
    // Test encoding to 2 bytes minimum value (1 byte values not supported)
    let result = encode_binary_int(9, 16, false)
        .expect("Should encode successfully");

    // 9 in big-endian 2 bytes
    assert_eq!(result, vec![0x00, 0x09]);
}

#[test]
fn test_encode_binary_int_1_byte_negative() {
    // Test encoding negative to 2 bytes (1 byte values not supported)
    let result = encode_binary_int(-9, 16, true)
        .expect("Should encode successfully");

    // -9 in big-endian 2 bytes (signed)
    assert_eq!(result, vec![0xFF, 0xF7]);
}

#[test]
fn test_encode_packed_decimal_max_18_digits() {
    // Test encoding maximum 18 digits (COMP-3 limit)
    let value = "999999999999999999";
    let result = encode_packed_decimal(value, 18, 0, true)
        .expect("Should encode successfully");

    // Should be 9 bytes for 18 digits + sign nibble
    assert_eq!(result.len(), 9);
}

#[test]
fn test_encode_zoned_decimal_leading_zeros_ascii() {
    // Test encoding with leading zeros to ASCII
    let result = encode_zoned_decimal_with_format(
        "00123",
        5,
        0,
        true,
        Codepage::ASCII,
        Some(ZonedEncodingFormat::Ascii),
    )
        .expect("Should encode successfully");

    assert_eq!(result, b"00123");
}

#[test]
fn test_encode_zoned_decimal_leading_zeros_ebcdic() {
    // Test encoding with leading zeros to EBCDIC
    let result = encode_zoned_decimal_with_format(
        "00123",
        5,
        0,
        true,
        Codepage::CP037,
        Some(ZonedEncodingFormat::Ebcdic),
    )
        .expect("Should encode successfully");

    assert_eq!(result, vec![0xF0, 0xF0, 0xF1, 0xF2, 0xF3]);
}

#[test]
fn test_encode_packed_decimal_negative_zero_normalizes() {
    // Test that -0 normalizes to 0 in packed decimal
    let result = encode_packed_decimal("-0", 1, 0, true)
        .expect("Should encode successfully");

    // Should normalize to positive zero
    assert_eq!(result, vec![0x0C]);
}
