// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Deep edge-case tests for COMP (binary integer) fields.
//!
//! Covers:
//! - All COMP sizes (2, 4, 8 bytes / 16, 32, 64 bits)
//! - Min/max values for each size (signed and unsigned)
//! - Negative values with two's complement encoding
//! - Zero values across all sizes
//! - Big-endian byte ordering verification
//! - Unsigned COMP fields (PIC 9 COMP)
//! - Full record-level decode/encode roundtrip

use copybook_codec::numeric::{decode_binary_int, encode_binary_int};
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

// ===========================================================================
// 1. 16-bit (2-byte) COMP — signed
// ===========================================================================

#[test]
fn test_comp_16bit_signed_zero() {
    let data = [0x00, 0x00];
    let result = decode_binary_int(&data, 16, true).unwrap();
    assert_eq!(result, 0);
}

#[test]
fn test_comp_16bit_signed_one() {
    let data = [0x00, 0x01];
    let result = decode_binary_int(&data, 16, true).unwrap();
    assert_eq!(result, 1);
}

#[test]
fn test_comp_16bit_signed_max() {
    // i16::MAX = 32767 = 0x7FFF
    let data = [0x7F, 0xFF];
    let result = decode_binary_int(&data, 16, true).unwrap();
    assert_eq!(result, 32767);
}

#[test]
fn test_comp_16bit_signed_min() {
    // i16::MIN = -32768 = 0x8000
    let data = [0x80, 0x00];
    let result = decode_binary_int(&data, 16, true).unwrap();
    assert_eq!(result, -32768);
}

#[test]
fn test_comp_16bit_signed_minus_one() {
    // -1 in two's complement = 0xFFFF
    let data = [0xFF, 0xFF];
    let result = decode_binary_int(&data, 16, true).unwrap();
    assert_eq!(result, -1);
}

#[test]
fn test_comp_16bit_signed_negative_100() {
    // -100 in two's complement 16-bit = 0xFF9C
    let data = (-100_i16).to_be_bytes();
    let result = decode_binary_int(&data, 16, true).unwrap();
    assert_eq!(result, -100);
}

// ===========================================================================
// 2. 16-bit (2-byte) COMP — unsigned
// ===========================================================================

#[test]
fn test_comp_16bit_unsigned_zero() {
    let data = [0x00, 0x00];
    let result = decode_binary_int(&data, 16, false).unwrap();
    assert_eq!(result, 0);
}

#[test]
fn test_comp_16bit_unsigned_max() {
    // u16::MAX = 65535 = 0xFFFF
    let data = [0xFF, 0xFF];
    let result = decode_binary_int(&data, 16, false).unwrap();
    assert_eq!(result, 65535);
}

#[test]
fn test_comp_16bit_unsigned_256() {
    let data = [0x01, 0x00];
    let result = decode_binary_int(&data, 16, false).unwrap();
    assert_eq!(result, 256);
}

// ===========================================================================
// 3. 32-bit (4-byte) COMP — signed
// ===========================================================================

#[test]
fn test_comp_32bit_signed_zero() {
    let data = [0x00, 0x00, 0x00, 0x00];
    let result = decode_binary_int(&data, 32, true).unwrap();
    assert_eq!(result, 0);
}

#[test]
fn test_comp_32bit_signed_max() {
    // i32::MAX = 2147483647 = 0x7FFFFFFF
    let data = [0x7F, 0xFF, 0xFF, 0xFF];
    let result = decode_binary_int(&data, 32, true).unwrap();
    assert_eq!(result, 2_147_483_647);
}

#[test]
fn test_comp_32bit_signed_min() {
    // i32::MIN = -2147483648 = 0x80000000
    let data = [0x80, 0x00, 0x00, 0x00];
    let result = decode_binary_int(&data, 32, true).unwrap();
    assert_eq!(result, -2_147_483_648);
}

#[test]
fn test_comp_32bit_signed_minus_one() {
    let data = [0xFF, 0xFF, 0xFF, 0xFF];
    let result = decode_binary_int(&data, 32, true).unwrap();
    assert_eq!(result, -1);
}

#[test]
fn test_comp_32bit_signed_negative_987654() {
    let data = (-987_654_i32).to_be_bytes();
    let result = decode_binary_int(&data, 32, true).unwrap();
    assert_eq!(result, -987_654);
}

// ===========================================================================
// 4. 32-bit (4-byte) COMP — unsigned
// ===========================================================================

#[test]
fn test_comp_32bit_unsigned_zero() {
    let data = [0x00, 0x00, 0x00, 0x00];
    let result = decode_binary_int(&data, 32, false).unwrap();
    assert_eq!(result, 0);
}

#[test]
fn test_comp_32bit_unsigned_max() {
    // u32::MAX = 4294967295 = 0xFFFFFFFF
    let data = [0xFF, 0xFF, 0xFF, 0xFF];
    let result = decode_binary_int(&data, 32, false).unwrap();
    assert_eq!(result, 4_294_967_295);
}

#[test]
fn test_comp_32bit_unsigned_one_million() {
    let data = (1_000_000_u32).to_be_bytes();
    let result = decode_binary_int(&data, 32, false).unwrap();
    assert_eq!(result, 1_000_000);
}

// ===========================================================================
// 5. 64-bit (8-byte) COMP — signed
// ===========================================================================

#[test]
fn test_comp_64bit_signed_zero() {
    let data = [0x00; 8];
    let result = decode_binary_int(&data, 64, true).unwrap();
    assert_eq!(result, 0);
}

#[test]
fn test_comp_64bit_signed_max() {
    // i64::MAX = 9223372036854775807
    let data = i64::MAX.to_be_bytes();
    let result = decode_binary_int(&data, 64, true).unwrap();
    assert_eq!(result, i64::MAX);
}

#[test]
fn test_comp_64bit_signed_min() {
    // i64::MIN = -9223372036854775808
    let data = i64::MIN.to_be_bytes();
    let result = decode_binary_int(&data, 64, true).unwrap();
    assert_eq!(result, i64::MIN);
}

#[test]
fn test_comp_64bit_signed_minus_one() {
    let data = [0xFF; 8];
    let result = decode_binary_int(&data, 64, true).unwrap();
    assert_eq!(result, -1);
}

// ===========================================================================
// 6. 64-bit (8-byte) COMP — unsigned
// ===========================================================================

#[test]
fn test_comp_64bit_unsigned_zero() {
    let data = [0x00; 8];
    let result = decode_binary_int(&data, 64, false).unwrap();
    assert_eq!(result, 0);
}

#[test]
fn test_comp_64bit_unsigned_i64_max() {
    // Maximum representable unsigned value in i64 return = i64::MAX
    let data = i64::MAX.to_be_bytes();
    let result = decode_binary_int(&data, 64, false).unwrap();
    assert_eq!(result, i64::MAX);
}

#[test]
fn test_comp_64bit_unsigned_overflow_rejected() {
    // u64::MAX = 0xFFFFFFFFFFFFFFFF exceeds i64::MAX — must error
    let data = [0xFF; 8];
    let result = decode_binary_int(&data, 64, false);
    assert!(
        result.is_err(),
        "unsigned 64-bit value > i64::MAX must fail"
    );
}

// ===========================================================================
// 7. Encode roundtrip for all sizes
// ===========================================================================

#[test]
fn test_encode_decode_roundtrip_16bit_signed() {
    for value in [-32768_i64, -1, 0, 1, 32767] {
        let encoded = encode_binary_int(value, 16, true).unwrap();
        let decoded = decode_binary_int(&encoded, 16, true).unwrap();
        assert_eq!(decoded, value, "roundtrip failed for {value}");
    }
}

#[test]
fn test_encode_decode_roundtrip_16bit_unsigned() {
    for value in [0_i64, 1, 255, 256, 65535] {
        let encoded = encode_binary_int(value, 16, false).unwrap();
        let decoded = decode_binary_int(&encoded, 16, false).unwrap();
        assert_eq!(decoded, value, "roundtrip failed for {value}");
    }
}

#[test]
fn test_encode_decode_roundtrip_32bit_signed() {
    for value in [-2_147_483_648_i64, -1, 0, 1, 2_147_483_647] {
        let encoded = encode_binary_int(value, 32, true).unwrap();
        let decoded = decode_binary_int(&encoded, 32, true).unwrap();
        assert_eq!(decoded, value, "roundtrip failed for {value}");
    }
}

#[test]
fn test_encode_decode_roundtrip_32bit_unsigned() {
    for value in [0_i64, 1, 65536, 4_294_967_295] {
        let encoded = encode_binary_int(value, 32, false).unwrap();
        let decoded = decode_binary_int(&encoded, 32, false).unwrap();
        assert_eq!(decoded, value, "roundtrip failed for {value}");
    }
}

#[test]
fn test_encode_decode_roundtrip_64bit_signed() {
    for value in [i64::MIN, -1, 0, 1, i64::MAX] {
        let encoded = encode_binary_int(value, 64, true).unwrap();
        let decoded = decode_binary_int(&encoded, 64, true).unwrap();
        assert_eq!(decoded, value, "roundtrip failed for {value}");
    }
}

// ===========================================================================
// 8. Encode out-of-range errors
// ===========================================================================

#[test]
fn test_encode_16bit_signed_overflow() {
    let result = encode_binary_int(32768, 16, true);
    assert!(result.is_err(), "32768 exceeds signed 16-bit range");
}

#[test]
fn test_encode_16bit_signed_underflow() {
    let result = encode_binary_int(-32769, 16, true);
    assert!(result.is_err(), "-32769 exceeds signed 16-bit range");
}

#[test]
fn test_encode_16bit_unsigned_negative() {
    let result = encode_binary_int(-1, 16, false);
    assert!(
        result.is_err(),
        "negative value invalid for unsigned 16-bit"
    );
}

#[test]
fn test_encode_32bit_signed_overflow() {
    let result = encode_binary_int(2_147_483_648, 32, true);
    assert!(result.is_err());
}

#[test]
fn test_encode_32bit_unsigned_negative() {
    let result = encode_binary_int(-1, 32, false);
    assert!(result.is_err());
}

// ===========================================================================
// 9. Decode with wrong data length
// ===========================================================================

#[test]
fn test_decode_16bit_with_1_byte_fails() {
    let result = decode_binary_int(&[0x01], 16, true);
    assert!(result.is_err());
}

#[test]
fn test_decode_32bit_with_2_bytes_fails() {
    let result = decode_binary_int(&[0x01, 0x02], 32, true);
    assert!(result.is_err());
}

#[test]
fn test_decode_64bit_with_4_bytes_fails() {
    let result = decode_binary_int(&[0x01, 0x02, 0x03, 0x04], 64, true);
    assert!(result.is_err());
}

#[test]
fn test_decode_unsupported_bit_width() {
    let result = decode_binary_int(&[0x01, 0x02, 0x03], 24, true);
    assert!(result.is_err());
}

// ===========================================================================
// 10. Big-endian byte-order verification
// ===========================================================================

#[test]
fn test_big_endian_16bit_byte_order() {
    // 0x0100 = 256 in big-endian
    let encoded = encode_binary_int(256, 16, false).unwrap();
    assert_eq!(encoded, [0x01, 0x00], "MSB first for big-endian");
}

#[test]
fn test_big_endian_32bit_byte_order() {
    // 0x00010000 = 65536
    let encoded = encode_binary_int(65536, 32, false).unwrap();
    assert_eq!(encoded, [0x00, 0x01, 0x00, 0x00]);
}

#[test]
fn test_big_endian_64bit_byte_order() {
    let encoded = encode_binary_int(1, 64, true).unwrap();
    assert_eq!(encoded, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
}

// ===========================================================================
// 11. Record-level decode/encode roundtrip with COMP fields
// ===========================================================================

#[test]
fn test_record_comp_s9_4_decode_encode_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(4) COMP.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // PIC S9(4) COMP => 16 bits signed => 2 bytes
    let data: Vec<u8> = 12345_i16.to_be_bytes().to_vec();
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "12345");
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_comp_9_4_unsigned_decode() {
    let cpy = "       01  REC.\n           05  F1 PIC 9(4) COMP.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // PIC 9(4) COMP => 16 bits unsigned => 2 bytes
    let data: Vec<u8> = 50000_u16.to_be_bytes().to_vec();
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "50000");
}

#[test]
fn test_record_comp_s9_9_decode_encode_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(9) COMP.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // PIC S9(9) COMP => 32 bits signed => 4 bytes
    let data: Vec<u8> = (-999_999_999_i32).to_be_bytes().to_vec();
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "-999999999");
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_comp_s9_18_decode_encode_roundtrip() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(18) COMP.\n";
    let schema = parse_copybook(cpy).expect("parse");
    // PIC S9(18) COMP => 64 bits signed => 8 bytes
    let value: i64 = 123_456_789_012_345_678;
    let data: Vec<u8> = value.to_be_bytes().to_vec();
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["F1"], "123456789012345678");
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}

#[test]
fn test_record_comp_zero_value_all_sizes() {
    // 16-bit
    let cpy16 = "       01  REC.\n           05  F1 PIC S9(4) COMP.\n";
    let s16 = parse_copybook(cpy16).unwrap();
    let j16 = decode_record(&s16, &[0, 0], &ascii_decode_opts()).unwrap();
    assert_eq!(j16["F1"], "0");

    // 32-bit
    let cpy32 = "       01  REC.\n           05  F1 PIC S9(9) COMP.\n";
    let s32 = parse_copybook(cpy32).unwrap();
    let j32 = decode_record(&s32, &[0, 0, 0, 0], &ascii_decode_opts()).unwrap();
    assert_eq!(j32["F1"], "0");

    // 64-bit
    let cpy64 = "       01  REC.\n           05  F1 PIC S9(18) COMP.\n";
    let s64 = parse_copybook(cpy64).unwrap();
    let j64 = decode_record(&s64, &[0; 8], &ascii_decode_opts()).unwrap();
    assert_eq!(j64["F1"], "0");
}

#[test]
fn test_record_comp_native_json_number_mode() {
    let cpy = "       01  REC.\n           05  F1 PIC S9(4) COMP.\n";
    let schema = parse_copybook(cpy).expect("parse");
    let data: Vec<u8> = 42_i16.to_be_bytes().to_vec();
    let json = decode_record(&schema, &data, &native_decode_opts()).unwrap();
    // COMP fields return string "42" in both modes (codec behaviour)
    assert_eq!(json["F1"].as_str().unwrap().trim(), "42");
}

#[test]
fn test_record_multi_comp_fields() {
    let cpy = "\
       01  REC.
           05  A PIC S9(4) COMP.
           05  B PIC 9(9) COMP.
           05  C PIC S9(18) COMP.
";
    let schema = parse_copybook(cpy).expect("parse");
    // A: 2 bytes, B: 4 bytes, C: 8 bytes = 14 bytes total
    let mut data = Vec::new();
    data.extend_from_slice(&(-1_i16).to_be_bytes());
    data.extend_from_slice(&1_000_000_u32.to_be_bytes());
    data.extend_from_slice(&i64::MAX.to_be_bytes());
    let json = decode_record(&schema, &data, &ascii_decode_opts()).unwrap();
    assert_eq!(json["A"], "-1");
    assert_eq!(json["B"], "1000000");
    assert_eq!(json["C"], "9223372036854775807");
    let re_encoded = encode_record(&schema, &json, &ascii_encode_opts()).unwrap();
    assert_eq!(re_encoded, data);
}
