// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Comprehensive encode tests covering all major COBOL data types.
//!
//! Covers:
//! 1. JSON string → DISPLAY numeric encoding
//! 2. JSON number → COMP-3 encoding (all sizes)
//! 3. JSON number → COMP binary encoding
//! 4. JSON string → alphanumeric with padding
//! 5. JSON null handling
//! 6. Truncation behavior (value too large for field)
//! 7. Error cases: type mismatches, overflow, invalid values

use copybook_codec::numeric::{
    encode_alphanumeric, encode_binary_int, encode_packed_decimal, encode_zoned_decimal,
};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use serde_json::json;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_enc() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true)
}

fn ascii_dec() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn ebcdic_enc() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true)
}

fn ebcdic_dec() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

// ===========================================================================
// 1. JSON string → DISPLAY numeric encoding
// ===========================================================================

#[test]
fn encode_display_zero_padded() {
    let encoded = encode_zoned_decimal("42", 5, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"00042");
}

#[test]
fn encode_display_exact_digits() {
    let encoded = encode_zoned_decimal("99999", 5, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"99999");
}

#[test]
fn encode_display_single_digit() {
    let encoded = encode_zoned_decimal("7", 1, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"7");
}

#[test]
fn encode_display_all_zeros() {
    let encoded = encode_zoned_decimal("0", 5, 0, false, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"00000");
}

#[test]
fn encode_display_signed_positive() {
    let encoded = encode_zoned_decimal("123", 3, 0, true, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 3);
}

#[test]
fn encode_display_record_level_pic_9() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let encoded = encode_record(&schema, &json!({"F": "00042"}), &ascii_enc()).unwrap();
    assert_eq!(encoded, b"00042");
}

#[test]
fn encode_display_record_level_pic_99() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 99.").unwrap();
    let encoded = encode_record(&schema, &json!({"F": "07"}), &ascii_enc()).unwrap();
    assert_eq!(encoded, b"07");
}

// ===========================================================================
// 2. JSON number → COMP-3 encoding
// ===========================================================================

#[test]
fn encode_comp3_positive_zero() {
    let encoded = encode_packed_decimal("0", 1, 0, true).unwrap();
    assert_eq!(encoded, vec![0x0C]);
}

#[test]
fn encode_comp3_positive_9() {
    let encoded = encode_packed_decimal("9", 1, 0, true).unwrap();
    assert_eq!(encoded, vec![0x9C]);
}

#[test]
fn encode_comp3_negative_5() {
    let encoded = encode_packed_decimal("-5", 1, 0, true).unwrap();
    assert_eq!(encoded, vec![0x5D]);
}

#[test]
fn encode_comp3_positive_123() {
    let encoded = encode_packed_decimal("123", 3, 0, true).unwrap();
    assert_eq!(encoded, vec![0x12, 0x3C]);
}

#[test]
fn encode_comp3_negative_999() {
    let encoded = encode_packed_decimal("-999", 3, 0, true).unwrap();
    assert_eq!(encoded, vec![0x99, 0x9D]);
}

#[test]
fn encode_comp3_positive_12345() {
    let encoded = encode_packed_decimal("12345", 5, 0, true).unwrap();
    assert_eq!(encoded, vec![0x12, 0x34, 0x5C]);
}

#[test]
fn encode_comp3_large_9_digits() {
    let encoded = encode_packed_decimal("123456789", 9, 0, true).unwrap();
    assert_eq!(encoded, vec![0x12, 0x34, 0x56, 0x78, 0x9C]);
}

#[test]
fn encode_comp3_record_level_with_scale() {
    // Use record-level API to properly handle scale via schema
    let schema = parse_copybook("01 REC.\n   05 F PIC S9(3)V99 COMP-3.").unwrap();
    let encoded = encode_record(&schema, &json!({"F": "123.45"}), &ascii_enc()).unwrap();
    assert_eq!(encoded, vec![0x12, 0x34, 0x5C]);
}

#[test]
fn encode_comp3_unsigned_f_sign() {
    let encoded = encode_packed_decimal("123", 3, 0, false).unwrap();
    assert_eq!(encoded, vec![0x12, 0x3F]);
}

#[test]
fn encode_comp3_record_level() {
    let schema = parse_copybook("01 REC.\n   05 F PIC S9(5) COMP-3.").unwrap();
    let encoded = encode_record(&schema, &json!({"F": "12345"}), &ascii_enc()).unwrap();
    assert_eq!(encoded, vec![0x12, 0x34, 0x5C]);
}

// ===========================================================================
// 3. JSON number → COMP binary encoding
// ===========================================================================

#[test]
fn encode_comp_2byte_signed_zero() {
    let encoded = encode_binary_int(0, 16, true).unwrap();
    assert_eq!(encoded, vec![0x00, 0x00]);
}

#[test]
fn encode_comp_2byte_signed_positive() {
    let encoded = encode_binary_int(1000, 16, true).unwrap();
    assert_eq!(encoded, vec![0x03, 0xE8]);
}

#[test]
fn encode_comp_2byte_signed_negative() {
    let encoded = encode_binary_int(-1, 16, true).unwrap();
    assert_eq!(encoded, vec![0xFF, 0xFF]);
}

#[test]
fn encode_comp_2byte_signed_max() {
    let encoded = encode_binary_int(32767, 16, true).unwrap();
    assert_eq!(encoded, vec![0x7F, 0xFF]);
}

#[test]
fn encode_comp_4byte_signed_positive() {
    let encoded = encode_binary_int(100, 32, true).unwrap();
    assert_eq!(encoded, vec![0x00, 0x00, 0x00, 0x64]);
}

#[test]
fn encode_comp_4byte_signed_negative() {
    let encoded = encode_binary_int(-100, 32, true).unwrap();
    assert_eq!(encoded, vec![0xFF, 0xFF, 0xFF, 0x9C]);
}

#[test]
fn encode_comp_8byte_signed_one() {
    let encoded = encode_binary_int(1, 64, true).unwrap();
    assert_eq!(
        encoded,
        vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
    );
}

#[test]
fn encode_comp_record_level_2byte() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(4) COMP.").unwrap();
    let encoded = encode_record(&schema, &json!({"F": "1234"}), &ascii_enc()).unwrap();
    assert_eq!(encoded, vec![0x04, 0xD2]);
}

// ===========================================================================
// 4. JSON string → alphanumeric with padding
// ===========================================================================

#[test]
fn encode_alpha_exact_length() {
    let encoded = encode_alphanumeric("HELLO", 5, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"HELLO");
}

#[test]
fn encode_alpha_short_padded() {
    let encoded = encode_alphanumeric("AB", 8, Codepage::ASCII).unwrap();
    assert_eq!(encoded.len(), 8);
    assert_eq!(&encoded[..2], b"AB");
    assert!(encoded[2..].iter().all(|&b| b == b' '));
}

#[test]
fn encode_alpha_empty_string() {
    let encoded = encode_alphanumeric("", 6, Codepage::ASCII).unwrap();
    assert!(encoded.iter().all(|&b| b == b' '));
}

#[test]
fn encode_alpha_single_char() {
    let encoded = encode_alphanumeric("Z", 1, Codepage::ASCII).unwrap();
    assert_eq!(encoded, b"Z");
}

#[test]
fn encode_alpha_ebcdic_padding() {
    let encoded = encode_alphanumeric("A", 4, Codepage::CP037).unwrap();
    assert_eq!(encoded.len(), 4);
    assert!(encoded[1..].iter().all(|&b| b == 0x40)); // EBCDIC space
}

#[test]
fn encode_alpha_record_level_exact() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(5).").unwrap();
    let encoded = encode_record(&schema, &json!({"F": "HELLO"}), &ascii_enc()).unwrap();
    assert_eq!(encoded, b"HELLO");
}

#[test]
fn encode_alpha_record_level_padded() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(10).").unwrap();
    let encoded = encode_record(&schema, &json!({"F": "HI"}), &ascii_enc()).unwrap();
    assert_eq!(encoded.len(), 10);
    assert_eq!(&encoded[..2], b"HI");
}

// ===========================================================================
// 5. JSON null handling
// ===========================================================================

#[test]
fn encode_null_pic_x_produces_output() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(5).").unwrap();
    let encoded = encode_record(&schema, &json!({"F": null}), &ascii_enc()).unwrap();
    // Null alphanumeric produces 5-byte output
    assert_eq!(encoded.len(), 5);
}

#[test]
fn encode_null_pic_9_produces_output() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let encoded = encode_record(&schema, &json!({"F": null}), &ascii_enc()).unwrap();
    // Null numeric produces 5-byte output
    assert_eq!(encoded.len(), 5);
}

#[test]
fn encode_null_comp3_produces_output() {
    let schema = parse_copybook("01 REC.\n   05 F PIC S9(3) COMP-3.").unwrap();
    let encoded = encode_record(&schema, &json!({"F": null}), &ascii_enc()).unwrap();
    // S9(3) COMP-3: 3+1=4 nibbles = 2 bytes
    assert_eq!(encoded.len(), 2);
}

// ===========================================================================
// 6. Truncation / overflow behavior
// ===========================================================================

#[test]
fn encode_alpha_overflow_rejected_strict() {
    let result = encode_alphanumeric("TOOLONG", 3, Codepage::ASCII);
    assert!(result.is_err(), "should reject text exceeding field length");
}

#[test]
fn encode_display_overflow_rejected() {
    let result = encode_zoned_decimal("100000", 5, 0, false, Codepage::ASCII);
    assert!(
        result.is_err(),
        "6-digit value should overflow 5-digit field"
    );
}

#[test]
fn encode_record_alpha_overflow_strict() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(3).").unwrap();
    let result = encode_record(&schema, &json!({"F": "ABCDEFGH"}), &ascii_enc());
    assert!(result.is_err());
}

// ===========================================================================
// 7. Error cases: type mismatch, invalid values
// ===========================================================================

#[test]
fn encode_numeric_field_with_non_numeric_string() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let result = encode_record(&schema, &json!({"F": "ABCDE"}), &ascii_enc());
    assert!(result.is_err());
}

#[test]
fn encode_comp3_invalid_string() {
    let result = encode_packed_decimal("ABC", 3, 0, true);
    assert!(result.is_err());
}

#[test]
fn encode_roundtrip_display_matches() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(5).").unwrap();
    let original = b"00042";
    let json = decode_record(&schema, original, &ascii_dec()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_enc()).unwrap();
    assert_eq!(encoded, original);
}

#[test]
fn encode_roundtrip_comp3_matches() {
    let schema = parse_copybook("01 REC.\n   05 F PIC S9(5) COMP-3.").unwrap();
    let original: &[u8] = &[0x12, 0x34, 0x5C];
    let json = decode_record(&schema, original, &ascii_dec()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_enc()).unwrap();
    assert_eq!(encoded, original);
}

#[test]
fn encode_roundtrip_comp_binary_matches() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(4) COMP.").unwrap();
    let original: &[u8] = &[0x04, 0xD2]; // 1234
    let json = decode_record(&schema, original, &ascii_dec()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_enc()).unwrap();
    assert_eq!(encoded, original);
}

#[test]
fn encode_ebcdic_display_roundtrip() {
    let schema = parse_copybook("01 REC.\n   05 F PIC 9(3).").unwrap();
    let original: &[u8] = &[0xF0, 0xF4, 0xF2]; // "042" in EBCDIC
    let json = decode_record(&schema, original, &ebcdic_dec()).unwrap();
    let encoded = encode_record(&schema, &json, &ebcdic_enc()).unwrap();
    assert_eq!(encoded, original);
}

#[test]
fn encode_ebcdic_alpha_roundtrip() {
    let schema = parse_copybook("01 REC.\n   05 F PIC X(5).").unwrap();
    let data =
        copybook_charset::utf8_to_ebcdic("HELLO", copybook_charset::Codepage::CP037).unwrap();
    let json = decode_record(&schema, &data, &ebcdic_dec()).unwrap();
    let encoded = encode_record(&schema, &json, &ebcdic_enc()).unwrap();
    assert_eq!(encoded, data);
}

#[test]
fn encode_multi_field_record() {
    let cpy = r"
        01 REC.
           05 ID     PIC 9(5).
           05 NAME   PIC X(10).
           05 AMOUNT PIC 9(7).
    ";
    let schema = parse_copybook(cpy).unwrap();
    let original = b"00042ALICE     0001234";
    let json = decode_record(&schema, original, &ascii_dec()).unwrap();
    let encoded = encode_record(&schema, &json, &ascii_enc()).unwrap();
    assert_eq!(encoded, original);
}
