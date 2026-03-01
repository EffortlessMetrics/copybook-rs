// SPDX-License-Identifier: AGPL-3.0-or-later
//! Numeric edge-case tests — structural beams for numeric fidelity.
//!
//! Covers COMP-3 (packed decimal), overpunch (zoned decimal), COMP (binary
//! integer), and COMP-1/COMP-2 (floating point) boundaries where decode/encode
//! correctness matters most.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;

// =============================================================================
// Helpers
// =============================================================================

fn decode_opts() -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: FloatFormat::IeeeBigEndian,
    }
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        on_encode_unmappable: UnmappablePolicy::Error,
        json_number_mode: JsonNumberMode::Lossless,
        zoned_encoding_override: None,
        float_format: FloatFormat::IeeeBigEndian,
    }
}

fn decode_opts_native() -> DecodeOptions {
    DecodeOptions {
        json_number_mode: JsonNumberMode::Native,
        ..decode_opts()
    }
}

fn encode_opts_native() -> EncodeOptions {
    EncodeOptions {
        json_number_mode: JsonNumberMode::Native,
        ..encode_opts()
    }
}

// =============================================================================
// 1. Single-byte COMP-3 (PIC S9 COMP-3) — values -9 to +9
// =============================================================================

#[test]
fn test_comp3_single_byte_positive_values() {
    // PIC S9 COMP-3 → 1 byte: high nibble = digit, low nibble = sign
    let copybook = "01 VAL PIC S9 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    for digit in 0u8..=9 {
        // Positive: sign nibble 0xC
        let data = [digit << 4 | 0x0C];
        let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
        assert_eq!(
            result["VAL"],
            serde_json::json!(digit.to_string()),
            "positive digit {digit}"
        );
    }
}

#[test]
fn test_comp3_single_byte_negative_values() {
    let copybook = "01 VAL PIC S9 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    for digit in 1u8..=9 {
        // Negative: sign nibble 0xD
        let data = [digit << 4 | 0x0D];
        let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
        assert_eq!(
            result["VAL"],
            serde_json::json!(format!("-{digit}")),
            "negative digit {digit}"
        );
    }
}

// =============================================================================
// 2. Maximum-digits COMP-3 (PIC S9(18) COMP-3) — boundary values
// =============================================================================

#[test]
fn test_comp3_max_positive_18_digits() {
    // PIC S9(18) COMP-3 → 10 bytes (18 digits + sign nibble, packed)
    // 999999999999999999 → 0x99 0x99 0x99 0x99 0x99 0x99 0x99 0x99 0x99 0x9C
    let copybook = "01 VAL PIC S9(18) COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 10] = [0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("999999999999999999"));
}

#[test]
fn test_comp3_max_negative_18_digits() {
    // -999999999999999999 → same digits, sign nibble 0xD
    let copybook = "01 VAL PIC S9(18) COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 10] = [0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9D];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-999999999999999999"));
}

// =============================================================================
// 3. COMP-3 zero with various sign nibbles
// =============================================================================

#[test]
fn test_comp3_zero_positive_sign() {
    // 0C → +0, should normalize to "0"
    let copybook = "01 VAL PIC S9 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let data = [0x0C];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("0"));
}

#[test]
fn test_comp3_zero_negative_sign() {
    // 0D → -0, should still decode to "0" (negative zero normalizes)
    let copybook = "01 VAL PIC S9 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let data = [0x0D];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("0"));
}

#[test]
fn test_comp3_zero_unsigned_sign() {
    // 0F → unsigned zero
    let copybook = "01 VAL PIC 9 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let data = [0x0F];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("0"));
}

// =============================================================================
// 4. COMP-3 with decimal scaling (PIC S9(5)V99 COMP-3)
// =============================================================================

#[test]
fn test_comp3_decimal_scaling() {
    // PIC S9(5)V99 COMP-3 → 4 bytes (7 digits + sign nibble)
    // 7 digits (odd) + sign = 8 nibbles = 4 bytes
    let copybook = "01 AMT PIC S9(5)V99 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    // 12345.67 → integer repr 1234567, 7 digits (odd) + sign = 8 nibbles = 4 bytes
    // Nibbles: [1][2] [3][4] [5][6] [7][C] → 0x12 0x34 0x56 0x7C
    let data: [u8; 4] = [0x12, 0x34, 0x56, 0x7C];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["AMT"], serde_json::json!("12345.67"));
}

#[test]
fn test_comp3_decimal_scaling_negative() {
    let copybook = "01 AMT PIC S9(5)V99 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    // -00001.50 → integer repr 0000150, packed: 0x00 0x00 0x15 0x0D
    let data: [u8; 4] = [0x00, 0x00, 0x15, 0x0D];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["AMT"], serde_json::json!("-1.50"));
}

#[test]
fn test_comp3_decimal_scaling_roundtrip() {
    let copybook = "01 AMT PIC S9(5)V99 COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "AMT": "12345.67" });
    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    assert_eq!(encoded, vec![0x12, 0x34, 0x56, 0x7C]);

    let decoded = decode_record(&schema, &encoded, &decode_opts()).expect("decode");
    assert_eq!(decoded["AMT"], serde_json::json!("12345.67"));
}

// =============================================================================
// 5. Unsigned COMP-3 (PIC 9(5) COMP-3)
// =============================================================================

#[test]
fn test_comp3_unsigned() {
    // PIC 9(5) COMP-3 → 3 bytes (5 digits + sign nibble F)
    // 5 digits (odd) + sign = 6 nibbles = 3 bytes
    let copybook = "01 VAL PIC 9(5) COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    // 12345 → nibbles: [1][2] [3][4] [5][F] → 0x12 0x34 0x5F
    let data: [u8; 3] = [0x12, 0x34, 0x5F];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("12345"));
}

#[test]
fn test_comp3_unsigned_zero() {
    let copybook = "01 VAL PIC 9(5) COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 3] = [0x00, 0x00, 0x0F];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("0"));
}

#[test]
fn test_comp3_unsigned_roundtrip() {
    let copybook = "01 VAL PIC 9(5) COMP-3.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "12345" });
    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts()).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("12345"));
}

// =============================================================================
// 6. Overpunch positive chars: {, A-I → 0-9 positive (EBCDIC)
// =============================================================================

#[test]
fn test_overpunch_positive_all_digits_ebcdic() {
    // EBCDIC overpunch positive: zone 0xC for last byte
    // Digit 0 → 0xC0, Digit 1 → 0xC1, ..., Digit 9 → 0xC9
    let copybook = "01 VAL PIC S9.";
    let schema = parse_copybook(copybook).expect("parse");

    for digit in 0u8..=9 {
        let data = [0xC0 + digit]; // EBCDIC positive overpunch
        let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
        assert_eq!(
            result["VAL"],
            serde_json::json!(digit.to_string()),
            "positive overpunch digit {digit}"
        );
    }
}

// =============================================================================
// 7. Overpunch negative chars: zone 0xD → 0-9 negative (EBCDIC)
// =============================================================================

#[test]
fn test_overpunch_negative_all_digits_ebcdic() {
    let copybook = "01 VAL PIC S9.";
    let schema = parse_copybook(copybook).expect("parse");

    for digit in 1u8..=9 {
        let data = [0xD0 + digit]; // EBCDIC negative overpunch
        let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
        assert_eq!(
            result["VAL"],
            serde_json::json!(format!("-{digit}")),
            "negative overpunch digit {digit}"
        );
    }
    // Negative zero normalizes to "0"
    let data = [0xD0];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("0"));
}

// =============================================================================
// 8. DISPLAY numeric field PIC S9(5) with overpunch last byte
// =============================================================================

#[test]
fn test_display_signed_overpunch_positive() {
    // EBCDIC PIC S9(5): digits 0xF1 0xF2 0xF3 0xF4 + last byte overpunch 0xC5 → +12345
    let copybook = "01 VAL PIC S9(5).";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 5] = [0xF1, 0xF2, 0xF3, 0xF4, 0xC5];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("12345"));
}

#[test]
fn test_display_signed_overpunch_negative() {
    // Last byte negative overpunch 0xD5 → -12345
    let copybook = "01 VAL PIC S9(5).";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 5] = [0xF1, 0xF2, 0xF3, 0xF4, 0xD5];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-12345"));
}

#[test]
fn test_display_signed_overpunch_roundtrip() {
    let copybook = "01 VAL PIC S9(5).";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 5] = [0xF1, 0xF2, 0xF3, 0xF4, 0xC5]; // +12345
    let decoded = decode_record(
        &schema,
        &data,
        &DecodeOptions {
            preserve_zoned_encoding: true,
            ..decode_opts()
        },
    )
    .expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("12345"));

    let re_encoded = encode_record(
        &schema,
        &decoded,
        &EncodeOptions {
            zoned_encoding_override: Some(ZonedEncodingFormat::Ebcdic),
            ..encode_opts()
        },
    )
    .expect("encode");
    assert_eq!(re_encoded, data);
}

// =============================================================================
// 9. SIGN SEPARATE LEADING — explicit +/- in first byte
// =============================================================================

#[test]
fn test_sign_separate_leading_positive() {
    // EBCDIC '+' = 0x4E, digits 0xF1..0xF5
    let copybook = "01 VAL PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 6] = [0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("12345"));
}

#[test]
fn test_sign_separate_leading_negative() {
    // EBCDIC '-' = 0x60
    let copybook = "01 VAL PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 6] = [0x60, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-12345"));
}

#[test]
fn test_sign_separate_leading_roundtrip() {
    let copybook = "01 VAL PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "-99999" });
    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    // EBCDIC '-' + 0xF9*5
    assert_eq!(encoded, vec![0x60, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9]);

    let decoded = decode_record(&schema, &encoded, &decode_opts()).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("-99999"));
}

// =============================================================================
// 10. SIGN SEPARATE TRAILING — explicit +/- in last byte
// =============================================================================

#[test]
fn test_sign_separate_trailing_positive() {
    let copybook = "01 VAL PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 6] = [0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0x4E];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("12345"));
}

#[test]
fn test_sign_separate_trailing_negative() {
    let copybook = "01 VAL PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 6] = [0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0x60];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-12345"));
}

#[test]
fn test_sign_separate_trailing_roundtrip() {
    let copybook = "01 VAL PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "54321" });
    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    assert_eq!(encoded, vec![0xF5, 0xF4, 0xF3, 0xF2, 0xF1, 0x4E]);

    let decoded = decode_record(&schema, &encoded, &decode_opts()).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("54321"));
}

// =============================================================================
// 11. COMP halfword (PIC S9(4) COMP) — 2 bytes, signed, big-endian
// =============================================================================

#[test]
fn test_comp_halfword_positive() {
    let copybook = "01 VAL PIC S9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // 1234 in big-endian i16 → 0x04D2
    let data: [u8; 2] = [0x04, 0xD2];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("1234"));
}

#[test]
fn test_comp_halfword_negative() {
    let copybook = "01 VAL PIC S9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // -1 in big-endian i16 → 0xFFFF
    let data: [u8; 2] = [0xFF, 0xFF];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-1"));
}

#[test]
fn test_comp_halfword_max() {
    let copybook = "01 VAL PIC S9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // i16::MAX = 32767 → 0x7FFF
    let data: [u8; 2] = [0x7F, 0xFF];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("32767"));
}

#[test]
fn test_comp_halfword_min() {
    let copybook = "01 VAL PIC S9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // i16::MIN = -32768 → 0x8000
    let data: [u8; 2] = [0x80, 0x00];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-32768"));
}

#[test]
fn test_comp_halfword_zero() {
    let copybook = "01 VAL PIC S9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 2] = [0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("0"));
}

// =============================================================================
// 12. COMP fullword (PIC S9(9) COMP) — 4 bytes, signed, big-endian
// =============================================================================

#[test]
fn test_comp_fullword_positive() {
    let copybook = "01 VAL PIC S9(9) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // 123456789 → 0x075BCD15
    let data: [u8; 4] = [0x07, 0x5B, 0xCD, 0x15];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("123456789"));
}

#[test]
fn test_comp_fullword_max() {
    let copybook = "01 VAL PIC S9(9) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // i32::MAX = 2147483647 → 0x7FFFFFFF
    let data: [u8; 4] = [0x7F, 0xFF, 0xFF, 0xFF];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("2147483647"));
}

#[test]
fn test_comp_fullword_min() {
    let copybook = "01 VAL PIC S9(9) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // i32::MIN = -2147483648 → 0x80000000
    let data: [u8; 4] = [0x80, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-2147483648"));
}

// =============================================================================
// 13. COMP doubleword (PIC S9(18) COMP) — 8 bytes, signed, big-endian
// =============================================================================

#[test]
fn test_comp_doubleword_positive() {
    let copybook = "01 VAL PIC S9(18) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // 1 → 0x0000000000000001
    let data: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("1"));
}

#[test]
fn test_comp_doubleword_max() {
    let copybook = "01 VAL PIC S9(18) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // i64::MAX = 9223372036854775807 → 0x7FFFFFFFFFFFFFFF
    let data: [u8; 8] = [0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("9223372036854775807"));
}

#[test]
fn test_comp_doubleword_min() {
    let copybook = "01 VAL PIC S9(18) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // i64::MIN = -9223372036854775808 → 0x8000000000000000
    let data: [u8; 8] = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("-9223372036854775808"));
}

#[test]
fn test_comp_doubleword_roundtrip() {
    let copybook = "01 VAL PIC S9(18) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "123456789012345678" });
    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    assert_eq!(encoded.len(), 8);

    let decoded = decode_record(&schema, &encoded, &decode_opts()).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("123456789012345678"));
}

// =============================================================================
// 14. Unsigned COMP (PIC 9(4) COMP) — unsigned interpretation
// =============================================================================

#[test]
fn test_comp_unsigned_halfword() {
    let copybook = "01 VAL PIC 9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // 9999 → 0x270F
    let data: [u8; 2] = [0x27, 0x0F];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("9999"));
}

#[test]
fn test_comp_unsigned_halfword_max_u16() {
    let copybook = "01 VAL PIC 9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    // 65535 → 0xFFFF (would be -1 if signed)
    let data: [u8; 2] = [0xFF, 0xFF];
    let result = decode_record(&schema, &data, &decode_opts()).expect("decode");
    assert_eq!(result["VAL"], serde_json::json!("65535"));
}

#[test]
fn test_comp_unsigned_roundtrip() {
    let copybook = "01 VAL PIC 9(4) COMP.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "VAL": "9999" });
    let encoded = encode_record(&schema, &json, &encode_opts()).expect("encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts()).expect("decode");
    assert_eq!(decoded["VAL"], serde_json::json!("9999"));
}

// =============================================================================
// 15. COMP-1 (single precision float) — round-trip through JSON
// =============================================================================

#[test]
fn test_comp1_roundtrip_1_0() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "RATE": 1.0_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_native()).expect("encode");
    assert_eq!(encoded, vec![0x3F, 0x80, 0x00, 0x00]); // IEEE 754 f32 1.0

    let decoded = decode_record(&schema, &encoded, &decode_opts_native()).expect("decode");
    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0_f64).abs() < f64::EPSILON);
}

#[test]
#[allow(clippy::approx_constant)]
fn test_comp1_lossy_value() {
    // 0.1 cannot be exactly represented in f32; verify we get close
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "RATE": 0.1_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_native()).expect("encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_native()).expect("decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    // f32 precision: ~7 significant digits; 0.1 rounds to ~0.100000001490116
    assert!((val - 0.1_f64).abs() < 1e-6);
}

// =============================================================================
// 16. COMP-2 (double precision float) — round-trip through JSON
// =============================================================================

#[test]
fn test_comp2_roundtrip_pi() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("parse");

    // IEEE 754 f64 for PI: 0x400921FB54442D18
    let data: [u8; 8] = [0x40, 0x09, 0x21, 0xFB, 0x54, 0x44, 0x2D, 0x18];
    let decoded = decode_record(&schema, &data, &decode_opts_native()).expect("decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - std::f64::consts::PI).abs() < 1e-14);

    // Re-encode and verify byte identity
    let re_encoded = encode_record(&schema, &decoded, &encode_opts_native()).expect("encode");
    assert_eq!(re_encoded, data);
}

#[test]
fn test_comp2_large_negative() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "RATE": -1.23e100_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_native()).expect("encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_native()).expect("decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - (-1.23e100_f64)).abs() < 1e88);
}

// =============================================================================
// 17. COMP-1 special values: zero, negative zero, max, min normal
// =============================================================================

#[test]
fn test_comp1_zero() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x00]; // +0.0
    let result = decode_record(&schema, &data, &decode_opts_native()).expect("decode");
    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn test_comp1_negative_zero() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("parse");

    // IEEE 754 -0.0 f32: 0x80000000
    let data: [u8; 4] = [0x80, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_native()).expect("decode");
    let val = result["RATE"].as_f64().expect("should be number");
    // -0.0 == 0.0 in IEEE 754
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn test_comp1_max_finite() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("parse");

    // f32::MAX = 3.40282347e+38 → 0x7F7FFFFF
    let data: [u8; 4] = [0x7F, 0x7F, 0xFF, 0xFF];
    let result = decode_record(&schema, &data, &decode_opts_native()).expect("decode");
    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val > 3.4e38 && val.is_finite());
}

#[test]
fn test_comp1_min_normal() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("parse");

    // f32::MIN_POSITIVE = 1.17549435e-38 → 0x00800000
    let data: [u8; 4] = [0x00, 0x80, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_native()).expect("decode");
    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val > 0.0 && val < 1.2e-38);
}

// =============================================================================
// 18. COMP-2 special values: zero, subnormal boundary
// =============================================================================

#[test]
fn test_comp2_zero() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("parse");

    let data: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_native()).expect("decode");
    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn test_comp2_min_positive_normal() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("parse");

    // f64::MIN_POSITIVE = 2.2250738585072014e-308 → 0x0010000000000000
    let data: [u8; 8] = [0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_native()).expect("decode");
    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val > 0.0 && val < 2.3e-308);
}

#[test]
fn test_comp2_subnormal_boundary() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("parse");

    // Largest subnormal f64: 0x000FFFFFFFFFFFFF ≈ 2.225e-308 (just below MIN_POSITIVE)
    let data: [u8; 8] = [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
    let result = decode_record(&schema, &data, &decode_opts_native()).expect("decode");
    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val > 0.0 && val < f64::MIN_POSITIVE);
}

#[test]
fn test_comp2_max_finite_roundtrip() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("parse");

    let json = serde_json::json!({ "RATE": f64::MAX });
    let encoded = encode_record(&schema, &json, &encode_opts_native()).expect("encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_native()).expect("decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!(val.is_finite() && val > 0.0);
    assert!((val - f64::MAX).abs() < 1e292);
}
