//! Golden fixture tests for SIGN LEADING/TRAILING SEPARATE encode/decode.
//!
//! Validates full-stack encode and decode of SIGN SEPARATE fields through the
//! schema parser and codec, including round-trip fidelity for both LEADING and
//! TRAILING placements.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    ZonedEncodingFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;

fn decode_opts_cp037() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(true)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Auto)
}

fn encode_opts_cp037() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_use_raw(false)
        .with_bwz_encode(false)
        .with_strict_mode(true)
        .with_coerce_numbers(false)
        .with_preferred_zoned_encoding(ZonedEncodingFormat::Auto)
}

// =============================================================================
// SIGN LEADING SEPARATE decode golden tests
// =============================================================================

#[test]
fn test_golden_sign_leading_positive_decode() {
    // 01 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.
    // Binary: EBCDIC '+' (0x4E) then digits 0xF0 0xF0 0xF1 0xF2 0xF3 -> "00123"
    // Lossless mode preserves leading zeros (returns "00123" not "123")
    let copybook = "01 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: &[u8] = &[0x4E, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    // Lossless mode: leading zeros are preserved in the string representation
    assert_eq!(result["AMOUNT"], serde_json::json!("00123"));
}

#[test]
fn test_golden_sign_leading_negative_decode() {
    // EBCDIC '-' (0x60) then digits for 00456
    let copybook = "01 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: &[u8] = &[0x60, 0xF0, 0xF0, 0xF4, 0xF5, 0xF6];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    // Lossless mode: leading zeros are preserved in the string representation
    assert_eq!(result["AMOUNT"], serde_json::json!("-00456"));
}

#[test]
fn test_golden_sign_leading_zero_decode() {
    // EBCDIC '+' (0x4E) then all zero digits
    let copybook = "01 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: &[u8] = &[0x4E, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    assert_eq!(result["AMOUNT"], serde_json::json!("0"));
}

// =============================================================================
// SIGN TRAILING SEPARATE decode golden tests
// =============================================================================

#[test]
fn test_golden_sign_trailing_positive_decode() {
    // 01 AMOUNT PIC S9(5) SIGN TRAILING SEPARATE.
    // Binary: digits 0xF0 0xF0 0xF7 0xF8 0xF9 then EBCDIC '+' (0x4E) -> "00789"
    // Lossless mode preserves leading zeros
    let copybook = "01 AMOUNT PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: &[u8] = &[0xF0, 0xF0, 0xF7, 0xF8, 0xF9, 0x4E];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    assert_eq!(result["AMOUNT"], serde_json::json!("00789"));
}

#[test]
fn test_golden_sign_trailing_negative_decode() {
    // digits 0xF0 0xF1 0xF0 0xF0 0xF0 then EBCDIC '-' (0x60) -> "-01000"
    let copybook = "01 AMOUNT PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: &[u8] = &[0xF0, 0xF1, 0xF0, 0xF0, 0xF0, 0x60];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    assert_eq!(result["AMOUNT"], serde_json::json!("-01000"));
}

#[test]
fn test_golden_sign_trailing_zero_decode() {
    // digits all 0xF0 then EBCDIC '+' (0x4E) -> "0"
    let copybook = "01 AMOUNT PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: &[u8] = &[0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0x4E];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    assert_eq!(result["AMOUNT"], serde_json::json!("0"));
}

// =============================================================================
// Decimal scale decode golden tests
// =============================================================================

#[test]
fn test_golden_sign_leading_with_scale_decode() {
    // PIC S9(5)V99 SIGN IS LEADING SEPARATE. Total: 1 sign + 7 digits = 8 bytes
    // EBCDIC '+', then digits for 12345.67
    let copybook = "01 AMOUNT PIC S9(5)V99 SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    // sign(+) + 1 2 3 4 5 6 7 -> "12345.67"
    let data: &[u8] = &[0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    assert_eq!(result["AMOUNT"], serde_json::json!("12345.67"));
}

#[test]
fn test_golden_sign_trailing_with_scale_negative_decode() {
    // PIC S9(5)V99 SIGN TRAILING SEPARATE. Total: 7 digits + 1 sign = 8 bytes
    // digits for -99999.99
    let copybook = "01 AMOUNT PIC S9(5)V99 SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: &[u8] = &[0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9, 0x60];
    let result = decode_record(&schema, data, &decode_opts_cp037()).expect("should decode");

    assert_eq!(result["AMOUNT"], serde_json::json!("-99999.99"));
}

// =============================================================================
// Round-trip golden tests (encode then decode)
// =============================================================================

#[test]
fn test_golden_sign_leading_roundtrip_positive() {
    let copybook = "01 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "12345" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");

    // EBCDIC '+' (0x4E) + 0xF1 0xF2 0xF3 0xF4 0xF5
    assert_eq!(encoded, vec![0x4E, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5]);

    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");
    assert_eq!(decoded["AMOUNT"], serde_json::json!("12345"));
}

#[test]
fn test_golden_sign_leading_roundtrip_negative() {
    let copybook = "01 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "-99999" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");

    // EBCDIC '-' (0x60) + 0xF9 0xF9 0xF9 0xF9 0xF9
    assert_eq!(encoded, vec![0x60, 0xF9, 0xF9, 0xF9, 0xF9, 0xF9]);

    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");
    assert_eq!(decoded["AMOUNT"], serde_json::json!("-99999"));
}

#[test]
fn test_golden_sign_trailing_roundtrip_positive() {
    let copybook = "01 AMOUNT PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "54321" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");

    // 0xF5 0xF4 0xF3 0xF2 0xF1 + EBCDIC '+' (0x4E)
    assert_eq!(encoded, vec![0xF5, 0xF4, 0xF3, 0xF2, 0xF1, 0x4E]);

    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");
    assert_eq!(decoded["AMOUNT"], serde_json::json!("54321"));
}

#[test]
fn test_golden_sign_trailing_roundtrip_negative() {
    let copybook = "01 AMOUNT PIC S9(5) SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "-10000" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");

    // 0xF1 0xF0 0xF0 0xF0 0xF0 + EBCDIC '-' (0x60)
    assert_eq!(encoded, vec![0xF1, 0xF0, 0xF0, 0xF0, 0xF0, 0x60]);

    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");
    assert_eq!(decoded["AMOUNT"], serde_json::json!("-10000"));
}

#[test]
fn test_golden_sign_leading_with_scale_roundtrip() {
    let copybook = "01 AMOUNT PIC S9(5)V99 SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "12345.67" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");

    assert_eq!(decoded["AMOUNT"], serde_json::json!("12345.67"));
}

#[test]
fn test_golden_sign_trailing_with_scale_roundtrip() {
    let copybook = "01 AMOUNT PIC S9(5)V99 SIGN TRAILING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "-99999.99" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");

    assert_eq!(decoded["AMOUNT"], serde_json::json!("-99999.99"));
}

// =============================================================================
// Edge cases: max/min values
// =============================================================================

#[test]
fn test_golden_sign_leading_max_value_s9_9() {
    let copybook = "01 AMOUNT PIC S9(9) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "999999999" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");

    assert_eq!(decoded["AMOUNT"], serde_json::json!("999999999"));
}

#[test]
fn test_golden_sign_leading_min_value_s9_9() {
    let copybook = "01 AMOUNT PIC S9(9) SIGN IS LEADING SEPARATE.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "AMOUNT": "-999999999" });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");

    assert_eq!(decoded["AMOUNT"], serde_json::json!("-999999999"));
}

// =============================================================================
// Record layout: SIGN SEPARATE within a group
// =============================================================================

#[test]
fn test_golden_sign_separate_in_group_roundtrip() {
    let copybook = r"
01 RECORD.
   05 ID      PIC 9(4).
   05 BALANCE PIC S9(7)V99 SIGN IS LEADING SEPARATE.
   05 STATUS  PIC X(1).
";
    let schema = parse_copybook(copybook).expect("should parse");

    // Record len: 4 (ID) + 10 (BALANCE: 1 sign + 9 digits) + 1 (STATUS) = 15 bytes
    // decode_record returns a flat JSON map keyed by field name (not nested under group name)
    let json = serde_json::json!({
        "ID": "1234",
        "BALANCE": "1234567.89",
        "STATUS": "A"
    });
    let encoded = encode_record(&schema, &json, &encode_opts_cp037()).expect("should encode");
    assert_eq!(encoded.len(), 15);

    let decoded = decode_record(&schema, &encoded, &decode_opts_cp037()).expect("should decode");
    assert_eq!(decoded["ID"], serde_json::json!("1234"));
    assert_eq!(decoded["BALANCE"], serde_json::json!("1234567.89"));
    assert_eq!(decoded["STATUS"], serde_json::json!("A"));
}
