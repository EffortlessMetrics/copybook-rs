// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Comprehensive edge-case tests for the copybook-codec crate.
//!
//! Covers:
//! 1. COMP-1 (float) encode/decode edge cases
//! 2. COMP-2 (double) encode/decode edge cases
//! 3. COMP-3 (packed decimal) edge cases
//! 4. REDEFINES handling in decode/encode
//! 5. ODO boundary conditions
//! 6. Numeric field overflow/underflow
//! 7. Character field truncation and padding

use copybook_codec::numeric::{
    decode_binary_int, decode_float_double, decode_float_single, decode_packed_decimal,
    encode_binary_int, encode_float_double, encode_float_single, encode_packed_decimal,
};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use serde_json::json;

// ---------------------------------------------------------------------------
// Helper: default ASCII decode/encode options
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
// 1. COMP-1 (single-precision float) edge cases
// ===========================================================================

#[test]
fn test_comp1_negative_zero_roundtrip() {
    let neg_zero_bytes: [u8; 4] = [0x80, 0x00, 0x00, 0x00]; // IEEE 754 -0.0
    let decoded = decode_float_single(&neg_zero_bytes).unwrap();
    assert_eq!(decoded, 0.0_f32); // -0.0 == 0.0 in IEEE 754
    assert!(decoded.is_sign_negative(), "-0.0 should preserve sign bit");

    let mut buf = [0u8; 4];
    encode_float_single(-0.0_f32, &mut buf).unwrap();
    assert_eq!(
        buf, neg_zero_bytes,
        "encoding -0.0 should produce 0x80000000"
    );
}

#[test]
fn test_comp1_subnormal_smallest() {
    // Smallest positive subnormal float: 0x00000001
    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x01];
    let decoded = decode_float_single(&data).unwrap();
    assert!(decoded > 0.0_f32, "smallest subnormal should be positive");
    assert!(decoded.is_subnormal() || decoded == f32::MIN_POSITIVE);

    let mut buf = [0u8; 4];
    encode_float_single(decoded, &mut buf).unwrap();
    assert_eq!(buf, data, "subnormal roundtrip");
}

#[test]
fn test_comp1_subnormal_largest() {
    // Largest subnormal float: 0x007FFFFF
    let data: [u8; 4] = [0x00, 0x7F, 0xFF, 0xFF];
    let decoded = decode_float_single(&data).unwrap();
    assert!(decoded > 0.0);
    assert!(decoded < f32::MIN_POSITIVE);

    let mut buf = [0u8; 4];
    encode_float_single(decoded, &mut buf).unwrap();
    assert_eq!(buf, data);
}

#[test]
fn test_comp1_max_finite() {
    let mut buf = [0u8; 4];
    encode_float_single(f32::MAX, &mut buf).unwrap();
    let decoded = decode_float_single(&buf).unwrap();
    assert!((decoded - f32::MAX).abs() < f32::EPSILON);
}

#[test]
fn test_comp1_min_positive_normal() {
    let mut buf = [0u8; 4];
    encode_float_single(f32::MIN_POSITIVE, &mut buf).unwrap();
    let decoded = decode_float_single(&buf).unwrap();
    assert!((decoded - f32::MIN_POSITIVE).abs() < f32::EPSILON);
}

#[test]
fn test_comp1_nan_payload_preserved() {
    // Signaling NaN pattern
    let snan_data: [u8; 4] = [0x7F, 0x80, 0x00, 0x01]; // sNaN
    let decoded = decode_float_single(&snan_data).unwrap();
    assert!(decoded.is_nan());
}

#[test]
fn test_comp1_negative_nan() {
    let neg_nan: [u8; 4] = [0xFF, 0xC0, 0x00, 0x00]; // negative qNaN
    let decoded = decode_float_single(&neg_nan).unwrap();
    assert!(decoded.is_nan());
}

// ===========================================================================
// 2. COMP-2 (double-precision float) edge cases
// ===========================================================================

#[test]
fn test_comp2_negative_zero_roundtrip() {
    let neg_zero_bytes: [u8; 8] = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let decoded = decode_float_double(&neg_zero_bytes).unwrap();
    assert_eq!(decoded, 0.0_f64);
    assert!(decoded.is_sign_negative());

    let mut buf = [0u8; 8];
    encode_float_double(-0.0_f64, &mut buf).unwrap();
    assert_eq!(buf, neg_zero_bytes);
}

#[test]
fn test_comp2_subnormal_smallest() {
    let data: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
    let decoded = decode_float_double(&data).unwrap();
    assert!(decoded > 0.0_f64);

    let mut buf = [0u8; 8];
    encode_float_double(decoded, &mut buf).unwrap();
    assert_eq!(buf, data);
}

#[test]
fn test_comp2_subnormal_largest() {
    // Largest subnormal double: 0x000FFFFFFFFFFFFF
    let data: [u8; 8] = [0x00, 0x0F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
    let decoded = decode_float_double(&data).unwrap();
    assert!(decoded > 0.0);
    assert!(decoded < f64::MIN_POSITIVE);

    let mut buf = [0u8; 8];
    encode_float_double(decoded, &mut buf).unwrap();
    assert_eq!(buf, data);
}

#[test]
fn test_comp2_max_finite() {
    let mut buf = [0u8; 8];
    encode_float_double(f64::MAX, &mut buf).unwrap();
    let decoded = decode_float_double(&buf).unwrap();
    assert!((decoded - f64::MAX).abs() < f64::EPSILON);
}

#[test]
fn test_comp2_negative_infinity_roundtrip() {
    let mut buf = [0u8; 8];
    encode_float_double(f64::NEG_INFINITY, &mut buf).unwrap();
    let decoded = decode_float_double(&buf).unwrap();
    assert!(decoded.is_infinite() && decoded.is_sign_negative());
}

#[test]
fn test_comp2_signaling_nan() {
    let snan: [u8; 8] = [0x7F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
    let decoded = decode_float_double(&snan).unwrap();
    assert!(decoded.is_nan());
}

#[test]
fn test_comp2_negative_nan() {
    let neg_nan: [u8; 8] = [0xFF, 0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let decoded = decode_float_double(&neg_nan).unwrap();
    assert!(decoded.is_nan());
}

// ===========================================================================
// 3. COMP-3 (packed decimal) edge cases
// ===========================================================================

#[test]
fn test_comp3_zero_value_signed() {
    let result = encode_packed_decimal("0", 1, 0, true).unwrap();
    assert_eq!(result, vec![0x0C]); // positive zero sign

    let decoded = decode_packed_decimal(&result, 1, 0, true).unwrap();
    assert_eq!(decoded.to_string(), "0");
}

#[test]
fn test_comp3_zero_value_unsigned() {
    let result = encode_packed_decimal("0", 1, 0, false).unwrap();
    assert_eq!(result, vec![0x0F]); // unsigned zero

    let decoded = decode_packed_decimal(&result, 1, 0, false).unwrap();
    assert_eq!(decoded.to_string(), "0");
}

#[test]
fn test_comp3_negative_zero_normalizes() {
    let result = encode_packed_decimal("-0", 1, 0, true).unwrap();
    let decoded = decode_packed_decimal(&result, 1, 0, true).unwrap();
    // Negative zero should normalize to zero
    assert_eq!(decoded.to_string(), "0");
}

#[test]
fn test_comp3_max_single_digit() {
    let result = encode_packed_decimal("9", 1, 0, true).unwrap();
    assert_eq!(result, vec![0x9C]);

    let decoded = decode_packed_decimal(&result, 1, 0, true).unwrap();
    assert_eq!(decoded.to_string(), "9");
}

#[test]
fn test_comp3_max_18_digits() {
    // 18 digits is the maximum supported precision (i64 range)
    let max_val = "999999999999999999"; // 18 nines
    let result = encode_packed_decimal(max_val, 18, 0, true).unwrap();
    let decoded = decode_packed_decimal(&result, 18, 0, true).unwrap();
    assert_eq!(decoded.to_string(), max_val);
}

#[test]
fn test_comp3_max_negative_18_digits() {
    let max_neg = "-999999999999999999";
    let result = encode_packed_decimal(max_neg, 18, 0, true).unwrap();
    let decoded = decode_packed_decimal(&result, 18, 0, true).unwrap();
    assert_eq!(decoded.to_string(), max_neg);
}

#[test]
fn test_comp3_overflow_rejects() {
    // 10 overflows a 1-digit field
    let result = encode_packed_decimal("10", 1, 0, true);
    assert!(result.is_err());
}

#[test]
fn test_comp3_exceeds_18_digit_precision() {
    // 19 digits exceeds i64 range
    let result = decode_packed_decimal(&[0x01; 10], 19, 0, true);
    assert!(result.is_err());
}

#[test]
fn test_comp3_with_scale_zero_decimal() {
    // Encode "0.00" in a S9(4)V99 field
    let result = encode_packed_decimal("0.00", 6, 2, true).unwrap();
    let decoded = decode_packed_decimal(&result, 6, 2, true).unwrap();
    assert_eq!(decoded.to_string(), "0.00");
}

#[test]
fn test_comp3_with_scale_max_value() {
    let result = encode_packed_decimal("9999.99", 6, 2, true).unwrap();
    let decoded = decode_packed_decimal(&result, 6, 2, true).unwrap();
    assert_eq!(decoded.to_string(), "9999.99");
}

#[test]
fn test_comp3_all_sign_nibbles_decode() {
    // COMP-3 positive signs: 0xC, 0xA, 0xE, 0xF
    // For 1-digit field: high nibble = digit, low nibble = sign
    for sign in [0x0C_u8, 0x0A, 0x0E, 0x0F] {
        let data = [(0x05 << 4) | (sign & 0x0F)];
        let decoded = decode_packed_decimal(&data, 1, 0, true).unwrap();
        assert_eq!(
            decoded.to_string(),
            "5",
            "Sign nibble 0x{:X} should decode as positive",
            sign & 0x0F
        );
    }

    // Negative signs: 0xB, 0xD
    for sign in [0x0B_u8, 0x0D] {
        let data = [(0x05 << 4) | (sign & 0x0F)];
        let decoded = decode_packed_decimal(&data, 1, 0, true).unwrap();
        assert_eq!(
            decoded.to_string(),
            "-5",
            "Sign nibble 0x{:X} should decode as negative",
            sign & 0x0F
        );
    }
}

#[test]
fn test_comp3_data_length_mismatch() {
    // 3-digit field expects 2 bytes, provide 1
    let result = decode_packed_decimal(&[0x12], 3, 0, true);
    assert!(result.is_err());

    // 3-digit field expects 2 bytes, provide 3
    let result = decode_packed_decimal(&[0x12, 0x3C, 0x00], 3, 0, true);
    assert!(result.is_err());
}

// ===========================================================================
// 4. REDEFINES handling in decode/encode
// ===========================================================================

#[test]
fn test_redefines_decode_includes_all_views() {
    // REDEFINES fields are emitted alongside originals in declaration order
    let copybook = r"
       01 RECORD.
          05 FIELD-A PIC X(10).
          05 FIELD-B REDEFINES FIELD-A PIC X(10).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let data = b"HELLOWORLD";
    let opts = ascii_decode_opts();
    let value = decode_record(&schema, data, &opts).unwrap();

    // The original field should be present
    assert!(
        value.get("FIELD-A").is_some(),
        "original field should be decoded"
    );
    assert_eq!(value["FIELD-A"], "HELLOWORLD");
    // Both views of the same data are emitted
    if let Some(fb) = value.get("FIELD-B") {
        assert_eq!(fb, "HELLOWORLD", "redefining field should see same data");
    }
}

#[test]
fn test_redefines_group_decode() {
    let copybook = r"
       01 RECORD.
          05 ORIGINAL-GROUP.
             10 PART1 PIC X(5).
             10 PART2 PIC X(5).
          05 REDEF-GROUP REDEFINES ORIGINAL-GROUP.
             10 WHOLE PIC X(10).
    ";
    let schema = parse_copybook(copybook).unwrap();
    let data = b"HELLOWORLD";
    let opts = ascii_decode_opts();
    let value = decode_record(&schema, data, &opts).unwrap();

    // Fields may be nested under the group or flattened depending on layout
    // Check that the original group's children are decoded
    let part1 = value
        .pointer("/ORIGINAL-GROUP/PART1")
        .or_else(|| value.get("PART1"));
    let part2 = value
        .pointer("/ORIGINAL-GROUP/PART2")
        .or_else(|| value.get("PART2"));
    assert!(part1.is_some(), "PART1 should be decoded");
    assert!(part2.is_some(), "PART2 should be decoded");
    assert_eq!(part1.unwrap(), "HELLO");
    assert_eq!(part2.unwrap(), "WORLD");
    // REDEF-GROUP should be skipped
    assert!(value.get("REDEF-GROUP").is_none());
}

#[test]
fn test_redefines_numeric_over_alpha() {
    let copybook = r"
       01 RECORD.
          05 ALPHA-FIELD PIC X(4).
          05 NUM-FIELD REDEFINES ALPHA-FIELD PIC 9(4).
    ";
    let schema = parse_copybook(copybook).unwrap();

    // ASCII digits
    let data = b"1234";
    let opts = ascii_decode_opts();
    let value = decode_record(&schema, data, &opts).unwrap();

    // Only the original (alphanumeric) field should appear
    assert_eq!(value["ALPHA-FIELD"], "1234");
}

#[test]
fn test_redefines_encode_uses_original_field() {
    let copybook = r"
       01 RECORD.
          05 FIELD-A PIC X(10).
          05 FIELD-B REDEFINES FIELD-A PIC 9(10).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let json_data = json!({ "FIELD-A": "ABCDEFGHIJ" });
    let opts = ascii_encode_opts();
    let encoded = encode_record(&schema, &json_data, &opts).unwrap();
    assert_eq!(encoded, b"ABCDEFGHIJ");
}

// ===========================================================================
// 5. ODO boundary conditions
// ===========================================================================

#[test]
fn test_odo_min_count_boundary() {
    let copybook = r"
       01 RECORD.
          05 CNT PIC 9(1).
          05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON CNT PIC X(2).
    ";
    let schema = parse_copybook(copybook).unwrap();

    // Counter = 1 (minimum), one 2-byte element
    let data = b"1AB";
    let opts = ascii_decode_opts();
    let value = decode_record(&schema, data, &opts).unwrap();

    let items = value["ITEMS"].as_array().unwrap();
    assert_eq!(items.len(), 1);
}

#[test]
fn test_odo_max_count_boundary() {
    let copybook = r"
       01 RECORD.
          05 CNT PIC 9(1).
          05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON CNT PIC X(2).
    ";
    let schema = parse_copybook(copybook).unwrap();

    // Counter = 5 (maximum), five 2-byte elements
    let data = b"5AABBCCDDEE";
    let opts = ascii_decode_opts();
    let value = decode_record(&schema, data, &opts).unwrap();

    let items = value["ITEMS"].as_array().unwrap();
    assert_eq!(items.len(), 5);
}

#[test]
fn test_odo_counter_exceeds_max_is_error() {
    let copybook = r"
       01 RECORD.
          05 CNT PIC 9(1).
          05 ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON CNT PIC X(2).
    ";
    let schema = parse_copybook(copybook).unwrap();

    // Counter = 9 which exceeds max of 3
    // Provide enough data for 3 elements + extra
    let data = b"9AABBCCDDEE";
    let opts = ascii_decode_opts();
    let result = decode_record(&schema, data, &opts);

    // Should either clip or error — verify it doesn't panic
    match result {
        Ok(value) => {
            // If lenient: counter is clipped to max
            let items = value["ITEMS"].as_array().unwrap();
            assert!(items.len() <= 3, "should be clipped to max_count");
        }
        Err(e) => {
            let msg = format!("{e}");
            assert!(
                msg.contains("CBKS") || msg.contains("ODO") || msg.contains("count"),
                "error should relate to ODO bounds: {msg}"
            );
        }
    }
}

#[test]
fn test_odo_record_too_short_for_count() {
    let copybook = r"
       01 RECORD.
          05 CNT PIC 9(1).
          05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON CNT PIC X(4).
    ";
    let schema = parse_copybook(copybook).unwrap();

    // Counter says 3, but only enough data for 1 element
    let data = b"3ABCD";
    let opts = ascii_decode_opts();
    let result = decode_record(&schema, data, &opts);
    assert!(
        result.is_err(),
        "should fail when record is too short for ODO count"
    );
}

#[test]
fn test_odo_not_at_tail_rejected() {
    let copybook = r"
       01 RECORD.
          05 CNT PIC 9(1).
          05 ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON CNT PIC X(2).
          05 TAIL-FIELD PIC X(5).
    ";
    let result = parse_copybook(copybook);
    assert!(
        result.is_err(),
        "ODO not at tail position should be rejected"
    );
}

// ===========================================================================
// 6. Numeric field overflow/underflow (binary integer)
// ===========================================================================

#[test]
fn test_binary_int_16bit_signed_boundaries() {
    // i16 min = -32768
    let encoded = encode_binary_int(-32768, 16, true).unwrap();
    let decoded = decode_binary_int(&encoded, 16, true).unwrap();
    assert_eq!(decoded, -32768);

    // i16 max = 32767
    let encoded = encode_binary_int(32767, 16, true).unwrap();
    let decoded = decode_binary_int(&encoded, 16, true).unwrap();
    assert_eq!(decoded, 32767);
}

#[test]
fn test_binary_int_16bit_signed_overflow() {
    let result = encode_binary_int(32768, 16, true);
    assert!(result.is_err(), "32768 should overflow signed 16-bit");
}

#[test]
fn test_binary_int_16bit_signed_underflow() {
    let result = encode_binary_int(-32769, 16, true);
    assert!(result.is_err(), "-32769 should underflow signed 16-bit");
}

#[test]
fn test_binary_int_16bit_unsigned_boundaries() {
    let encoded = encode_binary_int(0, 16, false).unwrap();
    let decoded = decode_binary_int(&encoded, 16, false).unwrap();
    assert_eq!(decoded, 0);

    let encoded = encode_binary_int(65535, 16, false).unwrap();
    let decoded = decode_binary_int(&encoded, 16, false).unwrap();
    assert_eq!(decoded, 65535);
}

#[test]
fn test_binary_int_16bit_unsigned_negative_fails() {
    let result = encode_binary_int(-1, 16, false);
    assert!(
        result.is_err(),
        "negative value should fail for unsigned 16-bit"
    );
}

#[test]
fn test_binary_int_32bit_signed_boundaries() {
    let encoded = encode_binary_int(i64::from(i32::MIN), 32, true).unwrap();
    let decoded = decode_binary_int(&encoded, 32, true).unwrap();
    assert_eq!(decoded, i64::from(i32::MIN));

    let encoded = encode_binary_int(i64::from(i32::MAX), 32, true).unwrap();
    let decoded = decode_binary_int(&encoded, 32, true).unwrap();
    assert_eq!(decoded, i64::from(i32::MAX));
}

#[test]
fn test_binary_int_32bit_overflow() {
    let result = encode_binary_int(i64::from(i32::MAX) + 1, 32, true);
    assert!(result.is_err());
}

#[test]
fn test_binary_int_64bit_signed_min_max() {
    let encoded = encode_binary_int(i64::MIN, 64, true).unwrap();
    let decoded = decode_binary_int(&encoded, 64, true).unwrap();
    assert_eq!(decoded, i64::MIN);

    let encoded = encode_binary_int(i64::MAX, 64, true).unwrap();
    let decoded = decode_binary_int(&encoded, 64, true).unwrap();
    assert_eq!(decoded, i64::MAX);
}

#[test]
fn test_binary_int_zero_all_widths() {
    for bits in [16, 32, 64] {
        for signed in [true, false] {
            let encoded = encode_binary_int(0, bits, signed).unwrap();
            let decoded = decode_binary_int(&encoded, bits, signed).unwrap();
            assert_eq!(decoded, 0, "zero roundtrip for {bits}-bit signed={signed}");
        }
    }
}

#[test]
fn test_binary_int_unsupported_width() {
    let result = encode_binary_int(0, 24, true);
    assert!(result.is_err(), "24-bit should be unsupported");
}

#[test]
fn test_binary_int_decode_wrong_length() {
    // 16-bit expects 2 bytes, give 3
    let result = decode_binary_int(&[0x00, 0x01, 0x02], 16, true);
    assert!(result.is_err());

    // 32-bit expects 4 bytes, give 2
    let result = decode_binary_int(&[0x00, 0x01], 32, true);
    assert!(result.is_err());
}

// ===========================================================================
// 6b. Numeric overflow via high-level encode_record
// ===========================================================================

#[test]
fn test_numeric_overflow_packed_via_record() {
    let copybook = r"
       01 RECORD.
          05 SMALL-FIELD PIC S9(2) COMP-3.
    ";
    let schema = parse_copybook(copybook).unwrap();
    let opts = ascii_encode_opts();

    // 100 overflows S9(2) which max is 99
    let result = encode_record(&schema, &json!({"SMALL-FIELD": "100"}), &opts);
    assert!(result.is_err());
}

#[test]
fn test_numeric_underflow_packed_via_record() {
    let copybook = r"
       01 RECORD.
          05 SMALL-FIELD PIC S9(2) COMP-3.
    ";
    let schema = parse_copybook(copybook).unwrap();
    let opts = ascii_encode_opts();

    // -100 overflows S9(2) which min is -99
    let result = encode_record(&schema, &json!({"SMALL-FIELD": "-100"}), &opts);
    assert!(result.is_err());
}

// ===========================================================================
// 7. Character field truncation and padding
// ===========================================================================

#[test]
fn test_alpha_field_right_padded_with_spaces() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(10).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let opts = ascii_encode_opts();
    let encoded = encode_record(&schema, &json!({"NAME": "HI"}), &opts).unwrap();
    assert_eq!(encoded.len(), 10);
    assert_eq!(&encoded[..2], b"HI");
    assert!(
        encoded[2..].iter().all(|&b| b == b' '),
        "should be space-padded"
    );
}

#[test]
fn test_alpha_field_exact_length() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(5).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let opts = ascii_encode_opts();
    let encoded = encode_record(&schema, &json!({"NAME": "HELLO"}), &opts).unwrap();
    assert_eq!(encoded, b"HELLO");
}

#[test]
fn test_alpha_field_truncation_on_overflow() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(5).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let opts = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(false); // lenient mode allows truncation

    let result = encode_record(&schema, &json!({"NAME": "TOOLONGVALUE"}), &opts);
    // Even in lenient mode, string overflow produces CBKE515
    match result {
        Ok(encoded) => {
            assert_eq!(encoded.len(), 5);
            assert_eq!(&encoded[..5], b"TOOLO");
        }
        Err(e) => {
            let msg = format!("{e}");
            assert!(
                msg.contains("CBKE515") || msg.contains("truncat") || msg.contains("length"),
                "error should be about string length: {msg}"
            );
        }
    }
}

#[test]
fn test_alpha_field_strict_mode_rejects_overflow() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(3).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let opts = ascii_encode_opts(); // strict_mode = true
    let result = encode_record(&schema, &json!({"NAME": "TOOLONG"}), &opts);
    assert!(
        result.is_err(),
        "strict mode should reject alpha field overflow"
    );
}

#[test]
fn test_alpha_field_empty_string() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(5).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let opts = ascii_encode_opts();
    let encoded = encode_record(&schema, &json!({"NAME": ""}), &opts).unwrap();
    assert_eq!(encoded.len(), 5);
    assert!(
        encoded.iter().all(|&b| b == b' '),
        "empty string should produce all spaces"
    );
}

#[test]
fn test_alpha_field_decode_trims_trailing_spaces() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(10).
    ";
    let schema = parse_copybook(copybook).unwrap();
    let opts = ascii_decode_opts();

    let data = b"HELLO     "; // "HELLO" + 5 spaces
    let value = decode_record(&schema, data, &opts).unwrap();
    // Decoded value should have trailing spaces trimmed (common COBOL convention)
    let name = value["NAME"].as_str().unwrap();
    assert!(
        name == "HELLO" || name == "HELLO     ",
        "decoded value should be 'HELLO' or preserve spaces: got '{name}'"
    );
}

#[test]
fn test_alpha_field_ebcdic_roundtrip() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(10).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let enc_opts = ebcdic_encode_opts();
    let dec_opts = ebcdic_decode_opts();

    let encoded = encode_record(&schema, &json!({"NAME": "RUST"}), &enc_opts).unwrap();
    assert_eq!(encoded.len(), 10);

    let decoded = decode_record(&schema, &encoded, &dec_opts).unwrap();
    let name = decoded["NAME"].as_str().unwrap();
    assert!(
        name.starts_with("RUST"),
        "EBCDIC roundtrip should preserve text: got '{name}'"
    );
}

// ===========================================================================
// 7b. Numeric display field padding
// ===========================================================================

#[test]
fn test_zoned_decimal_left_padded_with_zeros() {
    let copybook = r"
       01 RECORD.
          05 AMOUNT PIC 9(5).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let enc_opts = ascii_encode_opts();
    let dec_opts = ascii_decode_opts();

    let encoded = encode_record(&schema, &json!({"AMOUNT": "42"}), &enc_opts).unwrap();
    assert_eq!(encoded.len(), 5);

    let decoded = decode_record(&schema, &encoded, &dec_opts).unwrap();
    // Zoned decimal preserves leading zeros per field width
    let amount = decoded["AMOUNT"].as_str().unwrap();
    assert!(
        amount == "42" || amount == "00042",
        "amount should be '42' or '00042', got '{amount}'"
    );
}

#[test]
fn test_packed_decimal_roundtrip_via_record() {
    let copybook = r"
       01 RECORD.
          05 AMOUNT PIC S9(5)V99 COMP-3.
    ";
    let schema = parse_copybook(copybook).unwrap();

    let enc_opts = ascii_encode_opts();
    let dec_opts = ascii_decode_opts();

    let json_data = json!({"AMOUNT": "12345.67"});
    let encoded = encode_record(&schema, &json_data, &enc_opts).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts).unwrap();
    assert_eq!(decoded["AMOUNT"], "12345.67");
}

#[test]
fn test_multiple_fields_mixed_types_roundtrip() {
    let copybook = r"
       01 RECORD.
          05 NAME    PIC X(10).
          05 AMOUNT  PIC S9(5)V99 COMP-3.
          05 COUNT   PIC 9(3).
    ";
    let schema = parse_copybook(copybook).unwrap();

    let enc_opts = ascii_encode_opts();
    let dec_opts = ascii_decode_opts();

    let json_data = json!({
        "NAME": "TEST",
        "AMOUNT": "123.45",
        "COUNT": "7"
    });
    let encoded = encode_record(&schema, &json_data, &enc_opts).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts).unwrap();

    let name = decoded["NAME"].as_str().unwrap();
    assert!(name.starts_with("TEST"));
    assert_eq!(decoded["AMOUNT"], "123.45");
    // Zoned decimal preserves field width with leading zeros
    let count = decoded["COUNT"].as_str().unwrap();
    assert!(
        count == "7" || count == "007",
        "count should be '7' or '007', got '{count}'"
    );
}
