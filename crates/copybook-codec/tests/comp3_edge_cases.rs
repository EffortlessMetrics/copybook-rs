// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive COMP-3 (packed decimal) edge-case tests.
//!
//! Covers:
//! 1. Boundary values (0, 1, -1, MAX for each digit count 1-18)
//! 2. Sign nibbles (0xC positive, 0xD negative, 0xF unsigned, 0xA-0xE alternates)
//! 3. Scale handling (V99, V9999, integer-only)
//! 4. Odd vs even digit counts
//! 5. Leading zeros
//! 6. Maximum precision (S9(18) COMP-3)
//! 7. Invalid BCD nibbles in digit positions
//! 8. Round-trip fidelity via high-level encode_record / decode_record

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::{decode_packed_decimal, encode_packed_decimal};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use serde_json::json;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn comp3_schema(pic: &str) -> copybook_core::Schema {
    let cpy = format!("       01 REC.\n           05 FLD {} COMP-3.", pic);
    parse_copybook(&cpy).expect("schema should parse")
}

fn dec_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
}

fn enc_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true)
}

// ===========================================================================
// 1. Boundary values — zero for various digit counts
// ===========================================================================

#[test]
fn test_comp3_positive_zero_s9_1() {
    let data = vec![0x0C]; // 1-digit signed: [0|C]
    let d = decode_packed_decimal(&data, 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn test_comp3_positive_zero_s9_3() {
    // S9(3) -> 3 digits + 1 sign = 4 nibbles = 2 bytes: [d1 d2] [d3 sign]
    let data = vec![0x00, 0x0C];
    let d = decode_packed_decimal(&data, 3, 0, true).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn test_comp3_positive_zero_s9_5() {
    // S9(5) -> 5+1=6 nibbles = 3 bytes
    let data = vec![0x00, 0x00, 0x0C];
    let d = decode_packed_decimal(&data, 5, 0, true).unwrap();
    assert_eq!(d.to_string(), "0");
}

#[test]
fn test_comp3_positive_one_s9_1() {
    let d = decode_packed_decimal(&[0x1C], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "1");
}

#[test]
fn test_comp3_negative_one_s9_1() {
    let d = decode_packed_decimal(&[0x1D], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "-1");
}

#[test]
fn test_comp3_max_s9_1() {
    let d = decode_packed_decimal(&[0x9C], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "9");
}

#[test]
fn test_comp3_min_s9_1() {
    let d = decode_packed_decimal(&[0x9D], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "-9");
}

// ===========================================================================
// 1b. Boundary values via encode → decode round-trip (parametric)
// ===========================================================================

#[test]
fn test_comp3_boundary_values_1_to_18_digits() {
    for digits in 1u16..=18 {
        let max_val: String = "9".repeat(digits as usize);
        let min_val = format!("-{max_val}");

        // zero
        let enc = encode_packed_decimal("0", digits, 0, true).unwrap();
        let dec = decode_packed_decimal(&enc, digits, 0, true).unwrap();
        assert_eq!(dec.to_string(), "0", "zero for {digits} digits");

        // +1
        let enc = encode_packed_decimal("1", digits, 0, true).unwrap();
        let dec = decode_packed_decimal(&enc, digits, 0, true).unwrap();
        assert_eq!(dec.to_string(), "1", "+1 for {digits} digits");

        // -1
        let enc = encode_packed_decimal("-1", digits, 0, true).unwrap();
        let dec = decode_packed_decimal(&enc, digits, 0, true).unwrap();
        assert_eq!(dec.to_string(), "-1", "-1 for {digits} digits");

        // MAX
        let enc = encode_packed_decimal(&max_val, digits, 0, true).unwrap();
        let dec = decode_packed_decimal(&enc, digits, 0, true).unwrap();
        assert_eq!(dec.to_string(), max_val, "max for {digits} digits");

        // MIN (negative MAX)
        let enc = encode_packed_decimal(&min_val, digits, 0, true).unwrap();
        let dec = decode_packed_decimal(&enc, digits, 0, true).unwrap();
        assert_eq!(dec.to_string(), min_val, "min for {digits} digits");
    }
}

// ===========================================================================
// 2. Sign nibbles — all valid sign nibble values
// ===========================================================================

#[test]
fn test_comp3_sign_nibble_0xc_positive() {
    // 0xC = positive
    let d = decode_packed_decimal(&[0x5C], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "5");
    assert!(!d.is_negative());
}

#[test]
fn test_comp3_sign_nibble_0xd_negative() {
    // 0xD = negative
    let d = decode_packed_decimal(&[0x5D], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "-5");
    assert!(d.is_negative());
}

#[test]
fn test_comp3_sign_nibble_0xf_unsigned() {
    // 0xF = unsigned (positive)
    let d = decode_packed_decimal(&[0x5F], 1, 0, false).unwrap();
    assert_eq!(d.to_string(), "5");
}

#[test]
fn test_comp3_sign_nibble_0xa_positive() {
    // 0xA = alternate positive
    let d = decode_packed_decimal(&[0x7A], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "7");
    assert!(!d.is_negative());
}

#[test]
fn test_comp3_sign_nibble_0xb_negative() {
    // 0xB = alternate negative
    let d = decode_packed_decimal(&[0x7B], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "-7");
    assert!(d.is_negative());
}

#[test]
fn test_comp3_sign_nibble_0xe_positive() {
    // 0xE = alternate positive
    let d = decode_packed_decimal(&[0x3E], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "3");
    assert!(!d.is_negative());
}

#[test]
fn test_comp3_sign_nibble_0xf_treated_as_positive_when_signed() {
    // 0xF on a signed field is treated as positive
    let d = decode_packed_decimal(&[0x4F], 1, 0, true).unwrap();
    assert_eq!(d.to_string(), "4");
    assert!(!d.is_negative());
}

#[test]
fn test_comp3_unsigned_field_rejects_non_0xf_sign() {
    // Unsigned field must have 0xF sign nibble; 0xC should fail
    let result = decode_packed_decimal(&[0x5C], 1, 0, false);
    assert!(result.is_err(), "unsigned field should reject 0xC sign");
}

#[test]
fn test_comp3_invalid_sign_nibble_0x0() {
    // 0x0 is not a valid sign nibble for signed fields
    let result = decode_packed_decimal(&[0x50], 1, 0, true);
    assert!(result.is_err(), "0x0 should not be a valid sign nibble");
}

#[test]
fn test_comp3_invalid_sign_nibble_0x9() {
    // 0x9 is a digit, not a valid sign nibble
    let result = decode_packed_decimal(&[0x59], 1, 0, true);
    assert!(result.is_err(), "0x9 should not be a valid sign nibble");
}

#[test]
fn test_comp3_all_positive_sign_nibbles_multi_byte() {
    // 3-digit field (odd): 3+1=4 nibbles = 2 bytes [d1|d2][d3|sign]
    // Value 123: d1=1, d2=2, d3=3
    for sign in [0x0Cu8, 0x0A, 0x0E, 0x0F] {
        let data = [0x12, 0x30 | (sign & 0x0F)];
        let d = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(
            d.to_string(),
            "123",
            "sign 0x{sign:X} should be positive for 3-digit field"
        );
    }
}

#[test]
fn test_comp3_all_negative_sign_nibbles_multi_byte() {
    // 3-digit field (odd): 3+1=4 nibbles = 2 bytes [d1|d2][d3|sign]
    // Value -123: d1=1, d2=2, d3=3
    for sign in [0x0Bu8, 0x0D] {
        let data = [0x12, 0x30 | (sign & 0x0F)];
        let d = decode_packed_decimal(&data, 3, 0, true).unwrap();
        assert_eq!(
            d.to_string(),
            "-123",
            "sign 0x{sign:X} should be negative for 3-digit field"
        );
    }
}

// ===========================================================================
// 3. Scale handling — V99, V9999, integer-only
// ===========================================================================

#[test]
fn test_comp3_scale_v99_zero() {
    // S9(4)V99 = 6 total digits, scale 2
    let enc = encode_packed_decimal("0.00", 6, 2, true).unwrap();
    let dec = decode_packed_decimal(&enc, 6, 2, true).unwrap();
    assert_eq!(dec.to_string(), "0.00");
}

#[test]
fn test_comp3_scale_v99_positive() {
    let enc = encode_packed_decimal("1234.56", 6, 2, true).unwrap();
    let dec = decode_packed_decimal(&enc, 6, 2, true).unwrap();
    assert_eq!(dec.to_string(), "1234.56");
}

#[test]
fn test_comp3_scale_v99_negative() {
    let enc = encode_packed_decimal("-1234.56", 6, 2, true).unwrap();
    let dec = decode_packed_decimal(&enc, 6, 2, true).unwrap();
    assert_eq!(dec.to_string(), "-1234.56");
}

#[test]
fn test_comp3_scale_v99_max() {
    // S9(4)V99: max = 9999.99
    let enc = encode_packed_decimal("9999.99", 6, 2, true).unwrap();
    let dec = decode_packed_decimal(&enc, 6, 2, true).unwrap();
    assert_eq!(dec.to_string(), "9999.99");
}

#[test]
fn test_comp3_scale_v9999_four_decimal_places() {
    // S9(5)V9(4) = 9 total digits, scale 4
    let enc = encode_packed_decimal("12345.6789", 9, 4, true).unwrap();
    let dec = decode_packed_decimal(&enc, 9, 4, true).unwrap();
    assert_eq!(dec.to_string(), "12345.6789");
}

#[test]
fn test_comp3_scale_v9999_small_fraction() {
    let enc = encode_packed_decimal("0.0001", 9, 4, true).unwrap();
    let dec = decode_packed_decimal(&enc, 9, 4, true).unwrap();
    assert_eq!(dec.to_string(), "0.0001");
}

#[test]
fn test_comp3_integer_only_no_scale() {
    let enc = encode_packed_decimal("42", 3, 0, true).unwrap();
    let dec = decode_packed_decimal(&enc, 3, 0, true).unwrap();
    assert_eq!(dec.to_string(), "42");
}

// ===========================================================================
// 4. Odd vs even digit counts — packing differs
// ===========================================================================

#[test]
fn test_comp3_odd_digit_1_byte_layout() {
    // 1 digit (odd): 1+1=2 nibbles = 1 byte: [digit|sign]
    let enc = encode_packed_decimal("5", 1, 0, true).unwrap();
    assert_eq!(enc.len(), 1);
    assert_eq!(enc[0] >> 4, 5); // high nibble = digit
    assert_eq!(enc[0] & 0x0F, 0x0C); // low nibble = positive sign
}

#[test]
fn test_comp3_even_digit_2_padded_layout() {
    // 2 digits (even): 2+1=3 nibbles → pad to 4 nibbles = 2 bytes: [0|d1][d2|sign]
    let enc = encode_packed_decimal("12", 2, 0, true).unwrap();
    assert_eq!(enc.len(), 2);
    assert_eq!(enc[0] >> 4, 0); // padding nibble
    assert_eq!(enc[0] & 0x0F, 1); // first digit
    assert_eq!(enc[1] >> 4, 2); // second digit
    assert_eq!(enc[1] & 0x0F, 0x0C); // positive sign
}

#[test]
fn test_comp3_odd_digit_3_no_padding() {
    // 3 digits (odd): 3+1=4 nibbles = 2 bytes: [d1|d2][d3|sign]
    let enc = encode_packed_decimal("123", 3, 0, true).unwrap();
    assert_eq!(enc.len(), 2);
    assert_eq!(enc[0], 0x12); // d1=1, d2=2
    assert_eq!(enc[1], 0x3C); // d3=3, sign=C
}

#[test]
fn test_comp3_even_digit_4_padded_layout() {
    // 4 digits (even): 4+1=5 nibbles → pad to 6 = 3 bytes: [0|d1][d2|d3][d4|sign]
    let enc = encode_packed_decimal("1234", 4, 0, true).unwrap();
    assert_eq!(enc.len(), 3);
    assert_eq!(enc[0], 0x01); // pad=0, d1=1
    assert_eq!(enc[1], 0x23); // d2=2, d3=3
    assert_eq!(enc[2], 0x4C); // d4=4, sign=C
}

#[test]
fn test_comp3_odd_digit_5_layout() {
    // 5 digits (odd): 5+1=6 nibbles = 3 bytes: [d1|d2][d3|d4][d5|sign]
    let enc = encode_packed_decimal("12345", 5, 0, true).unwrap();
    assert_eq!(enc.len(), 3);
    assert_eq!(enc[0], 0x12);
    assert_eq!(enc[1], 0x34);
    assert_eq!(enc[2], 0x5C);
}

#[test]
fn test_comp3_even_digit_6_layout() {
    // 6 digits (even): 6+1=7 nibbles → 8 nibbles = 4 bytes: [0|d1][d2|d3][d4|d5][d6|sign]
    let enc = encode_packed_decimal("123456", 6, 0, true).unwrap();
    assert_eq!(enc.len(), 4);
    assert_eq!(enc[0], 0x01);
    assert_eq!(enc[1], 0x23);
    assert_eq!(enc[2], 0x45);
    assert_eq!(enc[3], 0x6C);
}

#[test]
fn test_comp3_byte_count_formula() {
    // Verify byte count = ceil((digits+1)/2) for all 1-18
    for digits in 1u16..=18 {
        let expected_bytes = ((digits + 1) as usize + 1) / 2;
        let enc = encode_packed_decimal("0", digits, 0, true).unwrap();
        assert_eq!(
            enc.len(),
            expected_bytes,
            "byte count mismatch for {digits} digits"
        );
    }
}

// ===========================================================================
// 5. Leading zeros
// ===========================================================================

#[test]
fn test_comp3_leading_zeros_encode() {
    // "000123" in a 6-digit field should pack the same as "123"
    let enc_with_zeros = encode_packed_decimal("000123", 6, 0, true).unwrap();
    let enc_plain = encode_packed_decimal("123", 6, 0, true).unwrap();
    assert_eq!(enc_with_zeros, enc_plain);
}

#[test]
fn test_comp3_leading_zeros_decode() {
    // A 6-digit field with value 123 decodes as "123" (leading zeros stripped)
    let enc = encode_packed_decimal("123", 6, 0, true).unwrap();
    let dec = decode_packed_decimal(&enc, 6, 0, true).unwrap();
    assert_eq!(dec.to_string(), "123");
}

#[test]
fn test_comp3_all_zeros_large_field() {
    // 18-digit field, all zeros
    let enc = encode_packed_decimal("0", 18, 0, true).unwrap();
    let dec = decode_packed_decimal(&enc, 18, 0, true).unwrap();
    assert_eq!(dec.to_string(), "0");
}

#[test]
fn test_comp3_leading_zeros_with_scale() {
    // "000001.23" in S9(6)V99
    let enc = encode_packed_decimal("1.23", 8, 2, true).unwrap();
    let dec = decode_packed_decimal(&enc, 8, 2, true).unwrap();
    assert_eq!(dec.to_string(), "1.23");
}

// ===========================================================================
// 6. Maximum precision — S9(18) COMP-3 (18 digits)
// ===========================================================================

#[test]
fn test_comp3_max_precision_18_digits_positive() {
    let max18 = "999999999999999999"; // 18 nines
    let enc = encode_packed_decimal(max18, 18, 0, true).unwrap();
    assert_eq!(enc.len(), 10); // (18+1+1)/2 = 10 bytes (even digit count, padded)
    let dec = decode_packed_decimal(&enc, 18, 0, true).unwrap();
    assert_eq!(dec.to_string(), max18);
}

#[test]
fn test_comp3_max_precision_18_digits_negative() {
    let min18 = "-999999999999999999";
    let enc = encode_packed_decimal(min18, 18, 0, true).unwrap();
    let dec = decode_packed_decimal(&enc, 18, 0, true).unwrap();
    assert_eq!(dec.to_string(), min18);
}

#[test]
fn test_comp3_19_digits_rejected() {
    // 19 digits exceeds i64 range
    let result = decode_packed_decimal(&[0x01; 10], 19, 0, true);
    assert!(result.is_err());
}

#[test]
fn test_comp3_max_precision_with_scale() {
    // S9(9)V9(9) = 18 total digits, scale 9
    let val = "999999999.999999999";
    let enc = encode_packed_decimal(val, 18, 9, true).unwrap();
    let dec = decode_packed_decimal(&enc, 18, 9, true).unwrap();
    assert_eq!(dec.to_string(), val);
}

// ===========================================================================
// 7. Invalid BCD nibbles in digit positions
// ===========================================================================

#[test]
fn test_comp3_invalid_digit_nibble_0xa_high() {
    // High nibble = 0xA (invalid digit), low nibble = 0xC (valid sign)
    let data = [0xAC_u8];
    let result = decode_packed_decimal(&data, 1, 0, true);
    assert!(result.is_err(), "0xA in digit position should error");
}

#[test]
fn test_comp3_invalid_digit_nibble_0xf_high() {
    // High nibble = 0xF (invalid digit)
    let data = [0xFC_u8];
    let result = decode_packed_decimal(&data, 1, 0, true);
    assert!(result.is_err(), "0xF in digit position should error");
}

#[test]
fn test_comp3_invalid_digit_nibble_in_middle_byte() {
    // 5-digit field: [d1|d2][d3|d4][d5|sign]
    // Put invalid 0xB in d3 (high nibble of byte 1)
    let data = [0x12, 0xB4, 0x5C];
    let result = decode_packed_decimal(&data, 5, 0, true);
    assert!(
        result.is_err(),
        "invalid nibble in middle byte should error"
    );
}

#[test]
fn test_comp3_invalid_digit_nibble_low() {
    // 3-digit field: [d1|d2][d3|sign]
    // Put 0xA in d2 (low nibble of byte 0)
    let data = [0x1A, 0x3C];
    let result = decode_packed_decimal(&data, 3, 0, true);
    assert!(result.is_err(), "0xA in low digit nibble should error");
}

#[test]
fn test_comp3_data_length_too_short() {
    // 5-digit field expects 3 bytes, provide 2
    let result = decode_packed_decimal(&[0x12, 0x3C], 5, 0, true);
    assert!(result.is_err(), "short data should error");
}

#[test]
fn test_comp3_data_length_too_long() {
    // 3-digit field expects 2 bytes, provide 3
    let result = decode_packed_decimal(&[0x01, 0x23, 0x4C], 3, 0, true);
    assert!(result.is_err(), "excess data should error");
}

#[test]
fn test_comp3_empty_data() {
    let result = decode_packed_decimal(&[], 1, 0, true);
    assert!(result.is_err(), "empty data should error");
}

// ===========================================================================
// 8. Round-trip fidelity — high-level encode_record / decode_record
// ===========================================================================

#[test]
fn test_comp3_roundtrip_s9_3_via_record() {
    let schema = comp3_schema("PIC S9(3)");
    let json_in = json!({"FLD": "123"});
    let encoded = encode_record(&schema, &json_in, &enc_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts()).unwrap();
    assert_eq!(decoded["FLD"], "123");
}

#[test]
fn test_comp3_roundtrip_negative_via_record() {
    let schema = comp3_schema("PIC S9(5)");
    let json_in = json!({"FLD": "-42"});
    let encoded = encode_record(&schema, &json_in, &enc_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts()).unwrap();
    assert_eq!(decoded["FLD"], "-42");
}

#[test]
fn test_comp3_roundtrip_v99_via_record() {
    let schema = comp3_schema("PIC S9(5)V99");
    let json_in = json!({"FLD": "12345.67"});
    let encoded = encode_record(&schema, &json_in, &enc_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts()).unwrap();
    assert_eq!(decoded["FLD"], "12345.67");
}

#[test]
fn test_comp3_roundtrip_zero_via_record() {
    let schema = comp3_schema("PIC S9(5)V99");
    let json_in = json!({"FLD": "0.00"});
    let encoded = encode_record(&schema, &json_in, &enc_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts()).unwrap();
    assert_eq!(decoded["FLD"], "0.00");
}

#[test]
fn test_comp3_roundtrip_unsigned_via_record() {
    let schema = comp3_schema("PIC 9(4)");
    let json_in = json!({"FLD": "1234"});
    let encoded = encode_record(&schema, &json_in, &enc_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts()).unwrap();
    assert_eq!(decoded["FLD"], "1234");
}

#[test]
fn test_comp3_roundtrip_max_s9_7_v99_via_record() {
    let schema = comp3_schema("PIC S9(7)V99");
    let json_in = json!({"FLD": "9999999.99"});
    let encoded = encode_record(&schema, &json_in, &enc_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts()).unwrap();
    assert_eq!(decoded["FLD"], "9999999.99");
}

#[test]
fn test_comp3_roundtrip_negative_fraction_via_record() {
    let schema = comp3_schema("PIC S9(3)V9(4)");
    let json_in = json!({"FLD": "-123.4567"});
    let encoded = encode_record(&schema, &json_in, &enc_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &dec_opts()).unwrap();
    assert_eq!(decoded["FLD"], "-123.4567");
}

#[test]
fn test_comp3_roundtrip_bytes_identity() {
    // encode(decode(bytes)) == bytes for well-formed COMP-3 data
    let schema = comp3_schema("PIC S9(5)V99");
    let original_bytes = encode_packed_decimal("12345.67", 7, 2, true).unwrap();
    let decoded = decode_record(&schema, &original_bytes, &dec_opts()).unwrap();
    let re_encoded = encode_record(&schema, &decoded, &enc_opts()).unwrap();
    assert_eq!(
        re_encoded, original_bytes,
        "re-encoded bytes must match original"
    );
}

#[test]
fn test_comp3_roundtrip_negative_bytes_identity() {
    let schema = comp3_schema("PIC S9(5)V99");
    let original_bytes = encode_packed_decimal("-99999.99", 7, 2, true).unwrap();
    let decoded = decode_record(&schema, &original_bytes, &dec_opts()).unwrap();
    let re_encoded = encode_record(&schema, &decoded, &enc_opts()).unwrap();
    assert_eq!(re_encoded, original_bytes);
}

// ===========================================================================
// Additional edge cases — negative zero, overflow, encode rejects
// ===========================================================================

#[test]
fn test_comp3_negative_zero_normalizes_to_positive() {
    let enc = encode_packed_decimal("-0", 1, 0, true).unwrap();
    let dec = decode_packed_decimal(&enc, 1, 0, true).unwrap();
    assert_eq!(dec.to_string(), "0");
    assert!(!dec.is_negative());
}

#[test]
fn test_comp3_encode_overflow_rejects() {
    // 100 overflows a 2-digit field
    let result = encode_packed_decimal("100", 2, 0, true);
    assert!(result.is_err());
}

#[test]
fn test_comp3_encode_overflow_via_record() {
    let schema = comp3_schema("PIC S9(2)");
    let result = encode_record(&schema, &json!({"FLD": "100"}), &enc_opts());
    assert!(result.is_err());
}

#[test]
fn test_comp3_unsigned_encodes_with_0xf_sign() {
    // Unsigned COMP-3 field always uses 0xF sign nibble
    let enc = encode_packed_decimal("5", 1, 0, false).unwrap();
    assert_eq!(enc[0] & 0x0F, 0x0F, "unsigned must use 0xF sign nibble");
}

// ===========================================================================
// Parametric round-trip for multiple digit/scale combos
// ===========================================================================

#[test]
fn test_comp3_roundtrip_parametric() {
    let cases: &[(&str, u16, i16, bool)] = &[
        ("5", 1, 0, true),
        ("-5", 1, 0, true),
        ("99", 2, 0, true),
        ("-99", 2, 0, true),
        ("123", 3, 0, true),
        ("1234", 4, 0, true),
        ("12345", 5, 0, true),
        ("123456", 6, 0, true),
        ("1234567", 7, 0, true),
        ("12345678", 8, 0, true),
        ("123456789", 9, 0, true),
        ("0.01", 3, 2, true),
        ("-0.01", 3, 2, true),
        ("999.99", 5, 2, true),
        ("-999.99", 5, 2, true),
        ("0.0001", 5, 4, true),
        ("1.0000", 5, 4, true),
        ("5", 3, 0, false), // unsigned
        ("999", 3, 0, false),
    ];
    for &(value, digits, scale, signed) in cases {
        let enc = encode_packed_decimal(value, digits, scale, signed).unwrap_or_else(|e| {
            panic!("encode failed for {value} digits={digits} scale={scale}: {e}")
        });
        let dec = decode_packed_decimal(&enc, digits, scale, signed).unwrap_or_else(|e| {
            panic!("decode failed for {value} digits={digits} scale={scale}: {e}")
        });
        assert_eq!(
            dec.to_string(),
            value,
            "round-trip mismatch for {value} digits={digits} scale={scale}"
        );
    }
}
