// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for COMP-1 (float32) and COMP-2 (float64) support.
//!
//! Covers decode, encode, roundtrip, special IEEE 754 values, subnormals,
//! precision edge cases, JSON number mode interaction, mixed records,
//! schema field kind verification, and error handling.

#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]
#![allow(clippy::approx_constant)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat, decode_record, encode_record,
};
use copybook_core::{FieldKind, parse_copybook};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts(mode: JsonNumberMode) -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number_mode: mode,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
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
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        on_encode_unmappable: UnmappablePolicy::Error,
        json_number_mode: JsonNumberMode::Native,
        zoned_encoding_override: None,
        float_format: FloatFormat::IeeeBigEndian,
    }
}

fn native_decode() -> DecodeOptions {
    decode_opts(JsonNumberMode::Native)
}

fn lossless_decode() -> DecodeOptions {
    decode_opts(JsonNumberMode::Lossless)
}

// =========================================================================
// COMP-1 decode: basic values
// =========================================================================

#[test]
fn comp1_decode_positive_one() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data: [u8; 4] = [0x3F, 0x80, 0x00, 0x00]; // 1.0f32
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - 1.0).abs() < f64::EPSILON);
}

#[test]
fn comp1_decode_negative_one() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data: [u8; 4] = [0xBF, 0x80, 0x00, 0x00]; // -1.0f32
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - (-1.0)).abs() < f64::EPSILON);
}

#[test]
fn comp1_decode_zero() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x00]; // 0.0f32
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn comp1_decode_max_float32() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = f32::MAX.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - f64::from(f32::MAX)).abs() < 1e30);
}

#[test]
fn comp1_decode_min_float32() {
    // f32::MIN is -3.4028235e38
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = f32::MIN.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - f64::from(f32::MIN)).abs() < 1e30);
}

#[test]
fn comp1_decode_min_positive() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = f32::MIN_POSITIVE.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val > 0.0 && val < 1e-37);
}

#[test]
fn comp1_decode_pi() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = std::f32::consts::PI.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - f64::from(std::f32::consts::PI)).abs() < 1e-6);
}

// =========================================================================
// COMP-1 decode: big-endian byte order verification
// =========================================================================

#[test]
fn comp1_big_endian_byte_order() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    // IEEE 754 binary32 for 1.0 is 0x3F800000 in big-endian
    // Byte 0 = 0x3F (MSB), Byte 3 = 0x00 (LSB)
    let be_bytes: [u8; 4] = [0x3F, 0x80, 0x00, 0x00];
    let result = decode_record(&schema, &be_bytes, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - 1.0).abs() < f64::EPSILON);

    // Verify it's NOT little-endian: reversed bytes would give a different value
    let le_bytes: [u8; 4] = [0x00, 0x00, 0x80, 0x3F];
    let result_le = decode_record(&schema, &le_bytes, &native_decode()).unwrap();
    let val_le = result_le["V"].as_f64().unwrap();
    // If it were LE, val_le would be 1.0 — but it should NOT be 1.0 with BE decode
    assert!((val_le - 1.0).abs() > 0.01);
}

// =========================================================================
// COMP-1 encode: JSON number → float32 binary
// =========================================================================

#[test]
fn comp1_encode_positive() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": 1.0 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(encoded, vec![0x3F, 0x80, 0x00, 0x00]);
}

#[test]
fn comp1_encode_negative() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": -1.0 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(encoded, vec![0xBF, 0x80, 0x00, 0x00]);
}

#[test]
fn comp1_encode_zero() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": 0.0 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(encoded, vec![0x00, 0x00, 0x00, 0x00]);
}

#[test]
fn comp1_encode_fractional() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": 0.5 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    // 0.5f32 = 0x3F000000
    assert_eq!(encoded, vec![0x3F, 0x00, 0x00, 0x00]);
}

// =========================================================================
// COMP-1 roundtrip: decode → encode == original bytes
// =========================================================================

#[test]
fn comp1_roundtrip_various_values() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let test_values: &[f32] = &[
        0.0,
        1.0,
        -1.0,
        0.5,
        -0.5,
        100.0,
        -100.0,
        std::f32::consts::E,
        1.23e-10,
        9.99e30,
    ];
    for &v in test_values {
        let data = v.to_be_bytes();
        let decoded = decode_record(&schema, &data, &native_decode()).unwrap();
        let encoded = encode_record(&schema, &decoded, &encode_opts()).unwrap();
        assert_eq!(
            data.to_vec(),
            encoded,
            "COMP-1 roundtrip failed for value {}",
            v
        );
    }
}

// =========================================================================
// COMP-2 decode: basic values
// =========================================================================

#[test]
fn comp2_decode_positive_one() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data: [u8; 8] = [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]; // 1.0f64
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - 1.0).abs() < f64::EPSILON);
}

#[test]
fn comp2_decode_negative_one() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data: [u8; 8] = [0xBF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - (-1.0)).abs() < f64::EPSILON);
}

#[test]
fn comp2_decode_zero() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data: [u8; 8] = [0x00; 8];
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn comp2_decode_max_float64() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = f64::MAX.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val.is_finite() && val > 0.0);
    assert!((val - f64::MAX).abs() < 1e292);
}

#[test]
fn comp2_decode_min_float64() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = f64::MIN.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val.is_finite() && val < 0.0);
}

#[test]
fn comp2_decode_pi() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = std::f64::consts::PI.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - std::f64::consts::PI).abs() < 1e-15);
}

#[test]
fn comp2_decode_e() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = std::f64::consts::E.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - std::f64::consts::E).abs() < 1e-15);
}

// =========================================================================
// COMP-2 decode: big-endian byte order verification
// =========================================================================

#[test]
fn comp2_big_endian_byte_order() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    // IEEE 754 binary64 for 1.0: 0x3FF0_0000_0000_0000
    let be_bytes: [u8; 8] = [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &be_bytes, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!((val - 1.0).abs() < f64::EPSILON);

    // Reversed bytes should NOT decode to 1.0
    let le_bytes: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x3F];
    let result_le = decode_record(&schema, &le_bytes, &native_decode()).unwrap();
    let val_le = result_le["V"].as_f64().unwrap();
    assert!((val_le - 1.0).abs() > 0.01);
}

// =========================================================================
// COMP-2 encode: JSON number → float64 binary
// =========================================================================

#[test]
fn comp2_encode_positive() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": 1.0 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(
        encoded,
        vec![0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );
}

#[test]
fn comp2_encode_negative() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": -1.0 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(
        encoded,
        vec![0xBF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );
}

#[test]
fn comp2_encode_zero() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": 0.0 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(encoded, vec![0x00; 8]);
}

#[test]
fn comp2_encode_large_value() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": 1.23e100 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    let expected = 1.23e100_f64.to_be_bytes().to_vec();
    assert_eq!(encoded, expected);
}

// =========================================================================
// COMP-2 roundtrip: decode → encode == original bytes
// =========================================================================

#[test]
fn comp2_roundtrip_various_values() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let test_values: &[f64] = &[
        0.0,
        1.0,
        -1.0,
        std::f64::consts::PI,
        std::f64::consts::E,
        f64::MIN_POSITIVE,
        1.23e-100,
        9.99e200,
        -42.195,
    ];
    for &v in test_values {
        let data = v.to_be_bytes();
        let decoded = decode_record(&schema, &data, &native_decode()).unwrap();
        let encoded = encode_record(&schema, &decoded, &encode_opts()).unwrap();
        assert_eq!(
            data.to_vec(),
            encoded,
            "COMP-2 roundtrip failed for value {}",
            v
        );
    }
}

// =========================================================================
// Special values: +Infinity, -Infinity, NaN → null in JSON
// =========================================================================

#[test]
fn comp1_decode_positive_infinity_returns_null() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = f32::INFINITY.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    assert!(
        result["V"].is_null(),
        "COMP-1 +Infinity should decode to null"
    );
}

#[test]
fn comp1_decode_negative_infinity_returns_null() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = f32::NEG_INFINITY.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    assert!(
        result["V"].is_null(),
        "COMP-1 -Infinity should decode to null"
    );
}

#[test]
fn comp1_decode_nan_returns_null() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = f32::NAN.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    assert!(result["V"].is_null(), "COMP-1 NaN should decode to null");
}

#[test]
fn comp2_decode_positive_infinity_returns_null() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = f64::INFINITY.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    assert!(
        result["V"].is_null(),
        "COMP-2 +Infinity should decode to null"
    );
}

#[test]
fn comp2_decode_negative_infinity_returns_null() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = f64::NEG_INFINITY.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    assert!(
        result["V"].is_null(),
        "COMP-2 -Infinity should decode to null"
    );
}

#[test]
fn comp2_decode_nan_returns_null() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = f64::NAN.to_be_bytes();
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    assert!(result["V"].is_null(), "COMP-2 NaN should decode to null");
}

// =========================================================================
// Subnormal numbers
// =========================================================================

#[test]
fn comp1_decode_subnormal() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    // Smallest positive subnormal f32: 0x00000001
    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x01];
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val > 0.0, "Subnormal f32 should be positive");
    assert!(
        val < f64::from(f32::MIN_POSITIVE),
        "Subnormal < MIN_POSITIVE"
    );
}

#[test]
fn comp2_decode_subnormal() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    // Smallest positive subnormal f64: 0x0000000000000001
    let data: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01];
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val > 0.0, "Subnormal f64 should be positive");
    assert!(val < f64::MIN_POSITIVE, "Subnormal < MIN_POSITIVE");
}

#[test]
fn comp1_roundtrip_subnormal() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let subnormal_data: [u8; 4] = [0x00, 0x00, 0x00, 0x01];
    let decoded = decode_record(&schema, &subnormal_data, &native_decode()).unwrap();
    let encoded = encode_record(&schema, &decoded, &encode_opts()).unwrap();
    assert_eq!(subnormal_data.to_vec(), encoded, "Subnormal f32 roundtrip");
}

// =========================================================================
// IEEE 754 precision edge cases
// =========================================================================

#[test]
fn comp1_precision_loss_from_f64() {
    // 1.1 cannot be exactly represented in f32
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": 1.1 });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &native_decode()).unwrap();
    let val = decoded["V"].as_f64().unwrap();
    // f32 precision: ~7 decimal digits
    assert!((val - 1.1).abs() < 1e-6);
}

#[test]
fn comp2_preserves_double_precision() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let precise = 1.000_000_000_000_001_f64;
    let json = serde_json::json!({ "V": precise });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    let decoded = decode_record(&schema, &encoded, &native_decode()).unwrap();
    let val = decoded["V"].as_f64().unwrap();
    assert!((val - precise).abs() < f64::EPSILON);
}

#[test]
fn comp1_negative_zero() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    // IEEE 754 -0.0f32 = 0x80000000
    let data: [u8; 4] = [0x80, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    // serde_json treats -0.0 and 0.0 the same
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn comp2_negative_zero() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    // IEEE 754 -0.0f64 = 0x8000000000000000
    let data: [u8; 8] = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &native_decode()).unwrap();
    let val = result["V"].as_f64().unwrap();
    assert!(val.abs() < f64::EPSILON);
}

// =========================================================================
// JSON number mode interaction (lossless vs native)
// =========================================================================

#[test]
fn comp1_lossless_mode_returns_number() {
    // Float fields always return JSON numbers regardless of mode
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = 2.5_f32.to_be_bytes();
    let result = decode_record(&schema, &data, &lossless_decode()).unwrap();
    assert!(
        result["V"].is_number(),
        "Lossless mode should still return number for floats"
    );
    let val = result["V"].as_f64().unwrap();
    assert!((val - 2.5).abs() < f64::EPSILON);
}

#[test]
fn comp2_lossless_mode_returns_number() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data = 2.5_f64.to_be_bytes();
    let result = decode_record(&schema, &data, &lossless_decode()).unwrap();
    assert!(
        result["V"].is_number(),
        "Lossless mode should still return number for floats"
    );
    let val = result["V"].as_f64().unwrap();
    assert!((val - 2.5).abs() < f64::EPSILON);
}

#[test]
fn comp1_native_and_lossless_agree_on_finite_values() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data = 42.0_f32.to_be_bytes();
    let native_result = decode_record(&schema, &data, &native_decode()).unwrap();
    let lossless_result = decode_record(&schema, &data, &lossless_decode()).unwrap();
    assert_eq!(native_result["V"], lossless_result["V"]);
}

// =========================================================================
// Mixed record with COMP-1 and COMP-2 fields
// =========================================================================

#[test]
fn mixed_record_comp1_and_comp2() {
    let copybook = r"
01 RECORD.
   05 ID         PIC 9(4).
   05 RATE-F32   COMP-1.
   05 RATE-F64   COMP-2.
   05 LABEL      PIC X(5).
";
    let schema = parse_copybook(copybook).unwrap();
    // Total: 4 + 4 + 8 + 5 = 21 bytes
    let json = serde_json::json!({
        "ID": "0042",
        "RATE-F32": 1.5,
        "RATE-F64": 3.14159265358979,
        "LABEL": "HELLO"
    });

    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(encoded.len(), 21);

    let decoded = decode_record(&schema, &encoded, &native_decode()).unwrap();
    assert_eq!(decoded["ID"], serde_json::json!("0042"));

    let f32_val = decoded["RATE-F32"].as_f64().unwrap();
    assert!((f32_val - 1.5).abs() < 1e-5);

    let f64_val = decoded["RATE-F64"].as_f64().unwrap();
    assert!((f64_val - 3.14159265358979).abs() < 1e-10);

    assert_eq!(decoded["LABEL"], serde_json::json!("HELLO"));
}

#[test]
fn mixed_record_multiple_floats_roundtrip() {
    let copybook = r"
01 REC.
   05 A COMP-1.
   05 B COMP-2.
   05 C COMP-1.
";
    let schema = parse_copybook(copybook).unwrap();
    // 4 + 8 + 4 = 16 bytes
    let json = serde_json::json!({
        "A": 1.0,
        "B": -999.999,
        "C": 0.0
    });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    assert_eq!(encoded.len(), 16);

    let decoded = decode_record(&schema, &encoded, &native_decode()).unwrap();
    let a = decoded["A"].as_f64().unwrap();
    let b = decoded["B"].as_f64().unwrap();
    let c = decoded["C"].as_f64().unwrap();
    assert!((a - 1.0).abs() < f64::EPSILON);
    assert!((b - (-999.999)).abs() < 1e-3);
    assert!(c.abs() < f64::EPSILON);
}

// =========================================================================
// Schema field kind verification
// =========================================================================

#[test]
fn schema_comp1_has_float_single_kind() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert!(
        matches!(schema.fields[0].kind, FieldKind::FloatSingle),
        "COMP-1 should produce FloatSingle, got {:?}",
        schema.fields[0].kind
    );
    assert_eq!(schema.fields[0].len, 4);
}

#[test]
fn schema_comp2_has_float_double_kind() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    assert_eq!(schema.fields.len(), 1);
    assert!(
        matches!(schema.fields[0].kind, FieldKind::FloatDouble),
        "COMP-2 should produce FloatDouble, got {:?}",
        schema.fields[0].kind
    );
    assert_eq!(schema.fields[0].len, 8);
}

#[test]
fn schema_comp1_in_group_has_correct_offset() {
    let copybook = r"
01 REC.
   05 A PIC X(10).
   05 B COMP-1.
";
    let schema = parse_copybook(copybook).unwrap();
    let group = &schema.fields[0];
    assert_eq!(group.children.len(), 2);
    let b = &group.children[1];
    assert!(matches!(b.kind, FieldKind::FloatSingle));
    assert_eq!(b.offset, 10, "COMP-1 should start at offset 10");
    assert_eq!(b.len, 4);
}

// =========================================================================
// Error: wrong byte count for float (record too short)
// =========================================================================

#[test]
fn comp1_decode_record_too_short() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let data: [u8; 3] = [0x3F, 0x80, 0x00]; // Only 3 bytes, need 4
    let result = decode_record(&schema, &data, &native_decode());
    assert!(
        result.is_err(),
        "Should fail with record too short for COMP-1"
    );
}

#[test]
fn comp2_decode_record_too_short() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let data: [u8; 7] = [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00]; // 7 bytes, need 8
    let result = decode_record(&schema, &data, &native_decode());
    assert!(
        result.is_err(),
        "Should fail with record too short for COMP-2"
    );
}

// =========================================================================
// Error: non-numeric JSON input for float field
// =========================================================================

#[test]
fn comp1_encode_boolean_input_error() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": true });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(result.is_err(), "Boolean input for COMP-1 should fail");
}

#[test]
fn comp2_encode_boolean_input_error() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": false });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(result.is_err(), "Boolean input for COMP-2 should fail");
}

#[test]
fn comp1_encode_array_input_error() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": [1, 2, 3] });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(result.is_err(), "Array input for COMP-1 should fail");
}

#[test]
fn comp2_encode_object_input_error() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": {"nested": 1} });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(result.is_err(), "Object input for COMP-2 should fail");
}

#[test]
fn comp1_encode_non_numeric_string_error() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": "not_a_number" });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(result.is_err(), "Non-numeric string for COMP-1 should fail");
}

#[test]
fn comp2_encode_non_numeric_string_error() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": "hello" });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(result.is_err(), "Non-numeric string for COMP-2 should fail");
}

// =========================================================================
// Encode from string numeric input
// =========================================================================

#[test]
fn comp1_encode_string_numeric_input() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": "1.5" });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    let expected = 1.5_f32.to_be_bytes().to_vec();
    assert_eq!(encoded, expected);
}

#[test]
fn comp2_encode_string_numeric_input() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": "3.14" });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    let expected = 3.14_f64.to_be_bytes().to_vec();
    assert_eq!(encoded, expected);
}

// =========================================================================
// Encode null → NaN roundtrip
// =========================================================================

#[test]
fn comp1_encode_null_produces_nan_bytes() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    let json = serde_json::json!({ "V": null });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    // NaN encoded as f32 BE
    let f = f32::from_be_bytes([encoded[0], encoded[1], encoded[2], encoded[3]]);
    assert!(f.is_nan(), "null should encode as NaN for COMP-1");
}

#[test]
fn comp2_encode_null_produces_nan_bytes() {
    let schema = parse_copybook("01 V COMP-2.").unwrap();
    let json = serde_json::json!({ "V": null });
    let encoded = encode_record(&schema, &json, &encode_opts()).unwrap();
    let f = f64::from_be_bytes([
        encoded[0], encoded[1], encoded[2], encoded[3], encoded[4], encoded[5], encoded[6],
        encoded[7],
    ]);
    assert!(f.is_nan(), "null should encode as NaN for COMP-2");
}

// =========================================================================
// COMP-1 encode overflow (f64 value too large for f32)
// =========================================================================

#[test]
fn comp1_encode_overflow_rejects_too_large() {
    let schema = parse_copybook("01 V COMP-1.").unwrap();
    // f64::MAX >> f32::MAX, should trigger CBKE531
    let json = serde_json::json!({ "V": f64::MAX });
    let result = encode_record(&schema, &json, &encode_opts());
    assert!(result.is_err(), "f64::MAX should overflow COMP-1 field");
}
