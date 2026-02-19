//! Golden fixture tests for COMP-1 (f32) and COMP-2 (f64) encode/decode.
//!
//! Validates full-stack encode and decode of IEEE 754 floating-point fields
//! through the schema parser and codec, including round-trip fidelity.
//! Both IEEE big-endian and IBM hexadecimal floating-point formats are covered.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;

fn decode_opts_ieee() -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number_mode: JsonNumberMode::Native,
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

fn encode_opts_ieee() -> EncodeOptions {
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
        json_number_mode: JsonNumberMode::Native,
        zoned_encoding_override: None,
        float_format: FloatFormat::IeeeBigEndian,
    }
}

fn decode_opts_ibm() -> DecodeOptions {
    DecodeOptions {
        float_format: FloatFormat::IbmHex,
        ..decode_opts_ieee()
    }
}

fn encode_opts_ibm() -> EncodeOptions {
    EncodeOptions {
        float_format: FloatFormat::IbmHex,
        ..encode_opts_ieee()
    }
}

// =============================================================================
// COMP-1 (f32 / single precision) decode golden tests — IEEE big-endian
// =============================================================================

#[test]
fn test_golden_comp1_positive_decode_ieee() {
    // IEEE 754 binary32 for 1.0: 0x3F800000
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 4] = [0x3F, 0x80, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0_f64).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp1_negative_decode_ieee() {
    // IEEE 754 binary32 for -1.0: 0xBF800000
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 4] = [0xBF, 0x80, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!((val - (-1.0_f64)).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp1_zero_decode_ieee() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 4] = [0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp1_large_positive_decode_ieee() {
    // IEEE 754 binary32 for 1.0e10 ~ 0x501502F9
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 4] = [0x50, 0x15, 0x02, 0xF9];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val > 9.0e9 && val < 1.1e10);
}

// =============================================================================
// COMP-2 (f64 / double precision) decode golden tests — IEEE big-endian
// =============================================================================

#[test]
fn test_golden_comp2_positive_decode_ieee() {
    // IEEE 754 binary64 for 1.0: 0x3FF0000000000000
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 8] = [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0_f64).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp2_negative_decode_ieee() {
    // IEEE 754 binary64 for -1.0: 0xBFF0000000000000
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 8] = [0xBF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!((val - (-1.0_f64)).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp2_zero_decode_ieee() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!(val.abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp2_pi_decode_ieee() {
    // IEEE 754 binary64 for PI: 0x400921FB54442D18
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 8] = [0x40, 0x09, 0x21, 0xFB, 0x54, 0x44, 0x2D, 0x18];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!((val - std::f64::consts::PI).abs() < 1e-14);
}

// =============================================================================
// IEEE 754 edge cases: special values
// =============================================================================

#[test]
fn test_golden_comp1_positive_infinity_decode_ieee() {
    // IEEE 754 +Infinity for f32: 0x7F800000
    // serde_json cannot represent Infinity as a JSON number; it becomes null
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 4] = [0x7F, 0x80, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should not panic");

    // Infinity becomes null in JSON (not a representable JSON number)
    assert!(
        result["RATE"].is_null() || result["RATE"].is_number(),
        "Infinity should decode to null or a number in JSON, got: {:?}",
        result["RATE"]
    );
}

#[test]
fn test_golden_comp2_negative_infinity_decode_ieee() {
    // IEEE 754 -Infinity for f64: 0xFFF0000000000000
    // serde_json cannot represent -Infinity as a JSON number; it becomes null
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 8] = [0xFF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should not panic");

    // Infinity becomes null in JSON (not a representable JSON number)
    assert!(
        result["RATE"].is_null() || result["RATE"].is_number(),
        "-Infinity should decode to null or a number in JSON, got: {:?}",
        result["RATE"]
    );
}

#[test]
fn test_golden_comp1_nan_decode_ieee() {
    // IEEE 754 NaN for f32: 0x7FC00000
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 4] = [0x7F, 0xC0, 0x00, 0x00];
    // NaN may produce null or a special JSON value; the important thing is no panic
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should not panic");
    // The result is either null or a number — both are acceptable
    assert!(result["RATE"].is_null() || result["RATE"].is_number());
}

#[test]
fn test_golden_comp2_nan_decode_ieee() {
    // IEEE 754 NaN for f64: 0x7FF8000000000000
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 8] = [0x7F, 0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ieee()).expect("should not panic");
    assert!(result["RATE"].is_null() || result["RATE"].is_number());
}

// =============================================================================
// Round-trip golden tests — IEEE big-endian
// =============================================================================

#[test]
fn test_golden_comp1_roundtrip_positive_ieee() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": 1.0_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_ieee()).expect("should encode");

    // IEEE 754 binary32 for 1.0: 0x3F800000
    assert_eq!(encoded, vec![0x3F, 0x80, 0x00, 0x00]);

    let decoded = decode_record(&schema, &encoded, &decode_opts_ieee()).expect("should decode");
    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp1_roundtrip_negative_ieee() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": -3.14_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_ieee()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_ieee()).expect("should decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    // f32 round-trip so small precision loss is expected
    assert!((val - (-3.14_f64)).abs() < 1e-5);
}

#[test]
fn test_golden_comp2_roundtrip_positive_ieee() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": 1.0_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_ieee()).expect("should encode");

    // IEEE 754 binary64 for 1.0: 0x3FF0000000000000
    assert_eq!(
        encoded,
        vec![0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );

    let decoded = decode_record(&schema, &encoded, &decode_opts_ieee()).expect("should decode");
    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp2_roundtrip_large_negative_ieee() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": -1.23e100_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_ieee()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_ieee()).expect("should decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - (-1.23e100_f64)).abs() < 1e88);
}

// =============================================================================
// IBM hexadecimal floating-point golden tests
// =============================================================================

#[test]
fn test_golden_comp1_positive_decode_ibm_hex() {
    // IBM HFP short for +1.0: 0x41100000
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 4] = [0x41, 0x10, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ibm()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0_f64).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp2_positive_decode_ibm_hex() {
    // IBM HFP long for +1.0: 0x41100000_00000000
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let data: [u8; 8] = [0x41, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let result = decode_record(&schema, &data, &decode_opts_ibm()).expect("should decode");

    let val = result["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0_f64).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp1_roundtrip_ibm_hex() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": 1.0_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_ibm()).expect("should encode");

    // IBM HFP short for 1.0: 0x41100000
    assert_eq!(encoded, vec![0x41, 0x10, 0x00, 0x00]);

    let decoded = decode_record(&schema, &encoded, &decode_opts_ibm()).expect("should decode");
    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - 1.0).abs() < f64::EPSILON);
}

#[test]
fn test_golden_comp2_roundtrip_ibm_hex() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": 0.15625_f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_ibm()).expect("should encode");

    // IBM HFP long for 0.15625: 0x40280000_00000000
    assert_eq!(
        encoded,
        vec![0x40, 0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );

    let decoded = decode_record(&schema, &encoded, &decode_opts_ibm()).expect("should decode");
    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!((val - 0.15625_f64).abs() < f64::EPSILON);
}

// =============================================================================
// COMP-1/COMP-2 in a group record (layout test)
// =============================================================================

#[test]
fn test_golden_comp_fields_in_record_roundtrip() {
    let copybook = r"
01 RECORD.
   05 ID         PIC 9(4).
   05 RATE-F32   COMP-1.
   05 RATE-F64   COMP-2.
   05 LABEL      PIC X(5).
";
    let schema = parse_copybook(copybook).expect("should parse");
    // Total: 4 + 4 + 8 + 5 = 21 bytes

    // decode_record returns a flat JSON map keyed by field name (not nested under group name)
    let json = serde_json::json!({
        "ID": "0001",
        "RATE-F32": 1.5_f64,
        "RATE-F64": 2.5_f64,
        "LABEL": "HELLO"
    });

    let encoded = encode_record(&schema, &json, &encode_opts_ieee()).expect("should encode");
    assert_eq!(encoded.len(), 21);

    let decoded = decode_record(&schema, &encoded, &decode_opts_ieee()).expect("should decode");
    assert_eq!(decoded["ID"], serde_json::json!("0001"));

    let f32_val = decoded["RATE-F32"].as_f64().expect("f32 number");
    assert!((f32_val - 1.5_f64).abs() < 1e-5);

    let f64_val = decoded["RATE-F64"].as_f64().expect("f64 number");
    assert!((f64_val - 2.5_f64).abs() < f64::EPSILON);

    assert_eq!(decoded["LABEL"], serde_json::json!("HELLO"));
}

// =============================================================================
// Subnormal / min-positive values
// =============================================================================

#[test]
fn test_golden_comp1_min_positive_roundtrip_ieee() {
    let copybook = "01 RATE COMP-1.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": f32::MIN_POSITIVE as f64 });
    let encoded = encode_record(&schema, &json, &encode_opts_ieee()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_ieee()).expect("should decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    // f32::MIN_POSITIVE = 1.1754944e-38
    assert!(val > 0.0 && val < 1e-37);
}

#[test]
fn test_golden_comp2_max_finite_roundtrip_ieee() {
    let copybook = "01 RATE COMP-2.";
    let schema = parse_copybook(copybook).expect("should parse");

    let json = serde_json::json!({ "RATE": f64::MAX });
    let encoded = encode_record(&schema, &json, &encode_opts_ieee()).expect("should encode");
    let decoded = decode_record(&schema, &encoded, &decode_opts_ieee()).expect("should decode");

    let val = decoded["RATE"].as_f64().expect("should be number");
    assert!(val.is_finite() && val > 0.0);
}
