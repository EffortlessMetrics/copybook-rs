//! E3.1 Edited PIC Encode Tests
//!
//! Tests for minimal edited PIC encode path (E3.1).
//! Covers digits, decimal point, zero suppress, zero insert, and basic sign handling.
//!
//! Note: This file tests edited PIC encoding only. Standard numeric PIC patterns
//! like "9(5)" or "9.99" are handled by different encoding logic
//! and are not tested here.

use copybook_codec::{DecodeOptions, EncodeOptions, decode_record, encode_record};
use copybook_core::parse_copybook;

/// Test fixture for a single edited PIC encode/decode roundtrip
struct EditedPicTest {
    name: &'static str,
    pic: &'static str,
    input_value: &'static str,
    expected_encoded: &'static str, // Expected EBCDIC bytes as hex string (for verification)
}

const E31_TEST_FIXTURES: &[EditedPicTest] = &[
    // Zero suppression
    EditedPicTest {
        name: "zero_suppression",
        pic: "ZZZ9",
        input_value: "123",
        expected_encoded: "40F1F2F3", // EBCDIC: space then "123"
    },
    // Zero insert
    EditedPicTest {
        name: "zero_insert",
        pic: "0009",
        input_value: "123",
        expected_encoded: "F0F1F2F3", // EBCDIC for "0123"
    },
    // Leading plus
    EditedPicTest {
        name: "leading_plus_positive",
        pic: "+999",
        input_value: "123",
        expected_encoded: "4EF1F2F3", // EBCDIC for "+123"
    },
    // Leading minus
    EditedPicTest {
        name: "leading_minus_positive",
        pic: "-999",
        input_value: "123",
        expected_encoded: "40F1F2F3", // EBCDIC: space then "123"
    },
];

/// Create a copybook with a single edited PIC field
fn create_edited_pic_copybook(pic: &str) -> String {
    format!(
        r#"
       01 TEST-RECORD.
          05 TEST-FIELD PIC {}.
"#,
        pic
    )
}

/// Parse hex string to bytes
fn hex_to_bytes(hex_str: &str) -> Vec<u8> {
    let mut bytes = Vec::new();
    for i in (0..hex_str.len()).step_by(2) {
        let byte_str = &hex_str[i..i + 2];
        let byte = u8::from_str_radix(byte_str, 16).unwrap_or(0);
        bytes.push(byte);
    }
    bytes
}

#[test]
fn test_e31_zero_suppression_encode() {
    let fixture = &E31_TEST_FIXTURES[0];
    let copybook_text = create_edited_pic_copybook(fixture.pic);
    let schema = parse_copybook(&copybook_text).unwrap();

    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        "TEST-FIELD".to_string(),
        serde_json::Value::String(fixture.input_value.to_string()),
    );
    let json = serde_json::Value::Object(json_obj);

    let result = encode_record(&schema, &json, &EncodeOptions::default()).unwrap();
    let expected = hex_to_bytes(fixture.expected_encoded);

    assert_eq!(
        result, expected,
        "Encoded bytes don't match for {}",
        fixture.name
    );
}

#[test]
fn test_e31_zero_insert_encode() {
    let fixture = &E31_TEST_FIXTURES[1];
    let copybook_text = create_edited_pic_copybook(fixture.pic);
    let schema = parse_copybook(&copybook_text).unwrap();

    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        "TEST-FIELD".to_string(),
        serde_json::Value::String(fixture.input_value.to_string()),
    );
    let json = serde_json::Value::Object(json_obj);

    let result = encode_record(&schema, &json, &EncodeOptions::default()).unwrap();
    let expected = hex_to_bytes(fixture.expected_encoded);

    assert_eq!(
        result, expected,
        "Encoded bytes don't match for {}",
        fixture.name
    );
}

#[test]
fn test_e31_leading_plus_encode() {
    let fixture = &E31_TEST_FIXTURES[2];
    let copybook_text = create_edited_pic_copybook(fixture.pic);
    let schema = parse_copybook(&copybook_text).unwrap();

    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        "TEST-FIELD".to_string(),
        serde_json::Value::String(fixture.input_value.to_string()),
    );
    let json = serde_json::Value::Object(json_obj);

    let result = encode_record(&schema, &json, &EncodeOptions::default()).unwrap();
    let expected = hex_to_bytes(fixture.expected_encoded);

    assert_eq!(
        result, expected,
        "Encoded bytes don't match for {}",
        fixture.name
    );
}

#[test]
fn test_e31_leading_minus_encode() {
    let fixture = &E31_TEST_FIXTURES[3];
    let copybook_text = create_edited_pic_copybook(fixture.pic);
    let schema = parse_copybook(&copybook_text).unwrap();

    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        "TEST-FIELD".to_string(),
        serde_json::Value::String(fixture.input_value.to_string()),
    );
    let json = serde_json::Value::Object(json_obj);

    let result = encode_record(&schema, &json, &EncodeOptions::default()).unwrap();
    let expected = hex_to_bytes(fixture.expected_encoded);

    assert_eq!(
        result, expected,
        "Encoded bytes don't match for {}",
        fixture.name
    );
}

#[test]
fn test_e31_roundtrip_zero_suppression() {
    let copybook_text = create_edited_pic_copybook("ZZZ9");
    let schema = parse_copybook(&copybook_text).unwrap();

    let encode_options = EncodeOptions::default();
    let decode_options = DecodeOptions::default();

    let original = "123";

    // Encode
    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        "TEST-FIELD".to_string(),
        serde_json::Value::String(original.to_string()),
    );
    let json = serde_json::Value::Object(json_obj);
    let encoded = encode_record(&schema, &json, &encode_options).unwrap();

    // Decode
    let decoded = decode_record(&schema, &encoded, &decode_options).unwrap();
    let decoded_value = decoded["fields"]["TEST-FIELD"].as_str().unwrap();

    assert_eq!(
        decoded_value, original,
        "Roundtrip failed for zero suppression"
    );
}

#[test]
fn test_e31_roundtrip_leading_plus() {
    let copybook_text = create_edited_pic_copybook("+999");
    let schema = parse_copybook(&copybook_text).unwrap();

    let encode_options = EncodeOptions::default();
    let decode_options = DecodeOptions::default();

    let original = "123";

    // Encode
    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        "TEST-FIELD".to_string(),
        serde_json::Value::String(original.to_string()),
    );
    let json = serde_json::Value::Object(json_obj);
    let encoded = encode_record(&schema, &json, &encode_options).unwrap();

    // Decode
    let decoded = decode_record(&schema, &encoded, &decode_options).unwrap();
    let decoded_value = decoded["fields"]["TEST-FIELD"].as_str().unwrap();

    assert_eq!(decoded_value, original, "Roundtrip failed for leading plus");
}

#[test]
fn test_e31_roundtrip_leading_minus_negative() {
    let copybook_text = create_edited_pic_copybook("-999");
    let schema = parse_copybook(&copybook_text).unwrap();

    let encode_options = EncodeOptions::default();
    let decode_options = DecodeOptions::default();

    let original = "-123";

    // Encode
    let mut json_obj = serde_json::Map::new();
    json_obj.insert(
        "TEST-FIELD".to_string(),
        serde_json::Value::String(original.to_string()),
    );
    let json = serde_json::Value::Object(json_obj);
    let encoded = encode_record(&schema, &json, &encode_options).unwrap();

    // Decode
    let decoded = decode_record(&schema, &encoded, &decode_options).unwrap();
    let decoded_value = decoded["fields"]["TEST-FIELD"].as_str().unwrap();

    assert_eq!(
        decoded_value, original,
        "Roundtrip failed for leading minus negative"
    );
}
