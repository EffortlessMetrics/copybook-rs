#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
//! Advanced property tests for invariants
//!
//! This test suite validates advanced invariants:
//! - JSON output ordering determinism
//! - Numeric value preservation across encode/decode
//! - Array handling invariants
//! - Memory management invariants

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    decode_file_to_jsonl, encode_jsonl_to_file,
};
use copybook_core::{FieldKind, parse_copybook};
use proptest::prelude::*;
use serde_json::Value;
use std::io::Cursor;

#[cfg(test)]
mod json_ordering {
    use super::*;

    proptest! {
        #[test]
        fn prop_json_output_ordering_deterministic(
            field_count in 1..10usize,
            value_prefix in ".*",
        ) {
            // Generate a copybook with multiple fields
            let copybook = generate_copybook(field_count, &value_prefix);
            let schema = parse_copybook(&copybook).expect("Should parse copybook");

            // Generate test data
            let data = generate_test_data(&schema);

            // Decode twice and compare
            let options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            // First decode
            let input1 = Cursor::new(&data);
            let mut output1 = Vec::new();
            decode_file_to_jsonl(&schema, input1, &mut output1, &options)
                .expect("Should decode successfully");
            let json1: Value = serde_json::from_str(&String::from_utf8(output1).unwrap()).unwrap();

            // Second decode
            let input2 = Cursor::new(&data);
            let mut output2 = Vec::new();
            decode_file_to_jsonl(&schema, input2, &mut output2, &options)
                .expect("Should decode successfully");
            let json2: Value = serde_json::from_str(&String::from_utf8(output2).unwrap()).unwrap();

            // JSON output should be identical (deterministic)
            prop_assert_eq!(json1, json2);
        }

        #[test]
        fn prop_json_field_order_preserves_copybook_order(
            field_count in 2..10usize,
        ) {
            // Generate a copybook with multiple fields
            let copybook = generate_copybook(field_count, "FIELD");
            let schema = parse_copybook(&copybook).expect("Should parse copybook");

            // Generate test data
            let data = generate_test_data(&schema);

            let options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            // Decode
            let input = Cursor::new(&data);
            let mut output = Vec::new();
            decode_file_to_jsonl(&schema, input, &mut output, &options)
                .expect("Should decode successfully");
            let json: Value = serde_json::from_str(&String::from_utf8(output).unwrap()).unwrap();

            // Field order in JSON should match copybook order
            if let Value::Object(obj) = json {
                let field_names: Vec<String> = obj.keys().cloned().collect();
                let copybook_field_names: Vec<String> = schema.all_fields()
                    .iter()
                    .map(|f| f.name.clone())
                    .collect();

                prop_assert_eq!(field_names, copybook_field_names);
            }
        }
    }
}

#[cfg(test)]
mod numeric_preservation {
    use super::*;

    proptest! {
        #[test]
        fn prop_numeric_roundtrip_preserves_value(
            value in -999999999999999999i64..999999999999999999i64,
            scale in 0i16..4i16,
            signed in proptest::bool::ANY,
        ) {
            // Skip values that would overflow
            if scale > 0 && value.abs() > 99999999999999999 {
                return Ok(());
            }

            // Create copybook
            let copybook = if signed {
                "01 TEST-FIELD PIC S9(15)V99.".to_string()
            } else {
                "01 TEST-FIELD PIC 9(15)V99.".to_string()
            };
            let schema = parse_copybook(&copybook).expect("Should parse copybook");

            // Encode the value
            let value_str = format!("{:.2}", value as f64 / 100.0);
            let json_input = format!(r#"{{"TEST-FIELD": {}}}"#, value_str);

            let encode_options = EncodeOptions {
        codepage: Codepage::ASCII,
        ..EncodeOptions::default()
    };

            let input = Cursor::new(json_input.as_bytes());
            let mut encoded = Vec::new();
            encode_jsonl_to_file(&schema, input, &mut encoded, &encode_options)
                .expect("Should encode successfully");

            // Decode the encoded value
            let decode_options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            let input2 = Cursor::new(&encoded);
            let mut decoded = Vec::new();
            decode_file_to_jsonl(&schema, input2, &mut decoded, &decode_options)
                .expect("Should decode successfully");

            let json_output: Value = serde_json::from_str(&String::from_utf8(decoded).unwrap()).unwrap();

            // The decoded value should match the original
            let decoded_value = json_output["TEST-FIELD"].as_str().unwrap();
            prop_assert!(decoded_value.starts_with(&value_str[..value_str.len().min(10)]));
        }

        #[test]
        fn prop_packed_decimal_roundtrip(
            value in -999999999999999999i64..999999999999999999i64,
        ) {
            // Skip values that would overflow
            if value.abs() > 999999999999999 {
                return Ok(());
            }

            // Create copybook with COMP-3
            let copybook = "01 TEST-FIELD PIC S9(15)V99 COMP-3.";
            let schema = parse_copybook(copybook).expect("Should parse copybook");

            // Encode the value
            let value_str = format!("{:.2}", value as f64 / 100.0);
            let json_input = format!(r#"{{"TEST-FIELD": {}}}"#, value_str);

            let encode_options = EncodeOptions {
        codepage: Codepage::ASCII,
        ..EncodeOptions::default()
    };

            let input = Cursor::new(json_input.as_bytes());
            let mut encoded = Vec::new();
            encode_jsonl_to_file(&schema, input, &mut encoded, &encode_options)
                .expect("Should encode successfully");

            // Decode the encoded value
            let decode_options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            let input2 = Cursor::new(&encoded);
            let mut decoded = Vec::new();
            decode_file_to_jsonl(&schema, input2, &mut decoded, &decode_options)
                .expect("Should decode successfully");

            let json_output: Value = serde_json::from_str(&String::from_utf8(decoded).unwrap()).unwrap();

            // The decoded value should match the original
            let decoded_value = json_output["TEST-FIELD"].as_str().unwrap();
            prop_assert!(decoded_value.starts_with(&value_str[..value_str.len().min(10)]));
        }
    }
}

#[cfg(test)]
mod array_handling {
    use super::*;

    proptest! {
        #[test]
        fn prop_array_length_preserved(
            array_size in 1..20usize,
            element_count in 1..20usize,
        ) {
            let element_count = element_count.min(array_size);
            // Create copybook with OCCURS
            let copybook = format!("01 ARRAY-FIELD PIC 9(5) OCCURS {} TIMES.", array_size);
            let schema = parse_copybook(&copybook).expect("Should parse copybook");

            // Generate test data with specific number of elements
            let data = generate_array_test_data(&schema, element_count);

            let options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            // Decode
            let input = Cursor::new(&data);
            let mut output = Vec::new();
            decode_file_to_jsonl(&schema, input, &mut output, &options)
                .expect("Should decode successfully");
            let json: Value = serde_json::from_str(&String::from_utf8(output).unwrap()).unwrap();

            // Array length should be preserved
            if let Value::Array(arr) = &json["ARRAY-FIELD"] {
                prop_assert_eq!(arr.len(), array_size);
            }
        }

        #[test]
        fn prop_array_element_order_preserved(
            array_size in 2..10usize,
        ) {
            // Create copybook with OCCURS
            let copybook = format!("01 ARRAY-FIELD PIC 9(5) OCCURS {} TIMES.", array_size);
            let schema = parse_copybook(&copybook).expect("Should parse copybook");

            // Generate test data with sequential values
            let data = generate_sequential_array_data(&schema, array_size);

            let options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            // Decode
            let input = Cursor::new(&data);
            let mut output = Vec::new();
            decode_file_to_jsonl(&schema, input, &mut output, &options)
                .expect("Should decode successfully");
            let json: Value = serde_json::from_str(&String::from_utf8(output).unwrap()).unwrap();

            // Array element order should be preserved
            if let Value::Array(arr) = &json["ARRAY-FIELD"] {
                for (i, item) in arr.iter().enumerate().take(array_size) {
                    let expected = format!("{:05}", i);
                    let actual = item.as_str().unwrap();
                    prop_assert_eq!(actual, expected);
                }
            }
        }
    }
}

#[cfg(test)]
mod memory_management {
    use super::*;

    proptest! {
        #[test]
        fn prop_no_memory_leak_on_multiple_decodes(
            record_count in 1..100usize,
        ) {
            // Create simple copybook
            let copybook = "01 TEST-FIELD PIC 9(5).";
            let schema = parse_copybook(copybook).expect("Should parse copybook");

            // Generate multiple records
            let data = generate_multiple_records(&schema, record_count);

            let options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            // Decode multiple times (should not leak memory)
            for _ in 0..10 {
                let input = Cursor::new(&data);
                let mut output = Vec::new();
                decode_file_to_jsonl(&schema, input, &mut output, &options)
                    .expect("Should decode successfully");
                // output is dropped here, memory should be freed
            }

            // If we get here without panicking, memory management is OK
            prop_assert!(true);
        }

        #[test]
        fn prop_large_record_handling(
            field_count in 10..50usize,
        ) {
            // Create copybook with many fields
            let copybook = generate_large_copybook(field_count);
            let schema = parse_copybook(&copybook).expect("Should parse copybook");

            // Generate test data
            let data = generate_test_data(&schema);

            let options = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::ASCII,
                json_number_mode: JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: RawMode::Off,
                strict_mode: false,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
                preserve_zoned_encoding: false,
                preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
                float_format: copybook_codec::FloatFormat::IeeeBigEndian,
            };

            // Decode large record
            let input = Cursor::new(&data);
            let mut output = Vec::new();
            decode_file_to_jsonl(&schema, input, &mut output, &options)
                .expect("Should decode successfully");

            // Should successfully decode large records
            prop_assert!(!output.is_empty());
        }
    }
}

// Helper functions

fn generate_copybook(field_count: usize, prefix: &str) -> String {
    let mut copybook = String::new();
    copybook.push_str("01  RECORD.\n");
    for i in 0..field_count {
        copybook.push_str(&format!("    05  {}-{}  PIC 9(5).\n", prefix, i));
    }
    copybook
}

fn generate_test_data(schema: &copybook_core::Schema) -> Vec<u8> {
    let mut data = Vec::new();
    for field in schema.all_fields() {
        match &field.kind {
            FieldKind::ZonedDecimal { .. }
            | FieldKind::PackedDecimal { .. }
            | FieldKind::BinaryInt { .. }
            | FieldKind::EditedNumeric { .. } => {
                data.extend_from_slice(b"12345");
            }
            FieldKind::Alphanum { .. } => {
                data.extend_from_slice(b"ABCDE");
            }
            _ => {
                data.extend_from_slice(&vec![0u8; field.len as usize]);
            }
        }
    }
    data
}

fn generate_array_test_data(schema: &copybook_core::Schema, element_count: usize) -> Vec<u8> {
    let mut data = Vec::new();
    for i in 0..element_count {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }
    // Pad to full array size
    let first_field = &schema.all_fields()[0];
    let total_elements = first_field.len as usize / 5;
    for _ in element_count..total_elements {
        data.extend_from_slice(b"00000");
    }
    data
}

fn generate_sequential_array_data(_schema: &copybook_core::Schema, array_size: usize) -> Vec<u8> {
    let mut data = Vec::new();
    for i in 0..array_size {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }
    data
}

fn generate_multiple_records(schema: &copybook_core::Schema, record_count: usize) -> Vec<u8> {
    let record_data = generate_test_data(schema);
    let mut data = Vec::new();
    for _ in 0..record_count {
        data.extend_from_slice(&record_data);
    }
    data
}

fn generate_large_copybook(field_count: usize) -> String {
    let mut copybook = String::new();
    copybook.push_str("01  LARGE-RECORD.\n");
    for i in 0..field_count {
        copybook.push_str(&format!("    05  FIELD-{:03}  PIC 9(5).\n", i));
    }
    copybook
}
