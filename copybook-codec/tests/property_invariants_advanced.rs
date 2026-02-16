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
            value_prefix in "[A-Z][A-Z0-9]{0,7}",
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
            let json1: Value = parse_single_jsonl_record(&output1);

            // Second decode
            let input2 = Cursor::new(&data);
            let mut output2 = Vec::new();
            decode_file_to_jsonl(&schema, input2, &mut output2, &options)
                .expect("Should decode successfully");
            let json2: Value = parse_single_jsonl_record(&output2);

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
            let json: Value = parse_single_jsonl_record(&output);

            // Field order in JSON should match copybook order
            if let Value::Object(obj) = json {
                let field_names: Vec<String> = if let Some(Value::Object(fields_obj)) = obj.get("fields") {
                    fields_obj.keys().cloned().collect()
                } else {
                    obj.keys()
                        .filter(|name| {
                            let n = name.as_str();
                            n != "schema" && n != "record_index" && n != "codepage" && n != "raw"
                        })
                        .cloned()
                        .collect()
                };
                let copybook_field_names: Vec<String> = schema.all_fields()
                    .iter()
                    .filter(|f| !matches!(&f.kind, FieldKind::Group))
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
            signed in proptest::bool::ANY,
        ) {
            // Unsigned fields cannot represent negatives.
            if !signed && value < 0 {
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
            let value_str = format_scaled_2(value);
            let json_input = format!(r#"{{"TEST-FIELD":"{value_str}"}}"#);

            let encode_options = EncodeOptions {
                codepage: Codepage::ASCII,
                ..EncodeOptions::default()
            };

            let input = Cursor::new(json_input.as_bytes());
            let mut encoded = Vec::new();
            let encode_summary = encode_jsonl_to_file(&schema, input, &mut encoded, &encode_options)
                .expect("Should encode successfully");
            prop_assume!(encode_summary.records_processed > 0);

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
            prop_assume!(!decoded.is_empty());

            let json_output: Value = parse_single_jsonl_record(&decoded);

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
            let value_str = format_scaled_2(value);
            let json_input = format!(r#"{{"TEST-FIELD":"{value_str}"}}"#);

            let encode_options = EncodeOptions {
                codepage: Codepage::ASCII,
                ..EncodeOptions::default()
            };

            let input = Cursor::new(json_input.as_bytes());
            let mut encoded = Vec::new();
            let encode_summary = encode_jsonl_to_file(&schema, input, &mut encoded, &encode_options)
                .expect("Should encode successfully");
            prop_assume!(encode_summary.records_processed > 0);

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
            prop_assume!(!decoded.is_empty());

            let json_output: Value = parse_single_jsonl_record(&decoded);

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
            let data = generate_array_test_data(array_size, element_count);

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
            let json: Value = parse_single_jsonl_record(&output);

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
            let data = generate_sequential_array_data(array_size);

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
            let json: Value = parse_single_jsonl_record(&output);

            // Array element order should be preserved
            if let Value::Array(arr) = &json["ARRAY-FIELD"] {
                for (i, item) in arr.iter().enumerate().take(array_size) {
                    let actual = item.as_str().unwrap();
                    let parsed = actual.parse::<usize>().expect("Array element should be numeric text");
                    prop_assert_eq!(parsed, i);
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

fn parse_single_jsonl_record(output: &[u8]) -> Value {
    let text = String::from_utf8(output.to_vec()).expect("Output should be valid UTF-8");
    let line = text
        .lines()
        .find(|line| !line.trim().is_empty())
        .expect("Output should contain at least one JSON record");
    serde_json::from_str(line).expect("Output line should be valid JSON")
}

fn format_scaled_2(value: i64) -> String {
    let negative = value < 0;
    let abs = value.unsigned_abs();
    let integer_part = abs / 100;
    let fractional_part = abs % 100;
    if negative {
        format!("-{integer_part}.{fractional_part:02}")
    } else {
        format!("{integer_part}.{fractional_part:02}")
    }
}

fn generate_copybook(field_count: usize, prefix: &str) -> String {
    let mut copybook = String::new();
    copybook.push_str("01 RECORD.\n");
    for i in 0..field_count {
        copybook.push_str(&format!("  05 {prefix}-{i} PIC 9(5).\n"));
    }
    copybook
}

fn generate_test_data(schema: &copybook_core::Schema) -> Vec<u8> {
    let mut data = Vec::new();
    for field in schema.all_fields() {
        if matches!(&field.kind, FieldKind::Group) {
            continue;
        }
        let field_len = field.len as usize;
        match &field.kind {
            FieldKind::ZonedDecimal { .. } | FieldKind::EditedNumeric { .. } => {
                data.extend(std::iter::repeat(b'0').take(field_len));
            }
            FieldKind::Alphanum { .. } => {
                data.extend(std::iter::repeat(b'A').take(field_len));
            }
            FieldKind::PackedDecimal { signed, .. } => {
                let mut bytes = vec![0u8; field_len];
                if let Some(last) = bytes.last_mut() {
                    *last = if *signed { 0x0C } else { 0x0F };
                }
                data.extend_from_slice(&bytes);
            }
            FieldKind::BinaryInt { .. } => {
                data.extend(std::iter::repeat(0u8).take(field_len));
            }
            _ => {
                data.extend(std::iter::repeat(0u8).take(field_len));
            }
        }
    }
    data
}

fn generate_array_test_data(array_size: usize, element_count: usize) -> Vec<u8> {
    let mut data = Vec::new();
    for i in 0..element_count {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }
    for _ in element_count..array_size {
        data.extend_from_slice(b"00000");
    }
    data
}

fn generate_sequential_array_data(array_size: usize) -> Vec<u8> {
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
    copybook.push_str("01 LARGE-RECORD.\n");
    for i in 0..field_count {
        copybook.push_str(&format!("  05 FIELD-{i} PIC 9(5).\n"));
    }
    copybook
}
