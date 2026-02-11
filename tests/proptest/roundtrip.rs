#![allow(unused_doc_comments, unused_imports, dead_code)]
//! Property tests for round-trip encoding/decoding
//!
//! These tests verify that encode(decode(data)) == data for various
//! data types and encodings, ensuring data integrity through the
//! encode/decode cycle.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::numeric::encode_zoned_decimal;
use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use proptest::prelude::*;
use rand::{RngCore, SeedableRng};
use serde_json::Value;

use super::config::*;
use super::generators::*;
use super::schema_record_length;

/// Property: ASCII zoned decimal round-trip preserves data
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_ascii_zoned_roundtrip_preserves_data(
        digits in prop::collection::vec(0u8..=9, 1..=18)
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", digits.len());
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Convert to ASCII zoned decimal
        let original_data: Vec<u8> = digits.iter().map(|&d| b'0' + d).collect();

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_zoned_encoding_format(copybook_codec::ZonedEncodingFormat::Ascii);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: EBCDIC zoned decimal round-trip preserves data
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_ebcdic_zoned_roundtrip_preserves_data(
        digits in prop::collection::vec(0u8..=9, 1..=18)
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", digits.len());
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Convert to EBCDIC zoned decimal
        let original_data: Vec<u8> = digits.iter().map(|&d| 0xF0 + d).collect();

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037)
            .with_zoned_encoding_format(copybook_codec::ZonedEncodingFormat::Ebcdic);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Signed ASCII zoned decimal round-trip preserves sign
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_ascii_signed_zoned_roundtrip_preserves_sign(
        digits in prop::collection::vec(0u8..=9, 2..=18),
        negative in any::<bool>()
    ) {
        let copybook = format!("01 FIELD PIC S9({}).", digits.len());
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let digit_str: String = digits
            .iter()
            .map(|&d| (b'0' + d) as char)
            .collect();
        let value_str = if negative {
            format!("-{digit_str}")
        } else {
            digit_str
        };
        let original_data = encode_zoned_decimal(
            &value_str,
            digits.len() as u16,
            0,
            true,
            Codepage::ASCII,
        )
        .expect("Failed to encode zoned decimal");

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_preserve_zoned_encoding(true);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_zoned_encoding_format(copybook_codec::ZonedEncodingFormat::Ascii);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Alphanumeric ASCII round-trip preserves data
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_alphanumeric_ascii_roundtrip_preserves_data(
        length in 1usize..=50,
        seed in any::<u64>()
    ) {
        let copybook = format!("01 FIELD PIC X({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Generate random alphanumeric ASCII data
        let mut original_data = Vec::with_capacity(length);
        let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
        for _ in 0..length {
            original_data.push((0x20 + (rng.next_u32() % 0x5E)) as u8); // Printable ASCII
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Multi-field record round-trip preserves all fields
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES, // Slower, use fewer cases
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_multi_field_roundtrip_preserves_all_fields(
        field1_len in 1usize..=10,
        field2_len in 1usize..=10,
        field3_len in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n   05 FIELD1 PIC 9({}).\n   05 FIELD2 PIC X({}).\n   05 FIELD3 PIC 9({}).",
            field1_len, field2_len, field3_len
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Generate test data
        let mut original_data = Vec::new();
        // FIELD1: ASCII zoned
        for _ in 0..field1_len {
            original_data.push(b'0' + (rand::random::<u8>() % 10));
        }
        // FIELD2: ASCII alphanumeric
        for _ in 0..field2_len {
            original_data.push(0x20 + (rand::random::<u8>() % 0x5E));
        }
        // FIELD3: ASCII zoned
        for _ in 0..field3_len {
            original_data.push(b'0' + (rand::random::<u8>() % 10));
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: JSON encode/decode round-trip preserves structure
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_json_encode_decode_roundtrip_preserves_structure(
        num_fields in 1usize..=5,
        field_len in 1usize..=10
    ) {
        // Generate a simple copybook
        let mut copybook = "01 RECORD.\n".to_string();
        for i in 0..num_fields {
            copybook.push_str(&format!("   05 FIELD{} PIC X({}).\n", i, field_len));
        }

        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Generate JSON data
        let mut json_obj = serde_json::Map::new();
        for i in 0..num_fields {
            let value: String = (0..field_len)
                .map(|_| (0x20 + (rand::random::<u8>() % 0x5E)) as char)
                .collect();
            json_obj.insert(format!("FIELD{}", i), Value::String(value));
        }
        let json_data = Value::Object(json_obj);

        // Encode to binary
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let binary_data = copybook_codec::encode_record(&schema, &json_data, &encode_options)
            .expect("Failed to encode");

        // Decode back to JSON
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let decoded_json = copybook_codec::decode_record(&schema, &binary_data, &decode_options)
            .expect("Failed to decode");

        // Verify all fields are present
        for i in 0..num_fields {
            let field_name = format!("FIELD{}", i);
            prop_assert!(decoded_json.get(&field_name).is_some(),
                "Field {} should exist in decoded JSON", field_name);
        }
    }
}

/// Property: Empty fields round-trip correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_empty_fields_roundtrip_correctly(
        field_len in 1usize..=20
    ) {
        let copybook = format!("01 FIELD PIC X({}).", field_len);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Empty field (all spaces)
        let original_data = vec![b' '; field_len];

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Round-trip preserves data size
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_roundtrip_preserves_data_size(
        copybook in simple_copybook_strategy(),
        seed in any::<u64>()
    ) {
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Calculate expected size from schema
        let expected_size = match schema_record_length(&schema) {
            Some(size) => size as usize,
            None => return Ok(()),
        };

        // Generate random data of correct size
        let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
        // Use printable ASCII to avoid multi-byte UTF-8 expansion on decode/encode
        let original_data: Vec<u8> = (0..expected_size)
            .map(|_| ((rng.next_u32() % 95) as u8) + 32)
            .collect();

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options);

        // If decode succeeds, verify round-trip
        if let Ok(json) = json_result {
            let encode_options = EncodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(Codepage::ASCII);

            let roundtrip_data = copybook_codec::encode_record(&schema, &json, &encode_options)
                .expect("Failed to encode");

            prop_assert_eq!(roundtrip_data.len(), original_data.len());
        }
    }
}
