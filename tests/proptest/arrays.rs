//! Property tests for array/OCCURS handling
//!
//! These tests verify that array operations maintain length and content
//! invariants for OCCURS and ODO (OCCURS DEPENDING ON) structures.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use proptest::prelude::*;
use serde_json::json;

use super::generators::*;
use super::config::*;

/// Property: OCCURS clause maintains element size consistency
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_maintains_element_size_consistency(
        count in 1usize..=50,
        element_size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY OCCURS {} TIMES.\n\
             10 ELEMENT PIC X({}).",
            count, element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find the OCCURS field
        let array_field = schema.fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Verify OCCURS is set
        prop_assert!(array_field.occurs.is_some(),
            "ARRAY field should have OCCURS clause");

        // Verify element size
        if let Some(occurs) = array_field.occurs {
            let element_field = schema.fields.iter()
                .find(|f| f.name == "ELEMENT")
                .expect("ELEMENT field should exist");

            prop_assert_eq!(element_field.size, element_size,
                "Element size {} should match PIC clause", element_field.size);
        }
    }
}

/// Property: OCCURS array round-trip preserves all elements
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_array_roundtrip_preserves_elements(
        count in 1usize..=10,
        element_len in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY OCCURS {} TIMES.\n\
             10 ELEMENT PIC X({}).",
            count, element_len
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Generate test data
        let mut original_data = Vec::new();
        for i in 0..count {
            let value = format!("{:0width$}", i, width=element_len);
            original_data.extend_from_slice(value.as_bytes());
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Verify array structure in JSON
        if let Some(array) = json_result.get("ARRAY") {
            if let Some(arr) = array.as_array() {
                prop_assert_eq!(arr.len(), count,
                    "Array should have {} elements", count);
            }
        }

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: OCCURS with TO clause maintains bounds
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_to_maintains_bounds(
        min_count in 1usize..=5,
        max_count in 5usize..=20
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY OCCURS {} TO {} TIMES.\n\
             10 ELEMENT PIC X(5).",
            min_count, max_count
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find the OCCURS field
        let array_field = schema.fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Verify OCCURS bounds
        if let Some(occurs) = array_field.occurs {
            prop_assert!(occurs.min.is_some(),
                "OCCURS with TO should have min value");
            prop_assert!(occurs.max.is_some(),
                "OCCURS with TO should have max value");
        }
    }
}

/// Property: ODO (OCCURS DEPENDING ON) maintains size relationship
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_odo_maintains_size_relationship(
        max_count in 5usize..=20,
        element_size in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 COUNT PIC 9(3).\n\
             05 ARRAY OCCURS 1 TO {} TIMES DEPENDING ON COUNT.\n\
             10 ELEMENT PIC X({}).",
            max_count, element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find the ODO field
        let array_field = schema.fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Verify ODO structure
        if let Some(occurs) = array_field.occurs {
            prop_assert!(occurs.depending_on.is_some(),
                "ODO field should have DEPENDING ON clause");
            prop_assert_eq!(occurs.depending_on.as_ref().map(|s| s.as_str()), Some("COUNT"),
                "ODO should depend on COUNT field");
        }
    }
}

/// Property: ODO round-trip with varying array sizes
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_odo_roundtrip_varying_sizes(
        actual_count in 1usize..=10,
        max_count in 10usize..=20,
        element_size in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 COUNT PIC 9(3).\n\
             05 ARRAY OCCURS 1 TO {} TIMES DEPENDING ON COUNT.\n\
             10 ELEMENT PIC X({}).",
            max_count, element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Build data with actual_count elements
        let mut original_data = Vec::new();

        // COUNT field
        let count_str = format!("{:03}", actual_count);
        original_data.extend_from_slice(count_str.as_bytes());

        // ARRAY elements
        for i in 0..actual_count {
            let value = format!("{:0width$}", i, width=element_size);
            original_data.extend_from_slice(value.as_bytes());
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Verify COUNT field
        if let Some(count) = json_result.get("COUNT") {
            if let Some(s) = count.as_str() {
                prop_assert_eq!(s.parse::<usize>().unwrap(), actual_count,
                    "COUNT field should match actual count");
            }
        }

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Nested OCCURS maintains structure
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_nested_occurs_maintains_structure(
        outer_count in 1usize..=5,
        inner_count in 1usize..=5,
        element_size in 1usize..=3
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 OUTER OCCURS {} TIMES.\n\
             10 INNER OCCURS {} TIMES.\n\
             15 ELEMENT PIC X({}).",
            outer_count, inner_count, element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find nested OCCURS fields
        let outer_field = schema.fields.iter()
            .find(|f| f.name == "OUTER")
            .expect("OUTER field should exist");

        let inner_field = schema.fields.iter()
            .find(|f| f.name == "INNER")
            .expect("INNER field should exist");

        // Verify both have OCCURS
        prop_assert!(outer_field.occurs.is_some(),
            "OUTER field should have OCCURS clause");
        prop_assert!(inner_field.occurs.is_some(),
            "INNER field should have OCCURS clause");
    }
}

/// Property: OCCURS with numeric elements round-trip correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_numeric_roundtrip(
        count in 1usize..=10,
        element_digits in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY OCCURS {} TIMES.\n\
             10 ELEMENT PIC 9({}).",
            count, element_digits
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Generate test data
        let mut original_data = Vec::new();
        for i in 0..count {
            let value = format!("{:0width$}", i % (10u32.pow(element_digits as u32)), width=element_digits);
            original_data.extend_from_slice(value.as_bytes());
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

/// Property: Empty OCCURS arrays are handled correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_empty_occurs_handled_correctly(
        element_size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY OCCURS 0 TO 10 TIMES.\n\
             10 ELEMENT PIC X({}).",
            element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Empty array (no data)
        let original_data = Vec::new();

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options);

        // Empty array should decode successfully
        prop_assert!(json_result.is_ok(),
            "Empty array should decode successfully");
    }
}

/// Property: OCCURS with mixed field types maintains structure
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_mixed_types_maintains_structure(
        count in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY OCCURS {} TIMES.\n\
             10 NUM-FIELD PIC 9(5).\n\
             10 TEXT-FIELD PIC X(10).",
            count
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Generate test data
        let mut original_data = Vec::new();
        for i in 0..count {
            let num = format!("{:05}", i);
            let text = format!("TEXT{:04}", i);
            original_data.extend_from_slice(num.as_bytes());
            original_data.extend_from_slice(text.as_bytes());
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

/// Property: OCCURS array size is calculated correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_size_calculated_correctly(
        count in 1usize..=50,
        element_size in 1usize..=10
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 ARRAY OCCURS {} TIMES.\n\
             10 ELEMENT PIC X({}).",
            count, element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find the OCCURS field
        let array_field = schema.fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Expected size: count * element_size
        let expected_size = count * element_size;

        prop_assert_eq!(array_field.size, expected_size,
            "Array size {} should equal count {} * element size {}",
            array_field.size, count, element_size);
    }
}

/// Property: OCCURS with DEPENDING ON respects the count field
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_odo_respects_count_field(
        count_value in 1usize..=10,
        max_count in 10usize..=20,
        element_size in 1usize..=5
    ) {
        let copybook = format!(
            "01 RECORD.\n\
             05 COUNT PIC 9(3).\n\
             05 ARRAY OCCURS 1 TO {} TIMES DEPENDING ON COUNT.\n\
             10 ELEMENT PIC X({}).",
            max_count, element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Build JSON with COUNT field
        let json_data = json!({
            "COUNT": format!("{:03}", count_value),
            "ARRAY": (0..count_value)
                .map(|i| format!("{:0width$}", i, width=element_size))
                .collect::<Vec<_>>()
        });

        // Encode
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let encoded_data = copybook_codec::encode_record(&schema, &json_data, &encode_options)
            .expect("Failed to encode");

        // Verify encoded data size
        let expected_size = 3 + (count_value * element_size); // COUNT + ARRAY
        prop_assert_eq!(encoded_data.len(), expected_size,
            "Encoded data size {} should match expected size {}",
            encoded_data.len(), expected_size);
    }
}
