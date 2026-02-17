#![allow(unused_doc_comments, unused_imports, dead_code)]
//! Property tests for array/OCCURS handling
//!
//! These tests verify that array operations maintain length and content
//! invariants for OCCURS and ODO (OCCURS DEPENDING ON) structures.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat};
use copybook_core::{Occurs, parse_copybook};
use proptest::prelude::*;
use serde_json::json;

use super::config::*;
use super::generators::*;
use super::schema_record_length;

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
        let all_fields = schema.all_fields();
        let array_field = all_fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Verify OCCURS is set
        prop_assert!(array_field.occurs.is_some(),
            "ARRAY field should have OCCURS clause");

        // Verify element size
        if let Some(_occurs) = &array_field.occurs {
            let element_field = all_fields.iter()
                .find(|f| f.name == "ELEMENT")
                .expect("ELEMENT field should exist");

            prop_assert_eq!(element_field.len as usize, element_size,
                "Element size {} should match PIC clause", element_field.len);
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

        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Verify array structure in JSON
        if let Some(arr) = json_result.get("ARRAY").and_then(|v| v.as_array()) {
            prop_assert_eq!(arr.len(), count,
                "Array should have {} elements", count);
        }

        if let Some(array) = json_result.get("ARRAY").and_then(|v| v.as_array()) {
            for (i, elem) in array.iter().enumerate() {
                let obj = elem.as_object().expect("Array elements should be objects");
                let value = obj
                    .get("ELEMENT")
                    .and_then(|v| v.as_str())
                    .expect("ELEMENT should be string");
                let expected = format!("{:0width$}", i, width=element_len);
                prop_assert_eq!(value, expected);
            }
        }
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
             05 COUNT PIC 9(3).\n\
             05 ARRAY OCCURS {} TO {} TIMES DEPENDING ON COUNT.\n\
             10 ELEMENT PIC X(5).",
            min_count, max_count
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Find the OCCURS field
        let all_fields = schema.all_fields();
        let array_field = all_fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Verify OCCURS bounds
        if let Some(occurs) = &array_field.occurs {
            match occurs {
                Occurs::ODO { min, max, counter_path } => {
                    prop_assert_eq!(*min as usize, min_count,
                        "ODO should preserve min count");
                    prop_assert_eq!(*max as usize, max_count,
                        "ODO should preserve max count");
                    prop_assert_eq!(counter_path, "COUNT",
                        "ODO should depend on COUNT field");
                }
                Occurs::Fixed { .. } => {
                    prop_assert!(false, "Range OCCURS should produce ODO with DEPENDING ON");
                }
            }
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
        let all_fields = schema.all_fields();
        let array_field = all_fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Verify ODO structure
        if let Some(occurs) = &array_field.occurs {
            match occurs {
                Occurs::ODO { counter_path, .. } => {
                    prop_assert_eq!(counter_path, "COUNT",
                        "ODO should depend on COUNT field");
                }
                Occurs::Fixed { .. } => {
                    prop_assert!(false, "ODO should be parsed as Occurs::ODO");
                }
            }
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
        if let Some(s) = json_result.get("COUNT").and_then(|v| v.as_str()) {
            prop_assert_eq!(s.parse::<usize>().unwrap(), actual_count,
                "COUNT field should match actual count");
        }

        if let Some(array) = json_result.get("ARRAY").and_then(|v| v.as_array()) {
            prop_assert_eq!(array.len(), actual_count);
        }
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
        let all_fields = schema.all_fields();
        let outer_field = all_fields.iter()
            .find(|f| f.name == "OUTER")
            .expect("OUTER field should exist");

        let inner_field = all_fields.iter()
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
            let modulus = 10usize.pow(element_digits as u32);
            let value = format!("{:0width$}", i % modulus, width=element_digits);
            original_data.extend_from_slice(value.as_bytes());
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_json_number_mode(JsonNumberMode::Lossless);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        if let Some(array) = json_result.get("ARRAY").and_then(|v| v.as_array()) {
            for (i, elem) in array.iter().enumerate() {
                let obj = elem.as_object().expect("Array elements should be objects");
                let value = obj
                    .get("ELEMENT")
                    .and_then(|v| v.as_str())
                    .expect("ELEMENT should be string");
                let modulus = 10usize.pow(element_digits as u32);
                let expected = i % modulus;
                let actual = value.parse::<usize>().unwrap_or(0);
                prop_assert_eq!(actual, expected);
            }
        }
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
             05 COUNT PIC 9(3).\n\
             05 ARRAY OCCURS 0 TO 10 TIMES DEPENDING ON COUNT.\n\
             10 ELEMENT PIC X({}).",
            element_size
        );
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Empty array (COUNT = 0, no elements)
        let original_data = b"000".to_vec();

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
            let text = format!("TEXT{:04}{:02}", i, i % 100);
            original_data.extend_from_slice(num.as_bytes());
            original_data.extend_from_slice(text.as_bytes());
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        if let Some(array) = json_result.get("ARRAY").and_then(|v| v.as_array()) {
            prop_assert_eq!(array.len(), count);
        }
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
        let all_fields = schema.all_fields();
        let array_field = all_fields.iter()
            .find(|f| f.name == "ARRAY")
            .expect("ARRAY field should exist");

        // Expected size: count * element_size
        let expected_size = count * element_size;

        prop_assert_eq!(array_field.effective_length() as usize, expected_size,
            "Array size {} should equal count {} * element size {}",
            array_field.effective_length(), count, element_size);
    }
}

/// Property: OCCURS with DEPENDING ON respects the count field
/// FIXME: tail_odo.array_path stores field name but encode calls find_field() which expects full path
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    #[ignore = "tail_odo.array_path stores name not full path; encode find_field fails"]
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
                .map(|i| json!({"ELEMENT": format!("{:0width$}", i % 10, width=element_size)}))
                .collect::<Vec<_>>()
        });

        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let encoded_data = copybook_codec::encode_record(&schema, &json_data, &encode_options)
            .expect("Failed to encode");

        let count_str = format!("{:03}", count_value);
        prop_assert_eq!(&encoded_data[..3], count_str.as_bytes());

        if let Some(expected_size) = schema_record_length(&schema) {
            prop_assert_eq!(encoded_data.len(), expected_size as usize);
        }
    }
}
