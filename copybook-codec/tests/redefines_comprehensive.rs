//! Comprehensive REDEFINES tests: shorter/equal/longer overlays, encode ambiguity, raw preservation
//!
//! This test suite validates REDEFINES handling according to the normative behavior
//! specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
};
use copybook_core::{FieldKind, parse_copybook};
use serde_json::{Value, json};
use std::io::Cursor;

fn create_redefines_schema() -> copybook_core::Schema {
    let copybook = r#"
01 ORIGINAL-FIELD PIC X(20).
01 SHORT-REDEFINES REDEFINES ORIGINAL-FIELD PIC 9(10).
01 EQUAL-REDEFINES REDEFINES ORIGINAL-FIELD PIC X(20).
01 LONG-REDEFINES REDEFINES ORIGINAL-FIELD PIC X(30).
01 NEXT-FIELD PIC X(5).
"#;

    parse_copybook(copybook).unwrap()
}

#[test]
fn test_redefines_shorter_overlay() {
    let schema = create_redefines_schema();

    // Find the SHORT-REDEFINES field
    let short_redefines = schema
        .fields
        .iter()
        .find(|f| f.name == "SHORT-REDEFINES")
        .unwrap();

    // Should have same offset as original
    assert_eq!(short_redefines.offset, 0);
    assert_eq!(short_redefines.len, 10); // 10 digits = 10 bytes for zoned
    assert!(short_redefines.redefines_of.is_some());
    assert_eq!(
        short_redefines.redefines_of.as_ref().unwrap(),
        "ORIGINAL-FIELD"
    );

    // Should be zoned decimal
    assert!(matches!(
        short_redefines.kind,
        FieldKind::ZonedDecimal {
            digits: 10,
            scale: 0,
            signed: false
        }
    ));
}

#[test]
fn test_redefines_equal_overlay() {
    let schema = create_redefines_schema();

    let equal_redefines = schema
        .fields
        .iter()
        .find(|f| f.name == "EQUAL-REDEFINES")
        .unwrap();

    // Should have same offset and length as original
    assert_eq!(equal_redefines.offset, 0);
    assert_eq!(equal_redefines.len, 20);
    assert!(equal_redefines.redefines_of.is_some());
    assert_eq!(
        equal_redefines.redefines_of.as_ref().unwrap(),
        "ORIGINAL-FIELD"
    );
}

#[test]
fn test_redefines_longer_overlay() {
    let schema = create_redefines_schema();

    let long_redefines = schema
        .fields
        .iter()
        .find(|f| f.name == "LONG-REDEFINES")
        .unwrap();

    // Should have same offset but longer length
    assert_eq!(long_redefines.offset, 0);
    assert_eq!(long_redefines.len, 30);
    assert!(long_redefines.redefines_of.is_some());

    // NEXT-FIELD should be positioned after the longest REDEFINES variant
    let next_field = schema
        .fields
        .iter()
        .find(|f| f.name == "NEXT-FIELD")
        .unwrap();
    assert_eq!(next_field.offset, 30); // After the 30-byte LONG-REDEFINES

    // Total record size should account for largest variant
    assert_eq!(schema.lrecl_fixed, Some(35)); // 30 + 5
}

#[test]
fn test_redefines_decode_all_views() {
    let schema = create_redefines_schema();

    // Create test data: 30 bytes (to fill the longest REDEFINES) + 5 bytes for NEXT-FIELD
    let test_data = b"HELLO WORLD 1234567890     12345";

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
    };

    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let summary =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    assert_eq!(summary.records_processed, 1);

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // All REDEFINES views should be present in declaration order
    assert!(json_record.get("ORIGINAL-FIELD").is_some());
    assert!(json_record.get("SHORT-REDEFINES").is_some());
    assert!(json_record.get("EQUAL-REDEFINES").is_some());
    assert!(json_record.get("LONG-REDEFINES").is_some());
    assert!(json_record.get("NEXT-FIELD").is_some());

    // Verify content
    assert_eq!(json_record["ORIGINAL-FIELD"], "HELLO WORLD 12345678");
    assert_eq!(json_record["EQUAL-REDEFINES"], "HELLO WORLD 12345678");
    assert_eq!(json_record["LONG-REDEFINES"], "HELLO WORLD 1234567890     ");
    assert_eq!(json_record["NEXT-FIELD"], "12345");
}

#[test]
fn test_redefines_encode_ambiguity_error() {
    let schema = create_redefines_schema();

    // JSON with multiple non-null REDEFINES views (ambiguous)
    let json_data = json!({
        "ORIGINAL-FIELD": "Hello World",
        "SHORT-REDEFINES": "1234567890",
        "EQUAL-REDEFINES": null,
        "LONG-REDEFINES": null,
        "NEXT-FIELD": "Test"
    });

    let jsonl_data = format!("{}\n", json_data.to_string());

    let options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // Should fail due to REDEFINES ambiguity
    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(result.is_err());

    let error = result.unwrap_err();
    assert!(error.message.contains("ambiguous") || error.message.contains("multiple"));
}

#[test]
fn test_redefines_encode_single_view_allowed() {
    let schema = create_redefines_schema();

    // JSON with single non-null REDEFINES view (allowed)
    let json_data = json!({
        "ORIGINAL-FIELD": "Hello World Test    ",
        "SHORT-REDEFINES": null,
        "EQUAL-REDEFINES": null,
        "LONG-REDEFINES": null,
        "NEXT-FIELD": "12345"
    });

    let jsonl_data = format!("{}\n", json_data.to_string());

    let options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        use_raw: false,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // Should succeed with single non-null view
    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 1);

    // Verify output length matches expected record size
    assert_eq!(output.len(), 35); // 30 (longest REDEFINES) + 5 (NEXT-FIELD)
}

#[test]
fn test_redefines_raw_data_precedence() {
    let schema = create_redefines_schema();

    // First, decode with raw capture to get baseline
    let test_data = b"HELLO WORLD 1234567890     12345";

    let decode_options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Record, // Capture raw data
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    let input = Cursor::new(test_data);
    let mut decode_output = Vec::new();

    let decode_summary =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut decode_output, &decode_options)
            .unwrap();
    assert_eq!(decode_summary.records_processed, 1);

    let decoded_str = String::from_utf8(decode_output).unwrap();
    let mut decoded_json: Value = serde_json::from_str(decoded_str.trim()).unwrap();

    // Verify raw data is present
    assert!(decoded_json.get("__raw_b64").is_some());

    // Modify multiple REDEFINES views (would normally be ambiguous)
    decoded_json["ORIGINAL-FIELD"] = json!("Modified Original   ");
    decoded_json["SHORT-REDEFINES"] = json!("9876543210");

    let jsonl_data = format!("{}\n", decoded_json.to_string());

    let encode_options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        use_raw: true, // Use raw data precedence
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut encode_output = Vec::new();

    // Should succeed due to raw data precedence (NORMATIVE step 1)
    let result =
        copybook_codec::encode_jsonl_to_file(&schema, input, &mut encode_output, &encode_options);
    assert!(result.is_ok());

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 1);

    // Output should match original test data (raw precedence)
    assert_eq!(encode_output, test_data);
}

#[test]
fn test_redefines_round_trip_preservation() {
    let schema = create_redefines_schema();

    let original_data = b"ORIGINAL DATA 123456789012345";

    // Decode with raw capture
    let decode_options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Record,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    let input = Cursor::new(original_data);
    let mut decode_output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut decode_output, &decode_options)
        .unwrap();

    // Encode back with raw usage
    let encode_options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        use_raw: true,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
    };

    let input = Cursor::new(&decode_output);
    let mut encode_output = Vec::new();

    copybook_codec::encode_jsonl_to_file(&schema, input, &mut encode_output, &encode_options)
        .unwrap();

    // Should be byte-identical round-trip
    assert_eq!(encode_output, original_data);
}

#[test]
fn test_redefines_cluster_size_calculation() {
    // Test that REDEFINES cluster size is max of all variants
    let copybook = r#"
01 BASE-FIELD PIC X(10).
01 SMALL-REDEF REDEFINES BASE-FIELD PIC 9(5).
01 LARGE-REDEF REDEFINES BASE-FIELD PIC X(25).
01 MEDIUM-REDEF REDEFINES BASE-FIELD PIC 9(15).
01 AFTER-CLUSTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();

    // All REDEFINES should have offset 0
    for field in &schema.fields[0..4] {
        assert_eq!(field.offset, 0);
    }

    // AFTER-CLUSTER should be positioned after largest variant (25 bytes)
    let after_field = &schema.fields[4];
    assert_eq!(after_field.offset, 25);
    assert_eq!(after_field.name, "AFTER-CLUSTER");

    // Total record size should be 25 + 3 = 28
    assert_eq!(schema.lrecl_fixed, Some(28));
}

#[test]
fn test_nested_redefines_groups() {
    let copybook = r#"
01 MAIN-RECORD.
   05 DATA-AREA PIC X(20).
   05 NUMERIC-VIEW REDEFINES DATA-AREA.
      10 PART1 PIC 9(10).
      10 PART2 PIC 9(10).
   05 BINARY-VIEW REDEFINES DATA-AREA PIC 9(9) COMP.
"#;

    let schema = parse_copybook(copybook).unwrap();

    let main_record = &schema.fields[0];
    assert_eq!(main_record.name, "MAIN-RECORD");
    assert_eq!(main_record.children.len(), 3);

    // DATA-AREA
    let data_area = &main_record.children[0];
    assert_eq!(data_area.name, "DATA-AREA");
    assert_eq!(data_area.offset, 0);
    assert_eq!(data_area.len, 20);

    // NUMERIC-VIEW (group REDEFINES)
    let numeric_view = &main_record.children[1];
    assert_eq!(numeric_view.name, "NUMERIC-VIEW");
    assert_eq!(numeric_view.offset, 0); // Same offset as DATA-AREA
    assert!(numeric_view.redefines_of.is_some());
    assert_eq!(numeric_view.children.len(), 2);

    // BINARY-VIEW
    let binary_view = &main_record.children[2];
    assert_eq!(binary_view.name, "BINARY-VIEW");
    assert_eq!(binary_view.offset, 0); // Same offset as DATA-AREA
    assert!(binary_view.redefines_of.is_some());
}

#[test]
fn test_redefines_with_occurs() {
    let copybook = r#"
01 ARRAY-AREA PIC X(50).
01 STRUCTURED-VIEW REDEFINES ARRAY-AREA.
   05 ITEMS PIC X(10) OCCURS 5 TIMES.
"#;

    let schema = parse_copybook(copybook).unwrap();

    assert_eq!(schema.fields.len(), 2);

    let array_area = &schema.fields[0];
    assert_eq!(array_area.name, "ARRAY-AREA");
    assert_eq!(array_area.len, 50);

    let structured_view = &schema.fields[1];
    assert_eq!(structured_view.name, "STRUCTURED-VIEW");
    assert_eq!(structured_view.offset, 0);
    assert!(structured_view.redefines_of.is_some());
    assert_eq!(structured_view.children.len(), 1);

    let items = &structured_view.children[0];
    assert_eq!(items.name, "ITEMS");
    assert!(items.occurs.is_some());

    if let Some(copybook_core::Occurs::Fixed { count }) = &items.occurs {
        assert_eq!(*count, 5);
    } else {
        panic!("Expected fixed OCCURS");
    }
}
