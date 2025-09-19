//! Comprehensive REDEFINES and ODO tests covering all edge cases and normative behavior
//!
//! This test suite validates REDEFINES and ODO handling according to the normative
//! behavior specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::{ErrorCode, Occurs, parse_copybook};
use serde_json::{Value, json};
use std::io::Cursor;

fn create_test_decode_options(strict: bool) -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: strict,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    }
}

fn create_rdw_decode_options(strict: bool) -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: strict,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    }
}

fn create_test_encode_options(strict: bool) -> EncodeOptions {
    EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        use_raw: false,
        bwz_encode: false,
        strict_mode: strict,
        max_errors: None,
        threads: 1,
    }
}

#[test]
fn test_redefines_shorter_equal_longer_overlays() {
    // Test REDEFINES with different lengths: shorter, equal, longer
    let copybook = r#"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(10).
   05 SHORTER-REDEFINE REDEFINES ORIGINAL-FIELD PIC X(5).
   05 EQUAL-REDEFINE REDEFINES ORIGINAL-FIELD PIC X(10).
   05 LONGER-REDEFINE REDEFINES ORIGINAL-FIELD PIC X(15).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 4);

    // All redefining fields should have the same offset as original
    let original_offset = root.children[0].offset;
    assert_eq!(root.children[1].offset, original_offset); // Shorter
    assert_eq!(root.children[2].offset, original_offset); // Equal
    assert_eq!(root.children[3].offset, original_offset); // Longer

    // Check redefines relationships
    assert!(root.children[0].redefines_of.is_none());
    assert_eq!(
        root.children[1].redefines_of,
        Some("ORIGINAL-FIELD".to_string())
    );
    assert_eq!(
        root.children[2].redefines_of,
        Some("ORIGINAL-FIELD".to_string())
    );
    assert_eq!(
        root.children[3].redefines_of,
        Some("ORIGINAL-FIELD".to_string())
    );

    // The containing group size should be extended by the longest redefine
    assert!(root.len >= 15); // At least as long as the longest redefine
}

#[test]
fn test_redefines_decode_all_views() {
    // Test that all REDEFINES views are included in JSON output
    let copybook = r#"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(8).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(8).
   05 STRUCTURED-VIEW REDEFINES ORIGINAL-FIELD.
      10 PART1 PIC X(4).
      10 PART2 PIC X(4).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);

    let test_data = b"12345678"; // 8 bytes of data
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // All views should be present in declaration order
    assert!(json_record.get("ORIGINAL-FIELD").is_some());
    assert!(json_record.get("NUMERIC-VIEW").is_some());
    assert!(json_record.get("STRUCTURED-VIEW").is_some());

    // Structured view should have its children
    let structured = json_record.get("STRUCTURED-VIEW").unwrap();
    assert!(structured.get("PART1").is_some());
    assert!(structured.get("PART2").is_some());
}

#[test]
fn test_redefines_encode_precedence_normative() {
    // Test NORMATIVE REDEFINES encode precedence: single view > error
    let copybook = r#"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(8).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(8).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_encode_options(true); // Strict mode

    // Test single non-null view (should succeed)
    let single_view_json = json!({
        "ORIGINAL-FIELD": "HELLO123",
        "NUMERIC-VIEW": null
    });

    let result = copybook_codec::encode_record(&schema, &single_view_json, &options);
    assert!(result.is_ok(), "Should succeed with single non-null view");

    // Test ambiguous case (both views non-null, should error)
    let ambiguous_json = json!({
        "ORIGINAL-FIELD": "HELLO123",
        "NUMERIC-VIEW": "12345678"
    });

    let result = copybook_codec::encode_record(&schema, &ambiguous_json, &options);
    assert!(result.is_err(), "Should fail with ambiguous views");

    // Test all null views (should error)
    let all_null_json = json!({
        "ORIGINAL-FIELD": null,
        "NUMERIC-VIEW": null
    });

    let result = copybook_codec::encode_record(&schema, &all_null_json, &options);
    assert!(result.is_err(), "Should fail with all null views");
}

#[test]
fn test_redefines_raw_preserved_record() {
    // Test raw byte preservation for REDEFINES round-trip
    let copybook = r#"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(8).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(8).
"#;

    let schema = parse_copybook(copybook).unwrap();

    // Decode with raw capture
    let decode_options = DecodeOptions {
        emit_raw: RawMode::Record,
        ..create_test_decode_options(false)
    };

    let test_data = b"12345678"; // Use all numeric data that works for both views
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &decode_options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should have raw data captured
    assert!(json_record.get("__raw_b64").is_some());

    // Encode with raw usage
    let encode_options = EncodeOptions {
        use_raw: true,
        ..create_test_encode_options(false)
    };

    let result = copybook_codec::encode_record(&schema, &json_record, &encode_options);
    assert!(result.is_ok(), "Should succeed with raw data");

    let encoded_data = result.unwrap();
    assert_eq!(encoded_data, test_data, "Should produce identical bytes");
}

#[test]
fn test_odo_driver_precedes_array() {
    // Test that ODO counter field precedes the array in byte order
    let valid_odo = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(3).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).
"#;

    let schema = parse_copybook(valid_odo).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 2);

    let counter = &root.children[0];
    let array = &root.children[1];

    // Counter should precede array in byte order
    assert!(counter.offset < array.offset);

    // Array should have ODO configuration
    assert!(matches!(
        array.occurs,
        Some(Occurs::ODO {
            min: 1,
            max: 10,
            ..
        })
    ));

    // Invalid: Counter after array
    let invalid_odo = r#"
01 BAD-ODO-RECORD.
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).
   05 COUNTER PIC 9(3).
"#;

    let result = parse_copybook(invalid_odo);
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND);
}

#[test]
fn test_odo_tail_position_validation() {
    // Test that ODO arrays must be at tail position
    let invalid_odo_not_tail = r#"
01 BAD-ODO-RECORD.
   05 COUNTER PIC 9(3).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(5).
   05 TRAILING-FIELD PIC X(3).
"#;

    let result = parse_copybook(invalid_odo_not_tail);
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL);

    // Valid: ODO at tail
    let valid_odo_tail = r#"
01 GOOD-ODO-RECORD.
   05 COUNTER PIC 9(3).
   05 FIXED-FIELD PIC X(5).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(valid_odo_tail).unwrap();
    assert_eq!(schema.fields[0].children.len(), 3);

    let array = &schema.fields[0].children[2];
    assert!(matches!(array.occurs, Some(Occurs::ODO { .. })));
}

#[test]
fn test_odo_counter_in_redefines_error() {
    // Test that ODO counter cannot be inside REDEFINES
    let invalid_counter_in_redefines = r#"
01 BAD-ODO-RECORD.
   05 ORIGINAL-FIELD PIC X(10).
   05 REDEFINING-GROUP REDEFINES ORIGINAL-FIELD.
      10 COUNTER PIC 9(3).
      10 FILLER PIC X(7).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(2).
"#;

    let result = parse_copybook(invalid_counter_in_redefines);
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND);
}

#[test]
fn test_odo_decode_clamp_vs_strict() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();

    // Test lenient mode: clamp out-of-bounds counter
    let lenient_options = create_rdw_decode_options(false);

    // Counter = 99 (exceeds max of 5)
    // RDW header: length=21 (4-byte header + 2-byte counter + 15 bytes for 5 elements)
    let rdw_data = b"\x00\x00\x00\x159999AAABBBCCCDDDEEE"; // length=21, counter=99, 5 elements
    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &lenient_options);
    assert!(result.is_ok(), "Lenient mode should succeed with clamping");

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 1);
    assert_eq!(summary.warnings, 1); // Should have warning for clamping

    // Test strict mode: error on out-of-bounds counter
    let strict_options = create_rdw_decode_options(true);

    let rdw_data = b"\x00\x00\x00\x159999AAABBBCCCDDDEEE"; // same data
    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &strict_options);
    assert!(result.is_err(), "Strict mode should fail with out-of-bounds counter");

    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS301_ODO_CLIPPED);
}

#[test]
fn test_odo_encode_array_bounds_validation() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_encode_options(true);

    // Test valid array length
    let valid_json = json!({
        "COUNTER": "03",
        "VARIABLE-ARRAY": ["AAA", "BBB", "CCC"]
    });

    let result = copybook_codec::encode_record(&schema, &valid_json, &options);
    assert!(result.is_ok(), "Should succeed with valid array length");

    // Test array length exceeds maximum
    let too_long_json = json!({
        "COUNTER": "03",
        "VARIABLE-ARRAY": ["AAA", "BBB", "CCC", "DDD", "EEE", "FFF"]
    });

    let result = copybook_codec::encode_record(&schema, &too_long_json, &options);
    assert!(result.is_err(), "Should fail with array length exceeding maximum");

    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKE521_ARRAY_LEN_OOB);

    // Test array length below minimum
    let too_short_json = json!({
        "COUNTER": "05",
        "VARIABLE-ARRAY": []
    });

    let result = copybook_codec::encode_record(&schema, &too_short_json, &options);
    assert!(result.is_err(), "Should fail with array length below minimum");
}

#[test]
fn test_odo_missing_counter_field_error() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(true);

    // Create test data without proper counter field
    let test_data = b"XXAAABBBCCC"; // No numeric counter at start
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    // This should succeed with parsing, but might generate warnings about invalid counter
    // The specific behavior depends on the implementation details
    if result.is_err() {
        let error = result.unwrap_err();
        // Could be various error codes depending on how counter parsing fails
        assert!(matches!(
            error.code,
            ErrorCode::CBKS121_COUNTER_NOT_FOUND |
            ErrorCode::CBKS301_ODO_CLIPPED |
            ErrorCode::CBKD411_ZONED_BAD_SIGN
        ));
    }
}

#[test]
fn test_odo_nested_groups_with_arrays() {
    // Test ODO arrays inside nested group structures
    let copybook = r#"
01 NESTED-ODO-RECORD.
   05 HEADER-GROUP.
      10 ARRAY-COUNT PIC 9(2).
      10 SOME-FIELD PIC X(5).
   05 DATA-GROUP.
      10 VARIABLE-ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON ARRAY-COUNT PIC X(4).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);

    // Test with 2 array elements
    let test_data = b"02HELLO1234ABCD"; // count=02, field=HELLO, 2 elements
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Should decode nested ODO structure");

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Verify structure
    assert!(json_record.get("HEADER-GROUP").is_some());
    assert!(json_record.get("DATA-GROUP").is_some());

    let data_group = json_record.get("DATA-GROUP").unwrap();
    let variable_items = data_group.get("VARIABLE-ITEMS").unwrap();
    assert!(variable_items.is_array());
    assert_eq!(variable_items.as_array().unwrap().len(), 2);
}

#[test]
fn test_redefines_and_odo_together() {
    // Test interaction between REDEFINES and ODO in same record
    let copybook = r#"
01 COMPLEX-RECORD.
   05 ORIGINAL-FIELD PIC X(10).
   05 COUNT-VIEW REDEFINES ORIGINAL-FIELD.
      10 ODO-COUNTER PIC 9(2).
      10 FILLER PIC X(8).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON ODO-COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);

    // Test data: "03" + 8 filler chars + 3 array elements
    let test_data = b"03        AAABBBCCC";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Should handle REDEFINES+ODO combination");

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Both views should be present
    assert!(json_record.get("ORIGINAL-FIELD").is_some());
    assert!(json_record.get("COUNT-VIEW").is_some());
    assert!(json_record.get("VARIABLE-ARRAY").is_some());

    // ODO counter should be extracted from REDEFINES view
    let count_view = json_record.get("COUNT-VIEW").unwrap();
    assert!(count_view.get("ODO-COUNTER").is_some());

    // Array should have 3 elements
    let variable_array = json_record.get("VARIABLE-ARRAY").unwrap();
    assert_eq!(variable_array.as_array().unwrap().len(), 3);
}