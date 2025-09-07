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

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);

    let test_data = b"12345678"; // 8 bytes of data

    // Set LRECL to match test data length
    schema.lrecl_fixed = Some(test_data.len() as u32);

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

    let mut schema = parse_copybook(copybook).unwrap();

    // Decode with raw capture
    let decode_options = DecodeOptions {
        emit_raw: RawMode::Record,
        ..create_test_decode_options(false)
    };

    let test_data = b"12345123"; // Use all numeric digits for zoned decimal compatibility

    // Set LRECL to match test data length
    schema.lrecl_fixed = Some(test_data.len() as u32);

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

    let mut schema = parse_copybook(copybook).unwrap();

    // Test lenient mode: clamp out-of-bounds counter
    let lenient_options = create_test_decode_options(false);

    // Counter = 99 (exceeds max of 5)
    let test_data = b"99ABCDEFGHIJKLMNO"; // Counter + 5 array elements

    // Set LRECL to match test data length for ODO schemas
    schema.lrecl_fixed = Some(test_data.len() as u32);

    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let summary =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &lenient_options)
            .unwrap();
    assert!(summary.has_warnings(), "Should have ODO clipped warning");

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should clamp to maximum (5 elements)
    let array = json_record
        .get("VARIABLE-ARRAY")
        .unwrap()
        .as_array()
        .unwrap();
    assert_eq!(array.len(), 5);

    // Test strict mode: error on out-of-bounds
    let strict_options = create_test_decode_options(true);
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &strict_options);
    assert!(
        result.is_err(),
        "Should fail in strict mode with out-of-bounds counter"
    );
}

#[test]
fn test_odo_encode_counter_update() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_encode_options(false);

    // Test encoding with array length different from counter
    let json_data = json!({
        "COUNTER": "02",
        "VARIABLE-ARRAY": ["ABC", "DEF", "GHI"] // 3 elements, counter says 2
    });

    let result = copybook_codec::encode_record(&schema, &json_data, &options);
    assert!(result.is_ok(), "Should succeed and update counter");

    let encoded_data = result.unwrap();

    // Counter should be updated to match array length (3)
    assert_eq!(&encoded_data[0..2], b"03"); // Counter updated to 03
    assert_eq!(&encoded_data[2..5], b"ABC"); // First element
    assert_eq!(&encoded_data[5..8], b"DEF"); // Second element
    assert_eq!(&encoded_data[8..11], b"GHI"); // Third element
}

#[test]
fn test_odo_payload_length_correctness() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNTER PIC X(4).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);

    // Test with counter = 3, should read exactly 3 elements
    let test_data = b"03ABCDEFGHIJKLMNOPQRSTUVWXYZ"; // Counter + more data than needed

    // Set LRECL for counter (2) + 3 array elements (4 bytes each) = 2 + 12 = 14
    schema.lrecl_fixed = Some(14);

    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    let array = json_record
        .get("VARIABLE-ARRAY")
        .unwrap()
        .as_array()
        .unwrap();
    assert_eq!(array.len(), 3);
    assert_eq!(array[0], "ABCD");
    assert_eq!(array[1], "EFGH");
    assert_eq!(array[2], "IJKL");

    // Remaining data should not be consumed
}

#[test]
fn test_nested_fixed_occurs_allowed() {
    // Test that nested fixed OCCURS are allowed
    let nested_occurs = r#"
01 NESTED-OCCURS-RECORD.
   05 OUTER-ARRAY OCCURS 3 TIMES.
      10 INNER-ARRAY OCCURS 2 TIMES PIC X(2).
      10 SCALAR-FIELD PIC 9(3).
"#;

    let schema = parse_copybook(nested_occurs).unwrap();
    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 1);

    let outer_array = &root.children[0];
    assert!(matches!(
        outer_array.occurs,
        Some(Occurs::Fixed { count: 3 })
    ));
    assert_eq!(outer_array.children.len(), 2);

    let inner_array = &outer_array.children[0];
    assert!(matches!(
        inner_array.occurs,
        Some(Occurs::Fixed { count: 2 })
    ));

    let scalar = &outer_array.children[1];
    assert!(scalar.occurs.is_none());
}

#[test]
fn test_odo_not_nested_under_odo() {
    // Test that ODO cannot be nested under another ODO
    let invalid_nested_odo = r#"
01 NESTED-ODO-RECORD.
   05 OUTER-COUNTER PIC 9(2).
   05 OUTER-ARRAY OCCURS 1 TO 3 TIMES DEPENDING ON OUTER-COUNTER.
      10 INNER-COUNTER PIC 9(2).
      10 INNER-ARRAY OCCURS 1 TO 2 TIMES DEPENDING ON INNER-COUNTER PIC X(1).
"#;

    let result = parse_copybook(invalid_nested_odo);
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL);
}

#[test]
fn test_comprehensive_error_context() {
    // Test that ODO/REDEFINES errors include proper context
    let invalid_copybook = r#"
01 ERROR-CONTEXT-RECORD.
   05 FIELD1 PIC X(5).
   05 COUNTER PIC 9(2).
   05 BAD-ODO OCCURS 1 TO 5 TIMES DEPENDING ON MISSING-COUNTER PIC X(3).
"#;

    let result = parse_copybook(invalid_copybook);
    assert!(result.is_err());

    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND);

    // Should have context information
    if let Some(context) = &error.context {
        assert!(context.field_path.is_some());
        assert!(context.details.is_some() || context.line_number.is_some());
    }
}

#[test]
fn test_redefines_cluster_sizing() {
    // Test that REDEFINES cluster size is max of all variants
    let copybook = r#"
01 CLUSTER-SIZING-RECORD.
   05 ORIGINAL PIC X(5).
   05 SHORTER REDEFINES ORIGINAL PIC X(3).
   05 LONGER REDEFINES ORIGINAL PIC X(10).
   05 MEDIUM REDEFINES ORIGINAL PIC X(7).
   05 NEXT-FIELD PIC X(2).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let root = &schema.fields[0];

    // Find the next field after the redefines cluster
    let next_field = root
        .children
        .iter()
        .find(|f| f.name == "NEXT-FIELD")
        .unwrap();

    // Next field should start after the longest redefine (10 bytes)
    let original_offset = root.children[0].offset;
    assert_eq!(next_field.offset, original_offset + 10);
}

#[test]
fn test_odo_minimum_counter_handling() {
    let copybook = r#"
01 ODO-MIN-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 3 TO 10 TIMES DEPENDING ON COUNTER PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false); // Lenient mode

    // Test with counter below minimum (should clamp to minimum)
    let test_data = b"01ABCDEFGHIJKLMNOPQR"; // Counter = 1, min = 3

    // Set LRECL for counter (2) + minimum 3 array elements (2 bytes each) = 2 + 6 = 8
    schema.lrecl_fixed = Some(8);

    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let summary =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    assert!(summary.has_warnings(), "Should have ODO raised warning");

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    let array = json_record
        .get("VARIABLE-ARRAY")
        .unwrap()
        .as_array()
        .unwrap();
    assert_eq!(array.len(), 3); // Should be clamped to minimum
}

#[test]
fn test_redefines_declaration_order() {
    // Test that REDEFINES are output in declaration order
    let copybook = r#"
01 ORDER-TEST-RECORD.
   05 ORIGINAL PIC X(8).
   05 THIRD-REDEFINE REDEFINES ORIGINAL PIC 9(8).
   05 FIRST-REDEFINE REDEFINES ORIGINAL.
      10 PART-A PIC X(4).
      10 PART-B PIC X(4).
   05 SECOND-REDEFINE REDEFINES ORIGINAL PIC X(8).
"#;

    let mut schema = parse_copybook(copybook).unwrap();

    let options = create_test_decode_options(false);

    let test_data = b"12345678"; // Use all numeric digits for zoned decimal compatibility

    // Set LRECL to match test data length
    schema.lrecl_fixed = Some(test_data.len() as u32);

    // Instead of using decode_file_to_jsonl which goes through string serialization,
    // use decode_record directly to avoid JSON string round-trip that might reorder keys
    let json_record = copybook_codec::decode_record(&schema, test_data, &options).unwrap();

    // Verify all views are present
    assert!(json_record.get("ORIGINAL").is_some());
    assert!(json_record.get("THIRD-REDEFINE").is_some());
    assert!(json_record.get("FIRST-REDEFINE").is_some());
    assert!(json_record.get("SECOND-REDEFINE").is_some());

    // JSON object should maintain insertion order (declaration order)
    let keys: Vec<&str> = json_record
        .as_object()
        .unwrap()
        .keys()
        .map(|s| s.as_str())
        .collect();
    let expected_order = vec![
        "ORIGINAL",
        "THIRD-REDEFINE",
        "FIRST-REDEFINE",
        "SECOND-REDEFINE",
    ];

    for (i, expected_key) in expected_order.iter().enumerate() {
        assert!(
            keys.iter().position(|&k| k == *expected_key).unwrap() == i,
            "Field {} not in expected position. Actual order: {:?}, Expected: {:?}",
            expected_key,
            keys,
            expected_order
        );
    }
}
