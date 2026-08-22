#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![cfg(feature = "comprehensive-tests")]
#![allow(
    clippy::needless_raw_string_hashes,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::cast_precision_loss
)]
//! Comprehensive REDEFINES and ODO tests covering all edge cases and normative behavior
//!
//! This test suite validates REDEFINES and ODO handling according to the normative
//! behavior specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
};
use copybook_core::{
    ErrorCode, Occurs, ParseOptions, Schema, parse_copybook, parse_copybook_with_options,
};
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
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    }
}

fn create_test_encode_options(strict: bool) -> EncodeOptions {
    EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        strict_mode: strict,
        ..EncodeOptions::default()
    }
}

fn record_len_from_schema(schema: &Schema) -> usize {
    schema
        .lrecl_fixed
        .map(|len| len as usize)
        .unwrap_or_else(|| {
            schema
                .all_fields()
                .iter()
                .map(|field| (field.offset + field.len) as usize)
                .max()
                .unwrap_or(0)
        })
}

fn field_keys(value: &Value) -> Vec<String> {
    value
        .get("fields")
        .and_then(Value::as_object)
        .unwrap()
        .keys()
        .cloned()
        .collect()
}

fn decode_plain_and_scratch(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
) -> (Value, Value) {
    let plain = copybook_codec::decode_record(schema, data, options).unwrap();
    let mut scratch = copybook_codec::runtime::ScratchBuffers::new();
    let with_scratch =
        copybook_codec::decode_record_with_scratch(schema, data, options, &mut scratch).unwrap();
    (plain, with_scratch)
}

#[test]
fn test_redefines_shorter_equal_longer_overlays() {
    // Test REDEFINES with different lengths: shorter, equal, longer
    let copybook = r"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(10).
   05 SHORTER-REDEFINE REDEFINES ORIGINAL-FIELD PIC X(5).
   05 EQUAL-REDEFINE REDEFINES ORIGINAL-FIELD PIC X(10).
   05 LONGER-REDEFINE REDEFINES ORIGINAL-FIELD PIC X(15).
";

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
    let copybook = r"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(8).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(8).
   05 STRUCTURED-VIEW REDEFINES ORIGINAL-FIELD.
      10 PART1 PIC X(4).
      10 PART2 PIC X(4).
";

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);

    let record_len = record_len_from_schema(&schema).max(8);
    let mut test_data = vec![b'0'; record_len];
    test_data[..8].copy_from_slice(b"12345678");
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let json_record = copybook_codec::decode_record(&schema, &test_data, &options).unwrap();

    let fields = json_record
        .get("fields")
        .and_then(|value| value.as_object())
        .unwrap();

    // Scalar views appear at the top level of the fields map; the group view
    // STRUCTURED-VIEW nests its children under its own name (consistent with
    // how every group field is represented). All views overlay ORIGINAL-FIELD.
    assert_eq!(
        fields.get("ORIGINAL-FIELD").and_then(Value::as_str),
        Some("12345678")
    );
    assert_eq!(
        fields.get("NUMERIC-VIEW").and_then(Value::as_str),
        Some("12345678")
    );
    let structured = fields
        .get("STRUCTURED-VIEW")
        .and_then(Value::as_object)
        .expect("STRUCTURED-VIEW group view should be present");
    assert_eq!(
        structured.get("PART1").and_then(Value::as_str),
        Some("1234")
    );
    assert_eq!(
        structured.get("PART2").and_then(Value::as_str),
        Some("5678")
    );
}

#[test]
fn test_redefines_encode_precedence_normative() {
    // Test NORMATIVE REDEFINES encode precedence: single view > error
    let copybook = r"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(8).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(8).
";

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_encode_options(true); // Strict mode

    // Test single non-null view (should succeed)
    let single_view_json = json!({
        "ORIGINAL-FIELD": "HELLO123"
    });

    let result = copybook_codec::encode_record(&schema, &single_view_json, &options);
    assert!(result.is_ok(), "Should succeed with single non-null view");

    // Test ambiguous case (both views non-null, should error)
    let ambiguous_json = json!({
        "ORIGINAL-FIELD": "HELLO123",
        "NUMERIC-VIEW": "12345678"
    });

    let result = copybook_codec::encode_record(&schema, &ambiguous_json, &options);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH
    );

    // Test all null views (should error)
    let all_null_json = json!({
        "ORIGINAL-FIELD": null,
        "NUMERIC-VIEW": null
    });

    let result = copybook_codec::encode_record(&schema, &all_null_json, &options);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH
    );
}

#[test]
fn test_redefines_raw_preserved_record() {
    // Test raw byte preservation for REDEFINES round-trip
    let copybook = r"
01 REDEFINES-RECORD.
   05 ORIGINAL-FIELD PIC X(8).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(8).
";

    let mut schema = parse_copybook(copybook).unwrap();

    // Decode with raw capture
    let decode_options = DecodeOptions {
        emit_raw: RawMode::Record,
        ..create_test_decode_options(false)
    };

    let test_data = b"12345123"; // Use all numeric digits for zoned decimal compatibility

    // Set LRECL to match test data length
    schema.lrecl_fixed = Some(u32::try_from(test_data.len()).unwrap());

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
        ..EncodeOptions::default()
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
    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL),
        Ok(_) => panic!("expected error CBKP021_ODO_NOT_TAIL"),
    }
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
    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL),
        Ok(_) => panic!("expected error CBKP021_ODO_NOT_TAIL"),
    }

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
    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND),
        Ok(_) => panic!("expected error CBKS121_COUNTER_NOT_FOUND"),
    }
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
    schema.lrecl_fixed = Some(u32::try_from(test_data.len()).unwrap());

    let result = copybook_codec::decode_record(&schema, test_data, &lenient_options).unwrap();
    let values = result
        .get("VARIABLE-ARRAY")
        .and_then(Value::as_array)
        .expect("decoded ODO array expected");
    assert_eq!(values.len(), 5);

    // Test strict mode: error on out-of-bounds
    let strict_options = create_test_decode_options(true);
    let result = copybook_codec::decode_record(&schema, test_data, &strict_options);
    assert!(
        result.is_err(),
        "Should fail in strict mode with out-of-bounds counter"
    );
}

#[test]
fn test_odo_encode_counter_array_mismatch_rejected() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_encode_options(false);

    // The counter is encoded independently as a scalar and the array is
    // written using its own length, so a mismatch between the two must be
    // rejected rather than silently writing elements the counter doesn't
    // account for (which would make them unrecoverable on decode).
    let json_data = json!({
        "COUNTER": "02",
        "VARIABLE-ARRAY": ["ABC", "DEF", "GHI"] // 3 elements, counter says 2
    });

    let result = copybook_codec::encode_record(&schema, &json_data, &options);
    assert!(
        result.is_err(),
        "Counter/array length mismatch must be rejected"
    );
    assert_eq!(result.unwrap_err().code, ErrorCode::CBKE521_ARRAY_LEN_OOB);
}

#[test]
fn test_odo_encode_counter_array_match_succeeds() {
    let copybook = r#"
01 ODO-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_encode_options(false);

    let json_data = json!({
        "COUNTER": "03",
        "VARIABLE-ARRAY": ["ABC", "DEF", "GHI"]
    });

    let result = copybook_codec::encode_record(&schema, &json_data, &options);
    assert!(
        result.is_ok(),
        "Matching counter/array length should succeed"
    );

    let encoded_data = result.unwrap();
    assert_eq!(&encoded_data[0..2], b"03");
    assert_eq!(&encoded_data[2..5], b"ABC");
    assert_eq!(&encoded_data[5..8], b"DEF");
    assert_eq!(&encoded_data[8..11], b"GHI");
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
    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKP022_NESTED_ODO),
        Ok(_) => panic!("expected error CBKP022_NESTED_ODO"),
    }
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

    match result {
        Err(error) => {
            assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND);

            // Should have context information
            if let Some(context) = &error.context {
                assert!(context.field_path.is_some());
                assert!(context.details.is_some() || context.line_number.is_some());
            }
        }
        Ok(_) => panic!("expected error CBKS121_COUNTER_NOT_FOUND"),
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
    let test_data = b"01ABCDEF"; // Counter = 1, min = 3 (exactly 8 bytes)

    // Set LRECL for counter (2) + minimum 3 array elements (2 bytes each) = 2 + 6 = 8
    schema.lrecl_fixed = Some(8);

    let result = copybook_codec::decode_record(&schema, test_data, &options).unwrap();
    let values = result
        .get("VARIABLE-ARRAY")
        .and_then(Value::as_array)
        .expect("decoded ODO array expected");
    assert_eq!(values.len(), 3);
}

#[test]
fn cobol_redefines_declaration_order() {
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

    let record_len = record_len_from_schema(&schema).max(8);
    let mut test_data = vec![b'0'; record_len];
    test_data[..8].copy_from_slice(b"12345678");
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (json_record, scratch_record) = decode_plain_and_scratch(&schema, &test_data, &options);

    let fields = json_record
        .get("fields")
        .and_then(|value| value.as_object())
        .unwrap();

    // All views are present; the group view FIRST-REDEFINE nests its children
    // under its own name (consistent with every group field).
    assert!(fields.get("ORIGINAL").is_some());
    assert!(fields.get("THIRD-REDEFINE").is_some());
    assert!(fields.get("SECOND-REDEFINE").is_some());
    let first_redefine = fields
        .get("FIRST-REDEFINE")
        .and_then(Value::as_object)
        .expect("FIRST-REDEFINE group view should be present");
    assert!(first_redefine.get("PART-A").is_some());
    assert!(first_redefine.get("PART-B").is_some());

    // A scalar-target group emits its children at the group's declaration
    // position. Its named group view is retained after all enclosing siblings.
    // This is the intentional Issue #820 / PR #821 contract, not an incidental map order.
    let expected_order = [
        "ORIGINAL",
        "THIRD-REDEFINE",
        "PART-A",
        "PART-B",
        "SECOND-REDEFINE",
        "FIRST-REDEFINE",
    ];
    assert_eq!(
        fields.keys().map(String::as_str).collect::<Vec<_>>(),
        expected_order,
    );
    assert_eq!(
        first_redefine
            .keys()
            .map(String::as_str)
            .collect::<Vec<_>>(),
        ["PART-A", "PART-B"],
    );
    assert_eq!(
        field_keys(&scratch_record),
        expected_order
            .iter()
            .map(|key| (*key).to_owned())
            .collect::<Vec<_>>(),
        "scratch-buffer decode must preserve the REDEFINES view order",
    );
    assert_eq!(json_record, scratch_record);
}

#[test]
fn cobol_redefines_order_contract_covers_scalar_cluster_and_group_skip() {
    let copybook = r#"
01 ORDER-BOUNDARIES.
   05 ORIGINAL PIC X(4).
   05 FIRST-SCALAR REDEFINES ORIGINAL PIC X(4).
   05 SECOND-SCALAR REDEFINES ORIGINAL PIC 9(4).
   05 AFTER-SCALAR PIC X(2).
   05 ORIGINAL-GROUP.
      10 GROUP-A PIC X(2).
      10 GROUP-B PIC X(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL-GROUP.
      10 ALTERNATE PIC X(4).
   05 AFTER-GROUP PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(14);
    let test_data = vec![b'0'; record_len];
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &test_data, &options);
    let expected_order = [
        "ORIGINAL",
        "FIRST-SCALAR",
        "SECOND-SCALAR",
        "AFTER-SCALAR",
        "ORIGINAL-GROUP",
        "AFTER-GROUP",
    ];
    assert_eq!(
        field_keys(&plain),
        expected_order
            .iter()
            .map(|key| (*key).to_owned())
            .collect::<Vec<_>>(),
    );
    assert_eq!(
        field_keys(&with_scratch),
        expected_order
            .iter()
            .map(|key| (*key).to_owned())
            .collect::<Vec<_>>(),
        "scratch-buffer decode must preserve the scalar and group skip order",
    );
    assert_eq!(plain, with_scratch);
    assert!(
        plain
            .get("fields")
            .and_then(Value::as_object)
            .unwrap()
            .get("GROUP-REDEFINE")
            .is_none()
    );
}

#[test]
fn cobol_level_one_redefines_group_is_flattened_without_named_wrapper() {
    let copybook = r#"
01 ORIGINAL-RECORD.
   05 ORIGINAL PIC X(4).
01 REDEFINED-RECORD REDEFINES ORIGINAL-RECORD.
   05 ALTERNATE PIC X(4).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &[b'0'; 4], &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();

    assert!(fields.get("ALTERNATE").is_some());
    assert!(fields.get("REDEFINED-RECORD").is_none());
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_scalar_target_group_collision_preserves_both_views() {
    let copybook = r#"
01 COLLIDING-REDEFINES.
   05 EXISTING PIC X(2).
   05 ORIGINAL PIC X(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL.
      10 EXISTING PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABB", &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();

    assert_eq!(fields.get("EXISTING").and_then(Value::as_str), Some("AA"));
    assert_eq!(
        fields.get("EXISTING__dup2").and_then(Value::as_str),
        Some("BB")
    );
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_nested_collision_preserves_deterministic_views() {
    let copybook = r#"
01 NESTED-COLLISION.
   05 OUTER-GROUP.
      10 ORIGINAL PIC X(2).
      10 GROUP-REDEFINE REDEFINES ORIGINAL.
         15 INNER PIC X(2).
      10 INNER PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABB", &options);
    let outer = plain
        .get("fields")
        .and_then(Value::as_object)
        .and_then(|fields| fields.get("OUTER-GROUP"))
        .and_then(Value::as_object)
        .unwrap();

    assert_eq!(outer.get("INNER").and_then(Value::as_str), Some("AA"));
    assert_eq!(outer.get("INNER__dup2").and_then(Value::as_str), Some("BB"));
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_later_enclosing_collision_preserves_flattened_view() {
    let copybook = r#"
01 REVERSE-COLLISION.
   05 ORIGINAL PIC X(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL.
      10 LATER PIC X(2).
   05 LATER PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABB", &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();

    assert_eq!(fields.get("LATER").and_then(Value::as_str), Some("AA"));
    assert_eq!(
        fields.get("LATER__dup2").and_then(Value::as_str),
        Some("BB")
    );
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_collision_continues_existing_duplicate_name_sequence() {
    let copybook = r#"
01 DUPLICATE-COLLISION.
   05 NAME PIC X(2).
   05 ORIGINAL PIC X(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL.
      10 NAME PIC X(2).
   05 NAME PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(6);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABBCC", &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();

    assert_eq!(fields.get("NAME").and_then(Value::as_str), Some("AA"));
    assert_eq!(fields.get("NAME__dup2").and_then(Value::as_str), Some("BB"));
    assert_eq!(fields.get("NAME__dup3").and_then(Value::as_str), Some("CC"));
    assert!(fields.get("NAME__dup2__dup2").is_none());
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_collision_raw_sidecars_follow_emitted_keys() {
    let copybook = r#"
01 RAW-COLLISION.
   05 EXISTING PIC X(2).
   05 ORIGINAL PIC X(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL.
      10 EXISTING PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions {
        emit_raw: RawMode::Field,
        ..create_test_decode_options(false)
    };
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABB", &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();

    assert_eq!(fields.get("EXISTING").and_then(Value::as_str), Some("AA"));
    assert_eq!(
        fields.get("EXISTING_raw_b64").and_then(Value::as_str),
        Some("QUE=")
    );
    assert_eq!(
        fields.get("EXISTING__dup2").and_then(Value::as_str),
        Some("BB")
    );
    assert_eq!(
        fields.get("EXISTING__dup2_raw_b64").and_then(Value::as_str),
        Some("QkI=")
    );
    assert!(fields.get("EXISTING_raw_b64__dup2").is_none());
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_emitted_filler_collision_sidecars_round_trip_by_emitted_key() {
    let copybook = r#"
01 FILLER-COLLISION.
   05 FILLER PIC X(2).
   05 FILLER PIC X(2).
"#;
    let schema = parse_copybook_with_options(
        copybook,
        &ParseOptions {
            emit_filler: true,
            ..ParseOptions::default()
        },
    )
    .unwrap();
    let options = DecodeOptions {
        emit_filler: true,
        emit_raw: RawMode::Field,
        ..create_test_decode_options(false)
    };
    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABB", &options);
    assert_eq!(plain, with_scratch);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(fields.get("_filler_0").and_then(Value::as_str), Some("AA"));
    assert_eq!(
        fields.get("_filler_0__dup2").and_then(Value::as_str),
        Some("BB")
    );
    assert_eq!(
        fields.get("_filler_0_raw_b64").and_then(Value::as_str),
        Some("QUE=")
    );
    assert_eq!(
        fields
            .get("_filler_0__dup2_raw_b64")
            .and_then(Value::as_str),
        Some("QkI=")
    );

    let mut modified = plain;
    modified
        .get_mut("fields")
        .and_then(Value::as_object_mut)
        .unwrap()
        .insert("_filler_0__dup2".to_owned(), Value::String("CC".to_owned()));
    let encoded =
        copybook_codec::encode_record(&schema, &modified, &create_test_encode_options(false))
            .unwrap();
    assert_eq!(encoded, b"AACC");
}

#[test]
fn cobol_nested_emitted_filler_collision_round_trips_by_emitted_key() {
    let copybook = r#"
01 NESTED-FILLER-COLLISION.
   05 FILLER PIC X(2).
   05 ORIGINAL PIC X(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL.
      10 FILLER PIC X(2).
"#;
    let schema = parse_copybook_with_options(
        copybook,
        &ParseOptions {
            emit_filler: true,
            ..ParseOptions::default()
        },
    )
    .unwrap();
    let options = DecodeOptions {
        emit_filler: true,
        emit_raw: RawMode::Field,
        ..create_test_decode_options(false)
    };
    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABB", &options);
    assert_eq!(plain, with_scratch);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(fields.get("_filler_0").and_then(Value::as_str), Some("AA"));
    assert_eq!(
        fields.get("_filler_0__dup2").and_then(Value::as_str),
        Some("BB")
    );
    assert_eq!(
        fields
            .get("_filler_0__dup2_raw_b64")
            .and_then(Value::as_str),
        Some("QkI=")
    );

    let mut modified = plain;
    modified
        .get_mut("fields")
        .and_then(Value::as_object_mut)
        .unwrap()
        .insert("_filler_0__dup2".to_owned(), Value::String("CC".to_owned()));
    let modified_fields = modified
        .get_mut("fields")
        .and_then(Value::as_object_mut)
        .unwrap();
    // Encode the flattened view rather than the named REDEFINES wrapper.
    modified_fields.remove("GROUP-REDEFINE");
    let encoded =
        copybook_codec::encode_record(&schema, &modified, &create_test_encode_options(false))
            .unwrap();
    assert_eq!(encoded, b"AACC");
}

#[test]
fn cobol_scalar_occurs_raw_sidecars_follow_duplicate_emitted_arrays() {
    let copybook = r#"
01 OCCURS-RAW-COLLISION.
   05 NAME OCCURS 2 TIMES PIC X(2).
   05 NAME OCCURS 2 TIMES PIC X(2).
   05 AMOUNT OCCURS 2 TIMES PIC 9(2).
   05 AMOUNT OCCURS 2 TIMES PIC 9(2).
"#;
    let mut schema = parse_copybook(copybook).unwrap();
    schema.lrecl_fixed = Some(16);
    let options = DecodeOptions {
        emit_raw: RawMode::Field,
        ..create_test_decode_options(false)
    };
    let data = [
        b'A', b'A', b'B', b'B', b'C', b'C', b'D', b'D', 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        0x38,
    ];
    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &data, &options);
    assert_eq!(plain, with_scratch);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(
        fields.get("NAME_raw_b64"),
        Some(&serde_json::json!(["QUE=", "QkI="]))
    );
    assert_eq!(
        fields.get("NAME__dup2_raw_b64"),
        Some(&serde_json::json!(["Q0M=", "REQ="]))
    );
    assert_eq!(
        fields.get("AMOUNT_raw_b64"),
        Some(&serde_json::json!(["MTI=", "MzQ="]))
    );
    assert_eq!(
        fields.get("AMOUNT__dup2_raw_b64"),
        Some(&serde_json::json!(["NTY=", "Nzg="]))
    );
}

#[test]
fn cobol_zoned_collision_metadata_follows_emitted_keys() {
    let copybook = r#"
01 ZONED-COLLISION.
   05 EXISTING PIC 9(2).
   05 ORIGINAL PIC 9(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL.
      10 EXISTING PIC 9(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions {
        preserve_zoned_encoding: true,
        ..create_test_decode_options(false)
    };
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"1234", &options);
    let metadata = plain
        .get("_encoding_metadata")
        .and_then(Value::as_object)
        .unwrap();

    assert_eq!(
        metadata.get("EXISTING").and_then(Value::as_str),
        Some("ascii")
    );
    assert_eq!(
        metadata.get("EXISTING__dup2").and_then(Value::as_str),
        Some("ascii")
    );
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_envelope_zoned_metadata_is_used_for_encode() {
    let schema = parse_copybook("01 ZONED-FIELD PIC 9(2).").unwrap();
    let decode_options = DecodeOptions {
        preserve_zoned_encoding: true,
        ..create_test_decode_options(false)
    };
    let decoded = copybook_codec::decode_record(&schema, b"12", &decode_options).unwrap();

    let encoded = copybook_codec::encode_record(
        &schema,
        &decoded,
        &EncodeOptions {
            codepage: Codepage::CP037,
            ..create_test_encode_options(false)
        },
    )
    .unwrap();
    assert_eq!(encoded, b"12");
}

#[test]
fn cobol_duplicate_zoned_fields_round_trip_by_emitted_metadata_key() {
    let copybook = r#"
01 DUPLICATE-ZONED.
   05 NAME PIC 9(2).
   05 NAME PIC 9(2).
"#;
    let mut schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions {
        preserve_zoned_encoding: true,
        ..create_test_decode_options(false)
    };
    schema.lrecl_fixed = Some(4);
    let decoded = copybook_codec::decode_record(&schema, b"1234", &options).unwrap();
    let fields = decoded.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(fields.get("NAME").and_then(Value::as_str), Some("12"));
    assert_eq!(fields.get("NAME__dup2").and_then(Value::as_str), Some("34"));
    let metadata = decoded
        .get("_encoding_metadata")
        .and_then(Value::as_object)
        .unwrap();
    assert_eq!(metadata.get("NAME").and_then(Value::as_str), Some("ascii"));
    assert_eq!(
        metadata.get("NAME__dup2").and_then(Value::as_str),
        Some("ascii")
    );

    let mut mixed = decoded.clone();
    mixed
        .get_mut("_encoding_metadata")
        .and_then(Value::as_object_mut)
        .unwrap()
        .insert("NAME__dup2".to_owned(), Value::String("ebcdic".to_owned()));
    let encoded = copybook_codec::encode_record(
        &schema,
        &mixed,
        &EncodeOptions {
            codepage: Codepage::CP037,
            ..create_test_encode_options(false)
        },
    )
    .unwrap();
    assert_eq!(encoded, [b'1', b'2', 0xF3, 0xF4]);
}

#[test]
fn cobol_duplicate_zoned_occurs_metadata_follows_emitted_keys() {
    let copybook = r#"
01 DUPLICATE-ZONED-OCCURS.
   05 AMOUNT OCCURS 2 TIMES PIC 9(2).
   05 AMOUNT OCCURS 2 TIMES PIC 9(2).
"#;
    let mut schema = parse_copybook(copybook).unwrap();
    schema.lrecl_fixed = Some(8);
    let options = DecodeOptions {
        preserve_zoned_encoding: true,
        ..create_test_decode_options(false)
    };
    let (decoded, scratch_decoded) = decode_plain_and_scratch(&schema, b"12345678", &options);
    assert_eq!(decoded, scratch_decoded);
    let fields = decoded.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(fields.get("AMOUNT"), Some(&serde_json::json!(["12", "34"])));
    assert_eq!(
        fields.get("AMOUNT__dup2"),
        Some(&serde_json::json!(["56", "78"]))
    );
    let metadata = decoded
        .get("_encoding_metadata")
        .and_then(Value::as_object)
        .unwrap();
    assert_eq!(
        metadata.get("AMOUNT").and_then(Value::as_str),
        Some("ascii")
    );
    assert_eq!(
        metadata.get("AMOUNT__dup2").and_then(Value::as_str),
        Some("ascii")
    );

    let mut mixed = decoded;
    mixed
        .get_mut("_encoding_metadata")
        .and_then(Value::as_object_mut)
        .unwrap()
        .insert(
            "AMOUNT__dup2".to_owned(),
            Value::String("ebcdic".to_owned()),
        );
    let encoded = copybook_codec::encode_record(
        &schema,
        &mixed,
        &EncodeOptions {
            codepage: Codepage::CP037,
            ..create_test_encode_options(false)
        },
    )
    .unwrap();
    assert_eq!(encoded, [b'1', b'2', b'3', b'4', 0xF5, 0xF6, 0xF7, 0xF8]);
}

#[test]
fn cobol_nested_and_reverse_zoned_metadata_follow_emitted_keys() {
    for copybook in [
        r#"
01 NESTED-ZONED-COLLISION.
   05 OUTER-GROUP.
      10 ORIGINAL PIC 9(2).
      10 GROUP-REDEFINE REDEFINES ORIGINAL.
         15 INNER PIC 9(2).
      10 INNER PIC 9(2).
"#,
        r#"
01 REVERSE-ZONED-COLLISION.
   05 ORIGINAL PIC 9(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL.
      10 LATER PIC 9(2).
   05 LATER PIC 9(2).
"#,
    ] {
        let mut schema = parse_copybook(copybook).unwrap();
        let options = DecodeOptions {
            preserve_zoned_encoding: true,
            ..create_test_decode_options(false)
        };
        let record_len = record_len_from_schema(&schema).max(4);
        schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

        let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"1234", &options);
        let metadata = plain
            .get("_encoding_metadata")
            .and_then(Value::as_object)
            .unwrap();

        let collision_name = if copybook.contains("INNER") {
            "INNER"
        } else {
            "LATER"
        };
        assert_eq!(
            metadata.get(collision_name).and_then(Value::as_str),
            Some("ascii")
        );
        let duplicate_name = format!("{collision_name}__dup2");
        assert_eq!(
            metadata.get(&duplicate_name).and_then(Value::as_str),
            Some("ascii")
        );
        assert_eq!(plain, with_scratch);
    }
}

#[test]
fn cobol_duplicate_packed_and_binary_values_use_emitted_keys() {
    let copybook = r#"
01 DUPLICATE-NUMERIC.
   05 AMOUNT PIC 9(3) COMP-3.
   05 AMOUNT PIC 9(3) COMP-3.
   05 COUNT PIC 9(4) COMP.
   05 COUNT PIC 9(4) COMP.
"#;
    let schema = parse_copybook(copybook).unwrap();
    let decode_options = create_test_decode_options(false);
    let encode_options = create_test_encode_options(false);
    let input = json!({
        "AMOUNT": "123",
        "AMOUNT__dup2": "456",
        "COUNT": "42",
        "COUNT__dup2": "43"
    });

    let encoded = copybook_codec::encode_record(&schema, &input, &encode_options).unwrap();
    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &encoded, &decode_options);
    assert_eq!(plain, with_scratch);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(fields.get("AMOUNT").and_then(Value::as_str), Some("123"));
    assert_eq!(
        fields.get("AMOUNT__dup2").and_then(Value::as_str),
        Some("456")
    );
    assert_eq!(fields.get("COUNT").and_then(Value::as_str), Some("42"));
    assert_eq!(
        fields.get("COUNT__dup2").and_then(Value::as_str),
        Some("43")
    );
}

#[test]
fn cobol_nested_duplicate_packed_and_binary_values_use_emitted_keys() {
    let copybook = r#"
01 NESTED-DUPLICATE-NUMERIC.
   05 OUTER.
      10 AMOUNT PIC 9(3) COMP-3.
      10 AMOUNT PIC 9(3) COMP-3.
      10 COUNT PIC 9(4) COMP.
      10 COUNT PIC 9(4) COMP.
"#;
    let schema = parse_copybook(copybook).unwrap();
    let decode_options = create_test_decode_options(false);
    let encode_options = create_test_encode_options(false);
    let input = json!({
        "OUTER": {
            "AMOUNT": "123",
            "AMOUNT__dup2": "456",
            "COUNT": "42",
            "COUNT__dup2": "43"
        }
    });

    let encoded = copybook_codec::encode_record(&schema, &input, &encode_options).unwrap();
    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &encoded, &decode_options);
    assert_eq!(plain, with_scratch);
    let fields = plain
        .get("fields")
        .and_then(Value::as_object)
        .and_then(|fields| fields.get("OUTER"))
        .and_then(Value::as_object)
        .unwrap();
    assert_eq!(fields.get("AMOUNT").and_then(Value::as_str), Some("123"));
    assert_eq!(
        fields.get("AMOUNT__dup2").and_then(Value::as_str),
        Some("456")
    );
    assert_eq!(fields.get("COUNT").and_then(Value::as_str), Some("42"));
    assert_eq!(
        fields.get("COUNT__dup2").and_then(Value::as_str),
        Some("43")
    );
}

#[test]
fn cobol_duplicate_sign_separate_values_keep_emitted_keys() {
    let copybook = r#"
01 DUPLICATE-SIGN-SEPARATE.
   05 SIGNED PIC S9(3) SIGN LEADING SEPARATE.
   05 SIGNED PIC S9(3) SIGN LEADING SEPARATE.
"#;
    let schema = parse_copybook(copybook).unwrap();
    let decode_options = create_test_decode_options(false);
    let encode_options = create_test_encode_options(false);
    let input = json!({"SIGNED": "123", "SIGNED__dup2": "456"});

    let encoded = copybook_codec::encode_record(&schema, &input, &encode_options).unwrap();
    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &encoded, &decode_options);
    assert_eq!(plain, with_scratch);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(fields.get("SIGNED").and_then(Value::as_str), Some("123"));
    assert_eq!(
        fields.get("SIGNED__dup2").and_then(Value::as_str),
        Some("456")
    );
}

#[test]
fn cobol_duplicate_scalar_occurs_numeric_values_use_emitted_keys() {
    let copybook = r#"
01 DUPLICATE-OCCURS-NUMERIC.
   05 AMOUNT OCCURS 2 TIMES PIC 9(3) COMP-3.
   05 AMOUNT OCCURS 2 TIMES PIC 9(3) COMP-3.
   05 COUNT OCCURS 2 TIMES PIC 9(4) COMP.
   05 COUNT OCCURS 2 TIMES PIC 9(4) COMP.
"#;
    let schema = parse_copybook(copybook).unwrap();
    let decode_options = create_test_decode_options(false);
    let encode_options = create_test_encode_options(false);
    let input = json!({
        "AMOUNT": ["123", "124"],
        "AMOUNT__dup2": ["456", "457"],
        "COUNT": ["42", "43"],
        "COUNT__dup2": ["52", "53"]
    });

    let encoded = copybook_codec::encode_record(&schema, &input, &encode_options).unwrap();
    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &encoded, &decode_options);
    assert_eq!(plain, with_scratch);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    assert_eq!(
        fields
            .get("AMOUNT")
            .and_then(Value::as_array)
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        fields
            .get("AMOUNT__dup2")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .map(Value::as_str)
            .collect::<Vec<_>>(),
        vec![Some("456"), Some("457")]
    );
    assert_eq!(
        fields
            .get("COUNT__dup2")
            .and_then(Value::as_array)
            .unwrap()
            .iter()
            .map(Value::as_str)
            .collect::<Vec<_>>(),
        vec![Some("52"), Some("53")]
    );
}

#[test]
fn cobol_group_over_group_fixed_occurs_emits_named_array() {
    let copybook = r#"
01 OCCURS-REDEFINES.
   05 ORIGINAL-GROUP OCCURS 2 TIMES.
      10 ORIGINAL PIC X(2).
   05 GROUP-REDEFINE REDEFINES ORIGINAL-GROUP OCCURS 2 TIMES.
      10 ALTERNATE PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, &[b'0'; 4], &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();

    assert!(fields.get("ORIGINAL-GROUP").is_some());
    let redefining_group = fields
        .get("GROUP-REDEFINE")
        .and_then(Value::as_array)
        .expect("fixed-OCCURS group-over-group view should be a named array");
    assert_eq!(redefining_group.len(), 2);
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_scalar_target_group_fixed_occurs_emits_named_array() {
    let copybook = r#"
01 SCALAR-OCCURS-REDEFINES.
   05 ORIGINAL PIC X(4).
   05 GROUP-REDEFINE REDEFINES ORIGINAL OCCURS 2 TIMES.
      10 ALTERNATE PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(false);
    let record_len = record_len_from_schema(&schema).max(4);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABB", &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();

    assert!(fields.get("ORIGINAL").is_some());
    let redefining_group = fields
        .get("GROUP-REDEFINE")
        .and_then(Value::as_array)
        .expect("fixed-OCCURS scalar-target view should be a named array");
    assert_eq!(redefining_group.len(), 2);
    assert_eq!(
        redefining_group[0].get("ALTERNATE").and_then(Value::as_str),
        Some("AA")
    );
    assert_eq!(
        redefining_group[1].get("ALTERNATE").and_then(Value::as_str),
        Some("BB")
    );
    assert!(fields.get("ALTERNATE").is_none());
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_group_array_preserves_child_offsets_across_elements() {
    let copybook = r#"
01 MULTI-CHILD-OCCURS.
   05 PAIR OCCURS 2 TIMES.
      10 LEFT PIC X(2).
      10 RIGHT PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions {
        emit_raw: RawMode::Field,
        ..create_test_decode_options(false)
    };
    let record_len = record_len_from_schema(&schema).max(8);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABBCCDD", &options);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    let pairs = fields
        .get("PAIR")
        .and_then(Value::as_array)
        .expect("group OCCURS should decode as an array");
    assert_eq!(pairs.len(), 2);
    assert_eq!(pairs[0].get("LEFT").and_then(Value::as_str), Some("AA"));
    assert_eq!(pairs[0].get("RIGHT").and_then(Value::as_str), Some("BB"));
    assert_eq!(pairs[1].get("LEFT").and_then(Value::as_str), Some("CC"));
    assert_eq!(pairs[1].get("RIGHT").and_then(Value::as_str), Some("DD"));
    assert!(fields.get("PAIR_raw_b64").is_none());
    assert_eq!(plain, with_scratch);
}

#[test]
fn cobol_group_array_child_raw_sidecars_are_element_aligned_and_round_trip() {
    let copybook = r#"
01 GROUP-RAW-OCCURS.
   05 PAIR OCCURS 2 TIMES.
      10 ITEM PIC X(2).
      10 ITEM PIC X(2).
"#;

    let mut schema = parse_copybook(copybook).unwrap();
    let options = DecodeOptions {
        emit_raw: RawMode::Field,
        ..create_test_decode_options(false)
    };
    let record_len = record_len_from_schema(&schema).max(8);
    schema.lrecl_fixed = Some(u32::try_from(record_len).unwrap());

    let (plain, with_scratch) = decode_plain_and_scratch(&schema, b"AABBCCDD", &options);
    assert_eq!(plain, with_scratch);
    let fields = plain.get("fields").and_then(Value::as_object).unwrap();
    let pairs = fields
        .get("PAIR")
        .and_then(Value::as_array)
        .expect("group OCCURS should decode as an array");
    assert_eq!(pairs.len(), 2);
    assert_eq!(pairs[0].get("ITEM").and_then(Value::as_str), Some("AA"));
    assert_eq!(
        pairs[0].get("ITEM__dup2").and_then(Value::as_str),
        Some("BB")
    );
    assert_eq!(
        pairs[0].get("ITEM_raw_b64"),
        Some(&serde_json::json!("QUE="))
    );
    assert_eq!(
        pairs[0].get("ITEM__dup2_raw_b64"),
        Some(&serde_json::json!("QkI="))
    );
    assert_eq!(
        pairs[1].get("ITEM_raw_b64"),
        Some(&serde_json::json!("Q0M="))
    );
    assert_eq!(
        pairs[1].get("ITEM__dup2_raw_b64"),
        Some(&serde_json::json!("REQ="))
    );
    assert!(fields.get("PAIR_raw_b64").is_none());

    let mut modified = plain;
    let modified_pairs = modified
        .get_mut("fields")
        .and_then(Value::as_object_mut)
        .and_then(|fields| fields.get_mut("PAIR"))
        .and_then(Value::as_array_mut)
        .unwrap();
    modified_pairs[1]
        .as_object_mut()
        .unwrap()
        .insert("ITEM__dup2".to_owned(), Value::String("ZZ".to_owned()));
    let encoded =
        copybook_codec::encode_record(&schema, &modified, &create_test_encode_options(false))
            .unwrap();
    assert_eq!(encoded, b"AABBCCZZ");
}
