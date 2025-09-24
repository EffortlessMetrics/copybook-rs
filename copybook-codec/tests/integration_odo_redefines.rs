#![cfg(feature = "comprehensive-tests")]
//! Integration tests for ODO and REDEFINES error handling
//!
//! This test suite validates the comprehensive error handling implementation
//! for ODO (OCCURS DEPENDING ON) and REDEFINES clauses according to the
//! normative behavior specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::{ErrorCode, parse_copybook};
use serde_json::json;
use std::io::Cursor;

#[test]
fn test_odo_redefines_integration() {
    // Test ODO and REDEFINES working together
    let copybook = r"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ORIGINAL-AREA PIC X(20).
   05 REDEFINE-AREA REDEFINES ORIGINAL-AREA.
      10 PART1 PIC X(10).
      10 PART2 PIC X(10).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(4).
";

    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    };

    // Test data: counter=3, original area, 3 array items
    let test_data = b"003HELLO WORLD      ITEM1ITEM2ITEM3";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let output_str = String::from_utf8(output).unwrap();
    let json_record: serde_json::Value = serde_json::from_str(output_str.trim()).unwrap();

    // Verify all fields are present
    assert!(json_record.get("COUNTER").is_some());
    assert!(json_record.get("ORIGINAL-AREA").is_some());
    assert!(json_record.get("REDEFINE-AREA").is_some());
    assert!(json_record.get("VARIABLE-ARRAY").is_some());

    // Verify ODO array has correct length
    let array = json_record["VARIABLE-ARRAY"].as_array().unwrap();
    assert_eq!(array.len(), 3);
}

#[test]
fn test_comprehensive_error_context() {
    // Test that errors include comprehensive context information
    let copybook = r#"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON COUNTER PIC X(5).
"#;

    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true, // Strict mode to trigger errors
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    };

    // Counter exceeds maximum
    let test_data = b"005ITEM1ITEM2ITEM3ITEM4ITEM5";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err());

    let error = result.unwrap_err();

    // Verify error has context
    if let Some(context) = &error.context {
        // Should have record index, field path, and byte offset
        assert!(
            context.record_index.is_some()
                || context.field_path.is_some()
                || context.byte_offset.is_some()
        );
    }
}

#[test]
fn test_redefines_encode_error_context() {
    // Test that REDEFINES encoding errors have proper context
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ORIGINAL-FIELD PIC X(10).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(10).
"#;

    let schema = parse_copybook(copybook).unwrap();

    // JSON with ambiguous REDEFINES (both views non-null)
    let json_data = json!({
        "ORIGINAL-FIELD": "HELLO12345",
        "NUMERIC-VIEW": "1234567890"
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
        coerce_numbers: false,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(result.is_err());

    let error = result.unwrap_err();

    // Should be a type mismatch error with context
    assert_eq!(error.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);

    // Should have context information
    if let Some(context) = &error.context {
        assert!(context.field_path.is_some() || context.record_index.is_some());
    }
}

#[test]
fn test_missing_counter_field_error() {
    // Test error when ODO counter field is missing from data
    let copybook = r#"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(4).
"#;

    let schema = parse_copybook(copybook).unwrap();

    // JSON missing the counter field
    let json_data = json!({
        "ITEMS": ["ITEM1", "ITEM2", "ITEM3"]
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
        coerce_numbers: false,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(result.is_err());

    let error = result.unwrap_err();

    // Should indicate missing field
    assert!(
        error.message.contains("missing")
            || error.message.contains("COUNTER")
            || error.message.contains("required")
    );
}
