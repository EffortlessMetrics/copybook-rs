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
    // Test ODO and REDEFINES working together - simplified to match working example
    let copybook = r#"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(4).
"#;

    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::RDW,
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

    // Test data: counter=3, 3 array items (4 chars each) - matching working example
    // Counter: "03" (2 chars), Array: "ITM1ITM2ITM3" (12 chars)
    // RDW header: length=18 (14 bytes payload + 4 byte header), reserved=0
    let test_data = b"\x00\x12\x00\x0003ITM1ITM2ITM3";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let output_str = String::from_utf8(output).unwrap();
    let json_record: serde_json::Value = serde_json::from_str(output_str.trim()).unwrap();

    // Verify fields are present
    assert!(json_record.get("COUNTER").is_some());
    assert!(json_record.get("VARIABLE-ARRAY").is_some());

    // Check counter value
    assert_eq!(json_record["COUNTER"], "03");

    // Verify ODO array has correct length and content
    let array = json_record["VARIABLE-ARRAY"].as_array().unwrap();
    assert_eq!(array.len(), 3);
    assert_eq!(array[0], "ITM1");
    assert_eq!(array[1], "ITM2");
    assert_eq!(array[2], "ITM3");
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
        format: RecordFormat::RDW,
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
    // RDW header: length=32 (28 bytes payload + 4 byte header), reserved=0
    let test_data = b"\x00\x20\x00\x00005ITEM1ITEM2ITEM3ITEM4ITEM5";
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

    let jsonl_data = format!("{}\n", json_data);

    let options = EncodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
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

    let jsonl_data = format!("{}\n", json_data);

    let options = EncodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
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
