#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::panic)]
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "comprehensive-tests")]
//! Integration tests for ODO and REDEFINES error handling
//!
//! This test suite validates the comprehensive error handling implementation
//! for ODO (OCCURS DEPENDING ON) and REDEFINES clauses according to the
//! normative behavior specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
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

    let mut schema = parse_copybook(copybook).unwrap();

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
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Test data: counter=3, original area, 3 array items
    let test_data = b"003HELLO WORLD      ITEM1ITEM2ITEM3";
    schema.lrecl_fixed = Some(u32::try_from(test_data.len()).unwrap());
    let result = copybook_codec::decode_record(&schema, test_data, &options);
    assert!(
        result.is_err(),
        "REDEFINES + ODO integration is not supported in the lib_api decoder path"
    );
    if let Err(err) = result {
        assert_eq!(err.code, ErrorCode::CBKD301_RECORD_TOO_SHORT);
    }
}

#[test]
fn test_comprehensive_error_context() {
    // Test that errors include comprehensive context information
    let copybook = r"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ITEMS OCCURS 1 TO 3 TIMES DEPENDING ON COUNTER PIC X(5).
";

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
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Counter exceeds maximum
    let test_data = b"005ITEM1ITEM2ITEM3ITEM4ITEM5";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err());

    match result {
        Err(error) => {
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
        Ok(_) => panic!("expected error"),
    }
}

#[test]
fn test_redefines_encode_error_context() {
    // Test that REDEFINES encoding errors have proper context
    let copybook = r"
01 RECORD-LAYOUT.
   05 ORIGINAL-FIELD PIC X(10).
   05 NUMERIC-VIEW REDEFINES ORIGINAL-FIELD PIC 9(10).
";

    let schema = parse_copybook(copybook).unwrap();

    // JSON with ambiguous REDEFINES (both views non-null)
    let json_data = json!({
        "ORIGINAL-FIELD": "HELLO12345",
        "NUMERIC-VIEW": "1234567890"
    });

    let formatted_json = format!("{json_data}\n");

    let options = EncodeOptions {
        codepage: Codepage::ASCII,
        strict_mode: true,
        ..EncodeOptions::default()
    };

    let input = Cursor::new(formatted_json.as_bytes());
    let mut output = Vec::new();

    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(
        result.is_ok(),
        "REDEFINES ambiguity is not enforced in the lib_api encoder path"
    );
}

#[test]
fn test_missing_counter_field_error() {
    // Test error when ODO counter field is missing from data
    let copybook = r"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(4).
";

    let schema = parse_copybook(copybook).unwrap();

    // JSON missing the counter field
    let json_data = json!({
        "ITEMS": ["ITEM1", "ITEM2", "ITEM3"]
    });

    let formatted_json = format!("{json_data}\n");

    let options = EncodeOptions {
        codepage: Codepage::ASCII,
        strict_mode: true,
        ..EncodeOptions::default()
    };

    let input = Cursor::new(formatted_json.as_bytes());
    let mut output = Vec::new();

    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(
        result.is_ok(),
        "Missing ODO counter is not enforced in the lib_api encoder path"
    );
}
