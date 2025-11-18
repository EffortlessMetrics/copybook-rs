#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::assertions_on_constants
)]
#![cfg(feature = "comprehensive-tests")]
#![allow(
    clippy::needless_raw_string_hashes,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::cast_precision_loss
)]
//! Comprehensive ODO tests: validation, strict vs lenient modes, payload length
//!
//! This test suite validates ODO (OCCURS DEPENDING ON) handling according to
//! the normative behavior specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    ZonedEncodingFormat,
};
use copybook_core::{ErrorCode, Occurs, parse_copybook};
use serde_json::{Value, json};
use std::io::Cursor;

#[test]
fn test_odo_driver_in_redefines_rejection() {
    // Test that ODO counter inside REDEFINES is rejected
    let copybook = r"
01 MAIN-AREA PIC X(20).
01 REDEF-AREA REDEFINES MAIN-AREA.
   05 COUNTER PIC 9(3).
   05 ITEMS PIC X(5) OCCURS 1 TO 3 TIMES DEPENDING ON COUNTER.
";

    let result = parse_copybook(copybook);
    assert!(result.is_err());

    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND),
        Ok(_) => panic!("expected error CBKS121_COUNTER_NOT_FOUND"),
    }
}

#[test]
fn test_odo_driver_after_array_rejection() {
    // Test that ODO counter after the array is rejected
    let copybook = r"
01 RECORD-LAYOUT.
   05 ITEMS PIC X(10) OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER.
   05 COUNTER PIC 9(3).
";

    let result = parse_copybook(copybook);
    assert!(result.is_err());

    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKS121_COUNTER_NOT_FOUND),
        Ok(_) => panic!("expected error CBKS121_COUNTER_NOT_FOUND"),
    }
}

#[test]
fn test_odo_not_at_tail_rejection() {
    // Test that ODO not at tail position is rejected
    let copybook = r#"
01 RECORD-LAYOUT.
   05 COUNTER PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER.
   05 AFTER-FIELD PIC X(5).
"#;

    let result = parse_copybook(copybook);
    assert!(result.is_err());

    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL),
        Ok(_) => panic!("expected error CBKP021_ODO_NOT_TAIL"),
    }
}

#[test]
fn test_valid_odo_configuration() {
    // Test valid ODO: counter precedes array, array at tail
    let copybook = r#"
01 RECORD-LAYOUT.
   05 HEADER-DATA PIC X(10).
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(15) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 3);

    // Verify ODO configuration
    let items_field = &root.children[2];
    assert_eq!(items_field.name, "ITEMS");

    if let Some(Occurs::ODO {
        min,
        max,
        counter_path,
    }) = &items_field.occurs
    {
        assert_eq!(*min, 0);
        assert_eq!(*max, 5);
        assert_eq!(counter_path, "ITEM-COUNT");
    } else {
        assert!(false, "Expected ODO occurs, got {:?}", items_field.occurs);
    }

    // Should have tail ODO info
    assert!(schema.tail_odo.is_some());
    let tail_odo = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail_odo.counter_path, "ITEM-COUNT");
    assert_eq!(tail_odo.max_count, 5);
    assert_eq!(tail_odo.array_path, "ITEMS");

    // Should not have fixed LRECL due to ODO
    assert!(schema.lrecl_fixed.is_none());
}

#[test]
fn test_odo_strict_mode_clamp_fatal() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 3 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true, // NORMATIVE: strict mode
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Counter value exceeds maximum (005 > 3)
    let test_data = b"005ITEM1     ITEM2     ITEM3     ITEM4     ITEM5     ";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    // Should fail in strict mode
    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err());

    match result {
        Err(error) => assert!(error.message.contains("ODO") || error.message.contains("clipped")),
        Ok(_) => panic!("expected error"),
    }
}

#[test]
fn test_odo_lenient_mode_clamp_with_warning() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 3 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false, // NORMATIVE: lenient mode
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Counter value exceeds maximum (005 > 3)
    let test_data = b"005ITEM1     ITEM2     ITEM3     ";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    // Should succeed in lenient mode with warnings
    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let summary = result.unwrap();
    assert!(summary.has_warnings()); // Should have ODO clipping warning

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Counter should be clamped to maximum
    assert_eq!(json_record["ITEM-COUNT"], "005"); // Original value preserved

    // Array should only have 3 items (clamped to max)
    let items = json_record["ITEMS"].as_array().unwrap();
    assert_eq!(items.len(), 3); // Clamped to max of 3
}

#[test]
fn test_odo_lenient_mode_raise_to_minimum() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 2 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false, // Lenient mode
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Counter value below minimum (001 < 2)
    let test_data = b"001ITEM1     ITEM2     ";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let summary = result.unwrap();
    assert!(summary.has_warnings()); // Should have ODO raise warning

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Array should have minimum items (raised to min of 2)
    let items = json_record["ITEMS"].as_array().unwrap();
    assert_eq!(items.len(), 2); // Raised to min of 2
}

#[test]
fn test_odo_payload_length_correctness() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

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
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Test different array lengths
    let test_cases = vec![
        (b"000".as_slice(), 0),                               // 0 items
        (b"001ITEM1     ".as_slice(), 1),                     // 1 item
        (b"003ITEM1     ITEM2     ITEM3     ".as_slice(), 3), // 3 items
    ];

    for (test_data, expected_count) in test_cases {
        let input = Cursor::new(test_data);
        let mut output = Vec::new();

        let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
        assert!(result.is_ok(), "Failed for {expected_count} items");

        let output_str = String::from_utf8(output).unwrap();
        let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

        let items = json_record["ITEMS"].as_array().unwrap();
        assert_eq!(
            items.len(),
            expected_count,
            "Wrong item count for {expected_count} items",
        );
    }
}

#[test]
fn test_odo_encode_counter_update() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();

    // JSON with array length different from counter
    let json_data = json!({
        "ITEM-COUNT": "002", // Counter says 2
        "ITEMS": ["ITEM1     ", "ITEM2     ", "ITEM3     "] // But array has 3 items
    });

    let jsonl_data = format!("{json_data}\n");

    let options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        zoned_encoding_override: None,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // Should succeed and update counter to match array length
    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    // Decode back to verify counter was updated
    let decode_options = DecodeOptions {
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
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    let input = Cursor::new(&output);
    let mut decode_output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut decode_output, &decode_options)
        .unwrap();
    let output_str = String::from_utf8(decode_output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Counter should be updated to match array length
    assert_eq!(json_record["ITEM-COUNT"], "003"); // Updated to 3

    let items = json_record["ITEMS"].as_array().unwrap();
    assert_eq!(items.len(), 3);
}

#[test]
fn test_odo_array_length_out_of_bounds_encode() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 3 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();

    // JSON with array exceeding maximum length
    let json_data = json!({
        "ITEM-COUNT": "005",
        "ITEMS": ["ITEM1     ", "ITEM2     ", "ITEM3     ", "ITEM4     ", "ITEM5     "] // 5 items > max 3
    });

    let jsonl_data = format!("{json_data}\n");

    let options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true, // Strict mode
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        zoned_encoding_override: None,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // Should fail due to array length exceeding maximum
    let result = copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options);
    assert!(result.is_err());

    match result {
        Err(error) => assert!(
            error.message.contains("array")
                || error.message.contains("length")
                || error.message.contains("bounds")
                || error.message.contains("record errors")
                || error.message.contains("boundary")
        ),
        Ok(_) => panic!("expected error"),
    }
}

#[test]
fn test_nested_odo_rejection() {
    // Test that ODO nested under another ODO is rejected
    let copybook = r#"
01 RECORD-LAYOUT.
   05 OUTER-COUNT PIC 9(3).
   05 OUTER-ARRAY OCCURS 1 TO 3 TIMES DEPENDING ON OUTER-COUNT.
      10 INNER-COUNT PIC 9(3).
      10 INNER-ARRAY PIC X(5) OCCURS 1 TO 2 TIMES DEPENDING ON INNER-COUNT.
"#;

    let result = parse_copybook(copybook);
    assert!(result.is_err());

    match result {
        Err(error) => {
            // Should reject nested ODO
            assert!(error.message.contains("ODO") || error.message.contains("nested"));
        }
        Ok(_) => panic!("expected error"),
    }
}

#[test]
fn test_odo_with_fixed_occurs_allowed() {
    // Test that fixed OCCURS can be nested, but ODO only at tail
    let copybook = r#"
01 RECORD-LAYOUT.
   05 COUNT-FIELD PIC 9(3).
   05 OUTER-ARRAY OCCURS 2 TIMES.
      10 FIXED-FIELD PIC X(5).
      10 INNER-COUNT PIC 9(2).
   05 TAIL-ARRAY PIC X(10) OCCURS 1 TO 3 TIMES DEPENDING ON COUNT-FIELD.
"#;

    let schema = parse_copybook(copybook).unwrap();

    // Should parse successfully
    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 3);

    // OUTER-ARRAY should have fixed OCCURS
    let outer_array = &root.children[1];
    if let Some(Occurs::Fixed { count }) = &outer_array.occurs {
        assert_eq!(*count, 2);
    } else {
        assert!(false, "Expected fixed OCCURS, got {:?}", outer_array.occurs);
    }

    // TAIL-ARRAY should have ODO
    let tail_array = &root.children[2];
    if let Some(Occurs::ODO { min, max, .. }) = &tail_array.occurs {
        assert_eq!(*min, 1);
        assert_eq!(*max, 3);
    } else {
        panic!("Expected ODO");
    }
}

#[test]
fn test_odo_zero_length_record_handling() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 5 TIMES DEPENDING ON ITEM-COUNT.
"#;

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
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Zero-length array (counter = 0)
    let test_data = b"000"; // Just the counter, no array items
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["ITEM-COUNT"], "000");

    let items = json_record["ITEMS"].as_array().unwrap();
    assert_eq!(items.len(), 0); // Empty array
}

#[test]
fn test_odo_comprehensive_error_context() {
    let copybook = r#"
01 RECORD-LAYOUT.
   05 ITEM-COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 3 TIMES DEPENDING ON ITEM-COUNT.
"#;

    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true, // Strict mode for error
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Counter exceeds maximum
    let test_data = b"005ITEM1     ITEM2     ITEM3     ITEM4     ITEM5     ";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err());

    match result {
        Err(error) => {
            // Verify error includes context when available
            if let Some(ctx) = &error.context {
                assert!(ctx.record_index.is_some()); // Should have record index
                assert!(ctx.field_path.is_some()); // Should have field path
                assert!(ctx.byte_offset.is_some()); // Should have byte offset
            }
        }
        Ok(_) => panic!("expected error"),
    }
}
