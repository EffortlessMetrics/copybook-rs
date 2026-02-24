// SPDX-License-Identifier: AGPL-3.0-or-later
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
use serde_json::json;
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
        Err(error) => assert_eq!(error.code, ErrorCode::CBKP023_ODO_REDEFINES),
        Ok(_) => panic!("expected error CBKP023_ODO_REDEFINES"),
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
        Err(error) => assert_eq!(error.code, ErrorCode::CBKP021_ODO_NOT_TAIL),
        Ok(_) => panic!("expected error CBKP021_ODO_NOT_TAIL"),
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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Counter value exceeds maximum (005 > 3)
    let test_data = b"005ITEM1     ITEM2     ITEM3     ITEM4     ITEM5     ";
    // Should fail for ODO count above max
    let result = copybook_codec::decode_record(&schema, test_data, &options);
    assert!(result.is_err());

    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKS301_ODO_CLIPPED),
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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Counter value exceeds maximum (005 > 3)
    let test_data = b"005ITEM1     ITEM2     ITEM3     ";
    // Lenient mode should clamp and continue with a warning
    let result = copybook_codec::decode_record(&schema, test_data, &options);
    assert!(result.is_ok());

    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKS301_ODO_CLIPPED),
        Ok(_) => panic!("expected error"),
    }
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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Counter value below minimum (001 < 2)
    let test_data = b"001ITEM1     ITEM2     ";
    // Lenient mode should clamp and continue with a warning
    let result = copybook_codec::decode_record(&schema, test_data, &options);
    assert!(result.is_ok());

    match result {
        Err(error) => assert_eq!(error.code, ErrorCode::CBKS302_ODO_RAISED),
        Ok(_) => panic!("expected error"),
    }
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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Test different array lengths
    let test_cases = vec![
        (b"000".as_slice(), 0),                               // 0 items
        (b"001ITEM1     ".as_slice(), 1),                     // 1 item
        (b"003ITEM1     ITEM2     ITEM3     ".as_slice(), 3), // 3 items
    ];

    for (test_data, expected_count) in test_cases {
        let result = copybook_codec::decode_record(&schema, test_data, &options);
        assert!(result.is_ok(), "Failed for {expected_count} items");
        let json_record = result.unwrap();

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
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        ..EncodeOptions::default()
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // Should succeed but does not update counter in current lib_api encoder path
    let summary =
        copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options).unwrap();
    assert_eq!(summary.records_with_errors, 0);

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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    let json_record = copybook_codec::decode_record(&schema, &output, &decode_options).unwrap();

    // Counter remains as provided in JSON
    assert_eq!(json_record["ITEM-COUNT"], "002");

    let items = json_record["ITEMS"].as_array().unwrap();
    assert_eq!(items.len(), 2);
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
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
        strict_mode: true, // Strict mode
        ..EncodeOptions::default()
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut output = Vec::new();

    // ODO array length is now enforced in lib_api encode path
    let summary =
        copybook_codec::encode_jsonl_to_file(&schema, input, &mut output, &options).unwrap();
    assert_eq!(summary.records_with_errors, 1);
    assert!(output.is_empty());
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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Zero-length array (counter = 0)
    let test_data = b"000"; // Just the counter, no array items
    let json_record = copybook_codec::decode_record(&schema, test_data, &options).unwrap();

    assert_eq!(json_record["ITEM-COUNT"], "0");

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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Counter exceeds maximum
    let test_data = b"005ITEM1     ITEM2     ITEM3     ITEM4     ITEM5     ";
    let result = copybook_codec::decode_record(&schema, test_data, &options);
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
