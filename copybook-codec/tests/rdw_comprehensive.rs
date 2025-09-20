//! Comprehensive RDW tests: reserved bytes, ASCII corruption detection, zero-length records
//!
//! This test suite validates RDW (Record Descriptor Word) handling according to
//! the normative behavior specified in the design document.

use base64::Engine;
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
};
use copybook_core::parse_copybook;
use serde_json::{Value, json};
use std::io::Cursor;

fn create_simple_schema() -> copybook_core::Schema {
    let copybook = "01 SIMPLE-RECORD PIC X(10).";
    parse_copybook(copybook).unwrap()
}

#[test]
fn test_rdw_normal_processing() {
    let schema = create_simple_schema();

    let options = DecodeOptions {
        format: RecordFormat::RDW,
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

    // RDW: 4-byte header (length + reserved) + data
    // Length = 10 (data only, excluding RDW), Reserved = 0x0000
    let test_data = b"\x00\x0A\x00\x00HELLO WRLD"; // 10 bytes data
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 1);

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIMPLE-RECORD"], "HELLO WRLD");
}

#[test]
fn test_rdw_reserved_nonzero_warning() {
    let schema = create_simple_schema();

    let options = DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false, // Lenient mode - should warn but continue
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    // RDW with non-zero reserved bytes
    let test_data = b"\x00\x0A\x12\x34HELLO WRLD"; // Reserved = 0x1234 (non-zero)
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let summary = result.unwrap();
    assert!(summary.has_warnings()); // Should have CBKR211_RDW_RESERVED_NONZERO warning

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIMPLE-RECORD"], "HELLO WRLD");
}

#[test]
fn test_rdw_reserved_nonzero_strict_fatal() {
    let schema = create_simple_schema();

    let options = DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true, // Strict mode - should be fatal
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    // RDW with non-zero reserved bytes
    let test_data = b"\x00\x0A\x12\x34HELLO WRLD";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err()); // Should fail in strict mode

    let error = result.unwrap_err();
    assert!(error.message.contains("reserved") || error.message.contains("RDW"));
}

#[test]
fn test_rdw_raw_preservation_with_reserved() {
    let schema = create_simple_schema();

    // First decode with raw capture
    let decode_options = DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::RecordRDW, // Capture RDW + record
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    // RDW with non-zero reserved bytes
    let original_data = b"\x00\x0A\x12\x34HELLO WRLD";
    let input = Cursor::new(original_data);
    let mut decode_output = Vec::new();

    let decode_result =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut decode_output, &decode_options);
    assert!(decode_result.is_ok());

    let decoded_str = String::from_utf8(decode_output).unwrap();
    let decoded_json: Value = serde_json::from_str(decoded_str.trim()).unwrap();

    // Should have raw data including RDW
    assert!(decoded_json.get("__raw_b64").is_some());

    // Now encode back with raw usage
    let jsonl_data = format!("{}\n", decoded_json.to_string());

    let encode_options = EncodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        use_raw: true, // Use raw data
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut encode_output = Vec::new();

    let encode_result =
        copybook_codec::encode_jsonl_to_file(&schema, input, &mut encode_output, &encode_options);
    assert!(encode_result.is_ok());

    // Should preserve reserved bytes exactly (NORMATIVE)
    assert_eq!(encode_output, original_data);
}

#[test]
fn test_rdw_suspect_ascii_heuristic() {
    let schema = create_simple_schema();

    let options = DecodeOptions {
        format: RecordFormat::RDW,
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

    // Create RDW that looks like ASCII digits (suspect corruption)
    // ASCII "0010" = 0x30303130 which could be mistaken for length
    let suspect_data = b"0010HELLO WRLD"; // ASCII digits that might be corrupted RDW
    let input = Cursor::new(suspect_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // This might succeed or fail depending on implementation, but should detect ASCII pattern
    if result.is_ok() {
        let summary = result.unwrap();
        // Should have warning about suspect ASCII corruption
        assert!(summary.has_warnings());
    } else {
        // Or might fail with ASCII corruption detection error
        let error = result.unwrap_err();
        assert!(error.message.contains("ASCII") || error.message.contains("corruption"));
    }
}

#[test]
fn test_rdw_zero_length_record_valid() {
    // Test zero-length record when schema has no fixed prefix
    let copybook = r#"
01 VARIABLE-RECORD.
   05 COUNT PIC 9(3).
   05 ITEMS PIC X(10) OCCURS 0 TO 5 TIMES DEPENDING ON COUNT.
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
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    // Zero-length record: RDW with length 0
    let test_data = b"\x00\x00\x00\x00"; // Length = 0, Reserved = 0
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // This should be valid when schema allows zero-length records
    if schema.tail_odo.is_some() {
        assert!(result.is_ok());

        let output_str = String::from_utf8(output).unwrap();
        if !output_str.trim().is_empty() {
            let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();
            // Should handle zero-length appropriately
            assert!(json_record.is_object());
        }
    }
}

#[test]
fn test_rdw_zero_length_record_invalid() {
    // Test zero-length record when schema has fixed prefix (should fail)
    let schema = create_simple_schema(); // Fixed 10-byte record

    let options = DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    // Zero-length record with fixed schema
    let test_data = b"\x00\x00\x00\x00"; // Length = 0
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err()); // Should fail - zero length invalid for fixed schema

    let error = result.unwrap_err();
    assert!(
        error.message.contains("underflow")
            || error.message.contains("length")
            || error.message.contains("record errors")
            || error.message.contains("payload")
    );
}

#[test]
fn test_rdw_length_recomputation_on_encode() {
    let schema = create_simple_schema();

    // First decode to get JSON
    let decode_options = DecodeOptions {
        format: RecordFormat::RDW,
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

    let original_data = b"\x00\x0A\x00\x00HELLO WRLD";
    let input = Cursor::new(original_data);
    let mut decode_output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut decode_output, &decode_options)
        .unwrap();

    let decoded_str = String::from_utf8(decode_output).unwrap();
    let mut decoded_json: Value = serde_json::from_str(decoded_str.trim()).unwrap();

    // Modify the data (change length)
    decoded_json["SIMPLE-RECORD"] = json!("MODIFIED  "); // Still 10 bytes

    let jsonl_data = format!("{}\n", decoded_json.to_string());

    let encode_options = EncodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        use_raw: false, // Don't use raw - should recompute length
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
    };

    let input = Cursor::new(jsonl_data.as_bytes());
    let mut encode_output = Vec::new();

    let result =
        copybook_codec::encode_jsonl_to_file(&schema, input, &mut encode_output, &encode_options);
    assert!(result.is_ok());

    // Should have correct RDW length (10 bytes for data)
    assert_eq!(encode_output.len(), 14); // 4 bytes RDW + 10 bytes data
    assert_eq!(&encode_output[0..4], b"\x00\x0A\x00\x00"); // Length = 10, Reserved = 0
    assert_eq!(&encode_output[4..14], b"MODIFIED  ");
}

#[test]
fn test_rdw_multiple_records() {
    let schema = create_simple_schema();

    let options = DecodeOptions {
        format: RecordFormat::RDW,
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

    // Multiple RDW records
    let mut test_data = Vec::new();
    test_data.extend_from_slice(b"\x00\x0A\x00\x00RECORD ONE"); // Record 1
    test_data.extend_from_slice(b"\x00\x0A\x00\x00RECORD TWO"); // Record 2
    test_data.extend_from_slice(b"\x00\x0A\x00\x00RECORD 3  "); // Record 3

    let input = Cursor::new(&test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok());

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 3);

    let output_str = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = output_str.trim().split('\n').collect();
    assert_eq!(lines.len(), 3);

    // Verify each record
    let record1: Value = serde_json::from_str(lines[0]).unwrap();
    let record2: Value = serde_json::from_str(lines[1]).unwrap();
    let record3: Value = serde_json::from_str(lines[2]).unwrap();

    assert_eq!(record1["SIMPLE-RECORD"], "RECORD ONE");
    assert_eq!(record2["SIMPLE-RECORD"], "RECORD TWO");
    assert_eq!(record3["SIMPLE-RECORD"], "RECORD 3  ");
}

#[test]
fn test_rdw_big_endian_length() {
    let schema = create_simple_schema();

    let options = DecodeOptions {
        format: RecordFormat::RDW,
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

    // Test big-endian length encoding
    // Length 256 (0x0100) should be encoded as 0x01 0x00
    let test_data = b"\x01\x00\x00\x00"; // Length = 256, but no data (will fail)
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    // This should fail due to insufficient data, but demonstrates big-endian parsing
    assert!(result.is_err());
}

#[test]
fn test_rdw_raw_record_only_mode() {
    let schema = create_simple_schema();

    let decode_options = DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Record, // Record only (no RDW)
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    let test_data = b"\x00\x0A\x00\x00HELLO WRLD";
    let input = Cursor::new(test_data);
    let mut decode_output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut decode_output, &decode_options)
        .unwrap();

    let decoded_str = String::from_utf8(decode_output).unwrap();
    let decoded_json: Value = serde_json::from_str(decoded_str.trim()).unwrap();

    // Should have raw data for record only (not including RDW)
    assert!(decoded_json.get("__raw_b64").is_some());

    // Raw data should be just the record content (10 bytes)
    let raw_b64 = decoded_json["__raw_b64"].as_str().unwrap();
    let raw_data = base64::engine::general_purpose::STANDARD
        .decode(raw_b64)
        .unwrap();
    assert_eq!(raw_data, b"HELLO WRLD");
    assert_eq!(raw_data.len(), 10); // Should not include 4-byte RDW
}
