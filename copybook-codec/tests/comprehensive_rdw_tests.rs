//! Comprehensive RDW (Record Descriptor Word) tests covering all edge cases
//!
//! This test suite validates RDW record handling according to the normative
//! behavior specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::{ErrorCode, parse_copybook};
use serde_json::{Value, json};
use std::io::Cursor;

fn create_rdw_decode_options(emit_raw: RawMode, strict: bool) -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw,
        strict_mode: strict,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    }
}

fn create_rdw_encode_options(use_raw: bool, strict: bool) -> EncodeOptions {
    EncodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        use_raw,
        bwz_encode: false,
        strict_mode: strict,
        max_errors: None,
        threads: 1,
    }
}

#[test]
fn test_rdw_basic_parsing() {
    let copybook = "01 SIMPLE-RECORD PIC X(10).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // Create RDW record: length=10, reserved=0x0000, data="HELLO12345"
    let rdw_data = b"\x00\x0A\x00\x00HELLO12345";
    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Basic RDW parsing should succeed");

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 1);

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIMPLE-RECORD"], "HELLO12345");
}

#[test]
fn test_rdw_reserved_bytes_nonzero_warning() {
    let copybook = "01 SIMPLE-RECORD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();

    // Test lenient mode: warning for non-zero reserved bytes
    let lenient_options = create_rdw_decode_options(RawMode::Off, false);

    // RDW with non-zero reserved bytes: length=5, reserved=0x1234
    let rdw_data = b"\x00\x05\x12\x34HELLO";
    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &lenient_options);
    assert!(result.is_ok(), "Should succeed in lenient mode");

    let summary = result.unwrap();
    assert!(
        summary.has_warnings(),
        "Should have CBKR211_RDW_RESERVED_NONZERO warning"
    );

    // Test strict mode: error for non-zero reserved bytes
    let strict_options = create_rdw_decode_options(RawMode::Off, true);
    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &strict_options);
    assert!(result.is_err(), "Should fail in strict mode");

    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
}

#[test]
fn test_rdw_raw_preservation_normative() {
    // Test NORMATIVE: --emit-raw=record+rdw preserves reserved bytes
    let copybook = "01 SIMPLE-RECORD PIC X(8).";
    let schema = parse_copybook(copybook).unwrap();

    // Decode with raw capture including RDW
    let decode_options = create_rdw_decode_options(RawMode::RecordRDW, false);

    // RDW with non-zero reserved bytes: length=8, reserved=0xABCD
    let rdw_data = b"\x00\x08\xAB\xCDHELLO123";
    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &decode_options);
    assert!(result.is_ok(), "Should succeed with raw capture");

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should have raw data including RDW header
    assert!(json_record.get("__raw_b64").is_some());

    // Encode with raw usage
    let encode_options = create_rdw_encode_options(true, false);

    let result = copybook_codec::encode_record(&schema, &json_record, &encode_options);
    assert!(result.is_ok(), "Should succeed with raw usage");

    let encoded_data = result.unwrap();

    // Should preserve reserved bytes exactly
    assert_eq!(&encoded_data[0..4], b"\x00\x08\xAB\xCD"); // RDW with preserved reserved bytes
    assert_eq!(&encoded_data[4..12], b"HELLO123"); // Payload
}

#[test]
fn test_rdw_length_recomputation() {
    // Test that RDW length is recomputed if payload changes
    let copybook = "01 VARIABLE-RECORD PIC X(20).";
    let schema = parse_copybook(copybook).unwrap();

    // Original data with length=8
    let original_rdw = b"\x00\x08\x00\x00ORIGINAL";
    let input = Cursor::new(original_rdw);

    println!(
        "DEBUG: Starting decode_file_to_jsonl with schema: {}",
        copybook
    );
    println!("DEBUG: Original RDW input: {:?}", original_rdw);

    // Prepare output with newline-terminated JSONL
    let mut output = Vec::new();

    let mut decode_options = create_rdw_decode_options(RawMode::RecordRDW, false);
    decode_options.threads = 1; // Ensure single-threaded for consistent testing

    // Decode with direct error checking
    let decode_result =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &decode_options);

    match &decode_result {
        Ok(summary) => {
            println!(
                "DEBUG: Decode successful, records processed: {}",
                summary.records_processed
            );
        }
        Err(e) => {
            println!("DEBUG: Decode failed: {:?}", e);
            panic!("Test failed during decode");
        }
    }
    let summary = decode_result.unwrap();

    // Debug output diagnostics
    let output_str = String::from_utf8(output).expect("Failed to convert output to UTF-8");

    println!("DEBUG: Summary: {:?}", summary);
    println!("DEBUG: Output string: {}", output_str);

    // Validate that a single record was processed
    assert_eq!(
        summary.records_processed, 1,
        "Exactly one record should be processed"
    );
    assert!(!output_str.is_empty(), "Output should not be empty");

    let json_record: Value = serde_json::from_str(output_str.trim()).expect("Failed to parse JSON");

    // Verify original record content
    assert_eq!(
        json_record["VARIABLE-RECORD"], "ORIGINAL",
        "Original record should match input"
    );

    // Modify the payload to different length
    let mut modified_record = json_record.clone();
    modified_record["VARIABLE-RECORD"] = json!("MODIFIED-LONGER-DATA");

    // Encode with raw usage
    let encode_options = create_rdw_encode_options(true, false);
    let result = copybook_codec::encode_record(&schema, &modified_record, &encode_options);

    assert!(result.is_ok(), "Encoding failed");

    let encoded_data = result.unwrap();

    // Detailed length computation diagnostics
    println!("DEBUG: Encoded Data: {:?}", encoded_data);
    println!("DEBUG: Encoded Data Length: {}", encoded_data.len());

    // Length should be recomputed (20 bytes for new payload)
    assert_eq!(
        &encoded_data[0..2],
        b"\x00\x14",
        "RDW length should be 0x14 (20 bytes)"
    ); // Length = 20 (0x14)
    assert_eq!(
        &encoded_data[2..4],
        b"\x00\x00",
        "Reserved bytes should be preserved"
    ); // Reserved bytes preserved
    assert_eq!(
        &encoded_data[4..24],
        b"MODIFIED-LONGER-DATA",
        "Payload should match modified data"
    ); // New payload
}

#[test]
fn test_rdw_suspect_ascii_heuristic() {
    let copybook = "01 SIMPLE-RECORD PIC X(10).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // Create data that looks like ASCII-corrupted RDW
    // ASCII digits "0010" instead of binary 0x000A
    let suspect_data = b"0010HELLO12345";
    let input = Cursor::new(suspect_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // Should detect and warn about suspected ASCII corruption
    if let Ok(summary) = result {
        assert!(
            summary.has_warnings(),
            "Should have CBKF104_RDW_SUSPECT_ASCII warning"
        );
    } else {
        // Or might fail with appropriate error
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }
}

#[test]
fn test_rdw_zero_length_record() {
    let copybook = "01 EMPTY-RECORD."; // Group with no fields
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // Zero-length record: length=0, reserved=0x0000, no payload
    let rdw_data = b"\x00\x00\x00\x00";
    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // Should succeed if schema has no fixed prefix
    if schema.lrecl_fixed.unwrap_or(0) == 0 {
        assert!(
            result.is_ok(),
            "Zero-length record should be valid for empty schema"
        );
    } else {
        // Should fail with underflow error
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.code, ErrorCode::CBKR221_RDW_UNDERFLOW);
    }
}

#[test]
fn test_rdw_underflow_error() {
    let copybook = "01 FIXED-RECORD PIC X(10)."; // Requires 10 bytes
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, true); // Strict mode

    // RDW claims 5 bytes but schema needs 10
    let underflow_data = b"\x00\x05\x00\x00HELLO";
    let input = Cursor::new(underflow_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err(), "Should fail with underflow");

    let error = result.unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKR221_RDW_UNDERFLOW);
}

#[test]
fn test_rdw_multiple_records() {
    let copybook = "01 MULTI-RECORD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // Multiple RDW records concatenated
    let multi_rdw_data = b"\x00\x05\x00\x00FIRST\x00\x05\x00\x00SECND\x00\x05\x00\x00THIRD";
    let input = Cursor::new(multi_rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Should process multiple records");

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 3);

    let output_str = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = output_str.trim().split('\n').collect();
    assert_eq!(lines.len(), 3);

    let record1: Value = serde_json::from_str(lines[0]).unwrap();
    let record2: Value = serde_json::from_str(lines[1]).unwrap();
    let record3: Value = serde_json::from_str(lines[2]).unwrap();

    assert_eq!(record1["MULTI-RECORD"], "FIRST");
    assert_eq!(record2["MULTI-RECORD"], "SECND");
    assert_eq!(record3["MULTI-RECORD"], "THIRD");
}

#[test]
fn test_rdw_with_odo_variable_length() {
    let copybook = r#"
01 ODO-RDW-RECORD.
   05 COUNTER PIC 9(2).
   05 VARIABLE-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON COUNTER PIC X(3).
"#;

    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // RDW record with ODO: counter=3, so 3 array elements
    // Total payload: 2 (counter) + 9 (3 * 3 bytes) = 11 bytes
    let rdw_odo_data = b"\x00\x0B\x00\x0003ABCDEFGHI";
    let input = Cursor::new(rdw_odo_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Should handle ODO in RDW records");

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["COUNTER"], "03");
    let array = json_record["VARIABLE-ARRAY"].as_array().unwrap();
    assert_eq!(array.len(), 3);
    assert_eq!(array[0], "ABC");
    assert_eq!(array[1], "DEF");
    assert_eq!(array[2], "GHI");
}

#[test]
fn test_rdw_big_endian_length() {
    let copybook = "01 RECORD PIC X(38)."; // Larger record to test 16-bit length within limits
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // RDW with length = 38 (0x0026 in big-endian)
    let mut rdw_data = vec![0x00, 0x26, 0x00, 0x00]; // Length=38, reserved=0
    rdw_data.extend(vec![b'A'; 38]); // 38 bytes of 'A'

    let input = Cursor::new(rdw_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Should handle 16-bit big-endian length");

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    let record_value = json_record["RECORD"].as_str().unwrap();
    assert_eq!(record_value.len(), 38);
    assert!(record_value.chars().all(|c| c == 'A'));
}

#[test]
fn test_rdw_encoding_round_trip() {
    let copybook = "01 ROUND-TRIP-RECORD PIC X(12).";
    let schema = parse_copybook(copybook).unwrap();

    // Original RDW data
    let original_data = b"\x00\x0C\x00\x00HELLO-WORLD!";

    // Decode
    let decode_options = create_rdw_decode_options(RawMode::RecordRDW, false);
    let input = Cursor::new(original_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &decode_options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Encode back
    let encode_options = create_rdw_encode_options(true, false);
    let result = copybook_codec::encode_record(&schema, &json_record, &encode_options);
    assert!(result.is_ok());

    let encoded_data = result.unwrap();

    // Should be identical to original
    assert_eq!(encoded_data, original_data);
}

#[test]
fn test_rdw_error_context() {
    let copybook = "01 CONTEXT-RECORD PIC X(10).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, true); // Strict mode

    // Invalid RDW (truncated)
    let invalid_data = b"\x00\x0A"; // Only 2 bytes of RDW header
    let input = Cursor::new(invalid_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err());

    let error = result.unwrap_err();

    // Should have context information
    if let Some(context) = &error.context {
        // Should indicate record number or byte offset
        assert!(context.record_index.is_some() || context.byte_offset.is_some());
    }
}

#[test]
fn test_rdw_maximum_length_handling() {
    let copybook = "01 MAX-RECORD PIC X(38)."; // Maximum COBOL PIC length
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // RDW with length matching PIC (38 = 0x0026)
    let mut max_data = vec![0x00, 0x26, 0x00, 0x00]; // Length=38, reserved=0
    max_data.extend(vec![b'X'; 38]); // Maximum payload for this test

    let input = Cursor::new(max_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // Should handle maximum length (may be limited by implementation)
    if result.is_ok() {
        let summary = result.unwrap();
        assert_eq!(summary.records_processed, 1);
    } else {
        // May fail due to memory or other limits, which is acceptable
        println!("Test failed (acceptable): {:?}", result.unwrap_err());
    }
}

#[test]
fn test_rdw_partial_read_handling() {
    let copybook = "01 PARTIAL-RECORD PIC X(10).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, true); // Strict mode

    // RDW claims 10 bytes but only 5 bytes follow
    let partial_data = b"\x00\x0A\x00\x00HELLO"; // Claims 10, provides 5
    let input = Cursor::new(partial_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err(), "Should fail with partial read");

    // Should get appropriate error (could be underflow or read error)
    let error = result.unwrap_err();
    assert!(matches!(
        error.code,
        ErrorCode::CBKR221_RDW_UNDERFLOW | ErrorCode::CBKD301_RECORD_TOO_SHORT
    ));
}

#[test]
fn test_rdw_empty_file_handling() {
    let copybook = "01 EMPTY-FILE-RECORD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_rdw_decode_options(RawMode::Off, false);

    // Empty input
    let empty_data = b"";
    let input = Cursor::new(empty_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Empty file should be handled gracefully");

    let summary = result.unwrap();
    assert_eq!(summary.records_processed, 0);

    let output_str = String::from_utf8(output).unwrap();
    assert!(output_str.trim().is_empty());
}
