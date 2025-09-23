//! Golden fixture tests for copybook-rs
//!
//! These tests verify that the library produces consistent, expected outputs
//! for well-known inputs. They serve as regression tests and documentation
//! of the expected behavior.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    decode_file_to_jsonl, encode_jsonl_to_file,
};
use copybook_core::parse_copybook;
use sha2::{Digest, Sha256};
use std::fs;
use std::io::Cursor;

/// Test that verifies COMP-3 round-trip encoding and decoding works correctly
#[test]
fn test_comp3_roundtrip_golden() {
    let copybook_text = fs::read_to_string("../fixtures/copybooks/comp3_test.cpy")
        .expect("Failed to read COMP-3 test copybook");

    let schema = parse_copybook(&copybook_text).expect("Failed to parse COMP-3 test copybook");

    // Read test JSONL data
    let jsonl_data = fs::read_to_string("../fixtures/data/comp3_test.jsonl")
        .expect("Failed to read COMP-3 test JSONL");

    // Configure encode options
    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_use_raw(false)
        .with_bwz_encode(false)
        .with_strict_mode(false)
        .with_max_errors(None)
        .with_threads(1)
        .with_coerce_numbers(true)
        .with_zoned_encoding_override(None);

    // Encode JSONL to binary
    let mut binary_output = Vec::new();
    let input_cursor = Cursor::new(jsonl_data.as_bytes());
    let encode_summary =
        encode_jsonl_to_file(&schema, input_cursor, &mut binary_output, &encode_options)
            .expect("Failed to encode COMP-3 test data");

    // Verify encoding succeeded
    assert_eq!(encode_summary.records_processed, 2);
    assert_eq!(encode_summary.records_with_errors, 0);

    // Configure decode options
    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(false)
        .with_emit_meta(false)
        .with_emit_raw(RawMode::Off)
        .with_strict_mode(true)
        .with_max_errors(None)
        .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
        .with_threads(1)
        .with_preserve_zoned_encoding(false)
        .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);

    // Decode binary back to JSONL
    let mut decoded_output = Vec::new();
    let binary_cursor = Cursor::new(&binary_output);
    let decode_summary =
        decode_file_to_jsonl(&schema, binary_cursor, &mut decoded_output, &decode_options)
            .expect("Failed to decode COMP-3 test data");

    // Verify decoding succeeded
    assert_eq!(decode_summary.records_processed, 2);
    assert_eq!(decode_summary.records_with_errors, 0);

    // Parse the decoded JSONL and verify content
    let decoded_jsonl =
        String::from_utf8(decoded_output).expect("Invalid UTF-8 in decoded output");
    let lines: Vec<&str> = decoded_jsonl.trim().split('\n').collect();
    assert_eq!(lines.len(), 2);

    // Verify first record
    let record1: serde_json::Value =
        serde_json::from_str(lines[0]).expect("Failed to parse first decoded record");
    assert_eq!(record1["RECORD-ID"], "0001");
    assert_eq!(record1["POSITIVE-AMOUNT"], "12345");
    assert_eq!(record1["NEGATIVE-AMOUNT"], "-67890");
    assert_eq!(record1["DECIMAL-AMOUNT"], "123.45");
    assert_eq!(record1["UNSIGNED-AMOUNT"], "999");

    // Verify second record
    let record2: serde_json::Value =
        serde_json::from_str(lines[1]).expect("Failed to parse second decoded record");
    assert_eq!(record2["RECORD-ID"], "0002");
    assert_eq!(record2["POSITIVE-AMOUNT"], "1");
    assert_eq!(record2["NEGATIVE-AMOUNT"], "-1");
    assert_eq!(record2["DECIMAL-AMOUNT"], "-999.99");
    assert_eq!(record2["UNSIGNED-AMOUNT"], "0");
}

/// Test that verifies schema parsing produces consistent output
#[test]
fn test_schema_parsing_golden() {
    let copybooks = [
        "../fixtures/copybooks/simple.cpy",
        "../fixtures/copybooks/complex.cpy",
        "../fixtures/copybooks/odo.cpy",
        "../fixtures/copybooks/comp3_test.cpy",
    ];

    for copybook_path in &copybooks {
        let copybook_text = fs::read_to_string(copybook_path)
            .unwrap_or_else(|_| panic!("Failed to read copybook: {copybook_path}"));

        let schema = parse_copybook(&copybook_text)
            .unwrap_or_else(|_| panic!("Failed to parse copybook: {copybook_path}"));

        // Verify schema has expected structure
        assert!(
            !schema.fields.is_empty(),
            "Schema should have fields for {copybook_path}"
        );

        // Verify schema fingerprint is consistent
        assert!(
            !schema.fingerprint.is_empty(),
            "Schema should have fingerprint for {copybook_path}"
        );

        // For fixed-length schemas, verify LRECL
        if copybook_path.contains("simple") || copybook_path.contains("comp3") {
            assert!(
                schema.lrecl_fixed.is_some(),
                "Fixed-length schema should have LRECL for {copybook_path}"
            );
        }
    }
}

/// Calculate SHA-256 hash of data for golden test validation
fn calculate_sha256(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
}

/// Test that creates golden hashes for regression detection
#[test]
fn test_create_golden_hashes() {
    // This test can be used to generate golden hashes when fixtures change
    // Normally it should be ignored in CI, but useful for updating fixtures
    if std::env::var("CREATE_GOLDEN_HASHES").is_ok() {
        let test_files = [
            "../fixtures/copybooks/simple.cpy",
            "../fixtures/copybooks/complex.cpy",
            "../fixtures/copybooks/odo.cpy",
            "../fixtures/copybooks/comp3_test.cpy",
        ];

        for file_path in &test_files {
            if let Ok(content) = fs::read(file_path) {
                let hash = calculate_sha256(&content);
                let hash_file = format!("{file_path}.sha256");
                fs::write(&hash_file, hash)
                    .unwrap_or_else(|_| panic!("Failed to write hash file: {hash_file}"));
                println!("Created hash file: {file_path} -> {hash_file}");
            }
        }
    }
}
