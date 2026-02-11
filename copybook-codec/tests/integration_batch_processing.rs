#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
//! Integration tests for batch processing workflows
//!
//! This test suite validates end-to-end batch processing:
//! - Large file batch processing
//! - Error handling in real scenarios
//! - Performance under realistic load
//! - Multi-threaded processing

use copybook_codec::{
    DecodeOptions, EncodeOptions, RecordFormat, Codepage, JsonNumberMode, RawMode,
    decode_file_to_jsonl, encode_jsonl_to_file,
};
use copybook_core::{parse_copybook};
use std::io::Cursor;

#[test]
fn test_batch_decode_small_file() {
    // Test decoding a small batch of records
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate 10 records
    let mut data = Vec::new();
    for i in 0..10 {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}

#[test]
fn test_batch_decode_medium_file() {
    // Test decoding a medium batch of records (100)
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate 100 records
    let mut data = Vec::new();
    for i in 0..100 {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}

#[test]
fn test_batch_decode_large_file() {
    // Test decoding a large batch of records (1000)
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate 1000 records
    let mut data = Vec::new();
    for i in 0..1000 {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}

#[test]
fn test_batch_encode_small_file() {
    // Test encoding a small batch of records
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    let options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        zoned_encoding_override: None,
    };

    // Generate 10 JSON records
    let mut json_input = String::new();
    for i in 0..10 {
        json_input.push_str(&format!(r#"{{"TEST-FIELD": "{:05}"}}"#, i));
        json_input.push('\n');
    }

    let input = Cursor::new(json_input.as_bytes());
    let mut output = Vec::new();

    let result = encode_jsonl_to_file(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert_eq!(output.len(), 10 * 5); // 10 records * 5 bytes each
}

#[test]
fn test_batch_encode_medium_file() {
    // Test encoding a medium batch of records (100)
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    let options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        zoned_encoding_override: None,
    };

    // Generate 100 JSON records
    let mut json_input = String::new();
    for i in 0..100 {
        json_input.push_str(&format!(r#"{{"TEST-FIELD": "{:05}"}}"#, i));
        json_input.push('\n');
    }

    let input = Cursor::new(json_input.as_bytes());
    let mut output = Vec::new();

    let result = encode_jsonl_to_file(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert_eq!(output.len(), 100 * 5);
}

#[test]
fn test_batch_roundtrip() {
    // Test batch encode/decode roundtrip
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    // Encode
    let encode_options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        use_raw: false,
        bwz_encode: false,
        strict_mode: false,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
        zoned_encoding_override: None,
    };

    let mut json_input = String::new();
    for i in 0..10 {
        json_input.push_str(&format!(r#"{{"TEST-FIELD": "{:05}"}}"#, i));
        json_input.push('\n');
    }

    let input1 = Cursor::new(json_input.as_bytes());
    let mut encoded = Vec::new();
    encode_jsonl_to_file(&schema, input1, &mut encoded, &encode_options)
        .expect("Should encode successfully");

    // Decode
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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    let input2 = Cursor::new(&encoded);
    let mut decoded = Vec::new();
    decode_file_to_jsonl(&schema, input2, &mut decoded, &decode_options)
        .expect("Should decode successfully");

    assert!(!decoded.is_empty());
}

#[test]
fn test_batch_with_errors() {
    // Test batch processing with some errors
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: Some(5), // Allow up to 5 errors
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate records with some invalid data
    let mut data = Vec::new();
    for i in 0..20 {
        if i % 5 == 0 {
            // Every 5th record is invalid (too short)
            data.extend_from_slice(b"123");
        } else {
            data.extend_from_slice(format!("{:05}", i).as_bytes());
        }
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    // Should succeed but with some errors
    assert!(result.is_ok());
}

#[test]
fn test_batch_multithreaded() {
    // Test batch processing with multiple threads
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        threads: 4, // Use 4 threads
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate 100 records
    let mut data = Vec::new();
    for i in 0..100 {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}

#[test]
fn test_batch_complex_record() {
    // Test batch processing with complex record structure
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(10).
            05  FIELD-3  PIC S9(7)V99.
            05  FIELD-4  PIC 9(3).
    "#;
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate 10 complex records
    let mut data = Vec::new();
    for i in 0..10 {
        data.extend_from_slice(format!("{:05}", i).as_bytes());
        data.extend_from_slice(format!("{:10}", "ABCDEFGHIJ").as_bytes());
        data.extend_from_slice(format!("{:09}", i * 100).as_bytes());
        data.extend_from_slice(format!("{:03}", i % 1000).as_bytes());
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}

#[test]
fn test_batch_rdw_format() {
    // Test batch processing with RDW format
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate RDW records
    let mut data = Vec::new();
    for i in 0..10 {
        // RDW header: 2 bytes for length (big-endian)
        let record_len: u16 = 5; // 5 bytes of data
        data.extend_from_slice(&record_len.to_be_bytes());
        // Record data
        data.extend_from_slice(format!("{:05}", i).as_bytes());
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}

#[test]
fn test_batch_with_ebcdic() {
    // Test batch processing with EBCDIC data
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037, // EBCDIC
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate EBCDIC records
    let mut data = Vec::new();
    for i in 0..10 {
        let digits = format!("{:05}", i);
        for c in digits.chars() {
            let digit = c.to_digit(10).unwrap() as u8;
            data.push(0xF0 + digit); // EBCDIC digit encoding
        }
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}

#[test]
fn test_batch_empty_input() {
    // Test batch processing with empty input
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    let data = Vec::new();
    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    // Empty input should be valid (no records)
    assert!(result.is_ok());
    assert!(output.is_empty());
}

#[test]
fn test_batch_performance_large_record() {
    // Test batch processing with large records
    let copybook = "01 TEST-FIELD PIC X(1000).";
    let schema = parse_copybook(copybook).expect("Should parse copybook");

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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
    };

    // Generate 10 large records
    let mut data = Vec::new();
    for i in 0..10 {
        let record = format!("{:01000}", i);
        data.extend_from_slice(record.as_bytes());
    }

    let input = Cursor::new(&data);
    let mut output = Vec::new();

    let result = decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
    assert!(!output.is_empty());
}
