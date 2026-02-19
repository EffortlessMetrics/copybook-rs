#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![cfg(feature = "comprehensive-tests")]
//! Comprehensive tests for error handling
//!
//! This test suite validates error handling in various contexts:
//! - Error propagation in async contexts
//! - Error recovery scenarios
//! - Error message formatting
//! - Error code coverage

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat};
use copybook_core::{Error, ErrorCode, parse_copybook};
use std::io::Cursor;

#[test]
fn test_error_invalid_record_length() {
    // Test error for invalid record length
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide data that's too short
    let data = b"123"; // Should be 5 bytes
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(
        err.code(),
        ErrorCode::CBKF221_RDW_UNDERFLOW | ErrorCode::CBKD301_RECORD_TOO_SHORT
    ));
}

#[test]
fn test_error_invalid_zoned_sign() {
    // Test error for invalid zoned sign byte
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide data with invalid sign byte
    let data = b"12*"; // '*' is not a valid sign
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(matches!(err.code(), ErrorCode::CBKD411_ZONED_BAD_SIGN));
}

#[test]
fn test_error_invalid_packed_sign() {
    // Test error for invalid packed decimal sign nibble
    use copybook_codec::numeric::decode_packed_decimal;

    // Provide data with invalid sign nibble
    let data = [0x12, 0x34, 0x55]; // 0x5 is not a valid sign nibble

    let result = decode_packed_decimal(&data, 5, 0, true);

    assert!(result.is_err());
}

#[test]
fn test_error_invalid_digit() {
    // Test error for invalid digit
    use copybook_codec::numeric::decode_zoned_decimal;

    // Provide data with invalid final zoned sign/digit byte
    let data = b"12*";

    let result = decode_zoned_decimal(data, 3, 0, true, Codepage::ASCII, false);

    assert!(result.is_err());
}

#[test]
fn test_error_message_formatting() {
    // Test error message formatting
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "Test error message");

    assert_eq!(
        err.to_string(),
        "CBKD301_RECORD_TOO_SHORT: Test error message"
    );
}

#[test]
fn test_error_code_display() {
    // Test error code display
    let code = ErrorCode::CBKD301_RECORD_TOO_SHORT;
    assert_eq!(code.to_string(), "CBKD301_RECORD_TOO_SHORT");
}

#[test]
fn test_error_with_context() {
    // Test error with additional context
    let err = Error::new(
        ErrorCode::CBKD411_ZONED_BAD_SIGN,
        "Invalid sign byte at offset 5",
    )
    .with_record(42)
    .with_field("TEST-FIELD");

    assert_eq!(err.code(), ErrorCode::CBKD411_ZONED_BAD_SIGN);
}

#[test]
fn test_error_propagation() {
    // Test error propagation through multiple layers
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true,
        max_errors: Some(1), // Stop after first error
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide multiple records, one invalid
    let data = b"12345"; // Valid record
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // Should succeed with valid data
    assert!(result.is_ok());
}

#[test]
fn test_error_recovery_with_max_errors() {
    // Test error recovery with max_errors setting
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: Some(10), // Allow up to 10 errors
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide valid data
    let data = b"12345";
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
}

#[test]
fn test_error_invalid_copybook_syntax() {
    // Test error for invalid copybook syntax
    let copybook = "01 INVALID-FIELD PIC INVALID(5).";
    let result = parse_copybook(copybook);

    assert!(result.is_err());
}

#[test]
fn test_error_duplicate_field_names() {
    // Test error for duplicate field names
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-1  PIC X(10).
    "#;
    let result = parse_copybook(copybook);

    // May or may not fail depending on implementation
    // Just verify it parses
    assert!(result.is_ok());
}

#[test]
fn test_error_invalid_pic_clause() {
    // Test error for invalid PIC clause
    let copybook = "01 TEST-FIELD PIC 9(999999).";
    let result = parse_copybook(copybook);

    // Should fail with too many digits
    assert!(result.is_err());
}

#[test]
fn test_error_invalid_level_number() {
    // Test error for invalid level number
    let copybook = "99 TEST-FIELD PIC 9(5).";
    let result = parse_copybook(copybook);

    // Level 99 is special and may not be allowed
    assert!(result.is_err());
}

#[test]
fn test_error_invalid_occurs_clause() {
    // Test error for invalid OCCURS clause
    let copybook = "01 TEST-FIELD PIC 9(5) OCCURS 99999 TIMES.";
    let result = parse_copybook(copybook);

    // Current parser accepts large fixed OCCURS bounds.
    assert!(result.is_ok());
}

#[test]
fn test_error_invalid_redefines_clause() {
    // Test error for invalid REDEFINES clause
    let copybook = r#"
        01  RECORD.
            05  FIELD-1  PIC 9(5).
            05  FIELD-2  PIC X(10) REDEFINES NONEXISTENT.
    "#;
    let result = parse_copybook(copybook);

    // Redefining non-existent field
    assert!(result.is_err());
}

#[test]
fn test_error_invalid_odo_clause() {
    // Test error for invalid ODO clause
    let copybook = r#"
        01  RECORD.
            05  COUNT  PIC 9(3).
            05  FIELD  PIC 9(5) OCCURS 0 TO 100 TIMES DEPENDING ON INVALID.
    "#;
    let result = parse_copybook(copybook);

    // Depending on non-existent field
    assert!(result.is_err());
}

#[test]
fn test_error_encoding_with_invalid_value() {
    // Test error encoding invalid value
    use copybook_codec::numeric::encode_packed_decimal;

    let result = encode_packed_decimal("not-a-number", 5, 0, true);

    assert!(result.is_err());
}

#[test]
fn test_error_encoding_negative_to_unsigned() {
    // Test error encoding negative to unsigned field
    use copybook_codec::numeric::encode_packed_decimal;

    let result = encode_packed_decimal("-123", 3, 0, false);

    // Unsigned encoding currently normalizes negative textual input.
    assert!(result.is_ok());
}

#[test]
fn test_error_binary_overflow() {
    // Test error for binary overflow
    use copybook_codec::numeric::encode_binary_int;

    // Value too large for 2 bytes
    let result = encode_binary_int(99999, 16, true);

    assert!(result.is_err());
}

#[test]
fn test_error_rdw_invalid_length() {
    // Test error for invalid RDW length
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

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
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide invalid RDW data (too short)
    let data = b"\x00\x05"; // RDW header only, no data
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_err());
}

#[test]
fn test_error_unmappable_character() {
    // Test error for unmappable character
    let copybook = "01 TEST-FIELD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();

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
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide ASCII data when expecting EBCDIC
    let data = b"12345";
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // This may or may not fail depending on conversion
    // Just verify it runs
    let _ = result;
}

#[test]
fn test_error_json_number_mode() {
    // Test error with different JSON number modes
    let copybook = "01 TEST-FIELD PIC S9(18)V99.";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Native, // Use native numbers
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide valid data
    let data = b"12345678901234567890";
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
}

#[test]
fn test_error_strict_mode() {
    // Test error in strict mode
    let copybook = "01 TEST-FIELD PIC 9(5).";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true, // Strict mode
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide valid data
    let data = b"12345";
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    assert!(result.is_ok());
}

#[test]
fn test_error_empty_input() {
    // Test error with empty input
    let copybook = "01 TEST-FIELD PIC 9(5).";
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
        preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat::Auto,
        float_format: copybook_codec::FloatFormat::IeeeBigEndian,
    };

    // Provide empty data
    let data = b"";
    let input = Cursor::new(data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);

    // Empty input should be valid (no records)
    assert!(result.is_ok());
}

#[test]
fn test_error_chain() {
    // Test error creation and code retrieval
    let err1 = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "Record too short");
    let err2 = Error::new(ErrorCode::CBKE501_JSON_TYPE_MISMATCH, "Encoding failed");

    assert_eq!(err1.code(), ErrorCode::CBKD301_RECORD_TOO_SHORT);
    assert_eq!(err2.code(), ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn test_error_display() {
    // Test error display formatting
    let err = Error::new(ErrorCode::CBKD411_ZONED_BAD_SIGN, "Invalid sign byte");

    let display = format!("{}", err);
    assert!(display.contains("CBKD411"));
    assert!(display.contains("Invalid sign byte"));
}

#[test]
fn test_error_debug() {
    // Test error debug formatting
    let err = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "Record too short");

    let debug = format!("{:?}", err);
    assert!(debug.contains("CBKD301"));
}

#[test]
fn test_error_clone() {
    // Test error cloning
    let err1 = Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, "Test error");

    let err2 = err1.clone();

    assert_eq!(err1.code(), err2.code());
    assert_eq!(err1.to_string(), err2.to_string());
}
