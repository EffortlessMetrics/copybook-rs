#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::assertions_on_constants
)]
#![cfg(feature = "comprehensive-tests")]
//! Comprehensive numeric type tests: zoned, packed, binary with all edge cases
//!
//! This test suite validates numeric type handling according to the normative
//! behavior specified in the design document.

use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, ZonedEncodingFormat,
};
use copybook_core::{FieldKind, parse_copybook};
use serde_json::Value;
use std::io::Cursor;

#[test]
fn test_zoned_decimal_ebcdic_sign_zones() {
    // Test EBCDIC overpunch sign zones
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
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
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Test positive signs (C zone = +)
    let positive_data = b"\xF1\xF2\xC3"; // "12C" in EBCDIC = +123
    let input = Cursor::new(positive_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-FIELD"], "123");

    // Test negative signs (D zone = -)
    let negative_data = b"\xF1\xF2\xD3"; // "12D" in EBCDIC = -123
    let input = Cursor::new(negative_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-FIELD"], "-123");
}

#[test]
fn test_zoned_decimal_ascii_sign_zones() {
    // Test ASCII overpunch sign zones
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
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

    // Test ASCII positive overpunch
    let positive_data = b"12{"; // "12{" = +120 in ASCII overpunch
    let input = Cursor::new(positive_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-FIELD"], "120");

    // Test ASCII negative overpunch
    let negative_data = b"12L"; // "12L" = -123 in ASCII overpunch
    let input = Cursor::new(negative_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-FIELD"], "-123");
}

#[test]
fn test_blank_when_zero_handling() {
    let copybook = "01 BWZ-FIELD PIC 9(5) BLANK WHEN ZERO.";
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

    // Test all spaces (should decode to 0 with warning)
    let blank_data = b"     "; // 5 spaces
    let input = Cursor::new(blank_data);
    let mut output = Vec::new();

    let summary =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    assert!(summary.has_warnings()); // Should have CBKD412_ZONED_BLANK_IS_ZERO warning

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["BWZ-FIELD"], "0");
}

#[test]
fn test_zoned_invalid_zone_error() {
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true, // Strict mode for errors
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Invalid zone in last character
    let invalid_data = b"12X"; // X is not a valid zone

    // Test with decode_record directly to get specific error
    let result = copybook_codec::decode_record(&schema, invalid_data, &options);
    assert!(result.is_err());

    match result {
        Err(error) => assert!(
            error.message.to_lowercase().contains("invalid")
                || error.message.to_lowercase().contains("zone")
        ),
        Ok(_) => panic!("expected error"),
    }
}

#[test]
fn test_zoned_negative_zero_normalization() {
    // Test that -0 is normalized to 0 (NORMATIVE)
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
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

    // Create -0 in ASCII overpunch (00} = -000)
    let negative_zero_data = b"00}";
    let input = Cursor::new(negative_zero_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should be normalized to "0", not "-0"
    assert_eq!(json_record["SIGNED-FIELD"], "0");
}

#[test]
fn test_packed_decimal_odd_digits() {
    let copybook = "01 PACKED-FIELD PIC 9(5) COMP-3."; // 5 digits = 3 bytes
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

    // Packed: 12345 = 0x12345C (3 bytes)
    let packed_data = b"\x12\x34\x5C"; // 12345 positive
    let input = Cursor::new(packed_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["PACKED-FIELD"], "12345");
}

#[test]
fn test_packed_decimal_even_digits() {
    let copybook = "01 PACKED-FIELD PIC 9(6) COMP-3."; // 6 digits = 4 bytes
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

    // Packed: 123456 = 0x123456C (4 bytes)
    let packed_data = b"\x12\x34\x56\x0C"; // 123456 positive
    let input = Cursor::new(packed_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["PACKED-FIELD"], "123456");
}

#[test]
fn test_packed_decimal_sign_nibbles() {
    let copybook = "01 SIGNED-PACKED PIC S9(3) COMP-3."; // 3 digits = 2 bytes
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

    // Test C sign (positive)
    let positive_data = b"\x12\x3C"; // 123 positive (C = +)
    let input = Cursor::new(positive_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-PACKED"], "123");

    // Test D sign (negative)
    let negative_data = b"\x12\x3D"; // 123 negative (D = -)
    let input = Cursor::new(negative_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-PACKED"], "-123");

    // Test F sign (positive, unsigned format)
    let unsigned_data = b"\x12\x3F"; // 123 unsigned (F = +)
    let input = Cursor::new(unsigned_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-PACKED"], "123");
}

#[test]
fn test_packed_decimal_invalid_nibble() {
    let copybook = "01 PACKED-FIELD PIC 9(3) COMP-3.";
    let schema = parse_copybook(copybook).unwrap();

    let options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: true, // Strict mode for errors
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Invalid nibble (A in digit position)
    let invalid_data = b"\x1A\x3C"; // 1A3 - A is invalid digit

    // Test with decode_record directly to get specific error
    let result = copybook_codec::decode_record(&schema, invalid_data, &options);
    assert!(result.is_err());

    match result {
        Err(error) => assert!(
            error.message.to_lowercase().contains("invalid")
                || error.message.to_lowercase().contains("nibble")
        ),
        Ok(_) => panic!("expected error"),
    }
}

#[test]
fn test_binary_width_by_digits() {
    // Test NORMATIVE binary width mapping: ≤4→2B, 5-9→4B, 10-18→8B
    let test_cases = vec![
        ("01 BIN1 PIC 9(1) COMP.", 2),  // ≤4 digits → 2 bytes
        ("01 BIN2 PIC 9(4) COMP.", 2),  // ≤4 digits → 2 bytes
        ("01 BIN3 PIC 9(5) COMP.", 4),  // 5-9 digits → 4 bytes
        ("01 BIN4 PIC 9(9) COMP.", 4),  // 5-9 digits → 4 bytes
        ("01 BIN5 PIC 9(10) COMP.", 8), // 10-18 digits → 8 bytes
        ("01 BIN6 PIC 9(18) COMP.", 8), // 10-18 digits → 8 bytes
    ];

    for (copybook, expected_len) in test_cases {
        let schema = parse_copybook(copybook).unwrap();
        let field = &schema.fields[0];

        assert_eq!(field.len, expected_len, "Failed for: {copybook}");

        if let FieldKind::BinaryInt { bits, .. } = &field.kind {
            assert_eq!(
                u32::from(*bits) / 8,
                expected_len,
                "Bit width mismatch for: {copybook}",
            );
        } else {
            panic!("Expected BinaryInt for: {copybook}");
        }
    }
}

#[test]
fn test_binary_signed_unsigned_edges() {
    let copybook = r"
01 UNSIGNED-BIN PIC 9(5) COMP.
01 SIGNED-BIN PIC S9(5) COMP.
";
    let schema = parse_copybook(copybook).unwrap();

    // Test maximum values for 32-bit binary
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

    // Maximum unsigned 32-bit: 4294967295 (0xFFFFFFFF)
    // Maximum signed 32-bit: 2147483647 (0x7FFFFFFF)
    let test_data = b"\xFF\xFF\xFF\xFF\x7F\xFF\xFF\xFF"; // Max unsigned, max signed
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["UNSIGNED-BIN"], "4294967295");
    assert_eq!(json_record["SIGNED-BIN"], "2147483647");
}

#[test]
fn test_binary_alignment_padding() {
    let copybook = r"
01 RECORD-WITH-ALIGNMENT.
   05 CHAR-FIELD PIC X(1).
   05 BINARY-FIELD PIC 9(5) USAGE COMP SYNCHRONIZED.
   05 ANOTHER-CHAR PIC X(3).
";

    let schema = parse_copybook(copybook).unwrap();

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 3);

    // CHAR-FIELD at offset 0
    let char_field = &root.children[0];
    assert_eq!(char_field.offset, 0);
    assert_eq!(char_field.len, 1);

    // BINARY-FIELD should be aligned to 4-byte boundary
    let binary_field = &root.children[1];
    assert_eq!(binary_field.offset, 4); // Aligned from 1 to 4
    assert_eq!(binary_field.len, 4); // 32-bit = 4 bytes
    assert_eq!(binary_field.sync_padding, Some(3)); // 3 padding bytes
    assert!(binary_field.synchronized);

    // Test decode with alignment padding
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

    // Data: 1 byte char + 3 padding + 4 bytes binary + 3 bytes char
    let test_data = b"A\x00\x00\x00\x00\x00\x01\x00XYZ"; // A + padding + 256 + XYZ
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["CHAR-FIELD"], "A");
    assert_eq!(json_record["BINARY-FIELD"], "256");
    assert_eq!(json_record["ANOTHER-CHAR"], "XYZ");
}

#[test]
fn test_fixed_scale_rendering() {
    // Test NORMATIVE fixed-scale rendering for decimals
    let copybook = r"
01 DECIMAL-FIELDS.
   05 SCALE-2 PIC 9(5)V99 COMP-3.
   05 SCALE-0 PIC 9(5) COMP-3.
   05 SCALE-4 PIC 9(3)V9999 COMP-3.
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
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
    };

    // Test data: 12345.67 (scale 2), 12345 (scale 0), 123.4567 (scale 4)
    // Correct packed representations: 1234567 -> \x12\x34\x56\x7C, 12345 -> \x12\x34\x5C
    let test_data = b"\x12\x34\x56\x7C\x12\x34\x5C\x12\x34\x56\x7C"; // Packed decimals
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should render with exactly the specified scale
    assert_eq!(json_record["SCALE-2"], "12345.67"); // Always 2 decimal places
    assert_eq!(json_record["SCALE-0"], "12345"); // No decimal point for scale 0
    assert_eq!(json_record["SCALE-4"], "123.4567"); // Always 4 decimal places
}

#[test]
fn test_explicit_binary_width() {
    // Test NORMATIVE explicit USAGE BINARY(n) for n ∈ {1,2,4,8}
    let copybook = r"
01 EXPLICIT-BINARY.
   05 BIN1 PIC 9(3) USAGE BINARY(1).
   05 BIN2 PIC 9(5) USAGE BINARY(2).
   05 BIN4 PIC 9(9) USAGE BINARY(4).
   05 BIN8 PIC 9(18) USAGE BINARY(8).
";

    let schema = parse_copybook(copybook).unwrap();

    let root = &schema.fields[0];
    assert_eq!(root.children.len(), 4);

    // Verify explicit widths override digit-based calculation
    assert_eq!(root.children[0].len, 1); // BINARY(1)
    assert_eq!(root.children[1].len, 2); // BINARY(2)
    assert_eq!(root.children[2].len, 4); // BINARY(4)
    assert_eq!(root.children[3].len, 8); // BINARY(8)
}
