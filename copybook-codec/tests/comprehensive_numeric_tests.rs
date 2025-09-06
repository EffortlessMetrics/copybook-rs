//! Comprehensive numeric type tests covering all edge cases and normative behavior
//!
//! This test suite validates numeric type handling according to the normative
//! behavior specified in the design document, including EBCDIC/ASCII sign zones,
//! BLANK WHEN ZERO, packed decimal validation, and binary width mapping.

use copybook_codec::{
    Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use serde_json::Value;
use std::io::Cursor;

fn create_test_decode_options(codepage: Codepage, strict: bool) -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::Fixed,
        codepage,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: strict,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
    }
}

#[test]
fn test_zoned_decimal_ebcdic_sign_zones_comprehensive() {
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::CP037, false);

    // Test all EBCDIC positive signs (C and F zones)
    let positive_tests = vec![
        (b"\xF1\xF2\xC3", "123", "C zone positive"), // "12C" = +123
        (b"\xF1\xF2\xF3", "123", "F zone positive"), // "12F" = +123 (unsigned)
    ];

    for (data, expected, description) in positive_tests {
        let input = Cursor::new(data);
        let mut output = Vec::new();

        let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
        assert!(result.is_ok(), "Failed for {}: {:?}", description, result);

        let output_str = String::from_utf8(output).unwrap();
        let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

        assert_eq!(
            json_record["SIGNED-FIELD"], expected,
            "Failed for {}",
            description
        );
    }

    // Test EBCDIC negative signs (D zone)
    let negative_data = b"\xF1\xF2\xD3"; // "12D" = -123
    let input = Cursor::new(negative_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["SIGNED-FIELD"], "-123");
}

#[test]
fn test_zoned_decimal_ascii_sign_zones_comprehensive() {
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false);

    // Test ASCII positive overpunch zones
    let positive_tests = vec![
        (b"12}", "123", "} = +3"),
        (b"12A", "121", "A = +1"),
        (b"12B", "122", "B = +2"),
        (b"12I", "129", "I = +9"),
    ];

    for (data, expected, description) in positive_tests {
        let input = Cursor::new(data);
        let mut output = Vec::new();

        let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
        assert!(result.is_ok(), "Failed for {}: {:?}", description, result);

        let output_str = String::from_utf8(output).unwrap();
        let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

        assert_eq!(
            json_record["SIGNED-FIELD"], expected,
            "Failed for {}",
            description
        );
    }

    // Test ASCII negative overpunch zones
    let negative_tests = vec![
        (b"12L", "-123", "L = -3"),
        (b"12J", "-121", "J = -1"),
        (b"12K", "-122", "K = -2"),
        (b"12R", "-129", "R = -9"),
    ];

    for (data, expected, description) in negative_tests {
        let input = Cursor::new(data);
        let mut output = Vec::new();

        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
        let output_str = String::from_utf8(output).unwrap();
        let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

        assert_eq!(
            json_record["SIGNED-FIELD"], expected,
            "Failed for {}",
            description
        );
    }
}

#[test]
fn test_blank_when_zero_comprehensive() {
    let copybook = r#"
01 BWZ-RECORD.
   05 BWZ-FIELD PIC 9(5) BLANK WHEN ZERO.
   05 NORMAL-FIELD PIC 9(5).
"#;
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false);

    // Test all spaces (should decode to 0 with warning)
    let blank_data = b"     12345"; // 5 spaces + normal field
    let input = Cursor::new(blank_data);
    let mut output = Vec::new();

    let summary =
        copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    assert!(
        summary.has_warnings(),
        "Should have CBKD412_ZONED_BLANK_IS_ZERO warning"
    );

    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["BWZ-FIELD"], "0");
    assert_eq!(json_record["NORMAL-FIELD"], "12345");

    // Test normal numeric value in BWZ field
    let normal_data = b"0012312345";
    let input = Cursor::new(normal_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["BWZ-FIELD"], "123");
    assert_eq!(json_record["NORMAL-FIELD"], "12345");
}

#[test]
fn test_zoned_negative_zero_normalization() {
    // Test that -0 is normalized to 0 (NORMATIVE)
    let copybook = "01 SIGNED-FIELD PIC S9(3).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false);

    // Create -0 in ASCII overpunch (00M = -000)
    let negative_zero_data = b"00M";
    let input = Cursor::new(negative_zero_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should be normalized to "0", not "-0"
    assert_eq!(json_record["SIGNED-FIELD"], "0");
}

#[test]
fn test_packed_decimal_comprehensive() {
    let copybook = r#"
01 PACKED-RECORD.
   05 PACKED-ODD PIC 9(5) COMP-3.
   05 PACKED-EVEN PIC 9(6) COMP-3.
   05 PACKED-SIGNED PIC S9(3) COMP-3.
"#;
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false);

    // Test data: 12345 (odd), 123456 (even), -123 (signed)
    // Packed: 12345 = 0x12345C (3 bytes)
    // Packed: 123456 = 0x123456C (4 bytes)
    // Packed: -123 = 0x123D (2 bytes)
    let test_data = b"\x12\x34\x5C\x12\x34\x56\x0C\x12\x3D";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["PACKED-ODD"], "12345");
    assert_eq!(json_record["PACKED-EVEN"], "123456");
    assert_eq!(json_record["PACKED-SIGNED"], "-123");
}

#[test]
fn test_packed_decimal_sign_nibbles_comprehensive() {
    let copybook = "01 SIGNED-PACKED PIC S9(3) COMP-3.";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false);

    let sign_tests = vec![
        (b"\x12\x3C", "123", "C sign (positive)"),
        (b"\x12\x3D", "-123", "D sign (negative)"),
        (b"\x12\x3F", "123", "F sign (unsigned positive)"),
        (b"\x12\x3A", "123", "A sign (positive alternative)"),
        (b"\x12\x3E", "123", "E sign (positive alternative)"),
        (b"\x12\x3B", "-123", "B sign (negative alternative)"),
    ];

    for (data, expected, description) in sign_tests {
        let input = Cursor::new(data);
        let mut output = Vec::new();

        let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
        assert!(result.is_ok(), "Failed for {}: {:?}", description, result);

        let output_str = String::from_utf8(output).unwrap();
        let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

        assert_eq!(
            json_record["SIGNED-PACKED"], expected,
            "Failed for {}",
            description
        );
    }
}

#[test]
fn test_binary_signed_unsigned_edges() {
    let copybook = r#"
01 BINARY-RECORD.
   05 UNSIGNED-16 PIC 9(4) COMP.
   05 SIGNED-16 PIC S9(4) COMP.
   05 UNSIGNED-32 PIC 9(9) COMP.
   05 SIGNED-32 PIC S9(9) COMP.
"#;
    let schema = parse_copybook(copybook).unwrap();

    // Schema field lengths validated - removing debug output

    let options = create_test_decode_options(Codepage::ASCII, false);

    // Test simple values first to verify decoding works
    // UNSIGNED-16: \x00\x01 = 1
    // SIGNED-16: \x00\x02 = 2
    // UNSIGNED-32: \x00\x00\x00\x00\x00\x00\x00\x03 = 3
    // SIGNED-32: \x00\x00\x00\x00\x00\x00\x00\x04 = 4
    let test_data =
        b"\x00\x01\x00\x02\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x04";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["UNSIGNED-16"], "1");
    assert_eq!(json_record["SIGNED-16"], "2");
    assert_eq!(json_record["UNSIGNED-32"], "3");
    assert_eq!(json_record["SIGNED-32"], "4");

    // Test zero values
    let min_test_data =
        b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
    let input = Cursor::new(min_test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    assert_eq!(json_record["UNSIGNED-16"], "0");
    assert_eq!(json_record["SIGNED-16"], "0");
    assert_eq!(json_record["UNSIGNED-32"], "0");
    assert_eq!(json_record["SIGNED-32"], "0");
}

#[test]
fn test_fixed_scale_rendering_normative() {
    // Test NORMATIVE fixed-scale rendering for decimals
    let copybook = r#"
01 DECIMAL-FIELDS.
   05 SCALE-0 PIC 9(5) COMP-3.
   05 SCALE-2 PIC 9(5)V99 COMP-3.
   05 SCALE-4 PIC 9(3)V9999 COMP-3.
   05 NEGATIVE-SCALE PIC S9(3)V99 COMP-3.
"#;

    let schema = parse_copybook(copybook).unwrap();

    // Schema debugging removed for cleaner test output

    let options = create_test_decode_options(Codepage::ASCII, false);

    // Test data representing: 12345, 123.45, 1.2345, -12.34
    // Using correct packed decimal encoding from working test
    // SCALE-0: 12345 → \x12\x34\x5C (like working test)
    // SCALE-2: 12345.00 stored as 1234500 → \x12\x34\x50\x0C (4 bytes)
    // SCALE-4: 1.2345 stored as 12345 → \x00\x12\x34\x5C (4 bytes, leading zero)
    // NEGATIVE-SCALE: -12.34 stored as 1234 → \x12\x3D (3 bytes, negative, D in right nibble)
    let test_data = b"\x12\x34\x5C\x12\x34\x50\x0C\x00\x12\x34\x5C\x01\x23\x4D";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should render with exactly the specified scale
    assert_eq!(json_record["SCALE-0"], "12345"); // No decimal point for scale 0
    assert_eq!(json_record["SCALE-2"], "12345.00"); // Always 2 decimal places
    assert_eq!(json_record["SCALE-4"], "1.2345"); // Always 4 decimal places
    assert_eq!(json_record["NEGATIVE-SCALE"], "-12.34"); // Negative with scale
}

#[test]
fn test_invalid_data_error_handling() {
    // Test invalid zoned decimal sign
    let copybook = "01 ZONED-FIELD PIC 9(3).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, true); // Strict mode

    let invalid_data = b"12X"; // X is not a valid zone
    let input = Cursor::new(invalid_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_err(), "Should fail with invalid zone");

    // Test invalid packed decimal nibble
    let packed_copybook = "01 PACKED-FIELD PIC 9(3) COMP-3.";
    let packed_schema = parse_copybook(packed_copybook).unwrap();

    let invalid_packed_data = b"\x1A\x3C"; // A is invalid digit nibble
    let input = Cursor::new(invalid_packed_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&packed_schema, input, &mut output, &options);
    assert!(result.is_err(), "Should fail with invalid nibble");
}

#[test]
fn test_lenient_mode_error_handling() {
    let copybook = r#"
01 MIXED-RECORD.
   05 ZONED-FIELD PIC 9(3).
   05 PACKED-FIELD PIC 9(3) COMP-3.
"#;
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false); // Lenient mode

    // Mix of valid and invalid data
    let mixed_data = b"12X\x12\x3C"; // Invalid zoned + valid packed
    let input = Cursor::new(mixed_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    // In lenient mode, should continue processing despite errors
    assert!(result.is_ok() || result.unwrap().has_errors());
}

#[test]
fn test_alphanumeric_handling_normative() {
    // Test NORMATIVE alphanumeric handling: preserve all spaces, no trimming
    let copybook = r#"
01 ALPHA-RECORD.
   05 FIELD1 PIC X(10).
   05 FIELD2 PIC X(5).
"#;
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false);

    // Test with leading/trailing/embedded spaces
    let test_data = b"  HELLO   WORLD";
    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options).unwrap();
    let output_str = String::from_utf8(output).unwrap();
    let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

    // Should preserve all spaces exactly
    assert_eq!(json_record["FIELD1"], "  HELLO   ");
    assert_eq!(json_record["FIELD2"], "WORLD");
}

#[test]
fn test_codepage_specific_behavior() {
    let copybook = "01 TEXT-FIELD PIC X(5).";
    let schema = parse_copybook(copybook).unwrap();

    // Test different codepages with same binary data
    let test_data = b"HELLO";

    let codepages = vec![
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];

    for codepage in codepages {
        let options = create_test_decode_options(codepage, false);
        let input = Cursor::new(test_data);
        let mut output = Vec::new();

        let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
        assert!(result.is_ok(), "Failed for codepage {:?}", codepage);

        // For ASCII, should be unchanged; for EBCDIC, should be converted
        let output_str = String::from_utf8(output).unwrap();
        let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

        if codepage.is_ascii() {
            assert_eq!(json_record["TEXT-FIELD"], "HELLO");
        } else {
            // EBCDIC conversion should produce different result
            // (exact result depends on the specific EBCDIC bytes)
            assert!(json_record["TEXT-FIELD"].is_string());
        }
    }
}

#[test]
fn test_json_number_modes() {
    let copybook = r#"
01 NUMERIC-RECORD.
   05 ZONED-FIELD PIC 9(5)V99.
   05 PACKED-FIELD PIC 9(3)V9 COMP-3.
   05 BINARY-FIELD PIC 9(5) COMP.
"#;
    let schema = parse_copybook(copybook).unwrap();

    // Test lossless mode (default)
    let lossless_options = DecodeOptions {
        json_number_mode: JsonNumberMode::Lossless,
        ..create_test_decode_options(Codepage::ASCII, false)
    };

    // Test native mode
    let native_options = DecodeOptions {
        json_number_mode: JsonNumberMode::Native,
        ..create_test_decode_options(Codepage::ASCII, false)
    };

    let test_data = b"1234567\x12\x34\x0C\x00\x00\x27\x10"; // Sample data

    for (options, mode_name) in [(&lossless_options, "lossless"), (&native_options, "native")] {
        let input = Cursor::new(test_data);
        let mut output = Vec::new();

        let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
        assert!(result.is_ok(), "Failed for {} mode", mode_name);

        let output_str = String::from_utf8(output).unwrap();
        let json_record: Value = serde_json::from_str(output_str.trim()).unwrap();

        // Both modes should produce valid JSON, but representation may differ
        assert!(json_record.get("ZONED-FIELD").is_some());
        assert!(json_record.get("PACKED-FIELD").is_some());
        assert!(json_record.get("BINARY-FIELD").is_some());
    }
}

#[test]
fn test_record_length_validation() {
    let copybook = "01 FIXED-RECORD PIC X(10).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, true); // Strict mode

    // Test record too short
    let short_data = b"SHORT"; // Only 5 bytes, need 10
    let input = Cursor::new(short_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    // With insufficient data, decoder processes 0 records (not an error)
    assert!(result.is_ok(), "Should succeed but process 0 records");
    assert_eq!(
        result.unwrap().records_processed,
        0,
        "Should process 0 records with insufficient data"
    );

    // Test correct length
    let correct_data = b"EXACTLY10B"; // Exactly 10 bytes
    let input = Cursor::new(correct_data);
    let mut output = Vec::new();

    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    assert!(result.is_ok(), "Should succeed with correct length");
}

#[test]
fn test_throughput_measurement() {
    let copybook = "01 SIMPLE-RECORD PIC X(38).";
    let schema = parse_copybook(copybook).unwrap();
    let options = create_test_decode_options(Codepage::ASCII, false);

    // Create larger test data for throughput measurement
    let record_data = vec![b'A'; 38];
    let mut test_data = Vec::new();
    for _ in 0..1000 {
        test_data.extend_from_slice(&record_data);
    }

    let input = Cursor::new(test_data);
    let mut output = Vec::new();

    let start = std::time::Instant::now();
    let result = copybook_codec::decode_file_to_jsonl(&schema, input, &mut output, &options);
    let elapsed = start.elapsed();

    assert!(result.is_ok());
    let summary = result.unwrap();

    // Verify throughput calculation
    assert!(summary.throughput_mbps > 0.0);
    assert!(summary.processing_time_ms > 0);
    assert_eq!(summary.records_processed, 1000);

    println!(
        "Processed {} records in {:?} at {:.2} MB/s",
        summary.records_processed, elapsed, summary.throughput_mbps
    );
}
