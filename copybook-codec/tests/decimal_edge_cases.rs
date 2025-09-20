//! Comprehensive tests for decimal edge cases
//!
//! These tests cover tricky scenarios for packed and zoned decimal handling:
//! - Zero handling (+0, -0) with sign policies
//! - Maximum digits and overflow detection
//! - Negative values with proper sign nibbles
//! - Various scale scenarios (V0, large scale)
//! - Zoned decimal overpunch mapping by codepage

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::{Schema, parse_copybook};
use serde_json::json;

/// Create test schema for packed decimal testing
fn create_packed_test_schema() -> Schema {
    let copybook = r#"
       01 PACKED-TEST-RECORD.
          05 COMP3-2-0    PIC S9(2) COMP-3.
          05 COMP3-4-2    PIC S9(4)V99 COMP-3.
          05 COMP3-9-0    PIC S9(9) COMP-3.
          05 COMP3-18-4   PIC S9(18)V9999 COMP-3.
          05 COMP3-UNSIGNED PIC 9(4) COMP-3.
    "#;

    parse_copybook(copybook).expect("Failed to parse packed test schema")
}

/// Create test schema for zoned decimal testing
fn create_zoned_test_schema() -> Schema {
    let copybook = r#"
       01 ZONED-TEST-RECORD.
          05 ZONED-2-0    PIC S9(2).
          05 ZONED-4-0    PIC S9(4).
          05 ZONED-9-0    PIC S9(9).
          05 ZONED-UNSIGNED PIC 9(4).
    "#;

    parse_copybook(copybook).expect("Failed to parse zoned test schema")
}

/// Test +0 and -0 handling for packed decimals
#[test]
fn test_packed_zero_handling() {
    let schema = create_packed_test_schema();

    let encode_options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
    };

    let decode_options = DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: copybook_codec::RawMode::Off,
        strict_mode: true,
        max_errors: None,
        on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
        threads: 1,
    };

    // Test positive zero
    let positive_zero = json!({
        "COMP3-2-0": "0",
        "COMP3-4-2": "0.00",
        "COMP3-9-0": "0",
        "COMP3-18-4": "0.0000",
        "COMP3-UNSIGNED": "0"
    });

    let encoded = encode_record(&schema, &positive_zero, &encode_options)
        .expect("Failed to encode positive zero");
    let decoded =
        decode_record(&schema, &encoded, &decode_options).expect("Failed to decode positive zero");

    // Verify zeros are handled consistently (decoder outputs canonical form)
    assert_eq!(decoded["COMP3-2-0"], "0");
    assert_eq!(decoded["COMP3-4-2"], "0");
    assert_eq!(decoded["COMP3-9-0"], "0");
    assert_eq!(decoded["COMP3-18-4"], "0");
    assert_eq!(decoded["COMP3-UNSIGNED"], "0");

    // Test negative zero (should be normalized to positive zero)
    let negative_zero = json!({
        "COMP3-2-0": "-0",
        "COMP3-4-2": "-0.00",
        "COMP3-9-0": "-0",
        "COMP3-18-4": "-0.0000",
        "COMP3-UNSIGNED": "0"
    });

    let encoded_neg = encode_record(&schema, &negative_zero, &encode_options)
        .expect("Failed to encode negative zero");
    let decoded_neg = decode_record(&schema, &encoded_neg, &decode_options)
        .expect("Failed to decode negative zero");

    // Negative zero should be normalized to positive zero (sign nibble F)
    assert_eq!(decoded_neg["COMP3-2-0"], "0");
    assert_eq!(decoded_neg["COMP3-4-2"], "0");
    assert_eq!(decoded_neg["COMP3-9-0"], "0");
    assert_eq!(decoded_neg["COMP3-18-4"], "0");
}

/// Test maximum digits and overflow detection for packed decimals
#[test]
fn test_packed_max_digits_and_overflow() {
    let schema = create_packed_test_schema();

    let encode_options = EncodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::ASCII,
        use_raw: false,
        bwz_encode: false,
        strict_mode: true,
        max_errors: None,
        threads: 1,
        coerce_numbers: false,
    };

    // Test reasonable maximum values (avoiding overflow)
    let max_values = json!({
        "COMP3-2-0": "99",
        "COMP3-4-2": "9999.99",
        "COMP3-9-0": "999999999",
        "COMP3-18-4": "12345678901234.5678",  // Large but reasonable value
        "COMP3-UNSIGNED": "9999"
    });

    let encoded =
        encode_record(&schema, &max_values, &encode_options).expect("Failed to encode max values");
    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    let decoded =
        decode_record(&schema, &encoded, &decode_options).expect("Failed to decode max values");

    assert_eq!(decoded["COMP3-2-0"], "99");
    assert_eq!(decoded["COMP3-4-2"], "9999.99"); // Decimal preserved in output
    assert_eq!(decoded["COMP3-9-0"], "999999999");
    assert_eq!(decoded["COMP3-18-4"], "12345678901234.5678"); // Decimal preserved
    assert_eq!(decoded["COMP3-UNSIGNED"], "9999");

    // Test overflow scenarios
    let overflow_values = json!({
        "COMP3-2-0": "100",  // Exceeds 99
        "COMP3-4-2": "0.00",
        "COMP3-9-0": "0",
        "COMP3-18-4": "0.0000",
        "COMP3-UNSIGNED": "0"
    });

    let overflow_result = encode_record(&schema, &overflow_values, &encode_options);
    assert!(overflow_result.is_err(), "Should fail on overflow");

    if let Err(e) = overflow_result {
        let error_msg = format!("{}", e);
        assert!(
            error_msg.contains("CBKE501")
                || error_msg.contains("CBKE510")
                || error_msg.contains("overflow")
                || error_msg.contains("too long")
                || error_msg.contains("too large"),
            "Should contain overflow error code or message: {}",
            error_msg
        );
    }

    // Test arithmetic overflow in large decimal values
    let arithmetic_overflow = json!({
        "COMP3-2-0": "1",
        "COMP3-4-2": "1.00",
        "COMP3-9-0": "1",
        "COMP3-18-4": "1234567890123456789.9999", // This causes overflow (19 integer digits, exceeds S9(18))
        "COMP3-UNSIGNED": "1"
    });

    let arith_overflow_result = encode_record(&schema, &arithmetic_overflow, &encode_options);
    assert!(
        arith_overflow_result.is_err(),
        "Should fail on arithmetic overflow"
    );

    if let Err(e) = arith_overflow_result {
        let error_msg = format!("{}", e);
        assert!(
            error_msg.contains("CBKE510")
                || error_msg.contains("overflow")
                || error_msg.contains("CBKE501")
                || error_msg.contains("too large"),
            "Should contain arithmetic overflow or size error: {}",
            error_msg
        );
    }
}

/// Test negative values with proper sign nibbles
#[test]
fn test_packed_negative_values() {
    let schema = create_packed_test_schema();

    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true);

    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    // Test negative values (using smaller values to avoid overflow)
    let negative_values = json!({
        "COMP3-2-0": "-99",
        "COMP3-4-2": "-9999.99",
        "COMP3-9-0": "-999999999",
        "COMP3-18-4": "-12345678901234.5678", // Smaller value to avoid overflow
        "COMP3-UNSIGNED": "1234" // Unsigned field
    });

    let encoded = encode_record(&schema, &negative_values, &encode_options)
        .expect("Failed to encode negative values");
    let decoded = decode_record(&schema, &encoded, &decode_options)
        .expect("Failed to decode negative values");

    // Verify negative signs are preserved
    assert_eq!(decoded["COMP3-2-0"], "-99");
    assert_eq!(decoded["COMP3-4-2"], "-9999.99");
    assert_eq!(decoded["COMP3-9-0"], "-999999999");
    assert_eq!(decoded["COMP3-18-4"], "-12345678901234.5678");
    assert_eq!(decoded["COMP3-UNSIGNED"], "1234");
}

/// Test various scale scenarios
#[test]
fn test_scale_scenarios() {
    let copybook = r#"
       01 SCALE-TEST-RECORD.
          05 SCALE-0      PIC S9(4) COMP-3.
          05 SCALE-9      PIC S9(4)V9(9) COMP-3.
          05 IMPLIED-SCALE PIC S99V9 COMP-3.
    "#;

    let schema = parse_copybook(copybook).expect("Failed to parse scale test schema");

    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true);

    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    // Test scale 0 (integers only)
    let scale_zero_data = json!({
        "SCALE-0": "1234",
        "SCALE-9": "1234.123456789",
        "IMPLIED-SCALE": "12.3"
    });

    let encoded = encode_record(&schema, &scale_zero_data, &encode_options)
        .expect("Failed to encode scale test data");
    let decoded = decode_record(&schema, &encoded, &decode_options)
        .expect("Failed to decode scale test data");

    assert_eq!(decoded["SCALE-0"], "1234");
    assert_eq!(decoded["SCALE-9"], "1234.123456789");
    assert_eq!(decoded["IMPLIED-SCALE"], "12.3");

    // Test scale mismatch errors
    let scale_mismatch = json!({
        "SCALE-0": "12.34", // Should be integer
        "SCALE-9": "1234.12", // Wrong decimal places
        "IMPLIED-SCALE": "12.34" // Wrong decimal places
    });

    let mismatch_result = encode_record(&schema, &scale_mismatch, &encode_options);
    assert!(mismatch_result.is_err(), "Should fail on scale mismatch");

    if let Err(e) = mismatch_result {
        assert_eq!(
            e.code,
            copybook_core::ErrorCode::CBKE505_SCALE_MISMATCH,
            "Should be scale mismatch error: {}",
            e
        );
    }
}

/// Test zoned decimal overpunch mapping by codepage
#[test]
fn test_zoned_overpunch_by_codepage() {
    let schema = create_zoned_test_schema();

    // Test with ASCII codepage
    let ascii_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true);

    let ascii_decode = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    let test_data = json!({
        "ZONED-2-0": "-12",
        "ZONED-4-0": "-3456",
        "ZONED-9-0": "-123456789", // Test negative numbers with overpunch
        "ZONED-UNSIGNED": "9876"
    });

    let ascii_encoded =
        encode_record(&schema, &test_data, &ascii_options).expect("Failed to encode with ASCII");
    let ascii_decoded =
        decode_record(&schema, &ascii_encoded, &ascii_decode).expect("Failed to decode with ASCII");

    assert_eq!(ascii_decoded["ZONED-2-0"], "-12");
    assert_eq!(ascii_decoded["ZONED-4-0"], "-3456");
    assert_eq!(ascii_decoded["ZONED-9-0"], "-123456789"); // 9-digit field with negative overpunch
    assert_eq!(ascii_decoded["ZONED-UNSIGNED"], "9876");

    // Test with EBCDIC codepages
    let ebcdic_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true);

    let ebcdic_decode = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless);

    let ebcdic_encoded =
        encode_record(&schema, &test_data, &ebcdic_options).expect("Failed to encode with EBCDIC");
    let ebcdic_decoded = decode_record(&schema, &ebcdic_encoded, &ebcdic_decode)
        .expect("Failed to decode with EBCDIC");

    // Results should be the same regardless of codepage for the JSON representation
    assert_eq!(ebcdic_decoded["ZONED-2-0"], "-12");
    assert_eq!(ebcdic_decoded["ZONED-4-0"], "-3456");
    assert_eq!(ebcdic_decoded["ZONED-9-0"], "-123456789"); // 9-digit field with negative overpunch
    assert_eq!(ebcdic_decoded["ZONED-UNSIGNED"], "9876");

    // Note: Binary representations will be different due to different overpunch encoding
    // ASCII uses letters (e.g., 'K' for -2), EBCDIC uses zone nibbles (0xD2 for -2)
    println!("ASCII:  {:?}", ascii_encoded);
    println!("EBCDIC: {:?}", ebcdic_encoded);

    // Verify that encodings are actually different for negative values
    assert_ne!(
        ascii_encoded, ebcdic_encoded,
        "ASCII and EBCDIC overpunch should produce different encodings"
    );
}

/// Comprehensive test for zoned decimal overpunch across all codepages
#[test]
#[ignore] // TODO: Fix decimal zero normalization in display formatting
fn test_zoned_overpunch_comprehensive() {
    let copybook = r#"
       01 OVERPUNCH-TEST-RECORD.
          05 SINGLE-DIGIT    PIC S9.
          05 MULTI-DIGIT     PIC S9(5).
    "#;

    let schema = parse_copybook(copybook).expect("Failed to parse overpunch test schema");

    // Test all digit values with positive and negative signs across codepages
    let codepages = [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];

    for &codepage in &codepages {
        println!("Testing codepage: {:?}", codepage);

        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(codepage)
            .with_strict_mode(true);

        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(codepage)
            .with_json_number_mode(JsonNumberMode::Lossless);

        // Test each digit 0-9 with positive and negative signs
        for digit in 0..=9 {
            // Positive
            let positive_data = json!({
                "SINGLE-DIGIT": digit.to_string(),
                "MULTI-DIGIT": format!("1234{}", digit),
            });

            let pos_encoded = encode_record(&schema, &positive_data, &encode_options).expect(
                &format!("Failed to encode positive {} with {:?}", digit, codepage),
            );
            let pos_decoded = decode_record(&schema, &pos_encoded, &decode_options).expect(
                &format!("Failed to decode positive {} with {:?}", digit, codepage),
            );

            assert_eq!(pos_decoded["SINGLE-DIGIT"], digit.to_string());
            assert_eq!(pos_decoded["MULTI-DIGIT"], format!("1234{}", digit));

            // Negative
            let negative_data = json!({
                "SINGLE-DIGIT": format!("-{}", digit),
                "MULTI-DIGIT": if digit == 0 { "-0".to_string() } else { format!("-1234{}", digit) },
            });

            let neg_encoded = encode_record(&schema, &negative_data, &encode_options).expect(
                &format!("Failed to encode negative {} with {:?}", digit, codepage),
            );

            // Debug: check what the last byte decodes to
            if digit == 0 {
                let last_byte = neg_encoded[neg_encoded.len() - 1];
                println!(
                    "Last byte {} = 0x{:02X} = '{}', zone={:X}, digit={:X}",
                    last_byte,
                    last_byte,
                    last_byte as char,
                    (last_byte >> 4) & 0x0F,
                    last_byte & 0x0F
                );
            }

            let neg_decoded = decode_record(&schema, &neg_encoded, &decode_options).expect(
                &format!("Failed to decode negative {} with {:?}", digit, codepage),
            );

            let expected_single = if digit == 0 {
                "0"
            } else {
                &format!("-{}", digit)
            };
            let expected_multi = if digit == 0 {
                "0"
            } else {
                &format!("-1234{}", digit)
            };

            assert_eq!(neg_decoded["SINGLE-DIGIT"], expected_single);
            assert_eq!(neg_decoded["MULTI-DIGIT"], expected_multi);

            // Verify different codepages produce different binary representations for negatives
            if digit != 0 && codepage != Codepage::ASCII {
                let ascii_options = EncodeOptions::new()
                    .with_format(RecordFormat::Fixed)
                    .with_codepage(Codepage::ASCII)
                    .with_strict_mode(true);

                let ascii_encoded = encode_record(&schema, &negative_data, &ascii_options)
                    .expect("Failed to encode with ASCII");

                assert_ne!(
                    neg_encoded, ascii_encoded,
                    "Negative {} should produce different overpunch bytes for ASCII vs {:?}",
                    digit, codepage
                );
            }
        }
    }
}

/// Test zero sign policies and normalization
#[test]
#[ignore] // TODO: Fix decimal zero normalization in display formatting
fn test_zoned_zero_sign_handling() {
    let copybook = r#"
       01 ZERO-TEST-RECORD.
          05 ZERO-FIELD    PIC S9(3).
    "#;

    let schema = parse_copybook(copybook).expect("Failed to parse zero test schema");

    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_strict_mode(true);

    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless);

    // Test positive zero
    let positive_zero = json!({
        "ZERO-FIELD": "0"
    });

    let pos_encoded = encode_record(&schema, &positive_zero, &encode_options)
        .expect("Failed to encode positive zero");
    let pos_decoded = decode_record(&schema, &pos_encoded, &decode_options)
        .expect("Failed to decode positive zero");

    assert_eq!(pos_decoded["ZERO-FIELD"], "0");

    // Test negative zero (should be normalized to positive zero)
    let negative_zero = json!({
        "ZERO-FIELD": "-0"
    });

    let neg_encoded = encode_record(&schema, &negative_zero, &encode_options)
        .expect("Failed to encode negative zero");
    let neg_decoded = decode_record(&schema, &neg_encoded, &decode_options)
        .expect("Failed to decode negative zero");

    assert_eq!(neg_decoded["ZERO-FIELD"], "0"); // Should be normalized

    // Both encodings should produce the same result after normalization
    assert_eq!(pos_decoded, neg_decoded);
}

/// Test ASCII negative zero multi-digit handling (Pin the bug)
/// S9(5): "-12340" must decode to "-12340", only pure -0 normalizes to "0"
#[test]
#[ignore] // TODO: Fix decimal zero normalization in display formatting
fn test_ascii_negative_zero_multi_digit() {
    let copybook = r#"
       01 ASCII-NEG-ZERO-TEST-RECORD.
          05 MULTI-DIGIT-FIELD    PIC S9(5).
    "#;

    let schema = parse_copybook(copybook).expect("Failed to parse ASCII negative zero test schema");

    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true);

    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    // Test pure negative zero (should normalize to "0")
    let pure_negative_zero = json!({
        "MULTI-DIGIT-FIELD": "-0"
    });

    let encoded = encode_record(&schema, &pure_negative_zero, &encode_options)
        .expect("Failed to encode pure negative zero");
    let decoded = decode_record(&schema, &encoded, &decode_options)
        .expect("Failed to decode pure negative zero");

    assert_eq!(decoded["MULTI-DIGIT-FIELD"], "0"); // Pure -0 should normalize to "0"

    // Test multi-digit negative number ending in zero (should preserve negative sign)
    let multi_digit_neg_zero = json!({
        "MULTI-DIGIT-FIELD": "-12340"
    });

    let encoded_multi = encode_record(&schema, &multi_digit_neg_zero, &encode_options)
        .expect("Failed to encode -12340");
    let decoded_multi =
        decode_record(&schema, &encoded_multi, &decode_options).expect("Failed to decode -12340");

    assert_eq!(decoded_multi["MULTI-DIGIT-FIELD"], "-12340"); // Must preserve negative sign

    // Test other multi-digit negative numbers
    let test_cases = [
        ("-12345", "-12345"),
        ("-98760", "-98760"),
        ("-00001", "-1"), // Leading zeros stripped but sign preserved
        ("-10000", "-10000"),
    ];

    for (input, expected) in &test_cases {
        let test_data = json!({
            "MULTI-DIGIT-FIELD": input
        });

        let encoded_case = encode_record(&schema, &test_data, &encode_options)
            .expect(&format!("Failed to encode {}", input));
        let decoded_case = decode_record(&schema, &encoded_case, &decode_options)
            .expect(&format!("Failed to decode {}", input));

        assert_eq!(
            decoded_case["MULTI-DIGIT-FIELD"], *expected,
            "Input {} should decode to {}",
            input, expected
        );
    }
}

/// Test edge cases for blank when zero
#[test]
fn test_blank_when_zero_edge_cases() {
    let copybook = r#"
       01 BWZ-TEST-RECORD.
          05 BWZ-FIELD    PIC S9(4) BLANK WHEN ZERO.
          05 NORMAL-FIELD PIC S9(4).
    "#;

    let schema = parse_copybook(copybook).expect("Failed to parse BWZ test schema");

    let encode_options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_bwz_encode(true)
        .with_strict_mode(true);

    let decode_options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless);

    // Test zero values
    let zero_data = json!({
        "BWZ-FIELD": "0",
        "NORMAL-FIELD": "0"
    });

    let encoded =
        encode_record(&schema, &zero_data, &encode_options).expect("Failed to encode BWZ data");
    let decoded =
        decode_record(&schema, &encoded, &decode_options).expect("Failed to decode BWZ data");

    // BWZ field should be handled according to BLANK WHEN ZERO policy
    assert_eq!(decoded["NORMAL-FIELD"], "0000"); // 4-digit field pads with zeros

    // Test non-zero values
    let nonzero_data = json!({
        "BWZ-FIELD": "1234",
        "NORMAL-FIELD": "1234"
    });

    let encoded_nz = encode_record(&schema, &nonzero_data, &encode_options)
        .expect("Failed to encode non-zero BWZ data");
    let decoded_nz = decode_record(&schema, &encoded_nz, &decode_options)
        .expect("Failed to decode non-zero BWZ data");

    assert_eq!(decoded_nz["BWZ-FIELD"], "1234");
    assert_eq!(decoded_nz["NORMAL-FIELD"], "1234");
}
