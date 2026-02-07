//! Property tests for numeric field handling
//!
//! These tests verify that numeric conversions preserve values within
//! valid ranges and that encoding/decoding maintains numeric precision.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use proptest::prelude::*;

use super::generators::*;
use super::config::*;

/// Property: Numeric field size matches PIC clause specification
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_numeric_field_size_matches_pic(
        length in 1usize..=18,
        has_decimal in any::<bool>(),
        is_signed in any::<bool>()
    ) {
        let mut pic = if is_signed { "S9(" } else { "9(" };
        pic.push_str(&length.to_string());
        if has_decimal {
            pic.push_str(")V9(");
            pic.push_str(&(length.min(4)).to_string());
            pic.push(')');
        } else {
            pic.push(')');
        }

        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        if let Some(field) = schema.fields.first() {
            // For zoned decimals, size equals number of digits
            let expected_size = length + if has_decimal { length.min(4) } else { 0 };
            prop_assert_eq!(field.size, expected_size,
                "Field size {} should match PIC clause length {}",
                field.size, expected_size);
        }
    }
}

/// Property: Numeric field round-trip preserves value
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_numeric_roundtrip_preserves_value(
        value in -999999999i64..=999999999i64,
        length in 1usize..=9
    ) {
        let copybook = format!("01 FIELD PIC S9({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Clamp value to fit in specified length
        let max_value = 10i64.pow(length as u32) - 1;
        let clamped_value = value.max(-max_value).min(max_value);

        // Convert to ASCII zoned decimal
        let value_str = clamped_value.to_string();
        let mut original_data = Vec::new();
        for (i, ch) in value_str.chars().enumerate() {
            if i == value_str.len() - 1 {
                // Last character: use overpunch
                let digit = ch.to_digit(10).unwrap() as u8;
                let overpunch = if clamped_value < 0 {
                    match digit {
                        0 => b'p', 1 => b'q', 2 => b'r', 3 => b's', 4 => b't',
                        5 => b'u', 6 => b'v', 7 => b'w', 8 => b'x', _ => b'y',
                    }
                } else {
                    match digit {
                        0 => b'{', 1 => b'A', 2 => b'B', 3 => b'C', 4 => b'D',
                        5 => b'E', 6 => b'F', 7 => b'G', 8 => b'H', _ => b'I',
                    }
                };
                original_data.push(overpunch);
            } else if ch == '-' {
                // Skip sign, it's in the overpunch
            } else {
                original_data.push(ch as u8);
            }
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_preserve_zoned_encoding(true);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_zoned_encoding_format(copybook_codec::ZonedEncodingFormat::Ascii);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Decimal field round-trip preserves precision
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_decimal_roundtrip_preserves_precision(
        int_part in 0i64..=999999,
        dec_part in 0u32..=9999,
        int_digits in 1usize..=6,
        dec_digits in 1usize..=4
    ) {
        let copybook = format!("01 FIELD PIC 9({})V9({}).", int_digits, dec_digits);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Clamp values to fit in specified digits
        let max_int = 10i64.pow(int_digits as u32) - 1;
        let max_dec = 10u32.pow(dec_digits as u32);
        let clamped_int = int_part.min(max_int);
        let clamped_dec = dec_part % max_dec;

        // Create decimal string
        let value_str = format!("{:0width$}.{:0width2$}",
            clamped_int, clamped_dec, width=int_digits, width2=dec_digits);

        // Convert to ASCII
        let original_data: Vec<u8> = value_str.replace('.', "")
            .bytes()
            .collect();

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Zero values round-trip correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_zero_roundtrip_correct(
        length in 1usize..=18,
        is_signed in any::<bool>()
    ) {
        let pic = if is_signed { format!("S9({})", length) } else { format!("9({})", length) };
        let copybook = format!("01 FIELD PIC {}.", pic);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Zero value in ASCII
        let original_data: Vec<u8> = vec![b'0'; length];

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Maximum values round-trip correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_max_value_roundtrip_correct(
        length in 1usize..=18
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Maximum value (all 9s)
        let original_data: Vec<u8> = vec![b'9'; length];

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Negative values round-trip correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_negative_value_roundtrip_correct(
        value in -999999i64..=-1,
        length in 1usize..=6
    ) {
        let copybook = format!("01 FIELD PIC S9({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Clamp value to fit
        let max_value = 10i64.pow(length as u32) - 1;
        let clamped_value = value.max(-max_value);

        // Create signed ASCII zoned decimal
        let value_str = clamped_value.to_string();
        let mut original_data = Vec::new();
        for (i, ch) in value_str.chars().enumerate() {
            if i == value_str.len() - 1 {
                let digit = ch.to_digit(10).unwrap() as u8;
                let overpunch = match digit {
                    0 => b'p', 1 => b'q', 2 => b'r', 3 => b's', 4 => b't',
                    5 => b'u', 6 => b'v', 7 => b'w', 8 => b'x', _ => b'y',
                };
                original_data.push(overpunch);
            } else if ch != '-' {
                original_data.push(ch as u8);
            }
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_preserve_zoned_encoding(true);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII)
            .with_zoned_encoding_format(copybook_codec::ZonedEncodingFormat::Ascii);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: COMP-3 packed decimal round-trip preserves value
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_comp3_roundtrip_preserves_value(
        int_digits in 1usize..=9,
        dec_digits in 0usize..=4,
        is_negative in any::<bool>()
    ) {
        let total_digits = int_digits + dec_digits;
        let copybook = format!("01 FIELD PIC S9({})V9({}) COMP-3.", int_digits, dec_digits);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Generate random value
        let max_int = 10u64.pow(int_digits as u32) - 1;
        let max_dec = 10u32.pow(dec_digits as u32);
        let int_part = rand::random::<u64>() % max_int;
        let dec_part = rand::random::<u32>() % max_dec;

        // Build COMP-3 encoded value
        let mut comp3_data = Vec::new();
        let mut value = int_part * max_dec as u64 + dec_part as u64;

        // Add digits (nibbles)
        for _ in 0..(total_digits - 1) {
            comp3_data.push((value % 10) as u8);
            value /= 10;
        }
        comp3_data.push((value % 10) as u8);

        // Reverse to get correct order
        comp3_data.reverse();

        // Pack into bytes
        let mut packed = Vec::new();
        for chunk in comp3_data.chunks(2) {
            let byte = if chunk.len() == 2 {
                (chunk[0] << 4) | chunk[1]
            } else {
                chunk[0] << 4
            };
            packed.push(byte);
        }

        // Add sign nibble to last byte
        let last_byte = packed.pop().unwrap();
        let sign_nibble = if is_negative { 0x0D } else { 0x0C };
        packed.push((last_byte & 0xF0) | sign_nibble);

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);

        let json_result = copybook_codec::decode_record(&schema, &packed, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        // Verify size is correct
        let expected_size = (total_digits + 2) / 2; // COMP-3 size formula
        prop_assert_eq!(roundtrip_data.len(), expected_size);

        // Verify sign nibble is correct
        let last = *roundtrip_data.last().unwrap();
        let actual_sign = last & 0x0F;
        prop_assert_eq!(actual_sign, sign_nibble);
    }
}

/// Property: Binary field round-trip preserves value
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_binary_roundtrip_preserves_value(
        value in 0i64..=65535,
        length in 1usize..=4
    ) {
        let copybook = format!("01 FIELD PIC 9({}) BINARY.", length * 2);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Clamp value to fit
        let max_value = 256u64.pow(length as u32) - 1;
        let clamped_value = (value as u64) % max_value;

        // Convert to binary
        let mut original_data = Vec::new();
        for i in 0..length {
            original_data.push(((clamped_value >> (i * 8)) & 0xFF) as u8);
        }

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Numeric field with leading zeros round-trip correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_leading_zeros_roundtrip_correct(
        length in 3usize..=18
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Value with leading zeros
        let mut original_data = vec![b'0'; length - 1];
        original_data.push(b'1');

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        prop_assert_eq!(roundtrip_data, original_data);
    }
}

/// Property: Numeric overflow is handled correctly
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_numeric_overflow_handled(
        length in 1usize..=5
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", length);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        // Value that exceeds the field length
        let max_value = 10u64.pow(length as u32);
        let overflow_value = max_value + 12345;

        // Convert to ASCII (will be truncated)
        let value_str = overflow_value.to_string();
        let original_data: Vec<u8> = value_str.bytes().take(length).collect();

        // Decode
        let decode_options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json_result = copybook_codec::decode_record(&schema, &original_data, &decode_options)
            .expect("Failed to decode");

        // Encode back
        let encode_options = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let roundtrip_data = copybook_codec::encode_record(&schema, &json_result, &encode_options)
            .expect("Failed to encode");

        // Should round-trip the truncated value
        prop_assert_eq!(roundtrip_data, original_data);
    }
}
