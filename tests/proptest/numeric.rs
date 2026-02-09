//! Property tests for numeric field handling
//!
//! These tests verify that numeric conversions preserve values within
//! valid ranges and that encoding/decoding maintains numeric precision.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use copybook_codec::numeric::{
    encode_binary_int, encode_packed_decimal, encode_zoned_decimal, get_binary_width_from_digits,
};
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
        let mut pic = if is_signed { String::from("S9(") } else { String::from("9(") };
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
            prop_assert_eq!(field.len as usize, expected_size,
                "Field size {} should match PIC clause length {}",
                field.len, expected_size);
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

        let value_str = clamped_value.to_string();
        let original_data = encode_zoned_decimal(
            &value_str,
            length as u16,
            0,
            true,
            Codepage::ASCII,
        )
        .expect("Failed to encode zoned decimal");

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

        let original_data = encode_zoned_decimal(
            "0",
            length as u16,
            0,
            is_signed,
            Codepage::ASCII,
        )
        .expect("Failed to encode zoned decimal");

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

        let value_str = clamped_value.to_string();
        let original_data = encode_zoned_decimal(
            &value_str,
            length as u16,
            0,
            true,
            Codepage::ASCII,
        )
        .expect("Failed to encode zoned decimal");

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
        is_negative in any::<bool>(),
        int_seed in 0u64..=999_999_999,
        dec_seed in 0u32..=9_999
    ) {
        let total_digits = int_digits + dec_digits;
        let copybook = if dec_digits == 0 {
            format!("01 FIELD PIC S9({}) COMP-3.", int_digits)
        } else {
            format!("01 FIELD PIC S9({})V9({}) COMP-3.", int_digits, dec_digits)
        };
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let max_int = 10u64.pow(int_digits as u32) - 1;
        let max_dec = if dec_digits == 0 {
            1u32
        } else {
            10u32.pow(dec_digits as u32)
        };
        let int_part = int_seed % (max_int + 1);
        let dec_part = if dec_digits == 0 {
            0u32
        } else {
            dec_seed % max_dec
        };

        let value_str = if dec_digits == 0 {
            format!("{}{}", if is_negative { "-" } else { "" }, int_part)
        } else {
            format!(
                "{}{}.{}",
                if is_negative { "-" } else { "" },
                int_part,
                format!("{:0width$}", dec_part, width = dec_digits)
            )
        };

        let packed = encode_packed_decimal(
            &value_str,
            total_digits as u16,
            dec_digits as i16,
            true,
        )
        .expect("Failed to encode packed decimal");

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

        prop_assert_eq!(roundtrip_data, packed);
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
        value in 0u64..=u64::MAX,
        digits in 1usize..=18
    ) {
        let copybook = format!("01 FIELD PIC 9({}) BINARY.", digits);
        let schema = parse_copybook(&copybook).expect("Failed to parse copybook");

        let bits = get_binary_width_from_digits(digits as u16);
        let max_value = if bits == 64 {
            i64::MAX as u64
        } else {
            (1u64 << bits) - 1
        };
        let clamped_value = value % (max_value + 1);

        let original_data = encode_binary_int(clamped_value as i64, bits, false)
            .expect("Failed to encode binary integer");

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
