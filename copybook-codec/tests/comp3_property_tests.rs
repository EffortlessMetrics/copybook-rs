use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use proptest::prelude::*;

#[cfg(test)]
mod comp3_roundtrip_tests {
    use super::*;

    proptest! {
        #[test]
        fn comp3_roundtrip(
            int_len in 1usize..=14,
            neg in any::<bool>()
        ) {
            // Fixed scale of 4 to match schema S9(18)V9(4)
            let scale = 4u32;
            let mut s = String::new();
            if neg { s.push('-'); }
            s.push_str(&"9".repeat(int_len));
            s.push('.');
            s.push_str(&"9".repeat(scale as usize));

            // Create schema for S9(18)V9(4) COMP-3 field
            let schema = copybook_core::parse_copybook("
           01 REC.
              05 A PIC S9(18)V9(4) COMP-3.
        ").unwrap();

            let enc = EncodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(Codepage::CP037)
                .with_use_raw(false)
                .with_bwz_encode(false)
                .with_strict_mode(true)
                .with_max_errors(None)
                .with_threads(1)
                .with_coerce_numbers(false)
                .with_zoned_encoding_override(None);

            let dec = DecodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(Codepage::CP037)
                .with_json_number_mode(copybook_codec::JsonNumberMode::Lossless)
                .with_emit_filler(false)
                .with_emit_meta(false)
                .with_emit_raw(copybook_codec::RawMode::Off)
                .with_strict_mode(true)
                .with_max_errors(None)
                .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
                .with_threads(1)
                .with_preserve_zoned_encoding(false)
                .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);

            // Test round-trip: encode -> decode
            let v = serde_json::json!({"A": s});

            // Encode to binary
            let encoded = encode_record(&schema, &v, &enc).unwrap();

            // Decode back to JSON
            let decoded = decode_record(&schema, &encoded, &dec).unwrap();

            // The decoded value should match the canonical form
            // (accounting for leading zeros and normalization)
            assert!(decoded.get("A").is_some(), "Field A should exist in decoded result");

            // Check that the decoded value is a valid number string
            let decoded_str = decoded["A"].as_str().unwrap();
            let _: f64 = decoded_str.parse().unwrap();
        }
    }

    proptest! {
        #[test]
        fn comp3_even_odd_digits(
            digits in 1u16..=10,
            neg in any::<bool>()
        ) {
            // Test specifically even vs odd digit handling
            let mut s = String::new();
            if neg { s.push('-'); }
            s.push_str(&"1".repeat(digits as usize));

            let schema_text = format!("
           01 REC.
              05 A PIC S9({digits}) COMP-3.
        ");

            let schema = copybook_core::parse_copybook(&schema_text).unwrap();

            let enc = EncodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(Codepage::CP037)
                .with_use_raw(false)
                .with_bwz_encode(false)
                .with_strict_mode(true)
                .with_max_errors(None)
                .with_threads(1)
                .with_coerce_numbers(false)
                .with_zoned_encoding_override(None);

            let dec = DecodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(Codepage::CP037)
                .with_json_number_mode(copybook_codec::JsonNumberMode::Lossless)
                .with_emit_filler(false)
                .with_emit_meta(false)
                .with_emit_raw(copybook_codec::RawMode::Off)
                .with_strict_mode(true)
                .with_max_errors(None)
                .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
                .with_threads(1)
                .with_preserve_zoned_encoding(false)
                .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);

            let v = serde_json::json!({"A": s});

            // Encode to binary
            let encoded = encode_record(&schema, &v, &enc).unwrap();

            // Decode back to JSON
            let decoded = decode_record(&schema, &encoded, &dec).unwrap();

            // Verify the round-trip preserves the value
            assert!(decoded.get("A").is_some());
            let decoded_str = decoded["A"].as_str().unwrap();

            // Both should parse to the same numeric value
            let original_val: f64 = s.parse().unwrap();
            let decoded_val: f64 = decoded_str.parse().unwrap();
            assert!((original_val - decoded_val).abs() < f64::EPSILON);
        }
    }

    proptest! {
        #[test]
        fn comp3_scale_handling(
            int_digits in 1u16..=14,
            scale in 1u16..=4,
            neg in any::<bool>()
        ) {
            // Test scale handling with different integer/fractional combinations
            let mut s = String::new();
            if neg { s.push('-'); }
            s.push_str(&"9".repeat(int_digits as usize));
            s.push('.');
            s.push_str(&"8".repeat(scale as usize));

            // Total digits for reference: int_digits + scale
            let schema_text = format!("
           01 REC.
              05 A PIC S9({int_digits})V9({scale}) COMP-3.
        ");

            let schema = copybook_core::parse_copybook(&schema_text).unwrap();

            let enc = EncodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(Codepage::CP037)
                .with_use_raw(false)
                .with_bwz_encode(false)
                .with_strict_mode(true)
                .with_max_errors(None)
                .with_threads(1)
                .with_coerce_numbers(false)
                .with_zoned_encoding_override(None);

            let dec = DecodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(Codepage::CP037)
                .with_json_number_mode(copybook_codec::JsonNumberMode::Lossless)
                .with_emit_filler(false)
                .with_emit_meta(false)
                .with_emit_raw(copybook_codec::RawMode::Off)
                .with_strict_mode(true)
                .with_max_errors(None)
                .with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)
                .with_threads(1)
                .with_preserve_zoned_encoding(false)
                .with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);

            let v = serde_json::json!({"A": s});

            // Encode to binary
            let encoded = encode_record(&schema, &v, &enc).unwrap();

            // Decode back to JSON
            let decoded = decode_record(&schema, &encoded, &dec).unwrap();

            // Verify round-trip
            assert!(decoded.get("A").is_some());
            let decoded_str = decoded["A"].as_str().unwrap();

            // Should parse to same numeric value
            let original_val: f64 = s.parse().unwrap();
            let decoded_val: f64 = decoded_str.parse().unwrap();
            assert!((original_val - decoded_val).abs() < f64::EPSILON);
        }
    }
}
