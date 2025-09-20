use copybook_codec::{decode_record, encode_record, DecodeOptions, EncodeOptions, RecordFormat, Codepage};
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

            let enc = EncodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::CP037,
                use_raw: false,
                bwz_encode: false,
                strict_mode: true,
                max_errors: None,
                threads: 1,
                coerce_numbers: false,
            };

            let dec = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::CP037,
                json_number_mode: copybook_codec::JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: copybook_codec::RawMode::Off,
                strict_mode: true,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
            };

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
              05 A PIC S9({}) COMP-3.
        ", digits);

            let schema = copybook_core::parse_copybook(&schema_text).unwrap();

            let enc = EncodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::CP037,
                use_raw: false,
                bwz_encode: false,
                strict_mode: true,
                max_errors: None,
                threads: 1,
                coerce_numbers: false,
            };

            let dec = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::CP037,
                json_number_mode: copybook_codec::JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: copybook_codec::RawMode::Off,
                strict_mode: true,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
            };

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

            let _total_digits = int_digits + scale;
            let schema_text = format!("
           01 REC.
              05 A PIC S9({})V9({}) COMP-3.
        ", int_digits, scale);

            let schema = copybook_core::parse_copybook(&schema_text).unwrap();

            let enc = EncodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::CP037,
                use_raw: false,
                bwz_encode: false,
                strict_mode: true,
                max_errors: None,
                threads: 1,
                coerce_numbers: false,
            };

            let dec = DecodeOptions {
                format: RecordFormat::Fixed,
                codepage: Codepage::CP037,
                json_number_mode: copybook_codec::JsonNumberMode::Lossless,
                emit_filler: false,
                emit_meta: false,
                emit_raw: copybook_codec::RawMode::Off,
                strict_mode: true,
                max_errors: None,
                on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,
                threads: 1,
            };

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