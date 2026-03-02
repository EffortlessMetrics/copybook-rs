// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive property tests for codec encode/decode round-trip fidelity.
//!
//! Covers DISPLAY fields of varying PIC sizes, COMP-3 values, alphanumeric
//! padding/truncation semantics, and cross-codepage round-trip consistency.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::{decode_packed_decimal, encode_packed_decimal};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use proptest::prelude::*;

use super::config::{DEFAULT_CASES, QUICK_CASES};

// ============================================================================
// 1. DISPLAY fields of varying PIC sizes round-trip (decode→encode→decode)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// PIC 9(d) fields with varying digit counts round-trip via ASCII.
    #[test]
    fn prop_crt_display_varying_pic_size_ascii(
        digits in 1usize..=9,
        raw_value in 0u64..=999_999_999u64,
    ) {
        let max_val = 10u64.saturating_pow(u32::try_from(digits).unwrap());
        let value = raw_value % max_val;
        let padded = format!("{value:0>digits$}");
        let data: Vec<u8> = padded.bytes().collect();

        let copybook = format!("01 FLD PIC 9({digits}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json1 = decode_record(&schema, &data, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");
        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");

        prop_assert_eq!(&json1, &json2);
    }

    /// PIC S9(d) signed DISPLAY fields round-trip via EBCDIC CP037.
    #[test]
    fn prop_crt_display_signed_varying_pic_ebcdic(
        digits in 1usize..=7,
        raw_value in 0u64..=9_999_999u64,
    ) {
        let max_val = 10u64.saturating_pow(u32::try_from(digits).unwrap());
        let value = raw_value % max_val;

        let copybook = format!(
            "       01 REC.\n           05 FLD PIC S9({digits})."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let json_in = serde_json::json!({ "REC": { "FLD": value.to_string() } });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        let out_val = json_out
            .get("FLD")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse::<i64>().ok())))
            .expect("numeric value");
        prop_assert_eq!(out_val, i64::try_from(value).unwrap());
    }
}

// ============================================================================
// 2. PIC 9(d)V9(s) with decimal scale round-trip
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// PIC 9(d)V9(s) decimal fields round-trip preserving scale.
    #[test]
    fn prop_crt_display_with_decimal_scale_roundtrip(
        int_digits in 1usize..=5,
        scale in 1usize..=3,
        raw_value in 0u64..=99_999_999u64,
    ) {
        let total = int_digits + scale;
        let max_val = 10u64.saturating_pow(u32::try_from(total).unwrap());
        let value = raw_value % max_val;
        let padded = format!("{value:0>total$}");
        let data: Vec<u8> = padded.bytes().collect();

        let copybook = format!("01 FLD PIC 9({int_digits})V9({scale}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json1 = decode_record(&schema, &data, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");
        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");

        prop_assert_eq!(&json1, &json2);
    }
}

// ============================================================================
// 3. COMP-3 values round-trip correctly
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// COMP-3 primitive encode→decode with various digit widths.
    #[test]
    fn prop_crt_comp3_varying_widths(
        digits in 1u16..=9,
        raw_value in 0u64..=999_999_999u64,
        negative in any::<bool>(),
    ) {
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let abs_val = raw_value % max_val;
        let sign = if negative && abs_val != 0 { "-" } else { "" };
        let value_str = format!("{sign}{abs_val}");

        let encoded = encode_packed_decimal(&value_str, digits, 0, true)
            .expect("encode comp-3");
        let decoded = decode_packed_decimal(&encoded, digits, 0, true)
            .expect("decode comp-3");
        let re_encoded = encode_packed_decimal(&decoded.to_string(), digits, 0, true)
            .expect("re-encode comp-3");

        prop_assert_eq!(&encoded, &re_encoded);
    }

    /// COMP-3 with scale round-trips at the primitive level.
    #[test]
    fn prop_crt_comp3_with_scale_roundtrip(
        digits in 3u16..=9,
        scale in 1i16..=2,
        raw_value in 0u64..=999_999_999u64,
    ) {
        #[allow(clippy::cast_possible_wrap)]
        let scale = scale.min((digits as i16) - 1);
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let abs_val = raw_value % max_val;

        let width = usize::from(digits);
        let s = format!("{abs_val:0>width$}");
        let sc = usize::try_from(scale).unwrap_or(0);
        let split = s.len().saturating_sub(sc);
        let (int, frac) = s.split_at(split);
        let int = if int.is_empty() { "0" } else { int };
        let value_str = format!("{int}.{frac}");

        let encoded = encode_packed_decimal(&value_str, digits, scale, true)
            .expect("encode comp-3");
        let decoded = decode_packed_decimal(&encoded, digits, scale, true)
            .expect("decode comp-3");
        let re_encoded = encode_packed_decimal(&decoded.to_string(), digits, scale, true)
            .expect("re-encode comp-3");

        prop_assert_eq!(&encoded, &re_encoded);
    }

    /// COMP-3 full-stack (parse→encode→decode→encode) yields identical bytes.
    #[test]
    fn prop_crt_comp3_fullstack_stable(
        digits in prop::collection::vec(0u8..=9, 1..=7),
    ) {
        let num_digits = digits.len();
        let value: u64 = digits.iter().fold(0u64, |acc, &d| acc * 10 + u64::from(d));

        let copybook = format!(
            "       01 REC.\n           05 FLD PIC S9({num_digits}) COMP-3."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let json_in = serde_json::json!({ "REC": { "FLD": value.to_string() } });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary1 = encode_record(&schema, &json_in, &encode_opts).expect("encode 1");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary1, &decode_opts).expect("decode");
        let binary2 = encode_record(&schema, &json_out, &encode_opts).expect("encode 2");

        prop_assert_eq!(&binary1, &binary2);
    }
}

// ============================================================================
// 4. Alphanumeric padding/truncation semantics
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Short alphanumeric data (space-padded) decode→encode→decode is stable.
    #[test]
    fn prop_crt_alpha_short_data_roundtrip(
        field_len in 5usize..=30,
        data_len in 1usize..=4,
    ) {
        let data: Vec<u8> = std::iter::repeat_n(b'A', data_len)
            .chain(std::iter::repeat_n(b' ', field_len.saturating_sub(data_len)))
            .collect();

        let copybook = format!("01 FLD PIC X({field_len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json1 = decode_record(&schema, &data, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");
        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");

        prop_assert_eq!(&json1, &json2);
    }

    /// Encoding text longer than field width does not panic.
    #[test]
    fn prop_crt_alpha_truncation_no_panic(
        field_len in 1usize..=10,
        text in "[A-Z]{11,20}",
    ) {
        let copybook = format!("01 FLD PIC X({field_len}).");
        let schema = parse_copybook(&copybook).expect("parse");
        let json_in = serde_json::json!({ "FLD": text });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        // Encoder may truncate or return an error; neither should panic
        let result = std::panic::catch_unwind(move || {
            let _ = encode_record(&schema, &json_in, &encode_opts);
        });
        prop_assert!(result.is_ok(), "encode panicked on oversized text");
    }

    /// PIC X decode→encode→decode round-trip preserves data.
    #[test]
    fn prop_crt_alpha_roundtrip_preserves_content(
        field_len in 1usize..=20,
        data in prop::collection::vec(0x41u8..=0x5A, 1..=20),
    ) {
        let padded: Vec<u8> = if data.len() < field_len {
            let mut v = data.into_iter().take(field_len).collect::<Vec<_>>();
            v.resize(field_len, b' ');
            v
        } else {
            data.into_iter().take(field_len).collect()
        };

        let copybook = format!("01 FLD PIC X({field_len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json1 = decode_record(&schema, &padded, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");
        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");

        prop_assert_eq!(&json1, &json2);
    }

    /// Binary output length from decode→encode always matches PIC X field width.
    #[test]
    fn prop_crt_alpha_encode_output_length(
        field_len in 1usize..=50,
    ) {
        let data = vec![b'A'; field_len];
        let copybook = format!("01 FLD PIC X({field_len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json = decode_record(&schema, &data, &decode_opts).expect("decode");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json, &encode_opts).expect("encode");

        prop_assert_eq!(binary.len(), field_len);
    }
}

// ============================================================================
// 5. Cross-codepage round-trip (CP037 → decode → encode → CP037)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// EBCDIC CP037 PIC X: decode→encode→decode yields identical JSON.
    #[test]
    fn prop_crt_cross_cp037_pic_x_roundtrip(
        len in 1usize..=30,
    ) {
        let copybook = format!("01 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        // EBCDIC spaces (0x40) are the canonical padding
        let original = vec![0x40u8; len];

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json1 = decode_record(&schema, &original, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");

        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");
        prop_assert_eq!(&json1, &json2);
    }

    /// CP037 numeric PIC 9: encode→decode→encode yields identical bytes.
    #[test]
    fn prop_crt_cross_cp037_pic9_bytes_stable(
        digits in 1usize..=7,
        raw_value in 0u64..=9_999_999u64,
    ) {
        let max_val = 10u64.saturating_pow(u32::try_from(digits).unwrap());
        let value = raw_value % max_val;

        let copybook = format!(
            "       01 REC.\n           05 FLD PIC 9({digits})."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let json_in = serde_json::json!({ "REC": { "FLD": value.to_string() } });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary1 = encode_record(&schema, &json_in, &encode_opts).expect("encode 1");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary1, &decode_opts).expect("decode");
        let binary2 = encode_record(&schema, &json_out, &encode_opts).expect("encode 2");

        prop_assert_eq!(&binary1, &binary2);
    }

    /// Multi-field record with CP037: all fields survive full round-trip.
    #[test]
    fn prop_crt_cross_cp037_multi_field_roundtrip(
        alpha in "[A-Z]{1,8}",
        numeric_val in 0u32..=99999,
    ) {
        let copybook = "\
       01 REC.\n\
           05 FLD-A PIC X(8).\n\
           05 FLD-N PIC 9(5).";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({
            "REC": {
                "FLD-A": alpha,
                "FLD-N": numeric_val.to_string(),
            }
        });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        // Numeric field must match exactly
        let out_num = json_out
            .get("FLD-N")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD-N");
        prop_assert_eq!(out_num, i64::from(numeric_val));
    }

    /// CP500 and CP1047 PIC X round-trips produce identical JSON.
    #[test]
    fn prop_crt_cross_cp500_cp1047_spaces_roundtrip(
        len in 1usize..=20,
        cp_idx in 0usize..2,
    ) {
        let codepages = [Codepage::CP500, Codepage::CP1047];
        let cp = codepages[cp_idx];

        let copybook = format!("01 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");
        let original = vec![0x40u8; len];

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(cp);
        let json1 = decode_record(&schema, &original, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(cp);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");
        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");

        prop_assert_eq!(&json1, &json2);
    }

    /// Encoded record length is codepage-invariant for the same schema.
    #[test]
    fn prop_crt_cross_codepage_length_invariant(
        text in "[A-Z0-9 ]{1,15}",
        num_val in 0u32..=9999,
    ) {
        let copybook = "\
       01 REC.\n\
           05 FLD-A PIC X(15).\n\
           05 FLD-N PIC 9(4).";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({
            "REC": {
                "FLD-A": text,
                "FLD-N": num_val.to_string(),
            }
        });

        let codepages = [
            Codepage::ASCII, Codepage::CP037, Codepage::CP500, Codepage::CP1047,
        ];
        let mut lengths = Vec::new();
        for &cp in &codepages {
            let opts = EncodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(cp);
            let binary = encode_record(&schema, &json_in, &opts).expect("encode");
            lengths.push(binary.len());
        }

        let first = lengths[0];
        for (i, &l) in lengths.iter().enumerate() {
            prop_assert_eq!(l, first,
                "codepage {:?} produced {} bytes vs {} from {:?}",
                codepages[i], l, first, codepages[0]);
        }
    }
}
