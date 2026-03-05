// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for codec bijection.
//!
//! Verifies that decode(encode(json)) == json and encode(decode(binary)) == binary
//! for supported types, and that scratch buffer reuse doesn't affect results.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record,
    decode_record_with_scratch, encode_record, memory::ScratchBuffers,
};
use copybook_core::parse_copybook;
use proptest::prelude::*;
use serde_json::Value;

use super::config::*;

// ---------------------------------------------------------------------------
// decode(encode(json)) == json for alphanumeric
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// For PIC X fields, encode then decode yields the same padded string.
    #[test]
    fn prop_alphanumeric_encode_decode_bijection(
        len in 1usize..=50,
        text in "[A-Za-z0-9 ]{1,50}",
    ) {
        let len = len.max(text.len()).min(50);
        let copybook = format!("01 FIELD PIC X({}).", len);
        let schema = parse_copybook(&copybook).expect("parse");

        // Pad or truncate to field length
        let padded: String = if text.len() >= len {
            text[..len].to_string()
        } else {
            format!("{:<width$}", text, width = len)
        };

        let json_in = serde_json::json!({ "FIELD": padded });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        let out_str = json_out.get("FIELD").and_then(Value::as_str).expect("field exists");
        prop_assert_eq!(out_str, &padded);
    }
}

// ---------------------------------------------------------------------------
// encode(decode(binary)) == binary for unsigned zoned decimal
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// For unsigned zoned decimal, decode then encode yields original bytes.
    #[test]
    fn prop_zoned_decimal_decode_encode_bijection(
        digits in prop::collection::vec(0u8..=9, 1..=18),
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", digits.len());
        let schema = parse_copybook(&copybook).expect("parse");

        let original: Vec<u8> = digits.iter().map(|&d| b'0' + d).collect();

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = decode_record(&schema, &original, &decode_opts).expect("decode");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let roundtrip = encode_record(&schema, &json_val, &encode_opts).expect("encode");

        prop_assert_eq!(roundtrip, original);
    }
}

// ---------------------------------------------------------------------------
// encode(decode(binary)) == binary for EBCDIC alphanumeric
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// For EBCDIC alphanumeric, decode then encode yields original bytes.
    #[test]
    fn prop_ebcdic_alpha_decode_encode_bijection(
        // EBCDIC letters: C1-C9 = A-I, D1-D9 = J-R, E2-E9 = S-Z, 40 = space
        len in 1usize..=30,
    ) {
        let copybook = format!("01 FIELD PIC X({}).", len);
        let schema = parse_copybook(&copybook).expect("parse");

        // Use EBCDIC spaces (0x40) as safe roundtrippable data
        let original = vec![0x40u8; len];

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_val = decode_record(&schema, &original, &decode_opts).expect("decode");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let roundtrip = encode_record(&schema, &json_val, &encode_opts).expect("encode");

        prop_assert_eq!(roundtrip, original);
    }
}

// ---------------------------------------------------------------------------
// Scratch buffer reuse doesn't affect results
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding with scratch buffers yields same result as without.
    #[test]
    fn prop_scratch_buffer_same_as_normal(
        digits in prop::collection::vec(0u8..=9, 1..=9),
    ) {
        let copybook = format!("01 FIELD PIC 9({}).", digits.len());
        let schema = parse_copybook(&copybook).expect("parse");

        let data: Vec<u8> = digits.iter().map(|&d| b'0' + d).collect();
        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let normal = decode_record(&schema, &data, &opts).expect("normal decode");

        let mut scratch = ScratchBuffers::new();
        let with_scratch = decode_record_with_scratch(&schema, &data, &opts, &mut scratch)
            .expect("scratch decode");

        prop_assert_eq!(normal, with_scratch);
    }

    /// Reusing scratch buffers across multiple decodes doesn't contaminate results.
    #[test]
    fn prop_scratch_reuse_no_contamination(
        digits_a in prop::collection::vec(0u8..=9, 1..=9),
        digits_b in prop::collection::vec(0u8..=9, 1..=9),
    ) {
        let len_a = digits_a.len();
        let len_b = digits_b.len();
        let schema_a = parse_copybook(&format!("01 FIELD PIC 9({}).", len_a)).expect("parse a");
        let schema_b = parse_copybook(&format!("01 FIELD PIC 9({}).", len_b)).expect("parse b");

        let data_a: Vec<u8> = digits_a.iter().map(|&d| b'0' + d).collect();
        let data_b: Vec<u8> = digits_b.iter().map(|&d| b'0' + d).collect();

        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        // Decode A without scratch
        let expected_a = decode_record(&schema_a, &data_a, &opts).expect("decode a");
        let expected_b = decode_record(&schema_b, &data_b, &opts).expect("decode b");

        // Decode A then B with same scratch
        let mut scratch = ScratchBuffers::new();
        let result_a = decode_record_with_scratch(&schema_a, &data_a, &opts, &mut scratch)
            .expect("scratch decode a");
        let result_b = decode_record_with_scratch(&schema_b, &data_b, &opts, &mut scratch)
            .expect("scratch decode b");

        prop_assert_eq!(result_a, expected_a);
        prop_assert_eq!(result_b, expected_b);
    }

    /// Encoding output length matches schema record length.
    #[test]
    fn prop_encode_output_matches_schema_length(
        num_fields in 1usize..=5,
        field_len in 1usize..=10,
    ) {
        let mut copybook = "01 RECORD.\n".to_string();
        let mut json_obj = serde_json::Map::new();
        let mut total_len = 0usize;

        for i in 0..num_fields {
            copybook.push_str(&format!("   05 F{i} PIC X({field_len}).\n"));
            let val: String = std::iter::repeat_n('A', field_len).collect();
            json_obj.insert(format!("F{i}"), Value::String(val));
            total_len += field_len;
        }

        let schema = parse_copybook(&copybook).expect("parse");
        let json_data = Value::Object(json_obj);

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_data, &encode_opts).expect("encode");

        prop_assert_eq!(binary.len(), total_len);
    }
}
