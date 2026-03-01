// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for encode/decode round-trip across all major field types.
//!
//! Verifies that decode→encode→decode (or encode→decode→encode) yields
//! identical results for PIC X, PIC 9, COMP-3, COMP/BINARY, and
//! mixed-field records. Also checks that decoded output is always valid JSON.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::{
    decode_binary_int, decode_packed_decimal, encode_binary_int, encode_packed_decimal,
};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use proptest::prelude::*;
use serde_json::Value;

use super::config::*;

// ---------------------------------------------------------------------------
// PIC X round-trip: decode→encode→decode for various lengths
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// PIC X fields of random length (1-100) round-trip through decode→encode→decode.
    #[test]
    fn prop_pic_x_roundtrip_various_lengths(
        len in 1usize..=100,
        data in prop::collection::vec(0x20u8..=0x7E, 1..=100),
    ) {
        let data: Vec<u8> = data.into_iter().take(len).collect();
        let padded: Vec<u8> = if data.len() < len {
            let mut v = data.clone();
            v.resize(len, b' ');
            v
        } else {
            data
        };

        let copybook = format!("01 FLD PIC X({len}).");
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
}

// ---------------------------------------------------------------------------
// PIC 9 round-trip: unsigned zoned with various decimal scales
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// PIC 9(d)V9(s) round-trips through decode→encode→decode.
    #[test]
    fn prop_pic9_with_scale_roundtrip(
        int_digits in 1usize..=9,
        scale in 0usize..=4,
        digits in prop::collection::vec(0u8..=9, 1..=13),
    ) {
        let total = int_digits + scale;
        let total = total.min(13);
        let digits: Vec<u8> = digits.into_iter().take(total).collect();
        if digits.is_empty() {
            return Ok(());
        }
        let total = digits.len();

        let copybook = if scale > 0 && scale < total {
            let int_part = total - scale;
            format!("01 FLD PIC 9({int_part})V9({scale}).")
        } else {
            format!("01 FLD PIC 9({total}).")
        };

        let schema = parse_copybook(&copybook).expect("parse");
        let original: Vec<u8> = digits.iter().map(|&d| b'0' + d).collect();

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json1 = decode_record(&schema, &original, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");

        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");
        prop_assert_eq!(&json1, &json2);
    }
}

// ---------------------------------------------------------------------------
// COMP-3 round-trip at the primitive level
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Packed decimal encode→decode yields the same value.
    #[test]
    fn prop_comp3_primitive_roundtrip(
        digits in 1u16..=9,
        scale in 0i16..=2,
        abs_value in 0u64..=999_999_999u64,
        negative in any::<bool>(),
    ) {
        // Clamp scale so there is at least 1 integer digit
        #[allow(clippy::cast_possible_wrap)]
        let scale = scale.min((digits as i16) - 1).max(0);
        #[allow(clippy::cast_possible_truncation)]
        let max_val = 10u64.pow(u32::from(digits));
        let abs_value = abs_value % max_val;
        let sign = if negative && abs_value != 0 { "-" } else { "" };
        let value_str = if scale > 0 {
            let width = usize::from(digits);
            let s = format!("{abs_value:0>width$}");
            let sc = usize::try_from(scale).unwrap_or(0);
            let split = s.len().saturating_sub(sc);
            let (int, frac) = s.split_at(split);
            let int = if int.is_empty() { "0" } else { int };
            format!("{sign}{int}.{frac}")
        } else {
            format!("{sign}{abs_value}")
        };

        let encoded = encode_packed_decimal(&value_str, digits, scale, true)
            .expect("encode");
        let decoded = decode_packed_decimal(&encoded, digits, scale, true)
            .expect("decode");

        // Re-encode from decoded to check stability
        let re_encoded = encode_packed_decimal(&decoded.to_string(), digits, scale, true)
            .expect("re-encode");

        prop_assert_eq!(&encoded, &re_encoded);
    }
}

// ---------------------------------------------------------------------------
// COMP-3 full-stack round-trip (parse → decode → encode → decode)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// COMP-3 field round-trips through full codec pipeline.
    #[test]
    fn prop_comp3_fullstack_roundtrip(
        digits in prop::collection::vec(0u8..=9, 1..=7),
    ) {
        let num_digits = digits.len();
        let copybook = format!("01 FLD PIC S9({num_digits}) COMP-3.");
        let schema = parse_copybook(&copybook).expect("parse");

        // Build valid packed decimal: digits + sign nibble (0x0C = positive)
        let expected_bytes = (num_digits + 1).div_ceil(2);
        let mut packed = vec![0u8; expected_bytes];
        // Even digit counts need a leading padding nibble (0)
        let nibble_offset = if num_digits % 2 == 0 { 1 } else { 0 };
        for (i, &d) in digits.iter().enumerate() {
            let ni = i + nibble_offset;
            let byte_idx = ni / 2;
            if ni % 2 == 0 {
                packed[byte_idx] |= d << 4;
            } else {
                packed[byte_idx] |= d;
            }
        }
        // Sign nibble: 0x0C (positive) in the last nibble
        let last_byte_idx = expected_bytes - 1;
        packed[last_byte_idx] |= 0x0C;

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json1 = decode_record(&schema, &packed, &decode_opts).expect("decode 1");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json1, &encode_opts).expect("encode");

        let json2 = decode_record(&schema, &binary, &decode_opts).expect("decode 2");
        prop_assert_eq!(&json1, &json2);
    }
}

// ---------------------------------------------------------------------------
// COMP/BINARY primitive round-trip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Binary integer (16-bit signed) encode→decode round-trip.
    #[test]
    fn prop_binary_int_16_signed_roundtrip(value in -32768i64..=32767) {
        let encoded = encode_binary_int(value, 16, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 16, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Binary integer (32-bit signed) encode→decode round-trip.
    #[test]
    fn prop_binary_int_32_signed_roundtrip(value in i64::from(i32::MIN)..=i64::from(i32::MAX)) {
        let encoded = encode_binary_int(value, 32, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 32, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Binary integer (16-bit unsigned) encode→decode round-trip.
    #[test]
    fn prop_binary_int_16_unsigned_roundtrip(value in 0i64..=65535) {
        let encoded = encode_binary_int(value, 16, false).expect("encode");
        let decoded = decode_binary_int(&encoded, 16, false).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Binary integer (32-bit unsigned) encode→decode round-trip.
    #[test]
    fn prop_binary_int_32_unsigned_roundtrip(value in 0i64..=i64::from(u32::MAX)) {
        let encoded = encode_binary_int(value, 32, false).expect("encode");
        let decoded = decode_binary_int(&encoded, 32, false).expect("decode");
        prop_assert_eq!(decoded, value);
    }
}

// ---------------------------------------------------------------------------
// COMP full-stack round-trip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// COMP (BINARY) field round-trips through full codec pipeline.
    #[test]
    fn prop_comp_fullstack_roundtrip(value in -32768i64..=32767) {
        let copybook = "       01 REC.\n           05 FLD PIC S9(4) BINARY.";
        let schema = parse_copybook(copybook).expect("parse");

        // Encoder expects string-typed numeric values
        let json_in = serde_json::json!({ "REC": {"FLD": value.to_string()} });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        let fld = json_out.get("FLD").expect("field exists");
        let out_val = fld
            .as_i64()
            .or_else(|| fld.as_str().and_then(|s| s.parse::<i64>().ok()))
            .expect("numeric value");
        prop_assert_eq!(out_val, value);
    }
}

// ---------------------------------------------------------------------------
// Decoded JSON is always valid JSON
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding any valid PIC X data always produces valid JSON.
    #[test]
    fn prop_decoded_pic_x_is_valid_json(
        len in 1usize..=50,
        data in prop::collection::vec(0x20u8..=0x7E, 1..=50),
    ) {
        let data: Vec<u8> = data.into_iter().take(len).collect();
        let padded: Vec<u8> = if data.len() < len {
            let mut v = data;
            v.resize(len, b' ');
            v
        } else {
            data
        };

        let copybook = format!("01 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = decode_record(&schema, &padded, &opts).expect("decode");

        // Serializing to string and re-parsing must succeed
        let serialized = serde_json::to_string(&json_val).expect("serialize");
        let _reparsed: Value = serde_json::from_str(&serialized).expect("reparse");
    }

    /// Decoding numeric fields produces valid JSON values.
    #[test]
    fn prop_decoded_pic9_is_valid_json(
        digits in prop::collection::vec(0u8..=9, 1..=9),
    ) {
        let copybook = format!("01 FLD PIC 9({}).", digits.len());
        let schema = parse_copybook(&copybook).expect("parse");

        let data: Vec<u8> = digits.iter().map(|&d| b'0' + d).collect();

        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = decode_record(&schema, &data, &opts).expect("decode");

        let serialized = serde_json::to_string(&json_val).expect("serialize");
        let reparsed: Value = serde_json::from_str(&serialized).expect("reparse");
        prop_assert!(reparsed.get("FLD").is_some());
    }

    /// Multi-field decoded record is always valid JSON with all expected keys.
    #[test]
    fn prop_multifield_decoded_is_valid_json(
        num_alpha in 1usize..=3,
        num_numeric in 1usize..=3,
        alpha_len in 1usize..=10,
        numeric_digits in 1usize..=5,
    ) {
        let mut lines = vec!["01 REC.".to_string()];
        let mut total_len = 0usize;
        for i in 0..num_alpha {
            lines.push(format!("   05 ALPHA{i} PIC X({alpha_len})."));
            total_len += alpha_len;
        }
        for i in 0..num_numeric {
            lines.push(format!("   05 NUM{i} PIC 9({numeric_digits})."));
            total_len += numeric_digits;
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        // Build test data
        let mut data = Vec::with_capacity(total_len);
        data.extend(std::iter::repeat_n(b'A', num_alpha * alpha_len));
        data.extend(std::iter::repeat_n(b'1', num_numeric * numeric_digits));

        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = decode_record(&schema, &data, &opts).expect("decode");

        let serialized = serde_json::to_string(&json_val).expect("serialize");
        let reparsed: Value = serde_json::from_str(&serialized).expect("reparse");

        for i in 0..num_alpha {
            let key = format!("ALPHA{i}");
            prop_assert!(reparsed.get(&key).is_some());
        }
        for i in 0..num_numeric {
            let key = format!("NUM{i}");
            prop_assert!(reparsed.get(&key).is_some());
        }
    }
}

// ---------------------------------------------------------------------------
// EBCDIC PIC X round-trip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// EBCDIC PIC X round-trips through decode→encode→decode.
    #[test]
    fn prop_ebcdic_pic_x_roundtrip(len in 1usize..=30) {
        let copybook = format!("01 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        // EBCDIC spaces (0x40) are safe roundtrippable data
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
}
