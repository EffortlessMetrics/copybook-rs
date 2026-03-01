// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for binary integer (COMP/BINARY) roundtrip invariants.
//!
//! Tests that the full-stack encode→decode pipeline preserves integer values
//! for all supported binary widths, signedness, and codepages. Also verifies
//! encoding length invariants and boundary behavior.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::numeric::{decode_binary_int, encode_binary_int};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

// ============================================================================
// 1. Low-level encode→decode roundtrip for all widths
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Signed i16 encode→decode preserves exact value.
    #[test]
    fn prop_binary_i16_exact_roundtrip(value in i64::from(i16::MIN)..=i64::from(i16::MAX)) {
        let buf = encode_binary_int(value, 16, true).expect("encode");
        prop_assert_eq!(buf.len(), 2, "i16 must encode to 2 bytes");
        let decoded = decode_binary_int(&buf, 16, true).expect("decode");
        prop_assert_eq!(decoded, value, "i16 roundtrip mismatch");
    }

    /// Signed i32 encode→decode preserves exact value.
    #[test]
    fn prop_binary_i32_exact_roundtrip(value in i64::from(i32::MIN)..=i64::from(i32::MAX)) {
        let buf = encode_binary_int(value, 32, true).expect("encode");
        prop_assert_eq!(buf.len(), 4, "i32 must encode to 4 bytes");
        let decoded = decode_binary_int(&buf, 32, true).expect("decode");
        prop_assert_eq!(decoded, value, "i32 roundtrip mismatch");
    }

    /// Signed i64 encode→decode preserves exact value.
    #[test]
    fn prop_binary_i64_exact_roundtrip(value in any::<i64>()) {
        let buf = encode_binary_int(value, 64, true).expect("encode");
        prop_assert_eq!(buf.len(), 8, "i64 must encode to 8 bytes");
        let decoded = decode_binary_int(&buf, 64, true).expect("decode");
        prop_assert_eq!(decoded, value, "i64 roundtrip mismatch");
    }

    /// Unsigned u16 encode→decode preserves exact value.
    #[test]
    fn prop_binary_u16_exact_roundtrip(value in 0i64..=i64::from(u16::MAX)) {
        let buf = encode_binary_int(value, 16, false).expect("encode");
        prop_assert_eq!(buf.len(), 2, "u16 must encode to 2 bytes");
        let decoded = decode_binary_int(&buf, 16, false).expect("decode");
        prop_assert_eq!(decoded, value, "u16 roundtrip mismatch");
    }

    /// Unsigned u32 encode→decode preserves exact value.
    #[test]
    fn prop_binary_u32_exact_roundtrip(value in 0i64..=i64::from(u32::MAX)) {
        let buf = encode_binary_int(value, 32, false).expect("encode");
        prop_assert_eq!(buf.len(), 4, "u32 must encode to 4 bytes");
        let decoded = decode_binary_int(&buf, 32, false).expect("decode");
        prop_assert_eq!(decoded, value, "u32 roundtrip mismatch");
    }
}

// ============================================================================
// 2. Encoding length invariant: width determines byte count
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Encoded byte count is always width / 8, regardless of value.
    #[test]
    fn prop_binary_encoding_length_invariant(
        value in any::<i64>(),
        width in prop_oneof![Just(16u16), Just(32u16), Just(64u16)],
        signed in any::<bool>(),
    ) {
        let clamped = match (width, signed) {
            (16, true) => value.clamp(i64::from(i16::MIN), i64::from(i16::MAX)),
            (16, false) => value.clamp(0, i64::from(u16::MAX)),
            (32, true) => value.clamp(i64::from(i32::MIN), i64::from(i32::MAX)),
            (32, false) => value.clamp(0, i64::from(u32::MAX)),
            (64, true) => value,
            (64, false) => value.clamp(0, i64::MAX),
            _ => unreachable!(),
        };
        let buf = encode_binary_int(clamped, width, signed).expect("encode");
        let expected_len = usize::from(width) / 8;
        prop_assert_eq!(
            buf.len(), expected_len,
            "width={} signed={} must produce {} bytes",
            width, signed, expected_len
        );
    }
}

// ============================================================================
// 3. Big-endian byte ordering: encode is network byte order
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// i16 encoding matches big-endian representation.
    #[test]
    fn prop_binary_i16_big_endian(value in i16::MIN..=i16::MAX) {
        let buf = encode_binary_int(i64::from(value), 16, true).expect("encode");
        let expected = value.to_be_bytes();
        prop_assert_eq!(buf.as_slice(), &expected[..], "i16 must be big-endian");
    }

    /// i32 encoding matches big-endian representation.
    #[test]
    fn prop_binary_i32_big_endian(value in i32::MIN..=i32::MAX) {
        let buf = encode_binary_int(i64::from(value), 32, true).expect("encode");
        let expected = value.to_be_bytes();
        prop_assert_eq!(buf.as_slice(), &expected[..], "i32 must be big-endian");
    }
}

// ============================================================================
// 4. Full-stack record-level roundtrip via encode_record/decode_record
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Full-stack i16 BINARY: JSON → encode_record → decode_record → same value.
    #[test]
    fn prop_binary_fullstack_i16(value in -32768i64..=32767) {
        let copybook = "       01 REC.\n           05 FLD PIC S9(4) BINARY.";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": value.to_string() } });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &dec).expect("decode");

        let out_val = json_out
            .get("FLD")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD");
        prop_assert_eq!(out_val, value, "full-stack i16 roundtrip mismatch");
    }

    /// Full-stack i32 BINARY: JSON → encode → decode → same value.
    #[test]
    fn prop_binary_fullstack_i32(value in i64::from(i32::MIN)..=i64::from(i32::MAX)) {
        let copybook = "       01 REC.\n           05 FLD PIC S9(9) BINARY.";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": value.to_string() } });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_out = decode_record(&schema, &binary, &dec).expect("decode");

        let out_val = json_out
            .get("FLD")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD");
        prop_assert_eq!(out_val, value, "full-stack i32 roundtrip mismatch");
    }

    /// Full-stack unsigned BINARY: JSON → encode → decode → same value.
    #[test]
    fn prop_binary_fullstack_unsigned(value in 0i64..=i64::from(u16::MAX)) {
        let copybook = "       01 REC.\n           05 FLD PIC 9(4) BINARY.";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": value.to_string() } });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &dec).expect("decode");

        let out_val = json_out
            .get("FLD")
            .and_then(|v| v.as_i64().or_else(|| v.as_str().and_then(|s| s.parse().ok())))
            .expect("FLD");
        prop_assert_eq!(out_val, value, "full-stack unsigned roundtrip mismatch");
    }
}

// ============================================================================
// 5. Double-encode stability: encode twice → identical bytes
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Encoding the same value twice produces identical byte sequences.
    #[test]
    fn prop_binary_double_encode_stable(
        value in any::<i64>(),
        width in prop_oneof![Just(16u16), Just(32u16), Just(64u16)],
    ) {
        let clamped = match width {
            16 => value.clamp(i64::from(i16::MIN), i64::from(i16::MAX)),
            32 => value.clamp(i64::from(i32::MIN), i64::from(i32::MAX)),
            64 => value,
            _ => unreachable!(),
        };
        let buf1 = encode_binary_int(clamped, width, true).expect("encode 1");
        let buf2 = encode_binary_int(clamped, width, true).expect("encode 2");
        prop_assert_eq!(&buf1, &buf2, "double encode must be byte-identical");
    }
}
