// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for alphanumeric (PIC X) roundtrip invariants.
//!
//! Verifies that PIC X encode→decode preserves content (modulo padding/trim),
//! that encoded output length always matches PIC width, that padding uses
//! the correct fill character, and that JSON output is deterministic.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

// ============================================================================
// 1. Encode→decode content preservation (trimmed)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// PIC X ASCII roundtrip: string → encode → decode → trimmed value matches.
    #[test]
    fn prop_alpha_ascii_content_preserved(
        len in 1usize..=50,
        text in "[A-Za-z0-9]{1,50}",
    ) {
        let text: String = text.chars().take(len).collect();
        let copybook = format!("       01 REC.\n           05 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": text } });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_out = decode_record(&schema, &binary, &dec).expect("decode");

        let out_str = json_out.get("FLD").and_then(|v| v.as_str()).expect("FLD string");
        let expected = if text.len() > len { &text[..len] } else { &text };
        prop_assert_eq!(
            out_str.trim_end(), expected.trim_end(),
            "content mismatch for PIC X({})", len
        );
    }

    /// PIC X EBCDIC roundtrip: string → encode (CP037) → decode → trimmed value matches.
    #[test]
    fn prop_alpha_ebcdic_content_preserved(
        len in 1usize..=40,
        text in "[A-Z0-9]{1,40}",
    ) {
        let text: String = text.chars().take(len).collect();
        let copybook = format!("       01 REC.\n           05 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": text } });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let json_out = decode_record(&schema, &binary, &dec).expect("decode");

        let out_str = json_out.get("FLD").and_then(|v| v.as_str()).expect("FLD");
        let expected = if text.len() > len { &text[..len] } else { &text };
        prop_assert_eq!(
            out_str.trim_end(), expected.trim_end(),
            "EBCDIC content mismatch for PIC X({})", len
        );
    }
}

// ============================================================================
// 2. Encoded length invariant: always matches PIC width
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Encoded byte count always equals the PIC X width.
    #[test]
    fn prop_alpha_encoded_length_equals_pic_width(
        len in 1usize..=80,
        text in "[A-Za-z0-9]{0,80}",
    ) {
        let text: String = text.chars().take(len).collect();
        let copybook = format!("       01 REC.\n           05 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": text } });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        prop_assert_eq!(
            binary.len(), len,
            "encoded length must equal PIC X({}) width", len
        );
    }
}

// ============================================================================
// 3. Padding fills with spaces (ASCII 0x20)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Short strings are right-padded with ASCII space bytes.
    #[test]
    fn prop_alpha_ascii_padding_is_spaces(
        pic_len in 5usize..=50,
        text_len in 1usize..=4,
    ) {
        let text: String = (0..text_len).map(|i| {
            char::from(b'A' + (i as u8 % 26))
        }).collect();

        let copybook = format!("       01 REC.\n           05 FLD PIC X({pic_len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": text } });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        // Trailing bytes (beyond text) must be ASCII space
        for (i, &byte) in binary.iter().enumerate().skip(text_len) {
            prop_assert_eq!(
                byte, 0x20,
                "byte at position {} should be space (0x20), got 0x{:02X}",
                i, byte
            );
        }
    }
}

// ============================================================================
// 4. Decode→encode→decode stability (double roundtrip)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// decode→encode→decode produces identical JSON output.
    #[test]
    fn prop_alpha_double_roundtrip_stable(
        len in 1usize..=40,
        data in prop::collection::vec(0x20u8..=0x7E, 1..=40),
    ) {
        let data: Vec<u8> = data.into_iter().take(len).collect();
        let padded: Vec<u8> = if data.len() < len {
            let mut v = data.clone();
            v.resize(len, b' ');
            v
        } else {
            data
        };

        let copybook = format!("       01 REC.\n           05 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json1 = decode_record(&schema, &padded, &dec).expect("decode 1");
        let binary = encode_record(&schema, &json1, &enc).expect("encode");
        let json2 = decode_record(&schema, &binary, &dec).expect("decode 2");

        prop_assert_eq!(
            &json1, &json2,
            "decode→encode→decode must be stable for PIC X({})", len
        );
    }
}

// ============================================================================
// 5. JSON output determinism: same bytes → identical JSON
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding the same bytes twice produces byte-identical JSON.
    #[test]
    fn prop_alpha_json_output_deterministic(
        len in 1usize..=50,
        data in prop::collection::vec(0x20u8..=0x7E, 1..=50),
    ) {
        let data: Vec<u8> = data.into_iter().take(len).collect();
        let padded: Vec<u8> = if data.len() < len {
            let mut v = data.clone();
            v.resize(len, b' ');
            v
        } else {
            data
        };

        let copybook = format!("       01 REC.\n           05 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let json1 = decode_record(&schema, &padded, &dec).expect("decode 1");
        let json2 = decode_record(&schema, &padded, &dec).expect("decode 2");

        let str1 = serde_json::to_string(&json1).expect("serialize 1");
        let str2 = serde_json::to_string(&json2).expect("serialize 2");

        prop_assert_eq!(
            &str1, &str2,
            "JSON output must be byte-identical for same input"
        );
    }
}

// ============================================================================
// 6. Multi-field alphanumeric record
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Multi-field PIC X record: each field independently preserved.
    #[test]
    fn prop_alpha_multi_field_independent(
        text_a in "[A-Z]{1,8}",
        text_b in "[0-9]{1,6}",
    ) {
        let copybook = "\
       01 REC.\n\
           05 FLD-A PIC X(8).\n\
           05 FLD-B PIC X(6).";
        let schema = parse_copybook(copybook).expect("parse");

        let json_in = serde_json::json!({
            "REC": {
                "FLD-A": text_a,
                "FLD-B": text_b,
            }
        });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        prop_assert_eq!(binary.len(), 14, "record must be 8+6=14 bytes");

        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_out = decode_record(&schema, &binary, &dec).expect("decode");

        let out_a = json_out.get("FLD-A").and_then(|v| v.as_str()).expect("FLD-A");
        let out_b = json_out.get("FLD-B").and_then(|v| v.as_str()).expect("FLD-B");

        let exp_a = if text_a.len() > 8 { &text_a[..8] } else { &text_a };
        let exp_b = if text_b.len() > 6 { &text_b[..6] } else { &text_b };

        prop_assert_eq!(out_a.trim_end(), exp_a.trim_end());
        prop_assert_eq!(out_b.trim_end(), exp_b.trim_end());
    }
}

// ============================================================================
// 7. All EBCDIC codepages produce same decoded content for ASCII-safe chars
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// All EBCDIC codepages decode identically for uppercase ASCII text.
    #[test]
    fn prop_alpha_all_ebcdic_codepages_agree(
        text in "[A-Z0-9]{1,20}",
    ) {
        let len = text.len();
        let copybook = format!("       01 REC.\n           05 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "REC": { "FLD": text } });

        let codepages = [
            Codepage::CP037,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];

        let mut results = Vec::new();
        for &cp in &codepages {
            let enc = EncodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(cp);
            let binary = encode_record(&schema, &json_in, &enc).expect("encode");

            let dec = DecodeOptions::new()
                .with_format(RecordFormat::Fixed)
                .with_codepage(cp);
            let json_out = decode_record(&schema, &binary, &dec).expect("decode");
            let val = json_out.get("FLD").and_then(|v| v.as_str()).unwrap_or("").to_string();
            results.push(val);
        }

        let first = &results[0];
        for (i, result) in results.iter().enumerate().skip(1) {
            prop_assert_eq!(
                result.trim_end(), first.trim_end(),
                "codepage {:?} disagrees with {:?}",
                codepages[i], codepages[0]
            );
        }
    }
}
