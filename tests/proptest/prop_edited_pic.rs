// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for edited PIC encode/decode.
//!
//! Verifies roundtrip fidelity, zero suppression, and sign editing
//! for edited numeric PICTURE clauses.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, EncodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use proptest::prelude::*;
use serde_json::json;

use super::config::*;

/// Strategy for generating unsigned integer values that fit in `digits` digits.
fn unsigned_value(digits: usize) -> impl Strategy<Value = u64> {
    #[allow(clippy::cast_possible_truncation)]
    let max = 10u64.pow(digits as u32) - 1;
    0u64..=max
}

// ---------------------------------------------------------------------------
// Roundtrip: encode then decode preserves numeric value for zero-suppressed fields
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Zero-suppressed field (ZZZ9): encode(decode(binary)) preserves the binary.
    #[test]
    fn prop_edited_pic_zzz9_roundtrip(value in unsigned_value(4)) {
        let copybook = "01 REC.\n   05 AMT PIC ZZZ9.";
        let schema = parse_copybook(copybook).expect("parse");

        // Build ASCII representation: right-justified, space-padded
        let formatted = format!("{value:>4}");
        let original: Vec<u8> = formatted.bytes().collect();

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = copybook_codec::decode_record(&schema, &original, &decode_opts)
            .expect("decode");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let roundtrip = copybook_codec::encode_record(&schema, &json_val, &encode_opts)
            .expect("encode");

        prop_assert_eq!(roundtrip, original);
    }
}

// ---------------------------------------------------------------------------
// Zero suppression preserves numeric value
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding a zero-suppressed field yields the correct numeric value.
    #[test]
    fn prop_zero_suppression_preserves_value(value in 0u64..=9999u64) {
        let copybook = "01 REC.\n   05 AMT PIC ZZZ9.";
        let schema = parse_copybook(copybook).expect("parse");

        let formatted = format!("{value:>4}");
        let data: Vec<u8> = formatted.bytes().collect();

        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = copybook_codec::decode_record(&schema, &data, &opts)
            .expect("decode");

        // The decoded value should represent the original number
        // Either string or number, the numeric content should match
        if let Some(n) = json_val.get("AMT").and_then(|v| v.as_i64()) {
            prop_assert_eq!(n as u64, value);
        } else if let Some(s) = json_val.get("AMT").and_then(|v| v.as_str()) {
            let trimmed = s.trim();
            if !trimmed.is_empty() {
                let parsed: u64 = trimmed.parse().expect("numeric string");
                prop_assert_eq!(parsed, value);
            } else {
                // Zero represented as empty/blank
                prop_assert_eq!(value, 0);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Asterisk check-protect roundtrip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Check-protect pattern (****9) roundtrips correctly.
    #[test]
    fn prop_check_protect_roundtrip(value in 0u64..=99999u64) {
        let copybook = "01 REC.\n   05 AMT PIC ****9.";
        let schema = parse_copybook(copybook).expect("parse");

        // Format with asterisk fill for leading zeros
        let digits = format!("{value:>5}");
        let formatted: String = digits.chars().map(|c| if c == ' ' { '*' } else { c }).collect();
        let original: Vec<u8> = formatted.bytes().collect();

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = copybook_codec::decode_record(&schema, &original, &decode_opts)
            .expect("decode");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let roundtrip = copybook_codec::encode_record(&schema, &json_val, &encode_opts)
            .expect("encode");

        prop_assert_eq!(roundtrip, original);
    }
}

// ---------------------------------------------------------------------------
// Edited PIC roundtrip (no decimal—parser period ambiguity; decimal patterns
// are covered by edited_pic_encode_e3_tests at the tokenizer level)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Edited field (ZZZZ9) roundtrips correctly.
    #[test]
    fn prop_edited_decimal_roundtrip(
        value in 0u64..=99999u64,
    ) {
        let copybook = "01 REC.\n   05 AMT PIC ZZZZ9.";
        let schema = parse_copybook(copybook).expect("parse");

        let formatted = format!("{value:>5}");
        let original: Vec<u8> = formatted.bytes().collect();

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = copybook_codec::decode_record(&schema, &original, &decode_opts)
            .expect("decode");

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let roundtrip = copybook_codec::encode_record(&schema, &json_val, &encode_opts)
            .expect("encode");

        prop_assert_eq!(roundtrip, original);
    }
}

// ---------------------------------------------------------------------------
// Sign editing preserves sign
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Signed edited field preserves sign through decode.
    #[test]
    fn prop_sign_editing_preserves_sign(
        value in 1u64..=999u64,
        negative in any::<bool>(),
    ) {
        let copybook = "01 REC.\n   05 AMT PIC +999.";
        let schema = parse_copybook(copybook).expect("parse");

        let sign_char = if negative { '-' } else { '+' };
        let formatted = format!("{sign_char}{value:03}");
        let data: Vec<u8> = formatted.bytes().collect();

        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_val = copybook_codec::decode_record(&schema, &data, &opts)
            .expect("decode");

        // Decoded value should preserve sign
        if let Some(n) = json_val.get("AMT").and_then(|v| v.as_i64()) {
            if negative {
                prop_assert!(n < 0, "Expected negative value, got {}", n);
            } else {
                prop_assert!(n >= 0, "Expected non-negative value, got {}", n);
            }
        }
    }
}
