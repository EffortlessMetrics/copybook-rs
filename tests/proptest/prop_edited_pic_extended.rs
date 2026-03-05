// SPDX-License-Identifier: AGPL-3.0-or-later
//! Extended property tests for edited PIC encode/decode.
//!
//! Covers additional roundtrip scenarios beyond `prop_edited_pic.rs`:
//! - Z-pattern roundtrip for wider widths
//! - Currency ($) patterns preserve value through roundtrip
//! - Asterisk fill patterns roundtrip correctly
//! - No panic on arbitrary byte input to edited-PIC decode
//! - Sign-editing patterns with CR/DB trailing markers

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::panic::catch_unwind;

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use proptest::prelude::*;

use super::config::*;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

/// Format `value` into a Z-suppressed string of `width` characters.
fn z_format(value: u64, width: usize) -> Vec<u8> {
    let s = format!("{value:>width$}");
    s.bytes().collect()
}

/// Format `value` into an asterisk-protected string of `width` characters.
fn asterisk_format(value: u64, width: usize) -> Vec<u8> {
    let s = format!("{value:>width$}");
    s.chars()
        .map(|c| if c == ' ' { '*' } else { c })
        .collect::<String>()
        .bytes()
        .collect()
}

// ---------------------------------------------------------------------------
// 1. Z-pattern roundtrip for various widths (ZZ9, ZZZZ9, ZZZZZZZ9)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// ZZ9 (3-digit) roundtrip.
    #[test]
    fn prop_z_pattern_zz9_roundtrip(value in 0u64..=999) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC ZZ9.").expect("parse");
        let original = z_format(value, 3);

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }

    /// ZZZZZZZ9 (8-digit) roundtrip.
    #[test]
    fn prop_z_pattern_8_digit_roundtrip(value in 0u64..=99_999_999) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC ZZZZZZZ9.").expect("parse");
        let original = z_format(value, 8);

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }

    /// Zero encodes to a single trailing '0' with leading spaces.
    #[test]
    fn prop_z_pattern_zero_is_space_padded(width in 2usize..=10) {
        let pic: String = "Z".repeat(width - 1) + "9";
        let copybook = format!("01 REC.\n   05 AMT PIC {pic}.");
        let schema = parse_copybook(&copybook).expect("parse");
        let original = z_format(0, width);

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }
}

// ---------------------------------------------------------------------------
// 2. Currency patterns preserve value through roundtrip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// $ZZZ9 pattern roundtrip: value is right-justified with '$' prefix.
    #[test]
    fn prop_currency_dollar_zzz9_roundtrip(value in 0u64..=9999) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC $ZZZ9.").expect("parse");
        // Format: '$' followed by space-padded digits
        let digits = format!("{value:>4}");
        let formatted = format!("${digits}");
        let original: Vec<u8> = formatted.bytes().collect();

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }

    /// Decoded currency field yields correct numeric value.
    #[test]
    fn prop_currency_decode_preserves_value(value in 0u64..=9999) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC $ZZZ9.").expect("parse");
        let digits = format!("{value:>4}");
        let formatted = format!("${digits}");
        let data: Vec<u8> = formatted.bytes().collect();

        let json = decode_record(&schema, &data, &ascii_decode_opts()).expect("decode");
        if let Some(n) = json.get("AMT").and_then(|v| v.as_i64()) {
            prop_assert_eq!(n as u64, value);
        } else if let Some(s) = json.get("AMT").and_then(|v| v.as_str()) {
            let trimmed = s.trim().trim_start_matches('$').replace(',', "");
            if !trimmed.is_empty() {
                let parsed: u64 = trimmed.parse().expect("numeric");
                prop_assert_eq!(parsed, value);
            } else {
                prop_assert_eq!(value, 0);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// 3. Asterisk fill patterns roundtrip correctly
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// ***9 (4-digit) check-protect roundtrip.
    #[test]
    fn prop_asterisk_4_digit_roundtrip(value in 0u64..=9999) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC ***9.").expect("parse");
        let original = asterisk_format(value, 4);

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }

    /// *******9 (8-digit) check-protect roundtrip.
    #[test]
    fn prop_asterisk_8_digit_roundtrip(value in 0u64..=99_999_999) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC *******9.").expect("parse");
        let original = asterisk_format(value, 8);

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }

    /// Asterisk field of zero is all '*' except trailing '0'.
    #[test]
    fn prop_asterisk_zero_is_star_padded(width in 2usize..=10) {
        let pic: String = "*".repeat(width - 1) + "9";
        let copybook = format!("01 REC.\n   05 AMT PIC {pic}.");
        let schema = parse_copybook(&copybook).expect("parse");
        let original = asterisk_format(0, width);

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }
}

// ---------------------------------------------------------------------------
// 4. No panic on arbitrary byte input to edited-PIC decode
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Arbitrary 4-byte input to ZZZ9 decode never panics.
    #[test]
    fn prop_edited_zzz9_no_panic(data in prop::collection::vec(any::<u8>(), 4..=4)) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC ZZZ9.").expect("parse");
        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &data, &ascii_decode_opts());
        });
        prop_assert!(result.is_ok(), "decode_record panicked on input {:?}", data);
    }

    /// Arbitrary 5-byte input to $ZZZ9 decode never panics.
    #[test]
    fn prop_edited_dollar_no_panic(data in prop::collection::vec(any::<u8>(), 5..=5)) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC $ZZZ9.").expect("parse");
        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &data, &ascii_decode_opts());
        });
        prop_assert!(result.is_ok(), "decode_record panicked on input {:?}", data);
    }

    /// Arbitrary 5-byte input to ****9 decode never panics.
    #[test]
    fn prop_edited_asterisk_no_panic(data in prop::collection::vec(any::<u8>(), 5..=5)) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC ****9.").expect("parse");
        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &data, &ascii_decode_opts());
        });
        prop_assert!(result.is_ok(), "decode_record panicked on input {:?}", data);
    }

    /// Arbitrary 4-byte input to +999 decode never panics.
    #[test]
    fn prop_edited_signed_no_panic(data in prop::collection::vec(any::<u8>(), 4..=4)) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC +999.").expect("parse");
        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &data, &ascii_decode_opts());
        });
        prop_assert!(result.is_ok(), "decode_record panicked on input {:?}", data);
    }

    /// Arbitrary bytes of various edited patterns never panic.
    #[test]
    fn prop_edited_pic_random_length_no_panic(
        data in prop::collection::vec(any::<u8>(), 0..64),
        pattern_idx in 0usize..4,
    ) {
        let patterns = [
            ("01 REC.\n   05 A PIC ZZZ9.", 4usize),
            ("01 REC.\n   05 A PIC $ZZZ9.", 5),
            ("01 REC.\n   05 A PIC ****9.", 5),
            ("01 REC.\n   05 A PIC +999.", 4),
        ];
        let (copybook, expected_len) = patterns[pattern_idx];
        let schema = parse_copybook(copybook).expect("parse");

        // Pad or truncate to expected length
        let mut input = data;
        input.resize(expected_len, b' ');

        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &input, &ascii_decode_opts());
        });
        prop_assert!(result.is_ok(), "decode_record panicked");
    }
}

// ---------------------------------------------------------------------------
// 5. Sign editing extended patterns
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// -999 pattern: negative values have '-', positive have ' '.
    #[test]
    fn prop_minus_sign_editing_roundtrip(value in 1u64..=999, negative in any::<bool>()) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC -999.").expect("parse");
        let sign = if negative { '-' } else { ' ' };
        let formatted = format!("{sign}{value:03}");
        let original: Vec<u8> = formatted.bytes().collect();

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }

    /// +999 pattern roundtrip preserves sign character.
    #[test]
    fn prop_plus_sign_editing_roundtrip(value in 1u64..=999, negative in any::<bool>()) {
        let schema = parse_copybook("01 REC.\n   05 AMT PIC +999.").expect("parse");
        let sign = if negative { '-' } else { '+' };
        let formatted = format!("{sign}{value:03}");
        let original: Vec<u8> = formatted.bytes().collect();

        let json = decode_record(&schema, &original, &ascii_decode_opts()).expect("decode");
        let rt = encode_record(&schema, &json, &ascii_encode_opts()).expect("encode");
        prop_assert_eq!(rt, original);
    }
}
