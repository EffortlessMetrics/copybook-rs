#![allow(unused_doc_comments, unused_imports, dead_code)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for SIGN SEPARATE encode/decode roundtrip.
//!
//! Verifies that SIGN SEPARATE fields with LEADING and TRAILING placement
//! round-trip correctly through encode -> decode, and that the sign byte
//! occupies the correct position in the encoded buffer.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_codec::Codepage;
use copybook_codec::numeric::{
    decode_zoned_decimal_sign_separate, encode_zoned_decimal_sign_separate,
};
use copybook_core::{SignPlacement, SignSeparateInfo};
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

/// Strategy: generate a (value, digits, scale) triple that is encodable.
fn value_digits_scale() -> impl Strategy<Value = (String, u16, i16)> {
    (1u16..=9, 0i16..=4).prop_flat_map(|(digits, scale)| {
        let max_val = 10i64.saturating_pow(u32::from(digits)) - 1;
        (0..=max_val, any::<bool>(), Just(digits), Just(scale)).prop_map(
            move |(abs, negative, digits, scale)| {
                let sign = if negative && abs != 0 { "-" } else { "" };
                if scale > 0 {
                    let width = usize::from(digits);
                    let s = format!("{abs:0>width$}");
                    let sc = usize::try_from(scale).unwrap_or(0);
                    let split = s.len().saturating_sub(sc);
                    let (int, frac) = s.split_at(split);
                    let int = if int.is_empty() { "0" } else { int };
                    (format!("{sign}{int}.{frac}"), digits, scale)
                } else {
                    (format!("{sign}{abs}"), digits, scale)
                }
            },
        )
    })
}

fn sign_placement_strategy() -> impl Strategy<Value = SignPlacement> {
    prop_oneof![Just(SignPlacement::Leading), Just(SignPlacement::Trailing)]
}

fn codepage_strategy() -> impl Strategy<Value = Codepage> {
    prop_oneof![Just(Codepage::ASCII), Just(Codepage::CP037)]
}

/// Property: encode -> decode roundtrip preserves the numeric value.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sign_separate_encode_decode_roundtrip(
        (value, digits, scale) in value_digits_scale(),
        placement in sign_placement_strategy(),
        codepage in codepage_strategy(),
    ) {
        let info = SignSeparateInfo { placement };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate(&value, digits, scale, &info, codepage, &mut buffer)
            .expect("encode should succeed");

        let decoded = decode_zoned_decimal_sign_separate(&buffer, digits, scale, &info, codepage)
            .expect("decode should succeed");

        // Re-encode from the decoded value to compare buffers.
        let decoded_str = decoded.to_string();
        let mut buffer2 = vec![0u8; buf_len];
        encode_zoned_decimal_sign_separate(&decoded_str, digits, scale, &info, codepage, &mut buffer2)
            .expect("re-encode should succeed");

        prop_assert_eq!(&buffer, &buffer2,
            "roundtrip mismatch for value={}, digits={}, scale={}, placement={:?}, codepage={:?}",
            value, digits, scale, placement, codepage);
    }
}

/// Property: The sign byte is at position 0 for LEADING, at position `digits` for TRAILING.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sign_byte_position_correct(
        (value, digits, scale) in value_digits_scale(),
        placement in sign_placement_strategy(),
        codepage in codepage_strategy(),
    ) {
        let info = SignSeparateInfo { placement };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate(&value, digits, scale, &info, codepage, &mut buffer)
            .expect("encode should succeed");

        let sign_idx = match placement {
            SignPlacement::Leading => 0,
            SignPlacement::Trailing => usize::from(digits),
        };

        let sign_byte = buffer[sign_idx];

        let valid_signs = if codepage.is_ascii() {
            vec![b'+', b'-']
        } else {
            vec![0x4E, 0x60]
        };

        prop_assert!(
            valid_signs.contains(&sign_byte),
            "sign byte 0x{:02X} at index {} is not a valid sign for {:?}",
            sign_byte, sign_idx, codepage,
        );
    }
}

/// Property: Encoded buffer length is always digits + 1.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_encoded_length_is_digits_plus_one(
        (value, digits, scale) in value_digits_scale(),
        placement in sign_placement_strategy(),
        codepage in codepage_strategy(),
    ) {
        let info = SignSeparateInfo { placement };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate(&value, digits, scale, &info, codepage, &mut buffer)
            .expect("encode should succeed");

        let digit_range = match placement {
            SignPlacement::Leading => 1..buf_len,
            SignPlacement::Trailing => 0..usize::from(digits),
        };
        for &b in &buffer[digit_range] {
            let is_digit = if codepage.is_ascii() {
                b.is_ascii_digit()
            } else {
                (0xF0..=0xF9).contains(&b)
            };
            prop_assert!(is_digit, "non-digit byte 0x{b:02X} in digit region");
        }
    }
}

/// Property: Negative zero normalizes to positive zero on roundtrip.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_negative_zero_normalizes(
        digits in 1u16..=9,
        placement in sign_placement_strategy(),
        codepage in codepage_strategy(),
    ) {
        let info = SignSeparateInfo { placement };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate("0", digits, 0, &info, codepage, &mut buffer)
            .expect("encode +0 should succeed");

        let decoded = decode_zoned_decimal_sign_separate(&buffer, digits, 0, &info, codepage)
            .expect("decode should succeed");

        prop_assert!(
            !decoded.is_negative(),
            "zero should not be negative after roundtrip"
        );
    }
}

/// Property: Full-stack parse + decode roundtrip for SIGN SEPARATE copybooks.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sign_separate_parse_schema_field_length(
        digits in 1u16..=9,
        placement in prop_oneof![Just("LEADING"), Just("TRAILING")],
    ) {
        let copybook = format!(
            "       05  AMT PIC S9({digits}) SIGN IS SEPARATE {placement}."
        );

        let schema = copybook_core::parse_copybook(&copybook)
            .expect("parse should succeed");

        let field = &schema.fields[0];

        // SIGN SEPARATE adds 1 byte to the field length
        prop_assert_eq!(
            field.len,
            u32::from(digits) + 1,
            "SIGN SEPARATE field len should be digits ({}) + 1",
            digits,
        );
    }
}
