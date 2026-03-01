// SPDX-License-Identifier: AGPL-3.0-or-later
//! Field-type-specific roundtrip property tests.
//!
//! Each COBOL field type gets dedicated encode→decode→encode (or
//! decode→encode→decode) property tests that verify value preservation.
//! A final section feeds arbitrary bytes to every decoder and asserts no panics.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::panic::catch_unwind;

use copybook_codec::numeric::{
    decode_binary_int, decode_packed_decimal, decode_zoned_decimal,
    decode_zoned_decimal_sign_separate, encode_binary_int, encode_packed_decimal,
    encode_zoned_decimal, encode_zoned_decimal_sign_separate,
};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use copybook_core::{SignPlacement, SignSeparateInfo};
use copybook_overpunch::{ZeroSignPolicy, decode_overpunch_byte, encode_overpunch_byte};
use proptest::prelude::*;

use super::config::{DEFAULT_CASES, QUICK_CASES};

// ============================================================================
// 1. Alphanumeric (PIC X) roundtrip
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Arbitrary printable ASCII → encode to PIC X → decode → matches (modulo trailing space trim).
    #[test]
    fn prop_alpha_ascii_roundtrip(
        len in 1usize..=100,
        data in prop::collection::vec(0x20u8..=0x7E, 1..=100),
    ) {
        let data: Vec<u8> = data.into_iter().take(len).collect();
        let padded: Vec<u8> = if data.len() < len {
            let mut v = data.clone();
            v.resize(len, b' ');
            v
        } else {
            data.clone()
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

        prop_assert_eq!(&json1, &json2, "decode→encode→decode mismatch for PIC X({})", len);
    }

    /// PIC X roundtrip: JSON string → encode → decode → trimmed value matches.
    #[test]
    fn prop_alpha_encode_first_roundtrip(
        len in 1usize..=50,
        text in "[A-Za-z0-9 ]{1,50}",
    ) {
        let text: String = text.chars().take(len).collect();
        let copybook = format!("01 FLD PIC X({len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let json_in = serde_json::json!({ "FLD": text });

        let encode_opts = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &encode_opts).expect("encode");

        prop_assert_eq!(binary.len(), len, "encoded length must match PIC X({})", len);

        let decode_opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let json_out = decode_record(&schema, &binary, &decode_opts).expect("decode");

        let out_str = json_out.get("FLD").and_then(|v| v.as_str()).expect("FLD string");
        // After roundtrip the value is right-padded with spaces then right-trimmed
        let expected = if text.len() > len { &text[..len] } else { &text };
        prop_assert_eq!(out_str.trim_end(), expected.trim_end());
    }
}

// ============================================================================
// 2. Display numeric (PIC 9) roundtrip
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Non-negative integer → encode PIC 9 → decode → same value.
    #[test]
    fn prop_display_numeric_roundtrip(
        digits in 1u16..=18,
        raw_value in 0u64..=999_999_999_999_999_999u64,
    ) {
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let value = raw_value % max_val;
        let value_str = value.to_string();

        let encoded = encode_zoned_decimal(&value_str, digits, 0, false, Codepage::ASCII)
            .expect("encode zoned");
        let decoded = decode_zoned_decimal(&encoded, digits, 0, false, Codepage::ASCII, false)
            .expect("decode zoned");

        prop_assert_eq!(decoded.value, i64::try_from(value).unwrap(),
            "PIC 9({}) roundtrip mismatch for {}", digits, value);
        prop_assert!(!decoded.negative);
    }

    /// Signed integer → encode PIC S9 → decode → same value.
    #[test]
    fn prop_display_signed_numeric_roundtrip(
        digits in 1u16..=18,
        raw_value in 0u64..=999_999_999_999_999_999u64,
        negative in any::<bool>(),
    ) {
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let abs_val = raw_value % max_val;
        let sign = if negative && abs_val != 0 { "-" } else { "" };
        let value_str = format!("{sign}{abs_val}");

        let encoded = encode_zoned_decimal(&value_str, digits, 0, true, Codepage::ASCII)
            .expect("encode zoned signed");
        let decoded = decode_zoned_decimal(&encoded, digits, 0, true, Codepage::ASCII, false)
            .expect("decode zoned signed");

        let expected_val = i64::try_from(abs_val).unwrap();
        prop_assert_eq!(decoded.value.abs(), expected_val);
        if abs_val == 0 {
            prop_assert!(!decoded.negative, "zero should normalise to positive");
        } else {
            prop_assert_eq!(decoded.negative, negative);
        }
    }

    /// Display numeric with decimal scale roundtrips.
    #[test]
    fn prop_display_numeric_with_scale_roundtrip(
        int_digits in 1u16..=9,
        scale in 1i16..=4,
        raw_value in 0u64..=999_999_999u64,
    ) {
        let total = int_digits + scale as u16;
        let max_val = 10u64.saturating_pow(u32::from(total));
        let abs_val = raw_value % max_val;

        let width = usize::from(total);
        let s = format!("{abs_val:0>width$}");
        let sc = usize::try_from(scale).unwrap();
        let split = s.len().saturating_sub(sc);
        let (int_part, frac_part) = s.split_at(split);
        let int_part = if int_part.is_empty() { "0" } else { int_part };
        let value_str = format!("{int_part}.{frac_part}");

        let encoded = encode_zoned_decimal(&value_str, total, scale, false, Codepage::ASCII)
            .expect("encode");
        let decoded = decode_zoned_decimal(&encoded, total, scale, false, Codepage::ASCII, false)
            .expect("decode");

        let re_encoded = encode_zoned_decimal(&decoded.to_string(), total, scale, false, Codepage::ASCII)
            .expect("re-encode");

        prop_assert_eq!(&encoded, &re_encoded,
            "PIC 9({})V9({}) roundtrip mismatch for {}", int_digits, scale, value_str);
    }
}

// ============================================================================
// 3. COMP-3 (packed decimal) roundtrip
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// i64 with arbitrary scale → encode COMP-3 → decode → same value.
    #[test]
    fn prop_comp3_signed_roundtrip(
        digits in 1u16..=9,
        scale in 0i16..=4,
        raw_value in 0u64..=999_999_999u64,
        negative in any::<bool>(),
    ) {
        #[allow(clippy::cast_possible_wrap)]
        let scale = scale.min((digits as i16) - 1).max(0);
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let abs_val = raw_value % max_val;
        let sign = if negative && abs_val != 0 { "-" } else { "" };

        let value_str = if scale > 0 {
            let width = usize::from(digits);
            let s = format!("{abs_val:0>width$}");
            let sc = usize::try_from(scale).unwrap();
            let split = s.len().saturating_sub(sc);
            let (int, frac) = s.split_at(split);
            let int = if int.is_empty() { "0" } else { int };
            format!("{sign}{int}.{frac}")
        } else {
            format!("{sign}{abs_val}")
        };

        let encoded = encode_packed_decimal(&value_str, digits, scale, true)
            .expect("encode comp-3");
        let decoded = decode_packed_decimal(&encoded, digits, scale, true)
            .expect("decode comp-3");

        let re_encoded = encode_packed_decimal(&decoded.to_string(), digits, scale, true)
            .expect("re-encode comp-3");

        prop_assert_eq!(&encoded, &re_encoded,
            "COMP-3 roundtrip mismatch for {}", value_str);
    }

    /// Unsigned COMP-3 roundtrip.
    #[test]
    fn prop_comp3_unsigned_roundtrip(
        digits in 1u16..=9,
        raw_value in 0u64..=999_999_999u64,
    ) {
        let max_val = 10u64.saturating_pow(u32::from(digits));
        let abs_val = raw_value % max_val;
        let value_str = abs_val.to_string();

        let encoded = encode_packed_decimal(&value_str, digits, 0, false)
            .expect("encode");
        let decoded = decode_packed_decimal(&encoded, digits, 0, false)
            .expect("decode");

        prop_assert_eq!(decoded.value, i64::try_from(abs_val).unwrap(),
            "unsigned COMP-3 roundtrip mismatch for {}", abs_val);
    }
}

// ============================================================================
// 4. COMP (BINARY) roundtrip
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Signed 16-bit binary roundtrip.
    #[test]
    fn prop_comp_binary_i16_roundtrip(value in i64::from(i16::MIN)..=i64::from(i16::MAX)) {
        let encoded = encode_binary_int(value, 16, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 16, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Signed 32-bit binary roundtrip.
    #[test]
    fn prop_comp_binary_i32_roundtrip(value in i64::from(i32::MIN)..=i64::from(i32::MAX)) {
        let encoded = encode_binary_int(value, 32, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 32, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Signed 64-bit binary roundtrip.
    #[test]
    fn prop_comp_binary_i64_roundtrip(value in any::<i64>()) {
        let encoded = encode_binary_int(value, 64, true).expect("encode");
        let decoded = decode_binary_int(&encoded, 64, true).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Unsigned 16-bit binary roundtrip.
    #[test]
    fn prop_comp_binary_u16_roundtrip(value in 0i64..=i64::from(u16::MAX)) {
        let encoded = encode_binary_int(value, 16, false).expect("encode");
        let decoded = decode_binary_int(&encoded, 16, false).expect("decode");
        prop_assert_eq!(decoded, value);
    }

    /// Unsigned 32-bit binary roundtrip.
    #[test]
    fn prop_comp_binary_u32_roundtrip(value in 0i64..=i64::from(u32::MAX)) {
        let encoded = encode_binary_int(value, 32, false).expect("encode");
        let decoded = decode_binary_int(&encoded, 32, false).expect("decode");
        prop_assert_eq!(decoded, value);
    }
}

// ============================================================================
// 5. SIGN SEPARATE roundtrip
// ============================================================================

fn sign_sep_value_strategy() -> impl Strategy<Value = (String, u16, i16)> {
    (1u16..=9, 0i16..=3).prop_flat_map(|(digits, scale)| {
        #[allow(clippy::cast_possible_wrap)]
        let scale = scale.min((digits as i16) - 1).max(0);
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

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// SIGN SEPARATE LEADING encode→decode roundtrip.
    #[test]
    fn prop_sign_separate_leading_roundtrip(
        (value, digits, scale) in sign_sep_value_strategy(),
        codepage in prop_oneof![Just(Codepage::ASCII), Just(Codepage::CP037)],
    ) {
        let info = SignSeparateInfo { placement: SignPlacement::Leading };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate(&value, digits, scale, &info, codepage, &mut buffer)
            .expect("encode");
        let decoded = decode_zoned_decimal_sign_separate(&buffer, digits, scale, &info, codepage)
            .expect("decode");

        let mut buffer2 = vec![0u8; buf_len];
        encode_zoned_decimal_sign_separate(&decoded.to_string(), digits, scale, &info, codepage, &mut buffer2)
            .expect("re-encode");

        prop_assert_eq!(&buffer, &buffer2, "SIGN SEPARATE LEADING roundtrip mismatch for {}", value);
    }

    /// SIGN SEPARATE TRAILING encode→decode roundtrip.
    #[test]
    fn prop_sign_separate_trailing_roundtrip(
        (value, digits, scale) in sign_sep_value_strategy(),
        codepage in prop_oneof![Just(Codepage::ASCII), Just(Codepage::CP037)],
    ) {
        let info = SignSeparateInfo { placement: SignPlacement::Trailing };
        let buf_len = usize::from(digits) + 1;
        let mut buffer = vec![0u8; buf_len];

        encode_zoned_decimal_sign_separate(&value, digits, scale, &info, codepage, &mut buffer)
            .expect("encode");
        let decoded = decode_zoned_decimal_sign_separate(&buffer, digits, scale, &info, codepage)
            .expect("decode");

        let mut buffer2 = vec![0u8; buf_len];
        encode_zoned_decimal_sign_separate(&decoded.to_string(), digits, scale, &info, codepage, &mut buffer2)
            .expect("re-encode");

        prop_assert_eq!(&buffer, &buffer2, "SIGN SEPARATE TRAILING roundtrip mismatch for {}", value);
    }
}

// ============================================================================
// 6. Overpunch roundtrip
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Overpunch encode→decode for arbitrary digit + sign (ASCII).
    #[test]
    fn prop_overpunch_ascii_roundtrip(
        digit in 0u8..=9,
        negative in any::<bool>(),
    ) {
        let encoded = encode_overpunch_byte(digit, negative, Codepage::ASCII, ZeroSignPolicy::Positive)
            .expect("encode overpunch");
        let (decoded_digit, decoded_neg) = decode_overpunch_byte(encoded, Codepage::ASCII)
            .expect("decode overpunch");

        prop_assert_eq!(decoded_digit, digit, "digit mismatch");
        if digit == 0 {
            // Zero sign handling: both +0 and -0 decode to positive
            // depending on policy; just check digit
        } else {
            prop_assert_eq!(decoded_neg, negative, "sign mismatch for digit {}", digit);
        }
    }

    /// Overpunch encode→decode for arbitrary digit + sign (EBCDIC CP037).
    #[test]
    fn prop_overpunch_ebcdic_roundtrip(
        digit in 0u8..=9,
        negative in any::<bool>(),
    ) {
        let encoded = encode_overpunch_byte(digit, negative, Codepage::CP037, ZeroSignPolicy::Positive)
            .expect("encode overpunch");
        let (decoded_digit, decoded_neg) = decode_overpunch_byte(encoded, Codepage::CP037)
            .expect("decode overpunch");

        prop_assert_eq!(decoded_digit, digit, "digit mismatch");
        if digit == 0 {
            // Zero sign normalisation is policy-dependent
        } else {
            prop_assert_eq!(decoded_neg, negative, "sign mismatch for digit {}", digit);
        }
    }

    /// Overpunch with Preferred zero policy (EBCDIC) roundtrip.
    #[test]
    fn prop_overpunch_ebcdic_preferred_zero_roundtrip(
        digit in 0u8..=9,
        negative in any::<bool>(),
    ) {
        let encoded = encode_overpunch_byte(digit, negative, Codepage::CP037, ZeroSignPolicy::Preferred)
            .expect("encode overpunch");
        let (decoded_digit, _decoded_neg) = decode_overpunch_byte(encoded, Codepage::CP037)
            .expect("decode overpunch");

        prop_assert_eq!(decoded_digit, digit, "digit mismatch with Preferred policy");
    }
}

// ============================================================================
// 7. All types no-panic: arbitrary bytes → decode attempt → never panics
// ============================================================================

const MIXED_SCHEMA_NOPANIC: &str = "\
       01 REC.
         05 FLD-X     PIC X(10).
         05 FLD-9     PIC 9(5).
         05 FLD-S9    PIC S9(5).
         05 FLD-P     PIC S9(7)V99 COMP-3.
         05 FLD-B     PIC S9(4) BINARY.
";

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Arbitrary bytes → full decode with mixed schema → never panics.
    #[test]
    fn prop_all_types_decode_no_panic(data in proptest::collection::vec(any::<u8>(), 0..200)) {
        let schema = parse_copybook(MIXED_SCHEMA_NOPANIC).expect("static schema");
        let opts = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037);
        let result = catch_unwind(|| {
            let _ = decode_record(&schema, &data, &opts);
        });
        prop_assert!(result.is_ok(), "decode panicked on {} bytes", data.len());
    }

    /// Arbitrary bytes → packed decimal decode → never panics.
    #[test]
    fn prop_comp3_decode_no_panic(
        data in proptest::collection::vec(any::<u8>(), 1..20),
        digits in 1u16..=18,
        scale in 0i16..=4,
    ) {
        let result = catch_unwind(move || {
            let _ = decode_packed_decimal(&data, digits, scale, true);
        });
        prop_assert!(result.is_ok(), "decode_packed_decimal panicked");
    }

    /// Arbitrary bytes → binary int decode → never panics.
    #[test]
    fn prop_binary_decode_no_panic(
        data in proptest::collection::vec(any::<u8>(), 0..20),
        bits in prop_oneof![Just(16u16), Just(32u16), Just(64u16)],
        signed in any::<bool>(),
    ) {
        let result = catch_unwind(move || {
            let _ = decode_binary_int(&data, bits, signed);
        });
        prop_assert!(result.is_ok(), "decode_binary_int panicked");
    }

    /// Arbitrary bytes → zoned decimal decode → never panics.
    #[test]
    fn prop_zoned_decode_no_panic(
        data in proptest::collection::vec(any::<u8>(), 1..30),
        signed in any::<bool>(),
    ) {
        let digits = u16::try_from(data.len()).unwrap_or(1);
        let result = catch_unwind(move || {
            let _ = decode_zoned_decimal(&data, digits, 0, signed, Codepage::ASCII, false);
        });
        prop_assert!(result.is_ok(), "decode_zoned_decimal panicked");
    }

    /// Arbitrary bytes → overpunch decode → never panics.
    #[test]
    fn prop_overpunch_decode_no_panic(
        byte in any::<u8>(),
        codepage in prop_oneof![Just(Codepage::ASCII), Just(Codepage::CP037)],
    ) {
        let result = catch_unwind(move || {
            let _ = decode_overpunch_byte(byte, codepage);
        });
        prop_assert!(result.is_ok(), "decode_overpunch_byte panicked on 0x{byte:02X}");
    }
}
