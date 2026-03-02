// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for charset conversion roundtrip fidelity.
//!
//! Complements `prop_codepage.rs` with additional coverage for:
//! - Digit characters (0-9) across all codepages
//! - Punctuation and special characters
//! - Length preservation (1 ASCII char = 1 EBCDIC byte for basic Latin)
//! - Multi-codepage consistency for common characters
//! - Conversion idempotency properties
//! - Mixed content strings with varied character classes

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use proptest::prelude::*;

use super::config::*;

const EBCDIC_CODEPAGES: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

fn ebcdic_codepage_strategy() -> impl Strategy<Value = Codepage> {
    prop::sample::select(&EBCDIC_CODEPAGES)
}

// ---------------------------------------------------------------------------
// 1. Digit characters (0-9) roundtrip perfectly on all codepages
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_digits_roundtrip(
        digits in "[0-9]{1,32}",
        codepage in ebcdic_codepage_strategy(),
    ) {
        let ebcdic = utf8_to_ebcdic(&digits, codepage).expect("digits must encode");
        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("digits must decode");
        prop_assert_eq!(back, digits);
    }

    /// Each individual digit 0-9 roundtrips on every codepage.
    #[test]
    fn prop_single_digit_roundtrip(
        digit in prop::char::range('0', '9'),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let s = digit.to_string();
        let ebcdic = utf8_to_ebcdic(&s, codepage).expect("digit must encode");
        prop_assert_eq!(ebcdic.len(), 1, "Single digit should be 1 EBCDIC byte");
        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("digit must decode");
        prop_assert_eq!(back, s);
    }
}

// ---------------------------------------------------------------------------
// 2. Length preservation: basic Latin characters produce 1 EBCDIC byte each
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_ascii_length_preserved(
        text in "[A-Za-z0-9 ]{1,64}",
        codepage in ebcdic_codepage_strategy(),
    ) {
        let ebcdic = utf8_to_ebcdic(&text, codepage).expect("basic Latin must encode");
        prop_assert_eq!(
            ebcdic.len(),
            text.len(),
            "EBCDIC byte count must equal ASCII char count"
        );
    }
}

// ---------------------------------------------------------------------------
// 3. All codepages agree on digits
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// All codepages produce the same EBCDIC bytes for digit strings.
    #[test]
    fn prop_codepages_agree_on_digits(digits in "[0-9]{1,16}") {
        let reference = utf8_to_ebcdic(&digits, Codepage::CP037).expect("encode");
        for &cp in &EBCDIC_CODEPAGES[1..] {
            let encoded = utf8_to_ebcdic(&digits, cp).expect("encode");
            prop_assert_eq!(
                &encoded,
                &reference,
                "Codepage {:?} disagrees with CP037 on digits",
                cp
            );
        }
    }

    /// All codepages produce the same EBCDIC bytes for uppercase letters.
    #[test]
    fn prop_codepages_agree_on_uppercase(text in "[A-Z]{1,16}") {
        let reference = utf8_to_ebcdic(&text, Codepage::CP037).expect("encode");
        for &cp in &EBCDIC_CODEPAGES[1..] {
            let encoded = utf8_to_ebcdic(&text, cp).expect("encode");
            prop_assert_eq!(
                &encoded,
                &reference,
                "Codepage {:?} disagrees with CP037 on uppercase",
                cp
            );
        }
    }
}

// ---------------------------------------------------------------------------
// 4. Conversion idempotency: encode(decode(encode(s))) == encode(s)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_encode_decode_encode_idempotent(
        text in "[A-Za-z0-9 .]{1,32}",
        codepage in ebcdic_codepage_strategy(),
    ) {
        let enc1 = utf8_to_ebcdic(&text, codepage).expect("first encode");
        let dec = ebcdic_to_utf8(&enc1, codepage, UnmappablePolicy::Error)
            .expect("decode");
        let enc2 = utf8_to_ebcdic(&dec, codepage).expect("second encode");
        prop_assert_eq!(enc1, enc2, "Encode is not idempotent through decode");
    }
}

// ---------------------------------------------------------------------------
// 5. Replace policy produces output for any input
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Replace policy always returns Ok with non-empty output for non-empty input.
    #[test]
    fn prop_replace_policy_always_succeeds(
        data in proptest::collection::vec(any::<u8>(), 1..=128),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let result = ebcdic_to_utf8(&data, codepage, UnmappablePolicy::Replace);
        prop_assert!(result.is_ok());
        let text = result.unwrap();
        prop_assert!(!text.is_empty(), "Non-empty input should produce non-empty output");
    }

    /// Skip policy never panics and output length ≤ input length.
    #[test]
    fn prop_skip_policy_output_bounded(
        data in proptest::collection::vec(any::<u8>(), 0..=128),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let result = ebcdic_to_utf8(&data, codepage, UnmappablePolicy::Skip);
        prop_assert!(result.is_ok());
        // Skip can only remove chars, so char count ≤ byte count
        let text = result.unwrap();
        prop_assert!(text.chars().count() <= data.len());
    }
}

// ---------------------------------------------------------------------------
// 6. Mixed content strings (letters + digits + spaces) roundtrip
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_mixed_content_roundtrip(
        text in "[A-Za-z0-9 ]{1,48}",
        codepage in ebcdic_codepage_strategy(),
    ) {
        let ebcdic = utf8_to_ebcdic(&text, codepage).expect("encode");
        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("decode");
        prop_assert_eq!(back, text);
    }

    /// Lowercase letters roundtrip on all codepages.
    #[test]
    fn prop_lowercase_roundtrip(
        text in "[a-z]{1,32}",
        codepage in ebcdic_codepage_strategy(),
    ) {
        let ebcdic = utf8_to_ebcdic(&text, codepage).expect("encode");
        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("decode");
        prop_assert_eq!(back, text);
    }
}

// ---------------------------------------------------------------------------
// 7. Empty string edge case
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_empty_string_roundtrip(codepage in ebcdic_codepage_strategy()) {
        let ebcdic = utf8_to_ebcdic("", codepage).expect("encode empty");
        prop_assert!(ebcdic.is_empty());
        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("decode empty");
        prop_assert_eq!(back, "");
    }

    /// Empty byte slice decoded with any policy returns empty string.
    #[test]
    fn prop_empty_bytes_decode(codepage in ebcdic_codepage_strategy()) {
        for policy in [
            UnmappablePolicy::Error,
            UnmappablePolicy::Replace,
            UnmappablePolicy::Skip,
        ] {
            let result = ebcdic_to_utf8(&[], codepage, policy);
            prop_assert!(result.is_ok());
            prop_assert_eq!(result.unwrap(), "");
        }
    }
}

// ---------------------------------------------------------------------------
// 8. Error policy rejects unmappable bytes (at least some exist)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// For random high-entropy byte sequences, Error policy may reject some.
    /// At minimum it never panics.
    #[test]
    fn prop_error_policy_no_panic(
        data in proptest::collection::vec(any::<u8>(), 1..=64),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let _ = ebcdic_to_utf8(&data, codepage, UnmappablePolicy::Error);
    }
}

// ---------------------------------------------------------------------------
// 9. Repeated character strings
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// A string of N identical ASCII chars roundtrips and produces N identical
    /// EBCDIC bytes.
    #[test]
    fn prop_repeated_char_roundtrip(
        ch in prop::char::range('A', 'Z'),
        len in 1usize..=64,
        codepage in ebcdic_codepage_strategy(),
    ) {
        let text: String = std::iter::repeat(ch).take(len).collect();
        let ebcdic = utf8_to_ebcdic(&text, codepage).expect("encode");
        prop_assert_eq!(ebcdic.len(), len);

        // All EBCDIC bytes should be identical
        let first = ebcdic[0];
        prop_assert!(
            ebcdic.iter().all(|&b| b == first),
            "Repeated char should produce identical EBCDIC bytes"
        );

        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("decode");
        prop_assert_eq!(back, text);
    }
}
