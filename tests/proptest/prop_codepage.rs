// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for codepage and charset conversion.
//!
//! Tests that EBCDIC ↔ UTF-8 roundtrips preserve data for mappable characters,
//! that different codepages yield different encodings, and that no input causes
//! a panic.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use copybook_codepage::space_byte;
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

/// All EBCDIC codepage variants for property testing.
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
// Roundtrip: printable ASCII chars → EBCDIC → UTF-8 == original
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Printable ASCII characters survive a UTF-8 → EBCDIC → UTF-8 roundtrip
    /// because all supported EBCDIC codepages map the basic Latin set.
    #[test]
    fn prop_printable_ascii_roundtrip(
        text in "[A-Za-z0-9 .]{1,64}",
        codepage in ebcdic_codepage_strategy(),
    ) {
        let ebcdic = utf8_to_ebcdic(&text, codepage)
            .expect("printable ASCII must encode");
        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("roundtrip must decode");
        prop_assert_eq!(back, text);
    }

    /// Single printable ASCII byte → EBCDIC → UTF-8 roundtrip.
    #[test]
    fn prop_single_char_roundtrip(
        ch in prop::char::range('A', 'Z'),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let s = ch.to_string();
        let ebcdic = utf8_to_ebcdic(&s, codepage)
            .expect("single uppercase letter must encode");
        let back = ebcdic_to_utf8(&ebcdic, codepage, UnmappablePolicy::Error)
            .expect("roundtrip decode");
        prop_assert_eq!(back, s);
    }
}

// ---------------------------------------------------------------------------
// Different codepages handle same byte differently
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding the same arbitrary EBCDIC byte slice with Replace policy
    /// through two different codepages may yield different strings; at minimum,
    /// neither call panics.
    #[test]
    fn prop_different_codepages_no_panic(
        data in proptest::collection::vec(any::<u8>(), 1..128),
    ) {
        for &cp in &EBCDIC_CODEPAGES {
            let _ = ebcdic_to_utf8(&data, cp, UnmappablePolicy::Replace);
        }
    }

    /// CP037 and CP1047 differ on at least some byte values, so decoding
    /// the full 0x00..=0xFF range with Replace policy should produce
    /// different strings.
    #[test]
    fn prop_cp037_vs_cp1047_differ_on_full_range(_dummy in Just(())) {
        let all_bytes: Vec<u8> = (0u8..=255).collect();
        let s037 = ebcdic_to_utf8(&all_bytes, Codepage::CP037, UnmappablePolicy::Replace)
            .expect("replace never errors");
        let s1047 = ebcdic_to_utf8(&all_bytes, Codepage::CP1047, UnmappablePolicy::Replace)
            .expect("replace never errors");
        prop_assert_ne!(s037, s1047, "CP037 and CP1047 must differ somewhere");
    }
}

// ---------------------------------------------------------------------------
// Invalid/arbitrary byte handling never panics
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Arbitrary byte slice → ebcdic_to_utf8 with Replace policy never panics.
    #[test]
    fn prop_ebcdic_to_utf8_replace_never_panics(
        data in proptest::collection::vec(any::<u8>(), 0..256),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let result = ebcdic_to_utf8(&data, codepage, UnmappablePolicy::Replace);
        prop_assert!(result.is_ok(), "Replace policy should not error");
    }

    /// Arbitrary byte slice → ebcdic_to_utf8 with Skip policy never panics.
    #[test]
    fn prop_ebcdic_to_utf8_skip_never_panics(
        data in proptest::collection::vec(any::<u8>(), 0..256),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let result = ebcdic_to_utf8(&data, codepage, UnmappablePolicy::Skip);
        prop_assert!(result.is_ok(), "Skip policy should not error");
    }

    /// Arbitrary byte slice → ebcdic_to_utf8 with Error policy returns Ok or
    /// Err but never panics.
    #[test]
    fn prop_ebcdic_to_utf8_error_policy_never_panics(
        data in proptest::collection::vec(any::<u8>(), 0..256),
        codepage in ebcdic_codepage_strategy(),
    ) {
        let _ = ebcdic_to_utf8(&data, codepage, UnmappablePolicy::Error);
    }
}

// ---------------------------------------------------------------------------
// Space byte consistency
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// For every EBCDIC codepage, the space byte 0x40 decodes to U+0020 (space).
    #[test]
    fn prop_space_byte_decodes_to_space(
        codepage in ebcdic_codepage_strategy(),
    ) {
        let sp = space_byte(codepage);
        let decoded = ebcdic_to_utf8(&[sp], codepage, UnmappablePolicy::Error)
            .expect("space byte must decode");
        prop_assert_eq!(decoded, " ");
    }

    /// Encoding a single space character yields the canonical space byte
    /// for each EBCDIC codepage.
    #[test]
    fn prop_space_encodes_to_space_byte(
        codepage in ebcdic_codepage_strategy(),
    ) {
        let ebcdic = utf8_to_ebcdic(" ", codepage)
            .expect("space must encode");
        prop_assert_eq!(ebcdic.len(), 1);
        prop_assert_eq!(ebcdic[0], space_byte(codepage));
    }
}
