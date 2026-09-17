// SPDX-License-Identifier: AGPL-3.0-or-later
//! Exhaustive independent-oracle check for the five EBCDIC decode tables.
//!
//! The expected tables come from `fixtures/codepage_oracle_tables.rs`, generated
//! offline from glibc iconv, cross-checked against CPython codecs, with disputed
//! slots pinned to the ICU UCM authority (see the fixture header). Comparing all
//! 256 decode slots plus the derived reverse mappings means swapping any pair
//! back fails here even if the encoder and decoder stay mutually consistent.
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

use copybook_charset::{Codepage, UnmappablePolicy, ebcdic_to_utf8, utf8_to_ebcdic};
use std::collections::HashMap;

#[path = "fixtures/codepage_oracle_tables.rs"]
mod oracle;

const PAGES: [(Codepage, &[u32; 256]); 5] = [
    (Codepage::CP037, &oracle::ORACLE_CP037),
    (Codepage::CP273, &oracle::ORACLE_CP273),
    (Codepage::CP500, &oracle::ORACLE_CP500),
    (Codepage::CP1047, &oracle::ORACLE_CP1047),
    (Codepage::CP1140, &oracle::ORACLE_CP1140),
];

/// Mirror of the control policy in `ebcdic_to_utf8`: raw table values below
/// U+0020 other than tab/LF/CR surface as U+FFFD under `Replace`.
fn policy_expected(raw: u32) -> char {
    if raw < 0x20 && raw != 0x09 && raw != 0x0A && raw != 0x0D {
        return '\u{FFFD}';
    }
    // The oracle-validity test below proves every fixture value is a scalar,
    // so this unwrap cannot fire on the pinned data.
    char::from_u32(raw).unwrap()
}

#[test]
fn oracle_holds_only_valid_scalars() {
    for (cp, table) in PAGES {
        for (byte, &raw) in table.iter().enumerate() {
            assert!(
                char::from_u32(raw).is_some(),
                "{cp}: oracle byte 0x{byte:02X} holds invalid scalar U+{raw:04X}"
            );
        }
    }
}

#[test]
fn oracle_decode_all_slots_all_codepages() {
    for (cp, table) in PAGES {
        for (byte, &raw) in table.iter().enumerate() {
            let expected = policy_expected(raw);
            let actual = ebcdic_to_utf8(&[byte as u8], cp, UnmappablePolicy::Replace).unwrap();
            assert_eq!(
                actual,
                expected.to_string(),
                "{cp}: byte 0x{byte:02X} must decode to U+{raw:04X}"
            );
        }
    }
}

#[test]
fn oracle_encode_matches_last_wins_reverse() {
    // Mirrors `utf8_to_ebcdic`'s reverse-table construction: later byte indexes
    // overwrite earlier ones for codepoints that appear more than once.
    for (cp, table) in PAGES {
        let mut expected_reverse: HashMap<char, u8> = HashMap::new();
        for (byte, &raw) in table.iter().enumerate() {
            // Validity is proven by `oracle_holds_only_valid_scalars`.
            let ch = char::from_u32(raw).unwrap();
            expected_reverse.insert(ch, byte as u8);
        }
        for (&ch, &byte) in &expected_reverse {
            let text = ch.to_string();
            let actual = utf8_to_ebcdic(&text, cp).unwrap();
            assert_eq!(
                actual,
                vec![byte],
                "{cp}: U+{:04X} must encode to 0x{byte:02X}",
                ch as u32
            );
        }
    }
}
