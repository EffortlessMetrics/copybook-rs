#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Zoned decimal overpunch encoding and decoding.
//!
//! This module provides centralized mapping for zoned decimal overpunch characters
//! across different codepages (ASCII and EBCDIC variants).
//!
//! Overpunch encoding combines the last digit with sign information:
//! - ASCII: uses letters A-I for positive digits 1-9, J-R for negative digits 1-9
//! - EBCDIC: uses zone nibbles 0xC (positive) and 0xD (negative)
//!
//! Use [`encode_overpunch_byte`] and [`decode_overpunch_byte`] for single-byte
//! conversion, or the lower-level zone helpers for EBCDIC-specific work.
//!
//! ### Overpunch rules (cheatsheet)
//! - ASCII last-digit:
//!   - +0..+9 → `{'`, `A`..`I`
//!   - -0..-9 → `'}'`, `J`..`R`
//! - EBCDIC last-digit zone:
//!   - Positive → `0xC`
//!   - Negative → `0xD`
//!   - Preferred-zero policy (EBCDIC) → `0xF` for zero regardless of sign

use copybook_codepage::Codepage;
use copybook_error::{Error, ErrorCode, Result};
use std::convert::TryFrom;

// Visible in tests; harmless in non-test builds. Keep pedantic quiet.
#[allow(dead_code)]
const DEFAULT_PROPTEST_CASE_COUNT: u32 = 512;

/// Policy for the sign zone nibble when the numeric value is exactly zero.
///
/// COBOL compilers differ on whether zero is positive (`0xC0`) or unsigned
/// (`0xF0`). This enum lets callers choose the convention.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZeroSignPolicy {
    /// Use positive sign for zero (C for EBCDIC, '{' for ASCII)
    Positive,
    /// Use 'F' sign for zero in EBCDIC, positive in ASCII
    Preferred,
}

/// ASCII overpunch for the *last* digit in a field.
/// +0..+9 encode to `{`, `A`..`I`; -0..-9 encode to `}`, `J`..`R`.
/// Non-final digits MUST be bare ASCII `0`..`9`; helpers enforce this.
/// Maps digit (0-9) to positive overpunch character.
static ASCII_POSITIVE_OVERPUNCH: [u8; 10] = [
    b'{', // 0 -> '{'
    b'A', // 1 -> 'A'
    b'B', // 2 -> 'B'
    b'C', // 3 -> 'C'
    b'D', // 4 -> 'D'
    b'E', // 5 -> 'E'
    b'F', // 6 -> 'F'
    b'G', // 7 -> 'G'
    b'H', // 8 -> 'H'
    b'I', // 9 -> 'I'
];

/// ASCII negative overpunch lookup table (O(1) access)
/// Maps digit (0-9) to negative overpunch character
static ASCII_NEGATIVE_OVERPUNCH: [u8; 10] = [
    b'}', // 0 -> '}'
    b'J', // 1 -> 'J'
    b'K', // 2 -> 'K'
    b'L', // 3 -> 'L'
    b'M', // 4 -> 'M'
    b'N', // 5 -> 'N'
    b'O', // 6 -> 'O'
    b'P', // 7 -> 'P'
    b'Q', // 8 -> 'Q'
    b'R', // 9 -> 'R'
];

/// ASCII overpunch decode table: byte -> (digit, `is_negative`)
/// Uses Option to handle invalid bytes
static ASCII_OVERPUNCH_DECODE: [Option<(u8, bool)>; 256] = {
    let mut table = [None; 256];

    // Positive overpunch characters
    table[b'{' as usize] = Some((0, false));
    table[b'A' as usize] = Some((1, false));
    table[b'B' as usize] = Some((2, false));
    table[b'C' as usize] = Some((3, false));
    table[b'D' as usize] = Some((4, false));
    table[b'E' as usize] = Some((5, false));
    table[b'F' as usize] = Some((6, false));
    table[b'G' as usize] = Some((7, false));
    table[b'H' as usize] = Some((8, false));
    table[b'I' as usize] = Some((9, false));

    // Negative overpunch characters
    table[b'}' as usize] = Some((0, true));
    table[b'J' as usize] = Some((1, true));
    table[b'K' as usize] = Some((2, true));
    table[b'L' as usize] = Some((3, true));
    table[b'M' as usize] = Some((4, true));
    table[b'N' as usize] = Some((5, true));
    table[b'O' as usize] = Some((6, true));
    table[b'P' as usize] = Some((7, true));
    table[b'Q' as usize] = Some((8, true));
    table[b'R' as usize] = Some((9, true));

    // Regular ASCII digits (0x30-0x39) - unsigned
    table[b'0' as usize] = Some((0, false));
    table[b'1' as usize] = Some((1, false));
    table[b'2' as usize] = Some((2, false));
    table[b'3' as usize] = Some((3, false));
    table[b'4' as usize] = Some((4, false));
    table[b'5' as usize] = Some((5, false));
    table[b'6' as usize] = Some((6, false));
    table[b'7' as usize] = Some((7, false));
    table[b'8' as usize] = Some((8, false));
    table[b'9' as usize] = Some((9, false));

    table
};

/// Encode a sign and digit into an EBCDIC overpunch zone byte.
///
/// Returns the zone nibble (high 4 bits) for the given digit and sign.
#[must_use]
#[inline]
pub fn encode_ebcdic_overpunch_zone(digit: u8, is_negative: bool, policy: ZeroSignPolicy) -> u8 {
    debug_assert!(digit <= 9, "Digit must be 0-9");

    if digit == 0 && policy == ZeroSignPolicy::Preferred {
        return 0xF; // Preferred zone for zero regardless of sign
    }

    if is_negative {
        0xD // EBCDIC negative zone
    } else {
        0xC // EBCDIC positive zone
    }
}

/// Decode an EBCDIC overpunch zone byte into its sign and digit.
///
/// Returns `None` for invalid zone nibbles.
#[must_use]
#[inline]
pub const fn decode_ebcdic_overpunch_zone(zone: u8) -> Option<(bool, bool)> {
    match zone {
        0xC | 0xF => Some((true, false)), // Positive (preferred for zero)
        0xD => Some((true, true)),        // Negative
        _ => None,                        // Invalid zone for signed field
    }
}

/// Encode overpunch byte for the given digit, sign, and codepage
///
/// # Errors
/// Returns an error if the digit is invalid or the encoding fails.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn encode_overpunch_byte(
    digit: u8,
    is_negative: bool,
    codepage: Codepage,
    policy: ZeroSignPolicy,
) -> Result<u8> {
    if digit > 9 {
        return Err(Error::new(
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
            format!("Invalid digit {digit} for overpunch encoding"),
        ));
    }

    if codepage == Codepage::ASCII {
        // O(1) ASCII overpunch lookup using direct array indexing
        if digit <= 9 {
            let byte_value = if is_negative {
                ASCII_NEGATIVE_OVERPUNCH[digit as usize]
            } else {
                ASCII_POSITIVE_OVERPUNCH[digit as usize]
            };
            Ok(byte_value)
        } else {
            Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("Invalid digit {digit} for ASCII overpunch encoding"),
            ))
        }
    } else {
        // EBCDIC: combine zone and digit nibbles
        let zone = encode_ebcdic_overpunch_zone(digit, is_negative, policy);
        Ok((zone << 4) | digit)
    }
}

/// Decode overpunch byte to extract digit and sign information
///
/// # Errors
/// Returns an error if the byte is not a valid overpunch character.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn decode_overpunch_byte(byte: u8, codepage: Codepage) -> Result<(u8, bool)> {
    if codepage == Codepage::ASCII {
        if let Some((digit, is_negative)) = ASCII_OVERPUNCH_DECODE[byte as usize] {
            Ok((digit, is_negative))
        } else {
            Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                format!("Invalid ASCII overpunch byte 0x{byte:02X}"),
            ))
        }
    } else {
        // EBCDIC: extract zone and digit nibbles
        let zone = (byte >> 4) & 0x0F;
        let digit = byte & 0x0F;

        if digit > 9 {
            return Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                format!("Invalid digit nibble 0x{digit:X} in EBCDIC overpunch byte 0x{byte:02X}"),
            ));
        }

        if let Some((is_signed, is_negative)) = decode_ebcdic_overpunch_zone(zone) {
            if !is_signed {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Unsigned zone 0x{zone:X} in signed EBCDIC field"),
                ));
            }
            Ok((digit, is_negative))
        } else {
            Err(Error::new(
                ErrorCode::CBKD411_ZONED_BAD_SIGN,
                format!("Invalid EBCDIC zone nibble 0x{zone:X} in byte 0x{byte:02X}"),
            ))
        }
    }
}

/// Returns `true` if `byte` is a recognised EBCDIC overpunch zone byte.
#[must_use]
#[inline]
pub fn is_valid_overpunch(byte: u8, codepage: Codepage) -> bool {
    if codepage == Codepage::ASCII {
        ASCII_OVERPUNCH_DECODE[byte as usize].is_some()
    } else {
        let zone = (byte >> 4) & 0x0F;
        let digit = byte & 0x0F;
        digit <= 9 && decode_ebcdic_overpunch_zone(zone).is_some()
    }
}

/// Returns every EBCDIC byte value that is a valid overpunch zone.
#[must_use]
#[inline]
pub fn get_all_valid_overpunch_bytes(codepage: Codepage) -> Vec<u8> {
    if codepage == Codepage::ASCII {
        ASCII_OVERPUNCH_DECODE
            .iter()
            .enumerate()
            .filter_map(|(byte, mapping)| mapping.and_then(|_| u8::try_from(byte).ok()))
            .collect()
    } else {
        let mut bytes = Vec::new();
        for zone in [0xC, 0xD, 0xF] {
            for digit in 0..=9 {
                bytes.push((zone << 4) | digit);
            }
        }
        bytes
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest::test_runner::RngSeed;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    fn proptest_case_count() -> u32 {
        option_env!("PROPTEST_CASES")
            .and_then(|s| s.parse().ok())
            .unwrap_or(DEFAULT_PROPTEST_CASE_COUNT)
    }

    fn zoned_overpunch_proptest_config() -> ProptestConfig {
        let mut cfg = ProptestConfig {
            cases: proptest_case_count(),
            max_shrink_time: 0,
            ..ProptestConfig::default()
        };

        if let Ok(seed_value) = std::env::var("PROPTEST_SEED")
            && !seed_value.is_empty()
        {
            let parsed_seed = seed_value.parse::<u64>().unwrap_or_else(|_| {
                let mut hasher = DefaultHasher::new();
                seed_value.hash(&mut hasher);
                hasher.finish()
            });
            cfg.rng_seed = RngSeed::Fixed(parsed_seed);
        }

        cfg
    }

    proptest! {
        #![proptest_config(zoned_overpunch_proptest_config())]
        #[test]
        fn prop_encode_decode_parity(
            digit in 0u8..=9,
            is_negative in any::<bool>(),
            codepage in prop_oneof![
                Just(Codepage::ASCII),
                Just(Codepage::CP037),
                Just(Codepage::CP273),
                Just(Codepage::CP500),
                Just(Codepage::CP1047),
                Just(Codepage::CP1140),
            ],
            policy in prop_oneof![Just(ZeroSignPolicy::Positive), Just(ZeroSignPolicy::Preferred)],
        ) {
            let encoded = encode_overpunch_byte(digit, is_negative, codepage, policy)
                .expect("encoding should succeed for digits 0-9");

            prop_assert!(is_valid_overpunch(encoded, codepage));

            let (decoded_digit, decoded_negative) =
                decode_overpunch_byte(encoded, codepage).expect("decode must succeed for encoded byte");

            prop_assert_eq!(decoded_digit, digit);

            let expected_negative = if codepage.is_ebcdic()
                && policy == ZeroSignPolicy::Preferred
                && digit == 0
            {
                // Preferred-zero policy always decodes as positive regardless of requested sign
                false
            } else {
                is_negative
            };

            prop_assert_eq!(decoded_negative, expected_negative);
        }
    }

    #[test]
    fn test_ascii_overpunch_encode_decode() {
        // Test positive digits
        for digit in 0..=9 {
            let encoded =
                encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive)
                    .expect("Failed to encode positive digit");
            let (decoded_digit, is_negative) = decode_overpunch_byte(encoded, Codepage::ASCII)
                .expect("Failed to decode positive digit");

            assert_eq!(decoded_digit, digit);
            assert!(!is_negative);
        }

        // Test negative digits
        for digit in 0..=9 {
            let encoded =
                encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive)
                    .expect("Failed to encode negative digit");
            let (decoded_digit, is_negative) = decode_overpunch_byte(encoded, Codepage::ASCII)
                .expect("Failed to decode negative digit");

            assert_eq!(decoded_digit, digit);
            assert!(is_negative);
        }
    }

    #[test]
    fn test_ebcdic_overpunch_encode_decode() {
        // Test positive digits with positive policy
        for digit in 0..=9 {
            let encoded =
                encode_overpunch_byte(digit, false, Codepage::CP037, ZeroSignPolicy::Positive)
                    .expect("Failed to encode positive digit");
            let (decoded_digit, is_negative) = decode_overpunch_byte(encoded, Codepage::CP037)
                .expect("Failed to decode positive digit");

            assert_eq!(decoded_digit, digit);
            assert!(!is_negative);

            // Check zone nibble is 0xC for positive
            let zone = (encoded >> 4) & 0x0F;
            if digit != 0 {
                assert_eq!(zone, 0xC);
            }
        }

        // Test negative digits
        for digit in 0..=9 {
            let encoded =
                encode_overpunch_byte(digit, true, Codepage::CP037, ZeroSignPolicy::Positive)
                    .expect("Failed to encode negative digit");
            let (decoded_digit, is_negative) = decode_overpunch_byte(encoded, Codepage::CP037)
                .expect("Failed to decode negative digit");

            assert_eq!(decoded_digit, digit);
            assert!(is_negative);

            // Check zone nibble is 0xD for negative
            let zone = (encoded >> 4) & 0x0F;
            assert_eq!(zone, 0xD);
        }
    }

    #[test]
    fn test_zero_sign_policies() {
        // Test positive policy for zero
        let encoded_pos =
            encode_overpunch_byte(0, false, Codepage::CP037, ZeroSignPolicy::Positive)
                .expect("Failed to encode zero with positive policy");
        let zone_pos = (encoded_pos >> 4) & 0x0F;
        assert_eq!(zone_pos, 0xC);

        // Test preferred policy for zero (should use 0xF regardless of sign)
        let encoded_pref_pos =
            encode_overpunch_byte(0, false, Codepage::CP037, ZeroSignPolicy::Preferred)
                .expect("Failed to encode zero with preferred policy");
        let zone_pref_pos = (encoded_pref_pos >> 4) & 0x0F;
        assert_eq!(zone_pref_pos, 0xF);

        let encoded_pref_neg =
            encode_overpunch_byte(0, true, Codepage::CP037, ZeroSignPolicy::Preferred)
                .expect("Failed to encode negative zero with preferred policy");
        let zone_pref_neg = (encoded_pref_neg >> 4) & 0x0F;
        assert_eq!(zone_pref_neg, 0xF);
    }

    #[test]
    fn test_invalid_inputs() {
        // Test invalid digit
        let result = encode_overpunch_byte(10, false, Codepage::ASCII, ZeroSignPolicy::Positive);
        assert!(result.is_err());

        // Test invalid ASCII byte
        let result = decode_overpunch_byte(0xFF, Codepage::ASCII);
        assert!(result.is_err());

        // Test invalid EBCDIC zone
        let invalid_ebcdic = 0x1F; // Zone 0x1, digit 0xF
        let result = decode_overpunch_byte(invalid_ebcdic, Codepage::CP037);
        assert!(result.is_err());
    }

    #[test]
    fn test_ascii_overpunch_golden_values() {
        // Test specific ASCII overpunch values
        assert_eq!(
            decode_overpunch_byte(b'{', Codepage::ASCII).unwrap(),
            (0, false)
        ); // +0
        assert_eq!(
            decode_overpunch_byte(b'A', Codepage::ASCII).unwrap(),
            (1, false)
        ); // +1
        assert_eq!(
            decode_overpunch_byte(b'I', Codepage::ASCII).unwrap(),
            (9, false)
        ); // +9
        assert_eq!(
            decode_overpunch_byte(b'}', Codepage::ASCII).unwrap(),
            (0, true)
        ); // -0
        assert_eq!(
            decode_overpunch_byte(b'J', Codepage::ASCII).unwrap(),
            (1, true)
        ); // -1
        assert_eq!(
            decode_overpunch_byte(b'R', Codepage::ASCII).unwrap(),
            (9, true)
        ); // -9
    }

    #[test]
    fn test_ebcdic_overpunch_golden_values() {
        // Test specific EBCDIC overpunch values
        assert_eq!(
            decode_overpunch_byte(0xC0, Codepage::CP037).unwrap(),
            (0, false)
        ); // +0
        assert_eq!(
            decode_overpunch_byte(0xC1, Codepage::CP037).unwrap(),
            (1, false)
        ); // +1
        assert_eq!(
            decode_overpunch_byte(0xC9, Codepage::CP037).unwrap(),
            (9, false)
        ); // +9
        assert_eq!(
            decode_overpunch_byte(0xD0, Codepage::CP037).unwrap(),
            (0, true)
        ); // -0
        assert_eq!(
            decode_overpunch_byte(0xD1, Codepage::CP037).unwrap(),
            (1, true)
        ); // -1
        assert_eq!(
            decode_overpunch_byte(0xD9, Codepage::CP037).unwrap(),
            (9, true)
        ); // -9
        assert_eq!(
            decode_overpunch_byte(0xF0, Codepage::CP037).unwrap(),
            (0, false)
        ); // Preferred 0
    }

    #[test]
    fn test_is_valid_overpunch() {
        // ASCII tests
        assert!(is_valid_overpunch(b'A', Codepage::ASCII));
        assert!(is_valid_overpunch(b'J', Codepage::ASCII));
        assert!(is_valid_overpunch(b'{', Codepage::ASCII));
        assert!(is_valid_overpunch(b'0', Codepage::ASCII)); // Regular digit
        assert!(!is_valid_overpunch(b'Z', Codepage::ASCII));

        // EBCDIC tests
        assert!(is_valid_overpunch(0xC0, Codepage::CP037));
        assert!(is_valid_overpunch(0xD9, Codepage::CP037));
        assert!(is_valid_overpunch(0xF5, Codepage::CP037));
        assert!(!is_valid_overpunch(0x1A, Codepage::CP037)); // Invalid zone
        assert!(!is_valid_overpunch(0xCA, Codepage::CP037)); // Invalid digit
    }

    #[test]
    fn test_get_all_valid_overpunch_bytes() {
        let ascii_bytes = get_all_valid_overpunch_bytes(Codepage::ASCII);
        assert!(ascii_bytes.len() >= 30); // At least 20 overpunch + 10 regular digits
        assert!(ascii_bytes.contains(&b'A'));
        assert!(ascii_bytes.contains(&b'J'));
        assert!(ascii_bytes.contains(&b'0'));

        let ebcdic_bytes = get_all_valid_overpunch_bytes(Codepage::CP037);
        assert_eq!(ebcdic_bytes.len(), 30); // 3 zones * 10 digits each
        assert!(ebcdic_bytes.contains(&0xC0));
        assert!(ebcdic_bytes.contains(&0xD9));
        assert!(ebcdic_bytes.contains(&0xF5));
    }

    // ── Exhaustive overpunch tests ──────────────────────────────────────

    #[test]
    fn test_ascii_decode_each_positive_overpunch_char() {
        let expected: [(u8, u8); 10] = [
            (b'{', 0),
            (b'A', 1),
            (b'B', 2),
            (b'C', 3),
            (b'D', 4),
            (b'E', 5),
            (b'F', 6),
            (b'G', 7),
            (b'H', 8),
            (b'I', 9),
        ];
        for (byte, digit) in expected {
            let (d, neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
            assert_eq!(d, digit, "positive overpunch char 0x{byte:02X}");
            assert!(
                !neg,
                "positive overpunch char 0x{byte:02X} should not be negative"
            );
        }
    }

    #[test]
    fn test_ascii_decode_each_negative_overpunch_char() {
        let expected: [(u8, u8); 10] = [
            (b'}', 0),
            (b'J', 1),
            (b'K', 2),
            (b'L', 3),
            (b'M', 4),
            (b'N', 5),
            (b'O', 6),
            (b'P', 7),
            (b'Q', 8),
            (b'R', 9),
        ];
        for (byte, digit) in expected {
            let (d, neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
            assert_eq!(d, digit, "negative overpunch char 0x{byte:02X}");
            assert!(
                neg,
                "negative overpunch char 0x{byte:02X} should be negative"
            );
        }
    }

    #[test]
    fn test_ascii_encode_each_digit_positive() {
        let expected_bytes: [u8; 10] = [b'{', b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I'];
        for digit in 0u8..=9 {
            let enc =
                encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive)
                    .unwrap();
            assert_eq!(
                enc, expected_bytes[digit as usize],
                "encode positive digit {digit}"
            );
        }
    }

    #[test]
    fn test_ascii_encode_each_digit_negative() {
        let expected_bytes: [u8; 10] = [b'}', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R'];
        for digit in 0u8..=9 {
            let enc = encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive)
                .unwrap();
            assert_eq!(
                enc, expected_bytes[digit as usize],
                "encode negative digit {digit}"
            );
        }
    }

    #[test]
    fn test_all_ebcdic_codepage_variants_encode_decode() {
        let ebcdic_codepages = [
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];
        for cp in ebcdic_codepages {
            for digit in 0u8..=9 {
                for is_negative in [false, true] {
                    let enc =
                        encode_overpunch_byte(digit, is_negative, cp, ZeroSignPolicy::Positive)
                            .unwrap_or_else(|e| {
                                panic!("{cp:?} digit={digit} neg={is_negative}: {e}")
                            });
                    let (d, neg) = decode_overpunch_byte(enc, cp)
                        .unwrap_or_else(|e| panic!("{cp:?} byte=0x{enc:02X}: {e}"));
                    assert_eq!(d, digit, "{cp:?} digit={digit} neg={is_negative}");
                    assert_eq!(neg, is_negative, "{cp:?} digit={digit} neg={is_negative}");
                }
            }
        }
    }

    #[test]
    fn test_ebcdic_zone_nibbles_all_codepages() {
        let ebcdic_codepages = [
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];
        for cp in ebcdic_codepages {
            for digit in 0u8..=9 {
                let pos =
                    encode_overpunch_byte(digit, false, cp, ZeroSignPolicy::Positive).unwrap();
                assert_eq!(
                    (pos >> 4) & 0x0F,
                    0xC,
                    "{cp:?} positive zone for digit {digit}"
                );
                assert_eq!(
                    pos & 0x0F,
                    digit,
                    "{cp:?} positive digit nibble for {digit}"
                );

                let neg = encode_overpunch_byte(digit, true, cp, ZeroSignPolicy::Positive).unwrap();
                assert_eq!(
                    (neg >> 4) & 0x0F,
                    0xD,
                    "{cp:?} negative zone for digit {digit}"
                );
                assert_eq!(
                    neg & 0x0F,
                    digit,
                    "{cp:?} negative digit nibble for {digit}"
                );
            }
        }
    }

    #[test]
    fn test_invalid_ascii_overpunch_bytes() {
        let invalid_bytes: [u8; 10] = [b'S', b'T', b'Z', b'a', b'z', b'!', b'@', b'#', 0x00, 0xFF];
        for byte in invalid_bytes {
            let result = decode_overpunch_byte(byte, Codepage::ASCII);
            assert!(
                result.is_err(),
                "byte 0x{byte:02X} should be invalid ASCII overpunch"
            );
            assert!(!is_valid_overpunch(byte, Codepage::ASCII));
        }
    }

    #[test]
    fn test_invalid_ebcdic_overpunch_bytes() {
        // Invalid zone nibbles (not 0xC, 0xD, or 0xF)
        let invalid_zones: [u8; 6] = [0x0, 0x1, 0x2, 0xA, 0xB, 0xE];
        for zone in invalid_zones {
            let byte = (zone << 4) | 0x05; // digit 5 with invalid zone
            let result = decode_overpunch_byte(byte, Codepage::CP037);
            assert!(
                result.is_err(),
                "zone 0x{zone:X} should be invalid for EBCDIC"
            );
            assert!(!is_valid_overpunch(byte, Codepage::CP037));
        }
        // Invalid digit nibbles (> 9)
        for digit_nibble in 0xAu8..=0xF {
            let byte = (0xC << 4) | digit_nibble; // positive zone with invalid digit
            let result = decode_overpunch_byte(byte, Codepage::CP037);
            assert!(
                result.is_err(),
                "digit nibble 0x{digit_nibble:X} should be invalid"
            );
            assert!(!is_valid_overpunch(byte, Codepage::CP037));
        }
    }

    #[test]
    fn test_encode_invalid_digit_rejected() {
        for digit in [10u8, 11, 15, 99, 255] {
            let r1 = encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive);
            assert!(r1.is_err(), "digit {digit} should be rejected for ASCII");
            let r2 = encode_overpunch_byte(digit, false, Codepage::CP037, ZeroSignPolicy::Positive);
            assert!(r2.is_err(), "digit {digit} should be rejected for EBCDIC");
        }
    }

    #[test]
    fn test_ascii_round_trip_all_digit_sign_combinations() {
        for digit in 0u8..=9 {
            for is_negative in [false, true] {
                let enc = encode_overpunch_byte(
                    digit,
                    is_negative,
                    Codepage::ASCII,
                    ZeroSignPolicy::Positive,
                )
                .unwrap();
                let (d, neg) = decode_overpunch_byte(enc, Codepage::ASCII).unwrap();
                assert_eq!(d, digit);
                assert_eq!(neg, is_negative);
            }
        }
    }

    #[test]
    fn test_ebcdic_preferred_zero_round_trip_all_codepages() {
        let ebcdic_codepages = [
            Codepage::CP037,
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ];
        for cp in ebcdic_codepages {
            // Preferred zero: both pos and neg zero encode to 0xF0, decode as positive
            let enc_pos = encode_overpunch_byte(0, false, cp, ZeroSignPolicy::Preferred).unwrap();
            let enc_neg = encode_overpunch_byte(0, true, cp, ZeroSignPolicy::Preferred).unwrap();
            assert_eq!(enc_pos, 0xF0, "{cp:?} preferred +0");
            assert_eq!(enc_neg, 0xF0, "{cp:?} preferred -0");

            let (d_pos, neg_pos) = decode_overpunch_byte(enc_pos, cp).unwrap();
            assert_eq!(d_pos, 0);
            assert!(!neg_pos, "{cp:?} preferred zero decodes as positive");

            // Non-zero digits should not be affected by preferred policy
            for digit in 1u8..=9 {
                let enc =
                    encode_overpunch_byte(digit, true, cp, ZeroSignPolicy::Preferred).unwrap();
                let zone = (enc >> 4) & 0x0F;
                assert_eq!(
                    zone, 0xD,
                    "{cp:?} preferred policy should not affect digit {digit}"
                );
            }
        }
    }

    #[test]
    fn test_positive_and_negative_zero_ascii() {
        // Positive zero
        let enc_pos =
            encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(enc_pos, b'{');
        let (d, neg) = decode_overpunch_byte(enc_pos, Codepage::ASCII).unwrap();
        assert_eq!(d, 0);
        assert!(!neg);

        // Negative zero
        let enc_neg =
            encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(enc_neg, b'}');
        let (d, neg) = decode_overpunch_byte(enc_neg, Codepage::ASCII).unwrap();
        assert_eq!(d, 0);
        assert!(neg);
    }

    #[test]
    fn test_positive_and_negative_zero_ebcdic() {
        // Positive zero with Positive policy -> zone 0xC
        let enc =
            encode_overpunch_byte(0, false, Codepage::CP037, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(enc, 0xC0);
        let (d, neg) = decode_overpunch_byte(enc, Codepage::CP037).unwrap();
        assert_eq!(d, 0);
        assert!(!neg);

        // Negative zero with Positive policy -> zone 0xD
        let enc =
            encode_overpunch_byte(0, true, Codepage::CP037, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(enc, 0xD0);
        let (d, neg) = decode_overpunch_byte(enc, Codepage::CP037).unwrap();
        assert_eq!(d, 0);
        assert!(neg);

        // Preferred zero (either sign) -> zone 0xF, decodes positive
        let enc =
            encode_overpunch_byte(0, true, Codepage::CP037, ZeroSignPolicy::Preferred).unwrap();
        assert_eq!(enc, 0xF0);
        let (d, neg) = decode_overpunch_byte(enc, Codepage::CP037).unwrap();
        assert_eq!(d, 0);
        assert!(!neg, "preferred zero always decodes as positive");
    }

    #[test]
    fn test_ascii_regular_digits_decode_as_unsigned_positive() {
        // Plain ASCII digits 0-9 are valid overpunch and decode as positive
        for digit in 0u8..=9 {
            let byte = b'0' + digit;
            let (d, neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
            assert_eq!(d, digit);
            assert!(!neg, "plain digit {digit} should decode as positive");
            assert!(is_valid_overpunch(byte, Codepage::ASCII));
        }
    }

    #[test]
    fn test_ebcdic_f_zone_digits_decode_as_positive() {
        // 0xF zone (unsigned/preferred) decodes as positive for all digits
        for digit in 0u8..=9 {
            let byte = 0xF0 | digit;
            let (d, neg) = decode_overpunch_byte(byte, Codepage::CP037).unwrap();
            assert_eq!(d, digit);
            assert!(!neg, "F-zone digit {digit} should decode as positive");
        }
    }

    #[test]
    fn test_boundary_single_digit_encode_decode() {
        // Boundary: smallest (0) and largest (9) single digit
        for &(digit, is_neg) in &[(0u8, false), (0, true), (9, false), (9, true)] {
            let ascii_enc =
                encode_overpunch_byte(digit, is_neg, Codepage::ASCII, ZeroSignPolicy::Positive)
                    .unwrap();
            let (d, n) = decode_overpunch_byte(ascii_enc, Codepage::ASCII).unwrap();
            assert_eq!(d, digit);
            assert_eq!(n, is_neg);

            let ebcdic_enc =
                encode_overpunch_byte(digit, is_neg, Codepage::CP037, ZeroSignPolicy::Positive)
                    .unwrap();
            let (d, n) = decode_overpunch_byte(ebcdic_enc, Codepage::CP037).unwrap();
            assert_eq!(d, digit);
            assert_eq!(n, is_neg);
        }
    }

    #[test]
    fn test_ascii_preferred_zero_policy_has_no_effect() {
        // ASCII ignores ZeroSignPolicy – the encode always uses the sign tables
        let pos =
            encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Preferred).unwrap();
        let neg =
            encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Preferred).unwrap();
        assert_eq!(pos, b'{', "ASCII preferred +0 should still be '{{' ");
        assert_eq!(neg, b'}', "ASCII preferred -0 should still be '}}' ");
    }
}
