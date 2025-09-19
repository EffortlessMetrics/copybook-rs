//! Zoned decimal overpunch encoding and decoding
//!
//! This module provides centralized mapping for zoned decimal overpunch characters
//! across different codepages (ASCII and EBCDIC variants).
//!
//! Overpunch encoding combines the last digit with sign information:
//! - ASCII: uses letters A-I for positive digits 1-9, J-R for negative digits 1-9
//! - EBCDIC: uses zone nibbles 0xC (positive) and 0xD (negative)

use crate::options::Codepage;
use copybook_core::{Error, ErrorCode, Result};

/// Zero sign policy for overpunch encoding
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZeroSignPolicy {
    /// Use positive sign for zero (C for EBCDIC, '{' for ASCII)
    Positive,
    /// Use 'F' sign for zero in EBCDIC, positive in ASCII
    Preferred,
}

/// Overpunch mapping table entry: (digit, sign, codepage) -> byte
#[derive(Debug, Clone, Copy)]
pub struct OverpunchMapping {
    /// The digit value (0-9)
    pub digit: u8,
    /// Whether this represents a negative value
    pub is_negative: bool,
    /// The encoded byte value
    pub byte_value: u8,
}

/// ASCII overpunch mapping table
/// Maps digit and sign to overpunch character
static ASCII_OVERPUNCH_ENCODE: [OverpunchMapping; 20] = [
    // Positive digits 0-9
    OverpunchMapping {
        digit: 0,
        is_negative: false,
        byte_value: b'{',
    }, // 0x7B
    OverpunchMapping {
        digit: 1,
        is_negative: false,
        byte_value: b'A',
    }, // 0x41
    OverpunchMapping {
        digit: 2,
        is_negative: false,
        byte_value: b'B',
    }, // 0x42
    OverpunchMapping {
        digit: 3,
        is_negative: false,
        byte_value: b'C',
    }, // 0x43
    OverpunchMapping {
        digit: 4,
        is_negative: false,
        byte_value: b'D',
    }, // 0x44
    OverpunchMapping {
        digit: 5,
        is_negative: false,
        byte_value: b'E',
    }, // 0x45
    OverpunchMapping {
        digit: 6,
        is_negative: false,
        byte_value: b'F',
    }, // 0x46
    OverpunchMapping {
        digit: 7,
        is_negative: false,
        byte_value: b'G',
    }, // 0x47
    OverpunchMapping {
        digit: 8,
        is_negative: false,
        byte_value: b'H',
    }, // 0x48
    OverpunchMapping {
        digit: 9,
        is_negative: false,
        byte_value: b'I',
    }, // 0x49
    // Negative digits 0-9
    OverpunchMapping {
        digit: 0,
        is_negative: true,
        byte_value: b'}',
    }, // 0x7D
    OverpunchMapping {
        digit: 1,
        is_negative: true,
        byte_value: b'J',
    }, // 0x4A
    OverpunchMapping {
        digit: 2,
        is_negative: true,
        byte_value: b'K',
    }, // 0x4B
    OverpunchMapping {
        digit: 3,
        is_negative: true,
        byte_value: b'L',
    }, // 0x4C
    OverpunchMapping {
        digit: 4,
        is_negative: true,
        byte_value: b'M',
    }, // 0x4D
    OverpunchMapping {
        digit: 5,
        is_negative: true,
        byte_value: b'N',
    }, // 0x4E
    OverpunchMapping {
        digit: 6,
        is_negative: true,
        byte_value: b'O',
    }, // 0x4F
    OverpunchMapping {
        digit: 7,
        is_negative: true,
        byte_value: b'P',
    }, // 0x50
    OverpunchMapping {
        digit: 8,
        is_negative: true,
        byte_value: b'Q',
    }, // 0x51
    OverpunchMapping {
        digit: 9,
        is_negative: true,
        byte_value: b'R',
    }, // 0x52
];

/// ASCII overpunch decode table: byte -> (digit, is_negative)
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

/// EBCDIC overpunch encoding: (digit, is_negative) -> zone nibble
/// Returns the zone nibble (high 4 bits) for the given digit and sign
#[must_use]
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

/// EBCDIC overpunch decoding: zone nibble -> (is_signed, is_negative)
/// Returns None for invalid zone nibbles
#[must_use]
pub const fn decode_ebcdic_overpunch_zone(zone: u8) -> Option<(bool, bool)> {
    match zone {
        0xC => Some((true, false)), // Positive
        0xD => Some((true, true)),  // Negative
        0xF => Some((true, false)), // Preferred positive (often used for zero)
        _ => None,                  // Invalid zone for signed field
    }
}

/// Encode overpunch byte for the given digit, sign, and codepage
///
/// # Errors
/// Returns an error if the digit is invalid or the encoding fails
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

    match codepage {
        Codepage::ASCII => {
            // Find the matching ASCII overpunch character
            for mapping in &ASCII_OVERPUNCH_ENCODE {
                if mapping.digit == digit && mapping.is_negative == is_negative {
                    return Ok(mapping.byte_value);
                }
            }

            // Should never reach here for valid inputs
            Err(Error::new(
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                format!("No ASCII overpunch mapping for digit {digit}, negative: {is_negative}"),
            ))
        }
        _ => {
            // EBCDIC: combine zone and digit nibbles
            let zone = encode_ebcdic_overpunch_zone(digit, is_negative, policy);
            Ok((zone << 4) | digit)
        }
    }
}

/// Decode overpunch byte to extract digit and sign information
///
/// # Errors
/// Returns an error if the byte is not a valid overpunch character
pub fn decode_overpunch_byte(byte: u8, codepage: Codepage) -> Result<(u8, bool)> {
    match codepage {
        Codepage::ASCII => {
            if let Some((digit, is_negative)) = ASCII_OVERPUNCH_DECODE[byte as usize] {
                Ok((digit, is_negative))
            } else {
                Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!("Invalid ASCII overpunch byte 0x{byte:02X}"),
                ))
            }
        }
        _ => {
            // EBCDIC: extract zone and digit nibbles
            let zone = (byte >> 4) & 0x0F;
            let digit = byte & 0x0F;

            if digit > 9 {
                return Err(Error::new(
                    ErrorCode::CBKD411_ZONED_BAD_SIGN,
                    format!(
                        "Invalid digit nibble 0x{digit:X} in EBCDIC overpunch byte 0x{byte:02X}"
                    ),
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
}

/// Check if a byte is a valid overpunch character for the given codepage
#[must_use]
pub fn is_valid_overpunch(byte: u8, codepage: Codepage) -> bool {
    match codepage {
        Codepage::ASCII => ASCII_OVERPUNCH_DECODE[byte as usize].is_some(),
        _ => {
            let zone = (byte >> 4) & 0x0F;
            let digit = byte & 0x0F;
            digit <= 9 && decode_ebcdic_overpunch_zone(zone).is_some()
        }
    }
}

/// Get all valid overpunch bytes for testing purposes
#[must_use]
pub fn get_all_valid_overpunch_bytes(codepage: Codepage) -> Vec<u8> {
    match codepage {
        Codepage::ASCII => ASCII_OVERPUNCH_DECODE
            .iter()
            .enumerate()
            .filter_map(|(byte, mapping)| {
                if mapping.is_some() {
                    Some(byte as u8)
                } else {
                    None
                }
            })
            .collect(),
        _ => {
            let mut bytes = Vec::new();
            for zone in [0xC, 0xD, 0xF] {
                for digit in 0..=9 {
                    bytes.push((zone << 4) | digit);
                }
            }
            bytes
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
