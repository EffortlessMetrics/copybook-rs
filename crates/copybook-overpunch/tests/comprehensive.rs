// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-overpunch.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codepage::Codepage;
use copybook_error::ErrorCode;
use copybook_overpunch::{
    ZeroSignPolicy, decode_overpunch_byte, encode_overpunch_byte, get_all_valid_overpunch_bytes,
    is_valid_overpunch,
};

const ALL_EBCDIC: [Codepage; 5] = [
    Codepage::CP037,
    Codepage::CP273,
    Codepage::CP500,
    Codepage::CP1047,
    Codepage::CP1140,
];

// ====================================================================
// 1. Decode positive sign characters ({, A-I)
// ====================================================================

#[test]
fn decode_ascii_positive_brace_is_zero() {
    let (digit, is_neg) = decode_overpunch_byte(b'{', Codepage::ASCII).unwrap();
    assert_eq!(digit, 0);
    assert!(!is_neg);
}

#[test]
fn decode_ascii_positive_a_through_i() {
    let expected: [(u8, u8); 9] = [
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
    for (byte, expected_digit) in expected {
        let (digit, is_neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
        assert_eq!(digit, expected_digit, "byte {byte}");
        assert!(!is_neg, "byte {byte} should be positive");
    }
}

#[test]
fn decode_ascii_plain_digits_as_positive() {
    for d in 0u8..=9 {
        let byte = b'0' + d;
        let (digit, is_neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
        assert_eq!(digit, d, "digit {d}");
        assert!(!is_neg, "plain digit {d} should be positive");
    }
}

// ====================================================================
// 2. Decode negative sign characters (}, J-R)
// ====================================================================

#[test]
fn decode_ascii_negative_brace_is_zero() {
    let (digit, is_neg) = decode_overpunch_byte(b'}', Codepage::ASCII).unwrap();
    assert_eq!(digit, 0);
    assert!(is_neg);
}

#[test]
fn decode_ascii_negative_j_through_r() {
    let expected: [(u8, u8); 9] = [
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
    for (byte, expected_digit) in expected {
        let (digit, is_neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
        assert_eq!(digit, expected_digit, "byte {byte}");
        assert!(is_neg, "byte {byte} should be negative");
    }
}

// ====================================================================
// 3. Encode positive values
// ====================================================================

#[test]
fn encode_ascii_positive_all_digits() {
    let expected_bytes = [b'{', b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I'];
    for digit in 0u8..=9 {
        let byte =
            encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(byte, expected_bytes[digit as usize], "digit {digit}");
    }
}

#[test]
fn encode_ebcdic_positive_all_digits_all_codepages() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            let byte = encode_overpunch_byte(digit, false, cp, ZeroSignPolicy::Positive).unwrap();
            let zone = (byte >> 4) & 0x0F;
            let d = byte & 0x0F;
            assert_eq!(d, digit, "{cp:?}: digit nibble mismatch");
            assert_eq!(zone, 0xC, "{cp:?}: positive zone should be 0xC");
        }
    }
}

// ====================================================================
// 4. Encode negative values
// ====================================================================

#[test]
fn encode_ascii_negative_all_digits() {
    let expected_bytes = [b'}', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R'];
    for digit in 0u8..=9 {
        let byte =
            encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(byte, expected_bytes[digit as usize], "digit {digit}");
    }
}

#[test]
fn encode_ebcdic_negative_all_digits_all_codepages() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            let byte = encode_overpunch_byte(digit, true, cp, ZeroSignPolicy::Positive).unwrap();
            let zone = (byte >> 4) & 0x0F;
            let d = byte & 0x0F;
            assert_eq!(d, digit, "{cp:?}: digit nibble mismatch");
            assert_eq!(zone, 0xD, "{cp:?}: negative zone should be 0xD");
        }
    }
}

// ====================================================================
// 5. Zero handling
// ====================================================================

#[test]
fn zero_positive_ascii() {
    let byte = encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(byte, b'{');
    let (digit, is_neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
    assert_eq!(digit, 0);
    assert!(!is_neg);
}

#[test]
fn zero_negative_ascii() {
    let byte = encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(byte, b'}');
    let (digit, is_neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
    assert_eq!(digit, 0);
    assert!(is_neg);
}

#[test]
fn zero_preferred_policy_ebcdic() {
    // Preferred zero uses 0xF zone regardless of sign
    for cp in ALL_EBCDIC {
        let pos = encode_overpunch_byte(0, false, cp, ZeroSignPolicy::Preferred).unwrap();
        let neg = encode_overpunch_byte(0, true, cp, ZeroSignPolicy::Preferred).unwrap();
        assert_eq!(pos, 0xF0, "{cp:?}: preferred +0 should be 0xF0");
        assert_eq!(neg, 0xF0, "{cp:?}: preferred -0 should be 0xF0");
    }
}

#[test]
fn zero_preferred_policy_ascii_ignored() {
    // ASCII ignores ZeroSignPolicy::Preferred
    let pos = encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Preferred).unwrap();
    let neg = encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Preferred).unwrap();
    assert_eq!(pos, b'{');
    assert_eq!(neg, b'}');
}

// ====================================================================
// 6. Boundary values
// ====================================================================

#[test]
fn boundary_digit_9_positive_roundtrip() {
    for cp in ALL_EBCDIC {
        let encoded = encode_overpunch_byte(9, false, cp, ZeroSignPolicy::Positive).unwrap();
        let (digit, is_neg) = decode_overpunch_byte(encoded, cp).unwrap();
        assert_eq!(digit, 9, "{cp:?}");
        assert!(!is_neg, "{cp:?}");
    }
    let encoded =
        encode_overpunch_byte(9, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(encoded, b'I');
    let (digit, is_neg) = decode_overpunch_byte(encoded, Codepage::ASCII).unwrap();
    assert_eq!(digit, 9);
    assert!(!is_neg);
}

#[test]
fn boundary_digit_9_negative_roundtrip() {
    for cp in ALL_EBCDIC {
        let encoded = encode_overpunch_byte(9, true, cp, ZeroSignPolicy::Positive).unwrap();
        let (digit, is_neg) = decode_overpunch_byte(encoded, cp).unwrap();
        assert_eq!(digit, 9, "{cp:?}");
        assert!(is_neg, "{cp:?}");
    }
    let encoded =
        encode_overpunch_byte(9, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(encoded, b'R');
}

#[test]
fn boundary_digit_0_roundtrip() {
    for cp in ALL_EBCDIC {
        let encoded = encode_overpunch_byte(0, false, cp, ZeroSignPolicy::Positive).unwrap();
        let (digit, is_neg) = decode_overpunch_byte(encoded, cp).unwrap();
        assert_eq!(digit, 0, "{cp:?}");
        assert!(!is_neg, "{cp:?}");
    }
}

#[test]
fn invalid_digit_10_rejected() {
    let result = encode_overpunch_byte(10, false, Codepage::ASCII, ZeroSignPolicy::Positive);
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKE501_JSON_TYPE_MISMATCH
    );
}

#[test]
fn invalid_digit_255_rejected() {
    let result = encode_overpunch_byte(255, false, Codepage::CP037, ZeroSignPolicy::Positive);
    assert!(result.is_err());
}

#[test]
fn invalid_ascii_byte_rejected() {
    // 0x00 is not a valid overpunch character in ASCII
    let result = decode_overpunch_byte(0x00, Codepage::ASCII);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
}

#[test]
fn invalid_ebcdic_zone_rejected() {
    // Zone 0xA with digit 0 → 0xA0 is not a valid EBCDIC overpunch byte
    let result = decode_overpunch_byte(0xA0, Codepage::CP037);
    assert!(result.is_err());
}

#[test]
fn invalid_ebcdic_digit_nibble_rejected() {
    // Zone 0xC with digit 0xA (10) → invalid digit nibble
    let result = decode_overpunch_byte(0xCA, Codepage::CP037);
    assert!(result.is_err());
}

// ====================================================================
// 7. Encode/decode parity (full round-trip all digits/signs/codepages)
// ====================================================================

#[test]
fn full_roundtrip_all_digits_signs_codepages() {
    let all_cp: [Codepage; 6] = [
        Codepage::ASCII,
        Codepage::CP037,
        Codepage::CP273,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ];
    for cp in all_cp {
        for digit in 0u8..=9 {
            for is_neg in [false, true] {
                let encoded =
                    encode_overpunch_byte(digit, is_neg, cp, ZeroSignPolicy::Positive).unwrap();
                let (dec_digit, dec_neg) = decode_overpunch_byte(encoded, cp).unwrap();
                assert_eq!(dec_digit, digit, "{cp:?}: digit {digit} neg={is_neg}");
                assert_eq!(dec_neg, is_neg, "{cp:?}: sign mismatch digit {digit}");
            }
        }
    }
}

// ====================================================================
// 8. is_valid_overpunch / get_all_valid_overpunch_bytes
// ====================================================================

#[test]
fn is_valid_overpunch_ascii_positive_chars() {
    for byte in [b'{', b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I'] {
        assert!(
            is_valid_overpunch(byte, Codepage::ASCII),
            "byte 0x{byte:02X}"
        );
    }
}

#[test]
fn is_valid_overpunch_ascii_negative_chars() {
    for byte in [b'}', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R'] {
        assert!(
            is_valid_overpunch(byte, Codepage::ASCII),
            "byte 0x{byte:02X}"
        );
    }
}

#[test]
fn is_valid_overpunch_ascii_plain_digits() {
    for d in b'0'..=b'9' {
        assert!(is_valid_overpunch(d, Codepage::ASCII));
    }
}

#[test]
fn is_valid_overpunch_ascii_invalid_chars() {
    for byte in [0x00u8, b'S', b'T', b'Z', b'!', b'@', 0xFF] {
        assert!(
            !is_valid_overpunch(byte, Codepage::ASCII),
            "byte 0x{byte:02X}"
        );
    }
}

#[test]
fn get_all_valid_overpunch_bytes_ascii_count() {
    let valid = get_all_valid_overpunch_bytes(Codepage::ASCII);
    // 10 positive ({, A-I) + 10 negative (}, J-R) + 10 digits (0-9) = 30
    assert_eq!(valid.len(), 30);
}

#[test]
fn get_all_valid_overpunch_bytes_ebcdic_count() {
    for cp in ALL_EBCDIC {
        let valid = get_all_valid_overpunch_bytes(cp);
        // 3 zones (0xC, 0xD, 0xF) × 10 digits = 30
        assert_eq!(valid.len(), 30, "{cp:?}");
    }
}

#[test]
fn all_valid_ebcdic_overpunch_bytes_are_decodable() {
    for cp in ALL_EBCDIC {
        let valid = get_all_valid_overpunch_bytes(cp);
        for byte in valid {
            assert!(
                decode_overpunch_byte(byte, cp).is_ok(),
                "{cp:?}: byte 0x{byte:02X} should decode"
            );
        }
    }
}

#[test]
fn all_valid_ascii_overpunch_bytes_are_decodable() {
    let valid = get_all_valid_overpunch_bytes(Codepage::ASCII);
    for byte in valid {
        assert!(
            decode_overpunch_byte(byte, Codepage::ASCII).is_ok(),
            "byte 0x{byte:02X} should decode"
        );
    }
}
