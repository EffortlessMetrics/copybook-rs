#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

//! Test for Issue #72: Verify Numeric Overpunch 'D' Mapping
//!
//! This test validates the ASCII overpunch character mappings for 'D' and 'M':
//! - 'D' (0x44) should map to +4 (positive 4)
//! - 'M' (0x4D) should map to -4 (negative 4)
//!
//! According to COBOL standards:
//! - Positive digits 0-9 map to: {, A, B, C, D, E, F, G, H, I
//! - Negative digits 0-9 map to: }, J, K, L, M, N, O, P, Q, R

use copybook_codec::zoned_overpunch::{
    decode_overpunch_byte, encode_overpunch_byte, ZeroSignPolicy,
};
use copybook_codec::Codepage;

#[test]
fn test_issue_72_d_character_positive_4() {
    // Test that 'D' (0x44) decodes to +4
    let (digit, is_negative) = decode_overpunch_byte(0x44, Codepage::ASCII)
        .expect("Failed to decode 'D' character");

    assert_eq!(digit, 4, "'D' should decode to digit 4");
    assert!(!is_negative, "'D' should be positive (not negative)");

    // Test that +4 encodes to 'D'
    let encoded = encode_overpunch_byte(4, false, Codepage::ASCII, ZeroSignPolicy::Positive)
        .expect("Failed to encode +4");

    assert_eq!(encoded, 0x44, "+4 should encode to 'D' (0x44)");
    assert_eq!(encoded as char, 'D', "+4 should encode to 'D'");
}

#[test]
fn test_issue_72_m_character_negative_4() {
    // Test that 'M' (0x4D) decodes to -4
    let (digit, is_negative) = decode_overpunch_byte(0x4D, Codepage::ASCII)
        .expect("Failed to decode 'M' character");

    assert_eq!(digit, 4, "'M' should decode to digit 4");
    assert!(is_negative, "'M' should be negative");

    // Test that -4 encodes to 'M'
    let encoded = encode_overpunch_byte(4, true, Codepage::ASCII, ZeroSignPolicy::Positive)
        .expect("Failed to encode -4");

    assert_eq!(encoded, 0x4D, "-4 should encode to 'M' (0x4D)");
    assert_eq!(encoded as char, 'M', "-4 should encode to 'M'");
}

#[test]
fn test_issue_72_complete_positive_overpunch_table() {
    // Verify complete positive overpunch table: {, A-I for digits 0-9
    let expected = [
        (0, b'{'), // +0
        (1, b'A'), // +1
        (2, b'B'), // +2
        (3, b'C'), // +3
        (4, b'D'), // +4
        (5, b'E'), // +5
        (6, b'F'), // +6
        (7, b'G'), // +7
        (8, b'H'), // +8
        (9, b'I'), // +9
    ];

    for (digit, expected_byte) in expected {
        let encoded = encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive)
            .unwrap_or_else(|_| panic!("Failed to encode positive digit {digit}"));

        assert_eq!(
            encoded, expected_byte,
            "Positive digit {digit} should encode to 0x{expected_byte:02X} = '{}'",
            expected_byte as char
        );

        let (decoded_digit, is_negative) = decode_overpunch_byte(encoded, Codepage::ASCII)
            .unwrap_or_else(|_| panic!("Failed to decode byte 0x{encoded:02X}"));

        assert_eq!(decoded_digit, digit, "Round-trip failed for positive digit {digit}");
        assert!(!is_negative, "Positive digit {digit} should not be negative");
    }
}

#[test]
fn test_issue_72_complete_negative_overpunch_table() {
    // Verify complete negative overpunch table: }, J-R for digits 0-9
    let expected = [
        (0, b'}'), // -0
        (1, b'J'), // -1
        (2, b'K'), // -2
        (3, b'L'), // -3
        (4, b'M'), // -4
        (5, b'N'), // -5
        (6, b'O'), // -6
        (7, b'P'), // -7
        (8, b'Q'), // -8
        (9, b'R'), // -9
    ];

    for (digit, expected_byte) in expected {
        let encoded = encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive)
            .unwrap_or_else(|_| panic!("Failed to encode negative digit {digit}"));

        assert_eq!(
            encoded, expected_byte,
            "Negative digit {digit} should encode to 0x{expected_byte:02X} = '{}'",
            expected_byte as char
        );

        let (decoded_digit, is_negative) = decode_overpunch_byte(encoded, Codepage::ASCII)
            .unwrap_or_else(|_| panic!("Failed to decode byte 0x{encoded:02X}"));

        assert_eq!(decoded_digit, digit, "Round-trip failed for negative digit {digit}");

        // Note: -0 normalizes to +0 in some contexts, so we only check is_negative for non-zero
        if digit != 0 {
            assert!(is_negative, "Negative digit {digit} should be negative");
        }
    }
}

#[test]
fn test_issue_72_roundtrip_all_digits() {
    // Comprehensive round-trip test for all digits with both signs
    for digit in 0..=9 {
        // Positive
        let pos_encoded =
            encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive)
                .expect("Encode positive should succeed");
        let (pos_digit, pos_negative) = decode_overpunch_byte(pos_encoded, Codepage::ASCII)
            .expect("Decode positive should succeed");

        assert_eq!(pos_digit, digit, "Positive round-trip failed for digit {digit}");
        assert!(!pos_negative, "Positive digit {digit} should not be negative");

        // Negative
        let neg_encoded =
            encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive)
                .expect("Encode negative should succeed");
        let (neg_digit, neg_negative) = decode_overpunch_byte(neg_encoded, Codepage::ASCII)
            .expect("Decode negative should succeed");

        assert_eq!(neg_digit, digit, "Negative round-trip failed for digit {digit}");
        if digit != 0 {
            assert!(neg_negative, "Negative digit {digit} should be negative");
        }
    }
}
