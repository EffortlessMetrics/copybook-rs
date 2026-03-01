// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-overpunch: EBCDIC/ASCII zoned decimal
//! overpunch encode, decode, round-trip, edge cases, and COBOL PIC boundaries.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codepage::Codepage;
use copybook_error::ErrorCode;
use copybook_overpunch::{
    ZeroSignPolicy, decode_ebcdic_overpunch_zone, decode_overpunch_byte,
    encode_ebcdic_overpunch_zone, encode_overpunch_byte, get_all_valid_overpunch_bytes,
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
// 1. EBCDIC zoned decimal overpunch decode – positive (0xC zone)
// ====================================================================

#[test]
fn ebcdic_decode_positive_c_zone_all_digits() {
    for digit in 0u8..=9 {
        let byte = 0xC0 | digit;
        for cp in ALL_EBCDIC {
            let (d, is_neg) = decode_overpunch_byte(byte, cp)
                .unwrap_or_else(|e| panic!("{cp:?}: 0x{byte:02X} failed: {e}"));
            assert_eq!(d, digit, "{cp:?}: digit mismatch for 0x{byte:02X}");
            assert!(!is_neg, "{cp:?}: 0xC{digit:X} should be positive");
        }
    }
}

// ====================================================================
// 2. EBCDIC zoned decimal overpunch decode – negative (0xD zone)
// ====================================================================

#[test]
fn ebcdic_decode_negative_d_zone_all_digits() {
    for digit in 0u8..=9 {
        let byte = 0xD0 | digit;
        for cp in ALL_EBCDIC {
            let (d, is_neg) = decode_overpunch_byte(byte, cp)
                .unwrap_or_else(|e| panic!("{cp:?}: 0x{byte:02X} failed: {e}"));
            assert_eq!(d, digit, "{cp:?}: digit mismatch for 0x{byte:02X}");
            assert!(is_neg, "{cp:?}: 0xD{digit:X} should be negative");
        }
    }
}

// ====================================================================
// 3. EBCDIC unsigned/preferred zone (0xF) decodes as positive
// ====================================================================

#[test]
fn ebcdic_decode_f_zone_all_digits_positive() {
    for digit in 0u8..=9 {
        let byte = 0xF0 | digit;
        for cp in ALL_EBCDIC {
            let (d, is_neg) = decode_overpunch_byte(byte, cp).unwrap();
            assert_eq!(d, digit, "{cp:?}: 0xF{digit:X} digit mismatch");
            assert!(!is_neg, "{cp:?}: 0xF zone should decode as positive");
        }
    }
}

// ====================================================================
// 4. EBCDIC zone helper: encode_ebcdic_overpunch_zone
// ====================================================================

#[test]
fn ebcdic_zone_helper_positive_returns_0xc() {
    for digit in 0u8..=9 {
        let zone = encode_ebcdic_overpunch_zone(digit, false, ZeroSignPolicy::Positive);
        assert_eq!(zone, 0xC, "positive zone for digit {digit}");
    }
}

#[test]
fn ebcdic_zone_helper_negative_returns_0xd() {
    for digit in 0u8..=9 {
        let zone = encode_ebcdic_overpunch_zone(digit, true, ZeroSignPolicy::Positive);
        assert_eq!(zone, 0xD, "negative zone for digit {digit}");
    }
}

#[test]
fn ebcdic_zone_helper_preferred_zero_returns_0xf() {
    let zone_pos = encode_ebcdic_overpunch_zone(0, false, ZeroSignPolicy::Preferred);
    let zone_neg = encode_ebcdic_overpunch_zone(0, true, ZeroSignPolicy::Preferred);
    assert_eq!(zone_pos, 0xF);
    assert_eq!(zone_neg, 0xF);
}

#[test]
fn ebcdic_zone_helper_preferred_nonzero_unaffected() {
    for digit in 1u8..=9 {
        let pos = encode_ebcdic_overpunch_zone(digit, false, ZeroSignPolicy::Preferred);
        let neg = encode_ebcdic_overpunch_zone(digit, true, ZeroSignPolicy::Preferred);
        assert_eq!(pos, 0xC, "preferred nonzero positive digit {digit}");
        assert_eq!(neg, 0xD, "preferred nonzero negative digit {digit}");
    }
}

// ====================================================================
// 5. EBCDIC zone helper: decode_ebcdic_overpunch_zone
// ====================================================================

#[test]
fn decode_zone_valid_zones() {
    // 0xC → (true, false) = signed, positive
    assert_eq!(decode_ebcdic_overpunch_zone(0xC), Some((true, false)));
    // 0xD → (true, true) = signed, negative
    assert_eq!(decode_ebcdic_overpunch_zone(0xD), Some((true, true)));
    // 0xF → (true, false) = signed, positive (preferred zero)
    assert_eq!(decode_ebcdic_overpunch_zone(0xF), Some((true, false)));
}

#[test]
fn decode_zone_invalid_zones_return_none() {
    for zone in [
        0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xE,
    ] {
        assert_eq!(
            decode_ebcdic_overpunch_zone(zone),
            None,
            "zone 0x{zone:X} should be invalid"
        );
    }
}

// ====================================================================
// 6. Overpunch encode – positive values trailing byte
// ====================================================================

#[test]
fn encode_ebcdic_positive_byte_structure() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            let byte = encode_overpunch_byte(digit, false, cp, ZeroSignPolicy::Positive).unwrap();
            assert_eq!(byte, 0xC0 | digit, "{cp:?}: +{digit}");
        }
    }
}

#[test]
fn encode_ebcdic_negative_byte_structure() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            let byte = encode_overpunch_byte(digit, true, cp, ZeroSignPolicy::Positive).unwrap();
            assert_eq!(byte, 0xD0 | digit, "{cp:?}: -{digit}");
        }
    }
}

// ====================================================================
// 7. Round-trip: decode(encode(value)) == value
// ====================================================================

#[test]
fn roundtrip_encode_then_decode_all_ebcdic() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            for is_neg in [false, true] {
                let encoded =
                    encode_overpunch_byte(digit, is_neg, cp, ZeroSignPolicy::Positive).unwrap();
                let (d, n) = decode_overpunch_byte(encoded, cp).unwrap();
                assert_eq!(d, digit, "{cp:?}: digit {digit} neg={is_neg}");
                assert_eq!(n, is_neg, "{cp:?}: sign for digit {digit}");
            }
        }
    }
}

#[test]
fn roundtrip_encode_then_decode_ascii() {
    for digit in 0u8..=9 {
        for is_neg in [false, true] {
            let encoded =
                encode_overpunch_byte(digit, is_neg, Codepage::ASCII, ZeroSignPolicy::Positive)
                    .unwrap();
            let (d, n) = decode_overpunch_byte(encoded, Codepage::ASCII).unwrap();
            assert_eq!(d, digit);
            assert_eq!(n, is_neg);
        }
    }
}

// ====================================================================
// 8. Round-trip: encode(decode(bytes)) == bytes for valid inputs
// ====================================================================

#[test]
fn roundtrip_decode_then_encode_ebcdic_c_and_d_zones() {
    for cp in ALL_EBCDIC {
        // C-zone (positive) bytes
        for digit in 0u8..=9 {
            let original = 0xC0 | digit;
            let (d, is_neg) = decode_overpunch_byte(original, cp).unwrap();
            let reencoded = encode_overpunch_byte(d, is_neg, cp, ZeroSignPolicy::Positive).unwrap();
            assert_eq!(
                reencoded, original,
                "{cp:?}: C-zone decode→encode mismatch for 0x{original:02X}"
            );
        }
        // D-zone (negative) bytes
        for digit in 0u8..=9 {
            let original = 0xD0 | digit;
            let (d, is_neg) = decode_overpunch_byte(original, cp).unwrap();
            let reencoded = encode_overpunch_byte(d, is_neg, cp, ZeroSignPolicy::Positive).unwrap();
            assert_eq!(
                reencoded, original,
                "{cp:?}: D-zone decode→encode mismatch for 0x{original:02X}"
            );
        }
    }
}

#[test]
fn roundtrip_decode_then_encode_ascii_overpunch_chars() {
    let positive_chars = [b'{', b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I'];
    let negative_chars = [b'}', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R'];

    for &byte in positive_chars.iter().chain(negative_chars.iter()) {
        let (d, is_neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
        let reencoded =
            encode_overpunch_byte(d, is_neg, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(
            reencoded, byte,
            "ASCII decode→encode mismatch for 0x{byte:02X}"
        );
    }
}

// ====================================================================
// 9. ASCII overpunch: last byte encoding tables
// ====================================================================

#[test]
fn ascii_positive_overpunch_table() {
    let expected: [(u8, u8); 10] = [
        (0, b'{'),
        (1, b'A'),
        (2, b'B'),
        (3, b'C'),
        (4, b'D'),
        (5, b'E'),
        (6, b'F'),
        (7, b'G'),
        (8, b'H'),
        (9, b'I'),
    ];
    for (digit, expected_byte) in expected {
        let byte =
            encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(byte, expected_byte, "positive digit {digit}");
    }
}

#[test]
fn ascii_negative_overpunch_table() {
    let expected: [(u8, u8); 10] = [
        (0, b'}'),
        (1, b'J'),
        (2, b'K'),
        (3, b'L'),
        (4, b'M'),
        (5, b'N'),
        (6, b'O'),
        (7, b'P'),
        (8, b'Q'),
        (9, b'R'),
    ];
    for (digit, expected_byte) in expected {
        let byte =
            encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(byte, expected_byte, "negative digit {digit}");
    }
}

// ====================================================================
// 10. Edge cases: zero (+0 and -0)
// ====================================================================

#[test]
fn zero_positive_ebcdic_all_codepages() {
    for cp in ALL_EBCDIC {
        let byte = encode_overpunch_byte(0, false, cp, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(byte, 0xC0, "{cp:?}: positive zero");
        let (d, n) = decode_overpunch_byte(byte, cp).unwrap();
        assert_eq!(d, 0);
        assert!(!n);
    }
}

#[test]
fn zero_negative_ebcdic_all_codepages() {
    for cp in ALL_EBCDIC {
        let byte = encode_overpunch_byte(0, true, cp, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(byte, 0xD0, "{cp:?}: negative zero");
        let (d, n) = decode_overpunch_byte(byte, cp).unwrap();
        assert_eq!(d, 0);
        assert!(n);
    }
}

#[test]
fn zero_preferred_collapses_sign_ebcdic() {
    for cp in ALL_EBCDIC {
        let pos = encode_overpunch_byte(0, false, cp, ZeroSignPolicy::Preferred).unwrap();
        let neg = encode_overpunch_byte(0, true, cp, ZeroSignPolicy::Preferred).unwrap();
        assert_eq!(pos, neg, "{cp:?}: preferred zero bytes must be identical");
        assert_eq!(pos, 0xF0, "{cp:?}: preferred zero should be 0xF0");
        // Both decode as positive (sign lost)
        let (d, n) = decode_overpunch_byte(pos, cp).unwrap();
        assert_eq!(d, 0);
        assert!(!n, "{cp:?}: preferred zero always decodes positive");
    }
}

// ====================================================================
// 11. Edge cases: invalid overpunch bytes produce errors
// ====================================================================

#[test]
fn invalid_ebcdic_zones_0x0_through_0xb_and_0xe() {
    let invalid_zones: [u8; 13] = [
        0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xE,
    ];
    for zone in invalid_zones {
        for digit in 0u8..=9 {
            let byte = (zone << 4) | digit;
            let result = decode_overpunch_byte(byte, Codepage::CP037);
            assert!(result.is_err(), "zone 0x{zone:X} digit {digit} should fail");
            let err = result.unwrap_err();
            assert_eq!(err.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
        }
    }
}

#[test]
fn invalid_ebcdic_digit_nibbles_a_through_f() {
    for nibble in 0xAu8..=0xF {
        for zone in [0xC, 0xD, 0xF] {
            let byte = (zone << 4) | nibble;
            let result = decode_overpunch_byte(byte, Codepage::CP037);
            assert!(
                result.is_err(),
                "byte 0x{byte:02X}: invalid digit nibble 0x{nibble:X} should fail"
            );
            assert_eq!(result.unwrap_err().code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
        }
    }
}

#[test]
fn invalid_ascii_bytes_produce_correct_error_code() {
    let invalid: [u8; 8] = [b'S', b'T', b'Z', b'a', b'z', b'!', 0x00, 0xFF];
    for byte in invalid {
        let result = decode_overpunch_byte(byte, Codepage::ASCII);
        assert!(result.is_err(), "byte 0x{byte:02X} should be invalid ASCII");
        assert_eq!(result.unwrap_err().code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
    }
}

#[test]
fn encode_invalid_digits_rejected_with_correct_error_code() {
    for digit in [10u8, 15, 99, 128, 255] {
        for cp in [Codepage::ASCII, Codepage::CP037] {
            let result = encode_overpunch_byte(digit, false, cp, ZeroSignPolicy::Positive);
            assert!(result.is_err(), "{cp:?}: digit {digit} should be rejected");
            assert_eq!(
                result.unwrap_err().code,
                ErrorCode::CBKE501_JSON_TYPE_MISMATCH
            );
        }
    }
}

// ====================================================================
// 12. Exhaustive: every byte 0x00-0xFF classified correctly
// ====================================================================

#[test]
fn all_256_bytes_either_valid_or_error_ascii() {
    for byte in 0u8..=255 {
        let valid = is_valid_overpunch(byte, Codepage::ASCII);
        let decode_result = decode_overpunch_byte(byte, Codepage::ASCII);
        assert_eq!(
            valid,
            decode_result.is_ok(),
            "ASCII byte 0x{byte:02X}: is_valid={valid} but decode={:?}",
            decode_result.is_ok()
        );
    }
}

#[test]
fn all_256_bytes_either_valid_or_error_ebcdic() {
    for byte in 0u8..=255 {
        let valid = is_valid_overpunch(byte, Codepage::CP037);
        let decode_result = decode_overpunch_byte(byte, Codepage::CP037);
        assert_eq!(
            valid,
            decode_result.is_ok(),
            "EBCDIC byte 0x{byte:02X}: is_valid={valid} but decode={:?}",
            decode_result.is_ok()
        );
    }
}

// ====================================================================
// 13. Cross-codepage consistency: all EBCDIC codepages behave the same
// ====================================================================

#[test]
fn all_ebcdic_codepages_produce_identical_results() {
    for byte in 0u8..=255 {
        let reference = decode_overpunch_byte(byte, Codepage::CP037);
        for cp in [
            Codepage::CP273,
            Codepage::CP500,
            Codepage::CP1047,
            Codepage::CP1140,
        ] {
            let result = decode_overpunch_byte(byte, cp);
            match (&reference, &result) {
                (Ok((d1, n1)), Ok((d2, n2))) => {
                    assert_eq!(d1, d2, "{cp:?} vs CP037: byte 0x{byte:02X} digit mismatch");
                    assert_eq!(n1, n2, "{cp:?} vs CP037: byte 0x{byte:02X} sign mismatch");
                }
                (Err(_), Err(_)) => {} // both error – fine
                _ => panic!("byte 0x{byte:02X}: CP037={reference:?} but {cp:?}={result:?}"),
            }
        }
    }
}

// ====================================================================
// 14. Boundary values per COBOL PIC: multi-digit field simulation
// ====================================================================

/// Simulate a multi-byte zoned decimal field: leading bytes are unsigned
/// digits (0xF zone for EBCDIC), trailing byte carries the overpunch sign.
fn build_ebcdic_zoned_field(digits: &[u8], is_negative: bool, cp: Codepage) -> Vec<u8> {
    assert!(!digits.is_empty());
    let mut field = Vec::with_capacity(digits.len());
    // Leading digits: unsigned (0xF zone)
    for &d in &digits[..digits.len() - 1] {
        field.push(0xF0 | d);
    }
    // Trailing digit: overpunch
    let last = *digits.last().unwrap();
    let trailing = encode_overpunch_byte(last, is_negative, cp, ZeroSignPolicy::Positive).unwrap();
    field.push(trailing);
    field
}

fn decode_ebcdic_zoned_field(field: &[u8], cp: Codepage) -> (i64, bool) {
    assert!(!field.is_empty());
    let mut value: i64 = 0;
    // Leading bytes: unsigned digits
    for &byte in &field[..field.len() - 1] {
        let (d, _) = decode_overpunch_byte(byte, cp).unwrap();
        value = value * 10 + i64::from(d);
    }
    // Trailing byte: overpunch
    let (d, is_neg) = decode_overpunch_byte(*field.last().unwrap(), cp).unwrap();
    value = value * 10 + i64::from(d);
    if is_neg {
        value = -value;
    }
    (value, is_neg)
}

#[test]
fn pic_s9_single_digit_signed() {
    // PIC S9: 1 byte, values -9 to +9
    for digit in 0u8..=9 {
        let field_pos = build_ebcdic_zoned_field(&[digit], false, Codepage::CP037);
        assert_eq!(field_pos.len(), 1);
        let (val, neg) = decode_ebcdic_zoned_field(&field_pos, Codepage::CP037);
        assert_eq!(val, i64::from(digit));
        assert!(!neg);

        let field_neg = build_ebcdic_zoned_field(&[digit], true, Codepage::CP037);
        let (val, neg) = decode_ebcdic_zoned_field(&field_neg, Codepage::CP037);
        assert_eq!(val, -i64::from(digit));
        assert!(neg);
    }
}

#[test]
fn pic_s9_4_four_digit_signed() {
    // PIC S9(4): 4 bytes, max +9999 / -9999
    let max_digits = [9u8, 9, 9, 9];
    let field = build_ebcdic_zoned_field(&max_digits, false, Codepage::CP037);
    assert_eq!(field.len(), 4);
    let (val, _) = decode_ebcdic_zoned_field(&field, Codepage::CP037);
    assert_eq!(val, 9999);

    let field_neg = build_ebcdic_zoned_field(&max_digits, true, Codepage::CP037);
    let (val, _) = decode_ebcdic_zoned_field(&field_neg, Codepage::CP037);
    assert_eq!(val, -9999);

    // Value 1234
    let field = build_ebcdic_zoned_field(&[1, 2, 3, 4], false, Codepage::CP037);
    let (val, _) = decode_ebcdic_zoned_field(&field, Codepage::CP037);
    assert_eq!(val, 1234);
}

#[test]
fn pic_s9_9_nine_digit_signed() {
    // PIC S9(9): 9 bytes, max ±999_999_999
    let max_digits = [9u8; 9];
    let field = build_ebcdic_zoned_field(&max_digits, false, Codepage::CP037);
    assert_eq!(field.len(), 9);
    let (val, _) = decode_ebcdic_zoned_field(&field, Codepage::CP037);
    assert_eq!(val, 999_999_999);

    let field_neg = build_ebcdic_zoned_field(&max_digits, true, Codepage::CP037);
    let (val, _) = decode_ebcdic_zoned_field(&field_neg, Codepage::CP037);
    assert_eq!(val, -999_999_999);
}

#[test]
fn pic_s9_18_max_cobol_digits() {
    // PIC S9(18): 18 bytes, max ±999_999_999_999_999_999
    let max_digits = [9u8; 18];
    let field = build_ebcdic_zoned_field(&max_digits, false, Codepage::CP037);
    assert_eq!(field.len(), 18);
    let (val, _) = decode_ebcdic_zoned_field(&field, Codepage::CP037);
    assert_eq!(val, 999_999_999_999_999_999);

    let field_neg = build_ebcdic_zoned_field(&max_digits, true, Codepage::CP037);
    let (val, _) = decode_ebcdic_zoned_field(&field_neg, Codepage::CP037);
    assert_eq!(val, -999_999_999_999_999_999);
}

#[test]
fn pic_s9_4_zero_value() {
    // All zeros, positive and negative
    let field_pos = build_ebcdic_zoned_field(&[0, 0, 0, 0], false, Codepage::CP037);
    let (val, _) = decode_ebcdic_zoned_field(&field_pos, Codepage::CP037);
    assert_eq!(val, 0);

    let field_neg = build_ebcdic_zoned_field(&[0, 0, 0, 0], true, Codepage::CP037);
    let (val, neg) = decode_ebcdic_zoned_field(&field_neg, Codepage::CP037);
    assert_eq!(val, 0);
    assert!(neg, "negative zero flag preserved");
}

#[test]
fn pic_s9_field_roundtrip_various_values() {
    let test_cases: &[(&[u8], bool, i64)] = &[
        (&[0], false, 0),
        (&[5], false, 5),
        (&[5], true, -5),
        (&[1, 0], false, 10),
        (&[4, 2], true, -42),
        (&[1, 0, 0], false, 100),
        (&[7, 6, 5], true, -765),
        (&[1, 2, 3, 4, 5, 6, 7, 8, 9], false, 123_456_789),
    ];
    for &(digits, is_neg, expected_val) in test_cases {
        let field = build_ebcdic_zoned_field(digits, is_neg, Codepage::CP037);
        let (val, neg) = decode_ebcdic_zoned_field(&field, Codepage::CP037);
        assert_eq!(val, expected_val, "digits={digits:?} neg={is_neg}");
        assert_eq!(neg, is_neg, "sign mismatch for {expected_val}");
    }
}

// ====================================================================
// 15. get_all_valid_overpunch_bytes consistency
// ====================================================================

#[test]
fn valid_bytes_set_all_decodable() {
    for cp in [Codepage::ASCII, Codepage::CP037, Codepage::CP500] {
        let valid = get_all_valid_overpunch_bytes(cp);
        for byte in &valid {
            assert!(
                decode_overpunch_byte(*byte, cp).is_ok(),
                "{cp:?}: byte 0x{byte:02X} in valid set but fails decode"
            );
            assert!(is_valid_overpunch(*byte, cp));
        }
    }
}

#[test]
fn valid_bytes_set_contains_no_duplicates() {
    for cp in [Codepage::ASCII, Codepage::CP037] {
        let valid = get_all_valid_overpunch_bytes(cp);
        let mut sorted = valid.clone();
        sorted.sort_unstable();
        sorted.dedup();
        assert_eq!(
            valid.len(),
            sorted.len(),
            "{cp:?}: duplicate bytes in valid set"
        );
    }
}

#[test]
fn valid_ebcdic_bytes_count_is_30() {
    // 3 zones (C, D, F) × 10 digits (0-9) = 30
    for cp in ALL_EBCDIC {
        let valid = get_all_valid_overpunch_bytes(cp);
        assert_eq!(valid.len(), 30, "{cp:?}");
    }
}

#[test]
fn valid_ascii_bytes_count_is_30() {
    // 10 positive ({, A-I) + 10 negative (}, J-R) + 10 digits (0-9) = 30
    let valid = get_all_valid_overpunch_bytes(Codepage::ASCII);
    assert_eq!(valid.len(), 30);
}
