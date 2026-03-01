// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep tests for copybook-overpunch: exhaustive encode/decode, roundtrips,
//! invalid input, EBCDIC zone nibbles, Display/Debug traits, boundary values,
//! and thread safety.
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

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

// ============================================================================
// 1. All 10 positive overpunch encodings ({, A-I)
// ============================================================================

#[test]
fn ascii_positive_overpunch_brace_maps_to_zero() {
    let (digit, neg) = decode_overpunch_byte(b'{', Codepage::ASCII).unwrap();
    assert_eq!(digit, 0);
    assert!(!neg);
}

#[test]
fn ascii_positive_overpunch_a_through_i() {
    for (i, byte) in (b'A'..=b'I').enumerate() {
        let (digit, neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
        assert_eq!(digit, (i + 1) as u8, "byte '{}'", byte as char);
        assert!(!neg, "byte '{}' should be positive", byte as char);
    }
}

// ============================================================================
// 2. All 10 negative overpunch encodings (}, J-R)
// ============================================================================

#[test]
fn ascii_negative_overpunch_brace_maps_to_zero() {
    let (digit, neg) = decode_overpunch_byte(b'}', Codepage::ASCII).unwrap();
    assert_eq!(digit, 0);
    assert!(neg);
}

#[test]
fn ascii_negative_overpunch_j_through_r() {
    for (i, byte) in (b'J'..=b'R').enumerate() {
        let (digit, neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
        assert_eq!(digit, (i + 1) as u8, "byte '{}'", byte as char);
        assert!(neg, "byte '{}' should be negative", byte as char);
    }
}

// ============================================================================
// 3. Decode positive/negative for each digit (0-9) on EBCDIC
// ============================================================================

#[test]
fn ebcdic_decode_positive_zone_c_for_each_digit() {
    for digit in 0u8..=9 {
        let byte = 0xC0 | digit;
        for cp in ALL_EBCDIC {
            let (d, neg) = decode_overpunch_byte(byte, cp).unwrap();
            assert_eq!(d, digit, "{cp:?}: C-zone digit {digit}");
            assert!(!neg, "{cp:?}: C-zone should be positive");
        }
    }
}

#[test]
fn ebcdic_decode_negative_zone_d_for_each_digit() {
    for digit in 0u8..=9 {
        let byte = 0xD0 | digit;
        for cp in ALL_EBCDIC {
            let (d, neg) = decode_overpunch_byte(byte, cp).unwrap();
            assert_eq!(d, digit, "{cp:?}: D-zone digit {digit}");
            assert!(neg, "{cp:?}: D-zone should be negative");
        }
    }
}

#[test]
fn ebcdic_decode_unsigned_zone_f_for_each_digit() {
    for digit in 0u8..=9 {
        let byte = 0xF0 | digit;
        for cp in ALL_EBCDIC {
            let (d, neg) = decode_overpunch_byte(byte, cp).unwrap();
            assert_eq!(d, digit, "{cp:?}: F-zone digit {digit}");
            assert!(!neg, "{cp:?}: F-zone should decode as positive");
        }
    }
}

// ============================================================================
// 4. Encode positive/negative for each digit
// ============================================================================

#[test]
fn ascii_encode_positive_each_digit() {
    let expected: [u8; 10] = [b'{', b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I'];
    for digit in 0u8..=9 {
        let enc =
            encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(enc, expected[digit as usize], "positive digit {digit}");
    }
}

#[test]
fn ascii_encode_negative_each_digit() {
    let expected: [u8; 10] = [b'}', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R'];
    for digit in 0u8..=9 {
        let enc =
            encode_overpunch_byte(digit, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        assert_eq!(enc, expected[digit as usize], "negative digit {digit}");
    }
}

#[test]
fn ebcdic_encode_positive_zone_is_0xc() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            let enc = encode_overpunch_byte(digit, false, cp, ZeroSignPolicy::Positive).unwrap();
            assert_eq!(
                (enc >> 4) & 0x0F,
                0xC,
                "{cp:?}: positive zone digit {digit}"
            );
            assert_eq!(enc & 0x0F, digit, "{cp:?}: digit nibble for {digit}");
        }
    }
}

#[test]
fn ebcdic_encode_negative_zone_is_0xd() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            let enc = encode_overpunch_byte(digit, true, cp, ZeroSignPolicy::Positive).unwrap();
            assert_eq!(
                (enc >> 4) & 0x0F,
                0xD,
                "{cp:?}: negative zone digit {digit}"
            );
            assert_eq!(enc & 0x0F, digit, "{cp:?}: digit nibble for {digit}");
        }
    }
}

// ============================================================================
// 5. Roundtrip: encode → decode == original
// ============================================================================

#[test]
fn roundtrip_ascii_all_digit_sign_combinations() {
    for digit in 0u8..=9 {
        for neg in [false, true] {
            let enc = encode_overpunch_byte(digit, neg, Codepage::ASCII, ZeroSignPolicy::Positive)
                .unwrap();
            let (d, n) = decode_overpunch_byte(enc, Codepage::ASCII).unwrap();
            assert_eq!(d, digit, "ASCII roundtrip digit {digit} neg={neg}");
            assert_eq!(n, neg, "ASCII roundtrip sign digit {digit} neg={neg}");
        }
    }
}

#[test]
fn roundtrip_ebcdic_all_codepages_positive_policy() {
    for cp in ALL_EBCDIC {
        for digit in 0u8..=9 {
            for neg in [false, true] {
                let enc = encode_overpunch_byte(digit, neg, cp, ZeroSignPolicy::Positive).unwrap();
                let (d, n) = decode_overpunch_byte(enc, cp).unwrap();
                assert_eq!(d, digit, "{cp:?}: digit {digit} neg={neg}");
                assert_eq!(n, neg, "{cp:?}: sign digit {digit} neg={neg}");
            }
        }
    }
}

#[test]
fn roundtrip_ebcdic_preferred_policy_nonzero_digits() {
    // Preferred policy only affects digit 0; non-zero digits should roundtrip normally
    for cp in ALL_EBCDIC {
        for digit in 1u8..=9 {
            for neg in [false, true] {
                let enc = encode_overpunch_byte(digit, neg, cp, ZeroSignPolicy::Preferred).unwrap();
                let (d, n) = decode_overpunch_byte(enc, cp).unwrap();
                assert_eq!(d, digit, "{cp:?}: preferred nonzero digit {digit}");
                assert_eq!(n, neg, "{cp:?}: preferred nonzero sign digit {digit}");
            }
        }
    }
}

#[test]
fn roundtrip_ebcdic_preferred_zero_always_positive() {
    for cp in ALL_EBCDIC {
        for neg in [false, true] {
            let enc = encode_overpunch_byte(0, neg, cp, ZeroSignPolicy::Preferred).unwrap();
            assert_eq!(enc, 0xF0, "{cp:?}: preferred zero → 0xF0");
            let (d, n) = decode_overpunch_byte(enc, cp).unwrap();
            assert_eq!(d, 0);
            assert!(!n, "{cp:?}: preferred zero decodes as positive");
        }
    }
}

// ============================================================================
// 6. Invalid byte handling (not an overpunch byte)
// ============================================================================

#[test]
fn invalid_ascii_bytes_rejected() {
    let invalid: &[u8] = &[
        b'S', b'T', b'U', b'Z', b'a', b'z', b'!', b'@', b'#', 0x00, 0xFF,
    ];
    for &byte in invalid {
        let result = decode_overpunch_byte(byte, Codepage::ASCII);
        assert!(result.is_err(), "byte 0x{byte:02X} should be rejected");
        let err = result.unwrap_err();
        assert_eq!(err.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
    }
}

#[test]
fn invalid_ebcdic_zone_nibbles_rejected() {
    // Valid zones: 0xC, 0xD, 0xF. All others should fail.
    for zone in [
        0x0u8, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xE,
    ] {
        let byte = (zone << 4) | 0x05;
        let result = decode_overpunch_byte(byte, Codepage::CP037);
        assert!(
            result.is_err(),
            "zone 0x{zone:X} with digit 5 should be rejected"
        );
    }
}

#[test]
fn invalid_ebcdic_digit_nibbles_rejected() {
    for digit_nibble in 0xAu8..=0xF {
        let byte = 0xC0 | digit_nibble; // valid zone C, invalid digit
        let result = decode_overpunch_byte(byte, Codepage::CP037);
        assert!(
            result.is_err(),
            "digit nibble 0x{digit_nibble:X} should be rejected"
        );
    }
}

#[test]
fn encode_digit_out_of_range_rejected() {
    for digit in [10u8, 11, 15, 50, 99, 255] {
        let r_ascii =
            encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive);
        assert!(r_ascii.is_err(), "digit {digit} should fail for ASCII");

        let r_ebcdic =
            encode_overpunch_byte(digit, false, Codepage::CP037, ZeroSignPolicy::Positive);
        assert!(r_ebcdic.is_err(), "digit {digit} should fail for EBCDIC");
    }
}

// ============================================================================
// 7. EBCDIC vs ASCII overpunch byte values
// ============================================================================

#[test]
fn ascii_and_ebcdic_encode_different_bytes_for_same_digit() {
    for digit in 0u8..=9 {
        let ascii_pos =
            encode_overpunch_byte(digit, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
        let ebcdic_pos =
            encode_overpunch_byte(digit, false, Codepage::CP037, ZeroSignPolicy::Positive).unwrap();
        // They should produce different byte values
        assert_ne!(
            ascii_pos, ebcdic_pos,
            "digit {digit}: ASCII and EBCDIC positive bytes should differ"
        );
    }
}

#[test]
fn ebcdic_overpunch_bytes_are_in_c_d_f_range() {
    for cp in ALL_EBCDIC {
        let valid = get_all_valid_overpunch_bytes(cp);
        for &byte in &valid {
            let zone = (byte >> 4) & 0x0F;
            assert!(
                zone == 0xC || zone == 0xD || zone == 0xF,
                "{cp:?}: byte 0x{byte:02X} has unexpected zone 0x{zone:X}"
            );
            let digit = byte & 0x0F;
            assert!(digit <= 9, "{cp:?}: byte 0x{byte:02X} has digit > 9");
        }
    }
}

#[test]
fn ascii_valid_overpunch_set_has_30_entries() {
    let bytes = get_all_valid_overpunch_bytes(Codepage::ASCII);
    // 10 positive ({, A-I) + 10 negative (}, J-R) + 10 digits (0-9) = 30
    assert!(
        bytes.len() >= 30,
        "ASCII should have at least 30 valid overpunch bytes, got {}",
        bytes.len()
    );
}

#[test]
fn ebcdic_valid_overpunch_set_has_30_entries() {
    for cp in ALL_EBCDIC {
        let bytes = get_all_valid_overpunch_bytes(cp);
        // 3 zones (C, D, F) × 10 digits = 30
        assert_eq!(
            bytes.len(),
            30,
            "{cp:?}: expected 30 valid bytes, got {}",
            bytes.len()
        );
    }
}

// ============================================================================
// 8. Zero handling (positive zero, negative zero)
// ============================================================================

#[test]
fn positive_zero_ascii() {
    let enc = encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(enc, b'{');
    let (d, neg) = decode_overpunch_byte(enc, Codepage::ASCII).unwrap();
    assert_eq!(d, 0);
    assert!(!neg);
}

#[test]
fn negative_zero_ascii() {
    let enc = encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(enc, b'}');
    let (d, neg) = decode_overpunch_byte(enc, Codepage::ASCII).unwrap();
    assert_eq!(d, 0);
    assert!(neg);
}

#[test]
fn positive_zero_ebcdic_positive_policy() {
    let enc = encode_overpunch_byte(0, false, Codepage::CP037, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(enc, 0xC0);
    let (d, neg) = decode_overpunch_byte(enc, Codepage::CP037).unwrap();
    assert_eq!(d, 0);
    assert!(!neg);
}

#[test]
fn negative_zero_ebcdic_positive_policy() {
    let enc = encode_overpunch_byte(0, true, Codepage::CP037, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(enc, 0xD0);
    let (d, neg) = decode_overpunch_byte(enc, Codepage::CP037).unwrap();
    assert_eq!(d, 0);
    assert!(neg);
}

#[test]
fn preferred_zero_ebcdic_ignores_sign() {
    // Both positive and negative zero with Preferred policy → 0xF0
    let pos = encode_overpunch_byte(0, false, Codepage::CP037, ZeroSignPolicy::Preferred).unwrap();
    let neg = encode_overpunch_byte(0, true, Codepage::CP037, ZeroSignPolicy::Preferred).unwrap();
    assert_eq!(pos, 0xF0);
    assert_eq!(neg, 0xF0);
    // Both decode as positive zero
    let (d, n) = decode_overpunch_byte(pos, Codepage::CP037).unwrap();
    assert_eq!(d, 0);
    assert!(!n);
}

#[test]
fn ascii_preferred_zero_policy_has_no_effect() {
    // ASCII ignores ZeroSignPolicy — always uses sign tables
    let pos_pref =
        encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Preferred).unwrap();
    let neg_pref =
        encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Preferred).unwrap();
    let pos_norm =
        encode_overpunch_byte(0, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    let neg_norm =
        encode_overpunch_byte(0, true, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap();
    assert_eq!(pos_pref, pos_norm);
    assert_eq!(neg_pref, neg_norm);
}

// ============================================================================
// 9. Display/Debug traits
// ============================================================================

#[test]
fn zero_sign_policy_debug_display() {
    let pos = ZeroSignPolicy::Positive;
    let pref = ZeroSignPolicy::Preferred;
    // Debug should produce something non-empty
    let pos_dbg = format!("{pos:?}");
    let pref_dbg = format!("{pref:?}");
    assert!(!pos_dbg.is_empty());
    assert!(!pref_dbg.is_empty());
    assert!(pos_dbg.contains("Positive"));
    assert!(pref_dbg.contains("Preferred"));
}

#[test]
fn zero_sign_policy_clone_and_eq() {
    let a = ZeroSignPolicy::Positive;
    let b = a;
    assert_eq!(a, b);
    let c = ZeroSignPolicy::Preferred;
    assert_ne!(a, c);
}

#[test]
fn codepage_debug_for_overpunch() {
    let dbg = format!("{:?}", Codepage::ASCII);
    assert!(dbg.contains("ASCII"));
    let dbg = format!("{:?}", Codepage::CP037);
    assert!(dbg.contains("CP037") || dbg.contains("Cp037"));
}

// ============================================================================
// 10. Boundary: 0x7B ({) and 0x7D (})
// ============================================================================

#[test]
fn boundary_0x7b_is_positive_zero_in_ascii() {
    assert_eq!(b'{', 0x7B);
    let (d, neg) = decode_overpunch_byte(0x7B, Codepage::ASCII).unwrap();
    assert_eq!(d, 0);
    assert!(!neg);
    assert!(is_valid_overpunch(0x7B, Codepage::ASCII));
}

#[test]
fn boundary_0x7d_is_negative_zero_in_ascii() {
    assert_eq!(b'}', 0x7D);
    let (d, neg) = decode_overpunch_byte(0x7D, Codepage::ASCII).unwrap();
    assert_eq!(d, 0);
    assert!(neg);
    assert!(is_valid_overpunch(0x7D, Codepage::ASCII));
}

#[test]
fn boundary_between_positive_and_negative_range() {
    // 0x7B = '{', 0x7C = '|' (not overpunch), 0x7D = '}'
    assert!(is_valid_overpunch(0x7B, Codepage::ASCII));
    assert!(!is_valid_overpunch(0x7C, Codepage::ASCII)); // | is not overpunch
    assert!(is_valid_overpunch(0x7D, Codepage::ASCII));
}

#[test]
fn boundary_0x7b_0x7d_are_not_overpunch_in_ebcdic() {
    // In EBCDIC, 0x7B and 0x7D have zone nibble 0x7 which is invalid
    for cp in ALL_EBCDIC {
        assert!(
            !is_valid_overpunch(0x7B, cp),
            "{cp:?}: 0x7B should not be valid EBCDIC overpunch"
        );
        assert!(
            !is_valid_overpunch(0x7D, cp),
            "{cp:?}: 0x7D should not be valid EBCDIC overpunch"
        );
    }
}

// ============================================================================
// 11. encode_ebcdic_overpunch_zone / decode_ebcdic_overpunch_zone
// ============================================================================

#[test]
fn encode_zone_positive_nonzero() {
    for digit in 1u8..=9 {
        let zone = encode_ebcdic_overpunch_zone(digit, false, ZeroSignPolicy::Positive);
        assert_eq!(zone, 0xC, "positive zone for digit {digit}");
    }
}

#[test]
fn encode_zone_negative_nonzero() {
    for digit in 1u8..=9 {
        let zone = encode_ebcdic_overpunch_zone(digit, true, ZeroSignPolicy::Positive);
        assert_eq!(zone, 0xD, "negative zone for digit {digit}");
    }
}

#[test]
fn encode_zone_preferred_zero_is_0xf() {
    let zone_pos = encode_ebcdic_overpunch_zone(0, false, ZeroSignPolicy::Preferred);
    let zone_neg = encode_ebcdic_overpunch_zone(0, true, ZeroSignPolicy::Preferred);
    assert_eq!(zone_pos, 0xF);
    assert_eq!(zone_neg, 0xF);
}

#[test]
fn decode_zone_valid_values() {
    // 0xC → positive
    let (is_signed, is_neg) = decode_ebcdic_overpunch_zone(0xC).unwrap();
    assert!(is_signed);
    assert!(!is_neg);
    // 0xD → negative
    let (is_signed, is_neg) = decode_ebcdic_overpunch_zone(0xD).unwrap();
    assert!(is_signed);
    assert!(is_neg);
    // 0xF → positive (preferred zero)
    let (is_signed, is_neg) = decode_ebcdic_overpunch_zone(0xF).unwrap();
    assert!(is_signed);
    assert!(!is_neg);
}

#[test]
fn decode_zone_invalid_values() {
    for zone in [
        0x0u8, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xE,
    ] {
        assert!(
            decode_ebcdic_overpunch_zone(zone).is_none(),
            "zone 0x{zone:X} should be invalid"
        );
    }
}

// ============================================================================
// 12. is_valid_overpunch exhaustive
// ============================================================================

#[test]
fn is_valid_overpunch_ascii_complete_enumeration() {
    let valid_bytes: Vec<u8> = (0u8..=255)
        .filter(|&b| is_valid_overpunch(b, Codepage::ASCII))
        .collect();

    // Positive: {, A-I (10), Negative: }, J-R (10), Digits: 0-9 (10) = 30
    assert_eq!(
        valid_bytes.len(),
        30,
        "expected exactly 30 valid ASCII overpunch bytes"
    );

    // Verify specific membership
    assert!(valid_bytes.contains(&b'{'));
    assert!(valid_bytes.contains(&b'}'));
    for ch in b'A'..=b'I' {
        assert!(valid_bytes.contains(&ch), "missing '{}'", ch as char);
    }
    for ch in b'J'..=b'R' {
        assert!(valid_bytes.contains(&ch), "missing '{}'", ch as char);
    }
    for ch in b'0'..=b'9' {
        assert!(valid_bytes.contains(&ch), "missing '{}'", ch as char);
    }
}

#[test]
fn is_valid_overpunch_ascii_rejects_lowercase_letters() {
    for ch in b'a'..=b'z' {
        assert!(
            !is_valid_overpunch(ch, Codepage::ASCII),
            "lowercase '{}' should not be valid",
            ch as char
        );
    }
}

// ============================================================================
// 13. Thread safety
// ============================================================================

#[test]
fn thread_safety_encode_decode_parallel() {
    use std::thread;

    let handles: Vec<_> = (0u8..=9)
        .flat_map(|digit| {
            [false, true].into_iter().map(move |neg| {
                thread::spawn(move || {
                    let enc = encode_overpunch_byte(
                        digit,
                        neg,
                        Codepage::ASCII,
                        ZeroSignPolicy::Positive,
                    )
                    .unwrap();
                    let (d, n) = decode_overpunch_byte(enc, Codepage::ASCII).unwrap();
                    assert_eq!(d, digit);
                    assert_eq!(n, neg);
                })
            })
        })
        .collect();

    for h in handles {
        h.join().expect("thread panicked");
    }
}

// ============================================================================
// 14. ASCII regular digits (0-9) accepted as unsigned
// ============================================================================

#[test]
fn ascii_regular_digits_are_valid_overpunch() {
    for d in 0u8..=9 {
        let byte = b'0' + d;
        assert!(is_valid_overpunch(byte, Codepage::ASCII));
        let (digit, neg) = decode_overpunch_byte(byte, Codepage::ASCII).unwrap();
        assert_eq!(digit, d);
        assert!(!neg, "plain digit {d} should be positive");
    }
}

// ============================================================================
// 15. Error codes are correct
// ============================================================================

#[test]
fn decode_error_code_is_cbkd411_for_ascii() {
    let err = decode_overpunch_byte(0xFF, Codepage::ASCII).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
}

#[test]
fn decode_error_code_is_cbkd411_for_ebcdic_bad_zone() {
    let err = decode_overpunch_byte(0x15, Codepage::CP037).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
}

#[test]
fn encode_error_code_is_cbke501_for_invalid_digit() {
    let err =
        encode_overpunch_byte(10, false, Codepage::ASCII, ZeroSignPolicy::Positive).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}
