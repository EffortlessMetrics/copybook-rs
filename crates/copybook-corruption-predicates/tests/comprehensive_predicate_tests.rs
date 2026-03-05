// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive tests for corruption predicate functions.
//! Tests all five predicates with boundary analysis and exhaustive range checks.

use copybook_corruption_predicates::{
    is_ascii_printable, is_invalid_comp3_high_nibble, is_invalid_comp3_low_nibble,
    is_invalid_comp3_sign_nibble, is_likely_corrupted_ebcdic_byte,
};

// ===========================================================================
// is_ascii_printable
// ===========================================================================

#[test]
fn ascii_printable_space_is_first() {
    assert!(is_ascii_printable(0x20)); // space
}

#[test]
fn ascii_printable_tilde_is_last() {
    assert!(is_ascii_printable(0x7E)); // ~
}

#[test]
fn ascii_printable_below_space_not_printable() {
    assert!(!is_ascii_printable(0x1F));
    assert!(!is_ascii_printable(0x00));
}

#[test]
fn ascii_printable_above_tilde_not_printable() {
    assert!(!is_ascii_printable(0x7F)); // DEL
    assert!(!is_ascii_printable(0x80));
    assert!(!is_ascii_printable(0xFF));
}

#[test]
fn ascii_printable_all_letters_and_digits() {
    for b in b'A'..=b'Z' {
        assert!(is_ascii_printable(b), "uppercase {b:#04x}");
    }
    for b in b'a'..=b'z' {
        assert!(is_ascii_printable(b), "lowercase {b:#04x}");
    }
    for b in b'0'..=b'9' {
        assert!(is_ascii_printable(b), "digit {b:#04x}");
    }
}

#[test]
fn ascii_printable_common_symbols() {
    let symbols = b"!@#$%^&*()-_=+[]{}|;:',.<>?/`~\"\\";
    for &b in symbols {
        assert!(is_ascii_printable(b), "symbol {b:#04x}");
    }
}

#[test]
fn ascii_printable_count_is_95() {
    let count = (0u8..=255).filter(|&b| is_ascii_printable(b)).count();
    assert_eq!(count, 95); // 0x20..=0x7E = 95 bytes
}

// ===========================================================================
// is_likely_corrupted_ebcdic_byte
// ===========================================================================

#[test]
fn ebcdic_c0_controls_all_corrupted() {
    for b in 0x00..=0x1F {
        assert!(
            is_likely_corrupted_ebcdic_byte(b),
            "C0 control 0x{b:02X} should be corrupted"
        );
    }
}

#[test]
fn ebcdic_c1_controls_and_del_all_corrupted() {
    for b in 0x7F..=0x9F {
        assert!(
            is_likely_corrupted_ebcdic_byte(b),
            "C1/DEL 0x{b:02X} should be corrupted"
        );
    }
}

#[test]
fn ebcdic_gap_0x20_to_0x7e_not_corrupted() {
    for b in 0x20..=0x7E {
        assert!(
            !is_likely_corrupted_ebcdic_byte(b),
            "0x{b:02X} should NOT be corrupted"
        );
    }
}

#[test]
fn ebcdic_high_range_0xa0_to_0xff_not_corrupted() {
    for b in 0xA0..=0xFF {
        assert!(
            !is_likely_corrupted_ebcdic_byte(b),
            "0x{b:02X} should NOT be corrupted"
        );
    }
}

#[test]
fn ebcdic_typical_ebcdic_letters_not_corrupted() {
    // EBCDIC: A=0xC1, Z=0xE9, 0=0xF0, 9=0xF9
    assert!(!is_likely_corrupted_ebcdic_byte(0xC1));
    assert!(!is_likely_corrupted_ebcdic_byte(0xE9));
    assert!(!is_likely_corrupted_ebcdic_byte(0xF0));
    assert!(!is_likely_corrupted_ebcdic_byte(0xF9));
}

#[test]
fn ebcdic_corrupted_count_is_65() {
    let count = (0u8..=255)
        .filter(|&b| is_likely_corrupted_ebcdic_byte(b))
        .count();
    // 0x00..=0x1F = 32, 0x7F..=0x9F = 33, total = 65
    assert_eq!(count, 65);
}

// ===========================================================================
// is_invalid_comp3_high_nibble
// ===========================================================================

#[test]
fn comp3_high_nibble_digits_0_to_9_valid() {
    for high in 0..=9u8 {
        let byte = high << 4;
        assert!(
            !is_invalid_comp3_high_nibble(byte),
            "high nibble {high} should be valid"
        );
    }
}

#[test]
fn comp3_high_nibble_a_b_e_invalid() {
    assert!(is_invalid_comp3_high_nibble(0xA0)); // A
    assert!(is_invalid_comp3_high_nibble(0xB0)); // B
    assert!(is_invalid_comp3_high_nibble(0xE0)); // E
}

#[test]
fn comp3_high_nibble_c_d_f_valid() {
    assert!(!is_invalid_comp3_high_nibble(0xC0)); // C
    assert!(!is_invalid_comp3_high_nibble(0xD0)); // D
    assert!(!is_invalid_comp3_high_nibble(0xF0)); // F
}

#[test]
fn comp3_high_nibble_low_nibble_does_not_affect() {
    // High nibble A with various low nibbles — always invalid
    for low in 0..=0x0F {
        assert!(
            is_invalid_comp3_high_nibble(0xA0 | low),
            "0xA{low:X} should have invalid high nibble"
        );
    }
    // High nibble 5 with various low nibbles — always valid
    for low in 0..=0x0F {
        assert!(
            !is_invalid_comp3_high_nibble(0x50 | low),
            "0x5{low:X} should have valid high nibble"
        );
    }
}

// ===========================================================================
// is_invalid_comp3_low_nibble
// ===========================================================================

#[test]
fn comp3_low_nibble_digits_0_to_9_valid() {
    for low in 0..=9u8 {
        assert!(
            !is_invalid_comp3_low_nibble(low),
            "low nibble {low} should be valid"
        );
    }
}

#[test]
fn comp3_low_nibble_a_b_e_invalid() {
    assert!(is_invalid_comp3_low_nibble(0x0A));
    assert!(is_invalid_comp3_low_nibble(0x0B));
    assert!(is_invalid_comp3_low_nibble(0x0E));
}

#[test]
fn comp3_low_nibble_c_d_f_valid() {
    assert!(!is_invalid_comp3_low_nibble(0x0C));
    assert!(!is_invalid_comp3_low_nibble(0x0D));
    assert!(!is_invalid_comp3_low_nibble(0x0F));
}

#[test]
fn comp3_low_nibble_high_nibble_does_not_affect() {
    // Low nibble A with various high nibbles — always invalid
    for high in 0..=0x0F {
        assert!(
            is_invalid_comp3_low_nibble((high << 4) | 0x0A),
            "0x{high:X}A should have invalid low nibble"
        );
    }
    // Low nibble 5 with various high nibbles — always valid
    for high in 0..=0x0F {
        assert!(
            !is_invalid_comp3_low_nibble((high << 4) | 0x05),
            "0x{high:X}5 should have valid low nibble"
        );
    }
}

// ===========================================================================
// is_invalid_comp3_sign_nibble
// ===========================================================================

#[test]
fn comp3_sign_digits_0_to_9_invalid() {
    for low in 0..=9u8 {
        assert!(
            is_invalid_comp3_sign_nibble(low),
            "sign nibble {low} should be invalid (it's a digit, not a sign)"
        );
    }
}

#[test]
fn comp3_sign_c_d_f_valid() {
    assert!(!is_invalid_comp3_sign_nibble(0x0C)); // C = positive
    assert!(!is_invalid_comp3_sign_nibble(0x0D)); // D = negative
    assert!(!is_invalid_comp3_sign_nibble(0x0F)); // F = unsigned
}

#[test]
fn comp3_sign_a_b_e_also_valid() {
    // A, B, E have low nibbles 0xA, 0xB, 0xE which are >= 0xA, not in 0..=9
    assert!(!is_invalid_comp3_sign_nibble(0x0A));
    assert!(!is_invalid_comp3_sign_nibble(0x0B));
    assert!(!is_invalid_comp3_sign_nibble(0x0E));
}

#[test]
fn comp3_sign_high_nibble_does_not_affect() {
    // Digit low nibble 5 is always invalid as a sign, regardless of high nibble
    for high in 0..=0x0F {
        assert!(
            is_invalid_comp3_sign_nibble((high << 4) | 0x05),
            "0x{high:X}5 should have invalid sign nibble"
        );
    }
    // C low nibble is always valid as a sign
    for high in 0..=0x0F {
        assert!(
            !is_invalid_comp3_sign_nibble((high << 4) | 0x0C),
            "0x{high:X}C should have valid sign nibble"
        );
    }
}

// ===========================================================================
// Cross-predicate consistency
// ===========================================================================

#[test]
fn comp3_sign_and_low_nibble_relationship() {
    // For a terminal byte, the sign nibble check applies.
    // For a non-terminal byte, the low nibble check applies.
    // A, B, E are invalid as low nibbles but valid as sign nibbles.
    // 0-9 are valid as low nibbles but invalid as sign nibbles.
    for val in [0x0A_u8, 0x0B, 0x0E] {
        assert!(is_invalid_comp3_low_nibble(val), "low nibble check");
        assert!(!is_invalid_comp3_sign_nibble(val), "sign nibble check");
    }
    for val in 0..=9u8 {
        assert!(!is_invalid_comp3_low_nibble(val), "low nibble check");
        assert!(is_invalid_comp3_sign_nibble(val), "sign nibble check");
    }
}

#[test]
fn comp3_c_d_f_valid_everywhere() {
    // C, D, F are valid as both low nibbles and sign nibbles
    for val in [0x0C_u8, 0x0D, 0x0F] {
        assert!(!is_invalid_comp3_low_nibble(val));
        assert!(!is_invalid_comp3_sign_nibble(val));
    }
}
