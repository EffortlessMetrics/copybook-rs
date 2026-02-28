// SPDX-License-Identifier: AGPL-3.0-or-later
//! Pure predicate helpers for transfer-corruption heuristics used by higher-level
//! codec error reporting.

/// Return `true` for ASCII-printable bytes.
#[inline]
#[must_use = "This predicate should be used intentionally in byte-level checks"]
pub const fn is_ascii_printable(byte: u8) -> bool {
    byte >= 0x20 && byte <= 0x7E
}

/// Predicate for control/control-like bytes commonly seen in EBCDIC text.
///
/// This intentionally flags:
/// - C0 controls (`0x00..=0x1F`)
/// - C1 controls and related (`0x7F..=0x9F`)
#[inline]
#[must_use = "This predicate should be used intentionally in byte-level checks"]
pub const fn is_likely_corrupted_ebcdic_byte(byte: u8) -> bool {
    matches!(byte, 0x00..=0x1F | 0x7F..=0x9F)
}

/// Detect whether a packed-decimal high nibble can never be part of COMP-3 payload.
#[inline]
#[must_use = "This predicate should be used intentionally in packed-decimal checks"]
pub const fn is_invalid_comp3_high_nibble(byte: u8) -> bool {
    matches!((byte >> 4) & 0x0F, 0xA | 0xB | 0xE)
}

/// Detect whether a packed-decimal non-terminal low nibble can never be part of
/// COMP-3 payload.
#[inline]
#[must_use = "This predicate should be used intentionally in packed-decimal checks"]
pub const fn is_invalid_comp3_low_nibble(byte: u8) -> bool {
    matches!(byte & 0x0F, 0xA | 0xB | 0xE)
}

/// Detect whether a packed-decimal sign nibble (last low nibble) is invalid.
#[inline]
#[must_use = "This predicate should be used intentionally in packed-decimal checks"]
pub const fn is_invalid_comp3_sign_nibble(byte: u8) -> bool {
    matches!(byte & 0x0F, 0x0..=0x9)
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    // ── is_ascii_printable ───────────────────────────────────────────

    #[test]
    fn ascii_printable_examples() {
        assert!(is_ascii_printable(b'A'));
        assert!(is_ascii_printable(b' '));
        assert!(!is_ascii_printable(0x1F));
    }

    #[test]
    fn ascii_printable_lower_boundary() {
        assert!(!is_ascii_printable(0x1F)); // just below range
        assert!(is_ascii_printable(0x20)); // space, first printable
    }

    #[test]
    fn ascii_printable_upper_boundary() {
        assert!(is_ascii_printable(0x7E)); // tilde, last printable
        assert!(!is_ascii_printable(0x7F)); // DEL, not printable
    }

    #[test]
    fn ascii_printable_all_digits() {
        for b in b'0'..=b'9' {
            assert!(is_ascii_printable(b));
        }
    }

    #[test]
    fn ascii_printable_all_uppercase() {
        for b in b'A'..=b'Z' {
            assert!(is_ascii_printable(b));
        }
    }

    #[test]
    fn ascii_printable_null_and_max() {
        assert!(!is_ascii_printable(0x00));
        assert!(!is_ascii_printable(0xFF));
    }

    // ── is_likely_corrupted_ebcdic_byte ──────────────────────────────

    #[test]
    fn likely_corrupted_ebcdic_examples() {
        assert!(is_likely_corrupted_ebcdic_byte(0x00));
        assert!(is_likely_corrupted_ebcdic_byte(0x7F));
        assert!(!is_likely_corrupted_ebcdic_byte(b'A'));
        assert!(!is_likely_corrupted_ebcdic_byte(0xFF));
    }

    #[test]
    fn ebcdic_c0_range_all_corrupted() {
        for b in 0x00..=0x1F {
            assert!(
                is_likely_corrupted_ebcdic_byte(b),
                "byte 0x{b:02X} should be corrupted"
            );
        }
    }

    #[test]
    fn ebcdic_c1_range_all_corrupted() {
        for b in 0x7F..=0x9F {
            assert!(
                is_likely_corrupted_ebcdic_byte(b),
                "byte 0x{b:02X} should be corrupted"
            );
        }
    }

    #[test]
    fn ebcdic_boundary_0x20_not_corrupted() {
        assert!(!is_likely_corrupted_ebcdic_byte(0x20));
    }

    #[test]
    fn ebcdic_boundary_0xa0_not_corrupted() {
        assert!(!is_likely_corrupted_ebcdic_byte(0xA0));
    }

    #[test]
    fn ebcdic_high_bytes_not_corrupted() {
        // 0xC1 is EBCDIC 'A', 0xF0 is EBCDIC '0'
        assert!(!is_likely_corrupted_ebcdic_byte(0xC1));
        assert!(!is_likely_corrupted_ebcdic_byte(0xF0));
    }

    // ── is_invalid_comp3_high_nibble ─────────────────────────────────

    #[test]
    fn comp3_invalid_high_nibble_detection() {
        assert!(is_invalid_comp3_high_nibble(0xA2));
        assert!(is_invalid_comp3_high_nibble(0xBE));
        assert!(!is_invalid_comp3_high_nibble(0x12));
    }

    #[test]
    fn comp3_high_nibble_valid_digits_0_to_9() {
        for high in 0..=9u8 {
            assert!(
                !is_invalid_comp3_high_nibble(high << 4),
                "high nibble {high} should be valid"
            );
        }
    }

    #[test]
    fn comp3_high_nibble_a_b_e_invalid() {
        assert!(is_invalid_comp3_high_nibble(0xA0));
        assert!(is_invalid_comp3_high_nibble(0xB0));
        assert!(is_invalid_comp3_high_nibble(0xE0));
    }

    #[test]
    fn comp3_high_nibble_c_d_f_valid() {
        // C, D, F are valid as sign nibbles but also valid high nibbles
        assert!(!is_invalid_comp3_high_nibble(0xC0));
        assert!(!is_invalid_comp3_high_nibble(0xD0));
        assert!(!is_invalid_comp3_high_nibble(0xF0));
    }

    // ── is_invalid_comp3_low_nibble ──────────────────────────────────

    #[test]
    fn comp3_invalid_low_nibble_detection() {
        assert!(is_invalid_comp3_low_nibble(0x0A));
        assert!(is_invalid_comp3_low_nibble(0x5B));
        assert!(!is_invalid_comp3_low_nibble(0x56));
    }

    #[test]
    fn comp3_low_nibble_valid_digits_0_to_9() {
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
        // C, D, F are valid sign nibbles; not flagged by low nibble check
        assert!(!is_invalid_comp3_low_nibble(0x0C));
        assert!(!is_invalid_comp3_low_nibble(0x0D));
        assert!(!is_invalid_comp3_low_nibble(0x0F));
    }

    // ── is_invalid_comp3_sign_nibble ─────────────────────────────────

    #[test]
    fn comp3_invalid_sign_nibble_detection() {
        assert!(is_invalid_comp3_sign_nibble(0x12));
        assert!(!is_invalid_comp3_sign_nibble(0x1C));
        assert!(!is_invalid_comp3_sign_nibble(0x1D));
    }

    #[test]
    fn comp3_sign_nibble_digits_0_to_9_all_invalid() {
        for low in 0..=9u8 {
            assert!(
                is_invalid_comp3_sign_nibble(low),
                "sign nibble {low} should be invalid"
            );
        }
    }

    #[test]
    fn comp3_sign_nibble_c_d_f_all_valid() {
        assert!(!is_invalid_comp3_sign_nibble(0x0C)); // C = positive
        assert!(!is_invalid_comp3_sign_nibble(0x0D)); // D = negative
        assert!(!is_invalid_comp3_sign_nibble(0x0F)); // F = unsigned
    }

    #[test]
    fn comp3_sign_nibble_a_b_e_also_valid() {
        // A, B, E are not in 0..=9 so they pass the sign check
        assert!(!is_invalid_comp3_sign_nibble(0x0A));
        assert!(!is_invalid_comp3_sign_nibble(0x0B));
        assert!(!is_invalid_comp3_sign_nibble(0x0E));
    }
}
