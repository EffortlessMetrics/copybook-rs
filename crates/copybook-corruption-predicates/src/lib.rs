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

    #[test]
    fn ascii_printable_examples() {
        assert!(is_ascii_printable(b'A'));
        assert!(is_ascii_printable(b' '));
        assert!(!is_ascii_printable(0x1F));
    }

    #[test]
    fn likely_corrupted_ebcdic_examples() {
        assert!(is_likely_corrupted_ebcdic_byte(0x00));
        assert!(is_likely_corrupted_ebcdic_byte(0x7F));
        assert!(!is_likely_corrupted_ebcdic_byte(b'A'));
        assert!(!is_likely_corrupted_ebcdic_byte(0xFF));
    }

    #[test]
    fn comp3_invalid_high_nibble_detection() {
        assert!(is_invalid_comp3_high_nibble(0xA2));
        assert!(is_invalid_comp3_high_nibble(0xBE));
        assert!(!is_invalid_comp3_high_nibble(0x12));
    }

    #[test]
    fn comp3_invalid_low_nibble_detection() {
        assert!(is_invalid_comp3_low_nibble(0x0A));
        assert!(is_invalid_comp3_low_nibble(0x5B));
        assert!(!is_invalid_comp3_low_nibble(0x56));
    }

    #[test]
    fn comp3_invalid_sign_nibble_detection() {
        assert!(is_invalid_comp3_sign_nibble(0x12));
        assert!(!is_invalid_comp3_sign_nibble(0x1C));
        assert!(!is_invalid_comp3_sign_nibble(0x1D));
    }
}
