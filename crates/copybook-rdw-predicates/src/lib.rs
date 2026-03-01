// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW header ASCII-digit detection helper.
//!
//! This crate owns exactly one predicate: whether RDW length bytes look like ASCII
//! digits and therefore likely represent ASCII-transfer corruption.

/// RDW header size in bytes.
pub const RDW_HEADER_LEN: usize = 4;

/// Returns `true` if the first two RDW header bytes are ASCII digits, indicating likely ASCII-transfer corruption.
#[inline]
#[must_use]
pub const fn rdw_is_suspect_ascii_corruption(rdw_header: [u8; RDW_HEADER_LEN]) -> bool {
    let b0 = rdw_header[0];
    let b1 = rdw_header[1];

    is_ascii_digit(b0) && is_ascii_digit(b1)
}

/// Slice-based variant of [`rdw_is_suspect_ascii_corruption`] that checks at least 4 bytes.
#[inline]
#[must_use]
pub fn rdw_is_suspect_ascii_corruption_slice(rdw_bytes: &[u8]) -> bool {
    rdw_bytes.len() >= RDW_HEADER_LEN
        && rdw_is_suspect_ascii_corruption([rdw_bytes[0], rdw_bytes[1], rdw_bytes[2], rdw_bytes[3]])
}

#[inline]
#[must_use]
const fn is_ascii_digit(byte: u8) -> bool {
    byte >= b'0' && byte <= b'9'
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn ascii_digit_bytes_are_suspect() {
        assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0x00, 0x00]));
        assert!(rdw_is_suspect_ascii_corruption([b'0', b'9', 0xFF, 0xEE]));
    }

    #[test]
    fn non_ascii_digit_length_bytes_are_not_suspect() {
        assert!(!rdw_is_suspect_ascii_corruption([b'1', b'G', 0x00, 0x00]));
        assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x01, 0x00, 0x00]));
        assert!(!rdw_is_suspect_ascii_corruption([0x31, 0x00, 0x30, 0x30]));
    }

    #[test]
    fn short_headers_are_not_suspect() {
        assert!(!rdw_is_suspect_ascii_corruption_slice(b"12"));
    }

    #[test]
    fn slice_uses_same_rule_as_array() {
        let header = [b'7', b'8', 0x10, 0x20];
        assert!(rdw_is_suspect_ascii_corruption_slice(&header));
    }

    // ── Additional tests ─────────────────────────────────────────────

    #[test]
    fn rdw_header_len_is_four() {
        assert_eq!(RDW_HEADER_LEN, 4);
    }

    #[test]
    fn all_ascii_digit_pairs_suspect() {
        for b0 in b'0'..=b'9' {
            for b1 in b'0'..=b'9' {
                assert!(
                    rdw_is_suspect_ascii_corruption([b0, b1, 0x00, 0x00]),
                    "expected suspect for ({b0}, {b1})"
                );
            }
        }
    }

    #[test]
    fn first_byte_non_digit_not_suspect() {
        // First byte is 0x2F (just below '0')
        assert!(!rdw_is_suspect_ascii_corruption([0x2F, b'5', 0x00, 0x00]));
        // First byte is 0x3A (just above '9')
        assert!(!rdw_is_suspect_ascii_corruption([0x3A, b'5', 0x00, 0x00]));
    }

    #[test]
    fn second_byte_non_digit_not_suspect() {
        assert!(!rdw_is_suspect_ascii_corruption([b'5', 0x2F, 0x00, 0x00]));
        assert!(!rdw_is_suspect_ascii_corruption([b'5', 0x3A, 0x00, 0x00]));
    }

    #[test]
    fn reserved_bytes_do_not_affect_detection() {
        // Detection only looks at first two bytes
        assert!(rdw_is_suspect_ascii_corruption([b'0', b'0', 0xFF, 0xFF]));
        assert!(rdw_is_suspect_ascii_corruption([b'9', b'9', b'A', b'B']));
    }

    #[test]
    fn all_zeros_not_suspect() {
        assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x00, 0x00, 0x00]));
    }

    #[test]
    fn all_0xff_not_suspect() {
        assert!(!rdw_is_suspect_ascii_corruption([0xFF, 0xFF, 0xFF, 0xFF]));
    }

    #[test]
    fn slice_empty_not_suspect() {
        assert!(!rdw_is_suspect_ascii_corruption_slice(&[]));
    }

    #[test]
    fn slice_exactly_4_bytes() {
        assert!(rdw_is_suspect_ascii_corruption_slice(&[
            b'3', b'4', 0x00, 0x00
        ]));
        assert!(!rdw_is_suspect_ascii_corruption_slice(&[
            0x00, 0x50, 0x00, 0x00
        ]));
    }

    #[test]
    fn slice_longer_than_4_uses_first_4() {
        let data = [b'1', b'2', 0x00, 0x00, 0xFF, 0xFF, 0xFF];
        assert!(rdw_is_suspect_ascii_corruption_slice(&data));
    }

    #[test]
    fn slice_3_bytes_not_suspect() {
        assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1', b'2', 0x00]));
    }
}
