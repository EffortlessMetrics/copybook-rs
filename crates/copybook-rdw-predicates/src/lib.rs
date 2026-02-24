// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW header ASCII-digit detection helper.
//!
//! This crate owns exactly one predicate: whether RDW length bytes look like ASCII
//! digits and therefore likely represent ASCII-transfer corruption.

/// RDW header size in bytes.
pub const RDW_HEADER_LEN: usize = 4;

#[inline]
#[must_use]
pub const fn rdw_is_suspect_ascii_corruption(rdw_header: [u8; RDW_HEADER_LEN]) -> bool {
    let b0 = rdw_header[0];
    let b1 = rdw_header[1];

    is_ascii_digit(b0) && is_ascii_digit(b1)
}

#[inline]
#[must_use]
pub fn rdw_is_suspect_ascii_corruption_slice(rdw_bytes: &[u8]) -> bool {
    rdw_bytes.len() >= RDW_HEADER_LEN
        && rdw_is_suspect_ascii_corruption([
            rdw_bytes[0],
            rdw_bytes[1],
            rdw_bytes[2],
            rdw_bytes[3],
        ])
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
        assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1', b'2']));
    }

    #[test]
    fn slice_uses_same_rule_as_array() {
        let header = [b'7', b'8', 0x10, 0x20];
        assert_eq!(rdw_is_suspect_ascii_corruption_slice(&header), true);
    }
}
