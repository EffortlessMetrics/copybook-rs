#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW-specific diagnostics.
//!
//! This module owns byte-level RDW transfer-corruption heuristics. It does not
//! interpret copybook schemas or codec layout semantics.

use copybook_error::{Error, ErrorCode};

/// Returns `true` when the first two RDW bytes look like ASCII digits.
///
/// This is a transfer-corruption heuristic, not a complete validity check for
/// an RDW header.
#[inline]
#[must_use]
pub const fn rdw_is_suspect_ascii_corruption(rdw_header: [u8; crate::RDW_HEADER_LEN]) -> bool {
    is_ascii_digit(rdw_header[0]) && is_ascii_digit(rdw_header[1])
}

/// Slice-based variant of [`rdw_is_suspect_ascii_corruption`].
#[inline]
#[must_use]
pub fn rdw_is_suspect_ascii_corruption_slice(rdw_bytes: &[u8]) -> bool {
    rdw_bytes.len() >= crate::RDW_HEADER_LEN
        && rdw_is_suspect_ascii_corruption([rdw_bytes[0], rdw_bytes[1], rdw_bytes[2], rdw_bytes[3]])
}

/// Detect likely ASCII-transfer corruption in an RDW header.
///
/// The returned stable error includes the observed header bytes and the
/// suspected remediation: verify that the source was transferred in binary
/// mode and that the RDW header was not passed through text conversion.
#[inline]
#[must_use = "Handle the returned error when corruption is detected"]
pub fn detect_rdw_ascii_corruption(rdw_bytes: &[u8]) -> Option<Error> {
    if rdw_bytes.len() < crate::RDW_HEADER_LEN {
        return None;
    }

    let length = u16::from_be_bytes([rdw_bytes[0], rdw_bytes[1]]);

    if rdw_is_suspect_ascii_corruption_slice(rdw_bytes) {
        return Some(ascii_corruption_error(format!(
            "RDW length field appears to contain ASCII digits: 0x{:02X}{:02X} ('{}{}')",
            rdw_bytes[0],
            rdw_bytes[1],
            ascii_char_or_dot(rdw_bytes[0]),
            ascii_char_or_dot(rdw_bytes[1])
        )));
    }

    if (0x3030..=0x3939).contains(&length) {
        return Some(ascii_corruption_error(format!(
            "RDW length field suspiciously large ({length}), may be ASCII-corrupted: 0x{length:04X}"
        )));
    }

    if is_ascii_printable(rdw_bytes[2])
        && is_ascii_printable(rdw_bytes[3])
        && rdw_bytes[2..4] != [0x00, 0x00]
    {
        return Some(ascii_corruption_error(format!(
            "RDW reserved bytes contain ASCII-like data: 0x{:02X}{:02X} ('{}{}')",
            rdw_bytes[2],
            rdw_bytes[3],
            ascii_char_or_dot(rdw_bytes[2]),
            ascii_char_or_dot(rdw_bytes[3])
        )));
    }

    None
}

#[inline]
fn ascii_corruption_error(detail: impl Into<String>) -> Error {
    let detail = detail.into();
    Error::new(
        ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
        format!("{detail}; verify binary transfer mode and preserve the RDW header bytes"),
    )
}

#[inline]
#[must_use]
const fn is_ascii_digit(byte: u8) -> bool {
    byte >= b'0' && byte <= b'9'
}

#[inline]
#[must_use]
const fn is_ascii_printable(byte: u8) -> bool {
    byte >= 0x20 && byte <= 0x7E
}

#[inline]
fn ascii_char_or_dot(byte: u8) -> char {
    if is_ascii_printable(byte) {
        byte as char
    } else {
        '.'
    }
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn digit_length_bytes_are_suspect() {
        assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0, 0]));
        assert!(!rdw_is_suspect_ascii_corruption([0, 0x50, 0, 0]));
    }

    #[test]
    fn slice_requires_a_complete_header() {
        assert!(!rdw_is_suspect_ascii_corruption_slice(b"12"));
        assert!(rdw_is_suspect_ascii_corruption_slice(b"12\0\0"));
    }

    #[test]
    fn detects_ascii_digit_length_with_remediation() {
        let error = detect_rdw_ascii_corruption(b"12\0\0").expect("digits should be detected");

        assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
        assert!(error.message.contains("binary transfer mode"));
    }

    #[test]
    fn detects_printable_reserved_bytes() {
        let error = detect_rdw_ascii_corruption(&[0x00, 0x50, b'A', b'B'])
            .expect("printable reserved bytes should be detected");

        assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
        assert!(error.message.contains("reserved bytes"));
    }

    #[test]
    fn clean_binary_header_is_not_corrupt() {
        assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
    }

    #[test]
    fn short_input_is_not_corrupt() {
        assert!(detect_rdw_ascii_corruption(&[b'1', b'2', 0]).is_none());
    }
}
