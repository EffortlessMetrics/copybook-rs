// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW corruption heuristics.
//!
//! This crate owns only RDW ASCII-corruption detection.

use copybook_core::{Error, ErrorCode};
use copybook_corruption_predicates::is_ascii_printable;
use copybook_rdw_predicates::rdw_is_suspect_ascii_corruption_slice;

/// Result type alias using `copybook-error`.
pub type Result<T> = std::result::Result<T, Error>;

/// Heuristics for detecting ASCII transfer corruption in RDW headers.
///
/// This function implements the `CBKF104_RDW_SUSPECT_ASCII` detection logic by
/// checking for patterns that suggest binary data was converted through ASCII
/// transfer by mistake (for example, EBCDIC/ASCII confusion around the length
/// field).
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn detect_rdw_ascii_corruption(rdw_bytes: &[u8]) -> Option<Error> {
    if rdw_bytes.len() < 4 {
        return None;
    }

    // Extract the length field (first 2 bytes, big-endian).
    let length_bytes = [rdw_bytes[0], rdw_bytes[1]];
    let length = u16::from_be_bytes(length_bytes);

    // Heuristic 1: Length field contains ASCII digits.
    if rdw_is_suspect_ascii_corruption_slice(rdw_bytes) {
        return Some(Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!(
                "RDW length field appears to contain ASCII digits: 0x{:02X}{:02X} ('{}{}')",
                rdw_bytes[0],
                rdw_bytes[1],
                ascii_char_or_dot(rdw_bytes[0]),
                ascii_char_or_dot(rdw_bytes[1])
            ),
        ));
    }

    // Heuristic 2: Unreasonably large length values that could be ASCII.
    if length > 0x3030 && length <= 0x3939 {
        // Range covers ASCII '00'..='99' when interpreted as binary.
        return Some(Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!(
                "RDW length field suspiciously large ({length}), may be ASCII-corrupted: 0x{length:04X}"
            ),
        ));
    }

    // Heuristic 3: Reserved bytes contain ASCII-like printable bytes.
    if is_ascii_printable(rdw_bytes[2])
        && is_ascii_printable(rdw_bytes[3])
        && rdw_bytes[2..4] != [0x00, 0x00]
    {
        return Some(Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!(
                "RDW reserved bytes contain ASCII-like data: 0x{:02X}{:02X} ('{}{}')",
                rdw_bytes[2],
                rdw_bytes[3],
                ascii_char_or_dot(rdw_bytes[2]),
                ascii_char_or_dot(rdw_bytes[3])
            ),
        ));
    }

    None
}

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
    use copybook_core::ErrorCode;
    use copybook_rdw_predicates::rdw_is_suspect_ascii_corruption_slice;
    use proptest::prelude::*;

    fn expected_corruption_present(data: &[u8]) -> bool {
        if data.len() < 4 {
            return false;
        }

        let length = u16::from_be_bytes([data[0], data[1]]);
        let reserved_bytes = [data[2], data[3]];
        rdw_is_suspect_ascii_corruption_slice(data)
            || (0x3030u16..=0x3939u16).contains(&length)
            || (is_ascii_printable(data[2])
                && is_ascii_printable(data[3])
                && reserved_bytes != [0x00, 0x00])
    }

    #[test]
    fn detects_ascii_digit_header() {
        let error = detect_rdw_ascii_corruption(b"12\0\0").expect("ASCII digits should be flagged");
        assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }

    #[test]
    fn short_headers_do_not_error() {
        assert!(detect_rdw_ascii_corruption(b"\x31\x32").is_none());
    }

    #[test]
    fn detects_reserved_printables() {
        let error = detect_rdw_ascii_corruption(&[0x00, 0x50, b'A', b'B'])
            .expect("reserved bytes should be flagged");
        assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }

    #[test]
    fn ascii_heuristic_and_reference_match() {
        let header = [b'0', b'1', b'2', b'3'];
        assert_eq!(
            detect_rdw_ascii_corruption(&header).is_some(),
            expected_corruption_present(&header)
        );
    }

    proptest! {
        #[test]
        fn matches_reference_model(data in prop::collection::vec(any::<u8>(), 0..128)) {
            let expected = expected_corruption_present(&data);
            let actual = detect_rdw_ascii_corruption(&data).is_some();
            prop_assert_eq!(actual, expected);
        }
    }
}
