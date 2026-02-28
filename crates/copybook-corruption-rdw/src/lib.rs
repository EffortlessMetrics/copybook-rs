// SPDX-License-Identifier: AGPL-3.0-or-later
//! RDW corruption heuristics.
//!
//! This crate owns only RDW ASCII-corruption detection.

use copybook_core::{Error, ErrorCode};
use copybook_corruption_predicates::is_ascii_printable;
use copybook_rdw_predicates::rdw_is_suspect_ascii_corruption_slice;

/// Heuristics for detecting ASCII transfer corruption in RDW headers.
///
/// This function implements the `CBKF104_RDW_SUSPECT_ASCII` detection logic by
/// checking for patterns that suggest binary data was converted through ASCII
/// transfer by mistake (for example, EBCDIC/ASCII confusion around the length
/// field).
#[inline]
#[must_use = "Handle the returned error when corruption is detected"]
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

    #[test]
    fn empty_input_returns_none() {
        assert!(detect_rdw_ascii_corruption(&[]).is_none());
    }

    #[test]
    fn exactly_3_bytes_returns_none() {
        assert!(detect_rdw_ascii_corruption(&[b'1', b'2', 0x00]).is_none());
    }

    #[test]
    fn clean_binary_header_returns_none() {
        // Normal RDW: length=80, reserved=0x0000
        assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
    }

    #[test]
    fn heuristic2_length_in_ascii_range_0x3031() {
        // 0x3031 is within 0x3030..=0x3939 (ASCII "01")
        let result = detect_rdw_ascii_corruption(&[0x30, 0x31, 0x00, 0x00]);
        // Heuristic 1 fires first since both are ASCII digits
        assert!(result.is_some());
        assert_eq!(result.unwrap().code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }

    #[test]
    fn heuristic2_length_0x3939_upper_bound() {
        // 0x3939 = ASCII "99", within suspect range
        let result = detect_rdw_ascii_corruption(&[0x39, 0x39, 0x00, 0x00]);
        assert!(result.is_some());
    }

    #[test]
    fn heuristic3_reserved_both_printable() {
        // Length bytes are not ASCII digits, but reserved bytes are printable
        let result = detect_rdw_ascii_corruption(&[0x00, 0x10, b'X', b'Y']);
        assert!(result.is_some());
        assert!(result.unwrap().message.contains("reserved bytes"));
    }

    #[test]
    fn heuristic3_reserved_one_printable_one_not() {
        // Only one reserved byte is printable — not flagged by heuristic 3
        assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, b'X', 0x01]).is_none());
    }

    #[test]
    fn heuristic3_reserved_both_zero_not_flagged() {
        // Reserved bytes [0x00, 0x00] are excluded even though 0x00 is not printable
        assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, 0x00, 0x00]).is_none());
    }

    #[test]
    fn longer_input_only_first_4_bytes_matter() {
        let data = [b'5', b'6', 0x00, 0x00, 0xFF, 0xFF, 0xFF];
        let result = detect_rdw_ascii_corruption(&data);
        assert!(result.is_some());
    }

    #[test]
    fn all_zeros_returns_none() {
        assert!(detect_rdw_ascii_corruption(&[0x00, 0x00, 0x00, 0x00]).is_none());
    }

    #[test]
    fn all_0xff_returns_none() {
        // 0xFF is not an ASCII digit and not ASCII printable
        assert!(detect_rdw_ascii_corruption(&[0xFF, 0xFF, 0xFF, 0xFF]).is_none());
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
