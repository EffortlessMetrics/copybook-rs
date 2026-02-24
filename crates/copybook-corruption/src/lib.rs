// SPDX-License-Identifier: AGPL-3.0-or-later
//! Transfer-corruption detection utilities.
//!
//! This crate intentionally owns only heuristics for common text/field transfer
//! corruption patterns used by higher-level codecs.

use copybook_core::{Error, ErrorCode};

/// Heuristics for detecting ASCII transfer corruption in RDW headers.
///
/// This function implements the `CBKF104_RDW_SUSPECT_ASCII` detection logic by
/// checking for patterns that suggest binary data was converted through ASCII
/// transfer by mistake (for example, EBCDIC/ASCII confusion around the length
/// field).
pub fn detect_rdw_ascii_corruption(rdw_bytes: &[u8]) -> Option<Error> {
    if rdw_bytes.len() < 4 {
        return None;
    }

    // Extract the length field (first 2 bytes, big-endian).
    let length_bytes = [rdw_bytes[0], rdw_bytes[1]];
    let length = u16::from_be_bytes(length_bytes);

    // Heuristic 1: Length field contains ASCII digits.
    if is_ascii_digits(&length_bytes) {
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
                "RDW length field suspiciously large ({}), may be ASCII-corrupted: 0x{:04X}",
                length,
                length
            ),
        ));
    }

    // Heuristic 3: Reserved bytes contain ASCII-like printable bytes.
    let reserved_bytes = [rdw_bytes[2], rdw_bytes[3]];
    if is_ascii_printable(&reserved_bytes) && reserved_bytes != [0x00, 0x00] {
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

/// Detect potential EBCDIC corruption in text fields.
pub fn detect_ebcdic_corruption(data: &[u8], field_path: &str) -> Vec<Error> {
    let mut errors = Vec::new();

    for (i, &byte) in data.iter().enumerate() {
        if is_likely_corrupted_ebcdic(byte) {
            let error = Error::new(
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                format!(
                    "Potentially corrupted EBCDIC byte 0x{:02X} at position {} in field {}",
                    byte, i, field_path
                ),
            )
            .with_field(field_path)
            .with_offset(i as u64);

            errors.push(error);
        }
    }

    errors
}

/// Detect patterns in packed decimal that suggest corruption.
pub fn detect_packed_corruption(data: &[u8], field_path: &str) -> Vec<Error> {
    let mut errors = Vec::new();

    for (i, &byte) in data.iter().enumerate() {
        let high_nibble = (byte >> 4) & 0x0F;
        let low_nibble = byte & 0x0F;

        if high_nibble == 0xA || high_nibble == 0xB || high_nibble == 0xE {
            let error = Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!(
                    "invalid high nibble 0x{:X} in packed decimal at byte {} (full byte: 0x{:02X})",
                    high_nibble, i, byte
                ),
            )
            .with_field(field_path)
            .with_offset(i as u64);

            errors.push(error);
        }

        if i == data.len() - 1 {
            // For the last byte, the low nibble is the sign nibble.
            if low_nibble <= 0x9 {
                let error = Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!(
                        "invalid sign nibble 0x{:X} in packed decimal (should be C/D/F), byte {} (full byte: 0x{:02X})",
                        low_nibble,
                        i,
                        byte
                    ),
                )
                .with_field(field_path)
                .with_offset(i as u64);

                errors.push(error);
            }
        } else if low_nibble == 0xA || low_nibble == 0xB || low_nibble == 0xE {
            let error = Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!(
                    "invalid low nibble 0x{:X} in packed decimal at byte {} (full byte: 0x{:02X})",
                    low_nibble,
                    i,
                    byte
                ),
            )
            .with_field(field_path)
            .with_offset(i as u64);

            errors.push(error);
        }
    }

    errors
}

fn is_ascii_digits(bytes: &[u8]) -> bool {
    bytes.iter().all(|&b| b >= b'0' && b <= b'9')
}

fn is_ascii_printable(bytes: &[u8]) -> bool {
    bytes.iter().all(|&b| b >= 0x20 && b <= 0x7E)
}

fn is_likely_corrupted_ebcdic(byte: u8) -> bool {
    match byte {
        // Control chars that are uncommon in normal text payloads.
        0x00..=0x1F => true,
        0x7F..=0x9F => true,
        _ => false,
    }
}

fn ascii_char_or_dot(byte: u8) -> char {
    if byte >= 0x20 && byte <= 0x7E {
        byte as char
    } else {
        '.'
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use anyhow::{anyhow, Result};

    #[test]
    fn test_rdw_ascii_corruption_detection() -> Result<()> {
        let rdw_with_ascii = [b'1', b'2', 0x00, 0x00];
        let result = detect_rdw_ascii_corruption(&rdw_with_ascii)
            .ok_or_else(|| anyhow!("expected ASCII corruption to be detected"))?;
        assert_eq!(result.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);

        let normal_rdw = [0x00, 0x50, 0x00, 0x00];
        let result = detect_rdw_ascii_corruption(&normal_rdw);
        assert!(result.is_none());

        let rdw_with_reserved_ascii = [0x00, 0x50, b'A', b'B'];
        let result = detect_rdw_ascii_corruption(&rdw_with_reserved_ascii);
        assert!(result.is_some());
        Ok(())
    }

    #[test]
    fn test_packed_corruption_detection() {
        let invalid_sign = [0x12, 0x34, 0x56];
        let errors = detect_packed_corruption(&invalid_sign, "TEST.FIELD");
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.message.contains("invalid sign nibble")));

        let invalid_high = [0xA2, 0x34, 0x5C];
        let errors = detect_packed_corruption(&invalid_high, "TEST.FIELD");
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.message.contains("invalid high nibble")));

        let valid_packed = [0x12, 0x34, 0x5C];
        let errors = detect_packed_corruption(&valid_packed, "TEST.FIELD");
        assert!(errors.is_empty(), "Valid packed decimal should not generate errors: {:?}", errors);
    }

    #[test]
    fn test_ebcdic_corruption_detection() {
        let corrupted_data = [0xC1, 0x00, 0x7F, 0xC2];
        let errors = detect_ebcdic_corruption(&corrupted_data, "TEXT.FIELD");
        assert_eq!(errors.len(), 2);

        let normal_data = [0xC1, 0xC2, 0xC3];
        let errors = detect_ebcdic_corruption(&normal_data, "TEXT.FIELD");
        assert!(errors.is_empty());
    }

    #[test]
    fn test_ascii_char_or_dot() {
        assert_eq!(ascii_char_or_dot(b'A'), 'A');
        assert_eq!(ascii_char_or_dot(0x00), '.');
        assert_eq!(ascii_char_or_dot(0x7F), '.');
        assert_eq!(ascii_char_or_dot(b' '), ' ');
    }
}
