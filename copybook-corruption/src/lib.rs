#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Transfer corruption detection utilities.
//!
//! This crate provides heuristics and detection methods for common data transfer
//! corruption patterns, particularly ASCII conversion of binary data.

use copybook_core::{Error, ErrorCode};

/// Heuristics for detecting ASCII transfer corruption in RDW headers.
#[must_use]
pub fn detect_rdw_ascii_corruption(rdw_bytes: &[u8]) -> Option<Error> {
    if rdw_bytes.len() < 4 {
        return None;
    }

    let length_bytes = [rdw_bytes[0], rdw_bytes[1]];
    let length = u16::from_be_bytes(length_bytes);

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

    if length > 0x3030 && length <= 0x3939 {
        return Some(Error::new(
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII,
            format!(
                "RDW length field suspiciously large ({}), may be ASCII-corrupted: 0x{:04X}",
                length, length
            ),
        ));
    }

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
#[must_use]
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
#[must_use]
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

        if i == data.len().saturating_sub(1) {
            if low_nibble <= 0x9 {
                let error = Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!(
                        "invalid sign nibble 0x{:X} in packed decimal (should be C/D/F), byte {} (full byte: 0x{:02X})",
                        low_nibble, i, byte
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
                    low_nibble, i, byte
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
    bytes.iter().all(|&b| b.is_ascii_digit())
}

fn is_ascii_printable(bytes: &[u8]) -> bool {
    bytes.iter().all(u8::is_ascii_graphic)
}

fn is_likely_corrupted_ebcdic(byte: u8) -> bool {
    matches!(byte, 0x00..=0x1F | 0x7F..=0x9F)
}

fn ascii_char_or_dot(byte: u8) -> char {
    if byte.is_ascii_graphic() || byte == b' ' {
        byte as char
    } else {
        '.'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{Result, anyhow};

    #[test]
    fn rdw_ascii_corruption_detection() -> Result<()> {
        let rdw_with_ascii = [b'1', b'2', 0x00, 0x00];
        let result = detect_rdw_ascii_corruption(&rdw_with_ascii)
            .ok_or_else(|| anyhow!("expected ASCII corruption to be detected"))?;
        assert_eq!(result.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);

        let normal_rdw = [0x00, 0x50, 0x00, 0x00];
        assert!(detect_rdw_ascii_corruption(&normal_rdw).is_none());

        let rdw_with_reserved_ascii = [0x00, 0x50, b'A', b'B'];
        assert!(detect_rdw_ascii_corruption(&rdw_with_reserved_ascii).is_some());
        Ok(())
    }

    #[test]
    fn packed_corruption_detection() {
        let invalid_sign = [0x12, 0x34, 0x56];
        let errors = detect_packed_corruption(&invalid_sign, "TEST.FIELD");
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("invalid sign nibble"))
        );

        let invalid_high = [0xA2, 0x34, 0x5C];
        let errors = detect_packed_corruption(&invalid_high, "TEST.FIELD");
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("invalid high nibble"))
        );

        let valid_packed = [0x12, 0x34, 0x5C];
        assert!(detect_packed_corruption(&valid_packed, "TEST.FIELD").is_empty());
    }

    #[test]
    fn ebcdic_corruption_detection() {
        let corrupted_data = [0xC1, 0x00, 0x7F, 0xC2];
        let errors = detect_ebcdic_corruption(&corrupted_data, "TEXT.FIELD");
        assert_eq!(errors.len(), 2);

        let normal_data = [0xC1, 0xC2, 0xC3];
        assert!(detect_ebcdic_corruption(&normal_data, "TEXT.FIELD").is_empty());
    }
}
