// SPDX-License-Identifier: AGPL-3.0-or-later
//! Focused field-level corruption detectors.
//!
//! This microcrate owns only packed-decimal and EBCDIC-byte scanners used by higher-level
//! copybook workflows.

use copybook_core::{Error, ErrorCode};
use copybook_corruption_predicates::{
    is_invalid_comp3_high_nibble, is_invalid_comp3_low_nibble, is_invalid_comp3_sign_nibble,
    is_likely_corrupted_ebcdic_byte,
};

/// Detect potential EBCDIC corruption in text fields.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn detect_ebcdic_corruption(data: &[u8], field_path: &str) -> Vec<Error> {
    let mut errors = Vec::new();

    for (i, &byte) in data.iter().enumerate() {
        if is_likely_corrupted_ebcdic_byte(byte) {
            let error = Error::new(
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                format!(
                    "Potentially corrupted EBCDIC byte 0x{byte:02X} at position {i} in field {field_path}"
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
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn detect_packed_corruption(data: &[u8], field_path: &str) -> Vec<Error> {
    let mut errors = Vec::new();

    for (i, &byte) in data.iter().enumerate() {
        let high_nibble = (byte >> 4) & 0x0F;
        let low_nibble = byte & 0x0F;

        if is_invalid_comp3_high_nibble(byte) {
            let error = Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!(
                    "invalid high nibble 0x{high_nibble:X} in packed decimal at byte {i} (full byte: 0x{byte:02X})"
                ),
            )
            .with_field(field_path)
            .with_offset(i as u64);

            errors.push(error);
        }

        if i == data.len() - 1 {
            if is_invalid_comp3_sign_nibble(byte) {
                let error = Error::new(
                    ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                    format!(
                        "invalid sign nibble 0x{low_nibble:X} in packed decimal (should be C/D/F), byte {i} (full byte: 0x{byte:02X})"
                    ),
                )
                .with_field(field_path)
                .with_offset(i as u64);

                errors.push(error);
            }
        } else if is_invalid_comp3_low_nibble(byte) {
            let error = Error::new(
                ErrorCode::CBKD401_COMP3_INVALID_NIBBLE,
                format!(
                    "invalid low nibble 0x{low_nibble:X} in packed decimal at byte {i} (full byte: 0x{byte:02X})"
                ),
            )
            .with_field(field_path)
            .with_offset(i as u64);

            errors.push(error);
        }
    }

    errors
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn detect_ebcdic_corruption_finds_corrupted_bytes() {
        let corrupted_data = [0xC1, 0x00, 0x7F, 0xC2];
        let errors = detect_ebcdic_corruption(&corrupted_data, "TEXT.FIELD");
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn detect_ebcdic_corruption_ignores_clean_input() {
        let normal_data = [0xC1, 0xC2, 0xC3];
        assert!(detect_ebcdic_corruption(&normal_data, "TEXT.FIELD").is_empty());
    }

    #[test]
    fn detect_packed_corruption_flags_invalid_sign() {
        let invalid_sign = [0x12, 0x34, 0x56];
        let errors = detect_packed_corruption(&invalid_sign, "TEST.FIELD");
        assert!(!errors.is_empty());
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("invalid sign nibble"))
        );
    }

    #[test]
    fn detect_packed_corruption_flags_invalid_high_nibble() {
        // 0xA2: high nibble A (>9) is invalid for packed decimal
        let invalid = [0xA2, 0x34, 0x5C];
        let errors = detect_packed_corruption(&invalid, "TEST.FIELD");
        assert!(!errors.is_empty());
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("invalid high nibble"))
        );
    }

    #[test]
    fn detect_packed_corruption_tolerates_valid_payload() {
        let valid_packed = [0x12, 0x34, 0x5C];
        let errors = detect_packed_corruption(&valid_packed, "TEST.FIELD");
        assert!(errors.is_empty());
    }
}
