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
#[must_use = "Inspect the returned errors to handle corruption findings"]
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
#[must_use = "Inspect the returned errors to handle corruption findings"]
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

    // ── EBCDIC corruption tests ──────────────────────────────────────

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
    fn ebcdic_empty_input_returns_no_errors() {
        assert!(detect_ebcdic_corruption(&[], "EMPTY").is_empty());
    }

    #[test]
    fn ebcdic_single_clean_byte() {
        assert!(detect_ebcdic_corruption(&[0xF0], "F").is_empty());
    }

    #[test]
    fn ebcdic_single_corrupted_byte() {
        let errors = detect_ebcdic_corruption(&[0x01], "F");
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    }

    #[test]
    fn ebcdic_boundary_0x1f_is_corrupted() {
        let errors = detect_ebcdic_corruption(&[0x1F], "BND");
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn ebcdic_boundary_0x20_is_clean() {
        assert!(detect_ebcdic_corruption(&[0x20], "BND").is_empty());
    }

    #[test]
    fn ebcdic_boundary_0x7e_is_clean() {
        assert!(detect_ebcdic_corruption(&[0x7E], "BND").is_empty());
    }

    #[test]
    fn ebcdic_boundary_0x7f_is_corrupted() {
        let errors = detect_ebcdic_corruption(&[0x7F], "BND");
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn ebcdic_boundary_0x9f_is_corrupted() {
        let errors = detect_ebcdic_corruption(&[0x9F], "BND");
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn ebcdic_boundary_0xa0_is_clean() {
        assert!(detect_ebcdic_corruption(&[0xA0], "BND").is_empty());
    }

    #[test]
    fn ebcdic_error_field_path_propagated() {
        let errors = detect_ebcdic_corruption(&[0x00], "REC.GROUP.FIELD");
        assert_eq!(
            errors[0]
                .context
                .as_ref()
                .and_then(|c| c.field_path.as_deref()),
            Some("REC.GROUP.FIELD")
        );
    }

    #[test]
    fn ebcdic_error_offset_is_correct() {
        let data = [0xC1, 0xC2, 0x05, 0xC3];
        let errors = detect_ebcdic_corruption(&data, "F");
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].context.as_ref().and_then(|c| c.byte_offset),
            Some(2)
        );
    }

    #[test]
    fn ebcdic_all_c0_controls_flagged() {
        let data: Vec<u8> = (0x00..=0x1F).collect();
        let errors = detect_ebcdic_corruption(&data, "C0");
        assert_eq!(errors.len(), 32);
    }

    #[test]
    fn ebcdic_all_c1_controls_flagged() {
        let data: Vec<u8> = (0x7F..=0x9F).collect();
        let errors = detect_ebcdic_corruption(&data, "C1");
        assert_eq!(errors.len(), 33);
    }

    // ── Packed decimal corruption tests ──────────────────────────────

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

    #[test]
    fn packed_empty_input_returns_no_errors() {
        assert!(detect_packed_corruption(&[], "EMPTY").is_empty());
    }

    #[test]
    fn packed_single_byte_valid_positive() {
        // 0x1C = digit 1, sign C (positive)
        assert!(detect_packed_corruption(&[0x1C], "S").is_empty());
    }

    #[test]
    fn packed_single_byte_valid_negative() {
        // 0x5D = digit 5, sign D (negative)
        assert!(detect_packed_corruption(&[0x5D], "S").is_empty());
    }

    #[test]
    fn packed_single_byte_valid_unsigned() {
        // 0x3F = digit 3, sign F (unsigned)
        assert!(detect_packed_corruption(&[0x3F], "S").is_empty());
    }

    #[test]
    fn packed_single_byte_invalid_sign() {
        // 0x17 = digit 1, sign nibble 7 (invalid)
        let errors = detect_packed_corruption(&[0x17], "S");
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("invalid sign nibble"));
    }

    #[test]
    fn packed_invalid_low_nibble_non_terminal() {
        // 0x1A in non-terminal position: low nibble A is invalid
        let errors = detect_packed_corruption(&[0x1A, 0x5C], "F");
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("invalid low nibble"))
        );
    }

    #[test]
    fn packed_both_nibbles_invalid() {
        // 0xAB: high nibble A invalid, low nibble B invalid (non-terminal)
        let errors = detect_packed_corruption(&[0xAB, 0x1C], "F");
        assert!(errors.len() >= 2);
    }

    #[test]
    fn packed_error_code_is_comp3_invalid_nibble() {
        let errors = detect_packed_corruption(&[0xA0, 0x1C], "X");
        assert!(
            errors
                .iter()
                .all(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
        );
    }

    #[test]
    fn packed_sign_c_d_f_all_valid() {
        assert!(detect_packed_corruption(&[0x1C], "V").is_empty()); // C = positive
        assert!(detect_packed_corruption(&[0x1D], "V").is_empty()); // D = negative
        assert!(detect_packed_corruption(&[0x1F], "V").is_empty()); // F = unsigned
    }

    #[test]
    fn packed_field_path_in_error() {
        let errors = detect_packed_corruption(&[0xBB], "MY.PACKED.FIELD");
        assert!(
            errors
                .iter()
                .all(|e| e.context.as_ref().and_then(|c| c.field_path.as_deref())
                    == Some("MY.PACKED.FIELD"))
        );
    }
}
