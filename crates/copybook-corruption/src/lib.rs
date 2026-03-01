// SPDX-License-Identifier: AGPL-3.0-or-later
//! Transfer-corruption detection façade.
//!
//! This crate keeps a narrow public surface: all corruption heuristics are
//! delegated to dedicated microcrates and re-exported here for stable call sites.

pub use copybook_corruption_detectors::{detect_ebcdic_corruption, detect_packed_corruption};
pub use copybook_corruption_rdw::detect_rdw_ascii_corruption;

#[cfg(test)]
#[allow(clippy::expect_used, clippy::unwrap_used)]
mod tests {
    use super::*;
    use copybook_core::ErrorCode;

    #[test]
    fn facade_detects_rdw_ascii_corruption() {
        let rdw_with_ascii = [b'1', b'2', 0x00, 0x00];
        let result = detect_rdw_ascii_corruption(&rdw_with_ascii)
            .expect("expected ASCII corruption to be detected");
        assert_eq!(
            result.code,
            copybook_core::ErrorCode::CBKF104_RDW_SUSPECT_ASCII
        );
    }

    #[test]
    fn facade_ebcdic_clean_data_returns_empty() {
        let clean = [0xC1, 0xC2, 0xC3, 0xC4];
        assert!(detect_ebcdic_corruption(&clean, "FIELD-A").is_empty());
    }

    #[test]
    fn facade_ebcdic_detects_null_byte() {
        let data = [0xC1, 0x00, 0xC3];
        let errors = detect_ebcdic_corruption(&data, "REC.FLD");
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    }

    #[test]
    fn facade_ebcdic_empty_data() {
        assert!(detect_ebcdic_corruption(&[], "EMPTY").is_empty());
    }

    #[test]
    fn facade_packed_valid_positive() {
        let valid = [0x12, 0x34, 0x5C]; // +12345
        assert!(detect_packed_corruption(&valid, "AMT").is_empty());
    }

    #[test]
    fn facade_packed_valid_negative() {
        let valid = [0x98, 0x76, 0x5D]; // -98765
        assert!(detect_packed_corruption(&valid, "AMT").is_empty());
    }

    #[test]
    fn facade_packed_valid_unsigned() {
        let valid = [0x01, 0x23, 0x4F]; // unsigned 01234
        assert!(detect_packed_corruption(&valid, "AMT").is_empty());
    }

    #[test]
    fn facade_packed_detects_bad_sign() {
        let bad_sign = [0x12, 0x37]; // sign nibble 7 is invalid
        let errors = detect_packed_corruption(&bad_sign, "BAD");
        assert!(!errors.is_empty());
    }

    #[test]
    fn facade_packed_empty_data() {
        assert!(detect_packed_corruption(&[], "EMPTY").is_empty());
    }

    #[test]
    fn facade_rdw_clean_header_returns_none() {
        let clean = [0x00, 0x50, 0x00, 0x00];
        assert!(detect_rdw_ascii_corruption(&clean).is_none());
    }

    #[test]
    fn facade_rdw_short_input_returns_none() {
        assert!(detect_rdw_ascii_corruption(&[0x31]).is_none());
    }

    #[test]
    fn facade_ebcdic_all_corrupted_range() {
        // Every byte in 0x00..=0x1F is flagged
        let data: Vec<u8> = (0x00..=0x1F).collect();
        let errors = detect_ebcdic_corruption(&data, "CTRL");
        assert_eq!(errors.len(), data.len());
    }
}
