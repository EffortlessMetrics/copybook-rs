// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive tests for the corruption detection façade.
//! Tests severity, error content, delegation consistency, and edge cases.

use copybook_core::ErrorCode;
use copybook_corruption::{
    detect_ebcdic_corruption, detect_packed_corruption, detect_rdw_ascii_corruption,
};

// ===========================================================================
// RDW ASCII corruption detection
// ===========================================================================

#[test]
fn rdw_ascii_digits_detected() {
    let header = [b'1', b'2', 0x00, 0x00];
    let err = detect_rdw_ascii_corruption(&header).expect("should detect");
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn rdw_clean_binary_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
}

#[test]
fn rdw_short_input_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[]).is_none());
    assert!(detect_rdw_ascii_corruption(&[0x31]).is_none());
    assert!(detect_rdw_ascii_corruption(&[0x31, 0x32]).is_none());
    assert!(detect_rdw_ascii_corruption(&[0x31, 0x32, 0x00]).is_none());
}

#[test]
fn rdw_reserved_bytes_ascii_printable_detected() {
    // Length bytes clean but reserved bytes are ASCII printable
    let header = [0x00, 0x10, b'X', b'Y'];
    let err = detect_rdw_ascii_corruption(&header).expect("should detect");
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn rdw_reserved_bytes_zero_not_flagged() {
    // Reserved [0x00, 0x00] is valid even though 0x00 < 0x20
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, 0x00, 0x00]).is_none());
}

#[test]
fn rdw_all_zero_not_flagged() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x00, 0x00, 0x00]).is_none());
}

// ===========================================================================
// EBCDIC corruption detection
// ===========================================================================

#[test]
fn ebcdic_clean_data_no_errors() {
    let clean = [0xC1, 0xC2, 0xC3, 0xC4]; // EBCDIC A, B, C, D
    assert!(detect_ebcdic_corruption(&clean, "FIELD-A").is_empty());
}

#[test]
fn ebcdic_empty_data_no_errors() {
    assert!(detect_ebcdic_corruption(&[], "EMPTY").is_empty());
}

#[test]
fn ebcdic_null_byte_detected() {
    let data = [0xC1, 0x00, 0xC3];
    let errors = detect_ebcdic_corruption(&data, "REC.FLD");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

#[test]
fn ebcdic_all_c0_controls_flagged() {
    let data: Vec<u8> = (0x00..=0x1F).collect();
    let errors = detect_ebcdic_corruption(&data, "CTRL");
    assert_eq!(errors.len(), 32);
}

#[test]
fn ebcdic_all_c1_controls_flagged() {
    let data: Vec<u8> = (0x7F..=0x9F).collect();
    let errors = detect_ebcdic_corruption(&data, "CTRL");
    assert_eq!(errors.len(), 33); // 0x7F through 0x9F inclusive
}

#[test]
fn ebcdic_boundary_0x20_clean() {
    assert!(detect_ebcdic_corruption(&[0x20], "BND").is_empty());
}

#[test]
fn ebcdic_boundary_0x1f_corrupted() {
    assert_eq!(detect_ebcdic_corruption(&[0x1F], "BND").len(), 1);
}

#[test]
fn ebcdic_boundary_0x7e_clean() {
    assert!(detect_ebcdic_corruption(&[0x7E], "BND").is_empty());
}

#[test]
fn ebcdic_boundary_0x7f_corrupted() {
    assert_eq!(detect_ebcdic_corruption(&[0x7F], "BND").len(), 1);
}

#[test]
fn ebcdic_boundary_0xa0_clean() {
    assert!(detect_ebcdic_corruption(&[0xA0], "BND").is_empty());
}

#[test]
fn ebcdic_boundary_0x9f_corrupted() {
    assert_eq!(detect_ebcdic_corruption(&[0x9F], "BND").len(), 1);
}

#[test]
fn ebcdic_high_ebcdic_bytes_clean() {
    // Typical EBCDIC values
    let data = [0xC1, 0xD7, 0xE2, 0xF0, 0xF9]; // A, P, S, 0, 9
    assert!(detect_ebcdic_corruption(&data, "EBCDIC").is_empty());
}

#[test]
fn ebcdic_field_path_propagated_in_error() {
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
fn ebcdic_offset_correct_in_error() {
    let data = [0xC1, 0xC2, 0x05, 0xC3]; // byte at index 2 is corrupted
    let errors = detect_ebcdic_corruption(&data, "F");
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].context.as_ref().and_then(|c| c.byte_offset),
        Some(2)
    );
}

#[test]
fn ebcdic_multiple_corrupted_bytes_all_reported() {
    let data = [0x00, 0x01, 0xC1, 0x0F, 0x80]; // 4 corrupted out of 5
    let errors = detect_ebcdic_corruption(&data, "MULTI");
    assert_eq!(errors.len(), 4);
}

// ===========================================================================
// Packed decimal (COMP-3) corruption detection
// ===========================================================================

#[test]
fn packed_valid_positive_no_errors() {
    let valid = [0x12, 0x34, 0x5C]; // +12345
    assert!(detect_packed_corruption(&valid, "AMT").is_empty());
}

#[test]
fn packed_valid_negative_no_errors() {
    let valid = [0x98, 0x76, 0x5D]; // -98765
    assert!(detect_packed_corruption(&valid, "AMT").is_empty());
}

#[test]
fn packed_valid_unsigned_no_errors() {
    let valid = [0x01, 0x23, 0x4F]; // unsigned 01234
    assert!(detect_packed_corruption(&valid, "AMT").is_empty());
}

#[test]
fn packed_empty_data_no_errors() {
    assert!(detect_packed_corruption(&[], "EMPTY").is_empty());
}

#[test]
fn packed_invalid_sign_nibble_detected() {
    let bad_sign = [0x12, 0x37]; // sign nibble 7 is invalid
    let errors = detect_packed_corruption(&bad_sign, "BAD");
    assert!(!errors.is_empty());
    assert!(errors.iter().any(|e| e.message.contains("sign")));
}

#[test]
fn packed_invalid_high_nibble_detected() {
    let invalid = [0xA2, 0x34, 0x5C]; // high nibble 0xA at byte 0
    let errors = detect_packed_corruption(&invalid, "FLD");
    assert!(!errors.is_empty());
    assert!(errors.iter().any(|e| e.message.contains("high nibble")));
}

#[test]
fn packed_invalid_low_nibble_non_terminal_detected() {
    let invalid = [0x1A, 0x5C]; // low nibble 0xA at non-terminal byte 0
    let errors = detect_packed_corruption(&invalid, "FLD");
    assert!(errors.iter().any(|e| e.message.contains("low nibble")));
}

#[test]
fn packed_single_byte_valid_signs() {
    assert!(detect_packed_corruption(&[0x1C], "S").is_empty()); // C = positive
    assert!(detect_packed_corruption(&[0x1D], "S").is_empty()); // D = negative
    assert!(detect_packed_corruption(&[0x1F], "S").is_empty()); // F = unsigned
}

#[test]
fn packed_single_byte_invalid_sign() {
    let errors = detect_packed_corruption(&[0x17], "S"); // sign nibble 7
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
}

#[test]
fn packed_both_nibbles_invalid_in_non_terminal() {
    // 0xAB: high nibble A invalid, low nibble B invalid (non-terminal)
    let errors = detect_packed_corruption(&[0xAB, 0x1C], "FLD");
    assert!(errors.len() >= 2);
}

#[test]
fn packed_error_code_always_comp3_invalid_nibble() {
    let errors = detect_packed_corruption(&[0xA0, 0xB0, 0x17], "X");
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
    );
}

#[test]
fn packed_field_path_in_errors() {
    let errors = detect_packed_corruption(&[0xBB], "MY.PACKED.FIELD");
    assert!(errors.iter().all(
        |e| e.context.as_ref().and_then(|c| c.field_path.as_deref()) == Some("MY.PACKED.FIELD")
    ));
}

#[test]
fn packed_all_valid_digits_no_errors() {
    // Each non-terminal byte has valid digits (0-9) in both nibbles
    // Terminal byte has valid digit in high nibble and valid sign (C) in low
    let valid = [0x00, 0x12, 0x34, 0x56, 0x78, 0x9C];
    assert!(detect_packed_corruption(&valid, "BIG").is_empty());
}

// ===========================================================================
// Facade consistency: all three detectors accessible from same crate
// ===========================================================================

#[test]
fn all_detectors_accessible_via_facade() {
    // RDW detector
    let _ = detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]);

    // EBCDIC detector
    let _ = detect_ebcdic_corruption(&[0xC1], "F");

    // Packed detector
    let _ = detect_packed_corruption(&[0x1C], "F");
}
