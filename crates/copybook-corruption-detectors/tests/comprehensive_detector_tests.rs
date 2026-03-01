// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive tests for the EBCDIC and packed decimal corruption detectors.

use copybook_core::ErrorCode;
use copybook_corruption_detectors::{detect_ebcdic_corruption, detect_packed_corruption};

// ===========================================================================
// EBCDIC corruption detector
// ===========================================================================

#[test]
fn ebcdic_empty_input_no_errors() {
    assert!(detect_ebcdic_corruption(&[], "EMPTY").is_empty());
}

#[test]
fn ebcdic_single_clean_byte() {
    assert!(detect_ebcdic_corruption(&[0xC1], "F").is_empty()); // EBCDIC 'A'
}

#[test]
fn ebcdic_single_corrupted_null() {
    let errors = detect_ebcdic_corruption(&[0x00], "F");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

#[test]
fn ebcdic_mixed_clean_and_corrupted() {
    let data = [0xC1, 0x00, 0xC2, 0x7F, 0xC3]; // corrupted at pos 1 and 3
    let errors = detect_ebcdic_corruption(&data, "MIX");
    assert_eq!(errors.len(), 2);
}

#[test]
fn ebcdic_all_c0_controls_detected() {
    let data: Vec<u8> = (0x00..=0x1F).collect();
    let errors = detect_ebcdic_corruption(&data, "C0");
    assert_eq!(errors.len(), 32);
}

#[test]
fn ebcdic_all_c1_controls_detected() {
    let data: Vec<u8> = (0x7F..=0x9F).collect();
    let errors = detect_ebcdic_corruption(&data, "C1");
    assert_eq!(errors.len(), 33);
}

#[test]
fn ebcdic_clean_range_0x20_to_0x7e() {
    let data: Vec<u8> = (0x20..=0x7E).collect();
    assert!(detect_ebcdic_corruption(&data, "CLEAN").is_empty());
}

#[test]
fn ebcdic_clean_range_0xa0_to_0xff() {
    let data: Vec<u8> = (0xA0..=0xFF).collect();
    assert!(detect_ebcdic_corruption(&data, "HIGH").is_empty());
}

#[test]
fn ebcdic_error_offsets_sequential() {
    let data = [0x01, 0x02, 0x03]; // all corrupted
    let errors = detect_ebcdic_corruption(&data, "F");
    for (i, err) in errors.iter().enumerate() {
        assert_eq!(
            err.context.as_ref().and_then(|c| c.byte_offset),
            Some(i as u64),
            "offset mismatch at index {i}"
        );
    }
}

#[test]
fn ebcdic_field_path_propagated() {
    let errors = detect_ebcdic_corruption(&[0x00], "MY.RECORD.FIELD");
    assert_eq!(
        errors[0]
            .context
            .as_ref()
            .and_then(|c| c.field_path.as_deref()),
        Some("MY.RECORD.FIELD")
    );
}

#[test]
fn ebcdic_boundary_values() {
    assert_eq!(detect_ebcdic_corruption(&[0x00], "B").len(), 1);
    assert_eq!(detect_ebcdic_corruption(&[0x1F], "B").len(), 1);
    assert!(detect_ebcdic_corruption(&[0x20], "B").is_empty());
    assert_eq!(detect_ebcdic_corruption(&[0x7F], "B").len(), 1);
    assert_eq!(detect_ebcdic_corruption(&[0x9F], "B").len(), 1);
    assert!(detect_ebcdic_corruption(&[0x7E], "B").is_empty());
    assert!(detect_ebcdic_corruption(&[0xA0], "B").is_empty());
}

#[test]
fn ebcdic_error_message_contains_hex() {
    let errors = detect_ebcdic_corruption(&[0x0A], "FLD");
    assert!(errors[0].message.contains("0x0A"));
}

// ===========================================================================
// Packed decimal (COMP-3) corruption detector
// ===========================================================================

#[test]
fn packed_empty_input_no_errors() {
    assert!(detect_packed_corruption(&[], "EMPTY").is_empty());
}

#[test]
fn packed_valid_single_byte_positive() {
    assert!(detect_packed_corruption(&[0x1C], "V").is_empty());
}

#[test]
fn packed_valid_single_byte_negative() {
    assert!(detect_packed_corruption(&[0x5D], "V").is_empty());
}

#[test]
fn packed_valid_single_byte_unsigned() {
    assert!(detect_packed_corruption(&[0x3F], "V").is_empty());
}

#[test]
fn packed_valid_multi_byte() {
    let valid = [0x12, 0x34, 0x56, 0x78, 0x9C];
    assert!(detect_packed_corruption(&valid, "AMT").is_empty());
}

#[test]
fn packed_invalid_sign_digit_as_sign() {
    for sign in 0..=9u8 {
        let byte = 0x10 | sign;
        let errors = detect_packed_corruption(&[byte], "S");
        assert!(!errors.is_empty(), "sign nibble {sign} should be invalid");
        assert!(errors.iter().any(|e| e.message.contains("sign")));
    }
}

#[test]
fn packed_valid_sign_nibbles() {
    for sign in 0xA..=0xF {
        let byte = 0x10 | sign;
        assert!(
            detect_packed_corruption(&[byte], "S").is_empty(),
            "sign nibble 0x{sign:X} should be valid"
        );
    }
}

#[test]
fn packed_invalid_high_nibble_a_b_e() {
    for high in [0xA0, 0xB0, 0xE0] {
        let byte = high | 0x0C;
        let errors = detect_packed_corruption(&[byte], "H");
        assert!(
            errors.iter().any(|e| e.message.contains("high nibble")),
            "high nibble 0x{:X} should be flagged",
            high >> 4
        );
    }
}

#[test]
fn packed_invalid_low_nibble_non_terminal_a_b_e() {
    for low in [0x0A_u8, 0x0B, 0x0E] {
        let non_terminal = 0x10 | low;
        let errors = detect_packed_corruption(&[non_terminal, 0x1C], "L");
        assert!(
            errors.iter().any(|e| e.message.contains("low nibble")),
            "low nibble 0x{low:X} should be flagged"
        );
    }
}

#[test]
fn packed_both_nibbles_bad_in_non_terminal() {
    let errors = detect_packed_corruption(&[0xAB, 0x1C], "BOTH");
    assert!(errors.len() >= 2);
}

#[test]
fn packed_error_code_consistency() {
    let errors = detect_packed_corruption(&[0xAB, 0xE0, 0x17], "X");
    for err in &errors {
        assert_eq!(err.code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
    }
}

#[test]
fn packed_field_path_in_all_errors() {
    let errors = detect_packed_corruption(&[0xAB], "MY.PACKED");
    for err in &errors {
        assert_eq!(
            err.context.as_ref().and_then(|c| c.field_path.as_deref()),
            Some("MY.PACKED")
        );
    }
}

#[test]
fn packed_offset_correct() {
    let errors = detect_packed_corruption(&[0x12, 0xA0, 0x1C], "F");
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].context.as_ref().and_then(|c| c.byte_offset),
        Some(1)
    );
}

#[test]
fn packed_terminal_vs_nonterminal_low_nibble_distinction() {
    // 0x1A as non-terminal: low nibble A is invalid
    let errors_non_terminal = detect_packed_corruption(&[0x1A, 0x1C], "NT");
    assert!(
        errors_non_terminal
            .iter()
            .any(|e| e.message.contains("low nibble"))
    );

    // 0x1A as terminal: low nibble A is valid as a sign nibble
    let errors_terminal = detect_packed_corruption(&[0x1A], "T");
    assert!(
        !errors_terminal.iter().any(|e| e.message.contains("sign")),
        "A (0xA) is a valid sign nibble"
    );
}

#[test]
fn packed_all_zeros_has_invalid_sign() {
    let errors = detect_packed_corruption(&[0x00], "Z");
    assert!(errors.iter().any(|e| e.message.contains("sign")));
}

#[test]
fn packed_realistic_amount_field() {
    let valid = [0x01, 0x23, 0x45, 0x6C];
    assert!(detect_packed_corruption(&valid, "AMOUNT").is_empty());
}

#[test]
fn packed_zero_value() {
    assert!(detect_packed_corruption(&[0x0C], "ZERO").is_empty());
}
