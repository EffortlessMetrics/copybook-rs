// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Independent tests for each corruption detector algorithm, covering clean
//! data, corrupted data, and boundary conditions.

use copybook_core::ErrorCode;
use copybook_corruption_detectors::{detect_ebcdic_corruption, detect_packed_corruption};

// ===========================================================================
// EBCDIC detector — clean data passes
// ===========================================================================

#[test]
fn ebcdic_clean_uppercase_letters() {
    // EBCDIC uppercase A-I (0xC1-0xC9)
    let data: Vec<u8> = (0xC1..=0xC9).collect();
    assert!(detect_ebcdic_corruption(&data, "LETTERS").is_empty());
}

#[test]
fn ebcdic_clean_digits_f0_f9() {
    // EBCDIC digits 0-9 (0xF0-0xF9)
    let data: Vec<u8> = (0xF0..=0xF9).collect();
    assert!(detect_ebcdic_corruption(&data, "DIGITS").is_empty());
}

#[test]
fn ebcdic_clean_space_0x40() {
    // 0x40 is EBCDIC space — well within clean range
    assert!(detect_ebcdic_corruption(&[0x40], "SP").is_empty());
}

#[test]
fn ebcdic_clean_full_valid_range_0x20_to_0x7e() {
    let data: Vec<u8> = (0x20..=0x7E).collect();
    assert!(detect_ebcdic_corruption(&data, "VALID-LOW").is_empty());
}

#[test]
fn ebcdic_clean_full_valid_range_0xa0_to_0xff() {
    let data: Vec<u8> = (0xA0..=0xFF).collect();
    assert!(detect_ebcdic_corruption(&data, "VALID-HIGH").is_empty());
}

// ===========================================================================
// EBCDIC detector — corrupted data detected
// ===========================================================================

#[test]
fn ebcdic_corrupted_single_null() {
    let errors = detect_ebcdic_corruption(&[0x00], "NUL");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

#[test]
fn ebcdic_corrupted_multiple_scattered() {
    // Clean bytes interspersed with corrupted (0x05 and 0x8A)
    let data = [0xC1, 0x05, 0xC2, 0x8A, 0xC3];
    let errors = detect_ebcdic_corruption(&data, "SCATTER");
    assert_eq!(errors.len(), 2);

    let offsets: Vec<u64> = errors
        .iter()
        .map(|e| e.context.as_ref().unwrap().byte_offset.unwrap())
        .collect();
    assert_eq!(offsets, vec![1, 3]);
}

#[test]
fn ebcdic_corrupted_all_bytes_in_c0_range() {
    let data: Vec<u8> = (0x00..=0x1F).collect();
    let errors = detect_ebcdic_corruption(&data, "C0-ALL");
    assert_eq!(errors.len(), 32);
}

#[test]
fn ebcdic_corrupted_all_bytes_in_c1_range() {
    // 0x7F..=0x9F = 33 bytes
    let data: Vec<u8> = (0x7F..=0x9F).collect();
    let errors = detect_ebcdic_corruption(&data, "C1-ALL");
    assert_eq!(errors.len(), 33);
}

#[test]
fn ebcdic_corrupted_error_message_contains_hex_and_position() {
    let data = [0xC1, 0x0B, 0xC2];
    let errors = detect_ebcdic_corruption(&data, "MSG");
    assert_eq!(errors.len(), 1);
    assert!(errors[0].message.contains("0x0B"));
    assert!(errors[0].message.contains("position 1"));
}

// ===========================================================================
// EBCDIC detector — boundary conditions
// ===========================================================================

#[test]
fn ebcdic_empty_input() {
    assert!(detect_ebcdic_corruption(&[], "EMPTY").is_empty());
}

#[test]
fn ebcdic_boundary_transitions() {
    // Just below/above each boundary
    assert_eq!(detect_ebcdic_corruption(&[0x1F], "B").len(), 1); // last C0 control
    assert!(detect_ebcdic_corruption(&[0x20], "B").is_empty()); // first clean
    assert!(detect_ebcdic_corruption(&[0x7E], "B").is_empty()); // last clean low
    assert_eq!(detect_ebcdic_corruption(&[0x7F], "B").len(), 1); // first C1 control
    assert_eq!(detect_ebcdic_corruption(&[0x9F], "B").len(), 1); // last C1 control
    assert!(detect_ebcdic_corruption(&[0xA0], "B").is_empty()); // first clean high
}

#[test]
fn ebcdic_large_clean_buffer() {
    // 1024 bytes of valid EBCDIC space (0x40)
    let data = vec![0x40u8; 1024];
    assert!(detect_ebcdic_corruption(&data, "BIG-CLEAN").is_empty());
}

#[test]
fn ebcdic_large_corrupted_buffer() {
    // 256 null bytes — all corrupted
    let data = vec![0x00u8; 256];
    let errors = detect_ebcdic_corruption(&data, "BIG-BAD");
    assert_eq!(errors.len(), 256);
}

#[test]
fn ebcdic_field_path_preserved_in_every_error() {
    let data = [0x01, 0x02, 0x03];
    let errors = detect_ebcdic_corruption(&data, "DEEP.NESTED.PATH");
    assert_eq!(errors.len(), 3);
    for err in &errors {
        assert_eq!(
            err.context.as_ref().and_then(|c| c.field_path.as_deref()),
            Some("DEEP.NESTED.PATH")
        );
    }
}

// ===========================================================================
// Packed decimal detector — clean data passes
// ===========================================================================

#[test]
fn packed_clean_positive_12345() {
    // +12345 = [0x12, 0x34, 0x5C]
    assert!(detect_packed_corruption(&[0x12, 0x34, 0x5C], "AMT").is_empty());
}

#[test]
fn packed_clean_negative_98765() {
    // -98765 = [0x98, 0x76, 0x5D]
    assert!(detect_packed_corruption(&[0x98, 0x76, 0x5D], "AMT").is_empty());
}

#[test]
fn packed_clean_unsigned_01234() {
    // unsigned 01234 = [0x01, 0x23, 0x4F]
    assert!(detect_packed_corruption(&[0x01, 0x23, 0x4F], "AMT").is_empty());
}

#[test]
fn packed_clean_zero_positive() {
    // +0 = [0x0C]
    assert!(detect_packed_corruption(&[0x0C], "ZERO").is_empty());
}

#[test]
fn packed_clean_zero_negative() {
    // -0 = [0x0D]
    assert!(detect_packed_corruption(&[0x0D], "ZERO").is_empty());
}

#[test]
fn packed_clean_zero_unsigned() {
    // unsigned 0 = [0x0F]
    assert!(detect_packed_corruption(&[0x0F], "ZERO").is_empty());
}

#[test]
fn packed_clean_all_nines() {
    // +999999999 = [0x99, 0x99, 0x99, 0x99, 0x9C]
    assert!(detect_packed_corruption(&[0x99, 0x99, 0x99, 0x99, 0x9C], "MAX").is_empty());
}

#[test]
fn packed_clean_all_valid_sign_nibbles() {
    // Signs A-F are all valid in terminal position
    for sign in 0x0A..=0x0F {
        let byte = 0x10 | sign;
        assert!(
            detect_packed_corruption(&[byte], "SIGN").is_empty(),
            "sign nibble 0x{sign:X} should be valid"
        );
    }
}

// ===========================================================================
// Packed decimal detector — corrupted data detected
// ===========================================================================

#[test]
fn packed_corrupted_invalid_sign_nibble_0_through_9() {
    for sign in 0x00..=0x09u8 {
        let byte = 0x10 | sign;
        let errors = detect_packed_corruption(&[byte], "BADSIGN");
        assert!(
            !errors.is_empty(),
            "sign nibble 0x{sign:X} should be flagged"
        );
        assert!(errors[0].message.contains("sign"));
    }
}

#[test]
fn packed_corrupted_high_nibble_a() {
    let errors = detect_packed_corruption(&[0xA1, 0x2C], "HI");
    assert!(errors.iter().any(|e| e.message.contains("high nibble")));
}

#[test]
fn packed_corrupted_high_nibble_b() {
    let errors = detect_packed_corruption(&[0xB1, 0x2C], "HI");
    assert!(errors.iter().any(|e| e.message.contains("high nibble")));
}

#[test]
fn packed_corrupted_high_nibble_e() {
    let errors = detect_packed_corruption(&[0xE1, 0x2C], "HI");
    assert!(errors.iter().any(|e| e.message.contains("high nibble")));
}

#[test]
fn packed_corrupted_low_nibble_non_terminal_a() {
    let errors = detect_packed_corruption(&[0x1A, 0x2C], "LO");
    assert!(errors.iter().any(|e| e.message.contains("low nibble")));
}

#[test]
fn packed_corrupted_low_nibble_non_terminal_b() {
    let errors = detect_packed_corruption(&[0x1B, 0x2C], "LO");
    assert!(errors.iter().any(|e| e.message.contains("low nibble")));
}

#[test]
fn packed_corrupted_low_nibble_non_terminal_e() {
    let errors = detect_packed_corruption(&[0x1E, 0x2C], "LO");
    assert!(errors.iter().any(|e| e.message.contains("low nibble")));
}

#[test]
fn packed_corrupted_both_nibbles_in_same_byte() {
    // 0xAB: high nibble 0xA (invalid), low nibble 0xB (invalid in non-terminal)
    let errors = detect_packed_corruption(&[0xAB, 0x1C], "BOTH");
    assert!(errors.len() >= 2);
}

#[test]
fn packed_corrupted_error_code_always_comp3_invalid_nibble() {
    let errors = detect_packed_corruption(&[0xAB, 0xE0, 0x17], "EC");
    for err in &errors {
        assert_eq!(err.code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
    }
}

// ===========================================================================
// Packed decimal detector — boundary conditions
// ===========================================================================

#[test]
fn packed_empty_input() {
    assert!(detect_packed_corruption(&[], "EMPTY").is_empty());
}

#[test]
fn packed_single_byte_is_always_terminal() {
    // Single byte: high nibble checked as digit, low nibble checked as sign
    // 0x1C: high=1 (valid), low=C (valid sign) → clean
    assert!(detect_packed_corruption(&[0x1C], "TERM").is_empty());
    // 0x17: high=1 (valid), low=7 (invalid sign) → error
    assert_eq!(detect_packed_corruption(&[0x17], "TERM").len(), 1);
}

#[test]
fn packed_terminal_low_nibble_a_is_valid_sign() {
    // 0xA is a valid sign nibble in terminal position
    assert!(detect_packed_corruption(&[0x1A], "TERMSIGN").is_empty());
}

#[test]
fn packed_non_terminal_low_nibble_a_is_invalid() {
    // Same nibble 0xA is invalid in non-terminal position
    let errors = detect_packed_corruption(&[0x1A, 0x2C], "NONTERM");
    assert!(!errors.is_empty());
}

#[test]
fn packed_field_path_propagated() {
    let errors = detect_packed_corruption(&[0xFF], "RECORD.GROUP.AMOUNT");
    for err in &errors {
        assert_eq!(
            err.context.as_ref().and_then(|c| c.field_path.as_deref()),
            Some("RECORD.GROUP.AMOUNT")
        );
    }
}

#[test]
fn packed_offset_tracks_byte_position() {
    // Byte 0 clean, byte 1 has invalid high nibble, byte 2 terminal
    let data = [0x12, 0xA3, 0x4C];
    let errors = detect_packed_corruption(&data, "OFF");
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].context.as_ref().and_then(|c| c.byte_offset),
        Some(1)
    );
}

#[test]
fn packed_long_valid_payload() {
    // 10-byte packed decimal, all valid digits, positive sign
    let data = [0x01, 0x23, 0x45, 0x67, 0x89, 0x01, 0x23, 0x45, 0x67, 0x8C];
    assert!(detect_packed_corruption(&data, "LONG").is_empty());
}

#[test]
fn packed_corruption_at_every_byte_position() {
    // Insert corruption at each position of a 5-byte packed field
    for pos in 0..4 {
        let mut data = [0x12, 0x34, 0x56, 0x78, 0x9C];
        data[pos] = 0xAB; // invalid high + low nibble
        let errors = detect_packed_corruption(&data, "POS");
        assert!(
            !errors.is_empty(),
            "corruption at position {pos} not detected"
        );
    }
}
