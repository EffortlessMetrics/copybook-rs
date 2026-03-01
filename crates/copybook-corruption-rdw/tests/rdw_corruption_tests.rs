// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Tests for RDW-specific corruption detection: valid headers, length
//! mismatches, reserved byte corruption, and truncated headers.

use copybook_core::ErrorCode;
use copybook_corruption_rdw::detect_rdw_ascii_corruption;

// ===========================================================================
// Valid RDW passes — no corruption detected
// ===========================================================================

#[test]
fn valid_rdw_length_80_reserved_zero() {
    // Standard mainframe record: length=80, reserved=0x0000
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
}

#[test]
fn valid_rdw_length_4_minimum() {
    // Minimum RDW: length=4 (header only), reserved=0x0000
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x04, 0x00, 0x00]).is_none());
}

#[test]
fn valid_rdw_length_32760_maximum_typical() {
    // Large record: length=32760 (0x7FF8), reserved=0x0000
    assert!(detect_rdw_ascii_corruption(&[0x7F, 0xF8, 0x00, 0x00]).is_none());
}

#[test]
fn valid_rdw_all_zeros() {
    // Degenerate but not ASCII-corrupted
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x00, 0x00, 0x00]).is_none());
}

#[test]
fn valid_rdw_high_bytes_not_ascii() {
    // Length bytes 0xFF, 0xFE — not ASCII digits, reserved 0x00
    assert!(detect_rdw_ascii_corruption(&[0xFF, 0xFE, 0x00, 0x00]).is_none());
}

#[test]
fn valid_rdw_reserved_binary_non_printable() {
    // Reserved bytes 0x01, 0x02 — not ASCII printable
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x01, 0x02]).is_none());
}

#[test]
fn valid_rdw_length_256() {
    // length=256 (0x0100), reserved=0x0000
    assert!(detect_rdw_ascii_corruption(&[0x01, 0x00, 0x00, 0x00]).is_none());
}

// ===========================================================================
// Heuristic 1: ASCII digit length bytes detected
// ===========================================================================

#[test]
fn h1_ascii_digits_00() {
    let err = detect_rdw_ascii_corruption(&[b'0', b'0', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert!(err.message.contains("ASCII digits"));
}

#[test]
fn h1_ascii_digits_99() {
    let err = detect_rdw_ascii_corruption(&[b'9', b'9', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn h1_ascii_digits_42() {
    let err = detect_rdw_ascii_corruption(&[b'4', b'2', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn h1_first_byte_not_digit_no_h1() {
    // 0x2F is '/' — just below '0', second byte is digit
    assert!(detect_rdw_ascii_corruption(&[0x2F, b'5', 0x00, 0x00]).is_none());
}

#[test]
fn h1_second_byte_not_digit_no_h1() {
    // First byte digit, second byte 0x3A (just above '9')
    // Falls to heuristic 2 since 0x303A is in range
    let result = detect_rdw_ascii_corruption(&[b'0', 0x3A, 0x00, 0x00]);
    if let Some(err) = result {
        // Should be heuristic 2, not heuristic 1
        assert!(!err.message.contains("ASCII digits"));
    }
}

#[test]
fn h1_takes_priority_over_h3() {
    // Both length bytes are ASCII digits AND reserved bytes are printable
    let err = detect_rdw_ascii_corruption(&[b'5', b'6', b'A', b'B']).unwrap();
    assert!(
        err.message.contains("ASCII digits"),
        "heuristic 1 should fire first"
    );
}

// ===========================================================================
// Heuristic 2: Suspiciously large length in ASCII range
// ===========================================================================

#[test]
fn h2_length_0x3031_in_range() {
    // 0x3031 = 12337 — in 0x3030..=0x3939 range
    // But both bytes are ASCII digits so h1 fires first
    let result = detect_rdw_ascii_corruption(&[0x30, 0x31, 0x00, 0x00]);
    assert!(result.is_some());
}

#[test]
fn h2_non_digit_first_byte_in_range() {
    // 0x30, 0x3A: first byte '0' is digit, second 0x3A is not → h1 doesn't fire
    // length=0x303A is in 0x3030..=0x3939 → h2 fires
    let err = detect_rdw_ascii_corruption(&[0x30, 0x3A, 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn h2_just_below_range_not_detected() {
    // 0x302F = 12335, just below 0x3030 = 12336
    // First byte is digit but second is not → h1 doesn't fire
    // length < 0x3030 → h2 doesn't fire
    assert!(detect_rdw_ascii_corruption(&[0x30, 0x2F, 0x00, 0x00]).is_none());
}

#[test]
fn h2_just_above_range_not_detected() {
    // 0x393A = 14650, above 0x3939
    // First byte '9' is digit, second 0x3A is not → h1 doesn't fire
    // length > 0x3939 → h2 doesn't fire
    // reserved bytes 0x00 → h3 doesn't fire
    assert!(detect_rdw_ascii_corruption(&[0x39, 0x3A, 0x00, 0x00]).is_none());
}

// ===========================================================================
// Heuristic 3: Reserved byte corruption (ASCII printable)
// ===========================================================================

#[test]
fn h3_both_reserved_printable() {
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, b'X', b'Y']).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn h3_reserved_space_space() {
    // 0x20 (space) is ASCII printable
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, 0x20, 0x20]).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn h3_reserved_tilde_tilde() {
    // 0x7E (~) is the highest ASCII printable
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, 0x7E, 0x7E]).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn h3_one_printable_one_control_no_flag() {
    // Only one reserved byte printable — not flagged
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, b'A', 0x01]).is_none());
}

#[test]
fn h3_reserved_zero_zero_excluded() {
    // [0x00, 0x00] is explicitly excluded from h3
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, 0x00, 0x00]).is_none());
}

#[test]
fn h3_reserved_del_del_not_printable() {
    // 0x7F (DEL) is not ASCII printable
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, 0x7F, 0x7F]).is_none());
}

// ===========================================================================
// Truncated headers (< 4 bytes)
// ===========================================================================

#[test]
fn truncated_empty() {
    assert!(detect_rdw_ascii_corruption(&[]).is_none());
}

#[test]
fn truncated_1_byte() {
    assert!(detect_rdw_ascii_corruption(&[b'5']).is_none());
}

#[test]
fn truncated_2_bytes() {
    assert!(detect_rdw_ascii_corruption(&[b'5', b'6']).is_none());
}

#[test]
fn truncated_3_bytes() {
    assert!(detect_rdw_ascii_corruption(&[b'5', b'6', 0x00]).is_none());
}

#[test]
fn truncated_3_bytes_all_ascii() {
    // Even with all ASCII digits, 3 bytes is too short
    assert!(detect_rdw_ascii_corruption(&[b'1', b'2', b'3']).is_none());
}

// ===========================================================================
// Longer input — only first 4 bytes matter
// ===========================================================================

#[test]
fn longer_input_ascii_header_detected() {
    let data = [b'5', b'6', 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
    assert!(detect_rdw_ascii_corruption(&data).is_some());
}

#[test]
fn longer_input_clean_header_stays_clean() {
    let data = [0x00, 0x50, 0x00, 0x00, b'A', b'B', b'C', b'D'];
    assert!(detect_rdw_ascii_corruption(&data).is_none());
}

// ===========================================================================
// Error code consistency across all heuristics
// ===========================================================================

#[test]
fn all_heuristics_return_same_error_code() {
    let h1 = detect_rdw_ascii_corruption(&[b'1', b'2', 0x00, 0x00]).unwrap();
    let h3 = detect_rdw_ascii_corruption(&[0x00, 0x10, b'X', b'Y']).unwrap();

    assert_eq!(h1.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert_eq!(h3.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn error_message_is_descriptive() {
    let err = detect_rdw_ascii_corruption(&[b'4', b'2', 0x00, 0x00]).unwrap();
    // Message should contain at least the hex representation
    assert!(err.message.contains("0x34") || err.message.contains("ASCII"));
}

// ===========================================================================
// Real-world-inspired scenarios
// ===========================================================================

#[test]
fn scenario_ftp_ascii_transfer_corruption() {
    // Simulates FTP ASCII mode corrupting a binary RDW header:
    // the length bytes became ASCII '80' instead of binary 0x0050
    let corrupted = [b'8', b'0', 0x00, 0x00];
    assert!(detect_rdw_ascii_corruption(&corrupted).is_some());
}

#[test]
fn scenario_clean_mainframe_record() {
    // Typical mainframe VB record: length=200, reserved=0x0000
    let clean = [0x00, 0xC8, 0x00, 0x00]; // 200 = 0x00C8
    assert!(detect_rdw_ascii_corruption(&clean).is_none());
}
