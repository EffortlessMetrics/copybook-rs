// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive tests for RDW-specific corruption detection heuristics.

use copybook_core::ErrorCode;
use copybook_corruption_rdw::detect_rdw_ascii_corruption;

// ===========================================================================
// Short/empty input handling
// ===========================================================================

#[test]
fn empty_input_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[]).is_none());
}

#[test]
fn one_byte_returns_none() {
    assert!(detect_rdw_ascii_corruption(b"5").is_none());
}

#[test]
fn two_bytes_returns_none() {
    assert!(detect_rdw_ascii_corruption(b"12").is_none());
}

#[test]
fn three_bytes_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[b'1', b'2', 0x00]).is_none());
}

// ===========================================================================
// Heuristic 1: ASCII digit length bytes
// ===========================================================================

#[test]
fn heuristic1_ascii_digit_pair_detected() {
    let err = detect_rdw_ascii_corruption(&[b'1', b'2', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert!(err.message.contains("ASCII digits"));
}

#[test]
fn heuristic1_all_digit_combos_at_boundaries() {
    // '0','0' and '9','9' should both fire
    assert!(detect_rdw_ascii_corruption(&[b'0', b'0', 0x00, 0x00]).is_some());
    assert!(detect_rdw_ascii_corruption(&[b'9', b'9', 0x00, 0x00]).is_some());
}

#[test]
fn heuristic1_just_below_ascii_0_not_detected() {
    assert!(detect_rdw_ascii_corruption(&[0x2F, b'5', 0x00, 0x00]).is_none());
}

#[test]
fn heuristic1_just_above_ascii_9_not_detected() {
    assert!(detect_rdw_ascii_corruption(&[0x3A, b'5', 0x00, 0x00]).is_none());
}

// ===========================================================================
// Heuristic 2: Suspiciously large length in ASCII range
// ===========================================================================

#[test]
fn heuristic2_length_0x3031_detected() {
    // 0x3031 is ASCII "01" interpreted as binary length=12337
    // Heuristic 1 fires first since '0' and '1' are digits
    let result = detect_rdw_ascii_corruption(&[0x30, 0x31, 0x00, 0x00]);
    assert!(result.is_some());
}

#[test]
fn heuristic2_length_just_above_0x3030_with_non_digit_byte() {
    // 0x30 is '0' so heuristic 1 would fire. Use a non-digit first byte
    // that still makes length in range. E.g., 0x30 + 0x3A = not both digits
    // but 0x303A is > 0x3030 and <= 0x3939? 0x303A = 12346, 0x3939 = 14649
    // Heuristic 1: b0=0x30 is digit, b1=0x3A is NOT digit -> h1 doesn't fire
    // Heuristic 2: length=0x303A, 0x3030 < 0x303A <= 0x3939 -> fires
    let result = detect_rdw_ascii_corruption(&[0x30, 0x3A, 0x00, 0x00]);
    assert!(result.is_some());
    assert_eq!(result.unwrap().code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn heuristic2_length_below_range_not_detected() {
    // Length = 0x302F = 12335, just below 0x3030
    // b0=0x30 is digit, b1=0x2F is NOT digit -> h1 doesn't fire
    // 0x302F < 0x3030 -> h2 doesn't fire
    assert!(detect_rdw_ascii_corruption(&[0x30, 0x2F, 0x00, 0x00]).is_none());
}

// ===========================================================================
// Heuristic 3: ASCII-printable reserved bytes
// ===========================================================================

#[test]
fn heuristic3_both_reserved_printable_detected() {
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, b'A', b'B']).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn heuristic3_one_reserved_not_printable_not_detected() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, b'A', 0x01]).is_none());
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, 0x01, b'B']).is_none());
}

#[test]
fn heuristic3_reserved_space_and_tilde_detected() {
    // Space (0x20) and tilde (0x7E) are both ASCII printable
    let result = detect_rdw_ascii_corruption(&[0x00, 0x10, 0x20, 0x7E]);
    assert!(result.is_some());
}

#[test]
fn heuristic3_reserved_zero_zero_not_flagged() {
    // [0x00, 0x00] is explicitly excluded from heuristic 3
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, 0x00, 0x00]).is_none());
}

#[test]
fn heuristic3_reserved_del_not_flagged() {
    // 0x7F (DEL) is NOT ASCII printable
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x10, 0x7F, 0x7F]).is_none());
}

// ===========================================================================
// Clean binary headers
// ===========================================================================

#[test]
fn clean_binary_length_80_no_corruption() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
}

#[test]
fn clean_all_zeros_no_corruption() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x00, 0x00, 0x00]).is_none());
}

#[test]
fn clean_all_0xff_no_corruption() {
    assert!(detect_rdw_ascii_corruption(&[0xFF, 0xFF, 0xFF, 0xFF]).is_none());
}

#[test]
fn clean_small_length_with_binary_reserved() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x04, 0x00, 0x00]).is_none());
}

// ===========================================================================
// Longer input (only first 4 bytes matter)
// ===========================================================================

#[test]
fn longer_input_only_first_4_bytes_checked() {
    let data = [b'5', b'6', 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF];
    assert!(detect_rdw_ascii_corruption(&data).is_some());
}

#[test]
fn longer_clean_input_stays_clean() {
    let data = [0x00, 0x50, 0x00, 0x00, b'A', b'B', b'C', b'D'];
    assert!(detect_rdw_ascii_corruption(&data).is_none());
}

// ===========================================================================
// Error code consistency
// ===========================================================================

#[test]
fn all_heuristics_produce_same_error_code() {
    // Heuristic 1
    let e1 = detect_rdw_ascii_corruption(&[b'1', b'2', 0x00, 0x00]).unwrap();
    // Heuristic 3
    let e3 = detect_rdw_ascii_corruption(&[0x00, 0x10, b'X', b'Y']).unwrap();

    assert_eq!(e1.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert_eq!(e3.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

// ===========================================================================
// Heuristic priority (heuristic 1 fires before 3)
// ===========================================================================

#[test]
fn heuristic1_takes_priority_over_heuristic3() {
    // Both length bytes are ASCII digits AND reserved bytes are printable
    let err = detect_rdw_ascii_corruption(b"12AB").unwrap();
    // Heuristic 1 fires first, so message should reference "ASCII digits"
    assert!(err.message.contains("ASCII digits"));
}
