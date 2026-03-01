// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive tests for RDW header ASCII-digit predicates
//! covering boundary values, length checks, and exhaustive ranges.

use copybook_rdw_predicates::{
    RDW_HEADER_LEN, rdw_is_suspect_ascii_corruption, rdw_is_suspect_ascii_corruption_slice,
};

// ===========================================================================
// RDW_HEADER_LEN constant
// ===========================================================================

#[test]
fn header_len_is_four() {
    assert_eq!(RDW_HEADER_LEN, 4);
}

// ===========================================================================
// Array-based predicate: rdw_is_suspect_ascii_corruption
// ===========================================================================

#[test]
fn both_ascii_digits_are_suspect() {
    assert!(rdw_is_suspect_ascii_corruption([b'0', b'0', 0x00, 0x00]));
    assert!(rdw_is_suspect_ascii_corruption([b'5', b'5', 0x00, 0x00]));
    assert!(rdw_is_suspect_ascii_corruption([b'9', b'9', 0xFF, 0xFF]));
}

#[test]
fn exhaustive_all_digit_pairs_suspect() {
    for b0 in b'0'..=b'9' {
        for b1 in b'0'..=b'9' {
            assert!(
                rdw_is_suspect_ascii_corruption([b0, b1, 0x00, 0x00]),
                "({b0:#04x}, {b1:#04x}) should be suspect"
            );
        }
    }
}

#[test]
fn first_byte_non_digit_not_suspect() {
    assert!(!rdw_is_suspect_ascii_corruption([0x2F, b'5', 0x00, 0x00])); // below '0'
    assert!(!rdw_is_suspect_ascii_corruption([0x3A, b'5', 0x00, 0x00])); // above '9'
    assert!(!rdw_is_suspect_ascii_corruption([b'A', b'5', 0x00, 0x00])); // letter
    assert!(!rdw_is_suspect_ascii_corruption([0x00, b'5', 0x00, 0x00])); // null
    assert!(!rdw_is_suspect_ascii_corruption([0xFF, b'5', 0x00, 0x00])); // high byte
}

#[test]
fn second_byte_non_digit_not_suspect() {
    assert!(!rdw_is_suspect_ascii_corruption([b'5', 0x2F, 0x00, 0x00]));
    assert!(!rdw_is_suspect_ascii_corruption([b'5', 0x3A, 0x00, 0x00]));
    assert!(!rdw_is_suspect_ascii_corruption([b'5', b'G', 0x00, 0x00]));
    assert!(!rdw_is_suspect_ascii_corruption([b'5', 0x00, 0x00, 0x00]));
}

#[test]
fn reserved_bytes_do_not_influence_detection() {
    // Detection is only based on bytes 0 and 1
    assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0xFF, 0xFF]));
    assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', b'A', b'B']));
    assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0x00, 0x00]));
}

#[test]
fn clean_binary_headers_not_suspect() {
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x50, 0x00, 0x00])); // length=80
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x00, 0x00, 0x00])); // all zeros
    assert!(!rdw_is_suspect_ascii_corruption([0xFF, 0xFF, 0xFF, 0xFF])); // all 0xFF
    assert!(!rdw_is_suspect_ascii_corruption([0x01, 0x00, 0x00, 0x00])); // length=256
}

// ===========================================================================
// Slice-based predicate: rdw_is_suspect_ascii_corruption_slice
// ===========================================================================

#[test]
fn slice_empty_not_suspect() {
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[]));
}

#[test]
fn slice_1_byte_not_suspect() {
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'5']));
}

#[test]
fn slice_2_bytes_not_suspect() {
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1', b'2']));
}

#[test]
fn slice_3_bytes_not_suspect() {
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1', b'2', 0x00]));
}

#[test]
fn slice_exactly_4_bytes_uses_array_logic() {
    assert!(rdw_is_suspect_ascii_corruption_slice(&[
        b'1', b'2', 0x00, 0x00
    ]));
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[
        0x00, 0x50, 0x00, 0x00
    ]));
}

#[test]
fn slice_longer_than_4_uses_first_4() {
    let data = [b'3', b'4', 0x00, 0x00, 0xAA, 0xBB, 0xCC];
    assert!(rdw_is_suspect_ascii_corruption_slice(&data));
}

#[test]
fn slice_agrees_with_array_variant() {
    let test_cases: &[[u8; 4]] = &[
        [b'1', b'2', 0x00, 0x00],
        [0x00, 0x50, 0x00, 0x00],
        [b'0', b'0', 0xFF, 0xFF],
        [b'9', b'9', b'A', b'B'],
        [b'A', b'0', 0x00, 0x00],
        [0xFF, 0xFF, 0xFF, 0xFF],
    ];

    for header in test_cases {
        assert_eq!(
            rdw_is_suspect_ascii_corruption(*header),
            rdw_is_suspect_ascii_corruption_slice(header),
            "array and slice should agree for {header:?}"
        );
    }
}

// ===========================================================================
// Boundary values around ASCII digit range (0x30..=0x39)
// ===========================================================================

#[test]
fn boundary_just_below_ascii_zero() {
    assert!(!rdw_is_suspect_ascii_corruption([0x2F, 0x2F, 0x00, 0x00]));
}

#[test]
fn boundary_exactly_ascii_zero() {
    assert!(rdw_is_suspect_ascii_corruption([0x30, 0x30, 0x00, 0x00]));
}

#[test]
fn boundary_exactly_ascii_nine() {
    assert!(rdw_is_suspect_ascii_corruption([0x39, 0x39, 0x00, 0x00]));
}

#[test]
fn boundary_just_above_ascii_nine() {
    assert!(!rdw_is_suspect_ascii_corruption([0x3A, 0x3A, 0x00, 0x00]));
}
