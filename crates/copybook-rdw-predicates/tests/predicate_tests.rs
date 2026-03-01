// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive predicate tests for `copybook-rdw-predicates`.
//!
//! Covers: header validation, length boundaries, reserved-byte handling,
//! multi-record consistency, heuristic detection patterns, and edge cases.

use copybook_rdw_predicates::{
    RDW_HEADER_LEN, rdw_is_suspect_ascii_corruption, rdw_is_suspect_ascii_corruption_slice,
};

// ===========================================================================
// 1. RDW header validation predicates – valid / invalid headers
// ===========================================================================

#[test]
fn valid_binary_header_length_80_not_suspect() {
    // Standard mainframe LRECL=80 record: big-endian 0x0050 = 80
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x50, 0x00, 0x00]));
}

#[test]
fn valid_binary_header_length_4_minimum_record() {
    // Minimal payload-less RDW: length=4 (header only)
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x04, 0x00, 0x00]));
}

#[test]
fn valid_binary_header_large_length() {
    // Near-max length: 0x7FFF = 32767
    assert!(!rdw_is_suspect_ascii_corruption([0x7F, 0xFF, 0x00, 0x00]));
}

#[test]
fn invalid_ascii_corrupted_header_suspect() {
    // ASCII "12" in length field → transfer corruption
    assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0x00, 0x00]));
}

#[test]
fn invalid_ascii_corrupted_header_all_digits() {
    // All four bytes are ASCII digits
    assert!(rdw_is_suspect_ascii_corruption([b'9', b'9', b'0', b'0']));
}

#[test]
fn mixed_digit_and_non_digit_first_byte_not_suspect() {
    // Only byte 1 is a digit — not suspect
    assert!(!rdw_is_suspect_ascii_corruption([0x00, b'5', 0x00, 0x00]));
}

// ===========================================================================
// 2. Length validation – minimum (4-byte header), boundary values
// ===========================================================================

#[test]
fn rdw_header_len_constant_equals_four() {
    assert_eq!(RDW_HEADER_LEN, 4);
}

#[test]
fn slice_below_minimum_length_returns_false() {
    // 0 bytes
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[]));
    // 1 byte
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1']));
    // 2 bytes
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1', b'2']));
    // 3 bytes – still below RDW_HEADER_LEN
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1', b'2', b'3']));
}

#[test]
fn slice_exactly_minimum_length_works() {
    assert!(rdw_is_suspect_ascii_corruption_slice(&[
        b'4', b'5', 0x00, 0x00
    ]));
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[
        0x00, 0x50, 0x00, 0x00
    ]));
}

#[test]
fn slice_well_above_minimum_uses_first_four() {
    // 128 bytes of data; only the first 4 matter
    let mut buf = vec![0xAAu8; 128];
    buf[0] = b'7';
    buf[1] = b'8';
    buf[2] = 0x00;
    buf[3] = 0x00;
    assert!(rdw_is_suspect_ascii_corruption_slice(&buf));
}

// ===========================================================================
// 3. Reserved bytes checks – zero vs non-zero
// ===========================================================================

#[test]
fn reserved_bytes_zero_does_not_affect_suspect_positive() {
    // Suspect because first two bytes are ASCII digits; reserved = 0x0000
    assert!(rdw_is_suspect_ascii_corruption([b'3', b'4', 0x00, 0x00]));
}

#[test]
fn reserved_bytes_nonzero_does_not_affect_suspect_positive() {
    // Still suspect — reserved bytes are irrelevant to the predicate
    assert!(rdw_is_suspect_ascii_corruption([b'3', b'4', 0xDE, 0xAD]));
}

#[test]
fn reserved_bytes_all_0xff_does_not_affect_clean_negative() {
    // Clean binary header stays clean regardless of reserved bytes
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x50, 0xFF, 0xFF]));
}

#[test]
fn reserved_bytes_ascii_printable_does_not_affect_predicate() {
    // Reserved bytes being printable ASCII doesn't change the predicate outcome
    // (the predicate only inspects bytes 0 and 1)
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x50, b'A', b'B']));
    assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', b'A', b'B']));
}

// ===========================================================================
// 4. Consistency – multiple records with compatible sizes
// ===========================================================================

#[test]
fn consecutive_clean_headers_all_not_suspect() {
    let records: &[[u8; 4]] = &[
        [0x00, 0x50, 0x00, 0x00], // 80-byte record
        [0x00, 0x28, 0x00, 0x00], // 40-byte record
        [0x00, 0xA0, 0x00, 0x00], // 160-byte record
        [0x01, 0x00, 0x00, 0x00], // 256-byte record
    ];
    for (i, hdr) in records.iter().enumerate() {
        assert!(
            !rdw_is_suspect_ascii_corruption(*hdr),
            "record {i} should NOT be suspect: {hdr:?}"
        );
    }
}

#[test]
fn consecutive_corrupt_headers_all_suspect() {
    let records: &[[u8; 4]] = &[
        [b'0', b'8', 0x00, 0x00], // ASCII "08"
        [b'4', b'0', 0x00, 0x00], // ASCII "40"
        [b'1', b'6', 0x00, 0x00], // ASCII "16"
    ];
    for (i, hdr) in records.iter().enumerate() {
        assert!(
            rdw_is_suspect_ascii_corruption(*hdr),
            "record {i} should be suspect: {hdr:?}"
        );
    }
}

#[test]
fn mixed_stream_detects_corruption_in_some_records() {
    let headers: &[[u8; 4]] = &[
        [0x00, 0x50, 0x00, 0x00], // clean
        [b'1', b'2', 0x00, 0x00], // corrupt
        [0x00, 0xA0, 0x00, 0x00], // clean
        [b'9', b'9', 0x00, 0x00], // corrupt
    ];
    let suspect_count = headers
        .iter()
        .filter(|h| rdw_is_suspect_ascii_corruption(**h))
        .count();
    assert_eq!(suspect_count, 2);
}

// ===========================================================================
// 5. Heuristic detection – RDW format detection from data samples
// ===========================================================================

#[test]
fn heuristic_all_records_clean_suggests_binary_rdw() {
    // A stream where no header triggers the predicate → likely genuine RDW
    let stream: &[u8] = &[
        0x00, 0x08, 0x00, 0x00, 0xC1, 0xC2, 0xC3,
        0xC4, // record 1: len=8, payload=4 EBCDIC bytes
        0x00, 0x06, 0x00, 0x00, 0xD1, 0xD2, // record 2: len=6, payload=2 EBCDIC bytes
    ];
    let hdr1: [u8; 4] = stream[0..4].try_into().unwrap();
    let hdr2: [u8; 4] = stream[8..12].try_into().unwrap();
    assert!(!rdw_is_suspect_ascii_corruption(hdr1));
    assert!(!rdw_is_suspect_ascii_corruption(hdr2));
}

#[test]
fn heuristic_ascii_transfer_file_detected() {
    // Simulates an ASCII-transferred file where length fields became ASCII
    let stream: &[u8] = &[
        b'0', b'8', 0x00, 0x00, 0x41, 0x42, 0x43, 0x44, // "08" + "ABCD"
        b'0', b'6', 0x00, 0x00, 0x45, 0x46, // "06" + "EF"
    ];
    assert!(rdw_is_suspect_ascii_corruption_slice(&stream[0..4]));
    assert!(rdw_is_suspect_ascii_corruption_slice(&stream[8..12]));
}

#[test]
fn heuristic_ratio_based_detection() {
    // If >50% of sampled headers are suspect, the file is likely corrupted
    let headers: Vec<[u8; 4]> = vec![
        [b'0', b'8', 0x00, 0x00],
        [0x00, 0x50, 0x00, 0x00],
        [b'1', b'6', 0x00, 0x00],
        [b'4', b'0', 0x00, 0x00],
    ];
    let suspect_ratio = headers
        .iter()
        .filter(|h| rdw_is_suspect_ascii_corruption(**h))
        .count() as f64
        / headers.len() as f64;
    assert!(
        suspect_ratio > 0.5,
        "expected majority suspect, got {suspect_ratio}"
    );
}

// ===========================================================================
// 6. Edge cases
// ===========================================================================

#[test]
fn edge_empty_slice() {
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[]));
}

#[test]
fn edge_single_byte_slice() {
    assert!(!rdw_is_suspect_ascii_corruption_slice(&[b'1']));
}

#[test]
fn edge_all_zero_header() {
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x00, 0x00, 0x00]));
}

#[test]
fn edge_all_0xff_header() {
    assert!(!rdw_is_suspect_ascii_corruption([0xFF, 0xFF, 0xFF, 0xFF]));
}

#[test]
fn edge_boundary_byte_0x2f_just_below_zero() {
    // 0x2F is one below ASCII '0' (0x30)
    assert!(!rdw_is_suspect_ascii_corruption([0x2F, 0x30, 0x00, 0x00]));
    assert!(!rdw_is_suspect_ascii_corruption([0x30, 0x2F, 0x00, 0x00]));
}

#[test]
fn edge_boundary_byte_0x3a_just_above_nine() {
    // 0x3A is one above ASCII '9' (0x39)
    assert!(!rdw_is_suspect_ascii_corruption([0x3A, 0x39, 0x00, 0x00]));
    assert!(!rdw_is_suspect_ascii_corruption([0x39, 0x3A, 0x00, 0x00]));
}

#[test]
fn edge_ascii_zero_zero_is_suspect() {
    // '0','0' are valid ASCII digits → suspect
    assert!(rdw_is_suspect_ascii_corruption([b'0', b'0', 0x00, 0x00]));
}

#[test]
fn edge_ascii_nine_nine_is_suspect() {
    // '9','9' are valid ASCII digits → suspect
    assert!(rdw_is_suspect_ascii_corruption([b'9', b'9', 0x00, 0x00]));
}

// ===========================================================================
// 7. Array ↔ slice agreement
// ===========================================================================

#[test]
fn array_and_slice_agree_on_representative_inputs() {
    let cases: &[[u8; 4]] = &[
        [b'0', b'0', 0x00, 0x00],
        [b'5', b'5', 0xFF, 0xFF],
        [b'9', b'9', b'Z', b'Z'],
        [0x00, 0x50, 0x00, 0x00],
        [0x00, 0x00, 0x00, 0x00],
        [0xFF, 0xFF, 0xFF, 0xFF],
        [0x2F, 0x30, 0x00, 0x00],
        [0x30, 0x2F, 0x00, 0x00],
        [0x3A, 0x39, 0x00, 0x00],
        [b'A', b'0', 0x00, 0x00],
    ];
    for header in cases {
        assert_eq!(
            rdw_is_suspect_ascii_corruption(*header),
            rdw_is_suspect_ascii_corruption_slice(header),
            "array and slice must agree for {header:02X?}"
        );
    }
}

// ===========================================================================
// 8. Cross-crate consistency (dev-dependency: copybook-rdw)
// ===========================================================================

#[test]
fn predicate_agrees_with_copybook_rdw_re_export() {
    use copybook_rdw::rdw_is_suspect_ascii_corruption as rdw_fn;

    let suspect = [b'5', b'6', 0x00, 0x00];
    let clean = [0x00, 0x50, 0x00, 0x00];

    assert_eq!(
        rdw_is_suspect_ascii_corruption(suspect),
        rdw_fn(suspect),
        "suspect header must agree"
    );
    assert_eq!(
        rdw_is_suspect_ascii_corruption(clean),
        rdw_fn(clean),
        "clean header must agree"
    );
}

#[test]
fn predicate_agrees_with_corruption_rdw_detection() {
    use copybook_corruption_rdw::detect_rdw_ascii_corruption;

    // When the predicate says suspect, the corruption detector should also flag it
    let suspect = [b'1', b'2', 0x00, 0x00];
    assert!(rdw_is_suspect_ascii_corruption(suspect));
    assert!(
        detect_rdw_ascii_corruption(&suspect).is_some(),
        "corruption detector should agree on suspect header"
    );

    // A clean binary header should not be flagged by either
    let clean = [0x00, 0x50, 0x00, 0x00];
    assert!(!rdw_is_suspect_ascii_corruption(clean));
    assert!(
        detect_rdw_ascii_corruption(&clean).is_none(),
        "corruption detector should agree on clean header"
    );
}
