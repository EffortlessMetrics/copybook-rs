// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Deep tests for RDW corruption detection: corrupted header patterns,
//! length overflow detection, sequence validation across records,
//! mixed valid/corrupt records, and recovery after corruption.

use copybook_core::ErrorCode;
use copybook_corruption_rdw::detect_rdw_ascii_corruption;

// ===========================================================================
// Corrupted RDW headers — various byte patterns
// ===========================================================================

#[test]
fn corrupt_header_all_ascii_digits() {
    // "1234" — all four bytes are ASCII digits.
    let err = detect_rdw_ascii_corruption(b"1234").unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn corrupt_header_length_ascii_reserved_binary() {
    // Length bytes are ASCII "42", reserved bytes are binary zeros.
    let err = detect_rdw_ascii_corruption(&[b'4', b'2', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn corrupt_header_length_binary_reserved_ascii_letters() {
    // Length bytes are valid binary, reserved bytes are ASCII letters.
    let err = detect_rdw_ascii_corruption(&[0x00, 0x50, b'X', b'Y']).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn corrupt_header_length_binary_reserved_ascii_digits() {
    // Reserved bytes are ASCII digits — printable but not zero.
    let err = detect_rdw_ascii_corruption(&[0x00, 0x50, b'1', b'2']).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn corrupt_header_all_0x30_ascii_zeros() {
    // "0000" in ASCII = [0x30, 0x30, 0x30, 0x30]
    let err = detect_rdw_ascii_corruption(b"0000").unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn corrupt_header_all_0x39_ascii_nines() {
    // "9999" in ASCII = [0x39, 0x39, 0x39, 0x39]
    let err = detect_rdw_ascii_corruption(b"9999").unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn corrupt_header_mixed_ascii_letters_reserved() {
    // Length is clean binary, reserved has mixed printable characters.
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, b'a', b'z']).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn corrupt_header_reserved_space_pair() {
    // Reserved bytes are both 0x20 (space) — printable.
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, 0x20, 0x20]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn corrupt_header_reserved_exclamation_tilde() {
    // 0x21 = '!' and 0x7E = '~' — both extremes of ASCII printable.
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, 0x21, 0x7E]).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn corrupt_header_ftp_text_mode_typical() {
    // FTP text mode: length "80" as ASCII, CR+LF in reserved.
    // b'8' = 0x38, b'0' = 0x30
    let err = detect_rdw_ascii_corruption(&[b'8', b'0', 0x0D, 0x0A]).unwrap();
    // Heuristic 1 fires (length bytes are digits).
    assert!(err.message.contains("ASCII digits"));
}

#[test]
fn corrupt_header_iconv_misconversion() {
    // Simulates iconv EBCDIC→ASCII misconversion: length bytes become
    // printable ASCII letters like 'A','B'. These are NOT digits so heuristic 1
    // doesn't fire, and 0x4142 > 0x3939 so heuristic 2 doesn't fire either.
    // With clean reserved bytes, no detection occurs.
    assert!(detect_rdw_ascii_corruption(&[b'A', b'B', 0x00, 0x00]).is_none());

    // However, if reserved bytes are also corrupted to printable ASCII,
    // heuristic 3 catches it.
    let err = detect_rdw_ascii_corruption(&[0x00, 0x50, b'A', b'B']).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

// ===========================================================================
// Length overflow detection
// ===========================================================================

#[test]
fn length_overflow_0xffff_not_ascii_corruption() {
    // 0xFFFF is a huge length but not in the ASCII suspect range.
    assert!(detect_rdw_ascii_corruption(&[0xFF, 0xFF, 0x00, 0x00]).is_none());
}

#[test]
fn length_overflow_0x7fff_not_ascii_corruption() {
    // 32767 — large but legitimate.
    assert!(detect_rdw_ascii_corruption(&[0x7F, 0xFF, 0x00, 0x00]).is_none());
}

#[test]
fn length_in_suspect_range_0x3131() {
    // 0x3131 = ASCII "11" = 12593 — in suspect range 0x3030..=0x3939.
    // Both bytes are ASCII digits → heuristic 1 fires.
    let err = detect_rdw_ascii_corruption(&[0x31, 0x31, 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn length_boundary_0x3030_lower_bound() {
    // 0x3030 = ASCII "00" = 12336 — heuristic 1 fires (both are digits).
    let err = detect_rdw_ascii_corruption(&[0x30, 0x30, 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn length_boundary_0x3939_upper_bound() {
    // 0x3939 = ASCII "99" = 14649 — heuristic 1 fires.
    let err = detect_rdw_ascii_corruption(&[0x39, 0x39, 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn length_boundary_just_below_0x3030() {
    // 0x302F = 12335 — first byte '0' is digit, second 0x2F is '/'.
    // Heuristic 1: not both digits. Heuristic 2: length < 0x3030 (condition is > 0x3030).
    assert!(detect_rdw_ascii_corruption(&[0x30, 0x2F, 0x00, 0x00]).is_none());
}

#[test]
fn length_boundary_just_above_0x3939() {
    // 0x393A = 14650 — first byte '9' is digit, second 0x3A is ':'.
    // Heuristic 1: not both digits. Heuristic 2: > 0x3939 → doesn't fire.
    assert!(detect_rdw_ascii_corruption(&[0x39, 0x3A, 0x00, 0x00]).is_none());
}

#[test]
fn length_zero_is_clean() {
    // length=0 — degenerate but not ASCII corruption.
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x00, 0x00, 0x00]).is_none());
}

#[test]
fn length_4_minimum_valid_rdw() {
    // length=4 is the minimum valid RDW (header only, no payload).
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x04, 0x00, 0x00]).is_none());
}

#[test]
fn length_values_0x01_through_0x2f_all_clean() {
    // None of these are in the ASCII digit range.
    for hi in 0x00..=0x02u8 {
        for lo in [0x00u8, 0x10, 0x2F] {
            assert!(
                detect_rdw_ascii_corruption(&[hi, lo, 0x00, 0x00]).is_none(),
                "length [{hi:#04X}, {lo:#04X}] should be clean"
            );
        }
    }
}

// ===========================================================================
// Sequence validation across records
// ===========================================================================

/// Scan a sequence of RDW headers and return (clean_count, corrupt_count).
fn scan_rdw_sequence(headers: &[[u8; 4]]) -> (usize, usize) {
    let mut clean = 0;
    let mut corrupt = 0;
    for header in headers {
        if detect_rdw_ascii_corruption(header).is_some() {
            corrupt += 1;
        } else {
            clean += 1;
        }
    }
    (clean, corrupt)
}

#[test]
fn sequence_all_clean_records() {
    let headers = [
        [0x00, 0x50, 0x00, 0x00],
        [0x00, 0x64, 0x00, 0x00],
        [0x00, 0xC8, 0x00, 0x00],
        [0x01, 0x00, 0x00, 0x00],
        [0x00, 0x04, 0x00, 0x00],
    ];
    let (clean, corrupt) = scan_rdw_sequence(&headers);
    assert_eq!(clean, 5);
    assert_eq!(corrupt, 0);
}

#[test]
fn sequence_all_corrupt_records() {
    let headers = [
        [b'1', b'0', 0x00, 0x00],
        [b'2', b'0', 0x00, 0x00],
        [b'8', b'0', 0x00, 0x00],
        [b'4', b'2', 0x00, 0x00],
        [b'9', b'9', 0x00, 0x00],
    ];
    let (clean, corrupt) = scan_rdw_sequence(&headers);
    assert_eq!(clean, 0);
    assert_eq!(corrupt, 5);
}

#[test]
fn sequence_corruption_starts_midway() {
    // First 3 clean, then corruption begins.
    let headers = [
        [0x00, 0x50, 0x00, 0x00],
        [0x00, 0x64, 0x00, 0x00],
        [0x00, 0xC8, 0x00, 0x00],
        [b'8', b'0', 0x00, 0x00],
        [b'4', b'2', 0x00, 0x00],
    ];
    let (clean, corrupt) = scan_rdw_sequence(&headers);
    assert_eq!(clean, 3);
    assert_eq!(corrupt, 2);
}

#[test]
fn sequence_alternating_clean_corrupt() {
    let headers = [
        [0x00, 0x50, 0x00, 0x00], // clean
        [b'1', b'2', 0x00, 0x00], // corrupt
        [0x00, 0x64, 0x00, 0x00], // clean
        [b'3', b'4', 0x00, 0x00], // corrupt
        [0x00, 0xC8, 0x00, 0x00], // clean
        [b'5', b'6', 0x00, 0x00], // corrupt
    ];
    let (clean, corrupt) = scan_rdw_sequence(&headers);
    assert_eq!(clean, 3);
    assert_eq!(corrupt, 3);
}

#[test]
fn sequence_single_corrupt_in_long_run() {
    let mut headers: Vec<[u8; 4]> = (0..100).map(|_| [0x00, 0x50, 0x00, 0x00]).collect();
    headers[50] = [b'8', b'0', 0x00, 0x00]; // corrupt record 50
    let (clean, corrupt) = scan_rdw_sequence(&headers);
    assert_eq!(clean, 99);
    assert_eq!(corrupt, 1);
}

#[test]
fn sequence_burst_corruption_then_recovery() {
    // 5 clean, 3 corrupt, 5 clean.
    let mut headers: Vec<[u8; 4]> = vec![[0x00, 0x50, 0x00, 0x00]; 13];
    headers[5] = [b'1', b'1', 0x00, 0x00];
    headers[6] = [b'2', b'2', 0x00, 0x00];
    headers[7] = [b'3', b'3', 0x00, 0x00];
    let (clean, corrupt) = scan_rdw_sequence(&headers);
    assert_eq!(clean, 10);
    assert_eq!(corrupt, 3);
}

// ===========================================================================
// Mixed valid/corrupt records — detailed analysis
// ===========================================================================

#[test]
fn mixed_different_heuristics_across_records() {
    // Record 1: heuristic 1 (ASCII digits in length)
    let h1 = detect_rdw_ascii_corruption(&[b'5', b'6', 0x00, 0x00]);
    // Record 2: heuristic 3 (ASCII printable in reserved)
    let h3 = detect_rdw_ascii_corruption(&[0x00, 0x10, b'A', b'B']);
    // Record 3: clean
    let clean = detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]);

    assert!(h1.is_some());
    assert!(h3.is_some());
    assert!(clean.is_none());

    // All corrupt records share the same error code.
    assert_eq!(h1.unwrap().code, h3.unwrap().code);
}

#[test]
fn mixed_only_reserved_corruption_pattern() {
    // All records have valid length but corrupt reserved bytes.
    let headers: Vec<[u8; 4]> = vec![
        [0x00, 0x50, b'A', b'B'],
        [0x00, 0x64, b'C', b'D'],
        [0x01, 0x00, b'E', b'F'],
    ];
    let corrupt_count = headers
        .iter()
        .filter(|h| detect_rdw_ascii_corruption(h.as_slice()).is_some())
        .count();
    assert_eq!(corrupt_count, 3);
}

#[test]
fn mixed_reserved_one_printable_one_control() {
    // When only one reserved byte is printable, heuristic 3 should NOT fire.
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, b'A', 0x01]).is_none());
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x01, b'B']).is_none());
}

// ===========================================================================
// Recovery after corruption
// ===========================================================================

/// Detect the first clean record after corruption starts.
fn find_recovery_point(headers: &[[u8; 4]]) -> Option<usize> {
    let mut saw_corruption = false;
    for (i, header) in headers.iter().enumerate() {
        if detect_rdw_ascii_corruption(header).is_some() {
            saw_corruption = true;
        } else if saw_corruption {
            return Some(i);
        }
    }
    None
}

#[test]
fn recovery_immediate_after_single_corrupt() {
    let headers = [
        [0x00, 0x50, 0x00, 0x00], // clean
        [b'8', b'0', 0x00, 0x00], // corrupt
        [0x00, 0x64, 0x00, 0x00], // recovered
        [0x00, 0xC8, 0x00, 0x00], // clean
    ];
    assert_eq!(find_recovery_point(&headers), Some(2));
}

#[test]
fn recovery_after_burst_of_corruption() {
    let headers = [
        [0x00, 0x50, 0x00, 0x00], // clean
        [b'1', b'1', 0x00, 0x00], // corrupt
        [b'2', b'2', 0x00, 0x00], // corrupt
        [b'3', b'3', 0x00, 0x00], // corrupt
        [0x00, 0x50, 0x00, 0x00], // recovered
    ];
    assert_eq!(find_recovery_point(&headers), Some(4));
}

#[test]
fn recovery_no_recovery_all_corrupt() {
    let headers = [
        [b'1', b'0', 0x00, 0x00],
        [b'2', b'0', 0x00, 0x00],
        [b'3', b'0', 0x00, 0x00],
    ];
    assert_eq!(find_recovery_point(&headers), None);
}

#[test]
fn recovery_no_corruption_no_recovery_point() {
    let headers = [[0x00, 0x50, 0x00, 0x00], [0x00, 0x64, 0x00, 0x00]];
    assert_eq!(find_recovery_point(&headers), None);
}

#[test]
fn recovery_corruption_at_end_no_recovery() {
    let headers = [
        [0x00, 0x50, 0x00, 0x00],
        [0x00, 0x64, 0x00, 0x00],
        [b'9', b'9', 0x00, 0x00],
    ];
    assert_eq!(find_recovery_point(&headers), None);
}

#[test]
fn recovery_large_stream_with_gap() {
    let mut headers: Vec<[u8; 4]> = vec![[0x00, 0x50, 0x00, 0x00]; 100];
    // Corrupt records 40-49.
    for i in 40..50 {
        headers[i] = [b'5', b'5', 0x00, 0x00];
    }
    assert_eq!(find_recovery_point(&headers), Some(50));
}

// ===========================================================================
// Edge cases and boundary conditions
// ===========================================================================

#[test]
fn edge_input_exactly_4_bytes_corrupt() {
    let err = detect_rdw_ascii_corruption(&[b'0', b'0', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn edge_input_5_bytes_only_first_4_matter() {
    // 5th byte is irrelevant.
    let result = detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00, b'Z']);
    assert!(result.is_none());
}

#[test]
fn edge_all_0x20_space_bytes() {
    // [0x20, 0x20, 0x20, 0x20] — length=0x2020=8224.
    // Not in digit range, not in 0x3030..0x3939.
    // Reserved bytes: both 0x20 (space) are printable → heuristic 3 fires.
    let err = detect_rdw_ascii_corruption(&[0x20, 0x20, 0x20, 0x20]).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn edge_all_0x7e_tilde_bytes() {
    // [0x7E, 0x7E, 0x7E, 0x7E] — length=0x7E7E=32382.
    // Not in digit range. Heuristic 2: > 0x3939 → no.
    // Heuristic 3: reserved bytes both 0x7E printable → fires.
    let err = detect_rdw_ascii_corruption(&[0x7E, 0x7E, 0x7E, 0x7E]).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn edge_reserved_0x7f_not_printable() {
    // 0x7F (DEL) is NOT considered ASCII printable.
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x7F, 0x7F]).is_none());
}

#[test]
fn edge_reserved_0x19_not_printable() {
    // 0x19 is below the printable range.
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x19, 0x19]).is_none());
}

// ===========================================================================
// Error message content validation
// ===========================================================================

#[test]
fn error_message_h1_contains_hex_representation() {
    let err = detect_rdw_ascii_corruption(&[b'4', b'2', 0x00, 0x00]).unwrap();
    assert!(err.message.contains("0x34") || err.message.contains("0x32"));
}

#[test]
fn error_message_h3_contains_reserved_bytes_label() {
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, b'M', b'N']).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn error_message_h2_contains_length_value() {
    // Use a non-digit first byte that puts length in 0x3030..0x3939.
    // 0x30 is '0' (digit), 0x3A is ':' (not digit) → only h2 fires.
    let err = detect_rdw_ascii_corruption(&[0x30, 0x3A, 0x00, 0x00]).unwrap();
    // The length is 0x303A = 12346.
    assert!(err.message.contains("12346") || err.message.contains("303A"));
}

// ===========================================================================
// Large-scale sequence tests
// ===========================================================================

#[test]
fn large_sequence_1000_headers_all_clean() {
    for length in (4..2004).step_by(2) {
        let hi = ((length >> 8) & 0xFF) as u8;
        let lo = (length & 0xFF) as u8;
        assert!(
            detect_rdw_ascii_corruption(&[hi, lo, 0x00, 0x00]).is_none(),
            "length={length} (0x{length:04X}) should be clean"
        );
    }
}

#[test]
fn large_sequence_sweep_all_ascii_digit_lengths() {
    // Verify every possible ASCII digit pair is detected.
    for d1 in b'0'..=b'9' {
        for d2 in b'0'..=b'9' {
            let result = detect_rdw_ascii_corruption(&[d1, d2, 0x00, 0x00]);
            assert!(
                result.is_some(),
                "digit pair ({}, {}) should be detected",
                d1 as char,
                d2 as char
            );
        }
    }
}
