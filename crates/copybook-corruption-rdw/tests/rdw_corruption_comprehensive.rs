// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive RDW corruption tests: invalid header lengths, length exceeds
//! file, length exceeds max record size, non-zero reserved bytes, truncated
//! RDW at end of file, and multiple sequential corruptions.

use copybook_core::ErrorCode;
use copybook_corruption_rdw::detect_rdw_ascii_corruption;

// ===========================================================================
// Invalid RDW header length (0, 1, 2, 3 bytes)
// ===========================================================================

#[test]
fn invalid_header_0_bytes_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[]).is_none());
}

#[test]
fn invalid_header_1_byte_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[0x00]).is_none());
    assert!(detect_rdw_ascii_corruption(b"5").is_none());
    assert!(detect_rdw_ascii_corruption(&[0xFF]).is_none());
}

#[test]
fn invalid_header_2_bytes_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50]).is_none());
    assert!(detect_rdw_ascii_corruption(b"12").is_none());
    assert!(detect_rdw_ascii_corruption(&[0xFF, 0xFF]).is_none());
}

#[test]
fn invalid_header_3_bytes_returns_none() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00]).is_none());
    assert!(detect_rdw_ascii_corruption(b"123").is_none());
    assert!(detect_rdw_ascii_corruption(&[0xFF, 0xFF, 0xFF]).is_none());
}

#[test]
fn valid_header_exactly_4_bytes_processed() {
    // Clean 4-byte header
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
    // Corrupt 4-byte header
    assert!(detect_rdw_ascii_corruption(&[b'8', b'0', 0x00, 0x00]).is_some());
}

// ===========================================================================
// RDW length exceeds remaining file (simulated via sequence scanning)
// ===========================================================================

/// Simulate a file as a sequence of RDW headers followed by payload.
/// Returns the number of records where the declared length exceeds available data.
fn count_length_overflows(file_data: &[u8]) -> usize {
    let mut pos = 0;
    let mut overflows = 0;
    while pos + 4 <= file_data.len() {
        let length = u16::from_be_bytes([file_data[pos], file_data[pos + 1]]) as usize;
        if length < 4 || pos + length > file_data.len() {
            overflows += 1;
            break; // Cannot continue after an overflow
        }
        pos += length;
    }
    overflows
}

#[test]
fn length_exceeds_file_single_record() {
    // RDW says length=80 but file is only 10 bytes
    let mut file = vec![0x00, 0x50, 0x00, 0x00]; // length=80
    file.extend(vec![0xC1u8; 6]); // only 6 payload bytes (total 10)
    assert_eq!(count_length_overflows(&file), 1);
}

#[test]
fn length_exceeds_file_second_record() {
    // First record OK (length=8), second overflows
    let mut file = vec![0x00, 0x08, 0x00, 0x00]; // length=8
    file.extend(vec![0xC1u8; 4]); // 4 payload bytes (record 1 complete)
    file.extend(vec![0x00, 0x50, 0x00, 0x00]); // length=80 for record 2
    file.extend(vec![0xC2u8; 4]); // only 4 payload bytes (total 8, need 76 more)
    assert_eq!(count_length_overflows(&file), 1);
}

#[test]
fn length_exactly_matches_file() {
    // Single record that exactly fills the file
    let mut file = vec![0x00, 0x08, 0x00, 0x00]; // length=8
    file.extend(vec![0xC1u8; 4]); // exactly 4 payload bytes
    assert_eq!(count_length_overflows(&file), 0);
}

#[test]
fn length_zero_is_invalid() {
    // RDW length=0 is invalid (minimum is 4)
    let file = vec![0x00, 0x00, 0x00, 0x00, 0xC1, 0xC2, 0xC3, 0xC4];
    assert_eq!(count_length_overflows(&file), 1);
}

// ===========================================================================
// RDW length exceeds maximum record size
// ===========================================================================

#[test]
fn length_0xffff_is_suspiciously_large_but_not_ascii() {
    // 0xFFFF = 65535 — huge but not in ASCII digit range
    assert!(detect_rdw_ascii_corruption(&[0xFF, 0xFF, 0x00, 0x00]).is_none());
}

#[test]
fn length_0x7fff_max_signed_not_ascii() {
    // 32767 — maximum signed 16-bit value
    assert!(detect_rdw_ascii_corruption(&[0x7F, 0xFF, 0x00, 0x00]).is_none());
}

#[test]
fn length_in_ascii_range_always_detected() {
    // Every value in 0x3030..=0x3939 where both bytes are ASCII digits
    for d1 in b'0'..=b'9' {
        for d2 in b'0'..=b'9' {
            assert!(
                detect_rdw_ascii_corruption(&[d1, d2, 0x00, 0x00]).is_some(),
                "ASCII digit pair ({}, {}) should be detected",
                d1 as char,
                d2 as char
            );
        }
    }
}

#[test]
fn typical_mainframe_lengths_all_clean() {
    let lengths: &[u16] = &[4, 80, 100, 200, 256, 500, 1024, 4096, 8192, 32760];
    for &len in lengths {
        let hi = ((len >> 8) & 0xFF) as u8;
        let lo = (len & 0xFF) as u8;
        assert!(
            detect_rdw_ascii_corruption(&[hi, lo, 0x00, 0x00]).is_none(),
            "length={len} should be clean"
        );
    }
}

// ===========================================================================
// Non-zero reserved bytes handling
// ===========================================================================

#[test]
fn reserved_both_zero_is_clean() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
}

#[test]
fn reserved_both_printable_ascii_detected() {
    let err = detect_rdw_ascii_corruption(&[0x00, 0x50, b'A', b'B']).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn reserved_both_digits_detected() {
    let err = detect_rdw_ascii_corruption(&[0x00, 0x50, b'1', b'2']).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn reserved_both_spaces_detected() {
    let err = detect_rdw_ascii_corruption(&[0x00, 0x50, 0x20, 0x20]).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn reserved_one_printable_one_control_not_detected() {
    // Need both reserved bytes printable to trigger
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, b'A', 0x01]).is_none());
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x01, b'B']).is_none());
}

#[test]
fn reserved_both_binary_non_printable_not_detected() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x01, 0x02]).is_none());
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x80, 0xFF]).is_none());
}

#[test]
fn reserved_del_char_not_printable() {
    // 0x7F = DEL is not considered ASCII printable
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x7F, 0x7F]).is_none());
}

#[test]
fn reserved_boundary_values_printable() {
    // 0x20 (space) and 0x7E (tilde) are edges of printable range
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x20, 0x7E]).is_some());
}

// ===========================================================================
// Truncated RDW at end of file
// ===========================================================================

/// Simulate reading RDW headers from a file; returns how many were truncated.
fn count_truncated_rdw(file_data: &[u8]) -> usize {
    let mut pos = 0;
    let mut truncated = 0;
    while pos < file_data.len() {
        if pos + 4 > file_data.len() {
            truncated += 1;
            break;
        }
        let length = u16::from_be_bytes([file_data[pos], file_data[pos + 1]]) as usize;
        if length < 4 {
            break; // invalid length, stop scanning
        }
        pos += length;
    }
    truncated
}

#[test]
fn truncated_rdw_at_eof_1_byte_remaining() {
    let mut file = vec![0x00, 0x08, 0x00, 0x00]; // length=8
    file.extend(vec![0xC1u8; 4]); // complete record 1
    file.push(0x00); // truncated record 2 header (1 byte)
    assert_eq!(count_truncated_rdw(&file), 1);
}

#[test]
fn truncated_rdw_at_eof_2_bytes_remaining() {
    let mut file = vec![0x00, 0x08, 0x00, 0x00];
    file.extend(vec![0xC1u8; 4]);
    file.extend([0x00, 0x50]); // 2 bytes of next RDW
    assert_eq!(count_truncated_rdw(&file), 1);
}

#[test]
fn truncated_rdw_at_eof_3_bytes_remaining() {
    let mut file = vec![0x00, 0x08, 0x00, 0x00];
    file.extend(vec![0xC1u8; 4]);
    file.extend([0x00, 0x50, 0x00]); // 3 bytes of next RDW
    assert_eq!(count_truncated_rdw(&file), 1);
}

#[test]
fn no_truncation_exact_fit() {
    let mut file = vec![0x00, 0x08, 0x00, 0x00];
    file.extend(vec![0xC1u8; 4]);
    assert_eq!(count_truncated_rdw(&file), 0);
}

#[test]
fn truncated_with_ascii_corruption_in_partial_header() {
    // Only 3 bytes remaining, even if they look like ASCII
    let partial = [b'8', b'0', 0x00];
    assert!(detect_rdw_ascii_corruption(&partial).is_none());
}

// ===========================================================================
// Multiple RDW corruptions in sequence
// ===========================================================================

/// Scan a sequence of headers and classify each.
fn classify_rdw_sequence(headers: &[[u8; 4]]) -> (Vec<usize>, Vec<usize>) {
    let mut clean_indices = Vec::new();
    let mut corrupt_indices = Vec::new();
    for (i, header) in headers.iter().enumerate() {
        if detect_rdw_ascii_corruption(header).is_some() {
            corrupt_indices.push(i);
        } else {
            clean_indices.push(i);
        }
    }
    (clean_indices, corrupt_indices)
}

#[test]
fn sequence_all_corrupt() {
    let headers = [
        [b'0', b'0', 0x00, 0x00],
        [b'1', b'1', 0x00, 0x00],
        [b'2', b'2', 0x00, 0x00],
        [b'3', b'3', 0x00, 0x00],
        [b'9', b'9', 0x00, 0x00],
    ];
    let (clean, corrupt) = classify_rdw_sequence(&headers);
    assert!(clean.is_empty());
    assert_eq!(corrupt.len(), 5);
}

#[test]
fn sequence_alternating_corruption() {
    let headers = [
        [0x00, 0x50, 0x00, 0x00], // clean
        [b'1', b'2', 0x00, 0x00], // corrupt
        [0x00, 0x64, 0x00, 0x00], // clean
        [b'3', b'4', 0x00, 0x00], // corrupt
        [0x01, 0x00, 0x00, 0x00], // clean
    ];
    let (clean, corrupt) = classify_rdw_sequence(&headers);
    assert_eq!(clean, vec![0, 2, 4]);
    assert_eq!(corrupt, vec![1, 3]);
}

#[test]
fn sequence_burst_then_recovery() {
    let mut headers: Vec<[u8; 4]> = vec![[0x00, 0x50, 0x00, 0x00]; 20];
    // Corrupt records 5..10
    for header in &mut headers[5..10] {
        *header = [b'5', b'5', 0x00, 0x00];
    }
    let (clean, corrupt) = classify_rdw_sequence(&headers);
    assert_eq!(clean.len(), 15);
    assert_eq!(corrupt.len(), 5);
    assert_eq!(corrupt, vec![5, 6, 7, 8, 9]);
}

#[test]
fn sequence_corruption_at_start_recovery_later() {
    let headers = [
        [b'8', b'0', 0x00, 0x00], // corrupt
        [b'4', b'2', 0x00, 0x00], // corrupt
        [0x00, 0x50, 0x00, 0x00], // clean
        [0x00, 0x64, 0x00, 0x00], // clean
    ];
    let (clean, corrupt) = classify_rdw_sequence(&headers);
    assert_eq!(corrupt, vec![0, 1]);
    assert_eq!(clean, vec![2, 3]);
}

#[test]
fn sequence_different_heuristics_across_records() {
    let headers = [
        [b'1', b'2', 0x00, 0x00], // heuristic 1 (digits)
        [0x00, 0x50, b'X', b'Y'], // heuristic 3 (reserved)
        [0x30, 0x3A, 0x00, 0x00], // heuristic 2 (suspect length)
        [0x00, 0x50, 0x00, 0x00], // clean
    ];
    let (clean, corrupt) = classify_rdw_sequence(&headers);
    assert_eq!(clean.len(), 1);
    assert_eq!(corrupt.len(), 3);
}

#[test]
fn sequence_100_records_single_corruption_at_position_50() {
    let mut headers: Vec<[u8; 4]> = vec![[0x00, 0x50, 0x00, 0x00]; 100];
    headers[50] = [b'9', b'9', 0x00, 0x00];
    let (clean, corrupt) = classify_rdw_sequence(&headers);
    assert_eq!(clean.len(), 99);
    assert_eq!(corrupt, vec![50]);
}

// ===========================================================================
// Error message content validation
// ===========================================================================

#[test]
fn heuristic1_message_contains_ascii_digits_label() {
    let err = detect_rdw_ascii_corruption(&[b'7', b'8', 0x00, 0x00]).unwrap();
    assert!(err.message.contains("ASCII digits"));
}

#[test]
fn heuristic3_message_contains_reserved_bytes_label() {
    let err = detect_rdw_ascii_corruption(&[0x00, 0x10, b'M', b'N']).unwrap();
    assert!(err.message.contains("reserved bytes"));
}

#[test]
fn heuristic2_message_contains_suspect_length() {
    // 0x30, 0x3A: first byte '0' digit, second 0x3A not digit → h1 skipped, h2 fires
    let err = detect_rdw_ascii_corruption(&[0x30, 0x3A, 0x00, 0x00]).unwrap();
    assert!(
        err.message.contains("suspiciously large") || err.message.contains("303A"),
        "h2 message should mention suspect length: {}",
        err.message
    );
}
