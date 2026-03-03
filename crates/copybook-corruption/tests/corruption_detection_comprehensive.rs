// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive corruption detection tests: all-zero records, all-space records,
//! invalid EBCDIC bytes, short records, unexpected binary patterns, severity
//! classification, multi-corruption files, and clean-record false-positive resistance.

use copybook_core::ErrorCode;
use copybook_corruption::{
    detect_ebcdic_corruption, detect_packed_corruption, detect_rdw_ascii_corruption,
};

// ===========================================================================
// All-zero record detection
// ===========================================================================

#[test]
fn all_zero_record_80_bytes_fully_corrupted() {
    let record = vec![0x00u8; 80];
    let errors = detect_ebcdic_corruption(&record, "ZERO-REC");
    assert_eq!(errors.len(), 80, "every null byte should be flagged");
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKC301_INVALID_EBCDIC_BYTE)
    );
}

#[test]
fn all_zero_packed_single_byte_invalid_sign() {
    // 0x00: high nibble 0 (valid digit), low nibble 0 (invalid sign)
    let errors = detect_packed_corruption(&[0x00], "ZERO-PKD");
    assert!(!errors.is_empty());
    assert!(errors.iter().any(|e| e.message.contains("sign")));
}

#[test]
fn all_zero_packed_multi_byte_invalid_sign() {
    let record = vec![0x00u8; 5];
    let errors = detect_packed_corruption(&record, "ZERO-PKD5");
    // Last byte has invalid sign nibble 0; all bytes are technically valid digits
    assert!(errors.iter().any(|e| e.message.contains("sign")));
}

#[test]
fn all_zero_rdw_header_not_flagged() {
    // All-zero RDW is degenerate but not ASCII-corrupted
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x00, 0x00, 0x00]).is_none());
}

// ===========================================================================
// All-space record detection (EBCDIC space = 0x40)
// ===========================================================================

#[test]
fn all_ebcdic_space_record_no_corruption() {
    let record = vec![0x40u8; 80];
    assert!(detect_ebcdic_corruption(&record, "SPACE-REC").is_empty());
}

#[test]
fn all_ascii_space_record_no_corruption() {
    // ASCII space 0x20 is in the clean range for the EBCDIC heuristic
    let record = vec![0x20u8; 80];
    assert!(detect_ebcdic_corruption(&record, "ASCII-SPACE").is_empty());
}

#[test]
fn mixed_space_types_no_corruption() {
    let mut record = vec![0x40u8; 40];
    record.extend(vec![0x20u8; 40]);
    assert!(detect_ebcdic_corruption(&record, "MIX-SPACE").is_empty());
}

// ===========================================================================
// Records with invalid EBCDIC bytes
// ===========================================================================

#[test]
fn single_invalid_ebcdic_byte_at_start() {
    let mut record = vec![0xC1u8; 80];
    record[0] = 0x00;
    let errors = detect_ebcdic_corruption(&record, "BAD-START");
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].context.as_ref().and_then(|c| c.byte_offset),
        Some(0)
    );
}

#[test]
fn single_invalid_ebcdic_byte_at_end() {
    let mut record = vec![0xC1u8; 80];
    record[79] = 0x7F;
    let errors = detect_ebcdic_corruption(&record, "BAD-END");
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].context.as_ref().and_then(|c| c.byte_offset),
        Some(79)
    );
}

#[test]
fn single_invalid_ebcdic_byte_in_middle() {
    let mut record = vec![0xF0u8; 80];
    record[40] = 0x0A;
    let errors = detect_ebcdic_corruption(&record, "BAD-MID");
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].context.as_ref().and_then(|c| c.byte_offset),
        Some(40)
    );
}

#[test]
fn multiple_invalid_bytes_scattered() {
    let mut record = vec![0xC1u8; 100];
    record[10] = 0x00;
    record[25] = 0x0D;
    record[50] = 0x7F;
    record[75] = 0x80;
    record[99] = 0x9F;
    let errors = detect_ebcdic_corruption(&record, "SCATTER");
    assert_eq!(errors.len(), 5);
    let offsets: Vec<u64> = errors
        .iter()
        .filter_map(|e| e.context.as_ref().and_then(|c| c.byte_offset))
        .collect();
    assert_eq!(offsets, vec![10, 25, 50, 75, 99]);
}

#[test]
fn contiguous_block_of_invalid_bytes() {
    let mut record = vec![0xC1u8; 80];
    for byte in &mut record[20..30] {
        *byte = 0x05;
    }
    let errors = detect_ebcdic_corruption(&record, "BLOCK-BAD");
    assert_eq!(errors.len(), 10);
}

// ===========================================================================
// Records shorter than schema LRECL
// ===========================================================================

#[test]
fn short_record_1_byte_clean() {
    assert!(detect_ebcdic_corruption(&[0xC1], "SHORT-1").is_empty());
}

#[test]
fn short_record_1_byte_corrupt() {
    let errors = detect_ebcdic_corruption(&[0x00], "SHORT-1");
    assert_eq!(errors.len(), 1);
}

#[test]
fn short_record_empty() {
    assert!(detect_ebcdic_corruption(&[], "EMPTY").is_empty());
    assert!(detect_packed_corruption(&[], "EMPTY").is_empty());
}

#[test]
fn partial_packed_field_2_bytes() {
    // Valid 2-byte packed: 0x12, 0x3C (digit + positive sign)
    assert!(detect_packed_corruption(&[0x12, 0x3C], "SHORT-PKD").is_empty());
}

// ===========================================================================
// Unexpected binary patterns
// ===========================================================================

#[test]
fn all_0xff_ebcdic_is_clean() {
    let record = vec![0xFFu8; 80];
    assert!(detect_ebcdic_corruption(&record, "ALL-FF").is_empty());
}

#[test]
fn alternating_0x00_0xff_pattern() {
    let record: Vec<u8> = (0..80)
        .map(|i| if i % 2 == 0 { 0x00 } else { 0xFF })
        .collect();
    let errors = detect_ebcdic_corruption(&record, "ALT-PATTERN");
    assert_eq!(errors.len(), 40, "every 0x00 byte should be flagged");
}

#[test]
fn incrementing_byte_pattern_covers_corrupt_ranges() {
    // Bytes 0x00..0x1F and 0x7F..0x9F are corrupt
    let data: Vec<u8> = (0x00..=0xFF).collect();
    let errors = detect_ebcdic_corruption(&data, "FULL-RANGE");
    // C0 controls: 0x00..=0x1F = 32 bytes
    // C1 controls: 0x7F..=0x9F = 33 bytes
    assert_eq!(errors.len(), 65);
}

#[test]
fn packed_0xff_single_byte_is_valid() {
    // 0xFF: high nibble F (valid: only A, B, E are invalid), low nibble F (valid sign)
    let errors = detect_packed_corruption(&[0xFF], "FF-PKD");
    assert!(errors.is_empty(), "0xFF is valid packed: digit F + sign F");
}

#[test]
fn packed_0xae_has_invalid_high_nibble() {
    // 0xAE: high nibble A (invalid), low nibble E (valid sign in terminal)
    let errors = detect_packed_corruption(&[0xAE], "AE-PKD");
    assert!(errors.iter().any(|e| e.message.contains("high nibble")));
}

#[test]
fn packed_repeating_0xab_pattern() {
    // 0xAB: high nibble A (invalid), low nibble B (invalid in non-terminal)
    let data = vec![0xABu8; 3];
    // Last byte: high nibble A (invalid), low nibble B (valid sign)
    let errors = detect_packed_corruption(&data, "AB-PATTERN");
    assert!(errors.len() >= 3); // multiple nibble errors across bytes
}

// ===========================================================================
// Severity classification (warning vs error by error code)
// ===========================================================================

#[test]
fn ebcdic_corruption_always_uses_cbkc301() {
    let data: Vec<u8> = (0x00..=0x1F).collect();
    let errors = detect_ebcdic_corruption(&data, "SEV");
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKC301_INVALID_EBCDIC_BYTE)
    );
}

#[test]
fn packed_corruption_always_uses_cbkd401() {
    let errors = detect_packed_corruption(&[0xAB, 0xCD, 0x17], "SEV-PKD");
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
    );
}

#[test]
fn rdw_corruption_always_uses_cbkf104() {
    let err = detect_rdw_ascii_corruption(&[b'1', b'2', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn all_three_error_codes_distinct() {
    let e1 = ErrorCode::CBKC301_INVALID_EBCDIC_BYTE;
    let e2 = ErrorCode::CBKD401_COMP3_INVALID_NIBBLE;
    let e3 = ErrorCode::CBKF104_RDW_SUSPECT_ASCII;
    assert_ne!(e1, e2);
    assert_ne!(e2, e3);
    assert_ne!(e1, e3);
}

// ===========================================================================
// Multiple corruption types in single file simulation
// ===========================================================================

/// Simulate scanning a record with RDW, text fields, and packed fields.
fn scan_full_record(
    rdw: &[u8],
    text_fields: &[(&[u8], &str)],
    packed_fields: &[(&[u8], &str)],
) -> Vec<copybook_core::Error> {
    let mut all = Vec::new();
    if let Some(err) = detect_rdw_ascii_corruption(rdw) {
        all.push(err);
    }
    for (data, path) in text_fields {
        all.extend(detect_ebcdic_corruption(data, path));
    }
    for (data, path) in packed_fields {
        all.extend(detect_packed_corruption(data, path));
    }
    all
}

#[test]
fn multi_type_rdw_and_ebcdic_corruption() {
    let errors = scan_full_record(
        &[b'8', b'0', 0x00, 0x00],
        &[(&[0x00, 0x01], "NAME")],
        &[(&[0x12, 0x3C], "AMT")],
    );
    let codes: Vec<_> = errors.iter().map(|e| e.code).collect();
    assert!(codes.contains(&ErrorCode::CBKF104_RDW_SUSPECT_ASCII));
    assert!(codes.contains(&ErrorCode::CBKC301_INVALID_EBCDIC_BYTE));
    assert!(!codes.contains(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE));
}

#[test]
fn multi_type_rdw_and_packed_corruption() {
    let errors = scan_full_record(
        &[b'4', b'2', 0x00, 0x00],
        &[(&[0xC1, 0xC2], "NAME")],
        &[(&[0xAB, 0x17], "AMT")],
    );
    let codes: Vec<_> = errors.iter().map(|e| e.code).collect();
    assert!(codes.contains(&ErrorCode::CBKF104_RDW_SUSPECT_ASCII));
    assert!(codes.contains(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE));
    assert!(!codes.contains(&ErrorCode::CBKC301_INVALID_EBCDIC_BYTE));
}

#[test]
fn multi_type_all_three_corruptions_present() {
    let errors = scan_full_record(
        b"10AB",
        &[(&[0x00, 0x7F, 0x80], "TEXT")],
        &[(&[0xAA, 0x17], "PKD")],
    );
    let codes: Vec<_> = errors.iter().map(|e| e.code).collect();
    assert!(codes.contains(&ErrorCode::CBKF104_RDW_SUSPECT_ASCII));
    assert!(codes.contains(&ErrorCode::CBKC301_INVALID_EBCDIC_BYTE));
    assert!(codes.contains(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE));
    assert!(errors.len() >= 5);
}

#[test]
fn multi_record_file_with_mixed_corruption() {
    let records: Vec<Vec<copybook_core::Error>> = vec![
        // Record 0: clean
        scan_full_record(
            &[0x00, 0x50, 0x00, 0x00],
            &[(&[0xC1, 0xC2], "R0.NAME")],
            &[(&[0x12, 0x3C], "R0.AMT")],
        ),
        // Record 1: RDW corrupt
        scan_full_record(
            &[b'8', b'0', 0x00, 0x00],
            &[(&[0xC1], "R1.NAME")],
            &[(&[0x1C], "R1.AMT")],
        ),
        // Record 2: EBCDIC corrupt
        scan_full_record(
            &[0x00, 0x50, 0x00, 0x00],
            &[(&[0x00, 0x7F], "R2.NAME")],
            &[(&[0x1C], "R2.AMT")],
        ),
        // Record 3: packed corrupt
        scan_full_record(
            &[0x00, 0x50, 0x00, 0x00],
            &[(&[0xC1], "R3.NAME")],
            &[(&[0xAB, 0x17], "R3.AMT")],
        ),
        // Record 4: clean
        scan_full_record(
            &[0x00, 0x50, 0x00, 0x00],
            &[(&[0xF0, 0xF1], "R4.NUM")],
            &[(&[0x99, 0x9C], "R4.AMT")],
        ),
    ];
    let clean_count = records.iter().filter(|r| r.is_empty()).count();
    let corrupt_count = records.iter().filter(|r| !r.is_empty()).count();
    assert_eq!(clean_count, 2);
    assert_eq!(corrupt_count, 3);
}

// ===========================================================================
// Clean records pass without false positives
// ===========================================================================

#[test]
fn clean_ebcdic_uppercase_alphabet() {
    let data: Vec<u8> = (0xC1..=0xC9)
        .chain(0xD1..=0xD9)
        .chain(0xE2..=0xE9)
        .collect();
    assert!(detect_ebcdic_corruption(&data, "ALPHA").is_empty());
}

#[test]
fn clean_ebcdic_digits() {
    let data: Vec<u8> = (0xF0..=0xF9).collect();
    assert!(detect_ebcdic_corruption(&data, "DIGITS").is_empty());
}

#[test]
fn clean_packed_all_sign_variants() {
    let patterns: &[&[u8]] = &[
        &[0x12, 0x3C], // positive
        &[0x45, 0x6D], // negative
        &[0x78, 0x9F], // unsigned
        &[0x0A],       // sign A
        &[0x0B],       // sign B
        &[0x0E],       // sign E
    ];
    for pattern in patterns {
        assert!(
            detect_packed_corruption(pattern, "CLEAN-PKD").is_empty(),
            "pattern {pattern:?} should be clean"
        );
    }
}

#[test]
fn clean_rdw_typical_lengths() {
    let headers: &[[u8; 4]] = &[
        [0x00, 0x04, 0x00, 0x00], // 4
        [0x00, 0x50, 0x00, 0x00], // 80
        [0x00, 0xC8, 0x00, 0x00], // 200
        [0x01, 0x00, 0x00, 0x00], // 256
        [0x10, 0x00, 0x00, 0x00], // 4096
        [0x7F, 0xFF, 0x00, 0x00], // 32767
    ];
    for header in headers {
        assert!(
            detect_rdw_ascii_corruption(header).is_none(),
            "header {header:?} should be clean"
        );
    }
}

#[test]
fn clean_full_record_no_false_positives() {
    let errors = scan_full_record(
        &[0x00, 0x50, 0x00, 0x00],
        &[
            (&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5], "NAME"),
            (&[0xF0, 0xF1, 0xF2, 0xF3, 0xF4], "NUM"),
            (&[0x40, 0x40, 0x40], "FILLER"),
        ],
        &[
            (&[0x12, 0x34, 0x5C], "AMT1"),
            (&[0x00, 0x00, 0x0C], "AMT2"),
            (&[0x99, 0x99, 0x9D], "AMT3"),
        ],
    );
    assert!(errors.is_empty(), "clean record should produce zero errors");
}

#[test]
fn clean_1000_records_no_false_positives() {
    for _ in 0..1000 {
        assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
        assert!(detect_ebcdic_corruption(&[0xC1, 0xF0, 0x40], "F").is_empty());
        assert!(detect_packed_corruption(&[0x12, 0x3C], "P").is_empty());
    }
}

// ===========================================================================
// Error context integrity across corruption types
// ===========================================================================

#[test]
fn error_contexts_carry_field_paths_correctly() {
    let ebcdic_errs = detect_ebcdic_corruption(&[0x00], "REC.GROUP.TEXT");
    let packed_errs = detect_packed_corruption(&[0x17], "REC.GROUP.AMT");

    assert_eq!(
        ebcdic_errs[0]
            .context
            .as_ref()
            .and_then(|c| c.field_path.as_deref()),
        Some("REC.GROUP.TEXT")
    );
    assert_eq!(
        packed_errs[0]
            .context
            .as_ref()
            .and_then(|c| c.field_path.as_deref()),
        Some("REC.GROUP.AMT")
    );
}

#[test]
fn error_byte_offsets_are_zero_indexed() {
    let data = [0xC1, 0x00, 0xC2, 0x01, 0xC3];
    let errors = detect_ebcdic_corruption(&data, "OFF");
    let offsets: Vec<u64> = errors
        .iter()
        .filter_map(|e| e.context.as_ref().and_then(|c| c.byte_offset))
        .collect();
    assert_eq!(offsets, vec![1, 3]);
}

// ===========================================================================
// Performance: large buffers process correctly
// ===========================================================================

#[test]
fn large_buffer_128kb_clean() {
    let data = vec![0xF0u8; 128 * 1024];
    assert!(detect_ebcdic_corruption(&data, "128K").is_empty());
}

#[test]
fn large_buffer_128kb_all_corrupt() {
    let data = vec![0x00u8; 128 * 1024];
    let errors = detect_ebcdic_corruption(&data, "128K-BAD");
    assert_eq!(errors.len(), 128 * 1024);
}
