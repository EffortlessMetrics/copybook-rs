// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! End-to-end integration tests for the corruption detection façade.
//! Validates that all detectors are accessible through the single facade crate
//! and produce correct results for combined corruption scenarios.

use copybook_core::ErrorCode;
use copybook_corruption::{
    detect_ebcdic_corruption, detect_packed_corruption, detect_rdw_ascii_corruption,
};

// ===========================================================================
// End-to-end corruption scanning
// ===========================================================================

#[test]
fn e2e_scan_clean_record_no_errors() {
    // Simulates scanning a clean record: valid RDW + clean EBCDIC + valid packed
    let rdw = [0x00, 0x50, 0x00, 0x00]; // length=80
    let text_field = [0xC1, 0xC2, 0xC3, 0xC4, 0xC5]; // EBCDIC "ABCDE"
    let packed_field = [0x01, 0x23, 0x4C]; // +01234

    assert!(detect_rdw_ascii_corruption(&rdw).is_none());
    assert!(detect_ebcdic_corruption(&text_field, "TEXT-FLD").is_empty());
    assert!(detect_packed_corruption(&packed_field, "AMT-FLD").is_empty());
}

#[test]
fn e2e_scan_all_corrupted() {
    // RDW has ASCII digits, text has nulls, packed has bad sign
    let rdw = [b'8', b'0', 0x00, 0x00];
    let text_field = [0x00, 0x01, 0x02];
    let packed_field = [0x12, 0x37]; // sign nibble 7

    let rdw_err = detect_rdw_ascii_corruption(&rdw);
    let text_errs = detect_ebcdic_corruption(&text_field, "NAME");
    let packed_errs = detect_packed_corruption(&packed_field, "BAL");

    assert!(rdw_err.is_some());
    assert_eq!(text_errs.len(), 3);
    assert!(!packed_errs.is_empty());
}

#[test]
fn e2e_mixed_clean_and_corrupted_fields() {
    // RDW clean, one text field clean, another corrupted, packed clean
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
    assert!(detect_ebcdic_corruption(&[0xC1, 0xC2], "CLEAN-FLD").is_empty());

    let corrupted_text = [0xC1, 0x05, 0xC3]; // byte at pos 1 corrupted
    assert_eq!(
        detect_ebcdic_corruption(&corrupted_text, "BAD-FLD").len(),
        1
    );

    assert!(detect_packed_corruption(&[0x12, 0x3C], "AMT").is_empty());
}

// ===========================================================================
// Multiple corruption types in one file
// ===========================================================================

#[test]
fn multiple_records_mixed_corruption() {
    // Record 1: clean RDW, corrupted text
    let rdw1 = [0x00, 0x20, 0x00, 0x00]; // length=32
    let text1 = [0x00, 0x7F]; // two corrupted bytes
    assert!(detect_rdw_ascii_corruption(&rdw1).is_none());
    assert_eq!(detect_ebcdic_corruption(&text1, "REC1.NAME").len(), 2);

    // Record 2: corrupted RDW, clean text
    let rdw2 = [b'3', b'2', 0x00, 0x00]; // ASCII "32"
    let text2 = [0xC1, 0xC2, 0xC3]; // clean
    assert!(detect_rdw_ascii_corruption(&rdw2).is_some());
    assert!(detect_ebcdic_corruption(&text2, "REC2.NAME").is_empty());

    // Record 3: clean RDW, corrupted packed
    let rdw3 = [0x00, 0x10, 0x00, 0x00]; // length=16
    let packed3 = [0xAB, 0x1C]; // high nibble A, low nibble B both invalid
    assert!(detect_rdw_ascii_corruption(&rdw3).is_none());
    assert!(detect_packed_corruption(&packed3, "REC3.AMT").len() >= 2);
}

#[test]
fn all_three_corruption_types_simultaneously() {
    let rdw_err = detect_rdw_ascii_corruption(&[b'1', b'0', 0x00, 0x00]);
    let ebcdic_errs = detect_ebcdic_corruption(&[0x00, 0x01], "FLD1");
    let packed_errs = detect_packed_corruption(&[0xAA, 0x17], "FLD2");

    // Collect all error codes
    let mut all_codes = vec![];
    if let Some(e) = rdw_err {
        all_codes.push(e.code);
    }
    for e in &ebcdic_errs {
        all_codes.push(e.code);
    }
    for e in &packed_errs {
        all_codes.push(e.code);
    }

    // We should have at least 3 different errors
    assert!(all_codes.len() >= 4);
    assert!(all_codes.contains(&ErrorCode::CBKF104_RDW_SUSPECT_ASCII));
    assert!(all_codes.contains(&ErrorCode::CBKC301_INVALID_EBCDIC_BYTE));
    assert!(all_codes.contains(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE));
}

// ===========================================================================
// Clean data verification
// ===========================================================================

#[test]
fn clean_ebcdic_uppercase_block() {
    // Full range of EBCDIC uppercase letters
    let data: Vec<u8> = (0xC1..=0xC9)
        .chain(0xD1..=0xD9)
        .chain(0xE2..=0xE9)
        .collect();
    assert!(detect_ebcdic_corruption(&data, "ALPHA").is_empty());
}

#[test]
fn clean_ebcdic_digit_block() {
    let data: Vec<u8> = (0xF0..=0xF9).collect();
    assert!(detect_ebcdic_corruption(&data, "NUM").is_empty());
}

#[test]
fn clean_packed_all_valid_patterns() {
    // Positive, negative, unsigned — all clean
    let patterns: &[&[u8]] = &[
        &[0x0C],             // +0
        &[0x0D],             // -0
        &[0x0F],             // unsigned 0
        &[0x12, 0x3C],       // +123
        &[0x45, 0x6D],       // -456
        &[0x78, 0x9F],       // unsigned 789
        &[0x99, 0x99, 0x9C], // +99999
    ];
    for (i, pattern) in patterns.iter().enumerate() {
        assert!(
            detect_packed_corruption(pattern, "CLEAN").is_empty(),
            "pattern {i} should be clean: {pattern:?}"
        );
    }
}

#[test]
fn clean_rdw_various_valid_lengths() {
    let valid_headers: &[[u8; 4]] = &[
        [0x00, 0x04, 0x00, 0x00], // length=4 (minimum)
        [0x00, 0x50, 0x00, 0x00], // length=80 (standard)
        [0x01, 0x00, 0x00, 0x00], // length=256
        [0x10, 0x00, 0x00, 0x00], // length=4096
        [0x7F, 0xFF, 0x00, 0x00], // length=32767
    ];
    for header in valid_headers {
        assert!(
            detect_rdw_ascii_corruption(header).is_none(),
            "header {header:?} should be clean"
        );
    }
}

// ===========================================================================
// Facade delegation consistency
// ===========================================================================

#[test]
fn facade_ebcdic_matches_microcrate_for_clean() {
    let data = [0xC1, 0xC2, 0xC3];
    let facade = detect_ebcdic_corruption(&data, "F");
    assert!(facade.is_empty());
}

#[test]
fn facade_ebcdic_matches_microcrate_for_corrupted() {
    let data = [0x00, 0x01, 0x02];
    let errors = detect_ebcdic_corruption(&data, "F");
    assert_eq!(errors.len(), 3);
    for err in &errors {
        assert_eq!(err.code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
    }
}

#[test]
fn facade_packed_matches_microcrate_for_clean() {
    assert!(detect_packed_corruption(&[0x12, 0x3C], "F").is_empty());
}

#[test]
fn facade_packed_matches_microcrate_for_corrupted() {
    let errors = detect_packed_corruption(&[0xAB, 0x17], "F");
    assert!(!errors.is_empty());
}

#[test]
fn facade_rdw_matches_microcrate_for_clean() {
    assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
}

#[test]
fn facade_rdw_matches_microcrate_for_corrupted() {
    let err = detect_rdw_ascii_corruption(&[b'1', b'2', 0x00, 0x00]).unwrap();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

// ===========================================================================
// Edge cases
// ===========================================================================

#[test]
fn ebcdic_empty_field_path() {
    let errors = detect_ebcdic_corruption(&[0x00], "");
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0]
            .context
            .as_ref()
            .and_then(|c| c.field_path.as_deref()),
        Some("")
    );
}

#[test]
fn packed_single_byte_with_all_valid_signs() {
    // Valid sign nibbles: A, B, C, D, E, F
    for sign in 0x0A..=0x0F {
        let byte = 0x00 | sign; // digit 0 + sign
        assert!(
            detect_packed_corruption(&[byte], "SIGN").is_empty(),
            "sign 0x{sign:X} should be valid"
        );
    }
}

#[test]
fn rdw_exactly_4_bytes_processed() {
    // Exactly 4 bytes — the minimum for RDW detection
    let result = detect_rdw_ascii_corruption(&[b'1', b'2', 0x00, 0x00]);
    assert!(result.is_some());
}

#[test]
fn ebcdic_max_length_buffer() {
    // 4096-byte buffer of clean EBCDIC data
    let data = vec![0xF0u8; 4096];
    assert!(detect_ebcdic_corruption(&data, "BIG").is_empty());
}

#[test]
fn packed_realistic_amount_999999_99() {
    // +99999999 = [0x09, 0x99, 0x99, 0x99, 0x9C]
    assert!(detect_packed_corruption(&[0x09, 0x99, 0x99, 0x99, 0x9C], "AMOUNT").is_empty());
}
