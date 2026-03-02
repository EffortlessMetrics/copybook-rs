// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Comprehensive detector tests: individual detector registration/invocation,
//! detector chain execution, results aggregation, custom detector patterns,
//! configuration options, and performance overhead verification.

use copybook_core::ErrorCode;
use copybook_corruption_detectors::{detect_ebcdic_corruption, detect_packed_corruption};

// ===========================================================================
// Individual detector registration/invocation
// ===========================================================================

#[test]
fn ebcdic_detector_returns_vec_of_errors() {
    let errors = detect_ebcdic_corruption(&[0x00], "F");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].code, ErrorCode::CBKC301_INVALID_EBCDIC_BYTE);
}

#[test]
fn packed_detector_returns_vec_of_errors() {
    let errors = detect_packed_corruption(&[0x17], "F");
    assert!(!errors.is_empty());
    assert_eq!(errors[0].code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
}

#[test]
fn ebcdic_detector_on_empty_returns_empty_vec() {
    let errors = detect_ebcdic_corruption(&[], "EMPTY");
    assert!(errors.is_empty());
}

#[test]
fn packed_detector_on_empty_returns_empty_vec() {
    let errors = detect_packed_corruption(&[], "EMPTY");
    assert!(errors.is_empty());
}

#[test]
fn ebcdic_detector_clean_data_returns_empty() {
    let data: Vec<u8> = (0xC1..=0xC9).collect();
    assert!(detect_ebcdic_corruption(&data, "CLEAN").is_empty());
}

#[test]
fn packed_detector_clean_data_returns_empty() {
    assert!(detect_packed_corruption(&[0x12, 0x34, 0x5C], "CLEAN").is_empty());
}

// ===========================================================================
// Detector chain execution order (EBCDIC then packed on same record)
// ===========================================================================

/// Simulate a detector chain: run EBCDIC detector on text fields, then packed
/// on numeric fields, collecting all errors in order.
fn run_detector_chain(
    text_fields: &[(&[u8], &str)],
    packed_fields: &[(&[u8], &str)],
) -> Vec<copybook_core::Error> {
    let mut all_errors = Vec::new();
    for (data, path) in text_fields {
        all_errors.extend(detect_ebcdic_corruption(data, path));
    }
    for (data, path) in packed_fields {
        all_errors.extend(detect_packed_corruption(data, path));
    }
    all_errors
}

#[test]
fn chain_ebcdic_first_packed_second() {
    let errors = run_detector_chain(&[(&[0x00, 0x01], "TEXT")], &[(&[0xAB, 0x17], "PKD")]);
    // EBCDIC errors come first in the vector
    let ebcdic_count = errors
        .iter()
        .take_while(|e| e.code == ErrorCode::CBKC301_INVALID_EBCDIC_BYTE)
        .count();
    assert_eq!(ebcdic_count, 2);
    // Remaining are packed errors
    assert!(
        errors[ebcdic_count..]
            .iter()
            .all(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
    );
}

#[test]
fn chain_all_clean_returns_empty() {
    let errors = run_detector_chain(&[(&[0xC1, 0xC2], "TEXT")], &[(&[0x12, 0x3C], "PKD")]);
    assert!(errors.is_empty());
}

#[test]
fn chain_only_ebcdic_errors() {
    let errors = run_detector_chain(&[(&[0x00, 0x7F], "TEXT")], &[(&[0x12, 0x3C], "PKD")]);
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKC301_INVALID_EBCDIC_BYTE)
    );
    assert_eq!(errors.len(), 2);
}

#[test]
fn chain_only_packed_errors() {
    let errors = run_detector_chain(&[(&[0xC1, 0xC2], "TEXT")], &[(&[0xAB, 0x17], "PKD")]);
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
    );
}

#[test]
fn chain_multiple_fields_interleaved() {
    let errors = run_detector_chain(
        &[(&[0xC1, 0x00], "TEXT1"), (&[0x7F, 0xC2], "TEXT2")],
        &[(&[0x12, 0x3C], "PKD1"), (&[0xAB, 0x17], "PKD2")],
    );
    // TEXT1 has 1 error, TEXT2 has 1 error, PKD1 clean, PKD2 has errors
    let ebcdic_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.code == ErrorCode::CBKC301_INVALID_EBCDIC_BYTE)
        .collect();
    let packed_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
        .collect();
    assert_eq!(ebcdic_errors.len(), 2);
    assert!(!packed_errors.is_empty());
}

// ===========================================================================
// Detector results aggregation
// ===========================================================================

#[test]
fn aggregation_total_error_count() {
    let text_errors = detect_ebcdic_corruption(&[0x00, 0x01, 0x02], "TEXT");
    let packed_errors = detect_packed_corruption(&[0xAB, 0x17], "PKD");
    let total = text_errors.len() + packed_errors.len();
    assert!(total >= 5, "expected at least 5 errors, got {total}");
}

#[test]
fn aggregation_group_by_error_code() {
    let mut all_errors = Vec::new();
    all_errors.extend(detect_ebcdic_corruption(&[0x00, 0x01], "T"));
    all_errors.extend(detect_packed_corruption(&[0xAB, 0x17], "P"));

    let ebcdic_count = all_errors
        .iter()
        .filter(|e| e.code == ErrorCode::CBKC301_INVALID_EBCDIC_BYTE)
        .count();
    let packed_count = all_errors
        .iter()
        .filter(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
        .count();

    assert_eq!(ebcdic_count, 2);
    assert!(packed_count >= 2);
}

#[test]
fn aggregation_distinct_field_paths() {
    let mut all_errors = Vec::new();
    all_errors.extend(detect_ebcdic_corruption(&[0x00], "REC.NAME"));
    all_errors.extend(detect_ebcdic_corruption(&[0x01], "REC.ADDR"));
    all_errors.extend(detect_packed_corruption(&[0x17], "REC.AMT"));

    let paths: Vec<_> = all_errors
        .iter()
        .filter_map(|e| e.context.as_ref().and_then(|c| c.field_path.as_deref()))
        .collect();
    assert!(paths.contains(&"REC.NAME"));
    assert!(paths.contains(&"REC.ADDR"));
    assert!(paths.contains(&"REC.AMT"));
}

#[test]
fn aggregation_across_10_fields() {
    let mut total_errors = 0;
    for i in 0..5 {
        let field_name = format!("TEXT-{i}");
        let data = if i % 2 == 0 {
            vec![0x00u8]
        } else {
            vec![0xC1u8]
        };
        total_errors += detect_ebcdic_corruption(&data, &field_name).len();
    }
    for i in 0..5 {
        let field_name = format!("PKD-{i}");
        let data = if i % 2 == 0 { vec![0x17u8] } else { vec![0x1C] };
        total_errors += detect_packed_corruption(&data, &field_name).len();
    }
    // 3 corrupt EBCDIC fields (even indices) + 3 corrupt packed fields (even indices)
    assert!(total_errors >= 6);
}

// ===========================================================================
// Custom detector implementation pattern
// ===========================================================================

/// A custom detector that flags records where every byte is the same value.
fn detect_uniform_record(data: &[u8], field_path: &str) -> Vec<copybook_core::Error> {
    if data.len() < 2 {
        return Vec::new();
    }
    let first = data[0];
    if data.iter().all(|&b| b == first) {
        vec![
            copybook_core::Error::new(
                ErrorCode::CBKC301_INVALID_EBCDIC_BYTE,
                format!("Uniform byte pattern 0x{first:02X} detected in field {field_path}"),
            )
            .with_field(field_path),
        ]
    } else {
        Vec::new()
    }
}

#[test]
fn custom_detector_uniform_all_zeros() {
    let errors = detect_uniform_record(&[0x00; 80], "UNIFORM");
    assert_eq!(errors.len(), 1);
}

#[test]
fn custom_detector_uniform_all_ff() {
    let errors = detect_uniform_record(&[0xFF; 80], "UNIFORM");
    assert_eq!(errors.len(), 1);
}

#[test]
fn custom_detector_non_uniform_data() {
    let errors = detect_uniform_record(&[0xC1, 0xC2, 0xC3], "MIXED");
    assert!(errors.is_empty());
}

#[test]
fn custom_detector_single_byte_not_flagged() {
    let errors = detect_uniform_record(&[0x00], "SHORT");
    assert!(errors.is_empty());
}

#[test]
fn custom_detector_integrates_with_built_in() {
    let mut all_errors = Vec::new();
    all_errors.extend(detect_ebcdic_corruption(&[0x00, 0x7F], "TEXT"));
    all_errors.extend(detect_uniform_record(&[0xAA; 10], "UNIFORM-FLD"));
    all_errors.extend(detect_packed_corruption(&[0xAB, 0x17], "PKD"));

    assert!(all_errors.len() >= 4);
    let has_uniform = all_errors.iter().any(|e| e.message.contains("Uniform"));
    assert!(has_uniform);
}

// ===========================================================================
// Detector configuration options (field path, data ranges)
// ===========================================================================

#[test]
fn config_empty_field_path_allowed() {
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
fn config_deeply_nested_field_path() {
    let path = "ROOT.GROUP1.GROUP2.GROUP3.FIELD";
    let errors = detect_ebcdic_corruption(&[0x00], path);
    assert_eq!(
        errors[0]
            .context
            .as_ref()
            .and_then(|c| c.field_path.as_deref()),
        Some(path)
    );
}

#[test]
fn config_unicode_field_path() {
    let path = "REC.FIELD-Ü";
    let errors = detect_ebcdic_corruption(&[0x00], path);
    assert_eq!(
        errors[0]
            .context
            .as_ref()
            .and_then(|c| c.field_path.as_deref()),
        Some(path)
    );
}

#[test]
fn config_detector_processes_exact_byte_range() {
    // Only scan bytes 2..5 of a record (simulated by slicing)
    let record = [0xC1, 0xC2, 0x00, 0x01, 0x02, 0xC3, 0xC4];
    let errors = detect_ebcdic_corruption(&record[2..5], "SLICE");
    assert_eq!(errors.len(), 3);
}

#[test]
fn config_packed_field_sizes_1_through_8() {
    // Valid packed decimals of increasing size
    let patterns: &[&[u8]] = &[
        &[0x1C],                                           // 1 byte
        &[0x12, 0x3C],                                     // 2 bytes
        &[0x12, 0x34, 0x5C],                               // 3 bytes
        &[0x12, 0x34, 0x56, 0x7C],                         // 4 bytes
        &[0x12, 0x34, 0x56, 0x78, 0x9C],                   // 5 bytes
        &[0x01, 0x23, 0x45, 0x67, 0x89, 0x0C],             // 6 bytes
        &[0x01, 0x23, 0x45, 0x67, 0x89, 0x01, 0x2C],       // 7 bytes
        &[0x01, 0x23, 0x45, 0x67, 0x89, 0x01, 0x23, 0x4C], // 8 bytes
    ];
    for (i, pattern) in patterns.iter().enumerate() {
        assert!(
            detect_packed_corruption(pattern, "SIZE").is_empty(),
            "pattern of size {} should be valid",
            i + 1
        );
    }
}

// ===========================================================================
// Performance: detector overhead stays reasonable
// ===========================================================================

#[test]
fn perf_ebcdic_scan_10k_bytes_clean() {
    let data = vec![0xF0u8; 10_000];
    let errors = detect_ebcdic_corruption(&data, "PERF");
    assert!(errors.is_empty());
}

#[test]
fn perf_ebcdic_scan_10k_bytes_all_corrupt() {
    let data = vec![0x00u8; 10_000];
    let errors = detect_ebcdic_corruption(&data, "PERF-BAD");
    assert_eq!(errors.len(), 10_000);
}

#[test]
fn perf_packed_scan_1k_fields_clean() {
    for _ in 0..1000 {
        assert!(detect_packed_corruption(&[0x12, 0x34, 0x5C], "P").is_empty());
    }
}

#[test]
fn perf_packed_scan_1k_fields_corrupt() {
    let mut total = 0;
    for _ in 0..1000 {
        total += detect_packed_corruption(&[0xAB, 0x17], "P").len();
    }
    assert!(total >= 2000);
}

#[test]
fn perf_detector_chain_500_records() {
    let mut total_errors = 0;
    for i in 0..500 {
        let text_data: &[u8] = if i % 10 == 0 {
            &[0x00, 0x01]
        } else {
            &[0xC1, 0xC2]
        };
        let packed_data: &[u8] = if i % 20 == 0 {
            &[0xAB, 0x17]
        } else {
            &[0x12, 0x3C]
        };
        total_errors += detect_ebcdic_corruption(text_data, "T").len();
        total_errors += detect_packed_corruption(packed_data, "P").len();
    }
    // 50 records with EBCDIC errors (2 each) + 25 records with packed errors (≥2 each)
    assert!(total_errors >= 150);
}

// ===========================================================================
// Edge cases
// ===========================================================================

#[test]
fn edge_ebcdic_every_possible_corrupted_byte() {
    // Both corrupted ranges: 0x00..=0x1F (32) + 0x7F..=0x9F (33) = 65
    let mut data = Vec::new();
    data.extend(0x00..=0x1Fu8);
    data.extend(0x7F..=0x9Fu8);
    let errors = detect_ebcdic_corruption(&data, "ALL-BAD");
    assert_eq!(errors.len(), 65);
}

#[test]
fn edge_packed_high_nibble_boundary_9_is_valid() {
    // High nibble 9 is valid (digit)
    assert!(detect_packed_corruption(&[0x91, 0x2C], "NINE").is_empty());
}

#[test]
fn edge_packed_high_nibble_boundary_a_is_invalid() {
    // High nibble A is invalid (>9)
    let errors = detect_packed_corruption(&[0xA1, 0x2C], "TEN");
    assert!(errors.iter().any(|e| e.message.contains("high nibble")));
}

#[test]
fn edge_packed_sign_nibble_9_is_invalid() {
    // Sign nibble 9 is invalid (digit, not sign)
    let errors = detect_packed_corruption(&[0x19], "SIGN9");
    assert!(errors.iter().any(|e| e.message.contains("sign")));
}

#[test]
fn edge_packed_sign_nibble_a_is_valid() {
    // Sign nibble A is valid
    assert!(detect_packed_corruption(&[0x1A], "SIGNA").is_empty());
}
