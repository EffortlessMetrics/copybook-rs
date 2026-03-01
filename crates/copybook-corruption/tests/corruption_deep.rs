// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Deep tests for corruption detection: ASCII transfer patterns, character
//! distribution analysis, multi-pattern detection, false-positive resistance,
//! confidence scoring, corruption report generation, large file simulation,
//! and streaming detection.

use copybook_core::ErrorCode;
use copybook_corruption::{
    detect_ebcdic_corruption, detect_packed_corruption, detect_rdw_ascii_corruption,
};

// ===========================================================================
// ASCII transfer detection — various patterns
// ===========================================================================

#[test]
fn ascii_transfer_full_ascii_text_in_ebcdic_field() {
    // An entire field contains ASCII printable text (0x20–0x7E) instead of
    // EBCDIC.  Only the control-range subset should be flagged; printable ASCII
    // bytes happen to overlap valid EBCDIC ranges (0x20–0x7E).
    let ascii_text = b"HELLO WORLD 12345";
    let errors = detect_ebcdic_corruption(ascii_text, "TEXT-FIELD");
    // ASCII uppercase letters 0x41-0x5A and digits 0x30-0x39 are in the
    // "clean" EBCDIC range (0x20-0x7E), so no flags expected.
    assert!(
        errors.is_empty(),
        "Printable ASCII overlaps valid EBCDIC range"
    );
}

#[test]
fn ascii_transfer_cr_lf_line_endings() {
    // FTP ASCII mode may inject CR (0x0D) and LF (0x0A) into binary data.
    let data = [0xC1, 0x0D, 0x0A, 0xC2];
    let errors = detect_ebcdic_corruption(&data, "LINE-FLD");
    assert_eq!(errors.len(), 2);
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKC301_INVALID_EBCDIC_BYTE)
    );
}

#[test]
fn ascii_transfer_tab_characters_injected() {
    // Tab (0x09) is a common ASCII control character.
    let data = [0xC1, 0x09, 0x09, 0xC3];
    let errors = detect_ebcdic_corruption(&data, "TAB-FLD");
    assert_eq!(errors.len(), 2);
}

#[test]
fn ascii_transfer_nul_padding_corruption() {
    // Entire record NUL-padded (all 0x00) — every byte is corrupt EBCDIC.
    let data = vec![0x00u8; 80];
    let errors = detect_ebcdic_corruption(&data, "NUL-REC");
    assert_eq!(errors.len(), 80);
}

#[test]
fn ascii_transfer_mixed_cr_lf_with_valid_ebcdic() {
    // Interleaved valid EBCDIC and stray CR/LF from line-ending conversion.
    let mut data = Vec::new();
    for _ in 0..10 {
        data.extend_from_slice(&[0xC1, 0xC2, 0x0D, 0x0A]); // EBCDIC AB + CR LF
    }
    let errors = detect_ebcdic_corruption(&data, "MIX-FLD");
    assert_eq!(errors.len(), 20); // 10 x CR + 10 x LF
}

#[test]
fn ascii_transfer_rdw_with_all_ascii_digit_pairs() {
    // Sweep all two-digit ASCII pairs "00" through "99" in length bytes.
    let mut detected_count = 0;
    for d1 in b'0'..=b'9' {
        for d2 in b'0'..=b'9' {
            if detect_rdw_ascii_corruption(&[d1, d2, 0x00, 0x00]).is_some() {
                detected_count += 1;
            }
        }
    }
    assert_eq!(detected_count, 100, "All 100 digit pairs should be flagged");
}

// ===========================================================================
// Character distribution analysis
// ===========================================================================

#[test]
fn distribution_all_ebcdic_uppercase_letters() {
    // Full EBCDIC uppercase: A–I (0xC1–0xC9), J–R (0xD1–0xD9), S–Z (0xE2–0xE9)
    let data: Vec<u8> = (0xC1..=0xC9)
        .chain(0xD1..=0xD9)
        .chain(0xE2..=0xE9)
        .collect();
    assert!(detect_ebcdic_corruption(&data, "UPPER").is_empty());
}

#[test]
fn distribution_all_ebcdic_lowercase_letters() {
    // EBCDIC lowercase: a–i (0x81–0x89), j–r (0x91–0x99), s–z (0xA2–0xA9)
    // All of these are outside the corrupted ranges.
    let data: Vec<u8> = (0x81..=0x89)
        .chain(0x91..=0x99)
        .chain(0xA2..=0xA9)
        .collect();
    // None of these are in 0x00-0x1F or 0x7F-0x9F — should all be clean
    // ... except 0x81-0x89 and 0x91-0x99 are in 0x80-0x9F range.
    // 0x80-0x9F: 0x7F is corrupt, 0x80-0x9F is corrupt.
    // Wait, the predicate checks 0x7F-0x9F as corrupt.
    // So 0x81-0x89 are in range 0x7F-0x9F → flagged as corrupt.
    // And 0x91-0x99 are in range 0x7F-0x9F → flagged as corrupt.
    // Only 0xA2-0xA9 are clean.
    let errors = detect_ebcdic_corruption(&data, "LOWER");
    let expected_corrupt = (0x81..=0x89).count() + (0x91..=0x99).count();
    assert_eq!(errors.len(), expected_corrupt);
}

#[test]
fn distribution_all_ebcdic_digits() {
    // EBCDIC digits 0–9 = 0xF0–0xF9
    let data: Vec<u8> = (0xF0..=0xF9).collect();
    assert!(detect_ebcdic_corruption(&data, "DIGITS").is_empty());
}

#[test]
fn distribution_ebcdic_space_only() {
    // EBCDIC space = 0x40 — valid byte
    let data = vec![0x40u8; 100];
    assert!(detect_ebcdic_corruption(&data, "SPACES").is_empty());
}

#[test]
fn distribution_high_byte_range_0xa0_to_0xff_all_clean() {
    // 0xA0–0xFF should all be considered clean EBCDIC.
    let data: Vec<u8> = (0xA0..=0xFF).collect();
    assert!(detect_ebcdic_corruption(&data, "HIGH").is_empty());
}

#[test]
fn distribution_packed_all_zero_digits_positive() {
    // +00000 = [0x00, 0x00, 0x0C]
    assert!(detect_packed_corruption(&[0x00, 0x00, 0x0C], "ZERO-AMT").is_empty());
}

#[test]
fn distribution_packed_all_nine_digits() {
    // +999999999 = [0x99, 0x99, 0x99, 0x99, 0x9C]
    assert!(detect_packed_corruption(&[0x99, 0x99, 0x99, 0x99, 0x9C], "MAX-AMT").is_empty());
}

// ===========================================================================
// Multi-pattern corruption detection
// ===========================================================================

#[test]
fn multi_pattern_ebcdic_plus_packed_corruption() {
    let ebcdic_errs = detect_ebcdic_corruption(&[0x00, 0x01, 0x0F], "FLD-A");
    let packed_errs = detect_packed_corruption(&[0xAB, 0x17], "FLD-B");
    let total = ebcdic_errs.len() + packed_errs.len();
    assert!(
        total >= 4,
        "Should detect errors in both fields, got {total}"
    );
}

#[test]
fn multi_pattern_rdw_plus_ebcdic_corruption() {
    let rdw_err = detect_rdw_ascii_corruption(&[b'4', b'2', 0x00, 0x00]);
    let ebcdic_errs = detect_ebcdic_corruption(&[0x00, 0x7F], "FLD-X");
    assert!(rdw_err.is_some());
    assert_eq!(ebcdic_errs.len(), 2);
}

#[test]
fn multi_pattern_all_three_detectors_fire_simultaneously() {
    let rdw = detect_rdw_ascii_corruption(&[b'8', b'0', b'A', b'B']);
    let ebcdic = detect_ebcdic_corruption(&[0x01, 0x02, 0x03], "TEXT");
    let packed = detect_packed_corruption(&[0xAA, 0xBB, 0x17], "COMP3");

    assert!(rdw.is_some());
    assert_eq!(ebcdic.len(), 3);
    assert!(packed.len() >= 3);
}

#[test]
fn multi_pattern_corruption_across_many_fields() {
    // Simulate scanning 10 fields, some clean, some corrupt.
    let fields: Vec<(&[u8], &str, bool)> = vec![
        (&[0xC1, 0xC2, 0xC3], "CLEAN-1", true),
        (&[0x00, 0xC2], "BAD-1", false),
        (&[0xF0, 0xF1, 0xF2], "CLEAN-2", true),
        (&[0x0A, 0x0D], "BAD-2", false),
        (&[0xC4, 0xC5], "CLEAN-3", true),
        (&[0x7F, 0x80], "BAD-3", false),
        (&[0xD1, 0xD2], "CLEAN-4", true),
        (&[0x01, 0x02, 0x03], "BAD-4", false),
        (&[0xE2, 0xE3], "CLEAN-5", true),
        (&[0x00], "BAD-5", false),
    ];
    for (data, path, expect_clean) in fields {
        let errors = detect_ebcdic_corruption(data, path);
        assert_eq!(
            errors.is_empty(),
            expect_clean,
            "Field {path}: expected clean={expect_clean}, errors={errors:?}"
        );
    }
}

// ===========================================================================
// Combining multiple detection signals
// ===========================================================================

/// Helper: aggregate corruption errors from a simulated record.
fn scan_record(
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
fn combined_clean_record_zero_errors() {
    let errors = scan_record(
        &[0x00, 0x50, 0x00, 0x00],
        &[(&[0xC1, 0xC2], "NAME")],
        &[(&[0x12, 0x3C], "AMT")],
    );
    assert!(errors.is_empty());
}

#[test]
fn combined_fully_corrupted_record() {
    let errors = scan_record(
        &[b'8', b'0', b'C', b'D'],
        &[(&[0x00, 0x01], "NAME"), (&[0x7F, 0x80], "ADDR")],
        &[(&[0xAA, 0x17], "BAL")],
    );
    assert!(errors.len() >= 6);
    let codes: Vec<_> = errors.iter().map(|e| e.code).collect();
    assert!(codes.contains(&ErrorCode::CBKF104_RDW_SUSPECT_ASCII));
    assert!(codes.contains(&ErrorCode::CBKC301_INVALID_EBCDIC_BYTE));
    assert!(codes.contains(&ErrorCode::CBKD401_COMP3_INVALID_NIBBLE));
}

#[test]
fn combined_only_rdw_corrupt_fields_clean() {
    let errors = scan_record(
        &[b'1', b'2', 0x00, 0x00],
        &[(&[0xC1], "F1")],
        &[(&[0x1C], "P1")],
    );
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn combined_only_packed_corrupt() {
    let errors = scan_record(
        &[0x00, 0x50, 0x00, 0x00],
        &[(&[0xC1, 0xC2], "NAME")],
        &[(&[0xAB, 0xCD, 0x17], "BAD-COMP3")],
    );
    assert!(
        errors
            .iter()
            .all(|e| e.code == ErrorCode::CBKD401_COMP3_INVALID_NIBBLE)
    );
    assert!(errors.len() >= 3);
}

// ===========================================================================
// False positive resistance — valid EBCDIC that looks suspicious
// ===========================================================================

#[test]
fn fp_resist_ebcdic_special_chars_not_flagged() {
    // EBCDIC special characters that are valid: . $ @ # ¢ etc.
    // 0x4B = '.', 0x5B = '$', 0x7C = '@', 0x7B = '#', 0x4A = '¢'
    let specials = [0x4B, 0x5B, 0x7C, 0x7B, 0x4A, 0x6B, 0x60, 0x61];
    assert!(detect_ebcdic_corruption(&specials, "SPECIAL").is_empty());
}

#[test]
fn fp_resist_ebcdic_0x40_space_is_clean() {
    let data = vec![0x40u8; 256];
    assert!(detect_ebcdic_corruption(&data, "SPACEFILL").is_empty());
}

#[test]
fn fp_resist_boundary_byte_0x20_not_flagged() {
    // 0x20 is right at the boundary — should be clean.
    assert!(detect_ebcdic_corruption(&[0x20], "BOUNDARY").is_empty());
}

#[test]
fn fp_resist_boundary_byte_0x7e_not_flagged() {
    // 0x7E is the last clean byte before the C1 control range.
    assert!(detect_ebcdic_corruption(&[0x7E], "BOUNDARY").is_empty());
}

#[test]
fn fp_resist_boundary_byte_0xa0_not_flagged() {
    // 0xA0 is the first clean byte after the C1 control range.
    assert!(detect_ebcdic_corruption(&[0xA0], "BOUNDARY").is_empty());
}

#[test]
fn fp_resist_packed_valid_sign_a_b_e() {
    // Sign nibbles A, B, E are also valid per some implementations.
    assert!(detect_packed_corruption(&[0x1A], "SIGN-A").is_empty());
    assert!(detect_packed_corruption(&[0x1B], "SIGN-B").is_empty());
    assert!(detect_packed_corruption(&[0x1E], "SIGN-E").is_empty());
}

#[test]
fn fp_resist_rdw_clean_large_length() {
    // A legitimate large record: length=4096 (0x1000) with clean reserved.
    assert!(detect_rdw_ascii_corruption(&[0x10, 0x00, 0x00, 0x00]).is_none());
}

#[test]
fn fp_resist_rdw_length_just_outside_ascii_range() {
    // 0x2F39 = 12089 — below the suspect 0x3030..0x3939 range.
    assert!(detect_rdw_ascii_corruption(&[0x2F, 0x39, 0x00, 0x00]).is_none());
}

#[test]
fn fp_resist_rdw_length_0x3a3a_above_range() {
    // 0x3A3A = 14906 — above the suspect range.
    assert!(detect_rdw_ascii_corruption(&[0x3A, 0x3A, 0x00, 0x00]).is_none());
}

// ===========================================================================
// Confidence scoring — corruption density as a signal
// ===========================================================================

/// Compute a corruption ratio for an EBCDIC field.
fn ebcdic_corruption_ratio(data: &[u8], field: &str) -> f64 {
    if data.is_empty() {
        return 0.0;
    }
    let errors = detect_ebcdic_corruption(data, field);
    errors.len() as f64 / data.len() as f64
}

#[test]
fn confidence_clean_data_zero_ratio() {
    let ratio = ebcdic_corruption_ratio(&[0xC1, 0xC2, 0xC3, 0xC4], "CLEAN");
    assert!((ratio - 0.0).abs() < f64::EPSILON);
}

#[test]
fn confidence_fully_corrupted_ratio_one() {
    let data: Vec<u8> = (0x00..=0x1F).collect();
    let ratio = ebcdic_corruption_ratio(&data, "FULL-CORRUPT");
    assert!((ratio - 1.0).abs() < f64::EPSILON);
}

#[test]
fn confidence_half_corrupted_ratio() {
    // 4 clean bytes + 4 corrupt bytes
    let data = [0xC1, 0xC2, 0xC3, 0xC4, 0x00, 0x01, 0x02, 0x03];
    let ratio = ebcdic_corruption_ratio(&data, "HALF");
    assert!((ratio - 0.5).abs() < f64::EPSILON);
}

#[test]
fn confidence_sparse_corruption_low_ratio() {
    // 99 clean bytes, 1 corrupt
    let mut data = vec![0xF0u8; 99];
    data.push(0x00);
    let ratio = ebcdic_corruption_ratio(&data, "SPARSE");
    assert!(ratio < 0.02);
    assert!(ratio > 0.0);
}

#[test]
fn confidence_packed_single_error_in_large_field() {
    // 9 valid bytes + 1 invalid sign byte = low corruption density
    let mut data = vec![0x12u8; 9];
    data.push(0x17); // bad sign
    let errors = detect_packed_corruption(&data, "BIG-AMT");
    // Multiple errors possible due to high-nibble checks, but sign is key.
    assert!(!errors.is_empty());
    let ratio = errors.len() as f64 / data.len() as f64;
    assert!(ratio < 0.5, "Corruption ratio should be moderate");
}

// ===========================================================================
// Corruption report generation
// ===========================================================================

/// Build a simple corruption report from a record scan.
fn build_corruption_report(
    record_index: usize,
    rdw: &[u8],
    text_fields: &[(&[u8], &str)],
    packed_fields: &[(&[u8], &str)],
) -> String {
    let errors = scan_record(rdw, text_fields, packed_fields);
    if errors.is_empty() {
        return format!("Record {record_index}: CLEAN");
    }
    let mut report = format!("Record {record_index}: {} error(s)\n", errors.len());
    for e in &errors {
        report.push_str(&format!("  [{:?}] {}\n", e.code, e.message));
    }
    report
}

#[test]
fn report_clean_record_says_clean() {
    let report = build_corruption_report(
        0,
        &[0x00, 0x50, 0x00, 0x00],
        &[(&[0xC1, 0xC2], "NAME")],
        &[(&[0x1C], "AMT")],
    );
    assert!(report.contains("CLEAN"));
}

#[test]
fn report_corrupted_record_lists_errors() {
    let report = build_corruption_report(
        1,
        &[b'8', b'0', 0x00, 0x00],
        &[(&[0x00], "NAME")],
        &[(&[0xAA, 0x17], "AMT")],
    );
    assert!(report.contains("Record 1:"));
    assert!(report.contains("error(s)"));
    assert!(report.contains("CBKF104"));
    assert!(report.contains("CBKC301"));
    assert!(report.contains("CBKD401"));
}

#[test]
fn report_multiple_records_summarized() {
    let reports: Vec<String> = (0..5)
        .map(|i| {
            if i % 2 == 0 {
                build_corruption_report(
                    i,
                    &[0x00, 0x50, 0x00, 0x00],
                    &[(&[0xC1], "F")],
                    &[(&[0x1C], "P")],
                )
            } else {
                build_corruption_report(
                    i,
                    &[b'1', b'2', 0x00, 0x00],
                    &[(&[0x00], "F")],
                    &[(&[0x17], "P")],
                )
            }
        })
        .collect();
    let clean_count = reports.iter().filter(|r| r.contains("CLEAN")).count();
    let corrupt_count = reports.len() - clean_count;
    assert_eq!(clean_count, 3);
    assert_eq!(corrupt_count, 2);
}

// ===========================================================================
// Large file simulation
// ===========================================================================

#[test]
fn large_file_1000_clean_records() {
    for _ in 0..1000 {
        assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
        assert!(detect_ebcdic_corruption(&[0xC1, 0xC2, 0xC3, 0xC4], "F").is_empty());
        assert!(detect_packed_corruption(&[0x12, 0x3C], "P").is_empty());
    }
}

#[test]
fn large_file_1000_records_with_periodic_corruption() {
    let mut corrupt_records = 0;
    for i in 0..1000 {
        if i % 100 == 0 {
            // Every 100th record is corrupted
            assert!(detect_rdw_ascii_corruption(&[b'1', b'2', 0x00, 0x00]).is_some());
            corrupt_records += 1;
        } else {
            assert!(detect_rdw_ascii_corruption(&[0x00, 0x50, 0x00, 0x00]).is_none());
        }
    }
    assert_eq!(corrupt_records, 10);
}

#[test]
fn large_field_64kb_all_clean() {
    let data = vec![0xF0u8; 65536];
    let errors = detect_ebcdic_corruption(&data, "HUGE-FIELD");
    assert!(errors.is_empty());
}

#[test]
fn large_field_64kb_all_corrupt() {
    let data = vec![0x00u8; 65536];
    let errors = detect_ebcdic_corruption(&data, "HUGE-CORRUPT");
    assert_eq!(errors.len(), 65536);
}

#[test]
fn large_packed_field_100_bytes() {
    // 100-byte packed decimal: 99 digit bytes + 1 sign byte
    let mut data = vec![0x12u8; 99];
    data.push(0x3C); // valid sign
    assert!(detect_packed_corruption(&data, "BIG-PACKED").is_empty());
}

// ===========================================================================
// Streaming detection — process chunks independently
// ===========================================================================

/// Simulates streaming: break data into chunks, detect corruption per chunk,
/// merge results.
fn streaming_ebcdic_scan(data: &[u8], chunk_size: usize, field: &str) -> Vec<copybook_core::Error> {
    let mut all_errors = Vec::new();
    for (chunk_idx, chunk) in data.chunks(chunk_size).enumerate() {
        let label = format!("{field}[chunk{chunk_idx}]");
        let mut errors = detect_ebcdic_corruption(chunk, &label);
        // Adjust offsets to be relative to the full data.
        for err in &mut errors {
            if let Some(ctx) = err.context.as_mut() {
                if let Some(offset) = ctx.byte_offset.as_mut() {
                    *offset += (chunk_idx * chunk_size) as u64;
                }
            }
        }
        all_errors.extend(errors);
    }
    all_errors
}

#[test]
fn streaming_single_chunk_matches_full_scan() {
    let data = [0xC1, 0x00, 0xC2, 0x01, 0xC3];
    let full = detect_ebcdic_corruption(&data, "F");
    let streamed = streaming_ebcdic_scan(&data, 1024, "F");
    assert_eq!(full.len(), streamed.len());
}

#[test]
fn streaming_byte_by_byte_same_count() {
    let data = [0x00, 0xC1, 0x01, 0xC2, 0x02];
    let full = detect_ebcdic_corruption(&data, "F");
    let streamed = streaming_ebcdic_scan(&data, 1, "F");
    assert_eq!(full.len(), streamed.len());
}

#[test]
fn streaming_adjusted_offsets_correct() {
    // 3 chunks of 2 bytes: [0xC1, 0x00], [0xC2, 0x01], [0xC3, 0x7F]
    let data = [0xC1, 0x00, 0xC2, 0x01, 0xC3, 0x7F];
    let errors = streaming_ebcdic_scan(&data, 2, "STREAM");
    assert_eq!(errors.len(), 3);
    let offsets: Vec<u64> = errors
        .iter()
        .filter_map(|e| e.context.as_ref().and_then(|c| c.byte_offset))
        .collect();
    assert_eq!(offsets, vec![1, 3, 5]);
}

#[test]
fn streaming_rdw_records_sequential_scan() {
    // Simulate reading RDW headers sequentially from a stream.
    let headers: Vec<[u8; 4]> = vec![
        [0x00, 0x50, 0x00, 0x00], // clean
        [b'8', b'0', 0x00, 0x00], // corrupt
        [0x00, 0x20, 0x00, 0x00], // clean
        [b'4', b'2', 0x00, 0x00], // corrupt
        [0x00, 0xC8, 0x00, 0x00], // clean
    ];
    let corrupt_count = headers
        .iter()
        .filter(|h| detect_rdw_ascii_corruption(h.as_slice()).is_some())
        .count();
    assert_eq!(corrupt_count, 2);
}

#[test]
fn streaming_large_data_chunked_detection() {
    // 1000 bytes: first 100 corrupt (0x00), rest clean (0xF0)
    let mut data = vec![0x00u8; 100];
    data.extend(vec![0xF0u8; 900]);
    let errors = streaming_ebcdic_scan(&data, 64, "BIGSTREAM");
    assert_eq!(errors.len(), 100);
}

// ===========================================================================
// Error context integrity
// ===========================================================================

#[test]
fn error_context_field_path_preserved_across_fields() {
    let paths = ["REC.NAME", "REC.ADDR.LINE1", "REC.GROUP.SUBGROUP.FIELD"];
    for path in paths {
        let errors = detect_ebcdic_corruption(&[0x00], path);
        let field = errors[0]
            .context
            .as_ref()
            .and_then(|c| c.field_path.as_deref());
        assert_eq!(field, Some(path));
    }
}

#[test]
fn error_context_offsets_sequential_in_multi_byte_field() {
    let data = [0x01, 0x02, 0x03, 0x04, 0x05];
    let errors = detect_ebcdic_corruption(&data, "SEQ");
    assert_eq!(errors.len(), 5);
    let offsets: Vec<u64> = errors
        .iter()
        .filter_map(|e| e.context.as_ref().and_then(|c| c.byte_offset))
        .collect();
    assert_eq!(offsets, vec![0, 1, 2, 3, 4]);
}

#[test]
fn packed_error_offset_matches_byte_position() {
    // Byte 0: 0xA0 (high nibble A = invalid), byte 1: 0x1C (valid)
    let errors = detect_packed_corruption(&[0xA0, 0x1C], "OFF-CHK");
    assert!(!errors.is_empty());
    let offset = errors[0].context.as_ref().and_then(|c| c.byte_offset);
    assert_eq!(offset, Some(0));
}
