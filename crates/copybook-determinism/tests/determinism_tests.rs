// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Comprehensive tests for copybook-determinism: primitives, stable hashing,
//! diff comparison, record-level determinism, and configuration options.

use copybook_determinism::{
    BLAKE3_HEX_LEN, ByteDiff, DEFAULT_MAX_DIFFS, DeterminismMode, DeterminismResult, blake3_hex,
    compare_outputs, compare_outputs_with_limit, find_byte_differences,
    find_byte_differences_with_limit,
};

// ===========================================================================
// Constants
// ===========================================================================

#[test]
fn blake3_hex_len_is_64() {
    assert_eq!(BLAKE3_HEX_LEN, 64);
}

#[test]
fn default_max_diffs_is_100() {
    assert_eq!(DEFAULT_MAX_DIFFS, 100);
}

// ===========================================================================
// DeterminismMode – traits and serialization
// ===========================================================================

#[test]
fn mode_debug_trait() {
    let mode = DeterminismMode::DecodeOnly;
    let debug = format!("{mode:?}");
    assert!(debug.contains("DecodeOnly"));
}

#[test]
fn mode_clone_and_copy() {
    let mode = DeterminismMode::RoundTrip;
    let cloned = mode;
    assert_eq!(mode, cloned);
}

#[test]
fn mode_equality_all_variants() {
    assert_eq!(DeterminismMode::DecodeOnly, DeterminismMode::DecodeOnly);
    assert_eq!(DeterminismMode::EncodeOnly, DeterminismMode::EncodeOnly);
    assert_eq!(DeterminismMode::RoundTrip, DeterminismMode::RoundTrip);
    assert_ne!(DeterminismMode::DecodeOnly, DeterminismMode::EncodeOnly);
    assert_ne!(DeterminismMode::DecodeOnly, DeterminismMode::RoundTrip);
    assert_ne!(DeterminismMode::EncodeOnly, DeterminismMode::RoundTrip);
}

#[test]
fn mode_serde_round_trip_all_variants() {
    for mode in [
        DeterminismMode::DecodeOnly,
        DeterminismMode::EncodeOnly,
        DeterminismMode::RoundTrip,
    ] {
        let json = serde_json::to_string(&mode).unwrap();
        let deserialized: DeterminismMode = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized, mode);
    }
}

#[test]
fn mode_serde_snake_case_names() {
    assert_eq!(
        serde_json::to_string(&DeterminismMode::DecodeOnly).unwrap(),
        "\"decode_only\""
    );
    assert_eq!(
        serde_json::to_string(&DeterminismMode::EncodeOnly).unwrap(),
        "\"encode_only\""
    );
    assert_eq!(
        serde_json::to_string(&DeterminismMode::RoundTrip).unwrap(),
        "\"round_trip\""
    );
}

// ===========================================================================
// ByteDiff – construction and traits
// ===========================================================================

#[test]
fn byte_diff_construction() {
    let diff = ByteDiff {
        offset: 10,
        round1_byte: 0xAA,
        round2_byte: 0xBB,
    };
    assert_eq!(diff.offset, 10);
    assert_eq!(diff.round1_byte, 0xAA);
    assert_eq!(diff.round2_byte, 0xBB);
}

#[test]
fn byte_diff_equality() {
    let a = ByteDiff {
        offset: 5,
        round1_byte: 1,
        round2_byte: 2,
    };
    let b = ByteDiff {
        offset: 5,
        round1_byte: 1,
        round2_byte: 2,
    };
    let c = ByteDiff {
        offset: 6,
        round1_byte: 1,
        round2_byte: 2,
    };
    assert_eq!(a, b);
    assert_ne!(a, c);
}

#[test]
fn byte_diff_clone() {
    let diff = ByteDiff {
        offset: 3,
        round1_byte: 0x10,
        round2_byte: 0x20,
    };
    let cloned = diff.clone();
    assert_eq!(diff, cloned);
}

#[test]
fn byte_diff_debug() {
    let diff = ByteDiff {
        offset: 0,
        round1_byte: 0,
        round2_byte: 255,
    };
    let debug = format!("{diff:?}");
    assert!(debug.contains("ByteDiff"));
}

#[test]
fn byte_diff_serde_round_trip() {
    let diff = ByteDiff {
        offset: 42,
        round1_byte: 0xDE,
        round2_byte: 0xAD,
    };
    let json = serde_json::to_string(&diff).unwrap();
    let deserialized: ByteDiff = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized, diff);
}

// ===========================================================================
// blake3_hex – stable hash computation
// ===========================================================================

#[test]
fn blake3_hex_empty_input() {
    let hash = blake3_hex(b"");
    assert_eq!(hash.len(), BLAKE3_HEX_LEN);
    assert!(hash.chars().all(|c| c.is_ascii_hexdigit()));
}

#[test]
fn blake3_hex_always_lowercase() {
    let hash = blake3_hex(b"test data");
    assert!(
        hash.chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit())
    );
}

#[test]
fn blake3_hex_identical_inputs_same_hash() {
    let h1 = blake3_hex(b"determinism");
    let h2 = blake3_hex(b"determinism");
    assert_eq!(h1, h2);
}

#[test]
fn blake3_hex_different_inputs_different_hashes() {
    let h1 = blake3_hex(b"hello");
    let h2 = blake3_hex(b"world");
    assert_ne!(h1, h2);
}

#[test]
fn blake3_hex_single_byte_inputs() {
    let h0 = blake3_hex(&[0x00]);
    let h1 = blake3_hex(&[0x01]);
    let hff = blake3_hex(&[0xFF]);
    assert_ne!(h0, h1);
    assert_ne!(h1, hff);
    assert_ne!(h0, hff);
    assert_eq!(h0.len(), BLAKE3_HEX_LEN);
}

#[test]
fn blake3_hex_large_input() {
    let data = vec![0x42_u8; 1_000_000];
    let hash = blake3_hex(&data);
    assert_eq!(hash.len(), BLAKE3_HEX_LEN);
    // Same large input produces same hash
    assert_eq!(blake3_hex(&data), hash);
}

#[test]
fn blake3_hex_incremental_data_differs() {
    let mut hashes = Vec::new();
    for i in 0..10_u8 {
        let data = vec![i; 32];
        hashes.push(blake3_hex(&data));
    }
    // All hashes should be unique
    for i in 0..hashes.len() {
        for j in (i + 1)..hashes.len() {
            assert_ne!(hashes[i], hashes[j], "hash collision at i={i}, j={j}");
        }
    }
}

#[test]
fn blake3_hex_binary_data_with_nulls() {
    let data = vec![0x00; 64];
    let hash = blake3_hex(&data);
    assert_eq!(hash.len(), BLAKE3_HEX_LEN);
    assert_ne!(hash, blake3_hex(b""));
}

// ===========================================================================
// find_byte_differences – diff comparison
// ===========================================================================

#[test]
fn find_diffs_identical_returns_empty() {
    let data = b"identical bytes here";
    let diffs = find_byte_differences(data, data);
    assert!(diffs.is_empty());
}

#[test]
fn find_diffs_single_mismatch() {
    let a = b"ABCDE";
    let b = b"ABxDE";
    let diffs = find_byte_differences(a, b);
    assert_eq!(diffs.len(), 1);
    assert_eq!(diffs[0].offset, 2);
    assert_eq!(diffs[0].round1_byte, b'C');
    assert_eq!(diffs[0].round2_byte, b'x');
}

#[test]
fn find_diffs_multiple_mismatches() {
    let a = b"ABCDEF";
    let b = b"xBCDEy";
    let diffs = find_byte_differences(a, b);
    assert_eq!(diffs.len(), 2);
    assert_eq!(diffs[0].offset, 0);
    assert_eq!(diffs[1].offset, 5);
}

#[test]
fn find_diffs_length_mismatch_round1_shorter() {
    let a = b"AB";
    let b = b"ABCD";
    let diffs = find_byte_differences(a, b);
    assert_eq!(diffs.len(), 2);
    assert_eq!(diffs[0].offset, 2);
    assert_eq!(diffs[0].round1_byte, 0); // padding for shorter
    assert_eq!(diffs[0].round2_byte, b'C');
    assert_eq!(diffs[1].offset, 3);
    assert_eq!(diffs[1].round1_byte, 0);
    assert_eq!(diffs[1].round2_byte, b'D');
}

#[test]
fn find_diffs_length_mismatch_round2_shorter() {
    let a = b"ABCD";
    let b = b"AB";
    let diffs = find_byte_differences(a, b);
    assert_eq!(diffs.len(), 2);
    assert_eq!(diffs[0].offset, 2);
    assert_eq!(diffs[0].round1_byte, b'C');
    assert_eq!(diffs[0].round2_byte, 0);
}

#[test]
fn find_diffs_empty_inputs() {
    let diffs = find_byte_differences(b"", b"");
    assert!(diffs.is_empty());
}

#[test]
fn find_diffs_empty_vs_non_empty() {
    let diffs = find_byte_differences(b"", b"XYZ");
    assert_eq!(diffs.len(), 3);
    for (i, diff) in diffs.iter().enumerate() {
        assert_eq!(diff.offset, i);
        assert_eq!(diff.round1_byte, 0);
    }
}

#[test]
fn find_diffs_all_bytes_different() {
    let a = vec![0u8; 10];
    let b = vec![1u8; 10];
    let diffs = find_byte_differences(&a, &b);
    assert_eq!(diffs.len(), 10);
    for (i, diff) in diffs.iter().enumerate() {
        assert_eq!(diff.offset, i);
        assert_eq!(diff.round1_byte, 0);
        assert_eq!(diff.round2_byte, 1);
    }
}

#[test]
fn find_diffs_interleaved_same_different() {
    let a = b"AaBbCc";
    let b = b"AxByCz";
    let diffs = find_byte_differences(a, b);
    assert_eq!(diffs.len(), 3);
    assert_eq!(diffs[0].offset, 1);
    assert_eq!(diffs[1].offset, 3);
    assert_eq!(diffs[2].offset, 5);
}

// ===========================================================================
// find_byte_differences_with_limit – limit behavior
// ===========================================================================

#[test]
fn find_diffs_with_limit_zero_returns_empty() {
    let diffs = find_byte_differences_with_limit(b"AAA", b"BBB", 0);
    assert!(diffs.is_empty());
}

#[test]
fn find_diffs_with_limit_one() {
    let a = vec![0u8; 50];
    let b = vec![1u8; 50];
    let diffs = find_byte_differences_with_limit(&a, &b, 1);
    assert_eq!(diffs.len(), 1);
    assert_eq!(diffs[0].offset, 0);
}

#[test]
fn find_diffs_with_limit_larger_than_actual_diffs() {
    let a = b"ABCDE";
    let b = b"AxCyE";
    let diffs = find_byte_differences_with_limit(a, b, 100);
    assert_eq!(diffs.len(), 2); // only 2 actual diffs
}

#[test]
fn find_diffs_with_limit_exact_match() {
    let a = vec![0u8; 5];
    let b = vec![1u8; 5];
    let diffs = find_byte_differences_with_limit(&a, &b, 5);
    assert_eq!(diffs.len(), 5);
}

#[test]
fn find_diffs_with_limit_caps_length_mismatch_diffs() {
    let a = b"AB";
    let b = b"ABCDEFGHIJ"; // 8 extra bytes
    let diffs = find_byte_differences_with_limit(a, b, 3);
    assert_eq!(diffs.len(), 3);
}

// ===========================================================================
// compare_outputs – determinism checking
// ===========================================================================

#[test]
fn compare_identical_is_deterministic() {
    let data = b"test output bytes";
    let result = compare_outputs(DeterminismMode::DecodeOnly, data, data);
    assert!(result.is_deterministic);
    assert!(result.passed());
    assert_eq!(result.diff_count(), 0);
    assert!(result.byte_differences.is_none());
    assert_eq!(result.round1_hash, result.round2_hash);
}

#[test]
fn compare_different_is_non_deterministic() {
    let result = compare_outputs(DeterminismMode::EncodeOnly, b"ABC", b"ABX");
    assert!(!result.is_deterministic);
    assert!(!result.passed());
    assert_ne!(result.round1_hash, result.round2_hash);
    assert!(result.byte_differences.is_some());
    assert_eq!(result.diff_count(), 1);
}

#[test]
fn compare_empty_slices_deterministic() {
    let result = compare_outputs(DeterminismMode::RoundTrip, b"", b"");
    assert!(result.passed());
    assert_eq!(result.round1_hash, result.round2_hash);
}

#[test]
fn compare_empty_vs_non_empty() {
    let result = compare_outputs(DeterminismMode::DecodeOnly, b"", b"X");
    assert!(!result.passed());
    assert_eq!(result.diff_count(), 1);
    let diffs = result.byte_differences.as_ref().unwrap();
    assert_eq!(diffs[0].offset, 0);
    assert_eq!(diffs[0].round1_byte, 0);
    assert_eq!(diffs[0].round2_byte, b'X');
}

#[test]
fn compare_preserves_mode_decode_only() {
    let result = compare_outputs(DeterminismMode::DecodeOnly, b"A", b"A");
    assert_eq!(result.mode, DeterminismMode::DecodeOnly);
}

#[test]
fn compare_preserves_mode_encode_only() {
    let result = compare_outputs(DeterminismMode::EncodeOnly, b"A", b"A");
    assert_eq!(result.mode, DeterminismMode::EncodeOnly);
}

#[test]
fn compare_preserves_mode_round_trip() {
    let result = compare_outputs(DeterminismMode::RoundTrip, b"A", b"A");
    assert_eq!(result.mode, DeterminismMode::RoundTrip);
}

#[test]
fn compare_first_byte_different() {
    let result = compare_outputs(DeterminismMode::DecodeOnly, b"XBCDE", b"ABCDE");
    assert!(!result.passed());
    assert_eq!(result.diff_count(), 1);
    assert_eq!(result.byte_differences.as_ref().unwrap()[0].offset, 0);
}

#[test]
fn compare_last_byte_different() {
    let result = compare_outputs(DeterminismMode::DecodeOnly, b"ABCDX", b"ABCDE");
    assert!(!result.passed());
    assert_eq!(result.diff_count(), 1);
    assert_eq!(result.byte_differences.as_ref().unwrap()[0].offset, 4);
}

#[test]
fn compare_binary_data_with_nulls() {
    let a = vec![0x00; 100];
    let b = vec![0x00; 100];
    let result = compare_outputs(DeterminismMode::DecodeOnly, &a, &b);
    assert!(result.passed());
}

#[test]
fn compare_hash_is_valid_hex() {
    let result = compare_outputs(DeterminismMode::DecodeOnly, b"test", b"test");
    assert_eq!(result.round1_hash.len(), BLAKE3_HEX_LEN);
    assert!(result.round1_hash.chars().all(|c| c.is_ascii_hexdigit()));
    assert_eq!(result.round2_hash.len(), BLAKE3_HEX_LEN);
}

// ===========================================================================
// compare_outputs_with_limit – configuration options
// ===========================================================================

#[test]
fn compare_with_limit_caps_diffs() {
    let a = vec![0u8; 50];
    let b = vec![1u8; 50];
    let result = compare_outputs_with_limit(DeterminismMode::DecodeOnly, &a, &b, 5);
    assert!(!result.passed());
    assert_eq!(result.diff_count(), 5);
}

#[test]
fn compare_with_limit_zero_reports_no_diffs() {
    let a = vec![0u8; 10];
    let b = vec![1u8; 10];
    let result = compare_outputs_with_limit(DeterminismMode::DecodeOnly, &a, &b, 0);
    assert!(!result.passed()); // still non-deterministic
    assert_eq!(result.diff_count(), 0); // but no diffs reported
}

#[test]
fn compare_with_limit_larger_than_actual() {
    let result = compare_outputs_with_limit(DeterminismMode::DecodeOnly, b"AB", b"Ax", 1000);
    assert_eq!(result.diff_count(), 1);
}

#[test]
fn compare_with_default_max_diffs() {
    let a = vec![0u8; 200];
    let b = vec![1u8; 200];
    let result = compare_outputs(DeterminismMode::DecodeOnly, &a, &b);
    assert_eq!(result.diff_count(), DEFAULT_MAX_DIFFS);
}

// ===========================================================================
// DeterminismResult – methods and serialization
// ===========================================================================

#[test]
fn result_passed_true_when_deterministic() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: blake3_hex(b"test"),
        round2_hash: blake3_hex(b"test"),
        is_deterministic: true,
        byte_differences: None,
    };
    assert!(result.passed());
    assert_eq!(result.diff_count(), 0);
}

#[test]
fn result_passed_false_when_non_deterministic() {
    let result = DeterminismResult {
        mode: DeterminismMode::EncodeOnly,
        round1_hash: blake3_hex(b"a"),
        round2_hash: blake3_hex(b"b"),
        is_deterministic: false,
        byte_differences: Some(vec![ByteDiff {
            offset: 0,
            round1_byte: b'a',
            round2_byte: b'b',
        }]),
    };
    assert!(!result.passed());
    assert_eq!(result.diff_count(), 1);
}

#[test]
fn result_serde_deterministic_skips_byte_differences() {
    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: blake3_hex(b"x"),
        round2_hash: blake3_hex(b"x"),
        is_deterministic: true,
        byte_differences: None,
    };
    let json = serde_json::to_string(&result).unwrap();
    assert!(
        !json.contains("byte_differences"),
        "None should be skipped via skip_serializing_if"
    );

    let deserialized: DeterminismResult = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized, result);
}

#[test]
fn result_serde_non_deterministic_includes_byte_differences() {
    let result = DeterminismResult {
        mode: DeterminismMode::EncodeOnly,
        round1_hash: blake3_hex(b"a"),
        round2_hash: blake3_hex(b"b"),
        is_deterministic: false,
        byte_differences: Some(vec![
            ByteDiff {
                offset: 0,
                round1_byte: 0xAA,
                round2_byte: 0xBB,
            },
            ByteDiff {
                offset: 5,
                round1_byte: 0xCC,
                round2_byte: 0xDD,
            },
        ]),
    };
    let json = serde_json::to_string(&result).unwrap();
    assert!(json.contains("byte_differences"));

    let deserialized: DeterminismResult = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized, result);
    assert_eq!(deserialized.diff_count(), 2);
}

#[test]
fn result_clone() {
    let result = compare_outputs(DeterminismMode::DecodeOnly, b"test", b"test");
    let cloned = result.clone();
    assert_eq!(result, cloned);
}

#[test]
fn result_debug() {
    let result = compare_outputs(DeterminismMode::DecodeOnly, b"a", b"b");
    let debug = format!("{result:?}");
    assert!(debug.contains("DeterminismResult"));
}

// ===========================================================================
// Record-level determinism – repeated comparisons
// ===========================================================================

#[test]
fn repeated_comparison_same_data_always_deterministic() {
    let data = b"COBOL record data with various fields 1234567890";
    for _ in 0..100 {
        let result = compare_outputs(DeterminismMode::DecodeOnly, data, data);
        assert!(result.passed());
    }
}

#[test]
fn repeated_hashing_is_stable() {
    let data = b"stable hash test input";
    let reference = blake3_hex(data);
    for _ in 0..100 {
        assert_eq!(blake3_hex(data), reference);
    }
}

#[test]
fn compare_outputs_hash_matches_blake3_hex() {
    let data = b"verify hash consistency";
    let result = compare_outputs(DeterminismMode::DecodeOnly, data, data);
    assert_eq!(result.round1_hash, blake3_hex(data));
    assert_eq!(result.round2_hash, blake3_hex(data));
}
