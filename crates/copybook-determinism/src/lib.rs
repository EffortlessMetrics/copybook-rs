#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::missing_inline_in_public_items)]
//! Determinism primitives for repeatable output validation.
//!
//! This crate isolates one responsibility:
//! compare two output byte streams, hash them, and report bounded byte-level differences.
//!
//! Use [`compare_outputs`] to check whether two codec runs produced identical bytes,
//! and [`blake3_hex`] for stable content hashing.

use serde::{Deserialize, Serialize};

/// Default cap used when collecting byte-level differences.
pub const DEFAULT_MAX_DIFFS: usize = 100;

/// Hex-encoded BLAKE3 digest length in characters.
pub const BLAKE3_HEX_LEN: usize = 64;

/// Mode of determinism checking (decode-only, encode-only, or full round-trip).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DeterminismMode {
    /// Check that decoding the same binary data twice produces identical JSON.
    DecodeOnly,
    /// Check that encoding the same JSON twice produces identical binary data.
    EncodeOnly,
    /// Check that decode→encode→decode produces identical JSON.
    RoundTrip,
}

/// Details about a byte difference found during determinism checking.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ByteDiff {
    /// Byte offset where the difference was found.
    pub offset: usize,
    /// Byte value from the first run.
    pub round1_byte: u8,
    /// Byte value from the second run.
    pub round2_byte: u8,
}

/// Result of a determinism check operation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DeterminismResult {
    /// The mode of checking that was performed.
    pub mode: DeterminismMode,
    /// BLAKE3 hash of the first run's output.
    pub round1_hash: String,
    /// BLAKE3 hash of the second run's output.
    pub round2_hash: String,
    /// Whether the two runs produced identical outputs.
    pub is_deterministic: bool,
    /// If non-deterministic, details of the byte differences.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte_differences: Option<Vec<ByteDiff>>,
}

impl DeterminismResult {
    /// Returns true if both runs produced identical outputs.
    #[must_use]
    #[inline]
    pub fn passed(&self) -> bool {
        self.is_deterministic
    }

    /// Returns the number of byte differences found (0 if deterministic).
    #[must_use]
    #[inline]
    pub fn diff_count(&self) -> usize {
        self.byte_differences.as_ref().map_or(0, Vec::len)
    }
}

/// Compute a lowercase hex BLAKE3 hash for a byte slice.
#[must_use]
#[inline]
pub fn blake3_hex(data: &[u8]) -> String {
    blake3::hash(data).to_hex().to_string()
}

/// Compare two byte slices and build a determinism result with default diff limit.
#[must_use]
#[inline]
pub fn compare_outputs(mode: DeterminismMode, round1: &[u8], round2: &[u8]) -> DeterminismResult {
    compare_outputs_with_limit(mode, round1, round2, DEFAULT_MAX_DIFFS)
}

/// Compare two byte slices and build a determinism result with an explicit diff limit.
#[must_use]
pub fn compare_outputs_with_limit(
    mode: DeterminismMode,
    round1: &[u8],
    round2: &[u8],
    max_diffs: usize,
) -> DeterminismResult {
    let hash1 = blake3::hash(round1);
    let hash2 = blake3::hash(round2);
    let is_deterministic = hash1 == hash2;

    DeterminismResult {
        mode,
        round1_hash: hash1.to_hex().to_string(),
        round2_hash: hash2.to_hex().to_string(),
        is_deterministic,
        byte_differences: if is_deterministic {
            None
        } else {
            Some(find_byte_differences_with_limit(round1, round2, max_diffs))
        },
    }
}

/// Find byte-level differences between two slices using [`DEFAULT_MAX_DIFFS`] entries at most.
#[must_use]
#[inline]
pub fn find_byte_differences(round1: &[u8], round2: &[u8]) -> Vec<ByteDiff> {
    find_byte_differences_with_limit(round1, round2, DEFAULT_MAX_DIFFS)
}

/// Find byte-level differences between two slices with an explicit limit.
#[must_use]
pub fn find_byte_differences_with_limit(
    round1: &[u8],
    round2: &[u8],
    max_diffs: usize,
) -> Vec<ByteDiff> {
    if max_diffs == 0 {
        return Vec::new();
    }

    let min_len = round1.len().min(round2.len());
    let max_len = round1.len().max(round2.len());
    let mut diffs = Vec::with_capacity(max_diffs.min(max_len));

    for (offset, (&byte_a, &byte_b)) in round1.iter().zip(round2.iter()).enumerate() {
        if byte_a != byte_b {
            diffs.push(ByteDiff {
                offset,
                round1_byte: byte_a,
                round2_byte: byte_b,
            });
            if diffs.len() >= max_diffs {
                return diffs;
            }
        }
    }

    if round1.len() != round2.len() {
        for offset in min_len..max_len {
            let byte_a = round1.get(offset).copied().unwrap_or(0);
            let byte_b = round2.get(offset).copied().unwrap_or(0);
            diffs.push(ByteDiff {
                offset,
                round1_byte: byte_a,
                round2_byte: byte_b,
            });
            if diffs.len() >= max_diffs {
                return diffs;
            }
        }
    }

    diffs
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn diff_bytes_reports_mismatches() {
        let a = b"ABCDEF";
        let b = b"ABxDEy";
        let diffs = find_byte_differences(a, b);

        assert_eq!(diffs.len(), 2);
        assert_eq!(diffs[0].offset, 2);
        assert_eq!(diffs[0].round1_byte, b'C');
        assert_eq!(diffs[0].round2_byte, b'x');
        assert_eq!(diffs[1].offset, 5);
        assert_eq!(diffs[1].round1_byte, b'F');
        assert_eq!(diffs[1].round2_byte, b'y');
    }

    #[test]
    fn diff_bytes_handles_length_mismatch() {
        let a = b"ABC";
        let b = b"ABCDE";
        let diffs = find_byte_differences(a, b);

        assert_eq!(diffs.len(), 2);
        assert_eq!(diffs[0].offset, 3);
        assert_eq!(diffs[0].round1_byte, 0);
        assert_eq!(diffs[0].round2_byte, b'D');
        assert_eq!(diffs[1].offset, 4);
        assert_eq!(diffs[1].round1_byte, 0);
        assert_eq!(diffs[1].round2_byte, b'E');
    }

    #[test]
    fn diff_bytes_limits_to_max() {
        let a = vec![0u8; 200];
        let b = vec![1u8; 200];
        let diffs = find_byte_differences_with_limit(&a, &b, 37);
        assert_eq!(diffs.len(), 37);
    }

    #[test]
    fn compare_outputs_marks_identical_bytes_deterministic() {
        let bytes = b"{\"k\":\"v\"}";
        let result = compare_outputs(DeterminismMode::DecodeOnly, bytes, bytes);

        assert!(result.is_deterministic);
        assert!(result.byte_differences.is_none());
        assert_eq!(result.round1_hash, result.round2_hash);
        assert_eq!(result.round1_hash.len(), BLAKE3_HEX_LEN);
        assert!(result.round1_hash.chars().all(|c| c.is_ascii_hexdigit()));
    }

    #[test]
    fn compare_outputs_reports_non_determinism() {
        let result = compare_outputs(DeterminismMode::EncodeOnly, b"ABC", b"ABX");
        assert!(!result.is_deterministic);
        assert_ne!(result.round1_hash, result.round2_hash);
        assert_eq!(result.diff_count(), 1);
        assert_eq!(result.byte_differences.as_ref().unwrap()[0].offset, 2);
    }

    #[test]
    fn blake3_hex_empty_input_produces_valid_hash() {
        let hash = blake3_hex(b"");
        assert_eq!(hash.len(), BLAKE3_HEX_LEN);
        assert!(hash.chars().all(|c| c.is_ascii_hexdigit()));
    }

    #[test]
    fn blake3_hex_different_inputs_produce_different_hashes() {
        let h1 = blake3_hex(b"hello");
        let h2 = blake3_hex(b"world");
        assert_ne!(h1, h2);
    }

    #[test]
    fn blake3_hex_identical_inputs_produce_same_hash() {
        let h1 = blake3_hex(b"determinism");
        let h2 = blake3_hex(b"determinism");
        assert_eq!(h1, h2);
    }

    #[test]
    fn compare_outputs_empty_slices_are_deterministic() {
        let result = compare_outputs(DeterminismMode::DecodeOnly, b"", b"");
        assert!(result.passed());
        assert_eq!(result.diff_count(), 0);
        assert!(result.byte_differences.is_none());
        assert_eq!(result.round1_hash, result.round2_hash);
    }

    #[test]
    fn compare_outputs_empty_vs_non_empty_is_non_deterministic() {
        let result = compare_outputs(DeterminismMode::EncodeOnly, b"", b"X");
        assert!(!result.passed());
        assert_eq!(result.diff_count(), 1);
        let diffs = result.byte_differences.as_ref().unwrap();
        assert_eq!(diffs[0].offset, 0);
        assert_eq!(diffs[0].round1_byte, 0);
        assert_eq!(diffs[0].round2_byte, b'X');
    }

    #[test]
    fn compare_outputs_round_trip_mode_sets_mode_field() {
        let result = compare_outputs(DeterminismMode::RoundTrip, b"ABC", b"ABC");
        assert_eq!(result.mode, DeterminismMode::RoundTrip);
        assert!(result.passed());
    }

    #[test]
    fn compare_outputs_with_limit_caps_reported_diffs() {
        let a = vec![0u8; 50];
        let b = vec![1u8; 50];
        let result = compare_outputs_with_limit(DeterminismMode::DecodeOnly, &a, &b, 5);
        assert!(!result.passed());
        assert_eq!(result.diff_count(), 5);
    }

    #[test]
    fn find_byte_differences_identical_inputs_returns_empty() {
        let data = b"identical bytes";
        let diffs = find_byte_differences(data, data);
        assert!(diffs.is_empty());
    }

    #[test]
    fn find_byte_differences_with_limit_zero_returns_empty() {
        let diffs = find_byte_differences_with_limit(b"AAA", b"BBB", 0);
        assert!(diffs.is_empty());
    }

    #[test]
    fn determinism_result_serde_round_trip() {
        let result = DeterminismResult {
            mode: DeterminismMode::EncodeOnly,
            round1_hash: blake3_hex(b"test"),
            round2_hash: blake3_hex(b"test"),
            is_deterministic: true,
            byte_differences: None,
        };
        let json = serde_json::to_string(&result).unwrap();
        let deserialized: DeterminismResult = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized, result);
    }

    proptest! {
        #[test]
        fn prop_identical_inputs_always_deterministic(data in prop::collection::vec(any::<u8>(), 0..512)) {
            let result = compare_outputs(DeterminismMode::RoundTrip, &data, &data);
            prop_assert!(result.passed());
            prop_assert_eq!(result.diff_count(), 0);
        }

        #[test]
        fn prop_diff_count_never_exceeds_limit(
            a in prop::collection::vec(any::<u8>(), 0..256),
            b in prop::collection::vec(any::<u8>(), 0..256),
            limit in 0usize..64usize
        ) {
            let diffs = find_byte_differences_with_limit(&a, &b, limit);
            prop_assert!(diffs.len() <= limit);
        }

        #[test]
        fn prop_hash_is_stable(data in prop::collection::vec(any::<u8>(), 0..512)) {
            let h1 = blake3_hex(&data);
            let h2 = blake3_hex(&data);
            prop_assert_eq!(h1.as_str(), h2.as_str());
            prop_assert_eq!(h1.len(), BLAKE3_HEX_LEN);
        }
    }
}
