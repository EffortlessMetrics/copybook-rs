// SPDX-License-Identifier: AGPL-3.0-or-later
//! Hash stability proofs: BLAKE3 hashing must be perfectly deterministic.
//! Same data → same hash across many iterations; different data → different hash.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_determinism::{
    BLAKE3_HEX_LEN, DeterminismMode, blake3_hex, compare_outputs, compare_outputs_with_limit,
};

// ═══════════════════════════════════════════════════════════════════════════
// 1. Same data → same hash across 100 iterations
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn hash_stability_short_string_100_iterations() {
    let data = b"hello world";
    let baseline = blake3_hex(data);
    for i in 0..100 {
        assert_eq!(blake3_hex(data), baseline, "Hash diverged at iteration {i}");
    }
}

#[test]
fn hash_stability_numeric_data_100_iterations() {
    let data = b"1234567890";
    let baseline = blake3_hex(data);
    for i in 0..100 {
        assert_eq!(blake3_hex(data), baseline, "Hash diverged at iteration {i}");
    }
}

#[test]
fn hash_stability_binary_data_100_iterations() {
    let data: Vec<u8> = (0..=255).collect();
    let baseline = blake3_hex(&data);
    for i in 0..100 {
        assert_eq!(
            blake3_hex(&data),
            baseline,
            "Hash diverged at iteration {i}"
        );
    }
}

#[test]
fn hash_stability_large_payload_100_iterations() {
    let data = vec![0xAB_u8; 4096];
    let baseline = blake3_hex(&data);
    for i in 0..100 {
        assert_eq!(
            blake3_hex(&data),
            baseline,
            "Hash diverged at iteration {i}"
        );
    }
}

#[test]
fn hash_stability_json_like_payload_100_iterations() {
    let data = br#"{"record_index":0,"codepage":"cp037","fields":{"ID":"00042","NAME":"ALICE"}}"#;
    let baseline = blake3_hex(data);
    for i in 0..100 {
        assert_eq!(blake3_hex(data), baseline, "Hash diverged at iteration {i}");
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// 2. Different data → different hash
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn hash_different_data_produces_different_hashes() {
    let h1 = blake3_hex(b"data_a");
    let h2 = blake3_hex(b"data_b");
    assert_ne!(h1, h2, "Different inputs should produce different hashes");
}

#[test]
fn hash_single_bit_difference_produces_different_hash() {
    let data_a = [0x00_u8; 32];
    let mut data_b = [0x00_u8; 32];
    data_b[0] = 0x01;
    assert_ne!(blake3_hex(&data_a), blake3_hex(&data_b));
}

#[test]
fn hash_length_difference_produces_different_hash() {
    let short = b"ABC";
    let long = b"ABCD";
    assert_ne!(blake3_hex(short), blake3_hex(long));
}

// ═══════════════════════════════════════════════════════════════════════════
// 3. Empty data handling
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn hash_empty_data_is_valid() {
    let h = blake3_hex(b"");
    assert_eq!(h.len(), BLAKE3_HEX_LEN);
    assert!(h.chars().all(|c| c.is_ascii_hexdigit()));
}

#[test]
fn hash_empty_data_stable_100_iterations() {
    let baseline = blake3_hex(b"");
    for i in 0..100 {
        assert_eq!(
            blake3_hex(b""),
            baseline,
            "Empty hash diverged at iteration {i}"
        );
    }
}

#[test]
fn hash_empty_differs_from_non_empty() {
    let empty_hash = blake3_hex(b"");
    let non_empty_hash = blake3_hex(b"\0");
    assert_ne!(empty_hash, non_empty_hash);
}

// ═══════════════════════════════════════════════════════════════════════════
// 4. compare_outputs stability
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn compare_identical_data_100_iterations() {
    let data = b"deterministic output bytes";
    for i in 0..100 {
        let result = compare_outputs(DeterminismMode::DecodeOnly, data, data);
        assert!(
            result.is_deterministic,
            "compare_outputs non-deterministic at iteration {i}"
        );
        assert_eq!(result.round1_hash, result.round2_hash);
    }
}

#[test]
fn compare_different_data_always_non_deterministic() {
    let a = b"alpha";
    let b = b"bravo";
    for _ in 0..100 {
        let result = compare_outputs(DeterminismMode::EncodeOnly, a, b);
        assert!(!result.is_deterministic);
        assert_ne!(result.round1_hash, result.round2_hash);
    }
}

#[test]
fn compare_with_limit_stable() {
    let a = vec![0u8; 64];
    let b = vec![1u8; 64];
    let r1 = compare_outputs_with_limit(DeterminismMode::RoundTrip, &a, &b, 10);
    let r2 = compare_outputs_with_limit(DeterminismMode::RoundTrip, &a, &b, 10);
    assert_eq!(r1.round1_hash, r2.round1_hash);
    assert_eq!(r1.round2_hash, r2.round2_hash);
    assert_eq!(r1.diff_count(), r2.diff_count());
}

// ═══════════════════════════════════════════════════════════════════════════
// 5. Hash format correctness
// ═══════════════════════════════════════════════════════════════════════════

#[test]
fn hash_always_64_hex_chars() {
    let inputs: &[&[u8]] = &[b"", b"a", b"ab", b"abc", &vec![0xFF; 1024]];
    for input in inputs {
        let h = blake3_hex(input);
        assert_eq!(
            h.len(),
            BLAKE3_HEX_LEN,
            "Hash length wrong for input len {}",
            input.len()
        );
        assert!(
            h.chars().all(|c| c.is_ascii_hexdigit()),
            "Non-hex char in hash for input len {}",
            input.len()
        );
    }
}

#[test]
fn hash_is_lowercase_hex() {
    let h = blake3_hex(b"test data for case check");
    assert!(
        h.chars()
            .all(|c| c.is_ascii_digit() || ('a'..='f').contains(&c)),
        "Hash should be lowercase hex: {h}"
    );
}
