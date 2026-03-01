// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for corruption detection.
//!
//! Verifies that:
//! - Random valid EBCDIC data never triggers false positives above threshold
//! - Known corruption patterns always detected
//! - Detection result is deterministic
//! - Corruption predicate results are bounded and consistent

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_corruption::{detect_ebcdic_corruption, detect_packed_corruption};
use copybook_corruption_predicates::{
    is_invalid_comp3_high_nibble, is_invalid_comp3_low_nibble, is_invalid_comp3_sign_nibble,
    is_likely_corrupted_ebcdic_byte,
};
use proptest::prelude::*;

use super::config::*;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Count expected EBCDIC corruption errors for `data` using predicates.
fn expected_ebcdic_errors(data: &[u8]) -> usize {
    data.iter()
        .filter(|&&b| is_likely_corrupted_ebcdic_byte(b))
        .count()
}

/// Count expected packed-decimal corruption errors for `data` using predicates.
fn expected_packed_errors(data: &[u8]) -> usize {
    data.iter()
        .enumerate()
        .map(|(idx, &byte)| {
            let high = usize::from(is_invalid_comp3_high_nibble(byte));
            if idx + 1 == data.len() {
                high + usize::from(is_invalid_comp3_sign_nibble(byte))
            } else {
                high + usize::from(is_invalid_comp3_low_nibble(byte))
            }
        })
        .sum()
}

/// Compute a "confidence score" as the ratio of corrupted bytes to total bytes.
/// Bounded [0.0, 1.0].
fn ebcdic_confidence(data: &[u8]) -> f64 {
    if data.is_empty() {
        return 0.0;
    }
    let bad = expected_ebcdic_errors(data);
    #[allow(clippy::cast_precision_loss)]
    let score = bad as f64 / data.len() as f64;
    score
}

// ---------------------------------------------------------------------------
// 1. Random EBCDIC data: false-positive rate bounded
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Printable ASCII/EBCDIC bytes (0x20..0x7E) produce zero EBCDIC errors.
    #[test]
    fn prop_valid_printable_no_false_positives(
        data in prop::collection::vec(0x20u8..=0x7E, 1..128),
    ) {
        let errors = detect_ebcdic_corruption(&data, "FIELD");
        prop_assert!(errors.is_empty(), "False positive on printable data");
    }

    /// High-range bytes (0xA0..0xFF) produce zero EBCDIC errors.
    #[test]
    fn prop_valid_high_range_no_false_positives(
        data in prop::collection::vec(0xA0u8..=0xFF, 1..128),
    ) {
        let errors = detect_ebcdic_corruption(&data, "FIELD");
        prop_assert!(errors.is_empty(), "False positive on high-range data");
    }

    /// Mixed valid bytes (printable + high-range) produce zero errors.
    #[test]
    fn prop_valid_mixed_no_false_positives(
        data in prop::collection::vec(
            prop_oneof![0x20u8..=0x7E, 0xA0u8..=0xFF],
            1..128,
        ),
    ) {
        let errors = detect_ebcdic_corruption(&data, "FIELD");
        prop_assert!(errors.is_empty(), "False positive on mixed valid data");
    }
}

// ---------------------------------------------------------------------------
// 2. Known corruption patterns always detected
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// A single NUL (0x00) byte injected into valid data is always detected.
    #[test]
    fn prop_nul_byte_always_detected(
        len in 1usize..64,
        pos in 0usize..64,
    ) {
        let pos = pos % len;
        let mut data = vec![0x41u8; len];
        data[pos] = 0x00;
        let errors = detect_ebcdic_corruption(&data, "F");
        prop_assert!(!errors.is_empty(), "NUL at {} not detected", pos);
    }

    /// Control characters (0x01..0x1F) in otherwise valid data are detected.
    #[test]
    fn prop_control_chars_detected(
        len in 1usize..64,
        pos in 0usize..64,
        ctrl in 0x01u8..=0x1F,
    ) {
        let pos = pos % len;
        let mut data = vec![0x41u8; len];
        data[pos] = ctrl;
        let errors = detect_ebcdic_corruption(&data, "F");
        prop_assert!(!errors.is_empty(), "Control byte 0x{:02X} at {} not detected", ctrl, pos);
    }

    /// Invalid COMP-3 sign nibble is always detected.
    #[test]
    fn prop_packed_invalid_sign_detected(
        len in 1usize..32,
        bad_sign in 0u8..=0x09,
    ) {
        let mut data = vec![0x12u8; len];
        data[len - 1] = (data[len - 1] & 0xF0) | bad_sign;
        let errors = detect_packed_corruption(&data, "F");
        prop_assert!(!errors.is_empty(), "Invalid sign 0x{:X} not detected", bad_sign);
    }

    /// Invalid COMP-3 high nibble (0xA, 0xB, 0xE) is always detected.
    #[test]
    fn prop_packed_invalid_high_nibble_detected(
        len in 1usize..32,
        pos in 0usize..32,
        bad_high in prop_oneof![Just(0x0Au8), Just(0x0Bu8), Just(0x0Eu8)],
    ) {
        let pos = pos % len;
        let mut data = vec![0x12u8; len];
        data[len - 1] = (data[len - 1] & 0xF0) | 0x0C; // valid sign
        data[pos] = (bad_high << 4) | (data[pos] & 0x0F);
        let errors = detect_packed_corruption(&data, "F");
        prop_assert!(!errors.is_empty(), "Invalid high nibble not detected");
    }
}

// ---------------------------------------------------------------------------
// 3. Detection result is deterministic
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// EBCDIC detection gives same result count on 10 repeated calls.
    #[test]
    fn prop_ebcdic_detection_deterministic_10x(
        data in prop::collection::vec(any::<u8>(), 0..128),
    ) {
        let baseline = detect_ebcdic_corruption(&data, "F");
        for i in 1..10 {
            let again = detect_ebcdic_corruption(&data, "F");
            prop_assert_eq!(baseline.len(), again.len(),
                "EBCDIC detection run {} differs: {} vs {}", i, baseline.len(), again.len());
            for (a, b) in baseline.iter().zip(again.iter()) {
                prop_assert_eq!(a.code, b.code, "Error code mismatch on run {}", i);
            }
        }
    }

    /// Packed detection gives same result on 10 repeated calls.
    #[test]
    fn prop_packed_detection_deterministic_10x(
        data in prop::collection::vec(any::<u8>(), 1..64),
    ) {
        let baseline = detect_packed_corruption(&data, "F");
        for i in 1..10 {
            let again = detect_packed_corruption(&data, "F");
            prop_assert_eq!(baseline.len(), again.len(),
                "Packed detection run {} differs: {} vs {}", i, baseline.len(), again.len());
            for (a, b) in baseline.iter().zip(again.iter()) {
                prop_assert_eq!(a.code, b.code, "Error code mismatch on run {}", i);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// 4. Confidence score is bounded [0.0, 1.0]
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// EBCDIC confidence score is always in [0.0, 1.0].
    #[test]
    fn prop_ebcdic_confidence_bounded(data in prop::collection::vec(any::<u8>(), 0..256)) {
        let score = ebcdic_confidence(&data);
        prop_assert!(score >= 0.0 && score <= 1.0,
            "Confidence {} out of bounds for data len {}", score, data.len());
    }

    /// All-valid data yields confidence == 0.0.
    #[test]
    fn prop_ebcdic_confidence_zero_for_valid(
        data in prop::collection::vec(0x20u8..=0x7E, 1..128),
    ) {
        let score = ebcdic_confidence(&data);
        prop_assert!((score - 0.0).abs() < f64::EPSILON,
            "Expected 0.0 confidence for valid data, got {}", score);
    }

    /// All-corrupt data yields confidence == 1.0.
    #[test]
    fn prop_ebcdic_confidence_one_for_corrupt(
        data in prop::collection::vec(0x00u8..=0x1F, 1..128),
    ) {
        let score = ebcdic_confidence(&data);
        prop_assert!((score - 1.0).abs() < f64::EPSILON,
            "Expected 1.0 confidence for fully corrupt data, got {}", score);
    }

    /// Predicates and detectors agree on error count.
    #[test]
    fn prop_predicate_detector_agreement(data in prop::collection::vec(any::<u8>(), 0..128)) {
        let expected = expected_ebcdic_errors(&data);
        let actual = detect_ebcdic_corruption(&data, "F").len();
        prop_assert_eq!(expected, actual,
            "Predicate count {} != detector count {}", expected, actual);
    }

    /// Packed predicates and detector agree on error count.
    #[test]
    fn prop_packed_predicate_detector_agreement(
        data in prop::collection::vec(any::<u8>(), 1..64),
    ) {
        let expected = expected_packed_errors(&data);
        let actual = detect_packed_corruption(&data, "F").len();
        prop_assert_eq!(expected, actual,
            "Packed predicate count {} != detector count {}", expected, actual);
    }
}
