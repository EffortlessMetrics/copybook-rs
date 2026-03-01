// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for corruption detection.
//!
//! Verifies that valid records are never flagged, corrupted records are detected
//! consistently, and detection is deterministic.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_corruption::{detect_ebcdic_corruption, detect_packed_corruption};
use copybook_corruption_predicates::{
    is_invalid_comp3_high_nibble, is_invalid_comp3_sign_nibble, is_likely_corrupted_ebcdic_byte,
};
use proptest::prelude::*;

use super::config::*;

// ---------------------------------------------------------------------------
// Valid records never flagged as corrupt
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Printable ASCII/EBCDIC bytes are never flagged by EBCDIC corruption detector.
    #[test]
    fn prop_valid_printable_bytes_not_flagged(
        data in prop::collection::vec(0x20u8..=0x7E, 1..128),
    ) {
        let errors = detect_ebcdic_corruption(&data, "TEST-FIELD");
        prop_assert!(errors.is_empty(),
            "Printable bytes should not be flagged as corrupt");
    }

    /// Valid COMP-3 packed decimal is not flagged.
    #[test]
    fn prop_valid_comp3_not_flagged(
        digit_pairs in prop::collection::vec(0u8..=0x09, 1..16),
        sign in prop_oneof![Just(0x0Cu8), Just(0x0Du8), Just(0x0Fu8)],
    ) {
        // Build valid packed data: each byte has two valid digit nibbles,
        // last byte has a digit + valid sign
        let mut data = Vec::with_capacity(digit_pairs.len());
        for i in 0..digit_pairs.len() {
            if i + 1 == digit_pairs.len() {
                // Last byte: digit nibble + sign nibble
                data.push((digit_pairs[i] << 4) | sign);
            } else {
                // Interior byte: two digit nibbles
                let next = if i + 1 < digit_pairs.len() { digit_pairs[i + 1] } else { 0 };
                data.push((digit_pairs[i] << 4) | next);
            }
        }
        // Ensure we have at least one byte for valid packed decimal
        if data.is_empty() {
            data.push(sign);
        }

        let errors = detect_packed_corruption(&data, "TEST-FIELD");
        // With properly constructed data, there should be no corruption
        // (the digit_pairs strategy ensures 0-9 nibbles)
        prop_assert!(errors.is_empty(),
            "Valid packed decimal should not be flagged: {:?}", errors);
    }
}

// ---------------------------------------------------------------------------
// Corrupted records detected consistently
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// A control byte injected into data is always detected.
    #[test]
    fn prop_control_byte_always_detected(
        len in 1usize..64,
        pos in 0usize..64,
        control in 0x00u8..=0x1F,
    ) {
        let pos = pos % len;
        let mut data = vec![0x41u8; len]; // 'A' in ASCII — non-corrupt
        data[pos] = control;

        let errors = detect_ebcdic_corruption(&data, "TEST-FIELD");
        prop_assert!(!errors.is_empty(),
            "Control byte 0x{:02X} at position {} should be detected", control, pos);
    }

    /// An invalid high nibble in packed decimal is always detected.
    #[test]
    fn prop_invalid_high_nibble_detected(
        len in 1usize..32,
        pos in 0usize..32,
        invalid_high in prop_oneof![Just(0x0Au8), Just(0x0Bu8), Just(0x0Eu8)],
    ) {
        let pos = pos % len;
        // Start with valid packed decimal (digit nibbles + valid sign on last byte)
        let mut data = vec![0x12u8; len];
        data[len - 1] = (data[len - 1] & 0xF0) | 0x0C; // valid sign

        // Inject invalid high nibble
        data[pos] = (invalid_high << 4) | (data[pos] & 0x0F);

        let errors = detect_packed_corruption(&data, "TEST-FIELD");
        prop_assert!(!errors.is_empty(),
            "Invalid high nibble 0x{:X} should be detected", invalid_high);
    }

    /// An invalid sign nibble is always detected.
    #[test]
    fn prop_invalid_sign_nibble_detected(
        len in 1usize..32,
        invalid_sign in 0u8..=0x09,
    ) {
        let mut data = vec![0x12u8; len];
        // Set last byte with valid high digit but invalid sign
        data[len - 1] = (data[len - 1] & 0xF0) | invalid_sign;

        let errors = detect_packed_corruption(&data, "TEST-FIELD");
        prop_assert!(!errors.is_empty(),
            "Invalid sign nibble 0x{:X} should be detected", invalid_sign);
    }
}

// ---------------------------------------------------------------------------
// Detection is deterministic
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// EBCDIC corruption detection yields identical results on repeated calls.
    #[test]
    fn prop_ebcdic_detection_deterministic(data in prop::collection::vec(any::<u8>(), 0..128)) {
        let first = detect_ebcdic_corruption(&data, "F");
        let second = detect_ebcdic_corruption(&data, "F");
        prop_assert_eq!(first.len(), second.len());
        for (a, b) in first.iter().zip(second.iter()) {
            prop_assert_eq!(a.code, b.code);
        }
    }

    /// Packed corruption detection yields identical results on repeated calls.
    #[test]
    fn prop_packed_detection_deterministic(data in prop::collection::vec(any::<u8>(), 1..64)) {
        let first = detect_packed_corruption(&data, "F");
        let second = detect_packed_corruption(&data, "F");
        prop_assert_eq!(first.len(), second.len());
        for (a, b) in first.iter().zip(second.iter()) {
            prop_assert_eq!(a.code, b.code);
        }
    }

    /// Predicate functions are pure: same input always gives same output.
    #[test]
    fn prop_predicates_are_pure(byte in any::<u8>()) {
        prop_assert_eq!(
            is_likely_corrupted_ebcdic_byte(byte),
            is_likely_corrupted_ebcdic_byte(byte)
        );
        prop_assert_eq!(
            is_invalid_comp3_high_nibble(byte),
            is_invalid_comp3_high_nibble(byte)
        );
        prop_assert_eq!(
            is_invalid_comp3_sign_nibble(byte),
            is_invalid_comp3_sign_nibble(byte)
        );
    }
}
