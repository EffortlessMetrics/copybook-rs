#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_corruption::{
    detect_ebcdic_corruption as detect_ebcdic_corruption_facade,
    detect_packed_corruption as detect_packed_corruption_facade,
    detect_rdw_ascii_corruption as detect_rdw_ascii_corruption_legacy,
};
use copybook_corruption_detectors::{
    detect_ebcdic_corruption as detect_ebcdic_corruption_micro,
    detect_packed_corruption as detect_packed_corruption_micro,
};
use copybook_corruption_rdw::detect_rdw_ascii_corruption;
use copybook_corruption_predicates::{
    is_invalid_comp3_high_nibble, is_invalid_comp3_low_nibble, is_invalid_comp3_sign_nibble,
    is_likely_corrupted_ebcdic_byte,
};
use copybook_core::ErrorCode;
use copybook_rdw_predicates::rdw_is_suspect_ascii_corruption_slice;
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

fn error_codes(errors: &[copybook_core::Error]) -> Vec<copybook_core::ErrorCode> {
    errors.iter().map(|error| error.code).collect()
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_rdw_ascii_corruption_detects_ascii_length_bytes(
        b0 in b'0'..=b'9',
        b1 in b'0'..=b'9',
    ) {
        let header = [b0, b1, 0x00, 0x00];
        let error = detect_rdw_ascii_corruption_legacy(&header)
            .expect("ASCII-like length bytes should be flagged");

        prop_assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }

    #[test]
    fn prop_rdw_ascii_corruption_returns_none_for_short_headers(
        data in vec(any::<u8>(), 0..4),
    ) {
        let error = detect_rdw_ascii_corruption_legacy(&data);
        prop_assert!(error.is_none());
    }

    #[test]
    fn prop_rdw_ascii_legacy_and_microcrate_match(header in prop::collection::vec(any::<u8>(), 4)) {
        let legacy = detect_rdw_ascii_corruption_legacy(&header).is_some();
        let microcrate = detect_rdw_ascii_corruption(&header).is_some();
        prop_assert_eq!(legacy, microcrate);
    }

    #[test]
    fn prop_rdw_ascii_heuristic_slice_and_function_agree(
        b0 in any::<u8>(),
        b1 in any::<u8>(),
        b2 in any::<u8>(),
        b3 in any::<u8>(),
    ) {
        let header = [b0, b1, b2, b3];
        let expected =
            (b'0'..=b'9').contains(&b0) && (b'0'..=b'9').contains(&b1);

        let heuristic = rdw_is_suspect_ascii_corruption_slice(&header);
        let detect =
            detect_rdw_ascii_corruption_legacy(&header).is_some_and(|error| {
                error.message.contains("RDW length field appears to contain ASCII digits")
            });

        prop_assert_eq!(heuristic, expected);
        prop_assert_eq!(detect, expected);
    }

    #[test]
    fn prop_rdw_ascii_corruption_detects_printable_reserved_bytes(
        reserved0 in 0x20u8..=0x7E,
        reserved1 in 0x20u8..=0x7E,
    ) {
        let header = [0x00, 0x05, reserved0, reserved1];
        let error = detect_rdw_ascii_corruption_legacy(&header)
            .expect("Printable reserved bytes should be flagged");

        prop_assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    }

    #[test]
    fn prop_rdw_ascii_corruption_ignores_clean_binary_headers(length in 0u16..=0x3030) {
        let [b0, b1] = length.to_be_bytes();
        prop_assume!(!(b0.is_ascii_digit() && b1.is_ascii_digit()));

        let header = [b0, b1, 0x00, 0x00];
        let error = detect_rdw_ascii_corruption_legacy(&header);
        prop_assert!(error.is_none());
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_ebcdic_corruption_flags_control_bytes(
        len in 1usize..64,
        index in 0usize..64,
        control_byte in prop_oneof![
            (0x00u8..=0x1Fu8),
            (0x7Fu8..=0x9Fu8),
        ],
    ) {
        let mut data = vec![b'A'; len];
        let index = index % len;
        data[index] = control_byte;

        let errors = detect_ebcdic_corruption_facade(&data, "FIELD");
        prop_assert!(!errors.is_empty());
    }

    #[test]
    fn prop_ebcdic_corruption_allows_non_control_bytes(
        data in vec(
            prop_oneof![(0x20u8..=0x7Eu8), (0xA0u8..=0xFFu8)],
            1..64,
        ),
    ) {
        let errors = detect_ebcdic_corruption_facade(&data, "FIELD");
        prop_assert!(errors.is_empty());
    }

    #[test]
    fn prop_ebcdic_predicate_matches_corruption_detector(
        data in vec(any::<u8>(), 0..128),
    ) {
        let expected = data
            .iter()
            .filter(|&&byte| is_likely_corrupted_ebcdic_byte(byte))
            .count();
        let errors = detect_ebcdic_corruption_facade(&data, "FIELD");

        prop_assert_eq!(errors.len(), expected);
    }

    #[test]
    fn prop_ebcdic_corruption_facade_and_detector_match(data in vec(any::<u8>(), 0..128)) {
        let facade = detect_ebcdic_corruption_facade(&data, "FIELD");
        let micro = detect_ebcdic_corruption_micro(&data, "FIELD");

        prop_assert_eq!(error_codes(&facade), error_codes(&micro));
        prop_assert_eq!(facade.len(), micro.len());
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_packed_corruption_detects_invalid_high_nibble(
        len in 1usize..64,
        index in 0usize..64,
        low_nibble in 0u8..=0x0F,
        invalid_high_nibble in prop_oneof![Just(0xAu8), Just(0xBu8), Just(0xEu8)],
    ) {
        let mut data = vec![0x12u8; len];
        data[len - 1] = (data[len - 1] & 0xF0) | 0x0C;

        let index = index % len;
        data[index] = (invalid_high_nibble << 4) | (low_nibble & 0x0F);

        let errors = detect_packed_corruption_facade(&data, "FIELD");
        prop_assert!(!errors.is_empty());
        prop_assert!(
            errors
                .iter()
                .any(|error| error.message.contains("invalid high nibble")),
            "Expected a high-nibble corruption error"
        );
    }

    #[test]
    fn prop_packed_corruption_detects_invalid_sign_nibble(
        len in 1usize..64,
        invalid_sign in 0u8..=0x09,
    ) {
        let mut data = vec![0x12u8; len];
        data[len - 1] = (data[len - 1] & 0xF0) | (invalid_sign & 0x0F);

        let errors = detect_packed_corruption_facade(&data, "FIELD");
        prop_assert!(!errors.is_empty());
        prop_assert!(
            errors
                .iter()
                .any(|error| error.message.contains("sign nibble")),
            "Expected a sign-nibble corruption error"
        );
    }

    #[test]
    fn prop_packed_corruption_returns_none_for_valid_digit_patterns(
        len in 1usize..64,
        high_nibbles in vec(0u8..=0x09, 1..64),
        sign in prop_oneof![Just(0xCu8), Just(0xDu8), Just(0xFu8)],
    ) {
        let mut data = Vec::with_capacity(len);
        for i in 0..len {
            let high = high_nibbles[i % high_nibbles.len()];
            let low = if i + 1 == len {
                sign
            } else {
                high_nibbles[(i + 1) % high_nibbles.len()]
            };

            data.push((high << 4) | low);
        }

        let errors = detect_packed_corruption_facade(&data, "FIELD");
        prop_assert!(errors.is_empty());
    }

    #[test]
    fn prop_packed_predicate_matches_corruption_detector(
        data in vec(any::<u8>(), 1usize..64),
    ) {
        let expected = data
            .iter()
            .enumerate()
            .map(|(idx, &byte)| {
                let high_nibble_invalid = is_invalid_comp3_high_nibble(byte);
                if idx + 1 == data.len() {
                    let sign_invalid = is_invalid_comp3_sign_nibble(byte);
                    usize::from(high_nibble_invalid) + usize::from(sign_invalid)
                } else {
                    let low_nibble_invalid = is_invalid_comp3_low_nibble(byte);
                    usize::from(high_nibble_invalid) + usize::from(low_nibble_invalid)
                }
            })
            .sum::<usize>();

        let errors = detect_packed_corruption_facade(&data, "FIELD");
        prop_assert_eq!(errors.len(), expected);
    }

    #[test]
    fn prop_packed_corruption_facade_and_detector_match(data in vec(any::<u8>(), 1..64)) {
        let facade = detect_packed_corruption_facade(&data, "FIELD");
        let micro = detect_packed_corruption_micro(&data, "FIELD");

        prop_assert_eq!(error_codes(&facade), error_codes(&micro));
        prop_assert_eq!(facade.len(), micro.len());
    }
}
