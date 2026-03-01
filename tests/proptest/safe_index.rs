// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_safe_index::{safe_divide, safe_slice_get};
use proptest::prelude::*;

use super::config::*;

proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_safe_divide_round_trip(
        numerator in 0usize..1_000_000usize,
        denominator in 1usize..1000usize,
    ) {
        let quotient = safe_divide(numerator, denominator, "prop").expect("divide");
        prop_assert_eq!(quotient, numerator / denominator);
    }

    #[test]
    fn prop_safe_divide_rejects_zero_denominator(numerator in 0usize..1_000_000usize) {
        prop_assert!(safe_divide(numerator, 0, "prop").is_err());
    }

    #[test]
    fn prop_safe_slice_get_round_trip(
        values in prop::collection::vec(0u32..1000u32, 1..200),
        index in 0usize..220usize,
    ) {
        let index = index % (values.len() + 1);
        let result = safe_slice_get(&values, index, "prop");

        if index < values.len() {
            prop_assert_eq!(result.expect("index in bounds"), values[index]);
        } else {
            prop_assert!(result.is_err());
        }
    }
}
