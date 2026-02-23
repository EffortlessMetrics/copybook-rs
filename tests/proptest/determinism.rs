// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_determinism::{
    BLAKE3_HEX_LEN, DeterminismMode, compare_outputs, compare_outputs_with_limit,
    find_byte_differences_with_limit,
};
use proptest::prelude::*;

proptest! {
    #[test]
    fn identical_buffers_are_deterministic(bytes in prop::collection::vec(any::<u8>(), 0..512)) {
        let result = compare_outputs(DeterminismMode::RoundTrip, &bytes, &bytes);
        prop_assert!(result.is_deterministic);
        prop_assert_eq!(result.diff_count(), 0);
        prop_assert_eq!(result.round1_hash.len(), BLAKE3_HEX_LEN);
        prop_assert_eq!(result.round2_hash.len(), BLAKE3_HEX_LEN);
    }

    #[test]
    fn single_byte_mutation_is_detected(
        bytes in prop::collection::vec(any::<u8>(), 1..256),
        replacement in any::<u8>(),
        idx in 0usize..255usize
    ) {
        let index = idx % bytes.len();
        prop_assume!(bytes[index] != replacement);

        let mut mutated = bytes.clone();
        mutated[index] = replacement;

        let result = compare_outputs(DeterminismMode::DecodeOnly, &bytes, &mutated);
        prop_assert!(!result.is_deterministic);
        prop_assert!(result.diff_count() > 0);
    }

    #[test]
    fn diff_collection_obeys_explicit_limit(
        a in prop::collection::vec(any::<u8>(), 0..300),
        b in prop::collection::vec(any::<u8>(), 0..300),
        limit in 0usize..48usize
    ) {
        let diffs = find_byte_differences_with_limit(&a, &b, limit);
        prop_assert!(diffs.len() <= limit);
    }

    #[test]
    fn compare_with_zero_limit_does_not_collect_diffs(
        a in prop::collection::vec(any::<u8>(), 0..128),
        b in prop::collection::vec(any::<u8>(), 0..128)
    ) {
        prop_assume!(a != b);
        let result = compare_outputs_with_limit(DeterminismMode::EncodeOnly, &a, &b, 0);
        prop_assert!(!result.is_deterministic);
        prop_assert_eq!(result.diff_count(), 0);
    }
}
