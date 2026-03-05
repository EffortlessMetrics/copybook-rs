// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for governance contracts.
//!
//! Verifies idempotency, consistency, and determinism of feature flags,
//! support matrix, and governance grid.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_contracts::feature_flags::all_features;
use copybook_contracts::{Feature, FeatureCategory, FeatureFlags};
use copybook_governance_grid::{
    feature_flags_for_support_id, governance_bindings, summarize_governance,
};
use copybook_governance_runtime::{
    governance_states, is_support_runtime_available, runtime_summary,
};
use copybook_support_matrix::{
    FeatureId, all_features as all_support_features, find_feature_by_id,
};
use proptest::prelude::*;

use super::config::*;

/// Strategy that picks a random Feature variant by index.
fn feature_strategy() -> impl Strategy<Value = Feature> {
    let features = all_features();
    (0..features.len()).prop_map(move |i| all_features()[i])
}

/// Strategy that picks a random `FeatureId` variant.
fn feature_id_strategy() -> impl Strategy<Value = FeatureId> {
    prop_oneof![
        Just(FeatureId::Level88Conditions),
        Just(FeatureId::Level66Renames),
        Just(FeatureId::OccursDepending),
        Just(FeatureId::EditedPic),
        Just(FeatureId::Comp1Comp2),
        Just(FeatureId::SignSeparate),
        Just(FeatureId::NestedOdo),
    ]
}

/// Strategy that builds a random `FeatureFlags` with a random subset of features enabled.
fn random_feature_flags() -> impl Strategy<Value = FeatureFlags> {
    prop::collection::vec(any::<bool>(), 22..=22).prop_map(|bits| {
        let features = all_features();
        let mut flags = FeatureFlags::builder();
        for (i, &enabled) in bits.iter().enumerate() {
            if i < features.len() {
                if enabled {
                    flags = flags.enable(features[i]);
                } else {
                    flags = flags.disable(features[i]);
                }
            }
        }
        flags.build()
    })
}

// ---------------------------------------------------------------------------
// Feature flag evaluation is idempotent
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Querying is_enabled twice yields the same result (idempotent read).
    #[test]
    fn prop_feature_flag_query_idempotent(
        flags in random_feature_flags(),
        feature in feature_strategy(),
    ) {
        let first = flags.is_enabled(feature);
        let second = flags.is_enabled(feature);
        prop_assert_eq!(first, second);
    }

    /// Double-toggle restores original state.
    #[test]
    fn prop_double_toggle_restores_state(
        feature in feature_strategy(),
    ) {
        let mut flags = FeatureFlags::default();
        let original = flags.is_enabled(feature);
        flags.toggle(feature);
        flags.toggle(feature);
        prop_assert_eq!(flags.is_enabled(feature), original);
    }

    /// enable then disable leaves the feature disabled.
    #[test]
    fn prop_enable_then_disable(feature in feature_strategy()) {
        let mut flags = FeatureFlags::default();
        flags.enable(feature);
        prop_assert!(flags.is_enabled(feature));
        flags.disable(feature);
        prop_assert!(!flags.is_enabled(feature));
    }
}

// ---------------------------------------------------------------------------
// Support matrix queries are consistent
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Every FeatureId can be found in the support matrix.
    #[test]
    fn prop_support_matrix_lookup_consistent(id in feature_id_strategy()) {
        let result = find_feature_by_id(id);
        prop_assert!(result.is_some(), "FeatureId {:?} should exist in support matrix", id);
        prop_assert_eq!(result.unwrap().id, id);
    }

    /// Support matrix length is constant across calls.
    #[test]
    fn prop_support_matrix_stable_length(_seed in any::<u32>()) {
        let first = all_support_features().len();
        let second = all_support_features().len();
        prop_assert_eq!(first, second);
    }
}

// ---------------------------------------------------------------------------
// Governance grid is deterministic
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// governance_bindings always returns the same count.
    #[test]
    fn prop_governance_bindings_deterministic(_seed in any::<u64>()) {
        let a = governance_bindings();
        let b = governance_bindings();
        prop_assert_eq!(a.len(), b.len());
    }

    /// Every governance binding maps to a valid support matrix entry.
    #[test]
    fn prop_governance_bindings_map_to_support(_seed in any::<u32>()) {
        for binding in governance_bindings() {
            let support = find_feature_by_id(binding.support_id);
            prop_assert!(support.is_some(),
                "Governance binding {:?} must have support matrix entry", binding.support_id);
        }
    }

    /// summarize_governance is deterministic.
    #[test]
    fn prop_governance_summary_deterministic(_seed in any::<u64>()) {
        let a = summarize_governance();
        let b = summarize_governance();
        prop_assert_eq!(a.total_support_features, b.total_support_features);
        prop_assert_eq!(a.mapped_support_features, b.mapped_support_features);
        prop_assert_eq!(a.total_linked_feature_flags, b.total_linked_feature_flags);
    }

    /// Runtime governance: missing_feature_flags is subset of required_feature_flags.
    #[test]
    fn prop_missing_flags_subset_of_required(flags in random_feature_flags()) {
        let states = governance_states(&flags);
        for state in &states {
            for missing in &state.missing_feature_flags {
                prop_assert!(
                    state.required_feature_flags.contains(missing),
                    "Missing flag {:?} should be in required set", missing
                );
            }
        }
    }

    /// Runtime enabled iff no missing flags.
    #[test]
    fn prop_runtime_enabled_iff_no_missing(flags in random_feature_flags()) {
        let states = governance_states(&flags);
        for state in &states {
            prop_assert_eq!(
                state.runtime_enabled,
                state.missing_feature_flags.is_empty(),
                "runtime_enabled should match empty missing_feature_flags"
            );
        }
    }

    /// runtime_summary counts are consistent.
    #[test]
    fn prop_runtime_summary_consistent(flags in random_feature_flags()) {
        let summary = runtime_summary(&flags);
        prop_assert!(summary.mapped_support_features <= summary.total_support_features);
        prop_assert_eq!(
            summary.runtime_enabled_features + summary.runtime_disabled_features,
            governance_states(&flags).len()
        );
    }
}
