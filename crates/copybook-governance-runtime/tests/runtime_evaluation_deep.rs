// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep runtime evaluation tests for governance state resolution.
//!
//! Covers systematic evaluation of all features, dependency satisfaction,
//! governance state invariants, and serialization completeness.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::HashSet;

use copybook_governance_runtime::{
    Feature, FeatureFlags, FeatureGovernanceState, FeatureId, governance_bindings,
    governance_state_for_support_id, governance_states, is_support_runtime_available,
    runtime_summary, summarize_governance,
};

// =========================================================================
// 1. All features evaluate correctly at runtime (systematic)
// =========================================================================

#[test]
fn each_support_id_resolves_to_governance_state_with_default_flags() {
    let flags = FeatureFlags::default();
    let ids = [
        FeatureId::Level88Conditions,
        FeatureId::Level66Renames,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::Comp1Comp2,
        FeatureId::SignSeparate,
        FeatureId::NestedOdo,
    ];
    for id in ids {
        let state = governance_state_for_support_id(id, &flags);
        assert!(state.is_some(), "No governance state for {id:?}");
    }
}

#[test]
fn governance_states_ids_match_governance_bindings_ids() {
    let flags = FeatureFlags::default();
    let state_ids: HashSet<FeatureId> = governance_states(&flags)
        .iter()
        .map(|s| s.support_id)
        .collect();
    let binding_ids: HashSet<FeatureId> =
        governance_bindings().iter().map(|b| b.support_id).collect();
    assert_eq!(state_ids, binding_ids);
}

#[test]
fn governance_states_no_duplicate_ids() {
    let flags = FeatureFlags::default();
    let states = governance_states(&flags);
    let ids: HashSet<FeatureId> = states.iter().map(|s| s.support_id).collect();
    assert_eq!(ids.len(), states.len());
}

// =========================================================================
// 2. Default feature set is correct
// =========================================================================

#[test]
fn default_flags_produce_exactly_six_enabled_and_one_disabled() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    // Level66Renames is disabled (RenamesR4R6 not default-on)
    assert_eq!(summary.runtime_enabled_features, 6);
    assert_eq!(summary.runtime_disabled_features, 1);
}

#[test]
fn default_flags_specific_availability_matches_expectations() {
    let flags = FeatureFlags::default();

    // Default-enabled governed features
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
    assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));

    // Default-disabled governed feature
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));

    // Ungoverned features (no flags required)
    assert!(is_support_runtime_available(
        FeatureId::Level88Conditions,
        &flags
    ));
    assert!(is_support_runtime_available(
        FeatureId::OccursDepending,
        &flags
    ));
    assert!(is_support_runtime_available(FeatureId::EditedPic, &flags));
    assert!(is_support_runtime_available(FeatureId::NestedOdo, &flags));
}

// =========================================================================
// 3. Feature dependencies (multi-flag bindings)
// =========================================================================

#[test]
fn comp1comp2_requires_both_flags_all_four_combinations() {
    let combos = [
        (true, true, true),
        (true, false, false),
        (false, true, false),
        (false, false, false),
    ];
    for (comp1, comp2, expected) in combos {
        let mut flags = FeatureFlags::default();
        if comp1 {
            flags.enable(Feature::Comp1);
        } else {
            flags.disable(Feature::Comp1);
        }
        if comp2 {
            flags.enable(Feature::Comp2);
        } else {
            flags.disable(Feature::Comp2);
        }

        let available = is_support_runtime_available(FeatureId::Comp1Comp2, &flags);
        assert_eq!(
            available, expected,
            "Comp1={comp1}, Comp2={comp2}: expected {expected}, got {available}"
        );
    }
}

#[test]
fn comp1comp2_missing_flags_count_matches_disabled_count() {
    // Only Comp1 disabled
    let flags = FeatureFlags::builder().disable(Feature::Comp1).build();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert_eq!(state.missing_feature_flags.len(), 1);
    assert!(state.missing_feature_flags.contains(&Feature::Comp1));

    // Only Comp2 disabled
    let flags = FeatureFlags::builder().disable(Feature::Comp2).build();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert_eq!(state.missing_feature_flags.len(), 1);
    assert!(state.missing_feature_flags.contains(&Feature::Comp2));

    // Both disabled
    let flags = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert_eq!(state.missing_feature_flags.len(), 2);
}

#[test]
fn single_flag_bindings_toggle_correctly() {
    // SignSeparate
    let flags_on = FeatureFlags::builder()
        .enable(Feature::SignSeparate)
        .build();
    let flags_off = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .build();
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags_on
    ));
    assert!(!is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags_off
    ));

    // RenamesR4R6
    let flags_on = FeatureFlags::builder().enable(Feature::RenamesR4R6).build();
    let flags_off = FeatureFlags::builder()
        .disable(Feature::RenamesR4R6)
        .build();
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags_on
    ));
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags_off
    ));
}

// =========================================================================
// 4. Runtime state matches compile-time configuration
// =========================================================================

#[test]
fn governance_grid_linked_flag_count_matches_runtime_summary() {
    let grid = summarize_governance();
    let flags = FeatureFlags::default();
    let rt = runtime_summary(&flags);

    assert_eq!(
        grid.total_linked_feature_flags,
        rt.total_linked_feature_flags
    );
}

#[test]
fn runtime_enabled_count_equals_states_where_runtime_enabled_is_true() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    let states = governance_states(&flags);
    let count_enabled = states.iter().filter(|s| s.runtime_enabled).count();
    assert_eq!(summary.runtime_enabled_features, count_enabled);
}

#[test]
fn runtime_disabled_count_equals_states_where_runtime_enabled_is_false() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    let states = governance_states(&flags);
    let count_disabled = states.iter().filter(|s| !s.runtime_enabled).count();
    assert_eq!(summary.runtime_disabled_features, count_disabled);
}

// =========================================================================
// 5. Governance state invariants
// =========================================================================

#[test]
fn runtime_enabled_iff_missing_flags_empty_for_all_configs() {
    let configs = [
        FeatureFlags::default(),
        FeatureFlags::builder().enable(Feature::RenamesR4R6).build(),
        FeatureFlags::builder()
            .disable(Feature::Comp1)
            .disable(Feature::SignSeparate)
            .build(),
        {
            let mut f = FeatureFlags::default();
            for feat in copybook_governance_runtime::feature_flags::all_features() {
                f.enable(feat);
            }
            f
        },
        {
            let mut f = FeatureFlags::default();
            for feat in copybook_governance_runtime::feature_flags::all_features() {
                f.disable(feat);
            }
            f
        },
    ];

    for flags in &configs {
        for state in governance_states(flags) {
            assert_eq!(
                state.runtime_enabled,
                state.missing_feature_flags.is_empty(),
                "{:?}: runtime_enabled={} but missing_flags.len()={}",
                state.support_id,
                state.runtime_enabled,
                state.missing_feature_flags.len()
            );
        }
    }
}

#[test]
fn missing_flags_are_subset_of_required_flags() {
    let flags = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::SignSeparate)
        .build();

    for state in governance_states(&flags) {
        let required: HashSet<Feature> = state.required_feature_flags.iter().copied().collect();
        for missing in &state.missing_feature_flags {
            assert!(
                required.contains(missing),
                "{:?}: missing flag {missing:?} not in required set",
                state.support_id
            );
        }
    }
}

#[test]
fn support_status_in_governance_state_matches_support_matrix() {
    let flags = FeatureFlags::default();
    for state in governance_states(&flags) {
        let support =
            copybook_governance_runtime::support_matrix::find_feature_by_id(state.support_id)
                .unwrap();
        assert_eq!(
            state.support_status, support.status,
            "{:?}: status mismatch between governance state and support matrix",
            state.support_id
        );
    }
}

// =========================================================================
// 6. Governance state field access
// =========================================================================

#[test]
fn governance_state_for_enabled_feature_has_expected_fields() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();

    assert!(state.runtime_enabled);
    assert!(state.missing_feature_flags.is_empty());
    assert!(!state.support_name.is_empty());
    assert!(!state.rationale.is_empty());
}

#[test]
fn governance_state_for_disabled_feature_has_missing_flags() {
    let flags = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .build();
    let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();

    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 1);
}

#[test]
fn all_governance_states_have_non_empty_rationale() {
    let flags = FeatureFlags::default();
    for state in governance_states(&flags) {
        assert!(
            !state.rationale.is_empty(),
            "{:?} has empty rationale",
            state.support_id
        );
    }
}

#[test]
fn runtime_summary_field_values_are_consistent() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);

    assert_eq!(
        summary.total_support_features,
        summary.mapped_support_features
    );
    assert_eq!(
        summary.runtime_enabled_features + summary.runtime_disabled_features,
        summary.mapped_support_features
    );
}

// =========================================================================
// 7. FeatureGovernanceState::from_support edge cases
// =========================================================================

#[test]
fn from_support_for_each_feature_preserves_all_metadata() {
    for feature in copybook_governance_runtime::support_matrix::all_features() {
        let state = FeatureGovernanceState::from_support(feature);
        assert_eq!(state.support_id, feature.id);
        assert_eq!(state.support_name, feature.name);
        assert_eq!(state.support_description, feature.description);
        assert_eq!(state.support_status, feature.status);
        assert_eq!(state.doc_ref, feature.doc_ref);
        assert!(state.runtime_enabled);
        assert!(state.required_feature_flags.is_empty());
        assert!(state.missing_feature_flags.is_empty());
    }
}

// =========================================================================
// 8. Summary predicate consistency
// =========================================================================

#[test]
fn all_support_rows_present_is_true_for_current_configuration() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert!(summary.all_support_rows_present());
    assert_eq!(
        summary.total_support_features,
        summary.mapped_support_features
    );
}

#[test]
fn has_runtime_unavailable_iff_disabled_count_positive() {
    let configs = [
        FeatureFlags::default(),
        FeatureFlags::builder().enable(Feature::RenamesR4R6).build(),
        FeatureFlags::builder()
            .disable(Feature::SignSeparate)
            .disable(Feature::Comp1)
            .build(),
    ];

    for flags in &configs {
        let summary = runtime_summary(flags);
        assert_eq!(
            summary.has_runtime_unavailable_features(),
            summary.runtime_disabled_features > 0
        );
    }
}

// =========================================================================
// 9. Increasing disabled flags increases disabled count monotonically
// =========================================================================

#[test]
fn disabling_more_flags_does_not_decrease_disabled_count() {
    let flags_default = FeatureFlags::default();
    let summary_default = runtime_summary(&flags_default);

    let flags_one_more = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .build();
    let summary_one_more = runtime_summary(&flags_one_more);

    let flags_two_more = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    let summary_two_more = runtime_summary(&flags_two_more);

    assert!(
        summary_one_more.runtime_disabled_features >= summary_default.runtime_disabled_features
    );
    assert!(
        summary_two_more.runtime_disabled_features >= summary_one_more.runtime_disabled_features
    );
}
