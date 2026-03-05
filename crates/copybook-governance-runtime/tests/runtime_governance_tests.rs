// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for copybook-governance-runtime state evaluation.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance_runtime::{
    Feature, FeatureFlags, FeatureGovernanceState, FeatureId, SupportStatus,
    governance_state_for_support_id, governance_states, is_support_runtime_available,
    runtime_summary, support_states,
};

// ── support_states (no runtime governance) ──────────────────────────────────

#[test]
fn support_states_returns_all_seven() {
    assert_eq!(support_states().len(), 7);
}

#[test]
fn support_states_are_all_runtime_enabled() {
    for state in support_states() {
        assert!(
            state.runtime_enabled,
            "{:?} should be enabled",
            state.support_id
        );
    }
}

#[test]
fn support_states_have_no_required_flags() {
    for state in support_states() {
        assert!(
            state.required_feature_flags.is_empty(),
            "{:?} should have no required flags",
            state.support_id
        );
        assert!(state.missing_feature_flags.is_empty());
    }
}

#[test]
fn support_states_have_non_governance_rationale() {
    for state in support_states() {
        assert_eq!(state.rationale, "No runtime governance mapping requested.");
    }
}

// ── governance_states (with runtime flags) ──────────────────────────────────

#[test]
fn governance_states_with_defaults_returns_seven() {
    let flags = FeatureFlags::default();
    let states = governance_states(&flags);
    assert_eq!(states.len(), 7);
}

#[test]
fn governance_states_default_renames_disabled() {
    let flags = FeatureFlags::default();
    let states = governance_states(&flags);
    let renames = states
        .iter()
        .find(|s| s.support_id == FeatureId::Level66Renames)
        .unwrap();
    assert!(!renames.runtime_enabled);
    assert!(
        renames
            .missing_feature_flags
            .contains(&Feature::RenamesR4R6)
    );
}

#[test]
fn governance_states_all_enabled_means_all_runtime_enabled() {
    let mut flags = FeatureFlags::default();
    for feature in copybook_governance_runtime::feature_flags::all_features() {
        flags.enable(feature);
    }
    for state in governance_states(&flags) {
        assert!(
            state.runtime_enabled,
            "{:?} should be enabled",
            state.support_id
        );
        assert!(state.missing_feature_flags.is_empty());
    }
}

#[test]
fn governance_states_all_disabled_disables_governed_features() {
    let mut flags = FeatureFlags::default();
    for feature in copybook_governance_runtime::feature_flags::all_features() {
        flags.disable(feature);
    }
    let states = governance_states(&flags);
    for state in &states {
        if !state.required_feature_flags.is_empty() {
            assert!(
                !state.runtime_enabled,
                "{:?} should be disabled",
                state.support_id
            );
        }
    }
}

// ── governance_state_for_support_id ─────────────────────────────────────────

#[test]
fn state_for_sign_separate_with_flag_enabled() {
    let flags = FeatureFlags::builder()
        .enable(Feature::SignSeparate)
        .build();
    let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();
    assert!(state.runtime_enabled);
    assert!(state.missing_feature_flags.is_empty());
    assert_eq!(state.required_feature_flags.len(), 1);
}

#[test]
fn state_for_sign_separate_with_flag_disabled() {
    let flags = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .build();
    let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();
    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 1);
    assert!(state.missing_feature_flags.contains(&Feature::SignSeparate));
}

#[test]
fn state_for_comp1comp2_partial_disable() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 1);
    assert!(state.missing_feature_flags.contains(&Feature::Comp2));
}

#[test]
fn state_preserves_support_metadata() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::EditedPic, &flags).unwrap();
    assert_eq!(state.support_id, FeatureId::EditedPic);
    assert_eq!(state.support_status, SupportStatus::Supported);
    assert!(!state.support_name.is_empty());
    assert!(!state.support_description.is_empty());
    assert!(state.doc_ref.is_some());
    assert!(!state.rationale.is_empty());
}

#[test]
fn state_for_ungoverned_feature_has_empty_flags() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::Level88Conditions, &flags).unwrap();
    assert!(state.runtime_enabled);
    assert!(state.required_feature_flags.is_empty());
    assert!(state.missing_feature_flags.is_empty());
}

// ── is_support_runtime_available ────────────────────────────────────────────

#[test]
fn is_available_for_all_defaults() {
    let flags = FeatureFlags::default();
    // Defaults enable SignSeparate, Comp1, Comp2, LruCache
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
    assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));
    assert!(is_support_runtime_available(
        FeatureId::Level88Conditions,
        &flags
    ));
    assert!(is_support_runtime_available(FeatureId::EditedPic, &flags));
    // RenamesR4R6 is NOT default-enabled
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
}

#[test]
fn is_available_becomes_true_when_flag_enabled() {
    let flags = FeatureFlags::builder().enable(Feature::RenamesR4R6).build();
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
}

#[test]
fn is_available_becomes_false_when_flag_disabled() {
    let flags = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .build();
    assert!(!is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
}

// ── FeatureGovernanceState::from_support ─────────────────────────────────────

#[test]
fn from_support_creates_ungoverned_state() {
    let feature =
        copybook_governance_runtime::support_matrix::find_feature_by_id(FeatureId::EditedPic)
            .unwrap();
    let state = FeatureGovernanceState::from_support(feature);
    assert_eq!(state.support_id, FeatureId::EditedPic);
    assert!(state.runtime_enabled);
    assert!(state.required_feature_flags.is_empty());
    assert!(state.missing_feature_flags.is_empty());
    assert_eq!(state.rationale, "No runtime governance mapping requested.");
}

// ── runtime_summary ─────────────────────────────────────────────────────────

#[test]
fn runtime_summary_with_defaults() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert_eq!(summary.total_support_features, 7);
    assert_eq!(summary.mapped_support_features, 7);
    assert!(summary.all_support_rows_present());
    // RenamesR4R6 not enabled by default -> 1 disabled
    assert!(summary.has_runtime_unavailable_features());
    assert_eq!(summary.runtime_disabled_features, 1);
    assert_eq!(summary.runtime_enabled_features, 6);
}

#[test]
fn runtime_summary_all_enabled_has_zero_disabled() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_runtime::feature_flags::all_features() {
        flags.enable(f);
    }
    let summary = runtime_summary(&flags);
    assert_eq!(summary.runtime_disabled_features, 0);
    assert!(!summary.has_runtime_unavailable_features());
    assert_eq!(summary.runtime_enabled_features, 7);
}

#[test]
fn runtime_summary_reflects_disabled_comp_flags() {
    let flags = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .disable(Feature::SignSeparate)
        .build();
    let summary = runtime_summary(&flags);
    // RenamesR4R6 (default off) + Comp1Comp2 + SignSeparate = 3 disabled
    assert_eq!(summary.runtime_disabled_features, 3);
    assert!(summary.has_runtime_unavailable_features());
}

#[test]
fn runtime_summary_enabled_plus_disabled_equals_total_governed() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert_eq!(
        summary.runtime_enabled_features + summary.runtime_disabled_features,
        summary.mapped_support_features
    );
}
