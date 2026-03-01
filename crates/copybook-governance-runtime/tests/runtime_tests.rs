//! Integration tests for copybook-governance-runtime.

use copybook_governance_runtime::{
    Feature, FeatureFlags, FeatureGovernanceState, FeatureId, governance_state_for_support_id,
    governance_states, is_support_runtime_available, runtime_summary, support_states,
};

// ---------------------------------------------------------------------------
// Runtime state evaluation
// ---------------------------------------------------------------------------

#[test]
fn support_states_returns_all_features() {
    let states = support_states();
    assert_eq!(states.len(), 7);
}

#[test]
fn support_states_are_all_runtime_enabled() {
    for state in support_states() {
        assert!(
            state.runtime_enabled,
            "{:?} should be runtime-enabled in ungoverned mode",
            state.support_id
        );
        assert!(state.required_feature_flags.is_empty());
        assert!(state.missing_feature_flags.is_empty());
    }
}

#[test]
fn from_support_sets_no_governance_rationale() {
    let feature =
        copybook_governance_runtime::support_matrix::find_feature_by_id(FeatureId::EditedPic)
            .unwrap();
    let state = FeatureGovernanceState::from_support(feature);
    assert_eq!(state.rationale, "No runtime governance mapping requested.");
    assert!(state.runtime_enabled);
}

#[test]
fn governance_state_for_supported_feature_with_defaults() {
    let flags = FeatureFlags::default();
    let state =
        governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).expect("should exist");
    assert_eq!(state.support_id, FeatureId::Comp1Comp2);
    assert!(state.runtime_enabled);
    assert_eq!(state.required_feature_flags.len(), 2);
    assert!(state.missing_feature_flags.is_empty());
}

#[test]
fn governance_state_reports_missing_flags() {
    let flags = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    let state =
        governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).expect("should exist");
    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 2);
    assert!(state.missing_feature_flags.contains(&Feature::Comp1));
    assert!(state.missing_feature_flags.contains(&Feature::Comp2));
}

#[test]
fn governance_state_partial_missing_disables_feature() {
    // Disable only Comp1 — Comp1Comp2 requires both
    let flags = FeatureFlags::builder().disable(Feature::Comp1).build();
    let state =
        governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).expect("should exist");
    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 1);
    assert!(state.missing_feature_flags.contains(&Feature::Comp1));
}

#[test]
fn governance_state_preserves_metadata() {
    let flags = FeatureFlags::default();
    let state =
        governance_state_for_support_id(FeatureId::SignSeparate, &flags).expect("should exist");
    assert_eq!(state.support_id, FeatureId::SignSeparate);
    assert!(!state.support_name.is_empty());
    assert!(!state.support_description.is_empty());
    assert!(state.doc_ref.is_some());
    assert!(!state.rationale.is_empty());
}

// ---------------------------------------------------------------------------
// Feature flag checking
// ---------------------------------------------------------------------------

#[test]
fn is_support_runtime_available_with_defaults() {
    let flags = FeatureFlags::default();
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
    assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));
    // Level88 has no flags, always available
    assert!(is_support_runtime_available(
        FeatureId::Level88Conditions,
        &flags
    ));
}

#[test]
fn is_support_runtime_available_disabled_when_flag_off() {
    let flags = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .build();
    assert!(!is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
}

#[test]
fn renames_disabled_by_default() {
    let flags = FeatureFlags::default();
    // RenamesR4R6 is not default-enabled
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
}

#[test]
fn renames_enabled_when_flag_on() {
    let flags = FeatureFlags::builder().enable(Feature::RenamesR4R6).build();
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
}

#[test]
fn ungoverned_features_always_available() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_runtime::feature_flags::all_features() {
        flags.disable(f);
    }
    // Features with no required_feature_flags should still be runtime-enabled
    let ungoverned = [
        FeatureId::Level88Conditions,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::NestedOdo,
    ];
    for id in ungoverned {
        let state = governance_state_for_support_id(id, &flags).unwrap();
        assert!(
            state.runtime_enabled,
            "{id:?} should be available even with all flags off"
        );
    }
}

// ---------------------------------------------------------------------------
// State consistency
// ---------------------------------------------------------------------------

#[test]
fn governance_states_with_all_enabled() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_runtime::feature_flags::all_features() {
        flags.enable(f);
    }
    let states = governance_states(&flags);
    for state in &states {
        assert!(
            state.runtime_enabled,
            "{:?} should be enabled when all flags on",
            state.support_id
        );
        assert!(state.missing_feature_flags.is_empty());
    }
}

#[test]
fn governance_states_with_all_disabled() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_runtime::feature_flags::all_features() {
        flags.disable(f);
    }
    let states = governance_states(&flags);
    for state in &states {
        if !state.required_feature_flags.is_empty() {
            assert!(
                !state.runtime_enabled,
                "{:?} should be disabled when all flags off",
                state.support_id
            );
        }
    }
}

#[test]
fn runtime_summary_counts_with_defaults() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert_eq!(summary.total_support_features, 7);
    assert_eq!(summary.mapped_support_features, 7);
    assert!(summary.all_support_rows_present());
    // RenamesR4R6 is off by default, so at least one disabled
    assert!(summary.has_runtime_unavailable_features());
    assert!(summary.runtime_disabled_features >= 1);
}

#[test]
fn runtime_summary_all_enabled_no_unavailable() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_runtime::feature_flags::all_features() {
        flags.enable(f);
    }
    let summary = runtime_summary(&flags);
    assert!(!summary.has_runtime_unavailable_features());
    assert_eq!(summary.runtime_disabled_features, 0);
    assert_eq!(
        summary.runtime_enabled_features,
        summary.mapped_support_features
    );
}

#[test]
fn runtime_summary_disabled_count_increases_with_more_flags_off() {
    let flags_default = FeatureFlags::default();
    let summary_default = runtime_summary(&flags_default);

    let flags_more_off = FeatureFlags::builder()
        .disable(Feature::SignSeparate)
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();
    let summary_more = runtime_summary(&flags_more_off);

    assert!(
        summary_more.runtime_disabled_features >= summary_default.runtime_disabled_features,
        "disabling more flags should not decrease disabled count"
    );
}

#[test]
fn support_states_count_matches_all_features() {
    let states = support_states();
    let all = copybook_governance_runtime::support_matrix::all_features();
    assert_eq!(states.len(), all.len());
}
