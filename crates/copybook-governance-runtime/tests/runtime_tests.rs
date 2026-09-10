//! Integration tests for copybook-governance-runtime.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance_runtime::{
    Feature, FeatureCategory, FeatureFlags, FeatureGovernanceState, FeatureId, SupportStatus,
    governance_state_for_support_id, governance_states, is_support_runtime_available,
    runtime_summary, support_states,
};

// ── support_states (ungoverned) ─────────────────────────────────────────────

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
fn support_states_have_no_governance_rationale() {
    for state in support_states() {
        assert_eq!(state.rationale, "No runtime governance mapping requested.");
    }
}

#[test]
fn support_states_preserve_support_matrix_ids() {
    let expected_ids = [
        FeatureId::Level88Conditions,
        FeatureId::Level66Renames,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::Comp1Comp2,
        FeatureId::SignSeparate,
        FeatureId::NestedOdo,
    ];
    let state_ids: Vec<_> = support_states().iter().map(|s| s.support_id).collect();
    for id in expected_ids {
        assert!(state_ids.contains(&id), "missing support state for {id:?}");
    }
}

#[test]
fn support_states_count_matches_all_features() {
    let states = support_states();
    let all = copybook_governance_runtime::support_matrix::all_features();
    assert_eq!(states.len(), all.len());
}

// ── FeatureGovernanceState::from_support ─────────────────────────────────────

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
fn from_support_copies_support_metadata_correctly() {
    let feature =
        copybook_governance_runtime::support_matrix::find_feature_by_id(FeatureId::SignSeparate)
            .unwrap();
    let state = FeatureGovernanceState::from_support(feature);
    assert_eq!(state.support_id, FeatureId::SignSeparate);
    assert_eq!(state.support_name, feature.name);
    assert_eq!(state.support_description, feature.description);
    assert_eq!(state.support_status, feature.status);
    assert_eq!(state.doc_ref, feature.doc_ref);
}

#[test]
fn from_support_for_every_feature_has_empty_flags() {
    for feature in copybook_governance_runtime::support_matrix::all_features() {
        let state = FeatureGovernanceState::from_support(feature);
        assert!(state.required_feature_flags.is_empty());
        assert!(state.missing_feature_flags.is_empty());
        assert!(state.runtime_enabled);
    }
}

// ── governance_state_for_support_id ─────────────────────────────────────────

#[test]
fn governance_state_for_supported_feature_with_defaults() {
    let flags = FeatureFlags::default();
    let state =
        governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).expect("should exist");
    assert_eq!(state.support_id, FeatureId::Comp1Comp2);
    assert!(state.runtime_enabled);
    // #656 Phase C: COMP-1/COMP-2 carry no flag linkage.
    assert!(state.required_feature_flags.is_empty());
    assert!(state.missing_feature_flags.is_empty());
}

#[test]
fn governance_state_reports_missing_flags() {
    // #656 Phase C: Level66Renames is the remaining flag-gated binding.
    let flags = FeatureFlags::builder()
        .disable(Feature::RenamesR4R6)
        .build();
    let state =
        governance_state_for_support_id(FeatureId::Level66Renames, &flags).expect("should exist");
    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 1);
    assert!(state.missing_feature_flags.contains(&Feature::RenamesR4R6));
}

#[test]
fn governance_state_stable_entries_ignore_flag_state() {
    // COMP-1/COMP-2 and SIGN SEPARATE stay available in every configuration.
    let mut all_off = FeatureFlags::default();
    for feat in copybook_governance_runtime::feature_flags::all_features() {
        all_off.disable(feat);
    }
    for id in [FeatureId::Comp1Comp2, FeatureId::SignSeparate] {
        let state = governance_state_for_support_id(id, &all_off).expect("should exist");
        assert!(state.runtime_enabled, "{id:?} should stay available");
        assert!(state.required_feature_flags.is_empty());
        assert!(state.missing_feature_flags.is_empty());
    }
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

#[test]
fn governance_state_for_ungoverned_feature_has_empty_flags() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::Level88Conditions, &flags).unwrap();
    assert!(state.runtime_enabled);
    assert!(state.required_feature_flags.is_empty());
    assert!(state.missing_feature_flags.is_empty());
}

#[test]
fn governance_state_renames_disabled_by_default() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::Level66Renames, &flags).unwrap();
    assert!(!state.runtime_enabled);
    assert!(state.missing_feature_flags.contains(&Feature::RenamesR4R6));
}

#[test]
fn governance_state_renames_enabled_when_flag_on() {
    let flags = FeatureFlags::builder().enable(Feature::RenamesR4R6).build();
    let state = governance_state_for_support_id(FeatureId::Level66Renames, &flags).unwrap();
    assert!(state.runtime_enabled);
    assert!(state.missing_feature_flags.is_empty());
}

#[test]
fn governance_state_sign_separate_always_available() {
    // #656 Phase C: SIGN SEPARATE carries no flag linkage.
    let flags = FeatureFlags::builder()
        .disable(Feature::RenamesR4R6)
        .build();
    let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();
    assert!(state.runtime_enabled);
    assert!(state.missing_feature_flags.is_empty());
}

#[test]
fn governance_state_has_non_empty_rationale_for_governed_features() {
    let flags = FeatureFlags::default();
    let governed_ids = [
        FeatureId::Level66Renames,
        FeatureId::Comp1Comp2,
        FeatureId::SignSeparate,
    ];
    for id in governed_ids {
        let state = governance_state_for_support_id(id, &flags).unwrap();
        assert!(
            !state.rationale.is_empty(),
            "{id:?} should have non-empty rationale"
        );
        assert_ne!(
            state.rationale, "No runtime governance mapping requested.",
            "{id:?} should have a governance-specific rationale"
        );
    }
}

// ── is_support_runtime_available ────────────────────────────────────────────

#[test]
fn is_support_runtime_available_with_defaults() {
    let flags = FeatureFlags::default();
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
    assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));
    assert!(is_support_runtime_available(
        FeatureId::Level88Conditions,
        &flags
    ));
}

#[test]
fn is_support_runtime_available_disabled_when_flag_off() {
    // #656 Phase C: Level66Renames is the remaining flag-gated binding.
    let flags = FeatureFlags::builder()
        .disable(Feature::RenamesR4R6)
        .build();
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
}

#[test]
fn renames_disabled_by_default() {
    let flags = FeatureFlags::default();
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
    let ungoverned = [
        FeatureId::Level88Conditions,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::NestedOdo,
    ];
    for id in ungoverned {
        assert!(
            is_support_runtime_available(id, &flags),
            "{id:?} should be available even with all flags off"
        );
    }
}

#[test]
fn comp1comp2_available_regardless_of_other_flags() {
    // #656 Phase C: COMP-1/COMP-2 carry no flag linkage.
    for flags in [
        FeatureFlags::default(),
        FeatureFlags::builder()
            .disable(Feature::RenamesR4R6)
            .build(),
        FeatureFlags::builder().disable(Feature::LruCache).build(),
    ] {
        assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));
    }
}

// ── governance_states (bulk) ────────────────────────────────────────────────

#[test]
fn governance_states_with_all_enabled() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_runtime::feature_flags::all_features() {
        flags.enable(f);
    }
    let states = governance_states(&flags);
    assert_eq!(states.len(), 7);
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
fn governance_states_default_count_matches_bindings() {
    let flags = FeatureFlags::default();
    let states = governance_states(&flags);
    let bindings = copybook_governance_runtime::governance_bindings();
    assert_eq!(states.len(), bindings.len());
}

#[test]
fn governance_states_no_duplicate_support_ids() {
    let flags = FeatureFlags::default();
    let states = governance_states(&flags);
    for (i, a) in states.iter().enumerate() {
        for b in &states[i + 1..] {
            assert_ne!(
                a.support_id, b.support_id,
                "duplicate support_id in governance_states"
            );
        }
    }
}

// ── runtime_summary ─────────────────────────────────────────────────────────

#[test]
fn runtime_summary_counts_with_defaults() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert_eq!(summary.total_support_features, 7);
    assert_eq!(summary.mapped_support_features, 7);
    assert!(summary.all_support_rows_present());
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
fn runtime_summary_disabled_count_drops_when_gated_flag_enabled() {
    // #656 Phase C: Level66Renames is the only flag-gated row. Defaults
    // (RenamesR4R6 off) report 1 disabled; enabling it reports 0.
    let summary_default = runtime_summary(&FeatureFlags::default());
    assert_eq!(summary_default.runtime_disabled_features, 1);

    let flags_on = FeatureFlags::builder().enable(Feature::RenamesR4R6).build();
    let summary_on = runtime_summary(&flags_on);
    assert_eq!(summary_on.runtime_disabled_features, 0);
    assert!(
        summary_on.runtime_disabled_features <= summary_default.runtime_disabled_features,
        "enabling the gated flag should not increase disabled count"
    );
}

#[test]
fn runtime_summary_enabled_plus_disabled_equals_mapped() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert_eq!(
        summary.runtime_enabled_features + summary.runtime_disabled_features,
        summary.mapped_support_features
    );
}

#[test]
fn runtime_summary_with_governed_disabled() {
    // #656 Phase C: only the Level66Renames row is flag-gated.
    let flags = FeatureFlags::builder()
        .disable(Feature::RenamesR4R6)
        .build();
    let summary = runtime_summary(&flags);
    assert_eq!(summary.runtime_disabled_features, 1);
    assert!(summary.has_runtime_unavailable_features());
}

// ── FeatureGovernanceSummary predicates ──────────────────────────────────────

#[test]
fn summary_all_support_rows_present_via_runtime() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    // All 7 support features are mapped in governance grid
    assert!(summary.all_support_rows_present());
    assert_eq!(
        summary.total_support_features,
        summary.mapped_support_features
    );
}

#[test]
fn summary_has_unavailable_when_renames_off() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    // RenamesR4R6 not default-enabled -> at least one disabled
    assert!(summary.has_runtime_unavailable_features());
    assert!(summary.runtime_disabled_features > 0);
}

#[test]
fn summary_no_unavailable_when_all_on() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance_runtime::feature_flags::all_features() {
        flags.enable(f);
    }
    let summary = runtime_summary(&flags);
    assert!(!summary.has_runtime_unavailable_features());
    assert_eq!(summary.runtime_disabled_features, 0);
}

// ── Re-exported type accessibility ──────────────────────────────────────────

#[test]
fn re_exported_feature_flags_builder_works() {
    let flags = FeatureFlags::builder()
        .enable(Feature::RenamesR4R6)
        .disable(Feature::LruCache)
        .build();
    assert!(flags.is_enabled(Feature::RenamesR4R6));
    assert!(!flags.is_enabled(Feature::LruCache));
}

#[test]
fn re_exported_feature_category_accessible() {
    assert_eq!(
        Feature::RenamesR4R6.category(),
        FeatureCategory::Experimental
    );
    assert_eq!(Feature::AuditSystem.category(), FeatureCategory::Enterprise);
}

#[test]
fn re_exported_support_status_accessible() {
    let feature =
        copybook_governance_runtime::support_matrix::find_feature_by_id(FeatureId::NestedOdo)
            .unwrap();
    assert_eq!(feature.status, SupportStatus::Partial);
}
