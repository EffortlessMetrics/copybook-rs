//! End-to-end integration tests for the copybook-governance façade.
//!
//! Verifies the full contracts → grid → runtime pipeline through the
//! top-level governance crate.

use copybook_governance::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureId, FeatureLifecycle,
    feature_flags_for_support_id, governance_bindings, governance_state_for_support_id,
    governance_states, is_support_runtime_available, runtime_summary, summarize_governance,
    support_states,
};

// ---------------------------------------------------------------------------
// End-to-end: contracts → grid → runtime
// ---------------------------------------------------------------------------

#[test]
fn e2e_default_flags_flow() {
    // contracts layer: build default flags
    let flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::SignSeparate));
    assert!(flags.is_enabled(Feature::Comp1));
    assert!(flags.is_enabled(Feature::Comp2));

    // grid layer: look up bindings
    let bindings = governance_bindings();
    assert_eq!(bindings.len(), 7);
    let comp_flags = feature_flags_for_support_id(FeatureId::Comp1Comp2).unwrap();
    assert_eq!(comp_flags.len(), 2);

    // runtime layer: evaluate state
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(state.runtime_enabled);
    assert!(state.missing_feature_flags.is_empty());

    // runtime summary: everything consistent
    let summary = runtime_summary(&flags);
    assert!(summary.all_support_rows_present());
}

#[test]
fn e2e_disable_flags_propagates_to_runtime() {
    let flags = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();

    // Grid still has the bindings
    let comp_flags = feature_flags_for_support_id(FeatureId::Comp1Comp2).unwrap();
    assert_eq!(comp_flags.len(), 2);

    // Runtime detects missing flags
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 2);

    // Summary reflects the disability
    let summary = runtime_summary(&flags);
    assert!(summary.has_runtime_unavailable_features());
}

#[test]
fn e2e_enable_all_makes_everything_available() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags.enable(f);
    }

    let states = governance_states(&flags);
    for state in &states {
        assert!(
            state.runtime_enabled,
            "{:?} should be enabled",
            state.support_id
        );
    }

    let summary = runtime_summary(&flags);
    assert_eq!(summary.runtime_disabled_features, 0);
    assert!(!summary.has_runtime_unavailable_features());
}

// ---------------------------------------------------------------------------
// Feature support matrix queries
// ---------------------------------------------------------------------------

#[test]
fn support_matrix_all_features_accessible() {
    let features = copybook_governance::support_matrix::all_features();
    assert_eq!(features.len(), 7);
}

#[test]
fn support_matrix_find_by_id_and_string_agree() {
    let pairs = [
        ("level-88", FeatureId::Level88Conditions),
        ("level-66-renames", FeatureId::Level66Renames),
        ("occurs-depending", FeatureId::OccursDepending),
        ("edited-pic", FeatureId::EditedPic),
        ("comp-1-comp-2", FeatureId::Comp1Comp2),
        ("sign-separate", FeatureId::SignSeparate),
        ("nested-odo", FeatureId::NestedOdo),
    ];
    for (name, id) in pairs {
        let by_id = copybook_governance::support_matrix::find_feature_by_id(id).unwrap();
        let by_str = copybook_governance::support_matrix::find_feature(name).unwrap();
        assert_eq!(by_id.id, by_str.id);
        assert_eq!(by_id.name, by_str.name);
        assert_eq!(by_id.status, by_str.status);
    }
}

#[test]
fn support_states_all_enabled_without_governance() {
    let states = support_states();
    assert_eq!(states.len(), 7);
    for state in &states {
        assert!(state.runtime_enabled);
        assert!(state.required_feature_flags.is_empty());
        assert!(state.missing_feature_flags.is_empty());
        assert_eq!(state.rationale, "No runtime governance mapping requested.");
    }
}

#[test]
fn governance_bindings_rationale_nonempty() {
    for binding in governance_bindings() {
        assert!(!binding.rationale.is_empty());
    }
}

// ---------------------------------------------------------------------------
// Consistency between layers
// ---------------------------------------------------------------------------

#[test]
fn grid_summary_and_runtime_summary_totals_agree() {
    let grid = summarize_governance();
    let flags = FeatureFlags::default();
    let rt = runtime_summary(&flags);

    assert_eq!(grid.total_support_features, rt.total_support_features);
    assert_eq!(grid.mapped_support_features, rt.mapped_support_features);
}

#[test]
fn governance_states_count_matches_bindings_count() {
    let flags = FeatureFlags::default();
    let states = governance_states(&flags);
    let bindings = governance_bindings();
    assert_eq!(states.len(), bindings.len());
}

#[test]
fn support_states_count_matches_support_matrix() {
    let states = support_states();
    let matrix = copybook_governance::support_matrix::all_features();
    assert_eq!(states.len(), matrix.len());
}

#[test]
fn runtime_enabled_plus_disabled_equals_mapped() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert_eq!(
        summary.runtime_enabled_features + summary.runtime_disabled_features,
        summary.mapped_support_features
    );
}

#[test]
fn handle_roundtrip_through_facade() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::AdvancedOptimization);
    assert!(handle.is_enabled(Feature::AdvancedOptimization));
    let snap = handle.snapshot();
    assert!(snap.is_enabled(Feature::AdvancedOptimization));

    // Use snapshot for runtime evaluation
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &snap).unwrap();
    assert!(state.runtime_enabled); // Comp1/Comp2 are default-on
}

#[test]
fn lifecycle_and_category_accessible_from_facade() {
    let _ = FeatureLifecycle::Experimental;
    let _ = FeatureLifecycle::Stable;
    let _ = FeatureLifecycle::Deprecated;

    assert_eq!(Feature::Comp1.category(), FeatureCategory::Experimental);
    assert_eq!(Feature::AuditSystem.category(), FeatureCategory::Enterprise);
}

#[test]
fn builder_category_enable_flows_to_runtime() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Experimental)
        .build();

    // RenamesR4R6 is Experimental, so Level66Renames should be available
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));

    // Comp1Comp2 needs Comp1+Comp2, both Experimental
    assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));

    // SignSeparate is also Experimental
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
}

#[test]
fn grid_explicit_bindings_count_is_four() {
    let summary = summarize_governance();
    assert_eq!(summary.explicit_bindings(), 4);
}
