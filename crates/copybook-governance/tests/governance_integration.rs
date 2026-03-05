//! End-to-end integration tests for the copybook-governance façade.
//!
//! Verifies the full contracts → grid → runtime pipeline through the
//! top-level governance crate.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_governance::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle, FeatureId,
    FeatureLifecycle, SupportStatus, feature_flags_for_support_id, governance_bindings,
    governance_state_for_support_id, governance_states, is_support_runtime_available,
    runtime_summary, summarize_governance, support_states,
};

// ---------------------------------------------------------------------------
// 1. Feature flag enables/disables feature at runtime
// ---------------------------------------------------------------------------

#[test]
fn feature_flag_enable_disables_at_runtime() {
    let mut flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::SignSeparate));
    flags.disable(Feature::SignSeparate);
    assert!(!flags.is_enabled(Feature::SignSeparate));
    flags.enable(Feature::SignSeparate);
    assert!(flags.is_enabled(Feature::SignSeparate));
}

#[test]
fn feature_flag_runtime_toggle_reflects_in_governance_state() {
    let mut flags = FeatureFlags::default();
    flags.disable(Feature::Comp1);
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(!state.runtime_enabled);

    flags.enable(Feature::Comp1);
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(state.runtime_enabled);
}

// ---------------------------------------------------------------------------
// 2. Support matrix reports correct status for each feature
// ---------------------------------------------------------------------------

#[test]
fn support_matrix_reports_correct_status_for_each_feature() {
    let expected = [
        (FeatureId::Level88Conditions, SupportStatus::Supported),
        (FeatureId::Level66Renames, SupportStatus::Partial),
        (FeatureId::OccursDepending, SupportStatus::Partial),
        (FeatureId::EditedPic, SupportStatus::Supported),
        (FeatureId::Comp1Comp2, SupportStatus::Supported),
        (FeatureId::SignSeparate, SupportStatus::Supported),
        (FeatureId::NestedOdo, SupportStatus::Partial),
    ];
    for (id, status) in expected {
        let feature = copybook_governance::support_matrix::find_feature_by_id(id)
            .unwrap_or_else(|| panic!("missing support matrix entry for {id:?}"));
        assert_eq!(feature.status, status, "status mismatch for {id:?}");
    }
}

// ---------------------------------------------------------------------------
// 3. Governance grid binds all features to their flags
// ---------------------------------------------------------------------------

#[test]
fn governance_grid_binds_all_features_to_flags() {
    let bindings = governance_bindings();
    assert_eq!(bindings.len(), 7);

    // Verify specific governed bindings
    let sign_flags = feature_flags_for_support_id(FeatureId::SignSeparate).unwrap();
    assert_eq!(sign_flags, &[Feature::SignSeparate]);

    let comp_flags = feature_flags_for_support_id(FeatureId::Comp1Comp2).unwrap();
    assert_eq!(comp_flags.len(), 2);
    assert!(comp_flags.contains(&Feature::Comp1));
    assert!(comp_flags.contains(&Feature::Comp2));

    let renames_flags = feature_flags_for_support_id(FeatureId::Level66Renames).unwrap();
    assert_eq!(renames_flags, &[Feature::RenamesR4R6]);

    // Verify ungoverned bindings have empty flag slices
    for id in [
        FeatureId::Level88Conditions,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::NestedOdo,
    ] {
        let flags = feature_flags_for_support_id(id).unwrap();
        assert!(flags.is_empty(), "{id:?} should have no feature flags");
    }
}

// ---------------------------------------------------------------------------
// 4. Governance runtime evaluates flags correctly
// ---------------------------------------------------------------------------

#[test]
fn governance_runtime_evaluates_flags_correctly() {
    let flags = FeatureFlags::default();

    // SignSeparate enabled by default → available
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));

    // Comp1+Comp2 enabled by default → available
    assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));

    // RenamesR4R6 NOT enabled by default → unavailable
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));

    // Ungoverned features are always available
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

// ---------------------------------------------------------------------------
// 5. All features in support matrix have corresponding governance binding
// ---------------------------------------------------------------------------

#[test]
fn all_support_matrix_features_have_governance_binding() {
    let matrix = copybook_governance::support_matrix::all_features();
    let binding_ids: Vec<FeatureId> = governance_bindings().iter().map(|b| b.support_id).collect();

    for feature in matrix {
        assert!(
            binding_ids.contains(&feature.id),
            "support matrix feature {:?} has no governance binding",
            feature.id
        );
    }
}

// ---------------------------------------------------------------------------
// 6. All governance bindings reference valid feature IDs
// ---------------------------------------------------------------------------

#[test]
fn all_governance_bindings_reference_valid_feature_ids() {
    for binding in governance_bindings() {
        let found = copybook_governance::support_matrix::find_feature_by_id(binding.support_id);
        assert!(
            found.is_some(),
            "governance binding references unknown support id {:?}",
            binding.support_id
        );
    }
}

// ---------------------------------------------------------------------------
// 7–9. Default-enabled features
// ---------------------------------------------------------------------------

#[test]
fn sign_separate_enabled_by_default() {
    let flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::SignSeparate));
}

#[test]
fn comp1_enabled_by_default() {
    let flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::Comp1));
}

#[test]
fn comp2_enabled_by_default() {
    let flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::Comp2));
}

// ---------------------------------------------------------------------------
// 10. Support matrix serde roundtrip preserves all entries
// ---------------------------------------------------------------------------

#[test]
fn support_matrix_serde_roundtrip_preserves_all_entries() {
    let features = copybook_governance::support_matrix::all_features();
    for feature in features {
        let json = serde_json::to_value(feature).unwrap();
        // Verify serialized fields
        assert!(
            json["id"].is_string(),
            "id should serialize for {:?}",
            feature.id
        );
        assert!(
            json["status"].is_string(),
            "status should serialize for {:?}",
            feature.id
        );
        assert!(json["name"].is_string());
        assert!(json["description"].is_string());

        // Verify FeatureId roundtrips through serde_json
        let id_json = serde_json::to_string(&feature.id).unwrap();
        let back: FeatureId = serde_json::from_str(&id_json).unwrap();
        assert_eq!(
            back, feature.id,
            "serde roundtrip failed for {:?}",
            feature.id
        );
    }
}

// ---------------------------------------------------------------------------
// 11. Governance contracts builder creates valid contracts
// ---------------------------------------------------------------------------

#[test]
fn governance_contracts_builder_creates_valid_contracts() {
    let flags: FeatureFlags = FeatureFlagsBuilder::default()
        .enable(Feature::RenamesR4R6)
        .enable(Feature::Profiling)
        .disable(Feature::LruCache)
        .build();

    assert!(flags.is_enabled(Feature::RenamesR4R6));
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(!flags.is_enabled(Feature::LruCache));
    // Defaults should still be applied for unmentioned features
    assert!(flags.is_enabled(Feature::SignSeparate));
    assert!(flags.is_enabled(Feature::Comp1));
    assert!(flags.is_enabled(Feature::Comp2));
}

// ---------------------------------------------------------------------------
// 12. Feature toggle lifecycle (enable → disable → enable)
// ---------------------------------------------------------------------------

#[test]
fn feature_toggle_lifecycle_enable_disable_enable() {
    let handle = FeatureFlagsHandle::new();

    // Start: AdvancedOptimization is off by default
    assert!(!handle.is_enabled(Feature::AdvancedOptimization));

    // Enable
    handle.enable(Feature::AdvancedOptimization);
    assert!(handle.is_enabled(Feature::AdvancedOptimization));

    // Disable
    handle.disable(Feature::AdvancedOptimization);
    assert!(!handle.is_enabled(Feature::AdvancedOptimization));

    // Re-enable
    handle.enable(Feature::AdvancedOptimization);
    assert!(handle.is_enabled(Feature::AdvancedOptimization));

    // Verify snapshot captures final state
    let snap = handle.snapshot();
    assert!(snap.is_enabled(Feature::AdvancedOptimization));
}

#[test]
fn feature_toggle_lifecycle_via_toggle_method() {
    let mut flags = FeatureFlags::default();
    let initial = flags.is_enabled(Feature::LruCache);
    assert!(initial);

    flags.toggle(Feature::LruCache);
    assert!(!flags.is_enabled(Feature::LruCache));

    flags.toggle(Feature::LruCache);
    assert!(flags.is_enabled(Feature::LruCache));
}

// ---------------------------------------------------------------------------
// 13. Grid completeness check (all features mapped)
// ---------------------------------------------------------------------------

#[test]
fn grid_completeness_all_features_mapped() {
    let summary = summarize_governance();
    assert!(
        summary.all_features_known(),
        "not all support features have governance bindings"
    );
    assert_eq!(
        summary.total_support_features,
        summary.mapped_support_features
    );
    assert_eq!(summary.total_support_features, 7);
    assert_eq!(summary.explicit_bindings(), 4);
}

// ---------------------------------------------------------------------------
// 14. Multiple features can be queried in sequence
// ---------------------------------------------------------------------------

#[test]
fn multiple_features_queried_in_sequence() {
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

    let mut results = Vec::new();
    for id in &ids {
        let state = governance_state_for_support_id(*id, &flags).unwrap();
        results.push((state.support_id, state.runtime_enabled));
    }

    assert_eq!(results.len(), 7);
    // All ungoverned + default-enabled governed should be true
    for (id, enabled) in &results {
        match id {
            FeatureId::Level66Renames => assert!(!enabled, "RenamesR4R6 off by default"),
            _ => assert!(enabled, "{id:?} should be enabled with defaults"),
        }
    }
}

// ---------------------------------------------------------------------------
// 15. Governance state is consistent across modules
// ---------------------------------------------------------------------------

#[test]
fn governance_state_consistent_across_modules() {
    let flags = FeatureFlags::default();

    // Grid layer
    let grid_summary = summarize_governance();

    // Runtime layer
    let rt_summary = runtime_summary(&flags);
    let states = governance_states(&flags);
    let support = support_states();

    // Totals agree across layers
    assert_eq!(
        grid_summary.total_support_features,
        rt_summary.total_support_features
    );
    assert_eq!(
        grid_summary.mapped_support_features,
        rt_summary.mapped_support_features
    );
    assert_eq!(
        grid_summary.total_linked_feature_flags,
        rt_summary.total_linked_feature_flags
    );

    // Governance states count matches bindings
    assert_eq!(states.len(), governance_bindings().len());

    // Support states count matches matrix
    assert_eq!(
        support.len(),
        copybook_governance::support_matrix::all_features().len()
    );

    // Enabled + disabled = mapped
    assert_eq!(
        rt_summary.runtime_enabled_features + rt_summary.runtime_disabled_features,
        rt_summary.mapped_support_features
    );
}

// ---------------------------------------------------------------------------
// Additional integration tests for coverage
// ---------------------------------------------------------------------------

#[test]
fn e2e_disable_flags_propagates_to_runtime() {
    let flags = FeatureFlags::builder()
        .disable(Feature::Comp1)
        .disable(Feature::Comp2)
        .build();

    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(!state.runtime_enabled);
    assert_eq!(state.missing_feature_flags.len(), 2);

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

#[test]
fn builder_category_enable_flows_to_runtime() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Experimental)
        .build();

    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
    assert!(is_support_runtime_available(FeatureId::Comp1Comp2, &flags));
    assert!(is_support_runtime_available(
        FeatureId::SignSeparate,
        &flags
    ));
}

#[test]
fn handle_snapshot_used_for_governance_evaluation() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::RenamesR4R6);
    let snap = handle.snapshot();

    // Snapshot enables Level66Renames
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &snap
    ));

    // Mutate handle after snapshot
    handle.disable(Feature::RenamesR4R6);
    // Snapshot is unaffected
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &snap
    ));
    // But fresh snapshot reflects change
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &handle.snapshot()
    ));
}

#[test]
fn governance_state_preserves_support_metadata() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();
    assert_eq!(state.support_id, FeatureId::SignSeparate);
    assert!(!state.support_name.is_empty());
    assert!(!state.support_description.is_empty());
    assert!(state.doc_ref.is_some());
    assert!(!state.rationale.is_empty());
    assert_eq!(state.support_status, SupportStatus::Supported);
}

#[test]
fn lifecycle_and_category_accessible_from_facade() {
    let _ = FeatureLifecycle::Experimental;
    let _ = FeatureLifecycle::Stable;
    let _ = FeatureLifecycle::Deprecated;

    assert_eq!(Feature::Comp1.category(), FeatureCategory::Experimental);
    assert_eq!(Feature::AuditSystem.category(), FeatureCategory::Enterprise);
    assert_eq!(Feature::LruCache.category(), FeatureCategory::Performance);
    assert_eq!(Feature::VerboseLogging.category(), FeatureCategory::Debug);
    assert_eq!(
        Feature::MutationTesting.category(),
        FeatureCategory::Testing
    );
}

#[test]
fn governance_bindings_have_no_duplicate_support_ids() {
    let bindings = governance_bindings();
    for (i, b) in bindings.iter().enumerate() {
        for other in &bindings[i + 1..] {
            assert_ne!(
                b.support_id, other.support_id,
                "duplicate support_id: {:?}",
                b.support_id
            );
        }
    }
}
