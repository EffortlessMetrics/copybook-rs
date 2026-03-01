// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for copybook-governance façade.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_governance::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle, FeatureId,
    FeatureLifecycle, FeatureSupport, GovernanceSummary, GovernedFeatureBinding, SupportStatus,
    feature_flags_for_support_id, governance_bindings, governance_state_for_support_id,
    governance_states, is_support_runtime_available, runtime_summary, summarize_governance,
    support_states,
};

// =========================================================================
// 1. Public type re-export validation
// =========================================================================

#[test]
fn all_public_types_accessible_from_facade() {
    let _feature: Feature = Feature::Comp1;
    let _category: FeatureCategory = FeatureCategory::Experimental;
    let _lifecycle: FeatureLifecycle = FeatureLifecycle::Stable;
    let _flags: FeatureFlags = FeatureFlags::default();
    let _builder: FeatureFlagsBuilder = FeatureFlags::builder();
    let _handle: FeatureFlagsHandle = FeatureFlagsHandle::new();
    let _id: FeatureId = FeatureId::EditedPic;
    let _status: SupportStatus = SupportStatus::Supported;
    let _bindings: &[GovernedFeatureBinding] = governance_bindings();
    let _summary: GovernanceSummary = summarize_governance();
}

#[test]
fn feature_flags_module_re_exports_all_features() {
    let all = copybook_governance::feature_flags::all_features();
    assert_eq!(all.len(), 22, "expected 22 feature variants");
}

#[test]
fn support_matrix_module_re_exports_all_features() {
    let all = copybook_governance::support_matrix::all_features();
    assert_eq!(all.len(), 7, "expected 7 support matrix features");
}

// =========================================================================
// 2. Feature flags defaults and builder
// =========================================================================

#[test]
fn default_flags_enable_expected_features() {
    let flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::SignSeparate));
    assert!(flags.is_enabled(Feature::Comp1));
    assert!(flags.is_enabled(Feature::Comp2));
    assert!(flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::RenamesR4R6));
    assert!(!flags.is_enabled(Feature::AuditSystem));
    assert!(!flags.is_enabled(Feature::Profiling));
}

#[test]
fn builder_enable_disable_individual_features() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .enable(Feature::RenamesR4R6)
        .disable(Feature::LruCache)
        .disable(Feature::Comp1)
        .build();
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::RenamesR4R6));
    assert!(!flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::Comp1));
}

#[test]
fn builder_enable_category_enables_all_in_category() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Enterprise)
        .build();
    assert!(flags.is_enabled(Feature::AuditSystem));
    assert!(flags.is_enabled(Feature::SoxCompliance));
    assert!(flags.is_enabled(Feature::HipaaCompliance));
    assert!(flags.is_enabled(Feature::GdprCompliance));
    assert!(flags.is_enabled(Feature::PciDssCompliance));
    assert!(flags.is_enabled(Feature::SecurityMonitoring));
}

#[test]
fn builder_disable_category_disables_all_in_category() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Debug)
        .disable_category(FeatureCategory::Debug)
        .build();
    assert!(!flags.is_enabled(Feature::VerboseLogging));
    assert!(!flags.is_enabled(Feature::DiagnosticOutput));
    assert!(!flags.is_enabled(Feature::Profiling));
    assert!(!flags.is_enabled(Feature::MemoryTracking));
}

#[test]
fn builder_chaining_last_wins() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .disable(Feature::Profiling)
        .build();
    assert!(!flags.is_enabled(Feature::Profiling));

    let flags2 = FeatureFlags::builder()
        .disable(Feature::LruCache)
        .enable(Feature::LruCache)
        .build();
    assert!(flags2.is_enabled(Feature::LruCache));
}

// =========================================================================
// 3. FeatureFlags toggle and iteration
// =========================================================================

#[test]
fn toggle_flips_feature_state() {
    let mut flags = FeatureFlags::default();
    let before = flags.is_enabled(Feature::LruCache);
    flags.toggle(Feature::LruCache);
    assert_ne!(flags.is_enabled(Feature::LruCache), before);
    flags.toggle(Feature::LruCache);
    assert_eq!(flags.is_enabled(Feature::LruCache), before);
}

#[test]
fn enabled_features_iterator_matches_count() {
    let flags = FeatureFlags::default();
    let count = flags.enabled_features().count();
    assert_eq!(
        count, 4,
        "default has 4 enabled: SignSeparate, Comp1, Comp2, LruCache"
    );
}

#[test]
fn enabled_in_category_returns_correct_subset() {
    let flags = FeatureFlags::default();
    let experimental = flags.enabled_in_category(FeatureCategory::Experimental);
    // SignSeparate, Comp1, Comp2 are default-on in Experimental
    assert_eq!(experimental.len(), 3);
    assert!(experimental.contains(&Feature::SignSeparate));
    assert!(experimental.contains(&Feature::Comp1));
    assert!(experimental.contains(&Feature::Comp2));
    assert!(!experimental.contains(&Feature::RenamesR4R6));
}

#[test]
fn features_in_category_static_counts() {
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Experimental).len(),
        4
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Enterprise).len(),
        6
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Performance).len(),
        4
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Debug).len(),
        4
    );
    assert_eq!(
        FeatureFlags::features_in_category(FeatureCategory::Testing).len(),
        4
    );
}

// =========================================================================
// 4. FeatureFlagsHandle (thread-safe handle)
// =========================================================================

#[test]
fn handle_enable_disable_roundtrip() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::AdvancedOptimization);
    assert!(handle.is_enabled(Feature::AdvancedOptimization));
    handle.disable(Feature::AdvancedOptimization);
    assert!(!handle.is_enabled(Feature::AdvancedOptimization));
}

#[test]
fn handle_toggle_roundtrip() {
    let handle = FeatureFlagsHandle::new();
    let before = handle.is_enabled(Feature::ParallelDecode);
    handle.toggle(Feature::ParallelDecode);
    assert_ne!(handle.is_enabled(Feature::ParallelDecode), before);
    handle.toggle(Feature::ParallelDecode);
    assert_eq!(handle.is_enabled(Feature::ParallelDecode), before);
}

#[test]
fn handle_snapshot_is_independent() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::AdvancedOptimization);
    let snap = handle.snapshot();
    handle.disable(Feature::AdvancedOptimization);
    assert!(snap.is_enabled(Feature::AdvancedOptimization));
    assert!(!handle.is_enabled(Feature::AdvancedOptimization));
}

#[test]
fn handle_clone_is_independent() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::Profiling);
    let cloned = handle.clone();
    handle.disable(Feature::Profiling);
    assert!(cloned.is_enabled(Feature::Profiling));
    assert!(!handle.is_enabled(Feature::Profiling));
}

// =========================================================================
// 5. Display and Debug traits
// =========================================================================

#[test]
fn feature_display_produces_snake_case() {
    assert_eq!(Feature::SignSeparate.to_string(), "sign_separate");
    assert_eq!(Feature::RenamesR4R6.to_string(), "renames_r4_r6");
    assert_eq!(Feature::Comp1.to_string(), "comp_1");
    assert_eq!(Feature::LruCache.to_string(), "lru_cache");
    assert_eq!(Feature::AuditSystem.to_string(), "audit_system");
}

#[test]
fn feature_category_display_produces_lowercase() {
    assert_eq!(FeatureCategory::Experimental.to_string(), "experimental");
    assert_eq!(FeatureCategory::Enterprise.to_string(), "enterprise");
    assert_eq!(FeatureCategory::Performance.to_string(), "performance");
    assert_eq!(FeatureCategory::Debug.to_string(), "debug");
    assert_eq!(FeatureCategory::Testing.to_string(), "testing");
}

#[test]
fn feature_lifecycle_display_produces_lowercase() {
    assert_eq!(FeatureLifecycle::Experimental.to_string(), "experimental");
    assert_eq!(FeatureLifecycle::Stable.to_string(), "stable");
    assert_eq!(FeatureLifecycle::Deprecated.to_string(), "deprecated");
}

#[test]
fn feature_debug_format_includes_variant_name() {
    let dbg = format!("{:?}", Feature::SignSeparate);
    assert!(dbg.contains("SignSeparate"));
}

#[test]
fn feature_flags_debug_is_nonempty() {
    let flags = FeatureFlags::default();
    let dbg = format!("{flags:?}");
    assert!(!dbg.is_empty());
}

#[test]
fn governance_summary_debug_is_nonempty() {
    let summary = summarize_governance();
    let dbg = format!("{summary:?}");
    assert!(!dbg.is_empty());
}

// =========================================================================
// 6. Serde roundtrip tests
// =========================================================================

#[test]
fn feature_serde_roundtrip_all_variants() {
    for feature in copybook_governance::feature_flags::all_features() {
        let json = serde_json::to_string(&feature).unwrap();
        let back: Feature = serde_json::from_str(&json).unwrap();
        assert_eq!(back, feature, "serde roundtrip failed for {feature}");
    }
}

#[test]
fn feature_category_serde_roundtrip() {
    let categories = [
        FeatureCategory::Experimental,
        FeatureCategory::Enterprise,
        FeatureCategory::Performance,
        FeatureCategory::Debug,
        FeatureCategory::Testing,
    ];
    for cat in categories {
        let json = serde_json::to_string(&cat).unwrap();
        let back: FeatureCategory = serde_json::from_str(&json).unwrap();
        assert_eq!(back, cat, "serde roundtrip failed for {cat}");
    }
}

#[test]
fn feature_lifecycle_serde_roundtrip() {
    let lifecycles = [
        FeatureLifecycle::Experimental,
        FeatureLifecycle::Stable,
        FeatureLifecycle::Deprecated,
    ];
    for lc in lifecycles {
        let json = serde_json::to_string(&lc).unwrap();
        let back: FeatureLifecycle = serde_json::from_str(&json).unwrap();
        assert_eq!(back, lc, "serde roundtrip failed for {lc}");
    }
}

#[test]
fn feature_flags_serde_roundtrip() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .disable(Feature::LruCache)
        .enable(Feature::AuditSystem)
        .build();
    let json = serde_json::to_string(&flags).unwrap();
    let back: FeatureFlags = serde_json::from_str(&json).unwrap();
    assert!(back.is_enabled(Feature::Profiling));
    assert!(!back.is_enabled(Feature::LruCache));
    assert!(back.is_enabled(Feature::AuditSystem));
}

#[test]
fn feature_governance_state_serializable() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    let json = serde_json::to_value(&state).unwrap();
    assert!(json["support_id"].is_string());
    assert!(json["runtime_enabled"].is_boolean());
    assert!(json["support_name"].is_string());
    assert!(json["rationale"].is_string());
}

#[test]
fn feature_governance_summary_serializable() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    let json = serde_json::to_value(&summary).unwrap();
    assert!(json["total_support_features"].is_number());
    assert!(json["mapped_support_features"].is_number());
    assert!(json["runtime_enabled_features"].is_number());
    assert!(json["runtime_disabled_features"].is_number());
}

// =========================================================================
// 7. Governance grid & bindings
// =========================================================================

#[test]
fn governance_bindings_count_is_seven() {
    assert_eq!(governance_bindings().len(), 7);
}

#[test]
fn governance_bindings_no_duplicate_support_ids() {
    let bindings = governance_bindings();
    for (i, b) in bindings.iter().enumerate() {
        for other in &bindings[i + 1..] {
            assert_ne!(
                b.support_id, other.support_id,
                "duplicate: {:?}",
                b.support_id
            );
        }
    }
}

#[test]
fn all_bindings_have_nonempty_rationale() {
    for binding in governance_bindings() {
        assert!(
            !binding.rationale.is_empty(),
            "{:?} empty rationale",
            binding.support_id
        );
    }
}

#[test]
fn feature_flags_for_all_support_ids_returns_some() {
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
        assert!(
            feature_flags_for_support_id(id).is_some(),
            "missing mapping for {id:?}"
        );
    }
}

#[test]
fn ungoverned_features_have_empty_flag_slices() {
    let ungoverned = [
        FeatureId::Level88Conditions,
        FeatureId::OccursDepending,
        FeatureId::EditedPic,
        FeatureId::NestedOdo,
    ];
    for id in ungoverned {
        let flags = feature_flags_for_support_id(id).unwrap();
        assert!(flags.is_empty(), "{id:?} should have no feature flags");
    }
}

#[test]
fn governed_features_have_expected_flag_counts() {
    assert_eq!(
        feature_flags_for_support_id(FeatureId::SignSeparate)
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        feature_flags_for_support_id(FeatureId::Comp1Comp2)
            .unwrap()
            .len(),
        2
    );
    assert_eq!(
        feature_flags_for_support_id(FeatureId::Level66Renames)
            .unwrap()
            .len(),
        1
    );
}

// =========================================================================
// 8. Governance summary
// =========================================================================

#[test]
fn summarize_governance_totals() {
    let summary = summarize_governance();
    assert_eq!(summary.total_support_features, 7);
    assert_eq!(summary.mapped_support_features, 7);
    assert!(summary.all_features_known());
    assert_eq!(summary.explicit_bindings(), 4);
}

#[test]
fn governance_summary_linked_flags_count_is_four() {
    let summary = summarize_governance();
    // SignSeparate(1) + RenamesR4R6(1) + Comp1+Comp2(2) = 4
    assert_eq!(summary.total_linked_feature_flags, 4);
}

// =========================================================================
// 9. Runtime governance states
// =========================================================================

#[test]
fn support_states_all_enabled_without_governance_mapping() {
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
fn governance_states_count_matches_bindings() {
    let flags = FeatureFlags::default();
    let states = governance_states(&flags);
    assert_eq!(states.len(), governance_bindings().len());
}

#[test]
fn governance_state_with_default_flags_comp12_enabled() {
    let flags = FeatureFlags::default();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(state.runtime_enabled);
    assert!(state.missing_feature_flags.is_empty());
    assert_eq!(state.required_feature_flags.len(), 2);
}

#[test]
fn governance_state_comp12_disabled_when_comp1_off() {
    let flags = FeatureFlags::builder().disable(Feature::Comp1).build();
    let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
    assert!(!state.runtime_enabled);
    assert!(state.missing_feature_flags.contains(&Feature::Comp1));
}

#[test]
fn governance_state_renames_disabled_by_default() {
    let flags = FeatureFlags::default();
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
}

#[test]
fn governance_state_renames_enabled_when_flag_on() {
    let flags = FeatureFlags::builder().enable(Feature::RenamesR4R6).build();
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
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
}

// =========================================================================
// 10. Runtime summary
// =========================================================================

#[test]
fn runtime_summary_defaults_have_one_disabled() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    // Level66Renames is disabled (needs RenamesR4R6 which is off by default)
    assert!(summary.runtime_disabled_features >= 1);
    assert!(summary.has_runtime_unavailable_features());
}

#[test]
fn runtime_summary_all_enabled_has_zero_disabled() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags.enable(f);
    }
    let summary = runtime_summary(&flags);
    assert_eq!(summary.runtime_disabled_features, 0);
    assert!(!summary.has_runtime_unavailable_features());
    assert_eq!(summary.runtime_enabled_features, 7);
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
fn runtime_summary_and_grid_summary_totals_agree() {
    let grid = summarize_governance();
    let flags = FeatureFlags::default();
    let rt = runtime_summary(&flags);
    assert_eq!(grid.total_support_features, rt.total_support_features);
    assert_eq!(grid.mapped_support_features, rt.mapped_support_features);
    assert_eq!(
        grid.total_linked_feature_flags,
        rt.total_linked_feature_flags
    );
}

#[test]
fn runtime_summary_all_rows_present() {
    let flags = FeatureFlags::default();
    let summary = runtime_summary(&flags);
    assert!(summary.all_support_rows_present());
}

// =========================================================================
// 11. Edge cases
// =========================================================================

#[test]
fn all_features_disabled_multiple_governance_rows_disabled() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags.disable(f);
    }
    let summary = runtime_summary(&flags);
    // At least SignSeparate, Comp1Comp2, Level66Renames should be disabled
    assert!(summary.runtime_disabled_features >= 3);
    assert!(summary.has_runtime_unavailable_features());
}

#[test]
fn governance_states_with_all_disabled_show_missing_flags() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags.disable(f);
    }
    let states = governance_states(&flags);
    for state in &states {
        if !state.required_feature_flags.is_empty() {
            assert!(
                !state.runtime_enabled,
                "{:?} should be disabled",
                state.support_id
            );
            assert!(
                !state.missing_feature_flags.is_empty(),
                "{:?} should have missing flags",
                state.support_id
            );
        }
    }
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
    }
}

#[test]
fn find_feature_unknown_string_returns_none() {
    assert!(copybook_governance::support_matrix::find_feature("nonexistent").is_none());
    assert!(copybook_governance::support_matrix::find_feature("").is_none());
}

#[test]
fn feature_description_nonempty_for_all() {
    for feature in copybook_governance::feature_flags::all_features() {
        assert!(
            !feature.description().is_empty(),
            "{feature} has empty description"
        );
    }
}

#[test]
fn feature_category_correct_for_all() {
    for feature in copybook_governance::feature_flags::all_features() {
        let _cat = feature.category();
    }
}

#[test]
fn support_features_all_have_doc_ref() {
    for f in copybook_governance::support_matrix::all_features() {
        assert!(f.doc_ref.is_some(), "{:?} should have doc_ref", f.id);
    }
}

#[test]
fn handle_snapshot_used_for_governance_evaluation() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::RenamesR4R6);
    let snap = handle.snapshot();
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &snap
    ));
    handle.disable(Feature::RenamesR4R6);
    // Snapshot should still show it enabled
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &snap
    ));
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &handle.snapshot()
    ));
}
