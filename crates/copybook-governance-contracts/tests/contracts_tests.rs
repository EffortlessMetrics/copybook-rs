//! Integration tests for copybook-governance-contracts façade.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance_contracts::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureId, FeatureLifecycle,
    SupportStatus, all_features, find_feature, find_feature_by_id,
};

// ── Builder & defaults ──────────────────────────────────────────────────────

#[test]
fn builder_produces_expected_defaults() {
    let flags = FeatureFlags::builder().build();
    assert!(flags.is_enabled(Feature::SignSeparate));
    assert!(flags.is_enabled(Feature::Comp1));
    assert!(flags.is_enabled(Feature::Comp2));
    assert!(flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::AuditSystem));
    assert!(!flags.is_enabled(Feature::RenamesR4R6));
}

#[test]
fn builder_enable_overrides_default_off() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .enable(Feature::AuditSystem)
        .build();
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::AuditSystem));
}

#[test]
fn builder_disable_overrides_default_on() {
    let flags = FeatureFlags::builder()
        .disable(Feature::LruCache)
        .disable(Feature::Comp1)
        .build();
    assert!(!flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::Comp1));
}

#[test]
fn builder_enable_then_disable_same_feature() {
    let flags = FeatureFlags::builder()
        .enable(Feature::Profiling)
        .disable(Feature::Profiling)
        .build();
    assert!(!flags.is_enabled(Feature::Profiling));
}

#[test]
fn builder_disable_then_enable_same_feature() {
    let flags = FeatureFlags::builder()
        .disable(Feature::LruCache)
        .enable(Feature::LruCache)
        .build();
    assert!(flags.is_enabled(Feature::LruCache));
}

#[test]
fn builder_enable_category_activates_all_members() {
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
fn builder_disable_category_deactivates_all_members() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Performance)
        .disable_category(FeatureCategory::Performance)
        .build();
    let perf = flags.enabled_in_category(FeatureCategory::Performance);
    assert!(perf.is_empty());
}

#[test]
fn builder_enable_category_then_disable_single_member() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Debug)
        .disable(Feature::VerboseLogging)
        .build();
    assert!(!flags.is_enabled(Feature::VerboseLogging));
    assert!(flags.is_enabled(Feature::DiagnosticOutput));
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(flags.is_enabled(Feature::MemoryTracking));
}

#[test]
fn toggle_flips_state() {
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
    // Default: SignSeparate, Comp1, Comp2, LruCache
    assert_eq!(count, 4);
}

#[test]
fn enabled_in_category_returns_only_that_category() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Enterprise)
        .build();
    let enterprise = flags.enabled_in_category(FeatureCategory::Enterprise);
    assert_eq!(enterprise.len(), 6);
    for f in &enterprise {
        assert_eq!(f.category(), FeatureCategory::Enterprise);
    }
}

#[test]
fn features_in_category_counts_match() {
    let exp = FeatureFlags::features_in_category(FeatureCategory::Experimental);
    assert_eq!(exp.len(), 4);
    let ent = FeatureFlags::features_in_category(FeatureCategory::Enterprise);
    assert_eq!(ent.len(), 6);
    let perf = FeatureFlags::features_in_category(FeatureCategory::Performance);
    assert_eq!(perf.len(), 4);
    let dbg = FeatureFlags::features_in_category(FeatureCategory::Debug);
    assert_eq!(dbg.len(), 4);
    let test = FeatureFlags::features_in_category(FeatureCategory::Testing);
    assert_eq!(test.len(), 4);
}

// ── Support matrix queries ──────────────────────────────────────────────────

#[test]
fn all_features_returns_seven_entries() {
    assert_eq!(all_features().len(), 7);
}

#[test]
fn find_feature_by_id_returns_correct_metadata() {
    let f = find_feature_by_id(FeatureId::EditedPic).unwrap();
    assert_eq!(f.id, FeatureId::EditedPic);
    assert_eq!(f.status, SupportStatus::Supported);
    assert!(!f.name.is_empty());
    assert!(f.doc_ref.is_some());
}

#[test]
fn find_feature_by_string_resolves_kebab_case() {
    let pairs = [
        ("level-88", FeatureId::Level88Conditions),
        ("level-66-renames", FeatureId::Level66Renames),
        ("occurs-depending", FeatureId::OccursDepending),
        ("edited-pic", FeatureId::EditedPic),
        ("comp-1-comp-2", FeatureId::Comp1Comp2),
        ("sign-separate", FeatureId::SignSeparate),
        ("nested-odo", FeatureId::NestedOdo),
    ];
    for (name, expected_id) in pairs {
        let f = find_feature(name).unwrap_or_else(|| panic!("missing feature '{name}'"));
        assert_eq!(f.id, expected_id);
    }
}

#[test]
fn find_feature_unknown_returns_none() {
    assert!(find_feature("nonexistent-feature").is_none());
    assert!(find_feature("").is_none());
}

#[test]
fn all_features_have_nonempty_name_and_description() {
    for f in all_features() {
        assert!(!f.name.is_empty(), "{:?} has empty name", f.id);
        assert!(
            !f.description.is_empty(),
            "{:?} has empty description",
            f.id
        );
    }
}

#[test]
fn all_features_have_doc_ref() {
    for f in all_features() {
        assert!(f.doc_ref.is_some(), "{:?} should have a doc_ref", f.id);
    }
}

// ── Support status coverage ─────────────────────────────────────────────────

#[test]
fn supported_features_have_correct_status() {
    let supported_ids = [
        FeatureId::Level88Conditions,
        FeatureId::EditedPic,
        FeatureId::Comp1Comp2,
        FeatureId::SignSeparate,
    ];
    for id in supported_ids {
        let f = find_feature_by_id(id).unwrap();
        assert_eq!(
            f.status,
            SupportStatus::Supported,
            "expected Supported for {id:?}"
        );
    }
}

#[test]
fn partial_features_have_correct_status() {
    let partial_ids = [
        FeatureId::Level66Renames,
        FeatureId::OccursDepending,
        FeatureId::NestedOdo,
    ];
    for id in partial_ids {
        let f = find_feature_by_id(id).unwrap();
        assert_eq!(
            f.status,
            SupportStatus::Partial,
            "expected Partial for {id:?}"
        );
    }
}

#[test]
fn all_support_status_variants_constructible() {
    let statuses = [
        SupportStatus::Supported,
        SupportStatus::Partial,
        SupportStatus::Planned,
        SupportStatus::NotPlanned,
    ];
    assert_eq!(statuses.len(), 4);
}

#[test]
fn feature_lifecycle_variants_constructible() {
    let lifecycles = [
        FeatureLifecycle::Experimental,
        FeatureLifecycle::Stable,
        FeatureLifecycle::Deprecated,
    ];
    assert_eq!(lifecycles.len(), 3);
}

#[test]
fn feature_category_round_trip_via_facade() {
    assert_eq!(Feature::Comp1.category(), FeatureCategory::Experimental);
    assert_eq!(Feature::AuditSystem.category(), FeatureCategory::Enterprise);
    assert_eq!(Feature::LruCache.category(), FeatureCategory::Performance);
    assert_eq!(Feature::VerboseLogging.category(), FeatureCategory::Debug);
    assert_eq!(
        Feature::MutationTesting.category(),
        FeatureCategory::Testing
    );
}

// ── FeatureFlagsHandle ──────────────────────────────────────────────────────

#[test]
fn handle_enable_disable_toggle() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::Profiling);
    assert!(handle.is_enabled(Feature::Profiling));
    handle.disable(Feature::Profiling);
    assert!(!handle.is_enabled(Feature::Profiling));
    handle.toggle(Feature::Profiling);
    assert!(handle.is_enabled(Feature::Profiling));
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

#[test]
fn handle_multiple_toggles_return_to_original() {
    let handle = FeatureFlagsHandle::new();
    let original = handle.is_enabled(Feature::LruCache);
    handle.toggle(Feature::LruCache);
    handle.toggle(Feature::LruCache);
    assert_eq!(handle.is_enabled(Feature::LruCache), original);
}

// ── Cross-module coherence ──────────────────────────────────────────────────

#[test]
fn cross_module_id_equality() {
    let from_matrix = find_feature_by_id(FeatureId::Level88Conditions).unwrap();
    let from_string = find_feature("level-88").unwrap();
    assert_eq!(from_matrix.id, from_string.id);
    assert_eq!(from_matrix.name, from_string.name);
}

#[test]
fn feature_flags_module_path_works() {
    use copybook_governance_contracts::feature_flags;
    let features = feature_flags::all_features();
    assert_eq!(features.len(), 22);
}

#[test]
fn support_matrix_module_path_works() {
    use copybook_governance_contracts::support_matrix;
    let features = support_matrix::all_features();
    assert_eq!(features.len(), 7);
}

#[test]
fn no_duplicate_feature_ids_in_support_matrix() {
    let ids: Vec<FeatureId> = all_features().iter().map(|f| f.id).collect();
    for (i, id) in ids.iter().enumerate() {
        assert!(!ids[i + 1..].contains(id), "duplicate feature id: {id:?}");
    }
}
