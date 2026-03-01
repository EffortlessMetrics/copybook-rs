// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for copybook-governance-contracts façade.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance_contracts::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureId, FeatureLifecycle,
    SupportStatus, all_features, find_feature, find_feature_by_id,
};

// ── Re-export coherence ─────────────────────────────────────────────────────

#[test]
fn feature_flags_reexport_matches_contracts_behaviour() {
    let flags = FeatureFlags::default();
    assert!(flags.is_enabled(Feature::SignSeparate));
    assert!(flags.is_enabled(Feature::Comp1));
    assert!(flags.is_enabled(Feature::Comp2));
    assert!(flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::AuditSystem));
}

#[test]
fn support_matrix_reexport_has_all_features() {
    assert_eq!(all_features().len(), 7);
}

#[test]
fn feature_lifecycle_reexport_is_accessible() {
    let _ = FeatureLifecycle::Experimental;
    let _ = FeatureLifecycle::Stable;
    let _ = FeatureLifecycle::Deprecated;
}

// ── Cross-module queries ────────────────────────────────────────────────────

#[test]
fn find_feature_by_id_and_string_agree() {
    let by_id = find_feature_by_id(FeatureId::Level88Conditions).unwrap();
    let by_str = find_feature("level-88").unwrap();
    assert_eq!(by_id.id, by_str.id);
    assert_eq!(by_id.name, by_str.name);
    assert_eq!(by_id.status, by_str.status);
}

#[test]
fn find_feature_returns_none_for_garbage_string() {
    assert!(find_feature("garbage").is_none());
}

// ── Builder through façade ──────────────────────────────────────────────────

#[test]
fn builder_enable_category_through_facade() {
    let flags = FeatureFlags::builder()
        .enable_category(FeatureCategory::Testing)
        .build();
    assert!(flags.is_enabled(Feature::MutationTesting));
    assert!(flags.is_enabled(Feature::FuzzingIntegration));
    assert!(flags.is_enabled(Feature::CoverageInstrumentation));
    assert!(flags.is_enabled(Feature::PropertyBasedTesting));
}

#[test]
fn builder_disable_category_through_facade() {
    let flags = FeatureFlags::builder()
        .disable_category(FeatureCategory::Performance)
        .build();
    assert!(!flags.is_enabled(Feature::AdvancedOptimization));
    assert!(!flags.is_enabled(Feature::LruCache));
    assert!(!flags.is_enabled(Feature::ParallelDecode));
    assert!(!flags.is_enabled(Feature::ZeroCopy));
}

// ── Handle through façade ───────────────────────────────────────────────────

#[test]
fn handle_snapshot_through_facade() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::DiagnosticOutput);
    let snap = handle.snapshot();
    assert!(snap.is_enabled(Feature::DiagnosticOutput));
    handle.disable(Feature::DiagnosticOutput);
    // snapshot is independent
    assert!(snap.is_enabled(Feature::DiagnosticOutput));
}

#[test]
fn handle_toggle_through_facade() {
    let handle = FeatureFlagsHandle::new();
    let before = handle.is_enabled(Feature::LruCache);
    handle.toggle(Feature::LruCache);
    assert_ne!(handle.is_enabled(Feature::LruCache), before);
}

// ── SupportStatus variants accessible ───────────────────────────────────────

#[test]
fn support_status_variants_are_all_accessible() {
    let statuses = [
        SupportStatus::Supported,
        SupportStatus::Partial,
        SupportStatus::Planned,
        SupportStatus::NotPlanned,
    ];
    assert_eq!(statuses.len(), 4);
}

#[test]
fn feature_id_all_variants_accessible() {
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
        assert!(find_feature_by_id(id).is_some());
    }
}

// ── Module path re-export coherence ─────────────────────────────────────────

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
