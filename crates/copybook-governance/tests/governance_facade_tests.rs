// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for copybook-governance main façade.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_governance::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsHandle, FeatureGovernanceState,
    FeatureGovernanceSummary, FeatureId, FeatureLifecycle, GovernanceSummary,
    GovernedFeatureBinding, SupportStatus, feature_flags_for_support_id, governance_bindings,
    governance_state_for_support_id, governance_states, is_support_runtime_available,
    runtime_summary, summarize_governance, support_states,
};

// ── Full stack re-export validation ─────────────────────────────────────────

#[test]
fn facade_reexports_all_grid_types() {
    let _b: &[GovernedFeatureBinding] = governance_bindings();
    let _s: GovernanceSummary = summarize_governance();
    let _f: Option<&[Feature]> = feature_flags_for_support_id(FeatureId::EditedPic);
}

#[test]
fn facade_reexports_all_runtime_types() {
    let flags = FeatureFlags::default();
    let _states: Vec<FeatureGovernanceState> = governance_states(&flags);
    let _summary: FeatureGovernanceSummary = runtime_summary(&flags);
    let _available: bool = is_support_runtime_available(FeatureId::Level88Conditions, &flags);
    let _support: Vec<FeatureGovernanceState> = support_states();
}

#[test]
fn facade_reexports_feature_flags_types() {
    let _f = Feature::SignSeparate;
    let _c = FeatureCategory::Experimental;
    let _l = FeatureLifecycle::Stable;
    let _b = FeatureFlags::builder();
    let _h = FeatureFlagsHandle::new();
}

#[test]
fn facade_reexports_support_matrix_types() {
    let _id = FeatureId::Level88Conditions;
    let _status = SupportStatus::Supported;
}

// ── End-to-end governance flow ──────────────────────────────────────────────

#[test]
fn end_to_end_enable_renames_changes_governance_state() {
    // Default: RenamesR4R6 disabled → Level66Renames governance disabled
    let default_flags = FeatureFlags::default();
    assert!(!is_support_runtime_available(
        FeatureId::Level66Renames,
        &default_flags
    ));

    // Enable the flag → governance enabled
    let flags = FeatureFlags::builder().enable(Feature::RenamesR4R6).build();
    assert!(is_support_runtime_available(
        FeatureId::Level66Renames,
        &flags
    ));
}

#[test]
fn end_to_end_disable_comp_flags_affects_summary() {
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
fn end_to_end_all_flags_enabled_full_coverage() {
    let mut flags = FeatureFlags::default();
    for f in copybook_governance::feature_flags::all_features() {
        flags.enable(f);
    }

    let summary = runtime_summary(&flags);
    assert_eq!(summary.runtime_disabled_features, 0);
    assert_eq!(summary.runtime_enabled_features, 7);
    assert!(summary.all_support_rows_present());
    assert!(!summary.has_runtime_unavailable_features());

    for state in governance_states(&flags) {
        assert!(
            state.runtime_enabled,
            "{:?} should be enabled",
            state.support_id
        );
    }
}

#[test]
fn end_to_end_governance_summary_and_runtime_summary_agree_on_totals() {
    let flags = FeatureFlags::default();
    let grid = summarize_governance();
    let runtime = runtime_summary(&flags);
    assert_eq!(grid.total_support_features, runtime.total_support_features);
    assert_eq!(
        grid.mapped_support_features,
        runtime.mapped_support_features
    );
    assert_eq!(
        grid.total_linked_feature_flags,
        runtime.total_linked_feature_flags
    );
}

// ── Module paths ────────────────────────────────────────────────────────────

#[test]
fn feature_flags_module_accessible() {
    use copybook_governance::feature_flags;
    let all = feature_flags::all_features();
    assert_eq!(all.len(), 22);
}

#[test]
fn support_matrix_module_accessible() {
    use copybook_governance::support_matrix;
    let all = support_matrix::all_features();
    assert_eq!(all.len(), 7);
}

// ── Builder through top-level façade ────────────────────────────────────────

#[test]
fn builder_enable_enterprise_category_through_facade() {
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
fn builder_selective_flags_through_facade() {
    let flags = FeatureFlags::builder()
        .enable(Feature::RenamesR4R6)
        .enable(Feature::Profiling)
        .disable(Feature::LruCache)
        .build();
    assert!(flags.is_enabled(Feature::RenamesR4R6));
    assert!(flags.is_enabled(Feature::Profiling));
    assert!(!flags.is_enabled(Feature::LruCache));
}

// ── Handle through façade ───────────────────────────────────────────────────

#[test]
fn handle_roundtrip_through_facade() {
    let handle = FeatureFlagsHandle::new();
    handle.enable(Feature::AdvancedOptimization);
    assert!(handle.is_enabled(Feature::AdvancedOptimization));
    let snap = handle.snapshot();
    handle.disable(Feature::AdvancedOptimization);
    assert!(snap.is_enabled(Feature::AdvancedOptimization));
    assert!(!handle.is_enabled(Feature::AdvancedOptimization));
}

#[test]
fn handle_toggle_through_facade() {
    let handle = FeatureFlagsHandle::new();
    let before = handle.is_enabled(Feature::ParallelDecode);
    handle.toggle(Feature::ParallelDecode);
    assert_ne!(handle.is_enabled(Feature::ParallelDecode), before);
    handle.toggle(Feature::ParallelDecode);
    assert_eq!(handle.is_enabled(Feature::ParallelDecode), before);
}
