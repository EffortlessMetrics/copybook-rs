#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Governance contracts and runtime interoperability for copybook-rs feature controls.
//!
//! Single-crate home for governance since #656 Phase F (the former
//! `copybook-governance-contracts`, `copybook-governance-grid`, and
//! `copybook-governance-runtime` packages collapsed here):
//! - Runtime feature flags and support-matrix rows (`copybook-contracts`,
//!   `copybook-support-matrix`).
//! - Static governance bindings between support rows and feature flags
//!   ([`bindings`]).
//! - Runtime evaluation of those bindings ([`runtime`]).

/// Static governance bindings between support rows and feature flags.
pub mod bindings;
/// Runtime evaluation of the static governance bindings.
pub mod runtime;

/// Re-exported feature flag types from the flag contracts.
pub mod feature_flags {
    pub use copybook_contracts::feature_flags::*;
}

/// Re-exported support matrix types from the support-matrix contracts.
pub mod support_matrix {
    pub use copybook_support_matrix::*;
}

pub use feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
    FeatureLifecycle,
};
pub use support_matrix::{
    FeatureId, FeatureSupport, SupportStatus, all_features, find_feature, find_feature_by_id,
};

pub use bindings::{
    GovernanceAudit, GovernanceSummary, GovernedFeatureBinding, audit_governance,
    feature_flags_for_support_id, governance_bindings, summarize_governance,
};

pub use runtime::{
    FeatureGovernanceState, FeatureGovernanceSummary, governance_state_for_support_id,
    governance_states, is_support_runtime_available, runtime_summary, support_states,
};

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_facade_feature_flags_default() {
        // #656 Phase C: LruCache is the only default-enabled flag.
        let flags = FeatureFlags::default();
        assert!(flags.is_enabled(Feature::LruCache));
        assert!(!flags.is_enabled(Feature::RenamesR4R6));
    }

    #[test]
    fn test_facade_builder_enable_disable() {
        let flags = FeatureFlags::builder()
            .enable(Feature::Profiling)
            .disable(Feature::LruCache)
            .build();
        assert!(flags.is_enabled(Feature::Profiling));
        assert!(!flags.is_enabled(Feature::LruCache));
    }

    #[test]
    fn test_facade_support_matrix_lookup() {
        let f = support_matrix::find_feature_by_id(FeatureId::EditedPic);
        assert!(f.is_some());
        assert_eq!(f.unwrap().status, SupportStatus::Supported);
    }

    #[test]
    fn test_facade_governance_bindings_nonempty() {
        let bindings = governance_bindings();
        assert!(!bindings.is_empty());
        assert_eq!(bindings.len(), 7);
    }

    #[test]
    fn test_facade_feature_flags_for_support_id() {
        // #656 Phase C: COMP-1/COMP-2 carry no flag linkage.
        let flags = feature_flags_for_support_id(FeatureId::Comp1Comp2).unwrap();
        assert!(flags.is_empty());
    }

    #[test]
    fn test_facade_summarize_governance() {
        let summary = summarize_governance();
        assert!(summary.all_features_known());
        assert_eq!(summary.total_support_features, 7);
    }

    #[test]
    fn test_facade_governance_states_nonempty() {
        let flags = FeatureFlags::default();
        let states = governance_states(&flags);
        assert!(!states.is_empty());
    }

    #[test]
    fn test_facade_runtime_summary_totals() {
        let flags = FeatureFlags::default();
        let summary = runtime_summary(&flags);
        assert_eq!(summary.total_support_features, 7);
        assert_eq!(summary.mapped_support_features, 7);
        assert!(summary.all_support_rows_present());
    }

    #[test]
    fn test_facade_is_support_runtime_available() {
        let flags = FeatureFlags::default();
        assert!(is_support_runtime_available(
            FeatureId::SignSeparate,
            &flags
        ));
        assert!(is_support_runtime_available(
            FeatureId::Level88Conditions,
            &flags
        ));
    }

    #[test]
    fn test_facade_support_states_nonempty() {
        let states = support_states();
        assert_eq!(states.len(), 7);
        for state in &states {
            assert!(state.runtime_enabled);
        }
    }

    #[test]
    fn test_facade_end_to_end_governance_flow() {
        // Build flags with specific configuration; Level66Renames is the
        // remaining flag-gated binding (#656 Phase C).
        let flags = FeatureFlags::builder()
            .disable(Feature::RenamesR4R6)
            .build();

        // Check specific governance state
        let state = governance_state_for_support_id(FeatureId::Level66Renames, &flags).unwrap();
        assert!(!state.runtime_enabled);
        assert_eq!(state.missing_feature_flags.len(), 1);

        // Stable language entries stay available regardless of flags.
        let comp_state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags).unwrap();
        assert!(comp_state.runtime_enabled);

        // Check overall summary
        let summary = runtime_summary(&flags);
        assert!(summary.has_runtime_unavailable_features());
        assert!(summary.runtime_disabled_features >= 1);
    }

    #[test]
    fn test_facade_feature_lifecycle_accessible() {
        let _ = FeatureLifecycle::Experimental;
        let _ = FeatureLifecycle::Stable;
        let _ = FeatureLifecycle::Deprecated;
    }

    #[test]
    fn test_facade_handle_roundtrip() {
        let handle = FeatureFlagsHandle::new();
        handle.enable(Feature::AdvancedOptimization);
        assert!(handle.is_enabled(Feature::AdvancedOptimization));
        let snap = handle.snapshot();
        assert!(snap.is_enabled(Feature::AdvancedOptimization));
    }
}
