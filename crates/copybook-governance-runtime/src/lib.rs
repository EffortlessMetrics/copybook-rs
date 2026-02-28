#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

//! Runtime governance evaluation for copybook-rs feature controls.
//!
//! This crate turns static governance mappings into runtime state by evaluating
//! active feature flags against support-matrix rows.

pub mod feature_flags {
    pub use copybook_governance_grid::feature_flags::*;
}

pub mod support_matrix {
    pub use copybook_governance_grid::support_matrix::*;
}

use serde::Serialize;

pub use copybook_governance_grid::{
    GovernanceSummary, GovernedFeatureBinding, feature_flags_for_support_id, governance_bindings,
    summarize_governance,
};
pub use feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
    FeatureLifecycle,
};
pub use support_matrix::{FeatureId, FeatureSupport, SupportStatus};

/// Runtime-aware governance state for a support matrix feature row.
#[derive(Debug, Clone, Serialize)]
#[non_exhaustive]
pub struct FeatureGovernanceState {
    /// Canonical support feature identifier from the support matrix.
    pub support_id: FeatureId,
    /// Human-readable support feature name.
    pub support_name: &'static str,
    /// Short support feature description.
    pub support_description: &'static str,
    /// Static support status from the support matrix.
    pub support_status: SupportStatus,
    /// Reference to canonical docs row, when available.
    pub doc_ref: Option<&'static str>,
    /// Why this support item is linked to feature flags.
    pub rationale: &'static str,
    /// Runtime flags required to express this feature in an environment.
    pub required_feature_flags: &'static [Feature],
    /// Runtime flags currently disabling this support item.
    pub missing_feature_flags: Vec<Feature>,
    /// Whether this support feature is currently fully enabled by active flags.
    pub runtime_enabled: bool,
}

impl FeatureGovernanceState {
    /// Build a non-governed runtime state row directly from static support metadata.
    #[inline]
    #[must_use]
    pub fn from_support(feature: &'static FeatureSupport) -> Self {
        Self {
            support_id: feature.id,
            support_name: feature.name,
            support_description: feature.description,
            support_status: feature.status,
            doc_ref: feature.doc_ref,
            rationale: "No runtime governance mapping requested.",
            required_feature_flags: &[],
            missing_feature_flags: Vec::new(),
            runtime_enabled: true,
        }
    }
}

/// Summary of static + runtime governance coverage.
#[derive(Debug, Clone, Copy, Serialize)]
#[non_exhaustive]
pub struct FeatureGovernanceSummary {
    /// Total number of support-matrix feature entries.
    pub total_support_features: usize,
    /// Number of support features with at least one governance binding.
    pub mapped_support_features: usize,
    /// Total number of feature-flag bindings across all entries.
    pub total_linked_feature_flags: usize,
    /// Number of features that are currently enabled at runtime.
    pub runtime_enabled_features: usize,
    /// Number of features that are currently disabled at runtime.
    pub runtime_disabled_features: usize,
}

impl FeatureGovernanceSummary {
    /// Returns `true` when every support feature has at least one governance row.
    #[inline]
    #[must_use]
    pub const fn all_support_rows_present(&self) -> bool {
        self.total_support_features == self.mapped_support_features
    }

    /// Returns `true` when at least one feature is disabled at runtime.
    #[inline]
    #[must_use]
    pub const fn has_runtime_unavailable_features(&self) -> bool {
        self.runtime_disabled_features > 0
    }
}

/// Return support rows without applying runtime governance mapping.
#[inline]
#[must_use]
pub fn support_states() -> Vec<FeatureGovernanceState> {
    support_matrix::all_features()
        .iter()
        .map(FeatureGovernanceState::from_support)
        .collect()
}

/// Build full governance state rows for a specific runtime flag set.
#[inline]
#[must_use]
pub fn governance_states(feature_flags: &FeatureFlags) -> Vec<FeatureGovernanceState> {
    governance_bindings()
        .iter()
        .filter_map(|binding| governance_state_for_support_id(binding.support_id, feature_flags))
        .collect()
}

/// Resolve runtime governance state for a single support item.
#[inline]
#[must_use]
pub fn governance_state_for_support_id(
    support_id: FeatureId,
    feature_flags: &FeatureFlags,
) -> Option<FeatureGovernanceState> {
    let binding = governance_bindings()
        .iter()
        .find(|entry| entry.support_id == support_id)?;
    let support = support_matrix::find_feature_by_id(support_id)?;

    let mut missing_feature_flags = Vec::new();
    for flag in binding.feature_flags {
        if !feature_flags.is_enabled(*flag) {
            missing_feature_flags.push(*flag);
        }
    }

    let runtime_enabled = missing_feature_flags.is_empty();
    Some(FeatureGovernanceState {
        support_id,
        support_name: support.name,
        support_description: support.description,
        support_status: support.status,
        doc_ref: support.doc_ref,
        rationale: binding.rationale,
        required_feature_flags: binding.feature_flags,
        missing_feature_flags,
        runtime_enabled,
    })
}

/// Evaluate whether a support feature is runtime-available with active flags.
#[inline]
#[must_use]
pub fn is_support_runtime_available(support_id: FeatureId, feature_flags: &FeatureFlags) -> bool {
    governance_state_for_support_id(support_id, feature_flags)
        .is_some_and(|state| state.runtime_enabled)
}

/// Aggregate runtime availability with static governance coverage metadata.
#[inline]
#[must_use]
pub fn runtime_summary(feature_flags: &FeatureFlags) -> FeatureGovernanceSummary {
    let base = summarize_governance();
    let states = governance_states(feature_flags);
    let runtime_enabled_features = states.iter().filter(|state| state.runtime_enabled).count();

    FeatureGovernanceSummary {
        total_support_features: base.total_support_features,
        mapped_support_features: base.mapped_support_features,
        total_linked_feature_flags: base.total_linked_feature_flags,
        runtime_enabled_features,
        runtime_disabled_features: states.len().saturating_sub(runtime_enabled_features),
    }
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn test_support_states_nonempty() {
        let states = support_states();
        assert!(!states.is_empty());
        assert!(states.iter().all(|state| state.runtime_enabled));
    }

    #[test]
    fn test_runtime_state_for_supported_feature() {
        let flags = FeatureFlags::default();
        let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags)
            .expect("state should exist for mapped feature");
        assert_eq!(state.support_id, FeatureId::Comp1Comp2);
        assert!(state.runtime_enabled);
        assert_eq!(state.required_feature_flags.len(), 2);
    }

    #[test]
    fn test_runtime_state_reports_missing_feature_flags() {
        let flags = FeatureFlags::builder()
            .disable(Feature::Comp1)
            .disable(Feature::Comp2)
            .build();
        let state = governance_state_for_support_id(FeatureId::Comp1Comp2, &flags)
            .expect("state should exist for mapped feature");
        assert!(!state.runtime_enabled);
        assert_eq!(state.missing_feature_flags.len(), 2);
    }

    #[test]
    fn test_runtime_summary_counts() {
        let flags = FeatureFlags::builder()
            .disable(Feature::SignSeparate)
            .build();
        let summary = runtime_summary(&flags);
        assert!(summary.total_support_features >= 1);
        assert!(summary.mapped_support_features <= summary.total_support_features);
        assert!(summary.runtime_enabled_features <= summary.total_support_features);
    }

    #[test]
    fn test_is_support_runtime_available() {
        let flags = FeatureFlags::builder()
            .enable(Feature::SignSeparate)
            .disable(Feature::Comp1)
            .build();
        assert!(is_support_runtime_available(
            FeatureId::SignSeparate,
            &flags
        ));
        assert!(!is_support_runtime_available(FeatureId::Comp1Comp2, &flags));
    }

    #[test]
    fn test_support_id_lookup() {
        assert!(support_matrix::find_feature_by_id(FeatureId::Level88Conditions).is_some());
        assert!(support_matrix::find_feature_by_id(FeatureId::NestedOdo).is_some());
        assert!(support_matrix::find_feature_by_id(FeatureId::SignSeparate).is_some());
    }

    #[test]
    fn test_support_states_count_matches_all_features() {
        let states = support_states();
        assert_eq!(states.len(), support_matrix::all_features().len());
    }

    #[test]
    fn test_support_states_all_have_empty_required_flags() {
        for state in support_states() {
            assert!(
                state.required_feature_flags.is_empty(),
                "support_states should have no required flags for {:?}",
                state.support_id
            );
            assert!(state.missing_feature_flags.is_empty());
        }
    }

    #[test]
    fn test_governance_states_with_all_defaults() {
        let flags = FeatureFlags::default();
        let states = governance_states(&flags);
        assert!(!states.is_empty());
        // With defaults, SignSeparate/Comp1/Comp2 are enabled, RenamesR4R6 is not
        let renames_state = states
            .iter()
            .find(|s| s.support_id == FeatureId::Level66Renames)
            .expect("Level66Renames should be in governance states");
        assert!(!renames_state.runtime_enabled);
    }

    #[test]
    fn test_governance_states_with_all_features_enabled() {
        let mut flags = FeatureFlags::default();
        for feature in feature_flags::all_features() {
            flags.enable(feature);
        }
        let states = governance_states(&flags);
        for state in &states {
            assert!(
                state.runtime_enabled,
                "{:?} should be runtime enabled when all flags on",
                state.support_id
            );
            assert!(state.missing_feature_flags.is_empty());
        }
    }

    #[test]
    fn test_governance_states_with_all_features_disabled() {
        let mut flags = FeatureFlags::default();
        for feature in feature_flags::all_features() {
            flags.disable(feature);
        }
        let states = governance_states(&flags);
        // Features with non-empty required_feature_flags should be disabled
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
    fn test_runtime_summary_with_all_enabled() {
        let mut flags = FeatureFlags::default();
        for feature in feature_flags::all_features() {
            flags.enable(feature);
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
    fn test_runtime_summary_all_support_rows_present() {
        let flags = FeatureFlags::default();
        let summary = runtime_summary(&flags);
        assert!(summary.all_support_rows_present());
        assert_eq!(summary.total_support_features, 7);
        assert_eq!(summary.mapped_support_features, 7);
    }

    #[test]
    fn test_from_support_sets_no_governance_rationale() {
        let feature = support_matrix::find_feature_by_id(FeatureId::EditedPic).unwrap();
        let state = FeatureGovernanceState::from_support(feature);
        assert_eq!(state.rationale, "No runtime governance mapping requested.");
        assert!(state.runtime_enabled);
        assert!(state.required_feature_flags.is_empty());
        assert!(state.missing_feature_flags.is_empty());
    }

    #[test]
    fn test_governance_state_for_support_id_preserves_metadata() {
        let flags = FeatureFlags::default();
        let state = governance_state_for_support_id(FeatureId::SignSeparate, &flags).unwrap();
        assert_eq!(state.support_id, FeatureId::SignSeparate);
        assert!(!state.support_name.is_empty());
        assert!(!state.support_description.is_empty());
        assert!(state.doc_ref.is_some());
        assert!(!state.rationale.is_empty());
    }

    #[test]
    fn test_is_support_runtime_available_for_ungoverned_feature() {
        // Level88 has no feature flags, so it's always available
        let flags = FeatureFlags::default();
        let state = governance_state_for_support_id(FeatureId::Level88Conditions, &flags).unwrap();
        assert!(state.runtime_enabled);
        assert!(state.required_feature_flags.is_empty());
    }

    #[test]
    fn test_runtime_summary_disabled_count_matches_expectations() {
        let flags = FeatureFlags::builder()
            .disable(Feature::SignSeparate)
            .disable(Feature::Comp1)
            .disable(Feature::Comp2)
            .build();
        let summary = runtime_summary(&flags);
        // SignSeparate disabled -> SignSeparate row disabled
        // Comp1+Comp2 disabled -> Comp1Comp2 row disabled
        // RenamesR4R6 not default-enabled -> Level66Renames row disabled
        assert!(summary.runtime_disabled_features >= 3);
        assert!(summary.has_runtime_unavailable_features());
    }
}
