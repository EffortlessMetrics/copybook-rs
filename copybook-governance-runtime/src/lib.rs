#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

//! Compatibility facade for runtime governance evaluation.
//!
//! Runtime evaluation logic now lives in `copybook-governance-eval` to keep
//! this crate thin and focused on stable public re-exports.

pub use copybook_governance_eval::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
    FeatureGovernanceState, FeatureGovernanceSummary, FeatureId, FeatureLifecycle, FeatureSupport,
    GovernanceSummary, GovernedFeatureBinding, SupportStatus, feature_flags_for_support_id,
    governance_bindings, governance_state_for_support_id, governance_states,
    is_support_runtime_available, runtime_summary, summarize_governance, support_states,
};

pub mod feature_flags {
    pub use copybook_governance_eval::feature_flags::*;
}

pub mod support_matrix {
    pub use copybook_governance_eval::support_matrix::*;
}
