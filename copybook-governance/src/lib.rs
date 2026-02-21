#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

//! Governance contracts and runtime interoperability for copybook-rs feature controls.
//!
//! This crate is the canonical compatibility façade for:
//! - Runtime feature flags and support-matrix rows (`copybook-governance-contracts`).
//! - Static governance bindings between support rows and feature flags (`copybook-governance-grid`).
//! - Runtime evaluation of those bindings (`copybook-governance-runtime`).

pub mod feature_flags {
    pub use copybook_governance_grid::feature_flags::*;
}

pub mod support_matrix {
    pub use copybook_governance_grid::support_matrix::*;
}

pub use feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
    FeatureLifecycle,
};
pub use support_matrix::{FeatureId, FeatureSupport, SupportStatus};

pub use copybook_governance_grid::{
    GovernanceSummary, GovernedFeatureBinding, feature_flags_for_support_id, governance_bindings,
    summarize_governance,
};

pub use copybook_governance_runtime::{
    FeatureGovernanceState, FeatureGovernanceSummary, governance_state_for_support_id,
    governance_states, is_support_runtime_available, runtime_summary, support_states,
};
