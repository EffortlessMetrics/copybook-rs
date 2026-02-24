#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

//! Governance feature mapping contracts for copybook-rs.
//!
//! This crate provides a stable interoperability surface that maps documented COBOL
//! support-matrix entries to runtime feature flags.

pub mod feature_flags {
    pub use copybook_governance_contracts::feature_flags::*;
}

pub mod support_matrix {
    pub use copybook_governance_contracts::support_matrix::*;
}

mod governance_grid;

pub use feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
    FeatureLifecycle,
};
pub use governance_grid::{
    GovernanceSummary, GovernedFeatureBinding, feature_flags_for_support_id, governance_bindings,
    summarize_governance,
};
pub use support_matrix::{FeatureId, FeatureSupport, SupportStatus};
