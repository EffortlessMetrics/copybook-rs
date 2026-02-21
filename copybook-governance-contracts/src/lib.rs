#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]

//! Contract façade for governance interoperability.
//!
//! This crate keeps the cross-cutting governance contract imports small and stable
//! for downstream crates.

pub mod feature_flags {
    pub use copybook_contracts::feature_flags::*;
}

pub mod support_matrix {
    pub use copybook_support_matrix::*;
}

pub use feature_flags::FeatureLifecycle;
pub use feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
};
pub use support_matrix::{
    FeatureId, FeatureSupport, SupportStatus, all_features, find_feature, find_feature_by_id,
};
