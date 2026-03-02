// SPDX-License-Identifier: AGPL-3.0-or-later
//! Shared contracts for feature-flag governance.

pub mod feature_flags;

pub use copybook_feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
    FeatureLifecycle, all_features,
};
