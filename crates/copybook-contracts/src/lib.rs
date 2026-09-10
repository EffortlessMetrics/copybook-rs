#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Shared contracts for feature-flag governance.
//!
//! Implementation-free forwarder since #656 Phase F: the flag types live in
//! `copybook-core`; this crate re-exports them through the 0.6 migration window.

/// Feature flag definitions, lifecycle states, and governance builder.
pub mod feature_flags {
    pub use copybook_core::feature_flags::*;
}

pub use feature_flags::{
    Feature, FeatureCategory, FeatureFlags, FeatureFlagsBuilder, FeatureFlagsHandle,
    FeatureLifecycle,
};
