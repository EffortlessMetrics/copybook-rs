// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::unused_async,
    clippy::expect_fun_call,
    clippy::uninlined_format_args,
    clippy::map_unwrap_or,
    clippy::match_same_arms,
    clippy::single_char_pattern,
    clippy::unreachable,
    clippy::panic,
    clippy::if_same_then_else,
    unused_variables,
    dead_code,
    unused_imports
)]
//! BDD tests for copybook-rs using Cucumber/Gherkin
//!
//! This module provides comprehensive BDD test coverage for the copybook-rs library,
//! including copybook parsing, encoding/decoding, field projection, dialect processing,
//! determinism validation, and enterprise audit system features.

mod helpers;
mod steps;
mod world;

use copybook_governance::{FeatureCategory, FeatureFlags};
use cucumber::World as _;

use crate::world::CopybookWorld;

/// Explicit per-scenario flags for the BDD harness (#656 Phase D: the
/// process-global flag instance is gone; each scenario world carries its
/// own resolved value).
pub(crate) fn harness_feature_flags() -> FeatureFlags {
    FeatureFlags::builder()
        .enable_category(FeatureCategory::Experimental)
        .enable_category(FeatureCategory::Enterprise)
        .build()
}

#[tokio::main]
async fn main() {
    CopybookWorld::cucumber()
        .with_default_cli()
        .before(|_, _, _, world| {
            Box::pin(async move {
                *world = CopybookWorld {
                    feature_flags: harness_feature_flags(),
                    ..Default::default()
                };
            })
        })
        .run_and_exit(concat!(env!("CARGO_MANIFEST_DIR"), "/features"))
        .await;
}
