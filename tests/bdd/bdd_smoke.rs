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
//! Gated BDD smoke tests for copybook-rs using Cucumber/Gherkin.
//!
//! This runner intentionally executes only stable smoke-tagged scenarios so the
//! suite can be integrated into fast CI while still exercising behavior-level
//! workflows end-to-end.

mod helpers;
mod steps;
mod world;

use copybook_core::{FeatureCategory, FeatureFlags};
use cucumber::World as _;

use crate::world::CopybookWorld;

const SMOKE_TAGS: [&str; 4] = ["raw", "metadata", "verify", "projection_smoke"];

#[tokio::main]
async fn main() {
    FeatureFlags::set_global(
        FeatureFlags::builder()
            .enable_category(FeatureCategory::Experimental)
            .enable_category(FeatureCategory::Enterprise)
            .build(),
    );

    let features_root = concat!(env!("CARGO_MANIFEST_DIR"), "/features");

    CopybookWorld::cucumber()
        .with_default_cli()
        .filter_run_and_exit(features_root, |feature, _rule, scenario| {
            feature
                .tags
                .iter()
                .chain(scenario.tags.iter())
                .any(|tag| SMOKE_TAGS.iter().any(|candidate| tag == candidate))
        })
        .await;
}
