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

use copybook_core::{FeatureCategory, FeatureFlags};
use cucumber::World as _;

use crate::world::CopybookWorld;

#[tokio::main]
async fn main() {
    FeatureFlags::set_global(
        FeatureFlags::builder()
            .enable_category(FeatureCategory::Experimental)
            .enable_category(FeatureCategory::Enterprise)
            .build(),
    );
    CopybookWorld::run(concat!(env!("CARGO_MANIFEST_DIR"), "/features")).await;
}
