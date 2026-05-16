#![allow(clippy::unwrap_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Performance regression detection for automated CI/CD integration
//!
//! Implements comprehensive performance regression detection with statistical analysis,
//! baseline management, and automated alerting for maintaining copybook-rs performance.

#![allow(dead_code, clippy::missing_errors_doc, clippy::needless_pass_by_value)]
#![allow(
    clippy::must_use_candidate,
    clippy::unused_self,
    clippy::unnecessary_wraps
)]
#![allow(
    clippy::useless_format,
    clippy::module_name_repetitions,
    clippy::uninlined_format_args
)]
#![allow(
    clippy::new_without_default,
    clippy::cast_precision_loss,
    clippy::single_match
)]
#![allow(clippy::wildcard_imports, clippy::redundant_closure_for_method_calls)]
#![allow(
    clippy::manual_midpoint,
    clippy::map_unwrap_or,
    clippy::needless_borrow
)]

mod alerts;
mod analyzer;
mod ci;
mod detector;
mod error;
mod repository;
mod types;
pub mod utils;

pub use error::*;
pub use types::*;

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests;
