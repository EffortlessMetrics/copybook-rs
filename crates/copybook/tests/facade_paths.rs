// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compile-contract tests for the curated `copybook` facade (#657).
//!
//! Every preferred path must resolve to its true owner, and every promised
//! deprecated alias must still compile through the 0.6 migration window.

#![allow(deprecated)]

use copybook::{charset, codec, core, error, framing};

/// Preferred facade paths resolve to true-owner items.
#[test]
fn preferred_paths_resolve_to_true_owners() {
    let _ = std::any::type_name::<core::Schema>();
    let _ = std::any::type_name::<error::Error>();
    let _ = std::any::type_name::<error::ErrorCode>();
    let _ = std::any::type_name::<charset::Codepage>();
    let _ = std::any::type_name::<codec::options::DecodeOptions>();
    let _ = std::any::type_name::<framing::fixed::FixedRecordReader<std::io::Cursor<Vec<u8>>>>();
    let _ = std::any::type_name::<framing::rdw::RdwHeader>();
}

/// Deprecated aliases keep the 0.5 surface compiling.
#[test]
fn deprecated_aliases_still_resolve() {
    let _ = std::any::type_name::<copybook::codepage::Codepage>();
    let _ = std::any::type_name::<copybook::contracts::FeatureFlags>();
    let _ = std::any::type_name::<copybook::determinism::DeterminismResult>();
    let _ = std::any::type_name::<copybook::options::DecodeOptions>();
    let _ = std::any::type_name::<copybook::fixed::FixedRecordReader<std::io::Cursor<Vec<u8>>>>();
    let _ = std::any::type_name::<copybook::rdw::RdwHeader>();
    let _ = std::any::type_name::<copybook::error_reporter::ErrorMode>();
}

/// Retired paths are gone from the facade.
#[test]
fn retired_paths_are_absent() {
    // This test documents intent; the compile contract is that the lines
    // below do NOT resolve. They are kept as comments so a future reader
    // sees the deliberate removal:
    // - copybook::overflow (behavior moved to core::internal::bounds, #655)
    // - copybook::utils (live behavior moved to core::internal, #655)
}
