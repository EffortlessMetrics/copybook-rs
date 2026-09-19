// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compile-contract tests for the curated `copybook` facade (#657).
//!
//! Every preferred path must resolve to its true owner, and every 0.6
//! compatibility alias removed in 0.7 must stay out of the facade.

use copybook::{charset, codec, core, error, framing};

/// Preferred facade paths resolve to true-owner items.
#[test]
fn preferred_paths_resolve_to_true_owners() {
    let _ = std::any::type_name::<core::Schema>();
    let _ = std::any::type_name::<error::Error>();
    let _ = std::any::type_name::<error::ErrorCode>();
    let _ = std::any::type_name::<charset::Codepage>();
    let _ = std::any::type_name::<codec::options::DecodeOptions>();
    let _ = std::any::type_name::<codec::options::profile::InterpretationProfile>();
    let _ = std::any::type_name::<framing::fixed::FixedRecordReader<std::io::Cursor<Vec<u8>>>>();
    let _ = std::any::type_name::<framing::rdw::RdwHeader>();
}

/// Retired paths stay out of the facade.
///
/// Restoring `pub mod overflow`/`pub mod utils` (or their dependencies) or
/// any 0.6 compatibility alias removed in 0.7 fails this test; the
/// docs-truth facade-invariant gate enforces the same deny-list
/// independently.
#[test]
fn retired_paths_are_absent() {
    const LIB: &str = include_str!("../src/lib.rs");
    const MANIFEST: &str = include_str!("../Cargo.toml");
    // Top-level declarations only: `framing::{fixed, rdw}` children are
    // indented and must not trip the deny-list.
    let top_level: Vec<&str> = LIB
        .lines()
        .filter(|line| line.starts_with("pub mod "))
        .collect();
    for module in [
        "pub mod overflow",
        "pub mod utils",
        "pub mod error_reporter",
        "pub mod codepage",
        "pub mod contracts",
        "pub mod determinism",
        "pub mod fixed",
        "pub mod options",
        "pub mod overpunch",
        "pub mod rdw",
        "pub mod record_io",
    ] {
        assert!(
            !top_level
                .iter()
                .any(|line| *line == module || line.starts_with(&format!("{module} "))),
            "retired facade module restored: {module}"
        );
    }
    for dep in [
        "copybook-overflow",
        "copybook-utils",
        "copybook-error-reporter",
        "copybook-contracts",
    ] {
        assert!(
            !MANIFEST.contains(dep),
            "retired facade dependency restored: {dep}"
        );
    }
}
