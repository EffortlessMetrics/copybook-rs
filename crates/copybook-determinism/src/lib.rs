#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Backward-compatible facade for codec-owned determinism primitives.
//!
//! The canonical implementation now lives in [`copybook_codec::determinism`].
//! This package remains available for consumers migrating from the 0.5 package
//! layout.

pub use copybook_codec::determinism::{
    BLAKE3_HEX_LEN, ByteDiff, DEFAULT_MAX_DIFFS, DeterminismMode, DeterminismResult, blake3_hex,
    compare_outputs, compare_outputs_with_limit, find_byte_differences,
    find_byte_differences_with_limit,
};
