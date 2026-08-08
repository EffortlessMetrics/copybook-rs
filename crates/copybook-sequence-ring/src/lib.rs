#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Backward-compatible facade for codec-owned sequence ordering.
//!
//! The canonical implementation lives in [`copybook_codec::runtime`]. This
//! package remains available for consumers migrating from the 0.5 package
//! layout and contains no sequence-ring implementation of its own.

pub use copybook_codec::runtime::{SequenceRing, SequenceRingStats, SequencedRecord};
