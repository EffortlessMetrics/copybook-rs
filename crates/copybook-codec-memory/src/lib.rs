#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Backward-compatible facade for codec-owned runtime support.
//!
//! The canonical implementation now lives in [`copybook_codec::runtime`].
//! This package remains available for consumers migrating from the 0.5 package
//! layout and continues to forward the stable memory/worker API.

pub use copybook_codec::runtime::{
    DigitBuffer, ScratchBuffers, StreamingProcessor, StreamingProcessorStats, WorkerPool,
    WorkerPoolStats,
};
pub use copybook_codec::runtime::{SequenceRing, SequenceRingStats, SequencedRecord};
