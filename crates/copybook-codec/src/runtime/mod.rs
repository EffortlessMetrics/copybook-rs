// SPDX-License-Identifier: AGPL-3.0-or-later
//! Codec-owned runtime support for bounded, reusable record processing.
//!
//! The runtime family owns scratch buffers, streaming accounting, and worker
//! coordination. Sequence ordering remains a separate compatibility dependency
//! until issue #654 Slice C moves that implementation into this module family.

mod scratch;
mod streaming;
mod worker_pool;

#[cfg(test)]
mod tests;

pub use scratch::{DigitBuffer, ScratchBuffers};
pub use streaming::{StreamingProcessor, StreamingProcessorStats};
pub use worker_pool::{WorkerPool, WorkerPoolStats};
// Sequence ordering remains externally implemented until issue #654 Slice C.
pub use copybook_sequence_ring::{SequenceRing, SequenceRingStats, SequencedRecord};
