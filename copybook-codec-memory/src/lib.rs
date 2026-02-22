// SPDX-License-Identifier: AGPL-3.0-or-later
//! Memory management utilities for streaming record processing
//!
//! This module provides bounded memory usage patterns, reusable scratch buffers,
//! and ordered parallel processing with sequence tracking.
//!
//! # Overview
//!
//! The memory module implements performance-critical memory management patterns
//! for high-throughput COBOL data processing. It provides three key capabilities:
//!
//! 1. **Scratch buffer reuse** ([`ScratchBuffers`]) - Eliminate allocations in hot paths
//! 2. **Deterministic parallel processing** ([`WorkerPool`], [`SequenceRing`]) - Maintain record order
//! 3. **Bounded memory usage** ([`StreamingProcessor`]) - Process multi-GB files with <256 MiB RAM
//!
//! # Performance Impact
//!
//! These utilities enable copybook-rs to achieve:
//! - **205 MiB/s** throughput on DISPLAY-heavy workloads (baseline: 2025-09-30)
//! - **58 MiB/s** throughput on COMP-3-heavy workloads
//! - **<256 MiB** steady-state memory for multi-GB file processing
//! - **Deterministic output** with parallel processing (1-8+ worker threads)
//!
//! # Examples
//!
//! ## Basic Scratch Buffer Usage
//!
//! ```rust
//! use copybook_codec::memory::ScratchBuffers;
//!
//! let mut scratch = ScratchBuffers::new();
//!
//! // Use buffers for processing
//! scratch.digit_buffer.push(5);
//! scratch.byte_buffer.extend_from_slice(b"data");
//! scratch.string_buffer.push_str("text");
//!
//! // Clear for reuse (no deallocation)
//! scratch.clear();
//! ```
//!
//! ## Parallel Processing with Deterministic Output
//!
//! ```rust
//! use copybook_codec::memory::{WorkerPool, ScratchBuffers};
//!
//! // Create worker pool with 4 threads
//! let mut pool = WorkerPool::new(
//!     4,   // num_workers
//!     100, // channel_capacity
//!     50,  // max_window_size
//!     |input: Vec<u8>, _scratch: &mut ScratchBuffers| -> String {
//!         // Process input (runs in parallel)
//!         String::from_utf8_lossy(&input).to_string()
//!     },
//! );
//!
//! // Collect chunks so we know the count
//! let chunks: Vec<Vec<u8>> = get_data_chunks();
//! let num_chunks = chunks.len();
//!
//! // Submit work
//! for chunk in chunks {
//!     pool.submit(chunk).unwrap();
//! }
//!
//! // Receive exactly num_chunks results in order (non-blocking pattern)
//! for _ in 0..num_chunks {
//!     let result = pool.recv_ordered().unwrap().unwrap();
//!     println!("{}", result);
//! }
//!
//! pool.shutdown().unwrap();
//! # fn get_data_chunks() -> Vec<Vec<u8>> { vec![vec![1, 2, 3]] }
//! ```
//!
//! ## Memory-Bounded Streaming
//!
//! ```rust
//! use copybook_codec::memory::StreamingProcessor;
//! # let records: Vec<Vec<u8>> = vec![vec![1, 2, 3]];
//!
//! let mut processor = StreamingProcessor::with_default_limit(); // 256 MiB
//!
//! for record in records {
//!     // Check memory pressure before processing
//!     if processor.is_memory_pressure() {
//!         // Flush buffers or throttle input
//!     }
//!
//!     // Track memory usage
//!     processor.update_memory_usage(record.len() as isize);
//!     processor.record_processed(record.len());
//! }
//!
//! // Get statistics
//! let stats = processor.stats();
//! println!("Processed {} records", stats.records_processed);
//! ```

mod scratch;
mod sequence_ring;
mod streaming;
mod worker_pool;

#[cfg(test)]
mod tests;

pub use scratch::{DigitBuffer, ScratchBuffers};
pub use sequence_ring::{SequenceRing, SequenceRingStats, SequencedRecord};
pub use streaming::{StreamingProcessor, StreamingProcessorStats};
pub use worker_pool::{WorkerPool, WorkerPoolStats};
