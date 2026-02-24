// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_sequence_ring::{SequenceRing, SequenceRingStats, SequencedRecord};
use crossbeam_channel::{Sender, bounded};
use std::sync::Arc;
use std::thread;
use tracing::{debug, warn};

use super::ScratchBuffers;

/// Worker pool for parallel record processing with bounded memory
///
/// Manages a pool of worker threads that process records in parallel while
/// maintaining deterministic output ordering. Combines [`SequenceRing`] for
/// ordered emission with per-worker [`ScratchBuffers`] for allocation-free
/// processing.
///
/// # Key Features
///
/// - **Deterministic output** - Records emitted in original input order
/// - **Bounded memory** - Fixed channel capacity prevents unbounded buffering
/// - **Worker-local buffers** - Each worker has dedicated scratch buffers
/// - **Automatic cleanup** - Workers terminated gracefully on shutdown
///
/// # Architecture
///
/// ```text
/// Input → WorkerPool::submit() → [Worker 1] → SequenceRing → recv_ordered() → Output
///                                 [Worker 2] ↗            ↘
///                                 [Worker N] ↗
/// ```
///
/// # Performance Tuning
///
/// - **`num_workers`**: Match CPU core count (or 2x for I/O-bound work)
/// - **`channel_capacity`**: 2-4x worker count for good pipeline depth
/// - **`max_window_size`**: `channel_capacity` / 2 to allow processing variance
///
/// # Examples
///
/// ## Basic Usage
///
/// ```rust
/// use copybook_codec_memory::{WorkerPool, ScratchBuffers};
///
/// let mut pool = WorkerPool::new(
///     4,   // 4 worker threads
///     16,  // 16 records in flight
///     8,   // 8 max reorder window
///     |input: i32, _scratch: &mut ScratchBuffers| -> i32 {
///         input * 2 // Processing function
///     },
/// );
///
/// // Submit work
/// for i in 1..=10 {
///     pool.submit(i).unwrap();
/// }
///
/// // Receive results in order
/// for i in 1..=10 {
///     let result = pool.recv_ordered().unwrap().unwrap();
///     assert_eq!(result, i * 2);
/// }
///
/// pool.shutdown().unwrap();
/// ```
///
/// ## COBOL Record Processing
///
/// ```ignore
/// use copybook_codec_memory::{WorkerPool, ScratchBuffers};
/// use copybook_codec::{decode_record_with_scratch, DecodeOptions};
/// use copybook_core::{parse_copybook, Schema};
/// use std::sync::Arc;
///
/// let copybook = "01 RECORD.\n   05 FIELD PIC X(10).";
/// let schema = Arc::new(parse_copybook(copybook).unwrap());
/// let options = DecodeOptions::new();
///
/// let schema_clone = Arc::clone(&schema);
/// let mut pool = WorkerPool::new(
///     4, 100, 50,
///     move |record_data: Vec<u8>, scratch: &mut ScratchBuffers| -> String {
///         decode_record_with_scratch(&schema_clone, &record_data, &options, scratch)
///             .unwrap()
///             .to_string()
///     },
/// );
///
/// // Collect records so we know the count
/// let records: Vec<Vec<u8>> = get_cobol_records();
/// let num_records = records.len();
///
/// // Submit COBOL records for parallel processing
/// for record in records {
///     pool.submit(record).unwrap();
/// }
///
/// // Receive exactly num_records JSON results in order
/// for _ in 0..num_records {
///     let json = pool.recv_ordered().unwrap().unwrap();
///     println!("{}", json);
/// }
///
/// pool.shutdown().unwrap();
/// // Return one EBCDIC record (0xF1 repeated = '1' in EBCDIC)
/// # fn get_cobol_records() -> Vec<Vec<u8>> { vec![vec![0xF1; 10]] }
/// ```
#[derive(Debug)]
pub struct WorkerPool<Input, Output> {
    /// Input channel for work items
    input_sender: Sender<SequencedRecord<Input>>,
    /// Output sequence ring for ordered results
    output_ring: SequenceRing<Output>,
    /// Worker thread handles
    worker_handles: Vec<thread::JoinHandle<()>>,
    /// Next sequence ID to assign
    next_input_sequence: u64,
}

impl<Input, Output> WorkerPool<Input, Output>
where
    Input: Send + 'static,
    Output: Send + 'static,
{
    /// Create a new worker pool
    ///
    /// # Arguments
    /// * `num_workers` - Number of worker threads
    /// * `channel_capacity` - Maximum records in flight
    /// * `max_window_size` - Maximum reordering window
    /// * `worker_fn` - Function to process each record
    #[inline]
    #[must_use]
    pub fn new<F>(
        num_workers: usize,
        channel_capacity: usize,
        max_window_size: usize,
        worker_fn: F,
    ) -> Self
    where
        F: Fn(Input, &mut ScratchBuffers) -> Output + Send + Sync + Clone + 'static,
    {
        let (input_sender, input_receiver) = bounded(channel_capacity);
        let output_ring = SequenceRing::new(channel_capacity, max_window_size);
        let output_sender = output_ring.sender();

        let worker_fn = Arc::new(worker_fn);
        let mut worker_handles = Vec::with_capacity(num_workers);

        // Spawn worker threads
        for worker_id in 0..num_workers {
            let input_receiver = input_receiver.clone();
            let output_sender = output_sender.clone();
            let worker_fn = Arc::clone(&worker_fn);

            let handle = thread::spawn(move || {
                let mut scratch_buffers = ScratchBuffers::new();
                debug!("Worker {} started", worker_id);

                while let Ok(sequenced_input) = input_receiver.recv() {
                    let SequencedRecord {
                        sequence_id,
                        data: input,
                    } = sequenced_input;

                    // Clear scratch buffers for reuse
                    scratch_buffers.clear();

                    // Process the record
                    let output = worker_fn(input, &mut scratch_buffers);

                    // Send result with sequence ID
                    let sequenced_output = SequencedRecord::new(sequence_id, output);
                    if output_sender.send(sequenced_output).is_err() {
                        debug!("Worker {} output channel closed", worker_id);
                        break;
                    }
                }

                debug!("Worker {} finished", worker_id);
            });

            worker_handles.push(handle);
        }

        Self {
            input_sender,
            output_ring,
            worker_handles,
            next_input_sequence: 1,
        }
    }

    /// Submit input for processing
    ///
    /// # Errors
    /// Returns an error if the worker channel is disconnected.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn submit(
        &mut self,
        input: Input,
    ) -> Result<(), crossbeam_channel::SendError<SequencedRecord<Input>>> {
        let sequenced_input = SequencedRecord::new(self.next_input_sequence, input);
        self.next_input_sequence += 1;
        self.input_sender.send(sequenced_input)
    }

    /// Receive the next processed result in order.
    ///
    /// # Errors
    /// Returns an error if the channel is disconnected.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn recv_ordered(&mut self) -> Result<Option<Output>, crossbeam_channel::RecvError> {
        self.output_ring.recv_ordered()
    }

    /// Try to receive the next processed result without blocking.
    ///
    /// # Errors
    /// Returns an error if the channel is disconnected or would block.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_recv_ordered(&mut self) -> Result<Option<Output>, crossbeam_channel::TryRecvError> {
        self.output_ring.try_recv_ordered()
    }

    /// Close the input channel and wait for all workers to finish.
    ///
    /// # Errors
    /// Returns an error if any worker thread panicked.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn shutdown(self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        // Close input channel
        drop(self.input_sender);

        // Wait for all workers to finish
        for (i, handle) in self.worker_handles.into_iter().enumerate() {
            if let Err(e) = handle.join() {
                warn!("Worker {} panicked: {:?}", i, e);
            }
        }

        Ok(())
    }

    /// Get statistics about the worker pool
    #[inline]
    #[must_use]
    pub fn stats(&self) -> WorkerPoolStats {
        WorkerPoolStats {
            num_workers: self.worker_handles.len(),
            next_input_sequence: self.next_input_sequence,
            sequence_ring_stats: self.output_ring.stats(),
        }
    }
}

/// Statistics about worker pool operation
#[derive(Debug, Clone)]
pub struct WorkerPoolStats {
    /// Number of worker threads
    pub num_workers: usize,
    /// Next input sequence ID to assign
    pub next_input_sequence: u64,
    /// Sequence ring statistics
    pub sequence_ring_stats: SequenceRingStats,
}
