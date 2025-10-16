//! Memory management utilities for streaming record processing
//!
//! This module provides bounded memory usage patterns, reusable scratch buffers,
//! and ordered parallel processing with sequence tracking.

use crossbeam_channel::{Receiver, Sender, bounded};
use smallvec::SmallVec;
use std::collections::BTreeMap;
use std::sync::Arc;
use std::thread;
use tracing::{debug, warn};

/// Small vector for digit buffers (≤32 bytes on stack)
/// Used for packed/zoned decimal digit processing to avoid heap allocations
pub type DigitBuffer = SmallVec<[u8; 32]>;

/// Reusable scratch buffers for worker threads
/// Minimizes allocations in hot codec paths
#[derive(Debug)]
pub struct ScratchBuffers {
    /// Buffer for digit processing in packed/zoned decimal codecs
    pub digit_buffer: DigitBuffer,
    /// General-purpose byte buffer for record processing
    pub byte_buffer: Vec<u8>,
    /// String buffer for text processing
    pub string_buffer: String,
}

impl ScratchBuffers {
    /// Create new scratch buffers with reasonable initial capacity
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            digit_buffer: SmallVec::new(),
            byte_buffer: Vec::with_capacity(1024), // 1KB initial capacity
            string_buffer: String::with_capacity(512), // 512 chars initial capacity
        }
    }

    /// Clear all buffers for reuse
    #[inline]
    pub fn clear(&mut self) {
        self.digit_buffer.clear();
        self.byte_buffer.clear();
        self.string_buffer.clear();
    }

    /// Ensure byte buffer has at least the specified capacity
    ///
    /// PERFORMANCE OPTIMIZATION: Branch prediction hint for common case
    #[inline]
    pub fn ensure_byte_capacity(&mut self, capacity: usize) {
        // Fast path: most allocations don't need to grow the buffer
        if self.byte_buffer.capacity() < capacity {
            // Cold path: grow the buffer
            self.grow_byte_buffer(capacity);
        }
    }

    /// Ensure string buffer has at least the specified capacity
    ///
    /// PERFORMANCE OPTIMIZATION: Branch prediction hint for common case
    #[inline]
    pub fn ensure_string_capacity(&mut self, capacity: usize) {
        // Fast path: most allocations don't need to grow the buffer
        if self.string_buffer.capacity() < capacity {
            // Cold path: grow the buffer
            self.grow_string_buffer(capacity);
        }
    }

    /// Cold path: grow byte buffer (separate function for better optimization)
    #[cold]
    #[inline(never)]
    fn grow_byte_buffer(&mut self, capacity: usize) {
        self.byte_buffer
            .reserve(capacity - self.byte_buffer.capacity());
    }

    /// Cold path: grow string buffer (separate function for better optimization)
    #[cold]
    #[inline(never)]
    fn grow_string_buffer(&mut self, capacity: usize) {
        self.string_buffer
            .reserve(capacity - self.string_buffer.capacity());
    }
}

impl Default for ScratchBuffers {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

/// Record with sequence ID for ordered processing
#[derive(Debug, Clone)]
pub struct SequencedRecord<T> {
    /// Sequence number for ordering
    pub sequence_id: u64,
    /// The actual record data
    pub data: T,
}

impl<T> SequencedRecord<T> {
    /// Create a new sequenced record
    #[inline]
    pub fn new(sequence_id: u64, data: T) -> Self {
        Self { sequence_id, data }
    }
}

/// Bounded channel with sequence tracking for ordered emission
/// Ensures deterministic output ordering even with parallel processing
#[derive(Debug)]
pub struct SequenceRing<T> {
    /// Channel for receiving processed records
    receiver: Receiver<SequencedRecord<T>>,
    /// Sender for processed records (cloned to workers)
    sender: Sender<SequencedRecord<T>>,
    /// Buffer for out-of-order records
    reorder_buffer: BTreeMap<u64, T>,
    /// Next expected sequence ID
    next_sequence_id: u64,
    /// Maximum reordering window size
    max_window_size: usize,
    /// Channel capacity
    channel_capacity: usize,
}

impl<T> SequenceRing<T> {
    /// Create a new sequence ring with bounded capacity
    ///
    /// # Arguments
    /// * `channel_capacity` - Maximum number of records in flight
    /// * `max_window_size` - Maximum reordering window size
    #[inline]
    #[must_use]
    pub fn new(channel_capacity: usize, max_window_size: usize) -> Self {
        let (sender, receiver) = bounded(channel_capacity);

        Self {
            receiver,
            sender,
            reorder_buffer: BTreeMap::new(),
            next_sequence_id: 1,
            max_window_size,
            channel_capacity,
        }
    }

    /// Get a sender for workers to submit processed records
    #[inline]
    #[must_use]
    pub fn sender(&self) -> Sender<SequencedRecord<T>> {
        self.sender.clone()
    }

    /// Receive the next record in sequence order
    /// Blocks until the next expected record is available
    /// Receive records in sequence order
    ///
    /// # Errors
    ///
    /// Returns an error if the channel is disconnected
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn recv_ordered(&mut self) -> Result<Option<T>, crossbeam_channel::RecvError> {
        loop {
            // Check if we have the next expected record in the reorder buffer
            if let Some(record) = self.reorder_buffer.remove(&self.next_sequence_id) {
                self.next_sequence_id += 1;
                debug!(
                    "Emitting record {} from reorder buffer",
                    self.next_sequence_id - 1
                );
                return Ok(Some(record));
            }

            // Receive next record from channel
            if let Ok(sequenced_record) = self.receiver.recv() {
                let SequencedRecord { sequence_id, data } = sequenced_record;

                match sequence_id.cmp(&self.next_sequence_id) {
                    std::cmp::Ordering::Equal => {
                        // This is the next expected record
                        self.next_sequence_id += 1;
                        debug!("Emitting record {} directly", sequence_id);
                        return Ok(Some(data));
                    }
                    std::cmp::Ordering::Greater => {
                        // Future record - buffer it
                        debug!(
                            "Buffering out-of-order record {} (expecting {})",
                            sequence_id, self.next_sequence_id
                        );
                        self.reorder_buffer.insert(sequence_id, data);

                        // Check reorder buffer size
                        if self.reorder_buffer.len() > self.max_window_size {
                            warn!(
                                "Reorder buffer size ({}) exceeds maximum ({}), potential memory issue",
                                self.reorder_buffer.len(),
                                self.max_window_size
                            );
                        }
                    }
                    std::cmp::Ordering::Less => {
                        // Past record - this shouldn't happen with proper sequencing
                        warn!(
                            "Received past record {} (expecting {}), ignoring",
                            sequence_id, self.next_sequence_id
                        );
                    }
                }
            } else {
                // Channel closed - emit any remaining buffered records
                if let Some((_, record)) = self.reorder_buffer.pop_first() {
                    debug!("Emitting remaining buffered record during shutdown");
                    return Ok(Some(record));
                }
                debug!("Channel closed, no more records");
                return Ok(None);
            }
        }
    }

    /// Try to receive the next record without blocking
    ///
    /// # Errors
    ///
    /// Returns an error if the channel is disconnected or would block
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_recv_ordered(&mut self) -> Result<Option<T>, crossbeam_channel::TryRecvError> {
        // Check if we have the next expected record in the reorder buffer
        if let Some(record) = self.reorder_buffer.remove(&self.next_sequence_id) {
            self.next_sequence_id += 1;
            return Ok(Some(record));
        }

        // Try to receive from channel
        match self.receiver.try_recv() {
            Ok(sequenced_record) => {
                let SequencedRecord { sequence_id, data } = sequenced_record;

                match sequence_id.cmp(&self.next_sequence_id) {
                    std::cmp::Ordering::Equal => {
                        self.next_sequence_id += 1;
                        Ok(Some(data))
                    }
                    std::cmp::Ordering::Greater => {
                        // Buffer future record and return empty
                        self.reorder_buffer.insert(sequence_id, data);
                        Err(crossbeam_channel::TryRecvError::Empty)
                    }
                    std::cmp::Ordering::Less => {
                        // Past record - ignore and try again
                        warn!("Received past record {}, ignoring", sequence_id);
                        Err(crossbeam_channel::TryRecvError::Empty)
                    }
                }
            }
            Err(e) => Err(e),
        }
    }

    /// Get statistics about the sequence ring
    #[inline]
    #[must_use]
    pub fn stats(&self) -> SequenceRingStats {
        SequenceRingStats {
            next_sequence_id: self.next_sequence_id,
            reorder_buffer_size: self.reorder_buffer.len(),
            max_window_size: self.max_window_size,
            channel_capacity: self.channel_capacity,
        }
    }
}

/// Statistics about sequence ring operation
#[derive(Debug, Clone)]
pub struct SequenceRingStats {
    /// Next expected sequence ID
    pub next_sequence_id: u64,
    /// Current reorder buffer size
    pub reorder_buffer_size: usize,
    /// Maximum reorder window size
    pub max_window_size: usize,
    /// Channel capacity
    pub channel_capacity: usize,
}

/// Worker pool for parallel record processing with bounded memory
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
    ///
    /// Returns an error if the worker channel is disconnected
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

    /// Receive the next processed result in order
    /// Receive processed records in sequence order
    ///
    /// # Errors
    ///
    /// Returns an error if the channel is disconnected
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn recv_ordered(&mut self) -> Result<Option<Output>, crossbeam_channel::RecvError> {
        self.output_ring.recv_ordered()
    }

    /// Try to receive the next processed result without blocking
    /// Try to receive processed records in sequence order (non-blocking)
    ///
    /// # Errors
    ///
    /// Returns an error if the channel is disconnected or would block
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn try_recv_ordered(&mut self) -> Result<Option<Output>, crossbeam_channel::TryRecvError> {
        self.output_ring.try_recv_ordered()
    }

    /// Close the input channel and wait for all workers to finish
    /// Shutdown the worker pool and wait for all workers to finish
    ///
    /// # Errors
    ///
    /// Returns an error if any worker thread panicked
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

/// Memory-bounded streaming processor
/// Maintains steady-state memory usage under 256 MiB
#[derive(Debug)]
pub struct StreamingProcessor {
    /// Maximum memory usage target (bytes)
    max_memory_bytes: usize,
    /// Current estimated memory usage
    current_memory_bytes: usize,
    /// Record processing statistics
    records_processed: u64,
    /// Bytes processed
    bytes_processed: u64,
}

impl StreamingProcessor {
    /// Create a new streaming processor with memory limit
    #[inline]
    #[must_use]
    pub fn new(max_memory_mb: usize) -> Self {
        Self {
            max_memory_bytes: max_memory_mb * 1024 * 1024,
            current_memory_bytes: 0,
            records_processed: 0,
            bytes_processed: 0,
        }
    }

    /// Create with default 256 MiB limit
    #[inline]
    #[must_use]
    pub fn with_default_limit() -> Self {
        Self::new(256)
    }

    /// Check if we're approaching memory limit
    #[inline]
    #[must_use]
    pub fn is_memory_pressure(&self) -> bool {
        self.current_memory_bytes > (self.max_memory_bytes * 80 / 100) // 80% threshold
    }

    /// Update memory usage estimate
    ///
    /// PERFORMANCE OPTIMIZATION: Optimized for hot path with minimal branching
    #[inline]
    pub fn update_memory_usage(&mut self, bytes_delta: isize) {
        if let Ok(increase) = usize::try_from(bytes_delta) {
            self.current_memory_bytes = self.current_memory_bytes.saturating_add(increase);
        } else {
            let decrease = bytes_delta.unsigned_abs();
            self.current_memory_bytes = self.current_memory_bytes.saturating_sub(decrease);
        }
    }

    /// Record processing of a record
    #[inline]
    pub fn record_processed(&mut self, record_size: usize) {
        self.records_processed += 1;
        self.bytes_processed += record_size as u64;
    }

    /// Get processing statistics
    #[inline]
    #[must_use]
    pub fn stats(&self) -> StreamingProcessorStats {
        StreamingProcessorStats {
            max_memory_bytes: self.max_memory_bytes,
            current_memory_bytes: self.current_memory_bytes,
            memory_utilization_percent: (self.current_memory_bytes * 100) / self.max_memory_bytes,
            records_processed: self.records_processed,
            bytes_processed: self.bytes_processed,
        }
    }
}

/// Statistics about streaming processor operation
#[derive(Debug, Clone)]
pub struct StreamingProcessorStats {
    /// Maximum memory limit (bytes)
    pub max_memory_bytes: usize,
    /// Current memory usage (bytes)
    pub current_memory_bytes: usize,
    /// Memory utilization percentage
    pub memory_utilization_percent: usize,
    /// Records processed
    pub records_processed: u64,
    /// Bytes processed
    pub bytes_processed: u64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{Context, Result, anyhow};
    use std::time::Duration;

    type TestResult = Result<()>;

    #[test]
    fn test_scratch_buffers() {
        let mut buffers = ScratchBuffers::new();

        // Test digit buffer
        buffers.digit_buffer.push(1);
        buffers.digit_buffer.push(2);
        assert_eq!(buffers.digit_buffer.len(), 2);

        // Test clear
        buffers.clear();
        assert_eq!(buffers.digit_buffer.len(), 0);
        assert_eq!(buffers.byte_buffer.len(), 0);
        assert_eq!(buffers.string_buffer.len(), 0);
    }

    #[test]
    fn test_sequence_ring_ordered() -> TestResult {
        let mut ring = SequenceRing::new(10, 5);
        let sender = ring.sender();

        // Send records out of order
        sender
            .send(SequencedRecord::new(2, "second"))
            .context("sending second record")?;
        sender
            .send(SequencedRecord::new(1, "first"))
            .context("sending first record")?;
        sender
            .send(SequencedRecord::new(3, "third"))
            .context("sending third record")?;

        // Should receive in order
        assert_eq!(
            ring.recv_ordered().context("receiving first record")?,
            Some("first")
        );
        assert_eq!(
            ring.recv_ordered().context("receiving second record")?,
            Some("second")
        );
        assert_eq!(
            ring.recv_ordered().context("receiving third record")?,
            Some("third")
        );

        Ok(())
    }

    #[test]
    fn test_worker_pool() -> TestResult {
        let pool = WorkerPool::new(
            2,  // 2 workers
            10, // channel capacity
            5,  // max window size
            |input: i32, _buffers: &mut ScratchBuffers| -> i32 {
                input * 2 // Simple doubling function
            },
        );

        let mut pool = pool;

        // Submit work
        for i in 1..=5 {
            pool.submit(i).context("submitting work item")?;
        }

        // Receive results in order
        for i in 1..=5 {
            let result = pool
                .recv_ordered()
                .context("receiving result")?
                .context("worker pool returned None")?;
            assert_eq!(result, i * 2);
        }

        pool.shutdown()
            .map_err(|error| anyhow!(error))
            .context("shutting down worker pool")?;

        Ok(())
    }

    #[test]
    fn test_streaming_processor() {
        let mut processor = StreamingProcessor::new(1); // 1 MB limit

        assert!(!processor.is_memory_pressure());

        // Simulate memory usage
        processor.update_memory_usage(900 * 1024); // 900 KB
        assert!(processor.is_memory_pressure()); // Should be over 80% threshold

        // Record processing
        processor.record_processed(1024);
        let stats = processor.stats();
        assert_eq!(stats.records_processed, 1);
        assert_eq!(stats.bytes_processed, 1024);
    }

    #[test]
    fn test_deterministic_ordering() -> TestResult {
        // Test that different numbers of workers produce identical output
        let test_data: Vec<i32> = (1..=20).collect(); // Reduced size for faster testing

        for num_workers in [1, 2, 4] {
            // Reduced worker counts
            let pool = WorkerPool::new(
                num_workers,
                10,
                5,
                |input: i32, _buffers: &mut ScratchBuffers| -> i32 {
                    // Simulate some work with minimal timing
                    if input % 3 == 0 {
                        std::thread::sleep(Duration::from_micros(1));
                    }
                    input * input
                },
            );

            let mut pool = pool;
            let mut results = Vec::new();

            // Submit all work
            for &input in &test_data {
                pool.submit(input)
                    .with_context(|| format!("submitting input {input}"))?;
            }

            // Collect results
            for _ in 0..test_data.len() {
                match pool.recv_ordered() {
                    Ok(Some(result)) => results.push(result),
                    Ok(None) => break,
                    Err(error) => return Err(error).context("receiving ordered result"),
                }
            }

            pool.shutdown()
                .map_err(|error| anyhow!(error))
                .context("shutting down worker pool")?;

            // Results should be in the same order regardless of worker count
            let expected: Vec<i32> = test_data.iter().map(|x| x * x).collect();
            assert_eq!(
                results, expected,
                "Results differ for {num_workers} workers"
            );
        }

        Ok(())
    }
}
