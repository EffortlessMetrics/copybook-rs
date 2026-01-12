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
//! // Submit work
//! for chunk in get_data_chunks() {
//!     pool.submit(chunk).unwrap();
//! }
//!
//! // Receive results in original order
//! while let Ok(Some(result)) = pool.recv_ordered() {
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
//! # let records: Vec<Vec<u8>> = vec![];
//! ```

use crossbeam_channel::{Receiver, Sender, bounded};
use smallvec::SmallVec;
use std::collections::BTreeMap;
use std::sync::Arc;
use std::thread;
use tracing::{debug, warn};

/// Small vector for digit buffers (≤32 bytes on stack)
///
/// Stack-allocated buffer used for packed/zoned decimal digit processing.
/// The 32-byte inline capacity handles typical COBOL numeric fields (up to
/// PIC S9(31)) without heap allocation.
pub type DigitBuffer = SmallVec<[u8; 32]>;

/// Reusable scratch buffers for worker threads
///
/// Pre-allocated buffers that can be reused across multiple record processing
/// operations to minimize heap allocations in hot codec paths. This is a critical
/// performance optimization that eliminates allocation overhead during high-throughput
/// data processing.
///
/// # Usage Pattern
///
/// 1. Create once per worker thread: [`ScratchBuffers::new()`]
/// 2. Use buffers during record processing
/// 3. Clear buffers after each record: [`clear()`](ScratchBuffers::clear)
/// 4. Reuse for next record (no reallocation)
///
/// # Performance Benefits
///
/// - **Zero allocations** in steady-state processing (after initial capacity growth)
/// - **CPU cache-friendly** - buffers stay hot in L1/L2 cache
/// - **Reduced GC pressure** - minimal heap churn
/// - **Thread-local** - no synchronization overhead
///
/// # Buffer Purposes
///
/// - **`digit_buffer`** - Packed/zoned decimal digit extraction (stack-allocated up to 32 bytes)
/// - **`byte_buffer`** - General-purpose byte storage (EBCDIC conversion, field data)
/// - **`string_buffer`** - UTF-8 text processing (field values, JSON strings)
///
/// # Examples
///
/// ## Basic Usage
///
/// ```rust
/// use copybook_codec::memory::ScratchBuffers;
///
/// let mut scratch = ScratchBuffers::new();
///
/// // Use buffers for processing
/// scratch.digit_buffer.push(5);
/// scratch.byte_buffer.extend_from_slice(b"EBCDIC data");
/// scratch.string_buffer.push_str("converted text");
///
/// // Clear for next record (no deallocation)
/// scratch.clear();
/// assert_eq!(scratch.digit_buffer.len(), 0);
/// assert_eq!(scratch.byte_buffer.len(), 0);
/// assert_eq!(scratch.string_buffer.len(), 0);
/// ```
///
/// ## Integration with Codec Functions
///
/// ```rust
/// use copybook_codec::memory::ScratchBuffers;
/// use copybook_codec::{decode_record_with_scratch, DecodeOptions};
/// use copybook_core::parse_copybook;
///
/// let copybook = "01 RECORD.\n   05 FIELD PIC X(10).";
/// let schema = parse_copybook(copybook).unwrap();
/// let data = vec![0xF1; 10]; // EBCDIC '1' repeated
///
/// let mut scratch = ScratchBuffers::new();
/// let options = DecodeOptions::new();
///
/// // Process records in loop (reusing scratch buffers)
/// for _ in 0..1000 {
///     let _value = decode_record_with_scratch(&schema, &data, &options, &mut scratch).unwrap();
///     scratch.clear(); // Reuse buffers for next record
/// }
/// ```
///
/// ## Capacity Management
///
/// ```rust
/// use copybook_codec::memory::ScratchBuffers;
///
/// let mut scratch = ScratchBuffers::new();
///
/// // Pre-allocate for large records
/// scratch.ensure_byte_capacity(8192);  // 8 KB record size
/// scratch.ensure_string_capacity(4096); // 4 KB string fields
///
/// // Buffers now have sufficient capacity for processing
/// assert!(scratch.byte_buffer.capacity() >= 8192);
/// assert!(scratch.string_buffer.capacity() >= 4096);
/// ```
#[derive(Debug)]
pub struct ScratchBuffers {
    /// Buffer for digit processing in packed/zoned decimal codecs
    ///
    /// Stack-allocated up to 32 bytes (typical COBOL numerics), then spills to heap.
    pub digit_buffer: DigitBuffer,

    /// General-purpose byte buffer for record processing
    ///
    /// Used for EBCDIC conversion, field extraction, and intermediate data storage.
    /// Initial capacity: 1 KB.
    pub byte_buffer: Vec<u8>,

    /// String buffer for text processing
    ///
    /// Used for UTF-8 text fields, JSON string construction, and character conversion.
    /// Initial capacity: 512 characters.
    pub string_buffer: String,
}

impl ScratchBuffers {
    /// Create new scratch buffers with reasonable initial capacity
    ///
    /// Allocates buffers with initial capacities optimized for typical COBOL records:
    /// - `digit_buffer`: Stack-allocated (no heap until >32 bytes)
    /// - `byte_buffer`: 1 KB initial heap capacity
    /// - `string_buffer`: 512 characters initial heap capacity
    ///
    /// These defaults handle most COBOL record layouts without reallocation.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::ScratchBuffers;
    ///
    /// let scratch = ScratchBuffers::new();
    /// assert_eq!(scratch.digit_buffer.len(), 0);
    /// assert!(scratch.byte_buffer.capacity() >= 1024);
    /// assert!(scratch.string_buffer.capacity() >= 512);
    /// ```
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
    ///
    /// Resets buffer lengths to zero without deallocating memory. This is the
    /// key operation for buffer reuse - call after each record to prepare for
    /// the next processing iteration.
    ///
    /// **Performance note**: This is O(1) and does not touch buffer capacity.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::ScratchBuffers;
    ///
    /// let mut scratch = ScratchBuffers::new();
    ///
    /// // Use buffers
    /// scratch.digit_buffer.push(5);
    /// scratch.byte_buffer.extend_from_slice(b"data");
    /// scratch.string_buffer.push_str("text");
    ///
    /// // Clear for reuse (capacity unchanged)
    /// let byte_capacity = scratch.byte_buffer.capacity();
    /// scratch.clear();
    ///
    /// assert_eq!(scratch.digit_buffer.len(), 0);
    /// assert_eq!(scratch.byte_buffer.len(), 0);
    /// assert_eq!(scratch.string_buffer.len(), 0);
    /// assert_eq!(scratch.byte_buffer.capacity(), byte_capacity);
    /// ```
    #[inline]
    pub fn clear(&mut self) {
        self.digit_buffer.clear();
        self.byte_buffer.clear();
        self.string_buffer.clear();
    }

    /// Ensure byte buffer has at least the specified capacity
    ///
    /// Pre-allocates byte buffer capacity to avoid reallocation during processing.
    /// Use this when you know the maximum record size in advance.
    ///
    /// **Performance optimization**: Fast-path optimized for common case where
    /// capacity is already sufficient. Growth only occurs when needed.
    ///
    /// # Arguments
    ///
    /// * `capacity` - Minimum required capacity in bytes
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::ScratchBuffers;
    ///
    /// let mut scratch = ScratchBuffers::new();
    ///
    /// // Pre-allocate for 8 KB records
    /// scratch.ensure_byte_capacity(8192);
    /// assert!(scratch.byte_buffer.capacity() >= 8192);
    ///
    /// // No reallocation on subsequent calls with same/lower capacity
    /// scratch.ensure_byte_capacity(4096); // No-op, already sufficient
    /// ```
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
    /// Pre-allocates string buffer capacity to avoid reallocation during text processing.
    /// Use this when you know the maximum string field size in advance.
    ///
    /// **Performance optimization**: Fast-path optimized for common case where
    /// capacity is already sufficient. Growth only occurs when needed.
    ///
    /// # Arguments
    ///
    /// * `capacity` - Minimum required capacity in characters (not bytes)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::ScratchBuffers;
    ///
    /// let mut scratch = ScratchBuffers::new();
    ///
    /// // Pre-allocate for 4096 character fields
    /// scratch.ensure_string_capacity(4096);
    /// assert!(scratch.string_buffer.capacity() >= 4096);
    ///
    /// // No reallocation on subsequent calls with same/lower capacity
    /// scratch.ensure_string_capacity(2048); // No-op, already sufficient
    /// ```
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
///
/// Wraps a record with a monotonically increasing sequence ID to enable
/// deterministic output ordering even when processing happens in parallel.
/// This is essential for maintaining data integrity when using multi-threaded
/// record processing.
///
/// # Examples
///
/// ```rust
/// use copybook_codec::memory::SequencedRecord;
///
/// let record1 = SequencedRecord::new(1, "first record");
/// let record2 = SequencedRecord::new(2, "second record");
///
/// assert_eq!(record1.sequence_id, 1);
/// assert_eq!(record2.data, "second record");
/// ```
#[derive(Debug, Clone)]
pub struct SequencedRecord<T> {
    /// Sequence number for ordering
    ///
    /// Monotonically increasing ID assigned when record enters processing.
    /// Used to reconstruct original order after parallel processing.
    pub sequence_id: u64,

    /// The actual record data
    ///
    /// Payload being processed (e.g., binary COBOL record, JSON value).
    pub data: T,
}

impl<T> SequencedRecord<T> {
    /// Create a new sequenced record
    ///
    /// # Arguments
    ///
    /// * `sequence_id` - Unique monotonic sequence number for ordering
    /// * `data` - Record payload to process
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::SequencedRecord;
    ///
    /// let record = SequencedRecord::new(42, vec![1, 2, 3]);
    /// assert_eq!(record.sequence_id, 42);
    /// assert_eq!(record.data, vec![1, 2, 3]);
    /// ```
    #[inline]
    pub fn new(sequence_id: u64, data: T) -> Self {
        Self { sequence_id, data }
    }
}

/// Bounded channel with sequence tracking for ordered emission
///
/// Provides deterministic output ordering for parallel processing by buffering
/// out-of-order records and emitting them in sequence order. This is critical
/// for maintaining data integrity when processing COBOL records in parallel.
///
/// # How It Works
///
/// 1. Workers submit processed records with sequence IDs via sender channel
/// 2. `SequenceRing` receives records (potentially out-of-order)
/// 3. Out-of-order records are buffered in reorder buffer (BTreeMap)
/// 4. Records are emitted in strict sequence order via [`recv_ordered()`](SequenceRing::recv_ordered)
///
/// # Memory Bounds
///
/// - **Channel capacity** - Maximum records in flight between workers and consumer
/// - **Reorder window** - Maximum buffered out-of-order records (warns if exceeded)
///
/// # Performance Characteristics
///
/// - **O(log n)** insertion/removal from reorder buffer (BTreeMap)
/// - **O(1)** emission when records arrive in order (hot path)
/// - **Memory usage** - Bounded by channel capacity + reorder window size
///
/// # Examples
///
/// ```rust
/// use copybook_codec::memory::{SequenceRing, SequencedRecord};
///
/// let mut ring = SequenceRing::new(100, 50); // 100 capacity, 50 max window
/// let sender = ring.sender();
///
/// // Simulate workers sending out-of-order results
/// sender.send(SequencedRecord::new(2, "second")).unwrap();
/// sender.send(SequencedRecord::new(1, "first")).unwrap();
/// sender.send(SequencedRecord::new(3, "third")).unwrap();
///
/// // Consumer receives in order
/// assert_eq!(ring.recv_ordered().unwrap(), Some("first"));
/// assert_eq!(ring.recv_ordered().unwrap(), Some("second"));
/// assert_eq!(ring.recv_ordered().unwrap(), Some("third"));
/// ```
#[derive(Debug)]
pub struct SequenceRing<T> {
    /// Channel for receiving processed records
    receiver: Receiver<SequencedRecord<T>>,

    /// Sender for processed records (cloned to workers)
    sender: Sender<SequencedRecord<T>>,

    /// Buffer for out-of-order records
    ///
    /// BTreeMap provides O(log n) ordered access to buffered records.
    reorder_buffer: BTreeMap<u64, T>,

    /// Next expected sequence ID
    ///
    /// Monotonically incremented as records are emitted in order.
    next_sequence_id: u64,

    /// Maximum reordering window size
    ///
    /// Warning threshold for reorder buffer size (indicates excessive skew).
    max_window_size: usize,

    /// Channel capacity
    channel_capacity: usize,
}

impl<T> SequenceRing<T> {
    /// Create a new sequence ring with bounded capacity
    ///
    /// # Arguments
    ///
    /// * `channel_capacity` - Maximum number of records in flight between workers and consumer
    /// * `max_window_size` - Maximum buffered out-of-order records (warning threshold)
    ///
    /// # Tuning Guidelines
    ///
    /// - **Channel capacity**: Should be 2-4x number of worker threads for good throughput
    /// - **Reorder window**: Should be channel_capacity / 2 to allow for processing variance
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::SequenceRing;
    ///
    /// // For 4-worker pool: 16 capacity, 8 window
    /// let ring: SequenceRing<String> = SequenceRing::new(16, 8);
    ///
    /// // For 8-worker pool: 32 capacity, 16 window
    /// let ring: SequenceRing<Vec<u8>> = SequenceRing::new(32, 16);
    /// ```
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
    ///
    /// Returns a cloneable sender that workers can use to submit processed
    /// records back to the sequence ring. Multiple workers can share clones
    /// of this sender.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::{SequenceRing, SequencedRecord};
    /// use std::thread;
    ///
    /// let mut ring = SequenceRing::new(10, 5);
    /// let sender1 = ring.sender();
    /// let sender2 = ring.sender(); // Clone for another worker
    ///
    /// // Workers can send concurrently
    /// let handle = thread::spawn(move || {
    ///     sender1.send(SequencedRecord::new(1, "data")).unwrap();
    /// });
    ///
    /// sender2.send(SequencedRecord::new(2, "data")).unwrap();
    /// handle.join().unwrap();
    /// ```
    #[inline]
    #[must_use]
    pub fn sender(&self) -> Sender<SequencedRecord<T>> {
        self.sender.clone()
    }

    /// Receive the next record in sequence order.
    /// Blocks until the next expected record is available.
    ///
    /// # Errors
    /// Returns an error if the channel is disconnected.
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

    /// Try to receive the next record without blocking.
    ///
    /// # Errors
    /// Returns an error if the channel is disconnected or would block.
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
    ///
    /// Returns current operational statistics including reorder buffer usage
    /// and sequence tracking state.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::{SequenceRing, SequencedRecord};
    ///
    /// let mut ring = SequenceRing::new(10, 5);
    /// let sender = ring.sender();
    ///
    /// sender.send(SequencedRecord::new(2, "data")).unwrap();
    /// sender.send(SequencedRecord::new(1, "data")).unwrap();
    ///
    /// let stats = ring.stats();
    /// assert_eq!(stats.next_sequence_id, 1);
    /// assert_eq!(stats.reorder_buffer_size, 1); // Record 2 buffered
    /// ```
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
///
/// Provides visibility into sequence ring health and performance characteristics.
#[derive(Debug, Clone)]
pub struct SequenceRingStats {
    /// Next expected sequence ID
    ///
    /// The sequence number of the next record to be emitted.
    pub next_sequence_id: u64,

    /// Current reorder buffer size
    ///
    /// Number of out-of-order records currently buffered. High values indicate
    /// significant processing variance between workers.
    pub reorder_buffer_size: usize,

    /// Maximum reorder window size
    ///
    /// Warning threshold for reorder buffer size.
    pub max_window_size: usize,

    /// Channel capacity
    ///
    /// Maximum records in flight between workers and consumer.
    pub channel_capacity: usize,
}

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
/// - **num_workers**: Match CPU core count (or 2x for I/O-bound work)
/// - **channel_capacity**: 2-4x worker count for good pipeline depth
/// - **max_window_size**: channel_capacity / 2 to allow processing variance
///
/// # Examples
///
/// ## Basic Usage
///
/// ```rust
/// use copybook_codec::memory::{WorkerPool, ScratchBuffers};
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
/// ```rust
/// use copybook_codec::memory::{WorkerPool, ScratchBuffers};
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
/// // Submit COBOL records for parallel processing
/// for record in get_cobol_records() {
///     pool.submit(record).unwrap();
/// }
///
/// // Receive JSON in original order
/// while let Ok(Some(json)) = pool.recv_ordered() {
///     println!("{}", json);
/// }
///
/// pool.shutdown().unwrap();
/// # fn get_cobol_records() -> Vec<Vec<u8>> { vec![] }
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

/// Memory-bounded streaming processor
///
/// Tracks memory usage and provides backpressure signals to maintain
/// bounded memory consumption during streaming record processing. Enables
/// processing multi-GB COBOL files with steady-state memory usage <256 MiB.
///
/// # Purpose
///
/// Prevents unbounded memory growth when processing large data files by:
/// - Tracking estimated memory usage across buffers and in-flight data
/// - Providing memory pressure signals for throttling/flushing
/// - Collecting processing statistics for monitoring
///
/// # Usage Pattern
///
/// 1. Create processor with memory limit: [`StreamingProcessor::new()`]
/// 2. Track memory allocations: [`update_memory_usage()`](StreamingProcessor::update_memory_usage)
/// 3. Check pressure before processing: [`is_memory_pressure()`](StreamingProcessor::is_memory_pressure)
/// 4. Throttle input or flush buffers when pressure detected
/// 5. Record completed work: [`record_processed()`](StreamingProcessor::record_processed)
///
/// # Memory Tracking
///
/// **What to track**:
/// - Input buffers (record data)
/// - Output buffers (JSON strings, encoded data)
/// - Reorder buffers (parallel processing)
/// - Scratch buffers (codec working memory)
///
/// **Pressure threshold**: 80% of max_memory_bytes
///
/// # Examples
///
/// ## Basic Streaming with Memory Bounds
///
/// ```rust
/// use copybook_codec::memory::StreamingProcessor;
///
/// let mut processor = StreamingProcessor::with_default_limit(); // 256 MiB
///
/// for record in get_records() {
///     // Check memory pressure
///     if processor.is_memory_pressure() {
///         // Flush output buffers or throttle input
///         flush_buffers();
///     }
///
///     // Track record allocation
///     processor.update_memory_usage(record.len() as isize);
///
///     // Process record
///     process_record(&record);
///
///     // Record completion
///     processor.record_processed(record.len());
///
///     // Track deallocation
///     processor.update_memory_usage(-(record.len() as isize));
/// }
///
/// let stats = processor.stats();
/// println!("Processed {} records, peak {} MiB",
///          stats.records_processed,
///          stats.current_memory_bytes / 1024 / 1024);
/// # fn get_records() -> Vec<Vec<u8>> { vec![] }
/// # fn flush_buffers() {}
/// # fn process_record(_: &[u8]) {}
/// ```
///
/// ## File Processing with Adaptive Batching
///
/// ```rust
/// use copybook_codec::memory::StreamingProcessor;
///
/// let mut processor = StreamingProcessor::new(512); // 512 MiB limit
/// let mut batch = Vec::new();
///
/// for record in file_records() {
///     batch.push(record.clone());
///     processor.update_memory_usage(record.len() as isize);
///
///     // Adaptive batching based on memory pressure
///     if processor.is_memory_pressure() || batch.len() >= 1000 {
///         // Process batch
///         for rec in batch.drain(..) {
///             process_and_write(&rec);
///             processor.record_processed(rec.len());
///             processor.update_memory_usage(-(rec.len() as isize));
///         }
///     }
/// }
///
/// // Process remaining records
/// for rec in batch {
///     process_and_write(&rec);
///     processor.record_processed(rec.len());
/// }
/// # fn file_records() -> Vec<Vec<u8>> { vec![] }
/// # fn process_and_write(_: &[u8]) {}
/// ```
#[derive(Debug)]
pub struct StreamingProcessor {
    /// Maximum memory usage target (bytes)
    max_memory_bytes: usize,

    /// Current estimated memory usage
    ///
    /// Updated via `update_memory_usage()` as data flows through the system.
    current_memory_bytes: usize,

    /// Record processing statistics
    ///
    /// Count of records successfully processed.
    records_processed: u64,

    /// Bytes processed
    ///
    /// Total byte volume processed (input record sizes).
    bytes_processed: u64,
}

impl StreamingProcessor {
    /// Create a new streaming processor with memory limit
    ///
    /// # Arguments
    ///
    /// * `max_memory_mb` - Maximum memory usage in megabytes (MiB)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::StreamingProcessor;
    ///
    /// let processor = StreamingProcessor::new(512); // 512 MiB limit
    /// assert!(!processor.is_memory_pressure());
    /// ```
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
    ///
    /// Default limit matches copybook-rs steady-state memory target for
    /// processing multi-GB COBOL files.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::StreamingProcessor;
    ///
    /// let processor = StreamingProcessor::with_default_limit();
    /// let stats = processor.stats();
    /// assert_eq!(stats.max_memory_bytes, 256 * 1024 * 1024);
    /// ```
    #[inline]
    #[must_use]
    pub fn with_default_limit() -> Self {
        Self::new(256)
    }

    /// Check if we're approaching memory limit
    ///
    /// Returns `true` when current memory usage exceeds 80% of the maximum limit.
    /// This is the signal to apply backpressure: flush buffers, throttle input,
    /// or reduce batch sizes.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::StreamingProcessor;
    ///
    /// let mut processor = StreamingProcessor::new(1); // 1 MiB limit
    ///
    /// processor.update_memory_usage(500 * 1024); // 500 KB
    /// assert!(!processor.is_memory_pressure()); // <80%
    ///
    /// processor.update_memory_usage(400 * 1024); // +400 KB = 900 KB total
    /// assert!(processor.is_memory_pressure()); // >80%
    /// ```
    #[inline]
    #[must_use]
    pub fn is_memory_pressure(&self) -> bool {
        self.current_memory_bytes > (self.max_memory_bytes * 80 / 100) // 80% threshold
    }

    /// Update memory usage estimate
    ///
    /// Track memory allocations (+) and deallocations (-) to maintain
    /// current memory usage estimate. Use this to track all significant
    /// buffers in the processing pipeline.
    ///
    /// **Performance optimization**: Optimized for hot path with minimal branching.
    ///
    /// # Arguments
    ///
    /// * `bytes_delta` - Signed byte count change (positive = allocation, negative = deallocation)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::StreamingProcessor;
    ///
    /// let mut processor = StreamingProcessor::new(256);
    ///
    /// // Allocate 8 KB buffer
    /// processor.update_memory_usage(8192);
    /// assert_eq!(processor.stats().current_memory_bytes, 8192);
    ///
    /// // Deallocate 4 KB
    /// processor.update_memory_usage(-4096);
    /// assert_eq!(processor.stats().current_memory_bytes, 4096);
    ///
    /// // Saturating behavior (no underflow)
    /// processor.update_memory_usage(-10000);
    /// assert_eq!(processor.stats().current_memory_bytes, 0);
    /// ```
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
    ///
    /// Updates counters for completed record processing. Call this after
    /// successfully processing each record.
    ///
    /// # Arguments
    ///
    /// * `record_size` - Size of the processed record in bytes
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::StreamingProcessor;
    ///
    /// let mut processor = StreamingProcessor::with_default_limit();
    ///
    /// processor.record_processed(1024);
    /// processor.record_processed(2048);
    ///
    /// let stats = processor.stats();
    /// assert_eq!(stats.records_processed, 2);
    /// assert_eq!(stats.bytes_processed, 3072);
    /// ```
    #[inline]
    pub fn record_processed(&mut self, record_size: usize) {
        self.records_processed += 1;
        self.bytes_processed += record_size as u64;
    }

    /// Get processing statistics
    ///
    /// Returns current operational statistics including memory usage,
    /// utilization percentage, and processing throughput metrics.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use copybook_codec::memory::StreamingProcessor;
    ///
    /// let mut processor = StreamingProcessor::new(100); // 100 MiB
    /// processor.update_memory_usage(50 * 1024 * 1024); // 50 MiB
    /// processor.record_processed(1000);
    ///
    /// let stats = processor.stats();
    /// assert_eq!(stats.memory_utilization_percent, 50);
    /// assert_eq!(stats.records_processed, 1);
    /// assert_eq!(stats.bytes_processed, 1000);
    /// ```
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
///
/// Snapshot of current memory usage and processing metrics.
#[derive(Debug, Clone)]
pub struct StreamingProcessorStats {
    /// Maximum memory limit (bytes)
    pub max_memory_bytes: usize,

    /// Current memory usage (bytes)
    ///
    /// Estimated total memory across tracked buffers.
    pub current_memory_bytes: usize,

    /// Memory utilization percentage
    ///
    /// Current usage as percentage of max (0-100).
    pub memory_utilization_percent: usize,

    /// Records processed
    ///
    /// Total count of successfully processed records.
    pub records_processed: u64,

    /// Bytes processed
    ///
    /// Total input byte volume processed.
    pub bytes_processed: u64,
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
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
