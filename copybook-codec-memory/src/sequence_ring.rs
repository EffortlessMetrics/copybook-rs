// SPDX-License-Identifier: AGPL-3.0-or-later
use crossbeam_channel::{Receiver, Sender, bounded};
use std::collections::BTreeMap;
use tracing::{debug, warn};

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
    #[must_use]
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
/// 3. Out-of-order records are buffered in reorder buffer (`BTreeMap`)
/// 4. Records are emitted in strict sequence order via [`recv_ordered()`](SequenceRing::recv_ordered)
///
/// # Memory Bounds
///
/// - **Channel capacity** - Maximum records in flight between workers and consumer
/// - **Reorder window** - Maximum buffered out-of-order records (warns if exceeded)
///
/// # Performance Characteristics
///
/// - **O(log n)** insertion/removal from reorder buffer (`BTreeMap`)
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
    /// `BTreeMap` provides O(log n) ordered access to buffered records.
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
    /// - **Reorder window**: Should be `channel_capacity` / 2 to allow for processing variance
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
    /// use crossbeam_channel::TryRecvError;
    ///
    /// let mut ring = SequenceRing::new(10, 5);
    /// let sender = ring.sender();
    ///
    /// // Send out-of-order record (seq 2 when expecting 1)
    /// sender.send(SequencedRecord::new(2, "data")).unwrap();
    ///
    /// // Force the ring to observe and buffer the out-of-order record
    /// assert!(matches!(ring.try_recv_ordered(), Err(TryRecvError::Empty)));
    ///
    /// let stats = ring.stats();
    /// assert_eq!(stats.next_sequence_id, 1); // Still waiting for record 1
    /// assert_eq!(stats.reorder_buffer_size, 1); // Record 2 is buffered
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
