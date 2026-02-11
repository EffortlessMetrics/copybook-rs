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
/// **Pressure threshold**: 80% of `max_memory_bytes`
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
