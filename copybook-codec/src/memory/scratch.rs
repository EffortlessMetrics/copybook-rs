// SPDX-License-Identifier: AGPL-3.0-or-later
use smallvec::SmallVec;

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
    ///
    /// Uses `reserve(capacity - len)` to ensure final capacity >= requested.
    /// Note: `reserve(additional)` guarantees `capacity >= len + additional`,
    /// so we compute based on `len`, not current capacity.
    #[cold]
    #[inline(never)]
    fn grow_byte_buffer(&mut self, capacity: usize) {
        let len = self.byte_buffer.len();
        if capacity > len {
            self.byte_buffer.reserve(capacity - len);
        }
    }

    /// Cold path: grow string buffer (separate function for better optimization)
    ///
    /// Uses `reserve(capacity - len)` to ensure final capacity >= requested.
    /// Note: `reserve(additional)` guarantees `capacity >= len + additional`,
    /// so we compute based on `len`, not current capacity.
    #[cold]
    #[inline(never)]
    fn grow_string_buffer(&mut self, capacity: usize) {
        let len = self.string_buffer.len();
        if capacity > len {
            self.string_buffer.reserve(capacity - len);
        }
    }
}

impl Default for ScratchBuffers {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
