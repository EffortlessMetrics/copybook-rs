// SPDX-License-Identifier: AGPL-3.0-or-later
use smallvec::SmallVec;

/// Small vector for digit buffers (≤32 bytes on stack)
///
/// Stack-allocated buffer used for packed/zoned decimal digit processing.
/// The 32-byte inline capacity handles typical COBOL numeric fields (up to
/// PIC S9(31)) without heap allocation.
pub type DigitBuffer = SmallVec<[u8; 32]>;

/// Reusable scratch buffers for worker threads
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
    /// Create new scratch buffers with reasonable initial capacity.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            digit_buffer: SmallVec::new(),
            byte_buffer: Vec::with_capacity(1024),
            string_buffer: String::with_capacity(512),
        }
    }

    /// Clear all buffers for reuse.
    #[inline]
    pub fn clear(&mut self) {
        self.digit_buffer.clear();
        self.byte_buffer.clear();
        self.string_buffer.clear();
    }

    /// Ensure byte buffer has at least the specified capacity.
    #[inline]
    pub fn ensure_byte_capacity(&mut self, capacity: usize) {
        if self.byte_buffer.capacity() < capacity {
            self.grow_byte_buffer(capacity);
        }
    }

    /// Ensure string buffer has at least the specified capacity.
    #[inline]
    pub fn ensure_string_capacity(&mut self, capacity: usize) {
        if self.string_buffer.capacity() < capacity {
            self.grow_string_buffer(capacity);
        }
    }

    #[cold]
    #[inline(never)]
    fn grow_byte_buffer(&mut self, capacity: usize) {
        let len = self.byte_buffer.len();
        if capacity > len {
            self.byte_buffer.reserve(capacity - len);
        }
    }

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn buffers_clear_without_shrinking() {
        let mut buffers = ScratchBuffers::new();
        buffers.digit_buffer.extend_from_slice(&[1, 2, 3]);
        buffers.byte_buffer.extend_from_slice(b"hello");
        buffers.string_buffer.push_str("world");

        let byte_capacity = buffers.byte_buffer.capacity();
        let string_capacity = buffers.string_buffer.capacity();
        buffers.clear();

        assert_eq!(buffers.digit_buffer.len(), 0);
        assert_eq!(buffers.byte_buffer.len(), 0);
        assert_eq!(buffers.string_buffer.len(), 0);
        assert_eq!(buffers.byte_buffer.capacity(), byte_capacity);
        assert_eq!(buffers.string_buffer.capacity(), string_capacity);
    }

    #[test]
    fn ensures_requested_capacities() {
        let mut buffers = ScratchBuffers::new();
        buffers.ensure_byte_capacity(8192);
        buffers.ensure_string_capacity(4096);

        assert!(buffers.byte_buffer.capacity() >= 8192);
        assert!(buffers.string_buffer.capacity() >= 4096);
    }
}
