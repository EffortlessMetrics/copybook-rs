// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Tests for `ScratchBuffers` memory discipline: creation, reuse, capacity
//! retention, thread-safety, and bounded memory in hot loops.

use copybook_codec_memory::ScratchBuffers;
use std::fmt::Write;

// ---------------------------------------------------------------------------
// 1. New ScratchBuffers → empty / default state
// ---------------------------------------------------------------------------

#[test]
fn new_scratch_buffers_are_empty_with_default_capacity() {
    let scratch = ScratchBuffers::new();

    assert_eq!(scratch.digit_buffer.len(), 0);
    assert_eq!(scratch.byte_buffer.len(), 0);
    assert_eq!(scratch.string_buffer.len(), 0);

    // Documented initial capacities
    assert!(scratch.byte_buffer.capacity() >= 1024);
    assert!(scratch.string_buffer.capacity() >= 512);
}

#[test]
fn default_trait_matches_new() {
    let from_new = ScratchBuffers::new();
    let from_default = ScratchBuffers::default();

    assert_eq!(from_new.digit_buffer.len(), from_default.digit_buffer.len());
    assert_eq!(
        from_new.byte_buffer.capacity(),
        from_default.byte_buffer.capacity()
    );
    assert_eq!(
        from_new.string_buffer.capacity(),
        from_default.string_buffer.capacity()
    );
}

// ---------------------------------------------------------------------------
// 2. Acquire buffer → use → clear → reuse → same allocation reused
// ---------------------------------------------------------------------------

#[test]
fn buffer_reuse_after_clear_keeps_same_allocation() {
    let mut scratch = ScratchBuffers::new();

    // First use – grow the buffers
    scratch.byte_buffer.extend_from_slice(&[0xAB; 2048]);
    scratch.string_buffer.push_str(&"z".repeat(1024));
    for i in 0..40_u8 {
        scratch.digit_buffer.push(i);
    }

    let byte_ptr = scratch.byte_buffer.as_ptr();
    let string_ptr = scratch.string_buffer.as_ptr();
    let byte_cap = scratch.byte_buffer.capacity();
    let string_cap = scratch.string_buffer.capacity();

    scratch.clear();

    // Second use – buffers should reuse the same heap allocation
    scratch.byte_buffer.extend_from_slice(&[0xCD; 512]);
    scratch.string_buffer.push_str("reused");

    assert_eq!(scratch.byte_buffer.as_ptr(), byte_ptr);
    assert_eq!(scratch.string_buffer.as_ptr(), string_ptr);
    assert_eq!(scratch.byte_buffer.capacity(), byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), string_cap);
}

// ---------------------------------------------------------------------------
// 3. Clear resets state without deallocation
// ---------------------------------------------------------------------------

#[test]
fn clear_resets_lengths_preserves_capacity() {
    let mut scratch = ScratchBuffers::new();

    scratch.byte_buffer.extend_from_slice(&[0xFF; 4096]);
    scratch.string_buffer.push_str(&"q".repeat(2048));
    for i in 0..64_u8 {
        scratch.digit_buffer.push(i);
    }

    let byte_cap = scratch.byte_buffer.capacity();
    let string_cap = scratch.string_buffer.capacity();
    let digit_cap = scratch.digit_buffer.capacity();

    scratch.clear();

    assert_eq!(scratch.byte_buffer.len(), 0);
    assert_eq!(scratch.string_buffer.len(), 0);
    assert_eq!(scratch.digit_buffer.len(), 0);

    assert_eq!(scratch.byte_buffer.capacity(), byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), string_cap);
    assert_eq!(scratch.digit_buffer.capacity(), digit_cap);
}

// ---------------------------------------------------------------------------
// 4. Multiple concurrent buffer acquisitions → independent buffers
// ---------------------------------------------------------------------------

#[test]
fn independent_scratch_buffers_do_not_interfere() {
    let mut a = ScratchBuffers::new();
    let mut b = ScratchBuffers::new();

    a.byte_buffer.extend_from_slice(b"alpha");
    b.byte_buffer.extend_from_slice(b"bravo");
    a.string_buffer.push_str("A");
    b.string_buffer.push_str("B");

    assert_eq!(&a.byte_buffer, b"alpha");
    assert_eq!(&b.byte_buffer, b"bravo");
    assert_eq!(&a.string_buffer, "A");
    assert_eq!(&b.string_buffer, "B");

    a.clear();
    assert_eq!(a.byte_buffer.len(), 0);
    // b must be untouched
    assert_eq!(&b.byte_buffer, b"bravo");
}

// ---------------------------------------------------------------------------
// 5. Buffer capacity grows on demand
// ---------------------------------------------------------------------------

#[test]
fn ensure_byte_capacity_grows_on_demand() {
    let mut scratch = ScratchBuffers::new();
    let initial_cap = scratch.byte_buffer.capacity();

    scratch.ensure_byte_capacity(8192);
    assert!(scratch.byte_buffer.capacity() >= 8192);
    assert!(scratch.byte_buffer.capacity() >= initial_cap);

    // Smaller request is a no-op
    let cap_after_grow = scratch.byte_buffer.capacity();
    scratch.ensure_byte_capacity(4096);
    assert_eq!(scratch.byte_buffer.capacity(), cap_after_grow);
}

#[test]
fn ensure_string_capacity_grows_on_demand() {
    let mut scratch = ScratchBuffers::new();

    scratch.ensure_string_capacity(4096);
    assert!(scratch.string_buffer.capacity() >= 4096);

    let cap = scratch.string_buffer.capacity();
    scratch.ensure_string_capacity(2048);
    assert_eq!(scratch.string_buffer.capacity(), cap);
}

// ---------------------------------------------------------------------------
// 6. After reset, capacity is retained (no deallocation)
// ---------------------------------------------------------------------------

#[test]
fn capacity_retained_after_clear_and_ensure() {
    let mut scratch = ScratchBuffers::new();

    scratch.ensure_byte_capacity(16_384);
    scratch.ensure_string_capacity(8192);
    scratch.byte_buffer.extend_from_slice(&[0u8; 10_000]);
    scratch.string_buffer.push_str(&"x".repeat(5000));

    let byte_cap = scratch.byte_buffer.capacity();
    let string_cap = scratch.string_buffer.capacity();

    scratch.clear();

    assert_eq!(scratch.byte_buffer.capacity(), byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), string_cap);
    assert_eq!(scratch.byte_buffer.len(), 0);
    assert_eq!(scratch.string_buffer.len(), 0);
}

// ---------------------------------------------------------------------------
// 7. Buffers are Send (compile-time check) + cross-thread move
// ---------------------------------------------------------------------------

#[test]
fn scratch_buffers_are_send() {
    fn assert_send<T: Send>() {}
    assert_send::<ScratchBuffers>();
}

#[test]
fn scratch_buffers_move_across_thread() {
    let scratch = ScratchBuffers::new();
    let handle = std::thread::spawn(move || {
        let mut s = scratch;
        s.byte_buffer.extend_from_slice(b"thread-owned");
        s.clear();
        s.byte_buffer.len()
    });
    assert_eq!(handle.join().unwrap(), 0);
}

// ---------------------------------------------------------------------------
// 8. Process 1000 records with same ScratchBuffers → no allocation growth
//    after warmup
// ---------------------------------------------------------------------------

#[test]
fn no_allocation_growth_after_warmup_1000_records() {
    let mut scratch = ScratchBuffers::new();

    // Warmup phase: first 10 records may grow the buffers
    for i in 0..10_u32 {
        simulate_record_processing(&mut scratch, i);
        scratch.clear();
    }

    // Capture capacity after warmup
    let byte_cap = scratch.byte_buffer.capacity();
    let string_cap = scratch.string_buffer.capacity();

    // Steady-state: next 990 records must not reallocate
    for i in 10..1000_u32 {
        simulate_record_processing(&mut scratch, i);
        scratch.clear();
    }

    assert_eq!(
        scratch.byte_buffer.capacity(),
        byte_cap,
        "byte_buffer reallocated after warmup"
    );
    assert_eq!(
        scratch.string_buffer.capacity(),
        string_cap,
        "string_buffer reallocated after warmup"
    );
}

/// Simulate per-record work matching typical codec patterns.
fn simulate_record_processing(scratch: &mut ScratchBuffers, record_id: u32) {
    // Digit extraction (packed decimal)
    for d in 0..8_u8 {
        scratch.digit_buffer.push(d);
    }
    // Byte work (field extraction)
    scratch
        .byte_buffer
        .extend_from_slice(format!("REC-{record_id:05}").as_bytes());
    // String building (JSON value)
    write!(scratch.string_buffer, "\"field_value_{record_id}\"").unwrap();
}

// ---------------------------------------------------------------------------
// 9. ScratchBuffers in tight loop → bounded memory usage
// ---------------------------------------------------------------------------

#[test]
fn tight_loop_bounded_memory() {
    let mut scratch = ScratchBuffers::new();

    // Pre-allocate to a known ceiling
    scratch.ensure_byte_capacity(4096);
    scratch.ensure_string_capacity(2048);

    let max_byte_cap = scratch.byte_buffer.capacity();
    let max_string_cap = scratch.string_buffer.capacity();

    // Run a tight loop that stays within the pre-allocated limits
    for i in 0..10_000_u32 {
        scratch.byte_buffer.extend_from_slice(&[0x42; 128]);
        write!(scratch.string_buffer, "val-{i}").unwrap();
        for d in 0..4_u8 {
            scratch.digit_buffer.push(d);
        }
        scratch.clear();
    }

    // Capacity should not have grown beyond what was pre-allocated
    assert_eq!(
        scratch.byte_buffer.capacity(),
        max_byte_cap,
        "byte_buffer grew beyond pre-allocated ceiling"
    );
    assert_eq!(
        scratch.string_buffer.capacity(),
        max_string_cap,
        "string_buffer grew beyond pre-allocated ceiling"
    );
}

// ---------------------------------------------------------------------------
// 10. decode_record_with_scratch produces identical output to decode_record
// ---------------------------------------------------------------------------

// NOTE: Tests 10 & 11 require copybook-codec (which depends on copybook-core
// for Schema). They live here as documentation of the contract; the actual
// integration tests are in crates/copybook-codec/tests/.

// ---------------------------------------------------------------------------
// 11. digit_buffer stack-to-heap spill boundary
// ---------------------------------------------------------------------------

#[test]
fn digit_buffer_stack_inline_up_to_32_bytes() {
    let mut scratch = ScratchBuffers::new();

    for i in 0..32_u8 {
        scratch.digit_buffer.push(i);
    }
    assert_eq!(scratch.digit_buffer.len(), 32);
    assert!(!scratch.digit_buffer.spilled(), "should still be inline at 32");

    scratch.digit_buffer.push(32);
    assert!(scratch.digit_buffer.spilled(), "should spill to heap at 33");

    scratch.clear();
    assert_eq!(scratch.digit_buffer.len(), 0);
}

// ---------------------------------------------------------------------------
// 12. ensure_*_capacity after partial fill preserves existing data
// ---------------------------------------------------------------------------

#[test]
fn ensure_capacity_preserves_existing_data() {
    let mut scratch = ScratchBuffers::new();

    scratch.byte_buffer.extend_from_slice(b"hello");
    scratch.ensure_byte_capacity(8192);
    assert!(scratch.byte_buffer.capacity() >= 8192);
    assert_eq!(&scratch.byte_buffer, b"hello");

    scratch.string_buffer.push_str("world");
    scratch.ensure_string_capacity(4096);
    assert!(scratch.string_buffer.capacity() >= 4096);
    assert_eq!(&scratch.string_buffer, "world");
}
