// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Comprehensive tests for copybook-codec-memory: ScratchBuffers, StreamingProcessor,
//! and WorkerPool covering creation, reuse, reset, thread safety, and edge cases.

use copybook_codec_memory::{ScratchBuffers, StreamingProcessor, WorkerPool};
use std::fmt::Write;

// ===========================================================================
// ScratchBuffers – creation
// ===========================================================================

#[test]
fn scratch_new_is_empty() {
    let s = ScratchBuffers::new();
    assert_eq!(s.digit_buffer.len(), 0);
    assert_eq!(s.byte_buffer.len(), 0);
    assert_eq!(s.string_buffer.len(), 0);
}

#[test]
fn scratch_new_has_documented_initial_capacities() {
    let s = ScratchBuffers::new();
    assert!(
        s.byte_buffer.capacity() >= 1024,
        "byte_buffer should start with >= 1 KiB"
    );
    assert!(
        s.string_buffer.capacity() >= 512,
        "string_buffer should start with >= 512 chars"
    );
}

#[test]
fn scratch_default_equivalent_to_new() {
    let from_new = ScratchBuffers::new();
    let from_default = ScratchBuffers::default();
    assert_eq!(
        from_new.byte_buffer.capacity(),
        from_default.byte_buffer.capacity()
    );
    assert_eq!(
        from_new.string_buffer.capacity(),
        from_default.string_buffer.capacity()
    );
    assert_eq!(from_new.digit_buffer.len(), from_default.digit_buffer.len());
}

#[test]
fn scratch_debug_trait_does_not_panic() {
    let s = ScratchBuffers::new();
    let debug_str = format!("{s:?}");
    assert!(debug_str.contains("ScratchBuffers"));
}

// ===========================================================================
// ScratchBuffers – buffer usage and data integrity
// ===========================================================================

#[test]
fn scratch_byte_buffer_stores_and_retrieves_data() {
    let mut s = ScratchBuffers::new();
    s.byte_buffer.extend_from_slice(b"\x00\xFF\x80\x7F");
    assert_eq!(s.byte_buffer.as_slice(), &[0x00, 0xFF, 0x80, 0x7F]);
}

#[test]
fn scratch_string_buffer_stores_utf8() {
    let mut s = ScratchBuffers::new();
    s.string_buffer.push_str("hello, 世界!");
    assert_eq!(&s.string_buffer, "hello, 世界!");
}

#[test]
fn scratch_digit_buffer_stores_digits() {
    let mut s = ScratchBuffers::new();
    for d in 0..10_u8 {
        s.digit_buffer.push(d);
    }
    assert_eq!(s.digit_buffer.as_slice(), &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
}

#[test]
fn scratch_multiple_extends_accumulate() {
    let mut s = ScratchBuffers::new();
    s.byte_buffer.extend_from_slice(b"AB");
    s.byte_buffer.extend_from_slice(b"CD");
    s.byte_buffer.extend_from_slice(b"EF");
    assert_eq!(s.byte_buffer.as_slice(), b"ABCDEF");
}

#[test]
fn scratch_write_macro_on_string_buffer() {
    let mut s = ScratchBuffers::new();
    write!(s.string_buffer, "value={}", 42).unwrap();
    assert_eq!(&s.string_buffer, "value=42");
}

// ===========================================================================
// ScratchBuffers – clear / reset behavior
// ===========================================================================

#[test]
fn scratch_clear_resets_all_lengths_to_zero() {
    let mut s = ScratchBuffers::new();
    s.digit_buffer.push(1);
    s.byte_buffer.extend_from_slice(b"data");
    s.string_buffer.push_str("text");

    s.clear();

    assert_eq!(s.digit_buffer.len(), 0);
    assert_eq!(s.byte_buffer.len(), 0);
    assert_eq!(s.string_buffer.len(), 0);
}

#[test]
fn scratch_clear_preserves_capacity() {
    let mut s = ScratchBuffers::new();
    s.byte_buffer.extend_from_slice(&[0xAA; 4096]);
    s.string_buffer.push_str(&"x".repeat(2048));

    let byte_cap = s.byte_buffer.capacity();
    let str_cap = s.string_buffer.capacity();

    s.clear();

    assert_eq!(s.byte_buffer.capacity(), byte_cap);
    assert_eq!(s.string_buffer.capacity(), str_cap);
}

#[test]
fn scratch_double_clear_is_safe() {
    let mut s = ScratchBuffers::new();
    s.byte_buffer.extend_from_slice(b"data");
    s.clear();
    s.clear(); // second clear on already-empty buffers
    assert_eq!(s.byte_buffer.len(), 0);
    assert_eq!(s.string_buffer.len(), 0);
    assert_eq!(s.digit_buffer.len(), 0);
}

#[test]
fn scratch_clear_on_fresh_instance() {
    let mut s = ScratchBuffers::new();
    s.clear(); // clear with no data written
    assert_eq!(s.byte_buffer.len(), 0);
}

// ===========================================================================
// ScratchBuffers – reuse across decode cycles
// ===========================================================================

#[test]
fn scratch_reuse_100_cycles_no_capacity_growth_after_warmup() {
    let mut s = ScratchBuffers::new();

    // Warmup: 5 cycles grow buffers
    for i in 0..5_u32 {
        s.byte_buffer
            .extend_from_slice(format!("record-{i:05}").as_bytes());
        write!(s.string_buffer, "\"field_{i}\"").unwrap();
        for d in 0..8_u8 {
            s.digit_buffer.push(d);
        }
        s.clear();
    }

    let byte_cap = s.byte_buffer.capacity();
    let str_cap = s.string_buffer.capacity();

    // Steady state: 95 more cycles must not reallocate
    for i in 5..100_u32 {
        s.byte_buffer
            .extend_from_slice(format!("record-{i:05}").as_bytes());
        write!(s.string_buffer, "\"field_{i}\"").unwrap();
        for d in 0..8_u8 {
            s.digit_buffer.push(d);
        }
        s.clear();
    }

    assert_eq!(
        s.byte_buffer.capacity(),
        byte_cap,
        "byte_buffer grew after warmup"
    );
    assert_eq!(
        s.string_buffer.capacity(),
        str_cap,
        "string_buffer grew after warmup"
    );
}

#[test]
fn scratch_reuse_preserves_heap_pointer() {
    let mut s = ScratchBuffers::new();
    s.byte_buffer.extend_from_slice(&[0xBB; 2048]);
    let ptr = s.byte_buffer.as_ptr();
    s.clear();
    s.byte_buffer.extend_from_slice(&[0xCC; 512]);
    assert_eq!(
        s.byte_buffer.as_ptr(),
        ptr,
        "buffer was reallocated on reuse"
    );
}

// ===========================================================================
// ScratchBuffers – ensure_capacity
// ===========================================================================

#[test]
fn ensure_byte_capacity_grows_when_needed() {
    let mut s = ScratchBuffers::new();
    s.ensure_byte_capacity(8192);
    assert!(s.byte_buffer.capacity() >= 8192);
}

#[test]
fn ensure_byte_capacity_noop_when_sufficient() {
    let mut s = ScratchBuffers::new();
    s.ensure_byte_capacity(8192);
    let cap = s.byte_buffer.capacity();
    s.ensure_byte_capacity(4096);
    assert_eq!(s.byte_buffer.capacity(), cap);
}

#[test]
fn ensure_string_capacity_grows_when_needed() {
    let mut s = ScratchBuffers::new();
    s.ensure_string_capacity(4096);
    assert!(s.string_buffer.capacity() >= 4096);
}

#[test]
fn ensure_capacity_zero_is_noop() {
    let mut s = ScratchBuffers::new();
    let byte_cap = s.byte_buffer.capacity();
    let str_cap = s.string_buffer.capacity();
    s.ensure_byte_capacity(0);
    s.ensure_string_capacity(0);
    assert_eq!(s.byte_buffer.capacity(), byte_cap);
    assert_eq!(s.string_buffer.capacity(), str_cap);
}

#[test]
fn ensure_capacity_preserves_existing_data() {
    let mut s = ScratchBuffers::new();
    s.byte_buffer.extend_from_slice(b"keep");
    s.string_buffer.push_str("retain");
    s.ensure_byte_capacity(8192);
    s.ensure_string_capacity(4096);
    assert_eq!(s.byte_buffer.as_slice(), b"keep");
    assert_eq!(&s.string_buffer, "retain");
}

// ===========================================================================
// ScratchBuffers – edge cases (empty, large)
// ===========================================================================

#[test]
fn scratch_empty_extend_and_push() {
    let mut s = ScratchBuffers::new();
    s.byte_buffer.extend_from_slice(b"");
    s.string_buffer.push_str("");
    assert_eq!(s.byte_buffer.len(), 0);
    assert_eq!(s.string_buffer.len(), 0);
}

#[test]
fn scratch_large_byte_buffer_1mib() {
    let mut s = ScratchBuffers::new();
    let data = vec![0x42_u8; 1_048_576];
    s.byte_buffer.extend_from_slice(&data);
    assert_eq!(s.byte_buffer.len(), 1_048_576);
    s.clear();
    assert_eq!(s.byte_buffer.len(), 0);
    assert!(s.byte_buffer.capacity() >= 1_048_576);
}

#[test]
fn scratch_digit_buffer_stack_to_heap_transition() {
    let mut s = ScratchBuffers::new();
    // SmallVec<[u8; 32]> stays inline up to 32 bytes
    for i in 0..32_u8 {
        s.digit_buffer.push(i);
    }
    assert!(!s.digit_buffer.spilled());

    s.digit_buffer.push(32);
    assert!(s.digit_buffer.spilled());

    s.clear();
    assert_eq!(s.digit_buffer.len(), 0);
}

// ===========================================================================
// ScratchBuffers – thread safety (Send/Sync)
// ===========================================================================

#[test]
fn scratch_is_send() {
    fn assert_send<T: Send>() {}
    assert_send::<ScratchBuffers>();
}

#[test]
fn scratch_move_across_thread() {
    let s = ScratchBuffers::new();
    let handle = std::thread::spawn(move || {
        let mut owned = s;
        owned.byte_buffer.extend_from_slice(b"from-thread");
        owned.clear();
        owned.byte_buffer.len()
    });
    assert_eq!(handle.join().unwrap(), 0);
}

#[test]
fn scratch_independent_per_thread() {
    let handles: Vec<_> = (0..4)
        .map(|id| {
            std::thread::spawn(move || {
                let mut s = ScratchBuffers::new();
                for _ in 0..50 {
                    write!(s.string_buffer, "thread-{id}").unwrap();
                    s.byte_buffer.extend_from_slice(b"data");
                    s.clear();
                }
                true
            })
        })
        .collect();

    for h in handles {
        assert!(h.join().unwrap());
    }
}

// ===========================================================================
// StreamingProcessor – creation and defaults
// ===========================================================================

#[test]
fn streaming_default_limit_is_256_mib() {
    let p = StreamingProcessor::with_default_limit();
    let stats = p.stats();
    assert_eq!(stats.max_memory_bytes, 256 * 1024 * 1024);
}

#[test]
fn streaming_custom_limit() {
    let p = StreamingProcessor::new(512);
    let stats = p.stats();
    assert_eq!(stats.max_memory_bytes, 512 * 1024 * 1024);
}

#[test]
fn streaming_initial_stats_are_zero() {
    let p = StreamingProcessor::with_default_limit();
    let stats = p.stats();
    assert_eq!(stats.current_memory_bytes, 0);
    assert_eq!(stats.records_processed, 0);
    assert_eq!(stats.bytes_processed, 0);
    assert_eq!(stats.memory_utilization_percent, 0);
}

// ===========================================================================
// StreamingProcessor – memory tracking
// ===========================================================================

#[test]
fn streaming_update_memory_positive() {
    let mut p = StreamingProcessor::new(1);
    p.update_memory_usage(8192);
    assert_eq!(p.stats().current_memory_bytes, 8192);
}

#[test]
fn streaming_update_memory_negative() {
    let mut p = StreamingProcessor::new(1);
    p.update_memory_usage(8192);
    p.update_memory_usage(-4096);
    assert_eq!(p.stats().current_memory_bytes, 4096);
}

#[test]
fn streaming_saturating_underflow() {
    let mut p = StreamingProcessor::new(1);
    p.update_memory_usage(100);
    p.update_memory_usage(-500);
    assert_eq!(p.stats().current_memory_bytes, 0);
}

#[test]
fn streaming_many_allocations_and_deallocations() {
    let mut p = StreamingProcessor::new(256);
    for _ in 0..1000 {
        p.update_memory_usage(1024);
        p.update_memory_usage(-1024);
    }
    assert_eq!(p.stats().current_memory_bytes, 0);
}

// ===========================================================================
// StreamingProcessor – pressure threshold
// ===========================================================================

#[test]
fn streaming_no_pressure_below_80_percent() {
    let mut p = StreamingProcessor::new(1); // 1 MiB
    p.update_memory_usage(500 * 1024); // ~49%
    assert!(!p.is_memory_pressure());
}

#[test]
fn streaming_pressure_above_80_percent() {
    let mut p = StreamingProcessor::new(1); // 1 MiB = 1_048_576 bytes
    p.update_memory_usage(900 * 1024); // ~88%
    assert!(p.is_memory_pressure());
}

#[test]
fn streaming_pressure_exact_boundary() {
    let mut p = StreamingProcessor::new(1); // 1 MiB = 1_048_576 bytes
    let limit = 1_048_576_usize;
    let eighty_pct = limit * 80 / 100; // 838_860

    p.update_memory_usage(eighty_pct as isize);
    assert!(
        !p.is_memory_pressure(),
        "at exactly 80% should not be pressure"
    );

    p.update_memory_usage(1);
    assert!(
        p.is_memory_pressure(),
        "one byte over 80% should trigger pressure"
    );
}

// ===========================================================================
// StreamingProcessor – record tracking and stats
// ===========================================================================

#[test]
fn streaming_record_processed_accumulates() {
    let mut p = StreamingProcessor::with_default_limit();
    p.record_processed(1024);
    p.record_processed(2048);
    p.record_processed(512);

    let stats = p.stats();
    assert_eq!(stats.records_processed, 3);
    assert_eq!(stats.bytes_processed, 3584);
}

#[test]
fn streaming_zero_byte_records() {
    let mut p = StreamingProcessor::with_default_limit();
    for _ in 0..10 {
        p.record_processed(0);
    }
    let stats = p.stats();
    assert_eq!(stats.records_processed, 10);
    assert_eq!(stats.bytes_processed, 0);
}

#[test]
fn streaming_utilization_percent_accuracy() {
    let mut p = StreamingProcessor::new(100); // 100 MiB
    let total = 100 * 1024 * 1024;

    p.update_memory_usage((total / 4) as isize); // 25%
    assert_eq!(p.stats().memory_utilization_percent, 25);

    p.update_memory_usage((total / 4) as isize); // 50%
    assert_eq!(p.stats().memory_utilization_percent, 50);

    p.update_memory_usage((total / 4) as isize); // 75%
    assert_eq!(p.stats().memory_utilization_percent, 75);

    p.update_memory_usage((total / 4) as isize); // 100%
    assert_eq!(p.stats().memory_utilization_percent, 100);
}

#[test]
fn streaming_stats_clone() {
    let mut p = StreamingProcessor::new(10);
    p.update_memory_usage(1024);
    p.record_processed(512);

    let stats1 = p.stats();
    let stats2 = stats1.clone();
    assert_eq!(stats1.current_memory_bytes, stats2.current_memory_bytes);
    assert_eq!(stats1.records_processed, stats2.records_processed);
}

// ===========================================================================
// WorkerPool – basic functionality
// ===========================================================================

#[test]
fn worker_pool_basic_doubling() {
    let mut pool = WorkerPool::new(
        2,
        10,
        5,
        |input: i32, _scratch: &mut ScratchBuffers| -> i32 { input * 2 },
    );

    for i in 1..=5 {
        pool.submit(i).unwrap();
    }

    for i in 1..=5 {
        let result = pool.recv_ordered().unwrap().unwrap();
        assert_eq!(result, i * 2);
    }

    pool.shutdown().unwrap();
}

#[test]
fn worker_pool_string_transform() {
    let mut pool = WorkerPool::new(
        2,
        8,
        4,
        |input: String, _scratch: &mut ScratchBuffers| -> String { input.to_uppercase() },
    );

    let inputs = vec!["hello", "world", "foo", "bar"];
    for s in &inputs {
        pool.submit(s.to_string()).unwrap();
    }

    for expected in &["HELLO", "WORLD", "FOO", "BAR"] {
        let result = pool.recv_ordered().unwrap().unwrap();
        assert_eq!(result, *expected);
    }

    pool.shutdown().unwrap();
}

#[test]
fn worker_pool_preserves_order_with_varying_work() {
    let mut pool = WorkerPool::new(
        4,
        20,
        10,
        |input: u64, _scratch: &mut ScratchBuffers| -> u64 {
            // Varying workload to test reordering
            if input % 2 == 0 {
                std::thread::sleep(std::time::Duration::from_micros(10));
            }
            input * input
        },
    );

    let count = 15;
    for i in 1..=count {
        pool.submit(i).unwrap();
    }

    let mut results = Vec::new();
    for _ in 1..=count {
        results.push(pool.recv_ordered().unwrap().unwrap());
    }

    let expected: Vec<u64> = (1..=count).map(|x| x * x).collect();
    assert_eq!(results, expected);

    pool.shutdown().unwrap();
}

#[test]
fn worker_pool_single_worker() {
    let mut pool = WorkerPool::new(
        1,
        10,
        5,
        |input: i32, _scratch: &mut ScratchBuffers| -> i32 { input + 100 },
    );

    for i in 0..10 {
        pool.submit(i).unwrap();
    }

    for i in 0..10 {
        let result = pool.recv_ordered().unwrap().unwrap();
        assert_eq!(result, i + 100);
    }

    pool.shutdown().unwrap();
}

#[test]
fn worker_pool_stats() {
    let pool: WorkerPool<i32, i32> = WorkerPool::new(
        3,
        10,
        5,
        |input: i32, _scratch: &mut ScratchBuffers| -> i32 { input },
    );

    let stats = pool.stats();
    assert_eq!(stats.num_workers, 3);
    assert_eq!(stats.next_input_sequence, 1); // no items submitted yet

    pool.shutdown().unwrap();
}

#[test]
fn worker_pool_uses_scratch_buffers() {
    let mut pool = WorkerPool::new(
        2,
        10,
        5,
        |input: Vec<u8>, scratch: &mut ScratchBuffers| -> usize {
            scratch.byte_buffer.extend_from_slice(&input);
            scratch.byte_buffer.len()
        },
    );

    pool.submit(vec![1, 2, 3]).unwrap();
    pool.submit(vec![4, 5]).unwrap();

    assert_eq!(pool.recv_ordered().unwrap().unwrap(), 3);
    assert_eq!(pool.recv_ordered().unwrap().unwrap(), 2);

    pool.shutdown().unwrap();
}

#[test]
fn worker_pool_shutdown_without_submissions() {
    let pool: WorkerPool<i32, i32> = WorkerPool::new(
        2,
        10,
        5,
        |input: i32, _scratch: &mut ScratchBuffers| -> i32 { input },
    );
    pool.shutdown().unwrap();
}
