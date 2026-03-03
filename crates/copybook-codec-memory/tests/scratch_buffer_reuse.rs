// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Tests for scratch buffer reuse patterns, capacity behavior, concurrent
//! coexistence, thread-local patterns, and integration with `StreamingProcessor`
//! and `WorkerPool`.

use copybook_codec_memory::{ScratchBuffers, StreamingProcessor, WorkerPool};
use std::fmt::Write;

// ===========================================================================
// 1. Interleaved buffer operations maintain isolation
// ===========================================================================

#[test]
fn interleaved_digit_byte_string_operations() {
    let mut scratch = ScratchBuffers::new();

    for cycle in 0..50_u32 {
        // Alternate which buffer gets written first each cycle
        if cycle % 3 == 0 {
            scratch.digit_buffer.push((cycle % 10) as u8);
            scratch.byte_buffer.extend_from_slice(b"byte-data");
            write!(scratch.string_buffer, "str-{cycle}").unwrap();
        } else if cycle % 3 == 1 {
            scratch.byte_buffer.extend_from_slice(b"first");
            write!(scratch.string_buffer, "second-{cycle}").unwrap();
            scratch.digit_buffer.push(0xFF);
        } else {
            write!(scratch.string_buffer, "leading-{cycle}").unwrap();
            scratch.digit_buffer.push(0x0A);
            scratch.byte_buffer.extend_from_slice(b"trailing");
        }

        // All buffers must have data
        assert!(!scratch.digit_buffer.is_empty());
        assert!(!scratch.byte_buffer.is_empty());
        assert!(!scratch.string_buffer.is_empty());

        scratch.clear();

        // All empty after clear
        assert_eq!(scratch.digit_buffer.len(), 0);
        assert_eq!(scratch.byte_buffer.len(), 0);
        assert_eq!(scratch.string_buffer.len(), 0);
    }
}

// ===========================================================================
// 2. Capacity growth is monotonically non-decreasing
// ===========================================================================

#[test]
fn capacity_growth_is_monotonic() {
    let mut scratch = ScratchBuffers::new();
    let mut prev_byte_cap = scratch.byte_buffer.capacity();
    let mut prev_str_cap = scratch.string_buffer.capacity();

    for size in (128..=4096).step_by(128) {
        scratch.byte_buffer.extend_from_slice(&vec![0xAB_u8; size]);
        scratch.string_buffer.push_str(&"x".repeat(size.min(2048)));

        assert!(
            scratch.byte_buffer.capacity() >= prev_byte_cap,
            "byte capacity shrank from {prev_byte_cap} to {}",
            scratch.byte_buffer.capacity()
        );
        assert!(
            scratch.string_buffer.capacity() >= prev_str_cap,
            "string capacity shrank from {prev_str_cap} to {}",
            scratch.string_buffer.capacity()
        );

        prev_byte_cap = scratch.byte_buffer.capacity();
        prev_str_cap = scratch.string_buffer.capacity();
        scratch.clear();

        // Capacity must not shrink after clear
        assert_eq!(scratch.byte_buffer.capacity(), prev_byte_cap);
        assert_eq!(scratch.string_buffer.capacity(), prev_str_cap);
    }
}

// ===========================================================================
// 3. Multiple scratch buffers coexist with independent memory
// ===========================================================================

#[test]
fn multiple_scratch_buffers_have_independent_memory() {
    let mut a = ScratchBuffers::new();
    let mut b = ScratchBuffers::new();
    let mut c = ScratchBuffers::new();

    a.byte_buffer.extend_from_slice(b"AAA");
    b.byte_buffer.extend_from_slice(b"BBBBB");
    c.byte_buffer.extend_from_slice(b"CC");

    a.string_buffer.push_str("alpha");
    b.string_buffer.push_str("bravo");
    c.string_buffer.push_str("charlie");

    // Verify independence
    assert_eq!(&a.byte_buffer, b"AAA");
    assert_eq!(&b.byte_buffer, b"BBBBB");
    assert_eq!(&c.byte_buffer, b"CC");

    // Clear one doesn't affect others
    b.clear();
    assert_eq!(&a.byte_buffer, b"AAA");
    assert_eq!(b.byte_buffer.len(), 0);
    assert_eq!(&c.byte_buffer, b"CC");

    assert_eq!(&a.string_buffer, "alpha");
    assert_eq!(b.string_buffer.len(), 0);
    assert_eq!(&c.string_buffer, "charlie");
}

// ===========================================================================
// 4. Thread-local scratch buffers are independent per thread
// ===========================================================================

#[test]
fn thread_local_scratch_buffers_independent() {
    let handles: Vec<_> = (0..8)
        .map(|tid| {
            std::thread::spawn(move || {
                let mut scratch = ScratchBuffers::new();
                let tag = format!("thread-{tid}");

                for i in 0..100 {
                    scratch.byte_buffer.extend_from_slice(tag.as_bytes());
                    write!(scratch.string_buffer, "iter-{i}").unwrap();
                    scratch.digit_buffer.push((tid * 10 + i) as u8);

                    // Verify data integrity within this thread
                    assert!(scratch.byte_buffer.starts_with(tag.as_bytes()));

                    scratch.clear();
                }

                // Return final capacity to prove each thread grew independently
                (
                    scratch.byte_buffer.capacity(),
                    scratch.string_buffer.capacity(),
                )
            })
        })
        .collect();

    let results: Vec<_> = handles.into_iter().map(|h| h.join().unwrap()).collect();
    // All threads completed successfully
    assert_eq!(results.len(), 8);
    // Each thread has reasonable capacity
    for (byte_cap, str_cap) in &results {
        assert!(*byte_cap > 0);
        assert!(*str_cap > 0);
    }
}

// ===========================================================================
// 5. Rapid clear/reset cycles don't leak
// ===========================================================================

#[test]
fn rapid_clear_cycles_no_capacity_leak() {
    let mut scratch = ScratchBuffers::new();

    // Fill once to establish a ceiling
    scratch.byte_buffer.extend_from_slice(&[0xFF; 2048]);
    scratch.string_buffer.push_str(&"q".repeat(1024));
    scratch.clear();

    let byte_cap = scratch.byte_buffer.capacity();
    let str_cap = scratch.string_buffer.capacity();

    // 10000 rapid fill+clear cycles staying under the ceiling
    for i in 0..10_000_u32 {
        scratch.byte_buffer.extend_from_slice(&[0xAB; 64]);
        write!(scratch.string_buffer, "v{i}").unwrap();
        scratch.clear();
    }

    // Capacity should not have changed since we stayed under ceiling
    assert_eq!(scratch.byte_buffer.capacity(), byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), str_cap);
}

// ===========================================================================
// 6. Buffer reuse after large spike then small records
// ===========================================================================

#[test]
fn reuse_after_large_spike() {
    let mut scratch = ScratchBuffers::new();

    // Spike: write a large payload
    scratch.byte_buffer.extend_from_slice(&[0x42; 65_536]);
    scratch.string_buffer.push_str(&"S".repeat(32_768));
    let spike_byte_cap = scratch.byte_buffer.capacity();
    let spike_str_cap = scratch.string_buffer.capacity();
    scratch.clear();

    // Subsequent small records reuse the large allocation
    for i in 0..500_u32 {
        scratch.byte_buffer.extend_from_slice(&[0x01; 128]);
        write!(scratch.string_buffer, "small-{i}").unwrap();
        scratch.clear();
    }

    // Capacity stays at spike level (no shrink, no further growth)
    assert_eq!(scratch.byte_buffer.capacity(), spike_byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), spike_str_cap);
}

// ===========================================================================
// 7. Digit buffer reuse after heap spill
// ===========================================================================

#[test]
fn digit_buffer_reuse_after_heap_spill() {
    let mut scratch = ScratchBuffers::new();

    // Force heap spill (> 32 bytes)
    for i in 0..64_u8 {
        scratch.digit_buffer.push(i);
    }
    assert!(scratch.digit_buffer.spilled());
    let spill_cap = scratch.digit_buffer.capacity();

    scratch.clear();

    // Reuse within the spilled heap allocation
    for i in 0..20_u8 {
        scratch.digit_buffer.push(i);
    }
    assert_eq!(scratch.digit_buffer.len(), 20);
    // Capacity at least as large as after spill
    assert!(scratch.digit_buffer.capacity() >= spill_cap);

    scratch.clear();

    // Another cycle staying under spill capacity
    for i in 0..40_u8 {
        scratch.digit_buffer.push(i);
    }
    assert_eq!(scratch.digit_buffer.len(), 40);
    assert!(scratch.digit_buffer.capacity() >= spill_cap);
}

// ===========================================================================
// 8. Buffer reuse with varying record sizes
// ===========================================================================

#[test]
fn varying_record_sizes_reuse_pattern() {
    let mut scratch = ScratchBuffers::new();
    let sizes = [10, 500, 50, 2000, 100, 8000, 200, 4000, 30, 1500];

    // First pass: grow to max
    for &size in &sizes {
        scratch.byte_buffer.extend_from_slice(&vec![0xCC; size]);
        scratch.string_buffer.push_str(&"v".repeat(size / 2));
        scratch.clear();
    }

    let byte_cap = scratch.byte_buffer.capacity();
    let str_cap = scratch.string_buffer.capacity();

    // Second pass: same sizes should not grow
    for &size in &sizes {
        scratch.byte_buffer.extend_from_slice(&vec![0xDD; size]);
        scratch.string_buffer.push_str(&"w".repeat(size / 2));
        scratch.clear();
    }

    assert_eq!(scratch.byte_buffer.capacity(), byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), str_cap);
}

// ===========================================================================
// 9. ensure_capacity after fill-clear retains old capacity
// ===========================================================================

#[test]
fn ensure_capacity_after_clear_retains_previous() {
    let mut scratch = ScratchBuffers::new();

    // Grow via data
    scratch.byte_buffer.extend_from_slice(&[0xAA; 4096]);
    scratch.string_buffer.push_str(&"z".repeat(2048));
    let byte_cap = scratch.byte_buffer.capacity();
    let str_cap = scratch.string_buffer.capacity();

    scratch.clear();

    // ensure_capacity with smaller request is no-op
    scratch.ensure_byte_capacity(1024);
    scratch.ensure_string_capacity(512);
    assert_eq!(scratch.byte_buffer.capacity(), byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), str_cap);

    // ensure_capacity with larger request grows
    scratch.ensure_byte_capacity(16_384);
    assert!(scratch.byte_buffer.capacity() >= 16_384);
    scratch.ensure_string_capacity(8192);
    assert!(scratch.string_buffer.capacity() >= 8192);
}

// ===========================================================================
// 10. WorkerPool scratch buffers are reused per-worker
// ===========================================================================

#[test]
fn worker_pool_scratch_reused_per_worker() {
    // Each worker should reuse its scratch buffers across items.
    // We test this indirectly: workers write accumulated data into scratch,
    // and the result should reflect per-item data only (cleared between items).
    let mut pool = WorkerPool::new(
        2,
        20,
        10,
        |input: usize, scratch: &mut ScratchBuffers| -> usize {
            // Write to scratch buffer
            scratch.byte_buffer.extend_from_slice(&vec![0xAB; input]);
            // Return the length (should be exactly `input` since scratch is cleared between calls)
            scratch.byte_buffer.len()
        },
    );

    let count = 20;
    for i in 0..count {
        pool.submit(100 + i).unwrap();
    }

    for i in 0..count {
        let result = pool.recv_ordered().unwrap().unwrap();
        assert_eq!(result, 100 + i, "scratch should be cleared between items");
    }

    pool.shutdown().unwrap();
}

// ===========================================================================
// 11. WorkerPool deterministic output with many items
// ===========================================================================

#[test]
fn worker_pool_deterministic_100_items() {
    let mut pool = WorkerPool::new(
        4,
        50,
        25,
        |input: u64, _scratch: &mut ScratchBuffers| -> u64 {
            // Simulate varying work
            if input.is_multiple_of(5) {
                std::thread::sleep(std::time::Duration::from_micros(5));
            }
            input * 3 + 1
        },
    );

    let count = 100_u64;
    for i in 0..count {
        pool.submit(i).unwrap();
    }

    let mut results = Vec::with_capacity(count as usize);
    for _ in 0..count {
        results.push(pool.recv_ordered().unwrap().unwrap());
    }

    let expected: Vec<u64> = (0..count).map(|x| x * 3 + 1).collect();
    assert_eq!(results, expected);

    pool.shutdown().unwrap();
}

// ===========================================================================
// 12. WorkerPool with different worker counts produces same output
// ===========================================================================

#[test]
fn worker_pool_output_independent_of_worker_count() {
    let test_data: Vec<i32> = (1..=30).collect();
    let expected: Vec<i32> = test_data.iter().map(|x| x * x + 1).collect();

    for num_workers in [1, 2, 4] {
        let mut pool = WorkerPool::new(
            num_workers,
            30,
            15,
            |input: i32, _scratch: &mut ScratchBuffers| -> i32 { input * input + 1 },
        );

        for &item in &test_data {
            pool.submit(item).unwrap();
        }

        let mut results = Vec::new();
        for _ in 0..test_data.len() {
            results.push(pool.recv_ordered().unwrap().unwrap());
        }

        assert_eq!(
            results, expected,
            "Output should be identical with {num_workers} workers"
        );

        pool.shutdown().unwrap();
    }
}

// ===========================================================================
// 13. StreamingProcessor + scratch buffer combined lifecycle
// ===========================================================================

#[test]
fn streaming_processor_with_scratch_lifecycle() {
    let mut processor = StreamingProcessor::new(1); // 1 MiB
    let mut scratch = ScratchBuffers::new();

    for i in 0..500_u32 {
        let record_size = 128 + (i % 64) as usize;

        // Track allocation
        processor.update_memory_usage(record_size as isize);

        // Simulate processing with scratch
        scratch
            .byte_buffer
            .extend_from_slice(&vec![0xAB; record_size]);
        write!(scratch.string_buffer, "record-{i}").unwrap();
        scratch.clear();

        // Track completion
        processor.record_processed(record_size);
        processor.update_memory_usage(-(record_size as isize));
    }

    let stats = processor.stats();
    assert_eq!(stats.records_processed, 500);
    assert_eq!(stats.current_memory_bytes, 0);
    assert!(stats.bytes_processed > 0);
}

// ===========================================================================
// 14. StreamingProcessor backpressure triggers correctly
// ===========================================================================

#[test]
fn streaming_processor_backpressure_with_accumulation() {
    let mut processor = StreamingProcessor::new(1); // 1 MiB = 1_048_576 bytes
    let mut pressure_triggered = false;

    // Accumulate memory without releasing
    for _ in 0..100 {
        processor.update_memory_usage(20_000); // 20 KB each

        if processor.is_memory_pressure() {
            pressure_triggered = true;
            break;
        }
    }

    assert!(
        pressure_triggered,
        "Pressure should trigger before 2 MiB accumulated in 1 MiB limit"
    );

    // After releasing memory, pressure should clear
    let stats = processor.stats();
    processor.update_memory_usage(-(stats.current_memory_bytes as isize));
    assert!(!processor.is_memory_pressure());
}

// ===========================================================================
// 15. WorkerPool workers don't corrupt each other on error
// ===========================================================================

#[test]
fn worker_pool_worker_isolation() {
    // Workers processing different ranges should not interfere
    let mut pool = WorkerPool::new(
        4,
        40,
        20,
        |input: (usize, usize), scratch: &mut ScratchBuffers| -> Vec<usize> {
            let (start, end) = input;
            let mut results = Vec::new();
            for i in start..end {
                scratch.byte_buffer.extend_from_slice(&[i as u8; 8]);
                results.push(i * 2);
            }
            results
        },
    );

    // Submit disjoint ranges
    let ranges = vec![(0, 10), (10, 20), (20, 30), (30, 40)];
    for range in &ranges {
        pool.submit(*range).unwrap();
    }

    let mut all_results = Vec::new();
    for _ in 0..ranges.len() {
        all_results.push(pool.recv_ordered().unwrap().unwrap());
    }

    // Results must be in submission order
    assert_eq!(all_results[0], (0..10).map(|x| x * 2).collect::<Vec<_>>());
    assert_eq!(all_results[1], (10..20).map(|x| x * 2).collect::<Vec<_>>());
    assert_eq!(all_results[2], (20..30).map(|x| x * 2).collect::<Vec<_>>());
    assert_eq!(all_results[3], (30..40).map(|x| x * 2).collect::<Vec<_>>());

    pool.shutdown().unwrap();
}

// ===========================================================================
// 16. Cross-thread scratch buffer transfer preserves data
// ===========================================================================

#[test]
fn scratch_buffer_transfer_across_threads_preserves_data() {
    let mut scratch = ScratchBuffers::new();
    scratch.byte_buffer.extend_from_slice(b"transfer-test");
    scratch.string_buffer.push_str("hello-world");
    scratch.digit_buffer.push(42);

    let handle = std::thread::spawn(move || {
        // Verify data survived the move
        assert_eq!(&scratch.byte_buffer, b"transfer-test");
        assert_eq!(&scratch.string_buffer, "hello-world");
        assert_eq!(scratch.digit_buffer[0], 42);

        // Modify and return
        scratch.clear();
        scratch.byte_buffer.extend_from_slice(b"from-thread");
        scratch
    });

    let returned = handle.join().unwrap();
    assert_eq!(&returned.byte_buffer, b"from-thread");
    assert_eq!(returned.string_buffer.len(), 0);
}

// ===========================================================================
// 17. StreamingProcessor stats accuracy after large batch
// ===========================================================================

#[test]
fn streaming_processor_stats_accurate_after_2000_records() {
    let mut processor = StreamingProcessor::with_default_limit();

    let record_size = 256_usize;
    let count = 2000_u64;

    for _ in 0..count {
        processor.update_memory_usage(record_size as isize);
        processor.record_processed(record_size);
        processor.update_memory_usage(-(record_size as isize));
    }

    let stats = processor.stats();
    assert_eq!(stats.records_processed, count);
    assert_eq!(stats.bytes_processed, count * record_size as u64);
    assert_eq!(stats.current_memory_bytes, 0);
    assert_eq!(stats.memory_utilization_percent, 0);
}

// ===========================================================================
// 18. WorkerPool with scratch-intensive processing
// ===========================================================================

#[test]
fn worker_pool_scratch_intensive_processing() {
    let mut pool = WorkerPool::new(
        3,
        30,
        15,
        |input: u32, scratch: &mut ScratchBuffers| -> String {
            // Simulate codec-like processing using all three buffers
            for d in 0..8_u8 {
                scratch.digit_buffer.push(d + input as u8);
            }
            scratch
                .byte_buffer
                .extend_from_slice(format!("REC-{input:05}").as_bytes());
            write!(scratch.string_buffer, "\"value_{input}\"").unwrap();

            scratch.string_buffer.clone()
        },
    );

    let count = 50_u32;
    for i in 0..count {
        pool.submit(i).unwrap();
    }

    for i in 0..count {
        let result = pool.recv_ordered().unwrap().unwrap();
        assert_eq!(result, format!("\"value_{i}\""));
    }

    pool.shutdown().unwrap();
}
