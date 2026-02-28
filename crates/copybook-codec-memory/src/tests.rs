// SPDX-License-Identifier: AGPL-3.0-or-later
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

    // Test byte buffer
    buffers.byte_buffer.extend_from_slice(b"hello");
    assert_eq!(buffers.byte_buffer.len(), 5);

    // Test string buffer
    buffers.string_buffer.push_str("world");
    assert_eq!(buffers.string_buffer.len(), 5);

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
    let test_data: Vec<i32> = (1..=10).collect();

    for num_workers in [1, 2] {
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
            pool.submit(input).context("submitting input")?;
        }

        // Collect results
        for _ in 0..test_data.len() {
            match pool.recv_ordered() {
                Ok(Some(result)) => results.push(result),
                Ok(None) => break,
                Err(_) => return Err(anyhow!("Worker pool error")),
            }
        }

        pool.shutdown()
            .map_err(|error| anyhow!(error))
            .context("shutting down worker pool")?;

        // Results should be in the same order regardless of worker count
        let expected: Vec<i32> = test_data.iter().map(|x| x * x).collect();
        assert_eq!(results, expected);
    }

    Ok(())
}

#[test]
fn test_sequenced_record_creation() {
    let record = SequencedRecord::new(42, "test data");
    assert_eq!(record.sequence_id, 42);
    assert_eq!(record.data, "test data");
}

#[test]
fn test_sequence_ring_empty() {
    let mut ring = SequenceRing::<&str>::new(10, 5);

    // try_recv_ordered on empty ring should return Empty error
    let result = ring.try_recv_ordered();
    assert!(result.is_err());
}

#[test]
fn test_sequence_ring_channel_capacity() -> TestResult {
    let ring = SequenceRing::new(2, 1); // Small channel
    let sender = ring.sender();

    // Fill channel
    sender.try_send(SequencedRecord::new(1, "first"))?;
    sender.try_send(SequencedRecord::new(2, "second"))?;

    // Third send should fail (channel full)
    let result = sender.try_send(SequencedRecord::new(3, "third"));
    assert!(result.is_err());

    Ok(())
}

#[test]
fn test_sequence_ring_stats() -> TestResult {
    let mut ring = SequenceRing::new(10, 5);
    let sender = ring.sender();

    // Send records
    for i in 1..=5 {
        sender.send(SequencedRecord::new(i, format!("record_{i}")))?;
    }

    // Get stats after sending (next_sequence_id hasn't advanced yet)
    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 1); // Still expecting first record
    assert_eq!(stats.reorder_buffer_size, 0); // Nothing reordered yet

    // Receive all records
    for _ in 1..=5 {
        assert!(ring.recv_ordered()?.is_some());
    }

    // Get final stats
    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 6); // After receiving all 5
    assert_eq!(stats.reorder_buffer_size, 0); // All received

    Ok(())
}

#[test]
fn test_streaming_processor_with_default_limit() {
    let mut processor = StreamingProcessor::with_default_limit();
    assert!(!processor.is_memory_pressure());

    // Default limit is 256 MB, pressure threshold is 80% (~205 MB)
    processor.update_memory_usage(210 * 1024 * 1024); // 210 MB > 80% of 256 MB
    assert!(processor.is_memory_pressure());

    let stats = processor.stats();
    assert_eq!(stats.max_memory_bytes, 256 * 1024 * 1024);
}

#[test]
fn test_streaming_processor_memory_tracking() {
    let mut processor = StreamingProcessor::new(1); // 1 MB limit

    assert!(!processor.is_memory_pressure());

    // Add memory usage
    processor.update_memory_usage(512 * 1024); // 512 KB
    assert_eq!(processor.stats().current_memory_bytes, 512 * 1024);
    assert!(!processor.is_memory_pressure()); // Still under 80% of 1 MB

    // Add more memory to trigger pressure (total ~1 MB > 80% of 1 MB)
    processor.update_memory_usage(512 * 1024); // 512 KB more = 1 MB total
    assert!(processor.is_memory_pressure());

    let stats = processor.stats();
    assert_eq!(stats.current_memory_bytes, 1024 * 1024);
    assert_eq!(stats.max_memory_bytes, 1024 * 1024); // 1 MB in bytes
}

#[test]
fn test_streaming_processor_record_tracking() {
    let mut processor = StreamingProcessor::with_default_limit();

    // Record processing
    processor.record_processed(1024);
    processor.record_processed(2048);

    let stats = processor.stats();
    assert_eq!(stats.records_processed, 2);
    assert_eq!(stats.bytes_processed, 3072);
}

// ---------------------------------------------------------------------------
// ScratchBuffers – creation, reset, growth, reuse, edge cases
// ---------------------------------------------------------------------------

#[test]
fn test_scratch_buffers_initial_capacities() {
    let scratch = ScratchBuffers::new();
    assert_eq!(scratch.digit_buffer.len(), 0);
    assert!(scratch.byte_buffer.capacity() >= 1024);
    assert!(scratch.string_buffer.capacity() >= 512);
}

#[test]
fn test_scratch_buffers_default_trait() {
    let scratch = ScratchBuffers::default();
    assert_eq!(scratch.digit_buffer.len(), 0);
    assert!(scratch.byte_buffer.capacity() >= 1024);
    assert!(scratch.string_buffer.capacity() >= 512);
}

#[test]
fn test_scratch_buffers_clear_preserves_capacity() {
    let mut scratch = ScratchBuffers::new();

    // Fill buffers to grow them
    scratch.byte_buffer.extend_from_slice(&[0xAA; 4096]);
    scratch.string_buffer.push_str(&"x".repeat(2048));
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

    // Capacity must not shrink after clear
    assert_eq!(scratch.byte_buffer.capacity(), byte_cap);
    assert_eq!(scratch.string_buffer.capacity(), string_cap);
    assert_eq!(scratch.digit_buffer.capacity(), digit_cap);
}

#[test]
fn test_scratch_buffers_ensure_byte_capacity_growth() {
    let mut scratch = ScratchBuffers::new();
    let original_cap = scratch.byte_buffer.capacity();

    // Request capacity larger than initial
    scratch.ensure_byte_capacity(8192);
    assert!(scratch.byte_buffer.capacity() >= 8192);

    // Requesting smaller capacity is a no-op
    let cap_after_grow = scratch.byte_buffer.capacity();
    scratch.ensure_byte_capacity(1024);
    assert_eq!(scratch.byte_buffer.capacity(), cap_after_grow);

    // Requesting same capacity is a no-op
    scratch.ensure_byte_capacity(cap_after_grow);
    assert_eq!(scratch.byte_buffer.capacity(), cap_after_grow);

    let _ = original_cap; // suppress unused warning
}

#[test]
fn test_scratch_buffers_ensure_string_capacity_growth() {
    let mut scratch = ScratchBuffers::new();

    scratch.ensure_string_capacity(4096);
    assert!(scratch.string_buffer.capacity() >= 4096);

    let cap = scratch.string_buffer.capacity();
    scratch.ensure_string_capacity(2048);
    assert_eq!(scratch.string_buffer.capacity(), cap);
}

#[test]
fn test_scratch_buffers_multiple_reuse_cycles() {
    let mut scratch = ScratchBuffers::new();

    for cycle in 0..100 {
        // Simulate per-record work
        scratch.digit_buffer.push((cycle % 10) as u8);
        scratch
            .byte_buffer
            .extend_from_slice(format!("record-{cycle}").as_bytes());
        scratch.string_buffer.push_str(&format!("output-{cycle}"));

        assert!(!scratch.byte_buffer.is_empty());
        assert!(!scratch.string_buffer.is_empty());
        assert!(!scratch.digit_buffer.is_empty());

        scratch.clear();

        assert_eq!(scratch.byte_buffer.len(), 0);
        assert_eq!(scratch.string_buffer.len(), 0);
        assert_eq!(scratch.digit_buffer.len(), 0);
    }

    // After many cycles, capacity should have grown to accommodate the data
    // but lengths must be zero
    assert!(scratch.byte_buffer.capacity() > 0);
    assert!(scratch.string_buffer.capacity() > 0);
}

#[test]
fn test_scratch_buffers_zero_length_operations() {
    let mut scratch = ScratchBuffers::new();

    // Zero-length extend / push
    scratch.byte_buffer.extend_from_slice(b"");
    scratch.string_buffer.push_str("");

    assert_eq!(scratch.byte_buffer.len(), 0);
    assert_eq!(scratch.string_buffer.len(), 0);

    // Clearing empty buffers is safe
    scratch.clear();
    assert_eq!(scratch.byte_buffer.len(), 0);

    // ensure_*_capacity(0) is a no-op
    scratch.ensure_byte_capacity(0);
    scratch.ensure_string_capacity(0);
}

#[test]
fn test_scratch_buffers_large_allocation() {
    let mut scratch = ScratchBuffers::new();

    // 1 MiB byte buffer
    let large_data = vec![0x42_u8; 1_048_576];
    scratch.byte_buffer.extend_from_slice(&large_data);
    assert_eq!(scratch.byte_buffer.len(), 1_048_576);

    // 512 KiB string
    let large_str: String = "A".repeat(524_288);
    scratch.string_buffer.push_str(&large_str);
    assert_eq!(scratch.string_buffer.len(), 524_288);

    scratch.clear();
    assert_eq!(scratch.byte_buffer.len(), 0);
    assert_eq!(scratch.string_buffer.len(), 0);

    // Capacity still available for reuse
    assert!(scratch.byte_buffer.capacity() >= 1_048_576);
    assert!(scratch.string_buffer.capacity() >= 524_288);
}

#[test]
fn test_scratch_buffers_digit_buffer_stack_to_heap() {
    let mut scratch = ScratchBuffers::new();

    // SmallVec<[u8; 32]> — first 32 bytes stay on stack
    for i in 0..32_u8 {
        scratch.digit_buffer.push(i);
    }
    assert_eq!(scratch.digit_buffer.len(), 32);
    assert!(!scratch.digit_buffer.spilled()); // still inline

    // Push past 32 → spills to heap
    scratch.digit_buffer.push(32);
    assert_eq!(scratch.digit_buffer.len(), 33);
    assert!(scratch.digit_buffer.spilled());

    scratch.clear();
    assert_eq!(scratch.digit_buffer.len(), 0);
}

#[test]
fn test_scratch_buffers_send_across_threads() {
    // ScratchBuffers should be Send so worker threads can own them
    let scratch = ScratchBuffers::new();
    let handle = std::thread::spawn(move || {
        let mut s = scratch;
        s.byte_buffer.extend_from_slice(b"thread");
        s.clear();
        s.byte_buffer.len()
    });
    assert_eq!(handle.join().unwrap(), 0);
}

#[test]
fn test_scratch_buffers_ensure_capacity_after_partial_fill() {
    let mut scratch = ScratchBuffers::new();

    // Partially fill, then request larger capacity
    scratch.byte_buffer.extend_from_slice(&[1; 512]);
    scratch.ensure_byte_capacity(4096);
    assert!(scratch.byte_buffer.capacity() >= 4096);
    // Existing data preserved
    assert_eq!(scratch.byte_buffer.len(), 512);

    scratch.string_buffer.push_str("abc");
    scratch.ensure_string_capacity(2048);
    assert!(scratch.string_buffer.capacity() >= 2048);
    assert_eq!(&scratch.string_buffer, "abc");
}

// ---------------------------------------------------------------------------
// StreamingProcessor – edge cases
// ---------------------------------------------------------------------------

#[test]
fn test_streaming_processor_saturating_underflow() {
    let mut processor = StreamingProcessor::new(1);

    // Negative delta larger than current usage → saturates to 0
    processor.update_memory_usage(100);
    processor.update_memory_usage(-500);
    assert_eq!(processor.stats().current_memory_bytes, 0);
}

#[test]
fn test_streaming_processor_utilization_percent() {
    let mut processor = StreamingProcessor::new(100); // 100 MiB

    processor.update_memory_usage(50 * 1024 * 1024); // 50 MiB
    let stats = processor.stats();
    assert_eq!(stats.memory_utilization_percent, 50);

    processor.update_memory_usage(50 * 1024 * 1024); // 100 MiB total
    let stats = processor.stats();
    assert_eq!(stats.memory_utilization_percent, 100);
}

#[test]
fn test_streaming_processor_pressure_boundary() {
    let mut processor = StreamingProcessor::new(1); // 1 MiB = 1_048_576 bytes
    let limit = 1_048_576_usize;

    // Exactly 80% — threshold is > 80%, so at 80% there is no pressure
    let eighty_pct = limit * 80 / 100; // 838_860
    processor.update_memory_usage(eighty_pct as isize);
    assert!(!processor.is_memory_pressure());

    // One byte over 80%
    processor.update_memory_usage(1);
    assert!(processor.is_memory_pressure());
}
