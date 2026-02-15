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
fn test_sequence_ring_channel_capacity() {
    let ring = SequenceRing::new(2, 1); // Small channel
    let sender = ring.sender();

    // Fill channel
    sender.try_send(SequencedRecord::new(1, "first")).unwrap();
    sender.try_send(SequencedRecord::new(2, "second")).unwrap();

    // Third send should fail (channel full)
    let result = sender.try_send(SequencedRecord::new(3, "third"));
    assert!(result.is_err());
}

#[test]
fn test_sequence_ring_stats() {
    let mut ring = SequenceRing::new(10, 5);
    let sender = ring.sender();

    // Send records
    for i in 1..=5 {
        sender
            .send(SequencedRecord::new(i, format!("record_{i}")))
            .unwrap();
    }

    // Get stats after sending (next_sequence_id hasn't advanced yet)
    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 1); // Still expecting first record
    assert_eq!(stats.reorder_buffer_size, 0); // Nothing reordered yet

    // Receive all records
    for _ in 1..=5 {
        assert!(ring.recv_ordered().unwrap().is_some());
    }

    // Get final stats
    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 6); // After receiving all 5
    assert_eq!(stats.reorder_buffer_size, 0); // All received
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
