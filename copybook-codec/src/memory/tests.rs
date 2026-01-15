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
