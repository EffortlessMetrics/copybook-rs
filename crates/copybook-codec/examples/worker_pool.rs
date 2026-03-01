#![allow(
// SPDX-License-Identifier: AGPL-3.0-or-later
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::items_after_statements
)]
// WorkerPool usage example
// Demonstrates parallel record processing with deterministic output ordering

use copybook_codec::memory::{ScratchBuffers, WorkerPool};
use copybook_codec::{Codepage, DecodeOptions, RecordFormat, decode_record_with_scratch};
use copybook_core::parse_copybook;
use std::sync::Arc;
use std::time::Instant;

fn main() {
    println!("=== WorkerPool Example ===\n");

    // Example 1: Basic parallel processing
    println!("--- Example 1: Basic Parallel Processing ---");
    let mut pool = WorkerPool::new(
        4,  // 4 worker threads
        16, // 16 records in flight
        8,  // 8 max reorder window
        |input: i32, _scratch: &mut ScratchBuffers| -> i32 {
            // Simulate processing work
            input * 2
        },
    );

    println!("Submitting work items 1-10...");
    for i in 1..=10 {
        pool.submit(i).unwrap();
    }

    println!("Receiving results in order:");
    for i in 1..=10 {
        let result = pool.recv_ordered().unwrap().unwrap();
        println!("  Input: {i}, Output: {result}");
        assert_eq!(result, i * 2);
    }

    pool.shutdown().unwrap();

    // Example 2: COBOL record processing
    println!("\n--- Example 2: COBOL Record Processing ---");

    // Example COBOL copybook for customer records
    let copybook = r"
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID       PIC 9(6).
          05 CUSTOMER-NAME     PIC X(30).
          05 ACCOUNT-TYPE      PIC X(1).
          05 BALANCE           PIC S9(7)V99 COMP-3.
          05 LAST-ACTIVITY     PIC 9(8).
    ";

    println!("🏗️  Parsing COBOL copybook...");
    let schema = match parse_copybook(copybook) {
        Ok(schema) => schema,
        Err(e) => {
            eprintln!("❌ Failed to parse copybook: {e}");
            return;
        }
    };
    println!("✅ Parsed schema with {} fields", schema.fields.len());

    // Configure decoding options
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // Create sample records (ASCII for simplicity)
    let records: Vec<Vec<u8>> = vec![
        b"000001ACME CORPORATION       B123456789012345678901234567890120241201".to_vec(),
        b"000002ANOTHER COMPANY       C9876543210987654321098765432109820241202".to_vec(),
        b"000003THIRD COMPANY         D5555555555555555555555555555555520241203".to_vec(),
        b"000004FOURTH COMPANY        E1111111111111111111111111111111120241204".to_vec(),
        b"000005FIFTH COMPANY         F2222222222222222222222222222222220241205".to_vec(),
    ];

    let num_records = records.len();
    let schema = Arc::new(schema);

    // Create worker pool for COBOL decoding
    let schema_clone = Arc::clone(&schema);
    let mut pool = WorkerPool::new(
        4,  // 4 worker threads
        16, // 16 records in flight
        8,  // 8 max reorder window
        move |record_data: Vec<u8>, scratch: &mut ScratchBuffers| -> String {
            decode_record_with_scratch(&schema_clone, &record_data, &options, scratch)
                .unwrap()
                .to_string()
        },
    );

    println!("Submitting {num_records} COBOL records for parallel processing...");
    let start = Instant::now();

    for record in records {
        pool.submit(record).unwrap();
    }

    println!("Receiving JSON results in order:");
    for i in 0..num_records {
        let json = pool.recv_ordered().unwrap().unwrap();
        println!("  Record {}: {}", i + 1, json);
    }

    let elapsed = start.elapsed();
    println!("Processing time: {elapsed:?}");

    pool.shutdown().unwrap();

    // Example 3: Performance tuning
    println!("\n--- Example 3: Performance Tuning ---");
    println!("WorkerPool parameters:");
    println!("  - num_workers: Number of worker threads");
    println!("  - channel_capacity: Maximum records in flight");
    println!("  - max_window_size: Maximum reordering window");
    println!();
    println!("Tuning Guidelines:");
    println!("  - num_workers: Match CPU core count (or 2x for I/O-bound work)");
    println!("  - channel_capacity: 2-4x worker count for good pipeline depth");
    println!("  - max_window_size: channel_capacity / 2 to allow processing variance");
    println!();
    println!("Examples:");
    println!("  - 4-core CPU: 4 workers, 16 capacity, 8 window");
    println!("  - 8-core CPU: 8 workers, 32 capacity, 16 window");
    println!("  - I/O-bound: 16 workers, 64 capacity, 32 window");

    // Example 4: Statistics
    println!("\n--- Example 4: Statistics ---");
    let mut pool2 = WorkerPool::new(
        2, // 2 worker threads
        8, // 8 records in flight
        4, // 4 max reorder window
        |input: i32, _scratch: &mut ScratchBuffers| -> i32 { input * 3 },
    );

    // Submit some work
    for i in 1..=5 {
        pool2.submit(i).unwrap();
    }

    // Get statistics
    let stats = pool2.stats();
    println!("Statistics:");
    println!("  Worker count: {}", stats.num_workers);
    println!(
        "  Channel capacity: {}",
        stats.sequence_ring_stats.channel_capacity
    );
    println!(
        "  Max window size: {}",
        stats.sequence_ring_stats.max_window_size
    );
    println!("  Next input sequence: {}", stats.next_input_sequence);

    // Receive results
    for _ in 1..=5 {
        pool2.recv_ordered().unwrap().unwrap();
    }

    pool2.shutdown().unwrap();

    println!("\n=== Key Features ===");
    println!("✅ Deterministic output - Records emitted in original input order");
    println!("✅ Bounded memory - Fixed channel capacity prevents unbounded buffering");
    println!("✅ Worker-local buffers - Each worker has dedicated scratch buffers");
    println!("✅ Automatic cleanup - Workers terminated gracefully on shutdown");
}
