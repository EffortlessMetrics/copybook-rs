#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::similar_names,
    clippy::too_many_lines,
    clippy::items_after_statements
)]
// SequenceRing usage example
// Demonstrates ordered emission for parallel processing

use copybook_codec::memory::{SequenceRing, SequencedRecord};
use std::thread;
use std::time::Duration;

fn main() {
    println!("=== SequenceRing Example ===\n");

    // Example 1: Basic ordered emission
    println!("--- Example 1: Basic Ordered Emission ---");
    let mut ring = SequenceRing::new(100, 50); // 100 capacity, 50 max window
    let sender = ring.sender();

    // Simulate workers sending out-of-order results
    println!("Sending records out of order: 2, 1, 3");
    sender.send(SequencedRecord::new(2, "second")).unwrap();
    sender.send(SequencedRecord::new(1, "first")).unwrap();
    sender.send(SequencedRecord::new(3, "third")).unwrap();

    // Consumer receives in order
    println!("Receiving records in sequence order:");
    let mut count = 0;
    while let Some(record) = ring.recv_ordered().unwrap() {
        count += 1;
        println!("  Sequence {count}: {record}");
    }

    // Example 2: Multi-threaded processing
    println!("\n--- Example 2: Multi-Threaded Processing ---");
    let mut ring2 = SequenceRing::new(16, 8);
    let sender2 = ring2.sender();

    // Spawn worker threads that process records out of order
    let mut handles = vec![];

    // Worker 1 processes even records
    let s1 = sender2.clone();
    let handle1 = thread::spawn(move || {
        for i in (2..=10).step_by(2) {
            // Simulate variable processing time
            thread::sleep(Duration::from_millis((10 - i) * 10));
            s1.send(SequencedRecord::new(i, format!("Worker1: Record {i}")))
                .unwrap();
        }
    });
    handles.push(handle1);

    // Worker 2 processes odd records
    let s2 = sender2.clone();
    let handle2 = thread::spawn(move || {
        for i in (1..=9).step_by(2) {
            // Simulate variable processing time
            thread::sleep(Duration::from_millis(i * 10));
            s2.send(SequencedRecord::new(i, format!("Worker2: Record {i}")))
                .unwrap();
        }
    });
    handles.push(handle2);

    // Drop the original sender so the ring knows when all work is done
    drop(sender2);

    // Receive results in order while workers are still running
    println!("Receiving records in sequence order (despite out-of-order processing):");
    let mut count = 0;
    while let Some(record) = ring2.recv_ordered().unwrap() {
        count += 1;
        println!("  Sequence {count}: {record}");
    }

    // Wait for workers to complete
    for handle in handles {
        handle.join().unwrap();
    }

    // Example 3: Tuning parameters
    println!("\n--- Example 3: Parameter Tuning ---");
    println!("Channel capacity: Number of records in flight between workers and consumer");
    println!("Reorder window: Maximum buffered out-of-order records (warning threshold)");
    println!();
    println!("Tuning Guidelines:");
    println!("  - Channel capacity: 2-4x number of worker threads");
    println!("  - Reorder window: channel_capacity / 2");
    println!();
    println!("Examples:");
    println!("  - 4 workers: 16 capacity, 8 window");
    println!("  - 8 workers: 32 capacity, 16 window");
    println!("  - 16 workers: 64 capacity, 32 window");

    // Example 4: Statistics
    println!("\n--- Example 4: Statistics ---");
    let mut ring3 = SequenceRing::new(10, 5);
    let sender3 = ring3.sender();

    // Send some records
    sender3.send(SequencedRecord::new(1, "data1")).unwrap();
    sender3.send(SequencedRecord::new(2, "data2")).unwrap();
    sender3.send(SequencedRecord::new(3, "data3")).unwrap();

    // Get statistics
    let stats = ring3.stats();
    println!("Statistics:");
    println!("  Channel capacity: {}", stats.channel_capacity);
    println!("  Max window size: {}", stats.max_window_size);
    println!("  Next sequence ID: {}", stats.next_sequence_id);
    println!("  Reorder buffer size: {}", stats.reorder_buffer_size);

    // Receive records
    while ring3.recv_ordered().unwrap().is_some() {}

    println!("\n=== Performance Characteristics ===");
    println!("✅ O(log n) insertion/removal from reorder buffer (BTreeMap)");
    println!("✅ O(1) emission when records arrive in order (hot path)");
    println!("✅ Memory usage bounded by channel capacity + reorder window");
}
