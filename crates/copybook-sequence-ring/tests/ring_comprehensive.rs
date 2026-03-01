// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive supplemental tests for `SequenceRing` covering
//! generic type handling, stats accuracy, try_recv edge cases,
//! channel shutdown semantics, and single-thread ordering.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_sequence_ring::{SequenceRing, SequencedRecord};
use crossbeam_channel::TryRecvError;
use std::thread;

// ====================================================================
// 1. Single-thread sequential ordering 1..100
// ====================================================================

#[test]
fn single_thread_sequential_1_to_100() {
    let mut ring = SequenceRing::new(128, 64);
    let sender = ring.sender();

    for id in 1..=100u64 {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    for expected in 1..=100u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ====================================================================
// 2. Out-of-order insertion → ordered retrieval (various patterns)
// ====================================================================

#[test]
fn out_of_order_random_pattern_20_items() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Specific scrambled order
    let order = [
        5, 1, 10, 3, 7, 2, 8, 4, 6, 9, 15, 11, 20, 13, 17, 12, 18, 14, 16, 19,
    ];
    for &id in &order {
        sender.send(SequencedRecord::new(id, id * 100)).unwrap();
    }

    for expected in 1..=20u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected * 100));
    }
}

#[test]
fn out_of_order_groups_of_three_reversed() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send in groups of 3, each group reversed: (3,2,1), (6,5,4), (9,8,7)
    for base in (1..=12u64).step_by(3) {
        for offset in (0..3).rev() {
            sender
                .send(SequencedRecord::new(base + offset, base + offset))
                .unwrap();
        }
    }

    for expected in 1..=12u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ====================================================================
// 3. Empty ring behavior
// ====================================================================

#[test]
fn empty_ring_stats_are_initial() {
    let ring = SequenceRing::<i32>::new(16, 8);
    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 1);
    assert_eq!(stats.reorder_buffer_size, 0);
    assert_eq!(stats.max_window_size, 8);
    assert_eq!(stats.channel_capacity, 16);
}

#[test]
fn empty_ring_try_recv_returns_empty_multiple_times() {
    let mut ring = SequenceRing::<String>::new(4, 2);
    for _ in 0..10 {
        match ring.try_recv_ordered() {
            Err(TryRecvError::Empty) => {}
            other => panic!("expected Empty, got {other:?}"),
        }
    }
    // Stats unchanged
    assert_eq!(ring.stats().next_sequence_id, 1);
}

// ====================================================================
// 4. Capacity limits and backpressure
// ====================================================================

#[test]
fn backpressure_blocks_sender_until_drained() {
    let capacity = 2;
    let mut ring = SequenceRing::new(capacity, capacity);
    let sender = ring.sender();

    // Fill to capacity
    sender.send(SequencedRecord::new(1, "a")).unwrap();
    sender.send(SequencedRecord::new(2, "b")).unwrap();

    // Channel full: try_send fails
    let result = sender.try_send(SequencedRecord::new(3, "c"));
    assert!(result.is_err());

    // Drain one
    assert_eq!(ring.recv_ordered().unwrap(), Some("a"));

    // Now one slot free
    sender.try_send(SequencedRecord::new(3, "c")).unwrap();

    assert_eq!(ring.recv_ordered().unwrap(), Some("b"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("c"));
}

#[test]
fn backpressure_with_producer_thread() {
    let capacity = 4;
    let total = 50u64;
    let mut ring = SequenceRing::new(capacity, capacity);
    let sender = ring.sender();

    let producer = thread::spawn(move || {
        for id in 1..=total {
            sender.send(SequencedRecord::new(id, id)).unwrap();
        }
    });

    let mut received = Vec::new();
    while received.len() < total as usize {
        if let Some(val) = ring.recv_ordered().unwrap() {
            received.push(val);
        }
    }
    producer.join().unwrap();

    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(received, expected);
}

// ====================================================================
// 5. Stats tracking accuracy
// ====================================================================

#[test]
fn stats_next_sequence_id_increments_correctly() {
    let mut ring = SequenceRing::new(32, 16);
    let sender = ring.sender();

    assert_eq!(ring.stats().next_sequence_id, 1);

    sender.send(SequencedRecord::new(1, ())).unwrap();
    ring.recv_ordered().unwrap();
    assert_eq!(ring.stats().next_sequence_id, 2);

    sender.send(SequencedRecord::new(2, ())).unwrap();
    sender.send(SequencedRecord::new(3, ())).unwrap();
    ring.recv_ordered().unwrap();
    ring.recv_ordered().unwrap();
    assert_eq!(ring.stats().next_sequence_id, 4);
}

#[test]
fn stats_reorder_buffer_grows_and_shrinks() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send 5, 4, 3 (all out of order)
    sender.send(SequencedRecord::new(5, 5)).unwrap();
    sender.send(SequencedRecord::new(4, 4)).unwrap();
    sender.send(SequencedRecord::new(3, 3)).unwrap();

    // Force buffering via try_recv
    for _ in 0..3 {
        let _ = ring.try_recv_ordered();
    }
    assert_eq!(ring.stats().reorder_buffer_size, 3);

    // Send 1 and 2 to fill the gap
    sender.send(SequencedRecord::new(1, 1)).unwrap();
    sender.send(SequencedRecord::new(2, 2)).unwrap();

    // Drain all 5
    for _ in 0..5 {
        ring.recv_ordered().unwrap();
    }
    assert_eq!(ring.stats().reorder_buffer_size, 0);
    assert_eq!(ring.stats().next_sequence_id, 6);
}

// ====================================================================
// 6. SequenceRingStats struct properties
// ====================================================================

#[test]
fn stats_clone_is_independent() {
    let ring = SequenceRing::<u8>::new(10, 5);
    let stats1 = ring.stats();
    let stats2 = stats1.clone();
    assert_eq!(stats1.channel_capacity, stats2.channel_capacity);
    assert_eq!(stats1.max_window_size, stats2.max_window_size);
    assert_eq!(stats1.next_sequence_id, stats2.next_sequence_id);
    assert_eq!(stats1.reorder_buffer_size, stats2.reorder_buffer_size);
}

#[test]
fn stats_debug_format_readable() {
    let ring = SequenceRing::<u8>::new(10, 5);
    let stats = ring.stats();
    let debug = format!("{stats:?}");
    assert!(debug.contains("next_sequence_id"));
    assert!(debug.contains("reorder_buffer_size"));
    assert!(debug.contains("max_window_size"));
    assert!(debug.contains("channel_capacity"));
}

// ====================================================================
// 7. SequencedRecord with various data types
// ====================================================================

#[test]
fn sequenced_record_with_string_data() {
    let record = SequencedRecord::new(1, String::from("hello world"));
    assert_eq!(record.sequence_id, 1);
    assert_eq!(record.data, "hello world");
}

#[test]
fn sequenced_record_with_tuple_data() {
    let record = SequencedRecord::new(42, (1, "two", 3.0));
    assert_eq!(record.sequence_id, 42);
    assert_eq!(record.data.0, 1);
    assert_eq!(record.data.1, "two");
}

#[test]
fn sequenced_record_with_option_data() {
    let some_record = SequencedRecord::new(1, Some(42));
    let none_record: SequencedRecord<Option<i32>> = SequencedRecord::new(2, None);
    assert_eq!(some_record.data, Some(42));
    assert_eq!(none_record.data, None);
}

// ====================================================================
// 8. Concurrent insertion with deterministic output
// ====================================================================

#[test]
fn concurrent_4_threads_deterministic_output() {
    let n_threads = 4u64;
    let per_thread = 25u64;
    let total = n_threads * per_thread;
    let mut ring = SequenceRing::new(total as usize, total as usize);

    let handles: Vec<_> = (0..n_threads)
        .map(|t| {
            let sender = ring.sender();
            thread::spawn(move || {
                let start = t * per_thread + 1;
                for id in start..start + per_thread {
                    sender
                        .send(SequencedRecord::new(id, format!("t{t}-{id}")))
                        .unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    // Output must be strictly ordered by sequence ID
    let mut prev_id = 0u64;
    for _ in 0..total {
        let val = ring.recv_ordered().unwrap().unwrap();
        // Extract the sequence id from the string (format: "tX-Y")
        let id: u64 = val.split('-').last().unwrap().parse().unwrap();
        assert!(id > prev_id, "non-monotonic: {id} after {prev_id}");
        prev_id = id;
    }
}

// ====================================================================
// 9. try_recv_ordered edge cases
// ====================================================================

#[test]
fn try_recv_in_order_delivery() {
    let mut ring = SequenceRing::new(16, 8);
    let sender = ring.sender();

    sender.send(SequencedRecord::new(1, "a")).unwrap();
    sender.send(SequencedRecord::new(2, "b")).unwrap();

    assert_eq!(ring.try_recv_ordered().unwrap(), Some("a"));
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("b"));
}

#[test]
fn try_recv_buffers_then_delivers_on_gap_fill() {
    let mut ring = SequenceRing::new(16, 8);
    let sender = ring.sender();

    // Send 3 first (out of order)
    sender.send(SequencedRecord::new(3, 30)).unwrap();
    assert!(ring.try_recv_ordered().is_err()); // buffered

    // Send 2 (still missing 1)
    sender.send(SequencedRecord::new(2, 20)).unwrap();
    assert!(ring.try_recv_ordered().is_err()); // buffered

    assert_eq!(ring.stats().reorder_buffer_size, 2);

    // Send 1 (completes the sequence)
    sender.send(SequencedRecord::new(1, 10)).unwrap();
    assert_eq!(ring.try_recv_ordered().unwrap(), Some(10));
    assert_eq!(ring.try_recv_ordered().unwrap(), Some(20));
    assert_eq!(ring.try_recv_ordered().unwrap(), Some(30));
}

// ====================================================================
// 10. Channel shutdown drains remaining records
// ====================================================================

#[test]
fn channel_shutdown_emits_buffered_in_order() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send 10..1 in reverse, then drop external sender
    for id in (1..=10u64).rev() {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }
    drop(sender);

    // Ring's internal sender keeps channel alive
    for expected in 1..=10u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ====================================================================
// 11. Multiple sender clones
// ====================================================================

#[test]
fn multiple_sender_clones_all_deliver() {
    let mut ring = SequenceRing::new(32, 16);
    let s1 = ring.sender();
    let s2 = ring.sender();
    let s3 = ring.sender();

    s1.send(SequencedRecord::new(1, "from-s1")).unwrap();
    s2.send(SequencedRecord::new(2, "from-s2")).unwrap();
    s3.send(SequencedRecord::new(3, "from-s3")).unwrap();

    assert_eq!(ring.recv_ordered().unwrap(), Some("from-s1"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("from-s2"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("from-s3"));
}
