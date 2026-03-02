// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for `SequenceRing` determinism hardening.
//!
//! Covers ordering correctness at various scales, backpressure behavior,
//! concurrent producer/consumer determinism, out-of-order insertion patterns,
//! capacity boundary tests, thread-safety proofs, gap handling, and
//! performance overhead validation.
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::cast_possible_truncation
)]

use copybook_sequence_ring::{SequenceRing, SequencedRecord};
use crossbeam_channel::TryRecvError;
use std::collections::HashSet;
use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};
use std::sync::{Arc, Barrier, Mutex};
use std::thread;
use std::time::{Duration, Instant};

// ===========================================================================
// 1. Ordering correctness with 1, 10, 100, 1000 items
// ===========================================================================

#[test]
fn ordering_correctness_1_item() {
    let mut ring = SequenceRing::new(4, 4);
    let sender = ring.sender();
    sender.send(SequencedRecord::new(1, "only")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("only"));
}

#[test]
fn ordering_correctness_10_items_in_order() {
    let mut ring = SequenceRing::new(16, 16);
    let sender = ring.sender();
    for i in 1..=10u64 {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in 1..=10u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn ordering_correctness_10_items_reverse() {
    let mut ring = SequenceRing::new(16, 16);
    let sender = ring.sender();
    for i in (1..=10u64).rev() {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in 1..=10u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn ordering_correctness_100_items_in_order() {
    let mut ring = SequenceRing::new(128, 64);
    let sender = ring.sender();
    for i in 1..=100u64 {
        sender.send(SequencedRecord::new(i, i * 10)).unwrap();
    }
    for expected in 1..=100u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected * 10));
    }
}

#[test]
fn ordering_correctness_100_items_reverse() {
    let mut ring = SequenceRing::new(128, 128);
    let sender = ring.sender();
    for i in (1..=100u64).rev() {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in 1..=100u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn ordering_correctness_1000_items_in_order() {
    let mut ring = SequenceRing::new(1024, 512);
    let sender = ring.sender();
    for i in 1..=1000u64 {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in 1..=1000u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn ordering_correctness_1000_items_reverse() {
    let mut ring = SequenceRing::new(1024, 1024);
    let sender = ring.sender();
    for i in (1..=1000u64).rev() {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in 1..=1000u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ===========================================================================
// 2. Backpressure behavior when ring is full
// ===========================================================================

#[test]
fn backpressure_try_send_fails_when_full() {
    let ring = SequenceRing::new(2, 2);
    let sender = ring.sender();

    sender.try_send(SequencedRecord::new(1, "a")).unwrap();
    sender.try_send(SequencedRecord::new(2, "b")).unwrap();

    // Channel is full — try_send should fail
    let result = sender.try_send(SequencedRecord::new(3, "c"));
    assert!(result.is_err(), "Channel should be full");
}

#[test]
fn backpressure_send_unblocks_after_recv() {
    let mut ring = SequenceRing::new(2, 2);
    let sender = ring.sender();

    sender.send(SequencedRecord::new(1, "a")).unwrap();
    sender.send(SequencedRecord::new(2, "b")).unwrap();

    // Drain one slot
    assert_eq!(ring.recv_ordered().unwrap(), Some("a"));

    // Now there's room for one more
    sender.try_send(SequencedRecord::new(3, "c")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("b"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("c"));
}

#[test]
fn backpressure_producer_blocks_until_consumer_drains() {
    let mut ring = SequenceRing::new(1, 1);
    let sender = ring.sender();

    sender.send(SequencedRecord::new(1, 1u64)).unwrap();

    // Spawn producer that will block on full channel
    let producer = thread::spawn(move || {
        sender.send(SequencedRecord::new(2, 2u64)).unwrap();
    });

    // Small delay to let producer attempt to send
    thread::sleep(Duration::from_millis(10));

    // Consume first item to unblock producer
    assert_eq!(ring.recv_ordered().unwrap(), Some(1u64));

    producer.join().unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some(2u64));
}

// ===========================================================================
// 3. Concurrent producer/consumer determinism
// ===========================================================================

#[test]
fn concurrent_2_producers_deterministic_output() {
    let n = 50u64;
    let mut ring = SequenceRing::new(128, 64);
    let sender1 = ring.sender();
    let sender2 = ring.sender();

    let h1 = thread::spawn(move || {
        for i in (1..=n).step_by(2) {
            sender1.send(SequencedRecord::new(i, i)).unwrap();
        }
    });
    let h2 = thread::spawn(move || {
        for i in (2..=n).step_by(2) {
            sender2.send(SequencedRecord::new(i, i)).unwrap();
        }
    });

    h1.join().unwrap();
    h2.join().unwrap();

    let mut output = Vec::with_capacity(n as usize);
    for _ in 0..n {
        output.push(ring.recv_ordered().unwrap().unwrap());
    }
    let expected: Vec<u64> = (1..=n).collect();
    assert_eq!(output, expected);
}

#[test]
fn concurrent_4_producers_deterministic_output() {
    let per_producer = 25u64;
    let total = per_producer * 4;
    let mut ring = SequenceRing::new(256, 128);

    let handles: Vec<_> = (0..4)
        .map(|worker_id| {
            let sender = ring.sender();
            thread::spawn(move || {
                for i in 0..per_producer {
                    let seq = worker_id * per_producer + i + 1;
                    sender.send(SequencedRecord::new(seq, seq)).unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    let mut output = Vec::with_capacity(total as usize);
    for _ in 0..total {
        output.push(ring.recv_ordered().unwrap().unwrap());
    }
    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(output, expected);
}

#[test]
fn concurrent_producer_consumer_interleaved() {
    let n = 100u64;
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    let producer = thread::spawn(move || {
        for i in 1..=n {
            sender.send(SequencedRecord::new(i, i)).unwrap();
            // Slight delay to interleave with consumer
            if i % 10 == 0 {
                thread::yield_now();
            }
        }
    });

    let mut received = Vec::with_capacity(n as usize);
    for _ in 0..n {
        received.push(ring.recv_ordered().unwrap().unwrap());
    }
    producer.join().unwrap();

    let expected: Vec<u64> = (1..=n).collect();
    assert_eq!(received, expected);
}

// ===========================================================================
// 4. Out-of-order insertion with correct final order
// ===========================================================================

#[test]
fn out_of_order_zigzag_pattern() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send: 2, 1, 4, 3, 6, 5, 8, 7, 10, 9
    for pair_start in (1..=10u64).step_by(2) {
        sender
            .send(SequencedRecord::new(pair_start + 1, pair_start + 1))
            .unwrap();
        sender
            .send(SequencedRecord::new(pair_start, pair_start))
            .unwrap();
    }

    for expected in 1..=10u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn out_of_order_last_first_pattern() {
    let n = 20u64;
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send last item first, then rest in order
    sender.send(SequencedRecord::new(n, n)).unwrap();
    for i in 1..n {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }

    for expected in 1..=n {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn out_of_order_middle_out_pattern() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send from middle outward: 5, 6, 4, 7, 3, 8, 2, 9, 1, 10
    let order = [5u64, 6, 4, 7, 3, 8, 2, 9, 1, 10];
    for &id in &order {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    for expected in 1..=10u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ===========================================================================
// 5. Ring capacity boundary tests
// ===========================================================================

#[test]
fn capacity_1_works_correctly() {
    let mut ring = SequenceRing::new(1, 1);
    let sender = ring.sender();

    sender.send(SequencedRecord::new(1, "first")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("first"));

    sender.send(SequencedRecord::new(2, "second")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("second"));
}

#[test]
fn capacity_exact_fill_and_drain() {
    let cap = 8;
    let mut ring = SequenceRing::new(cap, cap);
    let sender = ring.sender();

    // Fill to exact capacity
    for i in 1..=cap as u64 {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }

    // Drain all
    for expected in 1..=cap as u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn capacity_refill_after_drain() {
    let cap = 4;
    let mut ring = SequenceRing::new(cap, cap);
    let sender = ring.sender();

    // First cycle
    for i in 1..=cap as u64 {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in 1..=cap as u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }

    // Second cycle
    for i in (cap as u64 + 1)..=(2 * cap as u64) {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in (cap as u64 + 1)..=(2 * cap as u64) {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

#[test]
fn capacity_large_ring_1024() {
    let mut ring = SequenceRing::new(1024, 512);
    let sender = ring.sender();

    for i in 1..=1024u64 {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for expected in 1..=1024u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ===========================================================================
// 6. Thread-safety proof (Arc<Mutex> pattern)
// ===========================================================================

#[test]
fn arc_mutex_shared_counter_deterministic() {
    let counter = Arc::new(AtomicU64::new(1));
    let mut ring = SequenceRing::new(256, 128);
    let n_per_thread = 25u64;
    let n_threads = 4;
    let total = n_per_thread * n_threads;

    let handles: Vec<_> = (0..n_threads)
        .map(|_| {
            let c = Arc::clone(&counter);
            let sender = ring.sender();
            thread::spawn(move || {
                for _ in 0..n_per_thread {
                    let seq = c.fetch_add(1, AtomicOrdering::SeqCst);
                    sender.send(SequencedRecord::new(seq, seq)).unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    let mut received = Vec::with_capacity(total as usize);
    for _ in 0..total {
        received.push(ring.recv_ordered().unwrap().unwrap());
    }

    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(received, expected);
}

#[test]
fn arc_mutex_collected_results_no_duplicates() {
    let collected = Arc::new(Mutex::new(Vec::new()));
    let mut ring = SequenceRing::new(128, 64);
    let sender = ring.sender();
    let n = 50u64;

    // Send all records
    for i in 1..=n {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }

    // Consumer collects into Arc<Mutex<Vec>>
    let c = Arc::clone(&collected);
    let consumer = thread::spawn(move || {
        for _ in 0..n {
            let val = ring.recv_ordered().unwrap().unwrap();
            c.lock().unwrap().push(val);
        }
    });

    consumer.join().unwrap();

    let result = collected.lock().unwrap();
    let expected: Vec<u64> = (1..=n).collect();
    assert_eq!(*result, expected);

    // Verify no duplicates
    let unique: HashSet<u64> = result.iter().copied().collect();
    assert_eq!(unique.len(), n as usize);
}

#[test]
fn barrier_synchronized_producers() {
    let n_producers = 4;
    let per_producer = 10u64;
    let total = n_producers as u64 * per_producer;
    let mut ring = SequenceRing::new(256, 128);
    let barrier = Arc::new(Barrier::new(n_producers));
    let counter = Arc::new(AtomicU64::new(1));

    let handles: Vec<_> = (0..n_producers)
        .map(|_| {
            let b = Arc::clone(&barrier);
            let c = Arc::clone(&counter);
            let sender = ring.sender();
            thread::spawn(move || {
                b.wait(); // All producers start at the same time
                for _ in 0..per_producer {
                    let seq = c.fetch_add(1, AtomicOrdering::SeqCst);
                    sender.send(SequencedRecord::new(seq, seq)).unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    let mut output = Vec::with_capacity(total as usize);
    for _ in 0..total {
        output.push(ring.recv_ordered().unwrap().unwrap());
    }
    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(output, expected);
}

// ===========================================================================
// 7. Graceful handling of gaps in sequence numbers
// ===========================================================================

#[test]
fn gap_filled_later_emits_correct_order() {
    let mut ring = SequenceRing::new(16, 16);
    let sender = ring.sender();

    // Send 3, then fill gap with 1, 2
    sender.send(SequencedRecord::new(3, "c")).unwrap();
    sender.send(SequencedRecord::new(1, "a")).unwrap();
    sender.send(SequencedRecord::new(2, "b")).unwrap();

    assert_eq!(ring.recv_ordered().unwrap(), Some("a"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("b"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("c"));
}

#[test]
fn large_gap_buffers_correctly() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send records 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 (large gap initially)
    for i in (1..=10u64).rev() {
        sender.send(SequencedRecord::new(i, i * 100)).unwrap();
    }

    for expected in 1..=10u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected * 100));
    }
}

#[test]
fn gap_with_try_recv_buffers_future_records() {
    let mut ring = SequenceRing::new(16, 16);
    let sender = ring.sender();

    // Send record 5 (expecting 1) — should buffer
    sender.send(SequencedRecord::new(5, "e")).unwrap();
    assert!(matches!(ring.try_recv_ordered(), Err(TryRecvError::Empty)));
    assert_eq!(ring.stats().reorder_buffer_size, 1);

    // Send 3 (still expecting 1)
    sender.send(SequencedRecord::new(3, "c")).unwrap();
    assert!(matches!(ring.try_recv_ordered(), Err(TryRecvError::Empty)));
    assert_eq!(ring.stats().reorder_buffer_size, 2);

    // Now fill gaps: 1, 2, 4
    sender.send(SequencedRecord::new(1, "a")).unwrap();
    sender.send(SequencedRecord::new(2, "b")).unwrap();
    sender.send(SequencedRecord::new(4, "d")).unwrap();

    // Should now emit a, b, c, d, e in order
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("a"));
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("b"));
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("c"));
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("d"));
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("e"));
}

#[test]
fn past_sequence_ids_are_ignored() {
    let mut ring = SequenceRing::new(16, 16);
    let sender = ring.sender();

    // Consume record 1
    sender.send(SequencedRecord::new(1, "a")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("a"));

    // Send duplicate/past record 1
    sender.send(SequencedRecord::new(1, "duplicate")).unwrap();
    // try_recv should return Empty (past record ignored)
    assert!(matches!(ring.try_recv_ordered(), Err(TryRecvError::Empty)));

    // Normal flow continues
    sender.send(SequencedRecord::new(2, "b")).unwrap();
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("b"));
}

// ===========================================================================
// 8. Performance: ordering overhead stays reasonable
// ===========================================================================

#[test]
fn performance_1000_items_completes_in_reasonable_time() {
    let n = 1000u64;
    let mut ring = SequenceRing::new(1024, 512);
    let sender = ring.sender();

    let start = Instant::now();

    for i in 1..=n {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for _ in 0..n {
        ring.recv_ordered().unwrap().unwrap();
    }

    let elapsed = start.elapsed();
    // 1000 ordered items should complete well within 1 second
    assert!(
        elapsed < Duration::from_secs(1),
        "1000 items took {elapsed:?}, expected < 1s"
    );
}

#[test]
fn performance_reverse_order_1000_items_reasonable() {
    let n = 1000u64;
    let mut ring = SequenceRing::new(1024, 1024);
    let sender = ring.sender();

    let start = Instant::now();

    for i in (1..=n).rev() {
        sender.send(SequencedRecord::new(i, i)).unwrap();
    }
    for _ in 0..n {
        ring.recv_ordered().unwrap().unwrap();
    }

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(1),
        "Reverse-order 1000 items took {elapsed:?}, expected < 1s"
    );
}

#[test]
fn performance_concurrent_4_threads_1000_items() {
    let per_thread = 250u64;
    let total = per_thread * 4;
    let mut ring = SequenceRing::new(1024, 512);
    let counter = Arc::new(AtomicU64::new(1));

    let start = Instant::now();

    let handles: Vec<_> = (0..4)
        .map(|_| {
            let c = Arc::clone(&counter);
            let sender = ring.sender();
            thread::spawn(move || {
                for _ in 0..per_thread {
                    let seq = c.fetch_add(1, AtomicOrdering::SeqCst);
                    sender.send(SequencedRecord::new(seq, seq)).unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    for _ in 0..total {
        ring.recv_ordered().unwrap().unwrap();
    }

    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(2),
        "Concurrent 4-thread 1000 items took {elapsed:?}, expected < 2s"
    );
}

// ===========================================================================
// 9. Stats accuracy
// ===========================================================================

#[test]
fn stats_track_progress_accurately() {
    let mut ring = SequenceRing::new(64, 32);
    let sender = ring.sender();

    assert_eq!(ring.stats().next_sequence_id, 1);
    assert_eq!(ring.stats().reorder_buffer_size, 0);

    sender.send(SequencedRecord::new(1, 1u64)).unwrap();
    ring.recv_ordered().unwrap();
    assert_eq!(ring.stats().next_sequence_id, 2);

    sender.send(SequencedRecord::new(2, 2u64)).unwrap();
    sender.send(SequencedRecord::new(3, 3u64)).unwrap();
    ring.recv_ordered().unwrap();
    ring.recv_ordered().unwrap();
    assert_eq!(ring.stats().next_sequence_id, 4);
    assert_eq!(ring.stats().reorder_buffer_size, 0);
}

#[test]
fn stats_show_buffered_out_of_order_items() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send 5, 4, 3 (expecting 1)
    sender.send(SequencedRecord::new(5, 5u64)).unwrap();
    sender.send(SequencedRecord::new(4, 4u64)).unwrap();
    sender.send(SequencedRecord::new(3, 3u64)).unwrap();

    // Force buffering via try_recv
    assert!(matches!(ring.try_recv_ordered(), Err(TryRecvError::Empty)));
    assert!(matches!(ring.try_recv_ordered(), Err(TryRecvError::Empty)));
    assert!(matches!(ring.try_recv_ordered(), Err(TryRecvError::Empty)));

    assert_eq!(ring.stats().reorder_buffer_size, 3);
    assert_eq!(ring.stats().next_sequence_id, 1);
}

// ===========================================================================
// 10. Generic type support
// ===========================================================================

#[test]
fn ring_with_string_payloads() {
    let mut ring = SequenceRing::new(8, 8);
    let sender = ring.sender();

    sender
        .send(SequencedRecord::new(1, "hello world".to_string()))
        .unwrap();
    assert_eq!(
        ring.recv_ordered().unwrap(),
        Some("hello world".to_string())
    );
}

#[test]
fn ring_with_vec_payloads() {
    let mut ring = SequenceRing::new(8, 8);
    let sender = ring.sender();

    sender
        .send(SequencedRecord::new(1, vec![1u8, 2, 3, 4, 5]))
        .unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some(vec![1u8, 2, 3, 4, 5]));
}

#[test]
fn ring_with_tuple_payloads() {
    let mut ring = SequenceRing::new(8, 8);
    let sender = ring.sender();

    sender
        .send(SequencedRecord::new(1, (42, "answer")))
        .unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some((42, "answer")));
}

// ===========================================================================
// 11. Exactly-once delivery
// ===========================================================================

#[test]
fn exactly_once_delivery_no_duplicates() {
    let n = 100u64;
    let mut ring = SequenceRing::new(128, 64);
    let sender = ring.sender();

    // Send scrambled
    let mut order: Vec<u64> = (1..=n).collect();
    // Simple deterministic shuffle: reverse pairs
    for chunk in order.chunks_mut(2) {
        chunk.reverse();
    }
    for &id in &order {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    let mut received = HashSet::new();
    for _ in 0..n {
        let val = ring.recv_ordered().unwrap().unwrap();
        assert!(received.insert(val), "Duplicate value {val} received");
    }
    assert_eq!(received.len(), n as usize);
}

#[test]
fn exactly_once_delivery_preserves_all_values() {
    let n = 50u64;
    let mut ring = SequenceRing::new(64, 64);
    let sender = ring.sender();

    for i in (1..=n).rev() {
        sender.send(SequencedRecord::new(i, i * 7)).unwrap();
    }

    let mut output = Vec::with_capacity(n as usize);
    for _ in 0..n {
        output.push(ring.recv_ordered().unwrap().unwrap());
    }

    let expected: Vec<u64> = (1..=n).map(|i| i * 7).collect();
    assert_eq!(output, expected);
}

// ===========================================================================
// 12. Determinism: same input pattern → same output every time
// ===========================================================================

#[test]
fn deterministic_output_for_same_input_pattern_10_runs() {
    let input_order = [5u64, 3, 1, 4, 2, 10, 8, 6, 9, 7];
    let mut reference: Option<Vec<u64>> = None;

    for run in 0..10 {
        let mut ring = SequenceRing::new(16, 16);
        let sender = ring.sender();

        for &id in &input_order {
            sender.send(SequencedRecord::new(id, id)).unwrap();
        }

        let output: Vec<u64> = (0..10)
            .map(|_| ring.recv_ordered().unwrap().unwrap())
            .collect();

        if let Some(ref expected) = reference {
            assert_eq!(&output, expected, "Output diverged on run {run}");
        } else {
            // Output must be 1..=10
            let expected: Vec<u64> = (1..=10).collect();
            assert_eq!(output, expected);
            reference = Some(output);
        }
    }
}

#[test]
fn deterministic_output_concurrent_pattern_5_runs() {
    for run in 0..5 {
        let n = 20u64;
        let mut ring = SequenceRing::new(64, 32);
        let counter = Arc::new(AtomicU64::new(1));

        let handles: Vec<_> = (0..2)
            .map(|_| {
                let c = Arc::clone(&counter);
                let sender = ring.sender();
                thread::spawn(move || {
                    for _ in 0..10 {
                        let seq = c.fetch_add(1, AtomicOrdering::SeqCst);
                        sender.send(SequencedRecord::new(seq, seq)).unwrap();
                    }
                })
            })
            .collect();

        for h in handles {
            h.join().unwrap();
        }

        let mut output = Vec::with_capacity(n as usize);
        for _ in 0..n {
            output.push(ring.recv_ordered().unwrap().unwrap());
        }
        let expected: Vec<u64> = (1..=n).collect();
        assert_eq!(
            output, expected,
            "Concurrent output not deterministic on run {run}"
        );
    }
}
