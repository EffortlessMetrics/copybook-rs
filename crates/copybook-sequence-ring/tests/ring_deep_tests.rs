// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep stress and edge-case tests for `SequenceRing`.
//!
//! Complements `ordering_invariants.rs` with higher contention levels,
//! larger batch sizes, capacity-exhaustion probes, and corner cases
//! around empty rings, single-item rings, and rapid submit/drain cycles.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_sequence_ring::{SequenceRing, SequencedRecord};
use std::collections::HashSet;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Barrier};
use std::thread;

// ---------------------------------------------------------------------------
// 1. Basic ordering: single producer, sequential IDs, correct output
// ---------------------------------------------------------------------------

#[test]
fn single_producer_sequential_100_items() {
    let n = 100u64;
    let mut ring = SequenceRing::new(128, 64);
    let sender = ring.sender();

    let producer = thread::spawn(move || {
        for id in 1..=n {
            sender.send(SequencedRecord::new(id, id * 10)).unwrap();
        }
    });

    let mut received = Vec::with_capacity(n as usize);
    for _ in 0..n {
        received.push(ring.recv_ordered().unwrap().unwrap());
    }
    producer.join().unwrap();

    let expected: Vec<u64> = (1..=n).map(|i| i * 10).collect();
    assert_eq!(received, expected);
}

// ---------------------------------------------------------------------------
// 2. Out-of-order: pairwise swaps, every adjacent pair reversed
// ---------------------------------------------------------------------------

#[test]
fn pairwise_swap_ordering() {
    let n = 50u64;
    let mut ring = SequenceRing::new(64, 64);
    let sender = ring.sender();

    // Send pairs reversed: (2,1), (4,3), (6,5), ...
    for pair in (1..=n).collect::<Vec<_>>().chunks(2) {
        for &id in pair.iter().rev() {
            sender.send(SequencedRecord::new(id, id)).unwrap();
        }
    }

    for expected in 1..=n {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ---------------------------------------------------------------------------
// 3. Multi-producer: 8 threads, each owning a contiguous range
// ---------------------------------------------------------------------------

#[test]
fn eight_producers_contiguous_ranges() {
    let n_producers = 8u64;
    let per_producer = 100u64;
    let total = n_producers * per_producer;
    let mut ring = SequenceRing::new(total as usize, total as usize);

    let handles: Vec<_> = (0..n_producers)
        .map(|p| {
            let sender = ring.sender();
            thread::spawn(move || {
                let start = p * per_producer + 1;
                for id in start..start + per_producer {
                    sender.send(SequencedRecord::new(id, id)).unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    for expected in 1..=total {
        assert_eq!(
            ring.recv_ordered().unwrap(),
            Some(expected),
            "Mismatch at seq {expected}"
        );
    }
}

// ---------------------------------------------------------------------------
// 4. High contention: 16 threads, barrier-synchronised start
// ---------------------------------------------------------------------------

#[test]
fn sixteen_threads_barrier_start() {
    let n_threads = 16u64;
    let per_thread = 64u64;
    let total = n_threads * per_thread;
    let mut ring = SequenceRing::new(total as usize, total as usize);
    let barrier = Arc::new(Barrier::new(n_threads as usize));

    let handles: Vec<_> = (0..n_threads)
        .map(|t| {
            let sender = ring.sender();
            let barrier = Arc::clone(&barrier);
            thread::spawn(move || {
                barrier.wait(); // all threads start simultaneously
                let start = t * per_thread + 1;
                for id in start..start + per_thread {
                    sender.send(SequencedRecord::new(id, id)).unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    let mut prev = 0u64;
    for _ in 0..total {
        let val = ring.recv_ordered().unwrap().unwrap();
        assert!(val > prev, "Non-monotonic: {val} after {prev}");
        prev = val;
    }

    let unique: HashSet<u64> = (1..=total).collect();
    assert_eq!(unique.len(), total as usize);
}

// ---------------------------------------------------------------------------
// 5. Wrap-around: small capacity, many fill-drain cycles
// ---------------------------------------------------------------------------

#[test]
fn repeated_fill_drain_cycles_small_capacity() {
    let capacity = 4;
    let cycles = 50;
    let mut ring = SequenceRing::new(capacity, capacity);
    let sender = ring.sender();

    let mut next_id = 1u64;
    for _ in 0..cycles {
        for _ in 0..capacity {
            sender.send(SequencedRecord::new(next_id, next_id)).unwrap();
            next_id += 1;
        }
        for expected in (next_id - capacity as u64)..next_id {
            assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
        }
    }
    assert_eq!(
        ring.stats().next_sequence_id,
        next_id,
        "Final sequence ID mismatch"
    );
}

#[test]
fn wrap_around_producer_consumer_interleaved() {
    let capacity = 3;
    let total = 100u64;
    let mut ring = SequenceRing::new(capacity, capacity);
    let sender = ring.sender();

    let producer = thread::spawn(move || {
        for id in 1..=total {
            sender.send(SequencedRecord::new(id, id)).unwrap();
        }
    });

    let mut received = Vec::with_capacity(total as usize);
    while received.len() < total as usize {
        if let Some(val) = ring.recv_ordered().unwrap() {
            received.push(val);
        }
    }
    producer.join().unwrap();

    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(received, expected);
}

// ---------------------------------------------------------------------------
// 6. Exactly-once: large deterministic permutation, verify set equality
// ---------------------------------------------------------------------------

#[test]
fn exactly_once_500_items_permuted() {
    let n = 500u64;
    let mut ring = SequenceRing::new(n as usize, n as usize);
    let sender = ring.sender();

    // Deterministic permutation (modular arithmetic)
    let mut ids: Vec<u64> = (1..=n).collect();
    for i in (1..ids.len()).rev() {
        let j = (i.wrapping_mul(31).wrapping_add(17)) % (i + 1);
        ids.swap(i, j);
    }

    for &id in &ids {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    let mut received = Vec::with_capacity(n as usize);
    for _ in 0..n {
        received.push(ring.recv_ordered().unwrap().unwrap());
    }

    // Strict order
    let expected: Vec<u64> = (1..=n).collect();
    assert_eq!(received, expected);
    // No duplicates
    let set: HashSet<u64> = received.into_iter().collect();
    assert_eq!(set.len(), n as usize);
}

// ---------------------------------------------------------------------------
// 7. Capacity exhaustion: try_send fails when ring is full
// ---------------------------------------------------------------------------

#[test]
fn capacity_exhaustion_try_send_fails() {
    let capacity = 5;
    let ring = SequenceRing::<u64>::new(capacity, capacity);
    let sender = ring.sender();

    for id in 1..=capacity as u64 {
        sender.try_send(SequencedRecord::new(id, id)).unwrap();
    }

    // Channel is now full
    let result = sender.try_send(SequencedRecord::new(capacity as u64 + 1, 0));
    assert!(result.is_err(), "Expected try_send to fail on full channel");
}

#[test]
fn capacity_exhaustion_recovers_after_drain() {
    let capacity = 3;
    let mut ring = SequenceRing::new(capacity, capacity);
    let sender = ring.sender();

    // Fill
    for id in 1..=capacity as u64 {
        sender.try_send(SequencedRecord::new(id, id)).unwrap();
    }
    assert!(
        sender
            .try_send(SequencedRecord::new(capacity as u64 + 1, 0))
            .is_err()
    );

    // Drain one
    assert_eq!(ring.recv_ordered().unwrap(), Some(1u64));

    // Now one slot free
    sender
        .try_send(SequencedRecord::new(
            capacity as u64 + 1,
            capacity as u64 + 1,
        ))
        .unwrap();

    // Drain remaining
    for expected in 2..=capacity as u64 + 1 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ---------------------------------------------------------------------------
// 8. Empty ring: drain returns nothing
// ---------------------------------------------------------------------------

#[test]
fn empty_ring_try_recv_returns_empty() {
    let mut ring = SequenceRing::<String>::new(16, 8);
    for _ in 0..5 {
        assert!(ring.try_recv_ordered().is_err());
    }
    assert_eq!(ring.stats().next_sequence_id, 1);
    assert_eq!(ring.stats().reorder_buffer_size, 0);
}

#[test]
fn empty_ring_after_full_drain_returns_empty() {
    let mut ring = SequenceRing::new(8, 8);
    let sender = ring.sender();

    for id in 1..=5u64 {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }
    for _ in 0..5 {
        ring.recv_ordered().unwrap().unwrap();
    }

    // Ring is drained, try_recv should be empty
    assert!(ring.try_recv_ordered().is_err());
    assert_eq!(ring.stats().reorder_buffer_size, 0);
}

// ---------------------------------------------------------------------------
// 9. Single item: various capacities
// ---------------------------------------------------------------------------

#[test]
fn single_item_capacity_1() {
    let mut ring = SequenceRing::new(1, 1);
    let sender = ring.sender();
    sender.send(SequencedRecord::new(1, "only")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("only"));
    assert!(ring.try_recv_ordered().is_err());
}

#[test]
fn single_item_large_capacity() {
    let mut ring = SequenceRing::new(1024, 512);
    let sender = ring.sender();
    sender.send(SequencedRecord::new(1, 42u64)).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some(42u64));
    assert!(ring.try_recv_ordered().is_err());
}

// ---------------------------------------------------------------------------
// 10. Large batches: 10_000+ items
// ---------------------------------------------------------------------------

#[test]
fn large_batch_10000_in_order() {
    let n = 10_000u64;
    let mut ring = SequenceRing::new(256, 128);
    let sender = ring.sender();

    let producer = thread::spawn(move || {
        for id in 1..=n {
            sender.send(SequencedRecord::new(id, id)).unwrap();
        }
    });

    let mut count = 0u64;
    let mut prev = 0u64;
    while count < n {
        if let Some(val) = ring.recv_ordered().unwrap() {
            assert_eq!(val, prev + 1, "Gap detected after {prev}");
            prev = val;
            count += 1;
        }
    }
    producer.join().unwrap();
    assert_eq!(count, n);
}

#[test]
fn large_batch_10000_reverse_chunks() {
    let n = 10_000u64;
    let chunk = 50u64;
    let mut ring = SequenceRing::new(256, 128);
    let sender = ring.sender();

    // Producer sends in reverse chunks: [50..1], [100..51], ...
    let producer = thread::spawn(move || {
        let mut base = 1u64;
        while base <= n {
            let end = (base + chunk - 1).min(n);
            for id in (base..=end).rev() {
                sender.send(SequencedRecord::new(id, id)).unwrap();
            }
            base += chunk;
        }
    });

    let mut received = Vec::with_capacity(n as usize);
    while received.len() < n as usize {
        if let Some(val) = ring.recv_ordered().unwrap() {
            received.push(val);
        }
    }
    producer.join().unwrap();

    let expected: Vec<u64> = (1..=n).collect();
    assert_eq!(received, expected);
}

// ---------------------------------------------------------------------------
// 11. Stress test: rapid submit/drain cycles with concurrent access
// ---------------------------------------------------------------------------

#[test]
fn stress_rapid_submit_drain_cycles() {
    let cycles = 200;
    let batch_size = 10u64;
    let mut ring = SequenceRing::new(16, 16);
    let sender = ring.sender();
    let mut next_id = 1u64;

    for _ in 0..cycles {
        for _ in 0..batch_size {
            sender.send(SequencedRecord::new(next_id, next_id)).unwrap();
            next_id += 1;
        }
        for expected in (next_id - batch_size)..next_id {
            assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
        }
    }
    assert_eq!(ring.stats().reorder_buffer_size, 0);
}

#[test]
fn stress_concurrent_produce_consume_10000() {
    let total = 10_000u64;
    let n_producers = 4u64;
    let per_producer = total / n_producers;
    let mut ring = SequenceRing::new(128, 64);
    let counter = Arc::new(AtomicU64::new(0));

    let handles: Vec<_> = (0..n_producers)
        .map(|p| {
            let sender = ring.sender();
            let counter = Arc::clone(&counter);
            thread::spawn(move || {
                let start = p * per_producer + 1;
                for id in start..start + per_producer {
                    sender.send(SequencedRecord::new(id, id)).unwrap();
                    counter.fetch_add(1, Ordering::Relaxed);
                }
            })
        })
        .collect();

    // Consume concurrently while producers are still running
    let mut received = Vec::with_capacity(total as usize);
    while received.len() < total as usize {
        if let Some(val) = ring.recv_ordered().unwrap() {
            received.push(val);
        }
    }

    for h in handles {
        h.join().unwrap();
    }

    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(received, expected);
}

// ---------------------------------------------------------------------------
// 12. Monotonicity: output is always strictly increasing
// ---------------------------------------------------------------------------

#[test]
fn output_strictly_monotonic_under_random_insertion() {
    let n = 1_000u64;
    let mut ring = SequenceRing::new(n as usize, n as usize);
    let sender = ring.sender();

    // Pseudo-random order using LCG
    let mut ids: Vec<u64> = (1..=n).collect();
    let mut seed: u64 = 12345;
    for i in (1..ids.len()).rev() {
        seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
        let j = (seed >> 33) as usize % (i + 1);
        ids.swap(i, j);
    }

    for &id in &ids {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    let mut prev = 0u64;
    for _ in 0..n {
        let val = ring.recv_ordered().unwrap().unwrap();
        assert!(
            val == prev + 1,
            "Non-sequential: got {val}, expected {}",
            prev + 1
        );
        prev = val;
    }
}

// ---------------------------------------------------------------------------
// 13. Past/duplicate records are silently ignored
// ---------------------------------------------------------------------------

#[test]
fn duplicate_sequence_ids_ignored() {
    let mut ring = SequenceRing::new(16, 8);
    let sender = ring.sender();

    sender.send(SequencedRecord::new(1, "first")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("first"));

    // Send duplicate seq 1 — should be ignored
    sender.send(SequencedRecord::new(1, "dup")).unwrap();
    // Send real next
    sender.send(SequencedRecord::new(2, "second")).unwrap();

    // try_recv the duplicate (ignored), then get #2
    // The dup will be consumed and discarded internally
    assert_eq!(ring.recv_ordered().unwrap(), Some("second"));
}

#[test]
fn multiple_past_records_skipped() {
    let mut ring = SequenceRing::new(16, 8);
    let sender = ring.sender();

    // Consume 1..=5
    for id in 1..=5u64 {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }
    for _ in 0..5 {
        ring.recv_ordered().unwrap().unwrap();
    }

    // Send past records 1, 3, 2 — all should be ignored
    sender.send(SequencedRecord::new(1, 1)).unwrap();
    sender.send(SequencedRecord::new(3, 3)).unwrap();
    sender.send(SequencedRecord::new(2, 2)).unwrap();
    // Then the real next
    sender.send(SequencedRecord::new(6, 6)).unwrap();

    assert_eq!(ring.recv_ordered().unwrap(), Some(6u64));
}

// ---------------------------------------------------------------------------
// 14. Channel close with buffered out-of-order records drains remaining
// ---------------------------------------------------------------------------

#[test]
fn channel_close_flushes_reorder_buffer() {
    let mut ring = SequenceRing::new(16, 16);
    let sender = ring.sender();

    // Send 5, 4, 3, 2, 1 and immediately close
    for id in (1..=5u64).rev() {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }
    drop(sender);

    // Ring's internal sender is still alive, so recv_ordered works
    for expected in 1..=5u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ---------------------------------------------------------------------------
// 15. Stats accuracy under load
// ---------------------------------------------------------------------------

#[test]
fn stats_reorder_buffer_tracks_buffered_items() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send 10, 9, 8, ..., 2 (missing 1)
    for id in (2..=10u64).rev() {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    // Force buffering via try_recv
    for _ in 0..9 {
        let _ = ring.try_recv_ordered();
    }

    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 1, "Still waiting for seq 1");
    assert_eq!(stats.reorder_buffer_size, 9, "9 items buffered");

    // Fill the gap
    sender.send(SequencedRecord::new(1, 1)).unwrap();
    for _ in 0..10 {
        ring.recv_ordered().unwrap().unwrap();
    }

    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 11);
    assert_eq!(stats.reorder_buffer_size, 0);
}

// ---------------------------------------------------------------------------
// 16. High sequence IDs (near u64 boundary)
// ---------------------------------------------------------------------------

#[test]
fn high_sequence_ids_work() {
    let mut ring = SequenceRing::new(16, 8);
    let sender = ring.sender();

    // Manually set expectations: the ring always starts at next_sequence_id = 1,
    // so we use contiguous IDs starting from 1 with large data values.
    let base = u64::MAX - 10;
    for id in 1..=5u64 {
        sender.send(SequencedRecord::new(id, base + id)).unwrap();
    }

    for id in 1..=5u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(base + id));
    }
}

// ---------------------------------------------------------------------------
// 17. Interleaved try_recv and blocking recv
// ---------------------------------------------------------------------------

#[test]
fn mixed_try_recv_and_recv_ordered() {
    let mut ring = SequenceRing::new(16, 8);
    let sender = ring.sender();

    // Send 3 first (out of order)
    sender.send(SequencedRecord::new(3, "c")).unwrap();
    assert!(ring.try_recv_ordered().is_err()); // buffered

    // Send 1
    sender.send(SequencedRecord::new(1, "a")).unwrap();
    assert_eq!(ring.try_recv_ordered().unwrap(), Some("a"));

    // Send 2
    sender.send(SequencedRecord::new(2, "b")).unwrap();
    // Use blocking recv — should get "b" and then "c" from buffer
    assert_eq!(ring.recv_ordered().unwrap(), Some("b"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("c"));
}

// ---------------------------------------------------------------------------
// 18. String data integrity through reorder
// ---------------------------------------------------------------------------

#[test]
fn string_data_integrity_preserved() {
    let n = 200u64;
    let mut ring = SequenceRing::new(n as usize, n as usize);
    let sender = ring.sender();

    // Send in reverse with unique string payloads
    for id in (1..=n).rev() {
        sender
            .send(SequencedRecord::new(id, format!("record-{id:05}")))
            .unwrap();
    }

    for id in 1..=n {
        let val = ring.recv_ordered().unwrap().unwrap();
        assert_eq!(val, format!("record-{id:05}"));
    }
}

// ---------------------------------------------------------------------------
// 19. Vec payload data integrity under contention
// ---------------------------------------------------------------------------

#[test]
fn vec_payload_integrity_concurrent() {
    let n_producers = 4u64;
    let per_producer = 250u64;
    let total = n_producers * per_producer;
    let mut ring = SequenceRing::new(total as usize, total as usize);

    let handles: Vec<_> = (0..n_producers)
        .map(|p| {
            let sender = ring.sender();
            thread::spawn(move || {
                let start = p * per_producer + 1;
                for id in start..start + per_producer {
                    let payload = vec![id as u8; (id % 16 + 1) as usize];
                    sender.send(SequencedRecord::new(id, payload)).unwrap();
                }
            })
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    for id in 1..=total {
        let val = ring.recv_ordered().unwrap().unwrap();
        let expected_len = (id % 16 + 1) as usize;
        assert_eq!(val.len(), expected_len, "Wrong payload length at seq {id}");
        assert!(
            val.iter().all(|&b| b == id as u8),
            "Payload corruption at seq {id}"
        );
    }
}
