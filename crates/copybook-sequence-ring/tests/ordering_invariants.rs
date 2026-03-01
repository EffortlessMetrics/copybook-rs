// SPDX-License-Identifier: AGPL-3.0-or-later
//! Ordering invariant tests for `SequenceRing`.
//!
//! Validates that the sequence ring correctly reorders out-of-order records,
//! handles gaps, concurrent producers, wrap-around at capacity boundaries,
//! serial (capacity-1) mode, and exactly-once delivery guarantees.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_sequence_ring::{SequenceRing, SequencedRecord};
use std::collections::HashSet;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;

// ---------------------------------------------------------------------------
// Test: Insert elements out of order → emit in correct order
// ---------------------------------------------------------------------------

#[test]
fn out_of_order_insert_emits_in_order() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send in scrambled order: 5, 3, 1, 4, 2
    for &id in &[5u64, 3, 1, 4, 2] {
        sender
            .send(SequencedRecord::new(id, format!("item-{id}")))
            .unwrap();
    }

    // Must emit 1..=5 in order
    for expected in 1..=5u64 {
        let val = ring.recv_ordered().unwrap().unwrap();
        assert_eq!(
            val,
            format!("item-{expected}"),
            "Wrong order at seq {expected}"
        );
    }
}

#[test]
fn fully_reversed_input_emits_in_order() {
    let n = 20u64;
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    for id in (1..=n).rev() {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    for expected in 1..=n {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ---------------------------------------------------------------------------
// Test: Insert with gaps → blocks until gap filled
// ---------------------------------------------------------------------------

#[test]
fn gap_blocks_until_filled() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send record 3 first (gap: 1 and 2 are missing)
    sender.send(SequencedRecord::new(3, "three")).unwrap();

    // try_recv should not yield anything since 1 is missing
    let result = ring.try_recv_ordered();
    assert!(result.is_err() || result.unwrap().is_none());

    // Fill gap: send 1, then 2
    sender.send(SequencedRecord::new(1, "one")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("one"));

    sender.send(SequencedRecord::new(2, "two")).unwrap();
    assert_eq!(ring.recv_ordered().unwrap(), Some("two"));
    assert_eq!(ring.recv_ordered().unwrap(), Some("three"));
}

#[test]
fn large_gap_eventually_resolves() {
    let mut ring = SequenceRing::new(64, 64);
    let sender = ring.sender();

    // Send records 10, 9, ..., 2 (gap at 1)
    for id in (2..=10).rev() {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    // Nothing can be emitted yet
    assert!(ring.try_recv_ordered().is_err());

    // Fill the gap
    sender.send(SequencedRecord::new(1, 1u64)).unwrap();

    // Now all 10 should emit in order
    for expected in 1..=10u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ---------------------------------------------------------------------------
// Test: Concurrent producers, single consumer → output in sequence
// ---------------------------------------------------------------------------

#[test]
fn concurrent_producers_single_consumer_in_sequence() {
    let n_per_producer = 50u64;
    let n_producers = 4;
    let total = n_per_producer * n_producers;
    // Capacity must be >= total to avoid deadlock (producers join before consume)
    let mut ring = SequenceRing::new(total as usize, total as usize);

    let handles: Vec<_> = (0..n_producers)
        .map(|p| {
            let sender = ring.sender();
            thread::spawn(move || {
                let start = p * n_per_producer + 1;
                let end = start + n_per_producer;
                for id in start..end {
                    sender.send(SequencedRecord::new(id, id)).unwrap();
                }
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }

    // Consumer must receive 1..=total in order
    for expected in 1..=total {
        let val = ring.recv_ordered().unwrap().unwrap();
        assert_eq!(val, expected, "Concurrent ordering broken at seq {expected}");
    }
}

#[test]
fn concurrent_producers_interleaved() {
    let n = 50u64;
    let mut ring = SequenceRing::new(128, 64);

    // Two producers: odd and even sequence IDs
    let sender_odd = ring.sender();
    let sender_even = ring.sender();

    let h1 = thread::spawn(move || {
        for id in (1..=n).step_by(2) {
            sender_odd.send(SequencedRecord::new(id, id)).unwrap();
        }
    });

    let h2 = thread::spawn(move || {
        for id in (2..=n).step_by(2) {
            sender_even.send(SequencedRecord::new(id, id)).unwrap();
        }
    });

    h1.join().unwrap();
    h2.join().unwrap();

    for expected in 1..=n {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ---------------------------------------------------------------------------
// Test: Wrap-around at capacity boundary → correct ordering maintained
// ---------------------------------------------------------------------------

#[test]
fn wrap_around_at_capacity_boundary() {
    // Use a small capacity to force wrap-around behavior
    let capacity = 4;
    let mut ring = SequenceRing::new(capacity, capacity);
    let sender = ring.sender();

    // Process in batches that exceed channel capacity, requiring drain cycles
    let total = 20u64;
    let done = Arc::new(AtomicBool::new(false));
    let done_clone = done.clone();

    let producer = {
        let sender = sender.clone();
        thread::spawn(move || {
            for id in 1..=total {
                sender.send(SequencedRecord::new(id, id)).unwrap();
            }
            done_clone.store(true, Ordering::Release);
        })
    };

    let mut received = Vec::with_capacity(total as usize);
    while received.len() < total as usize {
        if let Some(val) = ring.recv_ordered().unwrap() {
            received.push(val);
        }
    }

    producer.join().unwrap();

    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(received, expected, "Wrap-around broke ordering");
}

#[test]
fn capacity_boundary_exact_fill_and_refill() {
    let capacity = 8;
    let mut ring = SequenceRing::new(capacity, capacity);
    let sender = ring.sender();

    // Fill to exact capacity
    for id in 1..=capacity as u64 {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    // Drain all
    for expected in 1..=capacity as u64 {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }

    // Refill with next batch
    let base = capacity as u64;
    for id in (base + 1)..=(base + capacity as u64) {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    for expected in (base + 1)..=(base + capacity as u64) {
        assert_eq!(ring.recv_ordered().unwrap(), Some(expected));
    }
}

// ---------------------------------------------------------------------------
// Test: Capacity 1 → serial behavior
// ---------------------------------------------------------------------------

#[test]
fn capacity_one_serial_behavior() {
    let mut ring = SequenceRing::new(1, 1);
    let sender = ring.sender();

    for id in 1..=10u64 {
        sender.send(SequencedRecord::new(id, id)).unwrap();
        let val = ring.recv_ordered().unwrap().unwrap();
        assert_eq!(val, id, "Capacity-1 serial behavior broken at seq {id}");
    }
}

#[test]
fn capacity_one_must_drain_before_next_send() {
    let ring = SequenceRing::new(1, 1);
    let sender = ring.sender();

    sender.send(SequencedRecord::new(1, "a")).unwrap();

    // Channel is full; try_send should fail
    let result = sender.try_send(SequencedRecord::new(2, "b"));
    assert!(
        result.is_err(),
        "Capacity-1 should reject second send without drain"
    );
}

// ---------------------------------------------------------------------------
// Test: Elements emitted exactly once (no duplication, no loss)
// ---------------------------------------------------------------------------

#[test]
fn elements_emitted_exactly_once_no_duplicates_no_loss() {
    let n = 200u64;
    // Capacity >= n so all sends complete before we start consuming
    let mut ring = SequenceRing::new(n as usize, n as usize);
    let sender = ring.sender();

    // Send in a shuffled order using a simple deterministic permutation
    let mut ids: Vec<u64> = (1..=n).collect();
    // Fisher-Yates-like shuffle with deterministic seed
    for i in (1..ids.len()).rev() {
        let j = (i * 7 + 3) % (i + 1);
        ids.swap(i, j);
    }

    for &id in &ids {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    let mut received = Vec::with_capacity(n as usize);
    for _ in 0..n {
        received.push(ring.recv_ordered().unwrap().unwrap());
    }

    // Verify strict sequential order
    let expected: Vec<u64> = (1..=n).collect();
    assert_eq!(
        received, expected,
        "Sequence mismatch: elements lost or reordered"
    );

    // Verify no duplicates
    let unique: HashSet<u64> = received.iter().copied().collect();
    assert_eq!(unique.len(), n as usize, "Duplicate elements detected");
}

#[test]
fn concurrent_exactly_once_guarantee() {
    let n_producers = 4;
    let n_per_producer = 50u64;
    let total = n_producers * n_per_producer;
    // Capacity >= total to avoid deadlock when joining before consuming
    let mut ring = SequenceRing::new(total as usize, total as usize);

    let handles: Vec<_> = (0..n_producers)
        .map(|p| {
            let sender = ring.sender();
            thread::spawn(move || {
                let start = p * n_per_producer + 1;
                let end = start + n_per_producer;
                for id in start..end {
                    sender.send(SequencedRecord::new(id, id)).unwrap();
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

    // All elements present, in order, no duplicates
    let expected: Vec<u64> = (1..=total).collect();
    assert_eq!(received, expected);

    let unique: HashSet<u64> = received.iter().copied().collect();
    assert_eq!(unique.len(), total as usize);
}

// ---------------------------------------------------------------------------
// Test: Stats remain consistent through reordering
// ---------------------------------------------------------------------------

#[test]
fn stats_consistent_after_reorder_drain() {
    let mut ring = SequenceRing::new(32, 32);
    let sender = ring.sender();

    // Send 10 records in reverse
    for id in (1..=10u64).rev() {
        sender.send(SequencedRecord::new(id, id)).unwrap();
    }

    // Drain all
    for _ in 1..=10 {
        ring.recv_ordered().unwrap();
    }

    let stats = ring.stats();
    assert_eq!(stats.next_sequence_id, 11);
    assert_eq!(stats.reorder_buffer_size, 0);
}
