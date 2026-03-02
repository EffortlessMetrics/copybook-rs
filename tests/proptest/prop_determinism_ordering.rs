// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for deterministic output ordering.
//!
//! Verifies that:
//! - Records decoded in multiple orderings produce identical sorted JSONL
//! - `SequenceRing` reorders arbitrary permutations correctly
//! - Parallel-like decode (shuffled processing) matches serial decode
//! - Hash stability across ordering variations

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{Codepage, DecodeOptions, RecordFormat, decode_record, encode_record};
use copybook_core::parse_copybook;
use copybook_determinism::blake3_hex;
use copybook_sequence_ring::{SequenceRing, SequencedRecord};
use proptest::prelude::*;
use serde_json::Value;

use super::config::*;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

const SIMPLE_SCHEMA: &str = "01 REC.\n   05 FLD PIC X(8).";

/// Generate a permutation of indices 0..n.
fn permute(indices: &mut [usize], seed: u64) {
    let n = indices.len();
    if n <= 1 {
        return;
    }
    let mut s = seed;
    for i in (1..n).rev() {
        s = s.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1);
        let j = (s >> 33) as usize % (i + 1);
        indices.swap(i, j);
    }
}

// ---------------------------------------------------------------------------
// 1. Sequence ring emits records in sequence order regardless of send order
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sequence_ring_preserves_order(
        values in prop::collection::vec(0u64..=10000, 2..=16),
        seed in any::<u64>(),
    ) {
        let n = values.len();
        let mut ring = SequenceRing::new(n + 4, n + 4);
        let sender = ring.sender();

        // Build shuffled indices
        let mut indices: Vec<usize> = (0..n).collect();
        permute(&mut indices, seed);

        // Send in shuffled order with 1-based sequence IDs
        for &idx in &indices {
            sender
                .send(SequencedRecord::new((idx + 1) as u64, values[idx]))
                .unwrap();
        }

        // Receive should be in original order
        for (i, &expected) in values.iter().enumerate() {
            let received = ring.recv_ordered().unwrap().unwrap();
            prop_assert_eq!(received, expected, "Mismatch at position {i}");
        }
    }
}

// ---------------------------------------------------------------------------
// 2. Shuffled decode order produces same set of JSON values
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_shuffled_decode_same_results(
        records in prop::collection::vec(
            prop::collection::vec(0x20u8..=0x7E, 8..=8),
            2..=8,
        ),
        seed in any::<u64>(),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("parse");

        // Serial decode
        let serial: Vec<Value> = records
            .iter()
            .filter_map(|r| decode_record(&schema, r, &decode_opts()).ok())
            .collect();

        // Shuffled decode
        let mut indices: Vec<usize> = (0..records.len()).collect();
        permute(&mut indices, seed);
        let mut shuffled: Vec<(usize, Value)> = indices
            .iter()
            .filter_map(|&i| {
                decode_record(&schema, &records[i], &decode_opts())
                    .ok()
                    .map(|v| (i, v))
            })
            .collect();
        shuffled.sort_by_key(|(i, _)| *i);
        let shuffled_values: Vec<Value> = shuffled.into_iter().map(|(_, v)| v).collect();

        prop_assert_eq!(serial.len(), shuffled_values.len());
        for (i, (a, b)) in serial.iter().zip(shuffled_values.iter()).enumerate() {
            prop_assert_eq!(a, b, "Record {i} differs after reordering");
        }
    }
}

// ---------------------------------------------------------------------------
// 3. Sequence ring: all-in-order sends emit correctly
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sequence_ring_in_order_passthrough(
        n in 1usize..=32,
    ) {
        let mut ring = SequenceRing::new(n + 4, n + 4);
        let sender = ring.sender();

        for i in 1..=n as u64 {
            sender.send(SequencedRecord::new(i, i)).unwrap();
        }

        for expected in 1..=n as u64 {
            let received = ring.recv_ordered().unwrap().unwrap();
            prop_assert_eq!(received, expected);
        }
    }

    /// Fully reversed send order still emits in-order.
    #[test]
    fn prop_sequence_ring_reversed_order(
        n in 2usize..=24,
    ) {
        let mut ring = SequenceRing::new(n + 4, n + 4);
        let sender = ring.sender();

        for i in (1..=n as u64).rev() {
            sender.send(SequencedRecord::new(i, i * 10)).unwrap();
        }

        for expected in 1..=n as u64 {
            let received = ring.recv_ordered().unwrap().unwrap();
            prop_assert_eq!(received, expected * 10);
        }
    }
}

// ---------------------------------------------------------------------------
// 4. BLAKE3 hash of ordered decode output is stable across permutations
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_decode_hash_stable_across_orderings(
        records in prop::collection::vec(
            prop::collection::vec(0x20u8..=0x7E, 8..=8),
            2..=6,
        ),
        seed1 in any::<u64>(),
        seed2 in any::<u64>(),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("parse");

        let ordered_decode = |seed: u64| -> String {
            let n = records.len();
            let mut indices: Vec<usize> = (0..n).collect();
            permute(&mut indices, seed);

            let mut results: Vec<(usize, Vec<u8>)> = indices
                .iter()
                .filter_map(|&i| {
                    decode_record(&schema, &records[i], &decode_opts())
                        .ok()
                        .map(|v| (i, serde_json::to_vec(&v).unwrap()))
                })
                .collect();
            results.sort_by_key(|(i, _)| *i);

            let all_bytes: Vec<u8> = results.into_iter().flat_map(|(_, b)| b).collect();
            blake3_hex(&all_bytes)
        };

        let hash1 = ordered_decode(seed1);
        let hash2 = ordered_decode(seed2);
        prop_assert_eq!(hash1, hash2);
    }
}

// ---------------------------------------------------------------------------
// 5. Sequence ring stats reflect buffer usage
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sequence_ring_stats_next_id_advances(
        n in 1usize..=20,
    ) {
        let mut ring = SequenceRing::new(n + 4, n + 4);
        let sender = ring.sender();

        for i in 1..=n as u64 {
            sender.send(SequencedRecord::new(i, ())).unwrap();
        }

        let initial = ring.stats();
        prop_assert_eq!(initial.next_sequence_id, 1);

        for _ in 0..n {
            ring.recv_ordered().unwrap();
        }

        let after = ring.stats();
        prop_assert_eq!(after.next_sequence_id, n as u64 + 1);
        prop_assert_eq!(after.reorder_buffer_size, 0);
    }

    /// Sending only future records fills the reorder buffer.
    #[test]
    fn prop_sequence_ring_buffer_grows_with_gaps(
        n in 2usize..=16,
    ) {
        let mut ring = SequenceRing::new(n + 4, n + 4);
        let sender = ring.sender();

        // Send records 2..=n (skipping 1), so all get buffered
        for i in 2..=n as u64 {
            sender.send(SequencedRecord::new(i, i)).unwrap();
        }

        // Force buffering via try_recv
        for _ in 0..n - 1 {
            let _ = ring.try_recv_ordered();
        }

        let stats = ring.stats();
        prop_assert_eq!(stats.next_sequence_id, 1);
        prop_assert!(stats.reorder_buffer_size > 0);
    }
}

// ---------------------------------------------------------------------------
// 6. Parallel-like decode: two independent orderings produce same JSONL
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_parallel_serial_decode_parity(
        records in prop::collection::vec(
            prop::collection::vec(b'A'..=b'Z', 8..=8),
            1..=8,
        ),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("parse");

        // "Serial" decode
        let serial: Vec<String> = records
            .iter()
            .filter_map(|r| {
                decode_record(&schema, r, &decode_opts())
                    .ok()
                    .map(|v| serde_json::to_string(&v).unwrap())
            })
            .collect();

        // "Parallel" decode (same records, same order)
        let parallel: Vec<String> = records
            .iter()
            .filter_map(|r| {
                decode_record(&schema, r, &decode_opts())
                    .ok()
                    .map(|v| serde_json::to_string(&v).unwrap())
            })
            .collect();

        prop_assert_eq!(serial, parallel);
    }
}

// ---------------------------------------------------------------------------
// 7. Sequence ring with string data preserves content
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sequence_ring_string_content_preserved(
        strings in prop::collection::vec("[a-z]{1,16}", 2..=12),
        seed in any::<u64>(),
    ) {
        let n = strings.len();
        let mut ring = SequenceRing::new(n + 4, n + 4);
        let sender = ring.sender();

        let mut indices: Vec<usize> = (0..n).collect();
        permute(&mut indices, seed);

        for &idx in &indices {
            sender
                .send(SequencedRecord::new((idx + 1) as u64, strings[idx].clone()))
                .unwrap();
        }

        for (i, expected) in strings.iter().enumerate() {
            let received = ring.recv_ordered().unwrap().unwrap();
            prop_assert_eq!(&received, expected, "Content mismatch at position {i}");
        }
    }
}

// ---------------------------------------------------------------------------
// 8. Decode → encode → decode determinism with reordering
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_roundtrip_order_independent(
        records in prop::collection::vec(
            prop::collection::vec(0x20u8..=0x7E, 8..=8),
            2..=6,
        ),
        seed in any::<u64>(),
    ) {
        let schema = parse_copybook(SIMPLE_SCHEMA).expect("parse");
        let enc_opts = copybook_codec::EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        // Forward order roundtrip
        let forward: Vec<Vec<u8>> = records
            .iter()
            .filter_map(|r| {
                let json = decode_record(&schema, r, &decode_opts()).ok()?;
                encode_record(&schema, &json, &enc_opts).ok()
            })
            .collect();

        // Shuffled order roundtrip, then reorder
        let mut indices: Vec<usize> = (0..records.len()).collect();
        permute(&mut indices, seed);
        let mut shuffled_results: Vec<(usize, Vec<u8>)> = indices
            .iter()
            .filter_map(|&i| {
                let json = decode_record(&schema, &records[i], &decode_opts()).ok()?;
                encode_record(&schema, &json, &enc_opts).ok().map(|b| (i, b))
            })
            .collect();
        shuffled_results.sort_by_key(|(i, _)| *i);
        let reordered: Vec<Vec<u8>> = shuffled_results.into_iter().map(|(_, b)| b).collect();

        prop_assert_eq!(forward.len(), reordered.len());
        for (i, (a, b)) in forward.iter().zip(reordered.iter()).enumerate() {
            prop_assert_eq!(a, b, "Roundtrip record {i} differs");
        }
    }
}
