// SPDX-License-Identifier: AGPL-3.0-or-later
//! Determinism hardening tests for parallel ordering and decode stability.
//!
//! These tests verify that decode, schema fingerprinting, JSON key ordering,
//! field ordering, OCCURS arrays, and REDEFINES all behave deterministically
//! across repeated runs and simulated parallel workers.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::determinism::check_decode_determinism;
use copybook_codec::{Codepage, DecodeOptions, decode_record};
use copybook_core::parse_copybook;
use copybook_determinism::{DeterminismMode, blake3_hex, compare_outputs};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn default_decode_opts() -> DecodeOptions {
    DecodeOptions::new().with_codepage(Codepage::CP037)
}

/// Simple DISPLAY-only copybook with multiple fields.
const MULTI_FIELD_COPYBOOK: &str = r"
       01 RECORD.
          05 FIRST-NAME   PIC X(10).
          05 LAST-NAME    PIC X(10).
          05 AGE          PIC 9(3).
          05 BALANCE      PIC S9(7)V99.
";

/// EBCDIC CP037 data for MULTI_FIELD_COPYBOOK (10 + 10 + 3 + 9 = 32 bytes).
/// "JOHN      " "SMITH     " "025" "001234567"
fn multi_field_data() -> Vec<u8> {
    let mut data = Vec::with_capacity(32);
    // FIRST-NAME: "JOHN      " in EBCDIC CP037
    data.extend_from_slice(&[0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40]);
    // LAST-NAME: "SMITH     " in EBCDIC CP037
    data.extend_from_slice(&[0xE2, 0xD4, 0xC9, 0xE3, 0xC8, 0x40, 0x40, 0x40, 0x40, 0x40]);
    // AGE: "025" in EBCDIC CP037
    data.extend_from_slice(&[0xF0, 0xF2, 0xF5]);
    // BALANCE: "001234567" in EBCDIC CP037 (signed trailing overpunch)
    data.extend_from_slice(&[0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7]);
    data
}

// ---------------------------------------------------------------------------
// Test: Decode N times single-threaded → all identical
// ---------------------------------------------------------------------------

#[test]
fn decode_single_thread_n_times_all_identical() {
    let copybook = r"
       01 RECORD.
          05 FIELD-A PIC X(5).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data: Vec<u8> = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5]; // ABCDE in EBCDIC
    let opts = default_decode_opts();

    let first = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
        .expect("serialize");

    for i in 1..50 {
        let output = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
            .expect("serialize");
        let result = compare_outputs(DeterminismMode::DecodeOnly, &first, &output);
        assert!(
            result.is_deterministic,
            "Single-thread decode diverged on iteration {i}"
        );
    }
}

// ---------------------------------------------------------------------------
// Test: Decode N times with simulated parallel workers → all identical
// ---------------------------------------------------------------------------

#[test]
fn decode_simulated_parallel_workers_all_identical() {
    let copybook = r"
       01 RECORD.
          05 NAME   PIC X(10).
          05 AMOUNT PIC S9(5)V99 COMP-3.
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // 10 bytes DISPLAY + 4 bytes COMP-3 = 14 bytes
    let mut data = vec![0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40];
    data.extend_from_slice(&[0x01, 0x23, 0x45, 0x6C]); // +12345.6 COMP-3
    let opts = default_decode_opts();

    // Establish reference output
    let reference = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
        .expect("serialize");

    for worker_count in [2, 4, 8] {
        let handles: Vec<_> = (0..worker_count)
            .map(|_| {
                let schema = schema.clone();
                let data = data.clone();
                let opts = opts.clone();
                std::thread::spawn(move || {
                    let mut results = Vec::new();
                    for _ in 0..10 {
                        let val = decode_record(&schema, &data, &opts).expect("decode");
                        results.push(serde_json::to_vec(&val).expect("serialize"));
                    }
                    results
                })
            })
            .collect();

        for handle in handles {
            for output in handle.join().expect("thread join") {
                let result = compare_outputs(DeterminismMode::DecodeOnly, &reference, &output);
                assert!(
                    result.is_deterministic,
                    "Parallel decode diverged with {worker_count} workers"
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Test: Schema fingerprint is deterministic (parse same copybook 100 times)
// ---------------------------------------------------------------------------

#[test]
fn schema_fingerprint_deterministic_100_parses() {
    let copybook = r"
       01 CUSTOMER-RECORD.
          05 CUST-ID      PIC 9(8).
          05 CUST-NAME    PIC X(30).
          05 CUST-BALANCE PIC S9(9)V99.
    ";

    let first = parse_copybook(copybook).expect("parse");
    let reference_fp = first.fingerprint.clone();
    assert!(!reference_fp.is_empty(), "Fingerprint should not be empty");

    for i in 1..100 {
        let schema = parse_copybook(copybook).expect("parse");
        assert_eq!(
            schema.fingerprint, reference_fp,
            "Fingerprint diverged on parse iteration {i}"
        );
    }
}

// ---------------------------------------------------------------------------
// Test: JSON key ordering is deterministic across decode runs
// ---------------------------------------------------------------------------

#[test]
fn json_key_ordering_deterministic() {
    let schema = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data = multi_field_data();
    let opts = default_decode_opts();

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_keys: Vec<&String> = first.as_object().expect("object").keys().collect();

    for i in 1..50 {
        let val = decode_record(&schema, &data, &opts).expect("decode");
        let keys: Vec<&String> = val.as_object().expect("object").keys().collect();
        assert_eq!(
            keys, first_keys,
            "JSON key ordering diverged on decode iteration {i}"
        );
    }
}

// ---------------------------------------------------------------------------
// Test: Field order in output matches copybook declaration order
// ---------------------------------------------------------------------------

#[test]
fn field_order_matches_copybook_declaration() {
    let schema = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data = multi_field_data();
    let opts = default_decode_opts();

    let val = decode_record(&schema, &data, &opts).expect("decode");

    // Fields should appear in copybook declaration order. Check that the leaf
    // field names appear in declaration order relative to each other.
    let expected_order = ["FIRST-NAME", "LAST-NAME", "AGE", "BALANCE"];
    let mut found: Vec<&str> = Vec::new();
    collect_leaf_keys(&val, &mut found);

    // Verify relative ordering: each expected field that appears in output
    // preserves the declaration order.
    let mut prev_idx = None;
    for expected in &expected_order {
        if let Some(pos) = found.iter().position(|k| k == expected) {
            if let Some(prev) = prev_idx {
                assert!(
                    pos > prev,
                    "Field {expected} appeared before a prior declaration-order field"
                );
            }
            prev_idx = Some(pos);
        }
    }
}

/// Recursively collect leaf-level JSON keys in traversal order.
fn collect_leaf_keys<'a>(val: &'a serde_json::Value, out: &mut Vec<&'a str>) {
    if let Some(obj) = val.as_object() {
        for (key, child) in obj {
            if child.is_object() {
                collect_leaf_keys(child, out);
            } else {
                out.push(key.as_str());
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Test: OCCURS array element ordering is deterministic
// ---------------------------------------------------------------------------

#[test]
fn occurs_array_element_ordering_deterministic() {
    let copybook = r"
       01 RECORD.
          05 ITEMS OCCURS 5 TIMES.
             10 ITEM-CODE PIC X(3).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // 5 * 3 = 15 bytes: "AAA", "BBB", "CCC", "DDD", "EEE" in EBCDIC
    let data: Vec<u8> = vec![
        0xC1, 0xC1, 0xC1, // AAA
        0xC2, 0xC2, 0xC2, // BBB
        0xC3, 0xC3, 0xC3, // CCC
        0xC4, 0xC4, 0xC4, // DDD
        0xC5, 0xC5, 0xC5, // EEE
    ];
    let opts = default_decode_opts();

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_json = serde_json::to_string(&first).expect("serialize");

    for i in 1..50 {
        let val = decode_record(&schema, &data, &opts).expect("decode");
        let json = serde_json::to_string(&val).expect("serialize");
        assert_eq!(
            json, first_json,
            "OCCURS array ordering diverged on iteration {i}"
        );
    }
}

// ---------------------------------------------------------------------------
// Test: REDEFINES decode always picks primary view deterministically
// ---------------------------------------------------------------------------

#[test]
fn redefines_decode_deterministic() {
    let copybook = r"
       01 RECORD.
          05 FIELD-A      PIC X(10).
          05 FIELD-B REDEFINES FIELD-A PIC 9(10).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // "1234567890" in EBCDIC
    let data: Vec<u8> = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0];
    let opts = default_decode_opts();

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_json = serde_json::to_string(&first).expect("serialize");

    for i in 1..50 {
        let val = decode_record(&schema, &data, &opts).expect("decode");
        let json = serde_json::to_string(&val).expect("serialize");
        assert_eq!(
            json, first_json,
            "REDEFINES decode diverged on iteration {i}"
        );
    }
}

// ---------------------------------------------------------------------------
// Test: BLAKE3 hash stability across decode runs
// ---------------------------------------------------------------------------

#[test]
fn blake3_hash_stable_across_decode_runs() {
    let schema = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data = multi_field_data();
    let opts = default_decode_opts();

    let first = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
        .expect("serialize");
    let reference_hash = blake3_hex(&first);

    for _ in 0..50 {
        let output = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
            .expect("serialize");
        assert_eq!(
            blake3_hex(&output),
            reference_hash,
            "BLAKE3 hash diverged across decode runs"
        );
    }
}

// ---------------------------------------------------------------------------
// Test: check_decode_determinism passes for all field types
// ---------------------------------------------------------------------------

#[test]
fn check_decode_determinism_all_field_types() {
    let copybook = r"
       01 RECORD.
          05 ALPHA      PIC X(5).
          05 NUMERIC    PIC 9(3).
          05 SIGNED-NUM PIC S9(5)V99.
          05 PACKED     PIC S9(5)V99 COMP-3.
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // 5 (X) + 3 (9) + 7 (S9V99) + 4 (COMP-3) = 19 bytes
    let data: Vec<u8> = vec![
        0xC1, 0xC2, 0xC3, 0xC4, 0xC5, // ALPHA: ABCDE
        0xF1, 0xF2, 0xF3, // NUMERIC: 123
        0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, // SIGNED-NUM: 01234.56
        0x01, 0x23, 0x45, 0x6C, // PACKED: +12345.6
    ];
    let opts = default_decode_opts();

    let result = check_decode_determinism(&schema, &data, &opts).expect("check");
    assert!(
        result.is_deterministic,
        "Mixed field types must be deterministic"
    );
}
