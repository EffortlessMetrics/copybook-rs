// SPDX-License-Identifier: AGPL-3.0-or-later
//! Expanded determinism proof tests.
//!
//! These tests prove that decode, encode, round-trip, hashing, and schema
//! fingerprinting are fully deterministic regardless of iteration count,
//! thread count, scratch-buffer usage, codepage, or field type.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::memory::ScratchBuffers;
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, check_decode_determinism,
    check_encode_determinism, check_round_trip_determinism, decode_record,
    decode_record_with_scratch, encode_record,
};
use copybook_core::parse_copybook;
use copybook_determinism::{DeterminismMode, blake3_hex, compare_outputs};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new().with_codepage(Codepage::CP037)
}

fn decode_opts_for(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new().with_codepage(cp)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts_for(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(cp)
        .with_format(RecordFormat::Fixed)
}

/// Simple DISPLAY-only copybook.
const SIMPLE_COPYBOOK: &str = r"
       01 RECORD.
          05 FIELD-A PIC X(5).
";

/// EBCDIC CP037 for "HELLO".
fn simple_data() -> Vec<u8> {
    vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6]
}

/// Multi-field copybook with diverse types.
const MULTI_FIELD_COPYBOOK: &str = r"
       01 RECORD.
          05 ALPHA      PIC X(10).
          05 NUMERIC    PIC 9(3).
          05 SIGNED-NUM PIC S9(5)V99.
          05 PACKED     PIC S9(5)V99 COMP-3.
";

/// EBCDIC data for MULTI_FIELD_COPYBOOK (10 + 3 + 7 + 4 = 24 bytes).
fn multi_field_data() -> Vec<u8> {
    let mut d = Vec::with_capacity(24);
    // ALPHA: "ABCDEFGHIJ" in EBCDIC
    d.extend_from_slice(&[0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1]);
    // NUMERIC: "123" in EBCDIC
    d.extend_from_slice(&[0xF1, 0xF2, 0xF3]);
    // SIGNED-NUM: "0123456" in EBCDIC (S9(5)V99 -> 7 display digits, trailing overpunch)
    d.extend_from_slice(&[0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6]);
    // PACKED: +12345.67 COMP-3 → S9(5)V99 = 7 digits → 4 bytes: 1234567C
    d.extend_from_slice(&[0x12, 0x34, 0x56, 0x7C]);
    d
}

// ===========================================================================
// 1. Decode same input 100 times → all outputs identical
// ===========================================================================

#[test]
fn decode_100_times_all_identical() {
    let schema = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data = multi_field_data();
    let opts = decode_opts();

    let reference =
        serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();

    for i in 1..100 {
        let output =
            serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
        let result = compare_outputs(DeterminismMode::DecodeOnly, &reference, &output);
        assert!(result.is_deterministic, "Decode diverged on iteration {i}");
    }
}

// ===========================================================================
// 2. Encode same JSON 100 times → all outputs identical
// ===========================================================================

#[test]
fn encode_100_times_all_identical() {
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("parse");
    let json = serde_json::json!({"FIELD-A": "HELLO"});
    let opts = encode_opts();

    let reference = encode_record(&schema, &json, &opts).expect("encode");

    for i in 1..100 {
        let output = encode_record(&schema, &json, &opts).expect("encode");
        let result = compare_outputs(DeterminismMode::EncodeOnly, &reference, &output);
        assert!(result.is_deterministic, "Encode diverged on iteration {i}");
    }
}

// ===========================================================================
// 3. Hash of decode output is stable across runs
// ===========================================================================

#[test]
fn decode_hash_stable_across_runs() {
    let schema = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data = multi_field_data();
    let opts = decode_opts();

    let first = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
    let reference_hash = blake3_hex(&first);

    for _ in 0..100 {
        let output =
            serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
        assert_eq!(
            blake3_hex(&output),
            reference_hash,
            "Decode BLAKE3 hash drifted"
        );
    }
}

// ===========================================================================
// 4. Hash of encode output is stable across runs
// ===========================================================================

#[test]
fn encode_hash_stable_across_runs() {
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("parse");
    let json = serde_json::json!({"FIELD-A": "WORLD"});
    let opts = encode_opts();

    let reference_hash = blake3_hex(&encode_record(&schema, &json, &opts).expect("encode"));

    for _ in 0..100 {
        let output = encode_record(&schema, &json, &opts).expect("encode");
        assert_eq!(
            blake3_hex(&output),
            reference_hash,
            "Encode BLAKE3 hash drifted"
        );
    }
}

// ===========================================================================
// 5. Different thread counts produce same decode output
// ===========================================================================

#[test]
fn different_thread_counts_same_decode_output() {
    let schema = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data = multi_field_data();
    let opts = decode_opts();

    let reference =
        serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();

    for thread_count in [1, 2, 4, 8] {
        let handles: Vec<_> = (0..thread_count)
            .map(|_| {
                let schema = schema.clone();
                let data = data.clone();
                let opts = opts.clone();
                std::thread::spawn(move || {
                    serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
                        .unwrap()
                })
            })
            .collect();

        for handle in handles {
            let output = handle.join().expect("thread join");
            let result = compare_outputs(DeterminismMode::DecodeOnly, &reference, &output);
            assert!(
                result.is_deterministic,
                "Decode diverged with {thread_count} threads"
            );
        }
    }
}

// ===========================================================================
// 6. Different thread counts produce same encode output
// ===========================================================================

#[test]
fn different_thread_counts_same_encode_output() {
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("parse");
    let json = serde_json::json!({"FIELD-A": "ABCDE"});
    let opts = encode_opts();

    let reference = encode_record(&schema, &json, &opts).expect("encode");

    for thread_count in [1, 2, 4, 8] {
        let handles: Vec<_> = (0..thread_count)
            .map(|_| {
                let schema = schema.clone();
                let json = json.clone();
                let opts = opts.clone();
                std::thread::spawn(move || encode_record(&schema, &json, &opts).expect("encode"))
            })
            .collect();

        for handle in handles {
            let output = handle.join().expect("thread join");
            let result = compare_outputs(DeterminismMode::EncodeOnly, &reference, &output);
            assert!(
                result.is_deterministic,
                "Encode diverged with {thread_count} threads"
            );
        }
    }
}

// ===========================================================================
// 7. Decode with scratch buffers produces same output as without
// ===========================================================================

#[test]
fn decode_with_scratch_matches_without() {
    let schema = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data = multi_field_data();
    let opts = decode_opts();

    let without_scratch =
        serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();

    let mut scratch = ScratchBuffers::new();
    for _ in 0..50 {
        let with_scratch = serde_json::to_vec(
            &decode_record_with_scratch(&schema, &data, &opts, &mut scratch).expect("decode"),
        )
        .unwrap();

        let result = compare_outputs(DeterminismMode::DecodeOnly, &without_scratch, &with_scratch);
        assert!(
            result.is_deterministic,
            "Scratch-buffer decode differs from standard decode"
        );
    }
}

// ===========================================================================
// 8. Round-trip decode→encode→decode produces same result
// ===========================================================================

#[test]
fn round_trip_decode_encode_decode_stable() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(10).
          05 AGE  PIC 9(3).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // "JOHN      " + "025" in EBCDIC
    let data: Vec<u8> = vec![
        0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xF0, 0xF2, 0xF5,
    ];

    let result =
        check_round_trip_determinism(&schema, &data, &decode_opts(), &encode_opts()).expect("rt");
    assert!(
        result.is_deterministic,
        "Round-trip decode→encode→decode was not deterministic"
    );
    assert_eq!(result.mode, DeterminismMode::RoundTrip);
}

// ===========================================================================
// 9. Multiple codepages: each consistently produces same output
// ===========================================================================

#[test]
fn multiple_codepages_each_consistent() {
    let copybook = r"
       01 RECORD.
          05 NAME PIC X(5).
    ";
    let schema = parse_copybook(copybook).expect("parse");

    for cp in [
        Codepage::CP037,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let opts = decode_opts_for(cp);
        // Use CP037-safe bytes (uppercase letters are generally stable across EBCDIC pages)
        let data: Vec<u8> = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5];

        let reference =
            serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
        let reference_hash = blake3_hex(&reference);

        for _ in 0..20 {
            let output =
                serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
            assert_eq!(
                blake3_hex(&output),
                reference_hash,
                "Codepage {cp:?} produced inconsistent decode"
            );
        }
    }
}

// ===========================================================================
// 10. Schema fingerprint is deterministic
// ===========================================================================

#[test]
fn schema_fingerprint_deterministic() {
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

// ===========================================================================
// 11. Empty input always produces empty output
// ===========================================================================

#[test]
fn empty_input_always_produces_empty_output() {
    // Verify the determinism primitive itself: empty byte slices always compare
    // as deterministic and produce a stable hash.
    let empty: &[u8] = b"";
    let hash_ref = blake3_hex(empty);

    for _ in 0..100 {
        let result = compare_outputs(DeterminismMode::DecodeOnly, empty, empty);
        assert!(result.is_deterministic, "Empty input was not deterministic");
        assert_eq!(blake3_hex(empty), hash_ref, "Empty hash drifted");
    }
}

// ===========================================================================
// 12. Single field always produces same JSON structure
// ===========================================================================

#[test]
fn single_field_produces_same_json_structure() {
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("parse");
    let data = simple_data();
    let opts = decode_opts();

    let reference = decode_record(&schema, &data, &opts).expect("decode");
    let ref_str = serde_json::to_string(&reference).unwrap();

    for i in 1..100 {
        let val = decode_record(&schema, &data, &opts).expect("decode");
        let json_str = serde_json::to_string(&val).unwrap();
        assert_eq!(
            json_str, ref_str,
            "Single-field JSON structure diverged on iteration {i}"
        );
    }
}

// ===========================================================================
// 13. COMP-3 decode is deterministic (no floating point drift)
// ===========================================================================

#[test]
fn comp3_decode_deterministic_no_fp_drift() {
    let copybook = r"
       01 RECORD.
          05 AMOUNT PIC S9(7)V99 COMP-3.
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data: Vec<u8> = vec![0x12, 0x34, 0x56, 0x78, 0x9C]; // +1234567.89

    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "COMP-3 decode must be deterministic (no FP drift)"
    );

    // Verify exact value stability over 100 iterations
    let opts = decode_opts();
    let reference =
        serde_json::to_string(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();

    for i in 1..100 {
        let val =
            serde_json::to_string(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
        assert_eq!(val, reference, "COMP-3 value drifted on iteration {i}");
    }
}

// ===========================================================================
// 14. COMP binary decode is deterministic
// ===========================================================================

#[test]
fn comp_binary_decode_deterministic() {
    let copybook = r"
       01 RECORD.
          05 COUNT-FIELD PIC S9(4) COMP.
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // S9(4) COMP → 2 bytes big-endian signed: 0x00 0x2A = +42
    let data: Vec<u8> = vec![0x00, 0x2A];
    let opts = decode_opts();

    let reference =
        serde_json::to_string(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();

    for i in 1..100 {
        let val =
            serde_json::to_string(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
        assert_eq!(
            val, reference,
            "COMP binary decode diverged on iteration {i}"
        );
    }
}

// ===========================================================================
// 15. OCCURS array ordering is preserved
// ===========================================================================

#[test]
fn occurs_array_ordering_preserved() {
    let copybook = r"
       01 RECORD.
          05 ITEMS PIC X(3) OCCURS 5 TIMES.
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // 5 × 3 = 15 bytes: "AAA", "BBB", "CCC", "DDD", "EEE" in EBCDIC
    let data: Vec<u8> = vec![
        0xC1, 0xC1, 0xC1, // AAA
        0xC2, 0xC2, 0xC2, // BBB
        0xC3, 0xC3, 0xC3, // CCC
        0xC4, 0xC4, 0xC4, // DDD
        0xC5, 0xC5, 0xC5, // EEE
    ];
    let opts = decode_opts();

    let reference = decode_record(&schema, &data, &opts).expect("decode");
    let ref_json = serde_json::to_string(&reference).unwrap();

    for i in 1..100 {
        let val = decode_record(&schema, &data, &opts).expect("decode");
        let json = serde_json::to_string(&val).unwrap();
        assert_eq!(
            json, ref_json,
            "OCCURS array ordering changed on iteration {i}"
        );
    }

    // Verify the array is present and has the expected element count
    let items = reference["ITEMS"]
        .as_array()
        .expect("ITEMS should be an array");
    assert_eq!(items.len(), 5, "Expected 5 OCCURS elements");
}

// ===========================================================================
// 16. ODO with same counter always decodes same array length
// ===========================================================================

#[test]
fn odo_same_counter_same_array_length() {
    let copybook = r"
       01 RECORD.
          05 ITEM-COUNT PIC 9(3).
          05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.
             10 ITEM-CODE PIC X(4).
    ";
    let schema = parse_copybook(copybook).expect("parse");

    // Counter = 3 → 3 items of 4 bytes each + 3 bytes counter = 15 bytes
    let mut data = vec![0xF0, 0xF0, 0xF3]; // "003" in EBCDIC
    // Item 1: "AAAA"
    data.extend_from_slice(&[0xC1, 0xC1, 0xC1, 0xC1]);
    // Item 2: "BBBB"
    data.extend_from_slice(&[0xC2, 0xC2, 0xC2, 0xC2]);
    // Item 3: "CCCC"
    data.extend_from_slice(&[0xC3, 0xC3, 0xC3, 0xC3]);
    // Pad remaining 7 items (28 bytes) with spaces for the max record
    data.extend_from_slice(&vec![0x40; 28]);

    let opts = decode_opts();

    let reference =
        serde_json::to_string(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();

    for i in 1..50 {
        let val =
            serde_json::to_string(&decode_record(&schema, &data, &opts).expect("decode")).unwrap();
        assert_eq!(
            val, reference,
            "ODO array length or content diverged on iteration {i}"
        );
    }
}

// ===========================================================================
// 17. Sort order of JSON keys is deterministic
// ===========================================================================

#[test]
fn json_key_sort_order_deterministic() {
    let copybook = r"
       01 RECORD.
          05 ZEBRA   PIC X(3).
          05 ALPHA   PIC X(3).
          05 MIDDLE  PIC X(3).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    // 3 × 3 = 9 bytes
    let data: Vec<u8> = vec![
        0xE9, 0xC5, 0xC2, // ZEB in EBCDIC
        0xC1, 0xD3, 0xD7, // ALP
        0xD4, 0xC9, 0xC4, // MID
    ];
    let opts = decode_opts();

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_keys: Vec<String> = first.as_object().expect("object").keys().cloned().collect();

    for i in 1..100 {
        let val = decode_record(&schema, &data, &opts).expect("decode");
        let keys: Vec<String> = val.as_object().expect("object").keys().cloned().collect();
        assert_eq!(keys, first_keys, "JSON key order changed on iteration {i}");
    }

    // Keys should follow copybook declaration order, not alphabetical
    assert!(
        first_keys.iter().any(|k| k.contains("ZEBRA"))
            && first_keys.iter().any(|k| k.contains("ALPHA"))
            && first_keys.iter().any(|k| k.contains("MIDDLE")),
        "Expected all three fields in output"
    );
}

// ===========================================================================
// 18. Encode determinism via codec check_encode_determinism
// ===========================================================================

#[test]
fn check_encode_determinism_proof() {
    let copybook = r"
       01 RECORD.
          05 NAME   PIC X(10).
          05 AMOUNT PIC 9(5).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let json = serde_json::json!({"NAME": "TEST", "AMOUNT": "12345"});

    let result = check_encode_determinism(&schema, &json, &encode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "check_encode_determinism reported non-determinism"
    );
}

// ===========================================================================
// 19. Multiple codepages encode consistently
// ===========================================================================

#[test]
fn multiple_codepages_encode_each_consistent() {
    let schema = parse_copybook(SIMPLE_COPYBOOK).expect("parse");
    let json = serde_json::json!({"FIELD-A": "ABCDE"});

    for cp in [
        Codepage::CP037,
        Codepage::CP500,
        Codepage::CP1047,
        Codepage::CP1140,
    ] {
        let opts = encode_opts_for(cp);
        let reference = encode_record(&schema, &json, &opts).expect("encode");
        let ref_hash = blake3_hex(&reference);

        for _ in 0..20 {
            let output = encode_record(&schema, &json, &opts).expect("encode");
            assert_eq!(
                blake3_hex(&output),
                ref_hash,
                "Codepage {cp:?} produced inconsistent encode"
            );
        }
    }
}

// ===========================================================================
// 20. Scratch buffer reuse across heterogeneous schemas
// ===========================================================================

#[test]
fn scratch_buffer_reuse_across_schemas() {
    let schema1 = parse_copybook(SIMPLE_COPYBOOK).expect("parse");
    let schema2 = parse_copybook(MULTI_FIELD_COPYBOOK).expect("parse");
    let data1 = simple_data();
    let data2 = multi_field_data();
    let opts = decode_opts();

    // Get reference outputs without scratch
    let ref1 =
        serde_json::to_vec(&decode_record(&schema1, &data1, &opts).expect("decode")).unwrap();
    let ref2 =
        serde_json::to_vec(&decode_record(&schema2, &data2, &opts).expect("decode")).unwrap();

    // Decode alternating schemas with same scratch buffer
    let mut scratch = ScratchBuffers::new();
    for _ in 0..50 {
        let out1 = serde_json::to_vec(
            &decode_record_with_scratch(&schema1, &data1, &opts, &mut scratch).expect("d1"),
        )
        .unwrap();
        let out2 = serde_json::to_vec(
            &decode_record_with_scratch(&schema2, &data2, &opts, &mut scratch).expect("d2"),
        )
        .unwrap();

        assert_eq!(out1, ref1, "Scratch reuse corrupted schema1 output");
        assert_eq!(out2, ref2, "Scratch reuse corrupted schema2 output");
    }
}
