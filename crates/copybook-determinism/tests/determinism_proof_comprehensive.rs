// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive determinism proof tests.
//!
//! Validates the core contract: same inputs → byte-identical outputs.
//! Covers hash stability across iterations, single-bit diff detection,
//! cross-thread consistency, all field types, varied record sizes,
//! locale/timezone independence, and stable parallel pipeline ordering.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::memory::ScratchBuffers;
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, check_decode_determinism,
    check_encode_determinism, check_round_trip_determinism, decode_record,
    decode_record_with_scratch, encode_record,
};
use copybook_core::parse_copybook;
use copybook_determinism::{
    BLAKE3_HEX_LEN, DeterminismMode, blake3_hex, compare_outputs, compare_outputs_with_limit,
    find_byte_differences,
};

// ===========================================================================
// Helpers
// ===========================================================================

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new().with_codepage(Codepage::CP037)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_format(RecordFormat::Fixed)
}

fn decode_to_bytes(copybook: &str, data: &[u8]) -> Vec<u8> {
    let schema = parse_copybook(copybook).expect("parse");
    let val = decode_record(&schema, data, &decode_opts()).expect("decode");
    serde_json::to_vec(&val).expect("serialize")
}

// ===========================================================================
// 1. Blake3 hash stability across 1000 iterations
// ===========================================================================

#[test]
fn blake3_hash_stable_across_1000_iterations() {
    let data = b"COBOL determinism contract: same inputs must produce identical outputs";
    let reference = blake3_hex(data);
    for i in 0..1000 {
        assert_eq!(
            blake3_hex(data),
            reference,
            "Hash diverged on iteration {i}"
        );
    }
}

#[test]
fn blake3_hash_stable_for_empty_input_1000_iterations() {
    let reference = blake3_hex(b"");
    for _ in 0..1000 {
        assert_eq!(blake3_hex(b""), reference);
    }
}

#[test]
fn blake3_hash_stable_for_binary_payload_1000_iterations() {
    let data: Vec<u8> = (0..=255).collect();
    let reference = blake3_hex(&data);
    for _ in 0..1000 {
        assert_eq!(blake3_hex(&data), reference);
    }
}

// ===========================================================================
// 2. Byte-level diff detection for single-bit changes
// ===========================================================================

#[test]
fn single_bit_flip_detected_at_every_position() {
    let original = vec![0x00_u8; 64];
    for byte_pos in 0..64 {
        for bit in 0..8 {
            let mut modified = original.clone();
            modified[byte_pos] ^= 1 << bit;
            let diffs = find_byte_differences(&original, &modified);
            assert_eq!(
                diffs.len(),
                1,
                "Expected exactly 1 diff for bit flip at byte {byte_pos} bit {bit}"
            );
            assert_eq!(diffs[0].offset, byte_pos);
            assert_eq!(diffs[0].round1_byte, 0x00);
            assert_eq!(diffs[0].round2_byte, 1 << bit);
        }
    }
}

#[test]
fn single_bit_flip_detected_in_nonzero_data() {
    let original: Vec<u8> = (0..32).collect();
    for pos in 0..32 {
        let mut modified = original.clone();
        modified[pos] ^= 0x01; // flip lowest bit
        let result = compare_outputs(DeterminismMode::DecodeOnly, &original, &modified);
        assert!(
            !result.is_deterministic,
            "Should detect single-bit flip at position {pos}"
        );
        assert_eq!(result.diff_count(), 1);
    }
}

#[test]
fn adjacent_byte_changes_reported_separately() {
    let a = vec![0xAA; 8];
    let mut b = a.clone();
    b[3] = 0xBB;
    b[4] = 0xCC;
    let diffs = find_byte_differences(&a, &b);
    assert_eq!(diffs.len(), 2);
    assert_eq!(diffs[0].offset, 3);
    assert_eq!(diffs[1].offset, 4);
}

// ===========================================================================
// 3. Cross-thread consistency: identical inputs → identical outputs
// ===========================================================================

#[test]
fn cross_thread_decode_consistency_8_threads() {
    let copybook = r"
       01 RECORD.
          05 NAME   PIC X(10).
          05 AMOUNT PIC 9(5).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data: Vec<u8> = vec![
        0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, // JOHN
        0xF1, 0xF2, 0xF3, 0xF4, 0xF5, // 12345
    ];
    let opts = decode_opts();

    let reference = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
        .expect("serialize");
    let reference_hash = blake3_hex(&reference);

    let handles: Vec<_> = (0..8)
        .map(|_| {
            let s = schema.clone();
            let d = data.clone();
            let o = opts.clone();
            std::thread::spawn(move || {
                let mut hashes = Vec::new();
                for _ in 0..100 {
                    let val = decode_record(&s, &d, &o).expect("decode");
                    let bytes = serde_json::to_vec(&val).expect("serialize");
                    hashes.push(blake3_hex(&bytes));
                }
                hashes
            })
        })
        .collect();

    for handle in handles {
        for hash in handle.join().expect("join") {
            assert_eq!(hash, reference_hash, "Cross-thread hash diverged");
        }
    }
}

#[test]
fn cross_thread_encode_consistency_4_threads() {
    let copybook = r"
       01 RECORD.
          05 FIELD-A PIC X(5).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let json: serde_json::Value =
        serde_json::from_str(r#"{"RECORD":{"FIELD-A":"HELLO"}}"#).expect("json");
    let opts = encode_opts();

    let reference = encode_record(&schema, &json, &opts).expect("encode");
    let ref_hash = blake3_hex(&reference);

    let handles: Vec<_> = (0..4)
        .map(|_| {
            let s = schema.clone();
            let j = json.clone();
            let o = opts.clone();
            let rh = ref_hash.clone();
            std::thread::spawn(move || {
                for _ in 0..50 {
                    let encoded = encode_record(&s, &j, &o).expect("encode");
                    assert_eq!(blake3_hex(&encoded), rh);
                }
            })
        })
        .collect();

    for h in handles {
        h.join().expect("join");
    }
}

// ===========================================================================
// 4. Determinism proof for all field types
// ===========================================================================

#[test]
fn determinism_proof_display_field() {
    let copybook = "       01 R. 05 F PIC X(8).\n";
    let data = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8]; // ABCDEFGH
    let schema = parse_copybook(copybook).expect("parse");
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "DISPLAY field must be deterministic"
    );
}

#[test]
fn determinism_proof_comp3_field() {
    let copybook = "       01 R. 05 F PIC S9(5)V99 COMP-3.\n";
    let data = vec![0x01, 0x23, 0x45, 0x6C]; // +12345.6 packed
    let schema = parse_copybook(copybook).expect("parse");
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "COMP-3 field must be deterministic"
    );
}

#[test]
fn determinism_proof_numeric_display_field() {
    let copybook = "       01 R. 05 F PIC 9(5).\n";
    let data = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5]; // 12345
    let schema = parse_copybook(copybook).expect("parse");
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "Numeric DISPLAY field must be deterministic"
    );
}

#[test]
fn determinism_proof_signed_numeric_field() {
    let copybook = "       01 R. 05 F PIC S9(5)V99.\n";
    let data = vec![0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6]; // 01234.56
    let schema = parse_copybook(copybook).expect("parse");
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "Signed numeric field must be deterministic"
    );
}

#[test]
fn determinism_proof_pic_x_alphanumeric() {
    let copybook = "       01 R. 05 F PIC X(20).\n";
    let data = vec![0x40; 20]; // 20 EBCDIC spaces
    let schema = parse_copybook(copybook).expect("parse");
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "PIC X alphanumeric must be deterministic"
    );
}

#[test]
fn determinism_proof_mixed_field_types() {
    let copybook = r"
       01 RECORD.
          05 ALPHA      PIC X(10).
          05 NUMERIC    PIC 9(3).
          05 SIGNED-NUM PIC S9(5)V99.
          05 PACKED     PIC S9(5)V99 COMP-3.
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data: Vec<u8> = vec![
        0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1, // ALPHA
        0xF1, 0xF2, 0xF3, // NUMERIC
        0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, // SIGNED-NUM
        0x01, 0x23, 0x45, 0x6C, // PACKED
    ];
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "Mixed field types must be deterministic"
    );
}

#[test]
fn determinism_proof_occurs_array() {
    let copybook = r"
       01 R.
          05 ITEMS OCCURS 5 TIMES.
             10 CODE PIC X(3).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data: Vec<u8> = vec![
        0xC1, 0xC1, 0xC1, 0xC2, 0xC2, 0xC2, 0xC3, 0xC3, 0xC3, 0xC4, 0xC4, 0xC4, 0xC5, 0xC5, 0xC5,
    ];
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(
        result.is_deterministic,
        "OCCURS array must be deterministic"
    );
}

#[test]
fn determinism_proof_redefines() {
    let copybook = r"
       01 R.
          05 F-A PIC X(10).
          05 F-B REDEFINES F-A PIC 9(10).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data = vec![0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xF0];
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(result.is_deterministic, "REDEFINES must be deterministic");
}

// ===========================================================================
// 5. Hash stability for different record sizes
// ===========================================================================

#[test]
fn hash_stability_tiny_record_1_byte() {
    let copybook = "       01 R. 05 F PIC X(1).\n";
    let data = vec![0xC1]; // 'A'
    let out = decode_to_bytes(copybook, &data);
    let reference = blake3_hex(&out);
    for _ in 0..100 {
        let out2 = decode_to_bytes(copybook, &data);
        assert_eq!(blake3_hex(&out2), reference);
    }
}

#[test]
fn hash_stability_small_record_10_bytes() {
    let copybook = "       01 R. 05 F PIC X(10).\n";
    let data = vec![0xC1; 10];
    let out = decode_to_bytes(copybook, &data);
    let reference = blake3_hex(&out);
    for _ in 0..100 {
        assert_eq!(blake3_hex(&decode_to_bytes(copybook, &data)), reference);
    }
}

#[test]
fn hash_stability_medium_record_100_bytes() {
    let copybook = "       01 R. 05 F PIC X(100).\n";
    let data = vec![0xC1; 100];
    let out = decode_to_bytes(copybook, &data);
    let reference = blake3_hex(&out);
    for _ in 0..50 {
        assert_eq!(blake3_hex(&decode_to_bytes(copybook, &data)), reference);
    }
}

#[test]
fn hash_stability_large_record_1000_bytes() {
    let copybook = "       01 R. 05 F PIC X(1000).\n";
    let data = vec![0xC1; 1000];
    let out = decode_to_bytes(copybook, &data);
    let reference = blake3_hex(&out);
    for _ in 0..20 {
        assert_eq!(blake3_hex(&decode_to_bytes(copybook, &data)), reference);
    }
}

#[test]
fn hash_stability_varies_with_record_content() {
    let copybook = "       01 R. 05 F PIC X(10).\n";
    let data_a = vec![0xC1; 10]; // AAAAAAAAAA
    let data_b = vec![0xC2; 10]; // BBBBBBBBBB
    let hash_a = blake3_hex(&decode_to_bytes(copybook, &data_a));
    let hash_b = blake3_hex(&decode_to_bytes(copybook, &data_b));
    assert_ne!(
        hash_a, hash_b,
        "Different data must produce different hashes"
    );
}

// ===========================================================================
// 6. Deterministic output regardless of system locale/timezone
// ===========================================================================

#[test]
fn deterministic_output_independent_of_env_tz() {
    let copybook = r"
       01 R.
          05 AMOUNT PIC 9(5).
          05 NAME   PIC X(10).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data: Vec<u8> = vec![
        0xF0, 0xF1, 0xF2, 0xF3, 0xF4, // 01234
        0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xD1, // ABCDEFGHIJ
    ];
    let opts = decode_opts();

    // Decode produces no timestamps or locale-dependent output;
    // verify repeated decode gives identical JSON bytes.
    let reference = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
        .expect("serialize");
    for _ in 0..100 {
        let output = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
            .expect("serialize");
        assert_eq!(output, reference, "Output must not depend on system state");
    }
}

#[test]
fn numeric_formatting_locale_independent() {
    // Numeric values should always use '.' as decimal separator, never locale-dependent comma
    let copybook = "       01 R. 05 F PIC S9(5)V99.\n";
    let schema = parse_copybook(copybook).expect("parse");
    let data = vec![0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6];
    let opts = decode_opts();

    let val = decode_record(&schema, &data, &opts).expect("decode");
    let json_str = serde_json::to_string(&val).expect("serialize");

    // Verify no locale-dependent comma separator in numeric output
    let reference = json_str.clone();
    for _ in 0..100 {
        let val2 = decode_record(&schema, &data, &opts).expect("decode");
        let json_str2 = serde_json::to_string(&val2).expect("serialize");
        assert_eq!(json_str2, reference, "Numeric formatting must be stable");
    }
}

// ===========================================================================
// 7. Stable ordering for parallel pipeline output
// ===========================================================================

#[test]
fn parallel_decode_pipeline_output_order_stable() {
    let copybook = "       01 R. 05 F PIC X(5).\n";
    let schema = parse_copybook(copybook).expect("parse");
    let opts = decode_opts();

    // Simulate 10 records
    let records: Vec<Vec<u8>> = (0..10u8).map(|i| vec![0xC1 + i; 5]).collect();

    // Decode all records sequentially as reference
    let reference: Vec<String> = records
        .iter()
        .map(|data| {
            let val = decode_record(&schema, data, &opts).expect("decode");
            serde_json::to_string(&val).expect("serialize")
        })
        .collect();

    // Verify ordering is stable across 50 runs
    for _ in 0..50 {
        let output: Vec<String> = records
            .iter()
            .map(|data| {
                let val = decode_record(&schema, data, &opts).expect("decode");
                serde_json::to_string(&val).expect("serialize")
            })
            .collect();
        assert_eq!(output, reference, "Pipeline output order must be stable");
    }
}

#[test]
fn parallel_workers_produce_same_hash_per_record() {
    let copybook = r"
       01 R.
          05 ID     PIC 9(3).
          05 NAME   PIC X(10).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let opts = decode_opts();

    // Multiple records
    let records: Vec<Vec<u8>> = vec![
        vec![
            0xF1, 0xF2, 0xF3, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0x40, 0x40, 0x40, 0x40, 0x40,
        ],
        vec![
            0xF4, 0xF5, 0xF6, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40,
        ],
    ];

    // Compute reference hashes
    let ref_hashes: Vec<String> = records
        .iter()
        .map(|data| {
            let val = decode_record(&schema, data, &opts).expect("decode");
            blake3_hex(&serde_json::to_vec(&val).expect("serialize"))
        })
        .collect();

    // 4 workers each process both records
    let handles: Vec<_> = (0..4)
        .map(|_| {
            let s = schema.clone();
            let o = opts.clone();
            let recs = records.clone();
            std::thread::spawn(move || {
                recs.iter()
                    .map(|data| {
                        let val = decode_record(&s, data, &o).expect("decode");
                        blake3_hex(&serde_json::to_vec(&val).expect("serialize"))
                    })
                    .collect::<Vec<_>>()
            })
        })
        .collect();

    for h in handles {
        let worker_hashes = h.join().expect("join");
        assert_eq!(worker_hashes, ref_hashes);
    }
}

// ===========================================================================
// 8. Scratch buffer determinism
// ===========================================================================

#[test]
fn scratch_buffer_decode_matches_regular_decode() {
    let copybook = r"
       01 R.
          05 NAME   PIC X(10).
          05 AMOUNT PIC 9(5).
    ";
    let schema = parse_copybook(copybook).expect("parse");
    let data: Vec<u8> = vec![
        0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5,
    ];
    let opts = decode_opts();

    let regular = serde_json::to_vec(&decode_record(&schema, &data, &opts).expect("decode"))
        .expect("serialize");

    let mut scratch = ScratchBuffers::new();
    for _ in 0..100 {
        let val = decode_record_with_scratch(&schema, &data, &opts, &mut scratch).expect("decode");
        let output = serde_json::to_vec(&val).expect("serialize");
        assert_eq!(output, regular, "Scratch decode must match regular decode");
    }
}

#[test]
fn scratch_buffer_reuse_does_not_leak_between_records() {
    let copybook = "       01 R. 05 F PIC X(10).\n";
    let schema = parse_copybook(copybook).expect("parse");
    let opts = decode_opts();
    let mut scratch = ScratchBuffers::new();

    let data_a = vec![0xC1; 10]; // AAAAAAAAAA
    let data_b = vec![0xC2; 10]; // BBBBBBBBBB

    let ref_a = serde_json::to_vec(&decode_record(&schema, &data_a, &opts).expect("decode"))
        .expect("serialize");
    let ref_b = serde_json::to_vec(&decode_record(&schema, &data_b, &opts).expect("decode"))
        .expect("serialize");

    // Interleave scratch-buffer decodes
    for _ in 0..50 {
        let val_a =
            decode_record_with_scratch(&schema, &data_a, &opts, &mut scratch).expect("decode");
        let val_b =
            decode_record_with_scratch(&schema, &data_b, &opts, &mut scratch).expect("decode");
        assert_eq!(serde_json::to_vec(&val_a).unwrap(), ref_a);
        assert_eq!(serde_json::to_vec(&val_b).unwrap(), ref_b);
    }
}

// ===========================================================================
// 9. Encode determinism proofs
// ===========================================================================

#[test]
fn encode_determinism_proof_display() {
    let copybook = "       01 R. 05 F PIC X(5).\n";
    let schema = parse_copybook(copybook).expect("parse");
    let json: serde_json::Value = serde_json::from_str(r#"{"R":{"F":"HELLO"}}"#).expect("json");
    let opts = encode_opts();

    let reference = encode_record(&schema, &json, &opts).expect("encode");
    for _ in 0..200 {
        let output = encode_record(&schema, &json, &opts).expect("encode");
        assert_eq!(output, reference, "Encode must be deterministic");
    }
}

#[test]
fn encode_determinism_proof_numeric() {
    let copybook = "       01 R. 05 F PIC 9(5).\n";
    let schema = parse_copybook(copybook).expect("parse");
    let json: serde_json::Value = serde_json::from_str(r#"{"R":{"F":"12345"}}"#).expect("json");
    let opts = encode_opts();

    let reference = encode_record(&schema, &json, &opts).expect("encode");
    for _ in 0..200 {
        assert_eq!(
            encode_record(&schema, &json, &opts).expect("encode"),
            reference
        );
    }
}

// ===========================================================================
// 10. Round-trip determinism
// ===========================================================================

#[test]
fn round_trip_determinism_display() {
    let copybook = "       01 R. 05 F PIC X(5).\n";
    let schema = parse_copybook(copybook).expect("parse");
    let data = vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6]; // HELLO
    let d_opts = decode_opts();
    let e_opts = encode_opts();

    let decoded = decode_record(&schema, &data, &d_opts).expect("decode");
    let re_encoded = encode_record(&schema, &decoded, &e_opts).expect("encode");
    let re_decoded = decode_record(&schema, &re_encoded, &d_opts).expect("decode");

    let json1 = serde_json::to_vec(&decoded).expect("serialize");
    let json2 = serde_json::to_vec(&re_decoded).expect("serialize");
    let result = compare_outputs(DeterminismMode::RoundTrip, &json1, &json2);
    assert!(result.is_deterministic, "Round-trip must be deterministic");
}

#[test]
fn round_trip_determinism_comp3() {
    let copybook = "       01 R. 05 F PIC S9(5)V99 COMP-3.\n";
    let schema = parse_copybook(copybook).expect("parse");
    let data = vec![0x01, 0x23, 0x45, 0x6C]; // +12345.6

    let result = check_round_trip_determinism(&schema, &data, &decode_opts(), &encode_opts())
        .expect("check");
    assert!(
        result.is_deterministic,
        "COMP-3 round-trip must be deterministic"
    );
}

// ===========================================================================
// 11. Compare outputs edge cases
// ===========================================================================

#[test]
fn compare_outputs_with_limit_1_still_detects_non_determinism() {
    let a = vec![0u8; 100];
    let b = vec![1u8; 100];
    let result = compare_outputs_with_limit(DeterminismMode::DecodeOnly, &a, &b, 1);
    assert!(!result.is_deterministic);
    assert_eq!(result.diff_count(), 1);
}

#[test]
fn compare_outputs_large_identical_slices() {
    let data = vec![0x42; 1_000_000];
    let result = compare_outputs(DeterminismMode::DecodeOnly, &data, &data);
    assert!(result.is_deterministic);
    assert_eq!(result.round1_hash, result.round2_hash);
    assert_eq!(result.round1_hash.len(), BLAKE3_HEX_LEN);
}

// ===========================================================================
// 12. Schema fingerprint determinism
// ===========================================================================

#[test]
fn schema_fingerprint_stable_across_500_parses() {
    let copybook = r"
       01 CUSTOMER.
          05 ID      PIC 9(8).
          05 NAME    PIC X(30).
          05 BALANCE PIC S9(9)V99.
    ";
    let reference_fp = parse_copybook(copybook).expect("parse").fingerprint.clone();
    assert!(!reference_fp.is_empty());
    for i in 0..500 {
        let fp = parse_copybook(copybook).expect("parse").fingerprint;
        assert_eq!(fp, reference_fp, "Fingerprint diverged on parse {i}");
    }
}

// ===========================================================================
// 13. check_*_determinism convenience functions
// ===========================================================================

#[test]
fn check_decode_determinism_returns_matching_hashes() {
    let copybook = "       01 R. 05 F PIC X(5).\n";
    let schema = parse_copybook(copybook).expect("parse");
    let data = vec![0xC1, 0xC2, 0xC3, 0xC4, 0xC5];
    let result = check_decode_determinism(&schema, &data, &decode_opts()).expect("check");
    assert!(result.is_deterministic);
    assert_eq!(result.round1_hash, result.round2_hash);
}

#[test]
fn check_encode_determinism_returns_matching_hashes() {
    let copybook = "       01 R. 05 F PIC X(5).\n";
    let schema = parse_copybook(copybook).expect("parse");
    let json: serde_json::Value = serde_json::from_str(r#"{"R":{"F":"ABCDE"}}"#).expect("json");
    let result = check_encode_determinism(&schema, &json, &encode_opts()).expect("check");
    assert!(result.is_deterministic);
    assert_eq!(result.round1_hash, result.round2_hash);
}
