// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for decode/encode determinism.
//!
//! Verifies that:
//! - Same schema + same data → identical decode output across repeated runs
//! - Same schema + same JSON → identical encode output across repeated runs
//! - Hash of output is stable across runs
//! - Sequential decode matches itself (proxy for parallel-vs-sequential parity)

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::parse_copybook;
use copybook_determinism::blake3_hex;
use proptest::prelude::*;
use serde_json::Value;

use super::config::*;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const MIXED_SCHEMA: &str = "\
       01 REC.
         05 FLD-X     PIC X(10).
         05 FLD-9     PIC 9(5).
         05 FLD-P     PIC S9(7)V99 COMP-3.
";

/// Record length for `MIXED_SCHEMA`: 10 + 5 + 5 = 20 bytes
const MIXED_RECORD_LEN: usize = 20;

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

/// Build a valid record for `MIXED_SCHEMA`.
fn build_mixed_record(alpha: [u8; 10], digits: [u8; 5], packed: [u8; 5]) -> Vec<u8> {
    let mut buf = Vec::with_capacity(MIXED_RECORD_LEN);
    buf.extend_from_slice(&alpha);
    buf.extend_from_slice(&digits);
    buf.extend_from_slice(&packed);
    buf
}

// ---------------------------------------------------------------------------
// 1. Same schema + same data → identical decode output (10 runs)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding the same PIC X record 10 times produces identical JSON each time.
    #[test]
    fn prop_decode_deterministic_pic_x(
        data in prop::collection::vec(0x20u8..=0x7E, 10..=10),
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").expect("parse");
        let first = decode_record(&schema, &data, &decode_opts()).expect("decode 0");
        for i in 1..10 {
            let again = decode_record(&schema, &data, &decode_opts())
                .unwrap_or_else(|e| panic!("decode run {i} failed: {e}"));
            prop_assert_eq!(&first, &again, "Decode run {} differs", i);
        }
    }

    /// Decoding the same PIC 9 record 10 times produces identical JSON.
    #[test]
    fn prop_decode_deterministic_pic_9(
        data in prop::collection::vec(b'0'..=b'9', 5..=5),
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(5).").expect("parse");
        let first = decode_record(&schema, &data, &decode_opts()).expect("decode 0");
        for i in 1..10 {
            let again = decode_record(&schema, &data, &decode_opts())
                .unwrap_or_else(|e| panic!("decode run {i} failed: {e}"));
            prop_assert_eq!(&first, &again, "Decode run {} differs", i);
        }
    }

    /// Decoding the same mixed-type record 10 times produces identical JSON.
    #[test]
    fn prop_decode_deterministic_mixed(
        alpha in prop::collection::vec(0x20u8..=0x7E, 10..=10),
        digits in prop::collection::vec(b'0'..=b'9', 5..=5),
        packed in prop::collection::vec(any::<u8>(), 5..=5),
    ) {
        let schema = parse_copybook(MIXED_SCHEMA).expect("parse");
        let alpha_arr: [u8; 10] = alpha.try_into().unwrap();
        let digit_arr: [u8; 5] = digits.try_into().unwrap();
        let packed_arr: [u8; 5] = packed.try_into().unwrap();
        let record = build_mixed_record(alpha_arr, digit_arr, packed_arr);

        // Best-effort decode; some packed combos are invalid
        if let Ok(first) = decode_record(&schema, &record, &decode_opts()) {
            for i in 1..10 {
                let again = decode_record(&schema, &record, &decode_opts())
                    .unwrap_or_else(|e| panic!("decode run {i} failed: {e}"));
                prop_assert_eq!(&first, &again, "Decode run {} differs", i);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// 2. Same schema + same JSON → identical encode output (10 runs)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Encoding the same PIC X JSON 10 times produces identical bytes.
    #[test]
    fn prop_encode_deterministic_pic_x(
        text in "[A-Z ]{10}",
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").expect("parse");
        let json: Value = serde_json::json!({ "FLD": text });
        let first = encode_record(&schema, &json, &encode_opts()).expect("encode 0");
        for i in 1..10 {
            let again = encode_record(&schema, &json, &encode_opts())
                .unwrap_or_else(|e| panic!("encode run {i} failed: {e}"));
            prop_assert_eq!(&first, &again, "Encode run {} differs", i);
        }
    }

    /// Encoding the same PIC 9 JSON 10 times produces identical bytes.
    #[test]
    fn prop_encode_deterministic_pic_9(value in 0u64..=99999) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(5).").expect("parse");
        let json: Value = serde_json::json!({ "FLD": value });
        let first = encode_record(&schema, &json, &encode_opts()).expect("encode 0");
        for i in 1..10 {
            let again = encode_record(&schema, &json, &encode_opts())
                .unwrap_or_else(|e| panic!("encode run {i} failed: {e}"));
            prop_assert_eq!(&first, &again, "Encode run {} differs", i);
        }
    }
}

// ---------------------------------------------------------------------------
// 3. Hash of output is stable across runs
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// BLAKE3 hash of decoded JSON is identical across 10 runs.
    #[test]
    fn prop_decode_hash_stable(
        data in prop::collection::vec(0x20u8..=0x7E, 10..=10),
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").expect("parse");
        let first = decode_record(&schema, &data, &decode_opts()).expect("decode");
        let first_bytes = serde_json::to_vec(&first).expect("json bytes");
        let first_hash = blake3_hex(&first_bytes);

        for _ in 1..10 {
            let again = decode_record(&schema, &data, &decode_opts()).expect("decode");
            let again_bytes = serde_json::to_vec(&again).expect("json bytes");
            let again_hash = blake3_hex(&again_bytes);
            prop_assert_eq!(&first_hash, &again_hash);
        }
    }

    /// BLAKE3 hash of encoded bytes is identical across 10 runs.
    #[test]
    fn prop_encode_hash_stable(
        text in "[A-Z ]{10}",
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").expect("parse");
        let json: Value = serde_json::json!({ "FLD": text });

        let first = encode_record(&schema, &json, &encode_opts()).expect("encode");
        let first_hash = blake3_hex(&first);

        for _ in 1..10 {
            let again = encode_record(&schema, &json, &encode_opts()).expect("encode");
            let again_hash = blake3_hex(&again);
            prop_assert_eq!(&first_hash, &again_hash);
        }
    }
}

// ---------------------------------------------------------------------------
// 4. Sequential runs produce same results (parallel proxy)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding N records sequentially twice produces identical output vectors.
    #[test]
    fn prop_sequential_decode_stable(
        records in prop::collection::vec(
            prop::collection::vec(0x20u8..=0x7E, 10..=10),
            1..=8,
        ),
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").expect("parse");

        let run = |_label: &str| -> Vec<Value> {
            records
                .iter()
                .filter_map(|rec| decode_record(&schema, rec, &decode_opts()).ok())
                .collect()
        };

        let first = run("run1");
        let second = run("run2");
        prop_assert_eq!(first.len(), second.len());
        for (i, (a, b)) in first.iter().zip(second.iter()).enumerate() {
            prop_assert_eq!(a, b, "Record {} differs between sequential runs", i);
        }
    }

    /// Encoding N JSON values sequentially twice produces identical output vectors.
    #[test]
    fn prop_sequential_encode_stable(
        values in prop::collection::vec(0u64..=99999, 1..=8),
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC 9(5).").expect("parse");

        let run = |_label: &str| -> Vec<Vec<u8>> {
            values
                .iter()
                .filter_map(|&v| {
                    let json = serde_json::json!({ "FLD": v });
                    encode_record(&schema, &json, &encode_opts()).ok()
                })
                .collect()
        };

        let first = run("run1");
        let second = run("run2");
        prop_assert_eq!(first.len(), second.len());
        for (i, (a, b)) in first.iter().zip(second.iter()).enumerate() {
            prop_assert_eq!(a, b, "Record {} differs between sequential runs", i);
        }
    }

    /// Decoding and re-encoding the same batch produces identical hashes.
    #[test]
    fn prop_batch_roundtrip_hash_stable(
        records in prop::collection::vec(
            prop::collection::vec(0x20u8..=0x7E, 10..=10),
            1..=8,
        ),
    ) {
        let schema = parse_copybook("01 REC.\n   05 FLD PIC X(10).").expect("parse");

        let roundtrip = |_label: &str| -> String {
            let mut all_bytes = Vec::new();
            for rec in &records {
                if let Ok(json) = decode_record(&schema, rec, &decode_opts()) {
                    if let Ok(encoded) = encode_record(&schema, &json, &encode_opts()) {
                        all_bytes.extend_from_slice(&encoded);
                    }
                }
            }
            blake3_hex(&all_bytes)
        };

        let h1 = roundtrip("run1");
        let h2 = roundtrip("run2");
        prop_assert_eq!(h1, h2);
    }
}
