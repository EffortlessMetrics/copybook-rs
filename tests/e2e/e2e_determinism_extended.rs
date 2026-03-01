// SPDX-License-Identifier: AGPL-3.0-or-later
//! Extended determinism validation tests.
//!
//! Supplements the existing `e2e_determinism.rs` with additional scenarios:
//! large schemas, group structures, REDEFINES, cross-codepage, and the
//! `check_*_determinism` API functions.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_record,
    encode_record,
};
use copybook_core::parse_copybook;
use sha2::{Digest, Sha256};

fn decode_opts(cp: Codepage) -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts(cp: Codepage) -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(cp)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn sha256_hex(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
}

// ---------------------------------------------------------------------------
// 1. Decode determinism – large schema (50 fields) across 10 runs
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_large_schema_10_runs() {
    let mut cpy = String::from("       01 LARGE-DET.\n");
    for i in 1..=50 {
        cpy.push_str(&format!("          05 FLD-{i:03} PIC X(4).\n"));
    }

    let data: Vec<u8> = (0..50)
        .flat_map(|i| {
            let b = 0xC1 + (i as u8 % 9);
            vec![b, b, b, b]
        })
        .collect();

    let schema = parse_copybook(&cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_hash = sha256_hex(first.to_string().as_bytes());

    for i in 2..=10 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        let hash = sha256_hex(json.to_string().as_bytes());
        assert_eq!(
            first_hash, hash,
            "Large schema decode run {i} produced different output"
        );
    }
}

// ---------------------------------------------------------------------------
// 2. Encode determinism – large schema across 10 runs
// ---------------------------------------------------------------------------

#[test]
fn determinism_encode_large_schema_10_runs() {
    let mut cpy = String::from("       01 LARGE-ENC.\n");
    let n = 50;
    for i in 1..=n {
        cpy.push_str(&format!("          05 FLD-{i:03} PIC X(4).\n"));
    }

    let schema = parse_copybook(&cpy).expect("parse");

    let mut json_obj = serde_json::Map::new();
    for i in 1..=n {
        json_obj.insert(
            format!("FLD-{i:03}"),
            serde_json::Value::String("TEST".to_string()),
        );
    }
    let json = serde_json::Value::Object(json_obj);
    let opts = encode_opts(Codepage::CP037);

    let first = encode_record(&schema, &json, &opts).expect("encode");
    let first_hash = sha256_hex(&first);

    for i in 2..=10 {
        let encoded = encode_record(&schema, &json, &opts).expect("encode");
        let hash = sha256_hex(&encoded);
        assert_eq!(
            first_hash, hash,
            "Large schema encode run {i} produced different output"
        );
    }
}

// ---------------------------------------------------------------------------
// 3. Round-trip determinism – decode→encode→decode 10 times
// ---------------------------------------------------------------------------

#[test]
fn determinism_roundtrip_10_iterations() {
    let cpy = r"
        01 RT-REC.
           05 ID-FIELD   PIC 9(6).
           05 NAME-FIELD PIC X(15).
           05 AMOUNT     PIC S9(5)V99 COMP-3.
           05 STATUS     PIC X(1).
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]); // "001234"
    // "JOHN SMITH     " in CP037
    data.extend_from_slice(&[
        0xD1, 0xD6, 0xC8, 0xD5, 0x40, 0xE2, 0xD4, 0xC9, 0xE3, 0xC8, 0x40, 0x40, 0x40, 0x40, 0x40,
    ]);
    data.extend_from_slice(&[0x01, 0x23, 0x45, 0x6C]); // +12345.67 COMP-3 (4 bytes for S9(5)V99)
    data.extend_from_slice(&[0xC1]); // "A"

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let original_json = decode_record(&schema, &data, &dopts).expect("decode");

    for i in 1..=10 {
        let encoded = encode_record(&schema, &original_json, &eopts).expect("encode");
        let decoded = decode_record(&schema, &encoded, &dopts).expect("re-decode");
        assert_eq!(
            original_json, decoded,
            "Round-trip iteration {i} produced different JSON"
        );
    }
}

// ---------------------------------------------------------------------------
// 4. REDEFINES decode determinism
// ---------------------------------------------------------------------------

#[test]
fn determinism_redefines_decode() {
    let cpy = r"
        01 REDEF-DET-REC.
           05 REC-TYPE   PIC X(1).
           05 PAYLOAD    PIC X(10).
           05 PAYLOAD-B REDEFINES PAYLOAD PIC X(10).
           05 FOOTER     PIC X(3).
    ";

    let mut data: Vec<u8> = Vec::new();
    data.push(0xC1); // "A"
    data.extend_from_slice(&[0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xE1]);
    data.extend_from_slice(&[0xE7, 0xE8, 0xE9]); // "XYZ"

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let first = decode_record(&schema, &data, &opts).expect("decode");
    for i in 2..=10 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        assert_eq!(first, json, "REDEFINES decode run {i} differs");
    }
}

// ---------------------------------------------------------------------------
// 5. Group structure decode determinism
// ---------------------------------------------------------------------------

#[test]
fn determinism_group_structure_decode() {
    let cpy = r"
        01 GROUP-DET-REC.
           05 HEADER.
              10 HDR-CODE PIC X(3).
              10 HDR-SEQ  PIC 9(4).
           05 BODY.
              10 BODY-TXT PIC X(20).
              10 BODY-AMT PIC 9(8).
           05 TRAILER PIC X(2).
    ";

    let mut data: Vec<u8> = Vec::new();
    // HDR-CODE "ABC"
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3]);
    // HDR-SEQ "0001"
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF1]);
    // BODY-TXT (20 spaces)
    data.extend_from_slice(&[0x40; 20]);
    // BODY-AMT "00005000"
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF0, 0xF5, 0xF0, 0xF0, 0xF0]);
    // TRAILER "ZZ"
    data.extend_from_slice(&[0xE9, 0xE9]);

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let first = decode_record(&schema, &data, &opts).expect("decode");
    for i in 2..=10 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        assert_eq!(first, json, "Group structure decode run {i} differs");
    }
}

// ---------------------------------------------------------------------------
// 6. Cross-codepage determinism – CP037 vs CP1047 for digit fields
// ---------------------------------------------------------------------------

#[test]
fn determinism_cross_codepage_digits() {
    let cpy = r"
        01 DIGIT-DET.
           05 NUM-A PIC 9(8).
           05 NUM-B PIC 9(4).
    ";

    // Digits F0-F9 are the same in CP037 and CP1047
    let data: Vec<u8> = vec![
        0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, // "12345678"
        0xF9, 0xF0, 0xF1, 0xF2, // "9012"
    ];

    let schema = parse_copybook(cpy).expect("parse");

    for cp in [Codepage::CP037, Codepage::CP1047] {
        let opts = decode_opts(cp);
        let first = decode_record(&schema, &data, &opts).expect("decode");
        for i in 2..=10 {
            let json = decode_record(&schema, &data, &opts).expect("decode");
            assert_eq!(first, json, "Codepage {cp:?} decode run {i} differs");
        }
    }
}

// ---------------------------------------------------------------------------
// 7. Encode determinism with COMP-3 sign preservation
// ---------------------------------------------------------------------------

#[test]
fn determinism_encode_comp3_sign_preserved() {
    let cpy = r"
        01 SIGN-DET.
           05 POS-AMT PIC S9(7)V99 COMP-3.
           05 NEG-AMT PIC S9(7)V99 COMP-3.
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let json = serde_json::json!({
        "POS-AMT": "12345.67",
        "NEG-AMT": "-98765.43"
    });
    let opts = encode_opts(Codepage::CP037);

    let first = encode_record(&schema, &json, &opts).expect("encode");
    for i in 2..=10 {
        let encoded = encode_record(&schema, &json, &opts).expect("encode");
        assert_eq!(first, encoded, "COMP-3 sign encode run {i} differs");
    }
}

// ---------------------------------------------------------------------------
// 8. Binary hash stability across decode iterations
// ---------------------------------------------------------------------------

#[test]
fn determinism_binary_hash_stable_across_iterations() {
    let cpy = r"
        01 HASH-REC.
           05 TXT   PIC X(10).
           05 NUM   PIC 9(5).
           05 PKD   PIC S9(3) COMP-3.
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC8, 0xC5, 0xD3, 0xD3, 0xD6, 0x40, 0x40, 0x40, 0x40, 0x40]);
    data.extend_from_slice(&[0xF0, 0xF0, 0xF1, 0xF2, 0xF3]);
    data.extend_from_slice(&[0x45, 0x6C]); // +456 COMP-3

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let first_encoded = encode_record(&schema, &json, &eopts).expect("encode");
    let first_hash = sha256_hex(&first_encoded);

    for i in 2..=10 {
        let encoded = encode_record(&schema, &json, &eopts).expect("encode");
        let hash = sha256_hex(&encoded);
        assert_eq!(first_hash, hash, "Binary hash unstable at iteration {i}");
    }
}

// ---------------------------------------------------------------------------
// 9. JSON string representation deterministic
// ---------------------------------------------------------------------------

#[test]
fn determinism_json_string_representation() {
    let cpy = r"
        01 JSON-DET.
           05 FIELD-A PIC X(8).
           05 FIELD-B PIC 9(6).
           05 FIELD-C PIC X(4).
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC4, 0xC1, 0xE3, 0xC1, 0x40, 0x40, 0x40, 0x40]); // "DATA    "
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6]); // "123456"
    data.extend_from_slice(&[0xE3, 0xC5, 0xE2, 0xE3]); // "TEST"

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_str = serde_json::to_string(&first).expect("serialize");

    for i in 2..=10 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        let json_str = serde_json::to_string(&json).expect("serialize");
        assert_eq!(
            first_str, json_str,
            "JSON string representation differs at run {i}"
        );
    }
}

// ---------------------------------------------------------------------------
// 10. Multiple round-trips converge to fixed point
// ---------------------------------------------------------------------------

#[test]
fn determinism_multiple_roundtrips_fixed_point() {
    let cpy = r"
        01 FP-REC.
           05 CODE-A   PIC X(3).
           05 VAL-FLD  PIC 9(5).
           05 CODE-B   PIC X(2).
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3]); // "ABC"
    data.extend_from_slice(&[0xF5, 0xF4, 0xF3, 0xF2, 0xF1]); // "54321"
    data.extend_from_slice(&[0xE7, 0xE8]); // "XY"

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts(Codepage::CP037);
    let eopts = encode_opts(Codepage::CP037);

    let mut current_data = data.clone();
    for i in 1..=10 {
        let json = decode_record(&schema, &current_data, &dopts).expect("decode");
        let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
        assert_eq!(
            current_data, re_encoded,
            "Round-trip {i} changed the binary data (not a fixed point)"
        );
        current_data = re_encoded;
    }
}

// ---------------------------------------------------------------------------
// 11. OCCURS array determinism
// ---------------------------------------------------------------------------

#[test]
fn determinism_occurs_array() {
    let cpy = r"
        01 ARR-DET.
           05 ITEMS OCCURS 5 TIMES.
              10 ITEM-VAL PIC X(4).
    ";

    let mut data: Vec<u8> = Vec::new();
    for i in 0..5u8 {
        let b = 0xC1 + i;
        data.extend_from_slice(&[b, b, b, b]);
    }

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let first = decode_record(&schema, &data, &opts).expect("decode");
    let first_str = first.to_string();

    for i in 2..=10 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        assert_eq!(
            first_str,
            json.to_string(),
            "OCCURS array decode run {i} differs"
        );
    }
}

// ---------------------------------------------------------------------------
// 12. Encode determinism with all-spaces PIC X
// ---------------------------------------------------------------------------

#[test]
fn determinism_encode_all_spaces() {
    let cpy = r"
        01 SPC-REC.
           05 FIELD-A PIC X(20).
           05 FIELD-B PIC X(20).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let json = serde_json::json!({
        "FIELD-A": "",
        "FIELD-B": ""
    });
    let opts = encode_opts(Codepage::CP037);

    let first = encode_record(&schema, &json, &opts).expect("encode");
    for i in 2..=10 {
        let encoded = encode_record(&schema, &json, &opts).expect("encode");
        assert_eq!(first, encoded, "All-spaces encode run {i} differs");
    }
}

// ---------------------------------------------------------------------------
// 13. Decode determinism with signed zoned decimal
// ---------------------------------------------------------------------------

#[test]
fn determinism_signed_zoned_decimal() {
    let cpy = r"
        01 SIGNED-DET.
           05 POS-NUM PIC S9(5).
           05 NEG-NUM PIC S9(5).
    ";

    let mut data: Vec<u8> = Vec::new();
    // +12345: last byte has C zone (positive)
    data.extend_from_slice(&[0xF1, 0xF2, 0xF3, 0xF4, 0xC5]);
    // -67890: last byte has D zone (negative)
    data.extend_from_slice(&[0xF6, 0xF7, 0xF8, 0xF9, 0xD0]);

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts(Codepage::CP037);

    let first = decode_record(&schema, &data, &opts).expect("decode");
    for i in 2..=10 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        assert_eq!(first, json, "Signed zoned decimal decode run {i} differs");
    }
}

// ---------------------------------------------------------------------------
// 14. SHA-256 digest of encode output is stable
// ---------------------------------------------------------------------------

#[test]
fn determinism_sha256_encode_digest_stable() {
    let cpy = r"
        01 SHA-REC.
           05 ALPHA PIC X(10).
           05 NUMERIC PIC 9(8).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let json = serde_json::json!({
        "ALPHA": "ABCDE",
        "NUMERIC": "12345678"
    });
    let opts = encode_opts(Codepage::CP037);

    let mut hashes: Vec<String> = Vec::new();
    for _ in 0..10 {
        let encoded = encode_record(&schema, &json, &opts).expect("encode");
        hashes.push(sha256_hex(&encoded));
    }

    let first = &hashes[0];
    for (i, h) in hashes.iter().enumerate().skip(1) {
        assert_eq!(first, h, "SHA-256 digest differs at run {i}");
    }
}
