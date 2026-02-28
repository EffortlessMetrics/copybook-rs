// SPDX-License-Identifier: AGPL-3.0-or-later
//! End-to-end determinism tests.
//!
//! Validates that the same input always produces byte-identical output,
//! both for decode (binary→JSON) and encode (JSON→binary) paths.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    decode_record, encode_record, Codepage, DecodeOptions, EncodeOptions, JsonNumberMode,
    RecordFormat,
};
use copybook_core::parse_copybook;
use sha2::{Digest, Sha256};

fn decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_format(RecordFormat::Fixed)
}

fn sha256_hex(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
}

// ---------------------------------------------------------------------------
// 1. Decode determinism – same binary → identical JSON across N runs
// ---------------------------------------------------------------------------

#[test]
fn determinism_decode_identical_across_runs() {
    let cpy = r"
        01 DET-REC.
           05 ID-FIELD    PIC 9(8).
           05 NAME-FIELD  PIC X(20).
           05 AMOUNT      PIC S9(7)V99 COMP-3.
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0xF0, 0xF0, 0xF0, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4]); // "00001234"
    // "DETERMINISM TEST    " in CP037
    data.extend_from_slice(&[
        0xC4, 0xC5, 0xE3, 0xC5, 0xD9, 0xD4, 0xC9, 0xD5, 0xC9, 0xE2, 0xD4, 0x40, 0xE3, 0xC5,
        0xE2, 0xE3, 0x40, 0x40, 0x40, 0x40,
    ]);
    data.extend_from_slice(&[0x00, 0x98, 0x76, 0x54, 0x3C]); // +98765.43 COMP-3

    let schema = parse_copybook(cpy).expect("parse");
    let opts = decode_opts();

    let first_json = decode_record(&schema, &data, &opts).expect("decode");
    let first_hash = sha256_hex(first_json.to_string().as_bytes());

    for i in 1..=50 {
        let json = decode_record(&schema, &data, &opts).expect("decode");
        let hash = sha256_hex(json.to_string().as_bytes());
        assert_eq!(
            first_hash, hash,
            "Decode run {i} produced different output (hash mismatch)"
        );
    }
}

// ---------------------------------------------------------------------------
// 2. Encode determinism – same JSON → identical binary across N runs
// ---------------------------------------------------------------------------

#[test]
fn determinism_encode_identical_across_runs() {
    let cpy = r"
        01 ENC-REC.
           05 CODE-FIELD  PIC X(4).
           05 QTY-FIELD   PIC 9(6).
    ";

    let schema = parse_copybook(cpy).expect("parse");
    let json = serde_json::json!({
        "CODE-FIELD": "ABCD",
        "QTY-FIELD": "012345"
    });

    let opts = encode_opts();

    let first_bytes = encode_record(&schema, &json, &opts).expect("encode");
    let first_hash = sha256_hex(&first_bytes);

    for i in 1..=50 {
        let bytes = encode_record(&schema, &json, &opts).expect("encode");
        let hash = sha256_hex(&bytes);
        assert_eq!(
            first_hash, hash,
            "Encode run {i} produced different output (hash mismatch)"
        );
    }
}

// ---------------------------------------------------------------------------
// 3. Round-trip determinism – decode→encode→decode always same JSON
// ---------------------------------------------------------------------------

#[test]
fn determinism_roundtrip_stable() {
    let cpy = r"
        01 RT-REC.
           05 HDR         PIC X(3).
           05 VALUE-A     PIC 9(5).
           05 VALUE-B     PIC S9(5)V99 COMP-3.
           05 TRAIL       PIC X(2).
    ";

    let mut data: Vec<u8> = Vec::new();
    // HDR "ABC" (CP037: A=C1 B=C2 C=C3)
    data.extend_from_slice(&[0xC1, 0xC2, 0xC3]);
    // VALUE-A "54321"
    data.extend_from_slice(&[0xF5, 0xF4, 0xF3, 0xF2, 0xF1]);
    // VALUE-B -123.45 COMP-3 (4 bytes: PIC S9(5)V99 = 7 digits + sign = 4 bytes)
    data.extend_from_slice(&[0x01, 0x23, 0x45, 0x0D]);
    // TRAIL "XY" (CP037: X=E7 Y=E8)
    data.extend_from_slice(&[0xE7, 0xE8]);

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts();
    let eopts = encode_opts();

    let original_json = decode_record(&schema, &data, &dopts).expect("decode");

    for i in 1..=20 {
        let encoded = encode_record(&schema, &original_json, &eopts).expect("encode");
        let decoded = decode_record(&schema, &encoded, &dopts).expect("re-decode");
        assert_eq!(
            original_json, decoded,
            "Round-trip iteration {i} produced different JSON"
        );
    }
}

// ---------------------------------------------------------------------------
// 4. COMP-3 sign nibble determinism
// ---------------------------------------------------------------------------

#[test]
fn determinism_comp3_sign_stable() {
    let cpy = r"
        01 COMP3-REC.
           05 POS-VAL  PIC S9(5) COMP-3.
           05 NEG-VAL  PIC S9(5) COMP-3.
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0x01, 0x23, 0x4C]); // +1234 COMP-3 (3 bytes)
    data.extend_from_slice(&[0x05, 0x67, 0x8D]); // -5678 COMP-3 (3 bytes)

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts();
    let eopts = encode_opts();

    let json = decode_record(&schema, &data, &dopts).expect("decode");
    let encoded = encode_record(&schema, &json, &eopts).expect("encode");

    // Re-encode multiple times – sign nibbles must be stable
    for i in 1..=30 {
        let re_encoded = encode_record(&schema, &json, &eopts).expect("encode");
        assert_eq!(
            encoded, re_encoded,
            "COMP-3 encode iteration {i} produced different bytes"
        );
    }
}

// ---------------------------------------------------------------------------
// 5. Multi-field schema determinism with all supported types
// ---------------------------------------------------------------------------

#[test]
fn determinism_mixed_types() {
    let cpy = r"
        01 MIX-REC.
           05 TXT-A      PIC X(15).
           05 NUM-A      PIC 9(8).
           05 COMP3-A    PIC S9(9)V99 COMP-3.
           05 TXT-B      PIC X(5).
           05 NUM-B      PIC 9(4).
    ";

    let mut data: Vec<u8> = Vec::new();
    data.extend_from_slice(&[0x40; 15]); // TXT-A spaces
    data.extend_from_slice(&[0xF2, 0xF0, 0xF2, 0xF5, 0xF0, 0xF1, 0xF0, 0xF1]); // "20250101"
    data.extend_from_slice(&[0x00, 0x00, 0x12, 0x34, 0x56, 0x7C]); // +1234.567 COMP-3 (6 bytes)
    data.extend_from_slice(&[0xE3, 0xC5, 0xE2, 0xE3, 0x40]); // "TEST "
    data.extend_from_slice(&[0xF9, 0xF9, 0xF9, 0xF9]); // "9999"

    let schema = parse_copybook(cpy).expect("parse");
    let dopts = decode_opts();

    let mut hashes: Vec<String> = Vec::new();
    for _ in 0..10 {
        let json = decode_record(&schema, &data, &dopts).expect("decode");
        hashes.push(sha256_hex(json.to_string().as_bytes()));
    }

    let first = &hashes[0];
    for (i, h) in hashes.iter().enumerate().skip(1) {
        assert_eq!(first, h, "Decode iteration {i} hash differs");
    }
}
