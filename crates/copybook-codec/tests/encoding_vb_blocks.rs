// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! VB/BDW block framing round-trip proofs (`encoding_*` feature family).
//!
//! These tests prove the Slice B wiring end to end: hand-framed BDW blocks
//! decode through [`copybook_codec::decode_file_to_jsonl`], JSONL encodes
//! through [`copybook_codec::encode_jsonl_to_file`] into single-record BDW
//! blocks, and the two compose into a byte-and-value round trip.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_file_to_jsonl,
    encode_jsonl_to_file,
};
use copybook_core::parse_copybook;
use serde_json::Value;
use std::io::Cursor;
use std::str::FromStr;

fn vb_decode_options(strict: bool) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Vb)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(strict)
}

fn vb_encode_options() -> EncodeOptions {
    EncodeOptions {
        format: RecordFormat::Vb,
        codepage: Codepage::ASCII,
        strict_mode: true,
        ..EncodeOptions::default()
    }
}

/// Frame payloads as one BDW block: `BDW len + RDW len + payload` per record.
fn frame_block(payloads: &[&[u8]]) -> Vec<u8> {
    let mut records = Vec::new();
    for payload in payloads {
        let rdw_len = u16::try_from(payload.len() + 4).expect("payload fits in RDW");
        records.extend_from_slice(&rdw_len.to_be_bytes());
        records.extend_from_slice(&[0, 0]);
        records.extend_from_slice(payload);
    }
    let block_len = u16::try_from(records.len() + 4).expect("block fits in BDW");
    let mut block = Vec::with_capacity(records.len() + 4);
    block.extend_from_slice(&block_len.to_be_bytes());
    block.extend_from_slice(&[0, 0]);
    block.extend_from_slice(&records);
    block
}

#[test]
fn encoding_vb_decodes_records_across_blocks() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    let mut input = frame_block(&[b"HELLO", b"WORLD"]);
    input.extend_from_slice(&frame_block(&[b"AGAIN"]));

    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&input),
        &mut Vec::new(),
        &vb_decode_options(false),
    )
    .expect("VB decode should succeed");
    assert_eq!(summary.records_processed, 3);
    // Framed record bytes only: BDW block headers are framing overhead.
    assert_eq!(summary.bytes_processed, 3 * (4 + 5));
}

#[test]
fn encoding_vb_decode_values_match_payloads() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    let input = frame_block(&[b"HELLO", b"WORLD"]);
    let mut output = Vec::new();

    decode_file_to_jsonl(
        &schema,
        Cursor::new(&input),
        &mut output,
        &vb_decode_options(false),
    )
    .expect("VB decode should succeed");

    let output_str = String::from_utf8(output).expect("output is UTF-8");
    let values: Vec<Value> = output_str
        .lines()
        .map(|line| serde_json::from_str(line).expect("line is JSON"))
        .collect();
    assert_eq!(values.len(), 2);
    assert_eq!(values[0]["SIMPLE-RECORD"], "HELLO");
    assert_eq!(values[1]["SIMPLE-RECORD"], "WORLD");
}

#[test]
fn encoding_vb_jsonl_encode_decode_round_trip() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    let jsonl = "{\"SIMPLE-RECORD\": \"HELLO\"}\n{\"SIMPLE-RECORD\": \"WORLD\"}\n";
    let mut binary = Vec::new();

    let encode_summary = encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut binary,
        &vb_encode_options(),
    )
    .expect("VB encode should succeed");
    assert_eq!(encode_summary.records_processed, 2);
    // Two single-record blocks: (4 BDW + 4 RDW + 5 payload) each.
    assert_eq!(binary.len(), 2 * (4 + 4 + 5));

    let mut decoded = Vec::new();
    let decode_summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&binary),
        &mut decoded,
        &vb_decode_options(true),
    )
    .expect("VB decode should succeed");
    assert_eq!(decode_summary.records_processed, 2);

    let decoded_str = String::from_utf8(decoded).expect("output is UTF-8");
    let values: Vec<Value> = decoded_str
        .lines()
        .map(|line| serde_json::from_str(line).expect("line is JSON"))
        .collect();
    assert_eq!(values[0]["SIMPLE-RECORD"], "HELLO");
    assert_eq!(values[1]["SIMPLE-RECORD"], "WORLD");
}

#[test]
fn encoding_vb_truncated_block_fails_strict() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    // BDW claims 13 bytes but only the RDW header follows.
    let input = [0x00, 0x0D, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00];

    let error = decode_file_to_jsonl(
        &schema,
        Cursor::new(&input),
        &mut Vec::new(),
        &vb_decode_options(true),
    )
    .expect_err("truncated VB block should fail in strict mode");
    assert!(
        matches!(
            error.code,
            copybook_core::ErrorCode::CBKF223_BDW_UNDERFLOW
                | copybook_core::ErrorCode::CBKF224_RDW_BEYOND_BLOCK
        ),
        "unexpected error code: {:?}",
        error.code
    );
}

#[test]
fn encoding_vb_format_name_round_trip() {
    assert_eq!(RecordFormat::Vb.to_string(), "vb");
    assert_eq!(
        RecordFormat::from_str("vb").expect("vb parses"),
        RecordFormat::Vb
    );
    assert!(RecordFormat::Vb.is_variable());
    assert!(!RecordFormat::Vb.is_fixed());
}

#[test]
fn encoding_vb_parallel_decode_matches_sequential() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    let input = frame_block(&[b"HELLO", b"WORLD", b"AGAIN", b"FORTH", b"FIFTH"]);

    let mut sequential = Vec::new();
    decode_file_to_jsonl(
        &schema,
        Cursor::new(&input),
        &mut sequential,
        &vb_decode_options(false),
    )
    .expect("sequential VB decode should succeed");

    let parallel_options = DecodeOptions {
        threads: 4,
        ..vb_decode_options(false)
    };
    let mut parallel = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(&input),
        &mut parallel,
        &parallel_options,
    )
    .expect("parallel VB decode should succeed");
    assert_eq!(summary.records_processed, 5);
    assert_eq!(parallel, sequential);
}
