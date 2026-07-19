// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]
//! Worker-configuration determinism matrix (issue #578).
//!
//! Existing determinism proofs (`decode_determinism_proofs.rs`,
//! `encode_determinism_proofs.rs`) repeat the *single-record* `decode_record`
//! path and never vary the worker count; the threaded streaming tests in
//! `streaming_file_tests.rs` assert error-recovery and summary invariants but
//! not byte-identical output equality across thread counts. This suite closes
//! the missing end-to-end plane: for **both** `Fixed` and `RDW` framing, the
//! streaming `decode_file_to_jsonl` / `encode_jsonl_to_file` entry points must
//! produce **byte-identical output across worker counts**, and the decode→encode
//! round-trip must reproduce the original bytes regardless of `threads`.
//!
//! Scenario rows (per #578):
//! * `determinism.fixed.decode.threaded` / `determinism.fixed.encode.threaded`
//! * `determinism.rdw.decode.threaded` / `determinism.rdw.encode.threaded`
//!
//! ...plus the fixed/RDW round-trip and cross-framing rows.

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl,
    encode_jsonl_to_file,
};
use copybook_core::parse_copybook;
use std::io::Cursor;

/// One 5-byte numeric field per record; ASCII digits keep the payload readable.
const COPYBOOK: &str = "01 REC.\n   05 N PIC 9(5).\n";

/// Worker counts swept by every row. 1 is the deterministic baseline the others
/// must match byte-for-byte.
const THREAD_COUNTS: &[usize] = &[1, 2, 4, 8];

/// Number of records — large enough that multiple workers actually partition the
/// stream, so any order- or worker-dependence would surface.
const RECORDS: usize = 250;

fn decode_opts(format: RecordFormat, threads: usize) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(format)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
        .with_threads(threads)
}

fn encode_opts(format: RecordFormat, threads: usize) -> EncodeOptions {
    EncodeOptions::new()
        .with_format(format)
        .with_codepage(Codepage::ASCII)
        .with_threads(threads)
}

/// Distinct, order-sensitive payloads: record `i` is the 5-digit zero-padded `i`.
fn fixed_stream() -> Vec<u8> {
    (0..RECORDS)
        .flat_map(|i| format!("{i:05}").into_bytes())
        .collect()
}

/// Wrap each 5-byte payload in a canonical RDW frame (2-byte BE length, 2
/// reserved zero bytes, payload).
fn rdw_stream() -> Vec<u8> {
    (0..RECORDS)
        .flat_map(|i| {
            let payload = format!("{i:05}");
            let mut frame = vec![0x00, 0x05, 0x00, 0x00];
            frame.extend_from_slice(payload.as_bytes());
            frame
        })
        .collect()
}

fn decode(
    schema: &copybook_core::Schema,
    data: &[u8],
    format: RecordFormat,
    threads: usize,
) -> Vec<u8> {
    let mut out = Vec::new();
    let summary = decode_file_to_jsonl(
        schema,
        Cursor::new(data),
        &mut out,
        &decode_opts(format, threads),
    )
    .unwrap_or_else(|e| panic!("decode ({format:?}, threads={threads}) failed: {e}"));
    assert_eq!(
        summary.records_processed, RECORDS as u64,
        "decode ({format:?}, threads={threads}) processed {} records",
        summary.records_processed
    );
    out
}

fn encode(
    schema: &copybook_core::Schema,
    jsonl: &[u8],
    format: RecordFormat,
    threads: usize,
) -> Vec<u8> {
    let mut out = Vec::new();
    encode_jsonl_to_file(
        schema,
        Cursor::new(jsonl),
        &mut out,
        &encode_opts(format, threads),
    )
    .unwrap_or_else(|e| panic!("encode ({format:?}, threads={threads}) failed: {e}"));
    out
}

/// Assert `decode_file_to_jsonl` output is byte-identical across worker counts.
fn assert_decode_threaded_stable(format: RecordFormat, data: &[u8]) {
    let schema = parse_copybook(COPYBOOK).expect("copybook parses");
    let baseline = decode(&schema, data, format, THREAD_COUNTS[0]);
    for &threads in &THREAD_COUNTS[1..] {
        let out = decode(&schema, data, format, threads);
        assert_eq!(
            out, baseline,
            "{format:?} decode differs at threads={threads} vs baseline threads={}",
            THREAD_COUNTS[0]
        );
    }
}

/// Assert `encode_jsonl_to_file` output is byte-identical across worker counts.
fn assert_encode_threaded_stable(format: RecordFormat, data: &[u8]) {
    let schema = parse_copybook(COPYBOOK).expect("copybook parses");
    // Produce canonical JSONL once (single-threaded), then encode it under each
    // worker count.
    let jsonl = decode(&schema, data, format, 1);
    let baseline = encode(&schema, &jsonl, format, THREAD_COUNTS[0]);
    for &threads in &THREAD_COUNTS[1..] {
        let out = encode(&schema, &jsonl, format, threads);
        assert_eq!(
            out, baseline,
            "{format:?} encode differs at threads={threads} vs baseline threads={}",
            THREAD_COUNTS[0]
        );
    }
}

/// Assert decode→encode reproduces the original bytes under every worker count.
fn assert_roundtrip_threaded_stable(format: RecordFormat, data: &[u8]) {
    let schema = parse_copybook(COPYBOOK).expect("copybook parses");
    for &threads in THREAD_COUNTS {
        let jsonl = decode(&schema, data, format, threads);
        let reencoded = encode(&schema, &jsonl, format, threads);
        assert_eq!(
            reencoded, data,
            "{format:?} round-trip not byte-identical at threads={threads}"
        );
    }
}

// ===========================================================================
// Fixed framing
// ===========================================================================

#[test]
fn fixed_decode_deterministic_across_threads() {
    assert_decode_threaded_stable(RecordFormat::Fixed, &fixed_stream());
}

#[test]
fn fixed_encode_deterministic_across_threads() {
    assert_encode_threaded_stable(RecordFormat::Fixed, &fixed_stream());
}

#[test]
fn fixed_roundtrip_deterministic_across_threads() {
    assert_roundtrip_threaded_stable(RecordFormat::Fixed, &fixed_stream());
}

// ===========================================================================
// RDW framing
// ===========================================================================

#[test]
fn rdw_decode_deterministic_across_threads() {
    assert_decode_threaded_stable(RecordFormat::RDW, &rdw_stream());
}

#[test]
fn rdw_encode_deterministic_across_threads() {
    assert_encode_threaded_stable(RecordFormat::RDW, &rdw_stream());
}

#[test]
fn rdw_roundtrip_deterministic_across_threads() {
    assert_roundtrip_threaded_stable(RecordFormat::RDW, &rdw_stream());
}

// ===========================================================================
// Cross-framing sanity: fixed and RDW decode the same logical records to the
// same field values (framing changes bytes, not decoded content).
// ===========================================================================

#[test]
fn fixed_and_rdw_decode_same_field_values_across_threads() {
    let schema = parse_copybook(COPYBOOK).expect("copybook parses");
    for &threads in THREAD_COUNTS {
        let fixed_json = decode(&schema, &fixed_stream(), RecordFormat::Fixed, threads);
        let rdw_json = decode(&schema, &rdw_stream(), RecordFormat::RDW, threads);
        assert_eq!(
            fixed_json, rdw_json,
            "fixed and RDW must decode to identical JSONL at threads={threads}"
        );
    }
}
