// SPDX-License-Identifier: AGPL-3.0-or-later
//! Reviewed record-bound enforcement on read paths (#1129, reads).
//!
//! Fixed layouts below the cap fail before input is consumed (output stays
//! absent); RDW/VB records declaring over-cap payloads fail with
//! `CBKF226_RECORD_BOUND_EXCEEDED` before allocation. Uncapped (direct)
//! runs keep architectural behavior on every path, including workers.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, ExecutionPolicy, RecordFormat, RecordIterator, decode_file_to_jsonl,
    decode_file_to_jsonl_with_policy,
};
use copybook_core::{ErrorCode, parse_copybook};
use std::io::Cursor;

/// 15-byte fixed layout.
const FIXED_COPYBOOK: &str =
    "       01  REC.\n           05  NAME     PIC X(10).\n           05  AMOUNT   PIC 9(5).\n";
const FIXED_RECORD: &[u8] = b"ALICE     00100";

fn fixed_options() -> DecodeOptions {
    DecodeOptions::default()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

fn rdw_options() -> DecodeOptions {
    DecodeOptions::default()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII)
}

fn rdw_record(payload_len: u16, payload: &[u8]) -> Vec<u8> {
    let mut out = Vec::from(u16::to_be_bytes(payload_len));
    out.extend_from_slice(&[0, 0]);
    out.extend_from_slice(payload);
    out
}

fn expect_bound_exceeded<T>(result: Result<T, copybook_core::Error>) {
    assert!(
        result.is_err(),
        "expected CBKF226_RECORD_BOUND_EXCEEDED"
    );
    if let Err(error) = result {
        assert_eq!(error.code, ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED);
    }
}

#[test]
fn fixed_cap_at_lrecl_decodes() {
    let schema = parse_copybook(FIXED_COPYBOOK).unwrap();
    let policy = ExecutionPolicy::reviewed(false, 15).unwrap();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(FIXED_RECORD),
        &mut output,
        &fixed_options(),
        policy,
    )
    .unwrap();
    assert_eq!(summary.records_processed, 1);
}

#[test]
fn fixed_cap_below_lrecl_fails_before_output() {
    let schema = parse_copybook(FIXED_COPYBOOK).unwrap();
    let policy = ExecutionPolicy::reviewed(false, 14).unwrap();
    let mut output = Vec::new();
    let result = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(FIXED_RECORD),
        &mut output,
        &fixed_options(),
        policy,
    );
    expect_bound_exceeded(result);
    assert!(
        output.is_empty(),
        "pre-execution failure leaves output absent"
    );
}

#[test]
fn fixed_cap_below_lrecl_fails_on_workers() {
    let schema = parse_copybook(FIXED_COPYBOOK).unwrap();
    let policy = ExecutionPolicy::reviewed(false, 14).unwrap();
    let options = fixed_options().with_threads(2);
    let result = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new([FIXED_RECORD, FIXED_RECORD].concat()),
        &mut Vec::new(),
        &options,
        policy,
    );
    expect_bound_exceeded(result);
}

#[test]
fn iterator_fixed_cap_below_lrecl_fails_before_read() {
    let schema = parse_copybook(FIXED_COPYBOOK).unwrap();
    let policy = ExecutionPolicy::reviewed(false, 14).unwrap();
    let result =
        RecordIterator::with_policy(Cursor::new(FIXED_RECORD), &schema, &fixed_options(), policy);
    expect_bound_exceeded(result.map(|_| ()));
}

#[test]
fn rdw_declared_over_cap_fails_with_identity() {
    let schema = parse_copybook("       01  REC PIC X(8).\n").unwrap();
    let policy = ExecutionPolicy::reviewed(false, 7).unwrap();
    let result = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(rdw_record(8, b"RECORD01")),
        &mut Vec::new(),
        &rdw_options(),
        policy,
    );
    expect_bound_exceeded(result.map(|_| ()));
}

#[test]
fn rdw_later_record_over_cap_fails() {
    let schema = parse_copybook("       01  REC PIC X(8).\n").unwrap();
    let policy = ExecutionPolicy::reviewed(false, 8).unwrap();
    let mut data = rdw_record(8, b"RECORD01");
    data.extend_from_slice(&rdw_record(9, b"TOOLONG99"));
    let result = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(data),
        &mut Vec::new(),
        &rdw_options(),
        policy,
    );
    expect_bound_exceeded(result.map(|_| ()));
}

#[test]
fn uncapped_direct_run_keeps_behavior() {
    // No profile, no cap: a long RDW payload still decodes leniently.
    let schema = parse_copybook("       01  REC PIC X(8).\n").unwrap();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(rdw_record(8, b"RECORD01")),
        &mut output,
        &rdw_options(),
    )
    .unwrap();
    assert_eq!(summary.records_processed, 1);
}
