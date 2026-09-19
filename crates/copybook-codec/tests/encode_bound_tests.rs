// SPDX-License-Identifier: AGPL-3.0-or-later
//! Reviewed record-bound enforcement on encode paths (#1129, writes).
//!
//! A fixed layout above the cap fails pre-execution (output stays absent);
//! field-encoded and raw-replay payloads above the cap fail per record with
//! `CBKF226_RECORD_BOUND_EXCEEDED` before their bytes reach the output.
//! Uncapped (direct) runs keep architectural behavior on every path,
//! including workers.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, EncodeOptions, ExecutionPolicy, RecordFormat, encode_jsonl_to_file,
    encode_jsonl_to_file_with_policy,
};
use copybook_core::{ErrorCode, parse_copybook};
use std::io::Cursor;

/// 15-byte fixed layout.
const FIXED_COPYBOOK: &str =
    "       01  REC.\n           05  NAME     PIC X(10).\n           05  AMOUNT   PIC 9(5).\n";
const FIXED_JSONL: &str = "{\"NAME\": \"ALICE     \", \"AMOUNT\": \"00100\"}\n";
const FIXED_RECORD: &[u8] = b"ALICE     00100";

fn fixed_options() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

fn expect_bound_failure(summary: &copybook_codec::RunSummary, record_index: u64) {
    assert_eq!(summary.records_processed, 0);
    assert_eq!(summary.failures.len(), 1);
    assert_eq!(
        summary.failures[0].error.code(),
        ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED
    );
    assert_eq!(summary.failures[0].record_index, record_index);
}

#[test]
fn fixed_cap_at_lrecl_encodes() {
    let schema = parse_copybook(FIXED_COPYBOOK).unwrap();
    let policy = ExecutionPolicy::reviewed(false, 15).unwrap();
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file_with_policy(
        &schema,
        Cursor::new(FIXED_JSONL.as_bytes()),
        &mut output,
        &fixed_options(),
        policy,
    )
    .unwrap();
    assert_eq!(summary.records_processed, 1);
    assert_eq!(output, FIXED_RECORD);
}

#[test]
fn fixed_cap_below_lrecl_fails_before_output() {
    let schema = parse_copybook(FIXED_COPYBOOK).unwrap();
    let policy = ExecutionPolicy::reviewed(false, 14).unwrap();
    let mut output = Vec::new();
    let result = encode_jsonl_to_file_with_policy(
        &schema,
        Cursor::new(FIXED_JSONL.as_bytes()),
        &mut output,
        &fixed_options(),
        policy,
    );
    assert!(result.is_err());
    if let Err(error) = result {
        assert_eq!(error.code, ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED);
    }
    assert!(
        output.is_empty(),
        "pre-execution failure leaves output absent"
    );
}

#[test]
fn rdw_produced_over_cap_fails_with_identity() {
    let schema = parse_copybook("       01  REC PIC X(8).\n").unwrap();
    let options = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII);
    let policy = ExecutionPolicy::reviewed(false, 7).unwrap();
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file_with_policy(
        &schema,
        Cursor::new(b"{\"REC\": \"RECORD01\"}\n".as_slice()),
        &mut output,
        &options,
        policy,
    )
    .unwrap();
    expect_bound_failure(&summary, 1);
    assert!(output.is_empty());
}

#[test]
fn vb_produced_over_cap_fails_with_identity() {
    let schema = parse_copybook("       01  REC PIC X(5).\n").unwrap();
    let options = EncodeOptions::new()
        .with_format(RecordFormat::Vb)
        .with_codepage(Codepage::ASCII);
    // 5 payload bytes framed as one BDW block: the cap binds the payload.
    let policy = ExecutionPolicy::reviewed(false, 4).unwrap();
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file_with_policy(
        &schema,
        Cursor::new(b"{\"REC\": \"HELLO\"}\n".as_slice()),
        &mut output,
        &options,
        policy,
    )
    .unwrap();
    expect_bound_failure(&summary, 1);
    assert!(output.is_empty());
}

#[test]
fn vb_produced_over_cap_fails_on_workers() {
    let schema = parse_copybook("       01  REC PIC X(5).\n").unwrap();
    let options = EncodeOptions::new()
        .with_format(RecordFormat::Vb)
        .with_codepage(Codepage::ASCII)
        .with_threads(2);
    let policy = ExecutionPolicy::reviewed(false, 4).unwrap();
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file_with_policy(
        &schema,
        Cursor::new(b"{\"REC\": \"HELLO\"}\n{\"REC\": \"WORLD\"}\n".as_slice()),
        &mut output,
        &options,
        policy,
    )
    .unwrap();
    assert_eq!(summary.records_processed, 0);
    assert!(
        summary
            .failures
            .iter()
            .all(|failure| failure.error.code() == ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED)
    );
    assert!(!summary.failures.is_empty());
}

#[test]
fn raw_record_replay_over_cap_fails_with_identity() {
    let schema = parse_copybook("       01  REC PIC X(15).\n").unwrap();
    let options = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII)
        .with_use_raw(true);
    let policy = ExecutionPolicy::reviewed(false, 14).unwrap();
    // base64 of 15 bare payload bytes: replay must not bypass the bound.
    let jsonl = "{\"REC\": \"ALICE     00100\", \"raw_b64\": \"QUxJQ0UgICAgIDAwMTAw\", \"raw_capture\": \"record\"}\n";
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file_with_policy(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut output,
        &options,
        policy,
    )
    .unwrap();
    expect_bound_failure(&summary, 1);
    assert!(output.is_empty());
}

#[test]
fn raw_record_rdw_replay_over_cap_fails_with_identity() {
    let schema = parse_copybook("       01  REC PIC X(15).\n").unwrap();
    let options = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII)
        .with_use_raw(true);
    let policy = ExecutionPolicy::reviewed(false, 14).unwrap();
    // base64 of `00 0F 00 00` + 15 payload bytes: header consistency is
    // validated and the 15-byte payload still hits the bound.
    let jsonl = "{\"REC\": \"ALICE     00100\", \"raw_b64\": \"AA8AAEFMSUNFICAgICAwMDEwMA==\", \"raw_capture\": \"record+rdw\"}\n";
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file_with_policy(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut output,
        &options,
        policy,
    )
    .unwrap();
    expect_bound_failure(&summary, 1);
    assert!(output.is_empty());
}

#[test]
fn uncapped_direct_encode_keeps_behavior() {
    // No profile, no cap: the 15-byte layout still encodes.
    let schema = parse_copybook(FIXED_COPYBOOK).unwrap();
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file(
        &schema,
        Cursor::new(FIXED_JSONL.as_bytes()),
        &mut output,
        &fixed_options(),
    )
    .unwrap();
    assert_eq!(summary.records_processed, 1);
    assert_eq!(output, FIXED_RECORD);
}
