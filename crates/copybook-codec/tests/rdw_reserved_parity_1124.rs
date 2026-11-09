// SPDX-License-Identifier: AGPL-3.0-or-later
//! Cross-path RDW reserved-byte parity lock for #1124.
//!
//! The reserved-byte validation fix made
//! [`RecordIterator`](copybook_codec::RecordIterator) enforce the same
//! framing contract as the bulk decode path (`RDWRecordReader`: strict
//! framing fails with `CBKR211_RDW_RESERVED_NONZERO`, lenient framing warns
//! and continues). Verify and occurrence Explain both consume the iterator
//! gate, so every public path must now name the same stable identity for
//! the same bytes.
//!
//! This file holds the single-fixture cross-path test that did not exist:
//! one non-zero-reserved RDW record exercised through bulk Decode, the
//! record iterator, the Verify loop shape, occurrence Explain, and the
//! determinism gate. Boundary distinctness (short header, invalid length,
//! truncated payload, suspect ASCII) is asserted only at the code level;
//! the exhaustive per-path unit coverage already lives beside the code
//! (`iterator.rs`, `determinism.rs`) and is deliberately not duplicated.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, ExecutionPolicy, RecordFormat, RecordIterator,
    check_decode_determinism_with_policy, decode_file_to_jsonl, decode_file_to_jsonl_with_policy,
    occurrence::{OccurrenceOptions, OccurrenceOutcome, explain_occurrence},
    record::RDWRecordReader,
};
use copybook_core::{ErrorCode, parse_copybook};
use std::io::Cursor;

/// Eight-byte layout the fixture payload satisfies exactly.
const COPYBOOK: &str = "01 RECORD.\n 05 FIELD-A PIC X(8).";

/// One RDW record: length 8, reserved `0x0001`, payload `RECORD01`.
fn reserved_nonzero_frame() -> Vec<u8> {
    let mut frame = vec![0x00, 0x08, 0x00, 0x01];
    frame.extend_from_slice(b"RECORD01");
    frame
}

fn rdw_options(strict: bool) -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(strict)
}

fn strict_policy() -> ExecutionPolicy {
    ExecutionPolicy::reviewed(true, 32_760).expect("valid test policy")
}

fn lenient_policy() -> ExecutionPolicy {
    ExecutionPolicy::reviewed(false, 32_760).expect("valid test policy")
}

/// The stable identity every strict path must name: code, 1-based record
/// index, and reserved-byte offset within the RDW header.
fn assert_reserved_identity(error: &copybook_core::Error) {
    assert_eq!(error.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
    let context = error.context.as_ref().expect("context is populated");
    assert_eq!(context.record_index, Some(1));
    assert_eq!(context.byte_offset, Some(2));
}

#[test]
fn strict_bulk_decode_names_reserved_identity() {
    let schema = parse_copybook(COPYBOOK).expect("parse copybook");
    // Broad strict mode through the legacy bulk entrypoint.
    let direct = decode_file_to_jsonl(
        &schema,
        Cursor::new(reserved_nonzero_frame()),
        &mut Vec::new(),
        &rdw_options(true),
    )
    .expect_err("strict bulk decode must fail");
    assert_reserved_identity(&direct);
    // Reviewed strict-reserved policy with otherwise lenient options
    // (the profile surface): same bytes, same identity.
    let reviewed = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(reserved_nonzero_frame()),
        &mut Vec::new(),
        &rdw_options(false),
        strict_policy(),
    )
    .expect_err("reviewed strict policy must fail");
    assert_reserved_identity(&reviewed);
}

#[test]
fn strict_iterator_names_reserved_identity() {
    let schema = parse_copybook(COPYBOOK).expect("parse copybook");
    // Broad strict mode constructor.
    let mut iterator = RecordIterator::new(
        Cursor::new(reserved_nonzero_frame()),
        &schema,
        &rdw_options(true),
    )
    .expect("iterator builds");
    let direct = iterator
        .read_raw_record()
        .expect_err("strict iterator must fail");
    assert_reserved_identity(&direct);
    // Reviewed strict-reserved policy constructor (the Verify loop shape).
    let mut iterator = RecordIterator::with_policy(
        Cursor::new(reserved_nonzero_frame()),
        &schema,
        &rdw_options(false),
        strict_policy(),
    )
    .expect("iterator builds");
    let reviewed = iterator
        .read_raw_record()
        .expect_err("reviewed strict policy must fail");
    assert_reserved_identity(&reviewed);
}

#[test]
fn strict_verify_loop_shape_names_reserved_identity() {
    // Verify consumes `RecordIterator::with_policy` + `read_raw_record` in
    // the loop below (same shape as the verify command); the framing gate
    // fires before any payload decode, so the identity must match the bulk
    // and iterator paths exactly.
    let schema = parse_copybook(COPYBOOK).expect("parse copybook");
    let mut iterator = RecordIterator::with_policy(
        Cursor::new(reserved_nonzero_frame()),
        &schema,
        &rdw_options(false),
        strict_policy(),
    )
    .expect("iterator builds");
    let mut records_total = 0u64;
    let error = loop {
        match iterator.read_raw_record() {
            Ok(Some(_)) => records_total += 1,
            Ok(None) => break None,
            Err(error) => {
                records_total += 1;
                break Some(error);
            }
        }
    }
    .expect("strict verify gate must stop on the framing failure");
    assert_eq!(records_total, 1);
    assert_reserved_identity(&error);
}

#[test]
fn strict_explain_names_reserved_identity() {
    let schema = parse_copybook(COPYBOOK).expect("parse copybook");
    let outcome = explain_occurrence(
        Cursor::new(reserved_nonzero_frame()),
        &schema,
        &rdw_options(true),
        &OccurrenceOptions {
            format: RecordFormat::RDW,
            codepage: Codepage::ASCII,
            target_record: None,
            code_filter: None,
        },
    )
    .expect("explain runs");
    assert!(
        matches!(&outcome, OccurrenceOutcome::Found(occurrence)
            if occurrence.code == "CBKR211_RDW_RESERVED_NONZERO"
                && occurrence.record_index == 1),
        "strict explain must surface the stable reserved identity, got: {outcome:?}"
    );
}

#[test]
fn strict_determinism_gate_names_reserved_identity() {
    let schema = parse_copybook(COPYBOOK).expect("parse copybook");
    let error = check_decode_determinism_with_policy(
        &schema,
        &reserved_nonzero_frame(),
        &rdw_options(false),
        strict_policy(),
    )
    .expect_err("strict determinism gate must fail");
    assert_reserved_identity(&error);
}

#[test]
fn lenient_mode_continues_on_every_library_path() {
    let schema = parse_copybook(COPYBOOK).expect("parse copybook");
    // Bulk decode: record processed, warning accounted.
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(reserved_nonzero_frame()),
        &mut output,
        &rdw_options(false),
        lenient_policy(),
    )
    .expect("lenient bulk decode continues");
    assert_eq!(summary.records_processed, 1);
    assert!(summary.has_warnings());
    // Iterator: payload readable.
    let mut iterator = RecordIterator::with_policy(
        Cursor::new(reserved_nonzero_frame()),
        &schema,
        &rdw_options(false),
        lenient_policy(),
    )
    .expect("iterator builds");
    let payload = iterator
        .read_raw_record()
        .expect("lenient iterator continues")
        .expect("record present");
    assert_eq!(payload, b"RECORD01");
    // Explain: no failure to locate.
    let outcome = explain_occurrence(
        Cursor::new(reserved_nonzero_frame()),
        &schema,
        &rdw_options(false),
        &OccurrenceOptions {
            format: RecordFormat::RDW,
            codepage: Codepage::ASCII,
            target_record: None,
            code_filter: None,
        },
    )
    .expect("explain runs");
    assert!(
        matches!(outcome, OccurrenceOutcome::Absent(_)),
        "lenient explain must find no failure, got: {outcome:?}"
    );
    // Determinism gate: still deterministic.
    let result = check_decode_determinism_with_policy(
        &schema,
        &reserved_nonzero_frame(),
        &rdw_options(false),
        lenient_policy(),
    )
    .expect("lenient determinism gate passes");
    assert!(result.is_deterministic);
}

#[test]
fn neighboring_framing_failures_keep_distinct_identities() {
    // Boundary-level distinctness only: short header, invalid length,
    // truncated payload, and suspect ASCII must never collapse into the
    // reserved-byte identity (or into each other where the reader
    // distinguishes them). Exhaustive per-path coverage lives beside the
    // code and is not duplicated here.
    let reserved = ErrorCode::CBKR211_RDW_RESERVED_NONZERO;

    // Short header under strict framing.
    let short = vec![0x00, 0x08];
    let error = RDWRecordReader::new(Cursor::new(short), true)
        .read_record()
        .expect_err("short header must fail");
    assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    assert_ne!(error.code, reserved);

    // Declared length exceeds the available body.
    let mut invalid = vec![0x00, 0x08, 0x00, 0x00];
    invalid.extend_from_slice(b"ABC");
    let error = RDWRecordReader::new(Cursor::new(invalid), true)
        .read_record()
        .expect_err("short payload must fail");
    assert_eq!(error.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
    assert_ne!(error.code, reserved);

    // Declared length exceeds the reviewed bound with a complete body
    // present: bound enforcement runs before any payload read, so this
    // isolates over-declaration from truncation.
    let mut overlong = vec![0x00, 0x64, 0x00, 0x00];
    overlong.extend_from_slice(&[0x41; 100]);
    let error = RDWRecordReader::new(Cursor::new(overlong), true)
        .with_max_record_length(Some(8))
        .read_record()
        .expect_err("over-bound declaration must fail");
    assert_eq!(error.code, ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED);
    assert_ne!(error.code, reserved);

    // Suspect ASCII transfer corruption (length bytes are digits).
    let mut suspect = vec![0x31, 0x32, 0x00, 0x00];
    suspect.extend_from_slice(b"AB");
    let error = RDWRecordReader::new(Cursor::new(suspect), true)
        .read_record()
        .expect_err("suspect ASCII must fail");
    assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert_ne!(error.code, reserved);
}
