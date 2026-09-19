// SPDX-License-Identifier: AGPL-3.0-or-later
//! Execution-policy seam proofs for #1125.
//!
//! A `DecodeOptions` struct literal written against the published 0.8.1
//! shape must compile unchanged: the literal below names every field, so
//! any added field (even a private one) breaks compilation here. The
//! profile-aware policy travels in [`ExecutionPolicy`](copybook_codec::ExecutionPolicy)
//! instead and reaches the same framing behavior through the additive
//! `*_with_policy` entrypoints.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_codec::{
    Codepage, DecodeOptions, ExecutionPolicy, FloatFormat, JsonNumberMode, PolicyError, RawMode,
    RecordFormat, UnmappablePolicy, ZonedEncodingFormat, decode_file_to_jsonl,
    decode_file_to_jsonl_with_policy,
};
use copybook_core::{ErrorCode, parse_copybook};
use std::io::Cursor;

/// Struct literal in the exact published 0.8.1 shape: every field named, no
/// additions. This is the downstream-compatibility probe, not a behavior
/// test; it compiles only while the public shape is unchanged.
fn legacy_options() -> DecodeOptions {
    DecodeOptions {
        format: RecordFormat::RDW,
        codepage: Codepage::ASCII,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: FloatFormat::IeeeBigEndian,
    }
}

/// RDW record with non-zero reserved bytes: length 5, reserved `0x1234`.
fn nonzero_reserved_rdw() -> &'static [u8] {
    b"\x00\x05\x12\x34HELLO"
}

#[test]
fn legacy_literal_compiles_and_decodes_lenient() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    let options = legacy_options();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(nonzero_reserved_rdw()),
        &mut output,
        &options,
    )
    .unwrap();
    assert_eq!(summary.records_processed, 1);
    assert!(summary.has_warnings());
}

#[test]
fn forwarding_import_exposes_same_shape() {
    let options = copybook_options::DecodeOptions {
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number_mode: JsonNumberMode::Lossless,
        emit_filler: false,
        emit_meta: false,
        emit_raw: RawMode::Off,
        strict_mode: false,
        max_errors: None,
        on_decode_unmappable: UnmappablePolicy::Error,
        threads: 1,
        preserve_zoned_encoding: false,
        preferred_zoned_encoding: ZonedEncodingFormat::Auto,
        float_format: FloatFormat::IeeeBigEndian,
    };
    let canonical = DecodeOptions::default();
    assert_eq!(options.format, canonical.format);
    assert_eq!(options.codepage, canonical.codepage);
    assert_eq!(options.threads, canonical.threads);
}

#[test]
fn reviewed_policy_rejects_empty_and_oversize_bounds() {
    assert_eq!(
        ExecutionPolicy::reviewed(true, 0).unwrap_err(),
        PolicyError::EmptyRecordBound
    );
    let oversize = 16 * 1024 * 1024 + 1;
    assert_eq!(
        ExecutionPolicy::reviewed(false, oversize).unwrap_err(),
        PolicyError::RecordBoundExceedsMaximum { value: oversize }
    );
    let policy = ExecutionPolicy::reviewed(true, 1024).unwrap();
    assert!(policy.reserved_strict());
    assert_eq!(policy.maximum_record_length(), Some(1024));
}

#[test]
fn reviewed_strict_policy_fails_nonzero_reserved() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    let options = DecodeOptions {
        format: RecordFormat::RDW,
        ..legacy_options()
    };
    let policy = ExecutionPolicy::reviewed(true, 1024).unwrap();
    let mut output = Vec::new();
    let result = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(nonzero_reserved_rdw()),
        &mut output,
        &options,
        policy,
    );
    assert!(
        result.is_err(),
        "reviewed strict policy should fail nonzero reserved bytes"
    );
    assert_eq!(
        result.unwrap_err().code,
        ErrorCode::CBKR211_RDW_RESERVED_NONZERO
    );
}

#[test]
fn legacy_and_reviewed_agree_on_equal_facts() {
    let schema = parse_copybook("01 SIMPLE-RECORD PIC X(5).").unwrap();
    // Strict broad mode and a reviewed strict-reserved policy fail alike.
    let strict_options = DecodeOptions {
        strict_mode: true,
        ..legacy_options()
    };
    let strict_result = decode_file_to_jsonl(
        &schema,
        Cursor::new(nonzero_reserved_rdw()),
        &mut Vec::new(),
        &strict_options,
    );
    let reviewed = ExecutionPolicy::reviewed(true, 2048).unwrap();
    let lenient_options = legacy_options();
    let reviewed_result = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(nonzero_reserved_rdw()),
        &mut Vec::new(),
        &lenient_options,
        reviewed,
    );
    for result in [strict_result, reviewed_result] {
        assert!(
            result.is_err(),
            "equal strict facts should fail nonzero reserved bytes alike"
        );
        assert_eq!(
            result.unwrap_err().code,
            ErrorCode::CBKR211_RDW_RESERVED_NONZERO
        );
    }
    // Lenient direct behavior and a lenient reviewed policy succeed alike.
    let direct_result = decode_file_to_jsonl(
        &schema,
        Cursor::new(nonzero_reserved_rdw()),
        &mut Vec::new(),
        &legacy_options(),
    );
    let lenient_reviewed = ExecutionPolicy::reviewed(false, 2048).unwrap();
    let reviewed_lenient_result = decode_file_to_jsonl_with_policy(
        &schema,
        Cursor::new(nonzero_reserved_rdw()),
        &mut Vec::new(),
        &legacy_options(),
        lenient_reviewed,
    );
    assert!(direct_result.is_ok());
    assert!(reviewed_lenient_result.is_ok());
}
