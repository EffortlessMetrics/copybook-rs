// SPDX-License-Identifier: AGPL-3.0-or-later
//! Reviewed record-bound enforcement (#1129, read side).
//!
//! A capped reader fails with `CBKF226_RECORD_BOUND_EXCEEDED` before any
//! payload allocation or read; an uncapped reader keeps architectural
//! behavior. The bare-RDW path uses the payload-length convention while
//! nested VB lengths include their 4-byte header (mainframe LL); both
//! checks compare the logical payload bytes presented downstream.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_error::ErrorCode;
use copybook_rdw::{RDWRecordReader, VbBlockReader, VbBlockWriter};
use std::io::Cursor;

fn rdw_bytes(payload_len: u16, payload: &[u8]) -> Vec<u8> {
    let mut out = Vec::from(u16::to_be_bytes(payload_len));
    out.extend_from_slice(&[0, 0]);
    out.extend_from_slice(payload);
    out
}

#[test]
fn rdw_declared_at_cap_reads() {
    let payload = b"HELLO";
    let mut reader = RDWRecordReader::new(Cursor::new(rdw_bytes(5, payload)), false)
        .with_max_record_length(Some(5));
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, payload);
}

#[test]
fn rdw_declared_over_cap_fails_before_payload() {
    let payload = b"HELLO";
    let mut reader = RDWRecordReader::new(Cursor::new(rdw_bytes(5, payload)), false)
        .with_max_record_length(Some(4));
    let error = reader.read_record().unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED);
}

#[test]
fn rdw_over_cap_short_payload_still_fails_on_declared_length() {
    // Declared 100 but only 5 bytes follow: the bound fires on the declared
    // length before the truncation path can run.
    let mut reader = RDWRecordReader::new(Cursor::new(rdw_bytes(100, b"HELLO")), false)
        .with_max_record_length(Some(50));
    let error = reader.read_record().unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED);
}

#[test]
fn rdw_uncapped_reader_ignores_length() {
    let payload = vec![0x41; 300];
    let mut reader = RDWRecordReader::new(Cursor::new(rdw_bytes(300, &payload)), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, payload);
}

#[test]
fn rdw_zero_length_record_passes_any_cap() {
    let mut reader =
        RDWRecordReader::new(Cursor::new(rdw_bytes(0, b"")), false).with_max_record_length(Some(1));
    let record = reader.read_record().unwrap().unwrap();
    assert!(record.payload.is_empty());
}

fn vb_block(payloads: &[&[u8]]) -> Vec<u8> {
    let mut out = Vec::new();
    let mut writer = VbBlockWriter::new(&mut out);
    for payload in payloads {
        writer.write_record_from_payload(payload, 0).unwrap();
    }
    writer.finish().unwrap();
    out
}

#[test]
fn vb_nested_over_cap_fails_with_block_context() {
    let data = vb_block(&[b"OK", b"TOO-LONG-PAYLOAD"]);
    let mut reader = VbBlockReader::new(Cursor::new(data), false).with_max_record_length(Some(5));
    let first = reader.read_record().unwrap().unwrap();
    assert_eq!(first.payload, b"OK");
    let error = reader.read_record().unwrap_err();
    assert_eq!(error.code, ErrorCode::CBKF226_RECORD_BOUND_EXCEEDED);
    let context = error.context.expect("bound error carries context");
    assert_eq!(context.record_index, Some(2));
}

#[test]
fn vb_uncapped_reader_keeps_architectural_behavior() {
    let data = vb_block(&[b"OK", b"TOO-LONG-PAYLOAD"]);
    let mut reader = VbBlockReader::new(Cursor::new(data), false);
    let mut count = 0;
    while reader.read_record().unwrap().is_some() {
        count += 1;
    }
    assert_eq!(count, 2);
}
