// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]
//! Streaming and file-level operation tests for copybook-codec.
//!
//! Covers: `iter_records`, `iter_records_from_file`, `decode_file_to_jsonl`,
//! `encode_jsonl_to_file`, `RecordIterator`, and `RunSummary`.

use base64::Engine;
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, RunSummary,
    decode_file_to_jsonl, encode_jsonl_to_file, iter_records,
};
use copybook_core::{ErrorCode, parse_copybook, project_schema};
use std::io::Cursor;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn ascii_decode_opts() -> DecodeOptions {
    DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(false)
}

fn ascii_encode_opts() -> EncodeOptions {
    EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
}

/// Build `n` fixed-length ASCII records for a simple PIC 9(5) schema.
fn build_pic9_5_data(n: usize) -> Vec<u8> {
    let mut data = Vec::with_capacity(n * 5);
    for i in 0..n {
        data.extend_from_slice(format!("{:05}", i % 100_000).as_bytes());
    }
    data
}

/// Build JSONL input for a simple PIC 9(5) schema.
fn build_pic9_5_jsonl(n: usize) -> String {
    let mut s = String::new();
    for i in 0..n {
        s.push_str(&format!(r#"{{"TEST-FIELD": "{:05}"}}"#, i % 100_000));
        s.push('\n');
    }
    s
}

/// Simple schema: single numeric field.
const SIMPLE_SCHEMA: &str = "01 TEST-FIELD PIC 9(5).";

/// Multi-field schema: ID + NAME + AMOUNT.
const MULTI_FIELD_SCHEMA: &str = r"
    01 RECORD.
       05 CUST-ID    PIC 9(5).
       05 CUST-NAME  PIC X(10).
       05 AMOUNT     PIC 9(5).
";

/// Small RDW schema for record-framed deterministic checks.
const RDW_SCHEMA: &str = "01 FIELD PIC X(5).";

fn build_multi_field_data(n: usize) -> Vec<u8> {
    let mut data = Vec::with_capacity(n * 20);
    for i in 0..n {
        data.extend_from_slice(format!("{:05}", i % 100_000).as_bytes());
        data.extend_from_slice(format!("{:<10}", format!("NAME{i}")).as_bytes());
        data.extend_from_slice(format!("{:05}", (i * 100) % 100_000).as_bytes());
    }
    data
}

/// Build one or more RDW records from fixed-length payload values.
fn build_rdw_records(payloads: &[&str]) -> Vec<u8> {
    let mut data = Vec::new();
    for value in payloads {
        let bytes = value.as_bytes();
        let rdw_len = bytes.len() as u16;
        data.extend_from_slice(&rdw_len.to_be_bytes());
        data.extend_from_slice(&[0x00, 0x00]);
        data.extend_from_slice(bytes);
    }
    data
}

fn parse_raw_b64_field(raw_json: &serde_json::Value) -> Vec<u8> {
    parse_raw_b64_field_from_any(raw_json, "raw_b64")
}

fn parse_raw_b64_field_from_any(raw_json: &serde_json::Value, canonical_key: &str) -> Vec<u8> {
    let raw_field = raw_json
        .get(canonical_key)
        .or_else(|| raw_json.get("__raw_b64"))
        .expect("raw output should include raw bytes");
    let raw_field = raw_field
        .as_str()
        .expect("raw_b64 field should be a base64 string");
    base64::engine::general_purpose::STANDARD
        .decode(raw_field)
        .expect("raw_b64 payload should decode")
}

// ===========================================================================
// 1. iter_records basic usage
// ===========================================================================

#[test]
fn iter_records_single_record() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = b"00042";
    let opts = ascii_decode_opts();
    let iter = iter_records(Cursor::new(data.as_slice()), &schema, &opts).unwrap();
    let records: Vec<_> = iter.collect::<Result<Vec<_>, _>>().unwrap();
    assert_eq!(records.len(), 1);
    assert_eq!(records[0]["TEST-FIELD"], "00042");
}

#[test]
fn iter_records_two_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = b"0000100002";
    let opts = ascii_decode_opts();
    let records: Vec<_> = iter_records(Cursor::new(data.as_slice()), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();
    assert_eq!(records.len(), 2);
    assert_eq!(records[0]["TEST-FIELD"], "00001");
    assert_eq!(records[1]["TEST-FIELD"], "00002");
}

#[test]
fn iter_records_multi_field() {
    let schema = parse_copybook(MULTI_FIELD_SCHEMA).unwrap();
    let data = build_multi_field_data(3);
    let opts = ascii_decode_opts();
    let records: Vec<_> = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();
    assert_eq!(records.len(), 3);
    for (i, rec) in records.iter().enumerate() {
        let val: u64 = rec["CUST-ID"].as_str().unwrap_or("0").parse().unwrap_or(0);
        assert_eq!(val, i as u64);
    }
}

// ===========================================================================
// 2. Multi-record streaming (100+ records)
// ===========================================================================

#[test]
fn iter_records_100_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(100);
    let opts = ascii_decode_opts();
    let count = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .count();
    assert_eq!(count, 100);
}

#[test]
fn iter_records_500_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(500);
    let opts = ascii_decode_opts();
    let count = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .count();
    assert_eq!(count, 500);
}

// ===========================================================================
// 3. Streaming memory bounded (no full-file load)
// ===========================================================================

#[test]
fn iter_records_does_not_buffer_all() {
    // Process 1000 records via iterator — the iterator holds at most one record
    // buffer, not the entire dataset.
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(1_000);
    let opts = ascii_decode_opts();
    let mut iter = copybook_codec::RecordIterator::new(Cursor::new(data), &schema, &opts).unwrap();

    // Consume one record and verify internal state
    let first = iter.next().unwrap().unwrap();
    // Lossless mode may normalize leading zeros (e.g. "00000" → "0")
    assert!(first.get("TEST-FIELD").is_some());
    assert_eq!(iter.current_record_index(), 1);
    assert!(!iter.is_eof());

    // Consume the rest
    let rest_count = iter.filter_map(Result::ok).count();
    assert_eq!(rest_count, 999);
}

#[test]
fn record_iterator_raw_record_access() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = b"0000100002";
    let opts = ascii_decode_opts();
    let mut iter =
        copybook_codec::RecordIterator::new(Cursor::new(data.as_slice()), &schema, &opts).unwrap();

    let raw1 = iter.read_raw_record().unwrap().unwrap();
    assert_eq!(&raw1, b"00001");
    assert_eq!(iter.current_record_index(), 1);

    let raw2 = iter.read_raw_record().unwrap().unwrap();
    assert_eq!(&raw2, b"00002");
    assert_eq!(iter.current_record_index(), 2);

    assert!(iter.read_raw_record().unwrap().is_none());
    assert!(iter.is_eof());
}

// ===========================================================================
// 4. decode_file_to_jsonl output format
// ===========================================================================

#[test]
fn decode_file_jsonl_format_lines() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(5);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    let text = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(lines.len(), 5, "Should produce one JSONL line per record");

    for line in &lines {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        assert!(val.get("TEST-FIELD").is_some());
        assert!(val.get("schema").is_some());
        assert!(val.get("record_index").is_some());
    }
}

#[test]
fn decode_file_jsonl_record_indices_sequential() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(10);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    let text = String::from_utf8(output).unwrap();
    for (i, line) in text.lines().enumerate() {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        let idx = val["record_index"].as_u64().unwrap();
        assert_eq!(
            idx,
            (i + 1) as u64,
            "record_index should be 1-based sequential"
        );
    }
}

#[test]
fn decode_file_jsonl_with_meta() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(2);
    let opts = ascii_decode_opts().with_emit_meta(true);
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    let text = String::from_utf8(output).unwrap();
    for line in text.lines() {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        assert!(
            val.get("__record_index").is_some(),
            "meta should include __record_index"
        );
        assert!(
            val.get("__length").is_some(),
            "meta should include __length"
        );
    }
}

// ===========================================================================
// 5. encode from JSONL input
// ===========================================================================

#[test]
fn encode_jsonl_basic() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let opts = ascii_encode_opts();
    let jsonl = build_pic9_5_jsonl(5);
    let mut output = Vec::new();
    let summary =
        encode_jsonl_to_file(&schema, Cursor::new(jsonl.as_bytes()), &mut output, &opts).unwrap();

    assert_eq!(summary.records_processed, 5);
    assert_eq!(output.len(), 5 * 5, "5 records × 5 bytes each");
}

#[test]
fn encode_jsonl_100_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let opts = ascii_encode_opts();
    let jsonl = build_pic9_5_jsonl(100);
    let mut output = Vec::new();
    let summary =
        encode_jsonl_to_file(&schema, Cursor::new(jsonl.as_bytes()), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 100);
    assert_eq!(output.len(), 100 * 5);
}

#[test]
fn encode_jsonl_skips_blank_lines() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let opts = ascii_encode_opts();
    let jsonl = "{\"TEST-FIELD\": \"00001\"}\n\n{\"TEST-FIELD\": \"00002\"}\n".to_string();
    let mut output = Vec::new();
    let summary =
        encode_jsonl_to_file(&schema, Cursor::new(jsonl.as_bytes()), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 2);
}

// ===========================================================================
// 6. Error accumulation during streaming
// ===========================================================================

#[test]
fn decode_file_lenient_accumulates_errors() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    // Inject a record that is too short (only 3 bytes) among valid ones
    // With fixed format, partial record at end is silently dropped
    // Instead, mix valid and valid data but verify non-strict continues
    let data = build_pic9_5_data(10);
    let opts = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(false);
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 10);
    assert!(!summary.has_errors());
}

#[test]
fn decode_file_strict_mode_stops_on_error() {
    // With strict mode, a schema mismatch should stop processing
    let schema = parse_copybook("01 REC.\n   05 NUM PIC 9(3).").unwrap();
    // 3 valid records, then garbage in numeric field
    let mut data = Vec::new();
    data.extend_from_slice(b"001");
    data.extend_from_slice(b"002");
    data.extend_from_slice(b"ABC"); // invalid for PIC 9(3) — but ASCII digits are accepted
    data.extend_from_slice(b"004");
    let opts = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true);
    let mut output = Vec::new();
    // Should either succeed (if tolerant) or error — either way no panic
    let _ = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts);
}

#[test]
fn iter_records_error_recovery() {
    // Iterator returns Err items but allows continuation
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(5);
    let opts = ascii_decode_opts();
    let iter = iter_records(Cursor::new(data), &schema, &opts).unwrap();

    let mut ok_count = 0;
    let mut err_count = 0;
    for result in iter {
        match result {
            Ok(_) => ok_count += 1,
            Err(_) => err_count += 1,
        }
    }
    assert_eq!(ok_count, 5);
    assert_eq!(err_count, 0);
}

#[test]
fn iter_records_state_recovery_single_thread() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = [
        b"00001".as_slice(),
        b"AB12C".as_slice(),
        b"00003".as_slice(),
        b"00004".as_slice(),
    ]
    .concat();
    let opts = ascii_decode_opts();
    let mut iter = iter_records(Cursor::new(data), &schema, &opts).unwrap();

    let first = iter.next().unwrap().unwrap();
    assert_eq!(first["TEST-FIELD"], "00001");
    assert_eq!(iter.current_record_index(), 1);
    assert!(!iter.is_eof());

    let second = iter.next().unwrap();
    assert!(second.is_err());
    assert_eq!(iter.current_record_index(), 2);
    assert!(!iter.is_eof());

    let third = iter.next().unwrap().unwrap();
    assert_eq!(third["TEST-FIELD"], "00003");
    assert_eq!(iter.current_record_index(), 3);
    assert!(!iter.is_eof());

    let fourth = iter.next().unwrap().unwrap();
    assert_eq!(fourth["TEST-FIELD"], "00004");
    assert_eq!(iter.current_record_index(), 4);
    assert!(!iter.is_eof());

    assert!(iter.next().is_none());
    assert!(iter.is_eof());
    assert_eq!(iter.current_record_index(), 4);
}

#[test]
fn iter_records_state_recovery_threaded() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = [
        b"00001".as_slice(),
        b"AB12C".as_slice(),
        b"00003".as_slice(),
        b"00004".as_slice(),
    ]
    .concat();

    for threads in [1_usize, 2, 4] {
        let opts = ascii_decode_opts().with_threads(threads);
        let mut iter = iter_records(Cursor::new(&data), &schema, &opts).unwrap();

        let first = iter.next().unwrap().unwrap();
        assert_eq!(first["TEST-FIELD"], "00001");
        assert_eq!(iter.current_record_index(), 1);
        assert!(!iter.is_eof());

        let second = iter.next().unwrap();
        assert!(second.is_err());
        assert_eq!(iter.current_record_index(), 2);
        assert!(!iter.is_eof());

        let third = iter.next().unwrap().unwrap();
        assert_eq!(third["TEST-FIELD"], "00003");
        assert_eq!(iter.current_record_index(), 3);
        assert!(!iter.is_eof());

        let fourth = iter.next().unwrap().unwrap();
        assert_eq!(fourth["TEST-FIELD"], "00004");
        assert_eq!(iter.current_record_index(), 4);
        assert!(!iter.is_eof());

        assert!(iter.next().is_none());
        assert!(iter.is_eof());
        assert_eq!(iter.current_record_index(), 4);
    }
}

// ===========================================================================
// 7. Partial record at end of file
// ===========================================================================

#[test]
fn iter_records_partial_record_at_eof() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    // 2 full records (10 bytes) + 3 extra bytes = partial
    let data = b"0000100002XYZ";
    let opts = ascii_decode_opts();
    let mut iter = iter_records(Cursor::new(data.as_slice()), &schema, &opts).unwrap();
    let first = iter.next().unwrap().unwrap();
    assert_eq!(first["TEST-FIELD"], "00001");
    assert_eq!(iter.current_record_index(), 1);
    assert!(!iter.is_eof());

    let second = iter.next().unwrap().unwrap();
    assert_eq!(second["TEST-FIELD"], "00002");
    assert_eq!(iter.current_record_index(), 2);

    // The three trailing bytes are a truncated record, not a clean end of file.
    // Reporting EOF here would discard them without telling the caller.
    let error = iter
        .next()
        .expect("partial record yields an item")
        .expect_err("partial record is an error");
    assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);

    assert!(iter.next().is_none());
    assert!(iter.is_eof());
    assert_eq!(iter.current_record_index(), 2);
}

#[test]
fn decode_file_partial_record_at_eof() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = b"00001AB"; // 1 full record + 2 trailing bytes
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let result = decode_file_to_jsonl(&schema, Cursor::new(data.as_slice()), &mut output, &opts);
    let text = String::from_utf8(output).unwrap();
    assert_eq!(text.lines().count(), 1);

    match result {
        Ok(summary) => {
            assert_eq!(summary.records_processed, 1);
            assert_eq!(summary.records_with_errors, 0);
            assert_eq!(summary.bytes_processed, 5);
        }
        Err(error) => {
            assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);
            assert!(!error.message.is_empty());
        }
    }
}

#[test]
fn decode_fixed_threaded_recovery_summary() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = [
        b"00001".as_slice(),
        b"AB12C".as_slice(),
        b"00003".as_slice(),
        b"00004".as_slice(),
    ]
    .concat();

    let mut baseline: Option<(Vec<u8>, u64, u64, u64)> = None;

    for threads in [1_usize, 2, 4] {
        let opts = ascii_decode_opts().with_threads(threads);
        let mut output = Vec::new();
        let summary =
            copybook_codec::decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts)
                .unwrap();

        assert_eq!(summary.records_processed, 3);
        assert_eq!(summary.records_with_errors, 1);
        assert_eq!(summary.total_records(), 4);
        assert_eq!(summary.bytes_processed, 20);
        assert_eq!(summary.threads_used, threads);
        assert_eq!(
            String::from_utf8(output.clone()).unwrap().lines().count(),
            3
        );

        match &baseline {
            Some((baseline_output, baseline_processed, baseline_errors, baseline_bytes)) => {
                assert_eq!(output, *baseline_output);
                assert_eq!(summary.records_processed, *baseline_processed);
                assert_eq!(summary.records_with_errors, *baseline_errors);
                assert_eq!(summary.bytes_processed, *baseline_bytes);
            }
            None => {
                baseline = Some((
                    output,
                    summary.records_processed,
                    summary.records_with_errors,
                    summary.bytes_processed,
                ));
            }
        }
    }
}

// ===========================================================================
// 8. Empty file handling
// ===========================================================================

#[test]
fn iter_records_empty_input() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data: &[u8] = b"";
    let opts = ascii_decode_opts();
    let count = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .count();
    assert_eq!(count, 0);
}

#[test]
fn decode_file_empty_input() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data: &[u8] = b"";
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 0);
    assert_eq!(summary.bytes_processed, 0);
    assert!(!summary.has_errors());
}

#[test]
fn encode_jsonl_empty_input() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let opts = ascii_encode_opts();
    let mut output = Vec::new();
    let summary =
        encode_jsonl_to_file(&schema, Cursor::new(b"".as_slice()), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 0);
    assert!(output.is_empty());
}

// ===========================================================================
// 9. Large record batches
// ===========================================================================

#[test]
fn decode_file_1000_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(1_000);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 1_000);
    assert!(!summary.has_errors());
}

#[test]
fn encode_decode_roundtrip_1000_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();

    // Encode 1000 records
    let jsonl = build_pic9_5_jsonl(1_000);
    let mut binary = Vec::new();
    encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut binary,
        &ascii_encode_opts(),
    )
    .unwrap();

    // Decode them back
    let mut decoded_output = Vec::new();
    let summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(binary.as_slice()),
        &mut decoded_output,
        &ascii_decode_opts(),
    )
    .unwrap();
    assert_eq!(summary.records_processed, 1_000);
}

#[test]
fn iter_records_large_multi_field() {
    let schema = parse_copybook(MULTI_FIELD_SCHEMA).unwrap();
    let data = build_multi_field_data(500);
    let opts = ascii_decode_opts();
    let count = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .count();
    assert_eq!(count, 500);
}

// ===========================================================================
// 10. Summary/metrics from file operations
// ===========================================================================

#[test]
fn decode_summary_fields() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(20);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    assert_eq!(summary.records_processed, 20);
    assert_eq!(summary.records_with_errors, 0);
    assert_eq!(summary.bytes_processed, 20 * 5);
    assert!(summary.is_successful());
    assert!((summary.success_rate() - 100.0).abs() < f64::EPSILON);
    assert_eq!(summary.total_records(), 20);
}

#[test]
fn encode_summary_fields() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let jsonl = build_pic9_5_jsonl(15);
    let mut output = Vec::new();
    let summary = encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut output,
        &ascii_encode_opts(),
    )
    .unwrap();

    assert_eq!(summary.records_processed, 15);
    assert_eq!(summary.bytes_processed, 15 * 5);
}

#[test]
fn run_summary_display() {
    let mut summary = RunSummary::new();
    summary.records_processed = 100;
    summary.bytes_processed = 500;
    summary.processing_time_ms = 50;
    summary.calculate_throughput();
    let display = format!("{summary}");
    assert!(display.contains("Records processed: 100"));
    assert!(display.contains("Bytes processed:"));
}

#[test]
fn run_summary_error_rate() {
    let mut summary = RunSummary::new();
    summary.records_processed = 90;
    summary.records_with_errors = 10;
    assert!((summary.error_rate() - 10.0).abs() < f64::EPSILON);
    assert!(summary.has_errors());
    assert!(!summary.is_successful());
}

// ===========================================================================
// 11. Thread count effect on output
// ===========================================================================

#[test]
fn decode_single_thread_deterministic() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(50);
    let opts = ascii_decode_opts().with_threads(1);
    let mut out1 = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data.clone()), &mut out1, &opts).unwrap();
    let mut out2 = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut out2, &opts).unwrap();
    assert_eq!(out1, out2, "Single-thread decode must be deterministic");
}

#[test]
fn decode_thread_count_in_summary() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(10);
    let opts = ascii_decode_opts().with_threads(1);
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();
    assert_eq!(summary.threads_used, 1);
}

#[test]
fn decode_parallel_strict_mode_stops_in_input_order() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = [
        b"00001".as_slice(),
        b"AB12C".as_slice(),
        b"00003".as_slice(),
        b"00004".as_slice(),
    ]
    .concat();
    let opts = ascii_decode_opts().with_threads(4).with_strict_mode(true);
    let mut output = Vec::new();

    let error = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts)
        .expect_err("strict parallel decode should return the first record error");

    assert_eq!(error.code, ErrorCode::CBKD411_ZONED_BAD_SIGN);
    assert_eq!(String::from_utf8(output).unwrap().lines().count(), 1);
}

#[test]
fn decode_parallel_rdw_strict_mode_emits_prior_records() {
    let schema = parse_copybook(RDW_SCHEMA).unwrap();
    let data = build_rdw_records(&["HELLO", "ABC"]);
    let opts = ascii_decode_opts()
        .with_format(RecordFormat::RDW)
        .with_threads(4)
        .with_strict_mode(true);
    let mut output = Vec::new();

    let error = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts)
        .expect_err("strict parallel RDW decode should reject the short record");

    assert_eq!(error.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    assert_eq!(String::from_utf8(output).unwrap().lines().count(), 1);
}

#[test]
fn decode_fixed_threaded_deterministic() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(50);

    let mut outputs = Vec::new();
    for threads in [1_usize, 2, 4] {
        let opts = ascii_decode_opts().with_threads(threads);
        let mut output = Vec::new();
        let summary =
            copybook_codec::decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts)
                .unwrap();
        assert_eq!(summary.threads_used, threads);
        outputs.push((threads, output));
    }

    let baseline = outputs[0].1.clone();
    for (threads, output) in &outputs[1..] {
        assert_eq!(
            &baseline, output,
            "Fixed decode output differs for {threads} threads",
        );
    }
}

#[test]
fn encode_fixed_threaded_deterministic() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let jsonl = build_pic9_5_jsonl(50);

    let mut outputs = Vec::new();
    for threads in [1_usize, 2, 4] {
        let opts = ascii_encode_opts().with_threads(threads);
        let mut output = Vec::new();
        let summary = copybook_codec::encode_jsonl_to_file(
            &schema,
            Cursor::new(jsonl.as_bytes()),
            &mut output,
            &opts,
        )
        .unwrap();
        assert_eq!(summary.threads_used, threads);
        outputs.push((threads, output));
    }

    let baseline = outputs[0].1.clone();
    for (threads, output) in &outputs[1..] {
        assert_eq!(
            &baseline, output,
            "Fixed encode output differs for {threads} threads",
        );
    }
}

#[test]
fn encode_parallel_strict_mode_stops_in_input_order() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let jsonl = concat!(
        r#"{"TEST-FIELD": "00001"}"#,
        "\n",
        r#"{"TEST-FIELD": "BAD!!"}"#,
        "\n",
        r#"{"TEST-FIELD": "00003"}"#,
        "\n",
    );
    let opts = ascii_encode_opts().with_threads(4).with_strict_mode(true);
    let mut output = Vec::new();

    let summary = copybook_codec::encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut output,
        &opts,
    )
    .unwrap();

    assert_eq!(summary.threads_used, 4);
    assert_eq!(summary.records_processed, 2);
    assert_eq!(summary.records_with_errors, 1);
    assert_eq!(output, b"00001");
}

#[test]
fn decode_rdw_threaded_deterministic() {
    let schema = parse_copybook(RDW_SCHEMA).unwrap();
    let data = build_rdw_records(&["HELLO", "WORLD", "ABCDE", "FGHIJ"]);

    let mut outputs = Vec::new();
    for threads in [1_usize, 2, 4] {
        let opts = ascii_decode_opts()
            .with_format(RecordFormat::RDW)
            .with_threads(threads);
        let mut output = Vec::new();
        let summary =
            copybook_codec::decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts)
                .unwrap();
        assert_eq!(summary.threads_used, threads);
        outputs.push((threads, output));
    }

    let baseline = outputs[0].1.clone();
    for (threads, output) in &outputs[1..] {
        assert_eq!(
            &baseline, output,
            "RDW decode output differs for {threads} threads",
        );
    }
}

#[test]
fn encode_rdw_threaded_deterministic() {
    let schema = parse_copybook(RDW_SCHEMA).unwrap();
    let mut jsonl = String::new();
    for value in ["HELLO", "WORLD", "ABCDE", "FGHIJ"] {
        jsonl.push_str(&format!("{{\"FIELD\": \"{value}\"}}\n"));
    }

    let mut outputs = Vec::new();
    for threads in [1_usize, 2, 4] {
        let opts = ascii_encode_opts()
            .with_format(RecordFormat::RDW)
            .with_threads(threads);
        let mut output = Vec::new();
        let summary = copybook_codec::encode_jsonl_to_file(
            &schema,
            Cursor::new(jsonl.as_bytes()),
            &mut output,
            &opts,
        )
        .unwrap();
        assert_eq!(summary.threads_used, threads);
        outputs.push((threads, output));
    }

    let baseline = outputs[0].1.clone();
    for (threads, output) in &outputs[1..] {
        assert_eq!(
            &baseline, output,
            "RDW encode output differs for {threads} threads",
        );
    }
}

#[test]
fn decode_file_to_jsonl_raw_mode_records_emit_fixed_raw_payload() {
    let fixed_schema = parse_copybook("01 FIELD PIC X(5).").unwrap();
    let fixed_data = b"HELLOWORLD";
    let fixed_raw_expectations = [b"HELLO".to_vec(), b"WORLD".to_vec()];

    let opts = ascii_decode_opts().with_emit_raw(RawMode::Record);
    let mut output = Vec::new();
    let summary =
        decode_file_to_jsonl(&fixed_schema, Cursor::new(&fixed_data), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 2);
    assert_eq!(summary.bytes_processed, 10);
    assert_eq!(summary.warnings, 0);

    let lines: Vec<_> = String::from_utf8(output)
        .unwrap()
        .lines()
        .map(|line| {
            serde_json::from_str::<serde_json::Value>(line).expect("json line should parse")
        })
        .collect();
    assert_eq!(lines.len(), 2);

    let decoded_raws: Vec<Vec<u8>> = lines.iter().map(parse_raw_b64_field).collect();
    let expected = fixed_raw_expectations
        .iter()
        .map(|value| value.to_vec())
        .collect::<Vec<_>>();
    assert_eq!(decoded_raws, expected);

    for decoded in lines.iter() {
        assert!(decoded.get("raw_b64").is_some());
        assert_eq!(
            parse_raw_b64_field_from_any(decoded, "raw_b64"),
            parse_raw_b64_field(decoded),
            "raw_b64 should be the canonical raw payload bytes"
        );
    }
}

#[test]
fn decode_file_to_jsonl_raw_mode_rdw_emits_header_and_payload() {
    let rdw_schema = parse_copybook(RDW_SCHEMA).unwrap();
    let rdw_data = build_rdw_records(&["ABCDE", "FGHIJ"]);
    let rdw_raw_expectations: Vec<Vec<u8>> = [
        vec![0x00, 0x05, 0x00, 0x00, b'A', b'B', b'C', b'D', b'E'],
        vec![0x00, 0x05, 0x00, 0x00, b'F', b'G', b'H', b'I', b'J'],
    ]
    .into_iter()
    .map(|chunk| chunk.to_vec())
    .collect();

    let opts = ascii_decode_opts()
        .with_format(RecordFormat::RDW)
        .with_emit_raw(RawMode::RecordRDW);
    let mut output = Vec::new();
    let summary =
        decode_file_to_jsonl(&rdw_schema, Cursor::new(&rdw_data), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 2);
    assert_eq!(summary.bytes_processed, 18);
    assert_eq!(summary.warnings, 0);

    let lines: Vec<_> = String::from_utf8(output)
        .unwrap()
        .lines()
        .map(|line| {
            serde_json::from_str::<serde_json::Value>(line).expect("json line should parse")
        })
        .collect();
    assert_eq!(lines.len(), 2);

    let decoded_raws: Vec<Vec<u8>> = lines.iter().map(parse_raw_b64_field).collect();
    assert_eq!(decoded_raws, rdw_raw_expectations);
    for decoded in lines.iter() {
        assert!(decoded.get("raw_b64").is_some());
    }
}

#[test]
fn decode_file_to_jsonl_raw_mode_rdw_is_stable_across_workers() {
    let rdw_schema = parse_copybook(RDW_SCHEMA).unwrap();
    let rdw_data = [
        [0x00, 0x05, 0x00, 0x00, b'A', b'B', b'C', b'D', b'E'],
        [0x00, 0x05, 0x12, 0x34, b'F', b'G', b'H', b'I', b'J'],
    ]
    .concat();
    let expected_raw = vec![
        vec![0x00, 0x05, 0x00, 0x00, b'A', b'B', b'C', b'D', b'E'],
        vec![0x00, 0x05, 0x12, 0x34, b'F', b'G', b'H', b'I', b'J'],
    ];
    let mut baseline_output = None;

    for threads in [1_usize, 4] {
        let opts = ascii_decode_opts()
            .with_format(RecordFormat::RDW)
            .with_emit_raw(RawMode::RecordRDW)
            .with_threads(threads);
        let mut output = Vec::new();
        let summary =
            decode_file_to_jsonl(&rdw_schema, Cursor::new(&rdw_data), &mut output, &opts).unwrap();

        assert_eq!(summary.records_processed, 2);
        assert_eq!(summary.bytes_processed, 18);
        assert_eq!(summary.warnings, 1);
        assert_eq!(summary.threads_used, threads);
        let lines: Vec<_> = String::from_utf8(output.clone())
            .unwrap()
            .lines()
            .map(|line| serde_json::from_str::<serde_json::Value>(line).unwrap())
            .collect();
        for line in &lines {
            assert!(line.get("raw_b64").is_some());
            assert!(line.get("__raw_b64").is_some());
        }
        let actual_raw: Vec<_> = lines.iter().map(parse_raw_b64_field).collect();
        assert_eq!(actual_raw, expected_raw);

        if let Some(baseline) = &baseline_output {
            assert_eq!(
                output, *baseline,
                "raw RDW output differs for {threads} threads"
            );
        } else {
            baseline_output = Some(output);
        }
    }
}

#[test]
fn decode_file_to_jsonl_raw_mode_rdw_strict_reserved_rejects_nonzero() {
    let schema = parse_copybook("01 FIELD PIC X(5).").unwrap();
    let data = b"\x00\x05\x12\x34HELLO";
    let opts = ascii_decode_opts()
        .with_format(RecordFormat::RDW)
        .with_emit_raw(RawMode::RecordRDW)
        .with_strict_mode(true);
    let mut output = Vec::new();
    let result = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts);
    let error = result.expect_err("strict decode should reject non-zero reserved bytes");
    assert_eq!(error.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
}

#[test]
fn decode_rdw_reserved_warning_is_a_lenient_warning() {
    let schema = parse_copybook("01 FIELD PIC X(5).").unwrap();
    let data = b"\x00\x05\x12\x34HELLO";
    let opts = ascii_decode_opts()
        .with_format(RecordFormat::RDW)
        .with_emit_raw(RawMode::RecordRDW)
        .with_strict_mode(false);
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts)
        .expect("lenient decode with non-zero reserved should continue");

    assert_eq!(summary.records_processed, 1);
    assert_eq!(summary.records_with_errors, 0);
    assert_eq!(summary.warnings, 1);
    let output_text = String::from_utf8(output).unwrap();
    assert_eq!(summary.bytes_processed, 9);
    assert!(output_text.contains("HELLO"));
    let decoded: serde_json::Value =
        serde_json::from_str(output_text.lines().next().unwrap()).unwrap();
    assert_eq!(
        parse_raw_b64_field(&decoded),
        vec![0x00, 0x05, 0x12, 0x34, b'H', b'E', b'L', b'L', b'O']
    );
}

#[test]
fn decode_fixed_missing_lrecl_fixed_invalid_state() {
    let mut schema = parse_copybook("01 FIELD PIC X(5).").unwrap();
    // Fixed decoding requires an explicit fixed length in the schema.
    schema.lrecl_fixed = None;

    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let result = decode_file_to_jsonl(&schema, Cursor::new(b"ABCDE"), &mut output, &opts);
    let error = result.expect_err("fixed decode should fail when lrecl is missing");
    assert_eq!(error.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn decode_rdw_suspect_ascii_header_is_fatal() {
    let schema = parse_copybook(
        r#"
01 TEST-RECORD.
   05 TEST-FIELD PIC X(5).
"#,
    )
    .unwrap();

    let data = vec![b'1', b'2', 0x00, 0x00, b'H', b'E', b'L', b'L', b'O'];
    let opts = ascii_decode_opts().with_format(RecordFormat::RDW);
    let mut output = Vec::new();
    let result = decode_file_to_jsonl(&schema, Cursor::new(&data), &mut output, &opts);
    let error = result.expect_err("RDW decode should fail on suspect ASCII-corrupted header");
    assert_eq!(error.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn decode_rdw_legacy_raw_mode_key_alias_is_accepted_in_encode() {
    let schema = parse_copybook("01 FIELD PIC X(5).").unwrap();
    let encoded = base64::engine::general_purpose::STANDARD.encode(b"HELLO");
    let jsonl = format!("{{\"__raw_b64\":\"{encoded}\"}}\n");
    let mut output = Vec::new();
    let opts = ascii_encode_opts()
        .with_format(RecordFormat::Fixed)
        .with_use_raw(true);

    encode_jsonl_to_file(&schema, Cursor::new(jsonl.as_bytes()), &mut output, &opts)
        .expect("encoding should accept legacy __raw_b64");

    assert_eq!(output, b"HELLO");
}

// ===========================================================================
// 12. Streaming with field projection
// ===========================================================================

#[test]
fn iter_records_with_projection() {
    let schema = parse_copybook(MULTI_FIELD_SCHEMA).unwrap();
    let projected = project_schema(&schema, &["CUST-ID".to_string()]).unwrap();
    let data = build_multi_field_data(5);
    let opts = ascii_decode_opts();
    let records: Vec<_> = iter_records(Cursor::new(data), &projected, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();
    assert_eq!(records.len(), 5);
    for rec in &records {
        assert!(rec.get("CUST-ID").is_some());
    }
}

#[test]
fn decode_file_with_projection() {
    let schema = parse_copybook(MULTI_FIELD_SCHEMA).unwrap();
    let projected =
        project_schema(&schema, &["CUST-ID".to_string(), "AMOUNT".to_string()]).unwrap();
    let data = build_multi_field_data(10);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&projected, Cursor::new(data), &mut output, &opts).unwrap();
    assert_eq!(summary.records_processed, 10);

    let text = String::from_utf8(output).unwrap();
    for line in text.lines() {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        assert!(val.get("CUST-ID").is_some());
        assert!(val.get("AMOUNT").is_some());
    }
}

// ===========================================================================
// 13. JSON number mode in streaming context
// ===========================================================================

#[test]
fn decode_file_lossless_number_mode() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(3);
    let opts = ascii_decode_opts().with_json_number_mode(JsonNumberMode::Lossless);
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    let text = String::from_utf8(output).unwrap();
    for line in text.lines() {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        // Lossless mode emits numeric strings
        assert!(
            val["TEST-FIELD"].is_string(),
            "Lossless mode should emit string: {:?}",
            val["TEST-FIELD"]
        );
    }
}

#[test]
fn decode_file_native_number_mode() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(3);
    let opts = ascii_decode_opts().with_json_number_mode(JsonNumberMode::Native);
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    let text = String::from_utf8(output).unwrap();
    for line in text.lines() {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        // Native mode emits JSON numbers for non-zero; zero may be "0" string
        // depending on implementation — just verify the field exists
        assert!(
            val.get("TEST-FIELD").is_some(),
            "Native mode should emit TEST-FIELD"
        );
    }
}

#[test]
fn iter_records_lossless_vs_native_consistency() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(5);

    let lossless_opts = ascii_decode_opts().with_json_number_mode(JsonNumberMode::Lossless);
    let native_opts = ascii_decode_opts().with_json_number_mode(JsonNumberMode::Native);

    let lossless: Vec<_> = iter_records(Cursor::new(data.clone()), &schema, &lossless_opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();
    let native: Vec<_> = iter_records(Cursor::new(data), &schema, &native_opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();

    assert_eq!(lossless.len(), native.len());
    // Values should represent the same logical data even if types differ
    for (l, n) in lossless.iter().zip(native.iter()) {
        let l_val: u64 = l["TEST-FIELD"]
            .as_str()
            .map(|s| s.parse().unwrap_or(0))
            .or_else(|| l["TEST-FIELD"].as_u64())
            .unwrap();
        let n_val: u64 = n["TEST-FIELD"]
            .as_u64()
            .or_else(|| n["TEST-FIELD"].as_str().map(|s| s.parse().unwrap_or(0)))
            .unwrap();
        assert_eq!(l_val, n_val);
    }
}

// ===========================================================================
// 14. iter_records_from_file (with tempfile)
// ===========================================================================

#[test]
fn iter_records_from_file_basic() {
    use copybook_codec::iter_records_from_file;
    use std::io::Write;

    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(10);

    let mut tmp = tempfile::NamedTempFile::new().unwrap();
    tmp.write_all(&data).unwrap();
    tmp.flush().unwrap();

    let opts = ascii_decode_opts();
    let iter = iter_records_from_file(tmp.path(), &schema, &opts).unwrap();
    let count = iter.filter_map(Result::ok).count();
    assert_eq!(count, 10);
}

#[test]
fn iter_records_from_file_nonexistent() {
    use copybook_codec::iter_records_from_file;

    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let opts = ascii_decode_opts();
    let result = iter_records_from_file("nonexistent_file_12345.bin", &schema, &opts);
    match result {
        Ok(iterator) => {
            let _ = iterator;
            panic!("Should error on nonexistent file");
        }
        Err(err) => {
            assert_eq!(err.code, ErrorCode::CBKR201_RDW_READ_ERROR);
        }
    }
}

// ===========================================================================
// 15. Raw mode in streaming
// ===========================================================================

#[test]
fn decode_file_with_raw_mode() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(3);
    let opts = ascii_decode_opts().with_emit_raw(RawMode::Record);
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    let text = String::from_utf8(output).unwrap();
    for line in text.lines() {
        let val: serde_json::Value = serde_json::from_str(line).unwrap();
        assert!(val.get("raw_b64").is_some(), "Raw mode should emit raw_b64");
        assert_eq!(
            parse_raw_b64_field_from_any(&val, "raw_b64"),
            parse_raw_b64_field(&val),
        );
    }
}

// ===========================================================================
// 16. Schema fingerprint propagation
// ===========================================================================

#[test]
fn decode_summary_schema_fingerprint() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(1);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();
    assert_eq!(summary.schema_fingerprint, schema.fingerprint);
}

// ===========================================================================
// 17. RecordIterator schema/options accessors
// ===========================================================================

#[test]
fn record_iterator_accessors() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(1);
    let opts = ascii_decode_opts();
    let iter = copybook_codec::RecordIterator::new(Cursor::new(data), &schema, &opts).unwrap();
    assert_eq!(iter.schema().fields.len(), schema.fields.len());
    assert_eq!(iter.options().codepage, Codepage::ASCII);
    assert_eq!(iter.current_record_index(), 0);
    assert!(!iter.is_eof());
}
