// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

//! Large-file streaming tests for copybook-codec.
//!
//! Validates processing of 1000+ records, memory-bounded iteration,
//! content correctness, scratch buffer integration, and streaming
//! processor patterns with the codec APIs.

use copybook_codec::memory::{ScratchBuffers, StreamingProcessor};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl,
    decode_record, decode_record_with_scratch, encode_jsonl_to_file, iter_records,
};
use copybook_core::parse_copybook;
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

const SIMPLE_SCHEMA: &str = "01 TEST-FIELD PIC 9(5).";

const MULTI_FIELD_SCHEMA: &str = r"
    01 RECORD.
       05 CUST-ID    PIC 9(5).
       05 CUST-NAME  PIC X(10).
       05 AMOUNT     PIC 9(5).
";

/// Build `n` fixed-length ASCII records for PIC 9(5).
fn build_pic9_5_data(n: usize) -> Vec<u8> {
    let mut data = Vec::with_capacity(n * 5);
    for i in 0..n {
        data.extend_from_slice(format!("{:05}", i % 100_000).as_bytes());
    }
    data
}

/// Build `n` multi-field records (20 bytes each).
fn build_multi_field_data(n: usize) -> Vec<u8> {
    let mut data = Vec::with_capacity(n * 20);
    for i in 0..n {
        data.extend_from_slice(format!("{:05}", i % 100_000).as_bytes());
        data.extend_from_slice(format!("{:<10}", format!("NAME{i}")).as_bytes());
        data.extend_from_slice(format!("{:05}", (i * 100) % 100_000).as_bytes());
    }
    data
}

/// Build JSONL input for PIC 9(5) schema.
fn build_pic9_5_jsonl(n: usize) -> String {
    let mut s = String::new();
    for i in 0..n {
        s.push_str(&format!(r#"{{"TEST-FIELD": "{:05}"}}"#, i % 100_000));
        s.push('\n');
    }
    s
}

// ===========================================================================
// 1. Process 2000 records via iter_records
// ===========================================================================

#[test]
fn iter_records_2000_records_count_correct() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(2_000);
    let opts = ascii_decode_opts();
    let count = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .count();
    assert_eq!(count, 2_000);
}

// ===========================================================================
// 2. First and last record content correct
// ===========================================================================

#[test]
fn iter_records_first_and_last_content() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let n = 1_500;
    let data = build_pic9_5_data(n);
    let opts = ascii_decode_opts();
    let records: Vec<_> = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();

    assert_eq!(records.len(), n);

    // First record: value 0 (lossless may normalize to "0")
    let first_val: u64 = records[0]["TEST-FIELD"].as_str().unwrap().parse().unwrap();
    assert_eq!(first_val, 0);

    // Last record: (n-1) % 100_000
    let last_val: u64 = records[n - 1]["TEST-FIELD"]
        .as_str()
        .unwrap()
        .parse()
        .unwrap();
    assert_eq!(last_val, (n as u64 - 1) % 100_000);
}

// ===========================================================================
// 3. Memory stays bounded during streaming (iterator holds one buffer)
// ===========================================================================

#[test]
fn iter_records_memory_bounded_1000_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(1_000);
    let opts = ascii_decode_opts();
    let mut iter = copybook_codec::RecordIterator::new(Cursor::new(data), &schema, &opts).unwrap();

    // Process one record at a time — iterator doesn't buffer entire file
    let first = iter.next().unwrap().unwrap();
    assert!(first.get("TEST-FIELD").is_some());
    assert_eq!(iter.current_record_index(), 1);
    assert!(!iter.is_eof());

    // Process remaining without collecting them all
    let mut count = 1_u64;
    for result in iter {
        result.unwrap();
        count += 1;
    }
    assert_eq!(count, 1_000);
}

// ===========================================================================
// 4. decode_file_to_jsonl handles 2000 records
// ===========================================================================

#[test]
fn decode_file_2000_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(2_000);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    assert_eq!(summary.records_processed, 2_000);
    assert!(!summary.has_errors());

    let text = String::from_utf8(output).unwrap();
    assert_eq!(text.lines().count(), 2_000);
}

// ===========================================================================
// 5. Decode with scratch buffers vs without produces identical output
// ===========================================================================

#[test]
fn scratch_vs_plain_decode_identical_over_100_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let opts = ascii_decode_opts();
    let mut scratch = ScratchBuffers::new();

    for i in 0..100_u32 {
        let data = format!("{:05}", i % 100_000);
        let plain = decode_record(&schema, data.as_bytes(), &opts).unwrap();
        let with_scratch =
            decode_record_with_scratch(&schema, data.as_bytes(), &opts, &mut scratch).unwrap();

        assert_eq!(
            plain, with_scratch,
            "Mismatch at record {i}: plain={plain}, scratch={with_scratch}"
        );
        scratch.clear();
    }
}

// ===========================================================================
// 6. Multi-field large streaming
// ===========================================================================

#[test]
fn iter_records_multi_field_1000() {
    let schema = parse_copybook(MULTI_FIELD_SCHEMA).unwrap();
    let data = build_multi_field_data(1_000);
    let opts = ascii_decode_opts();
    let records: Vec<_> = iter_records(Cursor::new(data), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();

    assert_eq!(records.len(), 1_000);

    // Verify first record fields (lossless may normalize leading zeros)
    let id0: u64 = records[0]["CUST-ID"].as_str().unwrap().parse().unwrap();
    assert_eq!(id0, 0);
    let name0 = records[0]["CUST-NAME"].as_str().unwrap();
    assert!(name0.starts_with("NAME0"));

    // Verify last record
    let last_id: u64 = records[999]["CUST-ID"].as_str().unwrap().parse().unwrap();
    assert_eq!(last_id, 999);
}

// ===========================================================================
// 7. StreamingProcessor tracks memory over 1000+ records
// ===========================================================================

#[test]
fn streaming_processor_tracks_1500_records() {
    let mut processor = StreamingProcessor::with_default_limit();
    let record_size = 128_usize;

    for _ in 0..1_500 {
        processor.update_memory_usage(record_size as isize);
        processor.record_processed(record_size);
        processor.update_memory_usage(-(record_size as isize));
    }

    let stats = processor.stats();
    assert_eq!(stats.records_processed, 1_500);
    assert_eq!(stats.bytes_processed, 1_500 * 128);
    assert_eq!(stats.current_memory_bytes, 0);
}

// ===========================================================================
// 8. Encode+decode roundtrip 1500 records
// ===========================================================================

#[test]
fn encode_decode_roundtrip_1500_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();

    // Encode
    let jsonl = build_pic9_5_jsonl(1_500);
    let mut binary = Vec::new();
    let enc_summary = encode_jsonl_to_file(
        &schema,
        Cursor::new(jsonl.as_bytes()),
        &mut binary,
        &ascii_encode_opts(),
    )
    .unwrap();
    assert_eq!(enc_summary.records_processed, 1_500);

    // Decode back
    let mut decoded_output = Vec::new();
    let dec_summary = decode_file_to_jsonl(
        &schema,
        Cursor::new(binary.as_slice()),
        &mut decoded_output,
        &ascii_decode_opts(),
    )
    .unwrap();
    assert_eq!(dec_summary.records_processed, 1_500);

    // Verify line count
    let text = String::from_utf8(decoded_output).unwrap();
    assert_eq!(text.lines().count(), 1_500);
}

// ===========================================================================
// 9. Record indices sequential in large batch
// ===========================================================================

#[test]
fn record_indices_sequential_2000() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(2_000);
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
            "record_index should be 1-based sequential at line {i}"
        );
    }
}

// ===========================================================================
// 10. Streaming processor detects pressure during accumulation
// ===========================================================================

#[test]
fn streaming_processor_pressure_during_large_batch() {
    let mut processor = StreamingProcessor::new(1); // 1 MiB limit
    let mut max_before_pressure = 0_usize;

    for i in 0..200_usize {
        let chunk = 10_000; // 10 KB
        processor.update_memory_usage(chunk as isize);

        if processor.is_memory_pressure() {
            max_before_pressure = i;
            break;
        }
    }

    // Pressure should trigger within ~84 chunks (840 KB > 80% of 1 MiB)
    assert!(
        max_before_pressure > 0 && max_before_pressure < 100,
        "Pressure triggered at chunk {max_before_pressure}"
    );
}

// ===========================================================================
// 11. iter_records + manual scratch buffer decode pattern
// ===========================================================================

#[test]
fn manual_scratch_decode_matches_iter_records() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let n = 200;
    let data = build_pic9_5_data(n);
    let opts = ascii_decode_opts();

    // Collect via iter_records
    let iter_results: Vec<_> = iter_records(Cursor::new(data.clone()), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();

    // Manual decode with scratch
    let mut scratch = ScratchBuffers::new();
    let record_len = 5;
    let mut manual_results = Vec::with_capacity(n);
    for i in 0..n {
        let start = i * record_len;
        let end = start + record_len;
        let record_data = &data[start..end];
        let json = decode_record_with_scratch(&schema, record_data, &opts, &mut scratch).unwrap();
        manual_results.push(json);
        scratch.clear();
    }

    // Compare TEST-FIELD values (envelope may differ due to record_index)
    assert_eq!(iter_results.len(), manual_results.len());
    for (i, (iter_val, manual_val)) in iter_results.iter().zip(manual_results.iter()).enumerate() {
        assert_eq!(
            iter_val["TEST-FIELD"], manual_val["TEST-FIELD"],
            "Mismatch at record {i}"
        );
    }
}

// ===========================================================================
// 12. Scratch buffer no growth after warmup during streaming
// ===========================================================================

#[test]
fn scratch_buffer_no_growth_during_streaming_decode() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let opts = ascii_decode_opts();
    let mut scratch = ScratchBuffers::new();

    // Warmup: 10 records
    for i in 0..10_u32 {
        let data = format!("{:05}", i);
        let _ = decode_record_with_scratch(&schema, data.as_bytes(), &opts, &mut scratch);
        scratch.clear();
    }

    let byte_cap = scratch.byte_buffer.capacity();
    let str_cap = scratch.string_buffer.capacity();

    // Steady-state: 990 more records
    for i in 10..1_000_u32 {
        let data = format!("{:05}", i % 100_000);
        let _ = decode_record_with_scratch(&schema, data.as_bytes(), &opts, &mut scratch);
        scratch.clear();
    }

    assert_eq!(
        scratch.byte_buffer.capacity(),
        byte_cap,
        "byte_buffer grew after warmup during streaming"
    );
    assert_eq!(
        scratch.string_buffer.capacity(),
        str_cap,
        "string_buffer grew after warmup during streaming"
    );
}

// ===========================================================================
// 13. Large multi-field decode with scratch vs plain parity
// ===========================================================================

#[test]
fn multi_field_scratch_vs_plain_parity() {
    let schema = parse_copybook(MULTI_FIELD_SCHEMA).unwrap();
    let opts = ascii_decode_opts();
    let mut scratch = ScratchBuffers::new();

    let data = build_multi_field_data(50);
    let record_len = 20;

    for i in 0..50 {
        let start = i * record_len;
        let end = start + record_len;
        let record_data = &data[start..end];

        let plain = decode_record(&schema, record_data, &opts).unwrap();
        let with_scratch =
            decode_record_with_scratch(&schema, record_data, &opts, &mut scratch).unwrap();

        assert_eq!(
            plain["CUST-ID"], with_scratch["CUST-ID"],
            "CUST-ID mismatch at record {i}"
        );
        assert_eq!(
            plain["CUST-NAME"], with_scratch["CUST-NAME"],
            "CUST-NAME mismatch at record {i}"
        );
        assert_eq!(
            plain["AMOUNT"], with_scratch["AMOUNT"],
            "AMOUNT mismatch at record {i}"
        );
        scratch.clear();
    }
}

// ===========================================================================
// 14. decode_file_to_jsonl content spot checks on 1000 records
// ===========================================================================

#[test]
fn decode_file_content_spot_checks_1000() {
    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(1_000);
    let opts = ascii_decode_opts();
    let mut output = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(data), &mut output, &opts).unwrap();

    let text = String::from_utf8(output).unwrap();
    let lines: Vec<&str> = text.lines().collect();
    assert_eq!(lines.len(), 1_000);

    // Spot check records 0, 499, 999 (lossless may normalize leading zeros)
    let r0: serde_json::Value = serde_json::from_str(lines[0]).unwrap();
    let v0: u64 = r0["TEST-FIELD"].as_str().unwrap().parse().unwrap();
    assert_eq!(v0, 0);

    let r499: serde_json::Value = serde_json::from_str(lines[499]).unwrap();
    let v499: u64 = r499["TEST-FIELD"].as_str().unwrap().parse().unwrap();
    assert_eq!(v499, 499);

    let r999: serde_json::Value = serde_json::from_str(lines[999]).unwrap();
    let v999: u64 = r999["TEST-FIELD"].as_str().unwrap().parse().unwrap();
    assert_eq!(v999, 999);
}

// ===========================================================================
// 15. iter_records_from_file with tempfile for 1000+ records
// ===========================================================================

#[test]
fn iter_records_from_file_1000_records() {
    use copybook_codec::iter_records_from_file;
    use std::io::Write;

    let schema = parse_copybook(SIMPLE_SCHEMA).unwrap();
    let data = build_pic9_5_data(1_000);

    let mut tmp = tempfile::NamedTempFile::new().unwrap();
    tmp.write_all(&data).unwrap();
    tmp.flush().unwrap();

    let opts = ascii_decode_opts();
    let records: Vec<_> = iter_records_from_file(tmp.path(), &schema, &opts)
        .unwrap()
        .filter_map(Result::ok)
        .collect();

    assert_eq!(records.len(), 1_000);

    // Verify first and last (lossless normalizes leading zeros)
    let v0: u64 = records[0]["TEST-FIELD"].as_str().unwrap().parse().unwrap();
    assert_eq!(v0, 0);
    let v999: u64 = records[999]["TEST-FIELD"]
        .as_str()
        .unwrap()
        .parse()
        .unwrap();
    assert_eq!(v999, 999);
}
