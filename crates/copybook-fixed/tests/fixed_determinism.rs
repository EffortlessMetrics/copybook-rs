// SPDX-License-Identifier: AGPL-3.0-or-later
//! Determinism tests for fixed-length record framing.
//!
//! These tests prove that `FixedRecordReader` and `FixedRecordWriter`
//! produce identical outputs given identical inputs, regardless of
//! iteration count or record content.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Read all records from a byte slice using the given LRECL.
fn read_all_records(data: &[u8], lrecl: u32) -> Vec<Vec<u8>> {
    let mut reader = FixedRecordReader::new(Cursor::new(data), Some(lrecl)).unwrap();
    let mut records = Vec::new();
    while let Some(record) = reader.read_record().unwrap() {
        records.push(record);
    }
    records
}

/// Write records through FixedRecordWriter and return the raw output.
fn write_records(records: &[&[u8]], lrecl: u32) -> Vec<u8> {
    let mut buf = Vec::new();
    let mut writer = FixedRecordWriter::new(Cursor::new(&mut buf), Some(lrecl)).unwrap();
    for record in records {
        writer.write_record(record).unwrap();
    }
    writer.flush().unwrap();
    buf
}

// =========================================================================
// 1. Fixed framing: same data + LRECL → identical record splits
// =========================================================================

#[test]
fn read_same_data_same_lrecl_identical_splits() {
    let data: Vec<u8> = (0..60).collect();
    let lrecl = 10;

    let first = read_all_records(&data, lrecl);
    for i in 0..50 {
        let result = read_all_records(&data, lrecl);
        assert_eq!(result, first, "Record split diverged on iteration {i}");
    }
}

#[test]
fn read_single_byte_lrecl_deterministic() {
    let data = vec![0xAA, 0xBB, 0xCC, 0xDD, 0xEE];
    let first = read_all_records(&data, 1);
    assert_eq!(first.len(), 5);

    for _ in 0..50 {
        let result = read_all_records(&data, 1);
        assert_eq!(result, first);
    }
}

#[test]
fn read_large_lrecl_deterministic() {
    let data: Vec<u8> = (0..=255).cycle().take(1024).collect();
    let lrecl = 256;

    let first = read_all_records(&data, lrecl);
    assert_eq!(first.len(), 4);

    for _ in 0..20 {
        let result = read_all_records(&data, lrecl);
        assert_eq!(result, first, "Large LRECL split diverged");
    }
}

// =========================================================================
// 2. Padding: same short record → identical padded output
// =========================================================================

#[test]
fn write_short_record_padding_deterministic() {
    let short_data = b"ABC";
    let lrecl = 10u32;

    let first = write_records(&[short_data.as_slice()], lrecl);
    assert_eq!(first.len(), 10, "Padded output should be LRECL bytes");
    assert_eq!(&first[..3], b"ABC");
    assert!(
        first[3..].iter().all(|&b| b == 0),
        "Padding should be null bytes"
    );

    for i in 0..50 {
        let result = write_records(&[short_data.as_slice()], lrecl);
        assert_eq!(result, first, "Padded output diverged on iteration {i}");
    }
}

#[test]
fn write_empty_record_padding_deterministic() {
    let empty: &[u8] = b"";
    let lrecl = 8u32;

    let first = write_records(&[empty], lrecl);
    assert_eq!(first.len(), 8);
    assert!(
        first.iter().all(|&b| b == 0),
        "All-padding record should be null bytes"
    );

    for _ in 0..50 {
        let result = write_records(&[empty], lrecl);
        assert_eq!(result, first);
    }
}

#[test]
fn write_exact_length_no_padding_deterministic() {
    let data = b"ABCDEFGHIJ";
    let lrecl = 10u32;

    let first = write_records(&[data.as_slice()], lrecl);
    assert_eq!(first, data.as_slice());

    for _ in 0..50 {
        let result = write_records(&[data.as_slice()], lrecl);
        assert_eq!(result, first, "Exact-length write diverged");
    }
}

// =========================================================================
// 3. Multi-record files: same file → identical record count and content
// =========================================================================

#[test]
fn multi_record_count_deterministic() {
    let data: Vec<u8> = vec![0x41; 100]; // 10 records of 10 bytes
    let lrecl = 10;

    for _ in 0..50 {
        let records = read_all_records(&data, lrecl);
        assert_eq!(records.len(), 10, "Record count should always be 10");
    }
}

#[test]
fn multi_record_content_deterministic() {
    // 5 records of 4 bytes each, each with distinct content
    let mut data = Vec::new();
    for i in 0u8..5 {
        data.extend_from_slice(&[i, i + 10, i + 20, i + 30]);
    }
    let lrecl = 4;

    let first = read_all_records(&data, lrecl);
    assert_eq!(first.len(), 5);

    for i in 0..50 {
        let result = read_all_records(&data, lrecl);
        assert_eq!(
            result, first,
            "Multi-record content diverged on iteration {i}"
        );
    }
}

#[test]
fn multi_record_write_deterministic() {
    let records: Vec<&[u8]> = vec![b"AAA", b"BBB", b"CCC", b"DDD"];
    let lrecl = 5u32;

    let first = write_records(&records, lrecl);
    assert_eq!(first.len(), 20); // 4 records × 5 bytes

    for i in 0..50 {
        let result = write_records(&records, lrecl);
        assert_eq!(
            result, first,
            "Multi-record write diverged on iteration {i}"
        );
    }
}

// =========================================================================
// 4. Round-trip: write then read produces identical content
// =========================================================================

#[test]
fn round_trip_write_read_deterministic() {
    let original_records: Vec<&[u8]> = vec![b"HELLO", b"WORLD", b"TESTS"];
    let lrecl = 8u32;

    for _ in 0..20 {
        let written = write_records(&original_records, lrecl);
        let read_back = read_all_records(&written, lrecl);

        assert_eq!(read_back.len(), 3);
        for (j, (original, read)) in original_records.iter().zip(read_back.iter()).enumerate() {
            assert_eq!(
                &read[..original.len()],
                *original,
                "Record {j} data mismatch after round-trip"
            );
            assert!(
                read[original.len()..].iter().all(|&b| b == 0),
                "Record {j} padding mismatch after round-trip"
            );
        }
    }
}

#[test]
fn round_trip_exact_length_records() {
    let records: Vec<Vec<u8>> = (0..5).map(|i| vec![i; 10]).collect();
    let refs: Vec<&[u8]> = records.iter().map(|r| r.as_slice()).collect();
    let lrecl = 10u32;

    let written = write_records(&refs, lrecl);
    let read_back = read_all_records(&written, lrecl);

    assert_eq!(read_back.len(), 5);
    for (i, (original, read)) in records.iter().zip(read_back.iter()).enumerate() {
        assert_eq!(original, read, "Round-trip mismatch for record {i}");
    }
}

// =========================================================================
// 5. Byte-content preservation
// =========================================================================

#[test]
fn all_byte_values_preserved_through_read() {
    let data: Vec<u8> = (0..=255).collect();
    let records = read_all_records(&data, 256);
    assert_eq!(records.len(), 1);
    assert_eq!(records[0], data);
}

#[test]
fn binary_data_preserved_through_write_read() {
    // EBCDIC-like bytes that might be confused with control chars
    let binary_record: Vec<u8> = vec![0x00, 0x01, 0x0A, 0x0D, 0x40, 0xC1, 0xF0, 0xFF, 0x7F, 0x80];
    let lrecl = 10u32;

    let written = write_records(&[binary_record.as_slice()], lrecl);
    let read_back = read_all_records(&written, lrecl);
    assert_eq!(read_back.len(), 1);
    assert_eq!(read_back[0], binary_record);
}

// =========================================================================
// 6. Record count accessor consistency
// =========================================================================

#[test]
fn record_count_increments_correctly() {
    let data: Vec<u8> = vec![0x42; 35]; // 7 records of 5 bytes
    let lrecl = 5u32;

    for _ in 0..20 {
        let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(lrecl)).unwrap();
        for expected in 1..=7 {
            reader.read_record().unwrap().unwrap();
            assert_eq!(reader.record_count(), expected);
        }
        assert!(reader.read_record().unwrap().is_none());
        assert_eq!(reader.record_count(), 7);
    }
}

#[test]
fn writer_record_count_increments_correctly() {
    let lrecl = 10u32;

    for _ in 0..20 {
        let mut buf = Vec::new();
        let mut writer = FixedRecordWriter::new(Cursor::new(&mut buf), Some(lrecl)).unwrap();
        for expected in 1..=5u64 {
            writer.write_record(b"test").unwrap();
            assert_eq!(writer.record_count(), expected);
        }
    }
}
