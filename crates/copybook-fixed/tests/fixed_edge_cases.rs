// SPDX-License-Identifier: AGPL-3.0-or-later
//! Edge-case tests for fixed-length record framing.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

// ====================================================================
// 1. Read zero-length input → empty iterator
// ====================================================================

#[test]
fn read_zero_length_input_returns_none() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(10)).unwrap();
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn read_zero_length_input_repeated_returns_none() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(80)).unwrap();
    for _ in 0..10 {
        assert!(reader.read_record().unwrap().is_none());
    }
    assert_eq!(reader.record_count(), 0);
}

// ====================================================================
// 2. Read input shorter than LRECL → error (not panic)
// ====================================================================

#[test]
fn read_input_shorter_than_lrecl_returns_error() {
    let data = b"SHORT";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(100)).unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn read_single_byte_input_with_large_lrecl_returns_error() {
    let data = vec![0x42u8];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(1000)).unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// ====================================================================
// 3. Read input exactly 1 LRECL → single record
// ====================================================================

#[test]
fn read_input_exactly_one_lrecl() {
    let data = b"EXACTRECORD_";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(12)).unwrap();

    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record, b"EXACTRECORD_");
    assert_eq!(reader.record_count(), 1);

    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn read_input_exactly_one_lrecl_binary() {
    let data: Vec<u8> = (0..128).collect();
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(128)).unwrap();

    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record, data);
    assert_eq!(reader.record_count(), 1);

    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 4. Read input not evenly divisible by LRECL → error on last partial
// ====================================================================

#[test]
fn read_input_not_divisible_by_lrecl_errors_on_partial() {
    // 13 bytes, LRECL=5: 2 full records (10 bytes) + 3 leftover
    let data = b"AAAAABBBBBCCC";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(5)).unwrap();

    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1, b"AAAAA");

    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2, b"BBBBB");

    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn read_input_one_extra_byte_errors_on_partial() {
    // 11 bytes, LRECL=10: 1 full record + 1 straggler
    let data = vec![0x41u8; 11];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(10)).unwrap();

    reader.read_record().unwrap().unwrap();

    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// ====================================================================
// 5. Read 1000 records → verify count and last record content
// ====================================================================

#[test]
fn read_1000_records_verify_count_and_last_content() {
    let lrecl = 16u32;
    let count = 1000u64;

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for i in 0..count {
            let mut payload = vec![0u8; lrecl as usize];
            // Stamp 8-byte record index into each record
            let idx_bytes = i.to_be_bytes();
            payload[..8].copy_from_slice(&idx_bytes);
            writer.write_record(&payload).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let mut last_record = Vec::new();
    let mut read_count = 0u64;
    while let Some(record) = reader.read_record().unwrap() {
        last_record = record;
        read_count += 1;
    }

    assert_eq!(read_count, count);
    assert_eq!(reader.record_count(), count);

    // Verify the last record contains the index 999
    let last_idx = u64::from_be_bytes(last_record[..8].try_into().unwrap());
    assert_eq!(last_idx, 999);
}

// ====================================================================
// 6. Write records and read back → verify identity
// ====================================================================

#[test]
fn write_read_roundtrip_identity() {
    let lrecl = 20u32;
    let payloads: &[&[u8]] = &[
        b"FIRST_RECORD________",
        b"SECOND_RECORD_______",
        b"THIRD_RECORD________",
    ];

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for &p in payloads {
            writer.write_record(p).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    for &expected in payloads {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.as_slice(), expected);
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn write_read_roundtrip_with_padding_identity() {
    let lrecl = 10u32;
    let payloads: &[&[u8]] = &[b"AB", b"CDEFGHIJ", b""];

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for &p in payloads {
            writer.write_record(p).unwrap();
        }
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), 30); // 3 × 10

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    for &expected in payloads {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(&record[..expected.len()], expected);
        assert!(record[expected.len()..].iter().all(|&b| b == 0));
    }
    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 7. LRECL of 1 byte → works correctly
// ====================================================================

#[test]
fn lrecl_one_byte_read() {
    let data = vec![0xAA, 0xBB, 0xCC, 0xDD, 0xEE];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(1)).unwrap();

    for &expected in &data {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record, vec![expected]);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 5);
}

#[test]
fn lrecl_one_byte_write_read_roundtrip() {
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(1)).unwrap();
        for b in 0..=255u8 {
            writer.write_record(&[b]).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), 256);
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(1)).unwrap();
    for expected in 0..=255u8 {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record, vec![expected]);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 256);
}

#[test]
fn lrecl_one_byte_empty_payload_pads() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(1)).unwrap();
    writer.write_record(b"").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0x00]);
}

// ====================================================================
// 8. LRECL of 32767 (max reasonable) → works correctly
// ====================================================================

#[test]
fn lrecl_32767_write_and_read() {
    let lrecl = 32_767u32;
    let payload = vec![0xABu8; lrecl as usize];

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), lrecl as usize);

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.len(), lrecl as usize);
    assert!(record.iter().all(|&b| b == 0xAB));
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn lrecl_32767_partial_payload_pads() {
    let lrecl = 32_767u32;
    let payload = vec![0xFFu8; 100];

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), lrecl as usize);

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(&record[..100], payload.as_slice());
    assert!(record[100..].iter().all(|&b| b == 0));
}

// ====================================================================
// 9. Write empty record list → empty output
// ====================================================================

#[test]
fn write_empty_record_list_produces_empty_output() {
    let mut output = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut output, Some(80)).unwrap();
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), 0);
    }
    assert!(output.is_empty());
}

#[test]
fn write_empty_record_list_then_read_returns_none() {
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(80)).unwrap();
        // Write nothing
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(80)).unwrap();
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}
