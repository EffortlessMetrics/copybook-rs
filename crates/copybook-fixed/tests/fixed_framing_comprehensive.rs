// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for fixed-length record framing.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_error::ErrorCode;
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

// ---------------------------------------------------------------------------
// Reader: exact LRECL
// ---------------------------------------------------------------------------

#[test]
fn read_exact_lrecl_single_record() {
    let data = vec![0xAAu8; 80];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(80)).unwrap();
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.len(), 80);
    assert!(rec.iter().all(|&b| b == 0xAA));
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn read_exact_lrecl_two_records() {
    let mut data = vec![0x01u8; 80];
    data.extend_from_slice(&[0x02u8; 80]);
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(80)).unwrap();

    let r1 = reader.read_record().unwrap().unwrap();
    assert!(r1.iter().all(|&b| b == 0x01));
    let r2 = reader.read_record().unwrap().unwrap();
    assert!(r2.iter().all(|&b| b == 0x02));
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 2);
}

#[test]
fn read_exact_lrecl_ten_records() {
    let lrecl = 40u32;
    let count = 10usize;
    let data: Vec<u8> = (0..count)
        .flat_map(|i| vec![i as u8; lrecl as usize])
        .collect();
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(lrecl)).unwrap();

    for i in 0..count {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.len(), lrecl as usize);
        assert!(
            rec.iter().all(|&b| b == i as u8),
            "record {i} content mismatch"
        );
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), count as u64);
}

// ---------------------------------------------------------------------------
// Reader: file not exact multiple of LRECL (error)
// ---------------------------------------------------------------------------

#[test]
fn read_partial_record_at_end_is_underflow() {
    let data = vec![0xBBu8; 83]; // 80 + 3 leftover
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(80)).unwrap();
    let _ = reader.read_record().unwrap().unwrap(); // first 80 bytes ok
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn read_short_by_one_byte_is_underflow() {
    let data = vec![0xCCu8; 79]; // need 80
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(80)).unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn read_one_extra_byte_is_underflow() {
    let data = vec![0xDDu8; 161]; // two records = 160, one extra
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(80)).unwrap();
    reader.read_record().unwrap().unwrap();
    reader.read_record().unwrap().unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// ---------------------------------------------------------------------------
// Reader: empty file handling
// ---------------------------------------------------------------------------

#[test]
fn read_empty_file_returns_none() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(80)).unwrap();
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn read_empty_file_twice_returns_none_both_times() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(10)).unwrap();
    assert!(reader.read_record().unwrap().is_none());
    assert!(reader.read_record().unwrap().is_none());
}

// ---------------------------------------------------------------------------
// Reader: single-record file
// ---------------------------------------------------------------------------

#[test]
fn read_single_record_file_exact() {
    let data = b"HELLO WORLD COBOL RECORD DATA 1234567890".to_vec();
    let lrecl = data.len() as u32;
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(lrecl)).unwrap();
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec, data);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 1);
}

#[test]
fn read_single_byte_record() {
    let data = vec![0x42u8];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(1)).unwrap();
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec, vec![0x42]);
    assert!(reader.read_record().unwrap().is_none());
}

// ---------------------------------------------------------------------------
// Reader: large LRECL (10000+ bytes)
// ---------------------------------------------------------------------------

#[test]
fn read_large_lrecl_10000() {
    let lrecl = 10_000u32;
    let data: Vec<u8> = (0..lrecl).map(|i| (i % 256) as u8).collect();
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(lrecl)).unwrap();
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.len(), 10_000);
    assert_eq!(rec, data);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn read_large_lrecl_32768_multiple_records() {
    let lrecl = 32_768u32;
    let record_count = 3;
    let data: Vec<u8> = (0..record_count)
        .flat_map(|i| vec![i as u8; lrecl as usize])
        .collect();
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(lrecl)).unwrap();
    for i in 0..record_count {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.len(), lrecl as usize);
        assert!(rec.iter().all(|&b| b == i as u8));
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn read_large_lrecl_50000() {
    let lrecl = 50_000u32;
    let data = vec![0xEEu8; lrecl as usize];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(lrecl)).unwrap();
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.len(), 50_000);
    assert!(reader.read_record().unwrap().is_none());
}

// ---------------------------------------------------------------------------
// Writer: padding
// ---------------------------------------------------------------------------

#[test]
fn write_shorter_data_pads_with_zero() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(10)).unwrap();
    writer.write_record(b"ABC").unwrap();
    writer.flush().unwrap();
    assert_eq!(output.len(), 10);
    assert_eq!(&output[..3], b"ABC");
    assert!(output[3..].iter().all(|&b| b == 0));
}

#[test]
fn write_empty_data_full_padding() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(5)).unwrap();
    writer.write_record(b"").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0u8; 5]);
}

#[test]
fn write_exact_data_no_padding() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    writer.write_record(b"ABCD").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, b"ABCD");
}

#[test]
fn write_oversize_data_is_error() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    let err = writer.write_record(b"ABCDE").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn write_one_byte_short_pads_single_zero() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(8)).unwrap();
    writer.write_record(b"ABCDEFG").unwrap();
    writer.flush().unwrap();
    assert_eq!(&output[..7], b"ABCDEFG");
    assert_eq!(output[7], 0);
}

// ---------------------------------------------------------------------------
// Write/read roundtrip
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_single_record() {
    let lrecl = 20u32;
    let payload = b"COBOL-RECORD-DATA!!";
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(payload).unwrap();
        writer.flush().unwrap();
    }
    assert_eq!(encoded.len(), lrecl as usize);

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let decoded = reader.read_record().unwrap().unwrap();
    assert_eq!(&decoded[..payload.len()], payload.as_slice());
    assert!(decoded[payload.len()..].iter().all(|&b| b == 0));
}

#[test]
fn roundtrip_multiple_records_varying_content() {
    let lrecl = 16u32;
    let payloads: Vec<&[u8]> = vec![b"AAAA", b"BBBBBBBBBBBBBBBB", b"CC", b""];
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for p in &payloads {
            writer.write_record(p).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), 4);
    }
    assert_eq!(encoded.len(), 64);

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    for (i, expected) in payloads.iter().enumerate() {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(
            &rec[..expected.len()],
            *expected,
            "record {i} data mismatch"
        );
        assert!(
            rec[expected.len()..].iter().all(|&b| b == 0),
            "record {i} padding should be zero"
        );
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_large_lrecl() {
    let lrecl = 10_000u32;
    let payload: Vec<u8> = (0..5000).map(|i| (i % 251) as u8).collect();
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let decoded = reader.read_record().unwrap().unwrap();
    assert_eq!(&decoded[..payload.len()], payload.as_slice());
}

// ---------------------------------------------------------------------------
// Flush semantics
// ---------------------------------------------------------------------------

#[test]
fn flush_on_empty_writer_succeeds() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(10)).unwrap();
    writer.flush().unwrap();
    assert!(output.is_empty());
}

#[test]
fn flush_after_write_preserves_data() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    writer.write_record(b"AB").unwrap();
    writer.flush().unwrap();
    assert_eq!(output.len(), 4);
    assert_eq!(&output[..2], b"AB");
}

#[test]
fn double_flush_is_idempotent() {
    let mut output = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
        writer.write_record(b"XY").unwrap();
        writer.flush().unwrap();
    }
    let snapshot = output.clone();
    {
        // Re-borrow for a second flush (writer is dropped, so just verify data unchanged)
        let mut writer2 = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
        writer2.flush().unwrap();
    }
    // The original data should still be there; second flush wrote nothing new
    assert_eq!(&output[..4], &snapshot[..]);
}

// ---------------------------------------------------------------------------
// Constructor validation
// ---------------------------------------------------------------------------

#[test]
fn reader_none_lrecl_is_invalid_state() {
    let err = FixedRecordReader::new(Cursor::new(b"test".to_vec()), None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn reader_zero_lrecl_is_invalid_state() {
    let err = FixedRecordReader::new(Cursor::new(b"test".to_vec()), Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_none_lrecl_is_invalid_state() {
    let mut out = Vec::new();
    let err = FixedRecordWriter::new(&mut out, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_zero_lrecl_is_invalid_state() {
    let mut out = Vec::new();
    let err = FixedRecordWriter::new(&mut out, Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

// ---------------------------------------------------------------------------
// Accessor methods
// ---------------------------------------------------------------------------

#[test]
fn reader_lrecl_accessor_returns_configured_value() {
    let reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(256)).unwrap();
    assert_eq!(reader.lrecl(), 256);
}

#[test]
fn writer_lrecl_accessor_returns_configured_value() {
    let mut out = Vec::new();
    let writer = FixedRecordWriter::new(&mut out, Some(256)).unwrap();
    assert_eq!(writer.lrecl(), 256);
}

#[test]
fn reader_record_count_starts_at_zero() {
    let reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(10)).unwrap();
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn writer_record_count_starts_at_zero() {
    let mut out = Vec::new();
    let writer = FixedRecordWriter::new(&mut out, Some(10)).unwrap();
    assert_eq!(writer.record_count(), 0);
}

// ---------------------------------------------------------------------------
// Binary data fidelity
// ---------------------------------------------------------------------------

#[test]
fn binary_data_preserved_through_roundtrip() {
    let lrecl = 256u32;
    let payload: Vec<u8> = (0u8..=255).collect();
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let decoded = reader.read_record().unwrap().unwrap();
    assert_eq!(decoded, payload);
}
