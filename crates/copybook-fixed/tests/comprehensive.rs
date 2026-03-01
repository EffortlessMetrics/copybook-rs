// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-fixed.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

// ====================================================================
// 1. Reading fixed-length records
// ====================================================================

#[test]
fn read_multiple_records() {
    let data = b"AAAABBBBCCCC";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(4)).unwrap();

    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1, b"AAAA");
    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2, b"BBBB");
    let r3 = reader.read_record().unwrap().unwrap();
    assert_eq!(r3, b"CCCC");
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 3);
}

#[test]
fn read_single_record() {
    let data = b"HELLO";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(5)).unwrap();
    let r = reader.read_record().unwrap().unwrap();
    assert_eq!(r, b"HELLO");
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 1);
}

#[test]
fn read_binary_data() {
    let data: Vec<u8> = (0x00..=0xFF).collect();
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(256)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.len(), 256);
    assert_eq!(record, data);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn read_record_with_lrecl_1() {
    let data = b"XYZ";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(1)).unwrap();
    assert_eq!(reader.read_record().unwrap().unwrap(), vec![b'X']);
    assert_eq!(reader.read_record().unwrap().unwrap(), vec![b'Y']);
    assert_eq!(reader.read_record().unwrap().unwrap(), vec![b'Z']);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 3);
}

#[test]
fn read_preserves_record_count() {
    let data = vec![0u8; 100];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(10)).unwrap();
    for _ in 0..10 {
        reader.read_record().unwrap().unwrap();
    }
    assert_eq!(reader.record_count(), 10);
}

// ====================================================================
// 2. Writing fixed-length records
// ====================================================================

#[test]
fn write_exact_length_no_padding() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(5)).unwrap();
    writer.write_record(b"HELLO").unwrap();
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 1);
    drop(writer);
    assert_eq!(output, b"HELLO");
}

#[test]
fn write_shorter_than_lrecl_pads_with_null() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(8)).unwrap();
    writer.write_record(b"AB").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, b"AB\x00\x00\x00\x00\x00\x00");
}

#[test]
fn write_empty_record_full_padding() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    writer.write_record(b"").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0u8; 4]);
}

#[test]
fn write_multiple_records_with_mixed_lengths() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(6)).unwrap();
    writer.write_record(b"ABCDEF").unwrap(); // exact
    writer.write_record(b"XY").unwrap(); // padded
    writer.write_record(b"").unwrap(); // all padding
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 3);
    drop(writer);

    assert_eq!(output.len(), 18); // 3 × 6
    assert_eq!(&output[0..6], b"ABCDEF");
    assert_eq!(&output[6..8], b"XY");
    assert_eq!(&output[8..12], &[0u8; 4]);
    assert_eq!(&output[12..18], &[0u8; 6]);
}

#[test]
fn write_binary_data_preserved() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    writer.write_record(&[0xFF, 0x00, 0xAB, 0xCD]).unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0xFF, 0x00, 0xAB, 0xCD]);
}

// ====================================================================
// 3. Record length validation
// ====================================================================

#[test]
fn writer_rejects_record_longer_than_lrecl() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    let err = writer.write_record(b"TOOLONG").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn writer_rejects_record_one_byte_over_lrecl() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    let err = writer.write_record(b"ABCDE").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn reader_zero_lrecl_rejected() {
    let err = FixedRecordReader::new(Cursor::new(b"test".as_slice()), Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn reader_none_lrecl_rejected() {
    let err = FixedRecordReader::new(Cursor::new(b"test".as_slice()), None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_zero_lrecl_rejected() {
    let mut output = Vec::new();
    let err = FixedRecordWriter::new(&mut output, Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_none_lrecl_rejected() {
    let mut output = Vec::new();
    let err = FixedRecordWriter::new(&mut output, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn lrecl_accessors() {
    let reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(42)).unwrap();
    assert_eq!(reader.lrecl(), 42);

    let mut output = Vec::new();
    let writer = FixedRecordWriter::new(&mut output, Some(99)).unwrap();
    assert_eq!(writer.lrecl(), 99);
}

// ====================================================================
// 4. Data that doesn't divide evenly by record length
// ====================================================================

#[test]
fn partial_record_at_eof_is_underflow() {
    // 7 bytes, LRECL=4: first record OK, second is partial (3 bytes)
    let data = b"ABCDEFG";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(4)).unwrap();
    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1, b"ABCD");
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn partial_record_single_byte_remainder() {
    // 5 bytes, LRECL=4: one record + 1 straggler
    let data = b"ABCDE";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(4)).unwrap();
    reader.read_record().unwrap().unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn partial_record_one_less_than_lrecl() {
    // LRECL=100, data=99 bytes → partial
    let data = vec![0x41u8; 99];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(100)).unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// ====================================================================
// 5. Empty input
// ====================================================================

#[test]
fn empty_input_returns_none_immediately() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(8)).unwrap();
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn empty_input_repeated_reads_still_none() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(4)).unwrap();
    for _ in 0..5 {
        assert!(reader.read_record().unwrap().is_none());
    }
    assert_eq!(reader.record_count(), 0);
}

// ====================================================================
// 6. Write/Read round-trip
// ====================================================================

#[test]
fn write_read_roundtrip_multiple_records() {
    let lrecl = 10u32;
    let payloads: &[&[u8]] = &[b"AAAAAAAAAA", b"BB", b"CCCCCC", b""];
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for p in payloads {
            writer.write_record(p).unwrap();
        }
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), payloads.len() * lrecl as usize);

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    for (i, &payload) in payloads.iter().enumerate() {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(
            &record[..payload.len()],
            payload,
            "record {i}: data mismatch"
        );
        // Remaining bytes should be zero-padded
        for (j, &b) in record[payload.len()..].iter().enumerate() {
            assert_eq!(b, 0, "record {i} padding byte {j}");
        }
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn write_read_roundtrip_large_lrecl() {
    let lrecl = 1000u32;
    let payload = vec![0x42u8; 500]; // Half-filled record
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), 1000);
    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(&record[..500], payload.as_slice());
    assert!(record[500..].iter().all(|&b| b == 0));
}

// ====================================================================
// 7. Streaming many records
// ====================================================================

#[test]
fn streaming_1000_records() {
    let lrecl = 20u32;
    let count = 1000u64;
    let payload = b"STREAM_TEST_RECORD__";

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for _ in 0..count {
            writer.write_record(payload).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), count);
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let mut read_count = 0u64;
    while let Some(record) = reader.read_record().unwrap() {
        assert_eq!(record.as_slice(), payload.as_slice());
        read_count += 1;
    }
    assert_eq!(read_count, count);
    assert_eq!(reader.record_count(), count);
}

// ====================================================================
// 8. Writer does not corrupt after rejection
// ====================================================================

#[test]
fn writer_continues_after_rejected_oversize_record() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();

    // This should fail
    assert!(writer.write_record(b"TOOLONG").is_err());
    assert_eq!(writer.record_count(), 0);

    // Should still be usable
    writer.write_record(b"OK").unwrap();
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 1);
    drop(writer);
    assert_eq!(output.len(), 4);
    assert_eq!(&output[..2], b"OK");
}
