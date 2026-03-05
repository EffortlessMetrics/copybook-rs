// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep integration tests for the copybook-rdw crate covering header construction,
//! byte layout, reserved bytes, min/max lengths, parsing, reader/writer,
//! round-trip, and edge cases.

#![allow(clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_rdw::{
    RDW_HEADER_LEN, RDW_MAX_PAYLOAD_LEN, RDWRecord, RDWRecordReader, RDWRecordWriter, RdwHeader,
    rdw_is_suspect_ascii_corruption, rdw_payload_len_to_u16, rdw_read_len, rdw_slice_body,
    rdw_try_peek_len, rdw_validate_and_finish,
};
use std::io::Cursor;

// ====================================================================
// 1. RDW header construction
// ====================================================================

#[test]
fn header_from_payload_len_zero() {
    let header = RdwHeader::from_payload_len(0, 0).unwrap();
    assert_eq!(header.length(), 0);
    assert_eq!(header.reserved(), 0);
    assert_eq!(header.bytes(), [0, 0, 0, 0]);
}

#[test]
fn header_from_payload_len_small() {
    let header = RdwHeader::from_payload_len(10, 0).unwrap();
    assert_eq!(header.length(), 10);
    assert_eq!(header.reserved(), 0);
}

#[test]
fn header_from_payload_len_max() {
    let header = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN, 0).unwrap();
    assert_eq!(header.length(), u16::MAX);
}

#[test]
fn header_from_payload_len_with_reserved() {
    let header = RdwHeader::from_payload_len(100, 0xABCD).unwrap();
    assert_eq!(header.length(), 100);
    assert_eq!(header.reserved(), 0xABCD);
}

#[test]
fn header_from_payload_len_oversize_fails() {
    let err = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN + 1, 0).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

// ====================================================================
// 2. Header byte layout (big-endian length)
// ====================================================================

#[test]
fn header_bytes_are_big_endian_length() {
    let header = RdwHeader::from_payload_len(256, 0).unwrap();
    // 256 = 0x0100 big-endian
    assert_eq!(header.bytes()[0], 0x01);
    assert_eq!(header.bytes()[1], 0x00);
}

#[test]
fn header_bytes_big_endian_4096() {
    let header = RdwHeader::from_bytes([0x10, 0x00, 0x00, 0x00]);
    assert_eq!(header.length(), 4096);
}

#[test]
fn header_bytes_big_endian_max() {
    let header = RdwHeader::from_bytes([0xFF, 0xFF, 0x00, 0x00]);
    assert_eq!(header.length(), 65535);
}

#[test]
fn header_from_bytes_roundtrip() {
    let original = [0x12, 0x34, 0x56, 0x78];
    let header = RdwHeader::from_bytes(original);
    assert_eq!(header.bytes(), original);
    assert_eq!(header.length(), 0x1234);
    assert_eq!(header.reserved(), 0x5678);
}

// ====================================================================
// 3. Reserved bytes handling
// ====================================================================

#[test]
fn reserved_bytes_zero_default() {
    let header = RdwHeader::from_payload_len(10, 0).unwrap();
    assert_eq!(header.reserved(), 0);
    assert_eq!(header.bytes()[2], 0);
    assert_eq!(header.bytes()[3], 0);
}

#[test]
fn reserved_bytes_preserved() {
    let header = RdwHeader::from_payload_len(10, 0xDEAD).unwrap();
    assert_eq!(header.reserved(), 0xDEAD);
    assert_eq!(header.bytes()[2], 0xDE);
    assert_eq!(header.bytes()[3], 0xAD);
}

#[test]
fn reserved_bytes_max_value() {
    let header = RdwHeader::from_payload_len(0, 0xFFFF).unwrap();
    assert_eq!(header.reserved(), 0xFFFF);
}

// ====================================================================
// 4. Minimum/maximum record lengths
// ====================================================================

#[test]
fn rdw_header_len_is_four() {
    assert_eq!(RDW_HEADER_LEN, 4);
}

#[test]
fn rdw_max_payload_len_is_u16_max() {
    assert_eq!(RDW_MAX_PAYLOAD_LEN, 65535);
}

#[test]
fn minimum_valid_record_is_empty_payload() {
    let record = RDWRecord::try_new(Vec::new()).unwrap();
    assert_eq!(record.length(), 0);
    assert!(record.payload.is_empty());
    assert_eq!(record.as_bytes().len(), RDW_HEADER_LEN);
}

#[test]
fn maximum_valid_record_is_u16_max_payload() {
    let payload = vec![0xABu8; RDW_MAX_PAYLOAD_LEN];
    let record = RDWRecord::try_new(payload).unwrap();
    assert_eq!(record.length(), u16::MAX);
    assert_eq!(record.payload.len(), RDW_MAX_PAYLOAD_LEN);
}

// ====================================================================
// 5. Header parsing from bytes
// ====================================================================

#[test]
fn rdw_read_len_parses_two_bytes_big_endian() {
    let mut cur = Cursor::new(vec![0x00, 0x05, 0xAA, 0xBB]);
    let len = rdw_read_len(&mut cur).unwrap();
    assert_eq!(len, 5);
}

#[test]
fn rdw_read_len_incomplete_is_error() {
    let mut cur = Cursor::new(vec![0x00]);
    let err = rdw_read_len(&mut cur).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn rdw_read_len_empty_is_error() {
    let mut cur = Cursor::new(Vec::<u8>::new());
    let err = rdw_read_len(&mut cur).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn rdw_try_peek_len_empty_returns_none() {
    let mut cur = Cursor::new(Vec::<u8>::new());
    assert!(rdw_try_peek_len(&mut cur).unwrap().is_none());
}

#[test]
fn rdw_try_peek_len_one_byte_returns_none() {
    let mut cur = Cursor::new(vec![0x00]);
    assert!(rdw_try_peek_len(&mut cur).unwrap().is_none());
}

#[test]
fn rdw_try_peek_len_two_bytes_returns_some() {
    let mut cur = Cursor::new(vec![0x00, 0x05]);
    assert!(rdw_try_peek_len(&mut cur).unwrap().is_some());
}

// ====================================================================
// 6. Invalid header detection (ASCII corruption heuristic)
// ====================================================================

#[test]
fn ascii_corruption_detected_for_digit_bytes() {
    assert!(rdw_is_suspect_ascii_corruption([b'1', b'2', 0, 0]));
    assert!(rdw_is_suspect_ascii_corruption([b'0', b'0', 0, 0]));
    assert!(rdw_is_suspect_ascii_corruption([b'9', b'9', b'3', b'4']));
}

#[test]
fn ascii_corruption_not_detected_for_binary_bytes() {
    assert!(!rdw_is_suspect_ascii_corruption([0x00, 0x0A, 0x00, 0x00]));
    assert!(!rdw_is_suspect_ascii_corruption([0xFF, 0xFF, 0x00, 0x00]));
}

#[test]
fn ascii_corruption_requires_both_length_bytes_to_be_digits() {
    assert!(!rdw_is_suspect_ascii_corruption([b'1', 0x00, 0x00, 0x00]));
    assert!(!rdw_is_suspect_ascii_corruption([0x00, b'2', 0x00, 0x00]));
}

#[test]
fn rdw_header_looks_ascii_corrupt_matches_function() {
    let header = RdwHeader::from_bytes([b'5', b'5', 0x00, 0x00]);
    assert!(header.looks_ascii_corrupt());

    let header = RdwHeader::from_bytes([0x00, 0x0A, 0x00, 0x00]);
    assert!(!header.looks_ascii_corrupt());
}

// ====================================================================
// 7. RDW reader (multi-record)
// ====================================================================

#[test]
fn reader_reads_single_record() {
    let data = vec![0, 5, 0, 0, b'h', b'e', b'l', b'l', b'o'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, b"hello");
    assert_eq!(record.length(), 5);
    assert_eq!(reader.record_count(), 1);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn reader_reads_multiple_records() {
    let data = vec![
        0, 2, 0, 0, b'h', b'i', //
        0, 3, 0, 0, b'b', b'y', b'e',
    ];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let first = reader.read_record().unwrap().unwrap();
    assert_eq!(first.payload, b"hi");

    let second = reader.read_record().unwrap().unwrap();
    assert_eq!(second.payload, b"bye");

    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 2);
}

#[test]
fn reader_reads_zero_length_record() {
    let data = vec![0, 0, 0, 0];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.length(), 0);
    assert!(record.payload.is_empty());
    assert_eq!(reader.record_count(), 1);
}

#[test]
fn reader_empty_file_returns_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn reader_reserved_nonzero_lenient_is_warning() {
    let data = vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.reserved(), 0x1234);
    assert_eq!(record.payload, b"test");
}

#[test]
fn reader_reserved_nonzero_strict_is_error() {
    let data = vec![0, 4, 0x12, 0x34, b't', b'e', b's', b't'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
}

#[test]
fn reader_incomplete_header_lenient_is_eof() {
    let data = vec![0, 4];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn reader_incomplete_header_strict_is_underflow() {
    let data = vec![0, 4];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn reader_incomplete_payload_is_error() {
    let data = vec![0, 10, 0, 0, b'h', b'i'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn reader_ascii_corruption_detected() {
    let data = vec![b'1', b'2', 0, 0, b'A', b'B', b'C'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn reader_single_byte_lenient_is_eof() {
    let data = vec![0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn reader_single_byte_strict_is_underflow() {
    let data = vec![0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn reader_three_byte_header_lenient_is_eof() {
    let data = vec![0x00, 0x05, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn reader_three_byte_header_strict_is_underflow() {
    let data = vec![0x00, 0x05, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// ====================================================================
// 8. RDW writer (with headers)
// ====================================================================

#[test]
fn writer_writes_single_record() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    let record = RDWRecord::try_new(b"test".to_vec()).unwrap();
    writer.write_record(&record).unwrap();
    assert_eq!(writer.record_count(), 1);
    assert_eq!(output, vec![0, 4, 0, 0, b't', b'e', b's', b't']);
}

#[test]
fn writer_writes_record_from_payload() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.write_record_from_payload(b"ABC", None).unwrap();
    assert_eq!(writer.record_count(), 1);
    assert_eq!(output, vec![0, 3, 0, 0, b'A', b'B', b'C']);
}

#[test]
fn writer_writes_record_from_payload_with_reserved() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer
        .write_record_from_payload(b"XY", Some(0x1234))
        .unwrap();
    assert_eq!(output, vec![0, 2, 0x12, 0x34, b'X', b'Y']);
}

#[test]
fn writer_writes_multiple_records() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    for i in 0u8..5 {
        writer.write_record_from_payload(&[i], None).unwrap();
    }
    assert_eq!(writer.record_count(), 5);
    // Each record: 4 header + 1 payload = 5 bytes
    assert_eq!(output.len(), 25);
}

#[test]
fn writer_flush_succeeds() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 0);
}

#[test]
fn writer_oversize_payload_is_error() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    let large = vec![0u8; RDW_MAX_PAYLOAD_LEN + 1];
    let err = writer.write_record_from_payload(&large, None).unwrap_err();
    // Error code for oversized payload from writer
    assert!(
        err.code == ErrorCode::CBKE501_JSON_TYPE_MISMATCH
            || err.code == ErrorCode::CBKF102_RECORD_LENGTH_INVALID
    );
}

#[test]
fn writer_empty_payload() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.write_record_from_payload(b"", None).unwrap();
    assert_eq!(output, vec![0, 0, 0, 0]);
}

// ====================================================================
// 9. Read/write roundtrip
// ====================================================================

#[test]
fn roundtrip_single_record() {
    let payload = b"hello world";
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, payload);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_multiple_records() {
    let payloads: Vec<&[u8]> = vec![b"alpha", b"", b"gamma delta", b"x"];
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for p in &payloads {
            writer.write_record_from_payload(p, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for expected in &payloads {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.payload.as_slice(), *expected);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 4);
}

#[test]
fn roundtrip_with_reserved_bytes() {
    let payload = b"reserved test";
    let reserved = 0xBEEF_u16;
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer
            .write_record_from_payload(payload, Some(reserved))
            .unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, payload);
    assert_eq!(record.reserved(), reserved);
}

#[test]
fn roundtrip_large_record() {
    let payload = vec![0xCDu8; 8192];
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload.len(), 8192);
    assert!(record.payload.iter().all(|&b| b == 0xCD));
}

#[test]
fn roundtrip_many_small_records() {
    let record_count = 200;
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for i in 0..record_count {
            let payload = format!("R{i:04}");
            writer
                .write_record_from_payload(payload.as_bytes(), None)
                .unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let mut count = 0u64;
    while let Some(record) = reader.read_record().unwrap() {
        let expected = format!("R{count:04}");
        assert_eq!(record.payload, expected.as_bytes());
        count += 1;
    }
    assert_eq!(count, record_count);
}

// ====================================================================
// 10. Edge cases
// ====================================================================

#[test]
fn edge_case_minimum_record_as_bytes() {
    let record = RDWRecord::try_new(Vec::new()).unwrap();
    let bytes = record.as_bytes();
    assert_eq!(bytes.len(), 4); // 4-byte header only
    assert_eq!(bytes, vec![0, 0, 0, 0]);
}

#[test]
fn edge_case_max_record_length_header() {
    let payload = vec![0u8; RDW_MAX_PAYLOAD_LEN];
    let record = RDWRecord::try_new(payload).unwrap();
    assert_eq!(record.length(), u16::MAX); // 65535
    assert_eq!(
        record.as_bytes().len(),
        RDW_HEADER_LEN + RDW_MAX_PAYLOAD_LEN
    );
}

#[test]
fn edge_case_rdw_record_try_new_oversize_fails() {
    let err = RDWRecord::try_new(vec![0u8; RDW_MAX_PAYLOAD_LEN + 1]).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn edge_case_rdw_record_try_with_reserved_oversize_fails() {
    let err = RDWRecord::try_with_reserved(vec![0u8; RDW_MAX_PAYLOAD_LEN + 1], 0x1234).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn edge_case_try_recompute_length_after_payload_change() {
    let mut record = RDWRecord::try_new(b"short".to_vec()).unwrap();
    assert_eq!(record.length(), 5);

    record.payload = b"longer_payload_here".to_vec();
    record.try_recompute_length().unwrap();
    assert_eq!(record.length(), 19);
}

#[test]
fn edge_case_try_recompute_length_oversize_fails() {
    let mut record = RDWRecord::try_new(b"ok".to_vec()).unwrap();
    record.payload = vec![0u8; RDW_MAX_PAYLOAD_LEN + 1];
    let err = record.try_recompute_length().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn edge_case_rdw_payload_len_to_u16_boundary_values() {
    assert_eq!(rdw_payload_len_to_u16(0).unwrap(), 0);
    assert_eq!(rdw_payload_len_to_u16(1).unwrap(), 1);
    assert_eq!(rdw_payload_len_to_u16(65535).unwrap(), 65535);
    assert!(rdw_payload_len_to_u16(65536).is_err());
}

#[test]
fn edge_case_rdw_slice_body_zero_length() {
    let mut cur = Cursor::new(vec![0xAA, 0xBB]);
    let body = rdw_slice_body(&mut cur, 0).unwrap();
    assert!(body.is_empty());
}

#[test]
fn edge_case_rdw_slice_body_insufficient_data() {
    let mut cur = Cursor::new(vec![0xAA]);
    let err = rdw_slice_body(&mut cur, 5).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn edge_case_rdw_validate_and_finish_is_identity() {
    let data = b"unchanged";
    assert_eq!(rdw_validate_and_finish(data), data);
}

#[test]
fn edge_case_rdw_record_as_bytes_structure() {
    let record = RDWRecord::try_with_reserved(b"AB".to_vec(), 0x0102).unwrap();
    let bytes = record.as_bytes();
    assert_eq!(bytes.len(), 6); // 4 header + 2 payload
    assert_eq!(&bytes[0..2], &[0, 2]); // length = 2
    assert_eq!(&bytes[2..4], &[0x01, 0x02]); // reserved
    assert_eq!(&bytes[4..6], b"AB"); // payload
}

#[test]
fn edge_case_rdw_record_clone_independence() {
    let original = RDWRecord::try_new(b"clone".to_vec()).unwrap();
    let mut cloned = original.clone();
    cloned.payload = b"modified".to_vec();
    cloned.try_recompute_length().unwrap();

    assert_eq!(original.payload, b"clone");
    assert_eq!(original.length(), 5);
    assert_eq!(cloned.payload, b"modified");
    assert_eq!(cloned.length(), 8);
}

#[test]
fn edge_case_rdw_record_debug_format() {
    let record = RDWRecord::try_new(b"dbg".to_vec()).unwrap();
    let debug = format!("{record:?}");
    assert!(debug.contains("RDWRecord"));
}

#[test]
fn edge_case_reader_max_record_size() {
    let payload = vec![0xABu8; u16::MAX as usize];
    let mut data = Vec::with_capacity(RDW_HEADER_LEN + payload.len());
    data.extend_from_slice(&[0xFF, 0xFF, 0x00, 0x00]);
    data.extend_from_slice(&payload);

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.length(), u16::MAX);
    assert_eq!(record.payload.len(), u16::MAX as usize);
}

#[test]
fn edge_case_reader_strict_empty_is_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), true);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}
