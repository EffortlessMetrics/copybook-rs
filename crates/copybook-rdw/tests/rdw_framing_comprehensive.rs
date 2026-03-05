// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for RDW (Record Descriptor Word) framing.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_error::ErrorCode;
use copybook_rdw::{
    RDW_HEADER_LEN, RDW_MAX_PAYLOAD_LEN, RDWRecord, RDWRecordReader, RDWRecordWriter, RdwHeader,
};
use std::io::Cursor;

// ---------------------------------------------------------------------------
// RDW header parsing (big-endian length)
// ---------------------------------------------------------------------------

#[test]
fn header_zero_length() {
    let hdr = RdwHeader::from_bytes([0x00, 0x00, 0x00, 0x00]);
    assert_eq!(hdr.length(), 0);
    assert_eq!(hdr.reserved(), 0);
}

#[test]
fn header_small_length_big_endian() {
    let hdr = RdwHeader::from_bytes([0x00, 0x0A, 0x00, 0x00]);
    assert_eq!(hdr.length(), 10);
}

#[test]
fn header_256_big_endian() {
    let hdr = RdwHeader::from_bytes([0x01, 0x00, 0x00, 0x00]);
    assert_eq!(hdr.length(), 256);
}

#[test]
fn header_1000_big_endian() {
    // 1000 = 0x03E8
    let hdr = RdwHeader::from_bytes([0x03, 0xE8, 0x00, 0x00]);
    assert_eq!(hdr.length(), 1000);
}

#[test]
fn header_max_length_big_endian() {
    let hdr = RdwHeader::from_bytes([0xFF, 0xFF, 0x00, 0x00]);
    assert_eq!(hdr.length(), u16::MAX);
}

#[test]
fn header_from_payload_len_encodes_big_endian() {
    let hdr = RdwHeader::from_payload_len(300, 0).unwrap();
    // 300 = 0x012C
    assert_eq!(hdr.bytes()[0], 0x01);
    assert_eq!(hdr.bytes()[1], 0x2C);
    assert_eq!(hdr.length(), 300);
}

// ---------------------------------------------------------------------------
// Valid RDW with reserved bytes
// ---------------------------------------------------------------------------

#[test]
fn header_reserved_bytes_preserved() {
    let hdr = RdwHeader::from_bytes([0x00, 0x10, 0xDE, 0xAD]);
    assert_eq!(hdr.length(), 16);
    assert_eq!(hdr.reserved(), 0xDEAD);
}

#[test]
fn header_from_payload_len_with_reserved() {
    let hdr = RdwHeader::from_payload_len(42, 0xBEEF).unwrap();
    assert_eq!(hdr.length(), 42);
    assert_eq!(hdr.reserved(), 0xBEEF);
    assert_eq!(hdr.bytes()[2], 0xBE);
    assert_eq!(hdr.bytes()[3], 0xEF);
}

#[test]
fn header_reserved_ffff() {
    let hdr = RdwHeader::from_payload_len(1, 0xFFFF).unwrap();
    assert_eq!(hdr.reserved(), 0xFFFF);
}

// ---------------------------------------------------------------------------
// Invalid RDW (length too small, length > max)
// ---------------------------------------------------------------------------

#[test]
fn header_payload_too_large_is_error() {
    let err = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN + 1, 0).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn header_payload_way_too_large_is_error() {
    let err = RdwHeader::from_payload_len(1_000_000, 0).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn reader_incomplete_header_3_bytes_lenient_is_eof() {
    let data = vec![0x00, 0x0A, 0x00]; // only 3 bytes, need 4
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn reader_incomplete_header_3_bytes_strict_is_underflow() {
    let data = vec![0x00, 0x0A, 0x00]; // only 3 bytes, need 4
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn reader_incomplete_payload_is_error() {
    // Header says 10 bytes payload but only 5 available
    let mut data = vec![0x00, 0x0A, 0x00, 0x00];
    data.extend_from_slice(&[0xAA; 5]);
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn reader_ascii_corrupted_header_detected() {
    // Both length bytes are ASCII digits
    let data = vec![b'9', b'9', 0x00, 0x00, 0x41, 0x42];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

// ---------------------------------------------------------------------------
// Variable-length record reading
// ---------------------------------------------------------------------------

#[test]
fn read_single_variable_record() {
    let mut data = Vec::new();
    // 7-byte payload
    data.extend_from_slice(&[0x00, 0x07, 0x00, 0x00]);
    data.extend_from_slice(b"ABCDEFG");
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, b"ABCDEFG");
    assert_eq!(rec.length(), 7);
}

#[test]
fn read_variable_record_preserves_binary() {
    let payload: Vec<u8> = (0u8..=255).collect();
    let mut data = Vec::new();
    data.extend_from_slice(&[0x01, 0x00, 0x00, 0x00]); // 256 bytes
    data.extend_from_slice(&payload);
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, payload);
}

// ---------------------------------------------------------------------------
// Multiple records with different sizes
// ---------------------------------------------------------------------------

#[test]
fn read_multiple_records_different_sizes() {
    let mut data = Vec::new();

    // Record 1: 3 bytes
    data.extend_from_slice(&[0x00, 0x03, 0x00, 0x00]);
    data.extend_from_slice(b"ABC");

    // Record 2: 1 byte
    data.extend_from_slice(&[0x00, 0x01, 0x00, 0x00]);
    data.push(b'X');

    // Record 3: 10 bytes
    data.extend_from_slice(&[0x00, 0x0A, 0x00, 0x00]);
    data.extend_from_slice(b"0123456789");

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1.payload, b"ABC");
    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2.payload, b"X");
    let r3 = reader.read_record().unwrap().unwrap();
    assert_eq!(r3.payload, b"0123456789");
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 3);
}

#[test]
fn read_five_records_increasing_size() {
    let mut data = Vec::new();
    let sizes = [1u16, 5, 10, 50, 100];
    for &sz in &sizes {
        let len_bytes = sz.to_be_bytes();
        data.extend_from_slice(&[len_bytes[0], len_bytes[1], 0x00, 0x00]);
        data.extend_from_slice(&vec![0xBB; sz as usize]);
    }

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    for &sz in &sizes {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.payload.len(), sz as usize);
    }
    assert!(reader.read_record().unwrap().is_none());
}

// ---------------------------------------------------------------------------
// Empty record (header only, length=0)
// ---------------------------------------------------------------------------

#[test]
fn read_zero_length_record() {
    let data = vec![0x00, 0x00, 0x00, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.length(), 0);
    assert!(rec.payload.is_empty());
    assert_eq!(reader.record_count(), 1);
}

#[test]
fn read_two_zero_length_records() {
    let data = vec![0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let r1 = reader.read_record().unwrap().unwrap();
    assert!(r1.payload.is_empty());
    let r2 = reader.read_record().unwrap().unwrap();
    assert!(r2.payload.is_empty());
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn read_zero_length_then_normal_record() {
    let mut data = Vec::new();
    // Zero-length record
    data.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
    // Normal 4-byte record
    data.extend_from_slice(&[0x00, 0x04, 0x00, 0x00]);
    data.extend_from_slice(b"TEST");

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let r1 = reader.read_record().unwrap().unwrap();
    assert!(r1.payload.is_empty());
    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2.payload, b"TEST");
}

// ---------------------------------------------------------------------------
// Maximum record size
// ---------------------------------------------------------------------------

#[test]
fn read_max_record_size() {
    let payload = vec![0xCC; RDW_MAX_PAYLOAD_LEN];
    let mut data = Vec::with_capacity(RDW_HEADER_LEN + payload.len());
    data.extend_from_slice(&[0xFF, 0xFF, 0x00, 0x00]);
    data.extend_from_slice(&payload);

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload.len(), RDW_MAX_PAYLOAD_LEN);
    assert_eq!(rec.length(), u16::MAX);
}

#[test]
fn write_max_record_size() {
    let payload = vec![0xDD; RDW_MAX_PAYLOAD_LEN];
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.write_record_from_payload(&payload, None).unwrap();
    writer.flush().unwrap();
    assert_eq!(output.len(), RDW_HEADER_LEN + RDW_MAX_PAYLOAD_LEN);
    assert_eq!(output[0], 0xFF);
    assert_eq!(output[1], 0xFF);
}

#[test]
fn write_oversize_payload_is_error() {
    let payload = vec![0xEE; RDW_MAX_PAYLOAD_LEN + 1];
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    let err = writer
        .write_record_from_payload(&payload, None)
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ---------------------------------------------------------------------------
// Write/read RDW roundtrip
// ---------------------------------------------------------------------------

#[test]
fn roundtrip_single_record() {
    let payload = b"HELLO_RDW_WORLD";
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, payload);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_with_reserved_bytes() {
    let payload = b"RESERVED_TEST";
    let reserved = 0xABCD;
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer
            .write_record_from_payload(payload, Some(reserved))
            .unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, payload);
    assert_eq!(rec.reserved(), reserved);
}

#[test]
fn roundtrip_multiple_records() {
    let payloads: Vec<&[u8]> = vec![b"alpha", b"bravo charlie", b"d", b"echo foxtrot golf"];
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for p in &payloads {
            writer.write_record_from_payload(p, None).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), payloads.len() as u64);
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for expected in &payloads {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.payload.as_slice(), *expected);
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_empty_payload() {
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(b"", None).unwrap();
        writer.flush().unwrap();
    }
    assert_eq!(encoded.len(), RDW_HEADER_LEN);

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert!(rec.payload.is_empty());
    assert_eq!(rec.length(), 0);
}

#[test]
fn roundtrip_binary_fidelity() {
    let payload: Vec<u8> = (0u8..=255).collect();
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, payload);
}

// ---------------------------------------------------------------------------
// Mixed record sizes in single stream
// ---------------------------------------------------------------------------

#[test]
fn mixed_sizes_stream() {
    let sizes: Vec<usize> = vec![0, 1, 100, 5, 1000, 0, 50];
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for &sz in &sizes {
            let payload = vec![0x77u8; sz];
            writer.write_record_from_payload(&payload, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for &sz in &sizes {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.payload.len(), sz);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), sizes.len() as u64);
}

#[test]
fn mixed_sizes_with_reserved_bytes() {
    let records: Vec<(Vec<u8>, u16)> = vec![
        (b"short".to_vec(), 0x0000),
        (vec![0xAA; 500], 0x1234),
        (Vec::new(), 0xFFFF),
        (b"medium length data here".to_vec(), 0x0001),
    ];

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for (payload, reserved) in &records {
            writer
                .write_record_from_payload(payload, Some(*reserved))
                .unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for (expected_payload, expected_reserved) in &records {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.payload, *expected_payload);
        assert_eq!(rec.reserved(), *expected_reserved);
    }
    assert!(reader.read_record().unwrap().is_none());
}

// ---------------------------------------------------------------------------
// Empty file handling
// ---------------------------------------------------------------------------

#[test]
fn empty_file_lenient_returns_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn empty_file_strict_returns_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), true);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

// ---------------------------------------------------------------------------
// Strict mode: reserved nonzero
// ---------------------------------------------------------------------------

#[test]
fn strict_mode_rejects_nonzero_reserved() {
    let data = vec![0x00, 0x02, 0x00, 0x01, b'A', b'B'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
}

#[test]
fn lenient_mode_accepts_nonzero_reserved() {
    let data = vec![0x00, 0x02, 0xFF, 0xFF, b'A', b'B'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, b"AB");
    assert_eq!(rec.reserved(), 0xFFFF);
}

// ---------------------------------------------------------------------------
// RDWRecord struct tests
// ---------------------------------------------------------------------------

#[test]
fn rdw_record_try_new_creates_valid_record() {
    let rec = RDWRecord::try_new(b"test data".to_vec()).unwrap();
    assert_eq!(rec.length(), 9);
    assert_eq!(rec.reserved(), 0);
    assert_eq!(rec.payload, b"test data");
}

#[test]
fn rdw_record_as_bytes_format() {
    let rec = RDWRecord::try_new(b"AB".to_vec()).unwrap();
    let bytes = rec.as_bytes();
    assert_eq!(bytes, vec![0x00, 0x02, 0x00, 0x00, b'A', b'B']);
}

#[test]
fn rdw_record_try_recompute_after_mutation() {
    let mut rec = RDWRecord::try_new(b"ABC".to_vec()).unwrap();
    assert_eq!(rec.length(), 3);
    rec.payload = b"ABCDEF".to_vec();
    rec.try_recompute_length().unwrap();
    assert_eq!(rec.length(), 6);
}

// ---------------------------------------------------------------------------
// ASCII corruption heuristic
// ---------------------------------------------------------------------------

#[test]
fn ascii_corruption_both_digits() {
    let hdr = RdwHeader::from_bytes([b'0', b'0', 0x00, 0x00]);
    assert!(hdr.looks_ascii_corrupt());
}

#[test]
fn ascii_corruption_first_byte_not_digit() {
    let hdr = RdwHeader::from_bytes([0x00, b'5', 0x00, 0x00]);
    assert!(!hdr.looks_ascii_corrupt());
}

#[test]
fn ascii_corruption_second_byte_not_digit() {
    let hdr = RdwHeader::from_bytes([b'5', 0x00, 0x00, 0x00]);
    assert!(!hdr.looks_ascii_corrupt());
}

#[test]
fn ascii_corruption_ignores_reserved_bytes() {
    // Reserved bytes being digits doesn't matter
    let hdr = RdwHeader::from_bytes([0x00, 0x0A, b'1', b'2']);
    assert!(!hdr.looks_ascii_corrupt());
}

// ---------------------------------------------------------------------------
// Writer record count tracking
// ---------------------------------------------------------------------------

#[test]
fn writer_record_count_increments() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    assert_eq!(writer.record_count(), 0);
    writer.write_record_from_payload(b"A", None).unwrap();
    assert_eq!(writer.record_count(), 1);
    writer.write_record_from_payload(b"B", None).unwrap();
    assert_eq!(writer.record_count(), 2);
    writer.write_record_from_payload(b"C", None).unwrap();
    assert_eq!(writer.record_count(), 3);
}
