// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-rdw covering header parsing,
//! reading, writing, round-trip, edge cases, and error paths.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_rdw::{
    RDW_HEADER_LEN, RDW_MAX_PAYLOAD_LEN, RDWRecord, RDWRecordReader, RDWRecordWriter, RdwHeader,
    rdw_payload_len_to_u16,
};
use std::io::Cursor;

// ====================================================================
// 1. Valid RDW header parsing (big-endian 2-byte length + 2 reserved)
// ====================================================================

#[test]
fn header_from_bytes_zero_length_zero_reserved() {
    let header = RdwHeader::from_bytes([0x00, 0x00, 0x00, 0x00]);
    assert_eq!(header.length(), 0);
    assert_eq!(header.reserved(), 0);
}

#[test]
fn header_from_bytes_small_length() {
    let header = RdwHeader::from_bytes([0x00, 0x0A, 0x00, 0x00]);
    assert_eq!(header.length(), 10);
    assert_eq!(header.reserved(), 0);
}

#[test]
fn header_from_bytes_big_endian_256() {
    // 0x0100 in big-endian = 256
    let header = RdwHeader::from_bytes([0x01, 0x00, 0x00, 0x00]);
    assert_eq!(header.length(), 256);
}

#[test]
fn header_from_bytes_big_endian_4096() {
    // 0x1000 in big-endian = 4096
    let header = RdwHeader::from_bytes([0x10, 0x00, 0x00, 0x00]);
    assert_eq!(header.length(), 4096);
}

#[test]
fn header_from_bytes_max_length() {
    let header = RdwHeader::from_bytes([0xFF, 0xFF, 0x00, 0x00]);
    assert_eq!(header.length(), u16::MAX);
}

#[test]
fn header_from_payload_len_roundtrip() {
    let header = RdwHeader::from_payload_len(1234, 0x5678).unwrap();
    assert_eq!(header.length(), 1234);
    assert_eq!(header.reserved(), 0x5678);
    // Verify raw bytes
    let bytes = header.bytes();
    assert_eq!(bytes[0], 0x04); // 1234 >> 8
    assert_eq!(bytes[1], 0xD2); // 1234 & 0xFF
    assert_eq!(bytes[2], 0x56);
    assert_eq!(bytes[3], 0x78);
}

#[test]
fn header_from_payload_len_zero() {
    let header = RdwHeader::from_payload_len(0, 0).unwrap();
    assert_eq!(header.length(), 0);
    assert_eq!(header.bytes(), [0x00, 0x00, 0x00, 0x00]);
}

#[test]
fn header_from_payload_len_max() {
    let header = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN, 0).unwrap();
    assert_eq!(header.length(), u16::MAX);
}

#[test]
fn header_from_payload_len_exceeds_max_returns_error() {
    let err = RdwHeader::from_payload_len(RDW_MAX_PAYLOAD_LEN + 1, 0).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

// ====================================================================
// 2. Writing RDW headers
// ====================================================================

#[test]
fn writer_writes_single_record() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    let record = RDWRecord::try_new(b"test".to_vec()).unwrap();
    writer.write_record(&record).unwrap();
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 1);
    assert_eq!(output, vec![0, 4, 0, 0, b't', b'e', b's', b't']);
}

#[test]
fn writer_from_payload_writes_correct_header() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.write_record_from_payload(b"HELLO", None).unwrap();
    writer.flush().unwrap();
    // Length = 5 => [0x00, 0x05], reserved = [0x00, 0x00]
    assert_eq!(&output[..4], &[0x00, 0x05, 0x00, 0x00]);
    assert_eq!(&output[4..], b"HELLO");
}

#[test]
fn writer_from_payload_with_reserved() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer
        .write_record_from_payload(b"AB", Some(0xBEEF))
        .unwrap();
    writer.flush().unwrap();
    assert_eq!(&output[..4], &[0x00, 0x02, 0xBE, 0xEF]);
    assert_eq!(&output[4..], b"AB");
}

#[test]
fn writer_empty_payload_writes_header_only() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.write_record_from_payload(b"", None).unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0x00, 0x00, 0x00, 0x00]);
}

#[test]
fn writer_rejects_payload_exceeding_u16_max() {
    let payload = vec![0u8; (u16::MAX as usize) + 1];
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    let err = writer
        .write_record_from_payload(&payload, None)
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ====================================================================
// 3. Variable-length records (different sizes in sequence)
// ====================================================================

#[test]
fn variable_length_records_in_sequence() {
    let payloads: &[&[u8]] = &[b"A", b"BB", b"CCC", b"DDDD", b"EEEEE"];

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for &p in payloads {
            writer.write_record_from_payload(p, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for &expected in payloads {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.payload.as_slice(), expected);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 5);
}

#[test]
fn variable_length_records_with_sizes_1_10_100_1000() {
    let sizes: &[usize] = &[1, 10, 100, 1000];

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for &size in sizes {
            let payload = vec![0x42u8; size];
            writer.write_record_from_payload(&payload, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for &expected_size in sizes {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.payload.len(), expected_size);
        assert!(record.payload.iter().all(|&b| b == 0x42));
    }
    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 4. Minimum RDW record (4-byte header, 0 payload)
// ====================================================================

#[test]
fn minimum_rdw_record_zero_payload() {
    let data = vec![0x00, 0x00, 0x00, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.length(), 0);
    assert!(record.payload.is_empty());
    assert_eq!(record.reserved(), 0);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn multiple_zero_length_records() {
    let mut data = Vec::new();
    for _ in 0..5 {
        data.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
    }
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    for _ in 0..5 {
        let record = reader.read_record().unwrap().unwrap();
        assert!(record.payload.is_empty());
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 5);
}

#[test]
fn zero_and_nonzero_records_interleaved() {
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(b"", None).unwrap();
        writer.write_record_from_payload(b"DATA", None).unwrap();
        writer.write_record_from_payload(b"", None).unwrap();
        writer.write_record_from_payload(b"MORE", None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let r1 = reader.read_record().unwrap().unwrap();
    assert!(r1.payload.is_empty());
    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2.payload, b"DATA");
    let r3 = reader.read_record().unwrap().unwrap();
    assert!(r3.payload.is_empty());
    let r4 = reader.read_record().unwrap().unwrap();
    assert_eq!(r4.payload, b"MORE");
    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 5. Maximum size boundary
// ====================================================================

#[test]
fn max_payload_size_u16_max() {
    let payload = vec![0xABu8; u16::MAX as usize];
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), RDW_HEADER_LEN + u16::MAX as usize);

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.length(), u16::MAX);
    assert_eq!(record.payload.len(), u16::MAX as usize);
}

#[test]
fn rdw_max_payload_len_constant() {
    assert_eq!(RDW_MAX_PAYLOAD_LEN, u16::MAX as usize);
    assert_eq!(RDW_MAX_PAYLOAD_LEN, 65535);
}

#[test]
fn rdw_header_len_constant() {
    assert_eq!(RDW_HEADER_LEN, 4);
}

// ====================================================================
// 6. Invalid RDW lengths (length < available, length > data)
// ====================================================================

#[test]
fn invalid_rdw_length_exceeds_data() {
    // Header says 100 bytes payload, but only 5 follow
    let data = vec![0x00, 0x64, 0x00, 0x00, b'A', b'B', b'C', b'D', b'E'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn invalid_rdw_max_length_no_payload() {
    // Header says 65535 bytes, no payload
    let data = vec![0xFF, 0xFF, 0x00, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn invalid_rdw_incomplete_header_1_byte_lenient() {
    let data = vec![0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn invalid_rdw_incomplete_header_1_byte_strict() {
    let data = vec![0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn invalid_rdw_incomplete_header_2_bytes_lenient() {
    let data = vec![0x00, 0x05];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn invalid_rdw_incomplete_header_2_bytes_strict() {
    let data = vec![0x00, 0x05];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn invalid_rdw_incomplete_header_3_bytes_lenient() {
    let data = vec![0x00, 0x05, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn invalid_rdw_incomplete_header_3_bytes_strict() {
    let data = vec![0x00, 0x05, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn invalid_rdw_partial_payload_after_valid_record() {
    let mut data = Vec::new();
    // Valid record
    data.extend_from_slice(&[0x00, 0x03, 0x00, 0x00, b'O', b'K', b'!']);
    // Broken second record: says 10 bytes, only 2 follow
    data.extend_from_slice(&[0x00, 0x0A, 0x00, 0x00, b'N', b'O']);

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1.payload, b"OK!");
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

// ====================================================================
// 7. Round-trip read/write
// ====================================================================

#[test]
fn roundtrip_single_record() {
    let payload = b"ROUNDTRIP";
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, payload);
    assert_eq!(record.reserved(), 0);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_multiple_records() {
    let payloads: &[&[u8]] = &[b"alpha", b"beta", b"", b"gamma", b"delta epsilon"];

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for &p in payloads {
            writer.write_record_from_payload(p, None).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), payloads.len() as u64);
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for &expected in payloads {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.payload.as_slice(), expected);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), payloads.len() as u64);
}

#[test]
fn roundtrip_500_records_with_varying_sizes() {
    let mut encoded = Vec::new();
    let count = 500u64;
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for i in 0..count {
            let size = (i % 100) as usize;
            let payload = vec![(i & 0xFF) as u8; size];
            writer.write_record_from_payload(&payload, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for i in 0..count {
        let record = reader.read_record().unwrap().unwrap();
        let expected_size = (i % 100) as usize;
        assert_eq!(record.payload.len(), expected_size, "record {i} size");
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), count);
}

#[test]
fn roundtrip_preserves_all_byte_values() {
    let payload: Vec<u8> = (0..=255).collect();
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, payload);
}

// ====================================================================
// 8. Reserved bytes handling
// ====================================================================

#[test]
fn reserved_bytes_zero_in_lenient_mode() {
    let data = vec![0x00, 0x02, 0x00, 0x00, b'O', b'K'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.reserved(), 0);
}

#[test]
fn reserved_bytes_nonzero_lenient_preserves() {
    let data = vec![0x00, 0x03, 0xDE, 0xAD, b'A', b'B', b'C'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.reserved(), 0xDEAD);
    assert_eq!(record.payload, b"ABC");
}

#[test]
fn reserved_bytes_nonzero_strict_errors() {
    let data = vec![0x00, 0x03, 0x00, 0x01, b'X', b'Y', b'Z'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
}

#[test]
fn reserved_bytes_roundtrip_via_writer() {
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer
            .write_record_from_payload(b"test", Some(0x1234))
            .unwrap();
        writer
            .write_record_from_payload(b"data", Some(0xFFFF))
            .unwrap();
        writer
            .write_record_from_payload(b"zero", Some(0x0000))
            .unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);

    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1.reserved(), 0x1234);
    assert_eq!(r1.payload, b"test");

    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2.reserved(), 0xFFFF);
    assert_eq!(r2.payload, b"data");

    let r3 = reader.read_record().unwrap().unwrap();
    assert_eq!(r3.reserved(), 0x0000);
    assert_eq!(r3.payload, b"zero");
}

// ====================================================================
// 9. RDWRecord struct methods
// ====================================================================

#[test]
fn rdw_record_try_new_sets_zero_reserved() {
    let record = RDWRecord::try_new(b"hello".to_vec()).unwrap();
    assert_eq!(record.length(), 5);
    assert_eq!(record.reserved(), 0);
    assert_eq!(record.payload, b"hello");
}

#[test]
fn rdw_record_try_with_reserved_preserves() {
    let record = RDWRecord::try_with_reserved(b"test".to_vec(), 0xABCD).unwrap();
    assert_eq!(record.length(), 4);
    assert_eq!(record.reserved(), 0xABCD);
}

#[test]
fn rdw_record_as_bytes_prepends_header() {
    let record = RDWRecord::try_new(b"XY".to_vec()).unwrap();
    let bytes = record.as_bytes();
    assert_eq!(bytes, vec![0x00, 0x02, 0x00, 0x00, b'X', b'Y']);
}

#[test]
fn rdw_record_try_recompute_length_updates_after_mutation() {
    let mut record = RDWRecord::try_new(b"AB".to_vec()).unwrap();
    assert_eq!(record.length(), 2);

    record.payload = b"ABCDEFGH".to_vec();
    record.try_recompute_length().unwrap();
    assert_eq!(record.length(), 8);
}

#[test]
fn rdw_record_try_new_oversized_returns_error() {
    let payload = vec![0u8; (u16::MAX as usize) + 1];
    let err = RDWRecord::try_new(payload).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn rdw_record_empty_payload() {
    let record = RDWRecord::try_new(Vec::new()).unwrap();
    assert_eq!(record.length(), 0);
    assert!(record.payload.is_empty());
    assert_eq!(record.as_bytes(), vec![0x00, 0x00, 0x00, 0x00]);
}

// ====================================================================
// 10. Empty input
// ====================================================================

#[test]
fn empty_input_lenient_returns_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn empty_input_strict_returns_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), true);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn empty_input_repeated_reads_all_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
    for _ in 0..5 {
        assert!(reader.read_record().unwrap().is_none());
    }
}

// ====================================================================
// 11. ASCII corruption detection
// ====================================================================

#[test]
fn ascii_corruption_detected_when_length_bytes_are_digits() {
    // Both length bytes are ASCII digits: '1' (0x31) and '2' (0x32)
    // 0x3132 = 12594 in big-endian
    let mut data = vec![0x31, 0x32, 0x00, 0x00];
    data.extend(vec![b'X'; 0x3132]);
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
}

#[test]
fn header_looks_ascii_corrupt_with_digits() {
    let header = RdwHeader::from_bytes([b'0', b'5', 0x00, 0x00]);
    assert!(header.looks_ascii_corrupt());
}

#[test]
fn header_looks_ascii_corrupt_false_for_binary() {
    let header = RdwHeader::from_bytes([0x00, 0x05, 0x00, 0x00]);
    assert!(!header.looks_ascii_corrupt());
}

#[test]
fn header_looks_ascii_corrupt_false_one_digit_one_binary() {
    let header = RdwHeader::from_bytes([b'3', 0x00, 0x00, 0x00]);
    assert!(!header.looks_ascii_corrupt());
}

// ====================================================================
// 12. rdw_payload_len_to_u16 edge cases
// ====================================================================

#[test]
fn payload_len_to_u16_zero() {
    assert_eq!(rdw_payload_len_to_u16(0).unwrap(), 0);
}

#[test]
fn payload_len_to_u16_one() {
    assert_eq!(rdw_payload_len_to_u16(1).unwrap(), 1);
}

#[test]
fn payload_len_to_u16_max() {
    assert_eq!(rdw_payload_len_to_u16(65535).unwrap(), u16::MAX);
}

#[test]
fn payload_len_to_u16_exceeds_max() {
    let err = rdw_payload_len_to_u16(65536).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

// ====================================================================
// 13. Writer record count tracking
// ====================================================================

#[test]
fn writer_record_count_increments_correctly() {
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

#[test]
fn writer_empty_output_after_flush_no_records() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 0);
    assert!(output.is_empty());
}

// ====================================================================
// 14. Output size verification
// ====================================================================

#[test]
fn output_size_matches_header_plus_payload() {
    let payloads: &[&[u8]] = &[b"short", b"medium length data", b"", b"x"];
    let mut output = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut output);
        for &p in payloads {
            writer.write_record_from_payload(p, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let expected_size: usize = payloads.iter().map(|p| RDW_HEADER_LEN + p.len()).sum();
    assert_eq!(output.len(), expected_size);
}

// ====================================================================
// 15. Binary payload preservation
// ====================================================================

#[test]
fn binary_payload_preserved_through_roundtrip() {
    // EBCDIC-like data
    let payload = vec![0xC8, 0xC5, 0xD3, 0xD3, 0xD6, 0x00, 0xFF, 0x80];
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, payload);
}

// ====================================================================
// 16. Large record round-trip (32 KB)
// ====================================================================

#[test]
fn large_record_32kb_roundtrip() {
    let payload = vec![0xCDu8; 32 * 1024];
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload.len(), 32 * 1024);
    assert!(record.payload.iter().all(|&b| b == 0xCD));
}

// ====================================================================
// 17. Repeated reads after EOF
// ====================================================================

#[test]
fn repeated_reads_after_eof_always_return_none() {
    let data = vec![0x00, 0x02, 0x00, 0x00, b'H', b'I'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    reader.read_record().unwrap().unwrap(); // consume record
    for _ in 0..5 {
        assert!(reader.read_record().unwrap().is_none());
    }
    assert_eq!(reader.record_count(), 1);
}
