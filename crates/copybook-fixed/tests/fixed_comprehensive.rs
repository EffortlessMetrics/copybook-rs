// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive integration tests for copybook-fixed covering reading,
//! writing, round-trip, edge cases, and error paths.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

// ====================================================================
// 1. Reading exact-length records
// ====================================================================

#[test]
fn read_exact_length_single_record() {
    let data = b"ABCDEFGH";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(8)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record, b"ABCDEFGH");
    assert_eq!(reader.record_count(), 1);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn read_exact_length_preserves_all_byte_values() {
    // All 256 byte values in a single record
    let data: Vec<u8> = (0..=255).collect();
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(256)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.len(), 256);
    for (i, &byte) in record.iter().enumerate() {
        assert_eq!(byte, i as u8, "byte at index {i}");
    }
}

#[test]
fn read_exact_length_with_null_bytes() {
    let data = vec![0u8; 16];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(16)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert!(record.iter().all(|&b| b == 0));
}

#[test]
fn read_exact_length_with_all_0xff() {
    let data = vec![0xFFu8; 32];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(32)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert!(record.iter().all(|&b| b == 0xFF));
}

// ====================================================================
// 2. Reading multiple records
// ====================================================================

#[test]
fn read_multiple_records_correct_boundaries() {
    let data = b"AAABBBCCC";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(3)).unwrap();

    assert_eq!(reader.read_record().unwrap().unwrap(), b"AAA");
    assert_eq!(reader.read_record().unwrap().unwrap(), b"BBB");
    assert_eq!(reader.read_record().unwrap().unwrap(), b"CCC");
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 3);
}

#[test]
fn read_multiple_records_record_count_increments() {
    let data = vec![0x41u8; 50]; // 10 records of 5 bytes
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(5)).unwrap();
    for expected_count in 1..=10 {
        reader.read_record().unwrap().unwrap();
        assert_eq!(reader.record_count(), expected_count);
    }
}

#[test]
fn read_multiple_records_each_has_distinct_content() {
    let mut data = Vec::new();
    for i in 0u8..10 {
        data.extend_from_slice(&[i; 4]);
    }
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(4)).unwrap();
    for i in 0u8..10 {
        let record = reader.read_record().unwrap().unwrap();
        assert!(
            record.iter().all(|&b| b == i),
            "record {i} content mismatch"
        );
    }
    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 3. Partial (truncated) last record handling
// ====================================================================

#[test]
fn partial_last_record_two_bytes_short() {
    // LRECL=8, data=14 bytes: 1 full record + 6-byte partial
    let data = vec![0x41u8; 14];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(8)).unwrap();
    reader.read_record().unwrap().unwrap(); // first full record
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    assert_eq!(reader.record_count(), 1); // only the successful read counted
}

#[test]
fn partial_record_exactly_one_byte() {
    // LRECL=10, data=1 byte
    let data = vec![0x42u8];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(10)).unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn partial_record_lrecl_minus_one() {
    // LRECL=50, data=49 bytes
    let data = vec![0x43u8; 49];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(50)).unwrap();
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn partial_record_after_multiple_good_records() {
    // 3 full records of 4 bytes + 2-byte partial
    let data = vec![0x44u8; 14];
    let mut reader = FixedRecordReader::new(Cursor::new(&data), Some(4)).unwrap();
    for _ in 0..3 {
        reader.read_record().unwrap().unwrap();
    }
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
    assert_eq!(reader.record_count(), 3);
}

// ====================================================================
// 4. Empty input
// ====================================================================

#[test]
fn empty_input_returns_none() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(8)).unwrap();
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn empty_input_multiple_reads_all_none() {
    let mut reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(4)).unwrap();
    for _ in 0..10 {
        assert!(reader.read_record().unwrap().is_none());
    }
    assert_eq!(reader.record_count(), 0);
}

// ====================================================================
// 5. Very large LRECL values
// ====================================================================

#[test]
fn large_lrecl_32768_read_write_roundtrip() {
    let lrecl = 32_768u32;
    let payload = vec![0xBBu8; lrecl as usize];

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.len(), lrecl as usize);
    assert!(record.iter().all(|&b| b == 0xBB));
}

#[test]
fn large_lrecl_65535_read_write_roundtrip() {
    let lrecl = 65_535u32;
    let payload = vec![0xCCu8; lrecl as usize];

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
    assert!(record.iter().all(|&b| b == 0xCC));
}

#[test]
fn large_lrecl_100000_with_partial_payload() {
    let lrecl = 100_000u32;
    let payload = vec![0xDDu8; 500]; // small payload, lots of padding

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), lrecl as usize);

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(&record[..500], payload.as_slice());
    assert!(record[500..].iter().all(|&b| b == 0));
}

// ====================================================================
// 6. Writing fixed-length records
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
fn write_empty_payload_full_padding() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(6)).unwrap();
    writer.write_record(b"").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0u8; 6]);
}

#[test]
fn write_multiple_records_output_length_correct() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(10)).unwrap();
    writer.write_record(b"AAAA").unwrap();
    writer.write_record(b"BBBBBBBBBB").unwrap();
    writer.write_record(b"").unwrap();
    writer.flush().unwrap();

    assert_eq!(writer.record_count(), 3);
    assert_eq!(output.len(), 30); // 3 * 10
}

#[test]
fn write_rejects_oversized_record() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    let err = writer.write_record(b"TOOLONG").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
    assert_eq!(writer.record_count(), 0);
}

#[test]
fn write_rejects_one_byte_over_lrecl() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(5)).unwrap();
    let err = writer.write_record(b"ABCDEF").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

#[test]
fn write_continues_after_rejected_record() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();

    // Reject oversized
    assert!(writer.write_record(b"TOOLONG").is_err());
    assert_eq!(writer.record_count(), 0);

    // Still usable
    writer.write_record(b"GOOD").unwrap();
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 1);
    assert_eq!(output, b"GOOD");
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
// 7. Round-trip read/write
// ====================================================================

#[test]
fn roundtrip_single_exact_record() {
    let lrecl = 12u32;
    let payload = b"ROUNDTRIP_OK";

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(payload).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.as_slice(), payload.as_slice());
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_multiple_records_with_padding() {
    let lrecl = 8u32;
    let payloads: &[&[u8]] = &[b"ABCDEFGH", b"XY", b"", b"12345678"];

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for &p in payloads {
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
            "record {i} data mismatch"
        );
        assert!(
            record[payload.len()..].iter().all(|&b| b == 0),
            "record {i} padding mismatch"
        );
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_streaming_500_records() {
    let lrecl = 20u32;
    let count = 500u64;

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for i in 0..count {
            let mut payload = vec![0u8; lrecl as usize];
            let idx_bytes = i.to_be_bytes();
            payload[..8].copy_from_slice(&idx_bytes);
            writer.write_record(&payload).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), count);
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    for i in 0..count {
        let record = reader.read_record().unwrap().unwrap();
        let idx = u64::from_be_bytes(record[..8].try_into().unwrap());
        assert_eq!(idx, i, "record index mismatch at {i}");
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), count);
}

#[test]
fn roundtrip_all_byte_values_per_record() {
    let lrecl = 256u32;
    let payload: Vec<u8> = (0..=255).collect();

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&payload).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record, payload);
}

// ====================================================================
// 8. Construction validation
// ====================================================================

#[test]
fn reader_rejects_zero_lrecl() {
    let err = FixedRecordReader::new(Cursor::new(b"test".as_slice()), Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn reader_rejects_none_lrecl() {
    let err = FixedRecordReader::new(Cursor::new(b"test".as_slice()), None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_rejects_zero_lrecl() {
    let mut output = Vec::new();
    let err = FixedRecordWriter::new(&mut output, Some(0)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn writer_rejects_none_lrecl() {
    let mut output = Vec::new();
    let err = FixedRecordWriter::new(&mut output, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

// ====================================================================
// 9. Accessor methods
// ====================================================================

#[test]
fn reader_lrecl_accessor() {
    let reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(80)).unwrap();
    assert_eq!(reader.lrecl(), 80);
}

#[test]
fn writer_lrecl_accessor() {
    let mut output = Vec::new();
    let writer = FixedRecordWriter::new(&mut output, Some(120)).unwrap();
    assert_eq!(writer.lrecl(), 120);
}

#[test]
fn reader_record_count_starts_at_zero() {
    let reader = FixedRecordReader::new(Cursor::new(Vec::<u8>::new()), Some(10)).unwrap();
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn writer_record_count_starts_at_zero() {
    let mut output = Vec::new();
    let writer = FixedRecordWriter::new(&mut output, Some(10)).unwrap();
    assert_eq!(writer.record_count(), 0);
}

// ====================================================================
// 10. Multiple flush calls are safe
// ====================================================================

#[test]
fn writer_multiple_flushes_safe() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    writer.write_record(b"AAAA").unwrap();
    writer.flush().unwrap();
    writer.flush().unwrap();
    writer.write_record(b"BBBB").unwrap();
    writer.flush().unwrap();
    assert_eq!(writer.record_count(), 2);
    assert_eq!(output, b"AAAABBBB");
}

// ====================================================================
// 11. LRECL = 1 byte edge case
// ====================================================================

#[test]
fn lrecl_one_read_write_roundtrip() {
    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(1)).unwrap();
        writer.write_record(b"A").unwrap();
        writer.write_record(b"B").unwrap();
        writer.write_record(b"C").unwrap();
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(1)).unwrap();
    assert_eq!(reader.read_record().unwrap().unwrap(), vec![b'A']);
    assert_eq!(reader.read_record().unwrap().unwrap(), vec![b'B']);
    assert_eq!(reader.read_record().unwrap().unwrap(), vec![b'C']);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn lrecl_one_empty_payload_pads() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(1)).unwrap();
    writer.write_record(b"").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0x00]);
}

// ====================================================================
// 12. Repeated reads after EOF
// ====================================================================

#[test]
fn repeated_reads_after_eof_always_return_none() {
    let data = b"ABCD";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(4)).unwrap();
    reader.read_record().unwrap().unwrap(); // consume the one record
    for _ in 0..5 {
        assert!(reader.read_record().unwrap().is_none());
    }
    assert_eq!(reader.record_count(), 1);
}

// ====================================================================
// 13. Validate record length
// ====================================================================

#[test]
fn validate_record_length_matching() {
    let data = b"ABCDEFGH";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(8)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    let schema = copybook_core::Schema::new();
    reader.validate_record_length(&schema, &record).unwrap();
}

#[test]
fn validate_record_length_mismatch() {
    let data = b"ABCDEFGH";
    let mut reader = FixedRecordReader::new(Cursor::new(data.as_slice()), Some(8)).unwrap();
    let _ = reader.read_record().unwrap().unwrap();
    let schema = copybook_core::Schema::new();
    let err = reader
        .validate_record_length(&schema, b"SHORT")
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// ====================================================================
// 14. Writer output size always equals N * LRECL
// ====================================================================

#[test]
fn writer_output_size_equals_n_times_lrecl() {
    for lrecl in [1u32, 5, 10, 80, 256] {
        let mut output = Vec::new();
        let count = 7;
        {
            let mut writer = FixedRecordWriter::new(&mut output, Some(lrecl)).unwrap();
            for _ in 0..count {
                writer.write_record(b"").unwrap(); // empty payloads
            }
            writer.flush().unwrap();
        }
        assert_eq!(
            output.len(),
            count * lrecl as usize,
            "output size mismatch for lrecl={lrecl}"
        );
    }
}

// ====================================================================
// 15. EBCDIC-like binary content preserved
// ====================================================================

#[test]
fn ebcdic_like_content_preserved() {
    // CP037 EBCDIC representation of "HELLO" is [0xC8, 0xC5, 0xD3, 0xD3, 0xD6]
    let ebcdic_hello = [0xC8u8, 0xC5, 0xD3, 0xD3, 0xD6];
    let lrecl = 8u32;

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        writer.write_record(&ebcdic_hello).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(&record[..5], &ebcdic_hello);
    assert!(record[5..].iter().all(|&b| b == 0));
}
