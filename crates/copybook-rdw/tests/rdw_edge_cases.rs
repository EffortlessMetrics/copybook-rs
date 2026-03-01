// SPDX-License-Identifier: AGPL-3.0-or-later
//! Edge-case tests for RDW record framing.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_rdw::{RDW_HEADER_LEN, RDWRecordReader, RDWRecordWriter, RdwHeader};
use std::io::Cursor;

// ====================================================================
// 1. Valid 4-byte RDW header → correct payload extraction
// ====================================================================

#[test]
fn valid_rdw_header_extracts_correct_payload() {
    // Header: length=5, reserved=0, followed by 5-byte payload
    let data = vec![0x00, 0x05, 0x00, 0x00, b'H', b'E', b'L', b'L', b'O'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, b"HELLO");
    assert_eq!(record.length(), 5);
    assert_eq!(record.reserved(), 0);
    assert_eq!(reader.record_count(), 1);

    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn valid_rdw_header_big_endian_256_byte_payload() {
    // 0x0100 big-endian = 256
    let mut data = vec![0x01, 0x00, 0x00, 0x00];
    data.extend(vec![0xABu8; 256]);

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.length(), 256);
    assert_eq!(record.payload.len(), 256);
    assert!(record.payload.iter().all(|&b| b == 0xAB));
}

// ====================================================================
// 2. RDW length of 0 (minimum, empty payload) → empty record
// ====================================================================

#[test]
fn rdw_zero_length_yields_empty_payload() {
    let data = vec![0x00, 0x00, 0x00, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.length(), 0);
    assert!(record.payload.is_empty());
    assert_eq!(reader.record_count(), 1);

    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 3. RDW with length field where data is truncated → error
// ====================================================================

#[test]
fn rdw_length_exceeds_available_data_returns_error() {
    // Header says payload is 10 bytes, but only 3 bytes follow
    let data = vec![0x00, 0x0A, 0x00, 0x00, b'A', b'B', b'C'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

// ====================================================================
// 4. Incomplete headers (< 4 bytes) → error or EOF per mode
// ====================================================================

#[test]
fn rdw_empty_input_returns_none() {
    let mut reader = RDWRecordReader::new(Cursor::new(Vec::<u8>::new()), false);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}

#[test]
fn rdw_one_byte_header_lenient_returns_none() {
    let data = vec![0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn rdw_one_byte_header_strict_returns_underflow() {
    let data = vec![0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn rdw_two_byte_header_lenient_returns_none() {
    let data = vec![0x00, 0x05];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn rdw_two_byte_header_strict_returns_underflow() {
    let data = vec![0x00, 0x05];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn rdw_three_byte_header_lenient_returns_none() {
    let data = vec![0x00, 0x05, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn rdw_three_byte_header_strict_returns_underflow() {
    let data = vec![0x00, 0x05, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);
    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

// ====================================================================
// 5. RDW with reserved bytes non-zero → lenient warns, strict errors
// ====================================================================

#[test]
fn rdw_reserved_nonzero_lenient_succeeds() {
    let data = vec![0x00, 0x03, 0xDE, 0xAD, b'A', b'B', b'C'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload, b"ABC");
    assert_eq!(record.reserved(), 0xDEAD);
}

#[test]
fn rdw_reserved_nonzero_strict_returns_error() {
    let data = vec![0x00, 0x03, 0xFF, 0xFF, b'A', b'B', b'C'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), true);

    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKR211_RDW_RESERVED_NONZERO);
}

#[test]
fn rdw_reserved_0x0001_lenient_preserves_value() {
    let data = vec![0x00, 0x02, 0x00, 0x01, b'X', b'Y'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.reserved(), 0x0001);
    assert_eq!(record.payload, b"XY");
}

// ====================================================================
// 6. Multiple consecutive RDW records → correct parsing
// ====================================================================

#[test]
fn multiple_consecutive_rdw_records() {
    let mut data = Vec::new();
    // Record 1: "AB"
    data.extend_from_slice(&[0x00, 0x02, 0x00, 0x00, b'A', b'B']);
    // Record 2: "CDE"
    data.extend_from_slice(&[0x00, 0x03, 0x00, 0x00, b'C', b'D', b'E']);
    // Record 3: "" (empty)
    data.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
    // Record 4: "F"
    data.extend_from_slice(&[0x00, 0x01, 0x00, 0x00, b'F']);

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1.payload, b"AB");

    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2.payload, b"CDE");

    let r3 = reader.read_record().unwrap().unwrap();
    assert!(r3.payload.is_empty());

    let r4 = reader.read_record().unwrap().unwrap();
    assert_eq!(r4.payload, b"F");

    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 4);
}

#[test]
fn multiple_records_with_varying_sizes() {
    let sizes: &[usize] = &[1, 10, 100, 500, 1000, 0, 50];

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for &size in sizes {
            let payload = vec![0x42u8; size];
            writer.write_record_from_payload(&payload, None).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), sizes.len() as u64);
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for &expected_size in sizes {
        let record = reader.read_record().unwrap().unwrap();
        assert_eq!(record.payload.len(), expected_size);
    }
    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 7. RDW with length exceeding remaining data → error (not panic)
// ====================================================================

#[test]
fn rdw_length_exceeds_remaining_data_returns_error_not_panic() {
    // Header says 1000 bytes, but only 5 follow
    let data = vec![0x03, 0xE8, 0x00, 0x00, b'X', b'Y', b'Z', b'W', b'Q'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn rdw_max_length_with_no_payload_data_returns_error() {
    // Header says 65535 bytes, but nothing follows
    let data = vec![0xFF, 0xFF, 0x00, 0x00];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

#[test]
fn rdw_partial_payload_after_valid_record_returns_error() {
    let mut data = Vec::new();
    // Valid first record
    data.extend_from_slice(&[0x00, 0x02, 0x00, 0x00, b'O', b'K']);
    // Second record: header says 10, but only 3 bytes
    data.extend_from_slice(&[0x00, 0x0A, 0x00, 0x00, b'X', b'Y', b'Z']);

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);

    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1.payload, b"OK");

    let err = reader.read_record().unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);
}

// ====================================================================
// 8. Write RDW records and read back → verify identity
// ====================================================================

#[test]
fn write_read_roundtrip_identity() {
    let payloads: &[&[u8]] = &[b"alpha", b"beta", b"gamma", b"delta"];

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
        assert_eq!(record.payload, expected);
        assert_eq!(record.reserved(), 0);
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn write_read_roundtrip_preserves_reserved_bytes() {
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer
            .write_record_from_payload(b"data1", Some(0x1234))
            .unwrap();
        writer
            .write_record_from_payload(b"data2", Some(0xABCD))
            .unwrap();
        writer
            .write_record_from_payload(b"data3", Some(0x0000))
            .unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);

    let r1 = reader.read_record().unwrap().unwrap();
    assert_eq!(r1.payload, b"data1");
    assert_eq!(r1.reserved(), 0x1234);

    let r2 = reader.read_record().unwrap().unwrap();
    assert_eq!(r2.payload, b"data2");
    assert_eq!(r2.reserved(), 0xABCD);

    let r3 = reader.read_record().unwrap().unwrap();
    assert_eq!(r3.payload, b"data3");
    assert_eq!(r3.reserved(), 0x0000);
}

#[test]
fn write_read_roundtrip_1000_records() {
    let count = 1000u64;

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for i in 0..count {
            let payload = format!("record-{i:04}");
            writer
                .write_record_from_payload(payload.as_bytes(), None)
                .unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), count);
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    for i in 0..count {
        let record = reader.read_record().unwrap().unwrap();
        let expected = format!("record-{i:04}");
        assert_eq!(record.payload, expected.as_bytes());
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), count);
}

// ====================================================================
// 9. Large RDW record (32KB payload) → works correctly
// ====================================================================

#[test]
fn large_rdw_record_32kb_payload() {
    let payload_size = 32 * 1024; // 32 KB
    let payload = vec![0xCDu8; payload_size];

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    assert_eq!(encoded.len(), RDW_HEADER_LEN + payload_size);

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload.len(), payload_size);
    assert!(record.payload.iter().all(|&b| b == 0xCD));
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn large_rdw_record_u16_max_payload() {
    let payload_size = u16::MAX as usize;
    let payload = vec![0xEFu8; payload_size];

    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.write_record_from_payload(&payload, None).unwrap();
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload.len(), payload_size);
    assert_eq!(record.length(), u16::MAX);
}

#[test]
fn rdw_payload_exceeding_u16_max_rejected_by_writer() {
    let payload = vec![0u8; (u16::MAX as usize) + 1];
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);

    let err = writer
        .write_record_from_payload(&payload, None)
        .unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ====================================================================
// 10. RDW big-endian length → correct interpretation
// ====================================================================

#[test]
fn rdw_big_endian_length_interpretation() {
    // Verify the RDW length field is big-endian
    let header = RdwHeader::from_bytes([0x00, 0x0A, 0x00, 0x00]);
    assert_eq!(header.length(), 10); // 0x000A = 10

    let header = RdwHeader::from_bytes([0x01, 0x00, 0x00, 0x00]);
    assert_eq!(header.length(), 256); // 0x0100 = 256

    let header = RdwHeader::from_bytes([0x10, 0x00, 0x00, 0x00]);
    assert_eq!(header.length(), 4096); // 0x1000 = 4096

    let header = RdwHeader::from_bytes([0xFF, 0xFF, 0x00, 0x00]);
    assert_eq!(header.length(), 65535); // 0xFFFF = 65535
}

#[test]
fn rdw_reader_interprets_length_as_big_endian() {
    // 0x0100 big-endian = 256 bytes of payload
    let mut data = vec![0x01, 0x00, 0x00, 0x00];
    data.extend(vec![0x42u8; 256]);

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader.read_record().unwrap().unwrap();
    assert_eq!(record.payload.len(), 256);
}

#[test]
fn rdw_header_from_payload_len_encodes_big_endian() {
    let header = RdwHeader::from_payload_len(256, 0).unwrap();
    let bytes = header.bytes();
    // 256 in big-endian is [0x01, 0x00]
    assert_eq!(bytes[0], 0x01);
    assert_eq!(bytes[1], 0x00);

    let header = RdwHeader::from_payload_len(1, 0).unwrap();
    let bytes = header.bytes();
    assert_eq!(bytes[0], 0x00);
    assert_eq!(bytes[1], 0x01);
}

#[test]
fn rdw_if_interpreted_as_little_endian_would_give_wrong_length() {
    // Header bytes [0x01, 0x00] should be 256 (big-endian), NOT 1 (little-endian)
    let header = RdwHeader::from_bytes([0x01, 0x00, 0x00, 0x00]);
    let be_length = header.length();
    let le_length = u16::from_le_bytes([0x01, 0x00]);

    assert_eq!(be_length, 256);
    assert_eq!(le_length, 1);
    assert_ne!(be_length, le_length);
}

// ====================================================================
// Bonus: Write empty record list → empty output
// ====================================================================

#[test]
fn write_empty_record_list_produces_empty_output() {
    let mut output = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut output);
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), 0);
    }
    assert!(output.is_empty());
}

#[test]
fn write_empty_record_list_then_read_returns_none() {
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 0);
}
