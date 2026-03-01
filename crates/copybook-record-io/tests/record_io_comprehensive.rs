// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive supplemental tests for `copybook-record-io` covering
//! format routing, reader/writer roundtrips for both formats, format
//! mismatch detection, streaming multiple records, and edge cases.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_error::ErrorCode;
use copybook_options::RecordFormat;
use copybook_record_io::{
    FixedRecordReader, FixedRecordWriter, RDWRecord, RDWRecordReader, RDWRecordWriter, read_record,
    write_record,
};
use std::io::Cursor;

// ====================================================================
// 1. Fixed format routing (detect fixed by LRECL)
// ====================================================================

#[test]
fn fixed_routing_with_lrecl_80() {
    let payload = vec![0x40u8; 80]; // EBCDIC spaces
    let mut cursor = Cursor::new(payload.clone());
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(80))
        .unwrap()
        .unwrap();
    assert_eq!(rec.len(), 80);
    assert_eq!(rec, payload);
}

#[test]
fn fixed_routing_with_lrecl_132() {
    let payload = vec![0x20u8; 132]; // ASCII spaces
    let mut cursor = Cursor::new(payload.clone());
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(132))
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn fixed_routing_rejects_missing_lrecl() {
    let mut cursor = Cursor::new(vec![0u8; 100]);
    let err = read_record(&mut cursor, RecordFormat::Fixed, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

// ====================================================================
// 2. RDW format routing
// ====================================================================

#[test]
fn rdw_routing_single_record_no_lrecl_needed() {
    let mut data = Vec::new();
    write_record(&mut data, b"RDW-PAYLOAD", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"RDW-PAYLOAD");
}

#[test]
fn rdw_routing_lrecl_is_ignored() {
    let mut data = Vec::new();
    write_record(&mut data, b"IGNORE", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    // Passing a random lrecl should be ignored by RDW format
    let rec = read_record(&mut cursor, RecordFormat::RDW, Some(999))
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"IGNORE");
}

// ====================================================================
// 3. Reader → Writer roundtrip for both formats
// ====================================================================

#[test]
fn roundtrip_fixed_multiple_records_via_dispatch() {
    let lrecl = 10u32;
    let records: &[&[u8]] = &[b"AAAAAAAAAA", b"BBBBBBBBBB", b"CCCCCCCCCC"];
    let mut wire = Vec::new();
    for &rec in records {
        write_record(&mut wire, rec, RecordFormat::Fixed).unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for &expected in records {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.as_slice(), expected);
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_rdw_multiple_records_via_dispatch() {
    let payloads: &[&[u8]] = &[b"SHORT", b"MEDIUM_DATA", b"X", b""];
    let mut wire = Vec::new();
    for &p in payloads {
        write_record(&mut wire, p, RecordFormat::RDW).unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&wire), false);
    for &expected in payloads {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.payload.as_slice(), expected);
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn roundtrip_fixed_single_byte_records() {
    let mut wire = Vec::new();
    for b in 0..=255u8 {
        write_record(&mut wire, &[b], RecordFormat::Fixed).unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&wire), Some(1)).unwrap();
    for expected_byte in 0..=255u8 {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec, vec![expected_byte]);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), 256);
}

#[test]
fn roundtrip_rdw_single_byte_records() {
    let mut wire = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut wire);
        for b in 0..=255u8 {
            writer.write_record_from_payload(&[b], None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&wire), false);
    for expected_byte in 0..=255u8 {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.payload, vec![expected_byte]);
    }
    assert!(reader.read_record().unwrap().is_none());
}

// ====================================================================
// 4. Error on mismatched format
// ====================================================================

#[test]
fn fixed_data_read_as_rdw_errors() {
    // 8 bytes of ASCII text interpreted as RDW
    let data = b"ABCDEFGH";
    let mut cursor = Cursor::new(data.to_vec());
    // 'AB' = 0x4142 = 16706 bytes expected, but only 4 remain after header
    let result = read_record(&mut cursor, RecordFormat::RDW, None);
    assert!(result.is_err(), "ASCII data should fail as RDW");
}

#[test]
fn rdw_data_read_as_fixed_includes_header_bytes() {
    let mut rdw_data = Vec::new();
    write_record(&mut rdw_data, b"TEST", RecordFormat::RDW).unwrap();
    // rdw_data = [0x00, 0x04, 0x00, 0x00, T, E, S, T]

    let mut cursor = Cursor::new(rdw_data);
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(8))
        .unwrap()
        .unwrap();
    // Fixed format reads raw bytes including the RDW header
    assert_eq!(rec.len(), 8);
    assert_eq!(&rec[0..4], &[0x00, 0x04, 0x00, 0x00]);
    assert_eq!(&rec[4..8], b"TEST");
}

// ====================================================================
// 5. Streaming multiple records
// ====================================================================

#[test]
fn streaming_fixed_100_records() {
    let lrecl = 20u32;
    let count = 100u64;
    let mut wire = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut wire, Some(lrecl)).unwrap();
        for i in 0..count {
            let mut payload = vec![0x40u8; lrecl as usize];
            payload[0] = (i & 0xFF) as u8;
            writer.write_record(&payload).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&wire), Some(lrecl)).unwrap();
    for i in 0..count {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec[0], (i & 0xFF) as u8, "record {i} tag byte");
        assert_eq!(rec.len(), lrecl as usize);
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), count);
}

#[test]
fn streaming_rdw_100_variable_records() {
    let count = 100u64;
    let mut wire = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut wire);
        for i in 0..count {
            let size = (i as usize % 50) + 1;
            let payload = vec![(i & 0xFF) as u8; size];
            writer.write_record_from_payload(&payload, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&wire), false);
    for i in 0..count {
        let rec = reader.read_record().unwrap().unwrap();
        let expected_size = (i as usize % 50) + 1;
        assert_eq!(rec.payload.len(), expected_size, "record {i} size");
        assert_eq!(rec.payload[0], (i & 0xFF) as u8, "record {i} content");
    }
    assert!(reader.read_record().unwrap().is_none());
    assert_eq!(reader.record_count(), count);
}

// ====================================================================
// 6. Re-exported type usage
// ====================================================================

#[test]
fn reexported_rdw_record_try_new() {
    let rec = RDWRecord::try_new(b"via-reexport".to_vec()).unwrap();
    assert_eq!(rec.length(), 12);
    assert_eq!(rec.payload, b"via-reexport");
}

#[test]
fn reexported_fixed_writer_with_padding() {
    let mut wire = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut wire, Some(10)).unwrap();
        writer.write_record(b"SHORT").unwrap(); // 5 bytes, padded to 10
        writer.flush().unwrap();
    }
    assert_eq!(wire.len(), 10);
    assert_eq!(&wire[..5], b"SHORT");
    assert!(wire[5..].iter().all(|&b| b == 0));
}

#[test]
fn reexported_fixed_writer_rejects_oversized() {
    let mut wire = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut wire, Some(4)).unwrap();
    let err = writer.write_record(b"TOOLONG").unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ====================================================================
// 7. Empty payloads
// ====================================================================

#[test]
fn fixed_empty_write_read_cycle() {
    let mut wire = Vec::new();
    write_record(&mut wire, b"", RecordFormat::Fixed).unwrap();
    assert!(wire.is_empty(), "empty fixed write produces no bytes");
}

#[test]
fn rdw_empty_write_produces_header_read_returns_empty() {
    let mut wire = Vec::new();
    write_record(&mut wire, b"", RecordFormat::RDW).unwrap();
    assert_eq!(wire.len(), 4, "empty RDW write produces 4-byte header");

    let mut cursor = Cursor::new(wire);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert!(rec.is_empty());
}

// ====================================================================
// 8. Large record roundtrip via dispatch
// ====================================================================

#[test]
fn large_fixed_record_8192_bytes() {
    let payload = vec![0xEE; 8192];
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::Fixed).unwrap();
    let mut cursor = Cursor::new(wire);
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(8192))
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

#[test]
fn large_rdw_record_60000_bytes() {
    let payload = vec![0xDD; 60_000];
    let mut wire = Vec::new();
    write_record(&mut wire, &payload, RecordFormat::RDW).unwrap();
    let mut cursor = Cursor::new(wire);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, payload);
}

// ====================================================================
// 9. Fixed writer truncation error vs exact fit
// ====================================================================

#[test]
fn fixed_writer_exact_lrecl_succeeds() {
    let mut wire = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut wire, Some(5)).unwrap();
    writer.write_record(b"EXACT").unwrap();
    writer.flush().unwrap();
    assert_eq!(wire, b"EXACT");
}

#[test]
fn fixed_writer_undersized_data_padded() {
    let mut wire = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut wire, Some(8)).unwrap();
    writer.write_record(b"HI").unwrap();
    writer.flush().unwrap();
    assert_eq!(wire.len(), 8);
    assert_eq!(&wire[..2], b"HI");
    // Remaining bytes zero-padded
    assert!(wire[2..].iter().all(|&b| b == 0));
}

// ====================================================================
// 10. Record count tracking through dispatch
// ====================================================================

#[test]
fn fixed_record_count_via_reader() {
    let data = vec![0x41u8; 15]; // 3 records of 5 bytes
    let mut reader = FixedRecordReader::new(Cursor::new(data), Some(5)).unwrap();
    assert_eq!(reader.record_count(), 0);

    reader.read_record().unwrap();
    assert_eq!(reader.record_count(), 1);
    reader.read_record().unwrap();
    assert_eq!(reader.record_count(), 2);
    reader.read_record().unwrap();
    assert_eq!(reader.record_count(), 3);

    // EOF doesn't increment
    reader.read_record().unwrap();
    assert_eq!(reader.record_count(), 3);
}

#[test]
fn rdw_record_count_via_writer() {
    let mut wire = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut wire);
    assert_eq!(writer.record_count(), 0);

    writer.write_record_from_payload(b"A", None).unwrap();
    assert_eq!(writer.record_count(), 1);
    writer.write_record_from_payload(b"BB", None).unwrap();
    assert_eq!(writer.record_count(), 2);
    writer.write_record_from_payload(b"CCC", None).unwrap();
    assert_eq!(writer.record_count(), 3);
}

// ====================================================================
// 11. RecordFormat enum properties
// ====================================================================

#[test]
fn record_format_is_fixed_and_is_variable_mutually_exclusive() {
    assert!(RecordFormat::Fixed.is_fixed());
    assert!(!RecordFormat::Fixed.is_variable());
    assert!(!RecordFormat::RDW.is_fixed());
    assert!(RecordFormat::RDW.is_variable());
}

#[test]
fn record_format_display_lowercase() {
    assert_eq!(format!("{}", RecordFormat::Fixed), "fixed");
    assert_eq!(format!("{}", RecordFormat::RDW), "rdw");
}

#[test]
fn record_format_description_non_empty() {
    assert!(!RecordFormat::Fixed.description().is_empty());
    assert!(!RecordFormat::RDW.description().is_empty());
}
