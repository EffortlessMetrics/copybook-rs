// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for record I/O format dispatch.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_error::ErrorCode;
use copybook_options::RecordFormat;
use copybook_record_io::{
    FixedRecordReader, FixedRecordWriter, RDWRecordReader, RDWRecordWriter, read_record,
    write_record,
};
use std::io::Cursor;

// ---------------------------------------------------------------------------
// Format dispatch: fixed vs RDW via read_record/write_record
// ---------------------------------------------------------------------------

#[test]
fn dispatch_read_fixed_format() {
    let data = b"COBOL_REC";
    let mut cursor = Cursor::new(data.to_vec());
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(9))
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"COBOL_REC");
}

#[test]
fn dispatch_read_rdw_format() {
    let mut data = Vec::new();
    data.extend_from_slice(&[0x00, 0x05, 0x00, 0x00]);
    data.extend_from_slice(b"HELLO");
    let mut cursor = Cursor::new(data);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"HELLO");
}

#[test]
fn dispatch_write_fixed_format() {
    let mut output = Vec::new();
    write_record(&mut output, b"TEST", RecordFormat::Fixed).unwrap();
    assert_eq!(output, b"TEST");
}

#[test]
fn dispatch_write_rdw_format() {
    let mut output = Vec::new();
    write_record(&mut output, b"DATA", RecordFormat::RDW).unwrap();
    assert_eq!(output, vec![0x00, 0x04, 0x00, 0x00, b'D', b'A', b'T', b'A']);
}

// ---------------------------------------------------------------------------
// Fixed format roundtrip through dispatch
// ---------------------------------------------------------------------------

#[test]
fn dispatch_fixed_roundtrip_exact_match() {
    let payload = b"EXACTLY_EIGHT";
    let lrecl = payload.len() as u32;
    let mut encoded = Vec::new();
    write_record(&mut encoded, payload, RecordFormat::Fixed).unwrap();

    let mut cursor = Cursor::new(encoded);
    let decoded = read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
        .unwrap()
        .unwrap();
    assert_eq!(decoded.as_slice(), payload.as_slice());
}

#[test]
fn dispatch_fixed_roundtrip_binary_data() {
    let payload: Vec<u8> = (0u8..=127).collect();
    let lrecl = payload.len() as u32;
    let mut encoded = Vec::new();
    write_record(&mut encoded, &payload, RecordFormat::Fixed).unwrap();

    let mut cursor = Cursor::new(encoded);
    let decoded = read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

// ---------------------------------------------------------------------------
// RDW format roundtrip through dispatch
// ---------------------------------------------------------------------------

#[test]
fn dispatch_rdw_roundtrip_simple() {
    let payload = b"RDW_PAYLOAD";
    let mut encoded = Vec::new();
    write_record(&mut encoded, payload, RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(encoded);
    let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(decoded.as_slice(), payload.as_slice());
}

#[test]
fn dispatch_rdw_roundtrip_empty_payload() {
    let mut encoded = Vec::new();
    write_record(&mut encoded, b"", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(encoded);
    let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert!(decoded.is_empty());
}

#[test]
fn dispatch_rdw_roundtrip_binary() {
    let payload: Vec<u8> = (0u8..=255).collect();
    let mut encoded = Vec::new();
    write_record(&mut encoded, &payload, RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(encoded);
    let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

// ---------------------------------------------------------------------------
// Error propagation from underlying readers
// ---------------------------------------------------------------------------

#[test]
fn dispatch_fixed_missing_lrecl_propagates_error() {
    let mut cursor = Cursor::new(b"test".to_vec());
    let err = read_record(&mut cursor, RecordFormat::Fixed, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn dispatch_fixed_partial_record_propagates_underflow() {
    let data = b"SHORT"; // 5 bytes, LRECL=10
    let mut cursor = Cursor::new(data.to_vec());
    let err = read_record(&mut cursor, RecordFormat::Fixed, Some(10)).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKF221_RDW_UNDERFLOW);
}

#[test]
fn dispatch_rdw_incomplete_header_propagates_error() {
    let data = vec![0x00, 0x0A, 0x00]; // only 3 bytes of header
    let mut cursor = Cursor::new(data);
    // In lenient mode, incomplete header at EOF is treated as EOF (None)
    let result = read_record(&mut cursor, RecordFormat::RDW, None).unwrap();
    assert!(result.is_none());
}

#[test]
fn dispatch_rdw_oversize_write_propagates_error() {
    let oversized = vec![0u8; (u16::MAX as usize) + 1];
    let mut output = Vec::new();
    let err = write_record(&mut output, &oversized, RecordFormat::RDW).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ---------------------------------------------------------------------------
// Streaming through both formats
// ---------------------------------------------------------------------------

#[test]
fn streaming_fixed_multiple_records() {
    let lrecl = 20u32;
    let payloads: Vec<Vec<u8>> = (0..5)
        .map(|i| {
            let mut p = vec![b'A' + i; 15];
            p.resize(lrecl as usize, 0);
            p
        })
        .collect();

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for p in &payloads {
            writer.write_record(p).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    for expected in &payloads {
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec, *expected);
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn streaming_rdw_multiple_records() {
    let payloads: Vec<&[u8]> = vec![b"one", b"two two", b"three three three"];
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
        let rec = reader.read_record().unwrap().unwrap();
        assert_eq!(rec.payload.as_slice(), *expected);
    }
    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn streaming_fixed_100_records() {
    let lrecl = 50u32;
    let count = 100;
    let payload = vec![0xAA; lrecl as usize];

    let mut encoded = Vec::new();
    {
        let mut writer = FixedRecordWriter::new(&mut encoded, Some(lrecl)).unwrap();
        for _ in 0..count {
            writer.write_record(&payload).unwrap();
        }
        writer.flush().unwrap();
        assert_eq!(writer.record_count(), count);
    }

    let mut reader = FixedRecordReader::new(Cursor::new(&encoded), Some(lrecl)).unwrap();
    let mut read_count = 0u64;
    while reader.read_record().unwrap().is_some() {
        read_count += 1;
    }
    assert_eq!(read_count, count);
}

#[test]
fn streaming_rdw_100_records_variable_sizes() {
    let count = 100;
    let mut encoded = Vec::new();
    {
        let mut writer = RDWRecordWriter::new(&mut encoded);
        for i in 0..count {
            let payload = vec![(i % 256) as u8; (i % 50) + 1];
            writer.write_record_from_payload(&payload, None).unwrap();
        }
        writer.flush().unwrap();
    }

    let mut reader = RDWRecordReader::new(Cursor::new(&encoded), false);
    let mut read_count = 0usize;
    while let Some(rec) = reader.read_record().unwrap() {
        let expected_len = (read_count % 50) + 1;
        assert_eq!(rec.payload.len(), expected_len);
        read_count += 1;
    }
    assert_eq!(read_count, count);
}

// ---------------------------------------------------------------------------
// Cross-format consistency
// ---------------------------------------------------------------------------

#[test]
fn same_payload_different_formats_same_content() {
    let payload = b"CROSS_FORMAT_TEST_DATA";

    // Write as fixed
    let mut fixed_out = Vec::new();
    write_record(&mut fixed_out, payload, RecordFormat::Fixed).unwrap();

    // Write as RDW
    let mut rdw_out = Vec::new();
    write_record(&mut rdw_out, payload, RecordFormat::RDW).unwrap();

    // RDW has 4-byte header overhead
    assert_eq!(rdw_out.len(), fixed_out.len() + 4);

    // Read back both
    let mut fixed_cursor = Cursor::new(fixed_out);
    let fixed_decoded = read_record(
        &mut fixed_cursor,
        RecordFormat::Fixed,
        Some(payload.len() as u32),
    )
    .unwrap()
    .unwrap();

    let mut rdw_cursor = Cursor::new(rdw_out);
    let rdw_decoded = read_record(&mut rdw_cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();

    assert_eq!(fixed_decoded, rdw_decoded);
}

#[test]
fn both_formats_handle_empty_stream() {
    let mut fixed_cursor = Cursor::new(Vec::<u8>::new());
    let fixed_result = read_record(&mut fixed_cursor, RecordFormat::Fixed, Some(10)).unwrap();
    assert!(fixed_result.is_none());

    let mut rdw_cursor = Cursor::new(Vec::<u8>::new());
    let rdw_result = read_record(&mut rdw_cursor, RecordFormat::RDW, None).unwrap();
    assert!(rdw_result.is_none());
}

#[test]
fn both_formats_preserve_binary_data() {
    let binary_payload: Vec<u8> = (0u8..=255).collect();

    // Fixed format
    let mut fixed_out = Vec::new();
    write_record(&mut fixed_out, &binary_payload, RecordFormat::Fixed).unwrap();
    let mut fixed_cursor = Cursor::new(fixed_out);
    let fixed_decoded = read_record(
        &mut fixed_cursor,
        RecordFormat::Fixed,
        Some(binary_payload.len() as u32),
    )
    .unwrap()
    .unwrap();

    // RDW format
    let mut rdw_out = Vec::new();
    write_record(&mut rdw_out, &binary_payload, RecordFormat::RDW).unwrap();
    let mut rdw_cursor = Cursor::new(rdw_out);
    let rdw_decoded = read_record(&mut rdw_cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();

    assert_eq!(fixed_decoded, binary_payload);
    assert_eq!(rdw_decoded, binary_payload);
    assert_eq!(fixed_decoded, rdw_decoded);
}

#[test]
fn fixed_write_is_passthrough_rdw_adds_header() {
    let payload = b"CHECK";

    let mut fixed_out = Vec::new();
    write_record(&mut fixed_out, payload, RecordFormat::Fixed).unwrap();
    // Fixed write is raw passthrough (no padding in dispatch API)
    assert_eq!(fixed_out, payload);

    let mut rdw_out = Vec::new();
    write_record(&mut rdw_out, payload, RecordFormat::RDW).unwrap();
    // RDW prepends 4-byte header
    assert_eq!(rdw_out.len(), payload.len() + 4);
    assert_eq!(&rdw_out[4..], payload);
}

// ---------------------------------------------------------------------------
// Re-exported types work correctly
// ---------------------------------------------------------------------------

#[test]
fn reexported_fixed_reader_works() {
    let data = b"12345678";
    let mut reader = FixedRecordReader::new(Cursor::new(data.to_vec()), Some(8)).unwrap();
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec, b"12345678");
}

#[test]
fn reexported_fixed_writer_works() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).unwrap();
    writer.write_record(b"AB").unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![b'A', b'B', 0, 0]);
}

#[test]
fn reexported_rdw_reader_works() {
    let data = vec![0x00, 0x03, 0x00, 0x00, b'X', b'Y', b'Z'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let rec = reader.read_record().unwrap().unwrap();
    assert_eq!(rec.payload, b"XYZ");
}

#[test]
fn reexported_rdw_writer_works() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer.write_record_from_payload(b"AB", None).unwrap();
    writer.flush().unwrap();
    assert_eq!(output, vec![0x00, 0x02, 0x00, 0x00, b'A', b'B']);
}
