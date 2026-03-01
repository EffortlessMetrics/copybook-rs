// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Integration tests for record-io dispatch covering fixed-length reading,
//! RDW reading, format dispatch logic, and error conditions.

use copybook_error::ErrorCode;
use copybook_options::RecordFormat;
use copybook_record_io::{FixedRecordReader, RDWRecordReader, read_record, write_record};
use std::io::{self, Cursor, Read, Write};

// ===========================================================================
// 1. Fixed-length record dispatch
// ===========================================================================

#[test]
fn fixed_reads_exact_lrecl_single_record() {
    let data = b"ABCDEFGH";
    let mut cursor = Cursor::new(data.to_vec());
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(8))
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"ABCDEFGH");
    // No more records
    assert!(
        read_record(&mut cursor, RecordFormat::Fixed, Some(8))
            .unwrap()
            .is_none()
    );
}

#[test]
fn fixed_reads_multiple_records_in_sequence() {
    // 3 records of LRECL=5
    let data = b"AAAAABBBBBCCCCC";
    let mut cursor = Cursor::new(data.to_vec());

    let r1 = read_record(&mut cursor, RecordFormat::Fixed, Some(5))
        .unwrap()
        .unwrap();
    let r2 = read_record(&mut cursor, RecordFormat::Fixed, Some(5))
        .unwrap()
        .unwrap();
    let r3 = read_record(&mut cursor, RecordFormat::Fixed, Some(5))
        .unwrap()
        .unwrap();

    assert_eq!(r1, b"AAAAA");
    assert_eq!(r2, b"BBBBB");
    assert_eq!(r3, b"CCCCC");

    assert!(
        read_record(&mut cursor, RecordFormat::Fixed, Some(5))
            .unwrap()
            .is_none()
    );
}

#[test]
fn fixed_partial_final_record_returns_error() {
    // 7 bytes with LRECL=5: one full record + 2 leftover bytes
    let data = b"AAAAABB";
    let mut cursor = Cursor::new(data.to_vec());

    // First record succeeds
    let r1 = read_record(&mut cursor, RecordFormat::Fixed, Some(5))
        .unwrap()
        .unwrap();
    assert_eq!(r1, b"AAAAA");

    // Second read fails because only 2 bytes remain (< LRECL of 5)
    let result = read_record(&mut cursor, RecordFormat::Fixed, Some(5));
    assert!(result.is_err(), "partial final record should be an error");
}

#[test]
fn fixed_empty_input_returns_none() {
    let mut cursor = Cursor::new(Vec::<u8>::new());
    let result = read_record(&mut cursor, RecordFormat::Fixed, Some(10)).unwrap();
    assert!(result.is_none(), "empty input must return None");
}

#[test]
fn fixed_single_byte_record() {
    let mut cursor = Cursor::new(vec![0xFF]);
    let rec = read_record(&mut cursor, RecordFormat::Fixed, Some(1))
        .unwrap()
        .unwrap();
    assert_eq!(rec, vec![0xFF]);
}

// ===========================================================================
// 2. RDW record dispatch
// ===========================================================================

/// Helper: build a raw RDW frame (4-byte header + payload).
fn build_rdw_frame(payload: &[u8]) -> Vec<u8> {
    let len = u16::try_from(payload.len()).unwrap();
    let mut frame = Vec::with_capacity(4 + payload.len());
    frame.extend_from_slice(&len.to_be_bytes());
    frame.extend_from_slice(&[0x00, 0x00]); // reserved
    frame.extend_from_slice(payload);
    frame
}

#[test]
fn rdw_valid_header_single_record() {
    let mut data = Vec::new();
    write_record(&mut data, b"HELLO", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"HELLO");
}

#[test]
fn rdw_multiple_variable_length_records_via_reader() {
    // read_record creates a new BufReader per call, so for streaming
    // multiple RDW records use the re-exported RDWRecordReader directly.
    let mut data = Vec::new();
    write_record(&mut data, b"A", RecordFormat::RDW).unwrap();
    write_record(&mut data, b"BCDE", RecordFormat::RDW).unwrap();
    write_record(&mut data, b"FG", RecordFormat::RDW).unwrap();

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let r1 = reader.read_record().unwrap().unwrap();
    let r2 = reader.read_record().unwrap().unwrap();
    let r3 = reader.read_record().unwrap().unwrap();

    assert_eq!(r1.payload, b"A");
    assert_eq!(r2.payload, b"BCDE");
    assert_eq!(r3.payload, b"FG");

    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn rdw_minimum_size_header_only_empty_payload() {
    // RDW with length=0 (header-only, empty payload)
    let frame = build_rdw_frame(b"");
    let mut cursor = Cursor::new(frame);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert!(rec.is_empty());
}

#[test]
fn rdw_boundary_single_byte_payload() {
    let mut data = Vec::new();
    write_record(&mut data, b"X", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"X");
}

#[test]
fn rdw_empty_stream_returns_none() {
    let mut cursor = Cursor::new(Vec::<u8>::new());
    let result = read_record(&mut cursor, RecordFormat::RDW, None).unwrap();
    assert!(result.is_none());
}

#[test]
fn rdw_large_payload_roundtrip() {
    // Test with a payload close to typical mainframe record sizes
    let payload = vec![0xAB; 32_000];
    let mut data = Vec::new();
    write_record(&mut data, &payload, RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec.len(), 32_000);
    assert!(rec.iter().all(|&b| b == 0xAB));
}

// ===========================================================================
// 3. Format dispatch logic
// ===========================================================================

#[test]
fn dispatch_selects_fixed_format_correctly() {
    let payload = b"FIXEDRECORD";
    let lrecl = u32::try_from(payload.len()).unwrap();

    let mut buf = Vec::new();
    write_record(&mut buf, payload, RecordFormat::Fixed).unwrap();
    // Fixed write is a direct passthrough
    assert_eq!(buf, payload);

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

#[test]
fn dispatch_selects_rdw_format_correctly() {
    let payload = b"RDWRECORD";

    let mut buf = Vec::new();
    write_record(&mut buf, payload, RecordFormat::RDW).unwrap();
    // RDW write produces header + payload (more bytes than input)
    assert_eq!(buf.len(), 4 + payload.len());

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

#[test]
fn dispatch_rdw_ignores_lrecl_configuration() {
    let mut data = Vec::new();
    write_record(&mut data, b"IGNORE_LRECL", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    // Pass an arbitrary lrecl; RDW dispatch should ignore it
    let rec = read_record(&mut cursor, RecordFormat::RDW, Some(42))
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"IGNORE_LRECL");
}

#[test]
fn dispatch_fixed_requires_lrecl_propagates_error() {
    let mut cursor = Cursor::new(b"DATA".to_vec());
    let err = read_record(&mut cursor, RecordFormat::Fixed, None).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

// ===========================================================================
// 4. Error conditions
// ===========================================================================

/// A reader that always returns an I/O error.
struct FailingReader;
impl Read for FailingReader {
    fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
        Err(io::Error::new(
            io::ErrorKind::BrokenPipe,
            "simulated read failure",
        ))
    }
}

/// A writer that always returns an I/O error.
struct FailingWriter;
impl Write for FailingWriter {
    fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
        Err(io::Error::new(
            io::ErrorKind::BrokenPipe,
            "simulated write failure",
        ))
    }
    fn flush(&mut self) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::BrokenPipe,
            "simulated flush failure",
        ))
    }
}

#[test]
fn error_fixed_record_shorter_than_lrecl() {
    // 3 bytes available but LRECL requires 8
    let mut cursor = Cursor::new(b"ABC".to_vec());
    let result = read_record(&mut cursor, RecordFormat::Fixed, Some(8));
    assert!(result.is_err(), "truncated fixed record must error");
}

#[test]
fn error_rdw_corrupted_header_truncated() {
    // Only 2 bytes instead of 4-byte RDW header.
    // In lenient mode (default), the reader treats incomplete headers as EOF.
    let mut cursor = Cursor::new(vec![0x00, 0x05]);
    let result = read_record(&mut cursor, RecordFormat::RDW, None).unwrap();
    assert!(
        result.is_none(),
        "truncated RDW header in lenient mode returns None (EOF)"
    );
}

#[test]
fn error_rdw_truncated_header_strict_mode_errors() {
    // In strict mode, an incomplete header is an error.
    let mut reader = RDWRecordReader::new(Cursor::new(vec![0x00, 0x05]), true);
    let result = reader.read_record();
    assert!(
        result.is_err(),
        "truncated header in strict mode must error"
    );
}

#[test]
fn error_rdw_header_length_exceeds_available_data() {
    // RDW header says payload is 10 bytes, but only 2 bytes follow
    let mut data = Vec::new();
    data.extend_from_slice(&[0x00, 0x0A, 0x00, 0x00]); // length=10
    data.extend_from_slice(b"AB"); // only 2 bytes
    let mut cursor = Cursor::new(data);
    let result = read_record(&mut cursor, RecordFormat::RDW, None);
    assert!(
        result.is_err(),
        "RDW with insufficient payload data must error"
    );
}

#[test]
fn error_io_propagates_on_fixed_read() {
    let mut reader = FailingReader;
    let err = read_record(&mut reader, RecordFormat::Fixed, Some(4));
    assert!(err.is_err());
}

#[test]
fn error_io_propagates_on_rdw_read() {
    let mut reader = FailingReader;
    let err = read_record(&mut reader, RecordFormat::RDW, None);
    assert!(err.is_err());
}

#[test]
fn error_io_propagates_on_fixed_write() {
    let mut writer = FailingWriter;
    let err = write_record(&mut writer, b"DATA", RecordFormat::Fixed);
    assert!(err.is_err());
}

#[test]
fn error_io_propagates_on_rdw_write() {
    let mut writer = FailingWriter;
    let err = write_record(&mut writer, b"DATA", RecordFormat::RDW);
    assert!(err.is_err());
}

#[test]
fn error_rdw_oversized_payload_rejected() {
    let mut out = Vec::new();
    let oversized = vec![0u8; usize::from(u16::MAX) + 1];
    let err = write_record(&mut out, &oversized, RecordFormat::RDW).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKE501_JSON_TYPE_MISMATCH);
}

// ===========================================================================
// 5. Additional edge cases
// ===========================================================================

#[test]
fn fixed_preserves_all_byte_values() {
    let payload: Vec<u8> = (0..=255).collect();
    let mut buf = Vec::new();
    write_record(&mut buf, &payload, RecordFormat::Fixed).unwrap();

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::Fixed, Some(256))
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

#[test]
fn rdw_preserves_all_byte_values() {
    let payload: Vec<u8> = (0..=255).collect();
    let mut buf = Vec::new();
    write_record(&mut buf, &payload, RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

#[test]
fn fixed_write_empty_payload_produces_nothing() {
    let mut out = Vec::new();
    write_record(&mut out, b"", RecordFormat::Fixed).unwrap();
    assert!(out.is_empty());
}

#[test]
fn rdw_write_empty_payload_produces_header_only() {
    let mut out = Vec::new();
    write_record(&mut out, b"", RecordFormat::RDW).unwrap();
    assert_eq!(out.len(), 4);
    // length=0, reserved=0
    assert_eq!(&out[..2], &[0x00, 0x00]);
    assert_eq!(&out[2..4], &[0x00, 0x00]);
}

#[test]
fn reexported_fixed_reader_matches_dispatch() {
    let data = b"LOCKTEST";
    // Via dispatch
    let mut c1 = Cursor::new(data.to_vec());
    let via_dispatch = read_record(&mut c1, RecordFormat::Fixed, Some(8))
        .unwrap()
        .unwrap();

    // Via re-exported reader directly
    let mut reader = FixedRecordReader::new(Cursor::new(data.to_vec()), Some(8)).unwrap();
    let via_reader = reader.read_record().unwrap().unwrap();

    assert_eq!(via_dispatch, via_reader);
}

#[test]
fn reexported_rdw_reader_matches_dispatch() {
    let mut data = Vec::new();
    write_record(&mut data, b"MATCH", RecordFormat::RDW).unwrap();

    // Via dispatch
    let mut c1 = Cursor::new(data.clone());
    let via_dispatch = read_record(&mut c1, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();

    // Via re-exported reader directly
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let via_reader = reader.read_record().unwrap().unwrap().payload;

    assert_eq!(via_dispatch, via_reader);
}
