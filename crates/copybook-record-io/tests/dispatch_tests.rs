// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

//! Integration tests for record-io dispatch, error propagation,
//! re-exports, and configuration options.

use copybook_options::RecordFormat;
use copybook_record_io::{
    FixedRecordReader, FixedRecordWriter, RDWRecord, RDWRecordReader, RDWRecordWriter, read_record,
    write_record,
};
use std::io::{self, Cursor, Read, Write};

// ---------------------------------------------------------------------------
// Re-export verification
// ---------------------------------------------------------------------------

#[test]
fn reexport_rdw_record_is_constructible() {
    let record = RDWRecord::try_new(b"HELLO".to_vec()).expect("should construct");
    assert_eq!(record.length(), 5);
    assert_eq!(record.reserved(), 0);
    assert_eq!(record.payload, b"HELLO");
}

#[test]
fn reexport_fixed_reader_writer_roundtrip() {
    let mut buf = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut buf, Some(6)).expect("writer");
    writer.write_record(b"AB").expect("write short record");
    writer.flush().expect("flush");

    let mut reader = FixedRecordReader::new(Cursor::new(buf), Some(6)).expect("reader");
    let rec = reader.read_record().expect("read").expect("record");
    // FixedRecordWriter pads to lrecl with 0x00
    assert_eq!(rec.len(), 6);
    assert_eq!(&rec[..2], b"AB");
}

#[test]
fn reexport_rdw_reader_writer_roundtrip() {
    let mut buf = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut buf);
    writer
        .write_record_from_payload(b"XYZ", None)
        .expect("write");

    let mut reader = RDWRecordReader::new(Cursor::new(buf), false);
    let rec = reader.read_record().expect("read").expect("record");
    assert_eq!(rec.payload, b"XYZ");
}

// ---------------------------------------------------------------------------
// Format dispatch: fixed vs RDW selection
// ---------------------------------------------------------------------------

#[test]
fn dispatch_fixed_reads_exact_lrecl_bytes() {
    let data = b"AAAABBBB";
    let mut cursor = Cursor::new(data.to_vec());

    let first = read_record(&mut cursor, RecordFormat::Fixed, Some(4))
        .unwrap()
        .unwrap();
    assert_eq!(first, b"AAAA");

    let second = read_record(&mut cursor, RecordFormat::Fixed, Some(4))
        .unwrap()
        .unwrap();
    assert_eq!(second, b"BBBB");
}

#[test]
fn dispatch_rdw_reads_single_variable_length_record() {
    // Build one RDW record using write_record to ensure correct framing
    let mut data = Vec::new();
    write_record(&mut data, b"VARIABLE", RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(data);
    let rec = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(rec, b"VARIABLE");

    // After the only record, next read returns None
    let eof = read_record(&mut cursor, RecordFormat::RDW, None).unwrap();
    assert!(eof.is_none());
}

#[test]
fn dispatch_rdw_multiple_records_via_reexported_reader() {
    // For streaming multiple RDW records, use the re-exported reader directly
    let mut data = Vec::new();
    write_record(&mut data, b"AB", RecordFormat::RDW).unwrap();
    write_record(&mut data, b"CDEF", RecordFormat::RDW).unwrap();

    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let first = reader.read_record().unwrap().unwrap();
    assert_eq!(first.payload, b"AB");

    let second = reader.read_record().unwrap().unwrap();
    assert_eq!(second.payload, b"CDEF");

    assert!(reader.read_record().unwrap().is_none());
}

#[test]
fn dispatch_write_fixed_then_read_fixed_roundtrip() {
    let payload = b"ROUNDTRIP";
    let lrecl = u32::try_from(payload.len()).unwrap();

    let mut buf = Vec::new();
    write_record(&mut buf, payload, RecordFormat::Fixed).unwrap();

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

#[test]
fn dispatch_write_rdw_then_read_rdw_roundtrip() {
    let payload = b"VARIABLE";

    let mut buf = Vec::new();
    write_record(&mut buf, payload, RecordFormat::RDW).unwrap();

    let mut cursor = Cursor::new(buf);
    let decoded = read_record(&mut cursor, RecordFormat::RDW, None)
        .unwrap()
        .unwrap();
    assert_eq!(decoded, payload);
}

// ---------------------------------------------------------------------------
// Empty / EOF behaviour
// ---------------------------------------------------------------------------

#[test]
fn read_fixed_empty_input_returns_none() {
    let mut cursor = Cursor::new(Vec::<u8>::new());
    let result = read_record(&mut cursor, RecordFormat::Fixed, Some(4)).unwrap();
    assert!(result.is_none(), "empty input should yield None");
}

#[test]
fn read_rdw_empty_input_returns_none() {
    let mut cursor = Cursor::new(Vec::<u8>::new());
    let result = read_record(&mut cursor, RecordFormat::RDW, None).unwrap();
    assert!(result.is_none(), "empty input should yield None");
}

// ---------------------------------------------------------------------------
// Error propagation from underlying readers / writers
// ---------------------------------------------------------------------------

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
fn read_fixed_propagates_io_error() {
    let mut reader = FailingReader;
    let err = read_record(&mut reader, RecordFormat::Fixed, Some(8));
    assert!(err.is_err(), "should propagate IO error for fixed format");
}

#[test]
fn read_rdw_propagates_io_error_on_header() {
    let mut reader = FailingReader;
    let err = read_record(&mut reader, RecordFormat::RDW, None);
    assert!(err.is_err(), "should propagate IO error for RDW format");
}

#[test]
fn write_fixed_propagates_io_error() {
    let mut writer = FailingWriter;
    let err = write_record(&mut writer, b"DATA", RecordFormat::Fixed);
    assert!(err.is_err(), "should propagate IO error for fixed write");
}

#[test]
fn write_rdw_propagates_io_error() {
    let mut writer = FailingWriter;
    let err = write_record(&mut writer, b"DATA", RecordFormat::RDW);
    assert!(err.is_err(), "should propagate IO error for RDW write");
}

// ---------------------------------------------------------------------------
// Configuration options
// ---------------------------------------------------------------------------

#[test]
fn record_format_fixed_config_accessors() {
    let fmt = RecordFormat::Fixed;
    assert!(fmt.is_fixed());
    assert!(!fmt.is_variable());
    assert!(!fmt.description().is_empty());
}

#[test]
fn record_format_rdw_config_accessors() {
    let fmt = RecordFormat::RDW;
    assert!(!fmt.is_fixed());
    assert!(fmt.is_variable());
    assert!(!fmt.description().is_empty());
}

#[test]
fn read_rdw_ignores_lrecl_parameter() {
    // Even if lrecl is passed, RDW dispatch ignores it
    let mut data = Vec::new();
    data.extend_from_slice(&[0x00, 0x03, 0x00, 0x00, b'A', b'B', b'C']);

    let mut cursor = Cursor::new(data);
    let record = read_record(&mut cursor, RecordFormat::RDW, Some(999))
        .unwrap()
        .unwrap();
    assert_eq!(record, b"ABC");
}

#[test]
fn read_fixed_truncated_record_returns_error() {
    // Input has only 3 bytes but lrecl expects 8 — partial record is an error
    let mut cursor = Cursor::new(b"ABC".to_vec());
    let result = read_record(&mut cursor, RecordFormat::Fixed, Some(8));
    assert!(
        result.is_err(),
        "truncated fixed record should return error"
    );
}

#[test]
fn write_fixed_empty_payload() {
    let mut output = Vec::new();
    write_record(&mut output, b"", RecordFormat::Fixed).unwrap();
    assert!(output.is_empty(), "empty fixed write produces no output");
}

#[test]
fn write_rdw_empty_payload() {
    let mut output = Vec::new();
    write_record(&mut output, b"", RecordFormat::RDW).unwrap();
    // RDW header is 4 bytes even for empty payload
    assert_eq!(output.len(), 4);
    assert_eq!(&output[..2], &[0x00, 0x00]); // length = 0
}
