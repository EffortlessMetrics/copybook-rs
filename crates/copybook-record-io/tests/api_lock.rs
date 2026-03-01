// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_options::RecordFormat;
use copybook_record_io::{
    FixedRecordReader, FixedRecordWriter, RDWRecordReader, RDWRecordWriter, read_record,
    write_record,
};
use std::io::Cursor;

#[test]
fn facade_exposes_fixed_record_surface() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(4)).expect("writer should build");
    writer
        .write_record(b"LOCK")
        .expect("writer should accept exact-length payload");
    writer.flush().expect("flush should work");

    let mut reader =
        FixedRecordReader::new(Cursor::new(output), Some(4)).expect("reader should build");
    let record = reader
        .read_record()
        .expect("read should succeed")
        .expect("record expected");
    assert_eq!(record, b"LOCK");
}

#[test]
fn facade_exposes_rdw_record_surface() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer
        .write_record_from_payload(b"RDW", Some(0))
        .expect("writer should encode payload");

    let mut reader = RDWRecordReader::new(Cursor::new(output), false);
    let record = reader
        .read_record()
        .expect("read should succeed")
        .expect("record expected");
    assert_eq!(record.payload, b"RDW");
}

#[test]
fn facade_exposes_dispatch_functions() {
    let mut output = Vec::new();
    write_record(&mut output, b"ABCD", RecordFormat::RDW).expect("write should succeed");

    let mut cursor = Cursor::new(output);
    let record = read_record(&mut cursor, RecordFormat::RDW, None)
        .expect("read should succeed")
        .expect("record expected");
    assert_eq!(record, b"ABCD");
}
