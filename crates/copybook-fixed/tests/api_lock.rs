// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

#[test]
fn facade_exposes_fixed_reader_surface() {
    let data = Cursor::new(b"ABCD1234".to_vec());
    let mut reader = FixedRecordReader::new(data, Some(8)).expect("reader should build");
    let record = reader.read_record().expect("read should succeed");
    assert_eq!(record.expect("first record"), b"ABCD1234");
    assert_eq!(reader.record_count(), 1);
}

#[test]
fn facade_exposes_fixed_writer_surface() {
    let mut output = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut output, Some(8)).expect("writer should build");
    writer.write_record(b"API").expect("write should succeed");
    assert_eq!(writer.record_count(), 1);
    writer.flush().expect("flush should succeed");
    drop(writer);
    assert_eq!(output.len(), 8);
    assert_eq!(&output[0..3], b"API");
}
