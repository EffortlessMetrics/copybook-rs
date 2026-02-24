// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_rdw::{
    RDW_HEADER_LEN, RDWRecord, RDWRecordReader, RDWRecordWriter, RdwHeader, rdw_payload_len_to_u16,
};
use std::io::Cursor;

#[test]
fn facade_exposes_rdw_record_surface() {
    let record = RDWRecord::try_with_reserved(b"LOCK".to_vec(), 0x0102)
        .expect("record should be constructible");
    assert_eq!(record.length(), 4);
    assert_eq!(record.reserved(), 0x0102);
    assert_eq!(record.as_bytes().len(), RDW_HEADER_LEN + 4);
}

#[test]
fn facade_exposes_rdw_writer_surface() {
    let mut output = Vec::new();
    let mut writer = RDWRecordWriter::new(&mut output);
    writer
        .write_record_from_payload(b"API", Some(0))
        .expect("writer should serialize payload");
    assert_eq!(writer.record_count(), 1);
    drop(writer);
    let header = RdwHeader::from_bytes(output[0..RDW_HEADER_LEN].try_into().expect("header"));
    assert_eq!(header.length(), 3);
}

#[test]
fn facade_exposes_rdw_reader_surface() {
    let data = vec![0x00, 0x03, 0x00, 0x00, b'A', b'P', b'I'];
    let mut reader = RDWRecordReader::new(Cursor::new(data), false);
    let record = reader
        .read_record()
        .expect("reader should parse one record")
        .expect("record expected");
    assert_eq!(record.payload, b"API");
    assert_eq!(reader.record_count(), 1);
}

#[test]
fn facade_exposes_length_conversion_function() {
    let len = rdw_payload_len_to_u16(10).expect("10 fits in u16");
    assert_eq!(len, 10);
}
