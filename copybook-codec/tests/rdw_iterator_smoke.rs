#![allow(clippy::expect_used)]
use copybook_codec::record::RDWRecordReader;
use copybook_core::ErrorCode;
use std::io::Cursor;

#[test]
fn rdw_reader_yields_multiple_records_in_order() {
    let data = [
        0x00, 0x03, 0x00, 0x00, b'A', b'B', b'C', //
        0x00, 0x02, 0x00, 0x00, b'D', b'E',
    ];
    let mut reader = RDWRecordReader::new(Cursor::new(&data[..]), false);

    let first = reader
        .read_record()
        .expect("first RDW read should succeed")
        .expect("first RDW record should exist");
    assert_eq!(first.length(), 3);
    assert_eq!(first.payload, b"ABC");

    let second = reader
        .read_record()
        .expect("second RDW read should succeed")
        .expect("second RDW record should exist");
    assert_eq!(second.length(), 2);
    assert_eq!(second.payload, b"DE");

    assert!(
        reader
            .read_record()
            .expect("EOF read should not error")
            .is_none(),
        "third read should signal EOF"
    );
}

#[test]
fn rdw_reader_surfaces_cbkf102_with_context() {
    let data = [
        0x00, 0x03, 0x00, 0x00, b'A', b'B', b'C', //
        0x00, 0x05, 0x00, 0x00, b'D', b'E', b'F', b'G',
    ];
    let mut reader = RDWRecordReader::new(Cursor::new(&data[..]), true);

    let first = reader
        .read_record()
        .expect("first RDW read should succeed")
        .expect("first RDW record should exist");
    assert_eq!(first.payload, b"ABC");
    assert_eq!(reader.record_count(), 1);

    let err = reader
        .read_record()
        .expect_err("truncated second record should error");
    assert_eq!(err.code, ErrorCode::CBKF102_RECORD_LENGTH_INVALID);

    let context = err.context.expect("context should be populated");
    assert_eq!(context.record_index, Some(2));
    assert_eq!(context.byte_offset, Some(4));
}
