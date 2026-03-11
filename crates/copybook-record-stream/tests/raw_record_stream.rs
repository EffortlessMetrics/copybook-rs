// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_record_stream::{RawRecordStream, RecordStreamFormat};
use std::io::Cursor;

#[test]
fn fixed_stream_reads_records() {
    let data = b"001ALICE002BOB  ";
    let mut stream = RawRecordStream::new(Cursor::new(data), RecordStreamFormat::Fixed, Some(8))
        .expect("stream should initialize");

    let first = stream
        .read_raw_record()
        .expect("first read should succeed")
        .expect("first record should exist");
    assert_eq!(first, b"001ALICE");
    assert_eq!(stream.current_record_index(), 1);

    let second = stream
        .read_raw_record()
        .expect("second read should succeed")
        .expect("second record should exist");
    assert_eq!(second, b"002BOB  ");
    assert_eq!(stream.current_record_index(), 2);

    assert!(
        stream
            .read_raw_record()
            .expect("eof read should succeed")
            .is_none()
    );
    assert!(stream.is_eof());
}

#[test]
fn rdw_stream_reads_payloads() {
    let data = vec![
        0x00, 0x05, 0x00, 0x00, b'H', b'E', b'L', b'L', b'O', 0x00, 0x03, 0x00, 0x00, b'B', b'Y',
        b'E',
    ];
    let mut stream = RawRecordStream::new(Cursor::new(data), RecordStreamFormat::RDW, None)
        .expect("stream should initialize");

    assert_eq!(
        stream
            .read_raw_record()
            .expect("first read should succeed")
            .expect("first record exists"),
        b"HELLO"
    );
    assert_eq!(
        stream
            .read_raw_record()
            .expect("second read should succeed")
            .expect("second record exists"),
        b"BYE"
    );
}
