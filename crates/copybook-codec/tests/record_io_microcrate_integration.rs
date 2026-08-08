// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::record::{
    read_rdw_record, read_record as codec_read_record, write_record as codec_write_record,
};
use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_file_to_jsonl,
    encode_jsonl_to_file,
};
use copybook_core::{ErrorCode, parse_copybook};
use std::io::{self, Cursor, Write};

struct FailingWriter;

impl Write for FailingWriter {
    fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
        Err(io::Error::new(
            io::ErrorKind::BrokenPipe,
            "simulated write failure",
        ))
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[test]
fn codec_facade_keeps_record_io_error_contract() {
    let mut input = Cursor::new(b"ABCD".to_vec());
    let err = copybook_codec::record::read_record(&mut input, RecordFormat::Fixed, None)
        .expect_err("fixed dispatch should require lrecl");
    assert_eq!(err.code, ErrorCode::CBKI001_INVALID_STATE);
}

#[test]
fn fixed_dispatch_maps_write_failure_to_fixed_framing_error() {
    let error = codec_write_record(&mut FailingWriter, b"DATA", RecordFormat::Fixed)
        .expect_err("fixed dispatch should report a failed writer");

    assert_eq!(error.code, ErrorCode::CBKR101_FIXED_RECORD_ERROR);
    assert!(error.message.contains("simulated write failure"));
}

#[test]
fn fixed_encode_output_roundtrips_through_codec_dispatch() {
    let schema = parse_copybook(
        r"
        01 FIXED-RECORD PIC X(8).
    ",
    )
    .expect("schema should parse");

    let options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    let json = r#"{"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"FIXED-RECORD":"ABCDEFGH"},"FIXED-RECORD":"ABCDEFGH"}"#;
    let mut encoded = Vec::new();
    encode_jsonl_to_file(
        &schema,
        Cursor::new(json.as_bytes()),
        &mut encoded,
        &options,
    )
    .expect("fixed encode should succeed");

    let mut cursor = Cursor::new(encoded.clone());
    let payload = codec_read_record(&mut cursor, RecordFormat::Fixed, Some(8))
        .expect("codec dispatch should parse fixed record")
        .expect("one fixed record expected");
    assert_eq!(payload, b"ABCDEFGH");

    let mut rebuilt = Vec::new();
    codec_write_record(&mut rebuilt, &payload, RecordFormat::Fixed)
        .expect("codec dispatch should serialize fixed payload");
    assert_eq!(rebuilt, encoded);
    assert!(
        codec_read_record(&mut cursor, RecordFormat::Fixed, Some(8))
            .expect("EOF read should succeed")
            .is_none()
    );
}

#[test]
fn rdw_encode_output_roundtrips_through_codec_dispatch() {
    let schema = parse_copybook(
        r"
        01 RDW-RECORD PIC X(5).
    ",
    )
    .expect("schema should parse");

    let options = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII);

    let json = r#"{"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"RDW-RECORD":"HELLO"},"RDW-RECORD":"HELLO"}"#;
    let mut encoded = Vec::new();
    encode_jsonl_to_file(
        &schema,
        Cursor::new(json.as_bytes()),
        &mut encoded,
        &options,
    )
    .expect("rdw encode should succeed");

    let mut cursor = Cursor::new(encoded.clone());
    let payload = codec_read_record(&mut cursor, RecordFormat::RDW, None)
        .expect("codec dispatch should parse rdw record")
        .expect("one rdw record expected");
    assert_eq!(payload, b"HELLO");

    let mut rebuilt = Vec::new();
    codec_write_record(&mut rebuilt, &payload, RecordFormat::RDW)
        .expect("codec dispatch should serialize rdw payload");
    assert_eq!(rebuilt, encoded);
}

#[test]
fn codec_dispatch_output_decodes_with_codec() {
    let schema = parse_copybook(
        r"
        01 SIMPLE-RECORD PIC X(5).
    ",
    )
    .expect("schema should parse");

    let mut encoded = Vec::new();
    codec_write_record(&mut encoded, b"HELLO", RecordFormat::RDW)
        .expect("codec dispatch should emit rdw-framed payload");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII);

    let mut jsonl = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(encoded), &mut jsonl, &options)
        .expect("codec should decode microcrate output");

    let output = String::from_utf8(jsonl).expect("jsonl should be utf8");
    assert!(output.contains("HELLO"));
}

#[test]
fn codec_lossless_rdw_dispatch_preserves_reserved_header_bytes() {
    let wire = [0x00, 0x05, 0x12, 0x34, b'H', b'E', b'L', b'L', b'O'];
    let mut input = Cursor::new(wire);

    let record = read_rdw_record(&mut input, false)
        .expect("lossless rdw dispatch should succeed")
        .expect("one rdw record expected");

    assert_eq!(record.header, [0x00, 0x05, 0x12, 0x34]);
    assert_eq!(record.payload, b"HELLO");
    assert_eq!(record.as_bytes(), wire);
}
