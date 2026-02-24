// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_file_to_jsonl,
    encode_jsonl_to_file,
};
use copybook_core::parse_copybook;
use copybook_fixed::{
    FixedRecordReader as FixedMicroReader, FixedRecordWriter as FixedMicroWriter,
};
use std::io::Cursor;

#[test]
fn fixed_encode_output_is_readable_with_microcrate_reader() {
    let schema = parse_copybook(
        r"
        01 SIMPLE-RECORD PIC X(8).
    ",
    )
    .expect("schema should parse");

    let options = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    let json = r#"{"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"SIMPLE-RECORD":"ABCDEFGH"},"SIMPLE-RECORD":"ABCDEFGH"}"#;
    let mut output = Vec::new();

    encode_jsonl_to_file(&schema, Cursor::new(json.as_bytes()), &mut output, &options)
        .expect("fixed encode should succeed");
    assert_eq!(output.len(), 8);

    let mut reader = FixedMicroReader::new(Cursor::new(output.clone()), Some(8))
        .expect("microcrate reader should build");
    let record = reader.read_record().expect("read should succeed");
    assert_eq!(record.expect("record expected"), output);
}

#[test]
fn fixed_microcrate_writer_output_decodes_with_codec() {
    let schema = parse_copybook(
        r"
        01 SIMPLE-RECORD PIC X(8).
    ",
    )
    .expect("schema should parse");

    let mut fixed_bytes = Vec::new();
    let mut writer = FixedMicroWriter::new(&mut fixed_bytes, Some(8)).expect("writer should build");
    writer
        .write_record(b"ABCD1234")
        .expect("writer should serialize fixed record");
    writer.flush().expect("flush should work");
    drop(writer);
    assert_eq!(fixed_bytes, b"ABCD1234");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    let mut jsonl = Vec::new();
    decode_file_to_jsonl(&schema, Cursor::new(fixed_bytes), &mut jsonl, &options)
        .expect("codec should decode microcrate output");

    let output = String::from_utf8(jsonl).expect("jsonl should be utf8");
    assert!(output.contains("ABCD1234"));
}

#[test]
fn codec_reexports_fixed_microcrate_surface() {
    let mut output = Vec::new();
    let mut writer = copybook_codec::record::FixedRecordWriter::new(&mut output, Some(4))
        .expect("codec re-exported writer should build");
    writer
        .write_record(b"LOCK")
        .expect("codec re-exported writer should write");
    writer.flush().expect("flush should succeed");

    let mut reader = copybook_codec::record::FixedRecordReader::new(Cursor::new(output), Some(4))
        .expect("codec re-exported reader should build");
    let record = reader.read_record().expect("read should succeed");
    assert_eq!(record.expect("record expected"), b"LOCK");
}
