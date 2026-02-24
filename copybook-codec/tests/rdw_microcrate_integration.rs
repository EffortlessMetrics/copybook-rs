// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_file_to_jsonl,
    encode_jsonl_to_file,
};
use copybook_core::{ErrorCode, parse_copybook};
use copybook_rdw::{RDWRecord, RDWRecordReader, RdwHeader};
use std::io::Cursor;

#[test]
fn rdw_decode_rejects_ascii_corrupted_header() {
    let schema = parse_copybook(
        r"
        01 TEST-RECORD.
           05 TEST-FIELD PIC X(5).
    ",
    )
    .expect("schema should parse");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII);

    // RDW length bytes are ASCII digits ('1''2') => ASCII-corruption heuristic.
    let input = Cursor::new(vec![b'1', b'2', 0, 0, b'H', b'E', b'L', b'L', b'O']);
    let mut output = Vec::new();

    let err = decode_file_to_jsonl(&schema, input, &mut output, &options)
        .expect_err("ASCII-corrupted RDW header should fail decode");
    assert_eq!(err.code, ErrorCode::CBKF104_RDW_SUSPECT_ASCII);
    assert!(err.message.contains("ASCII-corrupted"));
}

#[test]
fn rdw_reserved_nonzero_increments_run_summary_warnings() {
    let schema = parse_copybook(
        r"
        01 SIMPLE-RECORD PIC X(5).
    ",
    )
    .expect("schema should parse");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(false);

    // Baseline: reserved bytes are zero, so warning count should be unchanged.
    let mut out0 = Vec::new();
    let summary0 = decode_file_to_jsonl(
        &schema,
        Cursor::new(vec![0x00, 0x05, 0x00, 0x00, b'H', b'E', b'L', b'L', b'O']),
        &mut out0,
        &options,
    )
    .expect("decode should succeed");
    let baseline_warnings = summary0.warnings;

    // Non-zero reserved bytes should decode in lenient mode and increment warnings.
    let mut out1 = Vec::new();
    let summary1 = decode_file_to_jsonl(
        &schema,
        Cursor::new(vec![0x00, 0x05, 0x12, 0x34, b'H', b'E', b'L', b'L', b'O']),
        &mut out1,
        &options,
    )
    .expect("decode should succeed in lenient mode");

    assert_eq!(summary1.warnings, baseline_warnings + 1);
    assert!(
        String::from_utf8(out1)
            .expect("decoded jsonl should be valid UTF-8")
            .contains("HELLO")
    );
}

#[test]
fn rdw_encode_emits_header_consistent_with_microcrate() {
    let schema = parse_copybook(
        r"
        01 SIMPLE-RECORD PIC X(5).
    ",
    )
    .expect("schema should parse");

    let options = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII);

    let json = r#"{"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"SIMPLE-RECORD":"HELLO"},"SIMPLE-RECORD":"HELLO"}"#;
    let mut output = Vec::new();

    encode_jsonl_to_file(&schema, Cursor::new(json.as_bytes()), &mut output, &options)
        .expect("RDW encode should succeed");

    assert_eq!(output.len(), 9);
    let header = RdwHeader::from_bytes(output[0..4].try_into().expect("4-byte header"));
    assert_eq!(header.length(), 5);
    assert_eq!(header.reserved(), 0);
    assert_eq!(&output[4..], b"HELLO");
}

#[test]
fn codec_record_writer_accepts_microcrate_record_type() {
    let record = RDWRecord::try_with_reserved(b"ABCD".to_vec(), 0x0000)
        .expect("microcrate should build RDW record");

    let mut output = Vec::new();
    let mut writer = copybook_codec::record::RDWRecordWriter::new(&mut output);
    writer
        .write_record(&record)
        .expect("codec writer should accept microcrate record");

    assert_eq!(writer.record_count(), 1);
    assert_eq!(output, vec![0x00, 0x04, 0x00, 0x00, b'A', b'B', b'C', b'D']);
}

#[test]
fn codec_rdw_reader_returns_microcrate_record_shape() {
    let data = vec![0x00, 0x03, 0x00, 0x00, b'X', b'Y', b'Z'];
    let mut reader = copybook_codec::record::RDWRecordReader::new(Cursor::new(data), false);
    let record = reader
        .read_record()
        .expect("rdw read should succeed")
        .expect("one record expected");

    assert_eq!(record.length(), 3);
    assert_eq!(record.reserved(), 0);
    assert_eq!(
        record.as_bytes(),
        vec![0x00, 0x03, 0x00, 0x00, b'X', b'Y', b'Z']
    );
}

#[test]
fn microcrate_rdw_reader_decodes_codec_encoded_output() {
    let schema = parse_copybook(
        r"
        01 SIMPLE-RECORD PIC X(5).
    ",
    )
    .expect("schema should parse");

    let options = EncodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::ASCII);

    let json = r#"{"schema":"copybook.v1","record_index":0,"codepage":"ASCII","fields":{"SIMPLE-RECORD":"HELLO"},"SIMPLE-RECORD":"HELLO"}"#;
    let mut output = Vec::new();

    encode_jsonl_to_file(&schema, Cursor::new(json.as_bytes()), &mut output, &options)
        .expect("RDW encode should succeed");

    let mut reader = RDWRecordReader::new(Cursor::new(output), false);
    let record = reader
        .read_record()
        .expect("microcrate reader should parse output")
        .expect("one record expected");
    assert_eq!(record.payload, b"HELLO");
    assert!(
        reader
            .read_record()
            .expect("EOF read should succeed")
            .is_none()
    );
}
