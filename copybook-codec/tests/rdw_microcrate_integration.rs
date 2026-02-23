// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_file_to_jsonl,
    encode_jsonl_to_file,
};
use copybook_core::{ErrorCode, parse_copybook};
use copybook_rdw::RdwHeader;
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
