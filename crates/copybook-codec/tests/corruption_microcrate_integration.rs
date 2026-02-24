// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::io::Cursor;

use copybook_codec::{Codepage, DecodeOptions, RecordFormat, decode_file_to_jsonl};
use copybook_core::{ErrorCode, parse_copybook};

#[test]
fn decode_rejects_invalid_packed_decimal_in_strict_mode() {
    let schema = parse_copybook(
        r#"
        01 PACKED-RECORD.
           05 AMOUNT PIC S9(4) COMP-3.
    "#,
    )
    .expect("schema should parse");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(true);

    // Invalid sign nibble (0x6) in last byte.
    let invalid_input = vec![0x12, 0x34, 0x56];
    let mut output = Vec::new();

    let err = decode_file_to_jsonl(&schema, Cursor::new(invalid_input), &mut output, &options)
        .expect_err("invalid packed decimal should fail in strict mode");

    assert_eq!(err.code, ErrorCode::CBKD401_COMP3_INVALID_NIBBLE);
    assert!(err.message.contains("nibble"));
}

#[test]
#[ignore = "corruption warning integration not yet wired into decode pipeline"]
fn decode_emits_warning_for_ebcdic_control_byte_when_lenient() {
    let schema = parse_copybook(
        r#"
        01 TEXT-RECORD.
           05 TEXT PIC X(4).
    "#,
    )
    .expect("schema should parse");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_strict_mode(false);

    // NUL byte is treated as suspicious for EBCDIC corruption checks.
    let input = vec![b'A', 0x00, b'B', b'C'];
    let mut output = Vec::new();

    let summary = decode_file_to_jsonl(&schema, Cursor::new(input), &mut output, &options)
        .expect("decode should succeed in lenient mode with warning");

    assert!(summary.has_warnings());
    let output = String::from_utf8(output).expect("decoded output should be utf-8");
    assert!(output.contains("TEXT"));
}
