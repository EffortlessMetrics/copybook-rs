// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]
//! Level-88 mismatch is a failed condition, never a decode error.
//!
//! A record whose field value matches no `88` condition name still decodes
//! successfully; conditions are metadata, not decode gates.

use copybook_codec::{Codepage, DecodeOptions, RecordFormat, decode_file_to_jsonl};
use copybook_core::parse_copybook;
use std::io::Cursor;

#[test]
fn enterprise_level88_value_mismatch_decodes_without_error() {
    let schema =
        parse_copybook("01 REC. 05 STATUS-CODE PIC X. 88 SUCCESS VALUE 'S'. 88 FAILURE VALUE 'F'.")
            .unwrap();
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII);

    // 'X' matches neither 'S' nor 'F': decode must still succeed.
    let mut output = Vec::new();
    let summary = decode_file_to_jsonl(&schema, Cursor::new(b"X"), &mut output, &options)
        .expect("level-88 mismatch must not fail decode");
    assert_eq!(summary.records_processed, 1);

    let line = String::from_utf8(output).expect("output is UTF-8");
    let value: serde_json::Value = serde_json::from_str(line.trim()).expect("line is JSON");
    assert_eq!(value["STATUS-CODE"], "X");
}
