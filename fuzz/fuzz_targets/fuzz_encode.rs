#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codec::{Codepage, EncodeOptions, RecordFormat, encode_record};
use copybook_core::parse_copybook;
use libfuzzer_sys::fuzz_target;
use serde_json::Value;

/// Fuzz target for encode with random JSON bytes and a fixed schema.
///
/// Takes arbitrary bytes, attempts JSON parsing, then encodes against
/// multiple fixed schemas. Must never panic — only return errors.
fuzz_target!(|data: &[u8]| {
    // Try interpreting raw bytes as JSON; most inputs will fail here.
    let json_value: Value = match serde_json::from_slice(data) {
        Ok(v) => v,
        Err(_) => return,
    };

    // Fixed schema: mixed field types exercising multiple encode paths
    let schema_text = r#"
       01  FIXED-REC.
           05  CUST-NAME     PIC X(20).
           05  CUST-ID       PIC 9(8).
           05  BALANCE       PIC S9(7)V99 COMP-3.
           05  STATUS-CODE   PIC X(2).
           05  SEQ-NUM       PIC S9(9) COMP.
    "#;

    let schema = match parse_copybook(schema_text) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Default encode — should never panic
    let _ = encode_record(&schema, &json_value, &EncodeOptions::default());

    // Vary codepage
    for codepage in [Codepage::CP037, Codepage::ASCII, Codepage::CP1047] {
        let opts = EncodeOptions {
            codepage,
            ..Default::default()
        };
        let _ = encode_record(&schema, &json_value, &opts);
    }

    // RDW format
    let opts = EncodeOptions {
        format: RecordFormat::RDW,
        ..Default::default()
    };
    let _ = encode_record(&schema, &json_value, &opts);

    // Strict mode
    let opts = EncodeOptions {
        strict_mode: true,
        ..Default::default()
    };
    let _ = encode_record(&schema, &json_value, &opts);
});
