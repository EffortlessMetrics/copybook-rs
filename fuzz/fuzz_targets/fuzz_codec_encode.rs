#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codec::{Codepage, EncodeOptions, RecordFormat, encode_record};
use copybook_core::parse_copybook;
use libfuzzer_sys::fuzz_target;
use serde_json::Value;

/// Fuzz target for codec encode path with random JSON-like inputs.
///
/// Parses fuzzed bytes as JSON, then attempts encoding across schemas,
/// codepages, and record formats.
fuzz_target!(|data: &[u8]| {
    let json_value: Value = match serde_json::from_slice(data) {
        Ok(v) => v,
        Err(_) => return,
    };

    let schemas = [
        r#"
       01  REC-ALPHA.
           05  FLD-A      PIC X(20).
           05  FLD-B      PIC X(5).
        "#,
        r#"
       01  REC-NUMERIC.
           05  FLD-N1     PIC 9(8).
           05  FLD-N2     PIC S9(5)V99 COMP-3.
           05  FLD-N3     PIC S9(9) COMP.
        "#,
        r#"
       01  REC-MIXED.
           05  FLD-X      PIC X(4).
           05  FLD-9      PIC 9(3).
           05  FLD-P      PIC S9(7)V99 COMP-3.
           05  FLD-T      PIC X(10).
        "#,
    ];

    for copybook_text in &schemas {
        let schema = match parse_copybook(copybook_text) {
            Ok(s) => s,
            Err(_) => continue,
        };

        // Default options
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
    }
});
