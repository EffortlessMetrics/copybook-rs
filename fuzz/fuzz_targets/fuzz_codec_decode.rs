#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RawMode, RecordFormat, decode_record};
use copybook_core::parse_copybook;
use libfuzzer_sys::fuzz_target;

/// Fuzz target for codec decode path with varied schemas and binary data.
///
/// Exercises decode with multiple schemas (alphanumeric, packed decimal,
/// COMP fields) across codepages, JSON number modes, and raw modes.
fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    // Schema with a mix of field types
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

        // Vary codepage
        for codepage in [Codepage::CP037, Codepage::ASCII, Codepage::CP1047] {
            let opts = DecodeOptions {
                codepage,
                ..Default::default()
            };
            let _ = decode_record(&schema, data, &opts);
        }

        // Vary JSON number mode
        for mode in [JsonNumberMode::Lossless, JsonNumberMode::Native] {
            let opts = DecodeOptions {
                json_number_mode: mode,
                ..Default::default()
            };
            let _ = decode_record(&schema, data, &opts);
        }

        // With raw mode enabled
        for raw in [RawMode::Record, RawMode::Field] {
            let opts = DecodeOptions {
                emit_raw: raw,
                ..Default::default()
            };
            let _ = decode_record(&schema, data, &opts);
        }

        // RDW format
        let opts = DecodeOptions {
            format: RecordFormat::RDW,
            ..Default::default()
        };
        let _ = decode_record(&schema, data, &opts);
    }
});
