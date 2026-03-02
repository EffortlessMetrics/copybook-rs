#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codec::{DecodeOptions, EncodeOptions, decode_record, encode_record};
use copybook_core::parse_copybook;
use libfuzzer_sys::fuzz_target;

/// Fuzz target for decode→encode roundtrip.
///
/// Takes arbitrary bytes as binary record data, decodes to JSON, then
/// encodes back to binary. Exercises the full codec pipeline to verify
/// that no combination of fuzzed input causes a panic.
fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

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

    let decode_opts = DecodeOptions::default();
    let encode_opts = EncodeOptions::default();

    for copybook_text in &schemas {
        let schema = match parse_copybook(copybook_text) {
            Ok(s) => s,
            Err(_) => continue,
        };

        // Decode fuzzed binary data to JSON
        let json_value = match decode_record(&schema, data, &decode_opts) {
            Ok(v) => v,
            Err(_) => continue,
        };

        // Encode JSON back to binary — must not panic
        let _ = encode_record(&schema, &json_value, &encode_opts);
    }
});
