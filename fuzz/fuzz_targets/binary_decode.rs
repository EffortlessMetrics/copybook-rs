#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::{Codepage, DecodeOptions, RawMode, RecordFormat, decode_record};
use libfuzzer_sys::fuzz_target;
use copybook_core::parse_copybook;

/// Fuzz target for binary data decoding
///
/// This fuzzer tests the decoder with various binary inputs including:
/// - Valid binary data matching copybook schema
/// - Malformed binary data
/// - Random bytes
/// - Edge cases (all zeros, all ones, patterns)
/// - Data with RDW headers
/// - Various codepage encodings
fuzz_target!(|data: &[u8]| {
    // Simple copybook schema for testing
    let copybook_text = r#"
       01  TEST-RECORD.
           05  FIELD-1      PIC X(10).
           05  FIELD-2      PIC 9(5).
           05  FIELD-3      PIC S9(3)V99 COMP-3.
    "#;

    let schema = match parse_copybook(copybook_text) {
        Ok(s) => s,
        Err(_) => return, // Skip if schema parsing fails
    };

    // Test with default options
    let options = DecodeOptions::default();
    let _ = decode_record(&schema, data, &options);

    // Test with different codepages
    for codepage in [Codepage::CP037, Codepage::ASCII, Codepage::CP1047] {
        let options = DecodeOptions {
            codepage,
            ..Default::default()
        };
        let _ = decode_record(&schema, data, &options);
    }

    // Test with RDW format
    let rdw_options = DecodeOptions {
        format: RecordFormat::RDW,
        ..Default::default()
    };
    let _ = decode_record(&schema, data, &rdw_options);

    // Test with raw mode
    let raw_options = DecodeOptions {
        emit_raw: RawMode::Record,
        ..Default::default()
    };
    let _ = decode_record(&schema, data, &raw_options);
});
