#![no_main]
use libfuzzer_sys::fuzz_target;
use copybook_core::parse_copybook;
use copybook_codec::{decode_record, DecodeOptions, Codepage};

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
    let _ = decode_record(data, &schema, &options);

    // Test with different codepages
    for codepage in [Codepage::Cp037, Codepage::Utf8, Codepage::Cp1047] {
        let options = DecodeOptions {
            codepage,
            ..Default::default()
        };
        let _ = decode_record(data, &schema, &options);
    }

    // Test with RDW format
    let rdw_options = DecodeOptions {
        record_format: copybook_codec::RecordFormat::Rdw,
        ..Default::default()
    };
    let _ = decode_record(data, &schema, &rdw_options);

    // Test with raw mode
    let raw_options = DecodeOptions {
        raw_mode: copybook_codec::RawMode::All,
        ..Default::default()
    };
    let _ = decode_record(data, &schema, &raw_options);
});
