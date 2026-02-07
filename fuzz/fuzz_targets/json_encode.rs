#![no_main]
use libfuzzer_sys::fuzz_target;
use copybook_core::parse_copybook;
use copybook_codec::{encode_record, EncodeOptions, DecodeOptions, Codepage};
use serde_json::Value;

/// Fuzz target for JSON encoding
///
/// This fuzzer tests the JSON encoder with various inputs including:
/// - Valid JSON objects
/// - Malformed JSON
/// - Extreme values (very large numbers, deep nesting)
/// - Unicode strings
/// - Various data types (null, arrays, objects)
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

    // Try to parse as JSON
    let json_value: Result<Value, _> = serde_json::from_slice(data);

    if let Ok(value) = json_value {
        // Test encoding with default options
        let options = EncodeOptions::default();
        let _ = encode_record(&value, &schema, &options);

        // Test with different codepages
        for codepage in [Codepage::Cp037, Codepage::Utf8, Codepage::Cp1047] {
            let options = EncodeOptions {
                codepage,
                ..Default::default()
            };
            let _ = encode_record(&value, &schema, &options);
        }

        // Test with RDW format
        let rdw_options = EncodeOptions {
            record_format: copybook_codec::RecordFormat::Rdw,
            ..Default::default()
        };
        let _ = encode_record(&value, &schema, &rdw_options);
    }
});
