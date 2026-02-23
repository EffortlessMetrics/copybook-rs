#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::{Codepage, EncodeOptions, RecordFormat, encode_record};
use copybook_core::parse_copybook;
use libfuzzer_sys::fuzz_target;
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
        let _ = encode_record(&schema, &value, &options);

        // Test with different codepages
        for codepage in [Codepage::CP037, Codepage::ASCII, Codepage::CP1047] {
            let options = EncodeOptions {
                codepage,
                ..Default::default()
            };
            let _ = encode_record(&schema, &value, &options);
        }

        // Test with RDW format
        let rdw_options = EncodeOptions {
            format: RecordFormat::RDW,
            ..Default::default()
        };
        let _ = encode_record(&schema, &value, &rdw_options);
    }
});
