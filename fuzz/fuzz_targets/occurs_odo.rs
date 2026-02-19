#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later
use libfuzzer_sys::fuzz_target;
use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions};

/// Fuzz target for OCCURS/ODO handling
///
/// This fuzzer tests OCCURS and ODO (OCCURS DEPENDING ON) clauses with various inputs including:
/// - Valid OCCURS clauses
/// - ODO clauses with valid dependencies
/// - Invalid ODO positioning
/// - Nested OCCURS
/// - Edge cases (zero bounds, very large bounds)
fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, replacing invalid UTF-8 sequences
    let input = String::from_utf8_lossy(data);

    // Skip empty inputs
    if input.trim().is_empty() {
        return;
    }

    // Create copybook templates with OCCURS/ODO
    let templates = [
        // Simple OCCURS
        format!(r#"
       01  TEST-RECORD.
           05  COUNTER      PIC 9(3).
           05  ARRAY        OCCURS 5 TIMES.
               10  ITEM     PIC X(10).
        "#),

        // ODO with DEPENDING ON
        format!(r#"
       01  TEST-RECORD.
           05  COUNTER      PIC 9(3).
           05  ARRAY        OCCURS {} TO 10 TIMES
                           DEPENDING ON COUNTER.
               10  ITEM     PIC X(10).
        "#, input),

        // Nested OCCURS
        format!(r#"
       01  TEST-RECORD.
           05  OUTER        OCCURS 3 TIMES.
               10  INNER    OCCURS {} TIMES.
                   15  ITEM PIC X(5).
        "#, input),

        // ODO with complex structure
        format!(r#"
       01  TEST-RECORD.
           05  COUNTER      PIC 9(3).
           05  GROUP-1.
               10  FIELD-1  PIC X(5).
               10  ARRAY    OCCURS {} TO 20 TIMES
                               DEPENDING ON COUNTER.
                   15  ITEM  PIC 9(5).
               10  FIELD-2  PIC X(5).
        "#, input),
    ];

    // Test each template
    for template in templates {
        let _ = parse_copybook(&template);

        // Test with strict mode
        let strict_options = ParseOptions {
            strict: true,
            ..Default::default()
        };
        let _ = parse_copybook_with_options(&template, &strict_options);

        // Test with tolerant dialect
        let tolerant_options = ParseOptions {
            strict: false,
            ..Default::default()
        };
        let _ = parse_copybook_with_options(&template, &tolerant_options);
    }
});
