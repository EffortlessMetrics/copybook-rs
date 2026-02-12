#![no_main]
use libfuzzer_sys::fuzz_target;
use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions};

/// Fuzz target for REDEFINES processing
///
/// This fuzzer tests REDEFINES clauses with various inputs including:
/// - Valid REDEFINES clauses
/// - Multiple REDEFINES on same field
/// - REDEFINES with OCCURS
/// - REDEFINES with ODO
/// - Invalid REDEFINES positioning
/// - Complex REDEFINES hierarchies
fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, replacing invalid UTF-8 sequences
    let input = String::from_utf8_lossy(data);

    // Skip empty inputs
    if input.trim().is_empty() {
        return;
    }

    // Create copybook templates with REDEFINES
    let templates = [
        // Simple REDEFINES
        format!(r#"
       01  TEST-RECORD.
           05  FIELD-A      PIC X(10).
           05  FIELD-B      REDEFINES FIELD-A PIC 9(10).
        "#),

        // Multiple REDEFINES
        format!(r#"
       01  TEST-RECORD.
           05  FIELD-A      PIC X(10).
           05  FIELD-B      REDEFINES FIELD-A PIC 9(10).
           05  FIELD-C      REDEFINES FIELD-A PIC S9(5)V99 COMP-3.
        "#),

        // REDEFINES with OCCURS
        format!(r#"
       01  TEST-RECORD.
           05  FIELD-A      PIC X(10) OCCURS 5 TIMES.
           05  FIELD-B      REDEFINES FIELD-A PIC 9(10) OCCURS 5 TIMES.
        "#),

        // REDEFINES with nested structure
        format!(r#"
       01  TEST-RECORD.
           05  GROUP-A.
               10  SUB-1   PIC X(5).
               10  SUB-2   PIC X(5).
           05  GROUP-B      REDEFINES GROUP-A.
               10  NUM-1   PIC 9(5).
               10  NUM-2   PIC 9(5).
        "#),

        // REDEFINES with ODO
        format!(r#"
       01  TEST-RECORD.
           05  COUNTER      PIC 9(3).
           05  FIELD-A      PIC X(10) OCCURS {} TO 10 TIMES
                           DEPENDING ON COUNTER.
           05  FIELD-B      REDEFINES FIELD-A PIC 9(10) OCCURS {} TO 10 TIMES
                           DEPENDING ON COUNTER.
        "#, input, input),

        // Complex REDEFINES with mixed types
        format!(r#"
       01  TEST-RECORD.
           05  PAYLOAD      PIC X({}).
           05  NUMERIC-PAYLOAD REDEFINES PAYLOAD.
               10  NUM-1   PIC S9(9) COMP.
               10  NUM-2   PIC S9(9) COMP.
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

        // Test with filler emission
        let filler_options = ParseOptions {
            emit_filler: true,
            ..Default::default()
        };
        let _ = parse_copybook_with_options(&template, &filler_options);
    }
});
