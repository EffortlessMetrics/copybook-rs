#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codec::{DecodeOptions, decode_record};
use copybook_core::{parse_copybook, project_schema};
use libfuzzer_sys::fuzz_target;

/// Fuzz target for schema field projection.
///
/// Takes arbitrary bytes, interprets them as field-name selections, then
/// applies projection to fixed schemas. Exercises ODO auto-dependency,
/// parent group preservation, and error paths (unknown fields, invalid
/// ODO). Must never panic.
fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    // Extract fuzzed field names from input (split on null bytes)
    let input = String::from_utf8_lossy(data);
    let selections: Vec<String> = input
        .split('\0')
        .filter(|s| !s.is_empty())
        .map(|s| s.to_uppercase())
        .collect();

    if selections.is_empty() {
        return;
    }

    // Schema with groups, ODO, and varied field types
    let schemas = [
        // Simple flat schema
        r#"
       01  CUSTOMER-REC.
           05  CUST-ID        PIC 9(8).
           05  CUST-NAME      PIC X(30).
           05  BALANCE        PIC S9(7)V99.
           05  STATUS         PIC X(1).
        "#,
        // Schema with nested groups
        r#"
       01  ORDER-REC.
           05  ORDER-HEADER.
               10  ORDER-ID   PIC 9(10).
               10  ORDER-DATE PIC X(8).
           05  ORDER-DETAIL.
               10  ITEM-CODE  PIC X(6).
               10  QUANTITY   PIC 9(5).
               10  PRICE      PIC S9(5)V99.
        "#,
        // Schema with ODO (exercises auto-dependency)
        r#"
       01  VARIABLE-REC.
           05  REC-COUNT      PIC 9(3).
           05  ITEMS OCCURS 1 TO 10 DEPENDING ON REC-COUNT.
               10  ITEM-NAME  PIC X(10).
        "#,
    ];

    for copybook_text in &schemas {
        let schema = match parse_copybook(copybook_text) {
            Ok(s) => s,
            Err(_) => continue,
        };

        // Project with fuzzed field names — must not panic
        let projected = match project_schema(&schema, &selections) {
            Ok(s) => s,
            Err(_) => continue,
        };

        // If projection succeeded, verify decode doesn't panic either
        // Use a buffer sized for the largest schema
        let dummy_data = vec![0x40u8; 256];
        let _ = decode_record(&projected, &dummy_data, &DecodeOptions::default());
    }
});
