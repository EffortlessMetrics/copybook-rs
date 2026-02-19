#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later
use libfuzzer_sys::fuzz_target;
use copybook_core::pic::PicClause;

/// Fuzz target for PIC clause parsing
///
/// This fuzzer tests PIC clause parsing with various inputs including:
/// - Valid PIC clauses (X, 9, S9, V, etc.)
/// - Invalid PIC clauses
/// - Edge cases (very long clauses, complex patterns)
/// - Mixed type clauses
/// - Edited PIC patterns
fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, replacing invalid UTF-8 sequences
    let input = String::from_utf8_lossy(data);

    // Skip empty inputs
    if input.trim().is_empty() {
        return;
    }

    // Test PIC clause parsing
    let _ = PicClause::parse(&input);

    // Test with common PIC prefixes
    let prefixes = ["PIC ", "PICTURE ", "pic ", "picture "];

    for prefix in prefixes {
        let full_input = format!("{}{}", prefix, input);
        let _ = PicClause::parse(&full_input);
    }

    // Test with common suffixes
    let suffixes = [".", " VALUE 123", " OCCURS 5 TIMES"];

    for suffix in suffixes {
        let full_input = format!("{}{}", input, suffix);
        let _ = PicClause::parse(&full_input);
    }
});
