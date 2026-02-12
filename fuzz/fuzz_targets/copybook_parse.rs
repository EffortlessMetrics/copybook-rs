#![no_main]
use libfuzzer_sys::fuzz_target;
use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions};

/// Fuzz target for copybook parsing
///
/// This fuzzer tests the copybook parser with various inputs including:
/// - Valid copybook syntax
/// - Malformed copybook syntax
/// - Edge cases (empty input, very long lines, nested structures)
/// - Unicode and special characters
/// - Various COBOL dialect features
fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, replacing invalid UTF-8 sequences
    let input = String::from_utf8_lossy(data);

    // Skip empty inputs (handled by parser anyway)
    if input.trim().is_empty() {
        return;
    }

    // Test with default options
    let _ = parse_copybook(&input);

    // Test with strict mode
    let strict_options = ParseOptions {
        strict: true,
        strict_comments: true,
        ..Default::default()
    };
    let _ = parse_copybook_with_options(&input, &strict_options);

    // Test with tolerant mode
    let tolerant_options = ParseOptions {
        strict: false,
        allow_inline_comments: true,
        ..Default::default()
    };
    let _ = parse_copybook_with_options(&input, &tolerant_options);

    // Test with filler emission
    let filler_options = ParseOptions {
        emit_filler: true,
        ..Default::default()
    };
    let _ = parse_copybook_with_options(&input, &filler_options);
});
