#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_codec::edited_pic::{decode_edited_numeric, encode_edited_numeric, tokenize_edited_pic};
use libfuzzer_sys::fuzz_target;

/// Fuzz target for edited PIC tokenization, decode, and encode.
///
/// Exercises the edited-PIC pipeline: tokenize arbitrary PIC strings,
/// then decode and encode with the resulting patterns.
fuzz_target!(|data: &[u8]| {
    let input = String::from_utf8_lossy(data);
    if input.trim().is_empty() {
        return;
    }

    // Fuzz tokenizer with raw input
    let _ = tokenize_edited_pic(&input);

    // Known edited PIC patterns to fuzz decode/encode
    let patterns = [
        "ZZZ9",
        "ZZZ9.99",
        "$ZZ,ZZZ.99",
        "+ZZZ9",
        "-ZZZ9.99",
        "***9.99",
        "ZZ9",
    ];

    for pat in &patterns {
        let tokens = match tokenize_edited_pic(pat) {
            Ok(t) => t,
            Err(_) => continue,
        };

        // Decode: treat fuzzed input as an edited field value
        let _ = decode_edited_numeric(&input, &tokens, 2, false);
        let _ = decode_edited_numeric(&input, &tokens, 0, true);

        // Encode: treat fuzzed input as a numeric value string
        let _ = encode_edited_numeric(&input, &tokens, 2, false);
    }

    // If tokenization succeeds on raw input, use those tokens too
    if let Ok(tokens) = tokenize_edited_pic(&input) {
        let _ = decode_edited_numeric("12345", &tokens, 2, false);
        let _ = encode_edited_numeric("12345", &tokens, 2, false);
        let _ = decode_edited_numeric(&input, &tokens, 0, false);
        let _ = encode_edited_numeric(&input, &tokens, 0, false);
    }
});
