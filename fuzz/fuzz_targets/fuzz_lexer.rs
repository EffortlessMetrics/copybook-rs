#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_lexer::{Lexer, LexerOptions};
use libfuzzer_sys::fuzz_target;

// Hardened fuzz target for the COBOL lexer.
//
// Feeds arbitrary byte sequences (including non-ASCII, empty, and large
// inputs) through every lexer configuration to verify the lexer never
// panics regardless of input.
fuzz_target!(|data: &[u8]| {
    // Convert arbitrary bytes to a string; non-UTF-8 bytes become U+FFFD.
    let input = String::from_utf8_lossy(data);

    // Exercise default lexer
    let _ = Lexer::new(&input).tokenize();

    // Exercise all option combinations
    let option_combos = [
        LexerOptions {
            allow_inline_comments: false,
            strict_comments: false,
        },
        LexerOptions {
            allow_inline_comments: true,
            strict_comments: false,
        },
        LexerOptions {
            allow_inline_comments: false,
            strict_comments: true,
        },
        LexerOptions {
            allow_inline_comments: true,
            strict_comments: true,
        },
    ];

    for opts in option_combos {
        let tokens = Lexer::new_with_options(&input, opts).tokenize();
        // Touch every token to ensure iteration is safe
        for tok in &tokens {
            let _ = format!("{:?}", tok.token);
        }
    }

    // Also try the input as a pure ASCII slice (no lossy conversion).
    // This catches edge cases where valid UTF-8 sub-sequences matter.
    if let Ok(text) = std::str::from_utf8(data) {
        let _ = Lexer::new(text).tokenize();
    }
});
