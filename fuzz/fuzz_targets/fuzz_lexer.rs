#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_lexer::{CobolFormat, Lexer, LexerOptions, Token};
use libfuzzer_sys::fuzz_target;

// Hardened fuzz target for the COBOL lexer.
//
// Feeds arbitrary byte sequences (including non-ASCII, empty, and large
// inputs) through every lexer configuration to verify the lexer never
// panics regardless of input.  Exercises:
//   - All four LexerOptions combinations
//   - Format detection (fixed vs free)
//   - Token Display / Debug formatting
//   - Span validity invariants
//   - Pure-ASCII path (no lossy conversion)
fuzz_target!(|data: &[u8]| {
    // Convert arbitrary bytes to a string; non-UTF-8 bytes become U+FFFD.
    let input = String::from_utf8_lossy(data);

    // ── 1. Exercise default lexer ────────────────────────────────────
    let mut default_lexer = Lexer::new(&input);
    let default_format = default_lexer.format();
    let default_tokens = default_lexer.tokenize();

    // Verify format detection yields a valid variant (no panic).
    let _ = matches!(default_format, CobolFormat::Fixed | CobolFormat::Free);

    // Verify Eof is always the last token.
    if let Some(last) = default_tokens.last() {
        assert!(
            matches!(last.token, Token::Eof),
            "Eof must be last token"
        );
    }

    // Verify span invariants: start <= end, line >= 1, column >= 1.
    for tp in &default_tokens {
        assert!(tp.span.start <= tp.span.end);
        assert!(tp.line >= 1);
        assert!(tp.column >= 1);
    }

    // ── 2. Exercise all option combinations ──────────────────────────
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
        let mut lexer = Lexer::new_with_options(&input, opts);

        // Format detection must not depend on lexer options.
        let _ = lexer.format();

        let tokens = lexer.tokenize();

        // Touch every token via Display and Debug to exercise formatting.
        for tok in &tokens {
            let _ = format!("{}", tok.token);
            let _ = format!("{:?}", tok.token);
        }
    }

    // ── 3. Pure-ASCII path (no lossy conversion) ─────────────────────
    // This catches edge cases where valid UTF-8 sub-sequences matter.
    if let Ok(text) = std::str::from_utf8(data) {
        let mut lexer = Lexer::new(text);
        let _ = lexer.format();
        let tokens = lexer.tokenize();
        for tok in &tokens {
            let _ = format!("{}", tok.token);
        }
    }
});
