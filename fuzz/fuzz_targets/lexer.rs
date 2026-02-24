#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later
use libfuzzer_sys::fuzz_target;
use copybook_lexer::{Lexer, LexerOptions, Token};

/// Fuzz target for lexer tokenization.
///
/// Exercises tokenization, inline-comment modes, and EOF stability.
fuzz_target!(|data: &[u8]| {
    let input = String::from_utf8_lossy(data);

    // Skip empty inputs to keep corpus small.
    if input.trim().is_empty() {
        return;
    }

    let _ = Lexer::new(&input).tokenize();
    let _ = Lexer::new_with_options(
        &input,
        LexerOptions {
            allow_inline_comments: false,
            strict_comments: true,
        },
    )
    .tokenize();

    if input.contains("*>") {
        let with_comment_strip = Lexer::new_with_options(
            &input,
            LexerOptions {
                allow_inline_comments: true,
                strict_comments: false,
            },
        )
        .tokenize();
        let with_comment_strict = Lexer::new_with_options(
            &input,
            LexerOptions {
                allow_inline_comments: true,
                strict_comments: true,
            },
        )
        .tokenize();

        let strip_count = with_comment_strip
            .iter()
            .filter(|token| matches!(token.token, Token::InlineComment(_)))
            .count();
        let strict_count = with_comment_strict
            .iter()
            .filter(|token| matches!(token.token, Token::InlineComment(_)))
            .count();

        assert!(strict_count >= strip_count);
    }
});
