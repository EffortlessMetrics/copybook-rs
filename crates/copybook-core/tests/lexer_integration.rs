// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::unwrap_used, clippy::expect_used)]

use copybook_core::lexer::Lexer;
use copybook_core::{
    ParseOptions,
    lexer::{CobolFormat, Token},
};

#[test]
fn lexer_integration_defaults_to_free_form_for_unmarked_copybook() {
    let mut lexer = Lexer::new("01 FOO PIC X(10).\n");
    assert_eq!(lexer.format(), CobolFormat::Free);
    let tokens = lexer.tokenize();

    assert_eq!(tokens.last().unwrap().token, Token::Eof);
    assert!(matches!(tokens[1].token, Token::Identifier(_)));
}

#[test]
fn lexer_integration_maps_parse_options_to_inline_comment_mode() {
    let input = "01 FIELD PIC X(4). *> inline comment should survive only in strict mode";
    let strip_options = ParseOptions {
        allow_inline_comments: true,
        strict_comments: false,
        ..ParseOptions::default()
    };
    let strict_options = ParseOptions {
        allow_inline_comments: true,
        strict_comments: true,
        ..ParseOptions::default()
    };

    let mut strip_lexer = Lexer::new_with_options(input, &strip_options);
    let strip_tokens = strip_lexer.tokenize();
    let strip_comment_tokens = strip_tokens
        .iter()
        .filter(|token| matches!(token.token, Token::InlineComment(_)))
        .count();

    let mut strict_lexer = Lexer::new_with_options(input, &strict_options);
    let strict_tokens = strict_lexer.tokenize();
    let strict_comment_tokens = strict_tokens
        .iter()
        .filter(|token| matches!(token.token, Token::InlineComment(_)))
        .count();

    assert_eq!(strip_comment_tokens, 0);
    assert!(strict_comment_tokens > 0);
}

#[test]
fn lexer_integration_handles_fixed_form_input() {
    let fixed_like = "       01 FIXED-RECORD.\n      -    PIC X(10).";
    let mut lexer = Lexer::new_with_options(fixed_like, &ParseOptions::default());
    assert_eq!(lexer.format(), CobolFormat::Fixed);
    assert!(
        lexer
            .tokenize()
            .iter()
            .any(|token| matches!(token.token, Token::Period))
    );
}
