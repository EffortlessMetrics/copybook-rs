// SPDX-License-Identifier: AGPL-3.0-or-later
//! Compile-contract coverage for the deprecated 0.5 package path.
#![allow(deprecated)]

use copybook_lexer::{CobolFormat, Lexer, LexerOptions, Token, TokenPos};

#[test]
fn compatibility_aliases_preserve_lexer_surface() {
    let mut lexer = Lexer::new("01 FIELD PIC X.");
    let tokens: Vec<TokenPos> = lexer.tokenize();

    assert_eq!(lexer.format(), CobolFormat::Free);
    assert!(matches!(tokens[0].token, Token::Level(1)));
}

#[test]
fn compatibility_options_forward_to_core_owner() {
    let options = LexerOptions {
        allow_inline_comments: false,
        strict_comments: true,
    };
    let mut lexer = Lexer::new_with_options("01 FIELD PIC X. *> comment", options);
    assert!(
        lexer
            .tokenize()
            .iter()
            .any(|entry| { matches!(entry.token, Token::InlineComment(_)) })
    );
}
