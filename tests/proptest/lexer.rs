#![allow(unused_doc_comments, unused_imports, dead_code)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for copybook lexer behavior.
//!
//! These tests validate fuzz-like parser resilience invariants and option
//! interactions in the dedicated lexer microcrate.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_lexer::{Lexer, LexerOptions, Token};
use proptest::prelude::*;

/// Tokenization must never panic and should always finish with EOF.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_lexer_tokenization_ends_with_eof(
        bytes in proptest::collection::vec(0u8..=0x7f, 0..300),
    ) {
        let input = String::from_utf8(bytes).expect("only ASCII bytes should be valid UTF-8");
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokenize();

        prop_assert!(!tokens.is_empty());
        prop_assert_eq!(tokens.last().unwrap().token.clone(), Token::Eof);
    }
}

/// Inline-comment mode should be respected:
/// strict comments should keep `*>` payload tokens, default mode strips them.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: 128,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_inline_comment_options_affect_tokenization(seed in "[^\\n\\r]{0,120}") {
        let mut base = format!("01 FIELD PIC X(10). *> {seed}");
        if base.len() > 300 {
            base.truncate(300);
        }

        let mut strip_lexer = Lexer::new_with_options(
            &base,
            &LexerOptions {
                allow_inline_comments: true,
                strict_comments: false,
            },
        );
        let strip_tokens = strip_lexer.tokenize();
        let strip_count = strip_tokens
            .iter()
            .filter(|token| matches!(token.token, Token::InlineComment(_)))
            .count();

        let mut strict_lexer = Lexer::new_with_options(
            &base,
            &LexerOptions {
                allow_inline_comments: true,
                strict_comments: true,
            },
        );
        let strict_tokens = strict_lexer.tokenize();
        let strict_count = strict_tokens
            .iter()
            .filter(|token| matches!(token.token, Token::InlineComment(_)))
            .count();

        prop_assert!(strip_count == 0 || seed.is_empty());
        prop_assert!(strict_count >= strip_count);
        prop_assert!(strict_count >= 1 || seed.is_empty());
    }
}
