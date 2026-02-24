#![allow(clippy::must_use_candidate)]

//! COBOL copybook lexer shim.
//!
//! The tokenizer implementation now lives in `copybook-lexer`. This module
//! preserves the `copybook_core::lexer` API used across parser and tests while
//! keeping parsing logic in the dedicated microcrate.

use crate::parser::ParseOptions;

pub use copybook_lexer::{CobolFormat, Token, TokenPos};

/// Compatibility wrapper around `copybook_lexer::Lexer`.
pub struct Lexer<'a> {
    inner: copybook_lexer::Lexer<'a>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given input.
    pub fn new(input: &'a str) -> Self {
        Self::new_with_options(input, &ParseOptions::default())
    }

    /// Create a new lexer for the given input with specific parse options.
    ///
    /// `copybook-core` owns richer parser options, so we map only lexer-relevant
    /// flags into `copybook_lexer::LexerOptions` for backward compatibility.
    pub fn new_with_options(input: &'a str, options: &ParseOptions) -> Self {
        let lexer_options = copybook_lexer::LexerOptions {
            allow_inline_comments: options.allow_inline_comments,
            strict_comments: options.strict_comments,
        };

        Self {
            inner: copybook_lexer::Lexer::new_with_options(input, &lexer_options),
        }
    }

    /// Get the detected format for this input.
    #[inline]
    pub fn format(&self) -> CobolFormat {
        self.inner.format()
    }

    /// Tokenize the input and return all tokens with positions.
    #[inline]
    pub fn tokenize(&mut self) -> Vec<TokenPos> {
        self.inner.tokenize()
    }
}
