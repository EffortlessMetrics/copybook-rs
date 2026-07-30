#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deprecated compatibility package for the 0.5 lexer API.
//!
//! New code should use [`copybook_core::lexer`] directly. This package is
//! retained through the 0.6 compatibility window because the package name was
//! already published.

#![allow(clippy::missing_inline_in_public_items)]

/// Deprecated alias for [`copybook_core::lexer::CobolFormat`].
#[deprecated(
    since = "0.6.0",
    note = "use copybook_core::lexer::CobolFormat instead"
)]
pub type CobolFormat = copybook_core::lexer::CobolFormat;

/// Deprecated alias for [`copybook_core::lexer::Lexer`].
#[deprecated(since = "0.6.0", note = "use copybook_core::lexer::Lexer instead")]
pub type Lexer<'a> = copybook_core::lexer::Lexer<'a>;

/// Deprecated alias for [`copybook_core::lexer::LexerOptions`].
#[deprecated(
    since = "0.6.0",
    note = "use copybook_core::lexer::LexerOptions instead"
)]
pub type LexerOptions = copybook_core::lexer::LexerOptions;

/// Deprecated alias for [`copybook_core::lexer::Token`].
#[deprecated(since = "0.6.0", note = "use copybook_core::lexer::Token instead")]
pub type Token = copybook_core::lexer::Token;

/// Deprecated alias for [`copybook_core::lexer::TokenPos`].
#[deprecated(since = "0.6.0", note = "use copybook_core::lexer::TokenPos instead")]
pub type TokenPos = copybook_core::lexer::TokenPos;
