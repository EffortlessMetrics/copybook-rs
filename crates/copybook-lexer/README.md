# SPDX-License-Identifier: AGPL-3.0-or-later

A dedicated lexer microcrate extracted from `copybook-core`.

This crate owns COBOL copybook tokenization, including:

- Fixed and free-form preprocessing
- Continuation handling
- Tokenization with Logos
- Position tracking for parser diagnostics

Higher-level parser behavior and semantic checks still live in `copybook-core`.
