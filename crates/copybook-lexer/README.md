# SPDX-License-Identifier: AGPL-3.0-or-later

A deprecated compatibility package for the lexer API formerly published as a
standalone microcrate.

The implementation now lives in `copybook-core::lexer`. This package forwards
the 0.5 API during the 0.6 compatibility window and will not receive new
implementation behavior.

New code should use `copybook-core` directly for:

- Fixed and free-form preprocessing
- Continuation handling
- Tokenization with Logos
- Position tracking for parser diagnostics

Higher-level parser behavior and semantic checks remain in `copybook-core`.
