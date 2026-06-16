<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- COBOL copybook lexical analysis and tokenization primitives (logos-based).
- Layer: **Parser**.
- Workspace package `copybook-lexer` (see `Cargo.toml`).

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-lexer`
- Test: `cargo test -p copybook-lexer`
- Lint: `cargo clippy -p copybook-lexer -- -D warnings`
