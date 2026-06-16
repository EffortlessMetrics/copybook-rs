<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Search alias for the canonical copybook crate (pub use copybook::*).
- Layer: **Facade**.
- Workspace package `copybook-rs` (see `Cargo.toml`).
- Canonical crate this aliases: `../copybook/`

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-rs`
- Test: `cargo test -p copybook-rs`
- Lint: `cargo clippy -p copybook-rs -- -D warnings`
