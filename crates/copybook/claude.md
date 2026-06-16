<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Root facade re-exporting the canonical API for the copybook-rs crate family.
- Layer: **Facade**.
- Workspace package `copybook` (see `Cargo.toml`).
- Canonical crate this facade re-exports: `../copybook/`

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook`
- Test: `cargo test -p copybook`
- Lint: `cargo clippy -p copybook -- -D warnings`
