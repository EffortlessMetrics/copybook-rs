<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Codepage and unmappable-character policy types (CP037/CP273/CP500/CP1047/CP1140).
- Layer: **Codec**.
- Workspace package `copybook-codepage` (see `Cargo.toml`).

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-codepage`
- Test: `cargo test -p copybook-codepage`
- Lint: `cargo clippy -p copybook-codepage -- -D warnings`
