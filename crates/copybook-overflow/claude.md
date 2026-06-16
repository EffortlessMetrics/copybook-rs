<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Overflow-safe integer narrowing and bounds arithmetic.
- Layer: **Safety**.
- Workspace package `copybook-overflow` (see `Cargo.toml`).

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-overflow`
- Test: `cargo test -p copybook-overflow`
- Lint: `cargo clippy -p copybook-overflow -- -D warnings`
