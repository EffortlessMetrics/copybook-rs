<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Error types and taxonomy (10 families, 63 stable codes) for copybook-rs.
- Layer: **Safety**.
- Workspace package `copybook-error` (see `Cargo.toml`).
- Error code catalog: `../../docs/reference/ERROR_CODES.md`

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-error`
- Test: `cargo test -p copybook-error`
- Lint: `cargo clippy -p copybook-error -- -D warnings`
