<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Core COBOL copybook parser, schema, and validation primitives. Produces the Schema AST consumed by the codec.
- Layer: **Parser**.
- Workspace package `copybook-core` (see `Cargo.toml`).
- Audit module: `src/audit/`

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-core`
- Test: `cargo test -p copybook-core`
- Lint: `cargo clippy -p copybook-core -- -D warnings`
