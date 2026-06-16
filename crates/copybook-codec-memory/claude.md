<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Reusable memory utilities (scratch buffers) for codec streaming and parallelism.
- Layer: **Codec**.
- Workspace package `copybook-codec-memory` (see `Cargo.toml`).

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-codec-memory`
- Test: `cargo test -p copybook-codec-memory`
- Lint: `cargo clippy -p copybook-codec-memory -- -D warnings`
