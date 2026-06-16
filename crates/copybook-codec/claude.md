<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Deterministic COBOL copybook codec for EBCDIC/ASCII fixed and RDW records. Schema -> encode/decode binary <-> JSON.
- Layer: **Codec**.
- Workspace package `copybook-codec` (see `Cargo.toml`).
- Raw data capture (`__raw_b64`) convention: see root `CLAUDE.md`

## Navigation
- Crates index + layer map: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`

## Build
- `cargo build -p copybook-codec`
- Test: `cargo test -p copybook-codec`
- Lint: `cargo clippy -p copybook-codec -- -D warnings`
