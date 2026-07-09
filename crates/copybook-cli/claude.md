<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- CLI for parsing, decoding, encoding, and verifying COBOL copybook data (clap).
- Layer: **CLI**.
- Workspace package `copybook-cli` (see `Cargo.toml`).
- Subcommands: `src/commands/`

## Navigation
- Crates index + layer map: `../claude.md`
- Repository contract: `../../AGENTS.md`
- CLI-specific contract: `AGENTS.md`

## Build
- `cargo build -p copybook-cli`
- Test: `cargo test -p copybook-cli`
- Lint: `cargo clippy -p copybook-cli -- -D warnings`
