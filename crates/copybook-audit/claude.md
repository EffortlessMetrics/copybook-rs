<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Experimental enterprise audit crate (moved out of `copybook-core` per #656 Phase E): compliance, context, event, lineage, logger, performance, report, security.

## Navigation
- Crate root: `Cargo.toml`, `src/mod.rs`
- Upstream schema types: `../copybook-core/src/`
- CLI consumer: `../copybook-cli/src/commands/audit.rs`
- API contracts: `../../docs/audit-api-reference.md`

## Build
- `cargo build -p copybook-audit`
- `cargo test -p copybook-audit`
