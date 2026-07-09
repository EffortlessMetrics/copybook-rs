<!-- SPDX-License-Identifier-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Standalone example projects demonstrating how to consume copybook-rs crates as a user would.
- `kafka_pipeline` is excluded from the workspace (see root `Cargo.toml` `exclude`); build it standalone.

## Examples
| Example | Focus |
|---|---|
| `basic` | Minimal decode/encode usage |
| `enterprise` | Audit/governance/governance-runtime features |
| `integration` | Multi-crate composition |
| `kafka_pipeline` | Streaming pipeline (standalone crate, not a workspace member) |

## Navigation
- Repository contract: `../AGENTS.md`
- Example-specific contract: `AGENTS.md`
- Library API reference: `../docs/reference/LIBRARY_API.md`

## Run
- `cargo run --example <name>` (for workspace examples)
- `kafka_pipeline`: `cd kafka_pipeline && cargo run`
