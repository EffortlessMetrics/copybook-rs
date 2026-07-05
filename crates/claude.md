<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Holds the 38 publishable crates of the copybook-rs workspace (36 core + 2 facade/alias crates).
- Organized by layer; see the "Workspace Layout" table in `../CLAUDE.md`.

## Layers at a glance
| Layer | Crates |
|---|---|
| Facade | `copybook`, `copybook-rs` |
| Parser | `copybook-core`, `copybook-lexer` |
| Codec | `copybook-codec`, `copybook-codec-memory`, `copybook-codepage`, `copybook-charset`, `copybook-overpunch`, `copybook-zoned-format` |
| CLI | `copybook-cli`, `copybook-cli-determinism`, `copybook-options` |
| Framing | `copybook-fixed`, `copybook-rdw`, `copybook-rdw-predicates`, `copybook-record-io` |
| Schema | `copybook-dialect`, `copybook-determinism`, `copybook-support-matrix` |
| Governance | `copybook-contracts`, `copybook-governance`, `copybook-governance-contracts`, `copybook-governance-grid`, `copybook-governance-runtime` |
| Safety | `copybook-error`, `copybook-error-reporter`, `copybook-overflow`, `copybook-safe-index`, `copybook-safe-ops`, `copybook-safe-text`, `copybook-utils` |
| Quality | `copybook-corruption`, `copybook-corruption-detectors`, `copybook-corruption-predicates`, `copybook-corruption-rdw` |
| Other | `copybook-arrow`, `copybook-sequence-ring` |

## Navigation
- Repository root + invariants: `../CLAUDE.md`
- Per-crate guidance: each `<crate>/claude.md`
- Canonical feature/error docs: `../docs/`

## Build
- `cargo build --workspace` (everything) or `cargo build -p <crate>` (one)
- `cargo test --workspace`
- `cargo clippy --workspace -- -D warnings -W clippy::pedantic`
