<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Workspace test-only members (`publish = false`) that exercise the published crates end-to-end.
- Separated from per-crate unit/integration tests so they can consume crates exactly as external users do.

## Members
| Suite | Framework | Focus |
|---|---|---|
| `bdd` | Cucumber/Gherkin (`copybook-bdd`) | Behavior scenarios |
| `e2e` | integration (`copybook-e2e`) | End-to-end decode/encode/CLI flows |
| `proptest` | proptest (`copybook-proptest`) | Property-based round-trip and invariants |

## Navigation
- Repository root + invariants: `../CLAUDE.md`
- Shared test helpers: `common/`

## Run
- `cargo test -p copybook-bdd`
- `cargo test -p copybook-e2e`
- `cargo test -p copybook-proptest`
- All together: `cargo test --workspace`
