<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Property-based tests (crate `copybook-proptest`, `publish = false`) using `proptest`.
- Targets round-trip determinism and codec invariants.

## Navigation
- Repository root + invariants: `../../CLAUDE.md`
- Shared test helpers: `../common/`
- Determinism policy: `../../docs/PERFORMANCE_GOVERNANCE.md`

## Run
- `cargo test -p copybook-proptest`
