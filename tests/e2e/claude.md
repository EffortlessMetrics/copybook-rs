<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- End-to-end integration tests (crate `copybook-e2e`, `publish = false`).
- Exercises decode/encode round-trips, CLI subcommands, error taxonomy, determinism, and projection/dialect flags as an external user would.

## Navigation
- Repository root + invariants: `../../CLAUDE.md`
- Shared test helpers: `../common/`
- Error codes: `../../docs/reference/ERROR_CODES.md`

## Run
- `cargo test -p copybook-e2e`
