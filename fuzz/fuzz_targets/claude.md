<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Individual libFuzzer fuzz target binaries (one `.rs` per target).
- Belongs to the standalone `copybook-fuzz` crate at `../../fuzz`.

## Navigation
- Crate root: `../claude.md`
- Repository root: `../../CLAUDE.md`

## Run a target
- `cargo +nightly fuzz run <target_name>` from `../../fuzz`
