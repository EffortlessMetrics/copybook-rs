<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- LibFuzzer / `cargo-fuzz` targets for fuzzing codec and parser surfaces.
- Standalone; not a workspace member.

## Navigation
- Repository root + invariants: `../CLAUDE.md`
- Fuzz targets: `fuzz_targets/`

## Run
- `cargo +nightly fuzz run <target>` (requires nightly toolchain + `cargo-fuzz`)
