<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Dev-only crates (`publish = false`): benchmarks, fixture generation, maintenance scripts, and project automation.
- Not shipped to crates.io.

## Members
| Crate | Purpose |
|---|---|
| `copybook-bench` | Benchmark harness and baseline tooling (criterion) |
| `copybook-gen` | Test fixture and synthetic dataset generation |
| `copybook-scripts` | Native Rust replacements for small repo maintenance scripts |
| `xtask` | Project automation xtasks (CI/release helpers) |

## Navigation
- Repository root + invariants: `../CLAUDE.md`
- Perf receipts (bench output): `../scripts/bench/perf.json`

## Build
- `cargo build -p copybook-bench` etc.
- Run benchmarks: `cargo bench -p copybook-bench`
