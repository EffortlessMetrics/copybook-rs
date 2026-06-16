<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Benchmark harness and baseline tooling for copybook-rs (crate `copybook-bench`, `publish = false`).
- Uses `criterion`. Baselines feed the canonical perf receipts in `../../scripts/bench/perf.json`.

## Navigation
- Tools index: `../claude.md`
- Repository root + invariants: `../../CLAUDE.md`
- Performance policy: `../../docs/PERFORMANCE_GOVERNANCE.md`
- Perf receipts: `../../scripts/bench/perf.json`
- Sources: `src/`, binaries: `src/bin/`, benchmarks: `benches/`

## Run
- `cargo bench -p copybook-bench`
- Build only: `cargo build -p copybook-bench --all-features`
