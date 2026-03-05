<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# `copybook-bench`

Benchmark harness and baseline tooling for `copybook-rs`.

`copybook-bench` holds deterministic performance checks, baseline workflows, and issue-specific
regression suites used by CI and release validation.

## What it does

- Define and run Criterion benches (`decode_performance`, `comp3`, etc.).
- Run regression/acceptance test suites in CI-like conditions.
- Produce performance receipts used by gating and reporting scripts.
- Support optional modes:
  - `progressive` for complexity ramp-up scenarios
  - `diagnostics` for infrastructure overhead measurement
  - `perf` for CI-style performance runs

## Quick start

```bash
cargo bench -p copybook-bench
cargo bench -p copybook-bench --bench comp3
cargo test -p copybook-bench --features perf

cargo test -p copybook-bench --test baseline_reconciliation
cargo test -p copybook-bench --test regression_detection
cargo test -p copybook-bench --test ci_integration
```

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
