# `copybook-bench`

`copybook-bench` is the benchmark and performance baseline crate for copybook-rs.

## Purpose

- Run baseline and regression-oriented benchmark tests.
- Produce performance receipts for CI and reporting workflows.
- Provide benchmarks for:
  - `decode_performance`
  - `comp3`
  - `progressive` (feature-gated via `PERF=1`)
  - `diagnostics_benches` (feature-gated via `--features diagnostics`)

## Quick Start

```bash
# Run all bench tests (including issue-specific test suites)
cargo test -p copybook-bench

# Run issue 49-style test suites
cargo test -p copybook-bench --test baseline_reconciliation
cargo test -p copybook-bench --test regression_detection
cargo test -p copybook-bench --test ci_integration

# Run perf tests (writes perf receipts under `target/benchmarks`)
cargo test -p copybook-bench --features perf

# Run a specific benchmark
cargo bench -p copybook-bench --bench comp3
```

## Feature Flags

- `progressive`: enables progressive complexity performance benchmarking.
- `diagnostics`: enables infrastructure overhead diagnostics benchmark.
- `perf`: activates dedicated perf test mode used by release/CI SLO checks.

## Directory Layout

- `tests/` – test suites and acceptance criteria scaffolding (including `README.md` for issue-specific context)
- `src/` – benchmark support and reporting infrastructure
- `benches/` – Criterion bench modules
- `test_fixtures/` – deterministic fixture data

## License

Part of `copybook-rs`, licensed under `AGPL-3.0-or-later`.
