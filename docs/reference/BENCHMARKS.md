# Benchmark Receipts and Performance Tracking

This document describes copybook-rs's performance benchmarking infrastructure, including how to run benchmarks locally, interpret results, and understand the CI integration.

## Overview

copybook-rs uses a **receipt-based benchmarking system** to track performance over time and detect regressions. Benchmarks emit machine-readable JSON "receipts" that capture:

- **Performance metrics**: Throughput measurements (GiB/s for DISPLAY, MiB/s for COMP-3)
- **Run metadata**: Git SHA, Rustc version, timestamp, runner environment
- **SLO validation**: Pass/warn/fail status against Service Level Objectives

## Receipt Format

### Directory Structure

Benchmark receipts are written to `target/benchmarks/<timestamp>/`:

```
target/benchmarks/
  2025-11-12T20-30-45/          # Timestamped run
    perf.json                   # Performance report
    meta.json                   # Run metadata
```

### perf.json Schema

```json
{
  "display_gibs": 4.22,         // DISPLAY throughput in GiB/s (optional)
  "comp3_mibs": 571.0,          // COMP-3 throughput in MiB/s (optional)
  "timestamp": "2025-11-13T03:50:15.165810873+00:00",  // ISO 8601 timestamp
  "commit": "a2f53eb",          // Git commit SHA (short)
  "status": "success",          // "success", "warning", or "failure"
  "warnings": [],               // Performance warnings (array of strings)
  "errors": []                  // Performance errors (array of strings)
}
```

### meta.json Schema

```json
{
  "git_sha": "a2f53eb",                    // Git commit SHA (short)
  "rustc_version": "rustc 1.90.0 (...)",  // Rustc version string
  "timestamp_utc": "2025-11-13T03:50:15.165810873+00:00",  // ISO 8601 timestamp
  "bench_crate": "copybook-bench",        // Benchmark crate name
  "runner_os": "linux",                   // Operating system
  "cpu_count": 32                         // Number of CPUs
}
```

## Service Level Objectives (SLOs)

**Targets (advisory-only)**:
- **DISPLAY**: ≥80 MiB/s
- **COMP-3**: ≥40 MiB/s

In v0.4.0 these floors are tracked for visibility and regression discussion; they are not enforced as hard CI gates by default. See [ROADMAP.md](../ROADMAP.md) for current policy and guidance.

## Running Benchmarks Locally

### Basic Usage

```bash
# Run performance benchmarks and generate receipts
cargo run -p xtask -- perf

# Run with SLO enforcement (fails if SLOs violated)
cargo run -p xtask -- perf --enforce

# Run with custom output directory
cargo run -p xtask -- perf --out-dir target/benchmarks/my-run
```

### Interpreting Results

The `xtask perf` command will:

1. Run `cargo test -p copybook-bench --features perf` to execute performance tests
2. Parse test output to extract DISPLAY and COMP-3 throughput metrics
3. Validate metrics against SLOs
4. Generate `perf.json` and `meta.json` in the output directory
5. Print a summary with status indicators:
   - ✅ Success: All metrics meet SLOs
   - ⚠️ Warning: Metrics close to SLO thresholds (within 5%)
   - ❌ Failure: Metrics below SLO thresholds

Example output:

```
🚀 Running performance benchmarks...
📊 Running copybook-bench --features perf tests...

✅ Benchmark receipts written:
   target/benchmarks/2025-11-12T20-30-45/perf.json
   target/benchmarks/2025-11-12T20-30-45/meta.json

📊 DISPLAY: 4.22 GiB/s, COMP-3: 571 MiB/s ✅
```

### Performance Test Suites

The `--features perf` flag enables performance-specific tests that include:

- **`test_display_throughput_with_encoding_detection`**: Tests DISPLAY-heavy workloads (100,000 records, 180 bytes each)
- **`test_comp3_throughput_with_minimal_regression`**: Tests COMP-3-heavy workloads (500,000 records, 24 bytes each)

These tests are marked with `#[cfg_attr(not(feature = "perf"), ignore = "perf-only")]` to exclude them from normal test runs.

## CI Integration

### Perf Benchmarks Workflow

The `.github/workflows/perf-bench.yml` workflow:

- **Triggers**: Manual dispatch (`workflow_dispatch`) and daily scheduled runs at 4 AM UTC
- **Runner**: Ubuntu latest
- **Concurrency**: Protects against concurrent runs with workflow-level concurrency group
- **Outputs**: Uploads `target/benchmarks/**/*.json` as artifacts named `bench-receipts-<sha>`

### Non-Blocking Status

Currently, the perf workflow is **non-blocking** (uses `continue-on-error: true`). This means:

- Performance regressions will be reported but won't fail the build
- Receipts are still generated and uploaded for historical tracking
- Future PRs can promote this to a required check when baseline stability is established

See Issue #66 for plans to make this a gating check.

## Baseline Management

The `bench-report` CLI (in `copybook-bench/src/bin/bench-report.rs`) provides local baseline management:

```bash
# Validate a performance report
cargo run --bin bench-report -p copybook-bench -- validate target/benchmarks/*/perf.json

# Promote a report to the main baseline
cargo run --bin bench-report -p copybook-bench -- baseline promote target/benchmarks/*/perf.json

# Show current baseline
cargo run --bin bench-report -p copybook-bench -- baseline show

# Compare a report against the baseline (5% regression threshold)
cargo run --bin bench-report -p copybook-bench -- compare target/benchmarks/*/perf.json

# Show baseline and SLO summary
cargo run --bin bench-report -p copybook-bench -- summary
```

### Baseline Storage

Baselines are stored in `target/baselines/performance.json` with:

- **Current baseline**: Most recent promoted baseline
- **History**: Previous baselines (90-day retention policy)
- **Metadata**: Branch, commit, timestamp, sample count

## Performance Considerations

### Debug vs Release Builds

**Important**: The `xtask perf` command runs benchmarks in **debug mode** by default, which will show significantly lower throughput than release builds. For accurate performance measurements:

```bash
# Build and run in release mode for accurate results
cargo build --release -p xtask
cargo run --release -p xtask -- perf
```

Expected differences:
- Debug mode: ~0.02 GiB/s (DISPLAY), ~2 MiB/s (COMP-3)
- Release mode: ~205 MiB/s (DISPLAY), ~58 MiB/s (COMP-3) on reference hardware

### Environment Variability

Performance can vary based on:

- **CPU**: Different processors show different throughput (see [copybook-bench/HARDWARE_SPECS.md](../../copybook-bench/HARDWARE_SPECS.md))
- **OS**: WSL2 shows 10-30% overhead vs native Linux
- **System load**: Background processes can impact measurements
- **Variance**: Expect ~5% (DISPLAY) to ~8% (COMP-3) run-to-run variance

See [copybook-bench/BASELINE_METHODOLOGY.md](../../copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures.

## Related Documentation

- **[CLAUDE.md](../../CLAUDE.md)**: Performance targets, baseline methodology, known limitations
- **[ROADMAP.md](../ROADMAP.md)**: Performance improvement plans and v1.0 stability goals
- **[REPORT.md](../REPORT.md)**: Detailed readiness assessment and performance analysis
- **[copybook-bench/BASELINE_METHODOLOGY.md](../../copybook-bench/BASELINE_METHODOLOGY.md)**: Baseline establishment procedures
- **[copybook-bench/HARDWARE_SPECS.md](../../copybook-bench/HARDWARE_SPECS.md)**: Reference hardware specifications

## Future Enhancements

Planned improvements (see Issue #66):

1. **Per-scenario receipts**: Capture metrics for each test scenario (not just DISPLAY/COMP-3 summary)
2. **Regression detection**: Automated comparison against historical baselines in CI
3. **Promotion workflow**: Automated baseline promotion on main branch merges
4. **Dashboard**: Historical performance tracking dashboard
5. **Gating checks**: Promote perf workflow to required CI check when baseline is stable
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
