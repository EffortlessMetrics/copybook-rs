# CI Gating Policy

## Overview

This document describes the two-lane CI gating strategy for copybook-rs. The CI system is divided into two lanes:

1. **PR Lane** - Deterministic, fast, blocks merge
2. **Scheduled Lane** - Stochastic/expensive, non-blocking, runs on schedule

## PR Lane (Deterministic Gate)

The PR lane runs on every pull request to `main` and `develop` branches. All jobs in this lane must pass for a PR to be mergeable.

### Jobs

| Job | Description | Runtime |
|-----|-------------|---------|
| `lint` | `cargo fmt --check` + `cargo clippy --workspace --all-targets` | ~2 min |
| `test` | `cargo nextest run --workspace` | ~3 min |
| `proptest-smoke` | Bounded proptest (256 cases, seed "copybook-rs-proptest") | ~2 min |
| `determinism` | Determinism smoke test (advisory) | ~1 min |
| `security` | `cargo deny check` + `cargo audit` | ~1 min |

### Expected Total Runtime

- **Cold cache**: 8-10 minutes
- **Warm cache**: 4-6 minutes

### Job Details

#### `lint` Job
- Runs `cargo fmt --all -- --check` to verify formatting
- Runs `cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic`
- Runs `cargo clippy --workspace --tests --all-features -- -D warnings` with relaxed test lints
- Enforces panic prevention lints on shipped targets

#### `test` Job
- Runs `cargo nextest run --workspace --exclude copybook-bench --profile ci`
- Runs on Ubuntu, macOS, and Windows
- Tests against MSRV (1.92.0), stable, and beta Rust versions
- Tests with various feature combinations

#### `proptest-smoke` Job
- Runs property tests with bounded cases (256)
- Uses fixed seed "copybook-rs-proptest" for determinism
- Tests core and codec packages across multiple OS and Rust versions

#### `determinism` Job (Advisory)
- Runs determinism smoke tests
- Uses `continue-on-error: true` so failures don't block merge
- Provides early warning for potential non-deterministic behavior

#### `security` Job
- Runs `cargo deny check` on all PRs
- Runs `cargo audit` only when `Cargo.lock` changes
- Checks for known security vulnerabilities in dependencies

## Scheduled Lane (Stochastic/Expensive)

The scheduled lane runs on a nightly schedule and can be manually triggered via `workflow_dispatch`. These jobs are non-blocking and provide additional quality signals.

### Jobs

| Job | Description | Frequency | Runtime |
|-----|-------------|-----------|---------|
| `bdd-tests` | Full BDD test suite | Nightly | ~5 min |
| `proptest-extended` | Extended proptest (1024 cases) | Weekly | ~15 min |
| `fuzz` | Timeboxed fuzzing with crash artifacts | Weekly | ~60 min |
| `mutants` | Timeboxed mutation testing | Weekly | ~30 min |
| `bench` | Performance benchmarks with receipts | Nightly | ~20 min |

### Job Details

#### `bdd-tests`
- Runs full BDD test suite from `copybook-bdd` package
- Provides end-to-end validation of user-facing behavior

#### `proptest-extended`
- Runs property tests with extended cases (1024)
- Uses same fixed seed "copybook-rs-proptest" for reproducibility
- Tests across multiple OS and Rust versions

#### `fuzz`
- Runs extended proptest fuzzing on key targets:
  - zoned-decimal encoding
  - comp3 roundtrip
  - JSON fuzzing
- Timeboxed to 15 minutes per target
- Uploads crash artifacts for analysis

#### `mutants`
- Runs mutation testing on workspace
- Timeboxed to prevent excessive runtime
- Generates trend reports for mutation score
- Can be scoped to specific crates (core, codec, cli)

#### `bench`
- Runs performance benchmarks
- Generates receipts in JSON format
- Compares against baseline for regression detection
- Uploads artifacts for historical tracking

## Promoting Tests from Scheduled to PR Lane

To promote a test from the scheduled lane to the PR lane:

1. **Evaluate determinism**: The test must produce consistent results across runs
2. **Assess runtime**: The test should complete in under 5 minutes
3. **Update workflow**: Move the test configuration to the PR lane workflow
4. **Update documentation**: Reflect the change in this document

### Promotion Criteria

| Criterion | PR Lane | Scheduled Lane |
|-----------|---------|----------------|
| Determinism | Required | Optional |
| Runtime | < 5 min | Flexible |
| Blocking | Yes | No |
| Cases/Iterations | Bounded | Extended |

## Running Tests Locally

### PR Lane Tests

```bash
# Run all PR gate tests locally
just pr

# Equivalent to:
bash scripts/ci/quick.sh
bash scripts/ci/security.sh
```

### Scheduled Lane Tests

```bash
# Run scheduled tests locally (optional, for validation)
just scheduled

# Run specific scheduled tests
just bench
just mutants
```

## Workflow Files

| Workflow | Lane | Events |
|----------|------|--------|
| `.github/workflows/ci.yml` | PR + Scheduled | `push`, `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/ci-proptest.yml` | PR + Scheduled | `push`, `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/ci-fuzz.yml` | Scheduled | `workflow_dispatch` |
| `.github/workflows/ci-mutants.yml` | Scheduled | `schedule`, `workflow_dispatch` |
| `.github/workflows/perf.yml` | Scheduled | `schedule`, `workflow_dispatch` |
| `.github/workflows/determinism-smoke.yml` | PR (advisory) | `push`, `pull_request`, `workflow_dispatch` |

## CI Status Checks

### Required for Merge

- `lint` job must pass
- `test` job must pass
- `proptest-smoke` job must pass
- `security` job must pass

### Advisory Only

- `determinism` job failures should be investigated but don't block merge

### Non-Blocking

- All scheduled lane jobs provide quality signals but don't block merge

## Troubleshooting

### PR Lane Failures

1. **Lint failures**: Run `cargo fmt --all` and fix clippy warnings
2. **Test failures**: Run `cargo nextest run --workspace` locally
3. **Proptest failures**: Check for regressions in `proptest-regressions/` directories
4. **Security failures**: Update dependencies or add advisories to `deny.toml`

### Scheduled Lane Failures

1. **BDD failures**: May indicate behavioral regressions requiring investigation
2. **Fuzz failures**: Check crash artifacts and file issues for found bugs
3. **Mutant survivors**: Consider adding tests for uncaught mutants
4. **Bench regressions**: Investigate performance degradations > 5%

## Concurrency

All workflows use concurrency groups to cancel superseded runs:

```yaml
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}-${{ github.event_name }}
  cancel-in-progress: true
```

This ensures that:
- Only one run per workflow/branch/event type is active
- New commits cancel in-progress runs
- Resources are used efficiently
