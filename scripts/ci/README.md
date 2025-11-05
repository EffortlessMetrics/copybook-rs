# CI Scripts (Local-First)

## Overview

These scripts are the **single source of truth** for CI validation. Run them locally before pushing to ensure CI will pass.

## Design Principles

- **Local-first**: Exact commands run in CI are runnable locally
- **Fast feedback**: Most PRs complete in <10 min
- **Cost-controlled**: Heavy jobs (benches, coverage) are opt-in
- **Path-scoped**: Only runs if relevant files changed
- **Deterministic**: Same input → same output

## Scripts

### `quick.sh` — Fast Quality Gates

Runs fmt, clippy, build, tests, doctests. This is what runs on every PR.

```bash
# Run locally
./scripts/ci/quick.sh

# What it does:
# 1. cargo fmt --all --check
# 2. cargo clippy (pedantic, all warnings as errors)
# 3. cargo build --workspace --release
# 4. cargo nextest run (bounded parallelism)
# 5. cargo test --doc (with warnings denied)
```

**Expected time**: 5-10 minutes (cold), 2-5 minutes (warm)

### `security.sh` — Security Scanning

Runs cargo-deny always, cargo-audit only when Cargo.lock changes.

```bash
# Run locally
BASE_SHA=$(git merge-base origin/main HEAD) \
HEAD_SHA=$(git rev-parse HEAD) \
./scripts/ci/security.sh

# What it does:
# 1. cargo deny check (licenses, advisories, bans)
# 2. cargo audit (only if Cargo.lock changed or scheduled)
```

**Expected time**: 1-2 minutes

### `bench.sh` — Benchmark Receipts (Opt-in)

Generates `target/perf.json` with performance receipts. **Not run by default** on PRs.

```bash
# Run locally
./scripts/ci/bench.sh

# What it does:
# 1. cargo bench -p copybook-bench (with PERF=1)
# 2. Outputs target/perf.json
# 3. Validates with bench-report (if available)
# 4. Compares against baseline (non-fatal if missing)
```

**Expected time**: 10-15 minutes
**Trigger in CI**: Add `perf:run` label or manual dispatch

## Local Validation (Full Suite)

Run everything before pushing:

```bash
# Set up environment
export BASE_SHA=$(git merge-base origin/main HEAD)
export HEAD_SHA=$(git rev-parse HEAD)

# Run all gates
./scripts/ci/quick.sh && \
./scripts/ci/security.sh && \
./scripts/ci/bench.sh
```

## Prerequisites

### Required Tools
- `cargo` (Rust 1.90+ / MSRV)
- `cargo-nextest` (install: `cargo install nextest`)
- `cargo-deny` (install via CI or `cargo install cargo-deny`)

### Optional Tools
- `cargo-audit` (for security.sh when Cargo.lock changes)
- `bench-report` (built from copybook-bench for bench.sh)

## CI Workflows

### PR Quick Gates (`.github/workflows/ci-quick.yml`)

**Trigger**: PR opened/updated (except docs-only changes)
**Runs**: quick.sh + security.sh
**Cost**: ~$0.10 per PR (Ubuntu, 5-10 min)

### Weekly Security Scan (`.github/workflows/ci-security.yml`)

**Trigger**: Every Monday 05:17 UTC + manual
**Runs**: Full security audit (deny + audit)
**Cost**: ~$0.02 per run

### Bench Receipts (`.github/workflows/ci-bench.yml`)

**Trigger**: Manual dispatch or `perf:run` label
**Runs**: bench.sh
**Artifacts**: `perf.json` (7-day retention PR, 90-day main)
**Cost**: ~$0.50 per run (30-min timeout)

### Weekly Cross-OS (`.github/workflows/ci-weekly-os.yml`)

**Trigger**: Every Saturday 03:23 UTC + manual
**Runs**: quick.sh on Linux (+ macOS/Windows on manual only)
**Cost**: ~$0.02 (Linux only), ~$0.50 (all platforms)

## Cost Control Features

1. **Path filters**: Skip runs if only docs changed
2. **Concurrency control**: Cancel stale runs on new push
3. **Timeouts**: All jobs capped (15-30 min)
4. **Opt-in heavy jobs**: Benches/coverage only when requested
5. **Platform gating**: macOS/Windows only on manual dispatch
6. **Short retention**: PR artifacts expire in 7 days

## Troubleshooting

### "cargo-nextest not found"
```bash
cargo install nextest
# or
cargo binstall nextest
```

### "cargo-deny not found"
```bash
cargo install cargo-deny
```

### Bench script fails
- Ensure `copybook-bench` crate exists
- Build bench-report: `cargo build --release -p copybook-bench --bin bench-report`
- Check that PERF=1 is set

### Security script fails on audit
- cargo-audit requires network access
- Only runs if Cargo.lock changed (by design)
- Can skip locally if not testing Cargo.lock changes

## Performance Policy

**Current**: Advisory-only (accuracy-first posture)

- Benchmarks generate receipts (`perf.json`) but **do not block** PRs
- Local validation available anytime via `bench.sh`
- CI benches **opt-in** via `perf:run` label or manual dispatch
- Baseline comparison non-fatal if baseline missing

## Next Steps

1. **Enable CI**: Workflows are ready, test with a small PR
2. **Create labels**: `perf:run` label for benchmark triggers
3. **Document baselines**: Link to current baseline artifacts
4. **Monitor costs**: GitHub Actions usage dashboard

## References

- Roadmap: Issue #75 (Phase 0 complete, Phase 1-4 planned)
- Benchmark infrastructure: Issue #66 (80% complete)
- COBOL Support Matrix: Issue #111 (planned)
- Security scanning: Issue #35 (completed via cargo-deny)
