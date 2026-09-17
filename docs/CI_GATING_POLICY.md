<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# CI Gating Policy

## Overview

This document describes the two-lane CI gating strategy for copybook-rs. The CI system is divided into two lanes:

1. **PR Lane** - Deterministic, fast, blocks merge
2. **Scheduled Lane** - Stochastic/expensive, non-blocking, runs on schedule

## PR Lane (Deterministic Gate)

The PR lane runs on `pull_request` to `main`. Most PR workflows also trigger
on `push`, `schedule`, or `workflow_dispatch`; the table below records what
actually runs on PRs. Jobs without job-level `continue-on-error` must pass.
The required/advisory mapping is executable in
`scripts/ci/pr_head_status.py` (`POLICY`), which binds one assessment to the
current PR head SHA. Hosted branch-protection contents are not visible to
read-only tokens and are recorded as unknown by the helper, never assumed.

### Jobs (workflow evidence as of #994)

Unconditional checks run on every PR; conditional checks run when their
trigger paths or changes-gate select them, and are required when present.
Skipped checks (e.g. schedule-scoped jobs evaluated on a PR) are recorded,
never counted as passes.

| Check (as reported) | Workflow / job | Lane | Presence |
|-----|-----|-----|-----|
| `Rustfmt` | `ci.yml` / `fmt` | required | expected |
| `Clippy` | `ci.yml` / `clippy` | required | expected |
| `Test Suite (…)` | `ci.yml` / `test` (OS × toolchain × features matrix) | required | expected |
| `Build Examples (…)` | `ci.yml` / `examples` | required | expected |
| `Security Checks` | `ci.yml` / `security` (`cargo deny`; audit only if `Cargo.lock` changed) | required | expected |
| `Determinism Smoke` | `ci.yml` / `determinism-smoke` | required | expected |
| `Governance + BDD Smoke` | `ci.yml` / `bdd-tests` | required | expected |
| `RDW iterator tests` | `ci.yml` / `rdw-iterator-tests` | required | expected |
| `Exit code mapping (…)` | `ci.yml` / `exit-code-matrix` | required | expected |
| `Code Coverage` | `ci.yml` / `coverage` | required | expected |
| `Documentation` | `ci.yml` / `docs` | required | expected |
| `Strict Comments Mode` | `ci.yml` / `strict-comments` | required | expected |
| `Result Docs Advisory` | `ci.yml` / `result-docs-advisory` (job-level `continue-on-error`) | advisory | conditional |
| `Validate PR Title` | `commit-lint.yml` | required | expected |
| `API Freeze Check` | `api-freeze.yml` | required | expected |
| `Determinism smoke (codec + CLI)` | `determinism-smoke.yml` | required | expected |
| `validate-receipt`, `check-governance` | `perf-validation.yml` | required | expected |
| `Property Tests - Core/Codec/Integration (…)` | `ci-proptest.yml` (OS × toolchain matrix) | required | expected |
| `Property Test Summary` | `ci-proptest.yml` / `proptest-summary` | required | expected |
| `insights` | `pr-insights.yml` (same-repo PRs; forks record a skip) | required | expected |
| `truth` | `docs-truth.yml` (docs changes-gate) | required | conditional |
| `Classify changelog requirement`, `Validate Changie` | `changelog.yml` (changes-gate) | required | conditional |
| `changes`, `test (…)`, `testExpected` | `ci-quick.yml` / `feature-flags.yml` (changes-gates) | required | conditional |
| `test-features-module`, `test-cli-integration` | `feature-flags.yml` (changes-gate) | required | conditional |
| `Publish Plan Check` | `publish-plan-check.yml` (trigger path filter) | required | conditional |
| `Performance gate` | `perf-gate.yml` (trigger path filter) | required | conditional |
| `Memory Leak Detection (LSAN)` | `leak-detection.yml` (see #1000 for analyzer truthfulness) | required | conditional |
| `RIPR test-oracle pilot (advisory)` | `ripr.yml` (job-level `continue-on-error`) | advisory | conditional |
| `Coverage Diff` | `ci-coverage.yml` (job-level `continue-on-error`) | advisory | conditional |
| `codecov/*` | external coverage reporter | advisory | conditional |

Note: an earlier revision of this document described `lint`, `test`,
`proptest-smoke`, `determinism` (advisory), and `security` jobs and claimed
the determinism smoke lane was advisory via `continue-on-error`. The current
`ci.yml` has no job-level `continue-on-error` on `determinism-smoke`, so the
helper maps `Determinism Smoke` as required per workflow evidence. If the
maintainers intend that lane to be advisory, the workflow (not this prose)
must change, and the helper policy with it.

### Expected Total Runtime

- **Cold cache**: 8-10 minutes for the fast gates; full matrix longer
- **Warm cache**: 4-6 minutes for the fast gates

### Job Details

#### Format and clippy gates
- Runs `cargo fmt --all -- --check` to verify formatting
- Runs `cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic`
- Runs `cargo clippy --workspace --tests --all-features -- -D warnings` with relaxed test lints
- Enforces panic prevention lints on shipped targets

#### `Test Suite` matrix
- Runs `cargo nextest run --workspace --exclude copybook-bench --exclude copybook-bdd --profile ci`
- Runs on Ubuntu, macOS, and Windows
- Tests against MSRV (1.98.0), stable, and beta Rust versions
- Tests with various feature combinations

#### Property tests
- `ci-proptest.yml` runs core, codec, and integration property suites across the OS and toolchain matrix on every PR
- Bounded cases with fixed seeds for determinism; extended runs stay scheduled

#### Determinism smoke lanes
- `ci.yml` / `determinism-smoke` and `determinism-smoke.yml` both report on PRs with no job-level `continue-on-error`; both are required per workflow evidence (see the note above if advisory was intended)

#### `Security Checks`
- Runs `cargo deny check` on all PRs
- Runs `cargo audit` only when `Cargo.lock` changes
- Checks for known security vulnerabilities in dependencies

## Scheduled Lane (Stochastic/Expensive)

The scheduled lane runs on workflow-specific nightly or weekly cadences. Where
configured, it can also be triggered manually via `workflow_dispatch`. These
jobs are non-blocking and provide additional quality signals.

### Jobs

| Job | Description | Frequency | Runtime |
|-----|-------------|-----------|---------|
| `bdd-tests` | Full BDD test suite | Nightly | ~5 min |
| `proptest-extended` | Extended proptest (1024 cases) | Weekly | ~15 min |
| `fuzz` | Timeboxed fuzzing with crash artifacts | Weekly | ~60 min |
| `mutants` | Timeboxed mutation testing | Weekly | ~30 min |
| `bench` | Performance benchmarks with receipts | Nightly | ~20 min |
| `soak` | Sealed benchmark receipt plus enforced absolute floors | Weekly | ~20 min |

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

#### `soak`
- Is owned by the dedicated `.github/workflows/soak.yml` workflow
- Runs the canonical in-memory DISPLAY-heavy and COMP-3-heavy Criterion workloads
- Validates the sealed receipt, then evaluates the 80/8 MiB/s absolute floors
- Publishes the receipt; the workflow job conclusion records the gate decision
- Can be manually dispatched with `bash scripts/soak-dispatch.sh`
- Does not exercise generated fixed/RDW datasets, code-page/size axes, or prove leak absence

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
| `.github/workflows/ci.yml` | PR (+ scheduled) | `push`, `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/ci-proptest.yml` | PR (+ scheduled) | `push`, `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/ci-quick.yml` | PR (changes-gated) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/feature-flags.yml` | PR (changes-gated) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/changelog.yml` | PR (changes-gated) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/docs-truth.yml` | PR (docs changes-gate) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/commit-lint.yml` | PR | `pull_request` |
| `.github/workflows/api-freeze.yml` | PR | `pull_request`, `workflow_dispatch` |
| `.github/workflows/publish-plan-check.yml` | PR (trigger path filter) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/coverage.yml` | PR (trigger path filter) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/determinism-smoke.yml` | PR | `push`, `pull_request`, `workflow_dispatch` |
| `.github/workflows/perf-validation.yml` | PR | `pull_request` |
| `.github/workflows/perf-gate.yml` | PR (trigger path filter) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/pr-insights.yml` | PR (same-repo only) | `pull_request` |
| `.github/workflows/leak-detection.yml` | PR (+ scheduled) | `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/msrv-standalone.yml` | PR (trigger path filter) | `pull_request`, `workflow_dispatch` |
| `.github/workflows/ripr.yml` | PR pilot, explicitly advisory | `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/ci-comprehensive.yml` | PR (skips outside scope) + scheduled | `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/fuzz-integration.yml` | PR (+ scheduled) | `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/ci-coverage.yml` | PR (+ scheduled) | `pull_request`, `schedule`, `workflow_dispatch` |
| `.github/workflows/ci-fuzz.yml` | Dispatch-only | `workflow_dispatch` |
| `.github/workflows/ci-security.yml` | Scheduled | `schedule`, `workflow_dispatch` |
| `.github/workflows/perf-bench.yml`, `perf.yml`, `soak.yml`, `benchmark.yml` | Scheduled | `schedule`, `workflow_dispatch` |
| _(mutation testing has no CI workflow; local-only via `just mutants`)_ | — | — |

## CI Status Checks

The executable form of this section is `scripts/ci/pr_head_status.py`
(`POLICY`): one assessment bound to the current PR head SHA.

### Required for Merge

- Every unconditional PR check listed as required/expected in the Jobs table
  must pass on the current head.
- Every conditional required check that ran on the current head must pass.
- No unmapped (unclassified) non-skipped check may remain: classify it in
  the helper policy first.

### Advisory Only

- `Result Docs Advisory` (job-level `continue-on-error`), `Coverage Diff`,
  and the `RIPR test-oracle pilot (advisory)` are recorded but never block.
- External reporters (`codecov/*`, secret-scanner and review-bot checks) are
  informational; review-bot findings are still evaluated on their merits per
  `docs/design/AGENTIC_PR_OPERATIONS.md`.

### Non-Blocking

- All scheduled and dispatch-only lane jobs provide quality signals but don't block merge.
- A skipped check is recorded as skipped, never as a pass.

### Hosted protection visibility

The hosted branch ruleset contents are not visible to read-only tokens, so
this policy cannot confirm which checks GitHub itself requires. That gap is
recorded as unknown by the helper rather than assumed. Any actual
ruleset change remains a separate maintainer decision.

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
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
