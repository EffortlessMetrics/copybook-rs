<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# CI Current State Snapshot

**Captured:** 2026-05-09 (before rollout changes)

This document records the pre-rollout CI state so later PRs have an accurate
baseline and rollback reference.

## Workspace

| Property | Value |
|----------|-------|
| Rust edition | 2024 |
| MSRV | 1.92 (target after PR 01: 1.95) |
| Workspace crates | 36 publishable, 3 dev-only tools, 3 test suites |
| Resolver | 2 |

## Lint Profile (Pre-Rollout)

From `[workspace.lints]` in `Cargo.toml`:

```toml
[workspace.lints.rust]
unsafe_code = "forbid"

[workspace.lints.clippy]
# Panic prevention (warn — not deny)
unwrap_used = "warn"
expect_used = "warn"
panic = "warn"
unreachable = "warn"
todo = "warn"
unimplemented = "warn"

# Debug cruft
dbg_macro = "warn"

# Cast lints (allowed)
cast_lossless = "allow"
cast_possible_truncation = "allow"
cast_precision_loss = "allow"
cast_sign_loss = "allow"
```

Tests receive additional relaxation via `-A clippy::unwrap_used`,
`-A clippy::expect_used`, `-A clippy::panic`, etc. in the CI clippy step.

## Workflow Inventory (34 workflows)

### Core CI (`ci.yml`, 455 lines)

Jobs and their current behavior:

| Job | Runner | Default PR | Notes |
|-----|--------|-----------|-------|
| `fmt` | ubuntu | ✅ yes | Rustfmt check |
| `clippy` | ubuntu | ✅ yes | Pedantic + panic-deny for production; relaxed for tests |
| `test` | matrix | ✅ yes | **27 cells**: 3 OS × 3 toolchains × 3 features |
| `examples` | ubuntu | ✅ yes | Cargo examples build |
| `security` | ubuntu | ✅ yes | cargo-deny check |
| `determinism_smoke` | ubuntu | ✅ yes | Round-trip determinism smoke |
| `nightly_soak` | ubuntu | schedule | Runs nightly only |
| `bdd_tests` | ubuntu | ✅ yes | BDD governance tests |
| `rdw_iterator_tests` | ubuntu | ✅ yes | RDW iterator tests |
| `exit_code_matrix` | ubuntu | ✅ yes | CLI exit code matrix |
| `result_docs_advisory` | ubuntu | ✅ yes | Result type doc advisory |
| `coverage` | ubuntu | ✅ yes | tarpaulin coverage |
| `docs` | ubuntu | ✅ yes | rustdoc build |
| `strict_comments` | ubuntu | ✅ yes | Doc comment strictness |

### Additional Workflows

| Workflow file | Purpose | Default PR trigger |
|--------------|---------|-------------------|
| `api-freeze.yml` | API freeze check | PR (on Cargo.toml change) |
| `benchmark.yml` | Benchmark runner | manual / main |
| `changelog.yml` | Changelog validation | PR |
| `ci-bench.yml` | CI benchmark smoke | PR |
| `ci-comprehensive.yml` | Comprehensive CI | label / main |
| `ci-coverage.yml` | Coverage (alternate) | label |
| `ci-fuzz.yml` | Fuzz orchestration | nightly / manual |
| `ci-mutants.yml` | Mutation testing | nightly / manual |
| `ci-proptest.yml` | Proptest runs | label / main |
| `ci-quick.yml` | Quick CI gate | PR |
| `ci-sbom.yml` | SBOM generation | release |
| `ci-security.yml` | Security scan | PR / main |
| `ci-weekly-os.yml` | Weekly OS matrix | weekly schedule |
| `commit-lint.yml` | Commit message lint | PR |
| `coverage.yml` | Coverage upload | main |
| `determinism-smoke.yml` | Determinism standalone | PR / main |
| `docs-truth.yml` | Docs truth check | PR / main |
| `enterprise-perf.yml` | Enterprise perf | main / manual |
| `feature-flags.yml` | Feature flag check | PR |
| `fuzz-integration.yml` | Fuzz integration | nightly / manual |
| `leak-detection.yml` | Memory leak detection | main |
| `metrics-smoke.yml` | Metrics smoke | PR |
| `pedantic-diff.yml` | Pedantic diff check | PR |
| `perf-bench.yml` | Perf benchmarks | label / main |
| `perf-container.yml` | Perf in container | main / manual |
| `perf-validation.yml` | Perf SLO validation | main |
| `perf.yml` | Perf summary | main |
| `pr-bench-comment.yml` | PR bench comment | PR (comment bot) |
| `pr-insights.yml` | PR insights report | PR |
| `publish-dry-run.yml` | Publish dry run | release |
| `publish.yml` | Publish to crates.io | release |
| `security-scan.yml` | Security scan | PR |
| `soak.yml` | Nightly soak | nightly schedule |

## Current CI Issues

### Issue 1: 27-Job Test Matrix as Ordinary PR Default

The `test` job in `ci.yml` runs a 3×3×3 cross product on every PR push.
macOS at 10× multiplier dominates the budget.

**Target state after PR 10:** Ubuntu/stable/default only for ordinary PRs.

### Issue 2: Cache Saved on Every PR

`Swatinem/rust-cache@v2` saves cache on every run, including PRs.
This causes cache thrash as different PRs write different cache states.

**Target state after PR 08:** `save-if: github.ref == 'refs/heads/main'`

### Issue 3: Broad `cancel-in-progress: true`

Current concurrency uses `cancel-in-progress: true` globally. This can
cancel valuable label-triggered runs when a `synchronize` event arrives.

**Target state after PR 08:**
```yaml
cancel-in-progress: ${{ github.event_name == 'pull_request' && github.event.action == 'synchronize' }}
```

### Issue 4: Coverage as Ordinary PR Default

`coverage` runs on every PR. Coverage is execution-surface evidence, not
a merge gate for every commit.

**Target state after PR 11:** `main`, `coverage` label, `full-ci` label only.

### Issue 5: Clippy Test Carveout

Tests currently receive `-A clippy::unwrap_used -A clippy::expect_used
-A clippy::panic`. The target policy has no test carveouts.

**Target state after PR 01:** Tests compile under the same pedantic profile.

### Issue 6: xtask Policy Gap

`xtask` today has: docs sync, perf, pr-insights.
It is missing: audit-unwraps, audit-casts, check-file-policy,
check-no-panic-family, check-ci-lane-whitelist, policy-report.

**Target state after PR 02:** Full policy control plane in xtask.

## Rollback Reference

If any PR in this stack creates a regression, the rollback path is:

1. Revert the specific PR (not the entire stack).
2. Verify `cargo build --workspace` and `cargo test --workspace` still pass.
3. Document the regression in `docs/ci/current-state.md` under a new dated
   section so the next attempt can account for it.

The rollout PRs are intended to be individually revertible without affecting
product behavior.
