<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# LEM Budgeting

## LEM Definition

**LEM (Linux Equivalent Minute)** is the unit of CI cost used throughout
this rollout.

```
1 LEM = 1 minute on ubuntu-latest at $0.008 USD
```

All other runners are expressed as multiples of the ubuntu-latest rate.

## Runner Multipliers

| Runner | LEM multiplier | Notes |
|--------|---------------|-------|
| `ubuntu-latest` | 1.0 | Base unit |
| `ubuntu-22.04` | 1.0 | Same as ubuntu-latest |
| `windows-latest` | 2.0 | GitHub-hosted Windows |
| `macos-latest` | 10.0 | GitHub-hosted macOS |
| Nix build | 4.0 | Nix-based builds |
| WASM | 2.0 | WASM-targeted jobs |
| Docker build | 6.0 | Container image builds |
| External AI review | 4.0 | AI-assisted review comments |

## LEM Band Table

| Band | Range | Cost estimate | Meaning |
|------|-------|---------------|---------|
| Pennies | 0–12 | < $0.10 | Docs, metadata, light lint-only PRs |
| Default | 13–35 | $0.10–$0.28 | Ordinary Rust PR (preferred target) |
| Elevated | 36–75 | $0.29–$0.60 | Parser/file-format/expanded surface PR |
| High | 76–125 | $0.61–$1.00 | Explicit expensive PR |
| Over ceiling | > 125 | > $1.00 | Requires `ci-budget-ack` or `full-ci` label |

## LEM Calculation

For a single job:

```
job_lem = ceiling(wall_minutes × runner_multiplier × parallelism_factor)
```

For a matrix job:

```
matrix_lem = sum(job_lem for each matrix cell that actually runs)
```

For a complete PR run:

```
pr_lem = sum(job_lem for all jobs that run on the PR)
```

## LEM Estimation (Pre-Actuals)

Before actuals exist, use static floor estimates from `policy/ci-budget.toml`.

After ≥2 weeks of actuals, use the learned model:

```
estimate = max(static_floor, p50_recent_actual × 1.15)
warning_threshold = p90_recent_actual
hard_planning_threshold = p95_recent_actual
```

## LEM Accounting for the Current Test Matrix

The current `test` job (before PR 10 slims it) runs:

```
3 OS (ubuntu, macOS, windows) ×
3 Rust toolchains (MSRV 1.92, stable, beta) ×
3 feature sets ("", comp3_fast, audit)
= 27 matrix cells
```

Estimated LEM per matrix cell:

| OS | Toolchain | Multiplier | Est. wall min | Est. LEM |
|----|-----------|------------|---------------|----------|
| ubuntu | stable | 1.0 | 8 | 8 |
| ubuntu | MSRV | 1.0 | 10 | 10 |
| ubuntu | beta | 1.0 | 9 | 9 |
| macOS | stable | 10.0 | 8 | 80 |
| macOS | MSRV | 10.0 | 10 | 100 |
| macOS | beta | 10.0 | 9 | 90 |
| windows | stable | 2.0 | 12 | 24 |
| windows | MSRV | 2.0 | 14 | 28 |
| windows | beta | 2.0 | 13 | 26 |

For 3 feature sets each, and 27 cells total, the matrix alone is well above
the 35-LEM default ceiling. This motivates PR 10 (slim ordinary PR gate).

## Enforcement Timeline

| Milestone | Action |
|-----------|--------|
| PR 07 merged | PR Plan uploads LEM forecasts (advisory) |
| PR 15 merged | `ci-actuals.json` artifacts begin accumulating |
| 2 weeks of actuals | PR 16 adds soft warnings (36–75 elevated, 76–125 high) |
| 4 weeks of actuals | PR 18 switches to learned estimates (p50 × 1.15) |
| PR 17 merged | `PR Gate Success` becomes the single required check |
| Hard ceiling gate | Only activated after Phase 3 conditions are met |

## Labels That Affect LEM Accounting

| Label | Effect |
|-------|--------|
| `full-ci` | All lanes run; LEM ceiling suspended |
| `ci-budget-ack` | Acknowledges elevated spend; soft warning suppressed |
| `ci-budget-override` | Overrides hard ceiling; requires justification in PR body |
| `platform-matrix` | Adds macOS/Windows/beta; LEM rises to elevated or high band |
| `coverage` | Adds coverage lane (adds ~10 LEM) |
| `mutation` | Adds mutation testing lane (adds ~30 LEM) |
| `fuzz` | Adds fuzz integration lane (adds ~20 LEM) |
| `perf` | Adds benchmark/perf lanes (adds ~15 LEM) |
| `security-audit` | Adds full security audit lane (adds ~5 LEM) |
| `property-tests` | Adds heavy proptest runs (adds ~8 LEM) |
| `wasm` | Adds WASM build/test (adds ~10 LEM) |
| `arrow` | Adds Arrow integration tests (adds ~6 LEM) |
| `release-check` | Adds publish dry-run and release checks (adds ~8 LEM) |
