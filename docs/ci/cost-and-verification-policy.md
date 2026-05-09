<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Cost and Verification Policy

## Intent

`copybook-rs` needs strong verification. The goal is not to reduce it.

The goal is to make verification **scoped**: each PR buys the cheapest
deterministic proof relevant to its actual changed risk surface, while
`main`, nightly, release triggers, and explicit labels preserve the full
deep validation that correctness and release readiness require.

## What This Policy Does Not Do

- Does not remove any existing verification lane.
- Does not weaken product testing for COBOL parsing, EBCDIC conversion,
  COMP-3/overpunch/zoned decimal, RDW records, deterministic conversion,
  CLI behavior, governance contracts, or fixture proof.
- Does not reduce test count.
- Does not introduce test carveouts for Clippy lint profiles.

## What This Policy Does

- Routes expensive lanes to where they buy signal (main, labels, nightly,
  release triggers) rather than running them on every push.
- Makes CI cost visible via LEM forecasts before a PR spends that cost.
- Makes exceptions reviewable and expiring via TOML ledgers rather than
  silent CI flag proliferation.
- Creates a default ordinary PR path that is deterministic, fast, and
  provably sufficient for the changed surface.

## Economics Reference Point

OpenClaw's published Blacksmith runner spend of roughly `$511k`, mapped
against commit volume since February, works out directionally to about
`$20 per commit` on Blacksmith alone. Because OpenClaw appears to
squash-merge PRs, commit count is a reasonable proxy for merged PR count,
though the number is directional.

We do not read that as "OpenClaw tests too much." We read it as evidence
that verification demand is rising faster than verification efficiency.
Agentic development requires **more** verification than conventional PR
workflows, not less.

`copybook-rs` is targeting a different verification economics model:

| Technique | Economics |
|-----------|-----------|
| Rust compile-time checks | Near-zero marginal cost per PR |
| Clippy pedantic/strict | Near-zero marginal cost per PR |
| TOML policy ledgers | Static; no CI cost |
| `ripr` static analysis | ~2–5 LEM per diff; no mutation runtime |
| LEM budgeting | Forecast only; no CI cost |
| Risk-pack routing | Saves unused lane cost |
| Nextest profiles | Parallel test execution reduces wall time |
| Cache saves on `main` only | Reduces cache write churn |

## Budget Targets

| Band | LEM | Target |
|------|-----|--------|
| Pennies | 0–12 | Docs, metadata, config-only PRs |
| Default | 13–35 | Ordinary Rust PR (preferred) |
| Elevated | 36–75 | Parser/file-format/expanded surface |
| High | 76–125 | Explicit expensive PR |
| Over ceiling | >125 | Requires label override |

The preferred default is **below 35 LEM**, ideally **below $0.50**.
`$1/PR` is the ceiling, not the design center.

## Enforcement Phasing

| Phase | State | Condition |
|-------|-------|-----------|
| 1 — Visibility | Advisory only | No actuals yet |
| 2 — Soft warning | Warning on elevated+ | Actuals from ≥2 weeks |
| 3 — Hard ceiling | Fail on >125 without override | Learned estimates stable |
| 4 — Learned budgets | p50-based forecasts with 1.15× margin | ≥4 weeks of data |

Do not enter Phase 3 or 4 before their conditions are met.

## Verification Preservation Guarantee

The following deep lanes are **never removed** from the repository. They
are only rerouted from ordinary PR default to `main`/nightly/labels:

- Full 3-OS test matrix
- beta and MSRV toolchain testing
- Full feature matrix (`comp3_fast`, `audit`, combinations)
- Coverage (tarpaulin/llvm-cov)
- Mutation testing (`cargo-mutants`)
- Fuzz integration (libfuzzer/cargo-fuzz)
- Heavy proptest (high-case counts)
- BDD governance tests
- Exit code matrix
- Strict doc comments
- Determinism smoke
- RDW iterator tests
- Benchmark / enterprise perf
- Security audit (`cargo-deny`, `cargo-audit`)
- Docs / rustdoc
- WASM build and tests
- Arrow integration tests
- Nightly soak
- Publish dry-run
- Leak detection

These lanes remain available unconditionally via `full-ci` label.

## Glossary

| Term | Definition |
|------|------------|
| LEM | Linux Equivalent Minute; 1 minute of ubuntu-latest time at `$0.008/min` |
| Risk pack | Named set of crate paths and their associated CI lanes |
| Ordinary PR | A pull request without `full-ci`, `platform-matrix`, or cost-override labels |
| Lane | A single CI job or reusable workflow with a defined proof obligation |
| Default PR lane | A lane that runs on every ordinary pull request |
| Expensive lane | A lane with `base_lem > 20` or a runner multiplier > 1.0 |
| Deep lane | An expensive lane routed to `main`/nightly/labels rather than ordinary PRs |
