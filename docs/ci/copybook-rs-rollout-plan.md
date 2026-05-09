<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-rs CI Economics + Strict Rust Policy Rollout Plan

This document is the anchor for the full rollout. Every later PR in the stack
references it to stay on track. No workflow behavior changes are made here.

## Assumptions

- The rollout preserves or increases verification strength; it does not reduce it.
- MSRV 1.95 is the target platform baseline; the bump is a separate PR (01).
- Ordinary PRs should usually stay below 35 LEM and preferably below $0.50.
- Existing deep lanes remain available on `main`, nightly, release, or explicit labels.
- The current large OS/toolchain/feature matrix should not stay an ordinary PR default.
- The first phase is visibility and policy; hard budget enforcement comes only after
  actuals exist.
- `ripr` starts advisory and does not use runtime mutation terms like `killed` or
  `survived`.
- Generated baseline debt is temporary and expiring; each entry must carry an expiry
  date.

## Repo-Specific Framing

`copybook-rs` is a parser/file-format repository. It needs strong verification for:

- COBOL copybook lexing and parsing (copybook-lexer, copybook-core)
- EBCDIC/codepage conversion (copybook-codepage, copybook-charset)
- COMP-3, overpunch, and zoned decimal encoding (copybook-overpunch,
  copybook-zoned-format, copybook-overflow)
- RDW records and fixed-record formats (copybook-rdw, copybook-fixed,
  copybook-record-io)
- Deterministic round-trip conversion (copybook-determinism,
  copybook-cli-determinism)
- CLI behavior and exit codes (copybook-cli)
- Governance and BDD contracts (copybook-governance-*, tests/bdd)
- Golden fixture proof (SHA-256 verified corpus)
- Arrow and WASM bindings (copybook-arrow)
- Release and package metadata

The goal is **not** lighter CI. The goal is **scoped CI**: ordinary PRs buy the
cheapest deterministic proof relevant to the changed risk surface, while
`main`/nightly/release/labels preserve broad validation.

## Why CI Economics Matters Here

OpenClaw's published Blacksmith runner spend of roughly `$511k`, mapped against
commit volume since February, works out directionally to about `$20 per commit` on
Blacksmith alone. Because OpenClaw appears to squash-merge PRs, commit count is a
reasonable proxy for merged PR count, though the number is directional.

We do not read that as "OpenClaw tests too much." We read it as evidence that
verification demand is rising faster than verification efficiency. Agentic
development requires **more** verification than conventional PR workflows, not less.

`copybook-rs` targets a different verification economics model:

- Rust keeps compile-time and crate-local checks fast and cheap.
- Clippy prevents known-bad local code shapes before they reach CI.
- TOML policy ledgers make exceptions reviewable and expiring.
- `ripr` gives mutation-testing-lite value at static-analysis prices.
- LEM budgeting makes CI cost visible before a PR spends it.
- Risk-pack routing spends expensive lanes only where they buy signal.

The default ordinary PR target is **below 35 LEM** and preferably below `$0.50`.
`$1/PR` is a ceiling, not the design center.

## Hard Rules for Every PR in This Stack

| Rule | Detail |
|------|--------|
| No verification weakening | Deep validation lanes are preserved; only routing changes |
| No bare `#[allow(...)]` | Use `#[expect(..., reason = "...")]` where suppression is needed |
| No Clippy test carveouts | Tests receive the same lint profile as production code |
| `ripr` advisory only initially | Never blocking before calibration data exists |
| No hard budget enforcement | Wait for actuals before gating on learned thresholds |
| macOS/Windows not default PR | Route by label (`platform-matrix`) or main |
| No global `-D warnings` for staged lints | Only promote lints when warn-stage debt is intentionally clear |
| Expiry on all debt entries | Every exception, allowlist entry, and carveout carries an expiry |

## Operating Doctrine: Review Loop

For every PR in this stack:

1. Open as draft initially.
2. Include in the PR body: purpose, default PR LEM impact, workflows touched,
   branch protection impact, failure mode caught, cheaper signal considered,
   rollback path, commands run.
3. Read all bot/reviewer comments.
4. Fix actionable comments. Treat quota/rate-limit comments as non-actionable noise.
5. Treat stale comments against old commits as stale after verifying current HEAD.
6. Re-run relevant checks.
7. Mark ready only after self-review.
8. Merge when required checks are green and actionable feedback is resolved.
9. Rebase dependent PRs after each merge.

## Current Repo State (as of 2026-05-09)

| Property | Value |
|----------|-------|
| MSRV | 1.92 (target: 1.95) |
| Edition | 2024 |
| Publishable crates | 36 under `crates/` |
| Dev-only tools | 3 under `tools/` |
| Test suites | 3 under `tests/` |
| CI workflows | 34 `.github/workflows/*.yml` files |
| Test matrix | 3 OS × 3 Rust toolchains × 3 feature sets = 27 test jobs |
| Panic lints | `warn` in workspace (not deny; tests relaxed via `-A` flags) |
| Cast lints | Mostly `allow` in workspace |
| `unsafe_code` | `forbid` (invariant) |
| `xtask` commands | docs sync/verify, perf, pr-insights only |
| `docs/ci/` | New (created by PR 00) |
| `policy/` | New (created by PR 00) |

## Recommended PR Merge Order

```
00  docs / rollout path (this PR)
01  reconcile or implement strict Clippy MSRV 1.95 policy
02  reconcile or implement xtask audit/helper migration
03  CI lane whitelist + budget policy
04  CI lane whitelist checker
05  non-Rust TOML allowlist
06  semantic no-panic allowlist
07  PR Plan / LEM forecast
08  cache + cancellation cleanup
09  PR Gate Success summary
10  slim ordinary PR gate / route matrix
11  route coverage/mutation/fuzz/proptest/perf
12  route docs/examples/security/determinism/BDD
13  ripr advisory
14  nextest/JUnit telemetry
15  ci-actuals
16  soft budget warnings
17  branch protection migration
18  learned estimates
19  ripr soft-gate after calibration
```

Natural stacking:

```
00 → 03 → 04 → 07 → 09
01 → 05 → 06
07 → 15 → 16 → 18
09 → 17
13 → 19
```

Independent PRs (no stack dependency): 02, 08, 10, 11, 12, 14

## Final Target Default PR Path

After the rollout, an ordinary `copybook-rs` PR CI should look like:

```
PR Plan (LEM forecast, risk pack identification)
CI Policy (lane whitelist, file policy)
Rust Fast Gate
  - fmt
  - check
  - strict Clippy policy
  - no-panic policy
  - selected crate tests
Risk-pack tests (only when the touched surface requires them)
  - parser / codec / RDW / CLI / governance
ripr advisory
PR Gate Success
```

Deep validation stays available and is preserved on `main` and by labels:

```
macOS / Windows / beta matrix   → platform-matrix / full-ci
Full feature matrix             → full-ci
Examples                        → main / full-ci
Coverage                        → main / coverage / full-ci
Mutation testing                → nightly / mutation / full-ci
Fuzz integration                → nightly / fuzz / full-ci
Heavy proptest                  → property-tests / main
Benchmarks / enterprise perf    → perf / main / manual
Security audit                  → security-audit / main / full-ci
Docs / rustdoc                  → main / full-ci
WASM / Arrow                    → wasm / arrow / full-ci
Nightly soak                    → nightly schedule
Release checks                  → release-check / publish trigger
```

## Immediate Highest-Value Changes

In priority order:

1. **Document the path first** (this PR) — prevents the rollout from becoming
   an uncoordinated bundle of CI edits.
2. **Consolidate duplicate Clippy/policy PRs** — one governed MSRV 1.95 baseline,
   not several overlapping attempts.
3. **Normalize cache and cancellation** — current workflows save cache on PRs and
   use `cancel-in-progress: true` broadly, which can cancel label-triggered runs.
4. **Move the 27-job matrix off ordinary PRs** — keep Ubuntu/stable/default as the
   fast lane; move macOS/Windows/beta/full-feature breadth to `main`/labels.
5. **Route coverage/mutation/fuzz/perf** — valuable but not ordinary PR defaults.
6. **Add semantic no-panic and non-Rust TOML allowlists** — maintenance hatches
   with exact location, owner, reason, and expiry.
7. **Add `ripr` advisory** — COBOL parser/codec changes are exactly where cheap
   oracle-gap signal pays.

## Scope Guard

Phase 0 cleanup: inspect the current open PR queue before new implementation work.

| Topic | Instruction |
|-------|-------------|
| Governed Clippy/MSRV/policy PRs | Pick the most complete one, improve it, merge it, close duplicates as superseded |
| `xtask` audit/helper migration PRs | Pick the cleanest implementation, merge it, close duplicates |
| Coverage cleanup PRs | Merge in safe order if green; move to explicit/advisory/label-main |
| Dependabot/test/docs/product PRs | Keep separate from CI economics unless they block the rollout |

Do not blend unrelated docs/test/dependency PRs into the CI economics stack.

## Reference

| Document | Purpose |
|----------|---------|
| `docs/ci/cost-and-verification-policy.md` | Policy intent and economics model |
| `docs/ci/lem-budgeting.md` | LEM unit definition and band table |
| `docs/ci/labels.md` | Label definitions and semantics |
| `docs/ci/verification-ladder.md` | Verification tiers from fast to deep |
| `docs/ci/current-state.md` | Snapshot of CI before rollout changes |
| `docs/CLIPPY_POLICY.md` | Clippy lint policy and planned flips |
| `docs/NO_PANIC_POLICY.md` | No-panic policy and allowlist schema |
| `docs/FILE_POLICY.md` | Non-Rust file allowlist and classification |
