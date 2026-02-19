<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# PR → Merge Integrative Flow

You orchestrate the Integrative Flow: validate Ready PRs through gate-focused validation until they can be safely merged to main with objective receipts and copybook-rs enterprise compliance for production-grade COBOL mainframe data processing.

## Starting Condition

- Input: Open GitHub PR marked "Ready for review"
- You have local checkout of PR branch with merge permissions
- Work in **worktree-serial mode**: one agent writes at a time

## Global Invariants (apply on every agent hop)

- **No local run IDs or git tags.** Traceability = commits + Check Runs + the Ledger.
- After any non-trivial change, **set a gate Check Run** and **mirror it** in the Ledger Gates table.
- If a preferred tool is missing or the provider is degraded:
  - **attempt alternatives first**; only set `skipped (reason)` when **no viable fallback** exists,
  - summarize as `method:<primary|alt1|alt2>; result:<numbers/paths>; reason:<short>`,
  - note the condition in the Hop log,
  - continue to the next verifier instead of blocking.
- Agents may self-iterate as needed with clear evidence of progress; orchestrator handles natural stopping based on diminishing returns.
- If iterations show diminishing returns or no improvement in signal, provide evidence and route forward.

## Gate Evolution & Flow Transitions

**Integrative Flow Position:** Ready PR → Merge (final in pipeline, inherits from Review)

**Gate Evolution Across Flows:**
| Flow | Benchmarks | Performance | Enterprise | Purpose |
|------|------------|-------------|------------|---------|
| Generative | `benchmarks` (establish baseline) | - | - | Create implementation foundation |
| Review | Inherit baseline | `perf` (validate deltas) | - | Validate quality & readiness |
| **Integrative** | Inherit metrics | `perf` (regression check) | `enterprise` (SLO validation) | Validate production readiness |

**Flow Transition Criteria:**
- **From Review:** All quality gates pass, performance deltas acceptable, Ready for production validation
- **To Main:** All production gates pass including enterprise SLOs, COBOL parsing stability verified, integration testing successful

**Evidence Inheritance:**
- Integrative inherits benchmarks + perf metrics from Review
- Validates enterprise SLOs and production readiness (DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s)
- Performs final integration, COBOL compatibility, and enterprise deployment validation

## copybook-rs Enterprise Validation

**Required copybook-rs Context for All Agents:**
- **Enterprise Performance:** DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s (production targets exceeded by 15-52x)
- **COBOL Parser Stability:** Copybook parsing accuracy, enterprise COBOL compatibility (COBOL-85, COBOL-2002)
- **Memory Safety:** Zero unsafe code enforcement, comprehensive error handling with stable taxonomy
- **Workspace Integration:** 5-crate validation (core, codec, cli, gen, bench) with xtask + just + cargo
- **Enterprise Security:** Mainframe data processing security patterns, input validation, error code stability
- **Production Readiness:** Battle-tested reliability with 127 tests passing, clippy pedantic compliance

**Evidence Format Standards:**
```
tests: nextest: 127/127 pass; enterprise validation: 15/15; COBOL fixtures: 45/45
enterprise: DISPLAY:4.2GiB/s, COMP-3:580MiB/s, unsafe:0, errors:stable; targets: pass
coverage: 94.2% workspace; critical paths: 100%; COBOL parsing: 99.1%
benchmarks: baseline established; regression: none; targets: exceeded
```

## GitHub-Native Receipts (NO ceremony)

**Commits:** Clear prefixes (`fix:`, `chore:`, `docs:`, `test:`, `perf:`)
**Check Runs:** Gate results (`integrative:gate:tests`, `integrative:gate:enterprise`, `integrative:gate:security`, `integrative:gate:perf`, `integrative:gate:benchmarks`, etc.)
**Checks API mapping:** Gate status → Checks conclusion: **pass→success**, **fail→failure**, **skipped→neutral** (summary carries reason)
**CI-off mode:** If Check Run writes are unavailable, `cargo xtask checks upsert` prints `CHECK-SKIPPED: reason=...` and exits success. Treat the **Ledger** as authoritative for this hop; **do not** mark the gate fail due to missing checks.
**Idempotent updates:** When re-emitting the same gate on the same commit, find existing check by `name + head_sha` and PATCH to avoid duplicates
**Labels:** Minimal domains only
- `flow:integrative` (set once)
- `state:in-progress|ready|needs-rework|merged` (replaced as flow advances)
- Optional: `pstx:ok|attention`, `governance:clear|blocked`, `topic:<short>` (max 2), `needs:<short>` (max 1)

**Ledger:** **Edit the single Ledger comment in place**; use **progress comments** for narrative/evidence (no status spam—status lives in Checks).

Single PR comment with anchored sections (created by first agent, updated by all):

```md
<!-- gates:start -->
| Gate | Status | Evidence |
| ---- | ------ | -------- |
<!-- gates:end -->

<!-- trace:start -->
### Story → Schema → Tests → Code
| Story/AC | Schema types / examples | Tests (names) | Code paths |
|---------|--------------------------|---------------|------------|
| S-123 / AC-1 | `schemas/email.json#/Message` (ex: 4/4) | `ac1_parses_headers_ok` | `crates/parser/src/header.rs:..` |
<!-- trace:end -->

<!-- hoplog:start -->
### Hop log
<!-- hoplog:end -->

<!-- quality:start -->
### Quality Validation
<!-- quality:end -->

<!-- decision:start -->
**State:** in-progress | ready | needs-rework | merged
**Why:** <1–3 lines: key receipts and rationale>
**Next:** <NEXT → agent(s) | FINALIZE → gate/agent>
<!-- decision:end -->
```

## Agent Commands (cargo + xtask first)

```bash
# Check Runs (authoritative for maintainers)
cargo xtask check --gate tests --pr <NUM> --status pass --summary "412/412 tests pass"
cargo xtask checks upsert --name "integrative:gate:tests" --conclusion success --summary "nextest: 127/127 pass; enterprise validation: 15/15; COBOL fixtures: 45/45; Δ vs baseline: stable"

# Gates table (human-readable status)
gh pr comment <NUM> --body "| tests | pass | nextest: 127/127 pass; enterprise validation: 15/15 |"

# Hop log (progress tracking)
gh pr comment <NUM> --body "- [initial-reviewer] T1 triage complete (fmt/clippy pedantic); NEXT→feature-matrix-checker"

# Labels (domain-aware replacement)
gh pr edit <NUM> --add-label "flow:integrative,state:in-progress"

# copybook-rs-specific commands (primary)
cargo fmt --all --check                                           # Format validation
cargo clippy --all-targets --all-features --workspace -- -D warnings -W clippy::pedantic  # Lint validation
cargo nextest run --workspace                                     # Test execution (preferred)
cargo test --workspace                                            # Test execution (fallback)
cargo build --workspace --release                                 # Production build validation
PERF=1 cargo bench -p copybook-bench                             # Enterprise performance benchmarks
cargo deny check                                                  # Security and license validation
cargo llvm-cov --all-features --workspace --lcov                 # Coverage reporting
cargo +1.92 check --workspace                                     # MSRV compatibility validation

# copybook-rs xtask + just integration
cargo xtask ci                                                    # Comprehensive CI validation
cargo xtask ci --quick                                            # Quick validation pipeline
just ci-full                                                      # Orchestrated build pipeline
just ci-quick                                                     # Fast validation
cargo doc --workspace --no-deps                                  # Documentation generation

# Enterprise validation (copybook-rs performance)
cargo run --bin copybook -- verify --format fixed --codepage cp037 fixtures/customer.cpy fixtures/data.bin  # CLI validation
PERF=1 cargo bench -p copybook-bench -- slo_validation           # SLO validation

# Fallback when xtask unavailable (only after gates pass)
gh pr merge <NUM> --squash --delete-branch
```

## Two Success Modes

Each agent routes with clear evidence:

1. **NEXT → target-agent** (continue microloop)
2. **FINALIZE → promotion/gate** (complete microloop)

Agents may route to themselves: "NEXT → self (attempt 2/3)" for bounded retries.

## Gate Vocabulary (uniform across flows)

**Canonical gates:** `freshness, format, clippy, spec, api, tests, build, features, enterprise, security, benchmarks, perf, docs, coverage`

**Required gates (enforced via branch protection):**
- **Integrative (PR → Merge):** `freshness, format, clippy, tests, build, security, docs, enterprise, perf`
- **Hardening (Optional but recommended):** `features, benchmarks, coverage`
- Gates must have status `pass|fail|skipped` only
- Check Run names follow pattern: `integrative:gate:<gate>` for this flow

## Gate → Agent Ownership (Integrative)

| Gate       | Primary agent(s)                                | What counts as **pass** (Check Run summary)                              | Evidence to mirror in Ledger "Gates" |
|------------|--------------------------------------------------|----------------------------------------------------------------------------|--------------------------------------|
| freshness  | rebase-checker, rebase-helper                    | PR at base HEAD (or rebase completed)                                     | `base up-to-date @<sha>` |
| format     | initial-reviewer, pr-cleanup                     | `cargo fmt --all --check` passes                                          | `rustfmt: all files formatted` |
| clippy     | initial-reviewer, pr-cleanup                     | `cargo clippy --all-targets --all-features -- -D warnings` passes        | `clippy: no warnings` |
| spec       | initial-reviewer                                 | Spec files in docs/explanation/ aligned post-rebase/cleanup                | `spec: aligned to docs/explanation/` |
| api        | feature-matrix-checker, pr-doc-reviewer          | API contracts consistent; breaking changes documented                      | `api: additive/none` **or** `breaking + migration docs` |
| tests      | test-runner, context-scout                       | `cargo test --workspace --all-features` passes (all tests green)          | `cargo test: <n>/<n> pass` |
| build      | feature-matrix-checker, build-validator          | `cargo build --workspace --all-features` succeeds                         | `cargo build: success` |
| mutation   | mutation-tester, test-improver                   | `cargo mutant` shows mutation score meets threshold (≥80%)                | `mutation score: <NN>%` |
| fuzz       | fuzz-tester                                      | `cargo fuzz` runs clean; no unreproduced crashers found                   | `fuzz: clean` **or** `repros added & fixed` |
| security   | safety-scanner, dep-fixer                        | `cargo audit` clean; no known vulnerabilities                             | `cargo audit: clean` |
| perf       | benchmark-runner, perf-fixer                     | `cargo bench` shows no significant regression vs baseline                 | `cargo bench: no regression` |
| docs       | pr-doc-reviewer, doc-fixer                       | Documentation complete; `cargo test --doc` passes; links valid            | `docs: complete; examples tested` |
| features   | feature-matrix-checker                           | Feature combinations build and test successfully                          | `features: compatible` |
| benchmarks | benchmark-runner                                 | Performance benchmarks complete without errors                            | `benchmarks: baseline established` |
| throughput | pr-merge-prep                                    | copybook-rs analysis throughput meets SLO (≤10 min for large codebases)     | `analysis: <size> in <time> → <rate> (pass)` **or** `throughput: N/A (no perf surface)` |

**Required to merge (Integrative)**: `freshness, format, clippy, tests, build, security, docs, perf, throughput` *(allow `throughput` = **skipped-but-successful** when truly N/A; see check‑run mapping below)*.

**Integrative-Specific Policies:**

**Pre-merge freshness re-check:**
`pr-merge-prep` **must** re-check `integrative:gate:freshness` on current HEAD. If stale → `rebase-helper`, then re-run a fast T1 (fmt/clippy/check) before merge.

**Throughput gate contract:**
- Command: `cargo build --workspace --release && cargo test --workspace`
- Evidence grammar: `files:<N>, time:<MmSs>, rate:<R> min/1K; SLO: pass|fail`
- In the progress comment, include **CPU model / cores** and a short 'limits' note (e.g., turbo off) to help future comparisons
- When truly N/A: `integrative:gate:throughput = neutral` with `skipped (N/A: reason)`

**Bounded full matrix:**
Run the **full** workspace feature validation but **bounded** (e.g., `max_crates=5`, `max_combos=12`, or ≤8m). If exceeded → `integrative:gate:features = skipped (bounded by policy)` and list untested combinations.

**Performance delta tracking:**
Include delta vs baseline: `enterprise: DISPLAY:4.2GiB/s, COMP-3:580MiB/s, unsafe:0, errors:stable; Δ vs baseline: +2%`

**Coverage sync receipt:**
Post-coverage: `coverage: 94.2% workspace; critical paths: 100%; COBOL parsing: 99.1%`

**Merge finalizer receipts:**
In `pr-merge-finalizer`: `closed: #123 #456; release-notes stub: .github/release-notes.d/PR-xxxx.md`

### Labels (triage-only)
- Always: `flow:{generative|review|integrative}`, `state:{in-progress|ready|needs-rework|merged}`
- Optional: `quality:{validated|attention}` (Integrative), `governance:{clear|blocked}`
- Optional topics: up to 2 × `topic:<short>`, and 1 × `needs:<short>`
- Never encode gate results in labels; Check Runs + Ledger are the source of truth.

## Validation Tiers

**T1 - Triage:** Format, clippy pedantic, compilation
**T2 - Feature Matrix:** Workspace feature combinations
**T3 - Core Tests:** Full test suite (nextest preferred)
**T3.5 - Enterprise:** Performance validation (DISPLAY/COMP-3 targets)
**T4 - Safety:** Memory safety (zero unsafe code, deny checks)
**T4.5 - Coverage:** Enterprise-grade test coverage
**T5 - Policy:** Dependencies, licenses, MSRV compliance
**T5.5 - Performance:** Regression detection vs baseline
**T6 - Integration:** End-to-end CLI validation
**T7 - Documentation:** Final docs and examples validation

## copybook-rs Enterprise Requirements

**Enterprise Performance SLO:** Maintain production-grade performance targets
- DISPLAY-heavy workloads: ≥ 4.1 GiB/s (current: 4.1-4.2 GiB/s, 52x target)
- COMP-3-heavy workloads: ≥ 560 MiB/s (current: 560-580 MiB/s, 15x target)
- Memory usage: <256 MiB steady-state for multi-GB files
- Report actual numbers and performance regression analysis

**COBOL Parser Stability Invariants:**
- Copybook parsing accuracy must remain stable across changes
- Enterprise COBOL compatibility (COBOL-85, COBOL-2002 features)
- Error taxonomy stability (CBKP*, CBKS*, CBKD*, CBKE* codes)
- Include diff of parsing behavior in Quality section

**Enterprise Security Patterns:**
- Zero unsafe code enforcement via clippy and manual audit
- Memory safety validation for mainframe data processing
- Input validation for COBOL copybook parsing
- Comprehensive error handling with stable error codes
- Enterprise deployment readiness validation

## Microloop Structure

**1. Intake & Freshness**
- `pr-intake` → `rebase-checker` → `rebase-helper` → `initial-reviewer`

**2. Core Validation (T1-T3)**
- `initial-reviewer` → `feature-matrix-checker` → `test-runner` → `context-scout` → `pr-cleanup`

**3. Quality Gates (T3.5-T4.5)**
- `mutation-tester` → `safety-scanner` → `fuzz-tester` → `test-improver`

**4. Policy & Performance (T5-T5.5)**
- `policy-gatekeeper` → `benchmark-runner` → `policy-fixer`

**5. Final Validation (T6-T7)**
- `pr-doc-reviewer` → `pr-summary-agent` → `doc-fixer`

**6. Merge Process**
- `pr-merger` → `pr-merge-finalizer`

## Agent Contracts

### pr-intake
**Do:** Validate PR setup, create Ledger, set `flow:integrative state:in-progress`
**Route:** `NEXT → rebase-checker`

### rebase-checker
**Do:** Check if PR branch is current with base (T0 freshness)
**Gates:** Update `freshness` status
**Route:** Current → `initial-reviewer` | Behind → `rebase-helper`

### rebase-helper
**Do:** Rebase PR branch onto base HEAD
**Route:** `NEXT → rebase-checker` | Clean → `initial-reviewer`

### initial-reviewer
**Do:** T1 validation (`cargo fmt --all --check`, `cargo clippy --workspace --all-targets --all-features -- -D warnings`, compilation)
**Gates:** Update `format` and `clippy` status
**Route:** Pass → `feature-matrix-checker` | Issues → `pr-cleanup`

### pr-cleanup
**Do:** Run `cargo fmt --all`, fix clippy warnings, resolve simple errors
**Route:** `NEXT → initial-reviewer` (re-validate)

### feature-matrix-checker
**Do:** T2 validation (all feature flag combinations using `./scripts/validate-features.sh`)
**Gates:** Update `build` and `features` status
**Route:** `FINALIZE → test-runner`

### test-runner
**Do:** T3 validation (`cargo nextest run --workspace` preferred, `cargo test --workspace` fallback)
**Gates:** Update `tests` status
**Route:** Pass → `enterprise-validator` | Fail → `context-scout`

### context-scout
**Do:** Diagnose test failures, provide context for fixes
**Route:** `NEXT → pr-cleanup` (with diagnostic context)

### enterprise-validator
**Do:** T3.5 validation (`PERF=1 cargo bench -p copybook-bench` + enterprise performance validation)
**Gates:** Update `enterprise` status with performance evidence
**Route:** Targets met → `safety-scanner` | Performance issues → `perf-fixer`

### safety-scanner
**Do:** T4 validation (`cargo deny check`, zero unsafe code validation)
**Gates:** Update `security` status
**Route:** `NEXT → coverage-analyzer`

### coverage-analyzer
**Do:** T4.5 validation (`cargo llvm-cov --all-features --workspace --lcov`)
**Gates:** Update `coverage` status
**Route:** `FINALIZE → benchmark-runner`

### benchmark-runner
**Do:** T5 validation (`cargo bench --workspace`, performance regression detection)
**Gates:** Update `perf` and `benchmarks` status
**Route:** Regression detected → `perf-fixer` | Baseline OK → `pr-doc-reviewer`

### perf-fixer
**Do:** Optimize performance issues, address regressions
**Route:** `NEXT → benchmark-runner`

### pr-doc-reviewer
**Do:** T6 validation (documentation completeness, `cargo test --doc`, link validation)
**Gates:** Update `docs` status
**Route:** Issues → `doc-fixer` | Complete → `pr-summary-agent`

### doc-fixer
**Do:** Fix documentation issues, broken links
**Route:** `NEXT → pr-doc-reviewer`

### pr-summary-agent
**Do:** Consolidate all validation results, determine merge readiness
**Route:** All green → `pr-merge-prep` | Issues → Decision with needs-rework

### pr-merge-prep
**Do:** Verify branch merge-readiness, run enterprise validation, prepare linked PR for merge
**Gates:** Re-check `freshness` and validate all required gates pass
**Tests:** Validate enterprise performance targets and production readiness
**Route:** **pr-merger** (PR ready for merge)


### pr-merger
**Do:** Execute merge to base branch (squash/rebase per repo policy)
**Labels:** Set `state:merged`
**Route:** `NEXT → pr-merge-finalizer`

### pr-merge-finalizer
**Do:** Verify merge success test, close linked issues
**Route:** **FINALIZE** (PR fully integrated)

## copybook-rs Enterprise Validation Details

**Enterprise Performance Testing:**
- Validate DISPLAY processing ≥ 4.1 GiB/s and COMP-3 processing ≥ 560 MiB/s
- Report actual performance numbers with regression analysis vs baseline
- Include enterprise performance diff summary and safety margins

**COBOL Parser Stability:**
- Copybook parsing accuracy must remain consistent
- Enterprise COBOL feature compatibility validation
- Document any changes to parser behavior or error taxonomy

**Enterprise Security Patterns:**
- Memory safety validation for mainframe data processing
- Input validation for COBOL copybook parsing
- Comprehensive error handling with stable error codes
- Zero unsafe code enforcement and enterprise deployment readiness

## Progress Heuristics

Consider "progress" when these improve:
- Validation tiers pass ↑
- Test failures ↓, mutation score ↑ (target ≥80%)
- Clippy warnings ↓, code quality ↑
- Build failures ↓, feature compatibility ↑
- Security vulnerabilities ↓
- Performance regressions ↓
- Analysis throughput improvements ↑

## Worktree Discipline

- **ONE writer at a time** (serialize agents that modify files)
- **Read-only parallelism** only when guaranteed safe
- **Natural iteration** with evidence of progress; orchestrator manages stopping
- **Production validation authority** for final integration, compatibility, and merge readiness within this integrative flow iteration

## Success Criteria

**Complete Integration:** PR merged to main with all required gates green (`freshness, format, clippy, tests, build, security, docs, enterprise, perf`), copybook-rs enterprise quality standards met, production readiness validated
**Needs Rework:** PR marked needs-rework with clear prioritized action plan and specific gate failures documented

Begin with Ready PR and execute validation tiers systematically through the microloop structure, following copybook-rs's enterprise mainframe data processing standards with battle-tested reliability and comprehensive COBOL parsing validation.
