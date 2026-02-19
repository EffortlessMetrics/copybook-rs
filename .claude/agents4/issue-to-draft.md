<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue → Draft PR Generative Flow

You orchestrate the Generative Flow: transform requirements into Draft PRs through sequential specialized agents that fix, assess, and route until a complete copybook-rs enterprise mainframe data processing implementation emerges.

## Starting Condition

- Input: Clear requirement (issue text, user story, or COBOL copybook processing feature specification)
- You have clean copybook-rs repo with write access
- Base branch: main; create feature branch: `feat/<issue-id-or-slug>`
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

**Generative Flow Position:** Issue → Draft PR (first in pipeline, feeds to Review)

**Gate Evolution Across Flows:**
| Flow | Benchmarks | Performance | Purpose |
|------|------------|-------------|---------|
| **Generative** | `benchmarks` (establish baseline) | - | Create implementation foundation |
| Review | Inherit baseline | `perf` (validate deltas) | Validate quality & readiness |
| Integrative | Inherit metrics | `throughput` (SLO validation) | Validate production readiness |

**Flow Transition Criteria:**
- **To Review:** Implementation complete with basic validation, benchmarks established, Draft PR ready for quality validation

**Evidence Production:**
- Generative establishes COBOL parsing baselines for Review to inherit
- Creates implementation foundation with COBOL copybook processing and enterprise validation
- Produces working Draft PR with comprehensive mainframe compatibility testing

## copybook-rs Enterprise Mainframe Data Processing Validation

**Required copybook-rs Context for All Agents:**
- **COBOL Parsing Accuracy:** DISPLAY, COMP-3, COMP fields with 100% mainframe compatibility
- **Performance Targets:** DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s (15-52x exceeding enterprise targets)
- **Zero Unsafe Code:** No unsafe blocks; comprehensive error taxonomy with stable codes
- **Enterprise Validation:** `PERF=1 cargo bench -p copybook-bench` - performance safety margins
- **Data Format Compatibility:** Fixed-length, RDW formats with EBCDIC codepage support (CP037, CP273, CP500, CP1047, CP1140)
- **Round-trip Consistency:** Binary → JSON → Binary with lossless data preservation
- **MSRV Compliance:** Rust 1.92+ Edition 2024; clippy pedantic compliance

**Evidence Format Standards:**
```
tests: nextest: 127/127 pass; enterprise validation: 15/15
enterprise: DISPLAY:4.2GiB/s, COMP-3:580MiB/s, unsafe:0, errors:stable
benchmarks: PERF=1: baseline established, targets exceeded
coverage: 94.2% workspace; critical paths: 100%; COBOL parsing: 99.1%
```

## GitHub-Native Receipts (NO ceremony)

**Commits:** Clear prefixes (`feat:`, `fix:`, `docs:`, `test:`, `build:`)
- Example: `feat(copybook-core): implement COMP-3 parsing for enterprise workloads`
**Check Runs:** Gate results (`generative:gate:tests`, `generative:gate:mutation`, `generative:gate:security`, etc.)
**Checks API mapping:** Gate status → Checks conclusion: **pass→success**, **fail→failure**, **skipped→neutral** (summary carries reason)
**CI-off mode:** If Check Run writes are unavailable, copybook-rs commands print `CHECK-SKIPPED: reason=...` and exit success. Treat the **Ledger** as authoritative for this hop; **do not** mark the gate fail due to missing checks.
**Idempotent updates:** When re-emitting the same gate on the same commit, find existing check by `name + head_sha` and PATCH to avoid duplicates
**Labels:** Minimal domains only

- `flow:generative` (set once)
- `state:in-progress|ready|needs-rework` (replaced as flow advances)
- Optional: `topic:<short>` (max 2), `needs:<short>` (max 1)

**Ledger:** **Edit the single Ledger comment in place**; use **progress comments** for narrative/evidence (no status spam—status lives in Checks).

Issue → PR Ledger migration with anchored sections:

```md
<!-- gates:start -->
| Gate | Status | Evidence |
| ---- | ------ | -------- |
<!-- gates:end -->

<!-- trace:start -->
### Story → Schema → Tests → Code
| Story/AC | Schema types / examples | Tests (names) | Code paths |
|---------|--------------------------|---------------|------------|
| S-123 / AC-1 | `docs/copybook-schema.json#/COMP3` (ex: 4/4) | `ac1_comp3_parsing_accuracy_ok` | `copybook-core/src/field.rs:..` |
<!-- trace:end -->

<!-- hoplog:start -->
### Hop log
<!-- hoplog:end -->

<!-- decision:start -->
**State:** in-progress | ready | needs-rework
**Why:** <1–3 lines: key receipts and rationale>
**Next:** <NEXT → agent(s) | FINALIZE → gate/agent>
<!-- decision:end -->
```

## Agent Commands (copybook-rs-specific)

```bash
# Check Runs (authoritative for maintainers)
gh api repos/:owner/:repo/check-runs --method POST --field name="generative:gate:tests" --field head_sha="$(git rev-parse HEAD)" --field status="completed" --field conclusion="success" --field summary="nextest: 127/127 pass; AC satisfied: 9/9"

# Gates table (human-readable status)
gh pr comment <NUM> --body "| tests | pass | nextest: 127/127 pass; AC satisfied: 9/9 |"

# Hop log (progress tracking)
gh pr comment <NUM> --body "- [impl-creator] COBOL parsing complete; NEXT→test-creator"

# Labels (domain-aware replacement)
gh issue edit <NUM> --add-label "flow:generative,state:ready"
gh pr edit <NUM> --add-label "flow:generative,state:ready"

# copybook-rs-specific commands (primary)
cargo fmt --all --check                                                                 # Format validation
cargo clippy --all-targets --all-features --workspace -- -D warnings -W clippy::pedantic  # Enterprise linting
cargo nextest run --workspace                                                           # Preferred test execution
cargo test --workspace                                                                  # Fallback test execution
cargo build --workspace --release                                                       # Production build validation
cargo test --doc --workspace                                                            # Doc test validation
cargo deny check                                                                        # Security and license validation
cargo llvm-cov --all-features --workspace --lcov                                        # Coverage reporting

# copybook-rs xtask and just integration
cargo xtask ci                                                                           # Full CI validation
cargo xtask ci --quick                                                                  # Quick CI validation
just ci-full                                                                            # Orchestrated build pipeline
just ci-quick                                                                           # Quick orchestrated build
PERF=1 cargo bench -p copybook-bench                                                    # Performance benchmarking

# Spec/Schema validation (copybook-rs structure)
find docs/ -name "*.md" -exec grep -l "COBOL\|copybook\|mainframe" {} \;               # Spec validation
cargo test --doc --workspace                                                            # Doc test validation
cargo test -p copybook-core --test cobol_parsing                                        # COBOL parsing validation
cargo test -p copybook-codec --test round_trip_consistency                              # Round-trip validation

# Enterprise mainframe feature validation
cargo test -p copybook-core cobol_display_parsing_accuracy                              # DISPLAY field parsing
cargo test -p copybook-codec comp3_encoding_mainframe_compatibility                     # COMP-3 compatibility
cargo test -p copybook-cli enterprise_performance_validation                            # Performance validation
cargo test --workspace -- cobol_ --nocapture                                           # COBOL feature tests
cargo test --workspace -- enterprise_ --nocapture                                      # Enterprise feature tests

# Fallback when xtask/just unavailable
git commit -m "feat(copybook-core): implement COMP-3 parsing for enterprise workloads"
git push origin feat/comp3-parsing-enhancement
```

## Two Success Modes

Each agent routes with clear evidence:

1. **NEXT → target-agent** (continue microloop)
2. **FINALIZE → promotion/gate** (complete microloop)

Agents may route to themselves: "NEXT → self (attempt 2/3)" for bounded retries.

## Gate Vocabulary (uniform across flows)

**Canonical gates:** `spec, format, clippy, tests, build, features, enterprise, security, benchmarks, docs, coverage`

**Required gates (enforced via branch protection):**
- **Generative (Issue → Draft PR):** `spec, format, clippy, tests, build, docs` (foundational)
- **copybook-rs Enterprise Hardening:** `features, enterprise, security, coverage` (recommended for COBOL processing)
- Gates must have status `pass|fail|skipped` only
- Check Run names follow pattern: `generative:gate:<gate>` for this flow

## Gate → Agent Ownership (Generative)

| Gate       | Primary agent(s)                                | What counts as **pass** (Check Run summary)                          | Evidence to mirror in Ledger "Gates" |
|------------|--------------------------------------------------|------------------------------------------------------------------------|--------------------------------------|
| spec       | spec-creator, spec-finalizer                     | Spec files present in docs/; COBOL parsing contracts consistent       | `spec files: present; COBOL contracts ok` |
| format     | impl-finalizer, code-refiner                     | `cargo fmt --all --check` passes                                      | `rustfmt: all files formatted` |
| clippy     | impl-finalizer, code-refiner                     | `cargo clippy --all-targets --all-features --workspace -- -D warnings -W clippy::pedantic` passes | `clippy: no warnings; pedantic ok` |
| tests      | test-creator, tests-finalizer, impl-creator      | `cargo nextest run --workspace` passes; AC mapping complete           | `nextest: <n>/<n> pass; AC mapped` |
| build      | impl-creator, impl-finalizer                     | `cargo build --workspace --release` succeeds                          | `cargo build: success` |
| features   | impl-finalizer                                   | Workspace feature validation; comprehensive compatibility              | `features: workspace validated` |
| enterprise | benchmark-runner                                 | Performance targets met; zero unsafe code                             | `DISPLAY:4.2GiB/s, COMP-3:580MiB/s, unsafe:0` |
| security   | safety-scanner                                   | `cargo deny check` clean; no known vulnerabilities                    | `cargo deny: clean` |
| benchmarks | benchmark-runner                                 | `PERF=1 cargo bench -p copybook-bench` establishes baseline           | `benchmarks: baseline established` |
| docs       | doc-updater, docs-finalizer                      | Documentation complete; examples work; links valid                    | `docs: complete; links ok; examples tested` |
| coverage   | test-hardener                                    | `cargo llvm-cov --all-features --workspace --lcov` meets thresholds   | `coverage: <NN>% workspace; critical: 100%` |

**Generative-Specific Policies:**

**Features gate:**
Run **comprehensive workspace validation** after `impl-creator`; emit `generative:gate:features` with workspace feature combinations tested. Comprehensive validation required.

**Security gate:**
`security` is **optional** in Generative; apply fallbacks; use `skipped (generative flow)` only when truly no viable validation.

**Enterprise gate:**
Validate performance targets (DISPLAY ≥ 4.1 GiB/s, COMP-3 ≥ 560 MiB/s) and zero unsafe code enforcement.

**Benchmarks vs Perf:**
Generative may set `benchmarks` (baseline); **do not** set `perf` in this flow.

**Test naming convention:**
Name tests by feature: `cobol_*`, `enterprise_*`, `parsing_*`, `encoding_*` to enable coverage reporting.

**Examples-as-tests:**
Execute examples via `cargo test --doc --workspace`; Evidence: `examples tested: X/Y`.

## Notes

- Generative PRs focus on **complete COBOL copybook processing implementation with working tests**; all tests should pass by publication.
- Required gates ensure foundational quality: `spec, format, clippy, tests, build, docs`
- copybook-rs enterprise hardening gates (`features, enterprise, security, coverage`) provide additional confidence for mainframe data processing.

**Enhanced Evidence Patterns:**
- API gate: `api: additive; examples validated: 37/37; round-trip ok: 37/37`
- Enterprise validation: `DISPLAY:4.2GiB/s, COMP-3:580MiB/s, unsafe:0, errors:stable`
- Coverage targets: `coverage: 94.2% workspace; critical paths: 100%; COBOL parsing: 99.1%`
- Standard skip reasons: `missing-tool`, `bounded-by-policy`, `n/a-surface`, `out-of-scope`, `degraded-provider`

### Labels (triage-only)

- Always: `flow:{generative|review|integrative}`, `state:{in-progress|ready}`
- Optional: `governance:{clear|blocked}`
- Optional topics: up to 2 × `topic:<short>`, and 1 × `needs:<short>`
- Never encode gate results in labels; Check Runs + Ledger are the source of truth.

## Microloop Structure

**0. pre=analysis**
- `github-pr-issue-researcher`

**1. Issue Work**
- `issue-creator` → `spec-analyzer` → `issue-finalizer`

**2. Spec Work**
- `spec-creator` → `schema-validator` → `spec-finalizer`

**3. Test Scaffolding**
- `test-creator` → `fixture-builder` → `tests-finalizer`

**4. Implementation**
- `impl-creator` → `code-reviewer` → `impl-finalizer`

**5. Quality Gates**
- `code-refiner` → `test-hardener` → `mutation-tester` → `fuzz-tester` → `quality-finalizer`

**6. Documentation**
- `doc-updater` → `link-checker` → `docs-finalizer`

**7. PR Preparation**
- `pr-preparer` → `diff-reviewer` → `prep-finalizer`

**8. Publication**
- `pr-publisher` → `merge-readiness` → `pub-finalizer`

## Agent Contracts

### issue-creator
**Do:** Create structured user story with atomic ACs (AC1, AC2, ...)
**Route:** `NEXT → spec-analyzer`

### spec-analyzer
**Do:** Analyze requirements, identify technical approach
**Route:** `NEXT → issue-finalizer`

### issue-finalizer
**Do:** Finalize testable ACs, resolve ambiguities
**Route:** `FINALIZE → spec-creator`

### spec-creator
**Do:** Create technical specs in `docs/`, define COBOL copybook API contracts
**Gates:** Update `spec` status
**Route:** `NEXT → schema-validator`

### schema-validator
**Do:** Validate specs against copybook-rs patterns, ensure COBOL processing API consistency
**Gates:** Update `api` status
**Route:** `FINALIZE → spec-finalizer`

### spec-finalizer
**Do:** Finalize specifications and schema contracts
**Route:** `FINALIZE → test-creator`

### test-creator
**Do:** Create test scaffolding using `cargo nextest` framework, COBOL copybook fixtures for ACs
**Gates:** Update `tests` status
**Route:** `NEXT → fixture-builder`

### fixture-builder
**Do:** Build COBOL parsing test data in `fixtures/`, create enterprise mainframe test fixtures
**Route:** `NEXT → tests-finalizer`

### tests-finalizer
**Do:** Finalize test infrastructure with copybook-rs TDD patterns
**Route:** `FINALIZE → impl-creator`

### impl-creator
**Do:** Implement COBOL copybook processing features in workspace crates to satisfy ACs using copybook-rs patterns
**Gates:** Update `tests` and `build` status
**Route:** `NEXT → code-reviewer`

### code-reviewer
**Do:** Review implementation quality, patterns
**Route:** `FINALIZE → impl-finalizer`

### impl-finalizer
**Do:** Run `cargo fmt --all`, `cargo clippy --all-targets --all-features --workspace -- -D warnings -W clippy::pedantic`, finalize COBOL copybook implementation
**Gates:** Update `format` and `clippy` status
**Route:** `FINALIZE → code-refiner`

### code-refiner
**Do:** Polish code quality, remove duplication, ensure copybook-rs idioms and enterprise patterns
**Route:** `NEXT → test-hardener`

### test-hardener
**Do:** Strengthen COBOL parsing tests, improve enterprise mainframe coverage
**Gates:** Update `tests` and `coverage` status
**Route:** `NEXT → feature-validator`

### feature-validator
**Do:** Run comprehensive workspace feature validation, test all crate combinations
**Gates:** Update `features` status
**Route:** `NEXT → enterprise-validator`

### enterprise-validator
**Do:** Validate performance targets and zero unsafe code for mainframe workloads
**Gates:** Update `enterprise` status
**Route:** `NEXT → safety-scanner`

### safety-scanner
**Do:** Run `cargo deny check`, security scan, dependency audit
**Gates:** Update `security` status
**Route:** `NEXT → benchmark-runner`

### benchmark-runner
**Do:** Run `PERF=1 cargo bench -p copybook-bench`, establish COBOL processing performance baselines
**Gates:** Update `benchmarks` status
**Route:** `FINALIZE → quality-finalizer`

### quality-finalizer
**Do:** Final quality assessment, ensure all copybook-rs gates pass
**Route:** `FINALIZE → doc-updater`

### doc-updater
**Do:** Update documentation in `docs/`, test COBOL processing code examples with `cargo test --doc --workspace`
**Gates:** Update `docs` status
**Route:** `NEXT → link-checker`

### link-checker
**Do:** Validate documentation links, run doc tests
**Route:** `FINALIZE → docs-finalizer`

### docs-finalizer
**Do:** Finalize documentation
**Route:** `FINALIZE → policy-gatekeeper`

### policy-gatekeeper
**Do:** Check governance requirements, policy compliance
**Gates:** Update `governance` status
**Route:** Issues → `policy-fixer` | Clean → `pr-preparer`

### policy-fixer
**Do:** Address policy violations
**Route:** `NEXT → policy-gatekeeper`

### pr-preparer
**Do:** Prepare PR for publication, clean commit history
**Route:** `NEXT → diff-reviewer`

### diff-reviewer
**Do:** Review final diff, ensure quality
**Route:** `NEXT → prep-finalizer`

### prep-finalizer
**Do:** Final preparation validation
**Route:** `FINALIZE → pr-publisher`

### pr-publisher
**Do:** Create PR, set initial labels and description
**Labels:** Set `flow:generative state:in-progress`
**Route:** `NEXT → merge-readiness`

### merge-readiness
**Do:** Assess readiness for review process
**Route:** `FINALIZE → pub-finalizer`

### pub-finalizer
**Do:** Final publication validation, set appropriate state
**Labels:** Set final state (`ready` or `needs-rework`)
**Route:** **FINALIZE** (handoff to Review flow)

## Progress Heuristics

Consider "progress" when these improve:
- Failing tests ↓, test coverage ↑
- Clippy warnings ↓, code quality ↑
- Build failures ↓, feature compatibility ↑
- Mutation score ↑ (target ≥80%)
- Fuzz crashers ↓, security issues ↓
- Performance benchmarks established
- Documentation completeness ↑ (with working examples)
- API contracts validated

## Storage Convention Integration

- `docs/` - CLI reference, API documentation, troubleshooting guides, ADRs
- `copybook-core/src/` - COBOL parsing engine (lexer, parser, AST, layout)
- `copybook-codec/src/` - Data encoding/decoding, character conversion
- `copybook-cli/src/` - CLI implementation with subcommands
- `copybook-gen/src/` - Test fixture generation for COBOL copybooks
- `copybook-bench/src/` - Performance benchmarks and enterprise validation
- `xtask/src/` - Build automation and CI orchestration
- `fixtures/` - COBOL test data, enterprise copybook samples
- `examples/` - Usage examples for mainframe data processing
- `scripts/` - Performance testing, enterprise validation automation

## Worktree Discipline

- **ONE writer at a time** (serialize agents that modify files)
- **Read-only parallelism** only when guaranteed safe
- **Natural iteration** with evidence of progress; orchestrator manages stopping
- **Full implementation authority** for creating COBOL copybook processing features and implementations within this generative flow iteration

## Success Criteria

**Complete Implementation:** Draft PR exists with complete COBOL copybook processing implementation, all required gates pass (`spec, format, clippy, tests, build, docs`), TDD practices followed, copybook-rs enterprise compatibility validated
**Partial Implementation:** Draft PR with working COBOL parsing scaffolding, prioritized plan, evidence links, and clear next steps for completion

Begin with COBOL copybook processing issue requirements and invoke agents proactively through the microloop structure, following copybook-rs TDD-driven, Rust-first development standards with enterprise mainframe data processing validation.

## copybook-rs Generative Adapter — Required Behavior (subagent)

Receipts
- **Check Run:** emit exactly one for **`generative:gate:<GATE>`** with summary text.
- **Ledger:** update the single PR Ledger comment (edit in place):
  - Rebuild the Gates table row for `<GATE>`.
  - Append a one-line hop to Hoplog.
  - Refresh Decision with `State` and `Next`.

Status
- Use only `pass | fail | skipped`. Use `skipped (reason)` for N/A or missing tools.

Bounded Retries
- At most **2** self-retries on transient/tooling issues. Then route forward.

Commands (copybook-rs-specific)
- Prefer: `cargo nextest run --workspace`, `cargo build --workspace --release`, `cargo xtask ci`, `just ci-quick`, `PERF=1 cargo bench -p copybook-bench`.
- Enterprise validation with performance targets and zero unsafe code enforcement.
- Fallbacks allowed (gh/git). May post progress comments for transparency.

Generative-only Notes
- If `<GATE> = security` and issue is not security-critical → set `skipped (generative flow)`.
- If `<GATE> = benchmarks` → record baseline only; do **not** set `perf`.
- For feature verification → run **comprehensive workspace validation** and set `<GATE> = features`.
- For enterprise gates → validate performance targets and zero unsafe code.
- For parsing gates → test COBOL copybook parsing accuracy with fixtures.

Routing
- On success: **FINALIZE → <FINALIZE_TARGET>**.
- On recoverable problems: **NEXT → self** or **NEXT → <NEXT_TARGET>** with evidence.
