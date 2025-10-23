# Issue #74 Closure Plan: Documentation Truth & Receipts

**Date**: 2025-10-22
**Status**: Planning → Execution
**Objective**: Close all documentation gaps and establish receipts-backed claims

---

## Executive Summary

Based on comprehensive exploration (3 agents, 3 reports, 1,800+ lines of analysis), this plan resolves **5 critical issues** and **5 high-impact issues** across 10+ files through 5 focused PRs.

**Critical Findings**:
1. **4 tests currently failing** (LRECL validation in record.rs)
2. **Test counts are false** (claims 529, actually 277 passing + 4 failing)
3. **Performance claims unlinked** (2.33 GiB/s vs measured 66-95 MiB/s)
4. **Status contradictory** (PRODUCTION READY vs Engineering Preview)
5. **Level-88 undocumented** (CLAUDE.md says supported, README says unsupported)

**Infrastructure Status**:
- ✅ Tests: 664 functions, 111 files, comprehensive feature coverage
- ✅ Benchmarks: Criterion.rs, baseline management, JSON reporting
- ✅ CI: 13 jobs, multi-platform, security scanning
- ⚠️ Docs: Major contradictions across CLAUDE.md, README.md, REPORT.md, ROADMAP.md

---

## Source Reports

1. **ISSUE_74_DOCUMENTATION_AUDIT_REPORT.md** (555 lines)
   - Performance/marketing claims audit
   - Test count claims audit
   - COBOL support claims audit
   - CI infrastructure audit
   - MSRV/edition declarations audit

2. **docs/TEST_INFRASTRUCTURE_LANDSCAPE.md** (600 lines)
   - Test inventory by category (664 tests)
   - Feature coverage matrix
   - Error code test coverage (23 codes)
   - Golden fixtures deep dive

3. **docs/exploration-benchmark-infrastructure.md** (713 lines)
   - Benchmark crate structure
   - Baseline management (established 2025-09-30)
   - JSON reporting (perf.json schema)
   - CI integration (perf.yml, benchmark.yml)

---

## 5-PR Execution Plan

### PR-1: Verification & Test Count Truth
**Status**: CRITICAL (blocking all other PRs)
**Estimated Effort**: 2-3 hours
**Refs**: #74

**Objective**: Fix failing tests and establish single source of truth for test counts

**Tasks**:
1. ✅ Fix 4 failing tests in `copybook-codec/src/record.rs`
   - Tests expect `CBKP001_SYNTAX` but receive `CBKI001_INVALID_STATE`
   - Lines: 1097, 1106, 1146, 1155
   - Update assertions to match actual error codes

2. ✅ Run full test suite and capture actual counts
   - `cargo nextest run --workspace`
   - Document: X passing, Y failing, Z skipped, total W

3. ✅ Update documentation with true counts
   - `CLAUDE.md` lines 11, 225
   - `README.md` lines 38, 868
   - `docs/REPORT.md` line 20
   - Format: "X/Y tests passing (Z ignored)"

4. ✅ Add CI guard to prevent drift
   - Create `xtask docs verify-tests` command
   - Fail if docs claim different count than CI

**Files to Edit**:
- `copybook-codec/src/record.rs` (fix 4 tests)
- `CLAUDE.md` (lines 11, 225)
- `README.md` (lines 38, 868)
- `docs/REPORT.md` (line 20)

**Acceptance**:
- All tests passing (no failures)
- Documentation matches `cargo nextest` output
- CI enforces test count truth

---

### PR-2: Performance Truth & Linkage
**Status**: HIGH PRIORITY
**Estimated Effort**: 3-4 hours
**Refs**: #74, #52, #66, #49

**Objective**: Consolidate performance claims with dated baselines and remove unlinked claims

**Tasks**:
1. ✅ Audit all performance claims
   - Remove aspirational numbers (4.1+ GiB/s, 2.5+ GiB/s)
   - Link all numbers to baseline files with dates
   - Add baseline receipt references

2. ✅ Update CLAUDE.md
   - Line 10: Replace "2.33 GiB/s" with baseline-linked claim
   - Line 271-272: Remove golden fixtures performance targets or link to receipts
   - Add: "See REPORT.md line 35 for canonical baseline"

3. ✅ Update README.md
   - Line 16: Replace vague "tens of MiB/s" with specific baseline link
   - Add Performance section with baseline table
   - Reference `copybook-bench/BASELINE_METHODOLOGY.md`

4. ✅ Update REPORT.md
   - Keep as canonical source (lines 35-36: 205 MiB/s, 58 MiB/s)
   - Add explanation of why historic GiB/s targets changed
   - Document variance expectations (WSL2 vs native)

5. ✅ Clean up other files
   - `enterprise-security-validation.md` line 35: remove/link claim
   - `fixtures/enterprise/audit/README.md` line 18: remove/link claim
   - `docs/IMPLEMENTATION_FINALIZATION_STATUS.md` lines 49, 75, 83: mark aspirational

6. ✅ Document SLO policy
   - `.github/workflows/benchmark.yml`: Add comment why advisory (not blocking)
   - Link to Issue #89, #95 for historical context

**Files to Edit**:
- `CLAUDE.md` (lines 10, 271-272)
- `README.md` (line 16, add Performance section)
- `docs/REPORT.md` (add context section)
- `enterprise-security-validation.md` (line 35)
- `fixtures/enterprise/audit/README.md` (line 18)
- `docs/IMPLEMENTATION_FINALIZATION_STATUS.md` (lines 49, 75, 83)
- `.github/workflows/benchmark.yml` (add SLO policy comment)

**Acceptance**:
- All performance numbers link to dated baselines
- No unlinked GiB/s or MiB/s claims
- REPORT.md is canonical source
- Historic target changes explained

---

### PR-3: Status Alignment
**Status**: HIGH PRIORITY
**Estimated Effort**: 1-2 hours
**Refs**: #74, #75

**Objective**: Single, consistent status across all documentation

**Tasks**:
1. ✅ Establish canonical status in ROADMAP.md
   - Line 3: Confirm "⚠️ Engineering Preview (v0.3.1 maintenance)"

2. ✅ Update CLAUDE.md
   - Line 9: Change "PRODUCTION READY" to "Engineering Preview (v0.3.1)"
   - Add "Known Limitations & Roadmap" section
   - Link to ROADMAP.md for full context

3. ✅ Update README.md
   - Line 43: Align "Production-ready interfaces" with context
   - Lines 937-939: Ensure "Engineering Preview" is prominent
   - Add caveat: "Production-ready CLI, engineering preview for completeness"

4. ✅ Update REPORT.md
   - Line 149: Move "Cautious Adoption Recommended ⚠️" to top
   - Add status banner matching ROADMAP.md

**Files to Edit**:
- `docs/ROADMAP.md` (confirm line 3)
- `CLAUDE.md` (line 9, add section)
- `README.md` (lines 43, 937-939)
- `docs/REPORT.md` (line 149, add banner)

**Acceptance**:
- All 4 docs agree on "Engineering Preview" status
- ROADMAP.md is canonical source
- Users see consistent message

---

### PR-4: Feature Completeness Matrix
**Status**: MEDIUM PRIORITY
**Estimated Effort**: 4-5 hours
**Refs**: #74, #44, #51, #72, #86

**Objective**: Create COBOL Support Matrix and resolve Level-88 contradiction

**Tasks**:
1. ✅ Create `docs/reference/COBOL_SUPPORT_MATRIX.md`
   - Data types: DISPLAY, COMP-3, BINARY (supported); COMP-1/2 (not supported)
   - Structures: ODO, REDEFINES, OCCURS (supported); nested ODO, RENAMES (not supported)
   - Level-88: Resolve contradiction with test evidence
   - Edited PIC: Not supported (negative tests exist)
   - Sign handling: SIGN SEPARATE (not supported)
   - Record formats: Fixed, RDW (supported)
   - Codepages: CP037, CP273, CP500, CP1047, CP1140 (all supported)

2. ✅ Resolve Level-88 contradiction
   - Check `copybook-core/tests/golden_fixtures_ac2_level88_after_odo.rs` (638 lines)
   - If tests exist and pass: SUPPORTED
   - If tests exist but fail: PARTIALLY SUPPORTED
   - If no real tests: UNSUPPORTED
   - Document API examples if supported

3. ✅ Update CLAUDE.md
   - Line 20: Align with matrix decision
   - Add link to COBOL_SUPPORT_MATRIX.md

4. ✅ Update README.md
   - Line 825: Replace with matrix link
   - Add "See COBOL_SUPPORT_MATRIX.md for complete feature coverage"

5. ✅ Update REPORT.md
   - Line 87: Align with matrix

6. ✅ Add CLI support probe (optional future work)
   - `copybook support --matrix` (print table)
   - `copybook support --check <.cpy>` (exit non-zero if unsupported)

**Files to Create**:
- `docs/reference/COBOL_SUPPORT_MATRIX.md` (NEW)

**Files to Edit**:
- `CLAUDE.md` (line 20)
- `README.md` (line 825)
- `docs/REPORT.md` (line 87)

**Acceptance**:
- Single COBOL_SUPPORT_MATRIX.md is canonical
- Level-88 status resolved with evidence
- All docs reference matrix
- Each row has: Feature | Status | Test File | Notes

---

### PR-5: Toolchain & Baseline Alignment
**Status**: MEDIUM PRIORITY
**Estimated Effort**: 2-3 hours
**Refs**: #74

**Objective**: MSRV consistency, benchmark path fixes, leak detector tracking

**Tasks**:
1. ✅ Resolve MSRV mismatch
   - `Cargo.toml` line 15: `rust-version = "1.89"`
   - `ci.yml` line 44: Tests 1.90.0
   - `REPORT.md` line 96: Says "1.90+"
   - Decision: Test 1.89 in CI OR change Cargo.toml to 1.90
   - Add comment explaining why

2. ✅ Fix benchmark path inconsistency
   - README references `scripts/bench/perf.json`
   - Actual file: `test_perf.json` at root
   - Options: Move file OR update docs
   - Canonical path: `scripts/bench/perf.json` (create dir if needed)

3. ✅ Document leak detector backlog
   - README line 872: "eight leak detectors still queued"
   - Create tracking issue or link to existing
   - Add to CI as advisory check (not blocking)

4. ✅ Verify edition consistency
   - Cargo.toml: `edition = "2024"`
   - .config/rustfmt.toml: `edition = "2024"`
   - All workspace members: `edition.workspace = true`

**Files to Edit**:
- `Cargo.toml` (line 15, add comment or change version)
- `.github/workflows/ci.yml` (line 44, add 1.89 test or comment)
- `README.md` (line 862, align with decision)
- `CLAUDE.md` (line 178, align with decision)
- `docs/REPORT.md` (line 96, align with decision)
- README.md (line 872, add tracking link)

**Acceptance**:
- MSRV declared once, CI tests it
- Benchmark paths consistent
- Leak detector backlog tracked
- Edition 2024 everywhere

---

## Missing Issues to Create

Based on roadmap review, these issues need creation:

1. **RENAMES (66-level) Implementation**
   - Scope: Parser + layout projection + round-trip tests + negatives
   - Refs: #44 (umbrella), roadmap gap
   - Flip support matrix row only when fixtures pass

2. **Support Matrix CLI Probe**
   - `copybook support --matrix` (print table)
   - `copybook support --check <.cpy>` (fail on unsupported)
   - Generated from test annotations

3. **Fuzz/Property/Determinism Harness**
   - cargo-fuzz targets: lexer, PICTURE, numeric
   - Proptests for parser/codec cores
   - Deterministic JSON hashing receipt

4. **Reproducible Bench Harness + Operator Runbook**
   - Container/binary for perf.json emission
   - Plain-English runbook for operators
   - Matches CI fields exactly

---

## Dependency Graph

```
PR-1 (Tests)
  ↓
PR-2 (Performance) ← depends on test counts from PR-1
  ↓
PR-3 (Status) ← depends on reality from PR-1 + PR-2
  ↓
PR-4 (Matrix) ← needs test evidence from PR-1
  ↓
PR-5 (Toolchain) ← independent but benefits from PR-1 fixes
```

**Execution Order**: Strictly serial (PR-1 → PR-2 → PR-3 → PR-4 → PR-5)

---

## Gate Table (Post-Closure)

| Gate | Enforcing? | Blocking? | Source |
|------|-----------|-----------|--------|
| Format (rustfmt) | ✅ Yes | ✅ Yes | ci.yml |
| Clippy (pedantic) | ✅ Yes | ✅ Yes | pedantic-diff.yml |
| Build (workspace) | ✅ Yes | ✅ Yes | ci.yml |
| Tests (nextest) | ✅ Yes | ✅ Yes | ci.yml (post PR-1) |
| Coverage | ✅ Yes | ✅ Advisory | ci.yml |
| Security (audit) | ✅ Yes | ✅ Yes | security-scan.yml |
| Docs truth | ✅ Yes | ✅ Yes | NEW (PR-1) |
| Tests truth | ✅ Yes | ✅ Yes | NEW (PR-1) |
| Features matrix | ✅ Yes | ✅ Advisory | NEW (PR-4) |
| Perf/throughput | ✅ Yes | ⚠️ Neutral | perf.yml (policy: accuracy-first) |

---

## Commit Message Skeletons

```bash
# PR-1
git commit -m "test: fix LRECL validation; sync test counts across docs (refs #74)"

# PR-2
git commit -m "docs: perf claims → baseline receipts; remove unlinked GiB/s (refs #74, #52)"

# PR-3
git commit -m "docs: align status to 'Engineering Preview' across 4 docs (refs #74, #75)"

# PR-4
git commit -m "docs: COBOL Support Matrix; resolve Level-88 contradiction (refs #74, #44)"

# PR-5
git commit -m "docs: MSRV/edition/benchmark path normalization (refs #74)"
```

---

## Definition of "Done" for Issue #74

1. ✅ All tests passing (no failures)
2. ✅ Test counts in docs match CI output
3. ✅ Performance numbers link to dated baselines
4. ✅ Status consistent across CLAUDE.md, README.md, REPORT.md, ROADMAP.md
5. ✅ COBOL_SUPPORT_MATRIX.md exists and is canonical
6. ✅ Level-88 contradiction resolved with evidence
7. ✅ MSRV/edition consistent across Cargo.toml, CI, docs
8. ✅ Benchmark paths consistent
9. ✅ CI enforces test count truth
10. ✅ No orphaned performance claims

---

## Agent Execution Plan

### Phase 1: Fix & Verify (PR-1)
- **impl-creator**: Fix 4 failing tests in record.rs
- **impl-finalizer**: Validate fixes, run test suite
- **generative-code-reviewer**: Quality check before commit
- **doc-updater**: Update test counts in CLAUDE.md, README.md, REPORT.md

### Phase 2: Performance (PR-2)
- **doc-updater**: Audit and link performance claims
- **doc-fixer**: Clean up aspirational numbers
- **generative-diff-reviewer**: Pre-commit validation

### Phase 3: Status (PR-3)
- **doc-updater**: Align status across 4 documents
- **doc-fixer**: Ensure consistency

### Phase 4: Matrix (PR-4)
- **spec-creator**: Create COBOL_SUPPORT_MATRIX.md specification
- **doc-updater**: Populate matrix with test evidence
- **doc-fixer**: Resolve Level-88 contradiction
- **generative-link-checker**: Validate all matrix links

### Phase 5: Toolchain (PR-5)
- **doc-updater**: MSRV/edition alignment
- **policy-fixer**: Fix benchmark paths
- **doc-fixer**: Cleanup and final sweep

### Phase 6: Final Validation
- **docs-finalizer**: Verify all docs build and links valid
- **generative-diff-reviewer**: Final pre-publication review
- **pr-preparer**: Prepare each PR for GitHub

---

## Success Metrics

- **0 failing tests** (currently 4)
- **0 performance claims without receipts** (currently 8+)
- **0 status contradictions** (currently 4 documents disagree)
- **1 canonical support matrix** (currently scattered)
- **1 MSRV declaration** (currently 3+ variations)
- **100% CI enforcement** of test/doc truth

---

## Timeline Estimate

- **Day 1**: PR-1 (fix tests, sync counts) — 2-3 hours
- **Day 2**: PR-2 (perf truth) — 3-4 hours
- **Day 3**: PR-3 (status) — 1-2 hours
- **Day 4**: PR-4 (matrix) — 4-5 hours
- **Day 5**: PR-5 (toolchain) — 2-3 hours

**Total**: 12-17 hours over 5 days (single senior dev, worktree-serial)

---

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Level-88 tests failing | Document as "partial support" with caveats |
| Performance baseline unavailable | Use test_perf.json as interim, document limitations |
| MSRV change breaks users | Test both 1.89 and 1.90 in CI during transition |
| Matrix generation complex | Manual first pass, automate in future PR |

---

## References

- Issue #74: Documentation & Claims Gaps
- Issue #75: Roadmap (accuracy-first posture)
- Issue #44: Edited PIC / Separate Sign (umbrella)
- Issue #51: ODO Dialect Lower-bound
- Issue #72: Overpunch 'D' Verification
- Issue #86: Level-88 Comma in VALUE
- Issue #52: Benchmark Reporting Infrastructure
- Issue #66: Performance Gate Receipts
- Issue #49: Baseline Establishment

---

## Appendix: Key File Paths

**Documentation**:
- `/home/steven/code/Rust/copybook-rs/CLAUDE.md`
- `/home/steven/code/Rust/copybook-rs/README.md`
- `/home/steven/code/Rust/copybook-rs/docs/REPORT.md`
- `/home/steven/code/Rust/copybook-rs/docs/ROADMAP.md`

**Tests**:
- `/home/steven/code/Rust/copybook-rs/copybook-codec/src/record.rs` (4 failing tests)
- `/home/steven/code/Rust/copybook-rs/copybook-core/tests/golden_fixtures_ac2_level88_after_odo.rs`

**Benchmarks**:
- `/home/steven/code/Rust/copybook-rs/copybook-bench/BASELINE_METHODOLOGY.md`
- `/home/steven/code/Rust/copybook-rs/copybook-bench/HARDWARE_SPECS.md`
- `/home/steven/code/Rust/copybook-rs/test_perf.json`

**CI**:
- `/home/steven/code/Rust/copybook-rs/.github/workflows/ci.yml`
- `/home/steven/code/Rust/copybook-rs/.github/workflows/perf.yml`
- `/home/steven/code/Rust/copybook-rs/.github/workflows/benchmark.yml`

**To Create**:
- `/home/steven/code/Rust/copybook-rs/docs/reference/COBOL_SUPPORT_MATRIX.md`
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
