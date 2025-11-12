# copybook-rs Implementation Priorities

**Last Updated**: 2025-11-12
**Source**: Comprehensive research of 35 open GitHub issues
**Authority**: This document is the source of truth for prioritization. When in conflict with issue labels or other docs, this document wins.

This document tracks actionable implementation priorities based on comprehensive issue research. All priorities are informed by effort estimates, dependencies, and enterprise impact analysis.

---

## 🚀 Quick Wins (< 1 Week)

High-value features with minimal implementation effort, ready for immediate work.

### Issue #133: RENAMES Nested Group Attach
- **Effort**: 6-8 hours
- **Status**: Foundation complete (PR #129), resolver-only changes needed
- **Approach**: Add `find_group_span()` helper, update `resolve_renames_aliases()`
- **Impact**: Completes RENAMES same-scope support for levels 02-49
- **Files**: `copybook-core/src/layout.rs`
- **Tests**: Un-ignore 2 existing tests, add 2 new tests
- **Risk**: Low (backward compatible, resolver-only changes)

### Issue #88: Hardcoded Unix Paths
- **Effort**: 4-6 hours
- **Status**: Partially fixed (PR #150), 5 paths remain across 4 files
- **Approach**: Use `std::env::temp_dir()`, `PathBuf` with portable patterns
- **Impact**: Cross-platform compatibility (Windows, macOS, Linux)
- **Files**: `copybook-codec/tests/*.rs`, `copybook-cli/tests/*.rs`
- **Tests**: Existing tests validate portability
- **Risk**: Low (test-only changes)

### Issue #66: Benchmark Reporting (Complete)
- **Effort**: 1-1.5 days (80% complete)
- **Status**: `bench-report` CLI exists, missing PR comment automation
- **Approach**: GitHub API integration for automated PR comments
- **Impact**: Automated performance tracking in CI
- **Files**: `copybook-bench/src/bin/bench-report.rs`, `.github/workflows/perf.yml`
- **Tests**: CI validation with real PR comments
- **Risk**: Low (infrastructure mostly complete)

---

## 📦 Medium Efforts (1-2 Weeks)

Strategic features with moderate complexity and high enterprise value.

### Issue #111: Support Matrix CLI
- **Effort**: 2.5 days
- **Status**: CLI stub exists, comprehensive design complete
- **Approach**:
  1. Display support matrix (0.5 days)
  2. Two-phase detection (parse + AST traversal) (1.5 days)
  3. CLI integration with exit codes (0.5 days)
- **Impact**: Self-qualification before migration investment
- **Files**: `copybook-cli/src/commands/support.rs`
- **Tests**: Matrix display, feature detection, exit code validation
- **Risk**: Low (well-designed, clear acceptance criteria)

### Issue #113: Benchmark Harness Container
- **Effort**: 1.5-2.5 days
- **Status**: 80% infrastructure exists, containerization needed
- **Approach**:
  1. Multi-stage Dockerfile (0.5-0.75 days)
  2. CI integration (0.25-0.5 days)
  3. Operator runbook (0.5-1 day)
  4. Validation (0.25-0.5 days)
- **Impact**: Reproducible benchmarks, operator self-service
- **Files**: `Dockerfile`, `.github/workflows/docker-build.yml`, `docs/operations/BENCHMARK_RUNBOOK.md`
- **Tests**: Container build, smoke tests, end-to-end workflow
- **Risk**: Low (infrastructure foundation solid)

### Issue #112: Fuzz Testing + Determinism
- **Effort**: 5.5-8.5 days
- **Status**: proptest exists (50% complete), cargo-fuzz + BLAKE3 missing
- **Approach** (priority-resequenced):
  1. Determinism harness with BLAKE3 (1-2 days) — **HIGHEST PRIORITY**
  2. Enhanced property testing (2-3 days) — **HIGH PRIORITY**
  3. Fuzz infrastructure (2-3 days) — **MEDIUM PRIORITY**
  4. CI integration (0.5 days) — **FINAL PHASE**
- **Impact**: Enterprise audit compliance, crash discovery, correctness validation
- **Files**: `fuzz/`, `copybook-cli/src/commands/determinism.rs`, property test modules
- **Tests**: 200+ properties, 6 fuzz targets, determinism CI gate
- **Risk**: Medium (setup cost for fuzzing, determinism gate may need tuning)

---

## 🏗️ Large Efforts (2-4 Weeks)

Complex features requiring significant design and implementation work.

### Issue #110: RENAMES Codec Projection
- **Effort**: 9-12 days
- **Status**: Parser + resolver complete, codec projection needed
- **Approach**:
  1. Read-side decode (composite JSON) (4-5 days)
  2. Write-side encode (field distribution) (3-4 days)
  3. Round-trip validation (1-2 days)
  4. CLI + documentation (1-2 days)
- **Impact**: Complete RENAMES support for enterprise mainframe compatibility
- **Dependencies**: Issue #133 (nested group attach)
- **Files**: `copybook-codec/src/lib_api.rs`, `copybook-codec/src/json.rs`
- **Tests**: Codec round-trip, golden fixtures, performance validation
- **Risk**: Medium (complex codec changes, performance impact)

### Issue #51: ODO Lower Bound (Dialect)
- **Effort**: 2-3 weeks
- **Status**: Design complete, parser + codec changes needed
- **Approach**:
  1. Config infrastructure (2-3 days)
  2. Parser integration (3-4 days)
  3. Codec enforcement (4-5 days)
  4. Golden fixtures (2-3 days)
  5. Documentation (1-2 days)
- **Impact**: Dialect flexibility for IBM vs Micro Focus compatibility
- **Files**: Parser, codec, config system
- **Tests**: Golden fixtures for each dialect setting
- **Risk**: Medium (backward compatibility critical)

---

## 📊 Epic Tracking

Large-scale efforts requiring systematic approach and multi-phase implementation.

### Issue #84: Panic Audit
- **Effort**: 4-phase systematic audit
- **Status**: 403 panic sites identified, 1 critical hot-path panic
- **Approach**:
  1. Hot-path audit (parser, codec, iterator)
  2. Result propagation for 181 remaining unwraps
  3. CI enforcement (clippy lints)
  4. Documentation and monitoring
- **Impact**: Enterprise reliability, eliminates panic vectors
- **Risk**: High (workspace-wide changes, performance impact)

### Issue #87: Ignored Tests Documentation
- **Effort**: Documentation + triage
- **Status**: 73+ ignored tests cataloged
- **Approach**: Document rationale, prioritize unignore efforts
- **Impact**: Test suite hygiene, quality transparency

### Issue #75: Roadmap Maintenance
- **Effort**: Ongoing
- **Status**: Active maintenance required
- **Approach**: Update with research findings, track milestones
- **Impact**: Project transparency, planning alignment

---

## ❌ Recommended Closures

Issues identified as duplicates or already resolved during comprehensive research.

### Close as Duplicate
- **Issue #124** → Consolidate into #112 (fuzz testing)
- **Issue #123** → Consolidate into #111 (support matrix CLI)
- **Issue #125** → Already closed (benchmark harness)

### Close as Resolved
- **Issue #104** → Fixed in PR #105 (RDW field naming)
- **Issue #95** → Baseline established (COMP-3 performance), consolidate into #89
- **Issue #107** → Both followup items complete (PR #90)
- **Issue #72** → Overpunch D mapping verified correct
- **Issue #79** → Test doesn't exist, RDW design correct

### Clarify or Close
- **Issue #71** → `println!` is intentional CI tripwire, not debug code

---

## 🎯 Recommended Priority Order

Based on effort estimates, enterprise impact, and dependencies:

### Week 1: Quick Wins
1. Issue #133 (RENAMES nested groups) — 6-8 hours
2. Issue #88 (hardcoded paths) — 4-6 hours
3. Issue #66 (benchmark reporting) — 1-1.5 days
4. Close duplicates and resolved issues

### Week 2-3: Strategic Features
1. Issue #111 (Support Matrix CLI) — 2.5 days
2. Issue #113 (Benchmark container) — 1.5-2.5 days
3. Issue #112 Phase 1 (Determinism harness) — 1-2 days

### Week 4-6: Enhanced Quality
1. Issue #112 Phases 2-4 (Property tests + Fuzz) — 4.5-6.5 days
2. Issue #110 (RENAMES codec) — 9-12 days
3. Issue #84 Phase 1 (Hot-path panic audit)

### Future Milestones
1. Issue #51 (ODO lower bound dialect) — 2-3 weeks
2. Issue #84 Phases 2-4 (Systematic panic elimination)
3. Epic tracking (#87, #75)

---

## 📈 Enterprise Impact Summary

### Immediate Business Value (Week 1-3)
- **Cross-platform compatibility** (Issue #88)
- **Performance transparency** (Issue #66)
- **Feature self-qualification** (Issue #111)
- **RENAMES completion foundation** (Issue #133)

### Strategic Capabilities (Week 4-12)
- **Reproducible benchmarks** (Issue #113)
- **Audit compliance** (Issue #112 determinism)
- **Complete RENAMES support** (Issue #110)
- **Correctness validation** (Issue #112 fuzz/property)

### Long-term Reliability (3+ months)
- **Panic elimination** (Issue #84)
- **Dialect flexibility** (Issue #51)
- **Test suite hygiene** (Issue #87)
- **Project transparency** (Issue #75)

---

## 📝 Research Methodology

This priorities document is based on comprehensive research of all 35 open GitHub issues, completed 2025-11-12. Each issue received:

- ✅ Current state analysis
- ✅ Requirements specification
- ✅ Design approach with code examples
- ✅ Step-by-step implementation plans
- ✅ Effort estimates and dependencies
- ✅ Test strategies and acceptance criteria
- ✅ Enterprise mainframe compliance context

All findings have been posted as detailed comments to the respective GitHub issues for transparency and team collaboration.

---

## 🔄 Maintenance

This document should be updated:
- **Weekly**: After issue triage or priority changes
- **Monthly**: Comprehensive review with roadmap alignment
- **Per Milestone**: After v0.4.0, v0.5.0, v1.0.0 releases

**Document Owner**: Project maintainers
**Last Comprehensive Research**: 2025-11-12 (35 issues analyzed)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
