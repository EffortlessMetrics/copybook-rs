# copybook-rs Roadmap — Accuracy-First to Production MVP

**Status**: ⚠️ Engineering Preview (v0.4.3)
**Last Updated**: 2025-12-31
**Canonical Reference**: [docs/ROADMAP.md](https://github.com/EffortlessMetrics/copybook-rs/blob/main/docs/ROADMAP.md)

> **Policy (active)**: Performance is out-of-scope for MVP. Perf/throughput gates remain neutral (`skipped: policy`). MVP value = processing **accuracy**, round-trip fidelity, explicit **support matrix**, stable error codes, and **honest docs**. Every public claim links to a receipt (test, check-run, or artifact).

---

## Current State Summary (Dec 31, 2025)

### Quality Metrics
- **Tests**: 1015 passing, 60 skipped (nextest)
- **Production panics**: 0 (test-only panics acceptable)
- **Unsafe code**: 0 in public API paths
- **Error codes**: 40+ stable taxonomy (CBKD, CBKE, CBKF, CBKI, CBKS families)
- **Dependencies**: 85+ workspace-inherited

### Performance Baseline (Commit 1fa63633, 2025-09-30)
- **DISPLAY-heavy**: 205 MiB/s (2.56x above 80 MiB/s floor)
- **COMP-3-heavy**: 58 MiB/s (1.45x above 40 MiB/s floor)
- **Environment**: WSL2, AMD Ryzen 9 9950X3D (32 threads, 196 GiB RAM)
- **CI Enforcement**: Throughput floors advisory-only in v0.4.x

---

## Phase 0 — Baseline Honesty ✅ COMPLETE

**Completed**: 2025-11-05

| Objective | Status | Evidence |
|-----------|--------|----------|
| Remove "production-ready" + GiB/s claims | ✅ | README, REPORT.md updated |
| Security scanning in CI | ✅ | PR #77: cargo-deny/audit/Dependabot |
| Machine-readable bench receipts | ✅ | `bench-report` CLI with baseline management |
| Golden fixtures validation | ✅ | 4,375 lines, AC1-AC8 enterprise scenarios |
| Test count sync to CI | ✅ | 1015 tests auto-reported |
| CI infrastructure locked | ✅ | Single blocking gate (CI Quick / test) |
| Branch protection enforced | ✅ | Strict mode with linear history |

---

## Phase 1 — Core Feature Completeness 🔄 IN PROGRESS

### Recently Completed (Dec 2025)

| Feature | PR/Commit | Status |
|---------|-----------|--------|
| **E3.1: Edited PIC Encoding** (numeric fields) | `976ca0f` | ✅ Merged |
| **D0: Dialect Lever Contract** (OCCURS DEPENDING ON lower bound) | `a9609af` | ✅ Merged |
| **N1: Nested ODO Design** (O5/O6 rejection) | PR #172 | ✅ Merged |
| **Panic Elimination** (0 production panics) | PR #182 | ✅ Merged |
| **RENAMES R2/R3** (group resolver + alias API) | PR #162, #163 | ✅ Merged |
| **Determinism Phases 1-2** (codec harness + CLI) | PR #158, #160 | ✅ Merged |
| **Level-88 Condition Values** (full support) | v0.4.0 | ✅ Shipped |
| **Edited PIC E1/E2** (parse + decode) | v0.4.0 | ✅ Shipped |

### Outstanding Phase 1 Work

| Feature | Issue | Effort | Status |
|---------|-------|--------|--------|
| **E3.2: Sign Editing (+/-)** | v0.5.0 | 1-2 PD | 🔄 Ready |
| **E3.3: CR/DB** (credit/debit) | v0.5.0 | 1-2 PD | ⏳ Blocked by E3.2 |
| **E3.4: Commas & Separators** | v0.5.0 | 1-2 PD | ⏳ Blocked by E3.2 |
| **E3.5: Asterisk Fill** | v0.5.0 | 1 PD | ⏳ Blocked by E3.2 |
| **E3.6: Currency Symbols** | v0.5.0 | 1 PD | ⏳ Blocked by E3.2 |
| **D1: Dialect Core** (layout validation) | #51 | 2-3 PD | 🔄 Ready |
| **D2: CLI Integration** | #51 | 1 PD | ⏳ Blocked by D1 |
| **D3: Golden Fixtures** (dialect test data) | #51 | 1 PD | ⏳ Blocked by D1 |
| **D4: Docs & Examples** | #51 | 0.5 PD | ⏳ Blocked by D1 |
| **RENAMES codec projection** | #110 | 2-3 PD | 🔄 Ready |

---

## Phase 2 — Reliability & Regression Confidence 🔄 PARTIAL

| Objective | Issue | Status | Notes |
|-----------|-------|--------|-------|
| Determinism CI smoke test | #112 | ✅ Ready | Template in `.github/workflows/determinism-smoke.yml` |
| Mutation testing | #97 | ⏳ Planned | T4 quality gate |
| Fuzzing validation | #98 | ⏳ Planned | T4 quality gate |
| Security scanning (enhanced) | #99 | ⏳ Planned | safety-scanner agent |
| Policy gatekeeper | #100 | ⏳ Planned | T5 systematic enforcement |
| CI-level end-to-end tests | — | ⏳ Planned | CLI spawn with real copybooks |
| Audit trails | #60 | ⏳ Planned | Input hash, schema version, output hash |

---

## Phase 3 — Operational Readiness ⏳ NOT STARTED

| Deliverable | Issue | Status |
|-------------|-------|--------|
| Benchmark container + runbook | #113 | ⏳ Planned |
| Configuration guidance (50-100 MiB/s) | — | ⏳ Planned |
| Reproducible bench harness | — | ⏳ Planned |
| Manual perf validation runbook | — | ⏳ Planned |

---

## Phase 4 — Launch Readiness Review ⏳ NOT STARTED

| Objective | Status |
|-----------|--------|
| Re-run shipping assessment | ⏳ |
| Verify README/docs claims link to artifacts | ⏳ |
| Support matrix verification | ⏳ |
| Zero-sell vs cautious marketing decision | ⏳ |

---

## COBOL Feature Support Summary

### Fully Supported (✅)

| Feature | Test Evidence |
|---------|---------------|
| DISPLAY (PIC X) | 21+ tests |
| Zoned Decimal (PIC 9) | 18+ tests |
| COMP-3 (Packed Decimal) | 512+ property cases |
| BINARY (COMP) | 11+ tests |
| ODO (O1-O4 scenarios) | 21+ tests |
| REDEFINES | 34+ tests |
| Level-88 condition values | 24+ tests, 1476 lines golden fixtures |
| RENAMES (R1-R3) | 30+ tests |
| Fixed-length records | 35+ tests |
| RDW (Variable-length) | 26+ tests |
| CP037/273/500/1047/1140 | 8+ tests each |
| **Edited PIC E1/E2** (parse + decode) | 43 tests |
| **Edited PIC E3.1** (basic encode) | 920+ lines |

### Partially Supported (⚠️)

| Feature | Status | Notes |
|---------|--------|-------|
| Edited PIC encode (E3.0-E3.7) | ✅ | E1-E3.7 implemented; monitor feature parity for edge patterns |
| COMP-1/COMP-2 | ✅ | Fully supported (promoted to stable in v0.4.3) |
| Dialect lever (D1-D4) | ✅ | Completed and documented |
| SIGN SEPARATE | ✅ | Fully supported (promoted to stable in v0.4.3) |

### Not Supported (❌)

| Feature | Decision | Notes |
|---------|----------|-------|
| Nested ODO (O5) | #164 | Rejected by design |
| ODO over REDEFINES (O6) | #164 | Rejected by design |
| RENAMES R4-R6 | Policy-limited (R1-R3 default, R4-R6 partial/guarded) | OCCURS/REDEFINES interactions |

---

## Release Timeline

| Milestone | Target | Key Deliverables |
|-----------|--------|------------------|
| **v0.4.2** | Q1 2025 | E3.1 + D0 shipped; quality hardening |
| **v0.5.0** | Q1 2026 | E3.2-E3.6 (full edited PIC encode), D1-D4 (dialect lever), Determinism CI |
| **v1.0.0** | Q2 2026 | API freeze, ecosystem adapters, 6-month support policy |

---

## Issue Linkage

### Critical Path Issues

| ID | Title | Phase | Status |
|----|-------|-------|--------|
| #51 | Dialect: lower bound for OCCURS DEPENDING ON | 1 | 🔄 D0 complete, D1-D4 ready |
| #110 | RENAMES codec projection | 1 | 🔄 R1-R3 complete, projection ready |
| #112 | Fuzz testing + determinism harness | 2 | ✅ Phases 1-2 shipped |
| #113 | Benchmark harness container | 3 | ⏳ Planned |

### Quality Gate Issues

| ID | Title | Priority |
|----|-------|----------|
| #97 | Mutation testing (T4) | High |
| #98 | Fuzzing validation (T4) | High |
| #99 | Security scanning (safety-scanner) | High |
| #100 | Policy gatekeeper (T5) | Medium |

### Technical Debt

| ID | Title | Priority |
|----|-------|----------|
| #71 | CI tripwire for macro misuse | Medium |
| #87 | Document ignored tests | Low |
| #89 | COMP-3 SLO regression investigation | Medium |

---

## Success Criteria for MVP

1. **Correctness**: Documented support with fixtures/tests for high-priority COBOL constructs
2. **Honest documentation**: Current performance, feature coverage, operational processes
3. **Automated regression**: Benchmark tooling producing repeatable artifacts
4. **Clear backlog**: Owner-assigned issues tracking enhancements and deferrals
5. **Enterprise readiness**: Teams can validate copybooks against supported features

---

## Gate Table (MVP enforcement)

**Required**: spec, format, clippy, tests, build, features, enterprise(accuracy), coverage, security, docs

**Neutralized**: benchmarks, perf, throughput → `skipped (policy: accuracy-first)`

---

## Next Actions (Week of 2025-12-30)

1. ✅ Close E3.1 and D0 (committed)
2. 🔄 Update ROADMAP.md with E3.1/D0 completion status
3. 🔄 Update COBOL_SUPPORT_MATRIX.md with new features
4. ⏳ Begin E3.2 (Sign Editing) implementation
5. ⏳ Begin D1 (Dialect Core) implementation

---

*This issue tracks the canonical roadmap. For detailed technical status, see [docs/ROADMAP.md](https://github.com/EffortlessMetrics/copybook-rs/blob/main/docs/ROADMAP.md) and [docs/REPORT.md](https://github.com/EffortlessMetrics/copybook-rs/blob/main/docs/REPORT.md).*
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
