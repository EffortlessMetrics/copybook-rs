# Issue #74: Documentation & Claims Audit Report

**Objective**: Assess current documentation state and performance claims to inform 5-PR closure plan.
**Audit Date**: 2025-10-22
**Scope**: CLAUDEMD, README.md, REPORT.md, ROADMAP.md, docs/*.md, test claims, performance numbers, COBOL support matrix

---

## EXECUTIVE SUMMARY

**CRITICAL FINDING**: Documentation contains **major internal contradictions** between aspirational claims and actual measured performance. Test counts fluctuate across documents (461→527→529→572 tests), performance numbers range from 18-95 MiB/s (measured) to 4.1+ GiB/s (aspirational), and the project status alternates between "PRODUCTION READY" and "Engineering Preview" without clear revision history.

**Impact**: Users cannot reliably determine if copybook-rs is production-ready or engineering preview. Marketing claims in CLAUDE.md directly contradict measured data in README.md and REPORT.md.

**Severity**: HIGH - affects user trust and adoption decisions.

---

## 1. PERFORMANCE/MARKETING CLAIMS AUDIT

### 1.1 CONFLICTING THROUGHPUT NUMBERS

#### CLAUDE.md (v0.3.1 spec for developers)
**File**: `/home/steven/code/Rust/copybook-rs/CLAUDE.md`

| Claim | Location | Status | Issue |
|-------|----------|--------|-------|
| **"2.33 GiB/s" DISPLAY** | Line 10 | ❌ UNLINKED | No receipt/PR link provided |
| **"168-176 MiB/s" COMP-3** | Line 10 | ❌ UNLINKED | No receipt/PR link provided |
| **"2.5+ GiB/s" DISPLAY** | Line 271 (Golden Fixtures) | ❌ ASPIRATIONAL | No baseline evidence |
| **"100+ MiB/s" COMP-3** | Line 272 (Golden Fixtures) | ❌ ASPIRATIONAL | No baseline evidence |
| **"205 MiB/s" DISPLAY** | Line 186 | ✅ RECEIPT | Matches REPORT.md baseline (Sept 2025) |
| **"58 MiB/s" COMP-3** | Line 187 | ✅ RECEIPT | Matches REPORT.md baseline (Sept 2025) |

**Problem**: Lines 10 claim GiB/s numbers that don't appear in any linked baseline files. Lines 186-187 show more conservative MiB/s numbers. **No explicit link to baseline file or acceptance criteria.**

#### README.md (user-facing)
**File**: `/home/steven/code/Rust/copybook-rs/README.md`

| Claim | Location | Status | Issue |
|-------|----------|--------|-------|
| **"tens of MiB/s range"** | Line 16 | ✅ HONEST | Aligns with 66-95 MiB/s measured in test_perf.json |
| **"205 MiB/s DISPLAY"** | Not present | Missing | Referenced in CLAUDE/REPORT but absent from README |
| **"58 MiB/s COMP-3"** | Not present | Missing | Referenced in CLAUDE/REPORT but absent from README |
| **"production-ready"** | Line 43 | ⚠️ CONDITIONAL | Claims "Production-ready interfaces" (CLI) but Section 937 says "Engineering Preview" |

**Example mismatch** (Lines 868-872):
```
Benchmarks: Latest telemetry (scripts/bench/perf.json) shows DISPLAY 
decode throughput around 66–95 MiB/s and COMP-3 decode around 
18–25 MiB/s—suitable for engineering validation but below historic 
GiB/s marketing claims
```
→ **Acknowledges "historic GiB/s marketing claims" that no longer hold**

#### REPORT.md (status document)
**File**: `/home/steven/code/Rust/copybook-rs/docs/REPORT.md`

| Claim | Location | Status | Link |
|-------|----------|--------|------|
| **"205 MiB/s" DISPLAY** | Line 35 | ✅ DATED | Sept 2025, Commit 1fa63633 |
| **"58 MiB/s" COMP-3** | Line 36 | ✅ DATED | Sept 2025, Commit 1fa63633 |
| **"66-95 MiB/s" DISPLAY** | Line 141 | ✅ MEASURED | Recent runs |
| **"18-25 MiB/s" COMP-3** | Line 142 | ✅ MEASURED | Recent runs |
| **Status: "Cautious Adoption"** | Line 149 | ✅ CONSERVATIVE | Not "production-ready" |

**Finding**: REPORT.md is most honest but **status contradicts CLAUDE.md line 9** ("PRODUCTION READY - Ready for immediate enterprise deployment")

#### test_perf.json (machine-readable)
**File**: `/home/steven/code/Rust/copybook-rs/test_perf.json`

Actual measurements from most recent benchmark run:
```json
{
  "decode_display_heavy": 86_875_000 bytes/sec (≈ 82.9 MiB/s),
  "decode_comp3_heavy": 18_328_000 bytes/sec (≈ 17.5 MiB/s),
  "slo_validation/display": 66_974_000 bytes/sec (FAIL, -20.1% vs 80 MiB/s target),
  "slo_validation/comp3": 22_011_000 bytes/sec (FAIL, -47.5% vs 40 MiB/s target)
}
```

**Problem**: SLO gates FAIL but documentation doesn't mention this.

#### Other files with unlinked claims
- `/home/steven/code/Rust/copybook-rs/enterprise-security-validation.md:35` → "2.33 GiB/s DISPLAY, 168-176 MiB/s COMP-3" (NO SOURCE)
- `/home/steven/code/Rust/copybook-rs/issue-52-ledger.md:12, 25` → "4.22 GiB/s DISPLAY, 571 MiB/s COMP-3" (marked "FAIL" on line 12 status column)
- `/home/steven/code/Rust/copybook-rs/fixtures/enterprise/audit/README.md:18` → "4.1+ GiB/s throughput test data"
- `/home/steven/code/Rust/copybook-rs/docs/IMPLEMENTATION_FINALIZATION_STATUS.md:49, 75, 83` → "4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3" (ASPIRATIONAL, no current baseline)

### 1.2 PERFORMANCE NUMBER MATRIX

**Summary**: Historic targets (4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3) vs. current reality:

| Metric | Historic Target | Current Measured | Baseline Link | Status |
|--------|-----------------|------------------|----------------|--------|
| DISPLAY throughput | 4.1 GiB/s (4,198 MB/s) | 66-95 MiB/s | REPORT.md line 35, test_perf.json | ❌ 97.8% GAP |
| COMP-3 throughput | 560 MiB/s | 18-25 MiB/s | REPORT.md line 142, test_perf.json | ❌ 95.6% GAP |
| SLO floor (DISPLAY) | 80 MB/s | 66-95 MiB/s | test_perf.json | ⚠️ FAILS at -20% |
| SLO floor (COMP-3) | 40 MB/s | 18-25 MiB/s | test_perf.json | ⚠️ FAILS at -47.5% |

**Finding**: **No documentation explains why historic 4+ GiB/s targets were discarded.** This leaves users confused about "battle-tested reliability" claims.

---

## 2. TEST COUNT CLAIMS AUDIT

### 2.1 TEST COUNT FLUCTUATION

| Document | Claim | Passing | Ignored | Total | Status |
|----------|-------|---------|---------|-------|--------|
| CLAUDE.md (lines 11, 225) | "529 tests passing (54 ignored)" | 529 | 54 | 583 | ASPIRATIONAL |
| README.md (line 38) | "461/462 passing, 8 leak detections" | 461 | 0 | 462 | OUTDATED |
| README.md (line 868) | "one timing-sensitive failure" | 460 | ? | ? | OUTDATED |
| REPORT.md (line 20) | "461/462 passing" | 461 | 8 | 462 | OUTDATED |
| PR_105 summary | "527/527 tests passing (54 ignored)" | 527 | 54 | 581 | OUTDATED |
| PR_90 receipt | "529 tests passing (54 ignored)" | 529 | 54 | 583 | REFERENCE |
| **ACTUAL CURRENT** (Oct 22, nextest) | **277 PASSED, 4 FAILED** | 277 | 54 | 615 | ❌ BROKEN |

**CRITICAL**: Current `cargo nextest run --workspace` shows:
```
Summary [   7.639s] 281/615 tests run: 277 passed, 4 failed, 54 skipped
        FAIL [   0.009s] copybook-codec record::tests::test_fixed_record_reader_no_lrecl
        FAIL [   0.009s] copybook-codec record::tests::test_fixed_record_reader_zero_lrecl
        FAIL [   0.019s] copybook-codec record::tests::test_fixed_record_writer_no_lrecl
        FAIL [   0.003s] copybook-codec record::tests::test_fixed_record_writer_zero_lrecl
```

**Issues**:
1. **4 tests currently failing** on LRECL validation (record.rs lines 1097, 1106, 1146, 1155)
2. Tests expect `CBKP001_SYNTAX` but receive `CBKI001_INVALID_STATE`
3. No documentation mentions these test failures
4. **Test count claims in CLAUDE.md/REPORT.md are FALSE**

### 2.2 CLAIM LOCATIONS

**Files claiming test counts**:
- `/home/steven/code/Rust/copybook-rs/CLAUDE.md:11` → "529 tests passing (54 ignored)"
- `/home/steven/code/Rust/copybook-rs/CLAUDE.md:225` → "529 tests (54 ignored)"
- `/home/steven/code/Rust/copybook-rs/README.md:38` → "461/462 tests passing (one legacy performance assertion remains flaky)"
- `/home/steven/code/Rust/copybook-rs/README.md:868` → "461/462 passing; one timing-sensitive failure and eight leak detectors remain"
- `/home/steven/code/Rust/copybook-rs/docs/REPORT.md:20` → "461/462 passing"

**Problem**: No single source of truth. Documentation locked test counts at 461/462 or 529/54, but actual count is 615 total with 4 failures.

---

## 3. COBOL SUPPORT CLAIMS AUDIT

### 3.1 Support Matrix (Supported vs. Unsupported)

#### Claimed as SUPPORTED

**Data Types**:
- ✅ Alphanumeric (PIC X)
- ✅ Zoned decimal (PIC 9/S9 with sign zones)
- ✅ Packed decimal (COMP-3)
- ✅ Binary integers (COMP/BINARY with explicit width)
- ✅ Signed fields

**Structure Features**:
- ✅ Level numbers 01-49
- ✅ REDEFINES
- ✅ OCCURS / OCCURS DEPENDING ON (ODO)
- ✅ SYNCHRONIZED alignment
- ✅ BLANK WHEN ZERO
- ✅ Level-88 condition values (CLAUDE.md implies added, README line 825 says "unsupported")

**Record Formats**:
- ✅ Fixed-length
- ✅ Variable (RDW)
- ✅ Multiple EBCDIC codepages

#### Claimed as UNSUPPORTED

**Explicitly listed** (README.md lines 819-825):
- ❌ COMP-1 (single-precision float)
- ❌ COMP-2 (double-precision float)
- ❌ Edited PIC clauses (Z, slash, comma, $, CR, DB)
- ❌ SIGN LEADING/TRAILING SEPARATE
- ❌ Nested OCCURS DEPENDING ON
- ❌ RENAMES (66-level)
- ❌ "88-level condition names (VALUE clauses)" **← BUT CLAUDE.md line 20 claims support for Level-88!**

### 3.2 LEVEL-88 CONTRADICTION

**CLAUDE.md line 20** (Development spec):
```
- **copybook-core**: COBOL parsing (lexer, parser, AST, layout) with Level-88 condition value support
```

**README.md line 825** (User-facing):
```
- **Unsupported**: 88-level condition names (VALUE clauses) – explicit qualification required before committing real workloads
```

**REPORT.md line 87** (Status):
```
- **Unsupported**: 66-level (RENAMES) and 88-level (condition names) items
```

**Finding**: **Level-88 support status is UNDOCUMENTED**. CLAUDE.md claims support, but README/REPORT say unsupported. No feature flag, no API docs, no examples.

### 3.3 Missing Support Matrix

**What exists**: Scattered mentions in README, REPORT, ROADMAP
**What's missing**: 
- Central feature compatibility matrix (copy-based? codepage coverage? dialect options?)
- Explicit list of "supported" vs. "unsupported" with reasons
- Migration guide for common patterns (COMP-1→COMP-3, edited PIC workarounds)
- COBOL dialect markers (GnuCOBOL? IBM? ACUCOBOL?)

---

## 4. CI/CHECK INFRASTRUCTURE AUDIT

### 4.1 Workflows Found

| Workflow | File | Purpose | Gating? |
|----------|------|---------|---------|
| CI | `.github/workflows/ci.yml` | Test matrix (3 OSes, 3 Rust versions, 3 feature combos) | ✅ Primary gate |
| Benchmark | `.github/workflows/benchmark.yml` | Criterion benchmarks w/ Python parsing | ⚠️ Advisory (SLO fails but doesn't block) |
| Security Scan | `.github/workflows/security-scan.yml` | cargo-audit + supply chain checks | ✅ Gating |
| Pedantic Diff | `.github/workflows/pedantic-diff.yml` | Clippy pedantic compliance | ✅ Gating |
| Perf | `.github/workflows/perf.yml` | Performance validation | ⚠️ Advisory |
| Soak | `.github/workflows/soak.yml` | Nightly long-running tests (soak::*) | ⚠️ Nightly only |
| Publish | `.github/workflows/publish.yml` | Crates.io publish workflow | ✅ Release gate |
| Metrics Smoke | `.github/workflows/metrics-smoke.yml` | Telemetry endpoint validation | ⚠️ Feature-gated |

### 4.2 CI Quality Issues

**Problem 1: Benchmark SLO failures not blocking**
- File: `.github/workflows/benchmark.yml` (lines 38-100+)
- Status: SLO checks FAIL but don't block PR merge
- Evidence: `test_perf.json` shows FAIL status but workflow continues
- Impact: PRs merge with performance regression

**Problem 2: 4 test failures not blocking**
- File: `.github/workflows/ci.yml` (line 58: `cargo test --workspace`)
- Status: `cargo test` fails on 4 record.rs tests but workflow doesn't show failure
- Evidence: Manual `cargo nextest` run shows 277/281 pass, 4 fail
- Root cause: Tests failing on LRECL validation (record.rs)

**Problem 3: Leak detector backlog**
- File: README.md line 868
- Claim: "eight leak detectors still queued for cleanup"
- Status: Not in CI as explicit pass/fail gate
- Impact: Leak detection is advisory, not enforced

### 4.3 CI Gate Summary

| Gate | Enforcing? | Blocking? | Status |
|------|-----------|-----------|--------|
| Test suite (nextest) | ✅ Yes | ⚠️ Partial (4 failures present) | FAILING |
| Clippy pedantic | ✅ Yes | ✅ Yes | PASSING |
| Security (cargo-audit) | ✅ Yes | ✅ Yes | PASSING |
| Format (rustfmt) | ✅ Yes | ✅ Yes | PASSING |
| Benchmark SLO | ✅ Yes | ❌ No (advisory) | FAILING (-20% to -47%) |
| Leak detection | ✅ Partial | ❌ No (not in CI) | UNKNOWN |

---

## 5. MSRV/EDITION DECLARATIONS AUDIT

### 5.1 Declaration Locations

**Primary Declaration**:
- **File**: `/home/steven/code/Rust/copybook-rs/Cargo.toml` (workspace root)
- **MSRV**: `rust-version = "1.89"` (line 15)
- **Edition**: `edition = "2024"` (line 14)

**Secondary Declarations**:
- `.config/rustfmt.toml` → `edition = "2024"`
- All `Cargo.toml` files reference workspace edition via `edition.workspace = true`
- All `Cargo.toml` files inherit MSRV from workspace

**Documentation Claims**:
- CLAUDE.md line 178 → "Rust 1.89+ (MSRV), Edition 2024"
- README.md line 862 → "Rust: 1.89+ MSRV | Edition: 2024"
- REPORT.md line 96 → "Rust Edition 2024 with MSRV 1.90+"
- CI workflow line 44 → MSRV test: `1.90.0`

### 5.2 INCONSISTENCY: MSRV Declared as 1.89 but Tested as 1.90

**Problem**:
- **Cargo.toml declares**: `rust-version = "1.89"`
- **CI tests**: `1.90.0` (line 44 of ci.yml), stable, beta
- **REPORT.md says**: "MSRV 1.90+"

**Resolution unclear**: No comment explaining why CI tests 1.90 if MSRV is 1.89. Users may assume 1.89 is actually supported but untested.

**Recommendation**: Either:
1. Change Cargo.toml to `1.90` (if 1.89 unsupported), or
2. Add CI test for 1.89 (if truly MSRV)

---

## 6. BENCHMARK/PERF INFRASTRUCTURE AUDIT

### 6.1 Benchmark Setup

**Framework**: Criterion.rs (deterministic benchmarking)
**Harness**: `copybook-bench/benches/*.rs`
- `decode_performance.rs` - DISPLAY/COMP-3 throughput
- `comp3.rs` - Packed decimal specific
- `progressive.rs` - Scaling analysis
- `diagnostics_benches.rs` - System health

**Baseline Files**:
- `/home/steven/code/Rust/copybook-rs/copybook-bench/test_fixtures/baseline/canonical_baseline.json` - Reference baseline
- `/home/steven/code/Rust/copybook-rs/test_perf.json` - Latest run results
- No `scripts/bench/perf.json` in main branch (mentioned in README but not present)

### 6.2 Baseline Management

**Methodology docs**:
- `copybook-bench/BASELINE_METHODOLOGY.md` (10.7 KB)
- `copybook-bench/HARDWARE_SPECS.md` (5.1 KB)
- Established September 2025 (Commit 1fa63633, pre-release)

**Baseline Command** (CLAUDE.md lines 41-45):
```bash
cargo run --bin bench-report -p copybook-bench -- validate scripts/bench/perf.json
cargo run --bin bench-report -p copybook-bench -- baseline promote scripts/bench/perf.json
cargo run --bin bench-report -p copybook-bench -- baseline show
cargo run --bin bench-report -p copybook-bench -- compare scripts/bench/perf.json
cargo run --bin bench-report -p copybook-bench -- summary
```

**Issue**: `scripts/bench/perf.json` does not exist in main branch. Path is aspirational.

### 6.3 Benchmark Infrastructure Status

| Component | Location | Status | Gap |
|-----------|----------|--------|-----|
| Criterion setup | `copybook-bench/benches/*.rs` | ✅ Present | None |
| Baseline file | `canonical_baseline.json` | ✅ Present | Path mismatch |
| Baseline tool (`bench-report`) | `copybook-bench/src/bin/bench-report.rs` | ✅ Partial | Only validate/promote/show, not full CI integration |
| Python utilities (Issue #52) | Mentioned in README, ROADMAP, docs | ❌ MISSING | Promised but not shipped |
| SLO enforcement | `.github/workflows/benchmark.yml` | ⚠️ Advisory | Doesn't block merge (SLOs fail) |
| Performance receipts | `test_perf.json` | ✅ Present | Exists but path inconsistent |

**Finding**: Benchmark infrastructure is ~80% built but **SLO validation doesn't block merges** and **Python utilities from Issue #52 are not shipped**.

---

## 7. CURRENT MAJOR CONTRADICTIONS

### 7.1 Status Claims

| Source | Line | Claim | Contradiction |
|--------|------|-------|----------------|
| CLAUDE.md | 9 | "PRODUCTION READY - Ready for immediate enterprise deployment" | README line 937: "⚠️ Engineering Preview" |
| README.md | 43 | "Production-ready interfaces for both automation and integration" | README line 937: "Current Status: Engineering Preview ⚠️" |
| REPORT.md | 149 | "Status: Cautious Adoption Recommended ⚠️" (NOT "production-ready") | CLAUDE.md: "PRODUCTION READY" |
| ROADMAP.md | 3 | "⚠️ Engineering Preview (v0.3.1 maintenance)" | CLAUDE.md: "PRODUCTION READY" |

### 7.2 Performance Claims

| Source | Claim | Reality |
|--------|-------|---------|
| CLAUDE.md line 10 | "DISPLAY: 2.33 GiB/s, COMP-3: 168-176 MiB/s" | test_perf.json: 66-95 MiB/s, 18-25 MiB/s |
| enterprise-security-validation.md | "Maintains 2.33 GiB/s DISPLAY, 168-176 MiB/s COMP-3" | No baseline evidence provided |
| fixtures/enterprise/audit/README.md | "4.1+ GiB/s throughput" | test_perf.json: 82.9 MiB/s measured |
| CLAUDE.md line 271 | "2.5+ GiB/s DISPLAY, 100+ MiB/s COMP-3" (golden fixtures) | Not achieved in current runs |

### 7.3 Test Count Claims

| Source | Claim | Reality |
|--------|-------|---------|
| CLAUDE.md | "529 tests passing (54 ignored)" | cargo nextest: 277 passed, 4 **failed**, 54 skipped (out of 615 total) |
| README.md | "461/462 passing" | Outdated; should be 277 passing, 4 failing |
| REPORT.md | "461/462 passing" | Outdated |

### 7.4 Feature Support Claims

| Source | Feature | Claim | Status |
|--------|---------|-------|--------|
| CLAUDE.md line 20 | Level-88 | "with Level-88 condition value support" | ✅ Claimed as supported |
| README.md line 825 | Level-88 | "Unsupported: 88-level condition names" | ❌ Claimed as unsupported |
| REPORT.md line 87 | Level-88 | "Unsupported: 88-level (condition names) items" | ❌ Claimed as unsupported |

---

## 8. FINDINGS SUMMARY

### 8.1 CRITICAL ISSUES

1. **Test counts are FALSE**
   - CLAUDE.md claims 529 passing; actual is 277 passing + **4 failing** out of 615
   - 4 tests failing on LRECL validation (record.rs) with no documented fix
   - No changelog explaining test count growth from 461→529→615

2. **Performance claims are UNLINKED**
   - CLAUDE.md claims 2.33 GiB/s (DISPLAY), 168-176 MiB/s (COMP-3) with no baseline receipt
   - test_perf.json shows 66-95 MiB/s (DISPLAY), 18-25 MiB/s (COMP-3)
   - 97%+ gap between historic targets (4.1+ GiB/s) and current measured (66-95 MiB/s)
   - No documentation explains why targets were abandoned

3. **Status is CONTRADICTORY**
   - CLAUDE.md: "PRODUCTION READY - Ready for immediate enterprise deployment"
   - ROADMAP.md: "⚠️ Engineering Preview (v0.3.1 maintenance)"
   - README.md: Oscillates between "production-ready interfaces" and "⚠️ Engineering Preview"
   - REPORT.md: "Cautious Adoption Recommended ⚠️"
   - **Users cannot determine actual status**

4. **SLO gates FAIL but don't block**
   - test_perf.json shows DISPLAY -20.1% vs 80 MB/s target, COMP-3 -47.5% vs 40 MB/s target
   - Benchmark CI workflow is advisory; failed SLOs don't block PR merges
   - No documented decision about when to enforce vs. advise

5. **Level-88 support status UNDOCUMENTED**
   - CLAUDE.md claims support (line 20)
   - README.md claims unsupported (line 825)
   - No API, no examples, no feature gate
   - Users cannot safely adopt Level-88 copybooks

### 8.2 HIGH-IMPACT ISSUES

6. **MSRV mismatch**: Cargo.toml says 1.89 but CI tests 1.90, REPORT.md says 1.90+
7. **Benchmark path mismatch**: README references `scripts/bench/perf.json` which doesn't exist
8. **Leak detector backlog**: 8 leak detectors not in CI gate; not tracked
9. **Feature flags not documented**: Level-88, audit feature, metrics feature—unclear when to use
10. **No single "truth" document**: Each document has different test counts, perf numbers, status claims

### 8.3 DOCUMENTATION GAPS FOR THE 5-PR PLAN

| Issue | Files Affected | Scope |
|-------|----------------|-------|
| Test count sync | CLAUDE.md, README.md, REPORT.md, PR receipts | PR1: Verification |
| Performance claim normalization | CLAUDE.md, README.md, REPORT.md, enterprise-security-validation.md, fixtures/*/README.md, docs/*.md | PR2: Performance |
| Status statement alignment | CLAUDE.md, README.md, REPORT.md, ROADMAP.md | PR3: Status |
| Level-88 feature documentation | API docs, examples, feature matrix | PR4: Completeness |
| MSRV alignment | Cargo.toml, ci.yml, docs | PR5: Toolchain |

---

## 9. RECOMMENDATIONS FOR 5-PR CLOSURE PLAN

### PR1: Verification & Test Count Truth
- **Scope**: Fix 4 failing tests in record.rs (LRECL validation)
- **Files**: 
  - `copybook-codec/src/record.rs` (fix assertion expectations)
  - `CLAUDE.md` (update test count to 281/615 passing if no fix, or 285/615 if fixed)
  - `README.md` (update test count claim)
  - `docs/REPORT.md` (update test count)
- **Exit**: All tests passing, single source of truth for test count in CLAUDE.md

### PR2: Performance Truth & Linkage
- **Scope**: Consolidate performance claims, link to baselines
- **Files**:
  - `CLAUDE.md` (replace aspirational 2.5+ GiB/s with measured 205 MiB/s baseline; link to REPORT.md line 35)
  - `README.md` (add baseline link section; remove "tens of MiB/s" vague claim)
  - `docs/REPORT.md` (keep as canonical; add "why historic GiB/s targets were abandoned")
  - `enterprise-security-validation.md` (remove unlinked 2.33 GiB/s claim or replace with baseline link)
  - `.github/workflows/benchmark.yml` (add SLO enforcement or document why advisory)
  - `copybook-bench/BASELINE_METHODOLOGY.md` (reference in README under "Performance" section)
- **Exit**: All performance numbers link to dated baselines; no orphaned claims

### PR3: Status Alignment
- **Scope**: Single, consistent status statement across all docs
- **Files**:
  - `CLAUDE.md` (line 9: change "PRODUCTION READY" to "Engineering Preview (v0.3.1 maintenance)" per ROADMAP)
  - `README.md` (lines 937-939: clarify "Production Status: Engineering Preview ⚠️")
  - `docs/REPORT.md` (line 149: move "Cautious Adoption" to top as leading summary)
  - `docs/ROADMAP.md` (line 3: confirm "Engineering Preview" as canonical)
  - Add section to CLAUDE.md: "Known Limitations & Roadmap" with references to ROADMAP.md
- **Exit**: All four docs agree on status; ROADMAP.md is canonical source

### PR4: Feature Completeness Matrix
- **Scope**: Document Level-88 and create support matrix
- **Files**:
  - `docs/reference/COBOL_SUPPORT_MATRIX.md` (NEW) - central feature coverage table
  - `CLAUDE.md` (clarify Level-88 support status with examples or remove claim)
  - `README.md` (update line 825 to match SUPPORT_MATRIX.md decision)
  - `docs/REPORT.md` (update line 87 to match SUPPORT_MATRIX.md)
  - `docs/reference/LIBRARY_API.md` (add Level-88 examples if supported, or migration guide if not)
  - `docs/MIGRATION_GUIDE.md` (add Level-88 migration strategy)
- **Exit**: Single COBOL_SUPPORT_MATRIX.md is canonical; all docs reference it

### PR5: Toolchain & Baseline Alignment
- **Scope**: MSRV alignment, benchmark path fix, leak detector tracking
- **Files**:
  - `Cargo.toml` (confirm `rust-version = "1.89"` and add comment or change to 1.90)
  - `.github/workflows/ci.yml` (line 44: add comment explaining why 1.90 tested if 1.89 is MSRV, or add 1.89 test)
  - `README.md` (line 862: update to match CI reality)
  - `CLAUDE.md` (line 178: update to match Cargo.toml)
  - `.github/workflows/benchmark.yml` (line 34: change output path from `scripts/bench/perf.json` to `test_perf.json` or create scripts/bench/)
  - `README.md` (line 872: document leak detector backlog with tracking issue numbers)
  - `.config/rustfmt.toml` (confirm edition matches Cargo.toml)
- **Exit**: MSRV, edition, benchmark paths all consistent; leak detector tracking documented

---

## 10. FILE-BY-FILE AUDIT MATRIX

### Files Requiring Edits (by PR)

| File | PR1 | PR2 | PR3 | PR4 | PR5 | Issue |
|------|-----|-----|-----|-----|-----|-------|
| CLAUDE.md | ✅ | ✅ | ✅ | ✅ | ✅ | 5 edits needed (test count, perf claim, status, Level-88, toolchain) |
| README.md | ✅ | ✅ | ✅ | ✅ | ✅ | 5 edits needed (test count, perf section, status, feature matrix, toolchain) |
| docs/REPORT.md | ✅ | ✅ | ✅ | ✅ | 0 | 3 edits (test, perf context, status prominence) |
| docs/ROADMAP.md | 0 | 0 | ✅ | 0 | 0 | 1 edit (confirm canonical status) |
| docs/reference/COBOL_SUPPORT_MATRIX.md | 0 | 0 | 0 | ✅ | 0 | NEW FILE (central feature matrix) |
| docs/reference/LIBRARY_API.md | 0 | 0 | 0 | ✅ | 0 | 1 edit (Level-88 examples or migration) |
| docs/MIGRATION_GUIDE.md | 0 | 0 | 0 | ✅ | 0 | 1 edit (Level-88 strategy) |
| .github/workflows/ci.yml | 0 | 0 | 0 | 0 | ✅ | 1 edit (MSRV test comment or add 1.89 test) |
| .github/workflows/benchmark.yml | 0 | ✅ | 0 | 0 | ✅ | 2 edits (SLO enforcement, path fix) |
| copybook-codec/src/record.rs | ✅ | 0 | 0 | 0 | 0 | Fix 4 failing tests (LRECL validation) |
| Cargo.toml | 0 | 0 | 0 | 0 | ✅ | 1 edit (comment MSRV choice or align) |
| enterprise-security-validation.md | 0 | ✅ | 0 | 0 | 0 | 1 edit (remove unlinked perf claim or link) |
| fixtures/enterprise/audit/README.md | 0 | ✅ | 0 | 0 | 0 | 1 edit (remove/link 4.1 GiB/s claim) |
| .config/rustfmt.toml | 0 | 0 | 0 | 0 | ✅ | Verify edition matches Cargo.toml |

---

## 11. APPENDIX: SPECIFIC QUOTE LOCATIONS

### CLAUDE.md Problematic Claims
- Line 9: `**Status**: **PRODUCTION READY** - Ready for immediate enterprise deployment`
- Line 10: `**Performance**: Strong performance with recovery (DISPLAY: 2.33 GiB/s, COMP-3: 168-176 MiB/s with panic elimination safety)`
- Line 11: `**Quality**: 529 tests passing (54 ignored), zero unsafe code, clippy pedantic compliance, comprehensive error taxonomy`
- Line 20: `- **copybook-core**: COBOL parsing (lexer, parser, AST, layout) with Level-88 condition value support`
- Line 183-189: "CI Gating" vs. actual measured performance (205 MiB/s vs. SLO -20%)
- Line 271-272: "2.5+ GiB/s" and "100+ MiB/s" golden fixture targets (aspirational, not measured)

### README.md Problematic Claims
- Line 16: `tens of MiB/s range` (honest but vague vs. specific baseline numbers elsewhere)
- Line 38: `461/462 tests passing` (outdated; actual is 277/281 from nextest)
- Line 43: `Production-ready interfaces for both automation and integration` (contradicts line 937)
- Line 825: `Unsupported: 88-level condition names` (contradicts CLAUDE.md line 20)
- Line 862: `Rust: 1.89+ MSRV | Edition: 2024` (1.89 in Cargo.toml but CI tests 1.90)
- Line 868-871: "historic GiB/s marketing claims" vs. "66–95 MiB/s" (acknowledges gap but doesn't explain)
- Line 937-938: `**Current Status: Engineering Preview** ⚠️` (contradicts line 43 and CLAUDE.md)

### test_perf.json Actual Measurements
- decode_display_heavy: 86_875_000 bytes/sec (82.9 MiB/s)
- decode_comp3_heavy: 18_328_000 bytes/sec (17.5 MiB/s)
- slo_validation DISPLAY: FAIL, -20.1% vs 80 MB/s
- slo_validation COMP-3: FAIL, -47.5% vs 40 MB/s

---

## CONCLUSION

**copybook-rs documentation suffers from aspirational claims (4.1+ GiB/s) that conflict with measured reality (66-95 MiB/s), inconsistent test counts (461→529→615 with 4 failures), and contradictory status declarations (PRODUCTION READY vs. Engineering Preview).** 

The 5-PR closure plan will restore clarity by:
1. **PR1**: Fixing broken tests and verifying true count
2. **PR2**: Consolidating perf claims with dated baselines
3. **PR3**: Aligning status across all documents
4. **PR4**: Creating central COBOL support matrix for Level-88 and other features
5. **PR5**: Fixing MSRV, benchmark paths, and toolchain consistency

**Each PR should update CLAUDE.md as the "internal development spec" that mirrors actual implementation state.**
