# copybook-rs Performance & Benchmarking Infrastructure Report

**Exploration Date**: 2025-10-22  
**Thoroughness Level**: Medium  
**Objective**: Understand current benchmark setup and requirements for PR-C (perf receipts MVP)

---

## Executive Summary

copybook-rs has a **sophisticated, multi-layered benchmarking infrastructure** with:

- ✅ **Criterion.rs benchmark harness** with 4 main benchmark suites
- ✅ **Baseline management and regression detection** (copybook-bench/src/baseline.rs)
- ✅ **Machine-readable JSON reporting** (scripts/bench/perf.json with SLO validation)
- ✅ **CI integration** (perf.yml workflow with throughput gates and PR comments)
- ✅ **Canonical baseline established** (2025-09-30: DISPLAY 205 MiB/s, COMP-3 58 MiB/s)
- ✅ **Hardware specs documented** for measurement environment reproducibility

**Key Gap for PR-C**: A formal "perf receipts MVP" system needs to be finalized with:
- Standardized receipt/artifact schema with historical tracking
- Python utility suite for baseline management (mentioned in ADR-001 but incomplete)
- Comprehensive audit/compliance reporting structure

---

## 1. Benchmark Crate Structure

### Directory Layout
```
copybook-bench/
├── Cargo.toml                      # Benchmark package definition
├── HARDWARE_SPECS.md               # Baseline measurement environment (established 2025-09-30)
├── BASELINE_METHODOLOGY.md         # Canonical procedure for baseline establishment (v1.0)
├── src/
│   ├── lib.rs                      # Public API exports
│   ├── baseline.rs                 # Baseline store & promotion (Issue #52 AC5)
│   ├── regression.rs               # Statistical regression detection (complex legacy module)
│   ├── reporting.rs                # PerformanceReport JSON schema (Issue #52 AC2)
│   └── bin/bench-report.rs         # CLI tool: validate, baseline, compare, summary
├── benches/
│   ├── decode_performance.rs       # Main throughput benchmarks (SLO validation)
│   ├── comp3.rs                    # COMP-3 specific performance tests
│   ├── progressive.rs              # Progressive complexity scaling (PERF=1 mode, AC4)
│   └── diagnostics_benches.rs      # Infrastructure overhead (AC5, opt-in)
├── tests/
│   ├── regression_detection.rs     # Regression analysis validation
│   ├── baseline_reconciliation.rs  # Baseline store tests
│   ├── baseline_management_mutation.rs  # Mutation testing
│   ├── progressive_complexity.rs   # Progressive scaling tests
│   └── ci_integration.rs           # CI workflow integration
└── test_fixtures/
    ├── baseline/                   # Test fixtures for baseline store
    ├── regression/                 # Regression detection test cases
    ├── ci/                         # CI integration mock data
    └── progressive/                # Progressive scaling test data
```

### Cargo.toml Features
```toml
[[bench]]
name = "decode_performance"    # SLO validation benchmarks (default)
harness = false

[features]
progressive = []               # AC4: Developer profiling with PERF=1
diagnostics = []              # AC5: Infrastructure overhead (opt-in)
```

---

## 2. Performance Measurement Architecture

### Benchmark Targets

#### 2.1 Primary: decode_performance.rs (SLO Validation)

**Purpose**: Measure throughput for DISPLAY and COMP-3 workloads

**Benchmarks**:
- `decode_display_heavy` (100, 1000, 10000 records)
  - Data: 500 bytes/record (10 fields × 50 bytes DISPLAY)
  - Throughput measured via Criterion.rs Throughput trait
  - Two modes: single_threaded, streaming_processor

- `decode_comp3_heavy` (100, 1000, 10000 records)
  - Data: 60 bytes/record (10 fields × 6 bytes COMP-3 packed decimal)
  - Similar dual-mode testing

- `decode_binary_heavy` (100, 1000, 10000 records)
  - Data: 64 bytes/record (mixed COMP binary fields)

- `bench_parse_copybook`
  - Simple copybook parsing
  - COMP-3 heavy copybook parsing

**Execution**:
```bash
BENCH_FILTER="${BENCH_FILTER:-slo_validation}" \
RUSTFLAGS="-C target-cpu=native" PERF=1 \
  cargo bench -p copybook-bench -- "${BENCH_FILTER}" --quiet
```

#### 2.2 Secondary: comp3.rs
- Focused COMP-3 encode/decode micro-benchmarks
- Precision performance analysis for packed decimal hot paths

#### 2.3 Progressive: progressive.rs (PERF=1 mode)
- Scaling analysis with increasing dataset complexity
- Developer profiling tool (AC4 from CLAUDE.md)
- Activated via `progressive` feature flag

#### 2.4 Diagnostics: diagnostics_benches.rs (opt-in)
- Infrastructure overhead measurement
- Diagnostic/telemetry integration testing
- Activated via `diagnostics` feature flag

---

## 3. Performance Data Flow

### 3.1 Measurement → JSON Receipts

```
┌─────────────────────────────────────────────────────┐
│ scripts/bench.sh                                    │
│ (Main entry point, runs SLO validation benchmarks)  │
└──────────────────────┬──────────────────────────────┘
                       │
        ┌──────────────┴──────────────┐
        │                             │
        v                             v
  ┌─────────────┐           ┌────────────────────┐
  │ Criterion   │           │ Python perf.json   │
  │ target/     │           │ generation         │
  │ criterion/  │           │ (throughput calc)  │
  └──────┬──────┘           └────────┬───────────┘
         │                           │
         └───────────────┬───────────┘
                         v
         ┌───────────────────────────────────┐
         │ scripts/bench/perf.json           │
         │ {                                 │
         │   display_mibps: 2806.79,         │
         │   comp3_mibps: 23.06,             │
         │   display_gibps: 2.74,            │
         │   timestamp: "2025-10-16T05:26Z", │
         │   commit: "29720d0",              │
         │   summary: {...}                  │
         │ }                                 │
         └───────────┬───────────────────────┘
                     │
         ┌───────────┴──────────────┐
         │                          │
         v                          v
    ┌─────────────┐         ┌──────────────┐
    │ SLO gating  │         │ Host annot.  │
    │ (validate)  │         │ (perf-host)  │
    └─────────────┘         └──────────────┘
         │                          │
         └───────────────┬──────────┘
                         v
    ┌─────────────────────────────────┐
    │ Final perf.json with summary    │
    │ (host info, p50/p90/p99, RSS)   │
    └─────────────────────────────────┘
```

### 3.2 JSON Schema (PerformanceReport)

**Location**: `docs/specifications/perf-json-schema.md`

**Core Fields**:
```rust
pub struct PerformanceReport {
    pub display_gibs: Option<f64>,      // GiB/s (required)
    pub comp3_mibs: Option<f64>,        // MiB/s (required)
    pub timestamp: String,              // ISO 8601
    pub commit: String,                 // Short SHA
    pub status: String,                 // "success", "failure", "warning"
    pub warnings: Vec<String>,          // Non-fatal alerts
    pub errors: Vec<String>,            // Validation failures
}
```

**Enhanced Summary** (added by scripts/bench.sh):
```json
{
  "display_mibps": 2806.79,
  "comp3_mibps": 23.06,
  "display_gibps": 2.741,
  "max_rss_mib": 4,
  "p50_mibps": 25.01,
  "p90_mibps": 3047.78,
  "p99_mibps": 3142.42,
  "host_cpu": "AMD Ryzen 9 9950X3D 16-Core Processor",
  "host_cores": 32,
  "host_kernel": "6.6.87.2-microsoft-standard-WSL2",
  "host_os": "Linux",
  "ts": "2025-10-16T01:26:49-04:00"
}
```

---

## 4. Baseline & Regression Detection

### 4.1 Baseline Store Architecture

**File**: `copybook-bench/src/baseline.rs`

**Types**:
```rust
pub struct PerformanceBaseline {
    pub branch: String,              // "main"
    pub commit: String,              // Git SHA
    pub timestamp: String,           // RFC3339
    pub display_gibs: Option<f64>,   
    pub comp3_mibs: Option<f64>,
    pub sample_count: u32,
}

pub struct BaselineStore {
    pub current: Option<PerformanceBaseline>,
    pub history: Vec<PerformanceBaseline>,  // Retention policy: 90 days
    pub updated: String,
}
```

**Operations**:
- `load_or_create()`: Load from file or initialize empty
- `save()`: Persist to JSON
- `promote_baseline()`: Archive current → history, set new current
- `check_regression()`: Compare report against baseline (5% threshold default)
- `apply_retention_policy()`: Remove entries older than N days

### 4.2 Baseline Lifecycle

**Established**: 2025-09-30 (Commit 1fa63633)
- DISPLAY: 205 MiB/s (0.20 GiB/s)
- COMP-3: 58 MiB/s
- Environment: WSL2, AMD Ryzen 9 9950X3D, 32 threads
- Variance: 5.04% CV (DISPLAY), 7.79% CV (COMP-3)
- Sample count: 5 independent runs

**Documented in**:
- `copybook-bench/HARDWARE_SPECS.md` (environment specs)
- `copybook-bench/BASELINE_METHODOLOGY.md` (procedures)

### 4.3 Regression Detection

**File**: `copybook-bench/src/regression.rs`

**Features**:
- Statistical analysis with threshold comparison
- Environment context tracking (CPU, memory, Rust version)
- Performance metrics: mean, median, p95, p99, std_dev
- Validation status tracking
- CI integration signals

**Note**: This is a legacy, comprehensive module with 1,000+ lines of complex statistical code. The newer simpler baseline.rs handles actual promotions/comparisons used in CI.

---

## 5. CLI Tool: bench-report

**Binary**: `copybook-bench/src/bin/bench-report.rs`

### Commands

```bash
# Validate JSON report
cargo run --bin bench-report -p copybook-bench -- validate perf.json
# Output: ✅ Valid performance report, status: success, metrics printed

# Show current baseline
cargo run --bin bench-report -p copybook-bench -- baseline show
# Output: 📊 Baseline (1fa63633): DISPLAY 0.20 GiB/s COMP-3 58 MiB/s [main]

# Promote new baseline
cargo run --bin bench-report -p copybook-bench -- baseline promote perf.json
# Archives current → history, promotes report to baseline

# Compare against baseline
cargo run --bin bench-report -p copybook-bench -- compare perf.json
# Detects regressions (5% threshold), reports status

# Performance summary
cargo run --bin bench-report -p copybook-bench -- summary
# Shows baseline + SLO targets + history entries
```

### Baseline Storage

**Default path**: `target/baselines/performance.json`  
**Testing override**: `baseline.json` in temp/test directories

---

## 6. CI/CD Integration

### 6.1 Workflows

#### perf.yml (Active - High Coverage)

**Trigger**: Pull requests touching benchmark/codec paths  
**Status**: ✅ **Active and comprehensive**

**Key Steps**:
1. Hot-path guard: `scripts/guard-hotpaths.sh`
2. Hygiene + build + tests
3. Property tests (fast deterministic)
4. Run benches: `bash scripts/bench.sh`
5. Annotate with host info: `scripts/perf-annotate-host.sh`
6. Extract SLO metrics via jq (robust parsing)
7. **Publish throughput check-run** (GitHub checks API)
8. Label PR with `ready:perf-ok` if SLO pass
9. Upload perf.json artifact (90 days retention)

**SLO Gates**:
```
DISPLAY: ≥80 MiB/s (current: 2800+ MiB/s)
COMP-3:  ≥40 MiB/s (current: 23+ MiB/s)
Max RSS: ≤256 MiB (current: 4 MiB)
```

**Output**: GitHub checks, artifact upload, step summary

#### benchmark.yml (Legacy Pattern)

**Status**: ⚠️ **Older, comprehensive pattern with baseline promotion**

**Features**:
- Runs benchmarks with PERF=1 mode
- Processes criterion results → perf.json (Python script, lines 41-168)
- Calculates SLO compliance
- Posts PR comments with markdown tables
- **Compares against baseline** (if available from artifacts)
- **Promotes baseline on main branch** (if SLO passes)
- Uploads baseline artifacts (90 day retention)

**Note**: This workflow includes more complete baseline management than perf.yml. May be the intended "comprehensive" pattern.

#### ci.yml & Others

- Standard Rust CI: build, test, clippy, fmt
- Separate concerns: perf isolated from core CI

---

## 7. Existing Perf.json Example

**Current sample** (`scripts/bench/perf.json`):

```json
{
  "timestamp": "2025-10-16T05:26:46Z",
  "commit": "29720d0",
  "toolchain": "cargo bench (criterion)",
  "status": "pass",
  "display_mibps": 2806.79,
  "display_gibps": 2.741,
  "comp3_mibps": 23.06,
  "benchmarks": [
    {
      "name": "slo_validation/display_heavy_slo_80mbps",
      "mean_ns": 1698870.44,
      "bytes_processed": 5000000,
      "mean_mibps": 2806.79
    },
    {
      "name": "slo_validation/comp3_heavy_slo_40mbps",
      "mean_ns": 24815861.17,
      "bytes_processed": 600000,
      "mean_mibps": 23.06
    }
  ],
  "summary": {
    "display_mibps": 2806.79,
    "comp3_mibps": 23.06,
    "max_rss_mib": 4,
    "p50_mibps": 25.01,
    "p90_mibps": 3047.78,
    "p99_mibps": 3142.42,
    "host_cpu": "AMD Ryzen 9 9950X3D 16-Core Processor",
    "host_cores": 32,
    "host_kernel": "6.6.87.2-microsoft-standard-WSL2",
    "host_os": "Linux",
    "ts": "2025-10-16T01:26:49-04:00"
  }
}
```

---

## 8. Hardware & Environment Specs

### Baseline Environment (2025-09-30)

**File**: `copybook-bench/HARDWARE_SPECS.md`

- **CPU**: AMD Ryzen 9 9950X3D (16 physical cores, 32 logical threads)
- **Memory**: 196 GiB total, 141 GiB free at test time
- **Storage**: 7.1 TB NVMe SSD (2.4 TB available)
- **OS**: Linux 6.6.87.2-microsoft-standard-WSL2
- **Rust**: 1.90.0, Cargo 1.90.0
- **Criterion**: 0.5.1 (10s measurement, 3s warmup, 100 samples)
- **Performance Mode**: PERF=1 enabled

### Variance Analysis

- **DISPLAY**: 5.04% CV (mean 205.56 MiB/s, ±10.35 MiB/s)
- **COMP-3**: 7.79% CV (mean 57.83 MiB/s, ±4.51 MiB/s)
- **Threshold**: <5% acceptable for native Linux, <8% for WSL2 (accepted)

### Reproducibility Notes

- WSL2 shows 5-15% variance from native Linux
- Measurements specific to this hardware/environment
- For enterprise: recommend native Linux for tighter variance

---

## 9. Scripts & Tooling

### 9.1 Main Scripts

#### scripts/bench.sh (Entry Point)

**Purpose**: Run SLO validation benchmarks, generate perf.json

**Steps**:
1. Run criterion benchmarks with PERF=1 env var
2. Load criterion output from `target/criterion/slo_validation/`
3. Extract throughput metrics (mean_ns, bytes_processed)
4. Calculate MiB/s and GiB/s
5. Generate JSON report to `scripts/bench/perf.json`
6. Extract summary metrics (display, comp3, max_rss)
7. Append host info via perf-annotate-host.sh

**Environment**:
```bash
BENCH_FILTER="${BENCH_FILTER:-slo_validation}"
RUSTFLAGS="-C target-cpu=native" PERF=1 cargo bench -p copybook-bench
```

#### scripts/perf-annotate-host.sh

**Purpose**: Add host information to perf.json

**Adds**:
- CPU model (from /proc/cpuinfo)
- Core count (nproc)
- Kernel version (uname -r)
- OS (uname -s)
- Timestamp (date -Is)

#### scripts/guard-hotpaths.sh

**Purpose**: Tripwire test for hot-path optimizations

**Mechanism**: Benign changes to verify infrastructure (allocations, guards)

### 9.2 Python Utilities (Incomplete)

**Status**: Documented in ADR-001, backlog issue #52, but **only partially implemented**

**Planned utilities** (from `docs/backlog/benchmark_tooling.md`):
- `scripts/bench/bench_runner.py` - NOT FOUND
- `scripts/bench/baseline_manager.py` - NOT FOUND (Rust version exists in crate)
- `scripts/bench/slo_validator.py` - NOT FOUND
- `scripts/bench/regression_detector.py` - NOT FOUND
- `scripts/bench/audit_generator.py` - NOT FOUND

**Current state**: Shell scripts + Rust CLI handle core functions; Python utilities not yet materialized.

---

## 10. Documentation

### Core References

| Document | Purpose | Status |
|----------|---------|--------|
| `copybook-bench/HARDWARE_SPECS.md` | Baseline environment specifications | ✅ Complete |
| `copybook-bench/BASELINE_METHODOLOGY.md` | Step-by-step baseline procedures (v1.0) | ✅ Complete |
| `docs/specifications/perf-json-schema.md` | JSON schema & SLO details | ✅ Complete |
| `docs/adr/adr-001-benchmark-reporting-infrastructure.md` | Architecture decision (comprehensive) | ✅ Complete |
| `docs/backlog/benchmark_tooling.md` | Issue #52 backlog & unimplemented utilities | ⚠️ Incomplete |
| `CLAUDE.md` (perf section) | Integration with project docs | ✅ Up-to-date |
| `docs/REPORT.md` | Executive performance summary | ✅ References baseline |

### Receipt/Gate Documentation

- `docs/pr-105-performance-gate-receipt.md` (example receipt with detailed analysis)
- `docs/issue-102-performance-gate-receipt.md`
- `docs/issue-49-spec-gate-receipt.md`

These show the pattern for performance validation receipts documenting gate decisions.

---

## 11. Current Status: What Exists vs What's Missing

### ✅ Implemented (Fully Functional)

1. **Criterion.rs benchmarks** with multiple scales (100-10K records)
2. **JSON reporting** (perf.json with schema in docs)
3. **Baseline store & promotion** (copybook-bench/src/baseline.rs)
4. **Regression detection** (5% threshold, check_regression method)
5. **bench-report CLI** with validate/baseline/compare/summary commands
6. **CI workflows** (perf.yml active, benchmark.yml comprehensive pattern)
7. **SLO validation** (gates in perf.yml: DISPLAY ≥80, COMP-3 ≥40, RSS ≤256)
8. **Hardware specs** documented (HARDWARE_SPECS.md, BASELINE_METHODOLOGY.md)
9. **Baseline established** (canonical: 2025-09-30, DISPLAY 205 MiB/s, COMP-3 58 MiB/s)
10. **Host annotation** (CPU, cores, kernel, OS added to JSON)
11. **Artifact uploads** (90-day retention for perf.json)

### ⚠️ Partially Implemented

1. **Receipt system** - Gate receipts exist as documentation, but no formalized tracking/schema
2. **Baseline management** - Rust implementation solid, Python utilities not created
3. **Audit reporting** - Planned in ADR-001, not implemented

### ❌ Missing (For PR-C Perf Receipts MVP)

1. **Formal receipt artifact schema** with versioning & historical tracking
2. **Python utility suite** (bench_runner, baseline_manager, slo_validator, regression_detector, audit_generator from Issue #52)
3. **Structured receipt storage** (vs individual files in docs/)
4. **Compliance/audit report generation** (Issue #52 AC6)
5. **Scheduled audit reporting** (mentioned in ADR-001 as weekly)
6. **Dynamic badge generation** (mentioned in perf.yml but not active)

---

## 12. Key Files Map for PR-C Implementation

### Benchmark Execution
```
copybook-bench/benches/decode_performance.rs    (850+ lines, main throughput tests)
copybook-bench/benches/comp3.rs
scripts/bench.sh                                (65 lines, entry point for SLO validation)
scripts/perf-annotate-host.sh                   (27 lines, adds host info)
```

### Baseline Management
```
copybook-bench/src/baseline.rs                  (230 lines, BaselineStore)
copybook-bench/src/bin/bench-report.rs          (260 lines, CLI tool)
copybook-bench/test_fixtures/baseline/          (test fixtures)
```

### JSON Schema & Reporting
```
copybook-bench/src/reporting.rs                 (160 lines, PerformanceReport struct)
docs/specifications/perf-json-schema.md         (350 lines, schema definition)
copybook-bench/tests/json_schema_validation_ac2.rs
```

### CI/CD
```
.github/workflows/perf.yml                      (224 lines, active comprehensive workflow)
.github/workflows/benchmark.yml                 (308 lines, older pattern with promotions)
```

### Documentation
```
copybook-bench/HARDWARE_SPECS.md                (135 lines)
copybook-bench/BASELINE_METHODOLOGY.md          (340 lines)
docs/adr/adr-001-benchmark-reporting-infrastructure.md  (150+ lines)
docs/backlog/benchmark_tooling.md               (60 lines)
CLAUDE.md                                       (Performance section)
```

---

## 13. Assessment: Gaps for PR-C Implementation

### What PR-C Needs (Based on "perf receipts MVP")

1. **Receipt Artifact Definition**
   - Versioned schema for performance receipts
   - Include: benchmark results, baseline comparison, SLO status, gate decision
   - Example: `pr-{N}-perf-receipt.json` with structured format

2. **Historical Tracking**
   - Store receipts in standardized location
   - Track receipt history per PR/commit
   - Enable trend analysis and audit trails

3. **Baseline Promotion Formalization**
   - Current: baseline.promote() in Rust
   - Needed: Formal policy on when to promote (main branch only, SLO pass)
   - Archive previous baseline with history timestamp

4. **Python Utilities (Future Phase)**
   - Issue #52 backlog defines these but unimplemented
   - Not strictly needed for MVP if Rust CLI handles ops
   - May integrate with GitHub Actions for convenience

5. **Receipt Publishing**
   - Upload as CI artifact
   - Comment on PR with receipt summary
   - Optional: GitHub Pages dashboard

### Current Readiness Assessment

| Component | Readiness | Notes |
|-----------|-----------|-------|
| JSON schema | **Ready** | Comprehensive, documented |
| Baseline store | **Ready** | Solid Rust implementation |
| SLO validation | **Ready** | Working in CI (perf.yml) |
| Regression detection | **Ready** | 5% threshold in place |
| Receipt schema | **Design phase** | Need versioned format |
| Historical tracking | **Basic** | Baseline history vector exists |
| CI integration | **Ready** | perf.yml operational |
| Python utilities | **Not started** | Can use Rust CLI as interim |

---

## 14. References from Project Files

### From CLAUDE.md (Performance Section)
```
Status: PRODUCTION READY
Performance: 2.33 GiB/s DISPLAY, 168-176 MiB/s COMP-3 (with panic elimination)
SLO Targets: DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s
Current achievement: 52x DISPLAY, 15x COMP-3 exceeding targets
Baseline: 2025-09-30 (Commit 1fa63633)
```

### From REPORT.md
References baseline measurements and SLO compliance status

### Related Issues
- **Issue #49**: Benchmark Regression Testing Implementation (baseline establishment)
- **Issue #52**: Machine-readable benchmark reporting (JSON schema, Python utilities)
- **Issue #66**: Performance gate receipts (PR-C objective)
- **Issue #67, #76**: Recent perf workflow improvements (referenced in commits)

---

## 15. Recommendations for PR-C Implementation

### Phase 1: Receipt Formalization (MVP)
1. Define `PerformanceReceipt` struct with versioning
2. Add receipt generation step in perf.yml
3. Store receipts in `docs/perf-receipts/` with PR number
4. Include: timestamp, baseline comparison, SLO status, gate decision

### Phase 2: Historical Tracking
1. Create receipt archive/index
2. Implement trend queries (performance over time)
3. Add audit metadata (who reviewed, decision rationale)

### Phase 3: Python Utilities (If Desired)
1. Implement Issue #52 utilities if not using Rust CLI
2. Integrate with GitHub Actions for convenience
3. Add compliance reporting (SOX, PCI-DSS for enterprise)

### Immediate: Use Existing Infrastructure
- perf.yml is functional and comprehensive
- bench-report CLI covers validate/baseline/compare needs
- Scripts exist for SLO validation
- Only missing: formal receipt artifact structure

---

## File Inventory for Reference

### Absolute Paths

Core benchmarks:
- `/home/steven/code/Rust/copybook-rs/copybook-bench/benches/decode_performance.rs`
- `/home/steven/code/Rust/copybook-rs/copybook-bench/benches/comp3.rs`
- `/home/steven/code/Rust/copybook-rs/copybook-bench/benches/progressive.rs`

Baseline/reporting:
- `/home/steven/code/Rust/copybook-rs/copybook-bench/src/baseline.rs`
- `/home/steven/code/Rust/copybook-rs/copybook-bench/src/reporting.rs`
- `/home/steven/code/Rust/copybook-rs/copybook-bench/src/regression.rs`
- `/home/steven/code/Rust/copybook-rs/copybook-bench/src/bin/bench-report.rs`

Scripts:
- `/home/steven/code/Rust/copybook-rs/scripts/bench.sh`
- `/home/steven/code/Rust/copybook-rs/scripts/perf-annotate-host.sh`
- `/home/steven/code/Rust/copybook-rs/scripts/guard-hotpaths.sh`

Workflows:
- `/home/steven/code/Rust/copybook-rs/.github/workflows/perf.yml`
- `/home/steven/code/Rust/copybook-rs/.github/workflows/benchmark.yml`

Documentation:
- `/home/steven/code/Rust/copybook-rs/copybook-bench/HARDWARE_SPECS.md`
- `/home/steven/code/Rust/copybook-rs/copybook-bench/BASELINE_METHODOLOGY.md`
- `/home/steven/code/Rust/copybook-rs/docs/specifications/perf-json-schema.md`
- `/home/steven/code/Rust/copybook-rs/docs/adr/adr-001-benchmark-reporting-infrastructure.md`
- `/home/steven/code/Rust/copybook-rs/docs/backlog/benchmark_tooling.md`

---

## Conclusion

The copybook-rs project has a **mature, well-documented benchmarking infrastructure** with:
- Sophisticated Criterion.rs harness with multiple scales
- Production-grade baseline management and regression detection
- Active CI integration with SLO validation gates
- Comprehensive documentation of procedures and environment

The gap for PR-C is **not foundational** but rather **formalization** of receipt artifacts and historical tracking. All the underlying infrastructure is in place; the next step is to structure receipts as first-class artifacts with versioning, retention policies, and historical tracking.

Current baseline (2025-09-30): **DISPLAY 205 MiB/s, COMP-3 58 MiB/s** with documented measurement environment and <8% variance acceptable for WSL2.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
