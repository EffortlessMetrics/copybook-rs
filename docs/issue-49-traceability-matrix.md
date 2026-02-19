<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue #49 Traceability Matrix
## Performance Regression Monitoring - AC to Specification Mapping

### Overview

This matrix provides **complete traceability** between Issue #49 acceptance criteria and technical specifications, API contracts, implementation guides, and test scaffolding. Enables systematic TDD implementation with clear validation paths.

**Issue**: #49 - Performance Regression Monitoring and Benchmark Optimization
**Foundation**: Issue #52 (PR #67) - Machine-readable benchmark reporting infrastructure
**Status**: SPECIFICATION COMPLETE - Ready for TDD implementation

---

## Acceptance Criteria Coverage Matrix

| AC ID | Acceptance Criteria | Specification Section | API Contract | How-To Guide | Test Tag | Priority |
|-------|-------------------|---------------------|--------------|--------------|---------|---------|
| **AC1** | Regression Detection Validation | [Explanation §Regression Detection](#ac1-regression-detection) | [API §Regression Detection Algorithm](#ac1-api) | [How-To §AC1](#ac1-testing) | `// AC1` | HIGH |
| **AC2** | Performance Baseline Reconciliation | [Explanation §Baseline Management](#ac2-baseline) | [API §Baseline Management](#ac2-api) | [How-To §AC2](#ac2-baseline) | `// AC2` | **CRITICAL** |
| **AC3** | CI Regression Gating Validation | [Explanation §CI Integration](#ac3-ci) | [API §CI Integration Contracts](#ac3-api) | [How-To §AC3](#ac3-ci) | `// AC3` | HIGH |
| **AC4** | Progressive Complexity Testing | [Explanation §Progressive Testing](#ac4-progressive) | [API §Progressive Complexity](#ac4-api) | [How-To §AC4](#ac4-progressive) | `// AC4` | MEDIUM |
| **AC5** | Enhanced Diagnostics | [Explanation §Diagnostics](#ac5-diagnostics) | [API §Diagnostic API Contracts](#ac5-api) | [How-To §AC5](#ac5-diagnostics) | `// AC5` | MEDIUM |

---

## AC1: Regression Detection Validation

### Acceptance Criteria (From Issue #49)

> **AC1: Regression Detection Validation**
> - Verify >5% performance degradation triggers WARNING status in `bench-report compare` output
> - Verify >10% performance degradation triggers FAILURE status in `bench-report compare` output
> - Test missing baseline scenario (first-time PRs) returns NEUTRAL status without failing CI
> - Validate threshold calculation against baseline.json using synthetic performance deltas
> - Document expected behavior in `docs/performance-regression-testing.md` with test scenarios

### Specification Sections

#### Explanation Document
**File**: `docs/explanation/performance-regression-monitoring.md`
**Section**: "Regression Detection Architecture"

**Key Concepts**:
- Threshold Policy (5% WARNING, 10% FAILURE)
- Status Definitions (PASS, WARNING, FAILURE, NEUTRAL)
- Design Rationale for threshold percentages
- Regression Calculation Formula: `(baseline - current) / baseline * 100.0`

#### API Contract
**File**: `docs/reference/benchmark-api-contracts.md`
**Section**: "Regression Detection Algorithm"

**API Methods**:
```rust
impl BaselineStore {
    pub fn check_regression(
        &self,
        report: &PerformanceReport,
        threshold: f64
    ) -> Vec<String>;
}
```

**Threshold Policy Table**:
| Regression | Status | PR Gate | CI Exit Code |
|-----------|--------|---------|--------------|
| <5% | PASS | ✅ Pass | 0 |
| 5-10% | WARNING | ⚠️ Pass | 0 |
| >10% | FAILURE | ❌ Block | 1 |
| No baseline | NEUTRAL | ✅ Pass | 0 |

#### How-To Guide
**File**: `docs/how-to/benchmark-regression-testing.md`
**Section**: "AC1: Testing Regression Detection Thresholds"

**Step-by-Step Instructions**:
1. Create synthetic performance reports (PASS, WARNING, FAILURE scenarios)
2. Test PASS scenario (0% regression)
3. Test WARNING scenario (5-10% regression)
4. Test FAILURE scenario (>10% regression)
5. Test NEUTRAL scenario (missing baseline)
6. Write automated regression detection tests

### Test Scaffolding

**Test File**: `tests/regression_detection.rs`
**Test Tags**: `// AC1: Regression threshold validation`

**Required Tests**:
```rust
#[test]
fn test_regression_pass_no_change() {  // AC1
    let mut store = BaselineStore::new();
    let baseline = create_report(4.22, 571.0);
    store.promote_baseline(&baseline, "main", "abc123");

    let current = create_report(4.22, 571.0);  // 0% change
    let regressions = store.check_regression(&current, 5.0);

    assert!(regressions.is_empty());
}

#[test]
fn test_regression_warning_threshold() {  // AC1
    let mut store = BaselineStore::new();
    let baseline = create_report(4.22, 571.0);
    store.promote_baseline(&baseline, "main", "abc123");

    let current = create_report(3.97, 537.0);  // ~6% regression
    let regressions = store.check_regression(&current, 5.0);

    assert_eq!(regressions.len(), 2);
    assert!(regressions[0].contains("5.9"));
}

#[test]
fn test_regression_failure_threshold() {  // AC1
    let mut store = BaselineStore::new();
    let baseline = create_report(4.22, 571.0);
    store.promote_baseline(&baseline, "main", "abc123");

    let current = create_report(3.71, 502.0);  // ~12% regression
    let regressions = store.check_regression(&current, 5.0);

    assert_eq!(regressions.len(), 2);
    assert!(regressions[0].contains("12."));
}

#[test]
fn test_missing_baseline_neutral() {  // AC1
    let store = BaselineStore::new();  // No baseline
    let current = create_report(4.22, 571.0);

    let regressions = store.check_regression(&current, 5.0);

    assert!(regressions.is_empty());  // NEUTRAL
}
```

### Implementation Checklist

- [ ] Implement `check_regression()` method in `baseline.rs`
- [ ] Add threshold validation logic (5%, 10%)
- [ ] Handle missing baseline scenario (return empty Vec)
- [ ] Implement regression message formatting
- [ ] Write unit tests with `// AC1` tags
- [ ] Document expected behavior in specifications
- [ ] Validate with synthetic test reports
- [ ] Integrate with `bench-report compare` CLI command

### Validation Evidence

**Success Criteria**:
- All unit tests passing with `// AC1` tags
- `bench-report compare` returns correct exit codes (0 or 1)
- Regression messages contain correct percentages and values
- Missing baseline returns NEUTRAL without errors

---

## AC2: Performance Baseline Reconciliation

### Acceptance Criteria (From Issue #49)

> **AC2: Performance Baseline Reconciliation**
> - Establish canonical performance baseline with documented hardware specifications (CPU, RAM, OS)
> - Reconcile REPORT.md vs CLAUDE.md performance number discrepancies through comprehensive benchmark runs
> - Document measurement methodology including environment setup, Criterion configuration, and statistical approach
> - Run comprehensive benchmark suite (`PERF=1 cargo bench -p copybook-bench`) on clean environment to establish authoritative baseline
> - Update both CLAUDE.md and REPORT.md with consistent, verified performance numbers and measurement context
> - Create `docs/performance-measurement-methodology.md` documenting reproducible benchmark procedures

### Specification Sections

#### Explanation Document
**File**: `docs/explanation/performance-regression-monitoring.md`
**Section**: "Performance Context and Baseline Discrepancy", "Baseline Management Architecture"

**Key Concepts**:
- Current baseline discrepancy (CLAUDE.md vs REPORT.md)
- Performance baseline reconciliation procedure
- Environment documentation requirements
- Clean environment setup
- Canonical baseline measurement (5 runs, median selection)
- Statistical validation (CV <5%)

#### API Contract
**File**: `docs/reference/benchmark-api-contracts.md`
**Section**: "Baseline Management API"

**API Methods**:
```rust
impl BaselineStore {
    pub fn promote_baseline(
        &mut self,
        report: &PerformanceReport,
        branch: &str,
        commit: &str
    );

    pub fn save<P: AsRef<Path>>(&self, path: P) -> anyhow::Result<()>;
}

pub struct PerformanceBaseline {
    pub branch: String,
    pub commit: String,
    pub timestamp: String,
    pub display_gibs: Option<f64>,
    pub comp3_mibs: Option<f64>,
    pub sample_count: u32,
}
```

#### How-To Guide
**File**: `docs/how-to/benchmark-regression-testing.md`
**Section**: "AC2: Establishing Canonical Performance Baseline"

**Step-by-Step Instructions**:
1. Document hardware environment
2. Prepare clean measurement environment
3. Configure CPU for stable performance
4. Run canonical baseline measurements (5 runs)
5. Analyze statistical variance (CV <5%)
6. Promote canonical baseline
7. Update documentation (CLAUDE.md, REPORT.md)
8. Validate baseline quality

### Test Scaffolding

**Test File**: `tests/baseline_reconciliation.rs`
**Test Tags**: `// AC2: Baseline reconciliation`

**Required Tests**:
```rust
#[test]
fn test_baseline_measurement_methodology() {  // AC2
    let measurements = run_multiple_benchmarks(5);

    let mean_display = calculate_mean(&measurements.display_values);
    let stddev_display = calculate_stddev(&measurements.display_values);
    let cv_display = stddev_display / mean_display;

    assert!(cv_display < 0.05, "CV exceeds 5%");

    let methodology = generate_methodology_report(&measurements);
    assert!(methodology.contains("Hardware: "));
    assert!(methodology.contains("Software: "));
}

#[test]
fn test_baseline_promotion() {  // AC2
    let mut store = BaselineStore::new();
    let report = create_report(4.22, 571.0);

    store.promote_baseline(&report, "main", "abc12345");

    assert!(store.current.is_some());
    let baseline = store.current.as_ref().unwrap();
    assert_eq!(baseline.display_gibs, Some(4.22));
    assert_eq!(baseline.comp3_mibs, Some(571.0));
}

#[test]
fn test_baseline_persistence() {  // AC2
    let mut store = BaselineStore::new();
    let report = create_report(4.22, 571.0);
    store.promote_baseline(&report, "main", "abc12345");

    let temp_path = "/tmp/test_baseline.json";
    store.save(temp_path).unwrap();

    let loaded = BaselineStore::load_or_create(temp_path).unwrap();
    assert_eq!(loaded.current.unwrap().display_gibs, Some(4.22));
}
```

### Implementation Checklist

- [ ] Create `docs/performance-measurement-methodology.md`
- [ ] Document hardware specifications
- [ ] Run 5 canonical baseline measurements
- [ ] Analyze statistical variance (CV <5%)
- [ ] Select median values as canonical baseline
- [ ] Promote baseline using `bench-report baseline promote`
- [ ] Update CLAUDE.md with verified baseline
- [ ] Update REPORT.md with verified baseline
- [ ] Commit baseline to git (`target/baselines/performance.json`)
- [ ] Validate with `bench-report health-check`

### Validation Evidence

**Success Criteria**:
- `docs/performance-measurement-methodology.md` exists with complete hardware specs
- CLAUDE.md and REPORT.md have consistent performance numbers
- `target/baselines/performance.json` exists with canonical baseline
- Statistical variance <5% across 5 runs
- All baseline tests passing with `// AC2` tags

---

## AC3: CI Regression Gating Validation

### Acceptance Criteria (From Issue #49)

> **AC3: CI Regression Gating Validation**
> - Confirm PR comments display regression warnings with delta percentages and baseline comparison
> - Verify artifact uploads (`perf.json`, `baseline-main-*.zip`) with 90-day retention policy for audit compliance
> - Test baseline promotion on main branch merge creates `target/baselines/performance.json` correctly
> - Validate 30-minute timeout protection prevents stuck benchmark runners in CI
> - Test missing baseline scenario gracefully degrades to NEUTRAL without PR failure
> - Document CI workflow behavior in `.github/workflows/benchmark.yml` inline comments

### Specification Sections

#### Explanation Document
**File**: `docs/explanation/performance-regression-monitoring.md`
**Section**: "CI Integration Architecture"

**Key Concepts**:
- GitHub Actions workflow integration
- PR comment automation
- Baseline promotion on main branch
- Artifact uploads with 90-day retention
- Timeout protection (30 minutes)
- Missing baseline handling (NEUTRAL)

#### API Contract
**File**: `docs/reference/benchmark-api-contracts.md`
**Section**: "CI Integration Contracts"

**GitHub Actions Workflow**:
```yaml
- name: Compare Against Baseline
  if: github.event_name == 'pull_request'
  run: |
    cargo run --bin bench-report -- compare perf.json

- name: Promote Baseline
  if: github.ref == 'refs/heads/main'
  run: |
    cargo run --bin bench-report -- baseline promote perf.json

- name: Upload Artifacts
  uses: actions/upload-artifact@v4
  with:
    retention-days: 90
```

**PR Comment Format**:
```markdown
## Performance Benchmark Results

📊 Performance Comparison
   Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
   Current: DISPLAY: 4.18 GiB/s, COMP-3: 565 MiB/s ✅

✅ No performance regressions detected
   DISPLAY: -0.95% (within acceptable variance)
   COMP-3: -1.05% (within acceptable variance)
```

#### How-To Guide
**File**: `docs/how-to/benchmark-regression-testing.md`
**Section**: "AC3: Validating CI Integration"

**Step-by-Step Instructions**:
1. Review CI workflow configuration
2. Test PR comment generation locally
3. Validate artifact structure
4. Test baseline promotion workflow
5. Validate 90-day retention policy
6. Test timeout protection

### Test Scaffolding

**Test File**: `tests/ci_integration.rs`
**Test Tags**: `// AC3: CI integration`

**Required Tests**:
```rust
#[test]
fn test_pr_comment_generation() {  // AC3
    let report = create_report(4.18, 565.0);
    let summary = report.format_pr_summary();

    assert!(summary.contains("DISPLAY: 4.18 GiB/s"));
    assert!(summary.contains("COMP-3: 565 MiB/s"));
    assert!(summary.contains("✅"));
}

#[test]
fn test_artifact_retention_policy() {  // AC3
    let store = create_baseline_store_with_history(120);

    let retained = store.history.iter()
        .filter(|b| {
            let timestamp = chrono::DateTime::parse_from_rfc3339(&b.timestamp).unwrap();
            let age = chrono::Utc::now() - timestamp;
            age.num_days() <= 90
        })
        .count();

    assert_eq!(retained, store.history.len());
}

#[test]
fn test_baseline_promotion_on_main() {  // AC3
    let report = create_report(4.22, 571.0);
    let mut store = BaselineStore::new();

    store.promote_baseline(&report, "main", "abc12345");

    assert!(store.current.is_some());
    assert_eq!(store.current.as_ref().unwrap().branch, "main");
}
```

### Implementation Checklist

- [ ] Enhance `.github/workflows/benchmark.yml` with regression comparison
- [ ] Implement PR comment automation with `actions/github-script@v7`
- [ ] Add baseline promotion on main branch merges
- [ ] Configure artifact uploads with 90-day retention
- [ ] Add 30-minute timeout protection
- [ ] Handle missing baseline scenario (NEUTRAL)
- [ ] Write CI integration tests with `// AC3` tags
- [ ] Document workflow behavior with inline comments

### Validation Evidence

**Success Criteria**:
- PR comments posted/updated automatically
- Baseline promoted on main branch merges
- Artifacts uploaded with 90-day retention
- Missing baseline returns NEUTRAL without errors
- All CI integration tests passing with `// AC3` tags

---

## AC4: Progressive Complexity Testing

### Acceptance Criteria (From Issue #49)

> **AC4: Progressive Complexity Testing (Developer Mode)**
> - Implement 1KB → 10KB → 100KB → 1MB progressive benchmark suite in `copybook-bench/benches/progressive.rs`
> - Add early bailout if execution time exceeds configurable threshold (default: 10 seconds per size tier)
> - Keep progressive benchmarks in `PERF=1` developer-only mode (not executed in PR CI to avoid gate bloat)
> - Useful for performance profiling with tools like `cargo flamegraph`, `perf`, or `valgrind`
> - Document usage in CLAUDE.md under "Performance Profiling" section with example workflows

### Specification Sections

#### Explanation Document
**File**: `docs/explanation/performance-regression-monitoring.md`
**Section**: "Progressive Complexity Testing (AC4)"

**Key Concepts**:
- Progressive data sizes (1KB → 10KB → 100KB → 1MB)
- Early bailout mechanism (>10s per iteration)
- Developer-mode only (PERF=1, not PR-gated)
- Integration with profiling tools (flamegraph, perf, valgrind)

#### API Contract
**File**: `docs/reference/benchmark-api-contracts.md`
**Section**: "Progressive Complexity API"

**Benchmark Structure**:
```rust
// copybook-bench/benches/progressive.rs
const SIZES: &[(usize, &str)] = &[
    (1_024, "1KB"),
    (10_240, "10KB"),
    (102_400, "100KB"),
    (1_048_576, "1MB"),
];

fn progressive_decode_display(c: &mut Criterion) {
    for (size, label) in SIZES {
        // Benchmark with early bailout
    }
}
```

#### How-To Guide
**File**: `docs/how-to/benchmark-regression-testing.md`
**Section**: "AC4: Using Progressive Complexity Benchmarks"

**Step-by-Step Instructions**:
1. Create progressive complexity benchmark suite
2. Add feature flag (`progressive`)
3. Run progressive benchmarks
4. Integrate with profiling tools (flamegraph, valgrind, perf)
5. Document usage in CLAUDE.md

### Test Scaffolding

**Test File**: `tests/progressive_complexity.rs`
**Test Tags**: `// AC4: Progressive complexity testing`

**Required Tests**:
```rust
#[test]
fn test_progressive_scaling() {  // AC4
    const SIZES: &[usize] = &[1_024, 10_240, 102_400, 1_048_576];
    let mut results = Vec::new();

    for &size in SIZES {
        let data = generate_test_data(size);
        let start = std::time::Instant::now();

        decode_record(&schema, &data, &options).unwrap();

        let elapsed = start.elapsed();
        results.push((size, elapsed));

        if elapsed.as_secs() > 10 {
            break;  // Early bailout
        }
    }

    assert!(results.len() >= 2);
}

#[test]
fn test_early_bailout() {  // AC4
    let slow_function = || {
        std::thread::sleep(std::time::Duration::from_secs(11));
    };

    let start = std::time::Instant::now();
    let timeout = std::time::Duration::from_secs(10);

    let result = std::panic::catch_unwind(|| {
        if start.elapsed() > timeout {
            panic!("Timeout exceeded");
        }
        slow_function();
    });

    assert!(result.is_err());
}
```

### Implementation Checklist

- [ ] Create `copybook-bench/benches/progressive.rs`
- [ ] Implement progressive size benchmarks (1KB → 1MB)
- [ ] Add early bailout logic (>10s threshold)
- [ ] Add `progressive` feature flag to `Cargo.toml`
- [ ] Document usage in CLAUDE.md
- [ ] Test with `cargo flamegraph --bench progressive`
- [ ] Write progressive complexity tests with `// AC4` tags
- [ ] Validate PERF=1 developer-only execution

### Validation Evidence

**Success Criteria**:
- Progressive benchmarks execute successfully (1KB → 1MB)
- Early bailout prevents stuck benchmarks (>10s)
- Feature flag prevents PR CI execution
- Flamegraph integration working
- All progressive tests passing with `// AC4` tags

---

## AC5: Enhanced Diagnostics and Monitoring

### Acceptance Criteria (From Issue #49)

> **AC5: Enhanced Diagnostics and Monitoring**
> - Implement benchmark health check utility (`bench-report health-check`) validating environment setup
> - Add verbose logging mode (`--verbose` flag) for `bench-report` commands showing detailed calculation steps
> - Implement resource monitoring where platform-supported (memory usage, CPU utilization) during benchmark runs
> - Create diagnostic benches (`copybook-bench/benches/diagnostics.rs`) for infrastructure testing (baseline I/O, JSON parsing overhead)
> - Document diagnostic procedures in `docs/troubleshooting-performance.md` with common failure scenarios

### Specification Sections

#### Explanation Document
**File**: `docs/explanation/performance-regression-monitoring.md`
**Section**: "Enhanced Diagnostics and Monitoring (AC5)"

**Key Concepts**:
- Health check utility (environment validation)
- Verbose logging mode (detailed calculation steps)
- Resource monitoring (memory, CPU)
- Diagnostic benchmarks (infrastructure testing)

#### API Contract
**File**: `docs/reference/benchmark-api-contracts.md`
**Section**: "Diagnostic API Contracts"

**Health Check API**:
```bash
bench-report health-check

# Output:
# 🏥 Copybook Benchmark Health Check
# ✅ Rust version: 1.90.0
# ✅ Available memory: 28 GB
# ⚠️ CPU governor: powersave
# ✅ Baseline exists
# ✅ All health checks passed
```

**Verbose Logging API**:
```bash
bench-report --verbose validate perf.json

# Output:
# 🔍 Validating performance report
# ├─ Reading file: perf.json (142 bytes)
# ├─ Parsing JSON schema
# └─ Status: success ✅
```

#### How-To Guide
**File**: `docs/how-to/benchmark-regression-testing.md`
**Section**: "AC5: Diagnostic Tools and Troubleshooting"

**Step-by-Step Instructions**:
1. Implement health check utility
2. Use verbose logging mode
3. Create diagnostic benchmarks
4. Document common failure scenarios

### Test Scaffolding

**Test File**: `tests/diagnostics.rs`
**Test Tags**: `// AC5: Enhanced diagnostics`

**Required Tests**:
```rust
#[test]
fn test_health_check_validation() {  // AC5
    let health = run_health_check().unwrap();

    assert!(health.rust_version_ok);
    assert!(health.memory_sufficient);
    assert!(health.baseline_valid);
}

#[test]
fn test_verbose_logging() {  // AC5
    let output = run_validate_verbose("perf.json");

    assert!(output.contains("🔍 Validating"));
    assert!(output.contains("├─ Reading file"));
}

#[test]
fn test_resource_monitoring() {  // AC5
    let monitor = ResourceMonitor::new().unwrap();

    let metrics = monitor.measure(|| {
        decode_heavy_workload();
    });

    assert!(metrics.peak_memory_mb < 256);
}
```

### Implementation Checklist

- [ ] Implement `bench-report health-check` command
- [ ] Add `--verbose` flag to all `bench-report` commands
- [ ] Implement resource monitoring (memory, CPU)
- [ ] Create `copybook-bench/benches/diagnostics.rs`
- [ ] Create `docs/troubleshooting-performance.md`
- [ ] Write diagnostic tests with `// AC5` tags
- [ ] Validate health check components

### Validation Evidence

**Success Criteria**:
- Health check validates environment correctly
- Verbose logging provides detailed diagnostics
- Resource monitoring tracks memory/CPU
- Diagnostic benchmarks execute successfully
- All diagnostic tests passing with `// AC5` tags

---

## Cross-Cutting Concerns

### Zero Unsafe Code Enforcement

**Requirement**: All performance monitoring code must maintain zero unsafe code.

**Validation**:
```bash
cargo clippy --workspace -- -D warnings -W clippy::pedantic
```

**Affected ACs**: All (AC1-AC5)

### TDD Test Scaffolding

**Requirement**: All tests must include `// AC:ID` tags for traceability.

**Example**:
```rust
#[test]
fn test_regression_warning_threshold() {  // AC1: Regression threshold validation
    // Test implementation
}
```

**Test Tag Format**: `// AC1`, `// AC2`, `// AC3`, `// AC4`, `// AC5`

### MSRV Compliance

**Requirement**: Rust 1.90+ Edition 2024 compatibility.

**Validation**:
```bash
cargo build --workspace --release
cargo test --workspace
```

**Affected ACs**: All (AC1-AC5)

### Error Taxonomy

**Requirement**: All errors must use structured error codes (BENCH001-BENCH007).

**Error Codes**:
- `BENCH001_VALIDATION_FAILURE`: Performance validation failed
- `BENCH002_BASELINE_NOT_FOUND`: Baseline missing
- `BENCH003_REGRESSION_DETECTED`: Performance regression
- `BENCH004_HEALTH_CHECK_FAILED`: Health check failed
- `BENCH005_ARTIFACT_UPLOAD_FAILED`: Artifact upload failed
- `BENCH006_JSON_PARSING_FAILED`: JSON parsing error
- `BENCH007_IO_ERROR`: I/O error

**Affected ACs**: All (AC1-AC5)

---

## Implementation Priority and Dependencies

### Critical Path

```
AC2 (Baseline Reconciliation) MUST complete FIRST
     ↓
AC1 (Regression Detection) requires AC2 baseline
     ↓
AC3 (CI Integration) requires AC1 regression detection
     ↓
AC4 (Progressive Complexity) independent (can be parallel with AC1-AC3)
     ↓
AC5 (Enhanced Diagnostics) independent (can be parallel with AC1-AC4)
```

### Priority Order

1. **AC2 (CRITICAL)**: Establish canonical baseline before all other work
2. **AC1 (HIGH)**: Regression detection enables AC3
3. **AC3 (HIGH)**: CI integration completes core functionality
4. **AC4 (MEDIUM)**: Developer productivity enhancement
5. **AC5 (MEDIUM)**: Diagnostic and troubleshooting support

---

## Specification Document Links

### Primary Specifications

1. **Explanation Document** (Understanding-Oriented):
   - **File**: `docs/explanation/performance-regression-monitoring.md`
   - **Coverage**: All ACs (AC1-AC5) with architectural context
   - **Diátaxis Category**: Explanation

2. **API Contract Reference** (Information-Oriented):
   - **File**: `docs/reference/benchmark-api-contracts.md`
   - **Coverage**: All ACs (AC1-AC5) with API specifications
   - **Diátaxis Category**: Reference

3. **How-To Guide** (Task-Oriented):
   - **File**: `docs/how-to/benchmark-regression-testing.md`
   - **Coverage**: All ACs (AC1-AC5) with step-by-step procedures
   - **Diátaxis Category**: How-To

### Supporting Documentation

4. **Traceability Matrix** (This Document):
   - **File**: `docs/issue-49-traceability-matrix.md`
   - **Coverage**: AC-to-specification mapping

5. **Troubleshooting Guide**:
   - **File**: `docs/troubleshooting-performance.md`
   - **Coverage**: AC5 diagnostic procedures
   - **Diátaxis Category**: How-To

6. **Performance Measurement Methodology**:
   - **File**: `docs/performance-measurement-methodology.md`
   - **Coverage**: AC2 baseline establishment procedures
   - **Diátaxis Category**: Reference

---

## Test File Organization

### Test Files by AC

```
tests/
├── regression_detection.rs        # AC1 tests
├── baseline_reconciliation.rs     # AC2 tests
├── ci_integration.rs              # AC3 tests
├── progressive_complexity.rs      # AC4 tests
└── diagnostics.rs                 # AC5 tests

copybook-bench/benches/
├── progressive.rs                 # AC4 benchmarks
└── diagnostics.rs                 # AC5 benchmarks
```

### Test Tag Usage

**Search for AC-specific tests**:
```bash
# Find all AC1 tests
grep -r "// AC1" tests/

# Find all AC2 tests
grep -r "// AC2" tests/

# Count tests per AC
grep -r "// AC1" tests/ | wc -l
```

---

## Validation Checklist

### Specification Completeness

- [x] AC1 fully specified in all three documents
- [x] AC2 fully specified in all three documents
- [x] AC3 fully specified in all three documents
- [x] AC4 fully specified in all three documents
- [x] AC5 fully specified in all three documents
- [x] Traceability matrix complete
- [ ] Documentation index updated

### Test Coverage

- [ ] AC1 tests written and passing
- [ ] AC2 tests written and passing
- [ ] AC3 tests written and passing
- [ ] AC4 tests written and passing
- [ ] AC5 tests written and passing
- [ ] All tests tagged with `// AC:ID`

### Implementation Readiness

- [ ] AC2 baseline reconciliation complete (PRIORITY 1)
- [ ] AC1 regression detection implemented
- [ ] AC3 CI integration complete
- [ ] AC4 progressive benchmarks implemented
- [ ] AC5 diagnostics implemented

---

## Success Criteria Summary

### AC1 Success Criteria
- ✅ Regression detection thresholds validated (5%, 10%)
- ✅ PASS, WARNING, FAILURE, NEUTRAL scenarios tested
- ✅ Exit codes correct (0 for PASS/NEUTRAL, 1 for WARNING/FAILURE)
- ✅ Regression messages contain correct percentages

### AC2 Success Criteria
- ✅ Hardware specifications documented
- ✅ CLAUDE.md and REPORT.md have consistent baselines
- ✅ Canonical baseline established (CV <5%)
- ✅ `target/baselines/performance.json` exists

### AC3 Success Criteria
- ✅ PR comments posted/updated automatically
- ✅ Baseline promoted on main branch merges
- ✅ Artifacts uploaded with 90-day retention
- ✅ Missing baseline returns NEUTRAL

### AC4 Success Criteria
- ✅ Progressive benchmarks execute (1KB → 1MB)
- ✅ Early bailout prevents stuck benchmarks
- ✅ Flamegraph integration working
- ✅ Feature flag prevents PR CI execution

### AC5 Success Criteria
- ✅ Health check validates environment
- ✅ Verbose logging provides diagnostics
- ✅ Resource monitoring tracks memory/CPU
- ✅ Diagnostic benchmarks execute

---

**Matrix Version**: 1.0
**Last Updated**: 2025-09-30
**Related Issues**: #49 (this specification), #52 (foundation)
**Related Documents**:
- `docs/explanation/performance-regression-monitoring.md`
- `docs/reference/benchmark-api-contracts.md`
- `docs/how-to/benchmark-regression-testing.md`
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
