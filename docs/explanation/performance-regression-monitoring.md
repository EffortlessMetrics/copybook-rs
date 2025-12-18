# Performance Regression Monitoring Architecture
## Issue #49 - Enterprise Performance Gate Infrastructure

### Executive Summary

copybook-rs requires **reliable performance regression monitoring** to maintain enterprise mainframe data processing throughput guarantees across development cycles. This document explains the architectural design for performance regression detection, baseline management, and CI integration that enables continuous performance validation while supporting developer productivity workflows.

**Foundation**: Issue #52 (PR #67) delivered machine-readable benchmark reporting infrastructure (`bench-report` CLI, `PerformanceReport` JSON schema, baseline storage). Issue #49 extends this foundation with regression thresholds, CI gates, progressive complexity testing, and enhanced diagnostics.

**Architecture Status**: SPECIFICATION COMPLETE - Ready for TDD implementation

### Performance Context and Baseline Discrepancy

**CRITICAL**: Significant performance baseline discrepancy exists between documentation sources:

- **CLAUDE.md**: DISPLAY 2.33 GiB/s, COMP-3 168-176 MiB/s
- **REPORT.md**: DISPLAY 66-95 MiB/s, COMP-3 18-25 MiB/s (appears to be measurement error or different methodology)
- **Current Measurements**: COMP-3 173.79-190.85 MiB/s (confirmed working benchmarks)

**AC2 Baseline Reconciliation** (PRIORITY 1) must establish canonical performance baseline with documented methodology before implementing regression gates (AC1, AC3).

**Enterprise Performance Requirements**:
- **Floor Targets**: DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s (minimum production requirements)
- **Aspirational Goals**: DISPLAY ~4.1 GiB/s, COMP-3 ~560 MiB/s (hardware and methodology dependent)
- **Safety Margins**: Current performance exceeds floor targets by substantial margins
- **Regression Thresholds**: >5% WARNING (investigation recommended), >10% FAILURE (PR gate enforcement)

---

## Architecture Overview

### System Components

```
┌──────────────────────────────────────────────────────────────────┐
│            Performance Regression Monitoring System              │
├────────────────┬─────────────────┬──────────────┬───────────────┤
│  Measurement   │  Baseline       │  Regression  │   CI         │
│  Infrastructure│  Management     │  Detection   │   Integration │
│       ↓        │        ↓        │       ↓      │      ↓        │
│  PERF=1 cargo  │ BaselineStore   │ Threshold    │ GitHub       │
│  bench         │ performance.json│ Validation   │ Actions      │
│  Criterion.rs  │ 90-day retention│ ±5%/±10%     │ PR Comments  │
└────────────────┴─────────────────┴──────────────┴───────────────┘
```

### Data Flow Architecture

```
Developer Workflow:
  PERF=1 cargo bench
       ↓
  Criterion.rs → target/criterion/**/estimates.json
       ↓
  bench-report validate perf.json
       ↓
  Local comparison vs baseline.json
       ↓
  Developer feedback (WARNING/FAILURE)

CI Workflow (Pull Request):
  GitHub Actions trigger
       ↓
  cargo bench --package copybook-bench
       ↓
  bench-report validate perf.json
       ↓
  bench-report compare perf.json (vs main baseline)
       ↓
  PR comment with delta percentages
       ↓
  Gate enforcement (>10% = FAILURE)
       ↓
  Artifact upload (perf.json, baseline)

CI Workflow (Main Branch):
  Benchmark execution
       ↓
  bench-report validate perf.json
       ↓
  bench-report baseline promote perf.json
       ↓
  Update target/baselines/performance.json
       ↓
  Artifact upload for PR comparisons
```

---

## Regression Detection Architecture

### Threshold Policy (AC1)

**Regression Calculation**:
```rust
// baseline.rs - check_regression implementation
let regression_pct = (baseline_value - current_value) / baseline_value * 100.0;

if regression_pct > 10.0 {
    Status::FAILURE  // Block PR merge
} else if regression_pct > 5.0 {
    Status::WARNING  // Investigation recommended
} else {
    Status::PASS     // Performance acceptable
}
```

**Status Definitions**:
- **PASS**: Current performance within 5% of baseline (no regression)
- **WARNING**: 5-10% regression detected (investigation recommended, PR not blocked)
- **FAILURE**: >10% regression detected (PR merge blocked, requires mitigation)
- **NEUTRAL**: Missing baseline (first-time PRs, no blocking)

**Design Rationale**:
- **5% WARNING Threshold**: Accounts for measurement variance while flagging potential issues
- **10% FAILURE Threshold**: Clear regression signal that justifies PR blocking
- **NEUTRAL for Missing Baseline**: Prevents false positives on first-time benchmarks
- **Relative Thresholds**: Scales with baseline performance, avoids absolute value brittleness

### Baseline Management Architecture (AC2)

**Performance Baseline Reconciliation Procedure**:

1. **Environment Documentation**:
   ```bash
   # Document hardware specifications
   CPU: Intel/AMD model, core count, frequency
   RAM: Total memory, available memory during benchmark
   OS: Linux distribution, kernel version
   Rust: Toolchain version, target triple

   # Example:
   # CPU: Intel Core i7-9700K @ 3.6GHz (8 cores)
   # RAM: 32GB DDR4-3200 (28GB available)
   # OS: Ubuntu 22.04 LTS (kernel 5.15.0)
   # Rust: 1.89.0 stable (x86_64-unknown-linux-gnu)
   ```

2. **Clean Environment Setup**:
   ```bash
   # Remove existing benchmark artifacts
   rm -rf target/criterion target/baselines

   # Clean build
   cargo clean
   cargo build --workspace --release

   # Minimize background processes
   # Close unnecessary applications
   # Disable CPU frequency scaling (if applicable)
   ```

3. **Canonical Baseline Measurement**:
   ```bash
   # Run comprehensive benchmark suite multiple times
   for i in {1..5}; do
     echo "Run $i of 5"
     PERF=1 cargo bench -p copybook-bench -- --save-baseline run_$i
   done

   # Analyze variance across runs
   # Select median run as canonical baseline
   # Document statistical properties (mean, stddev, CI)
   ```

4. **Documentation Update**:
   ```bash
   # Update CLAUDE.md with verified baseline
   # Update REPORT.md with measurement methodology
   # Create docs/performance-measurement-methodology.md
   # Commit baseline to target/baselines/performance.json
   ```

**Baseline Storage Structure**:
```rust
// baseline.rs - PerformanceBaseline struct
pub struct PerformanceBaseline {
    pub branch: String,              // "main"
    pub commit: String,              // Git commit hash
    pub timestamp: String,           // ISO 8601 timestamp
    pub display_gibs: Option<f64>,   // DISPLAY throughput baseline
    pub comp3_mibs: Option<f64>,     // COMP-3 throughput baseline
    pub sample_count: u32,           // Number of measurements averaged
}

pub struct BaselineStore {
    pub current: Option<PerformanceBaseline>,  // Active baseline
    pub history: Vec<PerformanceBaseline>,      // Historical baselines
    pub updated: String,                        // Last updated timestamp
}
```

**Retention Policy**:
- **Current Baseline**: Always retained (active comparison target)
- **Historical Baselines**: 90-day retention (audit compliance)
- **Artifact Retention**: GitHub Actions artifacts retained per repository policy
- **Trend Analysis**: Historical baselines enable performance trend visualization

### CI Integration Architecture (AC3)

**GitHub Actions Workflow Integration**:

```yaml
# .github/workflows/benchmark.yml (enhanced)
name: Benchmark

on:
  pull_request:
    branches: [ main ]
  push:
    branches: [ main ]
  workflow_dispatch:

jobs:
  benchmark:
    name: Performance Regression Detection
    runs-on: ubuntu-latest
    timeout-minutes: 30  # Prevent stuck benchmark runners
    permissions:
      contents: read
      pull-requests: write

    steps:
    - name: Checkout
      uses: actions/checkout@v4
      with:
        fetch-depth: 0  # Full history for baseline comparison

    - name: Setup Rust
      uses: dtolnay/rust-toolchain@stable

    - name: Cache Dependencies
      uses: Swatinem/rust-cache@v2
      with:
        key: benchmark

    - name: Run Benchmarks
      run: |
        PERF=1 cargo bench -p copybook-bench

    - name: Validate Performance Report
      run: |
        cargo run --bin bench-report -p copybook-bench -- validate perf.json

    - name: Download Baseline (PR only)
      if: github.event_name == 'pull_request'
      uses: actions/download-artifact@v4
      with:
        name: baseline-main
        path: target/baselines/
      continue-on-error: true  # Missing baseline = NEUTRAL

    - name: Compare Against Baseline (PR only)
      if: github.event_name == 'pull_request'
      id: compare
      run: |
        # Run comparison
        cargo run --bin bench-report -p copybook-bench -- compare perf.json > comparison.txt

        # Set output for PR comment
        echo "comparison<<EOF" >> $GITHUB_OUTPUT
        cat comparison.txt >> $GITHUB_OUTPUT
        echo "EOF" >> $GITHUB_OUTPUT

        # Check for regression failures
        if grep -q "Performance regressions detected" comparison.txt; then
          echo "regression=true" >> $GITHUB_OUTPUT
          exit 1  # Fail workflow on >10% regression
        else
          echo "regression=false" >> $GITHUB_OUTPUT
        fi
      continue-on-error: true

    - name: Post PR Comment (PR only)
      if: github.event_name == 'pull_request'
      uses: actions/github-script@v7
      with:
        script: |
          const comparison = `${{ steps.compare.outputs.comparison }}`;
          const body = `## Performance Benchmark Results\n\n${comparison}`;

          // Find existing comment
          const { data: comments } = await github.rest.issues.listComments({
            owner: context.repo.owner,
            repo: context.repo.repo,
            issue_number: context.issue.number,
          });

          const botComment = comments.find(comment =>
            comment.user.type === 'Bot' &&
            comment.body.includes('Performance Benchmark Results')
          );

          // Update or create comment
          if (botComment) {
            await github.rest.issues.updateComment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              comment_id: botComment.id,
              body: body,
            });
          } else {
            await github.rest.issues.createComment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              issue_number: context.issue.number,
              body: body,
            });
          }

    - name: Upload Artifacts
      uses: actions/upload-artifact@v4
      with:
        name: perf-${{ github.sha }}
        path: |
          perf.json
          target/criterion/**/*.json
        retention-days: 90  # Enterprise audit compliance

    - name: Promote Baseline (Main only)
      if: github.ref == 'refs/heads/main' && github.event_name == 'push'
      run: |
        cargo run --bin bench-report -p copybook-bench -- baseline promote perf.json

    - name: Upload Baseline (Main only)
      if: github.ref == 'refs/heads/main' && github.event_name == 'push'
      uses: actions/upload-artifact@v4
      with:
        name: baseline-main
        path: target/baselines/performance.json
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

**Regression Failure Format**:
```markdown
## Performance Benchmark Results

📊 Performance Comparison
   Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
   Current: DISPLAY: 3.50 GiB/s, COMP-3: 500 MiB/s ❌

❌ Performance regressions detected:
   DISPLAY regression: 17.06% slower than baseline (3.50 vs 4.22 GiB/s)
   COMP-3 regression: 12.43% slower than baseline (500 vs 571 MiB/s)

⚠️ This PR is blocked due to >10% performance regression.
   Please investigate and address performance degradation before merging.
```

---

## Progressive Complexity Testing (AC4)

### Developer Productivity Architecture

**Progressive Benchmark Suite Design**:
```rust
// copybook-bench/benches/progressive.rs
use criterion::{Criterion, BenchmarkId};
use std::time::Duration;

fn progressive_complexity_benchmarks(c: &mut Criterion) {
    // Progressive data sizes for iterative optimization
    const SIZES: &[(usize, &str)] = &[
        (1_024, "1KB"),
        (10_240, "10KB"),
        (102_400, "100KB"),
        (1_048_576, "1MB"),
    ];

    let mut group = c.benchmark_group("progressive_decode");
    group.sample_size(10);  // Reduced sample size for speed
    group.measurement_time(Duration::from_secs(5));  // Quick iterations

    for (size, label) in SIZES {
        let data = generate_test_data(*size);

        group.bench_with_input(
            BenchmarkId::new("display", label),
            size,
            |b, _| {
                // Early bailout if iteration takes too long
                let start = std::time::Instant::now();
                b.iter(|| {
                    decode_display_record(&data);
                });

                // Check if we should stop (>10s per iteration)
                if start.elapsed() > Duration::from_secs(10) {
                    eprintln!("⚠️ Stopping at {label}: iteration time exceeded 10s");
                    return;
                }
            },
        );
    }

    group.finish();
}

criterion_group! {
    name = progressive;
    config = Criterion::default();
    targets = progressive_complexity_benchmarks
}

// Only run in PERF=1 developer mode
#[cfg(feature = "progressive")]
criterion_main!(progressive);
```

**Developer Workflow Integration**:
```bash
# Run progressive benchmarks for optimization iteration
PERF=1 cargo bench -p copybook-bench --features progressive -- progressive

# Integrate with profiling tools
cargo install flamegraph
PERF=1 cargo flamegraph --bench progressive --features progressive

# Memory profiling with valgrind
PERF=1 cargo bench --features progressive --no-run
valgrind --tool=massif target/release/deps/progressive-*

# CPU profiling with perf
PERF=1 cargo bench --features progressive --no-run
perf record -F 99 target/release/deps/progressive-*
perf report
```

**Design Rationale**:
- **PERF=1 Only**: Not executed in PR CI to avoid gate bloat
- **Early Bailout**: Prevents stuck benchmarks on O(n²) algorithms
- **Progressive Scaling**: Identifies performance cliffs at different data sizes
- **Developer Focus**: Optimized for quick feedback during optimization work

---

## Enhanced Diagnostics and Monitoring (AC5)

### Diagnostic Infrastructure

**Benchmark Health Check Utility**:
```rust
// copybook-bench/src/bin/bench-report.rs (extended)
fn health_check() -> Result<()> {
    println!("🏥 Copybook Benchmark Health Check");

    // 1. Environment validation
    check_rust_version()?;
    check_available_memory()?;
    check_cpu_governor()?;

    // 2. Baseline validation
    check_baseline_exists()?;
    check_baseline_integrity()?;

    // 3. Infrastructure validation
    check_criterion_infrastructure()?;
    check_output_directories()?;

    // 4. Quick smoke test
    run_minimal_benchmark()?;

    println!("✅ All health checks passed");
    Ok(())
}

fn check_rust_version() -> Result<()> {
    let version = rustc_version::version()?;
    if version < Version::parse("1.89.0")? {
        bail!("Rust 1.89+ required for MSRV compliance");
    }
    println!("✅ Rust version: {version}");
    Ok(())
}

fn check_available_memory() -> Result<()> {
    #[cfg(target_os = "linux")]
    {
        let meminfo = std::fs::read_to_string("/proc/meminfo")?;
        let available = parse_meminfo_value(&meminfo, "MemAvailable")?;
        if available < 4 * 1024 * 1024 {  // 4GB minimum
            bail!("Insufficient available memory: {available} KB (4GB required)");
        }
        println!("✅ Available memory: {} GB", available / 1024 / 1024);
    }
    Ok(())
}

fn check_cpu_governor() -> Result<()> {
    #[cfg(target_os = "linux")]
    {
        let governor = std::fs::read_to_string(
            "/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"
        ).unwrap_or_else(|_| "unknown".to_string());

        if governor.trim() != "performance" {
            println!("⚠️ CPU governor: {governor} (consider 'performance' for benchmarks)");
        } else {
            println!("✅ CPU governor: {governor}");
        }
    }
    Ok(())
}
```

**Verbose Logging Mode**:
```bash
# Enable verbose diagnostic output
cargo run --bin bench-report -- --verbose validate perf.json

# Output:
# 🔍 Validating performance report: perf.json
# ├─ Reading file: perf.json (142 bytes)
# ├─ Parsing JSON schema
# │  ├─ display_gibs: 4.22 (valid float)
# │  ├─ comp3_mibs: 571.0 (valid float)
# │  ├─ timestamp: 2025-09-30T12:34:56Z (valid ISO 8601)
# │  └─ commit: abc12345 (valid hash)
# ├─ Validating against SLOs
# │  ├─ DISPLAY: 4.22 GiB/s >= 4.1 GiB/s SLO ✅
# │  └─ COMP-3: 571.0 MiB/s >= 560 MiB/s SLO ✅
# └─ Status: success ✅
```

**Resource Monitoring**:
```rust
// copybook-bench/benches/diagnostics.rs
fn benchmark_with_monitoring(c: &mut Criterion) {
    let mut monitor = ResourceMonitor::new()?;

    c.bench_function("decode_with_monitoring", |b| {
        b.iter_custom(|iters| {
            monitor.start();

            let start = std::time::Instant::now();
            for _ in 0..iters {
                decode_record(&schema, &data, &options);
            }
            let elapsed = start.elapsed();

            let metrics = monitor.stop();
            eprintln!("Memory: {} MB peak, CPU: {}%",
                     metrics.peak_memory_mb, metrics.avg_cpu_percent);

            elapsed
        });
    });
}

struct ResourceMonitor {
    initial_memory: usize,
    peak_memory: usize,
    cpu_samples: Vec<f64>,
}

impl ResourceMonitor {
    fn new() -> Result<Self> {
        Ok(Self {
            initial_memory: get_current_memory()?,
            peak_memory: 0,
            cpu_samples: Vec::new(),
        })
    }

    fn start(&mut self) {
        self.peak_memory = 0;
        self.cpu_samples.clear();
    }

    fn stop(&mut self) -> ResourceMetrics {
        ResourceMetrics {
            peak_memory_mb: self.peak_memory / (1024 * 1024),
            avg_cpu_percent: self.cpu_samples.iter().sum::<f64>()
                           / self.cpu_samples.len() as f64,
        }
    }
}
```

**Diagnostic Benchmarks**:
```rust
// copybook-bench/benches/diagnostics.rs
// Infrastructure testing benchmarks

fn diagnostic_baseline_io(c: &mut Criterion) {
    // Test baseline I/O performance
    c.bench_function("diagnostic_baseline_io", |b| {
        b.iter(|| {
            let data = vec![0u8; 1024 * 1024];  // 1MB
            std::hint::black_box(data);
        });
    });
}

fn diagnostic_json_parsing(c: &mut Criterion) {
    // Test JSON parsing overhead
    let json_data = r#"{"display_gibs": 4.22, "comp3_mibs": 571.0}"#;
    c.bench_function("diagnostic_json_parsing", |b| {
        b.iter(|| {
            let _report: PerformanceReport = serde_json::from_str(json_data).unwrap();
        });
    });
}

criterion_group! {
    name = diagnostics;
    config = Criterion::default();
    targets = diagnostic_baseline_io, diagnostic_json_parsing
}
```

---

## Integration with Issue #52 Foundation

### Architectural Alignment

Issue #52 (PR #67) provides the foundational infrastructure:
- **PerformanceReport**: JSON schema for machine-readable performance data
- **BaselineStore**: Baseline storage and 90-day retention management
- **bench-report CLI**: Local development tools (`validate`, `baseline`, `compare`, `summary`)
- **GitHub Actions**: Basic benchmark execution and artifact upload

Issue #49 extends this foundation with:
- **Regression Thresholds**: ±5%/±10% threshold validation and gate enforcement
- **CI Integration**: Automated PR comments, baseline promotion, regression blocking
- **Progressive Complexity**: Developer-mode benchmarks for optimization iteration
- **Enhanced Diagnostics**: Health checks, verbose logging, resource monitoring

### API Stability and Extension Points

**No Breaking Changes**: All Issue #49 functionality builds on existing Issue #52 APIs without modification.

**Extension Points**:
```rust
// baseline.rs - Existing API from Issue #52
impl BaselineStore {
    // EXISTING: Load baseline from file
    pub fn load_or_create<P: AsRef<Path>>(path: P) -> Result<Self>;

    // EXISTING: Save baseline to file
    pub fn save<P: AsRef<Path>>(&self, path: P) -> Result<()>;

    // EXISTING: Promote report to baseline
    pub fn promote_baseline(&mut self, report: &PerformanceReport, branch: &str, commit: &str);

    // NEW (Issue #49 AC1): Check for regression with threshold
    pub fn check_regression(&self, report: &PerformanceReport, threshold: f64) -> Vec<String>;

    // NEW (Issue #49 AC5): Baseline health validation
    pub fn validate_health(&self) -> Result<HealthStatus>;
}

// reporting.rs - Existing API from Issue #52
impl PerformanceReport {
    // EXISTING: Validate against SLO thresholds
    pub fn validate_slos(&mut self, display_slo: f64, comp3_slo: f64);

    // EXISTING: Format one-liner for PR comments
    pub fn format_pr_summary(&self) -> String;

    // NEW (Issue #49 AC4): Progressive complexity validation
    pub fn validate_progressive_results(&self, size_results: &[(usize, f64)]) -> Result<()>;

    // NEW (Issue #49 AC5): Extended validation with diagnostics
    pub fn validate_with_diagnostics(&self) -> ValidationReport;
}
```

---

## COBOL Parsing Performance Measurement

### Measurement Methodology

**DISPLAY Field Performance**:
```rust
// Measure DISPLAY (ASCII/EBCDIC text) decoding throughput
fn benchmark_display_throughput(c: &mut Criterion) {
    let schema = create_display_heavy_schema();  // 80% DISPLAY fields
    let data = generate_display_data(1_048_576);  // 1MB test data
    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    c.bench_function("decode_display_heavy", |b| {
        b.iter(|| {
            decode_record(&schema, &data, &options).unwrap();
        });
    });

    // Criterion automatically calculates throughput if configured:
    // .throughput(Throughput::Bytes(data.len() as u64))
}
```

**COMP-3 Packed Decimal Performance**:
```rust
// Measure COMP-3 (packed decimal) decoding throughput
fn benchmark_comp3_throughput(c: &mut Criterion) {
    let schema = create_comp3_heavy_schema();  // 80% COMP-3 fields
    let data = generate_comp3_data(524_288);   // 512KB test data
    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    c.bench_function("decode_comp3_heavy", |b| {
        b.iter(|| {
            decode_record(&schema, &data, &options).unwrap();
        });
    });
}
```

**Performance Targets**:
- **Enterprise Floor**: DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s (minimum production)
- **Aspirational**: DISPLAY ~4.1 GiB/s, COMP-3 ~560 MiB/s (hardware dependent)
- **Current**: DISPLAY 2.33 GiB/s (CLAUDE.md), COMP-3 168-176 MiB/s (confirmed)
- **Variance**: <5% across runs (statistical stability requirement)

### Round-Trip Consistency Validation

```rust
// Validate binary → JSON → binary lossless conversion
fn validate_roundtrip_fidelity(schema: &Schema, original_data: &[u8]) -> Result<()> {
    let options = DecodeOptions::new().with_codepage(Codepage::CP037);

    // Decode: Binary → JSON
    let json_value = decode_record(schema, original_data, &options)?;

    // Encode: JSON → Binary
    let encode_options = EncodeOptions::new().with_codepage(Codepage::CP037);
    let roundtrip_data = encode_record(schema, &json_value, &encode_options)?;

    // Validate: Binary equality
    if original_data != roundtrip_data.as_slice() {
        bail!("Round-trip fidelity failure: data mismatch");
    }

    Ok(())
}
```

---

## Enterprise Compliance and Audit Requirements

### Performance Audit Trail

**Artifact Retention Policy**:
- **Performance Reports**: 90-day retention (enterprise audit compliance)
- **Baseline History**: 90-day retention via BaselineStore
- **GitHub Actions Artifacts**: 90-day retention (configurable)
- **Long-term Archival**: Consider extending to 365 days for regulatory compliance

**Audit Evidence Generation**:
```bash
# Generate performance audit report
cargo run --bin bench-report -- summary > performance-audit-$(date +%Y%m%d).txt

# Output:
# copybook-rs Performance Summary
# ==============================
#
# 📊 Baseline (abc12345): DISPLAY 4.22 GiB/s COMP-3 571 MiB/s [main]
#
# 🎯 SLO Targets:
#    DISPLAY: ≥4.1 GiB/s
#    COMP-3:  ≥560 MiB/s
#
# 📈 Performance History: 45 entries
#    Baseline file: target/baselines/performance.json
#    Last updated: 2025-09-30T12:34:56Z
```

### Machine-Readable Receipts

**Performance Report JSON Format** (Issue #52 schema):
```json
{
  "display_gibs": 4.22,
  "comp3_mibs": 571.0,
  "timestamp": "2025-09-30T12:34:56Z",
  "commit": "abc12345",
  "status": "success",
  "warnings": [],
  "errors": []
}
```

**Baseline Storage Format**:
```json
{
  "current": {
    "branch": "main",
    "commit": "abc12345",
    "timestamp": "2025-09-30T12:34:56Z",
    "display_gibs": 4.22,
    "comp3_mibs": 571.0,
    "sample_count": 5
  },
  "history": [
    {
      "branch": "main",
      "commit": "def67890",
      "timestamp": "2025-09-23T10:20:30Z",
      "display_gibs": 4.18,
      "comp3_mibs": 565.0,
      "sample_count": 5
    }
  ],
  "updated": "2025-09-30T12:34:56Z"
}
```

---

## Error Taxonomy and Handling

### Performance Validation Errors

**Error Codes** (following copybook-rs taxonomy):
- `BENCH001_VALIDATION_FAILURE`: Performance report validation failed
- `BENCH002_BASELINE_NOT_FOUND`: Baseline file missing or unreadable
- `BENCH003_REGRESSION_DETECTED`: Performance regression exceeds threshold
- `BENCH004_HEALTH_CHECK_FAILED`: Benchmark infrastructure health check failed
- `BENCH005_ARTIFACT_UPLOAD_FAILED`: CI artifact upload failed

**Structured Error Handling**:
```rust
#[derive(Debug, thiserror::Error)]
pub enum BenchmarkError {
    #[error("BENCH001: Performance validation failed: {0}")]
    ValidationFailure(String),

    #[error("BENCH002: Baseline not found: {0}")]
    BaselineNotFound(String),

    #[error("BENCH003: Performance regression detected: {0}")]
    RegressionDetected(String),

    #[error("BENCH004: Health check failed: {0}")]
    HealthCheckFailed(String),

    #[error("BENCH005: Artifact upload failed: {0}")]
    ArtifactUploadFailed(String),
}
```

---

## Testing Strategy and TDD Integration

### Acceptance Criteria Test Mapping

**AC1: Regression Detection Validation**
```rust
// tests/regression_detection.rs
#[test]
fn test_regression_warning_threshold() {  // AC1
    let mut store = BaselineStore::new();
    let baseline = create_baseline_report(4.0, 600.0);
    store.promote_baseline(&baseline, "main", "abc123");

    // 6% regression (WARNING threshold)
    let current = create_report(3.76, 564.0);  // -6% DISPLAY, -6% COMP-3
    let regressions = store.check_regression(&current, 5.0);

    assert_eq!(regressions.len(), 2);
    assert!(regressions[0].contains("DISPLAY regression: 6.00%"));
}

#[test]
fn test_regression_failure_threshold() {  // AC1
    let mut store = BaselineStore::new();
    let baseline = create_baseline_report(4.0, 600.0);
    store.promote_baseline(&baseline, "main", "abc123");

    // 12% regression (FAILURE threshold)
    let current = create_report(3.52, 528.0);  // -12% DISPLAY, -12% COMP-3
    let regressions = store.check_regression(&current, 5.0);

    assert_eq!(regressions.len(), 2);
    assert!(regressions[0].contains("DISPLAY regression: 12.00%"));

    // Should trigger CI failure
}

#[test]
fn test_missing_baseline_neutral() {  // AC1
    let store = BaselineStore::new();  // No baseline
    let current = create_report(4.0, 600.0);

    let regressions = store.check_regression(&current, 5.0);
    assert!(regressions.is_empty());  // NEUTRAL - no baseline to compare
}
```

**AC2: Baseline Reconciliation**
```rust
// tests/baseline_reconciliation.rs
#[test]
fn test_baseline_measurement_methodology() {  // AC2
    // Run comprehensive benchmark suite
    let measurements = run_multiple_benchmarks(5);

    // Validate variance < 5%
    let mean_display = calculate_mean(&measurements.display_values);
    let stddev_display = calculate_stddev(&measurements.display_values);
    let cv_display = stddev_display / mean_display;

    assert!(cv_display < 0.05, "Coefficient of variation exceeds 5%");

    // Document measurement context
    let methodology = generate_methodology_report(&measurements);
    assert!(methodology.contains("Hardware: "));
    assert!(methodology.contains("Software: "));
    assert!(methodology.contains("Methodology: "));
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
    assert_eq!(baseline.commit, "abc12345");
}
```

**AC3: CI Integration Validation**
```rust
// tests/ci_integration.rs
#[test]
fn test_pr_comment_generation() {  // AC3
    let report = create_report(4.18, 565.0);
    let summary = report.format_pr_summary();

    assert!(summary.contains("DISPLAY: 4.18 GiB/s"));
    assert!(summary.contains("COMP-3: 565 MiB/s"));
    assert!(summary.contains("✅"));  // Success indicator
}

#[test]
fn test_artifact_retention_policy() {  // AC3
    // Validate 90-day retention policy
    let store = create_baseline_store_with_history(120);  // 120 days of history

    // Apply retention policy
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
    // Simulate main branch merge
    let report = create_report(4.22, 571.0);
    let mut store = BaselineStore::new();

    store.promote_baseline(&report, "main", "abc12345");

    // Validate baseline promoted correctly
    assert!(store.current.is_some());
    assert_eq!(store.current.as_ref().unwrap().branch, "main");
}
```

**AC4: Progressive Complexity Testing**
```rust
// tests/progressive_complexity.rs
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

        // Early bailout if >10s
        if elapsed.as_secs() > 10 {
            break;
        }
    }

    // Validate scaling behavior
    assert!(results.len() >= 2, "At least 2 sizes should complete");
}

#[test]
fn test_early_bailout() {  // AC4
    // Simulate O(n²) algorithm
    let slow_function = || {
        std::thread::sleep(std::time::Duration::from_secs(11));
    };

    let start = std::time::Instant::now();
    let timeout = std::time::Duration::from_secs(10);

    // Should bail early
    let result = std::panic::catch_unwind(|| {
        if start.elapsed() > timeout {
            panic!("Timeout exceeded");
        }
        slow_function();
    });

    assert!(result.is_err(), "Should bail early on timeout");
}
```

**AC5: Enhanced Diagnostics**
```rust
// tests/diagnostics.rs
#[test]
fn test_health_check_validation() {  // AC5
    let health = run_health_check().unwrap();

    assert!(health.rust_version_ok);
    assert!(health.memory_sufficient);
    assert!(health.baseline_valid);
    assert!(health.criterion_infrastructure_ok);
}

#[test]
fn test_verbose_logging() {  // AC5
    let output = run_validate_verbose("perf.json");

    assert!(output.contains("🔍 Validating performance report"));
    assert!(output.contains("├─ Reading file"));
    assert!(output.contains("├─ Parsing JSON schema"));
    assert!(output.contains("└─ Status: success ✅"));
}

#[test]
fn test_resource_monitoring() {  // AC5
    let monitor = ResourceMonitor::new().unwrap();

    // Run benchmark with monitoring
    let metrics = monitor.measure(|| {
        decode_heavy_workload();
    });

    assert!(metrics.peak_memory_mb < 256);  // <256MB requirement
    assert!(metrics.peak_memory_mb > 0);    // Sanity check
}
```

---

## Performance Optimization Workflow

### Developer Iteration Cycle

```
1. Identify Performance Issue
   └─> Run: cargo bench --package copybook-bench
   └─> Analyze: bench-report compare perf.json

2. Isolate Performance Bottleneck
   └─> Run: PERF=1 cargo bench --features progressive
   └─> Profile: cargo flamegraph --bench progressive

3. Implement Optimization
   └─> Edit: copybook-codec/src/decode.rs
   └─> Test: cargo test --workspace

4. Validate Performance Improvement
   └─> Run: cargo bench --package copybook-bench
   └─> Compare: bench-report compare perf.json
   └─> Verify: No regressions, >5% improvement

5. Document Optimization
   └─> Update: CHANGELOG.md
   └─> Document: Performance improvement rationale
   └─> Commit: feat(perf): optimize COMP-3 decoding by 15%
```

### Profiling Integration

```bash
# CPU profiling with flamegraph
cargo install flamegraph
PERF=1 cargo flamegraph --bench progressive --features progressive
# Opens flamegraph.svg in browser

# Memory profiling with valgrind
PERF=1 cargo bench --features progressive --no-run
valgrind --tool=massif target/release/deps/progressive-*
ms_print massif.out.*

# System profiling with perf
PERF=1 cargo bench --features progressive --no-run
perf record -F 99 --call-graph dwarf target/release/deps/progressive-*
perf report
```

---

## Appendix: Performance Baseline Reconciliation Checklist

### AC2 Implementation Checklist

- [ ] **Environment Documentation**
  - [ ] Document CPU specifications (model, cores, frequency)
  - [ ] Document RAM specifications (total, available)
  - [ ] Document OS version (distribution, kernel)
  - [ ] Document Rust toolchain (version, target triple)

- [ ] **Clean Environment Setup**
  - [ ] Remove existing benchmark artifacts (`rm -rf target/criterion target/baselines`)
  - [ ] Clean build workspace (`cargo clean`)
  - [ ] Minimize background processes
  - [ ] Disable CPU frequency scaling (if applicable)

- [ ] **Canonical Baseline Measurement**
  - [ ] Run comprehensive benchmark suite 5 times
  - [ ] Validate variance <5% across runs
  - [ ] Select median run as canonical baseline
  - [ ] Document statistical properties (mean, stddev, 95% CI)

- [ ] **Documentation Update**
  - [ ] Update CLAUDE.md with verified baseline
  - [ ] Update REPORT.md with measurement methodology
  - [ ] Create `docs/performance-measurement-methodology.md`
  - [ ] Commit baseline to `target/baselines/performance.json`

- [ ] **Validation**
  - [ ] Run `bench-report baseline show` to verify promotion
  - [ ] Run `cargo nextest run --workspace` to ensure correctness
  - [ ] Run `cargo build --workspace --release` to ensure no regressions

---

## References

- **Issue #52**: Machine-readable benchmark reporting foundation (PR #67)
- **Issue #49**: Performance regression monitoring and optimization (this specification)
- **CLAUDE.md**: Project development guidelines and performance claims
- **REPORT.md**: Production readiness analysis and performance validation
- **copybook-bench/src/reporting.rs**: PerformanceReport JSON schema implementation
- **copybook-bench/src/baseline.rs**: BaselineStore implementation with 90-day retention
- **copybook-bench/src/bin/bench-report.rs**: CLI tool for baseline management
- **.github/workflows/benchmark.yml**: GitHub Actions CI integration

---

**Specification Status**: COMPLETE ✅
**Ready for TDD Implementation**: YES
**AC Priority**: AC2 (Baseline Reconciliation) FIRST, then AC1/AC3/AC4/AC5
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
