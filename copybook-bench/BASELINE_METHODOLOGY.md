<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Baseline Measurement Methodology

This document describes the canonical procedure for establishing and maintaining performance baselines for copybook-rs benchmark regression testing (Issue #49).

## Overview

Performance baselines provide a stable reference point for detecting performance regressions across code changes. The baseline measurement procedure ensures statistical reliability through multiple independent runs, variance analysis, and standardized measurement conditions.

## Baseline Measurement Procedure

### Prerequisites

1. **Clean Environment**:
   - No background processes consuming significant CPU/memory
   - System rebooted or idle for 5+ minutes
   - Consistent power settings (performance mode recommended)

2. **Required Tools**:
   - Rust 1.92+ toolchain
   - Cargo with release profile configuration
   - Criterion benchmark harness (v0.7+)
   - `bench-report` CLI tool (copybook-bench package)

3. **Hardware Documentation**:
   - Document hardware specifications before measurement
   - See `HARDWARE_SPECS.md` template for required fields
   - Record OS version, kernel version, Rust version

### Step 1: Clean Build Environment

Remove all previous build artifacts to ensure consistent compilation:

```bash
cd /path/to/copybook-rs
cargo clean
cargo build --release --workspace
```

**Validation**: Verify clean build completed without errors and produced optimized binaries.

### Step 2: Execute 5 Independent Measurement Runs

Run the benchmark suite 5 times with consistent configuration:

```bash
# Run 1
PERF=1 cargo bench --package copybook-bench --bench decode_performance -- --save-baseline run1

# Run 2
PERF=1 cargo bench --package copybook-bench --bench decode_performance -- --save-baseline run2

# Run 3
PERF=1 cargo bench --package copybook-bench --bench decode_performance -- --save-baseline run3

# Run 4
PERF=1 cargo bench --package copybook-bench --bench decode_performance -- --save-baseline run4

# Run 5
PERF=1 cargo bench --package copybook-bench --bench decode_performance -- --save-baseline run5
```

**Duration**: Each run takes approximately 5-10 minutes.

**Environment Variables**:
- `PERF=1`: Enables performance mode with optimized settings

**Criterion Settings**:
- Warmup time: 3 seconds
- Measurement time: 10 seconds
- Sample size: 100 iterations
- Confidence level: 95%

### Step 3: Extract and Analyze Results

Extract performance metrics from Criterion output:

```bash
# Extract DISPLAY-heavy benchmark results (10000 records)
for run in {1..5}; do
    cat target/criterion/decode_display_heavy/single_threaded/10000/run${run}/estimates.json \
        | jq '.mean.point_estimate'
done

# Extract COMP-3-heavy benchmark results (10000 records)
for run in {1..5}; do
    cat target/criterion/decode_comp3_heavy/single_threaded/10000/run${run}/estimates.json \
        | jq '.mean.point_estimate'
done
```

### Step 4: Calculate Statistical Variance

Compute mean, standard deviation, and coefficient of variation (CV):

```python
import statistics

# Example measurements (nanoseconds)
display_times = [21350634.96, 23961000.00, 23200000.00, 23620000.00, 24070000.00]

# Calculate statistics
mean = statistics.mean(display_times)
stdev = statistics.stdev(display_times)
cv = (stdev / mean) * 100

print(f"Mean: {mean/1e6:.2f} ms")
print(f"Std Dev: {stdev/1e6:.2f} ms")
print(f"CV: {cv:.2f}%")

# Validate CV < 5% threshold
assert cv < 5.0, f"Variance {cv:.2f}% exceeds 5% threshold"
```

**Acceptance Criteria**:
- Coefficient of Variation (CV) < 5%
- All 5 runs completed successfully
- No outliers beyond 2 standard deviations

**WSL2 Exception**: CV up to 8% is acceptable for WSL2 environments due to virtualization overhead. Document this in `HARDWARE_SPECS.md`.

### Step 5: Promote Canonical Baseline

Promote the validated measurements to the canonical baseline:

```bash
# Create baseline JSON
cat > /tmp/baseline.json << EOF
{
  "current": {
    "branch": "main",
    "commit": "$(git rev-parse --short HEAD)",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "display_gibs": $(python3 -c "print(round(205.56 / 1024, 2))"),
    "comp3_mibs": 57.83,
    "sample_count": 5
  },
  "history": [],
  "updated": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

# Copy to baseline location
mkdir -p target/baselines
cp /tmp/baseline.json target/baselines/performance.json

# Verify baseline promotion
cargo run --bin bench-report -p copybook-bench -- baseline show
```

**Expected Output**:
```
📊 Baseline (xxxxxxxx): DISPLAY 0.20 GiB/s COMP-3 58 MiB/s [main]
```

### Step 6: Update Documentation

Update the following documentation files with verified baseline numbers:

1. **CLAUDE.md** - Performance section:
   - Update "Targets vs Achieved" with measured throughput
   - Add baseline establishment date and commit
   - Reference hardware specifications

2. **docs/REPORT.md** - Performance Snapshot:
   - Update Executive Summary with baseline numbers
   - Replace "Current Measurements" with "Baseline Measurements"
   - Add measurement environment details

3. **HARDWARE_SPECS.md** - Baseline Update History:
   - Add entry to baseline update table
   - Update performance baselines section
   - Document variance analysis results

### Step 7: Commit Baseline Documentation

Commit the baseline updates to version control:

```bash
git add CLAUDE.md docs/REPORT.md copybook-bench/HARDWARE_SPECS.md
git add copybook-bench/BASELINE_METHODOLOGY.md
git commit -m "feat(bench): establish canonical baseline for Issue #49 AC2

- DISPLAY-heavy: 205 MiB/s (5.04% CV across 5 runs)
- COMP-3-heavy: 58 MiB/s (7.79% CV across 5 runs)
- Measurement environment: WSL2, AMD Ryzen 9 9950X3D
- Baseline commit: $(git rev-parse --short HEAD)

See copybook-bench/HARDWARE_SPECS.md for complete specifications.
See copybook-bench/BASELINE_METHODOLOGY.md for measurement procedures.

Resolves: #49 AC2"
```

### Step 8: Archive Baseline to CI Artifacts

For GitHub Actions integration (Issue #49 AC3):

```yaml
# .github/workflows/benchmark.yml
- name: Archive baseline measurements
  uses: actions/upload-artifact@v3
  with:
    name: performance-baseline
    path: target/baselines/performance.json
    retention-days: 90
```

## Baseline Maintenance

### When to Re-establish Baseline

Re-measure and promote a new baseline when:

1. **Major Performance Improvements**: Code changes that improve throughput by >10%
2. **Algorithm Changes**: Significant changes to encoding/decoding algorithms
3. **Dependency Updates**: Major version updates of performance-critical dependencies
4. **Hardware Changes**: New measurement environment or infrastructure
5. **Scheduled Review**: Quarterly baseline validation (every 3 months)

### Baseline Promotion Criteria

A new baseline should only be promoted if:

- ✅ All 5 measurement runs completed successfully
- ✅ Variance (CV) meets threshold (<5% for native Linux, <8% for WSL2)
- ✅ No regressions detected against previous baseline (unless intentional)
- ✅ Hardware specifications documented
- ✅ Code review approved for baseline promotion

### Baseline History Retention

- Current baseline: Always retained in `target/baselines/performance.json`
- Previous baselines: Archived to `history` array (retain last 10)
- CI artifacts: Retained for 90 days
- Git history: Full baseline history in `HARDWARE_SPECS.md` table

## Regression Detection Workflow

Once baseline is established, use it to detect regressions:

```bash
# Run current benchmarks
PERF=1 cargo bench -p copybook-bench --bench decode_performance

# Generate performance report JSON
cargo run --bin bench-report -p copybook-bench -- validate perf.json

# Compare against baseline
cargo run --bin bench-report -p copybook-bench -- compare perf.json
```

`bench-report compare` flags regressions when a metric is >5% slower than the stored baseline (advisory output; CI gating is handled separately).

## Statistical Methodology

### Measurement Approach

- **Sample Size**: n=5 independent runs provides statistical power for detecting medium-large effect sizes
- **Warmup Period**: 3 seconds eliminates cold-start effects and JIT warmup
- **Measurement Duration**: 10 seconds per benchmark provides stable measurements
- **Confidence Level**: 95% confidence intervals for mean estimates

### Variance Analysis

Coefficient of Variation (CV) is used as the primary variance metric:

```
CV = (Standard Deviation / Mean) × 100%
```

**Interpretation**:
- CV < 3%: Excellent reproducibility
- CV 3-5%: Good reproducibility (acceptable)
- CV 5-8%: Moderate reproducibility (WSL2 acceptable)
- CV > 8%: Poor reproducibility (investigate measurement environment)

### Outlier Detection

Measurements beyond 2 standard deviations from the mean are flagged as outliers. If >20% of measurements are outliers, re-run the baseline procedure with improved environmental control.

## Troubleshooting

### High Variance (CV > 5%)

**Symptoms**: Coefficient of variation exceeds 5% threshold

**Causes**:
- Background processes consuming CPU/memory
- Thermal throttling during measurement
- Inconsistent power settings
- WSL2 virtualization overhead

**Solutions**:
1. Close all unnecessary applications
2. Ensure system is idle before measurement
3. Verify CPU frequency scaling is disabled
4. For WSL2: Accept CV up to 8% and document in specs
5. Consider native Linux environment for production baselines

### Inconsistent Results Across Runs

**Symptoms**: Large variance between individual runs (>20% difference)

**Causes**:
- Non-deterministic code execution
- I/O contention
- Memory pressure

**Solutions**:
1. Verify benchmarks are pure computation (no I/O)
2. Ensure sufficient memory available (>8 GB free)
3. Run on dedicated hardware without other workloads

### Baseline Promotion Fails

**Symptoms**: `bench-report baseline promote` command fails

**Causes**:
- Invalid JSON format
- Missing required fields
- Insufficient permissions

**Solutions**:
1. Validate JSON with `jq .` before promotion
2. Ensure all required fields present (branch, commit, timestamp, metrics)
3. Check file permissions on `target/baselines/` directory

## References

- Issue #49: Benchmark Regression Testing Implementation
- AC2: Performance Baseline Reconciliation
- `HARDWARE_SPECS.md`: Hardware configuration and measurement environment
- `docs/issue-49-tdd-handoff-package.md`: TDD specification and acceptance criteria
- Criterion.rs documentation: https://bheisler.github.io/criterion.rs/book/

## Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2025-09-30 | 1.0 | Initial methodology documentation (Issue #49 AC2) |
