<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-bench Test Fixtures

Comprehensive test fixtures for Issue #49 "Performance regression monitoring and benchmark optimization" following copybook-rs TDD patterns.

## Overview

This directory contains synthetic test data and fixtures for validating the performance regression monitoring system across all 5 Acceptance Criteria (ACs). All fixtures support deterministic testing with known expected values aligned to enterprise copybook-rs performance targets.

**Enterprise Performance Targets:**
- **DISPLAY performance**: ≥80 MB/s (floor), ~4.1 GiB/s (aspirational), **2.33 GiB/s (current baseline)**
- **COMP-3 performance**: ≥40 MB/s (floor), ~560 MiB/s (aspirational), **172 MiB/s (current baseline)**
- **Regression thresholds**: >5% WARNING, >10% FAILURE
- **Variance requirement**: <5% across 5 runs (AC2)
- **Memory target**: <256 MiB steady-state

## Directory Structure

```
test_fixtures/
├── regression/          # AC1: Regression Detection
├── baseline/            # AC2: Baseline Reconciliation (CRITICAL PATH)
├── ci/                  # AC3: CI Integration
├── progressive/         # AC4: Progressive Complexity
├── diagnostics/         # AC5: Enhanced Diagnostics
└── README.md           # This file
```

## AC1: Regression Detection Fixtures (`regression/`)

Synthetic performance reports for regression threshold testing (5% WARNING, 10% FAILURE).

### Files

| File | Description | Expected Values |
|------|-------------|----------------|
| `baseline_reference.json` | Reference baseline (current production) | DISPLAY: 2.33 GiB/s, COMP-3: 172 MiB/s |
| `report_pass_no_change.json` | Identical to baseline (0% regression) | Status: PASS, no warnings |
| `report_warning_5pct.json` | 5.15% DISPLAY regression | Status: WARNING, exit code 0 |
| `report_warning_7pct.json` | 6-7% regression both metrics | Status: WARNING, exit code 0 |
| `report_failure_12pct.json` | 12% regression both metrics | Status: FAILURE, exit code 1 |
| `report_failure_15pct.json` | 15% regression both metrics | Status: FAILURE, exit code 1 |
| `report_neutral_missing_baseline.json` | No baseline available | Status: NEUTRAL, exit code 0 |
| `report_improvement_10pct.json` | 10% performance improvement | Status: PASS, no warnings |
| `report_boundary_exactly_5pct.json` | Exactly 5% regression (boundary) | Status: PASS (5% not exceeded) |
| `report_boundary_exactly_10pct.json` | Exactly 10% regression (boundary) | Status: WARNING (10% not exceeded for FAILURE) |

### Usage in Tests

```rust
use copybook_bench::reporting::PerformanceReport;
use std::fs;

#[test]
fn test_regression_warning() {
    let baseline_json = fs::read_to_string("test_fixtures/regression/baseline_reference.json")?;
    let baseline: PerformanceReport = serde_json::from_str(&baseline_json)?;

    let report_json = fs::read_to_string("test_fixtures/regression/report_warning_7pct.json")?;
    let report: PerformanceReport = serde_json::from_str(&report_json)?;

    // Test regression detection logic
    let mut store = BaselineStore::new();
    store.promote_baseline(&baseline, "main", "dcb9885a");
    let regressions = store.check_regression(&report, 5.0);

    assert_eq!(regressions.len(), 2); // Both DISPLAY and COMP-3
}
```

### Regression Calculation Formula

```
regression_pct = (baseline - current) / baseline * 100.0
```

**Example:** Baseline 2.33 GiB/s → Current 2.17 GiB/s
```
(2.33 - 2.17) / 2.33 * 100 = 6.87% regression
```

## AC2: Baseline Reconciliation Fixtures (`baseline/`)

**CRITICAL PATH**: AC2 must complete FIRST before AC1/AC3 implementation.

Baseline measurement methodology, statistical variance analysis, and hardware specifications.

### Files

| File | Description | Variance CV% | Status |
|------|-------------|--------------|--------|
| `canonical_baseline.json` | Current production baseline | N/A | Established |
| `measurements_run1.json` | 5 measurement runs (variance: 0.63%) | <5% | PASS |
| `measurements_run2.json` | 5 measurement runs (variance: 0.63%) | <5% | PASS |
| `measurements_high_variance.json` | Unacceptable variance (6.58%) | >5% | REJECT |
| `hardware_spec_template.json` | Hardware documentation template | N/A | Template |
| `baseline_with_history.json` | Baseline with historical archive | N/A | Example |

### Statistical Variance Calculation

**Coefficient of Variation (CV):**
```
mean = sum(measurements) / n
variance = sum((x - mean)^2) / n
std_dev = sqrt(variance)
cv_percent = (std_dev / mean) * 100
```

**Acceptance Criteria:** CV < 5%

### Usage in Tests

```rust
use serde_json::Value;

#[test]
fn test_variance_calculation() {
    let data = fs::read_to_string("test_fixtures/baseline/measurements_run1.json")?;
    let measurements: Value = serde_json::from_str(&data)?;

    let stats = &measurements["statistics"];
    let display_cv = stats["display_cv_percent"].as_f64().unwrap();

    assert!(display_cv < 5.0, "Expected CV <5%, got {:.2}%", display_cv);
}
```

### Hardware Specification Template

`hardware_spec_template.json` provides comprehensive hardware documentation structure:
- **CPU**: Model, cores, clock speeds, cache sizes
- **Memory**: Capacity, type, speed, channels
- **Storage**: Type, model, throughput
- **OS**: Name, version, kernel
- **Environment**: Rust version, CPU governor, power profile
- **Benchmark Config**: Criterion settings

## AC3: CI Integration Fixtures (`ci/`)

GitHub Actions workflow mock data, PR comment templates, and artifact retention scenarios.

### Files

| File | Description | Use Case |
|------|-------------|----------|
| `pr_comment_pass.md` | PASS status PR comment template | No regression detected |
| `pr_comment_warning.md` | WARNING status PR comment (5-10%) | Visual regression warning |
| `pr_comment_failure.md` | FAILURE status PR comment (>10%) | PR blocking message |
| `pr_comment_neutral.md` | NEUTRAL status (missing baseline) | First-time PR scenario |
| `github_actions_workflow_mock.json` | Workflow structure mock | CI pipeline testing |
| `artifact_retention_metadata.json` | 90-day artifact retention | Retention policy testing |
| `baseline_promotion_event.json` | Main branch merge event | Baseline promotion workflow |

### PR Comment Format

All PR comments include:
- **Performance Comparison Table**: Baseline vs Current with delta percentages
- **Status Indicators**: ✅ PASS, ⚠️ WARNING, ❌ FAILURE, ℹ️ NEUTRAL
- **Baseline Information**: Branch, commit, date
- **Current Build**: Commit, date
- **Actionable Recommendations**: For WARNING/FAILURE statuses

### Usage in Tests

```rust
#[test]
fn test_pr_comment_generation() {
    let template = fs::read_to_string("test_fixtures/ci/pr_comment_warning.md")?;

    assert!(template.contains("⚠️ **WARNING**"));
    assert!(template.contains("Performance Comparison"));
    assert!(template.contains("-6.87%")); // Expected regression percentage
}
```

## AC4: Progressive Complexity Fixtures (`progressive/`)

Progressive scaling test data (1KB → 10KB → 100KB → 1MB) with bailout scenarios.

### Files

| File | Description | Tier | Expected Duration |
|------|-------------|------|-------------------|
| `copybook_simple.cpy` | Simple COBOL copybook | All | N/A |
| `test_data_1kb.bin` | 1KB test data (16 records) | 1KB | 10ms |
| `progressive_test_config.json` | Tier configuration | All | Specified per tier |
| `memory_usage_profile.json` | Memory consumption analysis | All | <256 MiB total |
| `bailout_test_scenario.json` | Timeout bailout simulation | 100KB | 12.5s (triggers bailout) |
| `throughput_scaling_analysis.json` | Scaling efficiency analysis | All | Linear to super-linear |

### Progressive Tiers

| Tier | Size | Records | Timeout | Memory Limit |
|------|------|---------|---------|--------------|
| 1KB | 1,024 bytes | 16 | 1s | 10 MiB |
| 10KB | 10,240 bytes | 160 | 2s | 20 MiB |
| 100KB | 102,400 bytes | 1,600 | 5s | 50 MiB |
| 1MB | 1,048,576 bytes | 16,384 | 10s | 100 MiB |

### Bailout Conditions

- **Max Duration Per Tier**: 10 seconds
- **Max Memory**: 256 MiB
- **Max Total Runtime**: 60 seconds

If any threshold exceeded, benchmark bails out early to prevent stuck benchmarks.

### Usage in Tests

```rust
#[test]
fn test_progressive_scaling() {
    let config = fs::read_to_string("test_fixtures/progressive/progressive_test_config.json")?;
    let tiers: Value = serde_json::from_str(&config)?;

    for tier in tiers["progressive_tiers"].as_array().unwrap() {
        let size = tier["bytes"].as_u64().unwrap();
        let timeout_ms = tier["timeout_ms"].as_u64().unwrap();

        // Test execution with bailout protection
        // ...
    }
}
```

## AC5: Enhanced Diagnostics Fixtures (`diagnostics/`)

Health check validation, verbose logging, and resource monitoring test data.

### Files

| File | Description | Status | Details |
|------|-------------|--------|---------|
| `health_check_pass.json` | All checks passed | ✅ PASS | Rust 1.92+, 28GB RAM, performance governor |
| `health_check_warning.json` | Warnings present | ⚠️ WARNING | Low memory (1.2GB), powersave governor |
| `health_check_failure.json` | Critical failures | ❌ FAILURE | Old Rust (1.85), insufficient memory (512MB) |
| `verbose_logging_output.json` | Detailed calculation steps | N/A | Regression calculation breakdown |
| `resource_monitoring_snapshot.json` | CPU/memory monitoring | N/A | 92.5% CPU, 124.5 MiB peak memory |
| `diagnostic_benchmark_report.json` | Infrastructure self-test | PASS | All diagnostic checks passed |

### Health Check Components

1. **Rust Version**: >= 1.92.0 required
2. **Available Memory**: > 1 GB required
3. **CPU Governor**: "performance" recommended
4. **Baseline Exists**: Check baseline file presence
5. **Criterion Config**: Validate benchmark configuration

### Status Indicators

- ✅ **PASS**: Component check passed
- ⚠️ **WARNING**: Non-critical issue detected
- ❌ **FAILURE**: Critical issue blocking benchmarks

### Usage in Tests

```rust
#[test]
fn test_health_check() {
    let health_data = fs::read_to_string("test_fixtures/diagnostics/health_check_pass.json")?;
    let health: Value = serde_json::from_str(&health_data)?;

    assert_eq!(health["health_check"]["overall_status"], "pass");

    let checks = health["health_check"]["checks"].as_array().unwrap();
    for check in checks {
        assert_eq!(check["status"], "pass");
    }
}
```

## Fixture Generation Methodology

All fixtures are **synthetic** and follow these principles:

1. **Deterministic Values**: All performance numbers are calculated from documented baseline (DISPLAY: 2.33 GiB/s, COMP-3: 172 MiB/s)
2. **Realistic Variance**: Statistical variance follows real-world patterns (<5% for acceptable, >5% for rejection)
3. **Boundary Testing**: Exact threshold values (5.0%, 10.0%) for regression boundaries
4. **Enterprise Alignment**: Performance targets match copybook-rs CLAUDE.md and REPORT.md specifications
5. **JSON Schema Compliance**: All JSON fixtures match `PerformanceReport` and `BaselineStore` struct definitions

### Regression Percentage Calculations

All regression percentages in fixtures are calculated using:

```
regression_pct = (baseline - current) / baseline * 100
```

**Examples from fixtures:**
- **5.15% regression**: Baseline 2.33 → Current 2.21 = (2.33 - 2.21) / 2.33 * 100 = 5.15%
- **6.87% regression**: Baseline 2.33 → Current 2.17 = (2.33 - 2.17) / 2.33 * 100 = 6.87%
- **15.02% regression**: Baseline 2.33 → Current 1.98 = (2.33 - 1.98) / 2.33 * 100 = 15.02%

## Integration with Test Files

Fixtures are designed for use in `copybook-bench/tests/` test files:

| Test File | Primary Fixtures | ACs Covered |
|-----------|-----------------|-------------|
| `regression_detection.rs` | `regression/` | AC1 |
| `baseline_reconciliation.rs` | `baseline/` | AC2 |
| `ci_integration.rs` | `ci/` | AC3 |
| `progressive_complexity.rs` | `progressive/` | AC4 |
| `diagnostics.rs` | `diagnostics/` | AC5 |

## Fixture Validation

All fixtures validated against:

1. **API Contracts**: `docs/reference/benchmark-api-contracts.md`
2. **Performance Targets**: copybook-rs CLAUDE.md enterprise requirements
3. **JSON Schema**: `PerformanceReport` and `BaselineStore` struct definitions (from `copybook-bench/src/reporting.rs` and `copybook-bench/src/baseline.rs`)
4. **AC Specifications**: `docs/issue-49-tdd-handoff-package.md`

## Testing Best Practices

### Loading Fixtures

```rust
use std::fs;
use serde_json::Value;

// Load and parse JSON fixture
let fixture_path = concat!(env!("CARGO_MANIFEST_DIR"), "/test_fixtures/regression/baseline_reference.json");
let json = fs::read_to_string(fixture_path)?;
let data: Value = serde_json::from_str(&json)?;
```

### Testing Threshold Boundaries

```rust
// Test exactly at threshold (should pass)
let at_threshold = 2.215; // Exactly 5% below 2.33 baseline
let regressions = store.check_regression(&report, 5.0);
assert!(regressions.is_empty());

// Test just over threshold (should trigger warning)
let over_threshold = 2.21; // 5.15% below baseline
let regressions = store.check_regression(&report, 5.0);
assert_eq!(regressions.len(), 1);
```

### Testing Statistical Variance

```rust
let measurements = vec![2.50, 2.48, 2.52, 2.49, 2.51];
let mean = measurements.iter().sum::<f64>() / measurements.len() as f64;
let variance = measurements.iter()
    .map(|x| (x - mean).powi(2))
    .sum::<f64>() / measurements.len() as f64;
let cv = (variance.sqrt() / mean) * 100.0;

assert!(cv < 5.0); // Variance acceptance criterion
```

## Fixture Maintenance

When updating fixtures:

1. **Maintain Consistency**: Keep performance numbers aligned with copybook-rs baseline
2. **Document Changes**: Update this README with new fixture descriptions
3. **Validate JSON**: Ensure all JSON files are valid and parseable
4. **Test Coverage**: Verify all test cases reference appropriate fixtures
5. **Version Control**: Commit fixtures with corresponding test changes

## Traceability

All fixtures support `// AC:ID` tag traceability:

- **AC1**: Regression Detection (`regression/`)
- **AC2**: Baseline Reconciliation (`baseline/`)
- **AC3**: CI Integration (`ci/`)
- **AC4**: Progressive Complexity (`progressive/`)
- **AC5**: Enhanced Diagnostics (`diagnostics/`)

See `docs/issue-49-traceability-matrix.md` for complete traceability mapping.

## Related Documentation

- **Specification**: `docs/issue-49-spec.md`
- **TDD Handoff**: `docs/issue-49-tdd-handoff-package.md`
- **Traceability Matrix**: `docs/issue-49-traceability-matrix.md`
- **API Contracts**: `docs/reference/benchmark-api-contracts.md`
- **Performance Report**: `docs/REPORT.md`
- **Project README**: `CLAUDE.md`

## Questions or Issues

For questions about fixtures or to report fixture-related issues:

1. Review test scaffolding in `copybook-bench/tests/`
2. Consult API contracts in `docs/reference/benchmark-api-contracts.md`
3. Check traceability matrix for AC→test→fixture mapping
4. Verify fixture values match documented baseline (DISPLAY: 2.33 GiB/s, COMP-3: 172 MiB/s)

---

**Generated**: 2025-01-15 for Issue #49 Performance Regression Monitoring
**Baseline**: DISPLAY: 2.33 GiB/s, COMP-3: 172 MiB/s (from CLAUDE.md)
**Test Count**: 62 tests across 5 test files
**Fixture Count**: 35+ files across 5 ACs
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
