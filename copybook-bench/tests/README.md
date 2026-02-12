# Issue #49 Test Scaffolding Organization

Comprehensive test scaffolding for Performance Regression Monitoring and Benchmark Optimization.

## Test Structure Overview

This directory contains test files organized by acceptance criteria (AC1-AC5) following TDD principles with `// AC:ID` tags for traceability.

### Test Files by Acceptance Criteria

```
copybook-bench/tests/
├── baseline_reconciliation.rs     # AC2 tests (CRITICAL PATH - PRIORITY 1)
├── regression_detection.rs        # AC1 tests (validation of existing functionality)
├── ci_integration.rs              # AC3 tests (validation of existing functionality)
├── progressive_complexity.rs      # AC4 tests (NEW features)
└── diagnostics.rs                 # AC5 tests (NEW features)

copybook-bench/benches/
├── progressive.rs                 # AC4 benchmarks (feature-gated with PERF=1)
└── diagnostics_benches.rs         # AC5 benchmarks (infrastructure testing)
```

## Implementation Priority

**CRITICAL**: AC2 (Baseline Reconciliation) MUST complete FIRST before implementing AC1/AC3 tests.

### Priority Order

1. **AC2 (CRITICAL - PRIORITY 1)**: Baseline Reconciliation
   - Status: NEW implementation required
   - File: `baseline_reconciliation.rs`
   - Purpose: Establish canonical performance baseline
   - Blocks: AC1, AC3
   - Reason: Performance baseline discrepancy requires resolution
     - CLAUDE.md: DISPLAY 2.33 GiB/s, COMP-3 168-176 MiB/s
     - REPORT.md: DISPLAY 66-95 MiB/s, COMP-3 18-25 MiB/s

2. **AC1 (HIGH)**: Regression Detection Validation
   - Status: Validates existing implementation (Issue #52)
   - File: `regression_detection.rs`
   - Purpose: Test existing `BaselineStore::check_regression()` method
   - Dependencies: AC2 canonical baseline

3. **AC3 (HIGH)**: CI Integration Validation
   - Status: Validates existing implementation (Issue #52)
   - File: `ci_integration.rs`
   - Purpose: Test GitHub Actions baseline comparison workflow
   - Dependencies: AC1 regression detection

4. **AC4 (MEDIUM)**: Progressive Complexity Testing
   - Status: NEW implementation
   - Files: `progressive_complexity.rs`, `benches/progressive.rs`
   - Purpose: Developer productivity (performance profiling)
   - Dependencies: None (independent)

5. **AC5 (MEDIUM)**: Enhanced Diagnostics
   - Status: NEW implementation
   - Files: `diagnostics.rs`, `benches/diagnostics_benches.rs`
   - Purpose: Developer productivity (troubleshooting)
   - Dependencies: None (independent)

## Test Tag Usage

All tests include `// AC:ID` tags for traceability to acceptance criteria:

```rust
#[test]
fn test_regression_warning_threshold() {  // AC1
    // Test implementation
}

#[test]
fn test_baseline_measurement_methodology() {  // AC2
    // Test implementation
}

#[test]
fn test_pr_comment_generation() {  // AC3
    // Test implementation
}

#[test]
fn test_progressive_scaling_1kb_to_1mb() {  // AC4
    // Test implementation
}

#[test]
fn test_health_check_validation() {  // AC5
    // Test implementation
}
```

### Finding Tests by AC

```bash
# Find all AC1 tests
grep -r "// AC1" tests/

# Find all AC2 tests
grep -r "// AC2" tests/

# Count tests per AC
grep -r "// AC1" tests/ | wc -l
grep -r "// AC2" tests/ | wc -l
grep -r "// AC3" tests/ | wc -l
grep -r "// AC4" tests/ | wc -l
grep -r "// AC5" tests/ | wc -l
```

## Running Tests

### Run All Tests

```bash
# Run all Issue #49 tests
cargo test --package copybook-bench --tests

# Run specific test file
cargo test --package copybook-bench --test baseline_reconciliation
cargo test --package copybook-bench --test regression_detection
cargo test --package copybook-bench --test ci_integration
cargo test --package copybook-bench --test progressive_complexity
cargo test --package copybook-bench --test diagnostics
```

### Run Tests by Priority

```bash
# AC2 (CRITICAL PATH - Run FIRST)
cargo test --package copybook-bench --test baseline_reconciliation

# AC1 (After AC2)
cargo test --package copybook-bench --test regression_detection

# AC3 (After AC1)
cargo test --package copybook-bench --test ci_integration

# AC4 & AC5 (Can be parallel)
cargo test --package copybook-bench --test progressive_complexity
cargo test --package copybook-bench --test diagnostics
```

### Run Benchmarks

```bash
# AC5 diagnostic benchmarks
cargo bench --bench diagnostics_benches

# AC4 progressive benchmarks (PERF=1 only)
PERF=1 cargo bench --bench progressive --features progressive

# Progressive benchmarks with flamegraph
cargo flamegraph --bench progressive --features progressive
```

## Test File Descriptions

### AC2: baseline_reconciliation.rs (CRITICAL PATH)

**Purpose**: Test baseline measurement methodology and reconciliation procedures.

**Key Tests**:
- `test_baseline_measurement_methodology()` - Statistical variance <5%
- `test_baseline_variance_threshold()` - Variance threshold validation
- `test_baseline_promotion()` - Baseline promotion procedure
- `test_baseline_persistence()` - Baseline save/load across sessions
- `test_baseline_documentation()` - Hardware specification requirements
- `test_baseline_reconciliation_complete()` - CLAUDE.md/REPORT.md consistency

**Specifications**:
- `docs/issue-49-tdd-handoff-package.md#ac2-baseline-reconciliation`
- `docs/reference/benchmark-api-contracts.md#baseline-management-api`

### AC1: regression_detection.rs

**Purpose**: Validate existing regression detection functionality from Issue #52.

**Key Tests**:
- `test_regression_pass_no_change()` - PASS status (<5% variance)
- `test_regression_warning_threshold()` - WARNING status (5-10% variance)
- `test_regression_failure_threshold()` - FAILURE status (>10% variance)
- `test_missing_baseline_neutral()` - NEUTRAL status (missing baseline)
- `test_regression_calculation_accuracy()` - Formula validation
- `test_threshold_boundary_conditions()` - Boundary testing

**Specifications**:
- `docs/reference/benchmark-api-contracts.md#regression-detection-algorithm`

### AC3: ci_integration.rs

**Purpose**: Validate existing CI integration functionality from Issue #52.

**Key Tests**:
- `test_pr_comment_generation()` - PR comment format
- `test_pr_comment_with_regressions()` - Regression warnings in comments
- `test_artifact_retention_policy()` - 90-day retention enforcement
- `test_baseline_promotion_on_main()` - Main branch promotion
- `test_missing_baseline_neutral_ci()` - NEUTRAL status handling
- `test_ci_exit_codes()` - Exit code validation

**Specifications**:
- `docs/reference/benchmark-api-contracts.md#ci-integration-contracts`

### AC4: progressive_complexity.rs

**Purpose**: Test progressive benchmark execution with scaling data sizes.

**Key Tests**:
- `test_progressive_scaling_1kb_to_1mb()` - Progressive scaling (1KB → 1MB)
- `test_early_bailout_threshold()` - 10-second bailout protection
- `test_perf_mode_only_execution()` - PERF=1 feature flag enforcement
- `test_progressive_data_generation()` - Test data generation
- `test_memory_usage_bounded()` - Memory usage <256 MiB

**Specifications**:
- `docs/reference/benchmark-api-contracts.md#progressive-complexity-api`

### AC5: diagnostics.rs

**Purpose**: Test diagnostic utilities and health check validation.

**Key Tests**:
- `test_health_check_validation()` - Environment validation
- `test_health_check_output_format()` - Health check output
- `test_verbose_logging()` - Verbose mode diagnostics
- `test_resource_monitoring()` - Memory/CPU monitoring
- `test_diagnostic_benchmarks()` - Infrastructure testing

**Specifications**:
- `docs/reference/benchmark-api-contracts.md#diagnostic-api-contracts`

## Validation vs New Test Distinction

### Validation Tests (AC1, AC3)

Tests that validate **existing functionality** implemented in Issue #52:

- **AC1**: Tests `BaselineStore::check_regression()` method
- **AC3**: Tests GitHub Actions baseline comparison workflow

These tests do NOT modify existing implementation (backward compatibility).

### New Tests (AC2, AC4, AC5)

Tests for **new functionality** to be implemented:

- **AC2**: Baseline reconciliation procedure (CRITICAL PATH)
- **AC4**: Progressive complexity benchmarks (developer mode)
- **AC5**: Enhanced diagnostics and health checks

## Specification References

### Primary Documents

1. **TDD Handoff Package**: `docs/issue-49-tdd-handoff-package.md`
   - Complete test scaffolding requirements
   - AC priority and dependencies
   - Implementation procedures

2. **Traceability Matrix**: `docs/issue-49-traceability-matrix.md`
   - AC-to-specification mapping
   - Test tag cross-reference
   - Success criteria

3. **API Contracts**: `docs/reference/benchmark-api-contracts.md`
   - Regression detection algorithm
   - Baseline management API
   - CI integration contracts
   - Progressive complexity API
   - Diagnostic API contracts

4. **How-To Guide**: `docs/how-to/benchmark-regression-testing.md`
   - Step-by-step procedures
   - Testing workflows
   - Troubleshooting guidance

## Success Criteria

### AC2 Success Criteria (CRITICAL PATH)

- Hardware specifications documented in `docs/performance-measurement-methodology.md`
- CLAUDE.md and REPORT.md have consistent performance numbers
- Canonical baseline established with <5% variance across 5 runs
- `target/baselines/performance.json` exists with promoted baseline
- All AC2 tests passing

### AC1 Success Criteria

- All regression threshold tests passing (5%, 10%)
- PASS, WARNING, FAILURE, NEUTRAL scenarios validated
- Exit codes correct (0 for PASS/NEUTRAL, 1 for WARNING/FAILURE)
- Regression messages contain correct percentages and values

### AC3 Success Criteria

- PR comments posted/updated automatically
- Baseline promoted on main branch merges
- Artifacts uploaded with 90-day retention
- Missing baseline returns NEUTRAL without errors
- All CI integration tests passing

### AC4 Success Criteria

- Progressive benchmarks execute (1KB → 1MB)
- Early bailout prevents stuck benchmarks (>10s)
- Flamegraph integration working
- Feature flag prevents PR CI execution

### AC5 Success Criteria

- Health check validates environment correctly
- Verbose logging provides detailed diagnostics
- Resource monitoring tracks memory/CPU
- Diagnostic benchmarks execute successfully

## Enterprise Compliance

### Zero Unsafe Code Enforcement

All tests maintain zero unsafe code:

```bash
cargo clippy --package copybook-bench -- -D warnings -D clippy::pedantic
```

### MSRV Compliance

Tests require Rust 1.92+ Edition 2024:

```toml
[package]
rust-version = "1.92"
edition = "2024"
```

### Structured Error Taxonomy

Tests validate error codes (BENCH001-BENCH007):

- `BENCH001_VALIDATION_FAILURE`: Performance validation failed
- `BENCH002_BASELINE_NOT_FOUND`: Baseline missing
- `BENCH003_REGRESSION_DETECTED`: Performance regression
- `BENCH004_HEALTH_CHECK_FAILED`: Health check failed
- `BENCH005_ARTIFACT_UPLOAD_FAILED`: Artifact upload failed
- `BENCH006_JSON_PARSING_FAILED`: JSON parsing error
- `BENCH007_IO_ERROR`: I/O error

## Issue #52 Foundation

Issue #52 (PR #67) already implements AC1 & AC3 core functionality:

- `BaselineStore::check_regression()` method (`copybook-bench/src/baseline.rs`)
- `bench-report compare` CLI command (`copybook-bench/src/bin/bench-report.rs`)
- GitHub Actions baseline comparison workflow (`.github/workflows/benchmark.yml`)

AC1 and AC3 tests validate this existing functionality without modification (backward compatibility).

## Next Steps After Test Scaffolding

After test scaffolding completion:

1. **Route to fixture-builder** to create test fixtures:
   - Synthetic performance reports for AC1 threshold testing
   - Baseline JSON files for AC2 reconciliation testing
   - GitHub Actions workflow mocks for AC3 CI integration testing
   - Progressive benchmark payloads for AC4 testing
   - Diagnostic test fixtures for AC5 testing

2. **Route to tests-finalizer** for final validation:
   - Verify all tests compile successfully
   - Ensure comprehensive coverage of acceptance criteria
   - Validate test organization and documentation
   - Confirm traceability to specifications

## Related Issues

- **Issue #49**: This issue (Performance Regression Monitoring)
- **Issue #52**: Machine-readable Benchmark Reporting (foundation, PR #67)

## Contact

For questions or clarifications about test scaffolding:
- Review `docs/issue-49-tdd-handoff-package.md`
- Check `docs/issue-49-traceability-matrix.md`
- Consult `docs/reference/benchmark-api-contracts.md`
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
