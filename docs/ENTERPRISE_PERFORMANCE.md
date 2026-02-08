# Enterprise Mode Performance Impact

This document defines performance targets and overhead delta targets for enterprise features in copybook-rs. Enterprise features include audit logging, compliance validation, and security monitoring.

## Overview

Enterprise features add capabilities for regulatory compliance, security auditing, and data lineage tracking. These features are designed to have minimal performance impact while providing comprehensive enterprise-grade functionality.

**Status**: Enterprise features are currently experimental and subject to change.

## Performance Benchmarks

### Benchmark Suite

The enterprise performance benchmark suite measures the impact of each enterprise feature:

- **Baseline**: No enterprise features enabled
- **Audit**: Audit context creation and logging enabled
- **Compliance**: SOX compliance validation enabled
- **Security**: Security monitoring and access validation enabled
- **Combined**: All enterprise features enabled together

### Running Benchmarks

Run enterprise benchmarks locally:

```bash
# Run all enterprise benchmarks
just bench-enterprise

# Run enterprise SLO validation benchmarks
just bench-enterprise-slo

# Run baseline benchmarks only
just bench-baseline

# Compare baseline vs enterprise performance
just bench-compare baseline.json enterprise.json
```

## Overhead Delta Targets

### Individual Feature Targets

| Feature | Target Overhead | Rationale |
|---------|-----------------|------------|
| **Audit** | < 2% | Lightweight context creation should have minimal impact |
| **Compliance** | < 3% | Policy validation adds moderate overhead |
| **Security** | < 1% | Access checks should be very fast |
| **Combined** | < 5% | All features together should stay under 5% |

### Target Justification

- **Audit (< 2%)**: Audit context creation uses thread-local counters and lightweight ID generation. The primary overhead comes from metadata tracking, which is designed to be optional and minimal.

- **Compliance (< 3%)**: Compliance validation involves policy checks and validation logic. The SOX profile is used as a representative policy for benchmarking.

- **Security (< 1%)**: Security monitoring primarily involves access validation checks that should be fast and cache-friendly.

- **Combined (< 5%)**: The combined target allows for some additive overhead while ensuring enterprise features don't significantly impact throughput.

## Interpreting Overhead Results

### Overhead Calculation

Overhead is calculated as the percentage increase in execution time compared to baseline:

```
Overhead % = ((Enterprise Time - Baseline Time) / Baseline Time) * 100
```

### Status Indicators

| Overhead | Status | Meaning |
|-----------|---------|---------|
| < Target | ✅ Excellent | Feature overhead is within acceptable limits |
| Target + 1% | ⚠️ Warning | Slightly over target, monitor for trends |
| Target + 2% | ⚠️ Warning | Over target, investigation recommended |
| Target + 3%+ | ❌ Critical | Significantly over target, optimization needed |

### Example Interpretation

```
| Feature | Overhead | Target | Status |
|---------|----------|--------|--------|
| Audit | 1.2% | < 2% | ✅ Excellent |
| Compliance | 2.8% | < 3% | ⚠️ Warning |
| Security | 0.9% | < 1% | ✅ Excellent |
| Combined | 4.5% | < 5% | ✅ Excellent |
```

In this example:
- Audit and Security are performing well within targets
- Compliance is slightly over target and should be monitored
- Combined overhead is acceptable

## Promotion Path: Advisory to Blocking

### Phase 1: Advisory Mode (Current)

- **Status**: Enterprise overhead warnings are advisory only
- **Behavior**: PR comments show overhead warnings but do not block merges
- **Duration**: Initial data collection period (4-6 weeks)
- **Goal**: Establish baseline overhead measurements and identify trends

### Phase 2: Warning Mode

- **Trigger**: After 4-6 weeks of stable measurements with consistent results
- **Status**: Overhead warnings are highlighted but still non-blocking
- **Behavior**: PR comments show prominent warnings for overhead > target + 2%
- **Duration**: 2-4 weeks for optimization work
- **Goal**: Encourage optimization before enforcement

### Phase 3: Blocking Mode

- **Trigger**: After optimization period with clear baselines established
- **Status**: Overhead violations block PRs
- **Behavior**: CI fails if overhead exceeds target + 2%
- **Exceptions**: Documented performance regressions with investigation plans
- **Goal**: Maintain enterprise performance guarantees

### Promotion Criteria

Promotion from Advisory to Blocking requires:

1. **Stable Baseline**: At least 20 benchmark runs with consistent results
2. **Clear Targets**: Overhead targets validated across different workloads
3. **Optimization Path**: Known optimization strategies for features exceeding targets
4. **Team Agreement**: Consensus from maintainers on enforcement strategy

## CI Integration

### Performance Workflow

The `.github/workflows/perf.yml` workflow runs enterprise benchmarks:

- **On Schedule**: Weekly runs on main branch
- **On PR**: When `run-perf` label is added
- **On Push**: On version tags (v*)

### PR Comments

Performance PR comments include:

1. **Baseline Comparison**: DISPLAY and COMP-3 throughput comparison
2. **Enterprise Overhead**: Per-feature overhead with status indicators
3. **Overall Status**: Pass/Warning based on overhead targets

### Example PR Comment

```
## 📊 Performance Receipt

| Metric | Baseline | PR | Delta |
|--------|----------|-----|-------|
| DISPLAY | 85.2 MiB/s | 83.1 MiB/s | -2.5% ⚠️ |
| COMP-3 | 42.3 MiB/s | 41.8 MiB/s | -1.2% ✅ |

### 🔒 Enterprise Overhead

| Feature | Overhead | Target | Status |
|---------|----------|--------|--------|
| Audit | 1.2% | < 2% | ✅ |
| Compliance | 2.8% | < 3% | ⚠️ |
| Security | 0.9% | < 1% | ✅ |
| Combined | 4.5% | < 5% | ✅ |

**Status**: ⚠️ Warning
```

## Monitoring and Trends

### Metrics to Track

- **Individual Feature Overhead**: Track each feature's overhead over time
- **Combined Overhead**: Monitor total enterprise overhead
- **Throughput Impact**: Measure actual throughput degradation
- **Memory Usage**: Track RSS impact of enterprise features

### Trend Analysis

- **Increasing Overhead**: Investigate recent changes that may have added overhead
- **Decreasing Overhead**: Validate optimization improvements
- **Sudden Spikes**: Identify potential regressions or measurement issues

### Alerting

Alerts are configured for:

- Overhead exceeding target by > 2%
- Sudden overhead increase > 1% between runs
- Combined overhead approaching 5% limit

## Optimization Guidance

### Common Optimization Areas

1. **Audit Context Creation**
   - Use lightweight context creation methods
   - Reuse context objects where possible
   - Minimize metadata tracking in hot paths

2. **Compliance Validation**
   - Cache validation results
   - Use async validation where appropriate
   - Optimize policy evaluation logic

3. **Security Monitoring**
   - Cache access validation results
   - Minimize lock contention
   - Use fast path for common cases

### Performance Profiling

When investigating overhead:

1. Run benchmarks with profiling: `cargo bench -- --profile-time 5`
2. Identify hot paths in enterprise code
3. Optimize critical sections
4. Validate improvements with benchmark comparison

## References

- [Enterprise Audit System Spec](../copybook-core/src/audit/README.md)
- [Performance Benchmarks](../copybook-bench/benches/)
- [CI Performance Workflow](../.github/workflows/perf.yml)
- [Justfile Bench Targets](../justfile)

## Changelog

### 2026-02-07
- Initial enterprise performance targets defined
- Benchmark suite created
- CI integration established
- Advisory mode enabled
