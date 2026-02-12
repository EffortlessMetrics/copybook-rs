# Performance Receipts Documentation

> **🎯 Purpose**: This document describes how performance receipts are generated, stored, and used for regression detection in copybook-rs.

## Overview

Performance receipts are JSON artifacts generated from benchmark runs that capture throughput metrics, environment metadata, and integrity hashes. They serve as the canonical source of truth for performance measurements and enable automated regression detection.

## Receipt Generation

### Local Development

Run benchmarks locally to generate performance receipts:

```bash
# Run benchmarks and generate receipt
just perf

# Or using the bench script directly
bash scripts/bench.sh

# Or with custom benchmark filter
BENCH_FILTER=all just perf
```

This generates [`scripts/bench/perf.json`](../scripts/bench/perf.json) with the following structure:

```json
{
  "timestamp": "2025-12-16T17:05:22Z",
  "commit": "85b9f07",
  "toolchain": "cargo bench (criterion)",
  "status": "pass",
  "display_mibps": 3545.011041717901,
  "comp3_mibps": 26.04498724409417,
  "benchmarks": [...],
  "summary": {
    "display_mibps": 3545.011041717901,
    "comp3_mibps": 26.04498724409417,
    "max_rss_mib": 4,
    "p50_mibps": 29.437994496913237,
    "p90_mibps": 3794.040988865222,
    "p99_mibps": 3931.600474917219,
    "host_cpu": "AMD Ryzen 9 9950X3D 16-Core Processor",
    "host_cores": 32,
    "host_kernel": "6.6.87.2-microsoft-standard-WSL2",
    "host_os": "Linux"
  }
}
```

### CI/CD Generation

The [perf workflow](../.github/workflows/perf.yml) automatically generates receipts on:

- **Pull requests**: When labeled with `run-perf`
- **Push to main**: On version tags (`v*`)
- **Schedule**: Nightly at 03:00 UTC
- **Manual**: Via workflow_dispatch

## Artifact Storage

### GitHub Actions Artifacts

Performance artifacts are uploaded and retained:

| Artifact | Retention | Contents |
|----------|-----------|----------|
| `perf-json-{sha}` | 90 days | Machine-readable performance receipt |
| `criterion-data-{sha}` | 30 days | Raw criterion benchmark outputs |

### Accessing Artifacts

Artifacts can be downloaded from the workflow run page or via the GitHub CLI:

```bash
# List artifacts from a workflow run
gh run view <run-id> --log

# Download an artifact
gh run download <run-id> -n perf-json-<sha>
```

## PR Comment Automation

### How It Works

When a PR is labeled with `run-perf`, the workflow:

1. **Fetches baseline**: Downloads the most recent perf receipt from the main branch
2. **Runs benchmarks**: Executes the benchmark suite
3. **Compares results**: Calculates delta percentages between baseline and PR
4. **Posts comment**: Updates or creates a PR comment with the comparison

### PR Comment Format

```
## 📊 Performance Receipt

| Metric | Baseline | PR | Delta |
|--------|----------|-----|-------|
| DISPLAY | 3545.01 MiB/s | 3600.12 MiB/s | +1.55% ✅ |
| COMP-3 | 26.04 MiB/s | 25.80 MiB/s | -0.92% ✅ |

Status: ✅ Pass

Commit: 85b9f07
Workflow: [View Details](https://github.com/.../actions/runs/12345)

---
*This is an automated comment from the performance workflow. Artifacts are retained for 90 days.*
```

### Security Posture

- **Internal PRs**: Full PR comment automation enabled
- **Fork PRs**: Artifacts are still uploaded, but PR comments are skipped (no write access to fork PRs)

## Regression Detection

### Thresholds

The system operates in **advisory mode** with the following thresholds:

| Metric | Warn Threshold | Fail Threshold | Status |
|--------|----------------|----------------|--------|
| DISPLAY | >5% regression | Disabled | Advisory |
| COMP-3 | >5% regression | Disabled | Advisory |

### Status Interpretation

| Status | Meaning | Action |
|--------|---------|--------|
| ✅ Pass | All metrics within threshold | No action required |
| ⚠️ Warning | One or more metrics exceeded warn threshold | Review changes, consider optimization |
| ❌ Fail | Not currently enabled | N/A |

### Promotion Path

The system is designed to promote from advisory to blocking:

#### Phase 1: Advisory (Current)
- **Warn threshold**: ±5% regression
- **Fail threshold**: Disabled
- **Behavior**: PR comments show warnings, but do not block merges
- **Purpose**: Build understanding of natural variance

#### Phase 2: Blocking (Future)
- **Warn threshold**: ±3% regression
- **Fail threshold**: >10% regression
- **Behavior**: PR checks fail on regressions exceeding fail threshold
- **Trigger**: After sufficient data collection and variance understanding

#### Phase 3: PR-Gated (Future)
- **Integration**: Perf workflow added to PR-gated CI checks
- **Behavior**: PRs cannot merge without passing perf checks
- **Trigger**: After Phase 2 proves stable and reliable

## Local Comparison

Compare two performance receipts locally:

```bash
# Compare baseline against PR
just perf-compare baseline-perf.json pr-perf.json
```

Example output:
```
Comparing performance receipts:
  Baseline: baseline-perf.json
  PR:       pr-perf.json

| Metric | Baseline | PR | Delta |
|--------|----------|-----|-------|
| DISPLAY | 3545.01 MiB/s | 3600.12 MiB/s | +1.55% |
| COMP-3 | 26.04 MiB/s | 25.80 MiB/s | -0.92% |

Status: ✅ Pass
```

## Interpreting Results

### Delta Calculation

Delta percentage is calculated as:

```
delta = ((pr_value - baseline_value) / baseline_value) * 100
```

- **Positive delta**: Performance improvement
- **Negative delta**: Performance regression

### Common Scenarios

| Scenario | Delta | Interpretation |
|----------|-------|----------------|
| +10% | Improvement | Significant performance gain |
| +2% | Improvement | Minor improvement, within noise |
| -2% | Regression | Minor regression, likely noise |
| -10% | Regression | Significant regression, investigate |

### Environmental Factors

Performance can vary due to:

- **WSL2**: 10-30% overhead vs native Linux
- **CPU frequency**: Power management settings
- **System load**: Background processes
- **Compiler version**: Different optimization levels

Always compare receipts from the same environment type (GitHub Actions runners for canonical comparisons).

## Validation

### Automated Validation

Validate receipt structure and integrity:

```bash
./scripts/validate-perf-receipt.sh scripts/bench/perf.json
```

### Manual Validation

Check required fields:
- `format_version`: Semantic version (e.g., "1.0.0")
- `timestamp`: ISO 8601 datetime
- `commit`: Git commit hash (7-40 characters)
- `summary.display_mibps`: DISPLAY throughput
- `summary.comp3_mibps`: COMP-3 throughput
- `summary.max_rss_mib`: Maximum memory usage
- `integrity.sha256`: SHA-256 hash of receipt content

## Troubleshooting

### Baseline Not Found

**Error**: "No baseline found" in PR comment

**Solution**: This is expected for the first perf run on a branch. Subsequent runs will have a baseline from the previous successful run on main.

### Validation Failures

**Error**: Receipt validation fails

**Solution**: Regenerate the receipt:

```bash
just perf
./scripts/validate-perf-receipt.sh scripts/bench/perf.json
```

### Unexpected Regressions

**Symptom**: Large negative deltas that don't match expected changes

**Investigation**:
1. Check if environment changed (WSL2 vs native)
2. Verify benchmark code wasn't accidentally modified
3. Review system load during benchmark
4. Check for compiler version changes

## Related Documentation

- [Performance Receipt Reference](PERFORMANCE_RECEIPT_REFERENCE.md) - Detailed receipt structure
- [Performance Governance](PERFORMANCE_GOVERNANCE.md) - Performance policy and rules
- [Historical Performance](HISTORICAL_PERFORMANCE.md) - Performance evolution over time
- [Perf Receipt Schema](../schemas/perf-receipt-schema.json) - JSON schema validation
