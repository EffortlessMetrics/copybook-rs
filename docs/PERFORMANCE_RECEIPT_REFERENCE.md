<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Performance Receipt Reference Guide

> **🎯 Purpose**: This document provides comprehensive guidance for working with canonical performance receipts in copybook-rs.

## Canonical Receipt Location

**Primary Source**: [`scripts/bench/perf.json`](../scripts/bench/perf.json)

This is the single source of truth for all performance metrics in copybook-rs. All performance documentation must reference this file.

## Receipt Structure

### Required Fields
```json
{
  "format_version": "1.0.0",
  "timestamp": "2025-12-16T17:05:22Z",
  "commit": "85b9f07",
  "build_profile": "release",
  "target_cpu": "native",
  "environment": {
    "os": "Linux",
    "kernel": "6.6.87.2-microsoft-standard-WSL2",
    "cpu_model": "AMD Ryzen 9 9950X3D 16-Core Processor",
    "cpu_cores": 32,
    "wsl2_detected": true
  },
  "benchmarks": [
    {
      "name": "slo_validation/display_heavy_slo_80mbps",
      "mean_ns": 1345093.576837073,
      "bytes_processed": 5000000,
      "mean_mibps": 3545.011041717901
    },
    {
      "name": "slo_validation/comp3_heavy_slo_40mbps",
      "mean_ns": 21969854.869999994,
      "bytes_processed": 600000,
      "mean_mibps": 26.04498724409417
    }
  ],
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
    "host_os": "Linux",
    "ts": "2025-12-16T12:07:38-05:00"
  },
  "integrity": {
    "sha256": "..."
  }
}
```

## Using the Receipt

### For Documentation Updates
When updating performance documentation, use this template:

```markdown
## Performance

**Current Performance**: See [scripts/bench/perf.json](../scripts/bench/perf.json) for canonical measurements

**Latest Receipt**: [COMMIT_HASH] - [YYYY-MM-DD]
- **DISPLAY Throughput**: [X.XX MiB/s] (from receipt)
- **COMP-3 Throughput**: [XXX MiB/s] (from receipt)
- **Environment**: [OS], [Kernel], [CPU], [WSL2: Yes/No]
- **Build Profile**: [release/debug], Target CPU: [native/generic]

**Historical Context**: See [HISTORICAL_PERFORMANCE.md](HISTORICAL_PERFORMANCE.md) for archived targets
```

### For Code References
When referencing performance in code:

```bash
# Extract current performance values
DISPLAY_MIBPS=$(jq -r '.summary.display_mibps' scripts/bench/perf.json)
COMP3_MIBPS=$(jq -r '.summary.comp3_mibps' scripts/bench/perf.json)
COMMIT_HASH=$(jq -r '.commit' scripts/bench/perf.json)

# Validate receipt integrity
./scripts/validate-perf-receipt.sh scripts/bench/perf.json
```

### For Performance Analysis
When analyzing performance trends:

```bash
# Compare multiple receipts
jq -s '.summary.display_mibps' scripts/bench/perf.json* | head -10

# Calculate regression percentage
jq -n '
  def regress_percent(current, baseline):
    if baseline == 0 then return "0";
    ((current - baseline) / baseline * 100
  ;
  
  . as $baseline | .[0] as $current | regress_percent(.; .[0])
' baseline.json current.json
```

## Receipt Validation

### Automated Validation
Use the validation script to ensure receipt integrity:

```bash
# Validate receipt format and integrity
./scripts/validate-perf-receipt.sh scripts/bench/perf.json
```

### Manual Validation Checks
1. **Format Version**: Ensure `format_version` follows semantic versioning
2. **Required Fields**: All top-level fields must be present
3. **Environment Data**: OS, kernel, CPU, WSL2 detection must be captured
4. **Performance Values**: Throughput values must be reasonable (>0, <100000 MiB/s)
5. **Integrity Hash**: SHA-256 must match content

### Common Validation Issues
- **Missing Environment**: WSL2 detection or CPU model not captured
- **Invalid Format**: Missing required fields or wrong data types
- **Integrity Failure**: SHA-256 hash mismatch indicates corruption
- **Unreasonable Values**: Performance numbers outside expected ranges

## Environment Impact on Performance

### WSL2 Considerations
- **Expected Overhead**: 10-30% performance reduction vs native Linux
- **I/O Virtualization**: Storage I/O through 9P or ext4 adds latency
- **CPU Frequency**: May be limited by Windows power settings
- **Memory**: Near-native performance with slight overhead

### Build Profile Impact
- **Release Mode**: Optimized with full LLVM optimizations
- **Debug Mode**: 10-100x slower, not for production measurements
- **Target CPU**: `native` uses host-specific optimizations
- **Generic CPU**: Portable but may miss hardware-specific features

## Historical Context

### Performance Evolution
1. **Pre-2025**: Enterprise targets (4.1 GiB/s, 560 MiB/s) based on theoretical maximums
2. **2025-09-30**: First canonical baseline (205 MiB/s, 58 MiB/s) with standardized methodology
3. **2025-12**: Enhanced receipts with complete environment metadata and integrity validation

### Why Historical Targets Were Higher
- **Measurement Differences**: Early measurements used different benchmark approaches
- **Environment Variations**: Native Linux vs WSL2 performance differences
- **Hardware Changes**: Different CPU architectures and configurations
- **Methodology Standardization**: Current approach provides consistent, reproducible results

## Troubleshooting

### Receipt Not Found
```bash
# Generate new receipt if missing
./scripts/bench-enhanced.sh
```

### Validation Failures
```bash
# Check validation output for specific issues
./scripts/validate-perf-receipt.sh scripts/bench/perf.json 2>&1 | grep -E "❌"
```

### Performance Regressions
```bash
# Compare against baseline to detect regressions
jq -n '
  def regress_percent(current, baseline):
    if baseline == 0 then return "N/A";
    if current < baseline then ((baseline - current) / baseline * 100);
    else "0";
  ;
  
  . as $baseline | .[0] as $current | regress_percent(.; .[0])
' baseline.json current.json
```

## Integration Points

### CI/CD Pipeline
- **Automatic Generation**: Receipts generated in CI with full metadata
- **Artifact Upload**: Receipts uploaded as 90-day artifacts
- **Validation**: Automated format and integrity checks
- **Documentation Updates**: PR comments reference specific commit hashes

### Documentation System
- **Canonical Source**: All performance numbers reference [`scripts/bench/perf.json`](../scripts/bench/perf.json)
- **Historical Archive**: [`HISTORICAL_PERFORMANCE.md`](HISTORICAL_PERFORMANCE.md) for context
- **Governance Policy**: [`PERFORMANCE_GOVERNANCE.md`](PERFORMANCE_GOVERNANCE.md) for rules
- **Template Usage**: [`docs/templates/performance-section-template.md`](templates/performance-section-template.md) for consistency

---

> **Status**: ACTIVE - This is the authoritative reference for canonical performance receipts.
> 
> **Updated**: 2025-12-19 - Established as part of performance truth pipeline implementation.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
