# Historical Performance Claims - ARCHIVED

> **⚠️ IMPORTANT**: This document contains historical performance targets that are no longer current.  
> For the latest performance data, always reference [`scripts/bench/perf.json`](../scripts/bench/perf.json) which contains canonical receipts.

## Overview

This document archives historical performance targets and claims from copybook-rs development to maintain context while ensuring all current documentation references only canonical receipt data.

## Historical Enterprise Targets (OUTDATED)

### Original Enterprise Targets (Pre-2025)
- **DISPLAY Throughput**: ≥4.1 GiB/s
- **COMP-3 Throughput**: ≥560 MiB/s
- **Memory Usage**: <256 MiB steady-state

### Historical Measurements
Various historical measurements documented across different environments and methodologies:

| Date | Source | DISPLAY | COMP-3 | Environment | Notes |
|-------|--------|----------|----------|-------|
| 2025-09-30 | BASELINE_METHODOLOGY.md | 0.20 GiB/s (205 MiB/s) | 58 MiB/s | WSL2, AMD Ryzen 9 9950X3D | First canonical baseline |
| Various | Multiple docs | 2.33 GiB/s | 168-176 MiB/s | Unspecified | Conflicting claims across docs |
| Various | Multiple docs | 66-95 MiB/s | 18-25 MiB/s | Unspecified | Lower measurements in REPORT.md |

## Why Targets Changed

### Measurement Environment Differences
1. **WSL2 Virtualization**: Original targets likely measured on native Linux hardware
2. **Methodology Standardization**: Early measurements used different benchmark approaches
3. **Hardware Variations**: Different CPU architectures and configurations
4. **Build Configuration**: Inconsistent build profiles and optimization flags

### Current Canonical Approach
- **Single Source of Truth**: [`scripts/bench/perf.json`](../scripts/bench/perf.json) with complete metadata
- **Standardized Methodology**: [`scripts/bench-enhanced.sh`](../scripts/bench-enhanced.sh) with environment capture
- **Validation Framework**: [`scripts/validate-perf-receipt.sh`](../scripts/validate-perf-receipt.sh) for integrity checks

## Migration to Canonical Receipts

### From Historical Claims (Deprecated)
```markdown
<!-- DEPRECATED: Do not use these numbers -->
**Performance**: 4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3
```

### To Canonical Receipts (Current)
```markdown
<!-- CURRENT: Always reference canonical receipts -->
**Performance**: See [`scripts/bench/perf.json`](../scripts/bench/perf.json) for latest measurements
- Receipt includes: build profile, target-cpu, OS/kernel, WSL2 detection, CPU model/core count
- Integrity validation via SHA-256 hash
- Format versioning for compatibility
```

## Timeline of Performance Evolution

1. **Pre-2025**: Enterprise targets established (4.1 GiB/s, 560 MiB/s)
2. **2025-09-30**: First canonical baseline with standardized methodology (205 MiB/s, 58 MiB/s)
3. **2025-12**: Enhanced receipt format with complete environment metadata
4. **Current**: Canonical truth pipeline with validation and governance

## Guidance for Documentation Updates

When updating performance documentation:

1. **Always reference canonical receipts**: Link to [`scripts/bench/perf.json`](../scripts/bench/perf.json)
2. **Include receipt metadata**: Mention build profile, environment, and commit hash
3. **Use current measurements**: Never quote historical targets without clear "OUTDATED" labeling
4. **Validate with scripts**: Use [`scripts/validate-perf-receipt.sh`](../scripts/validate-perf-receipt.sh) to verify receipts

## References

- **Canonical Receipts**: [`scripts/bench/perf.json`](../scripts/bench/perf.json)
- **Receipt Schema**: [`schemas/perf-receipt-schema.json`](../schemas/perf-receipt-schema.json)
- **Validation Script**: [`scripts/validate-perf-receipt.sh`](../scripts/validate-perf-receipt.sh)
- **Enhanced Benchmarking**: [`scripts/bench-enhanced.sh`](../scripts/bench-enhanced.sh)

---

> **Status**: ARCHIVED - For reference only. Current performance data lives in canonical receipts.