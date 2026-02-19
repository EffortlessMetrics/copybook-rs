<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- Performance Documentation Template -->
<!-- Use this template for all performance sections in copybook-rs documentation -->
<!-- Replace placeholders with actual values from canonical receipts -->

## Performance

**Current Performance**: See [scripts/bench/perf.json](../../scripts/bench/perf.json) for canonical measurements

**Latest Receipt**: [COMMIT_HASH] - [YYYY-MM-DD]
- **DISPLAY Throughput**: [DISPLAY_MIBPS] MiB/s (from receipt)
- **COMP-3 Throughput**: [COMP3_MIBPS] MiB/s (from receipt)
- **Environment**: [OS_NAME], [KERNEL_VERSION], [CPU_MODEL], [WSL2_STATUS]
- **Build Profile**: [BUILD_PROFILE], Target CPU: [TARGET_CPU]

**Historical Context**: See [HISTORICAL_PERFORMANCE.md](HISTORICAL_PERFORMANCE.md) for archived targets

### Performance Methodology

All performance measurements are generated using the canonical benchmark process:

1. **Standardized Environment**: [`scripts/bench-enhanced.sh`](../../scripts/bench-enhanced.sh) captures complete host metadata
2. **Validated Receipts**: [`scripts/validate-perf-receipt.sh`](../../scripts/validate-perf-receipt.sh) ensures format compliance
3. **Integrity Verification**: SHA-256 hash guarantees receipt authenticity
4. **Version Tracking**: Format versioning enables future compatibility

### Receipt Reference

```bash
# Current performance data
jq '.summary' scripts/bench/perf.json

# Receipt integrity verification
scripts/validate-perf-receipt.sh scripts/bench/perf.json
```

### Environment Notes

[ENVIRONMENT_NOTES]

### Performance Trends

[TREND_ANALYSIS]

### Related Documentation

- [Canonical Receipts](../../scripts/bench/perf.json)
- [Historical Performance](HISTORICAL_PERFORMANCE.md)
- [Performance Governance](PERFORMANCE_GOVERNANCE.md)
- [Receipt Schema](../../schemas/perf-receipt-schema.json)

<!-- Template Variables (replace with actual values):
- COMMIT_HASH: Git commit hash from receipt
- YYYY-MM-DD: Date of measurement
- DISPLAY_MIBPS: DISPLAY throughput from receipt
- COMP3_MIBPS: COMP-3 throughput from receipt  
- OS_NAME: Operating system name
- KERNEL_VERSION: Kernel version string
- CPU_MODEL: CPU model identification
- WSL2_STATUS: WSL2 detection result (Yes/No)
- BUILD_PROFILE: Build profile (release/debug)
- TARGET_CPU: Target CPU flags
- ENVIRONMENT_NOTES: Specific environment observations
- TREND_ANALYSIS: Performance trend information
-->
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
