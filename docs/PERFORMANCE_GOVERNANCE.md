<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Performance Documentation Governance Policy

> **🎯 Objective**: Establish single canonical performance truth pipeline for copybook-rs to resolve inconsistencies between aspirational targets and actual implementation.

## Policy Statement

All performance claims in copybook-rs documentation MUST reference canonical receipts from [`scripts/bench/perf.json`](../scripts/bench/perf.json). Historical performance targets are archived in [`HISTORICAL_PERFORMANCE.md`](HISTORICAL_PERFORMANCE.md) and MUST NOT be referenced as current metrics.

## Canonical Receipt Requirements

### Required Elements for Performance Claims
Every performance claim must include:
1. **Receipt Reference**: Direct link to specific [`scripts/bench/perf.json`](../scripts/bench/perf.json) with commit hash
2. **Timestamp**: When measurement was taken (from receipt)
3. **Environment Context**: Build profile, target-cpu, OS/kernel, WSL2 detection
4. **Measured Values**: Actual throughput numbers from canonical receipt
5. **Integrity Verification**: SHA-256 hash validation

### Forbidden Elements
Performance documentation MUST NOT contain:
- Unreferenced historical targets (GiB/s, 560+ MiB/s)
- Aspirational goals without receipt backing
- Measurements without environment context
- Performance numbers without commit hash reference

## Documentation Templates

### Performance Section Template
```markdown
## Performance

**Current Performance**: See [`scripts/bench/perf.json`](../scripts/bench/perf.json) for canonical measurements

**Latest Receipt**: [Commit HASH] - [YYYY-MM-DD]
- **DISPLAY Throughput**: [X.XX MiB/s] (from receipt)
- **COMP-3 Throughput**: [XXX MiB/s] (from receipt)
- **Environment**: [OS], [Kernel], [CPU], [WSL2: Yes/No]
- **Build Profile**: [release/debug], Target CPU: [native/generic]

**Historical Context**: See [`HISTORICAL_PERFORMANCE.md`](HISTORICAL_PERFORMANCE.md) for archived targets
```

### Inline Performance References
```markdown
Throughput measured at [X.XX MiB/s](../scripts/bench/perf.json) (commit: [HASH])
```

## Update Process

### When Adding Performance Claims
1. **Generate New Receipt**: Run [`scripts/bench-enhanced.sh`](../scripts/bench-enhanced.sh)
2. **Validate Receipt**: Use [`scripts/validate-perf-receipt.sh`](../scripts/validate-perf-receipt.sh)
3. **Reference Receipt**: Link to specific commit hash in documentation
4. **Include Environment**: Capture build profile, target-cpu, OS details
5. **Update Templates**: Use approved template formats

### When Updating Existing Claims
1. **Check Current Receipt**: Verify latest [`scripts/bench/perf.json`](../scripts/bench/perf.json)
2. **Compare Measurements**: Note significant changes (>5% regression or >10% improvement)
3. **Update References**: Change commit hash and timestamp in documentation
4. **Add Context**: Explain environment differences if applicable

## Validation Requirements

### Pre-Publish Checklist
- [ ] Receipt exists at [`scripts/bench/perf.json`](../scripts/bench/perf.json)
- [ ] Receipt validates with [`scripts/validate-perf-receipt.sh`](../scripts/validate-perf-receipt.sh)
- [ ] Performance claims reference specific commit hash
- [ ] Historical claims properly labeled as outdated
- [ ] Environment context included for measurements
- [ ] No stray performance numbers without receipt references

### Automated Validation
```bash
# Validate receipt before documentation updates
./scripts/validate-perf-receipt.sh scripts/bench/perf.json

# Extract current values for documentation
jq -r '.summary.display_mibps' scripts/bench/perf.json
jq -r '.summary.comp3_mibps' scripts/bench/perf.json
jq -r '.commit' scripts/bench/perf.json
```

## Enforcement Mechanisms

### CI/CD Integration
1. **Receipt Validation**: CI must validate receipt format and integrity
2. **Documentation Checks**: Prevent merges with undocumented performance claims
3. **Reference Validation**: Ensure all performance references point to actual receipts
4. **Historical Quarantine**: Flag any historical target references as errors

### Manual Review Process
1. **Receipt Verification**: Check SHA-256 hash matches content
2. **Reference Tracing**: Verify all performance numbers trace to receipts
3. **Template Compliance**: Ensure documentation follows approved templates
4. **Historical Segregation**: Confirm historical claims are properly quarantined

## Consequences

### Policy Violations
1. **Documentation Blocked**: PRs with undocumented performance claims
2. **Historical References**: Commits referencing historical targets without context
3. **Missing Receipts**: Performance claims without canonical receipt backing
4. **Invalid Formats**: Performance documentation not following templates

### Remediation Steps
1. **Generate Receipt**: Run canonical benchmark process
2. **Validate Receipt**: Ensure format and integrity compliance
3. **Update Documentation**: Use approved templates with receipt references
4. **Remove Violations**: Eliminate undocumented or historical claims

## Governance Roles

### Performance Maintainers
- **Receipt Generation**: Ensure [`scripts/bench-enhanced.sh`](../scripts/bench-enhanced.sh) produces valid receipts
- **Validation Oversight**: Maintain [`scripts/validate-perf-receipt.sh`](../scripts/validate-perf-receipt.sh) accuracy
- **Documentation Review**: Enforce governance policy in documentation updates

### Contributors
- **Reference Receipts**: Always link to specific commit hashes
- **Environment Context**: Include build and environment details
- **Template Usage**: Use approved documentation templates

## Exception Process

### Legitimate Exceptions
1. **Historical Analysis**: Explicitly labeled historical context with clear dates
2. **Environment Comparisons**: Documented differences between environments
3. **Regression Analysis**: Referencing multiple receipts for trend analysis

### Exception Requirements
- **Clear Labeling**: Mark as "HISTORICAL" or "COMPARATIVE"
- **Context Provided**: Explain why historical reference is relevant
- **Receipt Links**: Include canonical receipt references for current data

---

> **Status**: ENFORCED - All performance documentation must follow this policy.
> 
> **Effective**: 2025-12-19 - Applies to all subsequent documentation updates.
> 
> **Owner**: Performance Engineering Team - Responsible for policy maintenance and enforcement.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
