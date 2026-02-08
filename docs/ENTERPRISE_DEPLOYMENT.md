# Enterprise Deployment Guide

This guide provides comprehensive instructions for deploying copybook-rs in production environments with enterprise features enabled. It covers feature flag defaults, audit/monitoring configuration, compliance policies, performance considerations, determinism guarantees, and safe upgrade practices.

## Table of Contents

- [Feature Flag Defaults](#feature-flag-defaults)
- [Audit Configuration](#audit-configuration)
- [Monitoring Configuration](#monitoring-configuration)
- [Compliance Configuration](#compliance-configuration)
- [Performance Caveats](#performance-caveats)
- [Determinism Guarantees](#determinism-guarantees)
- [Safe Upgrade Practices](#safe-upgrade-practices)

## Feature Flag Defaults

### Default Values by Environment

| Feature Flag | Development | CI/Staging | Production | Description |
|--------------|-------------|------------|------------|-------------|
| `audit_system` | Disabled | Enabled | Enabled | Enable audit system for compliance tracking |
| `sox_compliance` | Disabled | Enabled | Enabled | Enable SOX compliance validation |
| `hipaa_compliance` | Disabled | Disabled | Optional | Enable HIPAA compliance validation |
| `gdpr_compliance` | Disabled | Enabled | Enabled | Enable GDPR compliance validation |
| `pci_dss_compliance` | Disabled | Disabled | Optional | Enable PCI DSS compliance validation |
| `security_monitoring` | Disabled | Enabled | Enabled | Enable security monitoring integration |
| `verbose_logging` | Enabled | Disabled | Disabled | Enable verbose logging with detailed diagnostics |
| `diagnostic_output` | Enabled | Disabled | Disabled | Enable diagnostic output for troubleshooting |
| `lru_cache` | Enabled | Enabled | Enabled | Enable LRU cache for parsed copybooks |
| `advanced_optimization` | Disabled | Disabled | Enabled | Enable advanced optimization mode (SIMD, vectorization) |
| `parallel_decode` | Disabled | Enabled | Enabled | Enable parallel decoding for large files |
| `zero_copy` | Disabled | Disabled | Enabled | Enable zero-copy parsing where possible |

### Overriding Defaults

#### Environment Variables

Override feature flags via environment variables:

```bash
# Enable specific enterprise features
export COPYBOOK_FF_AUDIT_SYSTEM=1
export COPYBOOK_FF_SOX_COMPLIANCE=1
export COPYBOOK_FF_GDPR_COMPLIANCE=1
export COPYBOOK_FF_SECURITY_MONITORING=1

# Disable debug features in production
export COPYBOOK_FF_VERBOSE_LOGGING=0
export COPYBOOK_FF_DIAGNOSTIC_OUTPUT=0
```

#### Configuration Files

Create a production configuration file:

```toml
# production-flags.toml
[feature_flags]
enabled = [
  "audit_system",
  "sox_compliance",
  "gdpr_compliance",
  "security_monitoring",
  "lru_cache",
  "advanced_optimization",
  "parallel_decode"
]
disabled = [
  "verbose_logging",
  "diagnostic_output",
  "profiling",
  "memory_tracking"
]
```

Use the configuration file:

```bash
copybook decode --feature-flags-config /etc/copybook/production-flags.toml \
  /data/copybooks/schema.cpy \
  /data/input/data.bin \
  --output /data/output/data.jsonl
```

#### Kubernetes ConfigMap

Configure feature flags via Kubernetes ConfigMap:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: copybook-enterprise-config
data:
  # Enterprise feature flags
  COPYBOOK_FF_AUDIT_SYSTEM: "1"
  COPYBOOK_FF_SOX_COMPLIANCE: "1"
  COPYBOOK_FF_GDPR_COMPLIANCE: "1"
  COPYBOOK_FF_SECURITY_MONITORING: "1"
  COPYBOOK_FF_LRU_CACHE: "1"
  COPYBOOK_FF_ADVANCED_OPTIMIZATION: "1"
  COPYBOOK_FF_PARALLEL_DECODE: "1"

  # Disable debug features
  COPYBOOK_FF_VERBOSE_LOGGING: "0"
  COPYBOOK_FF_DIAGNOSTIC_OUTPUT: "0"
```

### Recommended Production Settings

For production deployments, use the following recommended settings:

```toml
# production-recommended.toml
[feature_flags]
# Enterprise features - required for compliance
enabled = [
  "audit_system",
  "sox_compliance",
  "gdpr_compliance",
  "security_monitoring",
]

# Performance features - enable for production workloads
enabled = [
  "lru_cache",
  "advanced_optimization",
  "parallel_decode",
  "zero_copy",
]

# Optional compliance features - enable based on requirements
# enabled = [
#   "hipaa_compliance",
#   "pci_dss_compliance",
# ]

# Debug features - always disabled in production
disabled = [
  "verbose_logging",
  "diagnostic_output",
  "profiling",
  "memory_tracking",
  "mutation_testing",
  "fuzzing_integration",
]
```

## Audit Configuration

### Enabling Audit Logging

Audit logging is enabled via the `audit_system` feature flag:

```bash
export COPYBOOK_FF_AUDIT_SYSTEM=1
```

Or via configuration file:

```toml
[feature_flags]
enabled = ["audit_system"]
```

### Audit Output Formats

#### JSONL Format (Recommended)

JSONL (JSON Lines) is the recommended format for audit logs:

```bash
copybook decode schema.cpy data.bin \
  --output output.jsonl \
  --audit-format jsonl \
  --audit-output audit.jsonl
```

Example audit event in JSONL format:

```json
{
  "event_id": "evt_abc123def456",
  "timestamp": "2024-12-01T14:30:00.123456789Z",
  "event_type": "decode_operation",
  "operation_id": "op_xyz789",
  "user": "system_user",
  "source_file": "data.bin",
  "target_file": "output.jsonl",
  "schema_fingerprint": "a1b2c3d4e5f6...",
  "records_processed": 1000000,
  "processing_duration_ms": 5234,
  "error_count": 0,
  "compliance_profiles": ["SOX", "GDPR"]
}
```

#### Stdout Format

For containerized deployments, audit logs can be written to stdout:

```bash
copybook decode schema.cpy data.bin \
  --output output.jsonl \
  --audit-format jsonl \
  --audit-output stdout
```

This allows audit logs to be captured by container log collectors (e.g., Fluentd, Loki, Splunk).

### Audit Redaction Policies

Configure redaction policies for sensitive data:

```toml
# audit-redaction.toml
[audit.redaction]
# Redact specific field patterns
redacted_fields = [
  "SSN",
  "CREDIT_CARD",
  "PASSWORD",
  "ACCOUNT_NUMBER",
]

# Redaction method: "mask" or "hash"
redaction_method = "mask"

# Mask character
mask_char = "*"

# Hash algorithm for hashing redaction
hash_algorithm = "SHA-256"
```

Use redaction policy:

```bash
copybook decode schema.cpy data.bin \
  --output output.jsonl \
  --audit-redaction-policy /etc/copybook/audit-redaction.toml
```

### Audit Log Rotation and Retention

#### File Rotation

Configure log rotation for audit files:

```bash
# Use logrotate for audit log rotation
cat > /etc/logrotate.d/copybook-audit << 'EOF'
/var/log/copybook/audit.jsonl {
  daily
  rotate 90
  compress
  delaycompress
  missingok
  notifempty
  create 0640 copybook copybook
  postrotate
    # Trigger audit log archival
    /usr/local/bin/copybook audit archive --rotate
  endscript
}
EOF
```

#### Kubernetes Log Rotation

For Kubernetes deployments, use log rotation at the container level:

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: copybook-rs
spec:
  containers:
    - name: copybook
      image: ghcr.io/effortlessmetrics/copybook-rs:latest
      env:
        - name: COPYBOOK_FF_AUDIT_SYSTEM
          value: "1"
      volumeMounts:
        - name: audit-log
          mountPath: /var/log/copybook
  volumes:
    - name: audit-log
      emptyDir:
        sizeLimit: 1Gi
```

#### Retention Policies

Configure retention based on compliance requirements:

```toml
# audit-retention.toml
[audit.retention]
# Retention period in days
retention_days = 2555  # 7 years for SOX

# Archive location
archive_location = "/var/lib/copybook/audit-archive"

# Compression
compress_archives = true
compression_format = "gzip"

# Cleanup policy
cleanup_policy = "after_retention"
```

### Performance Considerations for Audit

Audit logging has minimal performance impact when properly configured:

| Configuration | Overhead | Recommendation |
|---------------|----------|----------------|
| JSONL format | < 1% | Recommended for production |
| Stdout format | < 2% | Use with log aggregation |
| No redaction | < 1% | Fastest, but may expose sensitive data |
| Field redaction | 1-2% | Required for sensitive data |
| Full redaction | 2-3% | Use for highly sensitive data |

#### Optimization Tips

1. **Batch audit events**: Enable batch mode for high-throughput scenarios
   ```bash
   copybook decode schema.cpy data.bin \
     --audit-batch-size 100 \
     --audit-batch-interval-seconds 5
   ```

2. **Async audit logging**: Use async audit logging to reduce impact
   ```bash
   copybook decode schema.cpy data.bin \
     --audit-async \
     --audit-queue-size 10000
   ```

3. **Selective audit**: Audit only critical operations
   ```bash
   copybook decode schema.cpy data.bin \
     --audit-level critical
   ```

## Monitoring Configuration

### Enabling Security Monitoring

Security monitoring is enabled via the `security_monitoring` feature flag:

```bash
export COPYBOOK_FF_SECURITY_MONITORING=1
```

### Metrics Endpoints and Formats

#### Prometheus Metrics

Enable Prometheus metrics endpoint:

```bash
copybook decode schema.cpy data.bin \
  --metrics-port 9300 \
  --metrics-path /metrics
```

Example metrics:

```
# HELP copybook_decode_records_total Total number of records decoded
# TYPE copybook_decode_records_total counter
copybook_decode_records_total{schema="schema.cpy",status="success"} 1000000

# HELP copybook_decode_duration_seconds Total time spent decoding
# TYPE copybook_decode_duration_seconds histogram
copybook_decode_duration_seconds_bucket{le="0.1"} 100
copybook_decode_duration_seconds_bucket{le="1.0"} 500
copybook_decode_duration_seconds_bucket{le="+Inf"} 1000

# HELP copybook_audit_events_total Total number of audit events
# TYPE copybook_audit_events_total counter
copybook_audit_events_total{event_type="decode_operation"} 1

# HELP copybook_compliance_violations_total Total number of compliance violations
# TYPE copybook_compliance_violations_total counter
copybook_compliance_violations_total{profile="SOX"} 0
```

#### OpenTelemetry Metrics

Enable OpenTelemetry metrics:

```bash
copybook decode schema.cpy data.bin \
  --otel-endpoint http://otel-collector:4318 \
  --otel-service-name copybook-rs
```

### Cardinality Considerations

High cardinality metrics can impact performance. Use the following guidelines:

| Metric Dimension | Cardinality | Recommendation |
|------------------|-------------|----------------|
| `schema` | Low (< 100) | Safe to use |
| `status` | Low (< 10) | Safe to use |
| `event_type` | Low (< 50) | Safe to use |
| `compliance_profile` | Low (< 10) | Safe to use |
| `field_name` | High (> 1000) | Avoid or use labels |
| `user_id` | High (> 1000) | Avoid or use labels |

#### Reducing Cardinality

Use label sets instead of individual labels:

```bash
# Bad: High cardinality
copybook decode schema.cpy data.bin \
  --metrics-label user_id=12345

# Good: Low cardinality
copybook decode schema.cpy data.bin \
  --metrics-label user_type=internal
```

### Alerting Recommendations

#### Critical Alerts

Alert on critical issues that require immediate attention:

```yaml
# Prometheus alert rules
groups:
  - name: copybook_critical
    rules:
      - alert: HighErrorRate
        expr: rate(copybook_decode_errors_total[5m]) > 0.01
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: High error rate detected
          description: "Error rate is {{ $value }} errors/sec"

      - alert: ComplianceViolation
        expr: rate(copybook_compliance_violations_total[5m]) > 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: Compliance violation detected
          description: "{{ $value }} violations/sec"

      - alert: AuditLogFailure
        expr: rate(copybook_audit_errors_total[5m]) > 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: Audit log failure detected
```

#### Warning Alerts

Alert on issues that should be investigated:

```yaml
  - name: copybook_warning
    rules:
      - alert: HighLatency
        expr: histogram_quantile(0.95, rate(copybook_decode_duration_seconds_bucket[5m])) > 1
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: High decode latency detected
          description: "95th percentile latency is {{ $value }}s"

      - alert: LowThroughput
        expr: rate(copybook_decode_records_total[5m]) < 1000
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: Low throughput detected
          description: "Throughput is {{ $value }} records/sec"
```

## Compliance Configuration

### Enabling Compliance Policies

Compliance policies are enabled via feature flags:

```bash
# Enable SOX compliance
export COPYBOOK_FF_SOX_COMPLIANCE=1

# Enable GDPR compliance
export COPYBOOK_FF_GDPR_COMPLIANCE=1

# Enable HIPAA compliance
export COPYBOOK_FF_HIPAA_COMPLIANCE=1

# Enable PCI DSS compliance
export COPYBOOK_FF_PCI_DSS_COMPLIANCE=1
```

### SOX Compliance

SOX compliance requires:

1. **Data Integrity Controls**: Cryptographic validation of all financial data
2. **Audit Trail**: Complete audit trail with 7-year retention
3. **Access Control**: Comprehensive access logging
4. **Change Management**: Full audit trail of schema changes

Configuration:

```toml
# sox-compliance.toml
[compliance.sox]
enabled = true
validation_level = "strict"
retention_years = 7

internal_controls = [
  "data_integrity_validation",
  "access_control_validation",
  "change_management_tracking",
  "audit_trail_integrity",
]

data_classification = [
  "material_transactions",
  "financial_statements",
]

reporting = [
  "quarterly_compliance_report",
  "executive_certification",
]
```

### HIPAA Compliance

HIPAA compliance requires:

1. **PHI Protection**: Protected Health Information controls
2. **Access Logging**: Detailed logging of PHI access
3. **Minimum Necessary**: Validate only necessary data is processed
4. **Breach Detection**: Automated detection of unauthorized access

Configuration:

```toml
# hipaa-compliance.toml
[compliance.hipaa]
enabled = true
validation_level = "strict"
retention_years = 6

safeguards = [
  "administrative_safeguards",
  "physical_safeguards",
  "technical_safeguards",
]

phi_protection = [
  "encryption_at_rest",
  "encryption_in_transit",
  "access_logging",
  "minimum_necessary_validation",
]
```

### GDPR Compliance

GDPR compliance requires:

1. **Processing Records**: Records of personal data processing
2. **Legal Basis Tracking**: Link processing to legal basis
3. **Data Subject Rights**: Support for access requests
4. **Cross-Border Monitoring**: Track international data transfers

Configuration:

```toml
# gdpr-compliance.toml
[compliance.gdpr]
enabled = true
validation_level = "strict"
retention_years = 5

data_protection = [
  "processing_records",
  "legal_basis_tracking",
  "data_subject_rights",
  "cross_border_monitoring",
]

data_subject_requests = [
  "access_requests",
  "erasure_requests",
  "portability_requests",
]
```

### PCI DSS Compliance

PCI DSS compliance requires:

1. **Cardholder Data Protection**: Secure processing of payment card data
2. **Encryption Requirements**: End-to-end encryption validation
3. **Access Control**: Strict access controls
4. **Vulnerability Management**: Security control monitoring

Configuration:

```toml
# pci-dss-compliance.toml
[compliance.pci_dss]
enabled = true
validation_level = "strict"
retention_years = 1

cardholder_data_protection = [
  "encryption_at_rest",
  "encryption_in_transit",
  "tokenization",
  "masking",
]

access_control = [
  "role_based_access",
  "least_privilege",
  "multi_factor_authentication",
]

vulnerability_management = [
  "regular_scanning",
  "patch_management",
]
```

### Policy Enforcement Settings

Configure policy enforcement levels:

```toml
# policy-enforcement.toml
[compliance.enforcement]
# Enforcement level: "none", "warn", "block"
level = "block"

# Block on critical violations
block_on_critical = true

# Warn on non-critical violations
warn_on_non_critical = true

# Allow overrides with justification
allow_override = true
require_override_justification = true
```

### Audit Trail Requirements

Compliance requires specific audit trail requirements:

| Requirement | SOX | HIPAA | GDPR | PCI DSS |
|-------------|-----|-------|------|---------|
| Minimum retention | 7 years | 6 years | 5 years | 1 year |
| Immutable logs | Required | Required | Required | Required |
| Cryptographic integrity | Required | Required | Required | Required |
| Access logging | Required | Required | Required | Required |
| Change tracking | Required | Required | Required | Required |

## Performance Caveats

### Enterprise Mode Overhead Expectations

Enterprise features add minimal overhead to baseline performance:

| Feature | Overhead | Notes |
|---------|----------|-------|
| Audit system | < 2% | Lightweight context creation |
| Compliance validation | < 3% | Policy validation adds moderate overhead |
| Security monitoring | < 1% | Access checks are very fast |
| Combined | < 5% | All features together |

### When to Enable/Disable Features

#### Enable for Production

Always enable in production:

- `audit_system` - Required for compliance
- `security_monitoring` - Required for security
- `lru_cache` - Improves performance

Enable based on compliance requirements:

- `sox_compliance` - Financial services
- `hipaa_compliance` - Healthcare
- `gdpr_compliance` - EU data processing
- `pci_dss_compliance` - Payment processing

#### Disable for Performance

Disable for high-throughput scenarios:

- `verbose_logging` - Adds significant logging overhead
- `diagnostic_output` - Adds diagnostic overhead
- `profiling` - Adds profiling overhead
- `memory_tracking` - Adds memory tracking overhead

### Benchmarking Recommendations

#### Baseline Benchmarking

Establish baseline performance before enabling enterprise features:

```bash
# Run baseline benchmark
cargo bench --bench decode_performance -- --save-baseline baseline

# Enable enterprise features
export COPYBOOK_FF_AUDIT_SYSTEM=1
export COPYBOOK_FF_SOX_COMPLIANCE=1
export COPYBOOK_FF_SECURITY_MONITORING=1

# Run enterprise benchmark
cargo bench --bench decode_performance -- --save-baseline enterprise

# Compare results
cargo bench --bench decode_performance -- --baseline baseline
```

#### Production Benchmarking

Benchmark in production-like environment:

```bash
# Use production data and copybooks
copybook decode production_schema.cpy production_data.bin \
  --benchmark \
  --benchmark-output benchmark-results.json
```

### Resource Requirements

#### Memory Requirements

| Configuration | Base Memory | Enterprise Overhead | Total |
|---------------|-------------|---------------------|-------|
| Minimal | 128 MiB | 16 MiB | 144 MiB |
| Standard | 256 MiB | 32 MiB | 288 MiB |
| Large | 512 MiB | 64 MiB | 576 MiB |

#### CPU Requirements

| Configuration | Base CPU | Enterprise Overhead | Total |
|---------------|----------|---------------------|-------|
| Minimal | 250 m | 25 m | 275 m |
| Standard | 500 m | 50 m | 550 m |
| Large | 1000 m | 100 m | 1100 m |

#### Storage Requirements

| Configuration | Base Storage | Audit Storage | Total |
|---------------|--------------|---------------|-------|
| Minimal | 1 GiB | 100 MiB | 1.1 GiB |
| Standard | 10 GiB | 1 GiB | 11 GiB |
| Large | 100 GiB | 10 GiB | 110 GiB |

## Determinism Guarantees

### What is Guaranteed to be Deterministic

The following aspects of copybook-rs processing are guaranteed to be deterministic:

1. **Record parsing**: Given the same input data and copybook definition, records are parsed identically
2. **Field values**: Numeric and string field values are deterministic
3. **Record count**: The number of records processed is deterministic
4. **Error positions**: Error locations in input data are deterministic
5. **Schema fingerprint**: The schema fingerprint is deterministic for a given copybook

### What is NOT Guaranteed

The following aspects are NOT guaranteed to be deterministic:

1. **JSON key ordering**: JSON object key ordering is not guaranteed
2. **Error message formatting**: Error message wording may change between versions
3. **Performance metrics**: Timing metrics are not deterministic
4. **Audit event IDs**: Audit event IDs are generated using UUIDs
5. **Hash ordering**: Hash-based operations may have non-deterministic ordering

### Verifying Determinism

#### Using the Determinism CLI

Use the built-in determinism verification:

```bash
# Verify determinism of decode operation
copybook determinism verify \
  schema.cpy \
  data.bin \
  --runs 10 \
  --output determinism-report.json
```

#### Using the Verify Command

Use the verify command for round-trip determinism:

```bash
# Encode-decode round-trip verification
copybook verify \
  schema.cpy \
  data.bin \
  --roundtrip \
  --determinism
```

#### Programmatic Verification

```rust
use copybook_codec::{decode_file_to_jsonl, DecodeOptions};
use copybook_core::parse_copybook;

fn verify_determinism(
    copybook: &str,
    input: &std::path::Path,
    runs: usize,
) -> Result<bool, Box<dyn std::error::Error>> {
    let schema = parse_copybook(copybook)?;
    let options = DecodeOptions::new();

    let mut results = Vec::new();

    for i in 0..runs {
        let output = format!("output_{}.jsonl", i);
        decode_file_to_jsonl(&schema, input, std::path::Path::new(&output), &options)?;

        let content = std::fs::read_to_string(&output)?;
        results.push(content);
    }

    // Compare all results
    let first = &results[0];
    for result in &results[1..] {
        if result != first {
            return Ok(false);
        }
    }

    Ok(true)
}
```

## Safe Upgrade Practices

### Upgrading Between Versions

#### Pre-Upgrade Checklist

Before upgrading:

1. **Backup configuration**: Backup all configuration files
   ```bash
   cp /etc/copybook/*.toml /etc/copybook/backup/
   ```

2. **Backup audit logs**: Archive audit logs
   ```bash
   tar -czf audit-backup-$(date +%Y%m%d).tar.gz /var/log/copybook/
   ```

3. **Run compatibility check**: Check for breaking changes
   ```bash
   copybook --check-compatibility --from v0.4.0 --to v0.5.0
   ```

4. **Review changelog**: Review the changelog for breaking changes
   ```bash
   cat docs/CHANGELOG.md | grep -A 20 "v0.5.0"
   ```

#### Upgrade Procedure

1. **Stop services**: Stop all copybook-rs services
   ```bash
   systemctl stop copybook-rs
   ```

2. **Install new version**: Install the new version
   ```bash
   cargo install copybook-cli --version 0.5.0
   ```

3. **Update configuration**: Update configuration if needed
   ```bash
   # Update feature flags if deprecated
   # Update compliance policies if changed
   ```

4. **Start services**: Start copybook-rs services
   ```bash
   systemctl start copybook-rs
   ```

5. **Verify operation**: Verify operation
   ```bash
   copybook --version
   copybook decode test.cpy test.bin --output test.jsonl
   ```

### Feature Flag Changes Between Versions

#### Deprecated Feature Flags

When feature flags are deprecated:

1. **Check deprecation warnings**: Look for deprecation warnings
   ```bash
   copybook decode schema.cpy data.bin 2>&1 | grep -i deprecated
   ```

2. **Update configuration**: Replace deprecated flags
   ```toml
   # Old (deprecated)
   enabled = ["old_feature"]

   # New
   enabled = ["new_feature"]
   ```

3. **Test with new flags**: Test with new feature flags
   ```bash
   copybook decode schema.cpy data.bin \
     --feature-flags-config new-config.toml
   ```

#### New Feature Flags

When new feature flags are introduced:

1. **Review documentation**: Review feature flag documentation
   ```bash
   cat docs/FEATURE_FLAGS.md | grep -A 5 "new_feature"
   ```

2. **Evaluate impact**: Evaluate impact on your deployment
   ```bash
   # Test in staging first
   copybook decode schema.cpy data.bin \
     --enable-features new_feature
   ```

3. **Enable if beneficial**: Enable if beneficial to your use case
   ```toml
   [feature_flags]
   enabled = ["new_feature"]
   ```

### Migration Considerations

#### Schema Migration

When copybook schemas change:

1. **Version schemas**: Version your copybook schemas
   ```cobol
   * Version 1.0
   01 CUSTOMER-RECORD-V1.
      05 CUSTOMER-ID PIC 9(12).
      05 CUSTOMER-NAME PIC X(50).

   * Version 2.0
   01 CUSTOMER-RECORD-V2.
      05 CUSTOMER-ID PIC 9(12).
      05 CUSTOMER-NAME PIC X(50).
      05 CUSTOMER-EMAIL PIC X(100).
   ```

2. **Use schema registry**: Use a schema registry for versioning
   ```bash
   copybook schema register \
     --name CUSTOMER-RECORD \
     --version 2.0 \
     --file customer-v2.cpy
   ```

3. **Migrate data**: Migrate data using migration tools
   ```bash
   copybook migrate \
     --from-schema customer-v1.cpy \
     --to-schema customer-v2.cpy \
     --input data-v1.bin \
     --output data-v2.bin
   ```

#### Data Migration

When data formats change:

1. **Backup data**: Backup all data
   ```bash
   tar -czf data-backup-$(date +%Y%m%d).tar.gz /data/
   ```

2. **Run migration**: Run data migration
   ```bash
   copybook migrate \
     --from-format old-format \
     --to-format new-format \
     --input old-data.bin \
     --output new-data.bin
   ```

3. **Verify migration**: Verify migration results
   ```bash
   copybook verify \
     --schema schema.cpy \
     --input new-data.bin \
     --expected expected.jsonl
   ```

### Rollback Procedures

#### Immediate Rollback

For immediate rollback:

1. **Stop services**: Stop all copybook-rs services
   ```bash
   systemctl stop copybook-rs
   ```

2. **Restore previous version**: Restore previous version
   ```bash
   cargo install copybook-cli --version 0.4.0
   ```

3. **Restore configuration**: Restore previous configuration
   ```bash
   cp /etc/copybook/backup/*.toml /etc/copybook/
   ```

4. **Start services**: Start copybook-rs services
   ```bash
   systemctl start copybook-rs
   ```

#### Data Rollback

For data rollback:

1. **Stop processing**: Stop data processing
   ```bash
   systemctl stop copybook-rs
   ```

2. **Restore data**: Restore data from backup
   ```bash
   tar -xzf data-backup-$(date +%Y%m%d).tar.gz -C /
   ```

3. **Verify data**: Verify data integrity
   ```bash
   copybook verify \
     --schema schema.cpy \
     --input /data/input/data.bin \
     --expected /data/expected/expected.jsonl
   ```

4. **Resume processing**: Resume data processing
   ```bash
   systemctl start copybook-rs
   ```

#### Audit Trail Rollback

For audit trail rollback:

1. **Archive current audit logs**: Archive current audit logs
   ```bash
   tar -czf audit-current-$(date +%Y%m%d).tar.gz /var/log/copybook/
   ```

2. **Restore previous audit logs**: Restore previous audit logs
   ```bash
   tar -xzf audit-backup-$(date +%Y%m%d).tar.gz -C /var/log/copybook/
   ```

3. **Verify audit trail**: Verify audit trail integrity
   ```bash
   copybook audit verify \
     --audit-file /var/log/copybook/audit.jsonl \
     --integrity-check
   ```

## Related Documentation

- [Feature Flags](FEATURE_FLAGS.md) - Complete feature flag documentation
- [Enterprise Compliance Guide](enterprise-compliance-guide.md) - Detailed compliance configuration
- [Enterprise Performance](ENTERPRISE_PERFORMANCE.md) - Performance benchmarks and targets
- [Audit API Reference](audit-api-reference.md) - Complete audit API documentation
- [CLI Reference](CLI_REFERENCE.md) - Complete CLI command reference
- [Kubernetes Deployment](../deploy/kubernetes/README.md) - Kubernetes deployment guide

## Support

For enterprise deployment support:
- Review the troubleshooting sections in each documentation file
- Check existing issues on GitHub
- Contact enterprise support for production deployments
