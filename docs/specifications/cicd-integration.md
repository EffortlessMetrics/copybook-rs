# CI/CD Integration Strategy Specification

## Overview

The copybook-rs benchmark reporting system integrates seamlessly with GitHub Actions workflows to provide automated performance validation, regression detection, and enterprise audit compliance. This specification defines the CI/CD integration strategy for machine-readable benchmark reporting infrastructure supporting mainframe data processing workloads.

## GitHub Actions Workflow Integration

### PR Performance Validation Workflow

#### Workflow Trigger
```yaml
name: Performance Validation
on:
  pull_request:
    branches: [main]
    paths:
      - 'copybook-core/**'
      - 'copybook-codec/**'
      - 'copybook-cli/**'
      - 'copybook-bench/**'
      - 'Cargo.toml'
      - 'Cargo.lock'
```

#### Workflow Jobs

##### Performance Benchmark Job
```yaml
jobs:
  performance-validation:
    runs-on: ubuntu-latest
    timeout-minutes: 45
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Rust toolchain
        uses: dtolnay/rust-toolchain@stable
        with:
          toolchain: 1.90.0

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
            target/
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}

      - name: Setup Python environment
        uses: actions/setup-python@v4
        with:
          python-version: '3.9'

      - name: Install Python dependencies
        run: |
          cd scripts/bench
          pip install -r requirements.txt

      - name: Execute performance benchmarks
        run: |
          python scripts/bench/bench_runner.py --output scripts/bench/perf.json

      - name: Validate JSON schema
        run: |
          python scripts/bench/json_processor.py --validate scripts/bench/perf.json

      - name: Check SLO compliance
        run: |
          python scripts/bench/slo_validator.py --input scripts/bench/perf.json

      - name: Post PR comment
        if: github.event_name == 'pull_request'
        run: |
          python scripts/bench/pr_automation.py \
            --pr-number ${{ github.event.number }} \
            --performance-file scripts/bench/perf.json \
            --github-token ${{ secrets.GITHUB_TOKEN }}

      - name: Upload performance artifact
        uses: actions/upload-artifact@v3
        with:
          name: performance-report-${{ github.sha }}
          path: scripts/bench/perf.json
          retention-days: 90
```

### Baseline Promotion Workflow

#### Workflow Trigger
```yaml
name: Baseline Promotion
on:
  push:
    branches: [main]
  workflow_run:
    workflows: ["Performance Validation"]
    types: [completed]
    branches: [main]
```

#### Baseline Update Job
```yaml
jobs:
  promote-baseline:
    runs-on: ubuntu-latest
    if: github.event.workflow_run.conclusion == 'success'
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          token: ${{ secrets.GITHUB_TOKEN }}

      - name: Setup Python environment
        uses: actions/setup-python@v4
        with:
          python-version: '3.9'

      - name: Install Python dependencies
        run: |
          cd scripts/bench
          pip install -r requirements.txt

      - name: Download performance artifact
        uses: actions/download-artifact@v3
        with:
          name: performance-report-${{ github.sha }}
          path: scripts/bench/

      - name: Promote performance baseline
        run: |
          python scripts/bench/baseline_manager.py \
            --promote \
            --performance-file scripts/bench/perf.json \
            --baseline-file scripts/bench/baseline.json \
            --commit-sha ${{ github.sha }}

      - name: Commit baseline update
        run: |
          git config --local user.email "action@github.com"
          git config --local user.name "GitHub Action"
          git add scripts/bench/baseline.json
          git commit -m "chore: update performance baseline [skip ci]" || exit 0
          git push
```

### Enterprise Audit Workflow

#### Scheduled Audit Generation
```yaml
name: Enterprise Audit
on:
  schedule:
    - cron: '0 2 1 * *'  # First day of month at 2 AM UTC
  workflow_dispatch:     # Manual trigger for ad-hoc audits
```

#### Audit Generation Job
```yaml
jobs:
  generate-audit:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Python environment
        uses: actions/setup-python@v4
        with:
          python-version: '3.9'

      - name: Install Python dependencies
        run: |
          cd scripts/bench
          pip install -r requirements.txt

      - name: Generate enterprise audit report
        run: |
          python scripts/bench/audit_generator.py \
            --output-dir audit-reports/ \
            --format html,pdf,json \
            --period-months 12

      - name: Upload audit artifacts
        uses: actions/upload-artifact@v3
        with:
          name: enterprise-audit-${{ github.run_number }}
          path: audit-reports/
          retention-days: 1095  # 3 years
```

## Criterion.rs Integration Strategy

### Benchmark Execution Pipeline

#### Performance Data Collection
1. **Benchmark Execution**: `PERF=1 cargo bench -p copybook-bench`
2. **Output Parsing**: Extract Criterion.rs console output
3. **Metric Extraction**: Parse DISPLAY/COMP-3 performance data
4. **Unit Conversion**: Convert MB/s to GiB/s (DISPLAY) and MiB/s (COMP-3)
5. **JSON Generation**: Create machine-readable `perf.json` output

#### Integration with Existing Infrastructure
- **Benchmark Harness**: Leverage `copybook-bench/benches/` directory structure
- **Regression Detection**: Integrate with `copybook-bench/src/regression.rs` module
- **Test Validation**: Maintain compatibility with `cargo nextest run --workspace`
- **Performance Monitoring**: Preserve existing performance characteristics

### Benchmark Execution Environment

#### System Requirements
- **CPU**: Dedicated runner resources (no shared CPU contention)
- **Memory**: Minimum 8 GB RAM for large dataset benchmarks
- **Storage**: SSD storage for consistent I/O performance
- **Network**: Stable connection for artifact upload/download

#### Environment Configuration
```yaml
env:
  RUST_BACKTRACE: 1
  CARGO_INCREMENTAL: 0
  RUSTFLAGS: "-C target-cpu=native"
  PERF: 1
```

## Performance Validation Strategy

### SLO Compliance Validation

#### Performance Floor Enforcement
- **DISPLAY Minimum**: 80 MB/s (~0.074 GiB/s)
- **COMP-3 Minimum**: 40 MB/s
- **Regression Threshold**: 5% performance decrease
- **Validation Frequency**: Every PR and main branch commit

#### Safety Margin Monitoring
- **Current Achievement**: DISPLAY 4.1-4.2 GiB/s, COMP-3 560-580 MiB/s
- **Enterprise Multiplier**: 15-52x above minimum requirements
- **Headroom Tracking**: Monitor performance trend analysis
- **Alert Thresholds**: Warn if safety margins decrease below 10x

### Regression Detection Workflow

#### Baseline Comparison Process
1. **Current Performance**: Extract metrics from latest benchmark run
2. **Baseline Retrieval**: Load historical baseline from `scripts/bench/baseline.json`
3. **Regression Analysis**: Calculate percentage change vs. baseline
4. **Threshold Validation**: Check against 5% regression threshold
5. **Status Determination**: Generate [PASS/WARN/FAIL] status indicator

#### Regression Response Actions
- **Immediate**: Block PR merge on performance floor violations
- **Warning**: Flag PR with performance degradation warnings
- **Tracking**: Log regression incidents for enterprise audit trails
- **Recovery**: Validate performance restoration in subsequent commits

## PR Automation Strategy

### Comment Generation Workflow

#### Performance Summary Format
```
Performance Report: DISPLAY: 4.22 GiB/s, COMP-3: 571 MiB/s [PASS]
```

#### Status Indicator Logic
- **[PASS]**: Performance meets/exceeds SLOs, no regression detected
- **[WARN]**: Performance degradation detected but above enterprise floors
- **[FAIL]**: Performance below enterprise floors or severe regression

#### Comment Management
- **Update Strategy**: Update existing PR comment rather than creating new ones
- **Content Format**: Single-line summary for easy scanning
- **Error Handling**: Graceful fallback if GitHub API unavailable
- **Rate Limiting**: Respect GitHub API rate limits and retry logic

### GitHub API Integration

#### Authentication Strategy
```yaml
env:
  GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

#### API Endpoints
- **PR Comments**: `POST /repos/{owner}/{repo}/issues/{issue_number}/comments`
- **Comment Updates**: `PATCH /repos/{owner}/{repo}/issues/comments/{comment_id}`
- **Status Checks**: `POST /repos/{owner}/{repo}/statuses/{sha}`

#### Error Handling
- **Authentication Failures**: Validate token permissions and scopes
- **Rate Limiting**: Implement exponential backoff retry logic
- **Network Issues**: Graceful degradation with local logging
- **API Changes**: Version compatibility and error recovery

## Artifact Management Strategy

### Performance Data Artifacts

#### Artifact Structure
```
performance-report-{sha}/
├── perf.json           # Machine-readable performance data
├── benchmark.log       # Raw Criterion.rs output
├── system-info.json    # System configuration details
└── validation.log      # SLO compliance validation results
```

#### Retention Policies
- **PR Artifacts**: 90-day retention for debugging and analysis
- **Main Branch**: 1-year retention for baseline management
- **Audit Reports**: 3-year retention for regulatory compliance
- **Historical Baselines**: Permanent retention with periodic compression

### Baseline Management

#### Baseline Storage Strategy
```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "commit_sha": "abc123def456",
  "performance": {
    "display_gibs": 4.22,
    "comp3_mibs": 571.0
  },
  "environment": {
    "runner_os": "ubuntu-latest",
    "rust_version": "1.90.0",
    "cpu_model": "Intel Xeon E5-2673 v4"
  }
}
```

#### Promotion Criteria
- **Successful Validation**: All performance SLOs met
- **No Regression**: No performance degradation detected
- **Test Passing**: Complete test suite validation successful
- **Manual Override**: Emergency baseline promotion capability

## Error Handling and Recovery

### Workflow Failure Scenarios

#### Benchmark Execution Failures
- **Timeout Handling**: 45-minute workflow timeout with graceful termination
- **Resource Exhaustion**: Memory or CPU resource monitoring and alerts
- **Compilation Errors**: Rust compilation failure handling and reporting
- **Test Failures**: Integration with existing test failure protocols

#### Infrastructure Failures
- **Runner Unavailability**: Retry logic with alternative runner selection
- **Network Issues**: Artifact upload/download retry mechanisms
- **Storage Failures**: Backup artifact storage locations
- **API Outages**: Graceful degradation when GitHub API unavailable

### Monitoring and Alerting

#### Performance Monitoring Alerts
- **SLO Violations**: Immediate alerts for performance floor breaches
- **Regression Detection**: Automated alerts for performance degradation
- **Baseline Failures**: Alerts when baseline promotion fails
- **System Health**: Infrastructure health monitoring and reporting

#### Recovery Procedures
- **Automatic Retry**: Transient failure retry with exponential backoff
- **Manual Intervention**: Clear escalation procedures for persistent failures
- **Rollback Capability**: Baseline rollback procedures for emergency situations
- **Documentation**: Comprehensive runbook for troubleshooting workflows

## Enterprise Compliance Integration

### Regulatory Requirements

#### Audit Trail Generation
- **Performance History**: Complete historical performance tracking
- **Workflow Execution**: Detailed CI/CD execution logs and artifacts
- **Access Control**: Audit user access and permission changes
- **Change Management**: Track all configuration and baseline changes

#### Compliance Validation
- **SOX Compliance**: Financial reporting performance validation
- **PCI-DSS**: Secure data processing performance requirements
- **GDPR**: Data processing efficiency and compliance monitoring
- **ISO 27001**: Information security performance validation

### Security Considerations

#### Access Control
- **Token Security**: Secure GitHub token management and rotation
- **Artifact Access**: Controlled access to performance artifacts
- **Audit Logs**: Comprehensive access logging and monitoring
- **Permission Management**: Role-based access control for CI/CD workflows

#### Data Protection
- **Sensitive Data**: Ensure no sensitive data in performance reports
- **Encryption**: Encrypt artifacts at rest and in transit
- **Retention**: Secure deletion of expired artifacts
- **Compliance**: Meet all enterprise data protection requirements

This CI/CD integration strategy ensures that copybook-rs benchmark reporting provides enterprise-grade automation while maintaining the high performance standards and regulatory compliance required for production mainframe data processing environments.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
