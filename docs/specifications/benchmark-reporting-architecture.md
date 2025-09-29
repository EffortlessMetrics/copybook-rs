# Benchmark Reporting Architecture Specification

## Overview

The copybook-rs benchmark reporting infrastructure provides enterprise-grade performance monitoring and automated validation for mainframe data processing workloads. This system enables continuous integration performance validation, regression detection, and regulatory audit compliance through machine-readable JSON reporting and automated GitHub workflows.

## Architecture Components

### Python Utilities Directory Structure

```
scripts/bench/
├── bench_runner.py          # Main benchmark execution orchestrator
├── json_processor.py        # JSON report processing and validation
├── pr_automation.py         # GitHub PR comment automation
├── baseline_manager.py      # Baseline promotion workflows
├── audit_generator.py       # Enterprise audit reporting
├── slo_validator.py         # Performance floor validation
├── requirements.txt         # Python dependencies
├── README.md                # Usage documentation
├── config/
│   ├── thresholds.toml      # Performance thresholds configuration
│   ├── audit_config.yaml    # Enterprise audit configuration
│   └── pr_template.md       # PR comment template
└── templates/
    ├── performance_report.html  # HTML performance report template
    └── audit_report.html        # Enterprise audit report template
```

## Core Python Utilities Specification

### bench_runner.py - Main Benchmark Orchestrator

**Purpose**: Executes `PERF=1 cargo bench -p copybook-bench` and orchestrates performance data collection.

**Key Functions**:
- `execute_benchmark()` - Run benchmark with timeout and error handling
- `collect_performance_data()` - Parse Criterion.rs output for performance metrics
- `generate_json_report()` - Create machine-readable perf.json output
- `validate_execution_environment()` - Check system requirements and dependencies

**Integration Points**:
- Criterion.rs benchmark harness in `copybook-bench/benches/`
- Performance regression detection in `copybook-bench/src/regression.rs`
- JSON schema validation via `json_processor.py`

**Error Handling**:
- Benchmark execution timeouts (30-minute limit)
- Cargo build failures and dependency issues
- Performance data parsing errors
- JSON generation failures

### json_processor.py - JSON Report Processing

**Purpose**: Validates and processes machine-readable benchmark JSON output.

**JSON Schema Specification**:
```json
{
  "display_gibs": 4.22,     // DISPLAY throughput in GiB/s (decimal precision)
  "comp3_mibs": 571.0,      // COMP-3 throughput in MiB/s (decimal precision)
  "warnings": [],           // Performance warnings array
  "errors": []              // Performance errors array
}
```

**Key Functions**:
- `validate_json_schema()` - Verify perf.json against schema
- `extract_performance_metrics()` - Parse DISPLAY/COMP-3 performance data
- `calculate_slo_compliance()` - Validate against performance floors
- `detect_performance_regression()` - Compare against baselines

**Performance Floor Validation**:
- DISPLAY: ≥80 MB/s (~0.074 GiB/s)
- COMP-3: ≥40 MB/s
- Regression threshold: >5% performance decrease

### pr_automation.py - GitHub PR Comment Automation

**Purpose**: Automated GitHub PR comments with performance summaries.

**Comment Format**:
```
Performance Report: DISPLAY: 4.22 GiB/s, COMP-3: 571 MiB/s [PASS]
```

**Key Functions**:
- `post_pr_comment()` - Create/update GitHub PR comments
- `format_performance_summary()` - Generate one-liner performance status
- `authenticate_github_api()` - Handle GitHub API authentication
- `determine_status_indicator()` - Calculate [PASS/WARN/FAIL] status

**Status Indicators**:
- `[PASS]` - Performance meets/exceeds SLOs
- `[WARN]` - Performance degradation detected but above floors
- `[FAIL]` - Performance below enterprise floors

### baseline_manager.py - Baseline Promotion Workflows

**Purpose**: Manages performance baselines and automated promotion workflows.

**Key Functions**:
- `promote_baseline()` - Update baseline when PR merges to main
- `validate_promotion_criteria()` - Check baseline promotion requirements
- `store_historical_data()` - Maintain performance history
- `calculate_performance_trends()` - Analyze performance over time

**Promotion Criteria**:
- PR merged to main branch
- All performance SLOs met
- No regression detected
- Comprehensive test validation passed

### audit_generator.py - Enterprise Audit Reporting

**Purpose**: Generates regulatory compliance reports for enterprise deployments.

**Key Functions**:
- `generate_audit_report()` - Create comprehensive audit documentation
- `validate_compliance_requirements()` - Check regulatory requirements
- `export_historical_metrics()` - Extract performance history for audits
- `generate_html_report()` - Create formatted audit reports

**Audit Report Contents**:
- Performance baseline history
- SLO compliance tracking
- Regression incident reports
- Enterprise deployment validation
- Regulatory compliance certification

### slo_validator.py - Performance Floor Validation

**Purpose**: Validates performance against enterprise SLO requirements.

**Key Functions**:
- `validate_performance_floors()` - Check minimum performance requirements
- `detect_slo_violations()` - Identify performance floor breaches
- `calculate_safety_margins()` - Determine performance headroom
- `generate_violation_reports()` - Document SLO breaches

**Performance Targets**:
- Current Achievement: DISPLAY 4.1-4.2 GiB/s, COMP-3 560-580 MiB/s
- Enterprise Floors: DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s
- Safety Margins: 15-52x above enterprise requirements

## Configuration Management

### thresholds.toml - Performance Thresholds

```toml
[performance_floors]
display_min_mibs = 80      # Minimum DISPLAY throughput (MB/s)
comp3_min_mibs = 40        # Minimum COMP-3 throughput (MB/s)
regression_threshold = 0.05 # 5% regression threshold

[enterprise_targets]
display_target_gibs = 4.0   # Target DISPLAY throughput (GiB/s)
comp3_target_mibs = 500     # Target COMP-3 throughput (MiB/s)
safety_margin_multiplier = 10  # Safety margin above floors

[automation]
benchmark_timeout_minutes = 30
pr_comment_enabled = true
baseline_promotion_enabled = true
audit_generation_enabled = true
```

### audit_config.yaml - Enterprise Audit Configuration

```yaml
enterprise_audit:
  enabled: true
  compliance_standards:
    - "SOX"
    - "PCI-DSS"
    - "GDPR"

  reporting:
    frequency: "monthly"
    retention_months: 36
    export_formats: ["html", "pdf", "json"]

  performance_tracking:
    baseline_history_retention: 12  # months
    slo_violation_alerts: true
    regression_incident_tracking: true

  regulatory_requirements:
    performance_validation: true
    audit_trail_generation: true
    compliance_certification: true
```

## Integration Strategy

### CI/CD Integration

The benchmark reporting system integrates with existing GitHub Actions workflows:

1. **PR Validation**: Automated performance validation on pull requests
2. **Baseline Promotion**: Automatic baseline updates on main branch merges
3. **Audit Generation**: Scheduled audit report generation
4. **SLO Monitoring**: Continuous performance floor validation

### Criterion.rs Integration

The system leverages existing Criterion.rs benchmark infrastructure:

- **Benchmark Execution**: `PERF=1 cargo bench -p copybook-bench`
- **Output Parsing**: Extract performance metrics from Criterion output
- **Regression Detection**: Integration with `copybook-bench/src/regression.rs`
- **Historical Data**: Maintain baseline performance data

### Error Taxonomy

Performance monitoring errors follow copybook-rs structured error patterns:

- `CBKB001`: Benchmark execution timeout
- `CBKB002`: Performance data parsing failure
- `CBKB003`: JSON schema validation error
- `CBKB004`: GitHub API authentication failure
- `CBKB005`: SLO violation detected
- `CBKB006`: Baseline promotion failure
- `CBKB007`: Audit report generation error

## Performance Requirements

### System Performance Impact

- **Monitoring Overhead**: <2% impact on benchmark execution
- **Memory Usage**: <64 MiB additional memory for reporting
- **Network I/O**: Minimal GitHub API calls for PR automation
- **Storage**: Historical data maintained with configurable retention

### Execution Performance

- **Benchmark Timeout**: 30-minute maximum execution time
- **JSON Generation**: <1 second for report creation
- **PR Comment Latency**: <5 seconds for GitHub API interaction
- **Audit Report Generation**: <30 seconds for comprehensive reports

## Enterprise Compliance

### Regulatory Support

- **SOX Compliance**: Performance audit trails and validation
- **PCI-DSS**: Secure data processing performance monitoring
- **GDPR**: Data processing efficiency compliance
- **ISO 27001**: Information security performance validation

### Audit Capabilities

- **Historical Tracking**: 36-month performance baseline retention
- **Compliance Reporting**: Automated regulatory report generation
- **Incident Documentation**: Performance regression incident tracking
- **Certification Support**: Enterprise deployment validation reports

## Dependencies

### Python Requirements

```txt
requests>=2.31.0         # GitHub API integration
jsonschema>=4.19.0       # JSON schema validation
pyyaml>=6.0.1           # YAML configuration parsing
jinja2>=3.1.2           # Template rendering
click>=8.1.0            # CLI interface
tabulate>=0.9.0         # Table formatting
matplotlib>=3.7.0       # Performance visualization
pandas>=2.0.0           # Data analysis
```

### System Requirements

- **Python**: 3.9+ (compatible with enterprise environments)
- **Rust**: 1.90+ (MSRV compatibility)
- **Git**: 2.30+ (GitHub API integration)
- **Network**: HTTPS access for GitHub API

### Security Considerations

- **Zero Unsafe Code**: All Python utilities maintain memory safety
- **API Security**: Secure GitHub token handling and validation
- **Data Privacy**: No sensitive data in performance reports
- **Access Control**: Configurable enterprise access permissions

## Implementation Validation

The architecture supports comprehensive validation through existing test scaffolding:

- **AC1**: Directory structure and Python utilities presence validation
- **AC2**: JSON schema compliance and decimal precision validation
- **AC3**: Benchmark execution and JSON generation validation
- **AC4-AC10**: End-to-end workflow validation and enterprise compliance

All specifications align with copybook-rs TDD patterns using `// AC:ID` test tags and workspace validation via `cargo nextest run --workspace`.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
