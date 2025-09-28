# copybook-rs Benchmark Infrastructure

This directory contains the machine-readable benchmark reporting infrastructure for Issue #52, providing enterprise-grade performance monitoring and regulatory compliance capabilities for copybook-rs.

## 🚀 Overview

The benchmark infrastructure provides automated performance monitoring, regression detection, and compliance reporting for copybook-rs enterprise mainframe data processing. It integrates with existing Criterion.rs benchmarks and provides machine-readable output for CI/CD systems.

### Key Features

- **Enterprise Performance Monitoring**: DISPLAY ≥4.1 GiB/s, COMP-3 ≥560 MiB/s validation
- **Regulatory Compliance**: SOX, HIPAA, PCI-DSS, GDPR, ISO 27001 compliance reporting
- **Automated Regression Detection**: Statistical analysis with configurable thresholds
- **GitHub Integration**: Automated PR comments with performance analysis
- **Baseline Management**: Performance baseline promotion and trend analysis
- **Audit Trail Generation**: Enterprise audit reporting with evidence integrity

## 📁 Directory Structure

```
scripts/bench/
├── README.md                    # This documentation
├── requirements.txt             # Python dependencies
│
├── Python Utilities:
├── bench_runner.py             # Main benchmark execution orchestrator
├── json_processor.py           # JSON schema validation and processing
├── pr_automation.py            # GitHub PR comment automation
├── baseline_manager.py         # Baseline promotion workflows
├── audit_generator.py          # Enterprise audit report generation
├── slo_validator.py            # Performance floor validation
│
├── config/                     # Configuration files
│   ├── thresholds.toml         # Performance thresholds and targets
│   ├── audit_config.yaml       # Enterprise audit configuration
│   └── pr_template.md          # PR comment template
│
└── templates/                  # HTML report templates
    ├── performance_report.html # Performance report template
    └── audit_report.html       # Enterprise audit report template
```

## 🛠️ Quick Start

### Installation

```bash
# Install Python dependencies
cd scripts/bench
pip install -r requirements.txt

# Verify installation
python3 bench_runner.py --help
```

### Basic Usage

```bash
# Run benchmarks and generate performance report
python3 bench_runner.py --output perf.json

# Validate performance against enterprise targets
python3 slo_validator.py --perf-file perf.json --validate-enterprise-targets

# Generate GitHub PR comment
python3 pr_automation.py --pr 123 --perf-file perf.json --dry-run

# Generate enterprise audit report
python3 audit_generator.py --generate --perf-file perf.json --output audit.html
```

## 📊 Core Utilities

### 1. bench_runner.py - Benchmark Orchestrator

Main benchmark execution orchestrator that integrates with copybook-bench.

```bash
# Execute benchmarks with performance mode
PERF=1 python3 bench_runner.py --output perf.json

# Test mode for validation
python3 bench_runner.py --test-mode --output test.json

# CI/CD integration
python3 bench_runner.py --output perf.json --timeout 300
```

**Key Features:**
- Integrates with `PERF=1 cargo bench -p copybook-bench`
- Generates machine-readable JSON reports
- CI/CD environment detection
- Performance variance monitoring

### 2. json_processor.py - Schema Validation

JSON schema validation and processing for performance reports.

```bash
# Validate JSON schema
python3 json_processor.py --validate perf.json

# Extract performance metrics
python3 json_processor.py --extract-metrics perf.json

# Compare against baseline
python3 json_processor.py --compare-baseline current.json baseline.json
```

**JSON Schema:**
```json
{
  "display_gibs": 4.22,
  "comp3_mibs": 571.0,
  "warnings": [],
  "errors": []
}
```

### 3. pr_automation.py - GitHub Integration

Automated GitHub PR comment generation with performance analysis.

```bash
# Post performance comment to PR
python3 pr_automation.py --pr 123 --perf-file perf.json

# Include baseline comparison
python3 pr_automation.py --pr 123 --perf-file perf.json --baseline baseline.json

# Dry run for testing
python3 pr_automation.py --pr 123 --perf-file perf.json --dry-run
```

**Environment Variables:**
- `GITHUB_TOKEN`: GitHub API token for PR commenting

### 4. baseline_manager.py - Baseline Management

Performance baseline promotion and trend analysis.

```bash
# Validate current performance
python3 baseline_manager.py --validate-current --perf-file perf.json

# Promote new baseline
python3 baseline_manager.py --promote --perf-file perf.json --environment main

# Analyze performance trends
python3 baseline_manager.py --analyze-trends --environment main
```

**Features:**
- Automated baseline promotion based on performance improvements
- Statistical validation of performance stability
- Multi-environment support (dev, staging, production)

### 5. audit_generator.py - Enterprise Audit

Comprehensive enterprise audit reporting for regulatory compliance.

```bash
# Generate HTML audit report
python3 audit_generator.py --generate --perf-file perf.json --output audit.html

# JSON format for machine processing
python3 audit_generator.py --generate --perf-file perf.json --format json

# Compliance assessment only
python3 audit_generator.py --assess-compliance --perf-file perf.json
```

**Compliance Standards:**
- **SOX**: Sarbanes-Oxley financial controls
- **HIPAA**: Healthcare data processing
- **PCI-DSS**: Payment card data security
- **GDPR**: Data protection regulation
- **ISO 27001**: Information security management

### 6. slo_validator.py - SLO Validation

Service Level Objective validation and performance floor monitoring.

```bash
# Validate enterprise performance floors
python3 slo_validator.py --display-gibs 4.22 --comp3-mibs 571.0 --validate-floors

# Comprehensive enterprise validation
python3 slo_validator.py --perf-file perf.json --validate-enterprise-targets

# Regression detection
python3 slo_validator.py --perf-file current.json --baseline baseline.json --detect-regression
```

**Performance Targets:**
- **DISPLAY Parsing**: ≥4.1 GiB/s (enterprise floor)
- **COMP-3 Encoding**: ≥560 MiB/s (enterprise floor)
- **Regression Threshold**: <2% degradation tolerance

## ⚙️ Configuration

### Performance Thresholds (`config/thresholds.toml`)

```toml
[performance_floors]
display_floor_gibs = 4.1
comp3_floor_mibs = 560.0

[regression_detection]
minor_regression_percent = 2.0
major_regression_percent = 5.0
critical_regression_percent = 10.0
```

### Audit Configuration (`config/audit_config.yaml`)

```yaml
compliance_standards:
  sox:
    enabled: true
    requirements:
      data_integrity:
        zero_corruption_tolerance: true
        deterministic_processing: true
```

## 🔄 CI/CD Integration

### GitHub Actions Integration

```yaml
# .github/workflows/performance.yml
- name: Run Performance Benchmarks
  run: |
    cd scripts/bench
    python3 bench_runner.py --output perf.json
    python3 slo_validator.py --perf-file perf.json --validate-enterprise-targets

- name: Post PR Comment
  if: github.event_name == 'pull_request'
  run: |
    python3 pr_automation.py --pr ${{ github.event.number }} --perf-file perf.json
  env:
    GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### Exit Codes

The utilities use standard exit codes for CI/CD integration:

- `0`: Success, all validations passed
- `1`: Failure, performance below thresholds or errors detected
- `2`: Configuration or runtime error

## 📈 Performance Monitoring

### Enterprise Performance Standards

| Metric | Enterprise Floor | Enterprise Target | Current Performance |
|--------|------------------|-------------------|-------------------|
| **DISPLAY Parsing** | ≥4.1 GiB/s | ≥5.0 GiB/s | 4.22+ GiB/s ✅ |
| **COMP-3 Encoding** | ≥560 MiB/s | ≥650 MiB/s | 571+ MiB/s ✅ |

### Regression Detection

The system monitors for:
- **Minor Regression**: >2% performance decrease (warning)
- **Major Regression**: >5% performance decrease (failure)
- **Critical Regression**: >10% performance decrease (critical alert)

## 🔒 Security and Compliance

### Evidence Integrity

All performance evidence includes cryptographic verification:

```bash
# Evidence includes SHA-256 hash for integrity
{
  "evidence_id": "PERF_20240101_120000",
  "integrity": {
    "hash_algorithm": "SHA-256",
    "evidence_hash": "a1b2c3d4..."
  }
}
```

### Audit Trail

- **Retention**: 7 years (SOX compliance)
- **Integrity**: Cryptographic verification
- **Access**: Audit trail for all operations

## 🚨 Troubleshooting

### Common Issues

1. **Import Errors**
   ```bash
   pip install -r requirements.txt
   ```

2. **GitHub API Rate Limits**
   - Use `--dry-run` for testing
   - Ensure `GITHUB_TOKEN` is set

3. **Benchmark Timeout**
   ```bash
   python3 bench_runner.py --timeout 600  # 10 minutes
   ```

4. **Performance Variance**
   - Run in performance mode: `PERF=1`
   - Check system load and background processes

### Debug Mode

Enable verbose logging:

```bash
export DEBUG=1
python3 bench_runner.py --output perf.json
```

## 📚 Documentation

### API Documentation
- [Benchmark Reporting API Contracts](../../docs/benchmark-reporting-api-contracts.md)
- [SLO Validation Specification](../../docs/slo-validation-regression-detection-spec.md)
- [Enterprise Audit Compliance](../../docs/enterprise-audit-compliance-reporting-spec.md)

### Architecture Documentation
- [Benchmark Infrastructure Architecture](../../docs/explanation/benchmark-reporting-architecture.md)
- [Performance Monitoring Specification](../../docs/issue-52-technical-specification.md)

## 🤝 Contributing

### Development Setup

```bash
# Install development dependencies
pip install -r requirements.txt black flake8 mypy pytest

# Run tests
pytest tests/

# Format code
black scripts/bench/

# Type checking
mypy scripts/bench/
```

### Adding New Utilities

1. Follow the established pattern in existing utilities
2. Include comprehensive error handling
3. Add configuration options to `config/thresholds.toml`
4. Update documentation and tests

## 📞 Support

### Issues and Questions

- **Performance Issues**: Create issue with `performance` label
- **Compliance Questions**: Contact enterprise compliance team
- **Integration Support**: See [CI/CD Integration Guide](../../docs/github-actions-ci-integration-spec.md)

### Enterprise Support

For enterprise deployments, additional support is available for:
- Custom compliance requirements
- Integration with enterprise monitoring systems
- Performance optimization consulting

---

**Report Issues:** [GitHub Issues](https://github.com/copybook-rs/copybook-rs/issues)
**Documentation:** [docs/](../../docs/)
**License:** MIT License