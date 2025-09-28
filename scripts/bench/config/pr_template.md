# copybook-rs Performance Report Template

<!--
This template is used by pr_automation.py to generate standardized
performance comments on GitHub pull requests. It follows enterprise
reporting standards and regulatory compliance requirements.
-->

## 🚀 copybook-rs Performance Report

**Report Generated:** {timestamp}
**Environment:** {environment}
**copybook-rs Version:** {version}

### 📊 Enterprise Performance Metrics

| Metric | Current | Target | Status | Compliance |
|--------|---------|--------|--------|------------|
| **DISPLAY Parsing** | {display_gibs} GiB/s | ≥{display_target} GiB/s | {display_status} | {display_compliance}% |
| **COMP-3 Encoding** | {comp3_mibs} MiB/s | ≥{comp3_target} MiB/s | {comp3_status} | {comp3_compliance}% |

### 📈 Performance Analysis

**Overall Performance:** {overall_status}
**Enterprise Compliance:** {enterprise_compliance}%

{baseline_comparison_section}

{regression_analysis_section}

### 🎯 Performance Targets

copybook-rs maintains enterprise-grade performance standards:

- **DISPLAY Processing:** Minimum 4.1 GiB/s for enterprise deployment
- **COMP-3 Encoding:** Minimum 560 MiB/s for mainframe compatibility
- **Performance Variance:** <5% for production stability
- **Regression Threshold:** <2% degradation tolerance

### 📋 Quality Assurance

- **Zero Unsafe Code:** ✅ Maintained
- **Memory Safety:** ✅ Rust guarantees
- **Deterministic Processing:** ✅ Consistent results
- **Cross-Platform Compatibility:** ✅ Validated

{warnings_section}

{errors_section}

{recommendations_section}

### 🏢 Enterprise Readiness

**Regulatory Compliance:**
- **SOX:** Data integrity and processing consistency ✅
- **HIPAA:** Healthcare data processing performance ✅
- **PCI-DSS:** Payment processing real-time requirements ✅
- **GDPR:** Data portability and processing efficiency ✅
- **ISO 27001:** Security performance impact assessment ✅

**Production Deployment Status:** {deployment_status}

### 🔧 Technical Details

- **Benchmark Mode:** {perf_mode}
- **CI Environment:** {ci_environment}
- **Execution Time:** {execution_time}
- **Memory Usage:** {memory_usage}
- **CPU Utilization:** {cpu_usage}

### 📈 Historical Context

{performance_trend}

{stability_metrics}

### 🚨 Alerts and Notifications

{alert_summary}

### 📋 Action Items

{action_items}

### 🔍 Deep Dive Analysis

For detailed performance analysis and optimization recommendations:

- **Benchmark Details:** `cargo bench --package copybook-bench`
- **Profiling Data:** Available in CI artifacts
- **Performance Guide:** [docs/benchmark-reporting-api-contracts.md](docs/benchmark-reporting-api-contracts.md)
- **Enterprise Guide:** [docs/enterprise-deployment-guide.md](docs/enterprise-deployment-guide.md)

### 📞 Support and Escalation

**Performance Issues:**
- Create issue with `performance` label
- Tag `@copybook-rs/performance-team`
- Include benchmark reproduction steps

**Regression Concerns:**
- Immediate escalation for >5% degradation
- Performance team review for >2% degradation
- Statistical validation for <2% changes

---

### 🔒 Compliance and Audit

**Audit Trail ID:** {audit_id}
**Evidence Hash:** `{evidence_hash}`
**Compliance Officer:** Enterprise Compliance Team
**Next Review:** {next_review_date}

### 📚 Documentation References

- [Performance Architecture](docs/explanation/benchmark-reporting-architecture.md)
- [SLO Validation](docs/slo-validation-regression-detection-spec.md)
- [Enterprise Audit](docs/enterprise-audit-compliance-reporting-spec.md)
- [CI/CD Integration](docs/github-actions-ci-integration-spec.md)

---

*This report was generated automatically by the copybook-rs benchmark infrastructure.*
*Report ID: {report_id} | Generated at: {generation_timestamp}*

<!-- Template Variables Reference:
{timestamp} - Report generation timestamp
{environment} - Execution environment (CI/Local)
{version} - copybook-rs version
{display_gibs} - DISPLAY parsing throughput
{display_target} - DISPLAY performance target
{display_status} - ✅/❌ status indicator
{display_compliance} - Compliance percentage
{comp3_mibs} - COMP-3 encoding throughput
{comp3_target} - COMP-3 performance target
{comp3_status} - ✅/❌ status indicator
{comp3_compliance} - Compliance percentage
{overall_status} - Overall performance status
{enterprise_compliance} - Overall compliance percentage
{baseline_comparison_section} - Baseline comparison (if available)
{regression_analysis_section} - Regression analysis (if applicable)
{warnings_section} - Performance warnings
{errors_section} - Performance errors
{recommendations_section} - Optimization recommendations
{deployment_status} - Production readiness status
{perf_mode} - PERF=1 enabled/disabled
{ci_environment} - CI environment detection
{execution_time} - Benchmark execution time
{memory_usage} - Memory consumption
{cpu_usage} - CPU utilization
{performance_trend} - Historical performance trend
{stability_metrics} - Performance stability analysis
{alert_summary} - Performance alerts summary
{action_items} - Required actions
{audit_id} - Compliance audit trail ID
{evidence_hash} - Evidence integrity hash
{next_review_date} - Next compliance review
{report_id} - Unique report identifier
{generation_timestamp} - Report generation timestamp
-->