<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# ADR-001: Machine-Readable Benchmark Reporting Infrastructure Architecture

## Status
Accepted

## Context

copybook-rs requires enterprise-grade performance monitoring infrastructure to support continuous integration, performance regression detection, and regulatory audit requirements for mainframe data processing workloads. The current benchmark system provides human-readable output via Criterion.rs but lacks machine-readable formats necessary for automated performance validation and enterprise compliance reporting.

### Business Drivers
- **Enterprise Deployment Requirements**: Production mainframe workloads require automated performance validation against SLO floors (DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s)
- **Regulatory Compliance**: Financial institutions and enterprise customers require comprehensive audit trails and compliance reporting (SOX, PCI-DSS, etc.)
- **Development Velocity**: Teams need automated performance feedback in pull requests to prevent performance regressions
- **Operational Excellence**: Production deployments require confidence in performance characteristics with documented safety margins

### Technical Context
- **Existing Infrastructure**: Sophisticated Criterion.rs benchmark suite with 1,976 lines of statistical regression analysis
- **Performance Standards**: Current performance (DISPLAY: 4.2+ GiB/s, COMP-3: 570+ MiB/s) exceeds targets by 15-52x margins
- **Zero Unsafe Code**: All components must maintain copybook-rs zero unsafe code standards
- **TDD Approach**: Implementation must follow TDD patterns with comprehensive test coverage

### Problem Statement
The absence of machine-readable benchmark reporting creates several operational challenges:

1. **Manual Performance Validation**: Teams must manually analyze Criterion.rs output to assess performance changes
2. **Limited CI/CD Integration**: No automated performance gates or regression detection in CI pipelines
3. **Compliance Gaps**: Insufficient audit trails and reporting for enterprise regulatory requirements
4. **Baseline Management**: No systematic approach to managing performance baselines across releases
5. **Risk Assessment**: Difficulty in assessing enterprise readiness and deployment risk

## Decision

We will implement a comprehensive machine-readable benchmark reporting infrastructure consisting of:

### Core Architecture Components

1. **JSON Reporting Framework**
   - Simplified JSON schema with required fields: `{"display_gibs": 4.22, "comp3_mibs": 571.0, "warnings": [], "errors": []}`
   - Enterprise metadata including environment context, statistical properties, and audit trail
   - Schema versioning for backward compatibility and evolution

2. **Python Utilities Suite** (`scripts/bench/`)
   - `bench_runner.py`: Main benchmark execution coordinator with timeout handling
   - `slo_validator.py`: Performance floor validation against enterprise thresholds
   - `pr_automation.py`: GitHub PR comment automation with safety margin reporting
   - `baseline_manager.py`: Automated baseline promotion and management workflows
   - `audit_generator.py`: Enterprise audit reporting with regulatory compliance

3. **CI/CD Integration Framework**
   - GitHub Actions workflows for automated performance validation
   - PR comment automation with one-liner performance summaries
   - Baseline promotion on merge to main branch
   - Enterprise audit report generation (scheduled weekly)

4. **SLO Validation Engine**
   - Enterprise performance floor enforcement (DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s)
   - Safety margin calculation and monitoring (current: 52x DISPLAY, 15x COMP-3)
   - Statistical significance testing with confidence intervals
   - Compliance level assessment for enterprise readiness

5. **Enterprise Audit Framework**
   - Regulatory compliance validation (SOX, PCI-DSS, HIPAA, GDPR)
   - Historical performance tracking with trend analysis
   - Risk assessment and enterprise readiness scoring
   - Comprehensive reporting in multiple formats (HTML, JSON, PDF)

### Integration Strategy

**Leverage Existing Infrastructure**: Build upon the sophisticated regression detection system in `copybook-bench/src/regression.rs` rather than replacing it. This preserves the investment in statistical analysis while adding enterprise-grade automation.

**Conditional Activation**: JSON reporting only activates when `GENERATE_JSON_REPORT=1` environment variable is set, ensuring zero performance impact on normal benchmark execution.

**Backward Compatibility**: Maintain full compatibility with existing Criterion.rs benchmarks and human-readable output formats.

## Alternatives Considered

### Alternative 1: Extend Criterion.rs Directly
**Rejected**: Would require forking Criterion.rs or significant modifications to upstream dependency. Limited flexibility for enterprise-specific requirements and compliance features.

### Alternative 2: Replace Criterion.rs with Custom Framework
**Rejected**: Would lose sophisticated statistical analysis and established benchmark patterns. Significant reimplementation effort with unclear benefits.

### Alternative 3: External Monitoring Service Integration
**Rejected**: Introduces external dependencies and potential data privacy concerns for enterprise customers. Limited customization for copybook-rs specific requirements.

### Alternative 4: Simple JSON Output Only
**Rejected**: Insufficient for enterprise requirements. Lacks comprehensive audit capabilities, regulatory compliance features, and sophisticated automation required for production deployments.

## Consequences

### Positive Consequences

1. **Automated Performance Validation**
   - Immediate feedback on performance changes in pull requests
   - Automated regression detection with statistical significance testing
   - Reduced manual effort in performance analysis

2. **Enterprise Readiness**
   - Comprehensive audit trails for regulatory compliance
   - Automated compliance validation against industry standards
   - Risk assessment and deployment readiness scoring

3. **Operational Excellence**
   - Systematic baseline management across releases
   - Historical trend analysis and predictive insights
   - Proactive performance monitoring with configurable alerts

4. **Developer Experience**
   - Clear, actionable performance feedback in PRs
   - One-click baseline promotion workflows
   - Comprehensive error handling with structured error codes

### Negative Consequences

1. **Implementation Complexity**
   - Significant development effort across multiple languages (Rust, Python)
   - Complex integration testing requirements
   - Maintenance overhead for multiple report formats

2. **Infrastructure Dependencies**
   - Additional Python runtime requirements in CI/CD
   - GitHub API rate limiting considerations
   - Storage requirements for historical data and baselines

3. **Performance Overhead**
   - JSON generation adds <2% overhead when enabled
   - Additional CI/CD execution time for comprehensive reporting
   - Storage and bandwidth costs for audit data retention

### Risk Mitigation

1. **Dependency Risk**: Pin Python dependencies with comprehensive testing to prevent version conflicts
2. **API Rate Limiting**: Implement exponential backoff and retry logic for GitHub API interactions
3. **Data Privacy**: Ensure no sensitive data leakage in performance reports with anonymization features
4. **Performance Impact**: Conditional activation ensures zero impact when JSON reporting is disabled

## Implementation Plan

### Phase 1: Foundation (Days 1-2)
- JSON schema definition and validation framework
- Basic Python utilities for benchmark execution and SLO validation
- Integration with existing Criterion.rs infrastructure

### Phase 2: CI Automation (Days 3-4)
- GitHub Actions workflow integration
- PR comment automation with performance summaries
- Status check integration for automated gates

### Phase 3: Baseline Management (Days 5-6)
- Automated baseline promotion workflows
- Historical tracking and regression detection
- Cleanup and retention policy implementation

### Phase 4: Enterprise Audit (Days 7-8)
- Comprehensive audit report generation
- Regulatory compliance validation
- Multi-format export capabilities (HTML, JSON, PDF)

### Phase 5: Integration Testing (Days 9-10)
- End-to-end workflow validation
- Performance impact assessment
- Production readiness verification

## Monitoring and Success Criteria

### Quantitative Metrics
- JSON reporting overhead: <2% of baseline performance
- PR comment success rate: >98%
- Baseline promotion accuracy: >99%
- Enterprise audit generation time: <30 seconds for 90-day history

### Qualitative Metrics
- Developer satisfaction with automated performance feedback
- Enterprise customer confidence in compliance reporting
- Reduced time-to-deployment for performance-related changes
- Improved accuracy of performance regression detection

## Review Schedule

This ADR will be reviewed in 6 months (March 2025) to assess:
- Implementation effectiveness and developer adoption
- Performance impact validation and optimization opportunities
- Enterprise customer feedback on audit and compliance features
- Potential enhancements based on operational experience

## Related Decisions

- **ADR-002** (Future): Performance Monitoring Data Retention and Privacy
- **ADR-003** (Future): Enterprise Integration Patterns and API Evolution
- **ADR-004** (Future): Compliance Framework Extensions for Industry-Specific Regulations

## References

- [Issue #52: Machine-Readable Benchmark Reporting Infrastructure](https://github.com/copybook-rs/copybook-rs/issues/52)
- [copybook-bench Performance Regression Detection](../copybook-bench/src/regression.rs)
- [Enterprise Performance Requirements](../CLAUDE.md#performance)
- [Criterion.rs Statistical Analysis Patterns](https://bheisler.github.io/criterion.rs/book/index.html)
- [SOX Compliance Requirements for IT Systems](https://www.sec.gov/rules/final/33-8238.htm)
- [NIST Cybersecurity Framework](https://www.nist.gov/cyberframework)

## Document Metadata

- **Author**: copybook-rs Development Team
- **Created**: 2025-09-28
- **Status**: Accepted
- **Reviewers**: Enterprise Architecture Team, Security Team, Compliance Team
- **Next Review**: 2025-03-28
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
