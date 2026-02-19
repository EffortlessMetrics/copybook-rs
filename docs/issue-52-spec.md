<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue #52: Machine-Readable Benchmark Reporting Infrastructure

## Context

copybook-rs requires enterprise-grade performance monitoring infrastructure to support continuous integration, performance regression detection, and regulatory audit requirements for mainframe data processing workloads. Currently, benchmark results are only available in human-readable format, limiting automated performance validation and enterprise compliance reporting.

The system processes COBOL copybooks through a critical data pipeline (COBOL Parsing → Field Layout → Data Encoding/Decoding → CLI Processing → Output) that must maintain strict performance SLOs for enterprise mainframe workloads. Without machine-readable benchmark reporting, teams cannot automatically validate performance regressions or generate audit-compliant performance reports required for enterprise deployments.

This enhancement will establish comprehensive benchmark automation infrastructure including JSON output, PR automation, baseline promotion workflows, and enterprise audit capabilities while maintaining the existing performance floors (DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s).

## User Story

As an enterprise developer integrating copybook-rs into production mainframe data processing pipelines, I want machine-readable benchmark reporting with automated performance validation so that I can detect performance regressions early, generate compliance reports for regulatory audits, and maintain confidence in production deployment performance.

## Acceptance Criteria

AC1: Create scripts/bench/ directory structure with Python utilities for benchmark automation and JSON report generation

AC2: Implement simplified perf.json schema with required fields: {"display_gibs": 4.22, "comp3_mibs": 571.0, "warnings": [], "errors": []} supporting decimal precision

AC3: Generate machine-readable benchmark reports via `PERF=1 cargo bench -p copybook-bench` with JSON output to scripts/bench/perf.json

AC4: Implement PR comment automation system that posts one-liner performance summaries to GitHub PRs with format "DISPLAY: X.XX GiB/s, COMP-3: XXX MiB/s [status]"

AC5: Create baseline promotion workflow that updates performance baselines automatically when PRs merge to main branch

AC6: Validate SLO compliance automatically with performance floor enforcement (DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s) and regression detection

AC7: Support enterprise audit capabilities with historical performance tracking and compliance report generation

AC8: Ensure machine-readable reports capture benchmark warnings and errors for comprehensive performance diagnostics

AC9: Integrate with existing copybook-bench infrastructure without breaking current benchmark execution or human-readable output

AC10: Provide comprehensive error handling and validation for JSON output generation with proper anyhow::Result<T> patterns

## Technical Implementation Notes

- **Affected crates**: copybook-bench (primary), scripts/bench/ (new Python utilities)
- **Pipeline stages**: Performance monitoring affects entire data processing pipeline (COBOL parsing, field layout, encoding/decoding, CLI processing, output)
- **Performance considerations**: Must maintain existing performance targets while adding monitoring overhead <2%, enterprise audit compliance requirements, memory efficiency for large benchmark datasets
- **COBOL parsing requirements**: Benchmark reporting must support DISPLAY, COMP-3, and binary data performance validation with accuracy verification via `cargo nextest run --workspace`
- **Enterprise validation**: Machine-readable reports must support mainframe compatibility validation via `PERF=1 cargo bench -p copybook-bench` with regulatory audit capabilities
- **Workspace features**: JSON reporting must work across all 5 crates with comprehensive feature compatibility validation
- **Copybook compatibility**: Performance monitoring must support all copybook formats with field alignment and validation via `cargo xtask ci`
- **Testing strategy**: TDD implementation with `// AC:ID` tags, comprehensive workspace testing, enterprise validation, performance baseline establishment and regression detection
- **Dependencies**: Must resolve Issue #49 (benchmark hanging) before implementation to ensure stable benchmark execution
- **Automation requirements**: PR comment automation, baseline promotion workflows, CI/CD integration with GitHub Actions
- **Audit compliance**: Historical performance tracking, compliance report generation, enterprise regulatory requirements
- **Error taxonomy**: Comprehensive error handling for benchmark failures, JSON generation issues, automation failures with structured error codes
- **Zero unsafe code**: All Python utilities and Rust benchmark extensions must maintain zero unsafe code requirements
- **Deterministic processing**: Ensure reproducible benchmark results across runs with proper validation and statistical analysis
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
