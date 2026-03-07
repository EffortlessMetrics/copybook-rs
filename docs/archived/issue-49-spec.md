<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue #49: Performance Regression Monitoring and Benchmark Optimization

## Context

copybook-rs requires reliable performance regression monitoring to maintain enterprise mainframe data processing throughput guarantees. Issue #52 (PR #67) delivered foundational infrastructure for machine-readable benchmark reporting, baseline management, and CI integration. However, several gaps remain that prevent comprehensive performance regression detection and optimization workflows:

1. **Performance Baseline Reconciliation**: Significant discrepancy between CLAUDE.md (DISPLAY: 2.33 GiB/s, COMP-3: 168-176 MiB/s) and REPORT.md (DISPLAY: 66-95 MiB/s, COMP-3: 18-25 MiB/s) performance numbers requires investigation and establishment of canonical baseline with documented measurement methodology.

2. **Regression Detection Validation**: Existing infrastructure from Issue #52 provides baseline comparison (`bench-report compare`), but threshold behavior (>5% WARNING, >10% FAILURE) needs systematic validation across PR scenarios to ensure reliable gate enforcement.

3. **CI Integration Gaps**: While `.github/workflows/benchmark.yml` includes baseline promotion and artifact management, missing baseline scenario handling and regression failure thresholds need comprehensive validation to prevent false positives in PR workflows.

4. **Developer Productivity Tools**: Progressive complexity testing (1KB → 10KB → 100KB → 1MB) would enable performance profiling and optimization iteration without full enterprise-scale benchmark overhead. These developer-mode tools should remain in `PERF=1` mode to avoid PR gate bloat.

5. **Diagnostic Infrastructure**: Enhanced logging, resource monitoring, and benchmark health checks would improve troubleshooting when performance degradations occur, supporting faster root cause analysis for enterprise deployments.

**Affected copybook-rs Components**:
- `copybook-bench`: Benchmark suite with SLO validation and regression detection
- `.github/workflows/benchmark.yml`: CI integration with baseline management
- `docs/`: Performance documentation requiring reconciliation (CLAUDE.md, REPORT.md)

**Data Processing Pipeline Impact**:
- **COBOL Parsing**: Indirect impact through validation of parsing performance baselines
- **Field Layout**: Indirect impact through schema resolution performance validation
- **Data Encoding/Decoding**: Primary impact - core throughput measurements for DISPLAY and COMP-3 conversion
- **CLI Processing**: Indirect impact through end-to-end workflow performance validation
- **Output**: Indirect impact through JSONL generation performance tracking

**Performance Implications**:
- Enterprise targets: DISPLAY ≥80 MB/s floor, COMP-3 ≥40 MB/s floor (minimum production requirements)
- Current measurements require reconciliation and canonical baseline establishment
- Regression detection critical for maintaining throughput guarantees in production mainframe workloads
- Zero unsafe code enforcement maintained throughout performance optimization work

## User Story

As an enterprise developer maintaining copybook-rs for mainframe data processing, I want a reliable performance regression monitoring system so that I can detect performance degradations early and maintain production-ready throughput guarantees for large-scale COBOL data conversion workflows.

## Acceptance Criteria

**AC1: Regression Detection Validation**
- Verify >5% performance degradation triggers WARNING status in `bench-report compare` output
- Verify >10% performance degradation triggers FAILURE status in `bench-report compare` output
- Test missing baseline scenario (first-time PRs) returns NEUTRAL status without failing CI
- Validate threshold calculation against baseline.json using synthetic performance deltas
- Document expected behavior in `docs/performance-regression-testing.md` with test scenarios

**AC2: Performance Baseline Reconciliation**
- Establish canonical performance baseline with documented hardware specifications (CPU, RAM, OS)
- Reconcile REPORT.md vs CLAUDE.md performance number discrepancies through comprehensive benchmark runs
- Document measurement methodology including environment setup, Criterion configuration, and statistical approach
- Run comprehensive benchmark suite (`PERF=1 cargo bench -p copybook-bench`) on clean environment to establish authoritative baseline
- Update both CLAUDE.md and REPORT.md with consistent, verified performance numbers and measurement context
- Create `docs/performance-measurement-methodology.md` documenting reproducible benchmark procedures

**AC3: CI Regression Gating Validation**
- Confirm PR comments display regression warnings with delta percentages and baseline comparison
- Verify artifact uploads (`perf.json`, `baseline-main-*.zip`) with 90-day retention policy for audit compliance
- Test baseline promotion on main branch merge creates `target/baselines/performance.json` correctly
- Validate 30-minute timeout protection prevents stuck benchmark runners in CI
- Test missing baseline scenario gracefully degrades to NEUTRAL without PR failure
- Document CI workflow behavior in `.github/workflows/benchmark.yml` inline comments

**AC4: Progressive Complexity Testing (Developer Mode)**
- Implement 1KB → 10KB → 100KB → 1MB progressive benchmark suite in `copybook-bench/benches/progressive.rs`
- Add early bailout if execution time exceeds configurable threshold (default: 10 seconds per size tier)
- Keep progressive benchmarks in `PERF=1` developer-only mode (not executed in PR CI to avoid gate bloat)
- Useful for performance profiling with tools like `cargo flamegraph`, `perf`, or `valgrind`
- Document usage in CLAUDE.md under "Performance Profiling" section with example workflows

**AC5: Enhanced Diagnostics and Monitoring**
- Implement benchmark health check utility (`bench-report health-check`) validating environment setup
- Add verbose logging mode (`--verbose` flag) for `bench-report` commands showing detailed calculation steps
- Implement resource monitoring where platform-supported (memory usage, CPU utilization) during benchmark runs
- Create diagnostic benches (`copybook-bench/benches/diagnostics.rs`) for infrastructure testing (baseline I/O, JSON parsing overhead)
- Document diagnostic procedures in `docs/troubleshooting-performance.md` with common failure scenarios

## Technical Implementation Notes

**Affected Crates**:
- `copybook-bench`: Enhanced benchmark suite with progressive complexity and diagnostics
- `copybook-cli`: No direct changes (indirect validation through end-to-end performance testing)
- `copybook-core`: No direct changes (indirect validation through parsing performance baselines)
- `copybook-codec`: No direct changes (primary throughput measurement target)

**Pipeline Stages**:
- **COBOL Parsing**: Indirect validation through parsing performance baseline establishment
- **Field Layout**: Indirect validation through schema resolution performance tracking
- **Data Encoding/Decoding**: Primary focus - DISPLAY and COMP-3 throughput regression detection
- **CLI Processing**: Indirect validation through end-to-end workflow performance benchmarks
- **Output**: Indirect validation through JSONL generation throughput tracking

**Performance Considerations**:
- Enterprise floor targets: DISPLAY ≥80 MB/s, COMP-3 ≥40 MB/s (minimum production requirements)
- Current baseline requires reconciliation between documentation sources
- Regression thresholds: >5% WARNING (investigation recommended), >10% FAILURE (PR gate enforcement)
- Progressive complexity testing enables optimization iteration without full benchmark overhead
- Zero unsafe code enforcement maintained throughout performance work
- Memory efficiency: Maintain <256 MiB steady-state for streaming operations

**COBOL Parsing Requirements**:
- Validate parsing performance baselines for DISPLAY, COMP-3, binary data types
- Comprehensive validation via `cargo nextest run --workspace` for correctness preservation
- Performance optimization must not compromise parsing accuracy or error handling quality

**Enterprise Validation**:
- Mainframe compatibility validation via `PERF=1 cargo bench -p copybook-bench`
- 90-day artifact retention for audit compliance and historical trend analysis
- Baseline promotion on main branch ensures stable performance targets for PR comparisons
- Machine-readable `perf.json` format suitable for enterprise monitoring system integration

**Workspace Features**:
- Comprehensive benchmark coverage across 5 crates (core, codec, cli, gen, bench)
- Integration with existing `bench-report` CLI from Issue #52 foundation
- Backward compatibility with current `PERF=1 cargo bench` workflow patterns
- Progressive complexity testing remains developer-only (not PR-gated) to avoid CI bloat

**Copybook Compatibility**:
- Performance benchmarks exercise comprehensive copybook patterns (ODO, REDEFINES, Level-88)
- Field alignment and format validation preserved during optimization work
- Enterprise data processing scenarios validated through golden fixture integration

**Testing Strategy**:
1. **Regression Threshold Validation** (AC1):
   - Create synthetic baseline with known throughput values
   - Generate performance reports with +3%, +6%, +12% deltas
   - Verify WARNING/FAILURE status transitions at threshold boundaries
   - Test missing baseline scenario returns NEUTRAL without failure
   - Tag tests with `// AC1: Regression threshold validation`

2. **Baseline Reconciliation** (AC2):
   - Run `PERF=1 cargo bench -p copybook-bench` on clean environment
   - Document hardware specs, OS version, Rust toolchain in measurement report
   - Compare CLAUDE.md vs REPORT.md vs measured values for discrepancies
   - Update documentation with authoritative baseline and methodology
   - Tag validation tests with `// AC2: Baseline reconciliation`

3. **CI Integration Validation** (AC3):
   - Mock PR scenarios with baseline comparison
   - Validate artifact upload and retention policy enforcement
   - Test baseline promotion creates correct `performance.json` structure
   - Verify timeout protection prevents stuck runners
   - Tag integration tests with `// AC3: CI regression gating`

4. **Progressive Complexity** (AC4):
   - Implement benchmarks for 1KB, 10KB, 100KB, 1MB data sizes
   - Add early bailout logic with configurable thresholds
   - Validate PERF=1-only execution (not in PR CI)
   - Document profiling workflow with `cargo flamegraph` integration
   - Tag benchmarks with `// AC4: Progressive complexity testing`

5. **Enhanced Diagnostics** (AC5):
   - Implement `bench-report health-check` command
   - Add `--verbose` flag for detailed calculation logging
   - Create diagnostic benchmarks for infrastructure validation
   - Document troubleshooting procedures with common failure scenarios
   - Tag diagnostic utilities with `// AC5: Enhanced diagnostics`

**Dependencies**:
- ✅ Issue #52 (PR #67 merged): Machine-readable benchmark reporting infrastructure foundation
- ✅ Issue #48 (closed): Binary round-trip encoding resolved (no blocker)
- ✅ Issue #49 hanging issue (RESOLVED): Original hanging problem fixed, validation work remains

**Risk Mitigation**:
- Performance optimization work must preserve correctness: `cargo nextest run --workspace` must pass
- Regression thresholds require validation to prevent false positive PR failures
- Documentation reconciliation critical for stakeholder confidence in performance claims
- Progressive complexity testing enables iterative optimization without full benchmark overhead
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
