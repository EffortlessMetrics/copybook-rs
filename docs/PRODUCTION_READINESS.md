# Production Readiness Assessment

## Status: ⚠️ Under Validation (v0.3.1)

copybook-rs currently serves teams that need a memory-safe COBOL parser with trustworthy error reporting, but we are deliberately avoiding "production-ready" language. Throughput still falls short of historic GiB/s targets, benchmark automation is missing, and several COBOL constructs remain unsupported. This document summarises the latest evidence so adopters can make informed decisions.

## Current Evidence

| Area | Observation | Source |
|------|-------------|--------|
| Tests | `cargo nextest` reports 461/462 passing; `copybook-core::test_ac4_performance_large_scale_odo_tail_violation_fail` remains timing-sensitive; eight leak detectors flagged | `integrative_gate_summary.md`
| Performance | Receipts live at `scripts/bench/perf.json` (generated via `scripts/bench.sh`); current measurements are advisory-only and below historic GiB/s targets; see `PERFORMANCE_VALIDATION_FINAL.md` for the latest summarized run | `scripts/bench/perf.json`, `PERFORMANCE_VALIDATION_FINAL.md`
| Memory | Streaming architecture stays below 256 MiB on reference fixtures | Bench logs (`performance-final-validation.log`)
| COBOL Coverage | Missing COMP-1/COMP-2, edited PIC clauses, SIGN SEPARATE, nested ODOs, RENAMES (66-level), and 88-level condition names | `README.md`, parser backlog
| Tooling | Benchmark utilities (`bench_runner.py`, `baseline_manager.py`, `slo_validator.py`, etc.) not implemented; backlog opened | `docs/backlog/benchmark_tooling.md`

## Strengths

- Zero `unsafe` in public APIs; clippy pedantic enforcement across the workspace
- Deterministic encode/decode flows with rich error taxonomy (CBKP*/CBKS*/CBKD*/CBKE*)
- Streaming design keeps memory usage predictable on multi-GB fixtures
- Comprehensive documentation of limitations and validation evidence

## Gaps & Risks

- Throughput remains in the tens of MiB/s; both SLO checks fail for COMP-3 workloads
- Missing benchmark automation leaves regressions hard to detect
- Unsupported COBOL constructs can block adoption without upfront schema audits
- Timing-sensitive test and leak detectors undermine CI confidence

## Required Actions Before Renewed Production Claims

1. Restore benchmark automation (Issue #52) and gate on SLO verdicts
2. Address the timing-sensitive failure and clear the eight leak detectors
3. Decide on a roadmap for unsupported COBOL constructs (implement vs document hard warnings)
4. Re-baseline performance targets and publish repeatable receipts once improvements land

## Interim Guidance

- Run pilot projects only after validating copybooks against the supported feature set
- Record local benchmark results alongside `scripts/bench/perf.json` to track drift (sample fixture: `scripts/bench/test_perf.sample.json`)
- Reference `README.md`, `docs/REPORT.md`, and this assessment when communicating status to stakeholders
- Avoid marketing copy that implies production readiness until the actions above are complete

_Last updated: 2025-09-30_
