<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Production Readiness Assessment

## Status: ⚠️ Engineering Preview (v0.4.3)

**Status alignment rule**: Support claims align with [COBOL_SUPPORT_MATRIX.md](reference/COBOL_SUPPORT_MATRIX.md), performance claims align with [BENCHMARKS.md](reference/BENCHMARKS.md), and version posture aligns with [ROADMAP.md](ROADMAP.md). Release-gate policy (including security scope and deterministic evidence requirements) is driven by `.github/workflows/release-readiness.yml`; blocker notes are date-labeled in this section and cross-checked in [docs/REPORT.md](REPORT.md). Historical blocker notes are date-labeled.

copybook-rs currently serves teams that need a memory-safe COBOL parser with trustworthy error reporting, but we are deliberately avoiding "production-ready" language. Throughput in canonical receipts exceeds advisory SLOs (DISPLAY 205 MiB/s, COMP-3 58 MiB/s), benchmark automation is intermittent, and some COBOL constructs are experimental or unsupported by policy. This document summarises the latest evidence so adopters can make informed decisions.

## Current Evidence

| Area | Observation | Source |
|------|-------------|--------|
| Tests | `cargo test --workspace` reports 1652+ tests passing (10 ignored) | CI artifacts
| Performance | Receipts live at `scripts/bench/perf.json`; baseline: DISPLAY 205 MiB/s, COMP-3 58 MiB/s (see `docs/PERFORMANCE_GOVERNANCE.md`) | `scripts/bench/perf.json`, `docs/PERFORMANCE_GOVERNANCE.md`
| Memory | Streaming architecture stays below 256 MiB on reference fixtures | Bench logs (`performance-final-validation.log`)
| COBOL Coverage | COMP-1/COMP-2 and SIGN SEPARATE are fully supported (promoted to stable in v0.4.3); edited PIC and 88-level are implemented; nested ODOs, ODO-over-REDEFINES, and ODO-over-RENAMES remain out of scope; RENAMES is partially supported (R1-R3, R4-R6 policy-limited) | `README.md`, `reference/COBOL_SUPPORT_MATRIX.md`, parser backlog |
| Tooling | Benchmark utilities (`bench_runner.py`, `baseline_manager.py`, `slo_validator.py`, etc.) not implemented; backlog opened | `docs/backlog/benchmark_tooling.md`

## Strengths

- Zero `unsafe` in public APIs; clippy pedantic enforcement across the workspace
- Deterministic encode/decode flows with rich error taxonomy (CBKP*/CBKS*/CBKD*/CBKE*)
- Streaming design keeps memory usage predictable on multi-GB fixtures
- Comprehensive documentation of limitations and validation evidence

## Gaps & Risks

- Throughput exceeds advisory SLO checks (80/40 MiB/s) in canonical receipts; environment variance remains workload-sensitive
- Missing benchmark automation leaves regressions hard to detect
- Unsupported COBOL constructs can block adoption without upfront schema audits
- Timing-sensitive test and leak detectors were resolved (Date: 2026-02-28); blocker history is tracked in `QUALITY_BLOCKERS.md`

## Release Blocker Register

| Blocker | Status | Owner | Timestamp | Evidence |
|---------|--------|-------|-----------|----------|
| `TODO(AC5)` (`copybook-core/src/audit/security.rs`) | pass | @EffortlessSteven | 2026-02-28 | (status=pass; owner=@EffortlessSteven; evidence=[security audit implementation](copybook-core/src/audit/security.rs), [security audit tests](copybook-core/src/audit/security.rs), [AC5 gate contract](copybook-core/src/audit/mod.rs)) |
| `TODO(AC11)` (`copybook-core/src/audit/performance.rs`) | pass | @EffortlessSteven | 2026-02-28 | (status=pass; owner=@EffortlessSteven; evidence=[AC11 objective checks](copybook-cli/src/commands/audit.rs), [performance baseline model](copybook-core/src/audit/performance.rs)) |
| `TODO(AC16)` (`copybook-core/src/audit/lineage.rs`) | pass | @EffortlessSteven | 2026-02-28 | (status=pass; owner=@EffortlessSteven; evidence=Parser and codec lineage IDs are emitted from `parse_with_audit_lineage_metadata` and `decode_file_to_jsonl`, and compared in `run_lineage_analysis` via `lineage_analysis_id`, `source_lineage_id`, `target_lineage_id`, `source_codec_lineage_id`, and `target_codec_lineage_id`.) |
| Environment-gate risk (`artifacts/perf/non-wsl/perf.json`) | fail | @EffortlessSteven | 2026-02-28 | (status=fail; owner=@EffortlessSteven; evidence=Release readiness blocks if `artifacts/perf/non-wsl/perf.json` is missing, `non_wsl_evidence_status` is not `available|available-from-cache`, or checksum/baseline fields are absent.) |

## Required Actions Before Renewed Production Claims

1. Restore benchmark automation (Issue #52) and gate on SLO verdicts
2. ✅ Address the timing-sensitive failure and clear the eight leak detectors (Date: 2026-02-28); blocker history is in `QUALITY_BLOCKERS.md`
3. Decide on a roadmap for unsupported COBOL constructs (implement vs document hard warnings)
4. Re-baseline performance targets and publish repeatable receipts once improvements land

## Interim Guidance

- Run pilot projects only after validating copybooks against the supported feature set
- Record local benchmark results alongside `scripts/bench/perf.json` to track drift (sample fixture: `scripts/bench/test_perf.sample.json`)
- Reference `README.md`, `docs/REPORT.md`, and this assessment when communicating status to stakeholders
- Avoid marketing copy that implies production readiness until the actions above are complete
- AC9 monitoring is intentionally **non-production/stub-only** and must not be treated as production evidence; for the shared engineering validation command, use the
  [Shared AC9 monitoring validation command](../README.md#ac9-monitoring-stub-validation-non-production-stub-only)
  (status=warn; owner=@EffortlessSteven; evidence=use the shared AC9 monitoring validation command for non-production/stub-only validation.)

_Last updated: 2026-02-28_
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
