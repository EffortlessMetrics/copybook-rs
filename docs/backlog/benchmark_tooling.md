# Benchmark Tooling Backlog (Issue #52)

**Status**: Open  
**Maintainer**: TBD → assign during next planning sync  
**Related artifacts**: `issue-52-ledger.md`, `test_perf.json`, `PERFORMANCE_VALIDATION_FINAL.md`

## Scope

Rebuild the benchmark and regression automation promised in Issue #52 so that performance telemetry stops depending on ad-hoc manual steps. The deliverable set was never implemented, yet legacy documentation still references the tooling.

## Deliverables

1. `scripts/bench/bench_runner.py`
   - Run configured Criterion benches with knobs for dataset selection, thread counts, and JSON output
   - Emit `PerformanceReport` documents that match `docs/schemas/performance_report.schema.json`
2. `scripts/bench/baseline_manager.py`
   - Manage on-disk baselines (create, promote, prune) with retention controls (default: keep last 10)
   - Record provenance metadata (commit SHA, Rust version, platform)
3. `scripts/bench/slo_validator.py`
   - Compare current benchmark outputs against SLO floors and tolerance bands
   - Produce machine-readable summaries plus CLI exit codes suitable for CI gating
4. `scripts/bench/regression_detector.py`
   - Compare current runs against nominated baselines using statistical thresholds
   - Emit GitHub-friendly markdown summaries and JSON diagnostics
5. `scripts/bench/audit_generator.py`
   - Produce compliance/audit packages combining raw telemetry, baselines, and SLO verdicts
   - Support HTML/PDF export hooks defined in `benchmark-reporting-api-contracts.md`
6. Documentation refresh
   - Update README, REPORT, and CLI docs once the utilities exist
   - Provide quickstart examples (`docs/how-to/benchmarking.md`) and maintenance SOPs

## Acceptance Criteria

- CLI commands return structured exit codes: `0` success, `2` validation failure, `3` regression detected, `4` tooling error
- JSON schema validation enforced for every artifact (performance reports, baselines, regression summaries)
- GitHub Actions workflow integration with dry-run + promoted baseline jobs
- Bench pipelines run within 5% overhead relative to direct `cargo bench`
- All utilities linted (ruff), tested (pytest), and covered by `cargo xtask ci`

## Milestones

| Milestone | Target | Owner | Notes |
|-----------|--------|-------|-------|
| Discovery + design refresh | 2025-01-31 | TBD | Confirm spec alignment, assign maintainers, unblock dependencies |
| MVP utilities (runner + baseline) | 2025-02-28 | TBD | Ship core scripts with schema validation and manual invocation docs |
| SLO + regression enforcement | 2025-03-31 | TBD | Integrate into CI, wire exit codes, publish sample reports |
| Audit packaging + doc sweep | 2025-04-30 | TBD | Produce compliance bundle, update public docs, close Issue #52 |

## Open Questions

- Are we retaining the legacy GiB/s targets or re-baselining around realistic MiB/s goals?
- Should we migrate from Python to Rust for longevity (e.g., expand `copybook-bench` CLI)?
- Where should generated artifacts live (`artifacts/perf/` vs dedicated S3 bucket)?

## Next Steps

1. Assign ownership during the next triage meeting and backfill this doc with maintainers.
2. Create GitHub issue linking to this backlog entry and close the documentation gap in `issue-52-ledger.md`.
3. Stand up MVP tooling (runner + baseline) and verify JSON output matches the schemas.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
