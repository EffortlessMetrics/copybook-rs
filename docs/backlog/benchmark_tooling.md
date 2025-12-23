# Benchmark Tooling Backlog (Issue #52)

**Status**: In progress (bench-report shipped; CI polish pending)  
**Maintainer**: TBD → assign during next planning sync  
**Related artifacts**: `scripts/bench/perf.json`, `docs/PERFORMANCE_GOVERNANCE.md`, `docs/PERFORMANCE_RECEIPT_REFERENCE.md`

## Scope

Close the remaining automation gaps around **performance receipts** (generation, baselines, regression summaries) so perf telemetry does not depend on ad-hoc manual steps.

**Note**: The original Issue #52 plan referenced Python utilities under `scripts/bench/`. In v0.4.0 that approach is effectively superseded by:
- `scripts/bench.sh` / `scripts\\bench.bat` generating canonical receipts at `scripts/bench/perf.json`
- The `bench-report` CLI (in `copybook-bench`) for local validation + baseline management
- The `perf.yml` workflow uploading `perf-json` artifacts and posting a neutral SLO check

## Deliverables

1. Receipts generation (DONE)
   - Canonical receipts: `scripts/bench/perf.json` (from `bash scripts/bench.sh`)
   - Host annotation + percentiles: `bash scripts/perf-annotate-host.sh`, `bash scripts/soak-aggregate.sh`
2. Local tooling (DONE)
   - `bench-report` CLI: validate, baseline promote/show, compare, summary
3. CI wiring (PARTIAL)
   - `perf.yml`: uploads `perf-json` artifact and posts a neutral SLO check (floors are advisory-only in v0.4.0)
   - Missing: PR comment summary (human-friendly markdown) and baseline promotion automation
4. Documentation refresh (PARTIAL)
   - Keep all public docs aligned to receipt-based reporting (`scripts/bench/perf.json`) and advisory-only floors
   - Remove stale references to the deprecated Python-script plan

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
- Should SLO violations remain advisory-only until the CI perf environment is stable?
- Where should generated artifacts live (`artifacts/perf/` vs dedicated S3 bucket)?

## Next Steps

1. Assign ownership during the next triage meeting and backfill this doc with maintainers.
2. Create GitHub issue linking to this backlog entry and close the documentation gap in `issue-52-ledger.md`.
3. Stand up MVP tooling (runner + baseline) and verify JSON output matches the schemas.
