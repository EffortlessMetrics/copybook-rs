# Remaining Gaps Before Production Launch

## 1. Messaging Still Promises Performance We Are Deprioritizing
- The README and Production Status Report lead with "production ready" claims, 3–52× performance figures, and multi-GiB/s throughput tables that we no longer plan to market; the zero-sell stance means these promises should be removed rather than featured.【F:README.md†L3-L38】【F:docs/REPORT.md†L3-L48】【F:docs/REPORT.md†L138-L158】
- Internal artifacts show present throughput in the tens of MiB/s range, which is fine for engineering telemetry but reinforces that we must reframe the public narrative around correctness and safety instead of raw speed.【F:test_perf.json†L1-L45】【F:integrative_gate_summary.md†L22-L53】
- Recommended action: scrub external-facing docs of throughput marketing, keep benchmark numbers confined to internal validation notes, and restate value in terms of reliability.

## 2. Test Story and Messaging Are Still Out of Sync
- Marketing copy highlights "377+" or "127" perfect tests, yet the integrative gate summary documents one persistent failure and multiple leak detectors still open; we need to publish the truth instead of aspirational totals.【F:README.md†L13-L38】【F:docs/REPORT.md†L3-L48】【F:integrative_gate_summary.md†L12-L157】
- Recommended action: align README/report numbers with automated summaries and continue chasing the flaky/leaky entries until the dashboards are truly green.

## 3. Benchmark & Regression Tooling Remains Unimplemented and Untracked
- Issue #52’s ledger still calls for Python benchmark utilities (`bench_runner.py`, `baseline_manager.py`, `slo_validator.py`) that never shipped, yet nothing in `scripts/bench/` provides them today.【F:issue-52-ledger.md†L1-L52】
- We also lack a canonical tracker tying those gaps to owners and timelines, so the same items keep resurfacing without closure.【F:issue-52-ledger.md†L32-L43】
- Recommended action: stand up a real backlog entry (issue/project) that enumerates each missing tool, owners, and acceptance tests so we can verify delivery instead of rediscovering the gap later.

## 4. COBOL Coverage Gaps Still Block Real Workloads
- README copy still says "complete COBOL support" even while noting missing COMP-1/COMP-2 floats, edited PIC clauses, separate sign directives, nested ODO arrays, and RENAMES; Production Status doubles down by conceding 88-level support is absent.【F:README.md†L26-L38】【F:README.md†L732-L738】【F:docs/REPORT.md†L84-L90】
- Recommended action: either prioritize implementing these constructs or clearly warn adopters up front so they can qualify copybooks before committing.

## 5. Documentation Hygiene After the Rust Cleanup Remains Outstanding
- Several first-party narratives still describe the pre-cleanup toolchain and performance posture (e.g., MSRV references, throughput claims), so the docs conflict with the now-cleaner Rust baseline we ship.【F:docs/REPORT.md†L3-L108】【F:PERFORMANCE_VALIDATION_FINAL.md†L1-L18】
- We also continue to reference promotion/marketing collateral even though the goal is a zero-sell posture; until we revise or retire these documents, they will mislead readers.【F:README.md†L3-L38】【F:docs/REPORT.md†L3-L48】
- Recommended action: run a documentation audit keyed off the Rust cleanup to eliminate obsolete MSRV/performance text, strip marketing language, and ensure every surviving claim links to a maintained verification source.

### Recommended Next Steps
1. Update public docs to emphasize correctness, safety, and maintenance posture—no more throughput charts or "production ready" claims until we intentionally reintroduce them.
2. Synchronize reported test counts with CI output and fix the remaining failure/leaks so that honesty and quality improve together.
3. Create and own an implementation plan for the missing benchmark utilities, with check-ins that make future drift visible.
4. Decide whether to implement or explicitly call out unsupported COBOL constructs so prospects can self-select.
5. Complete the post-cleanup documentation sweep so every artifact reflects the current Rust codebase and zero-sell messaging.
