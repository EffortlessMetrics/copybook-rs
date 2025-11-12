# copybook-rs Roadmap

**Status:** ⚠️ Engineering Preview (v0.3.1 maintenance)

> **Canonical Status Source**: This file is the authoritative reference for copybook-rs project status, adoption guidance, and development timeline. For detailed technical assessment, see [REPORT.md](REPORT.md).

This roadmap tracks **what we will ship**, **how we'll measure it**, and **when it's done**. Each milestone has: Objectives → Deliverables → Exit Criteria → Risks/Mitigations.

---

## Principles (keep these stable)

* **Stability first**: No breaking public behaviors without a minor+ bump; hold API freeze before v1.0.
* **Performance budgeted**: Track throughput against realistic MiB/s floors and document any regressions; historic GiB/s targets are archived until we rediscover them.
* **Single source of truth**: Raw performance receipts live in `PERFORMANCE_VALIDATION_FINAL.md` and `test_perf.json`; public docs emphasise correctness and limitations.
* **Determinism**: Parallel decode remains deterministic; round-trip remains lossless.

---

## Milestone v0.4.0 — Distribution & CI ✅ **SHIPPED** (v0.3.1)

### Objectives

✅ Make installation trivial; make performance visible in PRs; lock structural semantics with fixtures.

### Deliverables

1. **✅ Crates.io publish** (`core`, `codec`, `cli`) — **READY**

   * ✅ crate metadata: categories, keywords, readme path, license files included
   * ✅ workspace dependencies configured for version-based publishing (=0.3.1)
   * ✅ publish dry-run validation in CI
   * ✅ automated publish workflow for tagged releases
   * 🔄 docs.rs builds for all crates (will complete after first publish)
2. **🔄 Bench receipts in CI** (#52, #66) — **80% COMPLETE**

   * ✅ `bench-report` CLI tool implemented (validate, baseline promote/show, compare, summary)
   * ✅ JSON schema validation with serde
   * ✅ Performance floors established (DISPLAY ≥80 MiB/s, COMP-3 ≥40 MiB/s)
   * ⏳ PR comment automation pending (1-1.5 days remaining)
   * ✅ Upload to GitHub Actions artifacts with 90-day retention
   * ✅ DISPLAY/COMP-3 metric extraction from benchmark results
3. **✅ Golden fixtures** (#53) — **COMPLETED**

   * ✅ `level-88 after ODO` (pass) - validates non-storage fields after ODO
   * ✅ `child-inside-ODO` (pass) - validates inner field access
   * ✅ `storage sibling after ODO` (fail) - enforces structural constraints
   * ✅ ODO with nested structure (pass) - validates complex hierarchies
   * ✅ Multiple ODO violation (fail) - enforces tail constraints
4. **✅ Docs nav + link hygiene** — **COMPLETED**

   * ✅ Documentation navigation table in README
   * ✅ `docs/CLI_REFERENCE.md` exists with complete command reference
   * ✅ Performance number consolidation: single source of truth
   * ✅ All duplicate perf numbers replaced with canonical links

5. **✅ RENAMES Foundation** (#122, #128, #129) — **COMPLETED**

   * ✅ Parser: Level-66 RENAMES syntax parsing with THRU/THROUGH keywords
   * ✅ Resolver: Same-scope resolution with comprehensive validations (8 error codes: CBKS601-608)
   * ✅ Test Coverage: 30 tests (28 passing, 2 deferred for nested groups)
   * ✅ Documentation: COBOL Support Matrix, Grammar reference, exploration reports
   * ⏳ Nested group attach pending (Issue #133, 6-8 hours)
   * ⏳ Codec projection pending (Issue #110, 9-12 days)

### Exit Criteria ⚠️ PARTIAL

* ✅ `cargo publish` sequence ready; dry-run passes for all crates
* ✅ Version pinned (0.3.1) with exact internal dependencies
* ✅ Golden fixtures all pass; structural constraints properly validated
* 🔄 Benchmark CI 80% complete; PR automation pending
* ✅ Documentation audit complete: no duplicate perf numbers, working navigation
* ✅ RENAMES parser + same-scope resolver complete

### **Status: ⚠️ Incomplete (follow-ups required)**

**Released**: 2025-09-23 (v0.3.1)
**Includes**: IndexMap deterministic JSON output, golden fixtures validation, docs navigation refresh
**Outstanding**: Benchmark automation deliverables from Issue #52; SLO enforcement; updated public messaging reflecting current throughput

---

## Milestone v0.5.0 — Quality & Feature Completion (Q1 2026)

### Objectives

* Complete RENAMES support; establish quality infrastructure (fuzz, mutation testing); add dialect flexibility for ODO bounds.

### Deliverables

1. **Quality Infrastructure** (Phase 1 Priority)

   * **Determinism Harness** (#112 Phase 1) — 1-2 days
     - BLAKE3 cryptographic verification
     - `copybook determinism-check` CLI command
     - CI gate for enterprise audit compliance

   * **Enhanced Property Testing** (#112 Phase 2) — 2-3 days
     - 200+ property tests for COBOL invariants
     - PIC clause domain properties, ODO bounds, sign handling
     - Nightly CI with 10,000 cases

   * **Fuzz Testing** (#112 Phase 3) — 2-3 days
     - cargo-fuzz infrastructure with 6 targets
     - Corpus seeding from golden fixtures
     - Nightly CI fuzz smoke test

   * **Support Matrix CLI** (#111) — 2.5 days
     - `copybook support --matrix` for feature detection
     - Self-qualification before migration
     - Exit codes for CI integration

2. **RENAMES Completion** (#110, #133)

   * **Nested Group Attach** (#133) — 6-8 hours
     - Level-66 under levels 02-49 (not just 01)
     - Un-ignore 2 existing tests

   * **Codec Projection** (#110) — 9-12 days
     - Read-side decode (composite JSON)
     - Write-side encode (field distribution)
     - Round-trip validation with golden fixtures

3. **Dialect lever** (#51) — 2-3 weeks

   * Config:

     ```toml
     [parser]
     occurs_fixed_with_depends_lower_bound = "n"   # allowed: "n" | "0" | "1"; default "n"
     ```
   * CLI flag and env var mapping (`--dialect-odo-lower-bound`, `COPYBOOK_DIALECT_ODO_LOWER`)
   * Golden fixtures for each setting; docs examples and migration notes

4. **Infrastructure Improvements**

   * **Benchmark Container** (#113) — 1.5-2.5 days
     - Containerized bench-report Docker image
     - Operator runbook documentation
     - CI integration for reproducible benchmarks

   * **Cross-platform Paths** (#88) — 4-6 hours
     - Fix 5 remaining hardcoded Unix paths
     - Portable `PathBuf` patterns

### Exit Criteria

* ✅ Determinism harness operational with CI gate
* ✅ Fuzz testing discovering crashes (0 crashes target)
* ✅ Property tests validating COBOL invariants (200+ properties)
* ✅ RENAMES fully supported (parse + resolve + codec)
* ✅ Support Matrix CLI enables self-qualification
* ✅ Benchmark container with operator runbook
* ✅ Cross-platform test compatibility
* ✅ Dialect lever with comprehensive fixtures
* ✅ No regressions vs v0.3.1 budgets; CI receipts show deltas ≤ 5%
* ✅ Default behavior unchanged (back-compat preserved)

### Recommended Implementation Order

**Week 1-2: Quick Wins**
- Issue #133 (RENAMES nested groups) — 6-8 hours
- Issue #88 (hardcoded paths) — 4-6 hours
- Issue #66 (benchmark PR comments) — 1-1.5 days
- Issue #111 (Support Matrix CLI) — 2.5 days

**Week 3-4: Quality Infrastructure**
- Issue #112 Phase 1 (Determinism) — 1-2 days
- Issue #112 Phase 2 (Property tests) — 2-3 days
- Issue #113 (Benchmark container) — 1.5-2.5 days

**Week 5-8: Feature Completion**
- Issue #112 Phase 3 (Fuzz testing) — 2-3 days
- Issue #110 (RENAMES codec) — 9-12 days

**Week 9-12: Dialect Flexibility**
- Issue #51 (ODO lower bound) — 2-3 weeks

### Risks & Mitigations

* **Determinism gate false positives** → Document platform-specific cases, manual overrides
* **Fuzz corpus growth** → Time-boxed runs, automated minimization
* **RENAMES codec performance** → <5% overhead target, scratch buffer optimization
* **Behavior drift for existing users** → Default remains current `"n"`; strong docs + examples

---

## Toward v1.0.0 — Stability & Ecosystem (Q2 2026)

### Objectives

* Lock public behaviors, add first-class connectors.

### Deliverables

* **API freeze window** (4 weeks): only doc/bench/test changes
* **Ecosystem adapters** (best-effort): Arrow/Parquet writer crate prototype, Kafka example pipeline
* **Support policy**: 6-month minor support window; security patches anytime

### Exit Criteria

* Changelog & README carry a "**Stability Guarantees**" section
* Example integrations build in CI; round-trip and perf budgets still satisfied

---

## Telemetry Rollout (Phase 6)

**Status:** Done — staging exporter live; dashboards + alerts active.

**Why:** make runtime behavior observable with a rollback lever.

**Plan**
- [x] Feature-gate metrics; exporter behind `--metrics-listen`
- [x] Emit low-cardinality series in decode path
- [x] README + Library API docs
- [x] Staging rollout (enable flag; Prom scrape)
- [x] Dashboards (QPS, bytes/s, error rate, MiB/s)
- [x] Alerts (error burst, throughput drop)
- [x] Perf sanity on staging (MiB/s, RSS in-band)

**Exit:** `/metrics` reachable; Prom scraping; dashboards green; SLO receipts steady; builds clean with/without metrics.

---

## Project board & labels

* **Board columns**: Backlog → In Progress → In Review → Bench Verified → Done
* **Labels**: `area:parser`, `area:bench`, `area:docs`, `type:feature`, `type:bug`, `type:tests`, `perf:budget-regression`
* **Rules**:

  * Anything touching hot paths must pass **Bench Verified** before **Done**
  * Any PR adding perf numbers must **not** repeat them—link to canonical section

---

## Release train

* **Minor**: every 6–8 weeks (feature batches)
* **Patch**: as-needed (bug or doc only)
* **Pre-release checklist**: `cargo build -r` • `nextest` • `bench receipts` • link check • changelog • tag • GitHub release

---

## Owner matrix (example; edit as you like)

| Area              | DRI        | Backup     |
| ----------------- | ---------- | ---------- |
| Parser & dialects | @you       | @contrib-A |
| Codec & perf      | @you       | @contrib-B |
| CLI & docs        | @contrib-C | @you       |
| CI/bench receipts | @contrib-D | @contrib-C |

---

## Paste-ready CI snippets

**Bench receipts (GitHub Actions)**

```yaml
- name: Run benches
  run: PERF=1 cargo bench -p copybook-bench -- --output-format bencher | tee bench.out

- name: Roll up perf JSON
  run: |
    python3 - << 'PY'
    import json,glob
    from pathlib import Path
    reports=list(glob.glob('target/criterion/**/new/benchmark.json',recursive=True))
    out={"display_mib_per_s":None,"comp3_mib_per_s":None}
    # TODO: parse actual metric keys; set SLOs
    Path('perf.json').write_text(json.dumps(out))
    PY

- name: Upload artifacts
  uses: actions/upload-artifact@v4
  with:
    name: bench-artifacts
    path: |
      perf.json
      target/criterion/**
    retention-days: 14

- name: Comment perf summary
  if: ${{ github.event_name == 'pull_request' }}
  run: gh pr comment ${{ github.event.pull_request.number }} --body "$(cat perf.json)"
```

**SLO gate (simple)**

```yaml
- name: Enforce SLOs
  run: |
    python3 - << 'PY'
    import json,sys
    with open('perf.json') as fh:
        data=json.load(fh)
    if data.get("display_mib_per_s",0) < 80:
        sys.exit("DISPLAY throughput below 80 MiB/s floor")
    if data.get("comp3_mib_per_s",0) < 40:
        sys.exit("COMP-3 throughput below 40 MiB/s floor")
    PY
```

---

## Definition of Done (all milestones)

* ✅ Tests green (unit/integration/goldens)
* ✅ Bench receipts uploaded; budgets respected
* ✅ Docs updated (no duplicated performance figures)
* ✅ Changelog entry & tag created
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
