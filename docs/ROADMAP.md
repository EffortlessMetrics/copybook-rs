# copybook-rs Roadmap

**Status:** ✅ Production Ready (v0.3.0)

This roadmap tracks **what we will ship**, **how we'll measure it**, and **when it's done**. Each milestone has: Objectives → Deliverables → Exit Criteria → Risks/Mitigations.

---

## Principles (keep these stable)

* **Stability first**: No breaking public behaviors without a minor+ bump; hold API freeze before v1.0.
* **Performance budgeted**: Any change must keep ≥ **DISPLAY 4.1–4.2 GiB/s** and **COMP-3 560–580 MiB/s**, memory < **256 MiB** steady-state.
* **Single source of truth**: Performance figures live in **README "Performance Specifications"**; other docs reference it.
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
2. **✅ Bench receipts in CI** (#52) — **COMPLETED**

   * ✅ criterion JSON parsing with real throughput calculation
   * ✅ PR comment: SLO deltas and pass/fail against budgets (±5% warn, >10% fail)
   * ✅ Upload to GitHub Actions artifacts with 14-day retention
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

### Exit Criteria ✅ ALL MET

* ✅ `cargo publish` sequence ready; dry-run passes for all crates
* ✅ Version pinned (0.3.1) with exact internal dependencies
* ✅ Golden fixtures all pass; structural constraints properly validated
* ✅ Benchmark CI parses real throughput data and enforces SLOs
* ✅ Documentation audit complete: no duplicate perf numbers, working navigation

### **Status: ✅ SHIPPED (v0.3.1)**

**Released**: 2025-09-23
**Includes**: IndexMap deterministic JSON output, complete CI benchmarking, golden fixtures validation

Ready for crates.io publishing with `cargo login` + publish sequence.

---

## Milestone v0.5.0 — Dialects & Optimizations (Q1 2026)

### Objectives

* Add a safe, explicit knob for ODO bounds across COBOL dialects; squeeze throughput without changing outputs.

### Deliverables

1. **Dialect lever** (#51)

   * Config:

     ```toml
     [parser]
     occurs_fixed_with_depends_lower_bound = "n"   # allowed: "n" | "0" | "1"; default "n"
     ```
   * CLI flag and env var mapping (`--dialect-odo-lower-bound`, `COPYBOOK_DIALECT_ODO_LOWER`)
   * Golden fixtures for each setting; docs examples and migration notes
2. **Perf tune (SIMD/I/O)**

   * Target +10–20% p95 throughput maintained under budgets; no API/behavior changes

### Exit Criteria

* Same inputs under each dialect mode produce **documented** and **tested** outcomes
* No regressions vs v0.3.0 budgets; CI receipts show deltas ≤ 5% except where improved
* Default behavior unchanged (back-compat preserved)

### Risks & Mitigations

* **Behavior drift for existing users** → default remains current `"n"`; strong docs + examples

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
    python - << 'PY'
    import json,glob,os,sys
    from pathlib import Path
    reports=list(glob.glob('target/criterion/**/new/benchmark.json',recursive=True))
    out={"display_gibs":None,"comp3_mibs":None}
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
    python - << 'PY'
    import json,sys
    d=json.load(open('perf.json'))
    assert d["display_gibs"] >= 4.1, "DISPLAY throughput below 4.1 GiB/s"
    assert d["comp3_mibs"] >= 560, "COMP-3 throughput below 560 MiB/s"
    PY
```

---

## Definition of Done (all milestones)

* ✅ Tests green (unit/integration/goldens)
* ✅ Bench receipts uploaded; budgets respected
* ✅ Docs updated (no duplicated performance figures)
* ✅ Changelog entry & tag created