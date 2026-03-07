<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Archived Roadmap: v0.4.0 and v0.5.0 Milestones

> **Archived**: This document preserves historical milestone content from the main ROADMAP.
> For current project status, see [ROADMAP.md](../ROADMAP.md).

---

## Testing Methodologies (Completed)

The copybook-rs project has implemented a comprehensive testing strategy with five complementary testing methodologies:

### 1. Behavior Driven Development (BDD)

**Status**: Complete

**Framework**: Cucumber (Gherkin syntax)

**Implementation**:
- 3 feature files: `copybook_parsing.feature`, `encode_decode.feature`, `error_handling.feature`
- Executable specifications in human-readable format
- Integration with CI workflow: `.github/workflows/ci.yml` (bdd-tests job)
- Documentation: [`docs/BDD_TESTING.md`](docs/BDD_TESTING.md)

**Coverage Areas**:
- Copybook parsing (simple fields, numeric fields, OCCURS, REDEFINES, Level-88)
- Encode/decode operations (ASCII/EBCDIC, binary to JSON, round-trip fidelity)
- Error handling (syntax errors, invalid clauses, data validation)

**Running BDD Tests**:
```bash
cargo test -p copybook-bdd --test bdd_smoke -- --nocapture
bash scripts/ci/governance-bdd-smoke.sh
```

### 2. Feature Flagging

**Status**: Complete (22 flags across 5 categories)

**Framework**: Custom feature flag contracts in [`copybook-contracts/src/feature_flags.rs`](../../crates/copybook-contracts/src/feature_flags.rs) with governance runtime layering

**Testing-Related Feature Flags**:
- `mutation_testing` - Enable mutation testing hooks
- `fuzzing_integration` - Enable fuzzing integration points
- `coverage_instrumentation` - Enable test coverage instrumentation
- `property_based_testing` - Enable property-based testing integration

**Integration**:
- CI workflow: `.github/workflows/feature-flags.yml`
- Tests all 22 feature flags in both enabled and disabled states
- CLI integration: `--list-features`, `--enable-features`, `--disable-features`

**Categories**:
- Experimental (4 flags): SignSeparate, RenamesR4R6, Comp1, Comp2 (SignSeparate, Comp1, Comp2 promoted to stable/default-enabled in v0.4.3)
- Enterprise (6 flags): AuditSystem, SoxCompliance, HipaaCompliance, GdprCompliance, PciDssCompliance, SecurityMonitoring
- Performance (4 flags): AdvancedOptimization, LruCache, ParallelDecode, ZeroCopy
- Debug (4 flags): VerboseLogging, DiagnosticOutput, Profiling, MemoryTracking
- Testing (4 flags): MutationTesting, FuzzingIntegration, CoverageInstrumentation, PropertyBasedTesting

### 3. Fuzzing

**Status**: Complete (6 fuzz targets)

**Framework**: cargo-fuzz with libfuzzer-sys

**Fuzz Targets**:
- `copybook_parse` - COBOL copybook parser
- `binary_decode` - Binary data decoder
- `json_encode` - JSON encoder
- `pic_clause` - PICTURE clause parser
- `occurs_odo` - OCCURS DEPENDING ON handler
- `redefines` - REDEFINES clause handler

**Integration**:
- CI workflow: `.github/workflows/fuzz-integration.yml`
- Scheduled runs: Nightly at 2 AM UTC
- Manual trigger support with configurable duration
- Corpus management with minimization

**Running Fuzzing**:
```bash
# Run all fuzzers for 5 minutes
cargo fuzz run copybook_parse -- -runs=0 -max_total_time=300

# Manual CI trigger
gh workflow run fuzz-integration.yml
```

### 4. Mutation Testing

**Status**: Complete (per-crate thresholds)

**Framework**: cargo-mutants with nextest

**Configuration**: [`mutants.toml`](../../mutants.toml)

**Per-Crate Thresholds**:
- `copybook-core`: 75% (critical parser code)
- `copybook-codec`: 75% (encoding/decoding correctness)
- `copybook-cli`: 65% (CLI with more boilerplate)
- `copybook-bench`: 60% (benchmark utilities)
- `copybook-gen`: 60% (test infrastructure)

**Integration**:
- CI workflow: `.github/workflows/ci-mutants.yml`
- Scheduled runs: Weekly on Sundays at 2 AM UTC
- Manual trigger with threshold configuration
- PR support with `mutation-test` label
- JSON output for metrics aggregation

**Running Mutation Testing**:
```bash
# Run on entire workspace
cargo mutants --workspace --test-tool nextest

# Run on specific crate
cargo mutants --package copybook-core
```

### 5. Property Testing

**Status**: Complete (50+ property tests)

**Framework**: proptest

**Implementation**: [`tests/proptest/`](../../tests/proptest/)

**Test Modules**:
- `roundtrip.rs` - Round-trip fidelity properties
- `parsing.rs` - Parser correctness properties
- `numeric.rs` - Numeric conversion properties
- `arrays.rs` - Array handling properties
- `pic_clauses.rs` - PICTURE clause properties
- `redefines.rs` - REDEFINES properties

**Integration**:
- CI workflow: `.github/workflows/ci-proptest.yml`
- Scheduled runs: Weekly on Mondays at 3 AM UTC
- Matrix: Ubuntu/macOS/Windows x stable/beta
- Configurable test cases (256 for PRs, 1024 for scheduled)

**Running Property Tests**:
```bash
# Run all property tests
cargo test -p copybook-proptest --lib

# Run with custom configuration
PROPTEST_CASES=512 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib
```

### Testing Integration Summary

See [`docs/TESTING_COMMANDS.md`](../TESTING_COMMANDS.md) for:
- Exact commands to run each test suite
- Runtime class (PR-gate vs scheduled)
- Expected runtime for each test suite
- Artifact locations for test outputs
- Quick reference table for all test suites
- Local development workflow guide

---

## Milestone v0.4.0 -- Distribution & CI

### Objectives

Make installation trivial; make performance visible in PRs; lock structural
semantics with fixtures.

### Deliverables

1. **Crates.io publish** (`core`, `codec`, `cli`) -- **Released**

   * Crate metadata: categories, keywords, readme path, license files included
   * Workspace dependencies configured for version-based publishing (=0.4.0)
   * Publish dry-run validation in CI
   * Automated publish workflow for tagged releases (core, codec, cli)
   * docs.rs builds for all crates (will complete after first publish)

2. **Bench receipts tooling** (#52) -- **Completed**

   * `bench-report` CLI tool with baseline management (validate, promote, compare, summary)
   * Criterion JSON parsing with real throughput calculation
   * PR comment automation: pr-bench-comment.yml (Issue #195, commit 761f16c)
   * Upload receipts to GitHub Actions artifacts (`perf-json`, 90-day retention)
   * DISPLAY/COMP-3 metric extraction from benchmark results

3. **Golden fixtures** (#53) -- **Completed**

   * `level-88 after ODO` (pass) - validates non-storage fields after ODO
   * `child-inside-ODO` (pass) - validates inner field access
   * `storage sibling after ODO` (fail) - enforces structural constraints
   * ODO with nested structure (pass) - validates complex hierarchies
   * Multiple ODO violation (fail) - enforces tail constraints

4. **Docs nav + link hygiene** -- **Completed**

   * Documentation navigation table in README
   * `docs/CLI_REFERENCE.md` exists with complete command reference
   * Performance number consolidation: single source of truth
   * All duplicate perf numbers replaced with canonical links

### Status: Released

**Released**: 2025-12-18 (v0.4.0)

**Includes**: Projection (`--select`) support, edited PIC decode (E1/E2),
deterministic JSON output, golden fixtures validation, docs navigation
refresh, bench-report CLI

**Follow-up**: v0.4.1 released with additional fixes

---

## Milestone v0.5.0 -- Edited PIC Encoding & Dialects (Q1 2026) -- Complete

### Objectives

* Implement Edited PIC Encoding (Phase E3) -- biggest functional gap
* Add a safe, explicit knob for ODO bounds across COBOL dialects; squeeze
  throughput without changing outputs.
* Complete Determinism CI Wiring (Phase 3) -- Phases 1-2 already shipped

### Deliverables

1. **Edited PIC E3 encode** -- **Complete**

   * **E3.0 -- Contract + Test Matrix PR** (docs-only) -- Completed
     * Define supported patterns table (sign placement, Z/0, decimal point,
       commas, currency, CR/DB)
     * Define error handling strategy (what returns "unsupported edited PIC
       encode" error)
   * **E3.1 -- Minimal Encode Path** (sign + basic Z/0) -- Completed (commit 976ca0f)
     * Scope: digit placement, decimal point, sign placement
     * Tests: 920+ lines of golden fixtures for representative formats
     * Implemented: `encode_edited_numeric()` in copybook-codec
   * **E3.2 -- Sign Editing (+/-)** (leading/trailing) -- Completed
     * Scope: trailing plus/minus sign encoding
     * Tests: 24 test cases for sign patterns
   * **E3.3 -- CR/DB** (credit/debit indicators) -- Completed
     * Scope: CR/DB sign indicators (2-char trailing)
     * Tests: 20 test cases for credit/debit patterns
   * **E3.4 -- Commas & Separators** (`,`, `/`) -- Completed
     * Scope: comma placement, slash for dates
     * Tests: 26 test cases for separator patterns
   * **E3.5 -- Asterisk Fill (`*`)** (check protection) -- Completed
     * Scope: asterisk fill for leading zeros
     * Tests: 28 test cases for check protection
   * **E3.6 -- Currency Symbols** (`$`, fixed position) -- Completed
     * Scope: fixed-position currency symbol
     * Tests: 17 test cases for currency patterns
   * **E3.7 -- Space (`B`) Insertion** -- Completed
     * Scope: space insertion at specified positions
     * Tests: 7 test cases for space insertion patterns

2. **Dialect lever** (#51) -- **Complete**

   * **D.0 -- Config Schema + Contract** -- Completed (commit a9609af)
     * Define config key and allowed values (`Dialect::Strict`, `Dialect::Tolerant`)
     * Core `copybook_core::dialect` module with `Dialect` enum
     * `effective_min_count()` function for ODO lower bound computation
     * 581 lines of D1 core tests (`dialect_d1_tests.rs`)
     * 287 lines of D2 CLI tests (`dialect_cli_d2_tests.rs`)
     * 61 lines of D3 golden fixtures tests (`dialect_fixtures_d3_tests.rs`)
     * Normative fixtures: `dialect_normative.bin`, `dialect_zero_tolerant.bin`, `dialect_one_tolerant.bin`
   * **D.1 -- Core Implementation** (core-only PR) -- Completed (included in D.0)
     * ODO lower bound validation in layout.rs
     * Default behavior preserved (strict mode = `"n"` interpretation)
   * **D.2 -- CLI Integration** -- Completed (included in D.0)
     * `--dialect n|0|1` flags on all commands (parse, inspect, decode, encode, verify)
     * `COPYBOOK_DIALECT` env var support with proper precedence
     * 287 lines of CLI tests in `dialect_cli_d2_tests.rs`
   * **D.3 -- Golden Fixtures** -- Completed (included in D.0)
     * Same copybook validated under each dialect setting
     * Documented behavioral differences
   * **D.4 -- Docs & Examples** (docs-only PR)
     * Migration notes for existing users
     * Example copybooks showing dialect differences

3. **Determinism Validation** (#112) -- **Phases 1-3 Shipped**

   * Phase 1 -- Codec harness (PR #158)
     * `copybook_codec::determinism` module
     * BLAKE3 hashes + bounded byte diffs
     * Adversarial tests for error paths and edge cases
   * Phase 2 -- CLI wiring (PR #160)
     * `copybook determinism decode|encode|round-trip`
     * Human + JSON output modes
     * Integration tests in `copybook-cli/tests/determinism_cli.rs`
   * Phase 3 -- CI smoke test template
     * `.github/workflows/determinism-smoke.yml` workflow template
     * `scripts/ci/determinism_smoke.sh` executable script (runs locally)
     * Tests both DISPLAY-heavy (`simple.cpy`) and COMP-3 (`comp3_test.cpy`) fixtures
     * Emits machine-readable JSON receipts for CI parsing
     * Advisory-only when activated (non-blocking), promotable to blocking gate

4. **Perf tune (SIMD/I/O)**

   * Target +10-20% p95 throughput maintained under budgets; no API/behavior
     changes

### Exit Criteria

* **Edited PIC E3 encode**: All E3.0-E3.6 phases complete; golden fixtures
  pass for sign, CR/DB, commas, asterisk fill, and currency symbols
* **Dialect lever**: Same inputs under each dialect mode produce
  **documented** and **tested** outcomes; default behavior unchanged
* **Determinism CI**: Phase 3 smoke test active
* No regressions vs v0.3.0 budgets; CI receipts show deltas <= 5% except where
  improved
* Default behavior unchanged (back-compat preserved)

### Risks & Mitigations

* **Edited PIC encode complexity** -- incremental phases (E3.0-E3.6) with
  golden fixtures; clear error handling for unsupported patterns
* **Behavior drift for existing users** -- default remains current `"n"` for
  dialect lever; strong docs + examples

---

## Telemetry Rollout (Phase 6)

**Status:** Done -- staging exporter live; dashboards + alerts active.

**Why:** make runtime behavior observable with a rollback lever.

### Plan

* Feature-gate metrics; exporter behind `--metrics-listen`
* Emit low-cardinality series in decode path
* README + Library API docs
* Staging rollout (enable flag; Prom scrape)
* Dashboards (QPS, bytes/s, error rate, MiB/s)
* Alerts (error burst, throughput drop)
* Perf sanity on staging (MiB/s, RSS in-band)

**Exit:** `/metrics` reachable; Prom scraping; dashboards green; SLO receipts
steady; builds clean with/without metrics.

---

## CI Snippets

### Bench receipts (GitHub Actions)

```yaml
- name: Run benches
  run: |
    PERF=1 cargo bench -p copybook-bench \
      -- --output-format bencher | tee bench.out

- name: Roll up perf JSON
  run: |
    python3 - << 'PY'
    import json,glob
    from pathlib import Path
    reports=list(
      glob.glob('target/criterion/**/new/benchmark.json',
      recursive=True))
    out={"display_mib_per_s":None,"comp3_mib_per_s":None}
    # Parse actual metric keys and set SLOs (implementation detail)
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
  run: |
    gh pr comment ${{ github.event.pull_request.number }} \
      --body "$(cat perf.json)"
```

### SLO gate (simple)

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

## Technical Debt (Completed Items)

### Documentation (Completed)

* All 47 public functions documented (was 22 undocumented)
* Memory module API fully documented

### Test Coverage (Completed)

* Dedicated tests for error codes CBKS701, CBKS702, CBKD101, CBKI001 (added 2025-12-31)
* CBKS703, CBKF102, CBKF104 tests verified (2025-12-31)
* CBKE510/515 fixed -- corrected error code usage (was using CBKE501):
  - CBKE515 now emitted for string length violations (3 locations)
  - CBKE510 now emitted for numeric overflow (3 locations)
  -- Fixed 2025-12-31 with 3 new tests in `error_code_tests.rs`

### Code Quality (Completed in v0.4.0+)

* Consolidated CLI command duplication (`parse_selectors`, `ParseOptions`, field projection)
* Fixed deprecated `cargo_bin` function in xtask tests
* Standardized workspace dependency inheritance (`sha2`, `chrono` in `copybook-gen`)
* Production panics at 0 on main (test-only panics remain acceptable) -- PR #182
* E3.1 Edited PIC Encoding for numeric fields -- commit 976ca0f
* E3.2-E3.6 Edited PIC Encoding -- Full encode support with 115 new tests (Dec 2025)
* D0 Dialect Lever Contract with comprehensive tests -- commit a9609af
* N1 Nested ODO Design with O5/O6 rejection -- PR #172
* RENAMES Codec (#110) -- R1-R3 decode/encode scenarios complete (Dec 2025)
* Determinism Phases 1-2 -- Codec harness + CLI validation complete (Dec 2025)

### CI Mode (at time of archival)

* **Full CI active**: CI Quick (PR gate) + CI Full (matrix on main + schedule)
* **12+ workflows**: ci-quick, ci, determinism-smoke, perf-bench, feature-flags, api-freeze, coverage, security, mutants, fuzz, SBOM, dependabot
* **Dispatch-only workflows**: Mutants (weekly + manual), fuzz (manual), SBOM (manual)

### Distribution

* **crates.io** = Public artifact distribution (crate tarball becomes public)
* **Internal/Private distribution**: Git tags on private repositories, private cargo registries
* **Note**: `cargo publish --dry-run` for workspace crates may fail for codec/cli prior to publishing core/codec (verification builds in isolation). Use `cargo package --no-verify` to inspect tarball contents.

---

## Project Board & Labels (Reference)

* **Board columns**: Backlog, In Progress, In Review, Bench Verified, Done
* **Labels**: `area:parser`, `area:bench`, `area:docs`, `type:feature`, `type:bug`, `type:tests`, `perf:budget-regression`
* **Rules**:
  * Anything touching hot paths must pass **Bench Verified** before **Done**
  * Any PR adding perf numbers must **not** repeat them -- link to canonical section

## Release Train (Reference)

* **Minor**: every 6-8 weeks (feature batches)
* **Patch**: as-needed (bug or doc only)
* **Pre-release checklist**: `cargo build -r`, `nextest`, `bench receipts`, link check, changelog, tag, GitHub release

## Owner Matrix (Reference)

| Area              | DRI             | Backup     |
| ----------------- | --------------- | ---------- |
| Parser & dialects | @EffortlessSteven | @contrib-A |
| Codec & perf      | @EffortlessSteven | @contrib-B |
| CLI & docs        | @EffortlessSteven | @contrib-D |
| CI/bench receipts | @EffortlessSteven | @contrib-C |

## Definition of Done (All Milestones)

* Tests green (unit/integration/golden fixtures)
* Bench receipts uploaded; budgets respected
* Docs updated (no duplicated performance figures)
* Changelog entry & tag created
