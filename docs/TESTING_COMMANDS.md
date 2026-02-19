<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Testing Commands Reference

**Last Updated**: 2026-02-07

This document provides a canonical reference for all testing commands in copybook-rs, including exact syntax, runtime class (PR-gate vs scheduled), and artifact locations. Use this as a quick reference for running tests locally and understanding CI behavior.

---

## Table of Contents

- [Section 1: Quick Reference Table](#section-1-quick-reference-table)
- [Section 2: PR-Gated Tests (Deterministic)](#section-2-pr-gated-tests-deterministic)
- [Section 3: Scheduled Tests (Stochastic/Expensive)](#section-3-scheduled-tests-stochasticexpensive)
- [Section 4: Artifact Locations](#section-4-artifact-locations)
- [Section 5: Local Development Workflow](#section-5-local-development-workflow)

---

## Section 1: Quick Reference Table

| Test Suite | Exact Command | Runtime Class | Expected Runtime | Artifact Locations |
|------------|---------------|----------------|------------------|-------------------|
| **Format Check** | `cargo fmt --all --check` | PR-gate | < 1 min | None |
| **Clippy (Libs/Bins/Examples)** | `cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic` | PR-gate | 2-3 min | None |
| **Clippy (Tests)** | `cargo clippy --workspace --tests --all-features -- -D warnings -A clippy::unwrap_used -A clippy::expect_used -A clippy::panic -A clippy::dbg_macro -A clippy::print_stdout -A clippy::print_stderr -A clippy::duplicated_attributes` | PR-gate | 1-2 min | None |
| **Clippy (Panic Prevention)** | `cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -D clippy::unwrap_used -D clippy::expect_used -D clippy::panic -D clippy::unreachable -D clippy::todo -D clippy::unimplemented` | PR-gate | 2-3 min | None |
| **Unit Tests (nextest)** | `cargo nextest run --workspace --exclude copybook-bench --profile ci --failure-output=immediate --status-level=fail` | PR-gate | 3-5 min | None |
| **Doctests** | `RUSTDOCFLAGS="--deny warnings" cargo test --doc --workspace --exclude copybook-bench` | PR-gate | 1-2 min | None |
| **Proptest Smoke** | `PROPTEST_CASES=256 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture` | PR-gate | 2-3 min | `copybook-core/tests/proptest-regressions/`, `copybook-codec/tests/proptest-regressions/` |
| **Determinism Smoke** | `scripts/ci/determinism_smoke.sh` | PR-gate | 2-3 min | `target/determinism-simple.json`, `target/determinism-comp3.json` |
| **Security (deny)** | `cargo deny check` | PR-gate | < 1 min | None |
| **Security (audit)** | `cargo audit` | PR-gate (if Cargo.lock changed) | 1-2 min | `target/security.audit.json` |
| **BDD Tests** | `cargo test -p copybook-bdd -- --nocapture` | Scheduled | 2-3 min | None |
| **Feature Flags** | Matrix of 22 flags × enabled/disabled states | PR-gate | 5-10 min | None |
| **Full Proptest** | `PROPTEST_CASES=1024 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture` | Scheduled | 5-10 min | `copybook-core/tests/proptest-regressions/`, `copybook-codec/tests/proptest-regressions/`, `tests/proptest-regressions/` |
| **Fuzzing** | `cargo fuzz run <target> -- -runs=0 -max_total_time=300` | Scheduled | 5-10 min per target | `fuzz/artifacts/<target>/`, `fuzz/corpus/<target>/` |
| **Mutation Testing** | `cargo mutants --package <crate> --timeout <timeout> --test-tool nextest --in-place --json --file mutants.toml` | Scheduled | 15-60 min per crate | `mutants.out/outcomes.json`, `mutants-summary.csv` |
| **Performance Benchmarks** | `BENCH_FILTER=slo_validation bash scripts/bench.sh` | Scheduled | 10-15 min | `target/perf.json` |
| **Soak Tests** | `cargo test -p copybook-cli --features soak -- --ignored --test-threads=1 --nocapture` | Scheduled | 10-20 min | None |

---

## Section 2: PR-Gated Tests (Deterministic)

These tests run on every PR and block merge. They are deterministic and fast.

### Format Check

**Purpose**: Ensures code follows Rust formatting standards.

**Exact Command**:
```bash
cargo fmt --all --check
```

**What it validates**:
- All Rust source files are formatted according to `rustfmt` standards
- No formatting changes are needed

**How to run locally**:
```bash
# Check formatting (read-only)
cargo fmt --all --check

# Apply formatting (write changes)
cargo fmt --all
```

**Expected runtime**: < 1 minute

---

### Clippy (Libs/Bins/Examples - Pedantic)

**Purpose**: Enforces code quality and catches common errors in production code.

**Exact Command**:
```bash
cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic
```

**What it validates**:
- All library, binary, and example code passes clippy lints
- Pedantic lints are enabled for stricter checking
- All warnings are treated as errors

**How to run locally**:
```bash
cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic
```

**Expected runtime**: 2-3 minutes

---

### Clippy (Tests - Relaxed)

**Purpose**: Enforces code quality in test code with relaxed lints for test-specific patterns.

**Exact Command**:
```bash
cargo clippy --workspace --tests --all-features \
  -- -D warnings \
  -A clippy::unwrap_used \
  -A clippy::expect_used \
  -A clippy::panic \
  -A clippy::dbg_macro \
  -A clippy::print_stdout \
  -A clippy::print_stderr \
  -A clippy::duplicated_attributes
```

**What it validates**:
- All test code passes clippy lints
- Common test patterns (unwrap, expect, panic, dbg!, print!) are allowed
- All warnings are still treated as errors

**How to run locally**:
```bash
cargo clippy --workspace --tests --all-features \
  -- -D warnings \
  -A clippy::unwrap_used \
  -A clippy::expect_used \
  -A clippy::panic \
  -A clippy::dbg_macro \
  -A clippy::print_stdout \
  -A clippy::print_stderr \
  -A clippy::duplicated_attributes
```

**Expected runtime**: 1-2 minutes

---

### Clippy (Panic Prevention)

**Purpose**: Enforces panic-free code in production targets.

**Exact Command**:
```bash
cargo clippy --workspace --lib --bins --examples --all-features \
  -- -D warnings \
  -D clippy::unwrap_used \
  -D clippy::expect_used \
  -D clippy::panic \
  -D clippy::unreachable \
  -D clippy::todo \
  -D clippy::unimplemented
```

**What it validates**:
- Production code (libs, bins, examples) has no panics
- No `unwrap()`, `expect()`, `panic!()`, `unreachable!()`, `todo!()`, or `unimplemented!()`

**How to run locally**:
```bash
cargo clippy --workspace --lib --bins --examples --all-features \
  -- -D warnings \
  -D clippy::unwrap_used \
  -D clippy::expect_used \
  -D clippy::panic \
  -D clippy::unreachable \
  -D clippy::todo \
  -D clippy::unimplemented
```

**Expected runtime**: 2-3 minutes

---

### Unit Tests (nextest)

**Purpose**: Runs all unit and integration tests using nextest for faster execution.

**Exact Command**:
```bash
cargo nextest run --workspace --exclude copybook-bench --profile ci --failure-output=immediate --status-level=fail
```

**What it validates**:
- All unit tests pass
- All integration tests pass
- Benchmark crate is excluded (not part of PR gate)

**How to run locally**:
```bash
# Run all tests with nextest
cargo nextest run --workspace --exclude copybook-bench --profile ci --failure-output=immediate --status-level=fail

# Run with bounded parallelism (leaves 2 cores free)
cargo nextest run --workspace --exclude copybook-bench -j $(($(nproc) > 2 ? $(nproc) - 2 : 1))
```

**Expected runtime**: 3-5 minutes

---

### Doctests

**Purpose**: Validates that all documentation examples compile and run correctly.

**Exact Command**:
```bash
RUSTDOCFLAGS="--deny warnings" cargo test --doc --workspace --exclude copybook-bench
```

**What it validates**:
- All doc comments compile without warnings
- All code examples in documentation execute correctly

**How to run locally**:
```bash
RUSTDOCFLAGS="--deny warnings" cargo test --doc --workspace --exclude copybook-bench
```

**Expected runtime**: 1-2 minutes

---

### Proptest Smoke (Bounded Cases + Fixed Seed)

**Purpose**: Quick property test run with bounded cases for PR validation.

**Exact Command**:
```bash
PROPTEST_CASES=256 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture
```

**What it validates**:
- All property tests pass with 256 test cases
- Tests are reproducible with fixed seed
- Invariants hold across generated inputs

**How to run locally**:
```bash
# Run with default configuration (256 cases, fixed seed)
PROPTEST_CASES=256 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture

# Run with custom configuration
PROPTEST_CASES=512 PROPTEST_SEED=my-seed cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture

# Run specific module
PROPTEST_CASES=256 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib roundtrip -- --test-threads=2 --nocapture
```

**Expected runtime**: 2-3 minutes

**Artifact locations**:
- `copybook-core/tests/proptest-regressions/` - Core property test regressions
- `copybook-codec/tests/proptest-regressions/` - Codec property test regressions

---

### Determinism Smoke (Advisory)

**Purpose**: Validates that encode/decode operations are deterministic.

**Exact Command**:
```bash
scripts/ci/determinism_smoke.sh
```

**What it validates**:
- Decode operations produce consistent output across multiple runs
- Both DISPLAY-heavy and COMP-3-heavy copybooks are tested
- CLI determinism command exits with code 0 (deterministic) or 2 (non-deterministic)

**How to run locally**:
```bash
# Run determinism smoke tests
scripts/ci/determinism_smoke.sh

# Or run specific determinism checks manually
cargo build --package copybook-cli --bin copybook --release
./target/release/copybook determinism decode \
  --format fixed \
  --codepage cp037 \
  --output json \
  fixtures/copybooks/simple.cpy \
  fixtures/data/simple.bin
```

**Expected runtime**: 2-3 minutes

**Artifact locations**:
- `target/determinism-simple.json` - Determinism report for simple.cpy
- `target/determinism-comp3.json` - Determinism report for comp3_test.cpy

---

### Security Checks (cargo deny)

**Purpose**: Validates dependency licenses, advisories, and bans.

**Exact Command**:
```bash
cargo deny check
```

**What it validates**:
- All dependencies have approved licenses
- No known security advisories in dependencies
- No duplicate or banned dependencies

**How to run locally**:
```bash
cargo deny check
```

**Expected runtime**: < 1 minute

---

### Security Checks (cargo audit)

**Purpose**: Checks for known security vulnerabilities in dependencies.

**Exact Command**:
```bash
cargo audit
```

**What it validates**:
- No known security vulnerabilities in dependencies
- Advisory database is up to date

**How to run locally**:
```bash
# Install cargo-audit if not already installed
cargo install cargo-audit --locked

# Run audit
cargo audit
```

**Expected runtime**: 1-2 minutes

**Artifact locations**:
- `target/security.audit.json` - Audit results in JSON format

**Note**: Only runs when `Cargo.lock` changes in CI.

---

### Feature Flags Testing

**Purpose**: Validates all 22 feature flags work correctly in both enabled and disabled states.

**Exact Command**:
```bash
# For each flag, run tests with flag enabled and disabled
COPYBOOK_FF_<FLAG_NAME>=1 cargo test --workspace --lib --features audit
COPYBOOK_FF_<FLAG_NAME>=0 cargo test --workspace --lib --features audit
```

**What it validates**:
- All feature flags can be toggled at runtime
- Code works correctly with each flag enabled
- Code works correctly with each flag disabled
- CLI integration with `--list-features`, `--enable-features`, `--disable-features`

**How to run locally**:
```bash
# Test specific flag
COPYBOOK_FF_MUTATION_TESTING=1 cargo test --workspace --lib --features audit

# Test CLI feature flag options
cargo build --release -p copybook-cli --features audit
./target/release/copybook --list-features
./target/release/copybook parse --enable-features mutation_testing,fuzzing_integration
```

**Expected runtime**: 5-10 minutes (for all 22 flags × 2 states)

---

## Section 3: Scheduled Tests (Stochastic/Expensive)

These tests run on schedule or manual trigger only. They are stochastic (non-deterministic) or expensive (long runtime).

### BDD Tests (Full Suite)

**Purpose**: Executable specifications in human-readable format.

**Exact Command**:
```bash
cargo test -p copybook-bdd -- --nocapture
```

**What it validates**:
- All BDD scenarios pass
- Copybook parsing behavior matches specifications
- Encode/decode operations work correctly
- Error handling works as expected

**How to run locally**:
```bash
# Run all BDD tests
cargo test -p copybook-bdd -- --nocapture

# Run specific feature file
cargo test -p copybook-bdd -- features/copybook_parsing.feature

# Run specific scenario
cargo test -p copybook-bdd -- "Parse a simple copybook with a single field"
```

**Expected runtime**: 2-3 minutes

**Trigger schedule**: Daily at 3:23 AM UTC (via `ci.yml`)

---

### Extended Proptest (More Cases/Seeds)

**Purpose**: Comprehensive property testing with more test cases.

**Exact Command**:
```bash
PROPTEST_CASES=1024 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture
```

**What it validates**:
- All property tests pass with 1024 test cases
- Tests are reproducible with fixed seed
- Invariants hold across a wider range of inputs

**How to run locally**:
```bash
# Run with extended configuration (1024 cases)
PROPTEST_CASES=1024 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture

# Run with custom seed for debugging
PROPTEST_CASES=1024 PROPTEST_SEED=my-debug-seed cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture
```

**Expected runtime**: 5-10 minutes

**Trigger schedule**: Weekly on Mondays at 3 AM UTC (via `ci-proptest.yml`)

**Artifact locations**:
- `copybook-core/tests/proptest-regressions/` - Core property test regressions
- `copybook-codec/tests/proptest-regressions/` - Codec property test regressions
- `tests/proptest-regressions/` - Integration property test regressions

**How to reproduce failures locally**:
```bash
# Run with the failing seed to reproduce
PROPTEST_CASES=1 PROPTEST_SEED=<failing-seed> cargo test -p copybook-proptest --lib -- --nocapture

# The regression file will be saved to the proptest-regressions directory
# Inspect the regression file to understand the failing input
```

---

### Fuzzing (Timeboxed)

**Purpose**: Automated discovery of edge cases and security vulnerabilities.

**Exact Command**:
```bash
# Run all fuzzers for 5 minutes
cargo fuzz run copybook_parse -- -runs=0 -max_total_time=300
cargo fuzz run binary_decode -- -runs=0 -max_total_time=300
cargo fuzz run json_encode -- -runs=0 -max_total_time=300
cargo fuzz run pic_clause -- -runs=0 -max_total_time=300
cargo fuzz run occurs_odo -- -runs=0 -max_total_time=300
cargo fuzz run redefines -- -runs=0 -max_total_time=300
```

**What it validates**:
- No crashes or panics with random inputs
- No memory safety issues
- No unexpected behaviors

**How to run locally**:
```bash
# Install cargo-fuzz
cargo install cargo-fuzz --version 0.13.4

# Run specific fuzzer for 5 minutes
cargo fuzz run copybook_parse -- -runs=0 -max_total_time=300

# Run with specific corpus
cargo fuzz run copybook_parse fuzz/corpus/copybook_parse/

# Minimize corpus
cargo fuzz cmin copybook_parse

# Triage crashes
cargo fuzz tmin copybook_parse fuzz/artifacts/copybook_parse/crash-*
```

**Expected runtime**: 5-10 minutes per target

**Trigger schedule**: Nightly at 2 AM UTC (via `fuzz-integration.yml`)

**Manual trigger**:
```bash
# Trigger with default settings (5 minutes)
gh workflow run fuzz-integration.yml

# Trigger with custom duration (10 minutes)
gh workflow run fuzz-integration.yml -f fuzz_seconds=600

# Trigger specific target
gh workflow run fuzz-integration.yml -f target=copybook_parse
```

**Artifact locations**:
- `fuzz/artifacts/<target>/` - Crash artifacts (90-day retention)
- `fuzz/corpus/<target>/` - Minimized corpus (90-day retention)

**How to reproduce failures locally**:
```bash
# Minimize the crash input
cargo fuzz tmin <target> fuzz/artifacts/<target>/crash-*

# The minimized input will help identify the root cause
# Report the issue with the minimized input
```

---

### Mutation Testing (Timeboxed, Scoped)

**Purpose**: Validates test suite effectiveness by mutating code.

**Exact Command**:
```bash
# Run on entire workspace
cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place

# Run on specific crate with custom threshold
cargo mutants --package copybook-core --timeout 300 --test-tool nextest --in-place --json --file mutants.toml
```

**What it validates**:
- Test suite catches at least X% of mutations
- Mutations that should be caught are caught
- Unviable mutations are properly identified

**How to run locally**:
```bash
# Install cargo-mutants
cargo install cargo-mutants

# Run on entire workspace
cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place

# Run on specific crate
cargo mutants --package copybook-core --file mutants.toml --test-tool nextest --in-place

# Run with custom threshold
cargo mutants --package copybook-core --threshold 80 --file mutants.toml --test-tool nextest --in-place

# Run with verbose output
cargo mutants --package copybook-core --file mutants.toml --test-tool nextest --in-place -vV
```

**Expected runtime**: 15-60 minutes per crate

**Per-crate thresholds**:
- `copybook-core`: 75% (critical parser code)
- `copybook-codec`: 75% (encoding/decoding correctness)
- `copybook-cli`: 65% (CLI with more boilerplate)
- `copybook-bench`: 60% (benchmark utilities)
- `copybook-gen`: 60% (test infrastructure)

**Trigger schedule**: Weekly on Sundays at 2 AM UTC (via `ci-mutants.yml`)

**Manual trigger**:
```bash
# Trigger with default settings
gh workflow run ci-mutants.yml

# Trigger with custom threshold (80%)
gh workflow run ci-mutants.yml -f threshold=80

# Trigger on specific workspace (core-only)
gh workflow run ci-mutants.yml -f workspace=core-only
```

**Artifact locations**:
- `mutants.out/outcomes.json` - Detailed mutation results
- `mutants-summary.csv` - Summary of mutation scores

**How to reproduce failures locally**:
```bash
# Run with verbose output to see which mutants were missed
cargo mutants --package <crate> --test-tool nextest -v

# Check specific mutant
cargo mutants --package <crate> --list-files
cargo mutants --package <crate> --file <file> --test-tool nextest
```

---

### Performance Benchmarks

**Purpose**: Measure and track performance characteristics.

**Exact Command**:
```bash
BENCH_FILTER=slo_validation bash scripts/bench.sh
```

**What it validates**:
- Performance meets SLO (Service Level Objective) targets
- No performance regressions compared to baseline
- Performance receipts are generated for tracking

**How to run locally**:
```bash
# Run SLO validation suite (default)
BENCH_FILTER=slo_validation bash scripts/bench.sh

# Run all benchmarks
BENCH_FILTER=all bash scripts/bench.sh

# Run specific benchmark
cargo bench -p copybook-bench <benchmark_name>
```

**Expected runtime**: 10-15 minutes

**Trigger schedule**: Manual dispatch or `perf:run` label (via `ci-bench.yml`)

**Artifact locations**:
- `target/perf.json` - Performance receipts

**How to reproduce failures locally**:
```bash
# Run benchmarks and inspect the output
BENCH_FILTER=slo_validation bash scripts/bench.sh

# Check the generated perf.json file
cat target/perf.json

# Compare against baseline (if available)
# The bench.sh script will automatically compare against baseline
```

---

### Soak Tests

**Purpose**: Long-running tests to catch memory leaks and resource issues.

**Exact Command**:
```bash
cargo test -p copybook-cli --features soak -- --ignored --test-threads=1 --nocapture
```

**What it validates**:
- No memory leaks over long-running operations
- No resource exhaustion
- Stable behavior over extended periods

**How to run locally**:
```bash
# Run soak tests
COPYBOOK_TEST_SLOW=1 cargo test -p copybook-cli --features soak -- --ignored --test-threads=1 --nocapture
```

**Expected runtime**: 10-20 minutes

**Trigger schedule**: Daily at 3:23 AM UTC (via `ci.yml`, schedule only)

---

## Section 4: Artifact Locations

This section documents where each test suite stores its artifacts.

### Proptest Regressions

**Purpose**: Store failing test cases for regression testing.

**Locations**:
- `copybook-core/tests/proptest-regressions/` - Core property test regressions
- `copybook-codec/tests/proptest-regressions/` - Codec property test regressions
- `tests/proptest-regressions/` - Integration property test regressions

**Retention**: 7 days in CI artifacts

**How to use**:
```bash
# When a proptest fails, the regression file is automatically saved
# To reproduce the failure:
PROPTEST_CASES=1 PROPTEST_SEED=<failing-seed> cargo test -p copybook-proptest --lib -- --nocapture

# After fixing the issue, remove the regression file
rm copybook-core/tests/proptest-regressions/<regression-file>
```

---

### Fuzz Crashers

**Purpose**: Store minimized crash inputs for debugging.

**Locations**:
- `fuzz/artifacts/<target>/` - Crash artifacts (e.g., `fuzz/artifacts/copybook_parse/crash-*`)
- `fuzz/corpus/<target>/` - Minimized corpus (e.g., `fuzz/corpus/copybook_parse/`)

**Retention**: 90 days in CI artifacts

**How to use**:
```bash
# To minimize a crash:
cargo fuzz tmin <target> fuzz/artifacts/<target>/crash-*

# To add a crash to the corpus:
cp fuzz/artifacts/<target>/crash-* fuzz/corpus/<target>/

# To run with a specific corpus:
cargo fuzz run <target> fuzz/corpus/<target>/
```

---

### Mutants Reports

**Purpose**: Store mutation testing results for analysis.

**Locations**:
- `mutants.out/outcomes.json` - Detailed mutation results
- `mutants-summary.csv` - Summary of mutation scores

**Retention**: Not uploaded to CI (local only)

**How to use**:
```bash
# To view mutation results:
cat mutants.out/outcomes.json | jq '.outcomes[] | select(.outcome == "MissedMutant")'

# To view summary:
cat mutants-summary.csv

# To generate a report:
cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place --json --list
```

---

### Performance Receipts

**Purpose**: Store performance measurements for tracking.

**Locations**:
- `target/perf.json` - Performance receipts

**Retention**: 7 days for PR artifacts, 90 days for main branch

**How to use**:
```bash
# To view performance receipts:
cat target/perf.json | jq .

# To compare against baseline:
# The bench.sh script automatically compares against baseline
# If baseline is missing, the comparison is non-fatal
```

---

### Determinism Receipts

**Purpose**: Store determinism validation results.

**Locations**:
- `target/determinism-simple.json` - Determinism report for simple.cpy
- `target/determinism-comp3.json` - Determinism report for comp3_test.cpy

**Retention**: 7 days in CI artifacts

**How to use**:
```bash
# To view determinism reports:
cat target/determinism-simple.json | jq .
cat target/determinism-comp3.json | jq .

# To run determinism checks manually:
cargo build --package copybook-cli --bin copybook --release
./target/release/copybook determinism decode \
  --format fixed \
  --codepage cp037 \
  --output json \
  fixtures/copybooks/simple.cpy \
  fixtures/data/simple.bin
```

---

### Security Audit Reports

**Purpose**: Store security audit results.

**Locations**:
- `target/security.audit.json` - Security audit results

**Retention**: 90 days in CI artifacts

**How to use**:
```bash
# To view security audit results:
cat target/security.audit.json | jq .

# To check for vulnerabilities:
cat target/security.audit.json | jq '.vulnerabilities.list | length'
```

---

## Section 5: Local Development Workflow

This section provides a quick start guide for running tests locally.

### 1. Run the Full PR Gate Locally

The **single canonical entrypoint** that matches PR CI exactly:

```bash
# Run the full PR gate (requires just)
just ci
```

**What it does**:
- Phase 1: Quick gates (fmt → clippy → build → nextest → doctests)
- Phase 2: Security gates (deny + conditional audit)

**Expected runtime**: 5-10 minutes (cold), 2-5 minutes (warm)

**Prerequisites**:
- `just` (install: `cargo install just`)
- `cargo-nextest` (install: `cargo install nextest`)
- `cargo-deny` (install: `cargo install cargo-deny`)

---

### 2. Run Individual Test Suites

#### Format Check
```bash
cargo fmt --all --check
```

#### Clippy
```bash
# Pedantic (libs/bins/examples)
cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic

# Tests (relaxed)
cargo clippy --workspace --tests --all-features \
  -- -D warnings \
  -A clippy::unwrap_used \
  -A clippy::expect_used \
  -A clippy::panic \
  -A clippy::dbg_macro \
  -A clippy::print_stdout \
  -A clippy::print_stderr \
  -A clippy::duplicated_attributes
```

#### Unit Tests
```bash
# Run with nextest (faster)
cargo nextest run --workspace --exclude copybook-bench --profile ci --failure-output=immediate --status-level=fail

# Run with cargo test (fallback)
cargo test --workspace --exclude copybook-bench
```

#### Doctests
```bash
RUSTDOCFLAGS="--deny warnings" cargo test --doc --workspace --exclude copybook-bench
```

#### Proptest
```bash
# Smoke test (256 cases)
PROPTEST_CASES=256 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture

# Extended test (1024 cases)
PROPTEST_CASES=1024 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --test-threads=2 --nocapture
```

#### Determinism
```bash
scripts/ci/determinism_smoke.sh
```

#### Security
```bash
# Deny (always)
cargo deny check

# Audit (only if Cargo.lock changed)
cargo audit
```

#### BDD Tests
```bash
cargo test -p copybook-bdd -- --nocapture
```

#### Fuzzing
```bash
# Install cargo-fuzz
cargo install cargo-fuzz --version 0.13.4

# Run specific fuzzer for 5 minutes
cargo fuzz run copybook_parse -- -runs=0 -max_total_time=300
```

#### Mutation Testing
```bash
# Install cargo-mutants
cargo install cargo-mutants

# Run on specific crate
cargo mutants --package copybook-core --file mutants.toml --test-tool nextest --in-place
```

#### Performance Benchmarks
```bash
# Run SLO validation suite
BENCH_FILTER=slo_validation bash scripts/bench.sh

# Run all benchmarks
BENCH_FILTER=all bash scripts/bench.sh
```

---

### 3. How to Reproduce CI Failures

#### Format Check Failure
```bash
# Apply formatting
cargo fmt --all

# Verify formatting is correct
cargo fmt --all --check
```

#### Clippy Failure
```bash
# Run clippy with the same flags as CI
cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic

# Fix the warnings and re-run
```

#### Unit Test Failure
```bash
# Run the failing test with output
cargo nextest run --workspace --exclude copybook-bench --profile ci --failure-output=immediate --status-level=fail

# Run specific test
cargo nextest run <test_name>
```

#### Doctest Failure
```bash
# Run doctests with output
RUSTDOCFLAGS="--deny warnings" cargo test --doc --workspace --exclude copybook-bench -- --nocapture

# Run specific doctest
cargo test --doc <crate_name>
```

#### Proptest Failure
```bash
# Run with the failing seed to reproduce
PROPTEST_CASES=1 PROPTEST_SEED=<failing-seed> cargo test -p copybook-proptest --lib -- --nocapture

# The regression file will be saved to the proptest-regressions directory
# Inspect the regression file to understand the failing input
```

#### Determinism Failure
```bash
# Run determinism smoke tests
scripts/ci/determinism_smoke.sh

# Inspect the determinism reports
cat target/determinism-simple.json | jq .
cat target/determinism-comp3.json | jq .
```

#### Security Failure
```bash
# Run deny check
cargo deny check

# Run audit (if Cargo.lock changed)
cargo audit

# Inspect the audit results
cat target/security.audit.json | jq .
```

#### Fuzzing Failure
```bash
# Minimize the crash input
cargo fuzz tmin <target> fuzz/artifacts/<target>/crash-*

# The minimized input will help identify the root cause
# Report the issue with the minimized input
```

#### Mutation Testing Failure
```bash
# Run with verbose output to see which mutants were missed
cargo mutants --package <crate> --test-tool nextest -v

# Check specific mutant
cargo mutants --package <crate> --list-files
cargo mutants --package <crate> --file <file> --test-tool nextest
```

#### Performance Failure
```bash
# Run benchmarks and inspect the output
BENCH_FILTER=slo_validation bash scripts/bench.sh

# Check the generated perf.json file
cat target/perf.json

# Compare against baseline (if available)
# The bench.sh script will automatically compare against baseline
```

---

## References

- [Testing Integration Summary](TESTING_INTEGRATION_SUMMARY.md) - How all testing methodologies work together
- [CI Scripts README](../scripts/ci/README.md) - Local-first CI scripts
- [Property Testing Documentation](PROPERTY_TESTING.md) - Property testing guide
- [Fuzzing Documentation](FUZZING.md) - Fuzzing guide
- [Mutation Testing Documentation](MUTATION_TESTING.md) - Mutation testing guide
- [Roadmap](ROADMAP.md) - Project roadmap and testing methodology overview
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
