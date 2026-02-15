# Copybook-RS Justfile
# Modern task runner for copybook-rs development

# Set environment variables
set dotenv-load := true

# Default recipe to display help
default:
    @just --list

# Build all workspace crates
build:
    cargo build --workspace

# Build with release optimizations
build-release:
    cargo build --workspace --release

# Run all tests using cargo-nextest (preferred)
test:
    cargo nextest run --workspace

# Run all tests, including long-running and ignored tests
test-all:
    cargo nextest run --workspace --run-ignored all

# Run tests with legacy cargo test (fallback)
test-legacy:
    cargo test --workspace

# Run clippy lints with pedantic warnings
lint:
    cargo clippy --workspace --lib --bins --examples --all-features -- -D warnings -W clippy::pedantic
    cargo clippy --workspace --tests --all-features -- -D warnings \
      -A clippy::unwrap_used \
      -A clippy::expect_used \
      -A clippy::panic \
      -A clippy::dbg_macro \
      -A clippy::print_stdout \
      -A clippy::print_stderr \
      -A clippy::duplicated_attributes

# Format all code
fmt:
    cargo fmt --all

# Check formatting without making changes
fmt-check:
    cargo fmt --all -- --check

# Run cargo-deny to check dependencies and licenses
deny:
    cargo deny check

# API Freeze Management
# Check API compatibility against baseline
api-check:
    bash scripts/api-baseline.sh check

# Generate/update API baseline for current version
api-baseline:
    bash scripts/api-baseline.sh generate

# Show current API baseline information
api-info:
    bash scripts/api-baseline.sh info

# Check if API freeze is active
api-freeze-status:
    bash scripts/api-baseline.sh freeze-status

# Build documentation
docs:
    cargo doc --workspace --no-deps

# Build documentation and open in browser
docs-open:
    cargo doc --workspace --no-deps --open

# Pedantic scan and summary
pedantic:
    bash scripts/clippy-pedantic-diff.sh scan

# Compare to saved baseline
pedantic-compare:
    bash scripts/clippy-pedantic-diff.sh compare /tmp/pedantic_base.log /tmp/pedantic_curr.log

# Fast perf receipts (SLO suite); set BENCH_FILTER=all to widen
bench-json:
    BENCH_FILTER=${BENCH_FILTER:-slo_validation} bash scripts/bench.sh

# Run performance benchmarks (JSON receipts)
bench:
    @just bench-json

# Run performance benchmarks and generate receipts (alias for bench)
perf:
    @just bench-json

# Run baseline benchmarks only (no enterprise features)
bench-baseline:
    BENCH_FILTER=enterprise_baseline RUSTFLAGS="-C target-cpu=native" PERF=1 \
      cargo bench -p copybook-bench -- enterprise_baseline --quiet

# Run enterprise benchmarks only (audit, compliance, security, combined)
bench-enterprise:
    RUSTFLAGS="-C target-cpu=native" PERF=1 \
      cargo bench -p copybook-bench -- "enterprise_(audit|compliance|security|combined)" --quiet

# Run all enterprise SLO validation benchmarks
bench-enterprise-slo:
    RUSTFLAGS="-C target-cpu=native" PERF=1 \
      cargo bench -p copybook-bench -- enterprise_slo_validation --quiet

# Compare baseline vs enterprise performance
# Usage: just bench-compare <baseline_file> <enterprise_file>
bench-compare baseline enterprise:
    #!/usr/bin/env bash
    set -euo pipefail
    
    BASELINE="{{baseline}}"
    ENTERPRISE="{{enterprise}}"
    
    if [[ ! -f "$BASELINE" ]]; then
        echo "❌ Baseline file not found: $BASELINE"
        exit 1
    fi
    
    if [[ ! -f "$ENTERPRISE" ]]; then
        echo "❌ Enterprise file not found: $ENTERPRISE"
        exit 1
    fi
    
    echo "Comparing performance receipts:"
    echo "  Baseline:   $BASELINE"
    echo "  Enterprise:  $ENTERPRISE"
    echo ""
    
    # Extract throughput values
    BASE_THROUGHPUT=$(jq -r '
        [
          .benchmarks[]? | select(.name | contains("baseline")) | .mean_mibps,
          .benchmarks[]? | select(.name | contains("slo")) | .mean_mibps
        ] | max // 0
    ' "$BASELINE")
    
    ENT_THROUGHPUT=$(jq -r '
        [
          .benchmarks[]? | select(.name | contains("combined")) | .mean_mibps,
          .benchmarks[]? | select(.name | contains("slo")) | .mean_mibps
        ] | max // 0
    ' "$ENTERPRISE")
    
    # Calculate overhead
    if [[ "$BASE_THROUGHPUT" != "0" && "$BASE_THROUGHPUT" != "null" ]]; then
        OVERHEAD=$(awk "BEGIN {printf \"%.2f\", (($BASE_THROUGHPUT - $ENT_THROUGHPUT) / $BASE_THROUGHPUT) * 100}")
    else
        OVERHEAD="N/A"
    fi
    
    echo "Baseline Throughput:   ${BASE_THROUGHPUT} MiB/s"
    echo "Enterprise Throughput: ${ENT_THROUGHPUT} MiB/s"
    echo "Enterprise Overhead:   ${OVERHEAD}%"
    echo ""
    
    # Check against targets
    if [[ "$OVERHEAD" != "N/A" ]]; then
        OVERHEAD_NUM=$(echo "$OVERHEAD" | awk '{print $1}')
        if (( $(echo "$OVERHEAD_NUM < 2.0" | bc -l) )); then
            echo "✅ Excellent: Overhead < 2%"
        elif (( $(echo "$OVERHEAD_NUM < 3.0" | bc -l) )); then
            echo "✅ Good: Overhead < 3%"
        elif (( $(echo "$OVERHEAD_NUM < 5.0" | bc -l) )); then
            echo "⚠️  Acceptable: Overhead < 5% (within target)"
        else
            echo "❌ Warning: Overhead >= 5% (exceeds target)"
        fi
    fi

# Compare two performance receipts
# Usage: just perf-compare <baseline_file> <pr_file>
perf-compare baseline pr:
    #!/usr/bin/env bash
    set -euo pipefail
    
    BASELINE="{{baseline}}"
    PR="{{pr}}"
    
    if [[ ! -f "$BASELINE" ]]; then
        echo "❌ Baseline file not found: $BASELINE"
        exit 1
    fi
    
    if [[ ! -f "$PR" ]]; then
        echo "❌ PR file not found: $PR"
        exit 1
    fi
    
    echo "Comparing performance receipts:"
    echo "  Baseline: $BASELINE"
    echo "  PR:       $PR"
    echo ""
    
    # Extract values
    BASE_DISPLAY=$(jq -r '.summary.display_mibps // 0' "$BASELINE")
    BASE_COMP3=$(jq -r '.summary.comp3_mibps // 0' "$BASELINE")
    CURR_DISPLAY=$(jq -r '.summary.display_mibps // 0' "$PR")
    CURR_COMP3=$(jq -r '.summary.comp3_mibps // 0' "$PR")
    
    # Calculate deltas
    if [[ "$BASE_DISPLAY" != "0" && "$BASE_DISPLAY" != "null" ]]; then
        DISPLAY_DELTA=$(awk "BEGIN {printf \"%.2f\", (($CURR_DISPLAY - $BASE_DISPLAY) / $BASE_DISPLAY) * 100}")
    else
        DISPLAY_DELTA="N/A"
    fi
    
    if [[ "$BASE_COMP3" != "0" && "$BASE_COMP3" != "null" ]]; then
        COMP3_DELTA=$(awk "BEGIN {printf \"%.2f\", (($CURR_COMP3 - $BASE_COMP3) / $BASE_COMP3) * 100}")
    else
        COMP3_DELTA="N/A"
    fi
    
    # Determine status (advisory mode: warn on >5% regression)
    STATUS="✅ Pass"
    WARNINGS=""
    
    # Check for regressions (negative delta means regression)
    if command -v bc >/dev/null 2>&1; then
        if [[ "$DISPLAY_DELTA" != "N/A" ]]; then
            DELTA_NUM=$(echo "$DISPLAY_DELTA" | awk '{print $1}')
            if (( $(echo "$DELTA_NUM < -5" | bc -l) )); then
                STATUS="⚠️ Warning"
                WARNINGS="$WARNINGS\n  - DISPLAY: ${DISPLAY_DELTA}% regression (warn threshold: -5%)"
            fi
        fi
        
        if [[ "$COMP3_DELTA" != "N/A" ]]; then
            DELTA_NUM=$(echo "$COMP3_DELTA" | awk '{print $1}')
            if (( $(echo "$DELTA_NUM < -5" | bc -l) )); then
                STATUS="⚠️ Warning"
                WARNINGS="$WARNINGS\n  - COMP-3: ${COMP3_DELTA}% regression (warn threshold: -5%)"
            fi
        fi
    fi
    
    # Output comparison table
    echo "| Metric | Baseline | PR | Delta |"
    echo "|--------|----------|-----|-------|"
    echo "| DISPLAY | ${BASE_DISPLAY} MiB/s | ${CURR_DISPLAY} MiB/s | ${DISPLAY_DELTA}% |"
    echo "| COMP-3 | ${BASE_COMP3} MiB/s | ${CURR_COMP3} MiB/s | ${COMP3_DELTA}% |"
    echo ""
    echo "Status: $STATUS"
    
    if [[ -n "$WARNINGS" ]]; then
        echo ""
        echo "Warnings:"
        echo -e "$WARNINGS"
    fi

# Quick CI checks (build, test, lint) - legacy version
ci-quick-legacy:
    just build
    just test
    just lint

# Full CI checks (includes docs and deny)
ci-full:
    just build
    just test
    just lint
    just fmt-check
    just deny
    just docs

# Local CI gate - matches PR CI exactly (quick.sh + security.sh)
# Expected runtime: 5-10 minutes (cold), 2-5 minutes (warm)
# Prerequisites: cargo-nextest, cargo-deny
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "==> Running local CI gate (matches PR CI)"
    echo ""
    echo "Phase 1: Quick gates (fmt → clippy → build → nextest → doctests)"
    bash scripts/ci/quick.sh
    echo ""
    echo "Phase 2: Security gates (deny + conditional audit)"
    # Set up BASE_SHA/HEAD_SHA for security.sh (uses HEAD~1 if not in git)
    export BASE_SHA="${BASE_SHA:-$(git merge-base origin/main HEAD 2>/dev/null || echo HEAD~1)}"
    export HEAD_SHA="${HEAD_SHA:-$(git rev-parse HEAD 2>/dev/null || echo HEAD)}"
    bash scripts/ci/security.sh
    echo ""
    echo "✅ Local CI gate passed - matches PR CI"

# PR gate - runs all PR-gated tests locally (same as just ci)
# Expected runtime: 5-10 minutes (cold), 2-5 minutes (warm)
# Prerequisites: cargo-nextest, cargo-deny
pr:
    @just ci

# Scheduled tests - runs scheduled lane tests locally (optional, for validation)
# Expected runtime: 30-60 minutes (varies by which tests you run)
# Prerequisites: cargo-nextest, cargo-deny, cargo-mutants
scheduled:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "==> Running scheduled lane tests locally"
    echo ""
    echo "Note: This is optional and for validation only."
    echo "Full scheduled tests can take 30-60 minutes."
    echo ""
    read -p "Run BDD tests? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "==> Running BDD tests"
        cargo test -p copybook-bdd -- --nocapture
    fi
    read -p "Run extended proptest (1024 cases)? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "==> Running extended proptest"
        PROPTEST_CASES=1024 PROPTEST_SEED=copybook-rs-proptest cargo test -p copybook-proptest --lib -- --nocapture
    fi
    read -p "Run mutation testing? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "==> Running mutation testing"
        cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place
    fi
    read -p "Run benchmarks? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "==> Running benchmarks"
        just bench
    fi
    echo ""
    echo "✅ Selected scheduled tests completed"

# Clean all build artifacts
clean:
    cargo clean

# Check MSRV compatibility (Rust 1.92)
check-msrv:
    cargo +1.92.0 check --workspace

# Run a specific crate's tests
test-crate crate:
    cargo nextest run -p {{crate}}

# Run a specific crate's benchmarks
bench-crate crate:
    if [ "{{crate}}" = "copybook-bench" ]; then just bench-json; else cargo bench -p {{crate}}; fi

# Generate test coverage report
coverage:
    cargo llvm-cov --all-features --workspace --lcov --output-path lcov.info

# Watch for changes and run tests
watch:
    cargo watch -x "nextest run --workspace"

# Watch for changes and run tests for specific crate
watch-crate crate:
    cargo watch -x "nextest run -p {{crate}}"

# Run xtask automation commands
xtask *ARGS:
    cargo run --package xtask --bin xtask -- {{ARGS}}

# Quick local CI using xtask (replaces GitHub Actions)
ci-local:
    cargo run --package xtask --bin xtask -- ci

# Quick local CI (skip docs and deny for speed)
ci-quick:
    bash scripts/ci/quick.sh

# Run mutation testing on all workspace crates
mutants:
    cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place

# Run mutation testing on specific crate
mutants-crate crate:
    cargo mutants --package {{crate}} --file mutants.toml --test-tool nextest --in-place

# Run mutation testing with custom threshold
mutants-threshold threshold:
    cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place --threshold {{threshold}}

# Run mutation testing with verbose output
mutants-verbose:
    cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place -vV

# Run mutation testing on core crates only (core, codec)
mutants-core:
    cargo mutants --package copybook-core --package copybook-codec --file mutants.toml --test-tool nextest --in-place

# Generate mutation testing report
mutants-report:
    cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place --json --list

# ============================================================================
# Examples and Adapters
# ============================================================================

# Build all examples
examples:
    cargo build --workspace --examples

# Build Arrow adapter
example-arrow:
    cargo build -p copybook-arrow --examples

# Run Arrow decode example
example-arrow-decode:
    cargo run --example decode_to_arrow -p copybook-arrow

# Run Arrow Parquet example
example-arrow-parquet:
    cargo run --example decode_to_parquet -p copybook-arrow

# Run Arrow batch processing example
example-arrow-batch:
    cargo run --example batch_processing -p copybook-arrow

# Build Kafka example
example-kafka:
    cargo build -p kafka-pipeline

# Run Kafka example
example-kafka-run:
    cargo run -p kafka-pipeline
