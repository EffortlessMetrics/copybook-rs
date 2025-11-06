# Copybook-RS Justfile
# Modern task runner for copybook-rs development

# Set environment variables
set dotenv-load := true

# Default recipe to display help
default:
    @just --list

# Local-first CI scripts (match GitHub Actions workflows)

# Run quick CI checks (fmt → clippy → build → nextest → doctests)
quick:
    bash scripts/ci/quick.sh

# Run security checks (cargo-deny always; cargo-audit when Cargo.lock changes)
sec:
    bash scripts/ci/security.sh

# Run benchmarks and generate perf.json receipts (advisory; opt-in)
bench-ci:
    bash scripts/ci/bench.sh

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
    cargo clippy --all-targets --all-features --workspace -- -D warnings -W clippy::pedantic

# Format all code
fmt:
    cargo fmt --all

# Check formatting without making changes
fmt-check:
    cargo fmt --all -- --check

# Run cargo-deny to check dependencies and licenses
deny:
    cargo deny check

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

# Clean all build artifacts
clean:
    cargo clean

# Check MSRV compatibility (Rust 1.90)
check-msrv:
    cargo +1.90 check --workspace

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
    cargo run --package xtask --bin xtask -- ci --quick
