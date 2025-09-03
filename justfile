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

# Run performance benchmarks (requires PERF=1)
bench:
    #!/usr/bin/env bash
    if [ "$PERF" != "1" ]; then
        echo "PERF environment variable not set to 1. Benchmarks are gated behind this flag."
        echo "Run with: PERF=1 just bench"
        exit 1
    fi
    cargo bench -p copybook-bench

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

# Check MSRV compatibility (Rust 1.89)
check-msrv:
    cargo +1.89 check --workspace

# Run a specific crate's tests
test-crate crate:
    cargo nextest run -p {{crate}}

# Run a specific crate's benchmarks
bench-crate crate:
    #!/usr/bin/env bash
    if [ "$PERF" != "1" ]; then
        echo "PERF environment variable not set to 1. Benchmarks are gated behind this flag."
        echo "Run with: PERF=1 just bench-crate {{crate}}"
        exit 1
    fi
    cargo bench -p {{crate}}

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