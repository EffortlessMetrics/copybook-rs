#!/usr/bin/env bash
set -euo pipefail

# Fast path: fmt → clippy → build → tests → doctests
echo "==> Running cargo fmt --all --check"
cargo fmt --all --check

# Pedantic for production targets, relaxed for integration tests
echo "==> Running cargo clippy (pedantic: libs/bins/examples)"
cargo clippy --workspace --lib --bins --examples --all-features \
  -- -D warnings -W clippy::pedantic

echo "==> Running cargo clippy (tests: allow common test-only lints)"
cargo clippy --workspace --tests --all-features \
  -- -D warnings \
  -A clippy::unwrap_used \
  -A clippy::expect_used \
  -A clippy::panic \
  -A clippy::dbg_macro \
  -A clippy::print_stdout \
  -A clippy::print_stderr \
  -A clippy::duplicated_attributes

echo "==> Running cargo build --workspace --release"
cargo build --workspace --release

# Keep PRs fast & deterministic; bound jobs
echo "==> Running cargo nextest (portable, leave 2 cores free)"
# Determine parallel jobs portably (Linux/macOS/Windows)
if command -v nproc >/dev/null 2>&1; then JOBS=$(nproc); else JOBS=$( (sysctl -n hw.ncpu 2>/dev/null || echo 2) ); fi
JOBS=$(( JOBS>2 ? JOBS-2 : 1 ))
# Run tests with bounded parallelism (panic=abort requires nightly -Zpanic_abort_tests)
cargo nextest run --workspace --exclude copybook-bench -j "$JOBS" --failure-output=immediate

# Doc tests (fail on warnings)
echo "==> Running doc tests"
RUSTDOCFLAGS="--deny warnings" cargo test --doc --workspace --exclude copybook-bench

echo "✅ Quick gates passed"
