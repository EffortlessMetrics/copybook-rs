#!/usr/bin/env bash
set -euo pipefail

# Fast path: fmt → clippy → build → tests → doctests
echo "==> Running cargo fmt --all --check"
cargo fmt --all --check

# Pedantic keeps quality high but predictable
echo "==> Running cargo clippy (pedantic)"
cargo clippy --workspace --all-targets --all-features \
  -- -D warnings -W clippy::pedantic

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
