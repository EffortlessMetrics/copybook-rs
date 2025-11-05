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
echo "==> Running cargo nextest (CI profile)"
export NEXTEST_PROFILE=ci
cargo nextest run --workspace --panic-abort -j "$(nproc --ignore=2)" --failure-output=immediate

# Doc tests (fail on warnings)
echo "==> Running doc tests"
RUSTDOCFLAGS="--deny warnings" cargo test --doc

echo "✅ Quick gates passed"
