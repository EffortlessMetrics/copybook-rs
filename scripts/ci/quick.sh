#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

# Prefer cargo.exe on Git Bash/Windows to avoid stale MSYS cargo binaries.
if command -v cargo.exe >/dev/null 2>&1; then
  CARGO_BIN="cargo.exe"
else
  CARGO_BIN="cargo"
fi

# Fast path: fmt → clippy → build → tests → governance+BDD smoke → doctests
echo "==> Running cargo fmt --all --check"
"$CARGO_BIN" fmt --all --check

# Pedantic for production targets, relaxed for integration tests
echo "==> Running cargo clippy (pedantic: libs/bins/examples)"
"$CARGO_BIN" clippy --workspace --lib --bins --examples --all-features \
  -- -D warnings -W clippy::pedantic

echo "==> Running cargo clippy (tests: allow common test-only lints)"
"$CARGO_BIN" clippy --workspace --tests --all-features \
  -- -D warnings \
  -A clippy::unwrap_used \
  -A clippy::expect_used \
  -A clippy::panic \
  -A clippy::dbg_macro \
  -A clippy::print_stdout \
  -A clippy::print_stderr \
  -A clippy::missing_inline_in_public_items \
  -A clippy::duplicated_attributes

echo "==> Running cargo build --workspace --release"
"$CARGO_BIN" build --workspace --release

# Keep PRs fast & deterministic; bound jobs
echo "==> Running cargo nextest (portable, leave 2 cores free)"
# Determine parallel jobs portably (Linux/macOS/Windows)
if command -v nproc >/dev/null 2>&1; then JOBS=$(nproc); else JOBS=$( (sysctl -n hw.ncpu 2>/dev/null || echo 2) ); fi
JOBS=$(( JOBS>2 ? JOBS-2 : 1 ))
# Run tests with bounded parallelism (panic=abort requires nightly -Zpanic_abort_tests)
"$CARGO_BIN" nextest run --workspace --exclude copybook-bench -j "$JOBS" --failure-output=immediate

echo "==> Running governance microcrate + BDD smoke gate"
bash scripts/ci/governance-bdd-smoke.sh

# Doc tests (fail on warnings)
echo "==> Running doc tests"
RUSTDOCFLAGS="--deny warnings" "$CARGO_BIN" test --doc --workspace --exclude copybook-bench

echo "✅ Quick gates passed"
