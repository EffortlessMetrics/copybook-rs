#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

echo "╔══════════════════════════════════════╗"
echo "║  Offline CI: full local validation  ║"
echo "╚══════════════════════════════════════╝"
echo

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

step() {
  echo
  echo "==> $1"
}

# 1) Fast hygiene gate (fmt + clippy + core tests)
step "Quick gate (fmt/clippy/build/tests/doctests)"
./scripts/ci/quick.sh

# 2) Support matrix drift detection
step "Support matrix drift check"
cargo run -p xtask -- docs verify-support-matrix

# 3) Support CLI unit tests
step "Support CLI unit tests"
cargo test -p copybook-cli --bin copybook support::tests

# 4) Perf receipts: generate + summarize (debug mode for structure)
OUT_DIR="target/benchmarks/offline-all"
mkdir -p "$OUT_DIR"

step "Generate perf receipts (xtask perf)"
cargo run -p xtask -- perf --out-dir "$OUT_DIR"

step "Summarize latest perf receipt"
cargo run -p xtask -- perf --summarize-last

# 5) xtask perf unit + integration tests
step "xtask perf unit/integration tests"
cargo test -p xtask

echo
echo "✅ Offline CI: all checks passed"
