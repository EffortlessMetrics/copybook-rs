#!/usr/bin/env bash
set -euo pipefail

echo "╔══════════════════════════════════════╗"
echo "║  Offline Semantic Validation Only   ║"
echo "╚══════════════════════════════════════╝"
echo

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

step() {
  echo
  echo "==> $1"
}

# Skip full rebuilds - just test the semantic guarantees

# 1) Support matrix drift detection
step "Support matrix drift check"
cargo run -p xtask -- docs verify-support-matrix

# 2) Support CLI unit tests (no rebuild needed if binary exists)
step "Support CLI unit tests"
cargo test -p copybook-cli --bin copybook support::tests --no-fail-fast

# 3) Perf receipts: use existing perf.json if available
if [ -f "scripts/bench/perf.json" ]; then
    step "Summarize existing perf receipt"
    cargo run -p xtask -- perf --summarize-last
else
    echo "  (Skipping - no perf.json available)"
fi

# 4) xtask perf unit + integration tests
step "xtask perf unit/integration tests"
cargo test -p xtask --no-fail-fast

echo
echo "✅ Offline semantic validation: all checks passed"
echo
echo "Note: Full builds skipped due to WSL environment instability."
echo "Run 'scripts/ci/offline-all.sh' on stable environment for complete gate."
