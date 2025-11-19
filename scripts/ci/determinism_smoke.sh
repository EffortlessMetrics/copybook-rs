#!/usr/bin/env bash
set -euo pipefail

# Determinism smoke test script
# -----------------------------
# This is the single source of truth for determinism smoke checks.
# CI (determinism-smoke.yml) and local runs both call this script.
#
# Exit codes:
#   0 = All determinism checks passed (deterministic)
#   2 = Determinism failure detected (non-deterministic)
#   3 = Error during validation (invalid data, schema issues, etc.)
#
# Usage (local):
#   scripts/ci/determinism_smoke.sh
#
# Usage (CI):
#   Called automatically by .github/workflows/determinism-smoke.yml

# Fixture paths (relative to repo root)
COPYBOOK_SIMPLE="fixtures/copybooks/simple.cpy"
DATA_SIMPLE="fixtures/data/simple.bin"

COPYBOOK_COMP3="fixtures/copybooks/comp3_test.cpy"
DATA_COMP3="fixtures/data/comp3_test.bin"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Determinism Smoke Tests (Issue #112 Phase 3)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo

echo "[1/3] Building copybook CLI..."
cargo build --package copybook-cli --bin copybook --release --quiet
echo "      ✓ Build complete"
echo

echo "[2/3] Running determinism smoke: DISPLAY-heavy (simple.cpy)"
./target/release/copybook determinism decode \
  --format fixed \
  --codepage cp037 \
  --output json \
  "${COPYBOOK_SIMPLE}" \
  "${DATA_SIMPLE}" \
  > target/determinism-simple.json

SIMPLE_STATUS=$?
if [ $SIMPLE_STATUS -eq 0 ]; then
  echo "      ✓ Deterministic (exit code: 0)"
elif [ $SIMPLE_STATUS -eq 2 ]; then
  echo "      ✗ Non-deterministic (exit code: 2)"
  cat target/determinism-simple.json
  exit 2
else
  echo "      ✗ Error during validation (exit code: ${SIMPLE_STATUS})"
  cat target/determinism-simple.json
  exit 3
fi
echo

echo "[3/3] Running determinism smoke: COMP-3-heavy (comp3_test.cpy)"
./target/release/copybook determinism decode \
  --format fixed \
  --codepage cp037 \
  --output json \
  "${COPYBOOK_COMP3}" \
  "${DATA_COMP3}" \
  > target/determinism-comp3.json

COMP3_STATUS=$?
if [ $COMP3_STATUS -eq 0 ]; then
  echo "      ✓ Deterministic (exit code: 0)"
elif [ $COMP3_STATUS -eq 2 ]; then
  echo "      ✗ Non-deterministic (exit code: 2)"
  cat target/determinism-comp3.json
  exit 2
else
  echo "      ✗ Error during validation (exit code: ${COMP3_STATUS})"
  cat target/determinism-comp3.json
  exit 3
fi
echo

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✓ All determinism smoke tests passed"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
