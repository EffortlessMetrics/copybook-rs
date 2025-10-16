#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-scan}" # scan | compare
BASE="${2:-/tmp/pedantic_base.log}"
CURR="${3:-/tmp/pedantic_curr.log}"

run_clippy() {
  cargo clippy --workspace --all-targets --all-features -- -W clippy::pedantic 2>&1 | tee "$1" >/dev/null
}

summarize() {
  local LOG="$1"
  echo "=== Top lints ==="
  awk '/clippy::/ {match($0,/clippy::([a-z0-9_]+)/,m); if(m[1]) print m[1];}' "$LOG" \
    | sort | uniq -c | sort -nr | head -20
  echo
  echo "=== Top files ==="
  awk -F: '/\.rs:/ {print $1}' "$LOG" | sort | uniq -c | sort -nr | head -20
}

if [[ "$MODE" == "scan" ]]; then
  run_clippy "$CURR"
  summarize "$CURR"
  exit 0
fi

if [[ "$MODE" == "compare" ]]; then
  [[ -f "$BASE" ]] || { echo "No baseline at $BASE; run: $0 scan && cp $CURR $BASE"; exit 1; }
  run_clippy "$CURR"

  echo "=== New lints since baseline ==="
  comm -13 <(rg -n 'clippy::' "$BASE" | sort) <(rg -n 'clippy::' "$CURR" | sort) || true

  echo
  echo "=== Resolved lints since baseline ==="
  comm -23 <(rg -n 'clippy::' "$BASE" | sort) <(rg -n 'clippy::' "$CURR" | sort) || true

  echo
  echo "=== Current summary ==="
  summarize "$CURR"
  exit 0
fi

echo "Usage: $0 [scan|compare] [baseline_log] [current_log]"
