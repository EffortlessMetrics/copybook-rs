#!/usr/bin/env bash
set -euo pipefail

PERF="scripts/bench/perf.json"

if [[ ! -f "$PERF" ]]; then
  echo "Missing $PERF. Run bench.sh first." >&2
  exit 1
fi

CPU=$(grep -m1 'model name' /proc/cpuinfo | sed 's/.*: //')
CORES=$(nproc)
KERN=$(uname -r)
OS=$(uname -s)
DATE=$(date -Is)

jq --arg cpu "$CPU" \
  --argjson cores "$CORES" \
  --arg kern "$KERN" \
  --arg os "$OS" \
  --arg ts "$DATE" \
  '.summary |= (. // {}) | .summary += {host_cpu:$cpu, host_cores:$cores, host_kernel:$kern, host_os:$os, ts:$ts}' \
  "$PERF" > "${PERF}.tmp"
mv "${PERF}.tmp" "$PERF"

echo "Annotated $PERF with host info."
