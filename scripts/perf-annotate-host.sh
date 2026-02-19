#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
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

# Detect WSL2 environment
WSL2_DETECTED="false"
if [[ -f /proc/version ]]; then
  if grep -qi "microsoft" /proc/version; then
    WSL2_DETECTED="true"
  fi
fi

jq --arg cpu "$CPU" \
  --argjson cores "$CORES" \
  --arg kern "$KERN" \
  --arg os "$OS" \
  --arg ts "$DATE" \
  --argjson wsl2 "$WSL2_DETECTED" \
  '.summary |= (. // {}) | .summary += {host_cpu:$cpu, host_cores:$cores, host_kernel:$kern, host_os:$os, wsl2_detected:$WSL2_DETECTED, ts:$ts}' \
  "$PERF" > "${PERF}.tmp"
mv "${PERF}.tmp" "$PERF"

echo "Annotated $PERF with host info (WSL2: $WSL2_DETECTED)."
