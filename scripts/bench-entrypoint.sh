#!/bin/bash
set -euo pipefail

echo "==> copybook-rs benchmark container"
echo "==> Commit: $(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')"
echo "==> Rustc: $(rustc --version)"
echo "==> CPU: $(grep -m1 'model name' /proc/cpuinfo | sed 's/.*: //')"
echo "==> Cores: $(nproc)"
echo ""

# Run benchmarks
bash scripts/bench.sh

# Copy perf.json to output volume (if mounted)
if [[ -d /workspace/output ]]; then
    cp scripts/bench/perf.json /workspace/output/perf.json
    echo "✅ Receipts copied to /workspace/output/perf.json"
else
    echo "⚠️ No output volume mounted at /workspace/output"
    echo "   Receipts available at /workspace/scripts/bench/perf.json (inside container)"
fi

# Summarize results
if command -v cargo >/dev/null 2>&1; then
    echo ""
    echo "==> Performance Summary:"
    cargo run -p xtask -- perf --summarize-last || true
fi
