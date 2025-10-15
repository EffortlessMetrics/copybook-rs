#!/usr/bin/env bash
set -euo pipefail
RUSTFLAGS="-C target-cpu=native" PERF=1 \
  cargo bench -p copybook-bench -- --output-format json > target/perf.json
mkdir -p scripts/bench
cp target/perf.json scripts/bench/perf.json
echo "✅ receipts: scripts/bench/perf.json"
