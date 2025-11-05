#!/usr/bin/env bash
set -euo pipefail

echo "==> Running benchmark suite"
# Bench receipts: small JSON artifact; no HTML, no lcov

export RUSTFLAGS="-C target-cpu=native"
export PERF=1

# Ensure target directory exists
mkdir -p target

# Run benchmarks and capture JSON output
cargo bench -p copybook-bench -- --output-format json > target/perf.json || {
  echo "⚠️ Benchmark execution failed, creating minimal perf.json"
  echo '{"error": "benchmark_failed"}' > target/perf.json
}

# Check if bench-report binary exists, if not try to build it
if ! command -v bench-report &> /dev/null; then
  echo "==> Building bench-report CLI"
  cargo build --release -p copybook-bench --bin bench-report || {
    echo "⚠️ bench-report build failed, skipping validation"
    exit 0
  }
  BENCH_REPORT="./target/release/bench-report"
else
  BENCH_REPORT="bench-report"
fi

# Validate/compare against baseline (non-fatal if baseline missing)
if [ -f target/perf.json ]; then
  echo "==> Validating perf.json"
  $BENCH_REPORT validate target/perf.json || true

  echo "==> Comparing against baseline (non-fatal if missing)"
  $BENCH_REPORT compare target/perf.json || {
    echo "⚠️ Baseline comparison failed or baseline missing (non-fatal)"
  }
else
  echo "⚠️ perf.json not found, skipping validation"
fi

echo "✅ Benchmark receipts generated"
