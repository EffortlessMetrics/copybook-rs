#!/bin/bash
# Run performance benchmarks
# Usage: ./scripts/bench.sh

set -e

echo "Running copybook-rs performance benchmarks..."
echo "This requires PERF=1 environment variable to enable criterion benchmarks"

if [ "$PERF" != "1" ]; then
    echo "PERF environment variable not set to 1. Benchmarks are gated behind this flag."
    echo "Run with: PERF=1 ./scripts/bench.sh"
    exit 1
fi

echo "Building benchmark crate..."
cargo build --release -p copybook-bench

echo "Running benchmarks (JSON receipts enabled)..."
cargo bench -p copybook-bench -- --output-format json > target/perf.json

echo "Bridging receipts to scripts/bench/perf.json for CI upload..."
mkdir -p scripts/bench
cp target/perf.json scripts/bench/perf.json

echo "Benchmark receipts available at scripts/bench/perf.json"
echo "Criterion reports remain under target/criterion/"
