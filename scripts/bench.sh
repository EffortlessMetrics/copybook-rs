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

echo "Running benchmarks..."
cargo bench -p copybook-bench

echo "Benchmark results saved to target/criterion/"
echo "Open target/criterion/report/index.html to view detailed results"