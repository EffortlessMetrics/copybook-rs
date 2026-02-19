#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

PERF_PATH="scripts/bench/perf.json"

if [[ ! -f "${PERF_PATH}" ]]; then
  echo "No perf receipts found at ${PERF_PATH}; skipping aggregate."
  exit 0
fi

python3 <<'PY'
import json
import math
import os
import pathlib
import sys

ROOT = pathlib.Path.cwd()
perf_path = ROOT / "scripts" / "bench" / "perf.json"

try:
    perf = json.loads(perf_path.read_text())
except FileNotFoundError:
    print(f"No perf receipts found at {perf_path}; skipping aggregate.")
    sys.exit(0)

benchmarks = perf.get("benchmarks", [])
if not benchmarks:
    print("No benchmarks found in perf.json; skipping aggregate.")
    sys.exit(0)

samples = []
for bench in benchmarks:
    name = bench.get("name")
    if not name:
        continue
    bench_root = ROOT / "target" / "criterion" / name
    sample_path = bench_root / "new" / "sample.json"
    benchmark_path = bench_root / "new" / "benchmark.json"

    if not sample_path.is_file() or not benchmark_path.is_file():
        continue

    try:
        sample = json.loads(sample_path.read_text())
        bench_meta = json.loads(benchmark_path.read_text())
    except json.JSONDecodeError as err:
        print(f"Failed to parse criterion output for {name}: {err}", file=sys.stderr)
        continue

    throughput_bytes = bench_meta.get("throughput", {}).get("Bytes")
    iters = sample.get("iters", [])
    times = sample.get("times", [])

    if throughput_bytes is None or not iters or not times:
        continue

    for iterations, elapsed_ns in zip(iters, times):
        if iterations <= 0 or elapsed_ns <= 0:
            continue
        seconds = elapsed_ns / 1e9
        total_bytes = throughput_bytes * iterations
        mibps = total_bytes / seconds / (1024 ** 2)
        samples.append(mibps)

if not samples:
    print("No samples found; skipping aggregate.")
    sys.exit(0)

samples.sort()
count = len(samples)

def percentile(frac: float) -> float:
    rank = int(count * frac)
    if rank < 1:
        rank = 1
    if rank > count:
        rank = count
    return samples[rank - 1]

p50 = percentile(0.50)
p90 = percentile(0.90)
p99 = percentile(0.99)

summary = perf.get("summary") or {}
summary.update(
    {
        "p50_mibps": p50,
        "p90_mibps": p90,
        "p99_mibps": p99,
    }
)
perf["summary"] = summary

perf_path.write_text(json.dumps(perf, indent=2) + "\n")
print(
    f"Added percentiles to {perf_path.relative_to(ROOT)}: "
    f"p50={p50:.2f} p90={p90:.2f} p99={p99:.2f}"
)
PY
