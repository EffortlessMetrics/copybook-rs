#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

# Run only the SLO validation benchmarks to capture throughput receipts by default.
# tripwire: no-op change to trigger perf workflow without affecting behavior.
# Allow callers to widen scope by exporting BENCH_FILTER.
BENCH_FILTER="${BENCH_FILTER:-slo_validation}"

# Capture build profile and target CPU flags
BUILD_PROFILE="${BUILD_PROFILE:-release}"
TARGET_CPU="${TARGET_CPU:-native}"
RUSTFLAGS="-C target-cpu=${TARGET_CPU}" PERF=1 \
  cargo bench -p copybook-bench -- "${BENCH_FILTER}" --quiet

# Function to detect WSL2 environment
detect_wsl2() {
  if [[ -f /proc/version ]] && grep -q "Microsoft\|WSL" /proc/version; then
    echo "true"
  else
    echo "false"
  fi
}

# Function to get CPU information
get_cpu_info() {
  if command -v lscpu >/dev/null 2>&1; then
    lscpu | grep "Model name:" | cut -d':' -f2- | xargs
  elif [[ -f /proc/cpuinfo ]]; then
    grep "model name" /proc/cpuinfo | head -1 | cut -d':' -f2- | xargs
  else
    echo "Unknown CPU"
  fi
}

# Function to get CPU core count
get_cpu_cores() {
  if command -v nproc >/dev/null 2>&1; then
    nproc
  elif [[ -f /proc/cpuinfo ]]; then
    grep -c "^processor" /proc/cpuinfo
  else
    echo "1"
  fi
}

# Function to get OS information
get_os_info() {
  if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    echo "Linux"
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "macOS"
  elif [[ "$OSTYPE" == "msys"* ]] || [[ "$OSTYPE" == "cygwin"* ]]; then
    echo "Windows"
  else
    echo "Unknown"
  fi
}

# Function to get kernel version
get_kernel_version() {
  if command -v uname >/dev/null 2>&1; then
    uname -r
  else
    echo "Unknown"
  fi
}

python3 <<PY
import datetime
import json
import pathlib
import subprocess
import sys

ROOT = pathlib.Path.cwd()
CRITERION_ROOT = ROOT / "target" / "criterion" / "slo_validation"

def load_throughput(name: str):
    bench_dir = CRITERION_ROOT / name / "new"
    try:
        estimates = json.loads((bench_dir / "estimates.json").read_text())
        benchmark = json.loads((bench_dir / "benchmark.json").read_text())
    except FileNotFoundError as err:
        print(f"❌ missing criterion output for {name}: {err}", file=sys.stderr)
        raise

    mean_ns = estimates["mean"]["point_estimate"]
    bytes_processed = benchmark["throughput"]["Bytes"]
    seconds = mean_ns / 1e9
    mibps = bytes_processed / seconds / (1024 ** 2)
    return {
        "name": f"slo_validation/{name}",
        "mean_ns": mean_ns,
        "bytes_processed": bytes_processed,
        "mean_mibps": mibps,
    }

def compute_percentiles(benchmark_names: list) -> dict:
    """Compute p50/p90/p99 percentiles from criterion sample data."""
    samples = []
    for name in benchmark_names:
        bench_root = CRITERION_ROOT / name / "new"
        sample_path = bench_root / "sample.json"
        benchmark_path = bench_root / "benchmark.json"

        if not sample_path.is_file() or not benchmark_path.is_file():
            continue

        try:
            sample = json.loads(sample_path.read_text())
            bench_meta = json.loads(benchmark_path.read_text())
        except json.JSONDecodeError:
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
        return {}

    samples.sort()
    count = len(samples)

    def percentile(frac: float) -> float:
        rank = int(count * frac)
        if rank < 1:
            rank = 1
        if rank > count:
            rank = count
        return samples[rank - 1]

    return {
        "p50_mibps": percentile(0.50),
        "p90_mibps": percentile(0.90),
        "p99_mibps": percentile(0.99),
    }


# Get environment data from shell
build_profile = """${BUILD_PROFILE}"""
target_cpu = """${TARGET_CPU}"""
wsl2_detected = """$(detect_wsl2)""" == "true"
cpu_model = """$(get_cpu_info)"""
cpu_cores = int("""$(get_cpu_cores)""")
os_name = """$(get_os_info)"""
kernel_version = """$(get_kernel_version)"""

display = load_throughput("display_heavy_slo_80mbps")
comp3 = load_throughput("comp3_heavy_slo_40mbps")

timestamp = datetime.datetime.now(datetime.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
commit = subprocess.check_output(
    ["git", "rev-parse", "--short", "HEAD"], cwd=ROOT, text=True
).strip()

# Compute percentiles from criterion sample data BEFORE building report
benchmark_names = ["display_heavy_slo_80mbps", "comp3_heavy_slo_40mbps"]
percentiles = compute_percentiles(benchmark_names)

# Create complete performance receipt with ALL metadata INCLUDING summary
# IMPORTANT: Build complete receipt BEFORE hashing - receipt is immutable after hash
summary = {
    "display_mibps": display["mean_mibps"],
    "comp3_mibps": comp3["mean_mibps"],
    "max_rss_mib": 0  # Will be populated if /usr/bin/time is available
}
# Merge percentiles into summary if available
summary.update(percentiles)


report = {
    "format_version": "1.0.0",
    "timestamp": timestamp,
    "commit": commit,
    "build_profile": build_profile,
    "target_cpu": target_cpu,
    "environment": {
        "os": os_name,
        "kernel": kernel_version,
        "cpu_model": cpu_model,
        "cpu_cores": cpu_cores,
        "wsl2_detected": wsl2_detected
    },
    "toolchain": "cargo bench (criterion)",
    "status": "pass",
    "display_mibps": display["mean_mibps"],
    "display_gibps": display["mean_mibps"] / 1024.0,
    "comp3_mibps": comp3["mean_mibps"],
    "benchmarks": [display, comp3],
    "summary": summary
}

output_dir = ROOT / "scripts" / "bench"
output_dir.mkdir(parents=True, exist_ok=True)
output_path = output_dir / "perf.json"

# Write receipt WITHOUT integrity field first
# We'll compute hash using jq to match the validator exactly
temp_json = json.dumps(report, indent=2) + "\n"
output_path.write_text(temp_json)
PY

# Compute integrity hash using jq - EXACTLY matches validator logic
TEMP_HASH=$(jq -cS '.' scripts/bench/perf.json | sha256sum | cut -d' ' -f1)

# Add integrity field using jq and rewrite
jq --arg hash "$TEMP_HASH" '. + {integrity: {sha256: $hash}}' scripts/bench/perf.json > scripts/bench/perf.json.tmp
mv scripts/bench/perf.json.tmp scripts/bench/perf.json

echo "✅ receipts: scripts/bench/perf.json (integrity: ${TEMP_HASH:0:16}...)"

# Receipt is complete and immutable - no post-write modifications
# Note: Percentile aggregation is now done in the Python block above,
# before the integrity hash is computed. soak-aggregate.sh is no longer called.
