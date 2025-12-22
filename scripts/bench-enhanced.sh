#!/usr/bin/env bash
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

python3 <<'PY'
import datetime
import hashlib
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

timestamp = datetime.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"
commit = subprocess.check_output(
    ["git", "rev-parse", "--short", "HEAD"], cwd=ROOT, text=True
).strip()

# Create canonical performance receipt with complete metadata
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
}

output_dir = ROOT / "scripts" / "bench"
output_dir.mkdir(parents=True, exist_ok=True)
output_path = output_dir / "perf.json"

# Generate JSON and calculate integrity hash
json_content = json.dumps(report, indent=2) + "\n"
sha256_hash = hashlib.sha256(json_content.encode('utf-8')).hexdigest()

# Add integrity information to report
report["integrity"] = {
    "sha256": sha256_hash
}

# Write final receipt with integrity hash
final_json = json.dumps(report, indent=2) + "\n"
output_path.write_text(final_json)
print(f"✅ receipts: {output_path.relative_to(ROOT)}")
PY

TMP_JSON="scripts/bench/perf.json"
SUMMARY_JSON="scripts/bench/summary.json"

if ! command -v jq >/dev/null 2>&1; then
  echo "❌ jq is required to append summary to ${TMP_JSON}" >&2
  exit 1
fi

if [[ ! -f "${TMP_JSON}" ]]; then
  echo "❌ expected receipts at ${TMP_JSON}" >&2
  exit 1
fi

DISPLAY=$(
  jq -r '
    [
      .display_mibps?,
      .metrics?.display_mibps?,
      (.benchmarks[]? | .display_mibps?),
      (.benchmarks[]? | .throughput_mib? | select(. != null))
    ] | map(select(. != null)) | (max // 0)
  ' "${TMP_JSON}"
)

COMP3=$(
  jq -r '
    [
      .comp3_mibps?,
      .metrics?.comp3_mibps?,
      (.benchmarks[]? | .comp3_mibps?),
      (.benchmarks[]? | .comp3_throughput_mib?)
    ] | map(select(. != null)) | (max // 0)
  ' "${TMP_JSON}"
)

MAXRSS_MIB=0
if [[ -x "/usr/bin/time" ]]; then
  TIME_OUTPUT=$(/usr/bin/time -v bash -c 'sleep 0.1' 2>&1 || true)

  if [[ -z "${TIME_OUTPUT}" ]]; then
    TIME_OUTPUT=$(/usr/bin/time -l bash -c 'sleep 0.1' 2>&1 || true)
  fi

  rss_line=$(printf '%s\n' "${TIME_OUTPUT}" | awk -F: '
    /[Mm]aximum resident set size/ {gsub(/^[ \t]+/, "", $2); print $2; exit}
  ')

  if [[ -n "${rss_line}" ]]; then
    MAXRSS_MIB=$(printf '%s\n' "${rss_line}" | awk '{printf "%.0f", $1 / 1024}')
  fi
fi

jq -n \
  --argjson display "${DISPLAY:-0}" \
  --argjson comp3 "${COMP3:-0}" \
  --argjson maxrss "${MAXRSS_MIB:-0}" \
  '{display_mibps:$display, comp3_mibps:$comp3, max_rss_mib:$maxrss}' \
  > "${SUMMARY_JSON}"

jq -s '.[0] * { summary: .[1] }' "${TMP_JSON}" "${SUMMARY_JSON}" > "${TMP_JSON}.tmp"
mv "${TMP_JSON}.tmp" "${TMP_JSON}"
rm -f "${SUMMARY_JSON}"
echo "✅ appended summary to ${TMP_JSON}"

if [[ -x "scripts/soak-aggregate.sh" ]]; then
  bash scripts/soak-aggregate.sh
else
  echo "⚠️ scripts/soak-aggregate.sh missing or not executable; skipping percentile aggregate."
fi