#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

# Run only the SLO validation benchmarks to capture throughput receipts by default.
# tripwire: no-op change to trigger perf workflow without affecting behavior.
# Allow callers to widen scope by exporting BENCH_FILTER.
BENCH_FILTER="${BENCH_FILTER:-slo_validation}"
RUSTFLAGS="-C target-cpu=native" PERF=1 \
  cargo bench -p copybook-bench -- "${BENCH_FILTER}" --quiet

python3 <<'PY'
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

display = load_throughput("display_heavy_slo_80mbps")
comp3 = load_throughput("comp3_heavy_slo_40mbps")

timestamp = datetime.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"
commit = subprocess.check_output(
    ["git", "rev-parse", "--short", "HEAD"], cwd=ROOT, text=True
).strip()

report = {
    "timestamp": timestamp,
    "commit": commit,
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
output_path.write_text(json.dumps(report, indent=2) + "\n")
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
