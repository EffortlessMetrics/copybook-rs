#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

# Determinism smoke test script
# -----------------------------
# This is the single source of truth for determinism smoke checks.
# CI (determinism-smoke.yml) and local runs both call this script.
#
# Exit codes:
#   0 = All determinism checks passed (deterministic)
#   2 = Determinism failure detected (non-deterministic)
#   3 = Error during validation (invalid data, schema issues, etc.)
#
# Usage (local):
#   scripts/ci/determinism_smoke.sh
#
# Usage (CI):
#   Called automatically by .github/workflows/determinism-smoke.yml

# Fixture paths (relative to repo root)
REPORT_DIR="target"
FAILURE_SUMMARY="${REPORT_DIR}/determinism-failure-details.txt"
DETERMINISM_CASE_TIMEOUT_SECONDS="${DETERMINISM_CASE_TIMEOUT_SECONDS:-45}"

mkdir -p "${REPORT_DIR}"
: >"${FAILURE_SUMMARY}"

echo "--------------------------------------------------"
echo "Determinism Smoke Tests (Issue #112)"
echo "--------------------------------------------------"
echo

print_report_summary() {
  local report_path="$1"
  local prefix="${2:-}"

  if [ ! -f "${report_path}" ]; then
    echo "${prefix}Expected determinism report was not created: ${report_path}"
    return 1
  fi

  if ! command -v python3 >/dev/null; then
    return 0
  fi

  python3 - "$report_path" "$prefix" <<'PY'
import json
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
prefix = sys.argv[2] if len(sys.argv) > 2 else ""
try:
    payload = json.loads(path.read_text())
except Exception:
    print(f"{prefix}Unable to parse JSON report")
    raise SystemExit(1)

required = {"mode", "is_deterministic", "round1_hash", "round2_hash"}
if not required.issubset(payload.keys()):
    missing = ", ".join(sorted(required - payload.keys()))
    print(f"{prefix}Invalid report payload; missing fields: {missing}")
    raise SystemExit(1)

mode = payload.get("mode")
deterministic = payload.get("is_deterministic")
if not deterministic:
    print(f"{prefix}Report indicates non-deterministic result for {mode}.")
    raise SystemExit(1)
round1_hash = payload.get("round1_hash")
round2_hash = payload.get("round2_hash")
if round1_hash != round2_hash:
    print(f"{prefix}Round-trip hashes differ: {round1_hash} != {round2_hash}")
    raise SystemExit(1)
diffs = payload.get("byte_differences") or []
diff_count = len(diffs)

print(f"{prefix}mode: {mode}")
print(f"{prefix}is_deterministic: {deterministic}")
print(f"{prefix}round1_hash: {round1_hash}")
print(f"{prefix}round2_hash: {round2_hash}")
print(f"{prefix}byte_differences: {diff_count}")
if diff_count > 0:
    print(f"{prefix}first_differences: {diffs[:5]}")
PY
}

emit_failure_details() {
  local label="$1"
  local mode="$2"
  local status="$3"
  local copybook="$4"
  local data="$5"
  local codepage="$6"
  local report_path="$7"
  local stderr_path="$8"
  local copybook_sha="$9"
  local data_sha="${10}"
  local command_str="${11}"
  local command_sha="${12}"

  echo "::error title=Determinism smoke failure::${mode}/${label} failed (exit ${status})"
  echo "Reproducibility details:"
  echo "  Label: ${label}"
  echo "  Mode: ${mode}"
  echo "  Codepage: ${codepage}"
  echo "  Copybook: ${copybook}"
  echo "  Data: ${data}"
  echo "  Copybook SHA-256: ${copybook_sha}"
  echo "  Data SHA-256: ${data_sha}"
  echo "  Command: ${command_str}"
  echo "  Command hash (sha256): ${command_sha}"
  echo "  Expected output report: ${report_path}"
  echo "  Repro command:"
  echo "    ${command_str}"
  echo
  echo "  Command stderr:"
  if [ -s "${stderr_path}" ]; then
    sed "s/^/    /" "${stderr_path}"
  else
    echo "    (empty)"
  fi
  echo
  if [ -s "${report_path}" ]; then
    echo "  Determinism result:"
    sed "s/^/    /" "${report_path}"
    print_report_summary "${report_path}" "    "
  else
    echo "  Determinism result: (no report captured)"
  fi

  {
    echo "FAILURE: ${label}/${mode} (exit ${status})"
    echo "  Copybook: ${copybook}"
    echo "  Copybook SHA-256: ${copybook_sha}"
    echo "  Data: ${data}"
    echo "  Data SHA-256: ${data_sha}"
    echo "  Codepage: ${codepage}"
    echo "  Command: ${command_str}"
    echo "  Command hash (sha256): ${command_sha}"
    echo
    if [ -s "${stderr_path}" ]; then
      echo "  stderr:"
      sed 's/^/    /' "${stderr_path}"
      echo
    fi
    if [ -s "${report_path}" ]; then
      echo "  report:"
      sed 's/^/    /' "${report_path}"
    fi
    echo "----"
  } >> "${FAILURE_SUMMARY}"
}

run_case() {
  local index="$1"
  local total="$2"
  local mode="$3"
  local label="$4"
  local copybook="$5"
  local data="$6"
  local codepage="$7"

  local safe_label
  local output_path
  local stderr_path
  local command
  local status
  local copybook_sha
  local data_sha
  local command_sha

  safe_label="$(printf '%s' "${label}" | tr '[:upper:]' '[:lower:]' | tr -c 'a-z0-9._-' '-')"
  output_path="${REPORT_DIR}/determinism-${mode}-${safe_label}.json"
  stderr_path="${REPORT_DIR}/.determinism-${mode}-${safe_label}.stderr"
  command=(./target/release/copybook determinism "${mode}" --format fixed --codepage "${codepage}" --output json "${copybook}" "${data}")
  copybook_sha="$(sha256sum "${copybook}" | awk '{print $1}')"
  data_sha="$(sha256sum "${data}" | awk '{print $1}')"

  command_str="${command[*]}"
  command_sha="$(printf '%s' "${command_str}" | sha256sum | awk '{print $1}')"

  echo
  echo "[${index}/${total}] Running determinism smoke: ${mode} / ${label}"
  set +e
  timeout "${DETERMINISM_CASE_TIMEOUT_SECONDS}" "${command[@]}" > "${output_path}" 2> "${stderr_path}"
  status=$?
  set -e

  if [ "${status}" -eq 0 ]; then
    if ! print_report_summary "${output_path}" "  "; then
      emit_failure_details \
        "${label}" \
        "${mode}" \
        3 \
        "${copybook}" \
        "${data}" \
        "${codepage}" \
        "${output_path}" \
        "${stderr_path}" \
        "${copybook_sha}" \
        "${data_sha}" \
        "${command_str}" \
        "${command_sha}"
      return 3
    fi

    echo "  PASS: ${label} (${mode})"
    return 0
  fi

  emit_failure_details \
    "${label}" \
    "${mode}" \
    "${status}" \
    "${copybook}" \
    "${data}" \
    "${codepage}" \
    "${output_path}" \
    "${stderr_path}" \
    "${copybook_sha}" \
    "${data_sha}" \
    "${command_str}" \
    "${command_sha}"

  return "${status}"
}

echo "[1/4] Building copybook CLI..."
cargo build --package copybook-cli --bin copybook --release --quiet
echo "      PASS: build complete"

TOTAL_CASES=4

run_case 1 "${TOTAL_CASES}" decode "simple-display" "fixtures/copybooks/simple.cpy" "fixtures/data/simple.bin" cp037
run_case 2 "${TOTAL_CASES}" decode "comp3-heavy" "fixtures/copybooks/comp3_test.cpy" "fixtures/data/comp3_test.bin" cp037
run_case 3 "${TOTAL_CASES}" decode "dialect-odo-normative" "test-data/dialect_test.cpy" "test-data/dialect_normative.bin" ascii
run_case 4 "${TOTAL_CASES}" round-trip "simple-round-trip" "fixtures/copybooks/simple.cpy" "fixtures/data/simple.bin" cp037

echo
echo "--------------------------------------------------"
echo "All determinism smoke tests passed"
echo "Reports:"
ls -1 "${REPORT_DIR}"/determinism-*.json 2>/dev/null || true
echo "--------------------------------------------------"
