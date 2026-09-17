#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: release_smoke.sh <version>" >&2
  echo "example: release_smoke.sh v0.3.2" >&2
  exit 1
fi

VERSION="${1#v}"
if [ -z "${VERSION}" ]; then
  echo "invalid version: '${1}'" >&2
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
# Receipt metadata (python) reads these from the environment.
export VERSION REPO_ROOT

FIXTURE_COPYBOOK="${REPO_ROOT}/fixtures/copybooks/simple.cpy"
FIXTURE_FIXED="${REPO_ROOT}/fixtures/data/simple.bin"

if [ ! -f "${FIXTURE_COPYBOOK}" ] || [ ! -f "${FIXTURE_FIXED}" ]; then
  echo "smoke fixtures missing in repository root" >&2
  exit 1
fi

PYTHON_BIN="${RELEASE_SMOKE_PYTHON:-python3}"
if ! command -v "${PYTHON_BIN}" >/dev/null 2>&1; then
  if [ -n "${RELEASE_SMOKE_PYTHON:-}" ]; then
    echo "RELEASE_SMOKE_PYTHON set to '${PYTHON_BIN}', but command was not found." >&2
    exit 1
  fi

  if command -v python >/dev/null 2>&1; then
    PYTHON_BIN="python"
  else
    echo "python or python3 is required to generate the RDW fixture." >&2
    exit 1
  fi
fi

mktemp_dir() {
  local dir
  if dir="$(mktemp -d -t copybook-release-smoke-XXXXXX 2>/dev/null)"; then
    printf '%s\n' "$dir"
    return 0
  fi
  mktemp -d "/tmp/copybook-release-smoke-XXXXXX"
}

readlink_f() {
  local path="$1"
  if command -v realpath >/dev/null 2>&1; then
    realpath -- "$path"
    return 0
  fi
  if command -v readlink >/dev/null 2>&1; then
    local resolved
    resolved="$(readlink -f -- "$path" 2>/dev/null || true)"
    if [ -n "$resolved" ]; then
      printf '%s\n' "$resolved"
      return 0
    fi
  fi
  "${PYTHON_BIN}" - "$path" <<'PY'
import os
import sys

print(os.path.realpath(sys.argv[1]))
PY
}

compare_bytes() {
  local expected="$1"
  local actual="$2"
  if ! cmp -- "${expected}" "${actual}" >/dev/null; then
    echo "byte comparison failed for: ${expected} != ${actual}" >&2
    exit 1
  fi
  if [ -n "${RESULTS_FILE:-}" ]; then
    printf 'compare ok: %s == %s\n' "${expected}" "${actual}" >> "${RESULTS_FILE}"
  fi
}

sha256_file() {
  sha256sum -- "$1" | cut -d' ' -f1
}

# timed_step <name> -- <command...>: run, record wall time and rc, propagate rc.
timed_step() {
  local name="$1"
  shift
  if [ "${1:-}" = "--" ]; then shift; fi
  local start end rc=0
  start="$(date -u +%s)"
  "$@" || rc="$?"
  end="$(date -u +%s)"
  printf '%s start=%s end=%s duration_s=%s rc=%s\n' \
    "${name}" "${start}" "${end}" "$((end - start))" "${rc}" >> "${TIMINGS_FILE}"
  return "${rc}"
}

# expect_exit <code> -- <command...>: the probe must exit with exactly <code>.
# Negative and malformed cases use this so a silently succeeding check fails
# loudly instead of passing vacuously.
expect_exit() {
  local expected="$1"
  shift
  if [ "${1:-}" = "--" ]; then shift; fi
  local rc=0
  "$@" || rc="$?"
  if [ "${rc}" -ne "${expected}" ]; then
    echo "expected exit ${expected}, got ${rc}: $*" >&2
    exit 1
  fi
  printf 'probe ok: [%s] exited %s\n' "$*" "${rc}" >> "${RESULTS_FILE}"
}

# advise envelope validation (stdlib only: the smoke runner has no extra
# python dependencies). Checks the selected-schema contract structurally:
# required keys, schema_version literal, verdict drawn from the known enum.
# Full JSON-Schema validation lives in unit/integration tests.
validate_advise_envelope() {
  local file="$1"
  "${PYTHON_BIN}" - "${file}" <<'PY'
import json
import sys

value = json.load(open(sys.argv[1], encoding="utf-8"))
required = {"schema_version", "stability_class", "verdict", "effective_options",
            "scenarios", "tool_version"}
missing = required - set(value)
if missing:
    sys.exit(f"advise envelope missing keys: {sorted(missing)}")
if value["schema_version"] != "1.0":
    sys.exit(f"unexpected schema_version: {value['schema_version']!r}")
known_verdicts = {"supported", "supported-with-limits", "beta",
                  "partial-unknown", "rejected", "invalid-input", "tool-failure"}
if value["verdict"] not in known_verdicts:
    sys.exit(f"unknown verdict literal: {value['verdict']!r}")
if not isinstance(value["scenarios"], list):
    sys.exit("scenarios must be an array")
PY
}

# assert_json <file> <dotted.path> <expected>: exact scalar equality.
# refute_json <file> <dotted.path> <wrong>: the deliberately wrong value must
# NOT appear (proves the probe fails on wrong expectations, #988).
json_assert() {
  local mode="$1" file="$2" path="$3" expected="$4"
  "${PYTHON_BIN}" - "${file}" "${path}" "${expected}" "${mode}" <<'PY'
import json
import sys

path = sys.argv[2].split(".")
expected, mode = sys.argv[3], sys.argv[4]
value = json.load(open(sys.argv[1], encoding="utf-8"))
node = value
for part in path:
    if isinstance(node, dict) and part in node:
        node = node[part]
    else:
        sys.exit(f"path {sys.argv[2]!r} not found")
seen = node if isinstance(node, str) else json.dumps(node, sort_keys=True)
if mode == "assert" and seen != expected:
    sys.exit(f"{sys.argv[2]}: want {expected!r}, got {seen!r}")
if mode == "refute" and seen == expected:
    sys.exit(f"{sys.argv[2]}: wrong value {expected!r} unexpectedly present")
PY
}

RUN_DIR="$(mktemp_dir)"
FIXTURE_DIR="${RUN_DIR}/fixtures"
PROJECT_DIR="${RUN_DIR}/copybook-smoke"
mkdir -p "${FIXTURE_DIR}" "${PROJECT_DIR}/src"
TIMINGS_FILE="${RUN_DIR}/step-timings.log"
RESULTS_FILE="${RUN_DIR}/probe-results.log"
: > "${TIMINGS_FILE}"
: > "${RESULTS_FILE}"
SMOKE_START_EPOCH="$(date -u +%s)"

# The receipt defaults inside the run directory for local runs. CI release
# acceptance sets RECEIPT_OUT to a workspace-persisted path so the record
# survives run-directory cleanup and Actions retention (#988).
RECEIPT_OUT="${RECEIPT_OUT:-${RUN_DIR}/acceptance-receipt.json}"
RECEIPT_EMITTED=0
export RUN_DIR RECEIPT_OUT SMOKE_START_EPOCH TIMINGS_FILE RESULTS_FILE SMOKE_MODE

on_exit() {
  local rc="$?"
  if [ "${rc}" -ne 0 ] && [ "${RECEIPT_EMITTED}" != "1" ]; then
    emit_acceptance_receipt "failed" || true
  fi
  rm -rf "${RUN_DIR}"
  exit "${rc}"
}

trap 'on_exit' EXIT

install_copybook_cli() {
  local features="$1"
  local target_root="$2"

  local feature_args=()
  if [ -n "${features}" ]; then
    feature_args+=(--features "${features}")
  fi

  cargo install "copybook-cli@${VERSION}" --locked --root "${target_root}" "${feature_args[@]}"
}

make_rdw_fixture() {
  local input="$1"
  local output="$2"

  "${PYTHON_BIN}" - "$input" "$output" <<'PY'
import pathlib
import struct
import sys

input_path, output_path = sys.argv[1:3]
payload = pathlib.Path(input_path).read_bytes()
# copybook-cli expects RDW payload length in the first two bytes
rdw = struct.pack(">H", len(payload)) + b"\x00\x00" + payload
pathlib.Path(output_path).write_bytes(rdw)
PY
}

run_with_binary() {
  local copybook_cli="$1"
  local mode="$2"
  local copybook="$3"
  local data_file="$4"
  local format="$5"
  local output_dir="$6"
  local threads="$7"

  local decode_out="${output_dir}/decode.jsonl"
  local encode_out="${output_dir}/encode.bin"
  local verify_out="${output_dir}/verify.json"
  local determinism_out="${output_dir}/determinism.json"
  local roundtrip_out="${output_dir}/roundtrip.jsonl"

  mkdir -p "${output_dir}"

  "${copybook_cli}" decode "${copybook}" "${data_file}" \
    --format "${format}" \
    --codepage cp037 \
    --threads "${threads}" \
    --output "${decode_out}"

  "${copybook_cli}" encode "${copybook}" "${decode_out}" \
    --format "${format}" \
    --codepage cp037 \
    --threads "${threads}" \
    --output "${encode_out}"

  "${copybook_cli}" verify "${copybook}" "${encode_out}" \
    --format "${format}" \
    --codepage cp037 \
    --report "${verify_out}"

  "${copybook_cli}" determinism round-trip "${copybook}" "${data_file}" \
    --format "${format}" \
    --codepage cp037 \
    --output json \
    > "${determinism_out}"

  "${copybook_cli}" decode "${copybook}" "${encode_out}" \
    --format "${format}" \
    --codepage cp037 \
    --output "${roundtrip_out}"

  if [ "${mode}" = "fixed" ]; then
    compare_bytes "${data_file}" "${encode_out}"
    compare_bytes "${decode_out}" "${roundtrip_out}"
  fi

  if [ "${format}" = "rdw" ]; then
    "${copybook_cli}" verify "${copybook}" "${data_file}" \
      --format "${format}" \
      --codepage cp037 \
      --report "${output_dir}/verify-input.json"
  fi
}

codepage_correction_witness() {
  local copybook_cli="$1"
  local dir="$2"
  mkdir -p "${dir}"
  printf '01 SIG-REC.\n   05 SIG PIC X(1).\n' > "${dir}/sig.cpy"

  COPYCASE_BIN="${copybook_cli}" COPYCASE_DIR="${dir}" "${PYTHON_BIN}" - <<'PY'
import json
import os
import pathlib
import subprocess
import sys

cli = os.environ["COPYCASE_BIN"]
work = pathlib.Path(os.environ["COPYCASE_DIR"])
cpy = str(work / "sig.cpy")

# Independently specified IBM-contract pairs (#998/#999): the expected
# characters and bytes are hardcoded from the IBM references, never derived
# from the binary under test, so a mutually consistent but wrong table fails.
cases = [
    ("cp1140", 0x9F, "€"),
    ("cp273", 0x59, "~"),
    ("cp273", 0xA1, "ß"),
    ("cp1047", 0x5F, "^"),
    ("cp1047", 0xB0, "¬"),
]

for codepage, byte, char in cases:
    data = work / f"in-{codepage}-{byte:02x}.bin"
    data.write_bytes(bytes([byte]))
    decoded = work / f"out-{codepage}-{byte:02x}.jsonl"
    subprocess.run(
        [cli, "decode", cpy, str(data), "--format", "fixed",
         "--codepage", codepage, "--output", str(decoded)],
        check=True,
    )
    line = decoded.read_text(encoding="utf-8").splitlines()[0]
    seen = json.loads(line).get("SIG")
    if seen != char:
        print(f"decode witness failed: {codepage} 0x{byte:02X} -> {seen!r}, want {char!r}")
        sys.exit(1)

    source = work / f"back-{codepage}-{byte:02x}.jsonl"
    source.write_text(json.dumps({"SIG": char}, ensure_ascii=False) + "\n", encoding="utf-8")
    encoded = work / f"back-{codepage}-{byte:02x}.bin"
    subprocess.run(
        [cli, "encode", cpy, str(source), "--format", "fixed",
         "--codepage", codepage, "--output", str(encoded)],
        check=True,
    )
    seen_bytes = encoded.read_bytes()
    if seen_bytes != bytes([byte]):
        print(f"encode witness failed: {codepage} {char!r} -> {seen_bytes.hex()}, want {byte:02x}")
        sys.exit(1)

print(f"codepage witness ok: {len(cases)} independent pairs in both directions")
PY
}

emit_smoke_manifest() {
  local manifest_path="$1"
  local mode="$2"

  if [ "${mode}" = "local" ]; then
    cat > "${manifest_path}" <<EOF
[package]
name = "copybook-smoke"
version = "0.1.0"
edition = "2021"

[dependencies]
copybook = { path = "${REPO_ROOT}/crates/copybook" }
copybook-rs = { path = "${REPO_ROOT}/crates/copybook-rs" }
EOF
    return
  fi

  cat > "${manifest_path}" <<EOF
[package]
name = "copybook-smoke"
version = "0.1.0"
edition = "2021"

[dependencies]
copybook = "=${VERSION}"
copybook-rs = "=${VERSION}"
EOF
}

# Equality definitions per framing policy (#988): fixed compares re-encoded
# bytes and re-decoded JSONL; RDW compares worker outputs byte-for-byte;
# VB compares decoded JSONL across encode/decode because the encoder emits
# single-record BDW blocks, so encoded bytes are framing, not payload.
VB_COPYBOOK_TEXT='       01 VB-REC PIC X(5).'

# Frame payloads as one BDW block: BDW len + (RDW len + payload) per record.
write_vb_fixtures() {
  local dir="$1"
  printf '%s\n' "${VB_COPYBOOK_TEXT}" > "${dir}/vb.cpy"
  "${PYTHON_BIN}" - "${dir}/vb_ok.bin" "${dir}/vb_truncated.bin" "${dir}/vb_badlen.bin" <<'PY'
import struct
import sys

def frame_block(payloads):
    records = b""
    for payload in payloads:
        records += struct.pack(">H", len(payload) + 4) + b"\x00\x00" + payload
    return struct.pack(">H", len(records) + 4) + b"\x00\x00" + records

ok, truncated, badlen = sys.argv[1:4]
good = frame_block([b"HELLO", b"WORLD"])
open(ok, "wb").write(good)
# Malformed framing: physical truncation mid-record.
open(truncated, "wb").write(good[:-3])
# Malformed framing: BDW length over-claims the block.
inflated = struct.pack(">H", len(good) + 40) + good[2:]
open(badlen, "wb").write(inflated)
PY
}

run_vb_probes() {
  local copybook_cli="$1"
  local dir="$2"
  mkdir -p "${dir}"
  write_vb_fixtures "${dir}"

  # Beta positive case: two framed records decode with checked values.
  # VB stays beta: this probe accepts current beta behavior, it does not
  # graduate the format.
  timed_step "vb-decode" -- "${copybook_cli}" decode "${dir}/vb.cpy" "${dir}/vb_ok.bin" \
    --format vb --codepage ascii --threads 1 \
    --output "${dir}/vb_decode.jsonl" > /dev/null
  "${PYTHON_BIN}" - "${dir}/vb_decode.jsonl" <<'PY'
import json
import sys

lines = open(sys.argv[1], encoding="utf-8").read().splitlines()
values = [json.loads(line).get("VB-REC") for line in lines]
if values != ["HELLO", "WORLD"]:
    sys.exit(f"vb values wrong: {values!r}")
PY
  printf 'probe ok: vb positive values [HELLO WORLD]\n' >> "${RESULTS_FILE}"

  # Malformed framing cases must fail with the record-format family exit
  # (CBKF=4), never decode silently.
  expect_exit 4 -- "${copybook_cli}" decode "${dir}/vb.cpy" "${dir}/vb_truncated.bin" \
    --format vb --codepage ascii --threads 1 \
    --output "${dir}/vb_truncated.jsonl" > /dev/null
  expect_exit 4 -- "${copybook_cli}" decode "${dir}/vb.cpy" "${dir}/vb_badlen.bin" \
    --format vb --codepage ascii --threads 1 \
    --output "${dir}/vb_badlen.jsonl" > /dev/null

  # Semantic round-trip: decode -> encode -> decode, JSONL compared because
  # encoded BDW framing is not byte-stable by design.
  timed_step "vb-roundtrip" -- "${copybook_cli}" encode "${dir}/vb.cpy" "${dir}/vb_decode.jsonl" \
    --format vb --codepage ascii --threads 1 \
    --output "${dir}/vb_reencode.bin" > /dev/null
  timed_step "vb-redecode" -- "${copybook_cli}" decode "${dir}/vb.cpy" "${dir}/vb_reencode.bin" \
    --format vb --codepage ascii --threads 1 \
    --output "${dir}/vb_roundtrip.jsonl" > /dev/null
  compare_bytes "${dir}/vb_decode.jsonl" "${dir}/vb_roundtrip.jsonl"
}

TAIL_ODO_COPYBOOK_TEXT='       01 TAIL.
           05 CNT PIC 9(2).
           05 DATA PIC X(4) OCCURS 0 TO 4 DEPENDING ON CNT.'
NONTAIL_ODO_COPYBOOK_TEXT='       01 NON-TAIL.
           05 A PIC X.
           05 TBL PIC X OCCURS 0 TO 3 DEPENDING ON N.
           05 B PIC X.
           05 N PIC 9(2).'

run_advise_probes() {
  local copybook_cli="$1"
  local dir="$2"
  mkdir -p "${dir}"
  printf '%s\n' "${TAIL_ODO_COPYBOOK_TEXT}" > "${dir}/tail_odo.cpy"
  printf '%s\n' "${NONTAIL_ODO_COPYBOOK_TEXT}" > "${dir}/nontail_odo.cpy"

  # Ordinary supported case: exit 0 with the honest supported literal.
  expect_exit 0 -- "${copybook_cli}" support --advise "${dir}/tail_odo.cpy" \
    > "${dir}/advise_table.txt"
  grep -q "Advisory verdict: supported" "${dir}/advise_table.txt" \
    || { echo "advise table missing supported verdict" >&2; exit 1; }
  printf 'probe ok: advise table verdict supported\n' >> "${RESULTS_FILE}"

  # Stable advise JSON validates against the selected-schema envelope and
  # carries the evaluated format literally.
  expect_exit 0 -- "${copybook_cli}" support --advise "${dir}/tail_odo.cpy" \
    --format json > "${dir}/advise_fixed.json"
  validate_advise_envelope "${dir}/advise_fixed.json"
  json_assert assert "${dir}/advise_fixed.json" "verdict" "supported"
  json_assert assert "${dir}/advise_fixed.json" "effective_options.format" "fixed"
  json_assert refute "${dir}/advise_fixed.json" "verdict" "partial-unknown"

  # VB flows into effective options literally.
  expect_exit 0 -- "${copybook_cli}" support --advise "${dir}/tail_odo.cpy" \
    --record-format vb --format json > "${dir}/advise_vb.json"
  validate_advise_envelope "${dir}/advise_vb.json"
  json_assert assert "${dir}/advise_vb.json" "effective_options.format" "vb"

  # Invalid input: exit 3 with the actionable parse identity, schema-valid.
  expect_exit 3 -- "${copybook_cli}" support --advise "${dir}/nontail_odo.cpy" \
    --format json > "${dir}/advise_invalid.json"
  validate_advise_envelope "${dir}/advise_invalid.json"
  json_assert assert "${dir}/advise_invalid.json" "verdict" "invalid-input"
  json_assert refute "${dir}/advise_invalid.json" "verdict" "supported"
  grep -q "CBKP021_ODO_NOT_TAIL" "${dir}/advise_invalid.json" \
    || { echo "advise invalid-input missing CBKP021 identity" >&2; exit 1; }
  printf 'probe ok: advise invalid-input carries CBKP021 identity\n' >> "${RESULTS_FILE}"

  # Governed corpus agreement: the checked-in rejection fixture reports identically.
  expect_exit 3 -- "${copybook_cli}" support --advise "${REPO_ROOT}/fixtures/corpus/nontail_odo.cpy" \
    --format json > "${dir}/advise_corpus.json"
  validate_advise_envelope "${dir}/advise_corpus.json"
  json_assert assert "${dir}/advise_corpus.json" "verdict" "invalid-input"

  # Ordinary mixed-construct case: honest non-supported literal, never inflated.
  expect_exit 3 -- "${copybook_cli}" support --advise "${FIXTURE_COPYBOOK}" \
    --format json > "${dir}/advise_mixed.json"
  validate_advise_envelope "${dir}/advise_mixed.json"
  json_assert assert "${dir}/advise_mixed.json" "verdict" "partial-unknown"
  json_assert refute "${dir}/advise_mixed.json" "verdict" "supported"
}

# Bounded, sanitized acceptance receipt (#988): hashes and outcomes only,
# never record payloads or copybook excerpts. Private record data must never
# enter public receipts.
emit_acceptance_receipt() {
  local status="$1"
  RECEIPT_EMITTED=1
  SMOKE_END_EPOCH="$(date -u +%s)"
  local exe_version exe_sha plan_digest rustc_v
  exe_version="$("${COPYBOOK_CLI_BIN:-false}" --version 2>/dev/null || echo unknown)"
  exe_sha="$([ -n "${COPYBOOK_CLI_BIN:-}" ] && sha256_file "${COPYBOOK_CLI_BIN}" || echo unknown)"
  rustc_v="$(rustc -vV 2>/dev/null | tr '\n' ';' || echo unknown)"
  plan_digest="not-computed"
  if [ "${SMOKE_MODE:-registry}" = "local" ] || [ -n "${COPYBOOK_CLI_BIN:-}" ]; then
    plan_digest="not-applicable (workspace-assisted execution)"
  elif (cargo run -q -p xtask -- publish plan --format json > "${RUN_DIR}/plan.json" 2>/dev/null); then
    plan_digest="$(sha256_file "${RUN_DIR}/plan.json")"
  fi
  RECEIPT_STATUS="${status}" RECEIPT_END="${SMOKE_END_EPOCH}" \
    RECEIPT_EXE_VERSION="${exe_version}" RECEIPT_EXE_SHA="${exe_sha}" \
    RECEIPT_RUSTC="${rustc_v}" RECEIPT_PLAN="${plan_digest}" \
    "${PYTHON_BIN}" - "${RECEIPT_OUT}" <<'PY'
import hashlib
import json
import os

run_dir = os.environ["RUN_DIR"]
repo = os.environ["REPO_ROOT"]

def sha(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(65536), b""):
            digest.update(chunk)
    return digest.hexdigest()

def lines(path):
    try:
        with open(path, encoding="utf-8") as handle:
            return [line.rstrip("\n") for line in handle]
    except OSError:
        return []

def durable(line):
    # Ephemeral absolute paths must not enter the durable receipt: the run
    # directory is deleted on exit and operator paths are environment noise.
    return line.replace(run_dir, "$RUN_DIR").replace(repo, "$REPO")

status = os.environ["RECEIPT_STATUS"]
receipt = {
    "schema": "copybook-acceptance-receipt/1",
    "issue": "#988 slice A (runner acceptance)",
    "status": status,
    "completed": status == "completed",
    "release": {
        "tag": os.environ.get("RELEASE_TAG", "v" + os.environ.get("VERSION", "unknown")),
        "version": os.environ.get("VERSION", "unknown"),
    },
    "mode": os.environ.get("SMOKE_MODE", "registry"),
    "execution_kind": ("workspace-assisted (operator COPYBOOK_CLI_BIN)"
                       if os.environ.get("COPYBOOK_CLI_BIN") else "registry-only"),
    "toolchain": os.environ["RECEIPT_RUSTC"],
    "executable": {
        "path": durable(os.environ.get("COPYBOOK_CLI_BIN", "registry-installed")),
        "version": os.environ["RECEIPT_EXE_VERSION"],
        "sha256": os.environ["RECEIPT_EXE_SHA"],
    },
    "publish_plan_sha256": os.environ["RECEIPT_PLAN"],
    "fixtures": {
        "simple.cpy": sha(os.path.join(repo, "fixtures/copybooks/simple.cpy")),
        "simple.bin": sha(os.path.join(repo, "fixtures/data/simple.bin")),
        "corpus/nontail_odo.cpy": sha(os.path.join(repo, "fixtures/corpus/nontail_odo.cpy")),
    },
    "comparisons": [durable(line) for line in lines(os.path.join(run_dir, "probe-results.log"))
                    if line.startswith("compare ok:")],
    "probes": [durable(line) for line in lines(os.path.join(run_dir, "probe-results.log"))
               if line.startswith("probe ok:")],
    "step_timings": lines(os.path.join(run_dir, "step-timings.log")),
    "timing": {
        "start_epoch": int(os.environ.get("SMOKE_START_EPOCH", "0")),
        "end_epoch": int(os.environ["RECEIPT_END"]),
    },
    "equality_policy": {
        "fixed": "re-encoded bytes and re-decoded JSONL must match",
        "rdw": "worker outputs must match byte-for-byte",
        "vb": "decoded JSONL must match across encode/decode (BDW framing excluded)",
    },
    "notes": [
        "VB probes accept current beta behavior; they do not graduate the format.",
        "Outputs are recorded by hash/reference only; no record payload in receipt.",
    ],
}
with open(os.environ["RECEIPT_OUT"], "w", encoding="utf-8") as handle:
    json.dump(receipt, handle, indent=2, sort_keys=True)
    handle.write("\n")
PY
  printf 'acceptance receipt (%s): %s\n' "${status}" "${RECEIPT_OUT}"
}

echo "=== Release smoke: version ${VERSION} ==="
SMOKE_MODE="${RELEASE_SMOKE_DEPS:-registry}"

if [ "${SMOKE_MODE}" != "local" ] && [ "${SMOKE_MODE}" != "registry" ]; then
  echo "RELEASE_SMOKE_DEPS must be either 'registry' (default) or 'local'." >&2
  exit 1
fi

if [ "${RELEASE_SMOKE_ADVISORY:-0}" = "1" ]; then
  # Advisory experimental-adapter smoke only. Arrow/Parquet and the
  # enterprise audit adapter are in the experimental adapter track and are
  # not part of the stable-core promise, so this mode is run from a
  # non-blocking workflow job.
  if [ "${SMOKE_MODE}" = "local" ]; then
    echo "Advisory adapter smoke requires registry mode (RELEASE_SMOKE_DEPS=registry)." >&2
    exit 1
  fi

  INSTALL_ARROW="${RUN_DIR}/copybook-arrow"
  echo "Installing copybook-cli@${VERSION} (arrow feature, advisory)"
  install_copybook_cli "arrow" "${INSTALL_ARROW}"
  "${INSTALL_ARROW}/bin/copybook" --version

  INSTALL_AUDIT="${RUN_DIR}/copybook-audit"
  echo "Installing copybook-cli@${VERSION} (audit feature, advisory)"
  install_copybook_cli "audit" "${INSTALL_AUDIT}"
  "${INSTALL_AUDIT}/bin/copybook" --version
  "${INSTALL_AUDIT}/bin/copybook" audit --help >/dev/null

  echo "Advisory experimental-adapter smoke completed successfully."
  exit 0
fi

if [ -n "${COPYBOOK_CLI_BIN:-}" ]; then
  COPYBOOK_CLI_BIN="$(readlink_f "${COPYBOOK_CLI_BIN}")"
  if [ ! -x "${COPYBOOK_CLI_BIN}" ]; then
    echo "COPYBOOK_CLI_BIN is set but not executable: ${COPYBOOK_CLI_BIN}" >&2
    exit 1
  fi
  echo "Using local copybook CLI: ${COPYBOOK_CLI_BIN}"
else
  INSTALL_DEFAULT="${RUN_DIR}/copybook-default"

  echo "Installing copybook-cli@${VERSION} (default features)"
  install_copybook_cli "" "${INSTALL_DEFAULT}"
  COPYBOOK_CLI_BIN="${INSTALL_DEFAULT}/bin/copybook"
fi
export COPYBOOK_CLI_BIN

"${COPYBOOK_CLI_BIN}" --version
"${COPYBOOK_CLI_BIN}" --help >/dev/null

emit_smoke_manifest "${PROJECT_DIR}/Cargo.toml" "${SMOKE_MODE}"

# The clean-room project must exercise the facade, not merely resolve it:
# parse a small copybook through copybook::core, decode/encode through
# copybook::codec, repeat through the copybook-rs redirect surface, and prove
# the redirect produces byte-identical behavior.
cat > "${PROJECT_DIR}/src/main.rs" <<'EOF'
use copybook::codec::{DecodeOptions, EncodeOptions};
use copybook::core::parse_copybook;

const COPYBOOK: &str = "       01  SMOKE-RECORD.\n           05  SMOKE-ID     PIC 9(5).\n           05  SMOKE-NAME   PIC X(5).\n";
// CP037 bytes for "12345" followed by "AB   ".
const RECORD: [u8; 10] = [0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xC1, 0xC2, 0x40, 0x40, 0x40];

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let schema = parse_copybook(COPYBOOK)?;
    let decoded = copybook::codec::decode_record(&schema, &RECORD, &DecodeOptions::default())?;
    let encoded = copybook::codec::encode_record(&schema, &decoded, &EncodeOptions::default())?;
    assert_eq!(encoded, RECORD, "copybook facade round-trip diverged");

    let rs_schema = copybook_rs::core::parse_copybook(COPYBOOK)?;
    let rs_decoded =
        copybook_rs::codec::decode_record(&rs_schema, &RECORD, &copybook_rs::codec::DecodeOptions::default())?;
    let rs_encoded =
        copybook_rs::codec::encode_record(&rs_schema, &rs_decoded, &copybook_rs::codec::EncodeOptions::default())?;
    assert_eq!(rs_encoded, encoded, "copybook-rs redirect diverged from copybook facade");

    println!(
        "facade smoke ok: {} bytes round-tripped identically via copybook and copybook-rs",
        encoded.len()
    );
    Ok(())
}
EOF

echo "Building and running clean-room facade smoke project"
timed_step "facade-smoke" -- cargo run --manifest-path "${PROJECT_DIR}/Cargo.toml"

echo "Running smoke fixed workflow (single worker)"
timed_step "fixed-t1" -- run_with_binary "${COPYBOOK_CLI_BIN}" fixed "${FIXTURE_COPYBOOK}" "${FIXTURE_FIXED}" fixed "${FIXTURE_DIR}/fixed/t1" 1

echo "Running smoke fixed workflow (multi-worker)"
timed_step "fixed-t4" -- run_with_binary "${COPYBOOK_CLI_BIN}" fixed "${FIXTURE_COPYBOOK}" "${FIXTURE_FIXED}" fixed "${FIXTURE_DIR}/fixed/t4" 4

echo "Comparing fixed output across worker settings"
compare_bytes "${FIXTURE_DIR}/fixed/t1/decode.jsonl" "${FIXTURE_DIR}/fixed/t4/decode.jsonl"
compare_bytes "${FIXTURE_DIR}/fixed/t1/encode.bin" "${FIXTURE_DIR}/fixed/t4/encode.bin"

RDW_FIXTURE="${FIXTURE_DIR}/simple.rdw.bin"
make_rdw_fixture "${FIXTURE_FIXED}" "${RDW_FIXTURE}"

echo "Running smoke RDW workflow (single worker)"
timed_step "rdw-t1" -- run_with_binary "${COPYBOOK_CLI_BIN}" rdw "${FIXTURE_COPYBOOK}" "${RDW_FIXTURE}" rdw "${FIXTURE_DIR}/rdw/t1" 1

echo "Running smoke RDW workflow (multi-worker)"
timed_step "rdw-t4" -- run_with_binary "${COPYBOOK_CLI_BIN}" rdw "${FIXTURE_COPYBOOK}" "${RDW_FIXTURE}" rdw "${FIXTURE_DIR}/rdw/t4" 4

echo "Comparing RDW output across worker settings"
compare_bytes "${FIXTURE_DIR}/rdw/t1/decode.jsonl" "${FIXTURE_DIR}/rdw/t4/decode.jsonl"
compare_bytes "${FIXTURE_DIR}/rdw/t1/encode.bin" "${FIXTURE_DIR}/rdw/t4/encode.bin"

echo "Running codepage-correction witness (installed binary, independent pairs)"
timed_step "codepage-witness" -- codepage_correction_witness "${COPYBOOK_CLI_BIN}" "${FIXTURE_DIR}/codepage"

echo "Running VB framing probes (beta positive plus malformed cases)"
timed_step "vb-probes" -- run_vb_probes "${COPYBOOK_CLI_BIN}" "${FIXTURE_DIR}/vb"

echo "Running advise probes (stable JSON, schema, identities, honest literals)"
timed_step "advise-probes" -- run_advise_probes "${COPYBOOK_CLI_BIN}" "${FIXTURE_DIR}/advise"

emit_acceptance_receipt "completed"

echo "Release smoke completed successfully."
