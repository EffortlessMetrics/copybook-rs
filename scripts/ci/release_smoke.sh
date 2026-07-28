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
}

RUN_DIR="$(mktemp_dir)"
FIXTURE_DIR="${RUN_DIR}/fixtures"
PROJECT_DIR="${RUN_DIR}/copybook-smoke"
mkdir -p "${FIXTURE_DIR}" "${PROJECT_DIR}/src"

trap 'rm -rf "${RUN_DIR}"' EXIT

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

echo "=== Release smoke: version ${VERSION} ==="
SMOKE_MODE="${RELEASE_SMOKE_DEPS:-registry}"

if [ "${SMOKE_MODE}" != "local" ] && [ "${SMOKE_MODE}" != "registry" ]; then
  echo "RELEASE_SMOKE_DEPS must be either 'registry' (default) or 'local'." >&2
  exit 1
fi

if [ "${RELEASE_SMOKE_ADVISORY:-0}" = "1" ]; then
  # Advisory experimental-adapter smoke only. Arrow/Parquet is in the
  # experimental adapter track and is not part of the stable-core promise,
  # so this mode is run from a non-blocking workflow job.
  if [ "${SMOKE_MODE}" = "local" ]; then
    echo "Advisory Arrow smoke requires registry mode (RELEASE_SMOKE_DEPS=registry)." >&2
    exit 1
  fi

  INSTALL_ARROW="${RUN_DIR}/copybook-arrow"
  echo "Installing copybook-cli@${VERSION} (arrow feature, advisory)"
  install_copybook_cli "arrow" "${INSTALL_ARROW}"
  "${INSTALL_ARROW}/bin/copybook" --version

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
cargo run --manifest-path "${PROJECT_DIR}/Cargo.toml"

echo "Running smoke fixed workflow (single worker)"
run_with_binary "${COPYBOOK_CLI_BIN}" fixed "${FIXTURE_COPYBOOK}" "${FIXTURE_FIXED}" fixed "${FIXTURE_DIR}/fixed/t1" 1

echo "Running smoke fixed workflow (multi-worker)"
run_with_binary "${COPYBOOK_CLI_BIN}" fixed "${FIXTURE_COPYBOOK}" "${FIXTURE_FIXED}" fixed "${FIXTURE_DIR}/fixed/t4" 4

echo "Comparing fixed output across worker settings"
compare_bytes "${FIXTURE_DIR}/fixed/t1/decode.jsonl" "${FIXTURE_DIR}/fixed/t4/decode.jsonl"
compare_bytes "${FIXTURE_DIR}/fixed/t1/encode.bin" "${FIXTURE_DIR}/fixed/t4/encode.bin"

RDW_FIXTURE="${FIXTURE_DIR}/simple.rdw.bin"
make_rdw_fixture "${FIXTURE_FIXED}" "${RDW_FIXTURE}"

echo "Running smoke RDW workflow (single worker)"
run_with_binary "${COPYBOOK_CLI_BIN}" rdw "${FIXTURE_COPYBOOK}" "${RDW_FIXTURE}" rdw "${FIXTURE_DIR}/rdw/t1" 1

echo "Running smoke RDW workflow (multi-worker)"
run_with_binary "${COPYBOOK_CLI_BIN}" rdw "${FIXTURE_COPYBOOK}" "${RDW_FIXTURE}" rdw "${FIXTURE_DIR}/rdw/t4" 4

echo "Comparing RDW output across worker settings"
compare_bytes "${FIXTURE_DIR}/rdw/t1/decode.jsonl" "${FIXTURE_DIR}/rdw/t4/decode.jsonl"
compare_bytes "${FIXTURE_DIR}/rdw/t1/encode.bin" "${FIXTURE_DIR}/rdw/t4/encode.bin"

echo "Release smoke completed successfully."
