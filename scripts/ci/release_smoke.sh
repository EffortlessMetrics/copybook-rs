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

RUN_DIR="$(mktemp -d -t copybook-release-smoke-XXXXXX)"
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

  local decode_out="${output_dir}/decode.jsonl"
  local encode_out="${output_dir}/encode.bin"
  local verify_out="${output_dir}/verify.json"
  local determinism_out="${output_dir}/determinism.json"
  local roundtrip_out="${output_dir}/roundtrip.jsonl"

  mkdir -p "${output_dir}"

  "${copybook_cli}" decode "${copybook}" "${data_file}" \
    --format "${format}" \
    --codepage cp037 \
    --output "${decode_out}"

  "${copybook_cli}" encode "${copybook}" "${decode_out}" \
    --format "${format}" \
    --codepage cp037 \
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
    cmp -n "$(( $(wc -c < "${data_file}") ))" "${data_file}" "${encode_out}"
    cmp "${decode_out}" "${roundtrip_out}"
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

if [ -n "${COPYBOOK_CLI_BIN:-}" ]; then
  COPYBOOK_CLI_BIN="$(readlink -f "${COPYBOOK_CLI_BIN}")"
  if [ ! -x "${COPYBOOK_CLI_BIN}" ]; then
    echo "COPYBOOK_CLI_BIN is set but not executable: ${COPYBOOK_CLI_BIN}" >&2
    exit 1
  fi
  echo "Using local copybook CLI: ${COPYBOOK_CLI_BIN}"
else
  INSTALL_DEFAULT="${RUN_DIR}/copybook-default"
  INSTALL_ARROW="${RUN_DIR}/copybook-arrow"

  echo "Installing copybook-cli@${VERSION} (default features)"
  install_copybook_cli "" "${INSTALL_DEFAULT}"
  COPYBOOK_CLI_BIN="${INSTALL_DEFAULT}/bin/copybook"

  echo "Installing copybook-cli@${VERSION} (arrow feature)"
  install_copybook_cli "arrow" "${INSTALL_ARROW}"
  "${INSTALL_ARROW}/bin/copybook" --version
fi

"${COPYBOOK_CLI_BIN}" --version
"${COPYBOOK_CLI_BIN}" --help >/dev/null

emit_smoke_manifest "${PROJECT_DIR}/Cargo.toml" "${SMOKE_MODE}"

cat > "${PROJECT_DIR}/src/main.rs" <<EOF
fn main() {}
EOF

echo "Validating copybook and copybook-rs dependency resolution"
cargo build --manifest-path "${PROJECT_DIR}/Cargo.toml"

echo "Running smoke fixed workflow"
run_with_binary "${COPYBOOK_CLI_BIN}" fixed "${FIXTURE_COPYBOOK}" "${FIXTURE_FIXED}" fixed "${FIXTURE_DIR}/fixed"

RDW_FIXTURE="${FIXTURE_DIR}/simple.rdw.bin"
make_rdw_fixture "${FIXTURE_FIXED}" "${RDW_FIXTURE}"

echo "Running smoke RDW workflow"
run_with_binary "${COPYBOOK_CLI_BIN}" rdw "${FIXTURE_COPYBOOK}" "${RDW_FIXTURE}" rdw "${FIXTURE_DIR}/rdw"

echo "Release smoke completed successfully."
