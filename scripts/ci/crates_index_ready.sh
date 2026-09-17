#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Exact-version readiness against the registry index used for resolution.
#
# Background (#996): the ordered publisher waited a fixed 60s after every
# successful upload. Cargo's own publish already establishes upload
# completion, so the only remaining question before publishing a dependent is
# whether the exact package/version is resolvable from the same sparse index
# Cargo resolves. This helper answers that with injectable boundaries so
# tests stay deterministic:
#
#   CRATES_INDEX_BASE_URL  index root (default https://index.crates.io)
#   CURL_BIN               curl binary (default curl)
#   SLEEP_BIN              sleep binary (default sleep)
#
# Usage:
#   probe_package_version <package> <version>
#     exit 0  exact version is listed for the package
#     exit 1  reachable index, exact version not listed (absent, different
#             version, or malformed entry) — readiness unconfirmed
#     exit 2  lookup/tool failure (index unreachable, curl missing/failed
#             for transport reasons) — must block the dependent publish
#   wait_for_version <package> <version> [max_attempts] [delay_seconds]
#     polls probe_package_version; exit 0 on first ready, exit 1 when the
#     bound expires (reports which package is blocked) or on tool failure.
set -euo pipefail

CURL_BIN="${CURL_BIN:-curl}"
SLEEP_BIN="${SLEEP_BIN:-sleep}"
CRATES_INDEX_BASE_URL="${CRATES_INDEX_BASE_URL:-https://index.crates.io}"

index_path_for() {
  local name="$1"
  local len="${#name}"
  if [ "${len}" -le 0 ]; then
    return 1
  fi
  if [ "${len}" -eq 1 ]; then
    printf '1/%s' "${name}"
  elif [ "${len}" -eq 2 ]; then
    printf '2/%s' "${name}"
  elif [ "${len}" -eq 3 ]; then
    printf '3/%s' "${name:0:1}/${name}"
  else
    printf '%s/%s/%s' "${name:0:2}" "${name:2:2}" "${name}"
  fi
}

probe_package_version() {
  local package="$1"
  local version="$2"
  local path
  local body
  if ! path="$(index_path_for "${package}")"; then
    printf 'probe: refusing empty package name\n' >&2
    return 2
  fi
  if ! body="$("${CURL_BIN}" -sfL --max-time 30 "${CRATES_INDEX_BASE_URL}/${path}" 2>/dev/null)"; then
    printf 'probe: index lookup failed for %s\n' "${package}" >&2
    return 2
  fi
  if [ -z "${body}" ]; then
    printf 'probe: empty index entry for %s\n' "${package}" >&2
    return 1
  fi
  # Sparse-index entries are newline-delimited JSON objects, one per version,
  # each carrying a "vers" field. Match the exact version string only: a
  # prefix or substring match (e.g. 0.8.1 vs 0.8.10) is not readiness.
  # Dots are escaped so the version cannot act as a regex; surrounding
  # whitespace is tolerated because only the version string is exact.
  local version_pattern
  version_pattern="$(printf '%s' "${version}" | sed 's/\./\\./g')"
  if printf '%s\n' "${body}" | grep -qE "\"vers\"[[:space:]]*:[[:space:]]*\"${version_pattern}\""; then
    return 0
  fi
  printf 'probe: %s@%s not listed in index entry\n' "${package}" "${version}" >&2
  return 1
}

wait_for_version() {
  local package="$1"
  local version="$2"
  local max_attempts="${3:-12}"
  local delay_seconds="${4:-10}"
  local attempt=1
  local rc=0
  while [ "${attempt}" -le "${max_attempts}" ]; do
    # Capture the probe status via `||`: `$?` after an `if` compound would
    # read the compound's own (zero) status instead of the probe's, and a
    # bare failing call would trip `set -e`. Reset first so a later success
    # cannot inherit an earlier failure code.
    rc=0
    probe_package_version "${package}" "${version}" || rc=$?
    if [ "${rc}" -eq 0 ]; then
      echo "  ${package}@${version} resolvable (attempt ${attempt}/${max_attempts})"
      return 0
    fi
    if [ "${rc}" -eq 2 ]; then
      echo "  FAILED: index lookup for ${package} failed; blocking dependent publish" >&2
      return 1
    fi
    if [ "${attempt}" -eq "${max_attempts}" ]; then
      break
    fi
    echo "  ${package}@${version} not yet resolvable (attempt ${attempt}/${max_attempts}); waiting ${delay_seconds}s"
    "${SLEEP_BIN}" "${delay_seconds}"
    attempt=$((attempt + 1))
  done
  echo "  FAILED: ${package}@${version} not resolvable after ${max_attempts} attempts; blocking dependent publish" >&2
  return 1
}
