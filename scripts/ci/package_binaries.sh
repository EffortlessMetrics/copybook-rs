#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Package a built `copybook` CLI binary into a release tarball plus SHA-256.
#
# usage: package_binaries.sh <version> <target-triple> <bindir>
# example: package_binaries.sh 0.9.0 x86_64-unknown-linux-gnu ./target/release
#
# Output (in the current directory):
#   copybook-<version>-<triple>.tar.gz
#   copybook-<version>-<triple>.tar.gz.sha256
#
# The tarball holds one file: the `copybook` binary (plus `.exe` on
# Windows triples). Deterministic metadata is intentionally not claimed;
# the `.sha256` beside the artifact is the integrity contract.
set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "usage: package_binaries.sh <version> <target-triple> <bindir>" >&2
  echo "example: package_binaries.sh 0.9.0 x86_64-unknown-linux-gnu ./target/release" >&2
  exit 1
fi

VERSION="${1#v}"
TRIPLE="$2"
BINDIR="$3"

if [ -z "${VERSION}" ]; then
  echo "invalid version: '$1'" >&2
  exit 1
fi
if [ -z "${TRIPLE}" ]; then
  echo "invalid target triple: '$2'" >&2
  exit 1
fi

BINARY="copybook"
case "${TRIPLE}" in
  *-pc-windows-*) BINARY="copybook.exe" ;;
esac

if [ ! -f "${BINDIR}/${BINARY}" ]; then
  echo "binary not found: ${BINDIR}/${BINARY}" >&2
  exit 1
fi

ARCHIVE="copybook-${VERSION}-${TRIPLE}.tar.gz"
STAGE="$(mktemp -d)"
trap 'rm -rf "${STAGE}"' EXIT
cp "${BINDIR}/${BINARY}" "${STAGE}/${BINARY}"
tar -czf "${ARCHIVE}" -C "${STAGE}" "${BINARY}"
# Portable SHA-256: macOS runners have no sha256sum.
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "${ARCHIVE}" > "${ARCHIVE}.sha256"
else
  shasum -a 256 "${ARCHIVE}" > "${ARCHIVE}.sha256"
fi

echo "packaged ${ARCHIVE}"
cat "${ARCHIVE}.sha256"
