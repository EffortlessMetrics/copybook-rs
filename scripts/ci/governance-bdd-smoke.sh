#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

# Prefer cargo.exe on Git Bash/Windows to avoid stale MSYS cargo binaries.
# Never under WSL: Windows interop exposes cargo.exe there, but the Windows
# binary mangles paths (\\wsl.localhost\...) and breaks incremental builds.
if [ -z "${WSL_DISTRO_NAME:-}" ] && ! grep -qi microsoft /proc/version 2>/dev/null && command -v cargo.exe >/dev/null 2>&1; then
  CARGO_BIN="cargo.exe"
else
  CARGO_BIN="cargo"
fi

governance_packages=(
  copybook-contracts
  copybook-support-matrix
  copybook-governance
)

package_args=()
for package in "${governance_packages[@]}"; do
  package_args+=("-p" "$package")
done

echo "==> Running governance microcrate checks"
"$CARGO_BIN" check "${package_args[@]}"

echo "==> Running governance microcrate tests"
"$CARGO_BIN" test "${package_args[@]}"

echo "==> Running BDD smoke tests"
"$CARGO_BIN" test -p copybook-bdd --test bdd_smoke -- --nocapture

echo "✅ Governance + BDD smoke gate passed"
