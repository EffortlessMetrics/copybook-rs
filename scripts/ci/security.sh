#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

# Prefer cargo.exe on Git Bash/Windows to avoid stale MSYS cargo binaries.
if command -v cargo.exe >/dev/null 2>&1; then
  CARGO_BIN="cargo.exe"
else
  CARGO_BIN="cargo"
fi

echo "==> Running cargo deny check"
# cargo-deny is fast and should gate all PRs
"$CARGO_BIN" deny check

# cargo-audit is networky: run only when Cargo.lock changes or on schedule
DIFF_OK=true
git diff --name-only "${BASE_SHA:-}"..."${HEAD_SHA:-}" >/dev/null 2>&1 || DIFF_OK=false
if $DIFF_OK && git diff --name-only "${BASE_SHA}"..."${HEAD_SHA}" | grep -q '^Cargo\.lock$'; then
  echo "==> Cargo.lock changed, running cargo audit"
  "$CARGO_BIN" install --locked cargo-audit >/dev/null 2>&1 || true
  "$CARGO_BIN" audit
else
  echo "==> No Cargo.lock diff (or base not available) – skipping cargo-audit (policy)"
fi

echo "✅ Security gates passed"
