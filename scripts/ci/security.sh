#!/usr/bin/env bash
set -euo pipefail

echo "==> Running cargo deny check"
# cargo-deny is fast and should gate all PRs
cargo deny check

# cargo-audit is networky: run only when Cargo.lock changes or on schedule
DIFF_OK=true
git diff --name-only "${BASE_SHA:-}"..."${HEAD_SHA:-}" >/dev/null 2>&1 || DIFF_OK=false
if $DIFF_OK && git diff --name-only "${BASE_SHA}"..."${HEAD_SHA}" | grep -q '^Cargo\.lock$'; then
  echo "==> Cargo.lock changed, running cargo audit"
  cargo install --locked cargo-audit >/dev/null 2>&1 || true
  cargo audit
else
  echo "==> No Cargo.lock diff (or base not available) – skipping cargo-audit (policy)"
fi

echo "✅ Security gates passed"
