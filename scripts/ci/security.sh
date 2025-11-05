#!/usr/bin/env bash
set -euo pipefail

echo "==> Running cargo deny check"
# cargo-deny is fast and should gate all PRs
cargo deny check

# cargo-audit is networky: run only when Cargo.lock changes or on schedule
if git diff --name-only "${BASE_SHA:-HEAD~1}...${HEAD_SHA:-HEAD}" 2>/dev/null | grep -q '^Cargo\.lock$'; then
  echo "==> Cargo.lock changed, running cargo audit"
  cargo install --locked cargo-audit >/dev/null 2>&1 || true
  cargo audit
else
  echo "==> Cargo.lock unchanged; skipping cargo-audit (policy)"
fi

echo "✅ Security gates passed"
