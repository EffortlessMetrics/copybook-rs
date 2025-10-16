#!/usr/bin/env bash
set -euo pipefail

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI not found; install GitHub CLI and authenticate before running." >&2
  exit 1
fi

echo "Triggering soak workflow (soak.yml)…"
gh workflow run soak.yml
echo "Triggered soak workflow; check Actions → Soak for artifacts and check-runs."
