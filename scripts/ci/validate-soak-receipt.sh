#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
RECEIPT_FILE="${1:-scripts/bench/perf.json}"
EXPECTED_COMMIT="${2:?expected workflow commit is required}"

cargo run --quiet --manifest-path "$ROOT_DIR/tools/copybook-scripts/Cargo.toml" -- \
  validate-soak-receipt "$RECEIPT_FILE" "$EXPECTED_COMMIT"
