#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

OLD="${1:?old jsonl}"  # current system output
NEW="${2:?new jsonl}"  # copybook jsonl

tmp_old="$(mktemp)"
tmp_new="$(mktemp)"
cleanup() {
  rm -f "$tmp_old" "$tmp_new"
}
trap cleanup EXIT

jq -S 'del(.schema, .record_index)' "$OLD" > "$tmp_old"
jq -S 'del(.schema, .record_index)' "$NEW" > "$tmp_new"
diff_output="$(diff -u "$tmp_old" "$tmp_new" || true)"
head_lines="$(printf '%s\n' "$diff_output" | head -200)"
echo "## Diff (first 200 lines):"
printf '%s\n' "$head_lines"

if [[ -n "${GITHUB_STEP_SUMMARY:-}" ]]; then
  {
    echo "### Shadow Diff (first 200 lines)"
    echo '```diff'
    printf '%s\n' "$head_lines"
    echo '```'
  } >> "${GITHUB_STEP_SUMMARY}"
fi
