#!/usr/bin/env bash
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
echo "## Diff (first 200 lines):"
diff -u "$tmp_old" "$tmp_new" | head -200 || true
