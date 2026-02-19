#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

fail=0

if matches=$(rg -n 'Value::String\([^)]*to_string\(' copybook-codec/src/lib_api.rs || true); then
  if [[ -n "${matches}" ]]; then
    printf '%s\n' "${matches}"
    echo "❌ Value::String(..to_string(..)) in lib_api.rs" >&2
    fail=1
  fi
fi

declare -A seen=()
while IFS=: read -r file line _; do
  [[ -z "${file:-}" || -z "${line:-}" ]] && continue
  key="${file}:${line}"
  if [[ -n "${seen[$key]:-}" ]]; then
    continue
  fi
  seen["$key"]=1

  start=$(( line > 2 ? line - 2 : 1 ))
  end=$(( line + 2 ))
  if sed -n "${start},${end}p" "$file" | rg -n '\.to_string\(' >/dev/null; then
    sed -n "${start},${end}p" "$file"
    echo "❌ to_string() adjacent to decimal decode in $file" >&2
    fail=1
  fi
done < <(rg -n --with-filename 'decode_(packed|zoned)_decimal_' copybook-codec/src/lib_api.rs || true)

if (( fail == 0 )); then
  echo "✅ Hot-path allocation guard clean"
else
  exit 1
fi
