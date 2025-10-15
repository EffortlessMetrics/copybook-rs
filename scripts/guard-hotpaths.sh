#!/usr/bin/env bash
set -euo pipefail

fail=0

if matches=$(rg -n 'Value::String\([^)]*to_string' copybook-codec/src/lib_api.rs); then
  echo "$matches"
  echo "❌ lib_api.rs: Value::String(..to_string(..)) found" >&2
  fail=1
fi

if rg -q 'decode_(packed|zoned)_decimal_' copybook-codec/src/lib_api.rs; then
  while IFS= read -r line; do
    file=${line%%:*}
    rest=${line#*:}
    lineno=${rest%%:*}
    start=$((lineno > 2 ? lineno - 2 : 1))
    end=$((lineno + 2))
    if sed -n "${start},${end}p" "$file" | rg -q '\.to_string\('; then
      sed -n "${start},${end}p" "$file"
      echo "❌ lib_api.rs: to_string() adjacent to decode_*_decimal_*" >&2
      fail=1
      break
    fi
  done <<<"$(rg -n 'decode_(packed|zoned)_decimal_.*' copybook-codec/src/lib_api.rs)"
fi

if [ "$fail" -ne 0 ]; then
  exit 1
fi

echo "✅ Hot-path allocation guard clean"
