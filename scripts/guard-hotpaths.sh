#!/usr/bin/env bash
set -euo pipefail
if rg -n "\.to_string\(" copybook-codec/src/lib_api.rs \
   | rg -E "(Zoned|Packed)Decimal|Value::String" ; then
  echo "❌ Hot-path allocation detected in lib_api.rs" >&2
  exit 1
fi
echo "✅ Hot-path allocation guard clean"
