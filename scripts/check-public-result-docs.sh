#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Lists public Result fns missing #[inline]/#[must_use]/# Errors
set -euo pipefail

scan_dirs=("crates/copybook-codec/src" "crates/copybook-core/src")
miss=0

for dir in "${scan_dirs[@]}"; do
  while IFS=: read -r file line sig; do
    # Inspect a small window above the signature for attrs/docs
    hdr="$(sed -n "$((line-4)),$((line-1))p" "$file")"
    if ! printf '%s\n' "$hdr" | rg -q '^\s*#\[\s*inline\s*\]'; then
      echo "missing #[inline]      @ $file:$line"; miss=1
    fi
    if ! printf '%s\n' "$hdr" | rg -q '^\s*#\[\s*must_use[^\]]*\]'; then
      echo "missing #[must_use]    @ $file:$line"; miss=1
    fi
    if ! printf '%s\n' "$hdr" | rg -q '^\s*///\s*#\s*Errors'; then
      echo "missing doc '# Errors' @ $file:$line"; miss=1
    fi
  done < <(rg -n '^\s*pub\s+fn\s+\w+.*->\s*Result<' "$dir")
done

exit $miss
