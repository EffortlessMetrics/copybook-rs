#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

base_sha="${BASE_SHA:-$(git merge-base origin/main HEAD 2>/dev/null || echo HEAD~1)}"
head_sha="${HEAD_SHA:-HEAD}"

if ! git rev-parse --verify "${base_sha}^{commit}" >/dev/null 2>&1; then
  printf 'error: cannot resolve panic-policy baseline %s\n' "$base_sha" >&2
  exit 1
fi

if ! git rev-parse --verify "${head_sha}^{commit}" >/dev/null 2>&1; then
  head_sha=HEAD
fi

if violations="$(git diff --unified=0 "${base_sha}"..."${head_sha}" -- '*.rs' | awk '
  /^\+\+\+ b\// { path = substr($0, 7); next }
  /^\+/ && !/^\+\+\+/ && /panic![[:space:]]*\(/ {
    print path ":" substr($0, 2)
    found = 1
  }
  END { exit found ? 1 : 0 }
')"; then
  exit 0
fi

printf 'error: new explicit panic macro in test or shipped Rust source:\n%s\n' "$violations" >&2
exit 1
