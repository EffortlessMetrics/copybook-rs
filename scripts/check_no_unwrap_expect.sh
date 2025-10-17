#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(git rev-parse --show-toplevel)"
cd "$ROOT_DIR"

fail=0
patterns=("unwrap\(" "expect\(")

for pattern in "${patterns[@]}"; do
  while IFS= read -r file; do
    if [[ -n "$file" ]]; then
      echo "error: disallowed ${pattern%\\(} usage in $file"
      fail=1
    fi
  done < <(rg --files-with-matches "$pattern" --glob 'src/**/*.rs' --glob '!target/*')
done

exit $fail
