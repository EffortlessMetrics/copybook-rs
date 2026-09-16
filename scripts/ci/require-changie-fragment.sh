#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Fail when user-facing paths changed without a changie fragment.
#
# Usage: require-changie-fragment.sh <userfacing:true|false> <fragment:true|false>
# The caller (changelog.yml paths-filter) computes the two signals; this
# script owns the decision so it stays unit-testable.
set -euo pipefail

USERFACING="${1:-}"
FRAGMENT="${2:-}"

if [ "$USERFACING" != "true" ] && [ "$USERFACING" != "false" ]; then
    echo "error: first argument must be 'true' or 'false', got '$USERFACING'" >&2
    exit 2
fi
if [ "$FRAGMENT" != "true" ] && [ "$FRAGMENT" != "false" ]; then
    echo "error: second argument must be 'true' or 'false', got '$FRAGMENT'" >&2
    exit 2
fi

if [ "$USERFACING" = "true" ] && [ "$FRAGMENT" != "true" ]; then
    cat >&2 <<'EOF'
error: user-facing paths changed without a changie fragment.
Add one under .changes/unreleased/ (see docs/CHANGELOG_GENERATION.md),
or mark the PR template checkbox N/A with a reason when the change truly
needs no release note. Release-preparation PRs that batch fragments into
.changes/vX.Y.Z.md satisfy this check automatically.
EOF
    exit 1
fi

echo "fragment requirement satisfied (userfacing=$USERFACING fragment=$FRAGMENT)"
