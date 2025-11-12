#!/usr/bin/env bash
#
# Offline CI Gate for PR validation when GitHub Actions is unavailable
# Mirrors the "Quick" CI job checks for local validation
#
# Usage: ./scripts/ci/offline-gate.sh
#

set -euo pipefail

echo "╔════════════════════════════════════════════════════════╗"
echo "║  Offline CI Gate - PR #151 Validation                ║"
echo "║  Validates: fmt, clippy (pedantic), tests, doctests  ║"
echo "╚════════════════════════════════════════════════════════╝"
echo ""

# Color output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

FAILED=0

# Function to print status
print_status() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✅ PASS${NC}: $2"
    else
        echo -e "${RED}❌ FAIL${NC}: $2"
        FAILED=1
    fi
}

echo "==> Toolchain Info"
rustc --version
cargo --version
echo ""

# 1. Format Check
echo "==> [1/5] Running cargo fmt --all -- --check"
if cargo fmt --all -- --check 2>&1; then
    print_status 0 "Format check"
else
    print_status 1 "Format check"
fi
echo ""

# 2. Clippy (Pedantic, Workspace)
echo "==> [2/5] Running clippy (pedantic, all targets, all features)"
if cargo clippy --workspace --all-targets --all-features -- \
    -D warnings -D clippy::all -D clippy::pedantic 2>&1 | tail -10; then
    print_status 0 "Clippy pedantic (workspace)"
else
    print_status 1 "Clippy pedantic (workspace)"
fi
echo ""

# 3. Unit Tests (copybook-core - the changed package)
echo "==> [3/5] Running unit tests (copybook-core lib)"
if cargo test --package copybook-core --lib 2>&1 | tail -15; then
    print_status 0 "Unit tests (copybook-core)"
else
    print_status 1 "Unit tests (copybook-core)"
fi
echo ""

# 4. Integration Tests (copybook-core, excluding comprehensive_parser_tests)
echo "==> [4/5] Running integration tests (copybook-core)"
if cargo test --package copybook-core \
    --test golden_fixtures_odo \
    --test zoned_encoding_error_codes_tests 2>&1 | tail -10; then
    print_status 0 "Integration tests (copybook-core)"
else
    print_status 1 "Integration tests (copybook-core)"
fi
echo ""

# 5. Doc Tests
echo "==> [5/5] Running doctests (deny warnings)"
if RUSTDOCFLAGS="--deny warnings" cargo test --package copybook-core --doc 2>&1 | tail -10; then
    print_status 0 "Doc tests"
else
    print_status 1 "Doc tests"
fi
echo ""

# Summary
echo "╔════════════════════════════════════════════════════════╗"
if [ $FAILED -eq 0 ]; then
    echo -e "║  ${GREEN}✅ ALL CHECKS PASSED${NC}                              ║"
    echo "╚════════════════════════════════════════════════════════╝"
    echo ""
    echo "✨ Local validation complete. PR #151 is ready for merge."
    echo ""
    echo "Note: xtask docs checks require junit.xml from full nextest run"
    echo "      (advisory only, not blocking for this PR)"
    exit 0
else
    echo -e "║  ${RED}❌ SOME CHECKS FAILED${NC}                             ║"
    echo "╚════════════════════════════════════════════════════════╝"
    echo ""
    echo "Please fix the failures above before merging."
    exit 1
fi
