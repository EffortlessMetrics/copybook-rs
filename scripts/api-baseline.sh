#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# API Baseline Management Script
# This script manages API baselines for semver checking

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Baseline directory
BASELINE_DIR="$PROJECT_ROOT/.api-baseline"
mkdir -p "$BASELINE_DIR"

# Current version from Cargo.toml
CURRENT_VERSION=$(grep -E '^version\s*=' "$PROJECT_ROOT/Cargo.toml" | head -1 | sed 's/version\s*=\s*"\([^"]*\)"/\1/')

# Function to print colored output
print_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

# Function to check if cargo-semver-checks is installed
check_semver_checks() {
    if ! command -v cargo-semver-checks &> /dev/null; then
        print_error "cargo-semver-checks is not installed"
        print_info "Install it with: cargo install --locked cargo-semver-checks --version 0.37.0"
        exit 1
    fi
}

# Function to generate API baseline for current version
generate_baseline() {
    print_info "Generating API baseline for version $CURRENT_VERSION..."

    cd "$PROJECT_ROOT"

    # Build metadata for all workspace crates
    cargo semver-checks build-metadata --workspace

    # Store the version info
    echo "$CURRENT_VERSION" > "$BASELINE_DIR/version.txt"
    echo "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" > "$BASELINE_DIR/timestamp.txt"

    print_success "API baseline generated for version $CURRENT_VERSION"
    print_info "Baseline stored in: $BASELINE_DIR"
}

# Function to check API compatibility against baseline
check_api() {
    print_info "Checking API compatibility against baseline..."

    if [ ! -f "$BASELINE_DIR/version.txt" ]; then
        print_error "No baseline found. Run 'just api-baseline' first."
        exit 1
    fi

    BASELINE_VERSION=$(cat "$BASELINE_DIR/version.txt")
    print_info "Baseline version: $BASELINE_VERSION"
    print_info "Current version: $CURRENT_VERSION"

    cd "$PROJECT_ROOT"

    # Run semver checks
    if cargo semver-checks check-release \
        --workspace \
        --baseline-root="$BASELINE_DIR" \
        --current-root=. \
        --error-on-missing; then
        print_success "API compatibility check passed"
        return 0
    else
        print_error "API compatibility check failed"
        return 1
    fi
}

# Function to show current baseline info
show_baseline_info() {
    print_info "API Baseline Information"

    if [ ! -f "$BASELINE_DIR/version.txt" ]; then
        print_warning "No baseline found"
        exit 0
    fi

    BASELINE_VERSION=$(cat "$BASELINE_DIR/version.txt")
    BASELINE_TIMESTAMP=$(cat "$BASELINE_DIR/timestamp.txt" 2>/dev/null || echo "unknown")

    echo ""
    echo "Baseline Version: $BASELINE_VERSION"
    echo "Baseline Timestamp: $BASELINE_TIMESTAMP"
    echo "Current Version: $CURRENT_VERSION"
    echo "Baseline Directory: $BASELINE_DIR"
}

# Function to check if API freeze is active
check_freeze_status() {
    print_info "Checking API freeze status..."

    if [ -f "$PROJECT_ROOT/.api-freeze" ]; then
        print_warning "API freeze is ACTIVE"
        echo ""
        cat "$PROJECT_ROOT/.api-freeze"
        return 0
    else
        print_success "API freeze is NOT active"
        return 1
    fi
}

# Main function
main() {
    local command="${1:-help}"

    check_semver_checks

    case "$command" in
        generate|baseline)
            generate_baseline
            ;;
        check)
            check_api
            ;;
        info)
            show_baseline_info
            ;;
        freeze-status)
            check_freeze_status
            ;;
        help|--help|-h)
            echo "API Baseline Management Script"
            echo ""
            echo "Usage: $0 <command>"
            echo ""
            echo "Commands:"
            echo "  generate, baseline  Generate API baseline for current version"
            echo "  check               Check API compatibility against baseline"
            echo "  info                Show current baseline information"
            echo "  freeze-status       Check if API freeze is active"
            echo "  help                Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0 generate    # Generate new baseline"
            echo "  $0 check       # Check API compatibility"
            echo "  $0 info        # Show baseline info"
            ;;
        *)
            print_error "Unknown command: $command"
            echo ""
            echo "Run '$0 help' for usage information"
            exit 1
            ;;
    esac
}

main "$@"
