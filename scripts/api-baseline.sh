#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# API Baseline Management Script
# This script manages API baselines for semver checking.

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

BASELINE_VERSION_FILE="$BASELINE_DIR/version.txt"
BASELINE_TIMESTAMP_FILE="$BASELINE_DIR/timestamp.txt"
BASELINE_REVISION_FILE="$BASELINE_DIR/revision.txt"
BASELINE_TOOL_VERSION_FILE="$BASELINE_DIR/tool-version.txt"
BASELINE_STABILITY_SCOPE_FILE="$BASELINE_DIR/stability-scope.txt"
BASELINE_PACKAGES_FILE="$BASELINE_DIR/stable-packages.txt"

REQUIRED_SEMVER_CHECKS_VERSION="0.46.0"
STABILITY_REGISTRY_PATH="$PROJECT_ROOT/docs/stability/surface-registry.json"
CARGO_SEMVER_CHECKS=""
CARGO_SEMVER_CHECKS_MODE="detect"
PYTHON_CMD=""

# Current version from Cargo.toml
CURRENT_VERSION="$(awk -F\" '/^version = /{print $2; exit}' "$PROJECT_ROOT/Cargo.toml")"

print_info() {
    echo -e "${BLUE}INFO:${NC} $1"
}

print_success() {
    echo -e "${GREEN}OK:${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}WARN:${NC} $1"
}

print_error() {
    echo -e "${RED}ERR:${NC} $1"
}

require_cargo_semver_checks() {
    if command -v python3 &> /dev/null; then
        PYTHON_CMD="python3"
    elif command -v python &> /dev/null; then
        PYTHON_CMD="python"
    else
        print_error "python (or python3) is not installed."
        exit 1
    fi

    if command -v cargo-semver-checks &> /dev/null; then
        CARGO_SEMVER_CHECKS="$(command -v cargo-semver-checks)"
    elif [[ -x "$HOME/.cargo/bin/cargo-semver-checks" ]]; then
        CARGO_SEMVER_CHECKS="$HOME/.cargo/bin/cargo-semver-checks"
    elif [[ -x "$HOME/.cargo/bin/cargo-semver-checks.exe" ]]; then
        CARGO_SEMVER_CHECKS="$HOME/.cargo/bin/cargo-semver-checks.exe"
    else
        print_error "cargo-semver-checks is not installed."
        print_info "Install with: cargo install --locked cargo-semver-checks --version ${REQUIRED_SEMVER_CHECKS_VERSION}"
        exit 1
    fi

    if "$CARGO_SEMVER_CHECKS" check-release --help >/dev/null 2>&1; then
        CARGO_SEMVER_CHECKS_MODE="standalone"
    elif cargo semver-checks check-release --help >/dev/null 2>&1; then
        CARGO_SEMVER_CHECKS="cargo"
        CARGO_SEMVER_CHECKS_MODE="cargo-subcommand"
    else
        print_error "Unable to invoke cargo-semver-checks."
        print_info "Expected either:"
        print_info "  - cargo-semver-checks check-release (standalone mode)"
        print_info "  - cargo semver-checks check-release (cargo subcommand mode)"
        exit 1
    fi

    local installed_version
    if [[ "$CARGO_SEMVER_CHECKS_MODE" == "standalone" ]]; then
        installed_version="$("$CARGO_SEMVER_CHECKS" --version | awk '{print $2}')"
    else
        installed_version="$(cargo semver-checks --version | awk '{print $2}')"
    fi

    local required_floor
    required_floor="${REQUIRED_SEMVER_CHECKS_VERSION}"

    if [[ "$(printf '%s\n%s\n' "$required_floor" "$installed_version" | sort -V | head -n1)" != "$required_floor" ]]; then
        print_error "cargo-semver-checks version ${installed_version} is older than required ${REQUIRED_SEMVER_CHECKS_VERSION}."
        print_info "Upgrade with: cargo install --locked cargo-semver-checks --version ${REQUIRED_SEMVER_CHECKS_VERSION}"
        exit 1
    fi
}

load_stable_packages() {
    if [[ ! -f "$STABILITY_REGISTRY_PATH" ]]; then
        print_error "Missing stability registry: $STABILITY_REGISTRY_PATH"
        exit 1
    fi

    "$PYTHON_CMD" - "$STABILITY_REGISTRY_PATH" <<'PY'
import json
import sys

with open(sys.argv[1], "r", encoding="utf-8") as handle:
    data = json.load(handle)

for package in data.get("packages", []):
    if package.get("class") == "stable":
        print(package.get("name", ""))
PY
}

load_workspace_packages() {
    local output

    if ! output="$(
        "$PYTHON_CMD" - <<'PY' 2>&1
import subprocess
import json

data = json.loads(
    subprocess.check_output(
        ["cargo", "metadata", "--format-version", "1", "--no-deps", "--locked"],
        text=True,
    )
)
workspace_members = set(data.get("workspace_members", []))

for package in data.get("packages", []):
    if package.get("id") in workspace_members:
        print(package.get("name", ""))
PY
    )"; then
        print_error "Failed to read workspace packages via cargo metadata."
        print_warning "Captured diagnostics:"
        echo "$output"
        print_info "This often means the active Cargo toolchain is too old for the workspace manifest."
        print_info "Verify toolchain with: cargo --version"
        exit 1
    fi

    printf '%s\n' "$output"
}

validate_stable_workspace_alignment() {
    local stable_packages="$1"
    local workspace_packages="$2"

    local missing_from_workspace
    local extra_in_workspace

    missing_from_workspace="$(comm -23 \
        <(printf '%s\n' "$stable_packages" | sort) \
        <(printf '%s\n' "$workspace_packages" | sort) || true)"
    extra_in_workspace="$(comm -23 \
        <(printf '%s\n' "$workspace_packages" | sort) \
        <(printf '%s\n' "$stable_packages" | sort) || true)"

    if [[ -n "$missing_from_workspace" ]]; then
        print_error "Stability registry references packages missing from workspace: $(echo "$missing_from_workspace" | tr '\n' ' ')"
        exit 1
    fi

    if [[ -n "$extra_in_workspace" ]]; then
        print_warning "Workspace packages not classified as stable (expected): $(echo "$extra_in_workspace" | tr '\n' ' ')"
    fi
}

build_exclude_args() {
    local workspace_packages="$1"
    local stable_packages="$2"
    local -a workspace
    local -a stable
    local -a excludes
    local package
    local stable_package

    mapfile -t workspace <<< "$workspace_packages"
    mapfile -t stable <<< "$stable_packages"

    for package in "${workspace[@]}"; do
        local is_stable="false"
        for stable_package in "${stable[@]}"; do
            if [[ "$package" == "$stable_package" ]]; then
                is_stable="true"
                break
            fi
        done
        if [[ "$is_stable" == "false" ]]; then
            excludes+=("$package")
        fi
    done

    if (( ${#excludes[@]} > 0 )); then
        printf '%s\n' "${excludes[@]}"
    fi
}

run_stable_semver_check() {
    local baseline_revision="$1"
    local exclude_args="$2"
    local -a semver_args

    if [[ "$CARGO_SEMVER_CHECKS_MODE" == "standalone" ]]; then
        semver_args=(
            "check-release"
            "--workspace"
            "--baseline-rev=${baseline_revision}"
        )
    else
        semver_args=(
            "semver-checks"
            "check-release"
            "--workspace"
            "--baseline-rev=${baseline_revision}"
        )
    fi

    if [[ -n "$exclude_args" ]]; then
        while IFS= read -r package_name; do
            semver_args+=("--exclude")
            semver_args+=("$package_name")
        done <<< "$exclude_args"
    fi

    if [[ "$CARGO_SEMVER_CHECKS_MODE" == "standalone" ]]; then
        (cd "$PROJECT_ROOT" && "$CARGO_SEMVER_CHECKS" "${semver_args[@]}")
    else
        (cd "$PROJECT_ROOT" && "${CARGO_SEMVER_CHECKS}" "${semver_args[@]}")
    fi
}

record_baseline_state() {
    local stable_packages="$1"
    local baseline_revision="$2"
    local semver_version
    if [[ "$CARGO_SEMVER_CHECKS_MODE" == "standalone" ]]; then
        semver_version="$("$CARGO_SEMVER_CHECKS" --version | awk '{print $2}')"
    else
        semver_version="$(cargo semver-checks --version | awk '{print $2}')"
    fi

    printf '%s\n' "$CURRENT_VERSION" > "$BASELINE_VERSION_FILE"
    printf '%s\n' "$(date -u +"%Y-%m-%dT%H:%M:%SZ")" > "$BASELINE_TIMESTAMP_FILE"
    printf '%s\n' "$baseline_revision" > "$BASELINE_REVISION_FILE"
    printf '%s\n' "$semver_version" > "$BASELINE_TOOL_VERSION_FILE"
    printf 'stable-crate-only\n' > "$BASELINE_STABILITY_SCOPE_FILE"
    printf '%s\n' "$stable_packages" > "$BASELINE_PACKAGES_FILE"
}

generate_baseline() {
    print_info "Generating stable API baseline for version ${CURRENT_VERSION}."

    local stable_packages
    local workspace_packages
    local exclude_args
    local baseline_revision

    stable_packages="$(load_stable_packages | sort)"
    workspace_packages="$(load_workspace_packages | sort)"

    validate_stable_workspace_alignment "$stable_packages" "$workspace_packages"

    baseline_revision="$(cd "$PROJECT_ROOT" && git rev-parse HEAD)"
    exclude_args="$(build_exclude_args "$workspace_packages" "$stable_packages")"

    print_info "Running baseline check once to ensure stable surface is comparable at ${baseline_revision}..."
    run_stable_semver_check "$baseline_revision" "$exclude_args"

    record_baseline_state "$stable_packages" "$baseline_revision"

    print_success "Stable API baseline generated for version ${CURRENT_VERSION}"
    print_info "Baseline metadata stored in ${BASELINE_DIR}"
}

check_api() {
    print_info "Checking API compatibility against stable-surface baseline."

    if [[ ! -f "$BASELINE_VERSION_FILE" ]] || [[ ! -f "$BASELINE_REVISION_FILE" ]] || [[ ! -f "$BASELINE_PACKAGES_FILE" ]]; then
        print_error "No baseline found. Run 'just api-baseline' first."
        exit 1
    fi

    local baseline_version
    local baseline_timestamp
    local baseline_revision
    local stable_packages
    local workspace_packages
    local exclude_args

    baseline_version="$(cat "$BASELINE_VERSION_FILE")"
    baseline_timestamp="$(cat "$BASELINE_TIMESTAMP_FILE")"
    baseline_revision="$(cat "$BASELINE_REVISION_FILE")"
    stable_packages="$(load_stable_packages | sort)"
    workspace_packages="$(load_workspace_packages | sort)"

    validate_stable_workspace_alignment "$stable_packages" "$workspace_packages"

    if ! (cd "$PROJECT_ROOT" && git cat-file -e "${baseline_revision}^{commit}" >/dev/null 2>&1); then
        print_error "Baseline commit ${baseline_revision} is unavailable."
        print_info "Re-run 'just api-baseline' after switching to a reachable commit."
        exit 1
    fi

    print_info "Baseline version: ${baseline_version}"
    print_info "Baseline revision: ${baseline_revision}"
    print_info "Baseline timestamp: ${baseline_timestamp}"
    print_info "Current version: ${CURRENT_VERSION}"

    exclude_args="$(build_exclude_args "$workspace_packages" "$stable_packages")"
    if run_stable_semver_check "$baseline_revision" "$exclude_args"; then
        print_success "Stable API compatibility check passed"
        return 0
    fi

    print_error "Stable API compatibility check failed"
    return 1
}

show_baseline_info() {
    print_info "API Baseline Information"

    if [[ ! -f "$BASELINE_VERSION_FILE" ]]; then
        print_warning "No baseline found"
        exit 0
    fi

    local baseline_version
    local baseline_timestamp
    local baseline_revision
    local baseline_tool_version
    local baseline_scope
    local baseline_packages_file

    baseline_version="$(cat "$BASELINE_VERSION_FILE")"
    baseline_timestamp="$(cat "$BASELINE_TIMESTAMP_FILE" 2>/dev/null || echo "unknown")"
    baseline_revision="$(cat "$BASELINE_REVISION_FILE" 2>/dev/null || echo "unknown")"
    baseline_tool_version="$(cat "$BASELINE_TOOL_VERSION_FILE" 2>/dev/null || echo "unknown")"
    baseline_scope="$(cat "$BASELINE_STABILITY_SCOPE_FILE" 2>/dev/null || echo "unknown")"
    baseline_packages_file="$BASELINE_PACKAGES_FILE"

    echo ""
    echo "Baseline Version: ${baseline_version}"
    echo "Baseline Timestamp: ${baseline_timestamp}"
    echo "Baseline Revision: ${baseline_revision}"
    echo "Baseline Tool: cargo-semver-checks ${baseline_tool_version}"
    echo "Baseline Scope: ${baseline_scope}"
    echo "Current Version: ${CURRENT_VERSION}"
    echo "Baseline Directory: ${BASELINE_DIR}"

    if [[ -f "$baseline_packages_file" ]]; then
        echo "Stable packages:"
        while IFS= read -r package; do
            echo "  - ${package}"
        done < "$baseline_packages_file"
    fi
}

check_freeze_status() {
    print_info "Checking API freeze status..."
    if [[ -f "$PROJECT_ROOT/.api-freeze" ]]; then
        print_warning "API freeze is ACTIVE"
        echo ""
        cat "$PROJECT_ROOT/.api-freeze"
        return 0
    fi

    print_success "API freeze is NOT active"
    return 1
}

main() {
    local command="${1:-help}"

    require_cargo_semver_checks

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
            echo "  generate, baseline  Generate stable API baseline for current version"
            echo "  check               Check API compatibility against stable baseline"
            echo "  info                Show current baseline information"
            echo "  freeze-status       Check if API freeze is active"
            echo "  help                Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0 generate   # Generate new stable baseline"
            echo "  $0 check      # Check stable API compatibility"
            echo "  $0 info       # Show baseline info"
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
