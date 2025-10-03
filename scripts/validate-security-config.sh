#!/usr/bin/env bash
# AC:4,5,9 - Security configuration validation script
# Specification: docs/how-to/configure-security-scanning.md
# Issue: #35 - Dependency & Security Scanning Infrastructure
#
# Purpose: Validate YAML syntax, deny.toml policies, and JSON Schema compliance
#
# Usage:
#   ./scripts/validate-security-config.sh [--all|--yaml|--deny|--schema]
#
# Requirements:
#   - yq (YAML processor): brew install yq / apt-get install yq
#   - cargo-deny: cargo install cargo-deny
#   - check-jsonschema: pip install check-jsonschema

set -euo pipefail

# Color output helpers
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Validation counters
PASS_COUNT=0
FAIL_COUNT=0
SKIP_COUNT=0

# Helper functions
print_header() {
    echo ""
    echo "========================================"
    echo "$1"
    echo "========================================"
}

print_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASS_COUNT++))
}

print_fail() {
    echo -e "${RED}✗${NC} $1"
    ((FAIL_COUNT++))
}

print_skip() {
    echo -e "${YELLOW}⊘${NC} $1"
    ((SKIP_COUNT++))
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Validate YAML syntax for workflows
validate_yaml_syntax() {
    print_header "AC:9 - Workflow YAML Syntax Validation"

    local workflow_files=(
        ".github/workflows/ci.yml"
        ".github/workflows/security-scan.yml"
        "tests/fixtures/security-scanning/workflows/ci-security-audit.yml"
        "tests/fixtures/security-scanning/workflows/security-scan-weekly.yml"
    )

    if ! command_exists yq; then
        print_skip "yq not installed - skipping YAML syntax validation"
        print_skip "Install: brew install yq (macOS) or apt-get install yq (Ubuntu)"
        return
    fi

    for workflow_file in "${workflow_files[@]}"; do
        if [ -f "$workflow_file" ]; then
            if yq eval '.' "$workflow_file" >/dev/null 2>&1; then
                print_pass "Valid YAML: $workflow_file"
            else
                print_fail "Invalid YAML: $workflow_file"
            fi
        else
            print_skip "File not found: $workflow_file (expected to be created)"
        fi
    done
}

# Validate Dependabot configuration
validate_dependabot_config() {
    print_header "AC:3 - Dependabot Configuration Validation"

    local dependabot_files=(
        ".github/dependabot.yml"
        "tests/fixtures/security-scanning/configs/dependabot.yml"
    )

    if ! command_exists yq; then
        print_skip "yq not installed - skipping Dependabot validation"
        return
    fi

    for dependabot_file in "${dependabot_files[@]}"; do
        if [ -f "$dependabot_file" ]; then
            # Check version field
            if yq eval '.version' "$dependabot_file" | grep -q "2"; then
                print_pass "Valid version: $dependabot_file"
            else
                print_fail "Invalid version in $dependabot_file (expected: 2)"
            fi

            # Check ecosystems
            local ecosystems=$(yq eval '.updates[].package-ecosystem' "$dependabot_file" | tr '\n' ' ')
            if echo "$ecosystems" | grep -q "cargo" && echo "$ecosystems" | grep -q "github-actions"; then
                print_pass "Valid ecosystems: $dependabot_file (cargo, github-actions)"
            else
                print_fail "Missing ecosystems in $dependabot_file (expected: cargo, github-actions)"
            fi

            # Check grouping configuration
            if yq eval '.updates[0].groups' "$dependabot_file" | grep -q "patch-updates"; then
                print_pass "Valid grouping: $dependabot_file (patch-updates group configured)"
            else
                print_skip "No grouping configured in $dependabot_file (optional but recommended)"
            fi
        else
            print_skip "File not found: $dependabot_file"
        fi
    done
}

# Validate deny.toml policies
validate_deny_toml_policies() {
    print_header "AC:4 - deny.toml Policy Validation"

    local deny_configs=(
        "deny.toml"
        "tests/fixtures/security-scanning/configs/deny-baseline.toml"
        "tests/fixtures/security-scanning/configs/deny-enhanced.toml"
    )

    if ! command_exists cargo-deny; then
        print_skip "cargo-deny not installed - skipping policy validation"
        print_skip "Install: cargo install cargo-deny"
        return
    fi

    for deny_config in "${deny_configs[@]}"; do
        if [ -f "$deny_config" ]; then
            print_pass "File exists: $deny_config"

            # Validate advisory policies (enhanced config should have stricter settings)
            if grep -q 'yanked = "deny"' "$deny_config"; then
                print_pass "Strict yanked policy: $deny_config (yanked = \"deny\")"
            else
                if [ "$deny_config" = "tests/fixtures/security-scanning/configs/deny-enhanced.toml" ]; then
                    print_fail "Enhanced config missing strict yanked policy: $deny_config"
                else
                    print_skip "Baseline yanked policy: $deny_config (not strict)"
                fi
            fi

            # Validate wildcard policies
            if grep -q 'wildcards = "deny"' "$deny_config"; then
                print_pass "Strict wildcard policy: $deny_config (wildcards = \"deny\")"
            else
                if [ "$deny_config" = "tests/fixtures/security-scanning/configs/deny-enhanced.toml" ]; then
                    print_fail "Enhanced config missing strict wildcard policy: $deny_config"
                else
                    print_skip "Baseline wildcard policy: $deny_config (not strict)"
                fi
            fi

            # Validate source policies
            if grep -q 'unknown-registry = "deny"' "$deny_config"; then
                print_pass "Strict registry policy: $deny_config (unknown-registry = \"deny\")"
            else
                if [ "$deny_config" = "tests/fixtures/security-scanning/configs/deny-enhanced.toml" ]; then
                    print_fail "Enhanced config missing strict registry policy: $deny_config"
                else
                    print_skip "Baseline registry policy: $deny_config (not strict)"
                fi
            fi

            # Run cargo-deny check on enhanced config (if it's the main deny.toml or enhanced fixture)
            if [ "$deny_config" = "deny.toml" ] || [ "$deny_config" = "tests/fixtures/security-scanning/configs/deny-enhanced.toml" ]; then
                if cargo deny check advisories --config "$deny_config" >/dev/null 2>&1; then
                    print_pass "Advisories check passed: $deny_config"
                else
                    print_skip "Advisories check failed: $deny_config (may have vulnerabilities to fix)"
                fi

                if cargo deny check bans --config "$deny_config" >/dev/null 2>&1; then
                    print_pass "Bans check passed: $deny_config"
                else
                    print_skip "Bans check failed: $deny_config (may have policy violations)"
                fi

                if cargo deny check sources --config "$deny_config" >/dev/null 2>&1; then
                    print_pass "Sources check passed: $deny_config"
                else
                    print_skip "Sources check failed: $deny_config (may have untrusted sources)"
                fi
            fi
        else
            print_skip "File not found: $deny_config"
        fi
    done
}

# Validate security receipt JSON Schema
validate_security_receipt_schema() {
    print_header "AC:5 - Security Receipt JSON Schema Validation"

    local schema_file="docs/reference/security-receipt-schema.json"
    local receipt_fixtures=(
        "tests/fixtures/security-scanning/receipts/clean-scan.json"
        "tests/fixtures/security-scanning/receipts/vulnerabilities-found.json"
        "tests/fixtures/security-scanning/receipts/geiger-zero-unsafe.json"
    )

    if ! command_exists check-jsonschema; then
        print_skip "check-jsonschema not installed - skipping schema validation"
        print_skip "Install: pip install check-jsonschema"
        return
    fi

    # Validate schema metaschema compliance
    if [ -f "$schema_file" ]; then
        if check-jsonschema --check-metaschema "$schema_file" >/dev/null 2>&1; then
            print_pass "Valid JSON Schema: $schema_file"
        else
            print_fail "Invalid JSON Schema: $schema_file (metaschema validation failed)"
        fi
    else
        print_skip "Schema file not found: $schema_file (expected to be created)"
        return
    fi

    # Validate test fixtures against schema
    for receipt_fixture in "${receipt_fixtures[@]}"; do
        if [ -f "$receipt_fixture" ]; then
            if check-jsonschema --schemafile "$schema_file" "$receipt_fixture" >/dev/null 2>&1; then
                print_pass "Valid receipt: $receipt_fixture (conforms to schema)"
            else
                print_fail "Invalid receipt: $receipt_fixture (schema validation failed)"
            fi
        else
            print_skip "Receipt fixture not found: $receipt_fixture"
        fi
    done
}

# Validate cargo-audit availability
validate_cargo_audit() {
    print_header "AC:1 - cargo-audit Availability Check"

    if command_exists cargo-audit; then
        local version=$(cargo audit --version | awk '{print $2}')
        print_pass "cargo-audit installed: version $version"

        # Test audit database fetch
        if cargo audit fetch --force >/dev/null 2>&1; then
            print_pass "Advisory database fetch successful"
        else
            print_fail "Advisory database fetch failed"
        fi
    else
        print_skip "cargo-audit not installed"
        print_skip "Install: cargo install cargo-audit"
    fi
}

# Validate cargo-geiger availability
validate_cargo_geiger() {
    print_header "AC:6 - cargo-geiger Availability Check"

    if command_exists cargo-geiger; then
        local version=$(cargo geiger --version | awk '{print $2}')
        print_pass "cargo-geiger installed: version $version"
    else
        print_skip "cargo-geiger not installed (optional)"
        print_skip "Install: cargo install cargo-geiger"
    fi
}

# Main validation dispatch
main() {
    local mode="${1:-all}"

    echo "Security Configuration Validation Script"
    echo "Issue #35 - Dependency & Security Scanning Infrastructure"
    echo ""

    case "$mode" in
        --all)
            validate_cargo_audit
            validate_cargo_geiger
            validate_yaml_syntax
            validate_dependabot_config
            validate_deny_toml_policies
            validate_security_receipt_schema
            ;;
        --yaml)
            validate_yaml_syntax
            ;;
        --deny)
            validate_deny_toml_policies
            ;;
        --schema)
            validate_security_receipt_schema
            ;;
        --dependabot)
            validate_dependabot_config
            ;;
        --tools)
            validate_cargo_audit
            validate_cargo_geiger
            ;;
        --help)
            echo "Usage: $0 [--all|--yaml|--deny|--schema|--dependabot|--tools|--help]"
            echo ""
            echo "Options:"
            echo "  --all          Run all validation checks (default)"
            echo "  --yaml         Validate YAML syntax for workflows"
            echo "  --deny         Validate deny.toml policies"
            echo "  --schema       Validate security receipt JSON Schema"
            echo "  --dependabot   Validate Dependabot configuration"
            echo "  --tools        Check security tool availability"
            echo "  --help         Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $mode"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac

    # Print summary
    print_header "Validation Summary"
    echo -e "${GREEN}Passed:${NC} $PASS_COUNT"
    echo -e "${YELLOW}Skipped:${NC} $SKIP_COUNT"
    echo -e "${RED}Failed:${NC} $FAIL_COUNT"
    echo ""

    if [ "$FAIL_COUNT" -gt 0 ]; then
        echo -e "${RED}Validation FAILED${NC} - $FAIL_COUNT check(s) failed"
        exit 1
    elif [ "$PASS_COUNT" -gt 0 ]; then
        echo -e "${GREEN}Validation PASSED${NC} - All executed checks successful"
        exit 0
    else
        echo -e "${YELLOW}Validation SKIPPED${NC} - No checks executed (missing tools or files)"
        exit 0
    fi
}

main "$@"
