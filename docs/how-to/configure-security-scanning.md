# How to Configure Security Scanning Infrastructure
## Issue #35 - Enterprise Dependency & Security Compliance

### Overview

This guide provides step-by-step procedures for implementing and configuring the copybook-rs security scanning infrastructure. Follow these procedures to establish automated vulnerability detection, dependency update management, and compliance audit trails for enterprise mainframe data processing.

**Prerequisites**:
- GitHub repository with copybook-rs workspace
- GitHub Actions enabled
- Administrative access for Dependabot configuration
- `cargo`, `cargo-audit`, `cargo-deny`, `cargo-geiger` installed locally for testing

**Time Estimate**: 2-3 hours for complete implementation and validation

---

## Table of Contents

1. [cargo-audit CI Integration](#1-cargo-audit-ci-integration)
2. [Weekly Security Scanning Workflow](#2-weekly-security-scanning-workflow)
3. [Dependabot Configuration](#3-dependabot-configuration)
4. [Enhanced deny.toml Policies](#4-enhanced-denytoml-policies)
5. [Security Receipt JSON Schema](#5-security-receipt-json-schema)
6. [cargo-geiger Optional Integration](#6-cargo-geiger-optional-integration)
7. [Testing & Validation](#7-testing--validation)
8. [Troubleshooting](#8-troubleshooting)

---

## 1. cargo-audit CI Integration

### 1.1 Install cargo-audit Locally

**Purpose**: Test cargo-audit locally before CI integration.

```bash
# Install cargo-audit via cargo-install
cargo install cargo-audit --locked

# Verify installation
cargo audit --version
# Expected: cargo-audit-audit 0.21.2 (or later)

# Test on copybook-rs workspace
cd /path/to/copybook-rs
cargo audit --json --all-features --workspace --deny warnings
```

**Expected Output**: JSON with `"vulnerabilities": {"found": false, "count": 0}`

### 1.2 Add cargo-audit Job to CI Workflow

**File**: `.github/workflows/ci.yml`

**Location**: Add after existing jobs (e.g., after `deny` job)

```yaml
  security-audit:
    name: Security Audit (cargo-audit)
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install Rust toolchain
        uses: dtolnay/rust-toolchain@stable

      - name: Cache dependencies and advisory DB
        uses: Swatinem/rust-cache@v2
        with:
          shared-key: advisory-db  # Cache advisory DB across jobs

      - name: Install cargo-audit
        uses: taiki-e/install-action@v2
        with:
          tool: cargo-audit

      - name: Fetch advisory database
        run: cargo audit fetch --force

      - name: Run security audit
        id: audit
        run: |
          cargo audit --json --all-features --workspace --deny warnings > audit-output.json
          echo "AUDIT_VERSION=$(cargo audit --version | awk '{print $2}')" >> $GITHUB_ENV

      - name: Generate security receipt
        if: always()
        run: |
          # Generate compliance artifact with full metadata
          cat > security-audit-$(date +%Y%m%d)-${{ github.sha }}.json <<'EOF'
          {
            "version": "1.0",
            "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
            "commit_sha": "${{ github.sha }}",
            "scan_type": "pr-gate",
            "rust_version": "$(rustc --version | awk '{print $2}')",
            "tools": {
              "cargo_audit": "${{ env.AUDIT_VERSION }}",
              "cargo_deny": "$(cargo deny --version 2>/dev/null | awk '{print $2}' || echo 'unknown')"
            },
            "vulnerabilities": $(cat audit-output.json | jq '.vulnerabilities'),
            "exit_status": "${{ steps.audit.outcome == 'success' && 'success' || 'vulnerabilities_found' }}"
          }
          EOF

      - name: Upload security receipt
        uses: actions/upload-artifact@v4
        if: always()
        with:
          name: security-audit-${{ github.sha }}
          path: security-audit-*.json
          retention-days: 90  # Compliance requirement (SOX/HIPAA)
```

### 1.3 Validate cargo-audit Job

```bash
# Create test branch
git checkout -b test/security-audit-integration

# Commit workflow changes
git add .github/workflows/ci.yml
git commit -m "feat(ci): add cargo-audit security scanning job (AC1)"

# Push and create PR
git push -u origin test/security-audit-integration
gh pr create --title "Test: cargo-audit CI Integration" --body "Validation for Issue #35 AC1"

# Watch CI execution
gh pr checks --watch

# Verify security-audit job passes
gh run view <run-id> --log | grep "security-audit"

# Download security receipt artifact
gh run download <run-id> --name security-audit-<sha>
cat security-audit-*.json | jq '.'
```

**Success Criteria**:
- ✅ `security-audit` job completes successfully
- ✅ Security receipt JSON artifact uploaded
- ✅ Artifact retention set to 90 days
- ✅ CI overhead <2 minutes

---

## 2. Weekly Security Scanning Workflow

### 2.1 Create Weekly Scan Workflow

**File**: `.github/workflows/security-scan.yml` (new file)

```yaml
name: Weekly Security Scan

on:
  schedule:
    - cron: '0 3 * * 1'  # Weekly Monday 03:00 UTC
  workflow_dispatch:     # Manual trigger for testing

env:
  CARGO_TERM_COLOR: always

jobs:
  weekly-security-scan:
    name: Weekly Security Scan
    runs-on: ubuntu-latest
    permissions:
      contents: read
      issues: write  # For automated issue creation

    steps:
      - uses: actions/checkout@v4

      - name: Install Rust toolchain
        uses: dtolnay/rust-toolchain@stable

      - name: Cache dependencies and advisory DB
        uses: Swatinem/rust-cache@v2
        with:
          shared-key: advisory-db

      - name: Install cargo-audit
        uses: taiki-e/install-action@v2
        with:
          tool: cargo-audit

      - name: Run security audit
        id: audit
        run: |
          cargo audit fetch --force
          cargo audit --json --all-features --workspace > audit.json

          # Check for vulnerabilities
          VULN_COUNT=$(cat audit.json | jq '.vulnerabilities.count')
          echo "vuln_count=$VULN_COUNT" >> $GITHUB_OUTPUT

          if [ "$VULN_COUNT" -gt 0 ]; then
            echo "status=vulnerabilities_found" >> $GITHUB_OUTPUT
          else
            echo "status=success" >> $GITHUB_OUTPUT
          fi

      - name: Create or update security issue
        if: steps.audit.outputs.vuln_count > 0
        uses: actions/github-script@v7
        with:
          script: |
            const fs = require('fs');
            const audit = JSON.parse(fs.readFileSync('audit.json', 'utf8'));

            const title = `Security Alert: ${new Date().toISOString().split('T')[0]}`;
            const body = `## 🔒 Weekly Security Scan Results\n\n` +
              `**Vulnerabilities Found**: ${audit.vulnerabilities.count}\n\n` +
              `### Affected Crates\n` +
              audit.vulnerabilities.list.map(v =>
                `- **${v.package.name}@${v.package.version}**: ${v.advisory.id} (${v.advisory.severity})\n` +
                `  - ${v.advisory.title}\n` +
                `  - URL: ${v.advisory.url}\n`
              ).join('\n') +
              `\n### Remediation Guidance\n` +
              `1. Review vulnerability details in RustSec advisory database\n` +
              `2. Update affected dependencies via \`cargo update\`\n` +
              `3. If no update available, consider advisory ignore with time-boxed justification\n` +
              `4. Escalate CRITICAL vulnerabilities per SECURITY.md procedures\n\n` +
              `**Scan Timestamp**: ${new Date().toISOString()}\n` +
              `**Commit**: ${process.env.GITHUB_SHA.substring(0, 8)}`;

            // Find existing security alert issue
            const { data: issues } = await github.rest.issues.listForRepo({
              owner: context.repo.owner,
              repo: context.repo.repo,
              state: 'open',
              labels: 'security'
            });

            const existingIssue = issues.find(i => i.title.startsWith('Security Alert:'));

            if (existingIssue) {
              await github.rest.issues.update({
                owner: context.repo.owner,
                repo: context.repo.repo,
                issue_number: existingIssue.number,
                body: body
              });
              console.log(`Updated existing issue #${existingIssue.number}`);
            } else {
              const { data: newIssue } = await github.rest.issues.create({
                owner: context.repo.owner,
                repo: context.repo.repo,
                title: title,
                body: body,
                labels: ['security', 'vulnerability']
              });
              console.log(`Created new issue #${newIssue.number}`);
            }

      - name: Generate security receipt
        if: always()
        run: |
          # Generate compliance artifact
          cat > security-scan-$(date +%Y%m%d)-${{ github.sha }}.json <<'EOF'
          {
            "version": "1.0",
            "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
            "commit_sha": "${{ github.sha }}",
            "scan_type": "weekly-scan",
            "rust_version": "$(rustc --version | awk '{print $2}')",
            "tools": {
              "cargo_audit": "$(cargo audit --version | awk '{print $2}')"
            },
            "vulnerabilities": $(cat audit.json | jq '.vulnerabilities'),
            "exit_status": "${{ steps.audit.outputs.status }}"
          }
          EOF

      - name: Upload security receipt
        uses: actions/upload-artifact@v4
        if: always()
        with:
          name: security-scan-${{ github.sha }}
          path: security-scan-*.json
          retention-days: 90
```

### 2.2 Test Weekly Scan Workflow

```bash
# Manually trigger workflow
gh workflow run security-scan.yml

# Watch execution
gh run watch

# Verify workflow completed
gh run list --workflow=security-scan.yml --limit 1

# Check for security issues created (if vulnerabilities found)
gh issue list --label security

# Download security receipt
gh run download <run-id> --name security-scan-<sha>
cat security-scan-*.json | jq '.'
```

**Success Criteria**:
- ✅ Workflow executes successfully on manual trigger
- ✅ Security receipt generated and uploaded
- ✅ GitHub issue created if vulnerabilities found
- ✅ Issue body includes remediation guidance

---

## 3. Dependabot Configuration

### 3.1 Create Dependabot Configuration

**File**: `.github/dependabot.yml` (new file)

```yaml
version: 2
updates:
  # Cargo dependencies - daily schedule with grouped patch updates
  - package-ecosystem: "cargo"
    directory: "/"
    schedule:
      interval: "daily"
      time: "03:00"  # Off-peak UTC
    open-pull-requests-limit: 10
    labels:
      - "dependencies"
      - "rust"
    commit-message:
      prefix: "chore(deps)"
      include: "scope"
    groups:
      patch-updates:
        patterns:
          - "*"
        update-types:
          - "patch"
    assignees:
      - "security-team"  # Replace with your team/user
    reviewers:
      - "security-team"  # Replace with your team/user

  # GitHub Actions - weekly security-focused updates
  - package-ecosystem: "github-actions"
    directory: "/"
    schedule:
      interval: "weekly"
      day: "monday"
      time: "03:00"
    open-pull-requests-limit: 5
    labels:
      - "ci"
      - "github-actions"
    commit-message:
      prefix: "chore(ci)"
```

### 3.2 Validate Dependabot Configuration

```bash
# Validate YAML syntax locally
cat .github/dependabot.yml | yq eval '.' -

# Verify configuration structure
cat .github/dependabot.yml | yq eval '.version' -
# Expected: 2

cat .github/dependabot.yml | yq eval '.updates[] | .package-ecosystem' -
# Expected: cargo, github-actions

# Verify grouping configuration
cat .github/dependabot.yml | yq eval '.updates[0].groups' -
# Expected: patch-updates group defined

# Commit and merge to main branch (Dependabot only runs on default branch)
git add .github/dependabot.yml
git commit -m "feat(deps): configure Dependabot for Cargo and GitHub Actions (AC3)"
git push origin main
```

### 3.3 Monitor First Dependabot PRs

**Timeline**: First PRs typically appear within 24-48 hours after merge to main.

```bash
# Check for Dependabot PRs
gh pr list --label dependencies --limit 10

# Verify PR format
gh pr view <pr-number>
# Expected:
# - Title: "chore(deps): update <crate> dependencies"
# - Labels: dependencies, rust
# - Conventional commit message

# Monitor first week for PR volume
# If overwhelming (>10 open PRs), adjust schedule:
# - Change interval from "daily" to "weekly"
# - Reduce open-pull-requests-limit
```

**Success Criteria**:
- ✅ Dependabot PRs created within 48 hours
- ✅ Patch updates grouped into single PR
- ✅ PR count within configured limits (10 Cargo, 5 Actions)
- ✅ Conventional commit format applied

---

## 4. Enhanced deny.toml Policies

### 4.1 Current deny.toml Baseline

**Before making changes, capture current baseline**:

```bash
# Verify current workspace passes existing policies
cargo deny check advisories
cargo deny check bans
cargo deny check sources

# Check for wildcards (should be none)
grep -r '\*' */Cargo.toml || echo "✅ No wildcards found"

# Check for yanked dependencies (should be none)
cargo tree --workspace | grep -i yank || echo "✅ No yanked crates"

# Validate registry sources (should be crates.io only)
cargo metadata --format-version=1 | jq '.packages[].source' | sort -u
# Expected: "registry+https://github.com/rust-lang/crates.io-index"
```

### 4.2 Apply Enhanced Policies

**File**: `deny.toml`

**Changes** (apply incrementally for safety):

```toml
# Configuration for cargo-deny
# https://embarkstudios.github.io/cargo-deny/

[graph]
targets = [
    { triple = "x86_64-unknown-linux-gnu" },
    { triple = "x86_64-pc-windows-msvc" },
    { triple = "x86_64-apple-darwin" },
]

[advisories]
# The path where the advisory database is cloned/fetched into
db-path = "~/.cargo/advisory-db"
# The url(s) of the advisory databases to use
db-urls = ["https://github.com/rustsec/advisory-db"]

# Enterprise mainframe data processing requires zero-tolerance for security vulnerabilities
vulnerability = "deny"  # AC4: Explicit HIGH/CRITICAL denial (was implicit)
yanked = "deny"         # AC4: Upgrade from "warn" - no yanked crates in production
unsound = "deny"        # AC4: New - reject soundness issues for enterprise reliability
notice = "warn"         # AC4: Track informational advisories for compliance reporting

# The lint level for unmaintained crates
unmaintained = "workspace"

# A list of advisory IDs to ignore (use for false positives with time-boxed expiration)
ignore = [
    # Add any specific advisories to ignore here
    # Example time-boxed ignore:
    # { id = "RUSTSEC-2024-XXXX", reason = "False positive - feature not used", expires = "2025-12-31" }
]

[licenses]
# The confidence threshold for detecting a license from a license text.
confidence-threshold = 0.8
# List of explicitly allowed licenses
allow = [
    "MIT",
    "Apache-2.0",
    "Apache-2.0 WITH LLVM-exception",
    "BSD-2-Clause",
    "Unicode-3.0",
    "AGPL-3.0-or-later",
]

[bans]
# Lint level for when multiple versions of the same crate are detected
multiple-versions = "warn"

# Deterministic builds required for regulatory compliance (SOX, PCI DSS)
wildcards = "deny"      # AC4: Upgrade from "allow" - prevent non-deterministic dependency resolution

# The graph highlighting used when creating dotgraphs for crates
highlight = "all"
# List of crates that are allowed
allow = []
# List of crates to deny
deny = [
    # Deny old versions of crates with known security issues
    { name = "openssl", version = "<0.10.55" },
]
# Certain crates/versions that will be skipped when doing duplicate detection
skip = [
    { name = "windows-sys", reason = "transitive, multiple versions unavoidable" },
    { name = "windows-targets", reason = "transitive, multiple versions unavoidable" },
    { name = "windows-link", reason = "transitive, multiple versions unavoidable" },
    { name = "windows_x86_64_gnu", reason = "transitive, multiple versions unavoidable" },
    { name = "windows_x86_64_msvc", reason = "transitive, multiple versions unavoidable" },
]
# Similarly to `skip` allows you to skip certain crates from being checked
skip-tree = []

[sources]
# Supply chain security for mainframe data processing trustworthiness
unknown-registry = "deny"  # AC4: Upgrade from "warn" - only trusted crates.io registry
unknown-git = "deny"       # AC4: Upgrade from "warn" - no untrusted git dependencies

# List of allowed registries
allow-registry = ["https://github.com/rust-lang/crates.io-index"]
# List of allowed Git repositories
allow-git = []
```

### 4.3 Validate Enhanced Policies

```bash
# Test enhanced policies against current workspace
cargo deny check advisories
# Expected: Pass (no vulnerabilities, no yanked crates, no unsound advisories)

cargo deny check bans
# Expected: Pass (no wildcard dependencies)

cargo deny check sources
# Expected: Pass (crates.io only)

# Negative test: Create test branch with yanked dependency
git checkout -b test/deny-yanked-validation
# (Temporarily add yanked crate to Cargo.toml for testing)
cargo deny check advisories
# Expected: Fail with error message about yanked crate

# Negative test: Wildcard dependency
git checkout -b test/deny-wildcard-validation
# (Add dependency = "*" to Cargo.toml for testing)
cargo deny check bans
# Expected: Fail with error message about wildcard

# Clean up test branches
git checkout main
git branch -D test/deny-yanked-validation test/deny-wildcard-validation
```

**Success Criteria**:
- ✅ All 7 enhanced policies configured
- ✅ Current workspace passes all checks
- ✅ Negative tests validate policy enforcement
- ✅ Documentation comments explain enterprise rationale

---

## 5. Security Receipt JSON Schema

### 5.1 Create JSON Schema Document

**File**: `docs/reference/security-receipt-schema.json` (see section below for full schema)

```bash
# Create reference directory if not exists
mkdir -p docs/reference

# Create schema file (content provided in reference documentation)
# See docs/reference/security-receipt-schema.md for full schema
```

### 5.2 Validate JSON Schema

```bash
# Install JSON Schema validator
pip install check-jsonschema

# Validate schema metaschema compliance
check-jsonschema --check-metaschema docs/reference/security-receipt-schema.json
# Expected: Schema is valid

# Create test security receipt
cat > test-receipt.json <<'EOF'
{
  "version": "1.0",
  "timestamp": "2025-10-02T12:00:00Z",
  "commit_sha": "abc123def456abc123def456abc123def456abc1",
  "scan_type": "pr-gate",
  "rust_version": "1.90.0",
  "tools": {
    "cargo_audit": "0.21.2",
    "cargo_deny": "0.16.0"
  },
  "vulnerabilities": {
    "count": 0,
    "by_severity": {
      "critical": 0,
      "high": 0,
      "medium": 0,
      "low": 0
    },
    "advisories": []
  },
  "exit_status": "success"
}
EOF

# Validate test receipt against schema
check-jsonschema --schemafile docs/reference/security-receipt-schema.json test-receipt.json
# Expected: Validation succeeded

# Clean up test receipt
rm test-receipt.json
```

**Success Criteria**:
- ✅ JSON Schema passes metaschema validation
- ✅ Test receipts validate successfully
- ✅ Schema includes all required fields
- ✅ Schema documentation complete

---

## 6. cargo-geiger Optional Integration

### 6.1 Install cargo-geiger Locally

```bash
# Install cargo-geiger
cargo install cargo-geiger --locked

# Verify installation
cargo geiger --version
# Expected: cargo-geiger 0.13.0 (or later)

# Test on copybook-rs workspace
cargo geiger --output-format Json --workspace --all-features > geiger.json

# Validate zero unsafe code
cat geiger.json | jq '.packages[] | select(.unsafety.used.functions > 0 or .unsafety.used.exprs > 0)'
# Expected: no output (zero unsafe code)

# Generate human-readable report
cargo geiger --output-format GitHubMarkdown --workspace --all-features
```

### 6.2 Add cargo-geiger Optional Job to CI

**File**: `.github/workflows/ci.yml`

**Location**: Add after `security-audit` job

```yaml
  cargo-geiger-check:
    name: Unsafe Code Enforcement
    runs-on: ubuntu-latest
    continue-on-error: true  # Optional during 30-day validation period

    steps:
      - uses: actions/checkout@v4

      - name: Install Rust toolchain
        uses: dtolnay/rust-toolchain@stable

      - name: Cache dependencies
        uses: Swatinem/rust-cache@v2

      - name: Install cargo-geiger
        uses: taiki-e/install-action@v2
        with:
          tool: cargo-geiger

      - name: Scan for unsafe code
        id: geiger
        run: |
          cargo geiger --output-format Json --workspace --all-features > geiger.json

          # Validate zero unsafe code (expected: 0 unsafe functions/expressions)
          cargo geiger --output-format GitHubMarkdown --workspace --all-features >> $GITHUB_STEP_SUMMARY

          # Extract metrics
          echo "UNSAFE_FUNCTIONS=$(cat geiger.json | jq '[.packages[].unsafety.used.functions] | add')" >> $GITHUB_ENV
          echo "UNSAFE_EXPRS=$(cat geiger.json | jq '[.packages[].unsafety.used.exprs] | add')" >> $GITHUB_ENV

      - name: Validate zero unsafe code
        run: |
          if [ "$UNSAFE_FUNCTIONS" -gt 0 ] || [ "$UNSAFE_EXPRS" -gt 0 ]; then
            echo "❌ Unsafe code detected: $UNSAFE_FUNCTIONS functions, $UNSAFE_EXPRS expressions"
            exit 1
          else
            echo "✅ Zero unsafe code validated"
          fi

      - name: Upload metrics artifact
        uses: actions/upload-artifact@v4
        if: always()
        with:
          name: unsafe-code-metrics-${{ github.sha }}
          path: geiger.json
          retention-days: 90
```

### 6.3 Graduation to Required Status (After 30 Days)

**After 30-day validation period**:

```yaml
# Remove `continue-on-error: true` line from cargo-geiger-check job
# This makes the job required for PR merge

cargo-geiger-check:
  name: Unsafe Code Enforcement
  runs-on: ubuntu-latest
  # continue-on-error: true  # REMOVED after 30-day validation
```

**Success Criteria**:
- ✅ cargo-geiger job runs successfully
- ✅ Zero unsafe code validated (0 functions, 0 expressions)
- ✅ Metrics artifact uploaded with 90-day retention
- ✅ GitHub Actions summary includes human-readable report
- ⏱️ 30-day validation period completed before graduation

---

## 7. Measuring CI Performance Overhead (AC7)

**Performance Budget**: <2 minutes additional CI overhead

### 7.1 Validation Procedure

**Baseline Measurement** (before security enhancements):
```bash
# Get recent CI run without security scanning
gh run view <baseline-run-id> --json jobs --jq '.jobs[] | {name: .name, duration: .completed_at - .started_at}'
```

**Enhanced Measurement** (after security-audit job):
```bash
# Get recent CI run with security scanning
gh run view <enhanced-run-id> --json jobs --jq '.jobs[] | select(.name == "security-audit") | {duration: .completed_at - .started_at}'

# Expected: ~45-60 seconds (well within <2 minute budget)
```

### 7.2 Optimization Strategies

**Advisory DB Caching**:
- Enabled via `shared-key: advisory-db` in ci.yml
- Reduces cargo-audit overhead by ~30 seconds
- Shared across all CI jobs using cargo

**Parallel Execution**:
- Security-audit job runs independently
- No blocking of other CI jobs (test, fmt, clippy, etc.)
- Total CI duration minimally impacted

### 7.3 Performance Monitoring

**Alert Thresholds**:
- security-audit job duration > 120 seconds → investigate caching
- Total CI overhead > 2 minutes → review optimization strategies

---

## 8. Testing & Validation

### 8.1 Comprehensive Integration Test Scenarios

**Scenario 1: PR Quality Gate (cargo-audit)**

```bash
# Create test PR with security changes
git checkout -b test/security-integration-ac1
git add .github/workflows/ci.yml
git commit -m "feat(ci): add cargo-audit security scanning (AC1)"
git push -u origin test/security-integration-ac1

# Create PR and watch checks
gh pr create --title "Test: Security Audit Integration (AC1)" --body "Validation for Issue #35"
gh pr checks --watch

# Verify security-audit job passes
# Download and validate security receipt
```

**Scenario 2: Weekly Scan Trigger**

```bash
# Manually trigger weekly scan
gh workflow run security-scan.yml

# Watch execution
gh run watch

# Verify security receipt generated
gh run download <run-id> --name security-scan-<sha>
cat security-scan-*.json | jq '.'
```

**Scenario 3: Dependabot PR Observation**

```bash
# After merging Dependabot config, wait 24-48 hours
# Check for Dependabot PRs
gh pr list --label dependencies

# Verify first PR has correct format
gh pr view <dependabot-pr-number>
```

**Scenario 4: deny.toml Policy Enforcement**

```bash
# Negative test: Yanked dependency
git checkout -b test/deny-yanked
# (Add yanked crate to Cargo.toml)
cargo deny check advisories
# Expected: Fail

# Negative test: Wildcard dependency
git checkout -b test/deny-wildcard
# (Add dep = "*" to Cargo.toml)
cargo deny check bans
# Expected: Fail
```

**Scenario 5: Security Receipt Schema Validation**

```bash
# Download real security receipt from CI
gh run download <run-id> --name security-audit-<sha>

# Validate against schema
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 security-audit-*.json
# Expected: Validation succeeded
```

**Scenario 6: cargo-geiger Zero Unsafe Code**

```bash
# Local validation
cargo geiger --workspace --all-features | grep "0/0"
# Expected: All crates show 0/0 (zero unsafe code)

# CI validation
gh run view <run-id> --log | grep "cargo-geiger"
# Verify job passes or is optional
```

### 8.2 Performance Budget Validation

```bash
# Record baseline CI duration (before security enhancements)
gh run list --workflow=ci.yml --limit 1 --json databaseId,startedAt,updatedAt
BASELINE_RUN_ID=<run-id>

gh run view $BASELINE_RUN_ID --json jobs --jq '.jobs[] | {name, startedAt, completedAt}' > baseline-timing.json

# After implementing security jobs, measure new duration
ENHANCED_RUN_ID=<run-id>
gh run view $ENHANCED_RUN_ID --json jobs --jq '.jobs[] | {name, startedAt, completedAt}' > enhanced-timing.json

# Calculate overhead (manual comparison)
# Expected: <2 minutes additional overhead
```

---

## 9. Validating Security Receipts Against JSON Schema (AC5)

copybook-rs security receipts conform to JSON Schema for compliance validation.

### 9.1 Prerequisites

```bash
# Install JSON Schema validator
pip install check-jsonschema
# OR
npm install -g ajv-cli
```

### 9.2 Validation Procedure

**Validate security receipt from CI artifact**:
```bash
# Download artifact from GitHub Actions
gh run download <run-id> --name security-audit-<commit-sha>

# Validate against schema
check-jsonschema \
  --schemafile /home/steven/code/Rust/copybook-rs/docs/reference/security-receipt-schema.json \
  security-audit-*.json

# Expected output: "✅ validation success"
```

**Validate test fixtures**:
```bash
# Clean scan fixture (if available)
check-jsonschema \
  --schemafile /home/steven/code/Rust/copybook-rs/docs/reference/security-receipt-schema.json \
  tests/fixtures/security-scanning/receipts/clean-scan.json

# Vulnerabilities found fixture (if available)
check-jsonschema \
  --schemafile /home/steven/code/Rust/copybook-rs/docs/reference/security-receipt-schema.json \
  tests/fixtures/security-scanning/receipts/vulnerabilities-found.json
```

### 9.3 Compliance Use Cases

**SOX Audit Trail**:
- Validate 90-day retention compliance
- Verify timestamp, commit SHA, tool versions present
- Confirm deterministic build metadata

**HIPAA Vulnerability Response**:
- Extract vulnerability severity and advisory IDs
- Verify automated issue tracking integration
- Confirm <48hr response timeline adherence

---

## 10. Rollback & Emergency Procedures (AC10)

### 10.1 Scenario 1: False Positive Blocking PR

**Symptoms**: cargo-audit fails on false positive advisory

**Resolution**:
1. Verify advisory is false positive (check RustSec database)
2. Add time-boxed ignore to `deny.toml`:
   ```toml
   [advisories]
   ignore = [
     { id = "RUSTSEC-2024-XXXX", reason = "False positive for unused feature", expires = "2025-12-31" }
   ]
   ```
3. Commit with justification in PR description
4. Schedule follow-up to remove ignore after advisory resolution

### 10.2 Scenario 2: Critical Vulnerability Requires Emergency Disable

**Symptoms**: Widespread vulnerability blocking all PRs, fix not yet available

**Resolution**:
1. Temporary disable security-audit job:
   ```yaml
   # .github/workflows/ci.yml
   security-audit:
     if: false  # EMERGENCY DISABLE - Ticket #XXX
   ```
2. Create tracking issue for re-enablement
3. Notify security team with timeline for fix
4. Re-enable after vulnerability patched or mitigated

### 10.3 Scenario 3: Dependabot PR Overload

**Symptoms**: Too many dependency update PRs overwhelming review capacity

**Resolution**:
1. Adjust PR limits in `.github/dependabot.yml`:
   ```yaml
   - package-ecosystem: "cargo"
     open-pull-requests-limit: 5  # Reduce from 10
   ```
2. Close non-security PRs temporarily
3. Batch review security updates first
4. Re-enable routine updates after backlog cleared

### 10.4 Scenario 4: Weekly Scan Creating Duplicate Issues

**Symptoms**: Multiple "Security Alert:" prefixed issues created

**Resolution**:
1. Check GitHub issue automation logic in security-scan.yml
2. Manually close duplicate issues
3. Verify issue title matching: `const title = "Security Alert: ${date}";`
4. If persistent, disable weekly scan temporarily and investigate

---

## 11. Troubleshooting

### 11.1 cargo-audit Fails with Advisory Database Error

**Symptom**: `cargo audit fetch --force` fails with network error.

**Solution**:
```bash
# Check advisory DB cache
ls -la ~/.cargo/advisory-db/

# Manual refresh
rm -rf ~/.cargo/advisory-db/
cargo audit fetch --force

# CI workaround: Add retry logic
- name: Fetch advisory database
  run: |
    for i in 1 2 3; do
      cargo audit fetch --force && break
      echo "Retry $i/3..."
      sleep 5
    done
```

### 11.2 Dependabot PRs Overwhelming

**Symptom**: >10 open Dependabot PRs, maintainer review burden excessive.

**Solution**:
```yaml
# Adjust .github/dependabot.yml schedule
- package-ecosystem: "cargo"
  schedule:
    interval: "weekly"  # Change from "daily"
    day: "monday"
```

**Emergency Disable**:
1. Navigate to GitHub repository → Settings → Code security and analysis
2. Scroll to "Dependabot version updates"
3. Click "Pause" to temporarily disable
4. Adjust configuration, then re-enable

### 11.3 deny.toml Policy Rejection

**Symptom**: `cargo deny check` fails after applying stricter policies.

**Solution**:
```bash
# Identify failing dependency
cargo deny check advisories --log-level debug

# Option 1: Update dependency
cargo update <crate-name>

# Option 2: Time-boxed ignore (false positive)
# Edit deny.toml [advisories] section:
ignore = [
    { id = "RUSTSEC-2024-XXXX", reason = "False positive - feature not used", expires = "2025-12-31" }
]

# Option 3: Rollback policy (temporary)
git revert <commit-sha>
# Create issue tracking policy enhancement blocker
```

### 11.4 Security Receipt Schema Validation Fails

**Symptom**: `check-jsonschema` fails with validation errors.

**Solution**:
```bash
# Check schema itself
check-jsonschema --check-metaschema docs/reference/security-receipt-schema.json

# Identify invalid field
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 security-audit-*.json 2>&1 | grep "ValidationError"

# Common issues:
# - Missing required field (add to receipt generation)
# - Invalid timestamp format (use ISO 8601: date -u +%Y-%m-%dT%H:%M:%SZ)
# - Invalid commit SHA (ensure full 40-character SHA)
```

### 11.5 cargo-geiger Performance Issues

**Symptom**: cargo-geiger job exceeds 2-minute budget.

**Solution**:
```yaml
# Keep as optional job indefinitely
cargo-geiger-check:
  continue-on-error: true  # Keep optional to prevent PR blocking

# Or move to weekly scan only
# Remove from ci.yml, add to security-scan.yml
```

### 11.6 CI Performance Budget Exceeded

**Symptom**: Security jobs add >2 minutes overhead to CI.

**Solution**:
```bash
# Verify advisory DB caching
grep "shared-key: advisory-db" .github/workflows/ci.yml .github/workflows/security-scan.yml

# Check cache hit rate in CI logs
gh run view <run-id> --log | grep "rust-cache"

# If caching ineffective:
# - Move cargo-audit to weekly scan only (remove from ci.yml)
# - Keep deny check (faster, <45 sec)
# - Document overhead exception with justification
```

---

## 12. Emergency Procedures

### 12.1 Emergency Disable: cargo-audit CI Gate

**When**: CRITICAL vulnerability requires immediate production deployment, but cargo-audit blocks CI.

**Procedure**:
```bash
# Option 1: Time-boxed advisory ignore
# Edit deny.toml:
[advisories]
ignore = [
    { id = "RUSTSEC-2024-XXXX", reason = "CRITICAL: Production deployment required, fix tracked in #<issue>", expires = "2025-10-09" }
]

# Commit and push
git add deny.toml
git commit -m "fix(security): time-boxed ignore for RUSTSEC-2024-XXXX (7-day emergency)"
git push

# Option 2: Temporarily disable cargo-audit job
# Edit .github/workflows/ci.yml:
security-audit:
  if: false  # TEMPORARY: Emergency disable - remove after fix

# Create tracking issue
gh issue create --title "Re-enable cargo-audit after RUSTSEC-2024-XXXX resolution" \
                --label security,high-priority \
                --body "Emergency disable: <justification>"
```

### 12.2 Ad-Hoc Security Receipt Generation

**When**: Compliance audit requires security receipt for specific commit.

**Procedure**:
```bash
# Checkout target commit
git checkout <commit-sha>

# Run cargo-audit locally
cargo audit --json --all-features --workspace > audit.json

# Generate security receipt manually
cat > security-audit-adhoc-$(date +%Y%m%d)-<commit-sha>.json <<EOF
{
  "version": "1.0",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "commit_sha": "<commit-sha>",
  "scan_type": "manual",
  "rust_version": "$(rustc --version | awk '{print $2}')",
  "tools": {
    "cargo_audit": "$(cargo audit --version | awk '{print $2}')"
  },
  "vulnerabilities": $(cat audit.json | jq '.vulnerabilities'),
  "exit_status": "success"
}
EOF

# Validate against schema
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 security-audit-adhoc-*.json

# Provide to compliance officer
```

---

## Related Documentation

- **Architecture Overview**: [security-scanning-architecture.md](../explanation/security-scanning-architecture.md)
- **API Reference**: [security-receipt-schema.md](../reference/security-receipt-schema.md)
- **Security Policy**: [SECURITY.md](../../SECURITY.md)
- **Issue Tracking**: [GitHub Issue #35](https://github.com/EffortlessMetrics/copybook-rs/issues/35)

---

**Document Status**: ✅ HOW-TO GUIDE COMPLETE - Ready for implementation (Issue #35)
**Last Updated**: 2025-10-02
**Guide Version**: 1.0
**Authors**: copybook-rs spec-creator agent (generative flow)
