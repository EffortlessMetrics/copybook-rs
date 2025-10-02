# Manual Security Scanning Validation Procedures
## Issue #35 - Dependency & Security Scanning Infrastructure

**Purpose**: Manual validation procedures for security scanning features that cannot be fully automated in test suites.

**Specifications**:
- Architecture: `docs/explanation/security-scanning-architecture.md`
- How-To Guide: `docs/how-to/configure-security-scanning.md`
- Acceptance Criteria: `docs/issue-35-ac-finalized.md`

---

## Table of Contents

1. [AC2: Weekly Security Scan Trigger and Issue Creation](#ac2-weekly-security-scan-trigger-and-issue-creation)
2. [AC3: Dependabot PR Observation](#ac3-dependabot-pr-observation)
3. [AC7: CI Performance Overhead Measurement](#ac7-ci-performance-overhead-measurement)
4. [AC8: Documentation Completeness Review](#ac8-documentation-completeness-review)
5. [AC9: End-to-End CI Integration Validation](#ac9-end-to-end-ci-integration-validation)
6. [AC10: Rollback Procedure Testing](#ac10-rollback-procedure-testing)

---

## AC2: Weekly Security Scan Trigger and Issue Creation

### Test Spec
`docs/explanation/security-scanning-architecture.md#2-weekly-security-scanning-workflow`

### Prerequisites
- `.github/workflows/security-scan.yml` created and merged to main branch
- GitHub Actions enabled in repository
- Workflow has `workflow_dispatch` trigger for manual testing

### Validation Procedure

#### Step 1: Manual Workflow Trigger
```bash
# Trigger weekly security scan workflow manually
gh workflow run security-scan.yml

# Verify workflow is queued
gh run list --workflow=security-scan.yml --limit 1

# Watch workflow execution
gh run watch
```

**Success Criteria**:
- [ ] Workflow triggers successfully via manual dispatch
- [ ] Workflow executes without errors
- [ ] Workflow completes within expected time (< 2 minutes with caching)

#### Step 2: Security Receipt Artifact Validation
```bash
# Get latest run ID
RUN_ID=$(gh run list --workflow=security-scan.yml --limit 1 --json databaseId --jq '.[0].databaseId')

# Download security receipt artifact
gh run download $RUN_ID --name security-scan-<sha>

# Validate receipt against JSON Schema
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 security-scan-*.json

# Verify 90-day retention policy
gh run view $RUN_ID --json artifacts --jq '.artifacts[] | select(.name | startswith("security-scan")) | .retentionDays'
```

**Success Criteria**:
- [ ] Security receipt artifact uploaded successfully
- [ ] Receipt validates against JSON Schema
- [ ] Artifact retention set to 90 days (compliance requirement)

#### Step 3: GitHub Issue Creation (if vulnerabilities found)

**Note**: This step can only be validated if vulnerabilities are detected.

```bash
# Check for security issues created by workflow
gh issue list --label security --limit 5

# View latest security alert issue
gh issue view <issue-number>
```

**Success Criteria**:
- [ ] GitHub issue created automatically when vulnerabilities found
- [ ] Issue title follows format: "Security Alert: YYYY-MM-DD"
- [ ] Issue body includes vulnerability count, affected crates, and remediation guidance
- [ ] Issue labels include "security" and "vulnerability"
- [ ] Existing security issue is updated (not duplicated) on subsequent scans

#### Step 4: Scheduled Execution Validation

**Timeline**: Wait until Monday 03:00 UTC (scheduled cron execution)

```bash
# Check workflow runs from schedule trigger
gh run list --workflow=security-scan.yml --limit 5 --json event,startedAt

# Verify scheduled runs execute automatically
gh run list --workflow=security-scan.yml --json event --jq '.[] | select(.event == "schedule")'
```

**Success Criteria**:
- [ ] Workflow executes automatically on Monday 03:00 UTC
- [ ] Scheduled executions do not require manual intervention
- [ ] Security receipts are generated for all scheduled runs

---

## AC3: Dependabot PR Observation

### Test Spec
`docs/explanation/security-scanning-architecture.md#3-dependabot-configuration`

### Prerequisites
- `.github/dependabot.yml` created and merged to main branch
- Dependabot enabled in repository settings
- At least 24-48 hours elapsed since merge (Dependabot initial scan)

### Validation Procedure

#### Step 1: Dependabot PR Detection
```bash
# Check for Dependabot PRs (wait 24-48 hours after config merge)
gh pr list --label dependencies --limit 10

# Filter for Cargo dependencies
gh pr list --label dependencies,rust --limit 10

# Filter for GitHub Actions dependencies
gh pr list --label ci,github-actions --limit 5
```

**Success Criteria**:
- [ ] Dependabot PRs appear within 24-48 hours of configuration merge
- [ ] PRs have correct labels: "dependencies", "rust" (for Cargo) or "ci", "github-actions" (for Actions)

#### Step 2: Patch Update Grouping Validation
```bash
# View first Dependabot PR for Cargo patch updates
gh pr view <dependabot-pr-number>

# Check PR title format
# Expected: "chore(deps): update <group> dependencies" or "chore(deps): bump <crate> from X to Y"

# View PR body to confirm grouped patch updates
gh pr view <dependabot-pr-number> --json body --jq '.body'
```

**Success Criteria**:
- [ ] Patch updates are grouped into single PR (if multiple patches available)
- [ ] PR title uses conventional commit format: "chore(deps): ..."
- [ ] PR body lists all updated crates (for grouped updates)

#### Step 3: PR Limit Validation

**Timeline**: Monitor for first 7 days after configuration merge

```bash
# Count open Dependabot PRs
gh pr list --label dependencies --state open --json number | jq 'length'

# Expected: ≤10 Cargo PRs + ≤5 Actions PRs
```

**Success Criteria**:
- [ ] Open Cargo dependency PRs ≤ 10 (configured limit)
- [ ] Open GitHub Actions PRs ≤ 5 (configured limit)
- [ ] Total open Dependabot PRs within reasonable limits (not overwhelming)

#### Step 4: First 30-Day Monitoring

**Timeline**: Monitor Dependabot activity for 30 days

**Monitoring Checklist**:
- [ ] Daily schedule produces manageable PR volume
- [ ] Grouped patch updates reduce PR noise effectively
- [ ] Security updates are prioritized appropriately
- [ ] No critical issues with Dependabot configuration

**Adjustment Procedure** (if needed):
```bash
# If PR volume overwhelming, adjust schedule to weekly
# Edit .github/dependabot.yml:
#   schedule:
#     interval: "weekly"  # Change from "daily"
#     day: "monday"

# Or reduce PR limits:
#   open-pull-requests-limit: 5  # Reduce from 10

# Emergency disable via GitHub UI:
# Repository → Settings → Code security and analysis → Dependabot version updates → Pause
```

---

## AC7: CI Performance Overhead Measurement

### Test Spec
`docs/explanation/security-scanning-architecture.md#performance-budget--optimization`

### Prerequisites
- Baseline CI run before security enhancements (capture run ID)
- Enhanced CI run after security jobs added (capture run ID)
- GitHub CLI with access to workflow runs

### Validation Procedure

#### Step 1: Capture Baseline CI Timing
```bash
# Identify last CI run BEFORE security enhancements
BASELINE_RUN_ID=$(gh run list --workflow=ci.yml --limit 10 --json databaseId,createdAt | \
                  jq '.[] | select(.createdAt < "YYYY-MM-DD") | .databaseId' | head -1)

# Extract job timing data
gh run view $BASELINE_RUN_ID --json jobs > baseline-ci-timing.json

# Calculate total wall time
cat baseline-ci-timing.json | jq '.jobs | map(.completedAt - .startedAt) | max'

# Extract per-job durations
cat baseline-ci-timing.json | jq '.jobs[] | {name, duration: (.completedAt - .startedAt)}'
```

**Baseline Expected**:
- test: ~8-12 min
- fmt: ~30 sec
- clippy: ~3-5 min
- deny: ~45 sec
- docs: ~2-3 min
- coverage: ~10-12 min
- **Total wall time**: ~12-15 min (parallelized)

#### Step 2: Capture Enhanced CI Timing
```bash
# Identify first CI run AFTER security enhancements
ENHANCED_RUN_ID=$(gh run list --workflow=ci.yml --limit 1 --json databaseId --jq '.[0].databaseId')

# Extract enhanced job timing data
gh run view $ENHANCED_RUN_ID --json jobs > enhanced-ci-timing.json

# Calculate total wall time
cat enhanced-ci-timing.json | jq '.jobs | map(.completedAt - .startedAt) | max'

# Extract security job durations
cat enhanced-ci-timing.json | jq '.jobs[] | select(.name | contains("security") or contains("audit") or contains("geiger")) | {name, duration: (.completedAt - .startedAt)}'
```

**Enhanced Expected**:
- security-audit: ~45 sec (with advisory DB cache)
- cargo-geiger-check: ~2-3 min (optional, doesn't block)
- **Additional overhead**: ~0.75 min blocking (cargo-audit only)

#### Step 3: Calculate Overhead
```bash
# Compare baseline vs enhanced total wall time
BASELINE_TIME=$(cat baseline-ci-timing.json | jq '.jobs | map(.completedAt - .startedAt) | max')
ENHANCED_TIME=$(cat enhanced-ci-timing.json | jq '.jobs | map(.completedAt - .startedAt) | max')

echo "Baseline CI: $BASELINE_TIME seconds"
echo "Enhanced CI: $ENHANCED_TIME seconds"
echo "Overhead: $((ENHANCED_TIME - BASELINE_TIME)) seconds"
```

**Success Criteria**:
- [ ] Security job overhead < 2 minutes (120 seconds) blocking time
- [ ] cargo-audit job completes in ~45 seconds with caching
- [ ] cargo-geiger job (if present) is optional and doesn't block PRs
- [ ] Total CI wall time increase within acceptable limits

#### Step 4: Advisory DB Cache Hit Rate Validation
```bash
# Check cache hit rate in CI logs
gh run view $ENHANCED_RUN_ID --log | grep "rust-cache" | grep "cache hit"

# Verify advisory DB caching strategy
grep "shared-key: advisory-db" .github/workflows/ci.yml .github/workflows/security-scan.yml
```

**Success Criteria**:
- [ ] Advisory DB cache hits on subsequent runs (> 80% hit rate target)
- [ ] Shared cache key `advisory-db` configured in workflows
- [ ] Cache reduces cargo-audit overhead to ~2 seconds (from ~45 seconds cold)

---

## AC8: Documentation Completeness Review

### Test Spec
`docs/issue-35-ac-finalized.md#ac8-documentation-updates`

### Validation Procedure

#### Checklist: Required Documentation Files

**SECURITY.md Updates**:
- [ ] Section added: "Security Scanning Infrastructure"
- [ ] cargo-audit CI integration described
- [ ] Weekly security scan workflow documented
- [ ] Vulnerability disclosure timeline: 90 days (matching artifact retention)
- [ ] False positive handling procedure documented

**README.md Updates**:
- [ ] Badges added: cargo-audit, cargo-deny status
- [ ] Security scanning infrastructure mentioned in features section
- [ ] Link to SECURITY.md for security policies

**How-To Guide** (`docs/how-to/configure-security-scanning.md`):
- [ ] Scanning frequency documented (PR gate, weekly scans)
- [ ] Artifact retention policy explained (90 days for SOX/HIPAA)
- [ ] Vulnerability response procedures documented
- [ ] False positive handling workflow documented
- [ ] Emergency disable procedures documented

**Architecture Documentation** (`docs/explanation/security-scanning-architecture.md`):
- [ ] cargo-audit CI integration architecture
- [ ] Weekly security scan workflow design
- [ ] Dependabot configuration strategy
- [ ] Enhanced deny.toml policies rationale
- [ ] Security receipt JSON Schema design
- [ ] cargo-geiger optional integration approach
- [ ] Performance budget and optimization strategies
- [ ] Enterprise compliance mapping (SOX, HIPAA, GDPR, PCI DSS)

**JSON Schema** (`docs/reference/security-receipt-schema.json`):
- [ ] Schema file exists and is valid JSON Schema draft-07
- [ ] All required fields documented with descriptions
- [ ] Examples provided for valid receipts
- [ ] Schema version 1.0 for evolution management

#### Validation Commands
```bash
# Verify SECURITY.md updates
grep -A 10 "Security Scanning Infrastructure" SECURITY.md

# Verify README.md badges
grep "cargo-audit" README.md
grep "cargo-deny" README.md

# Verify how-to guide exists
test -f docs/how-to/configure-security-scanning.md && echo "✓ Guide exists"

# Check how-to guide completeness
grep -E "Scanning Frequency|Artifact Retention|Vulnerability Response|False Positive Handling|Emergency Procedures" \
     docs/how-to/configure-security-scanning.md

# Verify architecture document
test -f docs/explanation/security-scanning-architecture.md && echo "✓ Architecture doc exists"

# Verify JSON Schema
test -f docs/reference/security-receipt-schema.json && echo "✓ Schema exists"
check-jsonschema --check-metaschema docs/reference/security-receipt-schema.json
```

**Success Criteria**:
- [ ] All required documentation files created or updated
- [ ] Documentation covers all ACs comprehensively
- [ ] Examples and validation commands provided
- [ ] Enterprise compliance requirements documented

---

## AC9: End-to-End CI Integration Validation

### Test Spec
`docs/explanation/security-scanning-architecture.md`

### Prerequisites
- All security workflows merged to main branch
- CI pipeline updated with security jobs
- Test PR available for validation

### Validation Procedure

#### Step 1: PR Quality Gate Validation
```bash
# Create test PR
git checkout -b test/security-integration-validation
echo "# Security Integration Test" > TEST.md
git add TEST.md
git commit -m "test: validate security scanning CI integration (AC9)"
git push -u origin test/security-integration-validation

# Create PR
gh pr create --title "Test: Security Scanning Integration (AC9)" \
             --body "End-to-end validation of Issue #35 security scanning infrastructure"

# Watch PR checks
gh pr checks --watch
```

**Success Criteria**:
- [ ] `security-audit` job appears in PR checks
- [ ] `security-audit` job completes successfully (no vulnerabilities)
- [ ] Security receipt artifact uploaded (check workflow logs)
- [ ] PR merge not blocked by security jobs (unless vulnerabilities found)

#### Step 2: Security Receipt Download and Validation
```bash
# Get PR run ID
PR_RUN_ID=$(gh pr checks --json databaseId --jq '.[0].databaseId')

# Download security receipt
gh run download $PR_RUN_ID --name security-audit-<sha>

# Validate against JSON Schema
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 security-audit-*.json

# Verify receipt content
cat security-audit-*.json | jq '{version, scan_type, vulnerabilities: .vulnerabilities.count}'
```

**Success Criteria**:
- [ ] Security receipt validates against JSON Schema
- [ ] Receipt `scan_type` is "pr-gate"
- [ ] Receipt includes commit SHA, timestamp, tool versions
- [ ] Vulnerability count is 0 (for clean workspace)

#### Step 3: cargo-geiger Optional Job Validation
```bash
# Check cargo-geiger job status (if present)
gh pr checks --json name,status,conclusion | jq '.[] | select(.name | contains("geiger"))'

# Verify job is optional (doesn't block PR)
# Expected: Job runs but `continue-on-error: true` allows PR merge even if it fails
```

**Success Criteria**:
- [ ] cargo-geiger job runs (if configured)
- [ ] Job status is optional (doesn't block PR merge during 30-day validation)
- [ ] Job validates zero unsafe code (0 functions, 0 expressions)

#### Step 4: Weekly Scan Workflow Integration
```bash
# Manually trigger weekly scan from main branch
gh workflow run security-scan.yml

# Verify workflow executes independently of PR workflow
gh run list --workflow=security-scan.yml --limit 1

# Check for security receipt artifact
gh run download <run-id> --name security-scan-<sha>
```

**Success Criteria**:
- [ ] Weekly scan workflow executes independently
- [ ] Weekly scan produces separate security receipt (scan_type: "weekly-scan")
- [ ] Weekly scan can trigger GitHub issue creation (if vulnerabilities found)

---

## AC10: Rollback Procedure Testing

### Test Spec
`docs/how-to/configure-security-scanning.md#emergency-procedures`

### Validation Procedure

#### Scenario 1: Emergency Disable cargo-audit CI Gate

**Trigger**: CRITICAL vulnerability requires immediate production deployment, but cargo-audit blocks CI.

**Procedure**:
```bash
# Option 1: Time-boxed advisory ignore (preferred)
# Edit deny.toml:
cat >> deny.toml <<EOF

[advisories]
ignore = [
    { id = "RUSTSEC-2024-XXXX", reason = "CRITICAL: Production deployment required, fix tracked in #<issue>", expires = "$(date -u -d '+7 days' +%Y-%m-%d)" }
]
EOF

git add deny.toml
git commit -m "fix(security): time-boxed ignore for RUSTSEC-2024-XXXX (7-day emergency)"
git push

# Option 2: Temporarily disable cargo-audit job (last resort)
# Edit .github/workflows/ci.yml:
# security-audit:
#   if: false  # TEMPORARY: Emergency disable - remove after fix

git add .github/workflows/ci.yml
git commit -m "fix(ci): temporary disable cargo-audit for emergency deployment"
git push

# Create tracking issue
gh issue create --title "Re-enable cargo-audit after RUSTSEC-2024-XXXX resolution" \
                --label security,high-priority \
                --body "Emergency disable: <justification>"
```

**Validation Checklist**:
- [ ] Time-boxed ignore allows PR merge with justification
- [ ] Tracking issue created for follow-up resolution
- [ ] Emergency disable procedure documented in how-to guide
- [ ] Re-enablement procedure clear (revert commit or update deny.toml)

#### Scenario 2: Dependabot PR Overload

**Trigger**: >10 open Dependabot PRs, maintainer review burden excessive.

**Procedure**:
```bash
# Option 1: Adjust schedule to weekly
# Edit .github/dependabot.yml:
# - package-ecosystem: "cargo"
#   schedule:
#     interval: "weekly"  # Change from "daily"
#     day: "monday"

git add .github/dependabot.yml
git commit -m "fix(deps): reduce Dependabot schedule to weekly"
git push

# Option 2: Emergency disable via GitHub UI
# Repository → Settings → Code security and analysis
# Scroll to "Dependabot version updates" → Click "Pause"

# Monitor PR count reduction
gh pr list --label dependencies --state open
```

**Validation Checklist**:
- [ ] Schedule adjustment reduces PR volume
- [ ] Emergency pause via GitHub UI works correctly
- [ ] Re-enablement procedure documented
- [ ] First 30-day monitoring identifies optimal schedule

#### Scenario 3: deny.toml Policy Rejection

**Trigger**: Enhanced deny.toml policies reject current dependencies unexpectedly.

**Procedure**:
```bash
# Identify failing dependency
cargo deny check advisories --log-level debug

# Option 1: Update dependency (preferred)
cargo update <crate-name>
cargo test --workspace
git add Cargo.lock
git commit -m "fix(deps): update <crate-name> to resolve advisory"
git push

# Option 2: Time-boxed ignore (false positive)
# Edit deny.toml:
# [advisories]
# ignore = [
#     { id = "RUSTSEC-2024-XXXX", reason = "False positive - feature not used", expires = "2025-12-31" }
# ]

# Option 3: Rollback policy (temporary)
git revert <deny-toml-enhancement-commit>
git push

# Create tracking issue
gh issue create --title "Resolve deny.toml policy blocker for <crate>" \
                --label security,blocked \
                --body "Policy enhancement blocked by: <details>"
```

**Validation Checklist**:
- [ ] Dependency update resolves advisory
- [ ] Time-boxed ignore with justification allows progress
- [ ] Rollback procedure preserves security posture
- [ ] Tracking issue ensures follow-up resolution

#### Scenario 4: CI Performance Budget Exceeded

**Trigger**: Security jobs add >2 minutes overhead to CI.

**Procedure**:
```bash
# Measure overhead
gh run view <enhanced-run-id> --json jobs --jq '.jobs[] | {name, duration: (.completedAt - .startedAt)}'

# Option 1: Verify advisory DB caching
grep "shared-key: advisory-db" .github/workflows/ci.yml .github/workflows/security-scan.yml

# Option 2: Move cargo-audit to weekly scan only
# Remove security-audit job from ci.yml
# Keep in security-scan.yml only

git add .github/workflows/ci.yml
git commit -m "fix(ci): move cargo-audit to weekly scan only (performance budget)"
git push

# Option 3: Optimize cargo-geiger (keep optional)
# Ensure cargo-geiger has continue-on-error: true
# Move to separate optional workflow if needed
```

**Validation Checklist**:
- [ ] Advisory DB caching reduces overhead to ~2 seconds
- [ ] Moving cargo-audit to weekly scan eliminates PR overhead
- [ ] cargo-geiger remains optional and doesn't block PRs
- [ ] Performance budget exception documented with justification

---

## Validation Summary Checklist

### AC2: Weekly Security Scan
- [ ] Manual trigger successful
- [ ] Security receipt generated and validates
- [ ] GitHub issue created on vulnerabilities
- [ ] Scheduled execution works automatically

### AC3: Dependabot
- [ ] Dependabot PRs appear within 48 hours
- [ ] Patch updates grouped correctly
- [ ] PR limits enforced (≤10 Cargo, ≤5 Actions)
- [ ] 30-day monitoring completed

### AC7: CI Performance
- [ ] Baseline timing captured
- [ ] Enhanced timing captured
- [ ] Overhead < 2 minutes
- [ ] Advisory DB cache hit rate > 80%

### AC8: Documentation
- [ ] SECURITY.md updated
- [ ] README.md updated
- [ ] How-to guide complete
- [ ] Architecture doc complete
- [ ] JSON Schema valid

### AC9: End-to-End Integration
- [ ] PR quality gate works
- [ ] Security receipts validate
- [ ] cargo-geiger optional status confirmed
- [ ] Weekly scan independent execution

### AC10: Rollback Procedures
- [ ] Emergency disable tested
- [ ] Dependabot pause tested
- [ ] deny.toml rollback tested
- [ ] Performance fallback tested

---

**Document Status**: Manual Validation Procedures Complete
**Last Updated**: 2025-10-02
**Issue**: #35 - Dependency & Security Scanning Infrastructure
