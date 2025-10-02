# Security Scanning Test Fixtures
## Issue #35 - Dependency & Security Scanning Infrastructure

This directory contains test fixtures for validating the copybook-rs security scanning infrastructure implementation.

## Directory Structure

```
security-scanning/
├── workflows/          # Example GitHub Actions workflow YAML files
│   ├── ci-security-audit.yml        # cargo-audit PR gate workflow example
│   └── security-scan-weekly.yml     # Weekly security scan workflow example
├── configs/            # Configuration file examples
│   ├── deny-baseline.toml           # Current deny.toml (before enhancement)
│   ├── deny-enhanced.toml           # Enhanced deny.toml with stricter policies
│   └── dependabot.yml               # Dependabot configuration example
└── receipts/           # Security receipt JSON examples
    ├── clean-scan.json              # Clean security scan (no vulnerabilities)
    ├── vulnerabilities-found.json   # Scan with vulnerabilities detected
    └── geiger-zero-unsafe.json      # cargo-geiger zero unsafe validation
```

## Test Fixture Purpose

### Workflows (`workflows/`)

**AC:1, AC:2, AC:9** - GitHub Actions workflow examples for security scanning

- `ci-security-audit.yml`: Example PR quality gate workflow with cargo-audit integration
  - Demonstrates advisory DB caching with `shared-key: advisory-db`
  - Shows security receipt generation with compliance metadata
  - Validates artifact upload with 90-day retention (SOX/HIPAA compliance)

- `security-scan-weekly.yml`: Example weekly security scan workflow
  - Demonstrates scheduled cron execution (Monday 03:00 UTC)
  - Shows automated GitHub issue creation on vulnerability detection
  - Validates workflow_dispatch for manual testing

### Configurations (`configs/`)

**AC:3, AC:4** - Configuration file examples for policy validation

- `deny-baseline.toml`: Baseline deny.toml configuration (before enhancement)
  - Represents current state with `yanked = "warn"`, `wildcards = "allow"`
  - Used for comparison testing to validate policy changes

- `deny-enhanced.toml`: Enhanced deny.toml with 7 stricter policies
  - `vulnerability = "deny"` (explicit HIGH/CRITICAL denial)
  - `yanked = "deny"` (upgrade from "warn")
  - `unsound = "deny"` (new policy for enterprise reliability)
  - `notice = "warn"` (track informational advisories)
  - `wildcards = "deny"` (deterministic builds for SOX/PCI DSS)
  - `unknown-registry = "deny"` (supply chain security)
  - `unknown-git = "deny"` (trusted sources only)

- `dependabot.yml`: Dependabot configuration example
  - Demonstrates grouped patch updates strategy
  - Shows PR limits (10 Cargo, 5 GitHub Actions)
  - Validates conventional commit message format

### Receipts (`receipts/`)

**AC:5** - Security receipt JSON examples for schema validation

- `clean-scan.json`: Example security receipt with no vulnerabilities
  - Validates all required fields: version, timestamp, commit_sha, scan_type
  - Shows zero unsafe code metrics from cargo-geiger
  - Demonstrates compliance metadata (SOX, HIPAA, GDPR, PCI DSS)

- `vulnerabilities-found.json`: Example security receipt with vulnerabilities
  - Demonstrates vulnerability severity breakdown (critical, high, medium, low)
  - Shows advisory details (RUSTSEC ID, crate, version, CVSS scores)
  - Validates non-compliant state when vulnerabilities present

- `geiger-zero-unsafe.json`: Example security receipt with cargo-geiger metrics
  - Validates zero unsafe code policy (0 functions, 0 expressions)
  - Shows per-crate unsafe code breakdown
  - Demonstrates integration between cargo-audit and cargo-geiger

## Usage in Tests

These fixtures are used by:

1. **Integration Tests** (`copybook-bench/tests/issue_35_security_scanning_integration.rs`):
   - Validate YAML workflow syntax
   - Test deny.toml policy enforcement
   - Verify security receipt JSON Schema compliance
   - Test cargo-geiger unsafe code detection

2. **Validation Scripts** (`scripts/validate-security-config.sh`):
   - YAML syntax validation with `yq`
   - deny.toml policy validation with `cargo deny check`
   - JSON Schema validation with `check-jsonschema`
   - Dependabot configuration validation

3. **Manual Test Procedures** (`tests/manual/SECURITY_SCANNING_VALIDATION.md`):
   - Reference examples for manual workflow testing
   - Configuration validation procedures
   - End-to-end CI integration validation

## Validation Commands

### Validate Workflow YAML Syntax
```bash
yq eval '.' tests/fixtures/security-scanning/workflows/ci-security-audit.yml
yq eval '.' tests/fixtures/security-scanning/workflows/security-scan-weekly.yml
```

### Validate deny.toml Configurations
```bash
# Baseline configuration
cargo deny check advisories --config tests/fixtures/security-scanning/configs/deny-baseline.toml
cargo deny check bans --config tests/fixtures/security-scanning/configs/deny-baseline.toml
cargo deny check sources --config tests/fixtures/security-scanning/configs/deny-baseline.toml

# Enhanced configuration (stricter policies)
cargo deny check advisories --config tests/fixtures/security-scanning/configs/deny-enhanced.toml
cargo deny check bans --config tests/fixtures/security-scanning/configs/deny-enhanced.toml
cargo deny check sources --config tests/fixtures/security-scanning/configs/deny-enhanced.toml
```

### Validate Dependabot Configuration
```bash
yq eval '.version' tests/fixtures/security-scanning/configs/dependabot.yml
yq eval '.updates[].package-ecosystem' tests/fixtures/security-scanning/configs/dependabot.yml
yq eval '.updates[0].groups' tests/fixtures/security-scanning/configs/dependabot.yml
```

### Validate Security Receipts Against JSON Schema
```bash
# Install validator
pip install check-jsonschema

# Validate receipts
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 tests/fixtures/security-scanning/receipts/clean-scan.json

check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 tests/fixtures/security-scanning/receipts/vulnerabilities-found.json

check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 tests/fixtures/security-scanning/receipts/geiger-zero-unsafe.json
```

## Acceptance Criteria Coverage

| AC | Test Fixture | Validation Method |
|----|--------------|-------------------|
| AC1 | `workflows/ci-security-audit.yml` | YAML syntax, cargo-audit integration patterns |
| AC2 | `workflows/security-scan-weekly.yml` | Scheduled workflow, issue creation patterns |
| AC3 | `configs/dependabot.yml` | Configuration syntax, grouping strategy |
| AC4 | `configs/deny-baseline.toml`, `configs/deny-enhanced.toml` | Policy enforcement, negative testing |
| AC5 | `receipts/*.json` | JSON Schema compliance validation |
| AC6 | `receipts/geiger-zero-unsafe.json` | cargo-geiger metrics structure |
| AC9 | All workflow fixtures | End-to-end CI integration patterns |

## Specification References

- **Architecture**: `docs/explanation/security-scanning-architecture.md`
- **How-To Guide**: `docs/how-to/configure-security-scanning.md`
- **JSON Schema**: `docs/reference/security-receipt-schema.json`
- **Acceptance Criteria**: `docs/issue-35-ac-finalized.md`
- **Integration Tests**: `copybook-bench/tests/issue_35_security_scanning_integration.rs`
- **Manual Procedures**: `tests/manual/SECURITY_SCANNING_VALIDATION.md`
- **Validation Script**: `scripts/validate-security-config.sh`

---

**Document Status**: Test Fixtures Complete
**Last Updated**: 2025-10-02
**Issue**: #35 - Dependency & Security Scanning Infrastructure
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../../LICENSE).
