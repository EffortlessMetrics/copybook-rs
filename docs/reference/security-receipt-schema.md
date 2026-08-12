<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Security Receipt JSON Schema Reference
## Issue #35 - Normalized Security Evidence Specification

### Overview

The Security Receipt JSON Schema defines a normalized structure for recording
security-scanner inputs, results, tool versions, and source revision context.
Schema validity means only that a document matches this data contract. It is
not a security certification or a regulatory-compliance determination.

**Schema Location**: `docs/reference/security-receipt-schema.json`
**Schema Version**: 1.0
**JSON Schema Draft**: draft-07
**Schema ID**: `https://copybook-rs.effortlessmetrics.com/schemas/security-receipt-v1.0.json`

---

## Table of Contents

1. [Schema Overview](#schema-overview)
2. [Required Fields](#required-fields)
3. [Optional Fields](#optional-fields)
4. [Field Specifications](#field-specifications)
5. [Validation Examples](#validation-examples)
6. [Enterprise Integration](#enterprise-integration)

---

## Schema Overview

### Purpose

The Security Receipt schema provides a standardized evidence format for
documenting security scan results. Each normalized document can support:

- **SIEM Integration**: Structured JSON for enterprise security monitoring platforms
- **Traceability**: Links scan results to specific code versions and toolchain configurations
- **Supply Chain Evidence**: Records dependency ecosystem state observed at scan time

### Generation Status

This schema defines the contract for a normalized security receipt. Current CI
workflows upload raw `cargo audit --json` scanner output, which is useful scan
evidence but is **not** a schema-valid receipt. Normalized receipt generation is
tracked separately in
[#761](https://github.com/EffortlessMetrics/copybook-rs/issues/761).

Intended generation contexts are:

1. **PR Quality Gate** (`scan_type: "pr-gate"`)
2. **Weekly Scanning** (`scan_type: "weekly-scan"`)
3. **Manual Audits** (`scan_type: "manual"`)

### Storage & Retention

- **Normalized receipt retention**: to be defined and verified by the
  receipt-generator lane
- **Normalized receipt naming**: to be defined by the receipt-generator lane
- **Current raw weekly artifact**: `cargo-audit-raw-{workflow-run-id}`
- **Access**: Download via `gh run download <run-id> --name <artifact-name>`

---

## Required Fields

All normalized security evidence documents **MUST** include the following
fields for schema validity:

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `version` | string (const "1.0") | Schema version for evolution management | `"1.0"` |
| `timestamp` | string (ISO 8601) | Scan execution time (UTC) | `"2025-10-02T12:00:00Z"` |
| `commit_sha` | string (40 hex chars) | Git commit SHA for code traceability | `"abc123def456..."` |
| `scan_type` | enum | Scan context: `pr-gate`, `weekly-scan`, `manual` | `"pr-gate"` |
| `rust_version` | string (semver) | Rust toolchain version | `"1.95.0"` |
| `tools` | object | Security tool versions | See [Tools Object](#tools-object) |
| `vulnerabilities` | object | Vulnerability detection results | See [Vulnerabilities Object](#vulnerabilities-object) |
| `exit_status` | enum | Scan outcome: `success`, `vulnerabilities_found`, `error` | `"success"` |

### Tools Object

**Required subfields**:
- `cargo_audit` (string): cargo-audit version (e.g., `"0.21.2"`)
- `cargo_deny` (string): cargo-deny version (e.g., `"0.16.0"`)

**Optional subfields**:
- `cargo_geiger` (string): cargo-geiger version (e.g., `"0.13.0"`)

### Vulnerabilities Object

**Required subfields**:
- `count` (integer ≥ 0): Total vulnerability count
- `by_severity` (object): Severity breakdown with required fields:
  - `critical` (integer ≥ 0)
  - `high` (integer ≥ 0)
  - `medium` (integer ≥ 0)
  - `low` (integer ≥ 0)
- `advisories` (array): Detailed advisory list (may be empty)

**Advisory Object Fields** (when `advisories` non-empty):
- `id` (string, pattern `RUSTSEC-YYYY-NNNN`): RustSec advisory ID
- `severity` (enum): `critical`, `high`, `medium`, `low`
- `crate` (string): Affected crate name
- `version` (string): Affected crate version
- `title` (string, optional): Human-readable vulnerability description
- `url` (string URI, optional): RustSec advisory URL
- `cvss` (string, optional): CVSS vector string
- `patched` (array of strings, optional): Patched version ranges

---

## Optional Fields

These fields add scan context and SIEM integration data but are not required
for schema validation:

### Scan Metadata

- **`scan_duration_ms`** (integer ≥ 0): Scan execution time in milliseconds for performance monitoring
- **`workflow_run_id`** (string): GitHub Actions workflow run ID for traceability

### Advisory Database Metadata

- **`advisory_db_version`** (object):
  - `last_commit` (string, 40 hex chars): RustSec database commit SHA
  - `last_updated` (string, ISO 8601): Database update timestamp
  - `advisory_count` (integer ≥ 0): Total advisories in database

### Supply Chain Verification

- **`crates_io_snapshot`** (string URI): crates.io index snapshot URL for reproducibility

### Unsafe Code Metrics

- **`unsafe_code_metrics`** (object, from cargo-geiger):
  - `total_functions` (integer ≥ 0): Total unsafe functions (expected: 0 for copybook-rs)
  - `total_expressions` (integer ≥ 0): Total unsafe expressions (expected: 0 for copybook-rs)
  - `crates` (array): Per-crate breakdown with:
    - `name` (string)
    - `unsafe_functions` (integer ≥ 0)
    - `unsafe_expressions` (integer ≥ 0)

---

## Field Specifications

### `version` (required)

**Type**: string
**Constraint**: Exact value `"1.0"`

**Purpose**: Schema version for evolution management. Future schema changes will increment version.

**Example**:
```json
{
  "version": "1.0"
}
```

**Validation**: Schema will reject receipts with any version other than `"1.0"`.

---

### `timestamp` (required)

**Type**: string
**Format**: ISO 8601 date-time (UTC)

**Purpose**: Scan execution timestamp for audit trail chronology.

**Example**:
```json
{
  "timestamp": "2025-10-02T12:00:00Z"
}
```

**Generation** (Bash):
```bash
date -u +%Y-%m-%dT%H:%M:%SZ
```

**Validation**: Must match RFC 3339 date-time format with timezone.

---

### `commit_sha` (required)

**Type**: string
**Pattern**: `^[0-9a-f]{40}$`

**Purpose**: Git commit SHA for code version traceability.

**Example**:
```json
{
  "commit_sha": "abc123def456abc123def456abc123def456abc1"
}
```

**Generation** (GitHub Actions):
```yaml
commit_sha: "${{ github.sha }}"
```

**Validation**: Must be exactly 40 hexadecimal characters (full SHA-1, not abbreviated).

---

### `scan_type` (required)

**Type**: string (enum)
**Values**: `"pr-gate"`, `"weekly-scan"`, `"manual"`

**Purpose**: Scan execution context for audit classification.

**Examples**:
```json
// PR quality gate (CI workflow)
{
  "scan_type": "pr-gate"
}

// Scheduled weekly scan
{
  "scan_type": "weekly-scan"
}

// Ad-hoc manual audit
{
  "scan_type": "manual"
}
```

**Workflow Mapping**:
- `pr-gate`: `.github/workflows/ci.yml` (every PR)
- `weekly-scan`: `.github/workflows/security-scan.yml` (cron schedule)
- `manual`: `workflow_dispatch` triggers or local execution

---

### `rust_version` (required)

**Type**: string
**Pattern**: `^\d+\.\d+\.\d+` (semantic versioning)

**Purpose**: Rust toolchain version for scan reproducibility.

**Example**:
```json
{
  "rust_version": "1.95.0"
}
```

**Generation** (Bash):
```bash
rustc --version | awk '{print $2}'
```

**Validation**: Must start with major.minor.patch digits (may include additional metadata like `1.95.0-nightly`).

---

### `tools` (required)

**Type**: object
**Required Subfields**: `cargo_audit`, `cargo_deny`
**Optional Subfields**: `cargo_geiger`

**Purpose**: Security tool versions for audit trail verification.

**Example**:
```json
{
  "tools": {
    "cargo_audit": "0.21.2",
    "cargo_deny": "0.16.0",
    "cargo_geiger": "0.13.0"
  }
}
```

**Generation** (Bash):
```bash
cargo audit --version | awk '{print $2}'
cargo deny --version 2>/dev/null | awk '{print $2}' || echo 'unknown'
cargo geiger --version 2>/dev/null | awk '{print $2}' || echo 'unknown'
```

---

### `vulnerabilities` (required)

**Type**: object
**Required Subfields**: `count`, `by_severity`, `advisories`

**Purpose**: Vulnerability detection results with severity classification.

**Example** (no vulnerabilities):
```json
{
  "vulnerabilities": {
    "count": 0,
    "by_severity": {
      "critical": 0,
      "high": 0,
      "medium": 0,
      "low": 0
    },
    "advisories": []
  }
}
```

**Example** (with vulnerabilities):
```json
{
  "vulnerabilities": {
    "count": 2,
    "by_severity": {
      "critical": 0,
      "high": 1,
      "medium": 1,
      "low": 0
    },
    "advisories": [
      {
        "id": "RUSTSEC-2024-0001",
        "severity": "high",
        "crate": "example-crate",
        "version": "1.2.3",
        "title": "Buffer overflow in parsing logic",
        "url": "https://rustsec.org/advisories/RUSTSEC-2024-0001",
        "cvss": "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H",
        "patched": [">=1.2.4"]
      },
      {
        "id": "RUSTSEC-2024-0002",
        "severity": "medium",
        "crate": "another-crate",
        "version": "0.5.1",
        "title": "Information disclosure via error messages",
        "url": "https://rustsec.org/advisories/RUSTSEC-2024-0002",
        "cvss": "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:L/I:N/A:N",
        "patched": [">=0.5.2"]
      }
    ]
  }
}
```

**Generation** (Bash with jq):
```bash
cargo audit --json --all-features --workspace > audit.json
cat audit.json | jq '.vulnerabilities'
```

---

### `exit_status` (required)

**Type**: string (enum)
**Values**: `"success"`, `"vulnerabilities_found"`, `"error"`

**Purpose**: Scanner outcome classification for automation and review.

**Examples**:
```json
// Successful scan, zero vulnerabilities
{
  "exit_status": "success"
}

// Scan completed, vulnerabilities detected
{
  "exit_status": "vulnerabilities_found"
}

// Scan failed (network error, tool crash, etc.)
{
  "exit_status": "error"
}
```

**Operational Interpretation**:
- `success`: The configured scan completed without reported vulnerabilities
- `vulnerabilities_found`: Requires remediation per SECURITY.md (<48hr triage)
- `error`: Scan validity uncertain, re-run required

---

## Validation Examples

### Example 1: Validate Schema Itself

```bash
# Install JSON Schema validator
pip install check-jsonschema

# Validate that the schema conforms to its metaschema
check-jsonschema --check-metaschema docs/reference/security-receipt-schema.json
# Expected output: "ok -- validation done, no errors!"
```

### Example 2: Validate Clean Security Receipt

```bash
# Create test receipt (zero vulnerabilities)
cat > test-receipt-clean.json <<'EOF'
{
  "version": "1.0",
  "timestamp": "2025-10-02T12:00:00Z",
  "commit_sha": "abc123def456abc123def456abc123def456abc1",
  "scan_type": "pr-gate",
  "rust_version": "1.95.0",
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

# Validate against schema
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 test-receipt-clean.json
# Expected output: "ok -- validation done, no errors!"
```

### Example 3: Validate Receipt with Vulnerabilities

```bash
# Create test receipt (with vulnerabilities)
cat > test-receipt-vulns.json <<'EOF'
{
  "version": "1.0",
  "timestamp": "2025-10-02T15:30:00Z",
  "commit_sha": "def456abc123def456abc123def456abc123def4",
  "scan_type": "weekly-scan",
  "rust_version": "1.95.0",
  "tools": {
    "cargo_audit": "0.21.2",
    "cargo_deny": "0.16.0"
  },
  "vulnerabilities": {
    "count": 1,
    "by_severity": {
      "critical": 0,
      "high": 1,
      "medium": 0,
      "low": 0
    },
    "advisories": [
      {
        "id": "RUSTSEC-2024-0001",
        "severity": "high",
        "crate": "example-crate",
        "version": "1.2.3",
        "title": "Buffer overflow in parsing logic",
        "url": "https://rustsec.org/advisories/RUSTSEC-2024-0001"
      }
    ]
  },
  "exit_status": "vulnerabilities_found"
}
EOF

# Validate against schema
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 test-receipt-vulns.json
# Expected output: "ok -- validation done, no errors!"
```

### Example 4: Validate Real CI Artifact

```bash
# Download a schema-valid security receipt after normalized generation exists
gh run download <run-id> --name <normalized-receipt-artifact>

# Validate downloaded artifact
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 security-audit-*.json
# Expected output: "ok -- validation done, no errors!"
```

### Example 5: Detect Schema Violations

```bash
# Create invalid receipt (missing required field)
cat > test-receipt-invalid.json <<'EOF'
{
  "version": "1.0",
  "timestamp": "2025-10-02T12:00:00Z",
  "commit_sha": "abc123def456abc123def456abc123def456abc1",
  "scan_type": "pr-gate",
  "rust_version": "1.95.0",
  "tools": {
    "cargo_audit": "0.21.2"
    // Missing required "cargo_deny" field
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

# Attempt validation
check-jsonschema --schemafile docs/reference/security-receipt-schema.json \
                 test-receipt-invalid.json
# Expected output: ValidationError describing missing "cargo_deny" field
```

---

## Enterprise Integration

### SIEM Integration

**Purpose**: Ingest security receipts into enterprise Security Information and Event Management (SIEM) platforms.

**Supported Platforms**:
- Splunk
- Elastic Security (ELK Stack)
- IBM QRadar
- ArcSight
- Azure Sentinel

**Integration Pattern 1: GitHub API Polling**

```python
# Example: Python script for SIEM artifact ingestion
import requests
import json

def fetch_security_receipts(repo, workflow_name, github_token):
    headers = {"Authorization": f"token {github_token}"}
    url = f"https://api.github.com/repos/{repo}/actions/workflows/{workflow_name}/runs"

    response = requests.get(url, headers=headers)
    runs = response.json()["workflow_runs"]

    for run in runs[:10]:  # Last 10 runs
        artifacts_url = run["artifacts_url"]
        artifacts = requests.get(artifacts_url, headers=headers).json()

        for artifact in artifacts["artifacts"]:
            if artifact["name"].startswith("security-audit"):
                # Download and parse security receipt
                download_url = artifact["archive_download_url"]
                receipt_zip = requests.get(download_url, headers=headers)
                # Extract JSON, validate schema, ingest to SIEM
                # ... (implementation specific to SIEM platform)

# Usage
fetch_security_receipts("EffortlessMetrics/copybook-rs", "ci.yml", "<your-github-token>")
```

**Integration Pattern 2: S3 Export**

```yaml
# Add to .github/workflows/ci.yml or security-scan.yml
- name: Upload security receipt to S3
  if: always()
  env:
    AWS_ACCESS_KEY_ID: ${{ secrets.AWS_ACCESS_KEY_ID }}
    AWS_SECRET_ACCESS_KEY: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
  run: |
    aws s3 cp security-audit-*.json \
              s3://enterprise-security-receipts/copybook-rs/ \
              --region us-east-1
```

**Integration Pattern 3: Webhook Delivery**

```yaml
# Add to .github/workflows/security-scan.yml
- name: Send security receipt to SIEM webhook
  if: always()
  run: |
    curl -X POST https://siem.example.com/webhooks/security-receipts \
         -H "Content-Type: application/json" \
         -H "Authorization: Bearer ${{ secrets.SIEM_WEBHOOK_TOKEN }}" \
         --data @security-scan-*.json
```

### Ticketing System Integration

**Purpose**: Automated vulnerability tracking in Jira, ServiceNow, or similar platforms.

**Integration via GitHub-Jira Sync**:

```yaml
# .github/workflows/security-scan.yml enhancement
- name: Create Jira ticket for vulnerabilities
  if: steps.audit.outputs.vuln_count > 0
  uses: atlassian/gajira-create@v3
  with:
    project: SECURITY
    issuetype: Bug
    summary: "Security Alert: ${{ steps.audit.outputs.vuln_count }} vulnerabilities in copybook-rs"
    description: |
      Automated security scan detected vulnerabilities.

      **Scan Details**:
      - Timestamp: ${{ steps.audit.outputs.timestamp }}
      - Commit: ${{ github.sha }}
      - Vulnerabilities: ${{ steps.audit.outputs.vuln_count }}

      **Remediation**:
      Review GitHub issue created by security-scan workflow.
    fields: '{"labels": ["security", "automated"], "priority": {"name": "High"}}'
```

---

## Schema Evolution

### Version Management

**Current Version**: 1.0 (initial release)

**Future Versions**: Schema will increment version for breaking changes:

- **1.1**: Add optional fields (backward compatible)
- **2.0**: Change required fields or field types (breaking change)

**Backward Compatibility Strategy**:
```json
{
  "version": "1.0",  // Receipts always specify version
  // ... current schema fields
}
```

Consumers should validate `version` field and handle unknown versions gracefully:

```python
# Example: Python schema version handling
def validate_receipt(receipt_json):
    version = receipt_json.get("version")

    if version == "1.0":
        # Use v1.0 schema
        validate_v1_0(receipt_json)
    elif version == "2.0":
        # Use v2.0 schema (future)
        validate_v2_0(receipt_json)
    else:
        raise ValueError(f"Unsupported schema version: {version}")
```

### Proposed Future Enhancements

**Version 1.1 Candidates** (non-breaking additions):
- `dependency_graph_snapshot`: Complete dependency tree for supply chain analysis
- `license_policy`: License validation results
- `performance_metrics`: Scan execution performance data
- `network_egress`: Network communication validation results

**Version 2.0 Candidates** (potential breaking changes):
- Nested vulnerability severity (CVSS 3.1 → CVSS 4.0)
- Multi-ecosystem support (Cargo + npm + Docker)

---

## Related Documentation

- **Architecture Overview**: [security-scanning-architecture.md](../explanation/security-scanning-architecture.md)
- **Configuration Guide**: [configure-security-scanning.md](../how-to/configure-security-scanning.md)
- **Security Policy**: [SECURITY.md](../../SECURITY.md)
- **JSON Schema Specification**: [security-receipt-schema.json](security-receipt-schema.json)
- **Issue Tracking**: [GitHub Issue #35](https://github.com/EffortlessMetrics/copybook-rs/issues/35)

---

## Appendix: JSON Schema Validation Tools

### Command-Line Tools

**check-jsonschema** (Python, recommended):
```bash
pip install check-jsonschema
check-jsonschema --schemafile schema.json receipt.json
```

**jsonschema CLI** (Python):
```bash
pip install jsonschema
jsonschema -i receipt.json schema.json
```

**ajv-cli** (Node.js):
```bash
npm install -g ajv-cli
ajv validate -s schema.json -d receipt.json
```

### Programmatic Validation

**Python**:
```python
import json
import jsonschema

with open("security-receipt-schema.json") as f:
    schema = json.load(f)

with open("receipt.json") as f:
    receipt = json.load(f)

jsonschema.validate(instance=receipt, schema=schema)
print("✅ Validation successful")
```

**Rust** (using `jsonschema` crate):
```rust
use jsonschema::JSONSchema;
use serde_json::json;

fn validate_receipt(schema_path: &str, receipt_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let schema = std::fs::read_to_string(schema_path)?;
    let receipt = std::fs::read_to_string(receipt_path)?;

    let schema_json: serde_json::Value = serde_json::from_str(&schema)?;
    let receipt_json: serde_json::Value = serde_json::from_str(&receipt)?;

    let compiled = JSONSchema::compile(&schema_json)?;
    compiled.validate(&receipt_json)?;

    println!("✅ Validation successful");
    Ok(())
}
```

**JavaScript**:
```javascript
const Ajv = require('ajv');
const fs = require('fs');

const schema = JSON.parse(fs.readFileSync('security-receipt-schema.json'));
const receipt = JSON.parse(fs.readFileSync('receipt.json'));

const ajv = new Ajv();
const validate = ajv.compile(schema);

if (validate(receipt)) {
    console.log('✅ Validation successful');
} else {
    console.error('❌ Validation failed:', validate.errors);
}
```

---

**Document Status**: ✅ API REFERENCE COMPLETE - Ready for implementation (Issue #35)
**Last Updated**: 2025-10-02
**Reference Version**: 1.0
**Authors**: copybook-rs spec-creator agent (generative flow)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
