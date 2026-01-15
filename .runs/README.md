# Build Receipts and Validation Evidence

This directory stores build receipts and validation evidence from agentic development workflows. Receipts provide verifiable artifacts that prove development work happened correctly.

## Purpose

Build receipts serve as:
- **Audit trail**: Traceable record of changes, commands, and outcomes
- **Validation evidence**: Proof that tests passed and builds succeeded
- **Issue tracking**: Documentation of discovered and fixed issues
- **Reproducibility**: Complete context for reproducing builds

## Directory Structure

```
.runs/
├── README.md                    # This file
├── .gitkeep                     # Keep directory in git
├── schema/
│   └── receipt.schema.json      # JSON schema for receipt files
└── <timestamp>-<run-id>.json    # Individual receipt files (gitignored)
```

## Receipt Format

Receipts are JSON files conforming to `schema/receipt.schema.json`. Each receipt includes:

### Core Fields

- **run_id**: Unique identifier for the build run (string)
- **timestamp**: ISO 8601 timestamp when the run started
- **base_commit**: Git commit SHA at the start of the run
- **head_commit**: Git commit SHA at the end of the run (if commits were made)

### Change Tracking

- **changes**: Array of file modifications with:
  - `path`: File path relative to repository root
  - `intent`: Human-readable description of the change

### Command Execution

- **commands**: Array of executed commands with:
  - `cmd`: Full command string executed
  - `exit_code`: Command exit status (0 = success)
  - `duration_ms`: Execution time in milliseconds

### Outputs

- **outputs**: Array of build artifacts with:
  - `name`: Artifact name (e.g., "test_results", "benchmark_report")
  - `path`: Absolute or relative path to the artifact

### Issue Management

- **issues_discovered**: Array of issue identifiers or descriptions found during the run
- **issues_fixed**: Array of issue identifiers or descriptions resolved during the run

## Usage Examples

### Agentic Development Workflow

```json
{
  "run_id": "gen-impl-20260112-010730",
  "timestamp": "2026-01-12T01:07:30Z",
  "base_commit": "595390c",
  "head_commit": "abc1234",
  "changes": [
    {
      "path": "copybook-core/src/parser.rs",
      "intent": "Add support for RENAMES clause parsing"
    }
  ],
  "commands": [
    {
      "cmd": "cargo test --workspace",
      "exit_code": 0,
      "duration_ms": 45230
    }
  ],
  "outputs": [
    {
      "name": "test_results",
      "path": "target/nextest/default/junit.xml"
    }
  ],
  "issues_discovered": [],
  "issues_fixed": ["#123"]
}
```

### CI/CD Integration

```json
{
  "run_id": "ci-perf-gate-20260112-020000",
  "timestamp": "2026-01-12T02:00:00Z",
  "base_commit": "abc1234",
  "head_commit": "abc1234",
  "commands": [
    {
      "cmd": "cargo bench --package copybook-bench",
      "exit_code": 0,
      "duration_ms": 120000
    },
    {
      "cmd": "cargo run --bin bench-report -- validate scripts/bench/perf.json",
      "exit_code": 0,
      "duration_ms": 500
    }
  ],
  "outputs": [
    {
      "name": "perf_results",
      "path": "scripts/bench/perf.json"
    }
  ],
  "issues_discovered": [],
  "issues_fixed": []
}
```

## gitignore Policy

- **Committed**: `README.md`, `.gitkeep`, `schema/` directory
- **Ignored**: Individual receipt files (`*.json` in `.runs/` root)

This ensures the infrastructure is tracked while keeping build artifacts local.

## Schema Validation

Validate receipts against the JSON schema:

```bash
# Using a JSON schema validator (example with ajv-cli)
ajv validate -s .runs/schema/receipt.schema.json -d .runs/my-receipt.json
```

## Retention Policy

Receipt files are local artifacts and are not committed to git. Teams should establish retention policies based on:
- Disk space constraints
- Audit requirements
- Compliance needs

Typical retention: 30-90 days for local development, longer for CI/CD archives.

## Related Documentation

- [CONTRIBUTING.md](../CONTRIBUTING.md) - Development workflow guidelines
- [docs/perf/OPERATOR_RUNBOOK.md](../docs/perf/OPERATOR_RUNBOOK.md) - Performance validation procedures
- [copybook-bench/BASELINE_METHODOLOGY.md](../copybook-bench/BASELINE_METHODOLOGY.md) - Benchmark methodology
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
