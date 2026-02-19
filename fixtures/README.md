<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Test Fixtures

This directory contains test fixtures for copybook-rs including:

- Sample copybooks
- Sample data files
- Golden test outputs with SHA-256 hashes
- JSON meta-schemas

## Structure

- `copybooks/` - Sample COBOL copybook files
- `data/` - Sample binary data files
- `golden/` - Expected outputs with hashes
- `schemas/` - JSON meta-schemas for validation

## Golden Test Validation

All golden outputs include SHA-256 hashes for regression detection:
- `*.jsonl.sha256` - Hashes for JSONL decode outputs
- `*.bin.sha256` - Hashes for binary encode outputs
- `*.schema.sha256` - Hashes for schema JSON outputs
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
