<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Stability Guarantees

This document defines what users can expect to remain stable across releases, providing a clear trust contract for production deployments.

## What is Stable

**Public APIs**
- Library APIs documented in [Library API Reference](reference/LIBRARY_API.md): `parse_copybook`, `decode_record`, `encode_record`, `decode_file_to_jsonl`, `encode_jsonl_to_file`, `RecordIterator`
- CLI command interfaces documented in [CLI Reference](CLI_REFERENCE.md): `parse`, `inspect`, `decode`, `encode`, `verify`, `support`
- Error taxonomy (CBKP*, CBKS*, CBKD*, CBKE*, CBKR*, CBKC*, CBKF*, CBKI*, CBKA*, CBKW* codes) - stable across minor versions
- Exit codes: 0 (success), 1 (unhandled), 2-5 (structured error codes)

**Output Formats**
- JSON schema structure and field naming conventions
- JSONL record format with envelope metadata
- Error message format and content
- CLI output for `inspect` and `support` commands

**Data Fidelity**
- Round-trip fidelity: Binary -> JSON -> Binary produces byte-identical output
- Determinism: Same input produces byte-identical output across runs and worker configurations
- Zoned decimal metadata preservation for maintaining copybook semantics
- Codepage conversion accuracy (EBCDIC <-> ASCII)

## What May Change

**Internal APIs**
- Private module APIs (not marked `pub`)
- Internal data structures and algorithms
- Performance characteristics (throughput may improve or regress within acceptable bounds)

**Output Details**
- JSON key ordering (not guaranteed - use schema for field access)
- Audit event IDs (not guaranteed for programmatic use)
- Error message wording (may improve clarity while preserving error codes)

**CLI Behavior**
- Default values for optional flags (may change in minor versions)
- Warning message thresholds (may adjust for better UX)

## Versioning Policy

copybook-rs follows [Semantic Versioning 2.0.0](https://semver.org/spec/v2.0.0.html).

### Pre-1.0 (current: 0.x)

The project is in Engineering Preview. During 0.x releases:

- **Minor (0.X.0)**: May include breaking changes to public APIs, CLI flags, or output formats. Migration notes provided in CHANGELOG.md.
- **Patch (0.0.X)**: Bug fixes, security patches, performance improvements. No breaking changes.

We aim to minimize churn, but reserve the right to make breaking changes between minor releases while below 1.0.

### Post-1.0 (future)

Once 1.0 is released, full SemVer guarantees apply:

| Version Change | Description |
|---------------|-------------|
| **Major (X.0.0)** | Breaking changes to public APIs, CLI commands, or output formats. Requires migration. |
| **Minor (x.Y.0)** | New features, additions to public APIs, backward-compatible changes. No migration required. |
| **Patch (x.y.Z)** | Bug fixes, security patches, performance improvements. No breaking changes. |

**Deprecation Policy (post-1.0)**
- Deprecated APIs will be marked with `#[deprecated]` attribute and documented in CHANGELOG.md
- Deprecated APIs will remain available for at least 2 minor versions before removal
- Breaking changes will be documented in CHANGELOG.md with migration guides

---

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
