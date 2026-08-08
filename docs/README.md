<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-rs Documentation

Start with **[START_HERE.md](START_HERE.md)** for the hand-maintained navigation page.

## Documentation Areas

### User-Facing
- [Getting Started](tutorials/getting-started.md) -- tutorial with bundled fixtures
- [CLI Reference](CLI_REFERENCE.md) -- command-line interface documentation
- [Library API](reference/LIBRARY_API.md) -- Rust API reference
- [User Guide](USER_GUIDE.md) -- end-to-end workflows

### Reference
- [COBOL Support Matrix](reference/COBOL_SUPPORT_MATRIX.md) -- feature coverage
- [Error Codes](reference/ERROR_CODES.md) -- 10 families, 64 stable codes
- [CLI Examples](reference/CLI_EXAMPLES.md) -- copy-paste command recipes

### Project
- [Roadmap](ROADMAP.md) -- project status, next/later plan
- [Report](REPORT.md) -- engineering status and performance snapshot
- [Stability Guarantees](STABILITY_GUARANTEES.md) -- API stability contract
- [Support Policy](SUPPORT_POLICY.md) -- release support windows
- [Performance Governance](PERFORMANCE_GOVERNANCE.md) -- baseline and policy
- [Release Process](RELEASE_PROCESS.md) -- release workflow

### Architecture
- [Architecture Decision Records](adr/) -- ADRs for significant decisions
- [Design docs](design/) -- behavior contracts and agentic PR operations
- [Agentic PR Operations](design/AGENTIC_PR_OPERATIONS.md) -- delegated lane authority and review-to-merge state machine
- [Internal feature specs](internal/features/) -- dialect lever, edited PIC

### Evidence

- [Fixed/RDW pipeline registry](evidence/fixed-rdw-pipeline.toml) -- current-main scenario and test-anchor evidence

### Diataxis Framework
Documentation follows [Diataxis](https://diataxis.fr/). Category indexes:
[tutorials/](tutorials/README.md) | [how-to/](how-to/README.md) | [explanation/](explanation/README.md) | [reference/](reference/README.md)

Mapping: [diataxis.manifest.yml](diataxis.manifest.yml)

### Archived
Historical artifacts from completed issues and gate receipts: [archived/](archived/README.md)

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
