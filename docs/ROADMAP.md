<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Roadmap

**Status**: Engineering Preview (v0.4.3)
**Target**: v1.0.0 (Q2-Q3 2026)

## Principles

- **Stability first**: No breaking public behaviors without a minor+ bump; API freeze before v1.0.
- **Performance budgeted**: Track throughput against realistic MiB/s floors; CI enforces DISPLAY >= 80 MiB/s, COMP-3 >= 8 MiB/s.
- **Single source of truth**: Raw performance receipts live in `scripts/bench/perf.json`; feature truth in [COBOL_SUPPORT_MATRIX.md](reference/COBOL_SUPPORT_MATRIX.md).
- **Determinism**: Parallel decode remains deterministic; round-trip remains lossless.

## Done Recently

| Feature | Detail |
|---------|--------|
| Edited PIC encode (E3.1-E3.7) | Full encode support -- Z, $, +/-, CR/DB, commas, asterisk, B insertion (115+ tests) |
| Dialect lever (D0-D4) | Normative / ZeroTolerant / OneTolerant modes for ODO min_count |
| Determinism validation (phases 1-3) | Codec harness, CLI wiring, CI smoke test |
| RENAMES codec (R1-R3) | Decode/encode with alias resolution (7 codec-layer tests) |
| Panic elimination | Zero production panics on main (PR #182) |
| Quality gates (#97-100) | All four issues closed |
| SIGN SEPARATE, COMP-1/COMP-2 | Promoted to stable and default-enabled in v0.4.3 |
| Blocking perf regression gate (#512) | `perf-gate.yml` fails PRs on DISPLAY ≥80 / COMP-3 ≥8 MiB/s floors + >5% relative regression vs committed baseline |
| Iterator module examples (#514) | Reference + how-to docs for the streaming iterator API; fixed fabricated `RecordDecoder` in LIBRARY_API.md/MIGRATION_GUIDE.md and a file-open error-code bug |
| Enterprise deployment docs | [ENTERPRISE_DEPLOYMENT.md](ENTERPRISE_DEPLOYMENT.md) (production operations, Kubernetes, monitoring) and [enterprise-compliance-guide.md](enterprise-compliance-guide.md) shipped |

**Test status**: 10,250+ passing (15 ignored), zero unsafe, clippy pedantic compliant.

## Next (v1.0.0 Blockers)

These items must ship before v1.0.0 can be tagged.

| Item | Est. Effort | Why it blocks |
|------|-------------|---------------|
| Enterprise audit/compliance | 8-12 weeks | SOX, HIPAA, GDPR, PCI DSS stubs are experimental; need production-grade outputs |
| API freeze window | 4 weeks | Only doc/bench/test changes; stabilizes public surface |

**Estimated path**: Enterprise features (Mar-May 2026) -> docs & polish (Jun 2026) -> API freeze & release (Jun-Jul 2026).

## Later (Post-v1.0)

- **Ecosystem adapters** -- copybook-arrow stabilization (currently experimental); stabilize the existing Kafka example pipeline (`examples/kafka_pipeline/`)
- **Native Linux benchmark baseline** -- replace WSL2 reference measurements with bare-metal numbers
- **RENAMES R4-R6** -- advanced REDEFINES/OCCURS interactions (if demand warrants)
- **Support policy** -- 6-month minor support window; security patches anytime

## Out of Scope

- **Nested ODO** (O5/O6) -- rejected by design ([Issue #164](https://github.com/EffortlessMetrics/copybook-rs/issues/164))
- **ODO over REDEFINES** (O6) -- rejected by design
- **RENAMES R4-R6** (REDEFINES/OCCURS interactions) -- out of scope unless demand emerges
- **EXTERNAL / GLOBAL clauses** -- no implementation planned

## What Blocks Wider Adoption

1. Enterprise audit system outputs are experimental stubs, not compliance evidence.
2. copybook-arrow (Arrow/Parquet) integration remains experimental, limiting analytics pipeline adoption.

## Performance Baseline

| Workload | Floor (CI) | Baseline (ref hardware) | Commit |
|----------|-----------|------------------------|--------|
| DISPLAY-heavy | 80 MiB/s | 205 MiB/s | 1fa63633 |
| COMP-3-heavy | 8 MiB/s† | 58 MiB/s | 1fa63633 |

† The COMP-3 floor is CI-grounded, not reference-hardware. COMP-3 packed-decimal
decode is throughput-bound at far lower rates than DISPLAY (12–14 MiB/s on
`ubuntu-latest`), and the per-record cost is fundamental — throughput *decreases*
with larger payloads. The 8 MiB/s floor sits ~35% below the worst observed CI
measurement, absorbing runner variance while still catching a real regression.

Baseline measured 2025-09-30 on WSL2 / AMD Ryzen 9 9950X3D.
The committed regression-gate baseline (`scripts/bench/baseline.json`) reflects
canonical `ubuntu-latest` CI measurements; see
[PERFORMANCE_GOVERNANCE.md](PERFORMANCE_GOVERNANCE.md).
See [BASELINE_METHODOLOGY.md](../tools/copybook-bench/BASELINE_METHODOLOGY.md) for procedures.

## History

Completed milestones (v0.4.0, v0.5.0) and testing methodology details are preserved in:
- [archived/ROADMAP_v0.4.0_v0.5.0.md](archived/ROADMAP_v0.4.0_v0.5.0.md)
- [TESTING_COMMANDS.md](TESTING_COMMANDS.md)

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
