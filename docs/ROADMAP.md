<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Roadmap

**Status**: Engineering Preview (v0.4.3)
**Target**: v1.0.0 (Q2-Q3 2026)

## Principles

- **Stability first**: No breaking public behaviors without a minor+ bump; API freeze before v1.0.
- **Performance budgeted**: Track throughput against realistic MiB/s floors; CI enforces DISPLAY >= 80 MiB/s, COMP-3 >= 40 MiB/s.
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

**Test status**: 10,250+ passing (15 ignored), zero unsafe, clippy pedantic compliant.

## Next (v1.0.0 Blockers)

These items must ship before v1.0.0 can be tagged.

| Item | Est. Effort | Why it blocks |
|------|-------------|---------------|
| Enterprise audit/compliance | 8-12 weeks | SOX, HIPAA, GDPR, PCI DSS stubs are experimental; need production-grade outputs |
| Performance regression gates | 2-4 weeks | Currently advisory-only; must become blocking CI gates |
| Iterator module examples | 1-2 weeks | Public API lacks usage docs, slows onboarding |
| Enterprise deployment guide | 1-2 weeks | No production operations documentation |
| API freeze window | 4 weeks | Only doc/bench/test changes; stabilizes public surface |

**Estimated path**: Enterprise features (Mar-May 2026) -> docs & polish (Jun 2026) -> API freeze & release (Jun-Jul 2026).

## Later (Post-v1.0)

- **Ecosystem adapters** -- copybook-arrow stabilization (currently experimental), Kafka example pipeline
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
2. Performance regression gates are advisory-only; no blocking CI enforcement yet.
3. Iterator and deployment documentation gaps reduce onboarding velocity.

## Performance Baseline

| Workload | Floor (CI) | Baseline (ref hardware) | Commit |
|----------|-----------|------------------------|--------|
| DISPLAY-heavy | 80 MiB/s | 205 MiB/s | 1fa63633 |
| COMP-3-heavy | 40 MiB/s | 58 MiB/s | 1fa63633 |

Baseline measured 2025-09-30 on WSL2 / AMD Ryzen 9 9950X3D.
See [BASELINE_METHODOLOGY.md](../tools/copybook-bench/BASELINE_METHODOLOGY.md) for procedures.

## History

Completed milestones (v0.4.0, v0.5.0) and testing methodology details are preserved in:
- [archived/ROADMAP_v0.4.0_v0.5.0.md](archived/ROADMAP_v0.4.0_v0.5.0.md)
- [TESTING_COMMANDS.md](TESTING_COMMANDS.md)

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
