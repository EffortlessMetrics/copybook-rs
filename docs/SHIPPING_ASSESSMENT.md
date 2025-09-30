# Shipping Assessment (Truthful Messaging)

## Executive Summary
- The workspace delivers a real end-to-end toolchain: the `copybook` CLI exposes parse, inspect, decode, encode, and verify flows that operate on COBOL copybooks and data without requiring legacy runtimes.
- Automated coverage is broad—`cargo test` succeeds across the workspace, exercising the numerous golden fixtures and CLI scenarios already in-tree.
- Despite the functioning pipeline, the latest benchmark artifacts top out at 66–94 MiB/s for DISPLAY data and 18–25 MiB/s for COMP-3, far short of the advertised 4.1 GiB/s and 560 MiB/s service targets; documentation must state these real numbers if we ship now.
- Feature coverage remains incomplete for several common COBOL constructs (COMP-1/COMP-2, edited PIC clauses, separate sign, nested ODO, RENAMES). These gaps must be disclosed prominently because they can block production copybooks.
- The Issue #52 ledger still calls out missing benchmark automation (Python utilities, CI glue) that has not materialized, so any launch story needs to admit that continuous performance tracking is manual today.

## Functional Readiness
- The CLI covers the workflows customers expect—schema parsing, human-readable inspection, binary↔JSON round-tripping, verification, and (behind a feature flag) audit automation—so day-one adopters can actually process data.
- Workspace documentation already walks through typical decode/encode flows, meaning we only need to adjust the marketing tone rather than invent usage guidance from scratch.

## Quality & Test Posture
- A fresh `cargo test` run passes, validating that the extensive integration suite still holds together in practice.
- Historical integrative reports cite 461/462 passing in nextest with remaining leak detectors, so we should communicate that flakiness exists and that one timing-sensitive failure is still unresolved on main.

## Performance Reality
- The most recent machine-readable artifact (`test_perf.json`) shows throughput below even the relaxed 80/40 MiB/s SLO assertions, with explicit FAIL status baked into the JSON; this is the number we must publish if we claim honesty.
- Internal status documents continue to juxtapose these real figures against multi-GiB/s claims; when rewriting the docs we should retain the explicit gap table so readers understand we are not meeting the 4.1 GiB/s / 560 MiB/s target yet.

## Feature & Tooling Gaps to Call Out
- Customers relying on floating-point numerics, edited PIC formats, or nested OCCURS will hit outright unsupported paths. These limitations already sit in the README and must remain front-and-center in the honest launch copy.
- Issue #52 still documents missing benchmark runners, baseline managers, and validation utilities, so there is no automated guardrail ensuring performance does not regress after launch; positioning should emphasize "manual monitoring" until those utilities exist.

## Recommended Launch Framing
- Present the release as a "functional preview" or "pre-production" build that is suitable for controlled migrations requiring deterministic conversion, not as a high-throughput replacement for incumbent mainframe tooling.
- State throughput as "~70 MiB/s DISPLAY / ~22 MiB/s COMP-3 on PCIe 5.0 SSD testbed" and explicitly note the enterprise targets we are still chasing.
- Highlight the working scenarios (fixed-length data, COMP-3 decoding, RDW handling) and explicitly list the unsupported COBOL features so prospects can self-qualify.
- Document the manual nature of performance tracking and the open work for Issue #52 so adopters know what operational toil to expect.

## Ship/No-Ship Decision
Given truthful documentation, the software can ship as an **opt-in early access build** for teams whose copybooks avoid the unsupported constructs and whose throughput needs fall below ~100 MiB/s. For broader enterprise launch, the current performance gap and missing COBOL features remain blockers, so the recommendation is **delay general-availability** until we either close those gaps or successfully reposition the product around lower-throughput, correctness-first workloads.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
