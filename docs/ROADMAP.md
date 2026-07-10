<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Roadmap

**Status**: Engineering Preview (v0.4.3)
**Release target**: v1.0.0 after the product-readiness gates below pass; there is no calendar-only release promise.
**Program tracker**: [#535 — v1 Product Readiness](https://github.com/EffortlessMetrics/copybook-rs/issues/535)

This document is the canonical roadmap. Current feature truth lives in
[COBOL_SUPPORT_MATRIX.md](reference/COBOL_SUPPORT_MATRIX.md), current engineering
status in [REPORT.md](REPORT.md), and performance policy/receipts in
[PERFORMANCE_GOVERNANCE.md](PERFORMANCE_GOVERNANCE.md).

## What “complete” means

`copybook-rs` does not need to accept every COBOL construct to reach v1.
It must instead provide an honest, end-to-end contract:

- Every feature marked **supported** works through parsing, layout, decode,
  encode, deterministic round-trip, CLI behavior, stable errors, tests, and
  documentation where those layers apply.
- Every deliberately unsupported construct is rejected predictably with a
  stable error/scenario identifier and practical remediation guidance.
- Experimental subsystems either meet explicit graduation criteria or remain
  outside the stable v1 contract.
- New users can install the CLI or depend on the canonical `copybook` facade
  without first learning the workspace topology.
- Releases are reproducible, resumable, auditable, registry-tested, and
  fix-forward.

## Principles

- **Truth before claims**: code, tests, canonical registries, and generated
  receipts outrank copied counts or narrative status text.
- **Stable core first**: the parser/codec/record-I/O/CLI contract can reach v1
  without pretending that every enterprise adapter is stable.
- **Determinism and fidelity**: supported encode/decode paths remain
  deterministic and byte-faithful under their documented policies.
- **Explicit non-support**: unsupported constructs fail before producing
  misleading output.
- **One source of truth**: package, feature, CLI, error, support, and release
  inventories are generated or verified against their canonical sources.
- **Evidence-led performance**: profile first; optimize only measured hot paths;
  preserve correctness, memory bounds, and API behavior.
- **Fix-forward releases**: inspect and resume partial publish state; do not use
  yanking as the normal recovery procedure.
- **Review-forward delivery**: one bounded concern per PR, with focused tests,
  current checks, full-diff review, and cleanup of temporary artifacts.

## Product tracks

### Stable core product

The stable v1 train covers:

- `copybook` as the canonical Rust library entrypoint
- `copybook-rs` as a redirect/search-alias package only
- copybook parsing, schema, and layout behavior
- fixed and RDW record I/O
- encode/decode and deterministic round-trip
- supported codepages and numeric representations
- projection and dialect handling
- stable errors, exit codes, CLI commands, and JSON/JSONL contracts
- packaging, installation, support, and release workflows

### Enterprise and adapter track

These remain beta or experimental until their individual graduation gates are
met:

- enterprise audit and regulatory-oriented evidence
- governance runtime
- Arrow/Parquet
- Kafka integrations
- advanced operational reporting

The project must not imply that a generated SOX, HIPAA, GDPR, or PCI-oriented
report constitutes regulatory certification.

## Done recently

The following capabilities are already present on `main`; their exact support
and evidence remain governed by the support matrix and tests:

- Edited PIC parse/decode/encode coverage
- dialect handling for ODO minimum-count interpretation
- deterministic decode, encode, and round-trip validation
- RENAMES R1–R3 resolution and codec integration
- SIGN SEPARATE and COMP-1/COMP-2 support
- stable error-code families and CLI exit-code mapping
- production panic-elimination and `unsafe` restrictions
- blocking performance-regression checks with machine-readable receipts
- iterator and enterprise deployment guidance
- Kafka producer/consumer examples
- API-freeze, stability, and support-policy documentation

## Phase 0 — Reset repository truth

**Status**: In progress

- [ ] [#536](https://github.com/EffortlessMetrics/copybook-rs/issues/536) —
      reset this roadmap and supersede stale tracker #75
- [ ] [#537](https://github.com/EffortlessMetrics/copybook-rs/issues/537) —
      make `copybook` the default library entrypoint in public docs
- [ ] [#540](https://github.com/EffortlessMetrics/copybook-rs/issues/540) —
      add one `xtask docs verify-all` source-of-truth gate

### Exit criteria

- One current roadmap and one program tracker.
- No known contradiction in versions, package inventories, error/support
  registries, CLI inventory, performance policy, or release documentation.
- Truth verification runs when code, manifests, workflows, receipts, or docs can
  invalidate a public claim.
- Each remaining program item has bounded acceptance criteria.

## Phase 1 — Make distribution and first use reliable

**Status**: Queued after the initial roadmap/docs reset

- [ ] [#538](https://github.com/EffortlessMetrics/copybook-rs/issues/538) —
      derive the publish plan from workspace metadata and include both facade
      packages
- [ ] [#539](https://github.com/EffortlessMetrics/copybook-rs/issues/539) —
      replace yank-based rollback with resumable fix-forward recovery

### Exit criteria

- A clean machine can install `copybook-cli` from the registry.
- A clean temporary project can compile against `copybook` without workspace
  path dependencies.
- `copybook` is published after all component dependencies; `copybook-rs` is
  last and remains redirect-only.
- Publish order, package count, release notes, docs.rs checks, and smoke tests
  are driven by one generated/validated plan.
- Partial publish state can be inspected and resumed without blindly rerunning
  every package.

## Phase 2 — Define the stable product contract

**Status**: Queued

- [ ] [#541](https://github.com/EffortlessMetrics/copybook-rs/issues/541) —
      classify every package and public feature as stable, beta, experimental,
      or internal/dev-only
- [ ] [#542](https://github.com/EffortlessMetrics/copybook-rs/issues/542) —
      baseline and audit the complete stable Rust/CLI/schema/error surface
- [ ] [#543](https://github.com/EffortlessMetrics/copybook-rs/issues/543) —
      complete the pre-v1 deprecation and migration audit

### Exit criteria

- Every workspace package and public Cargo feature has an explicit class.
- Every stable API/CLI/schema/error contract has a reproducible compatibility
  baseline or documented exception.
- No experimental API is accidentally promised stable.
- Every intentional pre-v1 break is complete with migration guidance or
  explicitly cancelled before freeze.
- The stable promise centers on `copybook`, documented stable component APIs,
  stable CLI contracts, and versioned data/error contracts—not every incidental
  implementation crate.

## Phase 3 — Complete correctness evidence

**Status**: Tracked by
[#551](https://github.com/EffortlessMetrics/copybook-rs/issues/551)

For every support-matrix row marked supported, require the applicable evidence
layers:

1. parse
2. layout resolution
3. decode
4. encode
5. deterministic and byte-faithful round-trip
6. boundary and negative behavior
7. CLI integration
8. relevant record formats and codepages

The umbrella includes dedicated stable-error tests, iterator/memory unit
coverage, property/fuzz/mutation lanes, cross-codepage fixtures, hostile and
truncated input, bounded-memory soak tests, corpus governance, and explicit
rejection evidence for designed non-goals.

### Exit criteria

- Every supported row links to exact machine-verified evidence.
- No feature is called fully supported based only on parsing or layout.
- Every stable error code has a dedicated triggering test.
- Unsupported constructs never silently produce misleading output.
- Determinism and round-trip hold across documented worker configurations.

## Phase 4 — Make the product pleasant to operate

**Status**: Tracked by
[#552](https://github.com/EffortlessMetrics/copybook-rs/issues/552)

Planned bounded capabilities:

- `support --advise`
- `copybook explain`
- `copybook compat` with CI `--fail-on` policy
- `copybook doctor`
- shared versioned JSON, Markdown, JUnit, and SARIF reports

### Exit criteria

- Users can localize representative record/schema failures without reading
  implementation source.
- CI can reject breaking copybook changes using a stable machine-readable
  verdict.
- Diagnostic/report output is deterministic, versioned, redaction-aware, and
  based on stable codes rather than message parsing.
- Suggestions map to real supported behavior and commands.

## Phase 5 — Graduate or isolate enterprise and adapters

**Status**: Parallel beta/experimental track in
[#553](https://github.com/EffortlessMetrics/copybook-rs/issues/553)

Each audit, governance, Arrow/Parquet, and Kafka surface must become one of:

- stable with complete evidence
- beta with precise limitations and graduation criteria
- experimental and explicitly outside the v1 stable contract
- removed from the advertised product surface

### Stable-core release policy

These surfaces are **not stable-core v1 blockers by default**. They become
blockers only if the project explicitly decides to include them in the stable
v1 contract. Their documentation and default-feature behavior must remain
honest while deferred.

## Phase 6 — Evidence-driven performance

**Status**: Tracked by
[#188](https://github.com/EffortlessMetrics/copybook-rs/issues/188)

Execution is profile-led:

- [ ] [#546](https://github.com/EffortlessMetrics/copybook-rs/issues/546) —
      current profiles and baseline receipts
- [ ] [#547](https://github.com/EffortlessMetrics/copybook-rs/issues/547) —
      I/O buffering/batching experiment
- [ ] [#548](https://github.com/EffortlessMetrics/copybook-rs/issues/548) —
      allocation-reduction experiment
- [ ] [#549](https://github.com/EffortlessMetrics/copybook-rs/issues/549) —
      codepage vectorization/SIMD experiment
- [ ] [#550](https://github.com/EffortlessMetrics/copybook-rs/issues/550) —
      PGO experiment

Optimization issues after #546 proceed only when the profile identifies the
candidate as material. A documented evidence-backed rejection is a valid result.

### Release policy

Optional optimization work does not block stable-core v1 while current governed
performance floors pass. A correctness, determinism, memory, or material
performance regression is a blocker.

## Phase 7 — Freeze, release candidate, and v1

**Status**: Blocked by the preceding stable-core gates; tracked by
[#189](https://github.com/EffortlessMetrics/copybook-rs/issues/189)

- [ ] [#544](https://github.com/EffortlessMetrics/copybook-rs/issues/544) —
      enforce the four-week freeze across the actual stable surface
- [ ] [#545](https://github.com/EffortlessMetrics/copybook-rs/issues/545) —
      prove a release candidate through registry-only installs and dogfood

### Exit criteria

- Four continuous weeks complete with no unplanned stable-contract break.
- No unresolved critical or high-severity correctness/security blocker.
- Every stable package passes package audit, registry-only resolution, and
  documentation/install smoke tests.
- Stable Rust, the declared MSRV, Linux, Windows, and macOS evidence is current
  for the release-candidate commit.
- Fixed and RDW dogfood/determinism workflows pass from released artifacts.
- Support matrix, API/CLI docs, schemas, changelog, package metadata, and release
  artifacts agree.
- The final go/no-go decision records all remaining risks and owners.

## Deliberate non-goals

Unless a separate demand-backed design changes policy, the following are not
required for v1 completeness:

- nested ODO (O5/O6)
- ODO over REDEFINES
- RENAMES R4–R6 interactions with REDEFINES/OCCURS
- `EXTERNAL` / `GLOBAL` clauses

Their rejection behavior, error/scenario identifiers, tests, support-matrix
status, and remediation guidance **are** required to be complete.

## Immediate execution order

1. #536 — roadmap reset
2. #537 — facade-first documentation
3. #538 — generated complete publish plan
4. #539 — resumable fix-forward release runbook
5. #540 — comprehensive truth gate
6. #541 — stable/beta/experimental classification
7. #542 — API/contract baselines
8. #543 — deprecation/migration audit
9. #551 and #552 — correctness evidence and operator usability
10. #544 and #545 — freeze and release-candidate proof

Do not begin source-level performance optimization before #546. Do not start the
freeze clock before #541–#543 are complete.

## History

Historical milestone detail is preserved in:

- [archived/ROADMAP_v0.4.0_v0.5.0.md](archived/ROADMAP_v0.4.0_v0.5.0.md)
- [TESTING_COMMANDS.md](TESTING_COMMANDS.md)
- legacy issue [#75](https://github.com/EffortlessMetrics/copybook-rs/issues/75),
  which will close as superseded when this reset is merged

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
