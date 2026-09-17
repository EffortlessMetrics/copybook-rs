<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Roadmap

**Status**: Engineering Preview (v0.8.1)
**Release target**: v0.9.0 next, then v0.10.0; there is no calendar-only release promise. v1.0.0 numbering is deferred — the Phase 7 freeze/RC discipline below applies to the stabilization cut when it is scheduled, not to an imminent v1.
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

**Status**: Done

- [x] [#536](https://github.com/EffortlessMetrics/copybook-rs/issues/536) —
      reset this roadmap and supersede stale tracker #75
- [x] [#537](https://github.com/EffortlessMetrics/copybook-rs/issues/537) —
      make `copybook` the default library entrypoint in public docs
- [x] [#540](https://github.com/EffortlessMetrics/copybook-rs/issues/540) —
      add one `xtask docs verify-all` source-of-truth gate

### Exit criteria

- One current roadmap and one program tracker.
- No known contradiction in versions, package inventories, error/support
  registries, CLI inventory, performance policy, or release documentation.
- Truth verification runs when code, manifests, workflows, receipts, or docs can
  invalidate a public claim.
- Each remaining program item has bounded acceptance criteria.

## Phase 1 — Make distribution and first use reliable

**Status**: Completed

- [x] [#538](https://github.com/EffortlessMetrics/copybook-rs/issues/538) —
      derive the publish plan from workspace metadata and include both facade
      packages
- [x] [#539](https://github.com/EffortlessMetrics/copybook-rs/issues/539) —
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

**Status**: Completed

- [x] [#541](https://github.com/EffortlessMetrics/copybook-rs/issues/541) —
      classify every package and public feature as stable, beta, experimental,
      or internal/dev-only
- [x] [#542](https://github.com/EffortlessMetrics/copybook-rs/issues/542) —
      baseline and audit the complete stable Rust/CLI/schema/error surface
- [x] [#543](https://github.com/EffortlessMetrics/copybook-rs/issues/543) —
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

Issue-compilation guardrail:

- Scenario-level evidence packets are owned by #551 only while they are still on the
  scenario-inventory seam.
- A child issue is only allowed after a specific scenario, concrete existing tests,
  and an exact current-main claim are documented in a single issue body.

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

      Keep this blocked on correctness evidence program completion (#551),
      freeze authorization, and release-policy sign-off.

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

## Immediate execution order (0.7.0 train)

The #536–#545 reset sequence is complete; what follows is the 0.7.0 cut,
tracked by the `v0.7.0` milestone under coordinator #948.

1. Actionable test-count gate output (#933)
2. MSRV 1.95 → 1.98 workspace-wide — done (#959)
3. Remove the eight deprecated 0.6 facade aliases; resolve every remaining
   0.6 deprecation with an explicit remove, re-window, or keep decision —
   done (#960)
4. `docs/migration/0.7.md` written from the actual removal diff — done (#961)
5. Machine-owned scenario ledger contract, then evidence reconciliation
   (numeric, fixed/RDW, codepage, stable errors)
6. Governed public corpus foundation plus VB/BDW framing with round-trip proof
7. Shared diagnostic verdict domain plus `support --advise`
8. Feature freeze, release-preparation PR, exact-SHA GO record, tag, publish,
   registry-only acceptance

Deliberately deferred past 0.7.0: field-driven record lengths, `explain`,
`doctor`, `compat`, JUnit/SARIF renderers without a consumer, full adapter
graduation, Kafka/audit/governance expansion, speculative performance work.

Do not begin source-level performance optimization before #546. Do not start the
freeze clock before the ledger reconciliation (item 5) is complete.

## Planned releases

- **v0.6.1** (previous release, superseded by the 0.7.0 train
  above): canonical-fingerprint patch —
  `verify --report` now emits the same SHA-256 `schema_fingerprint` as codec
  metadata, canonical fingerprints are fixed-width 64-hex, an additive
  `source_fingerprint` carries raw-source identity, and packaged
  README/crates.io metadata were corrected. No new COBOL surface.
- **v0.6.0**: workspace convergence release —
  retired crates folded into owners, curated `copybook` facade with
  deprecation shims, collapsed feature flags, MSRV Rust 1.95. Migration
  notes live in [migration/0.6.md](migration/0.6.md).
- **v0.7.0** (shipped 2026-09-16): Rust 1.98 release (MSRV moves from 1.95 to 1.98;
  a later 1.100 jump buys no present release control and is not a 0.7 gate).
  Removed the deprecated 0.6 facade aliases (eight flat modules plus the
  `copybook-codepage`/`copybook-lexer`/`copybook-dialect` shims; see
  [migration/0.7.md](migration/0.7.md)) and delivers
  the adoption-hardening scope in the immediate execution order above. No
  calendar promise; scope is set by coordinator #948 under the `v0.7.0`
  milestone.
- **v0.7.1** (shipped 2026-09-16): strictly non-breaking patch — `support --advise`
  contract graduates from beta (`0.7.0-beta.1`) to stable (`1.0`) with no
  shape change, plus the 0.7 status narrative and release-lane devex fixes
  (truth budget, local evidence policy, changelog curation docs).
  Coordinated under #975.
- **v0.8.0** (shipped 2026-09-16): "trust and velocity" — release-lane
  hill-climbs (`just preflight`, pinned 1.98 toolchain, fragment gate,
  CI-evidenced runbook), #553 graduation dispositions for
  audit/governance/Arrow/Kafka, codepage/numeric evidence close-out (#571,
  #573), and the #946 commercial-relicensing audit receipt. Strictly
  non-breaking.
- **v0.8.1** (shipped 2026-09-17): correctness patch — six wrong EBCDIC slots
  corrected against independent references (CP1140 Euro `0xFF`→`0x9F`, CP273
  `~/ß`, CP1047 `^`/`¬`), with pinned oracle fixtures, a release-smoke
  installed-binary witness, and byte-output migration notes. No API change.
- **v0.9.0** (next): operator diagnostics and evidence completion —
  release/devex hardening is done (#988 slices A+B, #990, #991, #993, #994,
  #996, #1000; #982 merged). Remaining: the `support --advise` defect
  cluster (#977–#981, #983), the deferred #552 surfaces (`explain`,
  `compat`, `doctor`), remaining evidence coordinators (#572 fixed/RDW
  pipeline, #576 stable-error/rejection), governance beta criteria (#985),
  preflight freshness/aggregation implementation (#992, sized), and the
  #946 commercial-relicensing audit receipt (human-gated). Completing this
  release finishes #551/#552, which unblocks the freeze clock.
- **v0.10.0** (after 0.9.0): stabilization cut — the four-week freeze (#544),
  registry-only release-candidate proof (#545), and the #189 go/no-go record
  run under 0.x numbering. Performance (#188) and adapter graduation (#553)
  stay parallel, per-surface, non-blocking tracks throughout.

## History

Historical milestone detail is preserved in:

- [archived/ROADMAP_v0.4.0_v0.5.0.md](archived/ROADMAP_v0.4.0_v0.5.0.md)
- [TESTING_COMMANDS.md](TESTING_COMMANDS.md)
- legacy issue [#75](https://github.com/EffortlessMetrics/copybook-rs/issues/75),
  which will close as superseded when this reset is merged

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
