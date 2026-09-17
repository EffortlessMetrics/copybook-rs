<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Roadmap

**Status**: Engineering Preview (v0.8.1)
**Release target**: v0.9.0 next, then v0.10.0 physical-record breadth and
v0.11.0 heterogeneous-record breadth. The stabilization cut follows those
selected breadth releases; its version is intentionally not assigned yet.
There is no calendar-only release promise. v1.0.0 numbering remains deferred.
**Program tracker**: [#535 — v1 Product Readiness](https://github.com/EffortlessMetrics/copybook-rs/issues/535)
**Breadth program**: [#1006 — physical-record and heterogeneous-file breadth](https://github.com/EffortlessMetrics/copybook-rs/issues/1006)

This document is the canonical roadmap. Current feature truth lives in
[COBOL_SUPPORT_MATRIX.md](reference/COBOL_SUPPORT_MATRIX.md), current engineering
status in [REPORT.md](REPORT.md), and performance policy/receipts in
[PERFORMANCE_GOVERNANCE.md](PERFORMANCE_GOVERNANCE.md).

## What “complete” means

`copybook-rs` does not need to accept every COBOL construct to reach a stable
release. It must instead provide an honest, end-to-end contract:

- Every feature marked **supported** works through parsing, layout, physical
  framing, layout selection where applicable, decode, encode, deterministic
  round-trip, CLI behavior, stable errors, tests, and documentation.
- Every deliberately unsupported construct or physical organization is rejected
  predictably with a stable error/scenario identifier and practical remediation.
- Experimental subsystems either meet explicit graduation criteria or remain
  outside the stable contract.
- New users can install the CLI or depend on the canonical `copybook` facade
  without first learning the workspace topology.
- Releases are reproducible, resumable, auditable, registry-tested, and
  fix-forward.

## Principles

- **Truth before claims**: code, tests, canonical registries, independent
  oracles, and generated receipts outrank copied counts or narrative status.
- **Representation before interpretation**: establish physical records before
  applying a copybook layout, and select the layout before decoding fields.
- **Stable core first**: parser, framing, codec, record-I/O, and CLI contracts
  can stabilize without pretending every enterprise adapter is stable.
- **Determinism and fidelity**: supported paths remain deterministic and
  byte-faithful under their documented policies.
- **Explicit non-support**: unsupported inputs fail before producing misleading
  output.
- **One source of truth**: package, feature, CLI, error, support, profile, and
  release inventories are generated or verified against canonical sources.
- **Independent evidence**: internal decode/encode agreement is not sufficient
  when both directions may share the same wrong premise.
- **Evidence-led performance**: profile first; optimize measured hot paths while
  preserving correctness, memory bounds, and API behavior.
- **Fix-forward releases**: inspect and resume partial publish state; do not use
  yanking as the normal recovery procedure.
- **Review-forward delivery**: one bounded concern per PR, with focused tests,
  current checks, full-diff review, and cleanup of temporary artifacts.

## Product tracks

### Stable core product

The eventual stable train covers:

- `copybook` as the canonical Rust library entrypoint;
- `copybook-rs` as a redirect/search-alias package only;
- copybook parsing, schema, and resolved layout behavior;
- fixed and bare-RDW record I/O;
- selected and evidenced physical-record formats from #1006;
- explicit input profiles for nontrivial framing and layout policy;
- selected and evidenced heterogeneous layout selection and encoding;
- encode/decode and deterministic round-trip;
- supported codepages and numeric representations;
- projection and dialect handling;
- stable errors, exit codes, CLI commands, and JSON/JSONL contracts;
- packaging, installation, support, and release workflows.

A selected breadth surface becomes stable only through its own complete
reader/writer, negative, accounting, independent-oracle, worker, CLI/library,
and registry-only evidence. Packaging success does not promote a beta surface.

### Parallel beta and experimental product work

These remain beta or experimental until their individual graduation gates are
met:

- line-delimited fixed-width framing;
- custom Rust `RecordFramer` extensions;
- parent/child sequence assembly;
- enterprise audit and regulatory-oriented evidence;
- governance runtime;
- Arrow/Parquet;
- Kafka integrations;
- advanced operational reporting.

The project must not imply that a generated SOX, HIPAA, GDPR, or PCI-oriented
report constitutes regulatory certification.

## Done recently

The following capabilities are already present on `main`; their exact support
and evidence remain governed by the support matrix and tests:

- Edited PIC parse/decode/encode coverage;
- dialect handling for ODO minimum-count interpretation;
- deterministic decode, encode, and round-trip validation;
- RENAMES R1–R3 resolution and codec integration;
- SIGN SEPARATE and COMP-1/COMP-2 support;
- fixed, bare-RDW, and beta VB/BDW framing;
- stable error-code families and CLI exit-code mapping;
- production panic-elimination and `unsafe` restrictions;
- blocking performance-regression checks with machine-readable receipts;
- governed corpus and an initial pinned JRecord differential slice;
- iterator and enterprise deployment guidance;
- Kafka producer/consumer examples;
- API-freeze, stability, and support-policy documentation.

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
- Every intentional pre-stabilization break is complete with migration guidance
  or explicitly cancelled before freeze.
- The stable promise centers on `copybook`, documented stable component APIs,
  stable CLI contracts, and versioned data/error contracts—not every incidental
  implementation crate.

## Phase 3 — Complete correctness evidence

**Status**: Tracked by
[#551](https://github.com/EffortlessMetrics/copybook-rs/issues/551)

For every support-matrix row marked supported, require the applicable evidence
layers:

1. parse;
2. layout resolution;
3. physical framing and layout selection where applicable;
4. decode;
5. encode;
6. deterministic and byte-faithful round-trip;
7. boundary and negative behavior;
8. CLI integration;
9. relevant formats, codepages, and worker configurations.

Issue-compilation guardrail:

- Scenario-level evidence packets are owned by #551 only while they remain on
  the scenario-inventory seam.
- A child issue is allowed only after a specific scenario, concrete existing
  tests, and an exact current-main claim are documented in one issue body.

The umbrella includes dedicated stable-error tests, iterator/memory unit
coverage, property/fuzz/mutation lanes, cross-codepage fixtures, hostile and
truncated input, bounded-memory soak tests, corpus governance, and explicit
rejection evidence for designed non-goals.

### Exit criteria

- Every supported row links to exact machine-verified evidence.
- No feature is called fully supported based only on parsing or layout.
- Every stable error code has a dedicated triggering test.
- Unsupported constructs and framing policies never silently produce misleading
  output.
- Determinism and round-trip hold across documented worker configurations.

## Phase 4 — Make the product pleasant to operate

**Status**: Targeted for v0.9.0; tracked by
[#552](https://github.com/EffortlessMetrics/copybook-rs/issues/552)

Selected bounded capabilities:

- finish the `support --advise` correctness/evidence cluster;
- `copybook explain`;
- `copybook doctor`;
- `copybook compat` with CI `--fail-on` policy;
- shared versioned reports only where an actual consumer is defined;
- remaining fixed/RDW and stable-error evidence reconciliation;
- release/devex truth repairs selected into the 0.9 train.

### Exit criteria

- Users can localize representative record/schema failures without reading
  implementation source.
- CI can reject breaking copybook changes using a stable machine-readable
  verdict.
- Diagnostic/report output is deterministic, versioned, redaction-aware, and
  based on stable codes rather than message parsing.
- Suggestions map to real supported behavior and commands.
- #551/#552 selected 0.9 obligations are complete before breadth integration
  changes their input surface.

## Phase 5 — Close physical-record and heterogeneous-file breadth

**Status**: Planned for v0.10.0 and v0.11.0; tracked by
[#1006](https://github.com/EffortlessMetrics/copybook-rs/issues/1006)

The product treats three layers separately:

1. **Physical framing** — establish where each record begins and ends.
2. **Layout selection** — choose which resolved 01-level layout applies.
3. **Sequence assembly** — optionally combine already framed and typed records
   into bounded groups.

### v0.10.0 — Physical-record breadth

- [ ] [#1007](https://github.com/EffortlessMetrics/copybook-rs/issues/1007) —
      inventory demand, fixtures, and independent oracles
- [ ] [#1008](https://github.com/EffortlessMetrics/copybook-rs/issues/1008) —
      define `InputPlan`, `FramingSpec`, `LayoutPlan`, and `RecordEnvelope`
- [ ] [#1009](https://github.com/EffortlessMetrics/copybook-rs/issues/1009) —
      route Fixed/RDW/VB through one framing runtime without behavior drift
- [ ] [#1010](https://github.com/EffortlessMetrics/copybook-rs/issues/1010) —
      bounded declarative length-prefix framing
- [ ] [#1011](https://github.com/EffortlessMetrics/copybook-rs/issues/1011) —
      field-derived record lengths
- [ ] [#1012](https://github.com/EffortlessMetrics/copybook-rs/issues/1012) —
      explicit fixed-blocked framing
- [ ] [#1013](https://github.com/EffortlessMetrics/copybook-rs/issues/1013) —
      independent multi-block VB graduation decision
- [ ] [#1022](https://github.com/EffortlessMetrics/copybook-rs/issues/1022) —
      input profiles and framing-aware operator surfaces
- [ ] [#1023](https://github.com/EffortlessMetrics/copybook-rs/issues/1023) —
      registry-only physical-record release proof

### v0.11.0 — Heterogeneous-record breadth

- [ ] [#1014](https://github.com/EffortlessMetrics/copybook-rs/issues/1014) —
      selectable catalog for multiple 01-level layouts
- [ ] [#1015](https://github.com/EffortlessMetrics/copybook-rs/issues/1015) —
      bounded discriminator-based layout selection
- [ ] [#1016](https://github.com/EffortlessMetrics/copybook-rs/issues/1016) —
      discriminator-to-length-and-layout mapping
- [ ] [#1017](https://github.com/EffortlessMetrics/copybook-rs/issues/1017) —
      explicit heterogeneous-stream encoding
- [ ] [#1024](https://github.com/EffortlessMetrics/copybook-rs/issues/1024) —
      registry-only mixed-stream release proof

### Parallel beta lane

- [ ] [#1018](https://github.com/EffortlessMetrics/copybook-rs/issues/1018) —
      line-delimited fixed-width records
- [ ] [#1019](https://github.com/EffortlessMetrics/copybook-rs/issues/1019) —
      custom Rust `RecordFramer` conformance seam
- [ ] [#1020](https://github.com/EffortlessMetrics/copybook-rs/issues/1020) —
      bounded parent/child sequence assembly

### Cross-cutting breadth evidence

- [ ] [#1021](https://github.com/EffortlessMetrics/copybook-rs/issues/1021) —
      pinned Cobrix/JRecord differential expansion
- [ ] [#776](https://github.com/EffortlessMetrics/copybook-rs/issues/776) —
      valid external-input performance receipts for selected workloads

### Exit criteria

- Existing Fixed/RDW behavior and public callers remain compatible.
- Every promoted physical format has bounded read/write, negative, accounting,
  raw, worker, independent-oracle, and registry-only evidence.
- Every promoted heterogeneous path selects one explicit layout before decode
  and one explicit identity before encode.
- Unsupported or ambiguous policies reject before allocation, decode, or partial
  record output as applicable.
- Beta line/custom/sequence surfaces remain isolated from stable defaults.
- No further incompatible stable framing/profile/layout API, CLI, schema, or
  output change is planned before the stabilization cut.

## Phase 6 — Graduate or isolate enterprise and adapters

**Status**: Parallel beta/experimental track in
[#553](https://github.com/EffortlessMetrics/copybook-rs/issues/553)

Each audit, governance, Arrow/Parquet, and Kafka surface must become one of:

- stable with complete evidence;
- beta with precise limitations and graduation criteria;
- experimental and explicitly outside the stable contract;
- removed from the advertised product surface.

These surfaces are not stable-core blockers by default. They become blockers
only if explicitly included in the stable promise. Documentation and default
feature behavior must remain honest while deferred.

## Phase 7 — Evidence-driven performance

**Status**: Parallel and profile-led; tracked by
[#188](https://github.com/EffortlessMetrics/copybook-rs/issues/188)

Execution order:

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

Optimization work proceeds only when current profiles identify the candidate as
material. An evidence-backed rejection is a valid result. Optional optimization
is non-blocking while current governed floors pass; correctness, determinism,
memory, or material performance regressions remain blockers.

## Phase 8 — Stabilization freeze and registry-only candidate

**Status**: Blocked by the preceding selected stable-core gates; tracked by
[#189](https://github.com/EffortlessMetrics/copybook-rs/issues/189)

The stabilization cut is scheduled only after v0.10.0 and v0.11.0 release
acceptance. Its version is assigned later; v1 numbering remains deferred.

- [x] [#544](https://github.com/EffortlessMetrics/copybook-rs/issues/544) —
      freeze-enforcement machinery across the actual stable surface
- [ ] [#545](https://github.com/EffortlessMetrics/copybook-rs/issues/545) —
      prove the exact frozen candidate through registry-only installs

The four-week execution itself remains owned by #189; closing #544 established
the enforcement machinery rather than completing the future freeze period.

### Entry criteria

- #551/#552 selected correctness and operator obligations are complete.
- The input-plan/profile contract is settled.
- Existing Fixed/RDW compatibility is current.
- Selected v0.10 physical surfaces are complete or explicitly beta/deferred.
- Selected v0.11 layout-selection and heterogeneous-encode surfaces are
  complete.
- Independent/differential findings are fixed or bounded.
- No further incompatible stable framing/layout API, CLI, schema, report, or
  output change is planned.
- Beta line/custom/sequence and adapter surfaces cannot alter stable defaults.
- Linux, Windows, macOS, stable, MSRV, security, packaging, and registry-only
  lanes are green.

### Exit criteria

- Four continuous weeks complete with no unplanned stable-contract break.
- No unresolved critical or high-severity correctness/security blocker.
- Every stable package passes package audit, registry-only resolution, and
  documentation/install smoke tests.
- Platform/toolchain evidence is current for the exact candidate commit.
- Fixed, RDW, and every promoted breadth surface pass released-artifact dogfood
  and determinism checks.
- Support matrix, profiles, API/CLI docs, schemas, changelog, package metadata,
  and release artifacts agree.
- The final GO/NO-GO records all remaining risks and owners.

## Current execution order

1. Complete and publish v0.9.0 under its existing diagnostic/evidence owners.
2. Run #1007 and settle the additive contract in #1008.
3. Land #1009 before adding new framers.
4. Develop #1010–#1013 with the applicable #1021/#1022 evidence and integrate
   through #1023.
5. Build #1014, then #1015, then #1016/#1017; integrate through #1024.
6. Keep #1018–#1020 beta and non-blocking unless separately promoted.
7. Begin the stabilization cut only after #1023 and #1024 complete and current
   source-of-truth checks agree.

Only real contract dependencies serialize implementation. Corpus, differential,
documentation, diagnostics, and release-acceptance preparation proceed alongside
code.

## Deliberate non-goals

Unless a separate demand-backed design changes policy, these are not required
for stable-core completion:

- nested ODO (O5/O6);
- ODO over REDEFINES;
- RENAMES R4–R6 interactions with REDEFINES/OCCURS;
- `EXTERNAL` / `GLOBAL` clauses;
- Spark, Hadoop, S3, or VSAM connectors;
- transparent compression in the stable core;
- arbitrary record-length expression evaluation;
- authoritative framing auto-detection during decode;
- dynamic native/JVM plugin loading;
- general CSV parsing under a COBOL copybook.

Their rejection behavior, error/scenario identifiers, tests, support-matrix
status, and remediation guidance must still be complete where users can reach
them.

## Planned releases

- **v0.5.0** (shipped 2026-07-28): first facade-first Engineering Preview and
  registry publication.
- **v0.6.0** (shipped 2026-09-11): workspace convergence, crate retirement and
  the Rust 1.95 migration surface. See [migration/0.6.md](migration/0.6.md).
- **v0.6.1** (shipped 2026-09-14): canonical SHA-256 schema/source fingerprint
  corrections and packaged-documentation fixes.
- **v0.7.0** (shipped 2026-09-16): Rust 1.98, removal of the deprecated 0.6
  facade aliases, governed corpus/scenario foundations, VB/BDW framing, and
  `support --advise`. See [migration/0.7.md](migration/0.7.md).
- **v0.7.1** (shipped 2026-09-16): stable `support --advise` 1.0 envelope and
  release-lane truth/devex fixes.
- **v0.8.0** (shipped 2026-09-16): trust-and-velocity release-lane work,
  adapter dispositions, evidence close-out, and commercial-relicensing audit
  receipt.
- **v0.8.1** (shipped 2026-09-17): six wrong EBCDIC slots corrected against
  independent references, with pinned oracle fixtures and installed-binary
  witnesses.
- **v0.9.0** (next): operator diagnostics and evidence completion under #552,
  #551, and the selected release/devex repairs.
- **v0.10.0**: physical-record breadth under #1006, completed by registry-only
  acceptance in #1023.
- **v0.11.0**: heterogeneous-record breadth under #1006, completed by
  registry-only acceptance in #1024.
- **Stabilization cut**: four-week freeze, exact frozen-candidate registry proof,
  and GO/NO-GO under #189/#544/#545. Version assigned when scheduled; v1.0.0
  numbering is not implied.

## History

Historical milestone detail is preserved in:

- [archived/ROADMAP_v0.4.0_v0.5.0.md](archived/ROADMAP_v0.4.0_v0.5.0.md)
- [TESTING_COMMANDS.md](TESTING_COMMANDS.md)
- legacy issue [#75](https://github.com/EffortlessMetrics/copybook-rs/issues/75)

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
