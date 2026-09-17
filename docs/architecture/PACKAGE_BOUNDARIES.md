<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Package boundaries

`copybook-rs` designs implementation seams aggressively but publishes packages conservatively. A Cargo package is a distribution and compatibility promise, not merely a convenient folder.

## Decision rule

A production seam remains a published package only when it is one of:

1. a canonical product or application entrypoint;
2. an independently useful domain engine;
3. a shared leaf required by multiple surviving public packages without dependency inversion;
4. a real external-format or ecosystem adapter;
5. the permanent `copybook-rs` search alias; or
6. a finite compatibility package during an already-published migration.

Single-owner implementation seams remain explicit module families under their true owner. They keep focused folders, narrow visibility, invariant tests, fuzz/property targets, and review ownership without retaining a separate `Cargo.toml` or crates.io support promise.

## Required package evidence

A retained public package must name an external user story, its invariant, its dependency direction, its stability class, and why a module is insufficient. Compatibility and retiring package names must instead state their finite migration story, forwarding/no-new-implementation invariant, and why package retention is temporary. The authoritative decisions live in `docs/stability/surface-registry.json` under each package's `boundary` object. Consumer and publish-closure data are generated from Cargo metadata by the architecture checker tracked in #644.

## Migration rules

- Never yank 0.5.0 artifacts.
- Primary packages never depend on forwarding packages.
- Ownership moves and behavior corrections use separate PRs whenever practical.
- Compatibility packages contain only re-exports, deprecations, migration documentation, and compile-contract tests.
- Deep packages already classified internal-only receive no automatic forwarding release; they remain available at 0.5.0 and stop receiving versions once primary consumers move.
- `copybook-rs` remains a permanent alias containing only `pub use copybook::*;`.

## Working primary family for 0.6.0

The current target is `copybook`, `copybook-rs`, `copybook-core`, `copybook-codec`, `copybook-error`, `copybook-charset`, `copybook-cli`, `copybook-arrow`, and `copybook-audit` (experimental enterprise-audit adapter moved out of the stable core per #656 Phase E). `copybook-fixed` is retained as a primary external-format package after the clean-room proof in `examples/copybook-fixed-clean-room`; `copybook-rdw` remains conditional on its own schema-independent direct-use proof. Governance and support-matrix packages remain conditional rather than being promoted by topology alone.

## Audit/governance boundary decision (#553, recorded 0.8.0)

#553 asked for one of: (1) move audit capabilities into the governance family
and keep `copybook-core` focused, or (2) retain audit in core but exclude it
from the stable-core contract. The as-built answer is option 1, verified
rather than assumed:

- `copybook-audit` is a separate crate depending only on stable
  (`copybook-core`, `copybook-error`); `copybook-governance` likewise depends
  only on `copybook-core` and `copybook-support-matrix`.
- `copybook-core` has no dependency on audit, governance, Arrow, codec, or
  CLI. The `core-upward` rule in `tools/xtask/src/architecture.rs` rejects
  any such edge, and `architecture check` runs it on every gate.
- Audit is not re-exported by the `copybook` facade; Arrow is CLI-opt-in
  only; governance travels with its beta class through `copybook::governance`.
- Kafka remains workspace-excluded examples, never published crates.

Audit, governance, Arrow/Parquet, and Kafka are therefore excluded from the
stable-core contract while keeping their current classes (experimental, beta,
experimental, experimental-examples). Per-surface dispositions and graduation
evidence live in `docs/stability/surface-registry.json` and #984–#987.

Stable-envelope exception (#985): the `support --advise` machine envelope
(`schemas/advise-result.json`, `schema_version` `1.0`, stable since 0.7.1) is
not part of the beta governance/support-matrix classification. A beta family
label authorizes no incompatible change to that envelope, and a stable child
schema is not erased by its package's beta status. Automation must match
`schema_version` exactly and never parse human prose.

Dependency presence versus runtime use (#985): the CLI links
`copybook-governance` as a normal dependency, but governance *policy*
(`governance_states`, bindings plus feature-flag evaluation) executes only
under the explicit opt-in `--with-governance` flag. The default path uses
`support_states`, documented as support rows *without* runtime governance
mapping, and the `--advise` path never receives governance state at all:
advise output is byte-identical with and without the flag (pinned by
`support_advise_ignores_governance_flag_on_stable_path`). Dependency
presence therefore proves linkage, never default policy execution.

Exercised governance behavior with exact results (#985): `support --format
json --with-governance` emits a 7-row governed matrix carrying
`runtime_enabled`, `required_feature_flags`, and `missing_feature_flags`
per row (pinned by `support_json_with_governance_outputs_runtime_fields`
and `support_check_with_governance_includes_runtime_flags`); that output
is byte-identical across runs and carries no filesystem paths (pinned by
`support_governance_output_is_deterministic_and_path_free`). The
`core-upward` rule in `tools/xtask/src/architecture.rs` rejects
`copybook-core` edges to codec, CLI, Arrow, audit, and governance, and
`architecture check` runs it on every gate.

Explicitly unverified, with next actions: the measured runtime cost of
the governance path is unproven (next: trace under `copybook-bench`
before any cost claim); per-criterion lifecycle/state-transition
graduation proof beyond the current runtime suite is unproven (next:
link one test per graduation criterion under #553). Neither gap blocks
the beta disposition recorded above.
