<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Scenario ledger contract (#951)

The scenario ledger (`docs/evidence/scenario-ledger.toml`) is the one
machine-owned source for scenario evidence behind support claims. It answers,
per scenario: what the status is, which evidence layers apply, exactly where
each layer is proven, which formats/codepages/workers/CLI paths/errors apply,
and which full commit last verified the row.

## Ownership decision

#951 offers two options: extend an existing registry, or add one companion.
This design takes option 2, a companion, because the existing registries own
narrower planes that must keep their own gates:

| Artifact | Owns | Does not own |
| --- | --- | --- |
| `docs/evidence/scenario-ledger.toml` | scenario status and per-layer evidence anchors | plane detail (framing bytes, taxonomy coverage) |
| `docs/evidence/fixed-rdw-pipeline.toml` | fixed/RDW framing proof + content digest | scenario status |
| `docs/evidence/stable-errors.toml` | taxonomy coverage and per-code triggers | scenario status |
| `copybook-support-matrix` crate + `COBOL_SUPPORT_MATRIX.md` | feature status vocabulary and rendering | evidence anchors |
| `docs/stability/surface-registry.json` | package publish roles | scenario evidence |

Duplication is prevented by reference, not repetition: a ledger row links
plane evidence through `shared_evidence_relationships` using the other
registry's scenario or error IDs (e.g. `fixed-rdw:format.fixed.basic`,
`stable-error:CBKD301_RECORD_TOO_SHORT`). Copying their anchors into the
ledger is forbidden; the verifier rejects unknown relationship targets.

`COBOL_SUPPORT_MATRIX.md` remains a human rendering. Step 5 of #951 makes it
generated-from or verified-against the ledger; until then the ledger links
rows to matrix feature identities (`feature_identity`) without owning them.

## Row contract

Every `[[scenarios]]` entry records exactly these fields:

| Field | Shape |
| --- | --- |
| `scenario_id` | dotted ID, unique (`struct.odo.tail_fixed`) |
| `feature_identity` | support-matrix feature ID (`occurs-depending`) or `none` with reason |
| `support_status` | closed enum (below) |
| `stability_class` | `stable`, `beta`, or `reserved` |
| `required_evidence_layers` | subset of `parse`, `layout`, `decode`, `encode`, `round_trip`, `negative`, `cli` |
| `parse_evidence`, `layout_evidence`, `decode_evidence`, `encode_evidence`, `round_trip_evidence`, `negative_or_rejection_evidence`, `cli_evidence` | anchor or explicit N/A |
| `fixed_rdw_vb_applicability` | list subset of `fixed`, `rdw`, `vb`, or empty with reason in limitations |
| `codepage_applicability` | list of codepage IDs, `all`, or empty with reason |
| `worker_applicability` | `single`, `parallel`, `both`, or `not_applicable` with reason |
| `stable_error_or_rejection_identity` | `CBK*` code(s), `reserved:<name>`, or `none` with reason |
| `known_limitations_or_remediation` | free text; required when any layer is N/A or status is not `supported` |
| `shared_evidence_relationships` | `fixed-rdw:<id>`, `stable-error:<CODE>`, or `ledger:<scenario_id>` targets |
| `last_verified_full_sha` | 40-hex commit the row was last proven against |

## Closed enums

`support_status`: `supported`, `partial`, `beta`, `rejected`, `non_goal`.

- `supported`: every required layer has direct evidence; advertised claims
  may cite the row.
- `partial`: some required layers proven; the row states which are missing
  and why the claim stays bounded.
- `beta`: behavior exists but is not claim-backed (e.g. new VB rows before
  reconciliation).
- `rejected`: deliberately refused input; must name the exact stable error
  identity plus remediation or non-goal context.
- `non_goal`: out of scope by design; must name remediation or the
  explicitly deferred alternative.

`stability_class`: `stable` (claim-backed), `beta` (provisional),
`reserved` (identity held for a named future use, never advertised).

## Evidence anchors

An applicable layer holds a list of anchors:

```toml
decode_evidence = [
  { ref = "crates/copybook-codec/tests/streaming_file_tests.rs::decode_file_jsonl_format_lines", kind = "direct" },
]
```

`ref` is `path::test_fn`: a repo-relative file plus the test or helper
that executes the product path and asserts the behavior. An inapplicable
layer holds exactly one N/A entry with a governed reason:

```toml
encode_evidence = [{ kind = "not_applicable", reason = "rejection rows never encode" }]
```

Rules:

- An empty layer list is malformed. N/A must say why.
- `kind = "direct"` requires the referenced function to exist and the test
  to execute the claimed path; taxonomy snapshots, enumeration helpers,
  documentation mentions, and unasserted fixtures do not qualify.
- The verifier checks reference existence (file + symbol). Behavioral
  directness is asserted by the row author and spot-checked in review, then
  pinned by the row's verified SHA.

## Verification SHAs

`last_verified_full_sha` is the full 40-hex SHA of the commit whose tree the
row's anchors were proven against. Abbreviated SHAs are rejected, and the
SHA must resolve to a commit object in history: a well-formed SHA naming no
commit fails verification. Record a mainline commit, never a branch head
that squash-merge will orphan. Ancestry is not required, so equivalent
content survives squash/rebase without treating topology as behavior. In a
shallow checkout without that history the row reports explicitly unavailable
and skips freshness checks instead of counting as verified. The
freshness policy is explicit re-verification: touching a row's anchors
without updating its SHA fails the verifier only when the referenced content
moved (relationship targets are content-pinned by their own registries).
Every direct anchor is proven twice: against the working tree (fast local
signal) and against the claimed commit's tree (the SHA must contain the
anchor file and the anchored symbol there). A test added after the claimed
commit proves nothing about that commit. The freshness contract is scoped
to the anchored symbol's body: the extracted function item (`fn` through
its matching close brace) must be byte-identical in the claimed commit and
the working tree. A retained name with changed assertions fails the row,
while prose edits and new tests elsewhere in the anchor file leave it
valid. Identical bodies survive squash/rebase without treating topology as
behavior. This command is inventory validation only: it resolves anchors
against their claimed commits but never executes the anchored tests. A
passing verification is not an execution receipt; execution evidence comes
from running the owning test suites.

## Verifier failure catalog

`cargo run -p xtask -- docs verify-scenario-ledger` (also inside
`docs verify-all`) fails on:

- duplicate `scenario_id`;
- unknown `support_status`, `stability_class`, layer, format, or worker value;
- a `supported` row missing any required applicable layer;
- a `ref` whose file or symbol does not exist;
- a `rejected`/`non_goal` row without exact error identity and remediation;
- a `stable` row citing a `reserved` error identity;
- a `shared_evidence_relationships` target outside the known
  `fixed-rdw:`, `stable-error:`, `ledger:` namespaces or naming an unknown ID;
- an empty evidence layer (N/A reason required instead);
- a malformed or abbreviated verification SHA;
- a well-formed verification SHA that resolves to no commit object
  (or to a non-commit object);
- a direct anchor whose file is absent from the claimed commit's tree, or
  whose symbol that tree's file does not declare;
- a direct anchor whose symbol body differs between the claimed commit and
  the working tree (changed assertions fail; unrelated edits pass).

Failures print `scenario_id`, field, expected shape, actual reference, and
the repair command (`docs verify-scenario-ledger`, relevant sync, or the
owning test path).

## Migration

- #574 structural rows are imported first, converted to this row format with
  their final policy (supported vs policy-limited vs non-goal preserved).
- `fixed-rdw-pipeline.toml` scenarios and `stable-errors.toml` codes are
  linked, never copied.
- #571 (numeric), #572 (fixed/RDW + VB), #573 (codepage), #576 (errors) then
  reconcile coordinator by coordinator per #951 step 4.

## Non-goals

No new tests to fill cells, no support-vocabulary redesign, no second
hand-maintained Markdown matrix, no VB/BDW or diagnostics implementation
inside the ledger change, and no closure of #551 from schema existence.
