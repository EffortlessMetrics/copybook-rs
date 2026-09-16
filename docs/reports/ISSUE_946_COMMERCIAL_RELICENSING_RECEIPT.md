<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Issue #946 receipt: commercial-relicensing rights and incorporated-code provenance

**Status**: audit complete, one pre-CLA item recorded for maintainer sign-off.\
**Method**: GitHub PR-authorship census over all 530 merged PRs (authoritative;
local clone is shallow so git archaeology is partial), mainline author
enumeration, file-level provenance checks, license-metadata reconciliation.\
**Verified**: 2026-09-17 against `main` at f3246135.\
**Public distribution remains `AGPL-3.0-or-later`** (workspace `Cargo.toml`,
root `LICENSE`). The CLA preserves commercial relicensing rights. This receipt
does not introduce permissive licensing, pricing, or support terms.

## Contribution classes

### 1. Maintainer contributions

- identity: EffortlessSteven (Steven Zimmerman), all committer emails
- provenance: original authorship, 496/530 merged PRs plus direct pushes
- coverage basis: copyright holder; CLA self-grant
- disposition: **clear** — no third-party rights involved

### 2. Dependabot version bumps

- identity: app/dependabot, 33/530 merged PRs
- provenance: automated `Cargo.toml`/`Cargo.lock` version-number bumps only
- coverage basis: no copyrightable authorship; ordinary dependency maintenance
- disposition: **clear and excluded** — not incorporated code

### 3. Agent-authored PR #13 (pre-CLA, shipped)

- identity: app/google-labs-jules; merged 2025-09-03 by EffortlessSteven
- paths: `docs/archived/INVESTIGATION_REPORT.md`,
  `tools/copybook-gen/examples/roundtrip_repro.rs` (relocated since; content survives)
- provenance: agent acting at maintainer direction; merger acceptance on record
- coverage basis: **pre-CLA gap** — `CLA.md` first landed 2025-10-03, one month
  after this merge, so no CLA grant covers it. The surviving files are a dev-tool
  repro example and an archived investigation report; the repro carries the
  project SPDX header (`AGPL-3.0-or-later`).
- disposition: **include, pending maintainer sign-off** — the sole residual
  item. Remediation if sign-off is withheld: rewrite the repro example and drop
  the archived report (both are non-structural).

### 4. Unmerged agent branches (not shipped)

- identity: google-labs-jules perf branches, `claude/*` session branches
- provenance: verified NOT ancestors of `main` (e.g. 4952b1a9, 0b63d756,
  d8e9876a, 0e3416ed); Claude commits are branch-merge bookkeeping only
- disposition: **excluded** — out of scope unless and until merged, at which
  point the then-current CLA process applies

## Incorporated-code inventory (non-dependency)

- `vendor/`, `third_party/`: do not exist — nothing vendored.
- EBCDIC/codepage tables (`crates/copybook-charset`, `crates/copybook-codepage`):
  hand-authored mapping code with no third-party provenance header; mappings are
  functional facts implemented originally in-repo.
- Fixtures/binary records: generated in-repo by `tools/copybook-gen`; no
  external corpus incorporated.
- Crypto-shaped test inputs: generated at test runtime (`tests/e2e`
  `uselesskey` runtime fixtures) — no secret-like blobs committed.
- Ordinary Cargo dependencies: out of scope per #946 (distinct from
  incorporated code); gated separately by `cargo deny` (`deny.toml`).

## Metadata reconciliation

- Root `LICENSE` (+ `LICENSE_SCOPE.md`) and workspace `license =
  "AGPL-3.0-or-later"` agree with `CLA.md`'s AGPL-public/commercial model.
- SPDX headers: source sweep clean (e.g. all `copybook-codec` sources carry
  the identifier); new files are required to keep the header per AGENTS.md.
- Contribution policy (`CLA.md` + PR process) matches the settled model.

## Remaining for #946 closure

Maintainer sign-off on item 3 (or its rewrite/removal). No other blocker found.
