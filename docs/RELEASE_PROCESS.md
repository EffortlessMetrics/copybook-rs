<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Release Process (Overview)

This document is the authority and policy overview for releasing copybook-rs.
For step-by-step execution, use the canonical runbook:
[RELEASE_RUNBOOK.md](RELEASE_RUNBOOK.md). This overview intentionally does not
duplicate the runbook's step list; where the two could disagree, the runbook and
the generated publish plan win.

## Authority model

| Question | Authoritative source |
| --- | --- |
| Step-by-step release execution | [RELEASE_RUNBOOK.md](RELEASE_RUNBOOK.md) |
| Publishable package set and order | `cargo run -p xtask -- publish plan` |
| What the release automation does | `.github/workflows/publish.yml` |
| What the post-publish smoke proves | `scripts/ci/release_smoke.sh` |
| Versioning and compatibility policy | [STABILITY_GUARANTEES.md](STABILITY_GUARANTEES.md) |

The publish plan is generated from workspace metadata at release time. The
number of publishable packages and their dependency order come from the plan
output — never from a hand-maintained list in documentation. If documentation
and the plan disagree, the plan is correct and the documentation is stale.

Package roles in the plan:

- Component crates (`copybook-core`, `copybook-codec`, and the rest of the
  publishable workspace) publish first, in generated dependency order.
- `copybook` is the canonical facade crate: it re-exports the component crates
  and publishes after all of its component dependencies.
- `copybook-rs` is a redirect-only compatibility package that points users at
  the `copybook` facade. It publishes last.

## Pre-Release Validation Gates

All of the following gates must pass on the exact release commit before
tagging. The canonical combined local gate is `just pr` (also `just ci`).

| Gate | Command | Pass Criteria |
|------|---------|---------------|
| Format | `just fmt-check` (`cargo fmt --all -- --check`) | Exit 0 |
| Lint | `just lint` (clippy, workspace, pedantic) | Exit 0 |
| Build | `cargo build --workspace --release` | Exit 0 |
| Test | `just test` (nextest; excludes BDD and bench crates) | All pass |
| Doctests | `cargo test --workspace --doc` | All pass |
| Docs | `just docs` (`cargo doc --workspace --no-deps`) | No warnings |
| Changelog | manual | `CHANGELOG.md` has a section for the target version |
| Publish plan | `cargo run -p xtask -- publish plan --check` | Exit 0 |

## Automated release flow

A release is a single automated pipeline driven by
`.github/workflows/publish.yml`:

1. **Trigger**: push an annotated, signed `v*` tag from `main` (for example
   `v0.5.0`), or dispatch the workflow manually with the tag as input
   (`gh workflow run publish.yml -f tag=vX.Y.Z`).
2. **Tag-identity validation**: the tag must match
   `vMAJOR.MINOR.PATCH[-prerelease]`, must equal `[workspace.package].version`,
   and the checked-out commit must be the tagged commit. Any mismatch fails the
   run before anything is published.
3. **Preflight**: release build of the workspace, workspace tests (excluding
   `copybook-bench` and `copybook-bdd`), crates.io metadata validation
   (keywords/categories limits), and packaging spot-checks.
4. **Publish plan**: `xtask publish plan --check` regenerates and validates the
   plan; the JSON plan (package set, order, count, sha256) is archived as a
   workflow artifact and is the recovery reference for resumes.
5. **Publication**: crates publish in generated dependency order. The publisher
   honors crates.io rate limits (HTTP 429) with backoff derived from the
   registry's retry-after hint, treats an "already exists" response as a
   resume point, and waits for index propagation between crates.
6. **Blocking stable-core registry smoke** (`scripts/ci/release_smoke.sh`):
   installs `copybook-cli@<version>` from crates.io into a clean root and proves
   the published artifacts work: default CLI install, a clean-room consumer
   project exercising the `copybook` facade and the `copybook-rs` redirect
   (byte-identical behavior between them), fixed-length and RDW
   decode/encode/verify round-trips, and byte-identical output between
   single-worker and multi-worker runs. A failure here blocks the release.
7. **Advisory experimental-adapter smoke**: the same script in advisory mode
   (`RELEASE_SMOKE_ADVISORY=1`) installs the CLI with the `arrow` feature.
   Arrow and other experimental adapters are not part of the stable-core
   promise, so this job is non-blocking: a failure produces a follow-up issue,
   not a failed release.
8. **GitHub release**: created (or updated) only after publication and the
   blocking smoke pass, with per-crate crates.io/docs.rs links generated from
   the archived plan. crates.io publication is irreversible; the GitHub release
   is not, so it comes last. docs.rs links are informational — docs.rs builds
   asynchronously.

The publish job runs in the workflow's protected GitHub environment
(`production`); keep environment approval guardrails in place so publication
cannot start unreviewed.

## Failure and recovery policy

- **Fix forward.** Recovery from a partial publication is inspect-then-resume:
  classify the failure, verify crate visibility on crates.io, and resume from
  the first unpublished crate in the archived plan. The publisher already
  treats an existing version as a successful resume point.
- **Never yank as normal recovery.** Yank is reserved for legal/security
  exceptions and must be documented (reason, approver, exact crates and
  versions, replacement plan). For non-exceptional problems, prefer a patch
  release plus clear migration notes.
- **Never overwrite or move tags.** A published tag is immutable; a corrected
  release gets a new version and a new tag.

See [RELEASE_RUNBOOK.md](RELEASE_RUNBOOK.md) sections 5–7 for the detailed
recovery and rollback procedure.

## Version bumps and changelogs (local helpers only)

`release.toml` and `cliff.toml` configure optional local helpers
(`cargo-release`, `git-cliff`) that can draft a workspace version bump or
changelog entries while preparing a release PR. They are conveniences only —
they are **not** the release path. Publication happens exclusively through the
automated flow above, from a tag on `main` whose version equals
`[workspace.package].version`.

## See Also

- [RELEASE_RUNBOOK.md](RELEASE_RUNBOOK.md) — canonical execution runbook
- [STABILITY_GUARANTEES.md](STABILITY_GUARANTEES.md) — stability commitments
- [SUPPORT_POLICY.md](SUPPORT_POLICY.md) — support policy
- [Conventional Commits](https://www.conventionalcommits.org/)
- [Semantic Versioning](https://semver.org/)

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
