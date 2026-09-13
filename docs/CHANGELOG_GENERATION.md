<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Changelog Management

copybook-rs uses [Changie](https://changie.dev/) to collect release-note fragments during normal development and assemble the canonical root `CHANGELOG.md` in a reviewed release-preparation pull request.

Changie manages changelog text only. It does **not** choose the release version, tag the repository, publish crates, or replace the release controls in [RELEASE_RUNBOOK.md](RELEASE_RUNBOOK.md).

## Files and source of truth

- `.changie.yaml` — Changie configuration and public changelog categories.
- `.changes/unreleased/` — one small YAML fragment per notable unreleased change.
- `.changes/vX.Y.Z.md` — batched, reviewable release notes for a version.
- `.changes/header.tpl.md` — stable changelog header.
- `CHANGELOG.md` — canonical public changelog assembled by `changie merge`.
- `.changes/v0.6.0.md` — the 0.6.0 release section from the pre-Changie changelog.
- `.changes/v0.5.0.md` — the 0.5.0-and-earlier historical baseline from the pre-Changie changelog.

The root changelog remains the public source of truth. The `.changes/` tree is the evidence used to reproduce it and to prepare the next release without deriving public notes from commit-message heuristics.

The historical baseline is deliberately split at the supported release-line boundary. Keeping 0.6.0 separate from the 0.5.0-and-earlier history means a future 0.5.x maintenance release sorts below 0.6.0 and above 0.5.0 when Changie merges the version files.

## Install Changie

CI pins Changie v1.26.0. Use that version locally so validation and release preparation run against the same implementation. For a Go source install:

```bash
go install github.com/miniscruff/changie@v1.26.0
```

Changie injects its displayed release version through GoReleaser, so a binary built by `go install` may report `vdev`. The embedded Go module metadata still records the selected source version:

```bash
go version -m "$(command -v changie)" | grep github.com/miniscruff/changie
```

The official Changie release binary reports `v1.26.0` directly. Other supported installation methods are documented by Changie; keep the installed version aligned with the CI pin.

## Add a change fragment

For a notable user-facing, compatibility, security, or release-relevant change, run:

```bash
changie new
```

Select the category and write the public-facing change. The configured categories match the project changelog vocabulary:

- `Added`
- `Changed`
- `Deprecated`
- `Removed`
- `Fixed`
- `Security`

For scripted or agentic work, the same operation can be non-interactive:

```bash
changie new --kind fixed --body '**codec**: Reject malformed numeric input instead of silently defaulting the field'
```

Commit the generated file under `.changes/unreleased/` with the code or documentation change it describes. Ordinary pull requests should **not** hand-edit `CHANGELOG.md`; batching happens once, in release preparation.

Not every PR needs a fragment. Internal refactors, tests, CI maintenance, dependency churn, formatting, and documentation corrections that do not belong in public release notes may omit one.

## Validate locally

The lightweight changelog checks are:

```bash
# Parse and sort unreleased fragments without writing a release file.
changie batch patch --dry-run

# Rebuild released history without changing CHANGELOG.md.
changie merge --dry-run > /tmp/CHANGELOG.md

diff -u CHANGELOG.md /tmp/CHANGELOG.md
```

The `patch` argument in the dry run is only a validation version. It does not select the real release version or write files.

The `Changelog` GitHub Actions workflow runs the same checks on pull requests and on pushes to `main`. It also reads `[workspace.package].version` from `Cargo.toml` and requires the matching `.changes/vX.Y.Z.md` file, so a version bump cannot merge without its batched release notes.

## Prepare a release

Choose the release version through the normal release process. Then batch the accumulated fragments with that **explicit** version; do not use `changie batch auto` as a substitute for the project's versioning decision.

```bash
VERSION="X.Y.Z"

changie batch "v${VERSION}"
changie merge

git diff -- \
  CHANGELOG.md \
  ".changes/v${VERSION}.md" \
  .changes/unreleased
```

Review and, when useful, curate `.changes/v${VERSION}.md` before the final `changie merge`. That version file is the release-note candidate; edits remain explicit and reviewable rather than being regenerated from git history.

The finished release-preparation PR must leave:

1. the target version file present under `.changes/`;
2. its fragments removed from `.changes/unreleased/` by the batch operation;
3. `CHANGELOG.md` reproducible by `changie merge --dry-run`; and
4. the public notes reviewed for migration or compatibility guidance required by project policy.

The target file is the release-line-specific proof. Do not require `changie latest` to equal the target: a supported maintenance release may be lower than a newer version already present in `.changes/`.

After that PR is merged, follow [RELEASE_RUNBOOK.md](RELEASE_RUNBOOK.md) for the exact-commit gates, tag, protected publication workflow, smoke tests, and recovery procedure.

## Conventional commits

The repository still uses Conventional Commit-style commit and PR titles because they are useful development metadata. They are no longer the changelog source. Public release notes come from Changie fragments, so a squash title or internal commit sequence cannot silently rewrite the release narrative.

## Historical migration

Changie was adopted after the 0.6.0 changelog already existed. The migration keeps the public text unchanged while storing it in two ordered inputs: the 0.6.0 section in `.changes/v0.6.0.md`, and the 0.5.0-and-earlier record in `.changes/v0.5.0.md`. The split preserves the supported 0.5.x maintenance insertion point without forcing a cosmetic reconstruction of every older release section.

Together with `.changes/header.tpl.md`, those files make `changie merge` reproduce the canonical changelog byte for byte before any new release is batched.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
