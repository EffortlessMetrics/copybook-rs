<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Release Runbook

**Purpose**: Repeatable release process for publishing `copybook-rs` crates to crates.io.

**Canonical status**: `docs/ROADMAP.md`

---

## Scope and invariants

This runbook is for release execution only. It documents a resumable, ambiguity-safe process that
prefers inspection, checkpoints, and fix-forward recovery. It intentionally does not treat yank as the
default rollback mechanism.

Non-goals:

- publishing directly from a dirty worktree
- publishing from non-`main` branches
- blind recovery by re-running everything
- normal cleanup by yank

---

## 0) Prepare the release changelog in a reviewed PR

Changie preparation happens before selecting the exact release commit. It assembles public release
notes; it does not choose the version, tag the repository, or publish anything.

Choose the target version through the normal versioning decision, then batch the accumulated fragments
with that explicit version:

```bash
VERSION="X.Y.Z"
changie batch "v${VERSION}"
changie merge
```

Review `.changes/v${VERSION}.md` and `CHANGELOG.md`. Curate the batched version file when the release
needs clearer migration or compatibility context, then run `changie merge` again. Do not use
`changie batch auto` to substitute fragment categories for the release version decision.

Before merging the release-preparation PR, prove the target release file exists and the tracked
changelog is reproducible:

```bash
test -f ".changes/v${VERSION}.md"
changie merge --dry-run > /tmp/CHANGELOG.md
diff -u CHANGELOG.md /tmp/CHANGELOG.md
```

Do not require `changie latest` to equal the target. A supported maintenance release can legitimately
batch below a newer release line; the target version file plus reproducible merged output is the
release-line-specific proof.

The release-preparation PR should contain the target `.changes/v${VERSION}.md`, the merged
`CHANGELOG.md`, the corresponding version changes, and any required migration documentation. Once that
PR is merged to `main`, continue below using its exact commit.

See [CHANGELOG_GENERATION.md](CHANGELOG_GENERATION.md) for normal fragment authoring and the migration
baseline.

## 1) Choose the exact release commit

1. Ensure you are on a clean `main` branch:

```bash
git fetch --all --tags
git checkout main
git pull
git status
```

1. Confirm the commit you will tag is exactly what passed the required gates:

```bash
git rev-parse HEAD
```

1. Verify the commit is not dirty:

```bash
test -z "$(git status --porcelain)"
```

---

## 2) Run required gates on the exact commit

```bash
just ci
```

and at minimum:

```bash
cargo run -p xtask -- docs verify-support-matrix
cargo run -p xtask -- publish plan --check
cargo run -p xtask -- publish plan
cargo run -p xtask -- publish plan --format json
```

If any step fails, do not proceed.

Local `just ci` output is pre-push proof only. The release proof is the CI
output on the exact release commit: record the run IDs or URLs for the
CI Quick, Changelog validate, and Publish Plan Check runs in the release
ticket (for example with `gh run list --branch main --limit 5`). A release
must never cite a local-only gate run as its evidence.

## 3) Capture release plan and evidence

The runbook uses the xtask-generated publish plan as the single publishable
crate source of truth. Its JSON entries retain package role,
version, dependency reason, and compatibility status for recovery and audit;
the workflow publishes the `package` field from each entry. The planner keeps
the established manifest-driven package set for supported 0.5.x patch and
security releases.

```bash
RELEASE_TAG="vX.Y.Z" # no leading whitespace
mkdir -p "release-state/${RELEASE_TAG}"
cargo run -p xtask -- publish plan --format json > "release-state/${RELEASE_TAG}/publish-plan.json"
wc -l "release-state/${RELEASE_TAG}/publish-plan.json"
```

Store the following next to the plan:

- `git rev-parse HEAD`
- CI run IDs or URLs for the quick, changelog, and publish-plan gates
- `git log --oneline -1 HEAD`
- a link to the release ticket or tag notes draft

These artifacts are the recovery point source for resumable publishes.

## 4) Tag and publish controls

1. Create and push the tag only after evidence capture is complete.

```bash
git tag -a "${RELEASE_TAG}" -m "copybook-rs ${RELEASE_TAG}"
git push origin "${RELEASE_TAG}"
```

The push starts `publish.yml` through its `push: tags: 'v*'` trigger. Publishing
is tag-only; if a run fails, retry that same tag-push workflow run through
GitHub Actions rather than opening a second publication entrance.

`publish.yml` uses `tools/xtask` plan output for publish order and count. There
is no approval click: the job still targets the `production` environment
(retained for continuity and secrets scoping), but the environment has no
required reviewers, so an authorized tag push publishes unattended.

The active `release-tags` repository ruleset is the authorization boundary. It
covers `refs/tags/v*`, blocks matching-tag creation/deletion/non-fast-forward
updates by default, and grants the intended Maintain/Admin repository roles the
release bypass. Verify that ruleset remains active before cutting a release.

---

## 5) Recovery model (resume + fix-forward)

Treat each publish step as potentially ambiguous unless a post-step verification proves success.

### On timeout or interruption

1. Pause and classify the failure as **ambiguous** until checked.
1. Inspect workflow logs and capture the last checkpoint crate index.
1. Record the checkpoint under `release-state/${RELEASE_TAG}/` locally before continuing.
1. For every crate before the failed checkpoint, verify it is visible on crates.io:

```bash
CRATE_NAME="copybook-core"
VERSION="X.Y.Z"
curl -sf "https://crates.io/api/v1/crates/${CRATE_NAME}/${VERSION}" | jq -r '.version.num'
```

1. Resume publishing from the first unchecked crate in `publish-plan.json`.

### On partial visibility

If a crate is present on crates.io at the target version but later crates failed, resume from the next
missing crate; do not republish prior crates.

### If crates.io state is unclear

If index lookups or API checks remain inconclusive:

- stop automated retries
- open an incident note on the release ticket
- do not yank as the first recovery action

---

## 6) Required post-release checks

Run clean-room verification for stable installation and docs references:

```bash
VERSION="X.Y.Z"
cargo install copybook-cli@${VERSION} --locked
cargo install copybook-cli@${VERSION} --locked --features arrow
```
For early local verification before crates are published, run:

```bash
RELEASE_SMOKE_DEPS=local \
  bash scripts/ci/release_smoke.sh "v${VERSION}"
```

This keeps the smoke workflow identical while resolving the smoke fixture crate
dependencies from the workspace checkouts. Set `RELEASE_SMOKE_PYTHON` to
`python3` or `python` if your shell requires a specific binary name.

Validate public visibility from `publish-plan.json` (all crates listed there, including `copybook` and
`copybook-rs`) on both crates.io and docs.rs. Docs availability is checked
from the plan by the `registry-docs` publish job
(`scripts/ci/docs_availability.py`): crate page versus built-docs URL per
package@version, with binary-only crates recorded as not-applicable and
pending builds as pending — never pass, never proven failure.

## 6b) Acceptance evidence, durable archive, and retrieval

The blocking smoke job writes a bounded sanitized acceptance receipt
(`copybook-acceptance-receipt/1`: probe/comparison outcomes, step timings,
fixture/toolchain/executable identities as hashes — no record payload) to
`RECEIPT_OUT`, asserts registry-only completed execution, and uploads the
receipt as a 90-day transport artifact. The `registry-docs` job uploads the
docs-availability receipt the same way. The `github-release` job attaches
both as release assets: the release is the durable archive, independent of
Actions retention.

Completion states are distinct — published, accepted, docs available,
archived, closed — and each is recorded, none inferred. The release notes
embed the acceptance and docs states (see the `github-release` job).

Retrieve the record after the local run directory is gone and after
Actions retention expires:

```bash
TAG="vX.Y.Z"
gh release download "${TAG}" \
  --pattern 'acceptance-receipt-*.json' \
  --pattern 'docs-availability-*.json'
python3 -c "import json,glob; [print(f, json.load(open(f))['status' if f.startswith('acceptance') else 'overall']) for f in sorted(glob.glob('acceptance-*.json') + glob.glob('docs-availability-*.json'))]"
```

A simulated unavailable Actions artifact does not destroy the release
record: re-download from the release above. A docs timeout yields a
resumable incomplete receipt (re-run the `registry-docs` job by re-running
the publish workflow's failed jobs); it never triggers republishing,
retagging, or a yank.

## 6c) Prebuilt CLI binaries

The `release-binaries` publish job builds `copybook-cli` in release mode
for four triples — `x86_64-unknown-linux-gnu` (ubuntu),
`aarch64-apple-darwin` (macOS Apple Silicon), `x86_64-apple-darwin`
(macOS Intel), `x86_64-pc-windows-msvc` (Windows) — and
attaches `copybook-<version>-<triple>.tar.gz` plus `.sha256` to the GitHub
release after `github-release` creates it. Each leg packages with
`scripts/ci/package_binaries.sh`, verifies the checksum, extracts, and
smokes its own binary (`--version`, `explain`, `doctor` and `decode` over
the bundled demo fixtures) before `gh release upload --clobber`.

Verify an asset after download:

```bash
TAG="vX.Y.Z"
gh release download "${TAG}" --pattern 'copybook-*.tar.gz*'
sha256sum -c copybook-*.tar.gz.sha256
```

A failed binaries leg never blocks or reopens publication: rerun the
failed matrix legs from the publish workflow run. Missing legs stay
missing (and visible) until rerun; the release notes name the intended
matrix regardless.

---

## 7) Rollback guidance

Normal recovery is resumable and fix-forward, not yank.

- Avoid yanking as a routine release recovery path.
- Use yank only when required by legal/security policy.
- For non-exceptional releases, prefer:
  - patch release over cleanup by yank
  - clear migration instructions in changelog and release notes

If yank is used, document:

- reason and approver
- exact crates and versions yanked
- whether a replacement patch release is planned

---

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
