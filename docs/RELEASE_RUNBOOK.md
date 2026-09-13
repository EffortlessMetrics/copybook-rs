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

## 3) Capture release plan and evidence

The runbook uses the xtask-generated publish plan as the single publishable
crate source of truth. For 0.6 releases, its JSON entries retain package role,
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
- gate command output
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
`copybook-rs`) on both crates.io and docs.rs.

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
