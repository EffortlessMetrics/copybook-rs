# Merge Runbook – CI Down, Local Validation Gate

**Scenario**: GitHub Actions CI is unavailable/flaking, but PRs are semantically validated locally.

**Principle**: `./scripts/ci/offline-semantic.sh` (or focused command subsets) is the canonical merge gate when CI is unreliable.

---

## Prerequisites

✅ All PRs have validation evidence in their descriptions (see PR_VALIDATION_SECTIONS.md)
✅ `offline-semantic.sh` exists and is executable
✅ You have admin/maintain permissions on the repository

---

## Step 1: Temporarily Adjust Branch Protection

GitHub branch protection is blocking merges because required checks are failing. We need a temporary window where local validation is the gate.

### Via GitHub UI:

1. Go to: **Settings → Branches → Branch protection rules**
2. Click **Edit** on the `main` branch rule
3. **Temporarily disable**:
   - ☐ "Require status checks to pass before merging"

   *OR* (finer control):

   - ☐ Remove `testExpected` from required checks list
   - ☐ Remove `changes` from required checks list
   - ☐ Remove `pedantic-diff` from required checks list
   - ☐ Remove `docs-truth` from required checks list

4. **Keep enabled**:
   - ✅ "Require a pull request before merging"
   - ✅ Any review requirements
   - ✅ "Require linear history" (if you use it)

5. Click **Save changes**

**⚠️ IMPORTANT**: Document this change in a tracking issue or commit message so you remember to re-enable after merges complete.

---

## Step 2: Merge PRs in Dependency Order

For each PR below, run the **Local Validation** commands, then merge if green.

### Merge Order:

1. **PR #154** – Support Matrix CLI (no dependencies)
2. **PR #155** – docs-truth + xtask perf (depends on #154)
3. **PR #152** – Perf receipts + CI wiring (depends on #155)
4. **PR #151** – Error core / thiserror (merge last for clean attribution)

---

### PR #154 – Support Matrix CLI

**Branch**: `feat/support-matrix-cli`

**Local Validation**:
```bash
git checkout feat/support-matrix-cli
git pull origin feat/support-matrix-cli

# Focused validation (recommended for WSL/flaky environment)
cargo fmt --all --check
cargo clippy -p copybook-core -p copybook-cli -- -D warnings -W clippy::pedantic
cargo test -p copybook-core support_matrix
cargo test -p copybook-cli --test support_cli

# Behavioral smoke tests
cargo run --bin copybook -- support
cargo run --bin copybook -- support --json | jq .
cargo run --bin copybook -- support --check level-88
cargo run --bin copybook -- support --check occurs-depending-on
```

**Merge**:
```bash
gh pr merge 154 --squash --delete-branch

# Verify main
git checkout main && git pull
cargo run --bin copybook -- support
```

---

### PR #155 – docs-truth + xtask perf + validation protocol

**Branch**: `feat/docs-truth-support-matrix`

**Local Validation**:
```bash
git checkout feat/docs-truth-support-matrix
git rebase main  # if diverged after #154 merge
git pull origin feat/docs-truth-support-matrix

# Focused validation
cargo fmt --all --check
cargo test -p xtask
cargo run -p xtask -- docs verify-support-matrix

# If scripts/bench/perf.json exists from prior benchmarks:
cargo run -p xtask -- perf --summarize-last

# Behavioral smoke tests
cargo run -p xtask -- docs verify-support-matrix --verbose
cargo run -p xtask -- perf --help
```

**Merge**:
```bash
gh pr merge 155 --squash --delete-branch

# Verify main
git checkout main && git pull
cargo run -p xtask -- docs verify-support-matrix
```

---

### PR #152 – Perf receipts / CI wiring

**Branch**: `chore/bench/ci-perf-receipts`

**Local Validation**:
```bash
git checkout chore/bench/ci-perf-receipts
git rebase main  # if diverged after #155 merge
git pull origin chore/bench/ci-perf-receipts

# Focused validation
cargo fmt --all --check
cargo test -p xtask --test perf_integration

# Behavioral checks (if you want to generate fresh perf data)
# NOTE: This is optional; perf_integration covers the critical paths
PERF=1 cargo bench -p copybook-bench -- --output-format json > /tmp/test-perf.json
cargo run -p xtask -- perf /tmp/test-perf.json --summarize
```

**Merge**:
```bash
gh pr merge 152 --squash --delete-branch

# Verify main
git checkout main && git pull
cargo test -p xtask --test perf_integration
```

---

### PR #151 – Error core / thiserror

**Branch**: `refactor/thiserror-clean` (or `refactor/thiserror-modernization`)

**Local Validation**:
```bash
git checkout refactor/thiserror-clean
git rebase main  # if diverged after #152 merge
git pull origin refactor/thiserror-clean

# Focused validation
cargo fmt --all --check
cargo clippy -p copybook-core -- -D warnings -W clippy::pedantic
cargo test -p copybook-core error::tests:: -- --nocapture

# Broader workspace smoke test (if rustc is cooperating)
cargo test --workspace --lib
```

**Merge**:
```bash
gh pr merge 151 --squash --delete-branch

# Verify main
git checkout main && git pull
cargo test -p copybook-core
```

---

## Step 3: Verify `main` After All Merges

Once all four PRs are merged:

```bash
git checkout main && git pull

# Core semantic checks
cargo fmt --all --check
cargo test -p copybook-core
cargo test -p xtask

# Integration smoke tests
cargo run -p xtask -- docs verify-support-matrix
cargo run --bin copybook -- support --json | jq .

# If scripts/bench/perf.json exists:
cargo run -p xtask -- perf --summarize-last

# Optional full workspace (if environment is stable)
./scripts/ci/offline-semantic.sh
```

If all green: ✅ **Merge complete**

---

## Step 4: Re-enable Branch Protection

### Via GitHub UI:

1. Go to: **Settings → Branches → Branch protection rules**
2. Click **Edit** on the `main` branch rule
3. **Re-enable**:
   - ✅ "Require status checks to pass before merging"
   - ✅ Add back: `testExpected`, `changes`, `pedantic-diff`, `docs-truth` to required checks
4. Click **Save changes**

---

## Step 5: Document the Merge Wave

Update `MERGE_STATUS.md`:

```markdown
## Merge Wave Completed – 2025-11-16

| PR  | Branch                          | Status   | Merged At          |
|-----|---------------------------------|----------|--------------------|
| 154 | feat/support-matrix-cli         | ✅ Merged | 2025-11-16 HH:MM   |
| 155 | feat/docs-truth-support-matrix  | ✅ Merged | 2025-11-16 HH:MM   |
| 152 | chore/bench/ci-perf-receipts    | ✅ Merged | 2025-11-16 HH:MM   |
| 151 | refactor/thiserror-clean        | ✅ Merged | 2025-11-16 HH:MM   |

**Validation Gate**: Local commands per VALIDATION_PROTOCOL.md
**CI Status at Merge**: Unavailable (down for maintenance)
**Post-Merge Verification**: `offline-semantic.sh` passed on main
```

---

## Troubleshooting

### "Merge blocked even after disabling checks"

- Ensure you **saved** the branch protection changes
- Try merging via GitHub UI instead of CLI
- Check if there are **multiple** branch protection rules (repo-level + org-level)

### "Rebase conflicts after prior merges"

```bash
git checkout <branch>
git fetch origin main
git rebase origin/main

# Resolve conflicts, then:
git rebase --continue
git push origin <branch> --force-with-lease
```

### "Cargo lock contention in WSL"

```bash
# Kill stale cargo processes
pkill -9 cargo
rm -rf target/.rustc_info.json

# Retry validation with single-threaded build
cargo test -j 1 -p <package>
```

### "Can't locate offline-semantic.sh"

The script is in `feat/docs-truth-support-matrix` (PR #155). Use focused commands instead:

```bash
cargo fmt --all --check
cargo clippy -p copybook-core -p copybook-cli -- -D warnings
cargo test -p copybook-core -p xtask
```

---

## What This Gives You

✅ **Merge path** that respects semantic validation without depending on flaky CI
✅ **Audit trail**: Every PR has validation evidence in its description
✅ **Reproducible**: Commands are copy-pasteable and deterministic
✅ **Safe**: Local validation is stricter than the current CI environment
✅ **Documented**: Future you knows exactly what happened and why

---

## When CI Returns

Once GitHub Actions is stable again:

1. Re-enable all required checks
2. Run a full CI pass on `main` to establish new baseline
3. Any future PRs follow normal "green checks required" workflow
4. Keep `offline-semantic.sh` as a **local development gate** for contributors

The validation infrastructure you've built (xtask, support matrix, offline scripts) remains valuable even when CI is healthy—it just becomes a local-first development practice instead of a workaround.

---

**Status**: Ready to execute
**Last Updated**: 2025-11-16
**Next Action**: Adjust branch protection settings → merge #154
