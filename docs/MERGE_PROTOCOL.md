<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Merge Protocol for Infrastructure & Tooling PRs

## Goals

- Avoid merging partially validated infra changes
- Keep main in a state where `./scripts/ci/offline-semantic.sh` is always green
- Make merge behavior reproducible and auditable
- Provide clear validation evidence for each PR

## Agentic Review Loop

When a maintainer delegates an issue and PR queue, routine review-bot handling
is part of the normal merge protocol. The agent addresses every actionable
comment that remains within the selected concern, runs the relevant proof,
pushes the reviewed head, and resolves the addressed thread. A second
authorization is not required for those routine operations.

The agent still stops for ambiguous or conflicting feedback, material scope
expansion, force-push or direct-main requests, release or deployment actions,
secrets, or missing required checks. Informational, duplicate, outdated, and
rate-limit bot messages do not block a lane by themselves. The complete state
machine and authority boundary live in
[`docs/design/AGENTIC_PR_OPERATIONS.md`](design/AGENTIC_PR_OPERATIONS.md).

The merge decision must use the current GitHub head: required checks are green,
the PR is mergeable, and no actionable review thread remains unresolved.

## Local Gates

### Hard Gate (Required for ALL infra PRs)

```bash
./scripts/ci/offline-semantic.sh
```

This script runs:
- Docs verification (`cargo run -p xtask -- docs verify-support-matrix`)
- Format check (`cargo fmt --all --check`)
- Clippy with pedantic lints
- Full workspace test suite
- Build verification

### Optional Gates (When Environment is Stable)

```bash
./scripts/ci/quick.sh          # Fast format + clippy + core tests
./scripts/ci/offline-all.sh    # Full validation including benches
```

> **Note**: `offline-all.sh` is reserved for stable hosts. Avoid on WSL2 with rustc ICE issues.

## Standard Merge Order (Current Batch)

For the current wave of infrastructure PRs (2025-11):

1. **PR #154** – Support Matrix CLI
   - Low coupling, user-visible feature
   - No dependencies on other PRs
   - Validation: CLI smoke tests + offline-semantic

2. **PR #155** – docs-truth + validation protocol + xtask perf
   - Infrastructure backbone
   - Depends on: support CLI merged to main
   - Validation: docs-truth verification + perf summarization

3. **PR #152** – Perf receipts + xtask perf CI wiring
   - Narrower scope, depends on xtask plumbing from #155
   - Validation: perf receipt generation + summarization

4. **PR #151** – Error core / thiserror
   - Touches core semantics
   - Merge last to isolate any regressions
   - Validation: targeted error tests + offline-semantic

> **Rationale**: This order minimizes merge conflicts and makes regressions easy to attribute.

## Per-PR Merge Checklist

Before merging any infrastructure PR:

### 1. Validation Evidence

Ensure PR description contains a `## Semantic Validation` section with:
- Commands run
- Adversarial tests performed (if applicable)
- Manual verification steps (if applicable)
- Performance evidence (for perf-related PRs)

Example:
```markdown
## Semantic Validation

**Commands Run**:
```bash
./scripts/ci/offline-semantic.sh
cargo run -p xtask -- perf --summarize-last
cargo run --bin copybook -- support --json | jq
```

**Evidence**: All gates green, support CLI verified with 7 feature entries.
```

### 2. Local Validation

On your local branch for that PR:

```bash
# Switch to PR branch
git checkout <branch-name>
git pull origin <branch-name>

# Run hard gate
./scripts/ci/offline-semantic.sh
```

If any failures:
- Fix on the branch
- Push updates
- Re-run validation
- Update PR description with new evidence

### 3. Merge Execution

If validation is green:

```bash
# Merge via gh CLI (preferred)
gh pr merge <number> --squash --delete-branch

# Or merge in GitHub UI with:
# - Squash and merge
# - Delete branch after merge
```

### 4. Post-Merge Validation

Immediately after merge:

```bash
# Update local main
git checkout main
git pull origin main

# Verify main is still green
./scripts/ci/offline-semantic.sh
```

If post-merge validation fails:
- Investigate immediately
- Consider reverting if fix is not trivial
- Document incident in tracking issue

### 5. CI Stability Note

If GitHub Actions are unstable at merge time, do not merge around the required
checks. Record local evidence in the PR while waiting for the current GitHub
checks to recover, or stop and request direction if the required check cannot
be restored:

```markdown
## CI Status

GitHub Actions currently unstable (rustc ICE / environment issues).
Local semantic validation (`./scripts/ci/offline-semantic.sh`) is supporting
evidence only; required GitHub checks must still be green before merge.

Evidence: [paste offline-semantic output]
```

## Merge Workflow Example

Complete example for merging PR #154:

```bash
# 1. Local validation
git checkout feat/support-matrix-cli
git pull origin feat/support-matrix-cli
./scripts/ci/offline-semantic.sh
# ✅ All checks passed

# 2. Optional: smoke test the feature
cargo run --bin copybook -- support
cargo run --bin copybook -- support --json | jq

# 3. Merge
gh pr merge 154 --squash --delete-branch

# 4. Post-merge validation
git checkout main
git pull origin main
./scripts/ci/offline-semantic.sh
# ✅ Main still green

# 5. Document in tracking issue
echo "✅ PR #154 merged successfully" >> MERGE_STATUS.md
```

## Troubleshooting

### offline-semantic fails due to docs drift

**Symptom**: `verify-support-matrix` reports discrepancies

**Fix**:
1. Run `cargo run -p xtask -- docs verify-support-matrix` to see exact diff
2. Update either:
   - `COBOL_SUPPORT_MATRIX.md` (if registry is correct)
   - Feature registry in code (if docs are correct)
3. Re-run validation

### Perf summary fails

**Symptom**: `xtask perf --summarize-last` can't find perf.json

**Fix**:
```bash
# Check if perf.json exists
ls -la scripts/bench/perf.json

# If missing, generate baseline
cargo bench --package copybook-bench
mkdir -p scripts/bench
cp target/criterion/perf.json scripts/bench/perf.json

# Retry summarization
cargo run -p xtask -- perf --summarize-last
```

See `docs/VALIDATION_PROTOCOL.md` for more examples.

### WSL2 rustc ICE during benches

**Symptom**: `offline-all.sh` crashes with internal compiler error

**Workaround**:
- Use `offline-semantic.sh` instead (no benches)
- Run targeted tests: `cargo test -p copybook-core`
- Skip `cargo bench` until rustc upgrade

### Merge conflict on main

**Symptom**: PR shows conflicts after another PR merged

**Fix**:
```bash
# On PR branch
git checkout <branch-name>
git fetch origin
git merge --no-edit origin/main

# Resolve conflicts
git add <resolved-files>
git commit

# Publish the non-rewriting update
git push origin <branch-name>

# Re-run validation
./scripts/ci/offline-semantic.sh
```

If resolving the conflict requires a product decision or expands the selected
concern, stop and request direction rather than rewriting the remote branch.

## Validation Protocol Reference

See `docs/VALIDATION_PROTOCOL.md` for detailed validation procedures:
- Performance measurement with `xtask perf`
- Docs-truth verification
- Golden fixture validation
- Adversarial testing patterns

## Status Tracking

Track merge progress in `MERGE_STATUS.md` or in a dedicated tracking issue.

Example tracking table:

| PR | Feature | Validation | Status |
|----|---------|------------|--------|
| #154 | Support CLI | ✅ offline-semantic | ⏳ Ready |
| #155 | docs-truth + xtask | ✅ offline-semantic + perf | ⏳ Ready |
| #152 | Perf receipts | ✅ offline-semantic | ⏳ Blocked by #155 |
| #151 | Error core | ✅ offline-semantic + targeted tests | ⏳ Blocked by #152 |

## Post-Merge Verification

After all 4 PRs are merged, verify the complete system:

```bash
# 1. Docs-truth
cargo run -p xtask -- docs verify-support-matrix

# 2. Support CLI
cargo run --bin copybook -- support --json | jq

# 3. Perf summarization
cargo run -p xtask -- perf --summarize-last

# 4. Full validation
./scripts/ci/offline-semantic.sh

# 5. Performance baseline
cargo bench --package copybook-bench
cargo run -p xtask -- perf --summarize-last
```

All should be green and functional.

## Maintenance

Update this protocol when:
- New infrastructure PRs require different merge order
- New validation gates are added
- CI environment changes (e.g., Actions become stable again)
- Merge conflicts reveal coupling not captured here

Keep `MERGE_PROTOCOL.md` synchronized with `VALIDATION_PROTOCOL.md` for consistency.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
