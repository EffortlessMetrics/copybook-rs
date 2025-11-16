# Infrastructure Merge Wave – Tracking Issue

**Created**: 2025-11-15
**Target**: Merge validation protocol + tooling PRs to main
**Status**: 🟡 In Progress

---

## Overview

This tracking issue coordinates the merge of 4 infrastructure PRs that establish validation protocols, tooling, and error handling improvements for copybook-rs.

**Related Documentation**:
- [MERGE_PROTOCOL.md](docs/MERGE_PROTOCOL.md) – Merge order and gates
- [MERGE_STATUS.md](MERGE_STATUS.md) – Live status dashboard
- [VALIDATION_PROTOCOL.md](docs/VALIDATION_PROTOCOL.md) – Validation procedures

---

## PRs in Merge Wave

| PR | Feature | Status | Validation | Ready |
|----|---------|--------|------------|-------|
| [#154](https://github.com/EffortlessMetrics/copybook-rs/pull/154) | Support Matrix CLI | ⏳ Open | ✅ Complete | ✅ Yes |
| [#155](https://github.com/EffortlessMetrics/copybook-rs/pull/155) | docs-truth + xtask perf | ⏳ Open | ✅ Complete | ⏳ After #154 |
| [#152](https://github.com/EffortlessMetrics/copybook-rs/pull/152) | Perf receipts + CI | ⏳ Open | ✅ Complete | ⏳ After #155 |
| [#151](https://github.com/EffortlessMetrics/copybook-rs/pull/151) | Error core / thiserror | ⏳ Open | ✅ Complete | ⏳ After #152 |

---

## Merge Order & Rationale

### 1. PR #154 – Support Matrix CLI (First)

**Why First**:
- Zero dependencies on other PRs
- Low coupling, pure CLI feature
- Small blast radius

**Merge Checklist**:
- [x] Validation section in PR description
- [ ] Local `./scripts/ci/offline-semantic.sh` green
- [ ] CLI smoke tests pass
- [ ] Merge via `gh pr merge 154 --squash --delete-branch`
- [ ] Post-merge: main validation green
- [ ] Update `MERGE_STATUS.md`

**Post-Merge Verification**:
```bash
git checkout main && git pull
cargo run --bin copybook -- support
cargo run --bin copybook -- support --json | jq
./scripts/ci/offline-semantic.sh
```

---

### 2. PR #155 – docs-truth + validation protocol (Second)

**Why Second**:
- Provides xtask infrastructure for #152
- Requires support CLI from #154
- Pure tooling, no runtime changes

**Merge Checklist**:
- [x] Validation section in PR description
- [ ] #154 merged to main
- [ ] Rebase on main if needed
- [ ] Local `./scripts/ci/offline-semantic.sh` green
- [ ] Docs verification passes
- [ ] Perf summarization functional
- [ ] Merge via `gh pr merge 155 --squash --delete-branch`
- [ ] Post-merge: main validation green
- [ ] Update `MERGE_STATUS.md`

**Post-Merge Verification**:
```bash
git checkout main && git pull
cargo run -p xtask -- docs verify-support-matrix
cargo run -p xtask -- perf --summarize-last
./scripts/ci/offline-semantic.sh
```

---

### 3. PR #152 – Perf receipts + xtask perf CI (Third)

**Why Third**:
- Depends on xtask plumbing from #155
- Narrower scope (just CI wiring)
- Performance-focused, no semantic changes

**Merge Checklist**:
- [x] Validation section in PR description
- [ ] #155 merged to main
- [ ] Rebase on main if needed
- [ ] Local `./scripts/ci/offline-semantic.sh` green
- [ ] Perf receipt generation works
- [ ] Merge via `gh pr merge 152 --squash --delete-branch`
- [ ] Post-merge: main validation green
- [ ] Update `MERGE_STATUS.md`

**Post-Merge Verification**:
```bash
git checkout main && git pull
cargo run -p xtask -- perf --summarize-last
./scripts/ci/offline-semantic.sh
```

---

### 4. PR #151 – Error core / thiserror (Fourth/Last)

**Why Last**:
- Touches core semantics
- Highest coupling (affects error handling across workspace)
- Merging last makes regressions easy to attribute

**Merge Checklist**:
- [x] Validation section in PR description
- [ ] #152 merged to main
- [ ] Rebase on main if needed
- [ ] Local `./scripts/ci/offline-semantic.sh` green
- [ ] Targeted error tests pass
- [ ] Merge via `gh pr merge 151 --squash --delete-branch`
- [ ] Post-merge: main validation green
- [ ] Update `MERGE_STATUS.md`

**Post-Merge Verification**:
```bash
git checkout main && git pull
cargo test -p copybook-core --lib error::tests::
./scripts/ci/offline-semantic.sh
```

---

## CI Status

**Current State**: ⚠️ GitHub Actions Unstable (Nov 2025)

**Issues**:
- WSL2 environment + rustc ICE on benchmark compilation
- CI will be out for next couple weeks
- Local validation is canonical gate

**Mitigation**:
- All PRs validated locally via `./scripts/ci/offline-semantic.sh`
- Validation evidence documented in each PR description
- CI stability note added to all PRs

**When CI Recovers**:
- Actions will provide additional validation signal
- Local validation remains authoritative

---

## Validation Protocol

**Hard Gate** (Required before every merge):
```bash
./scripts/ci/offline-semantic.sh
```

This runs:
- Docs verification (`verify-support-matrix`)
- Format check
- Clippy (workspace + pedantic)
- Full test suite
- Build verification

**Per-Merge Workflow**:
1. Switch to PR branch
2. Run `./scripts/ci/offline-semantic.sh`
3. If green → merge
4. Switch to main, pull, re-run validation
5. Update `MERGE_STATUS.md`

See [MERGE_PROTOCOL.md](docs/MERGE_PROTOCOL.md) for complete procedures.

---

## Post-All-Merges System Verification

After all 4 PRs merged, run complete system validation:

```bash
# 1. Docs truth
cargo run -p xtask -- docs verify-support-matrix

# 2. Support CLI
cargo run --bin copybook -- support --json | jq

# 3. Perf summarization
cargo run -p xtask -- perf --summarize-last

# 4. Full validation
./scripts/ci/offline-semantic.sh

# 5. Performance baseline (optional)
cargo bench --package copybook-bench
cargo run -p xtask -- perf --summarize-last
```

**Success Criteria**: All commands ✅ green, no regressions.

---

## Progress Log

### 2025-11-15: Infrastructure Created

- ✅ Created `docs/MERGE_PROTOCOL.md`
- ✅ Created `MERGE_STATUS.md` dashboard
- ✅ Updated all 4 PR descriptions with validation sections
- ✅ Created this tracking issue template
- ⏳ Ready to begin merge sequence

### Next: PR #154 Merge

- [ ] Run final `./scripts/ci/offline-semantic.sh`
- [ ] Merge PR #154
- [ ] Verify main is green
- [ ] Update dashboard

---

## Troubleshooting

### offline-semantic fails

**Check**:
1. `cargo run -p xtask -- docs verify-support-matrix` (docs drift?)
2. `cargo fmt --all --check` (formatting?)
3. `cargo clippy --workspace` (lints?)
4. `cargo test --workspace` (test failures?)

**Fix** and re-run.

### Perf summarization fails

**Check**:
```bash
ls -la scripts/bench/perf.json
cargo run -p xtask -- perf --summarize-last
```

If missing, generate baseline:
```bash
cargo bench --package copybook-bench
mkdir -p scripts/bench
cp target/criterion/perf.json scripts/bench/perf.json
```

### Merge conflict

```bash
git checkout <branch>
git fetch origin
git rebase origin/main
# Resolve conflicts
git add <files>
git rebase --continue
git push --force-with-lease
./scripts/ci/offline-semantic.sh
```

---

## Success Criteria

- [ ] All 4 PRs merged to main
- [ ] Main branch passes `./scripts/ci/offline-semantic.sh`
- [ ] Support CLI functional
- [ ] Docs-truth verification working
- [ ] Perf summarization working
- [ ] No regressions in existing functionality
- [ ] `MERGE_STATUS.md` updated with final status

---

## References

- **MERGE_PROTOCOL.md**: Formal merge procedures
- **MERGE_STATUS.md**: Live dashboard
- **VALIDATION_PROTOCOL.md**: Validation command reference
- **PR_VALIDATION_SECTIONS.md**: Validation evidence for each PR

---

**Status Legend**:
- ✅ Complete
- ⏳ In Progress / Pending
- ❌ Blocked / Failed
- 🟢 Green / Passing
- 🟡 In Progress
- 🔴 Failed / Blocked

---

_Last updated: 2025-11-15_
_Next update: After each PR merge_
