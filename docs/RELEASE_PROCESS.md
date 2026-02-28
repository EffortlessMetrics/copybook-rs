<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Release Process

This document describes the automated release process for copybook-rs using `cargo-release` and `git-cliff`.

## Prerequisites

Install required tools:

```bash
cargo install cargo-release
cargo install git-cliff  # Already installed
```

Configure GPG signing (optional but recommended):

```bash
git config --global user.signingkey <your-gpg-key-id>
git config --global commit.gpgsign false  # We only sign tags, not commits
git config --global tag.gpgsign true
```

If GPG is not available, edit `release.toml` and set `sign-tag = false`.

## Pre-Release Validation Gates

All of the following gates must pass before executing any release. Running these checks ensures that formatting, linting, build integrity, test coverage, and documentation are in order.

| Gate | Command | Pass Criteria |
|------|---------|---------------|
| Format | `cargo fmt --all --check` | Exit 0 |
| Lint | `cargo clippy --workspace -- -D warnings -W clippy::pedantic` | Exit 0 |
| Build | `cargo build --workspace --release` | Exit 0 |
| Test | `cargo nextest run --workspace` | All pass |
| Doctests | `cargo test --workspace --doc` | All pass |
| Docs | `cargo doc --workspace --no-deps` | No warnings |
| Changelog | manual | CHANGELOG.md has release entry for target version |

## Release Readiness Gate

Before any publish, the `release-readiness` workflow now runs:

- `version-check` (tag and Cargo.toml workspace/crate versions aligned)
- `changelog-check` (release section exists in `CHANGELOG.md`)
- `non-wsl-benchmark-evidence` (requires `artifacts/perf/non-wsl/perf.json` with `non_wsl_evidence_status` in `available|available-from-cache`, a baseline date, and checksum)
- `publishable-crates-dry-run` (`cargo publish --dry-run` for `copybook-core`, `copybook-codec`, and `copybook-cli`)
- `sbom-provenance` (SBOM and provenance artifacts generated)
- `release-gate` (all required checks above must succeed)

This is now the required precondition for `.github/workflows/publish.yml` on tag-based releases.

To run all automated gates in sequence:

```bash
cargo fmt --all --check \
  && cargo clippy --workspace -- -D warnings -W clippy::pedantic \
  && cargo build --workspace --release \
  && cargo nextest run --workspace \
  && cargo test --workspace --doc \
  && cargo doc --workspace --no-deps
```

After the automated gates pass, manually verify that `CHANGELOG.md` contains an unreleased section with entries for the upcoming release.

## AC9 Validation (Optional, Non-Production)

AC9 monitoring paths are deterministic stubs and are disabled by default.
Validate them only when explicitly requested for controlled testing.
Use the shared, non-production/stub-only command documented in:

[Shared AC9 monitoring validation command](../README.md#ac9-monitoring-stub-validation-non-production-stub-only)

Release teams should not treat this as production audit evidence.

## Release Status Register (triaged blockers)

| Blocker | Status | Owner | Timestamp | Evidence |
|---|---|---|---|---|
| `TODO(AC5)` (`copybook-core/src/audit/security.rs`) | pass | @EffortlessSteven | 2026-02-28 | (status=pass; owner=@EffortlessSteven; evidence=[security audit implementation](copybook-core/src/audit/security.rs), [security audit path](copybook-cli/src/commands/audit.rs), [AC5 gate contract](copybook-core/src/audit/mod.rs)) |
| `TODO(AC11)` (`copybook-core/src/audit/performance.rs`) | pass | @EffortlessSteven | 2026-02-28 | (status=pass; owner=@EffortlessSteven; evidence=[performance objective checks](copybook-cli/src/commands/audit.rs), [AC11 baseline model](copybook-core/src/audit/performance.rs)) |
| `TODO(AC16)` (`copybook-core/src/audit/lineage.rs`) | pass | @EffortlessSteven | 2026-02-28 | (status=pass; owner=@EffortlessSteven; evidence=Parser and codec lineage IDs are emitted and compared in `run_lineage_analysis`.) |
| Environment-gate risk (`artifacts/perf/non-wsl/perf.json`) | fail | @EffortlessSteven | 2026-02-28 | Release readiness blocks RC unless this artifact has `non_wsl_evidence_status`=`available|available-from-cache`, a baseline date, and checksum. |

## Quick Reference

```bash
# Preview release (no changes made)
cargo release patch --dry-run

# Patch release (0.4.2 -> 0.4.3)
cargo release patch

# Minor release (0.4.2 -> 0.5.0)
cargo release minor

# Major release (0.4.2 -> 1.0.0)
cargo release major
```

## Release Workflow

When you run `cargo release <level>`, the following happens automatically:

1. **Version Bump**: Updates version in all workspace `Cargo.toml` files
   - Workspace root: `[workspace.package] version = "X.Y.Z"`
   - Individual crates: Inherit from workspace

2. **Changelog Update**: Runs `git-cliff` to update `CHANGELOG.md`
   - Extracts unreleased commits following conventional commits format
   - Groups by category (Added, Fixed, Documentation, etc.)
   - Links issue numbers to GitHub

3. **Git Commit**: Creates commit with message `chore(release): prepare vX.Y.Z`

4. **Git Tag**: Creates signed tag `vX.Y.Z` with message `Release vX.Y.Z`

5. **Push**: Pushes commit and tag to `origin` (triggers GitHub release CI)

6. **Manual Publishing**: Does NOT auto-publish to crates.io
   - Review the release on GitHub
   - Manually publish with `cargo publish -p <crate>` if desired
   - The `Publish Crates` workflow now runs release-readiness before attempting publish

## Detailed Steps

### 1. Prepare Release

Ensure working directory is clean:

```bash
git status  # Should be clean
git checkout main
git pull origin main
```

Run tests to ensure everything passes:

```bash
cargo build --workspace --release
cargo test --workspace
cargo clippy --workspace -- -D warnings
```

### 2. Preview Release

Use `--dry-run` to see what will happen:

```bash
cargo release patch --dry-run
```

Review the output:
- Version changes in `Cargo.toml` files
- CHANGELOG.md diff
- Git commit and tag details

### 3. Execute Release

If preview looks good, run without `--dry-run`:

```bash
cargo release patch  # or minor/major
```

This will:
- Prompt for confirmation at each step
- Update files
- Create commit and tag
- Push to remote

### 4. Verify Release

Check GitHub:

```bash
# View the tag
git tag -v v0.4.3  # Verify GPG signature

# View the release commit
git show v0.4.3

# Check GitHub releases page
open https://github.com/EffortlessMetrics/copybook-rs/releases
```

### 5. Publish to crates.io (Optional)

After verifying the release on GitHub:

```bash
# Publish core first (dependency order)
cargo publish -p copybook-core
sleep 10  # Wait for crates.io to index

cargo publish -p copybook-codec
sleep 10

cargo publish -p copybook-cli
cargo publish -p copybook-gen
cargo publish -p copybook-bench
# Note: xtask is typically not published
```

## Configuration

Release behavior is controlled by `release.toml` in the workspace root.

Key settings:
- `tag-name = "v{{version}}"`: Tag format
- `sign-tag = true`: GPG sign tags (set to `false` if GPG unavailable)
- `publish = false`: Don't auto-publish to crates.io
- `allow-branch = ["main"]`: Only allow releases from main branch
- `pre-release-hook`: Runs `git-cliff` to update CHANGELOG.md

## Troubleshooting

### GPG signing fails

If you see GPG errors:

```bash
# Option 1: Configure GPG
git config --global user.signingkey <key-id>

# Option 2: Disable signing
# Edit release.toml and set: sign-tag = false
```

### Changelog not updated

Ensure `git-cliff` is installed:

```bash
cargo install git-cliff
```

### Push fails

Check remote access:

```bash
git remote -v  # Verify origin URL
git push --dry-run origin main  # Test push access
```

### Dependency version mismatch

If you see version mismatch errors, ensure all workspace crates use exact versions for internal dependencies:

```toml
# In Cargo.toml workspace.dependencies
copybook-core = { version = "=0.4.2", path = "copybook-core" }
copybook-codec = { version = "=0.4.2", path = "copybook-codec" }
```

## Manual Changelog Update

To manually update the changelog without releasing:

```bash
# Update CHANGELOG.md with unreleased commits
git-cliff --unreleased --output CHANGELOG.md

# Or generate full changelog from scratch
git-cliff --output CHANGELOG.md
```

## Rollback

If something goes wrong after release:

```bash
# Delete local tag
git tag -d v0.4.3

# Delete remote tag (if already pushed)
git push origin :refs/tags/v0.4.3

# Reset to previous commit
git reset --hard HEAD~1

# Force push (dangerous - only if tag not yet published)
git push --force origin main
```

## Best Practices

1. **Always use `--dry-run` first** to preview changes
2. **Test thoroughly** before releasing
3. **Use semantic versioning**:
   - Patch (0.4.2 -> 0.4.3): Bug fixes only
   - Minor (0.4.2 -> 0.5.0): New features, backward compatible
   - Major (0.4.2 -> 1.0.0): Breaking changes
4. **Review CHANGELOG.md** after generation for accuracy
5. **Verify GPG signatures** on tags
6. **Wait for CI** to pass before publishing to crates.io
7. **Coordinate with team** before major releases

## Performance Gate Semantics

Two performance workflows now have explicit, different gate roles:

- `benchmark.yml`: historical benchmark baselines are generated and commented on, but outcomes are advisory only; failures do **not** fail the workflow.
- `perf.yml`: release-sensitive runs (PRs and `v*` tags) include a required regression gate. These runs must satisfy:
  - no >5% regression versus the current main baseline when comparison is available, and
  - throughput floor (`DISPLAY >= 80 MiB/s`, `COMP-3 >= 40 MiB/s`).
  - Throughput check failure in this context fails the workflow.

## See Also

- [cargo-release documentation](https://github.com/crate-ci/cargo-release)
- [git-cliff documentation](https://git-cliff.org/)
- [Conventional Commits](https://www.conventionalcommits.org/)
- [Semantic Versioning](https://semver.org/)
- [GIT_CLIFF_SETUP.md](../GIT_CLIFF_SETUP.md) - git-cliff configuration details
- [CHANGELOG_GENERATION.md](CHANGELOG_GENERATION.md) - Changelog generation details
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
