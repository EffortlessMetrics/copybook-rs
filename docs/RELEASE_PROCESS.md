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

## See Also

- [cargo-release documentation](https://github.com/crate-ci/cargo-release)
- [git-cliff documentation](https://git-cliff.org/)
- [Conventional Commits](https://www.conventionalcommits.org/)
- [Semantic Versioning](https://semver.org/)
- [GIT_CLIFF_SETUP.md](../GIT_CLIFF_SETUP.md) - git-cliff configuration details
- [CHANGELOG_GENERATION.md](CHANGELOG_GENERATION.md) - Changelog generation details
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
