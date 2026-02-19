<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# git-cliff Setup for copybook-rs

This document describes the git-cliff automated changelog generation setup for the copybook-rs project.

## Files Created

1. **cliff.toml** - git-cliff configuration file
   - Conventional commit parsing (feat, fix, docs, etc.)
   - Grouping by type with Keep a Changelog style headers
   - Issue/PR number linking
   - Filters out dependency update commits
   - Matches existing CHANGELOG.md format

2. **.github/workflows/changelog.yml** - GitHub Actions workflow
   - Runs on push to main (incremental)
   - Manual trigger for full regeneration
   - Outputs as downloadable artifact (no auto-commit)

3. **docs/CHANGELOG_GENERATION.md** - Usage documentation
   - Local usage instructions
   - GitHub Actions workflow documentation
   - Commit conventions reference
   - Best practices

## Quick Start

### Install git-cliff

```bash
cargo install git-cliff --locked
```

### Generate Unreleased Changes

```bash
git cliff --config cliff.toml --unreleased --no-exec
```

### Generate Full Changelog

```bash
git cliff --config cliff.toml --no-exec --output CHANGELOG_new.md
```

## Workflow Behavior

### Automatic (Push to Main)

When code is pushed to `main`:
1. Workflow runs automatically
2. Generates incremental changelog for unreleased commits
3. Uploads `changelog-incremental` artifact (30 day retention)
4. No automatic commits - manual review required

### Manual (Full Regeneration)

From GitHub Actions UI:
1. Navigate to Actions → Changelog
2. Click "Run workflow"
3. Check "Regenerate full changelog"
4. Download `changelog-full` artifact (90 day retention)
5. Review and manually merge into CHANGELOG.md

## Configuration Highlights

### Commit Type Mapping

- `feat:` → **Added** section
- `fix:` → **Fixed** section
- `docs:` → **Documentation** section
- `perf:` → **Performance** section
- `refactor:` → **Changed** section
- `test:` → **Testing** section
- `release:`, `release-prep:` → **Release** section
- `ci:`, `chore:` → **Miscellaneous Tasks** section
- `build:` → **Build** section

### Automatic Filtering

These commits are skipped in the changelog:
- `chore(deps):`
- `build(deps):`
- `chore(pr):`
- `chore(pull):`

### Issue/PR Linking

Commit messages with `(#123)` are automatically converted to:
```
([#123](https://github.com/EffortlessMetrics/copybook-rs/issues/123))
```

## Design Decisions

1. **No Auto-Commit**: Changelog generation produces artifacts only. This allows for:
   - Manual review before updating CHANGELOG.md
   - Adding release notes and context
   - Quality control of public-facing content

2. **--no-exec Flag**: Prevents external command execution and GitHub API calls:
   - Avoids API rate limits
   - No dependency on network/GitHub tokens
   - Fully reproducible from local git history

3. **Remote Integration Disabled**: GitHub remote configuration is commented out:
   - Prevents API quota exhaustion
   - Works in CI without special tokens
   - PR numbers extracted from commit messages instead

4. **Incremental + Full Modes**:
   - Incremental (automatic): Shows unreleased changes for quick review
   - Full (manual): Regenerates entire changelog for major updates

## Integration with Existing Workflow

1. **Conventional Commits**: Project already uses conventional commit format
2. **Keep a Changelog**: Configuration matches existing CHANGELOG.md structure
3. **Manual Curation**: Generated content is a starting point, not a replacement
4. **Release Process**: Integrate into release workflow as needed

## Testing

The configuration has been tested locally and produces output matching the existing CHANGELOG.md format:

```bash
# Test unreleased changes
git cliff --config cliff.toml --unreleased --no-exec

# Test latest release
git cliff --config cliff.toml --latest --no-exec

# Test full changelog
git cliff --config cliff.toml --no-exec
```

## Next Steps

1. Review generated changelog output
2. Test workflow in CI (automatic trigger on next push to main)
3. Consider integrating into release process
4. Update contributing guidelines if needed

## References

- [git-cliff documentation](https://git-cliff.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [Conventional Commits](https://www.conventionalcommits.org/)
