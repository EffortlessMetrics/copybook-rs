# Changelog Generation

This project uses [git-cliff](https://git-cliff.org/) for automated changelog generation based on conventional commits.

## Prerequisites

Install git-cliff:

```bash
cargo install git-cliff --locked
```

## Local Usage

### Generate Unreleased Changes

To see what changes would be added to the changelog for unreleased commits:

```bash
git cliff --config cliff.toml --unreleased --no-exec
```

### Generate Full Changelog

To regenerate the complete changelog from all tags:

```bash
git cliff --config cliff.toml --no-exec --output CHANGELOG.md
```

### Generate Changelog for Specific Range

```bash
git cliff --config cliff.toml --no-exec v0.4.0..HEAD
```

## GitHub Actions Workflow

The project includes an automated changelog workflow at `.github/workflows/changelog.yml`.

### Automatic Generation

- **Triggers**: Runs automatically on every push to `main`
- **Output**: Generates incremental changelog for unreleased commits
- **Artifact**: Available as `changelog-incremental` artifact for 30 days

### Manual Full Regeneration

To manually regenerate the full changelog:

1. Go to the Actions tab in GitHub
2. Select the "Changelog" workflow
3. Click "Run workflow"
4. Check "Regenerate full changelog"
5. Download the `changelog-full` artifact
6. Review and manually update `CHANGELOG.md` if desired

## Configuration

The changelog configuration is in `cliff.toml` at the repository root.

### Commit Conventions

The changelog generation follows these conventional commit types:

- `feat:` → Added
- `fix:` → Fixed
- `docs:` → Documentation
- `perf:` → Performance
- `refactor:` → Changed
- `style:` → Styling
- `test:` → Testing
- `ci:` → Miscellaneous Tasks
- `chore:` → Miscellaneous Tasks (or skipped for deps/pr)
- `build:` → Build
- `release:` → Release

### Filtering

The following commits are automatically filtered out:

- `chore(deps)`: Dependency updates
- `build(deps)`: Build dependency updates
- `chore(pr)`: PR-related chores
- `chore(pull)`: Pull request automation

### Issue/PR References

Commit messages with issue/PR numbers like `(#123)` are automatically converted to links:

```
feat: add new feature (#123)
```

becomes:

```
feat: add new feature ([#123](https://github.com/EffortlessMetrics/copybook-rs/issues/123))
```

## Best Practices

1. **Use Conventional Commits**: Follow the conventional commit format for automatic categorization
2. **Review Before Committing**: The workflow generates artifacts but doesn't auto-commit - review before updating CHANGELOG.md
3. **Manual Curation**: For releases, review the generated changelog and add additional context or notes as needed
4. **Consistency**: Keep the existing CHANGELOG.md format when merging generated content

## Notes

- The `--no-exec` flag is used to prevent external command execution (GitHub API calls)
- GitHub remote integration is commented out in `cliff.toml` to avoid API rate limits
- The workflow uses local git history only, no external API dependencies
- Changelog generation is reproducible and doesn't depend on network access
