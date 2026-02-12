# API Freeze Documentation

This document describes the API freeze policy and procedures for the copybook-rs project.

## Overview

API freeze is a period during which no changes to the public API are allowed. This ensures stability for users and allows for thorough testing before a release. The freeze is enforced through CI checks and manual processes.

## What is API Freeze?

API freeze is a development phase where:
- The public API of published crates is locked
- Only certain types of changes are allowed
- CI checks enforce the freeze automatically
- Release preparation can proceed with confidence

## Public API Scope

The following crates have frozen public APIs:
- **copybook-core**: Core parsing and schema types
- **copybook-codec**: Encoding and decoding codecs
- **copybook-cli**: Command-line interface (binary crate)

The public API includes:
- All `pub` functions and methods
- All `pub` structs and their fields
- All `pub` enums and their variants
- All `pub` traits and their methods
- All `pub` type aliases
- All `pub` constants

## Allowed Changes During Freeze

### Documentation Changes (docs/)
- API documentation updates
- README improvements
- Example additions
- Tutorial updates
- Architecture documentation

### Benchmark Changes (copybook-bench/)
- New benchmarks
- Benchmark infrastructure updates
- Performance measurement improvements
- Baseline updates

### Test Changes (tests/)
- New test cases
- Test infrastructure improvements
- Bug fixes in tests
- Test coverage improvements

### CI/CD Changes (.github/, scripts/)
- Workflow improvements
- Build script updates
- Tooling improvements
- Documentation generation

## Prohibited Changes During Freeze

Any changes that affect the public API are prohibited:

### Function/Method Changes
- Adding or removing public functions
- Changing function signatures (parameters, return types)
- Changing function visibility (pub → pub(crate))

### Struct Changes
- Adding or removing public fields
- Changing field types
- Changing field visibility
- Adding or removing `#[non_exhaustive]` attribute

### Enum Changes
- Adding or removing public variants
- Changing variant data types
- Adding or removing `#[non_exhaustive]` attribute

### Trait Changes
- Adding or removing trait methods
- Changing method signatures
- Adding or removing supertraits
- Changing associated types

### Type Changes
- Adding or removing type aliases
- Changing type alias definitions
- Changing generic parameter bounds

## How to Check for API Changes

### Using Just Targets

```bash
# Check API compatibility against baseline
just api-check

# Show current API baseline information
just api-info

# Check if API freeze is active
just api-freeze-status
```

### Using the Script Directly

```bash
# Generate API baseline
bash scripts/api-baseline.sh generate

# Check API compatibility
bash scripts/api-baseline.sh check

# Show baseline info
bash scripts/api-baseline.sh info

# Check freeze status
bash scripts/api-baseline.sh freeze-status
```

### Using cargo-semver-checks Directly

```bash
# Install cargo-semver-checks
cargo install --locked cargo-semver-checks --version 0.37.0

# Check API compatibility
cargo semver-checks check-release \
  --workspace \
  --baseline-root=origin/main \
  --current-root=. \
  --error-on-missing
```

## How to Update Baseline

### On Release

1. **Remove the freeze file**:
   ```bash
   rm .api-freeze
   git commit -m "Release: Lift API freeze for v1.0.0"
   ```

2. **Update version numbers** in `Cargo.toml`:
   ```toml
   [workspace.package]
   version = "1.0.0"
   ```

3. **Generate new API baseline**:
   ```bash
   just api-baseline
   ```

4. **Commit and tag the release**:
   ```bash
   git add .
   git commit -m "Release v1.0.0"
   git tag v1.0.0
   ```

5. **Publish crates** to crates.io

6. **Re-establish freeze** for next release:
   ```bash
   # Create .api-freeze file with new version info
   echo "# API Freeze Active
   #
   # This file indicates that the public API is frozen for the upcoming release.
   #
   # Current Status
   # - API Version: v1.0.0
   # - Freeze Started: $(date -u +"%Y-%m-%d")
   # - Target Release: v1.1.0
   " > .api-freeze
   git add .api-freeze
   git commit -m "Release: Establish API freeze for v1.1.0"
   ```

## Release Process

### Pre-Release Checklist

1. [ ] Ensure all tests pass
2. [ ] Run `just api-check` to verify API compatibility
3. [ ] Update CHANGELOG.md with release notes
4. [ ] Update version numbers in Cargo.toml
5. [ ] Generate API baseline: `just api-baseline`
6. [ ] Create release commit and tag

### Release Steps

1. **Lift API freeze**:
   ```bash
   rm .api-freeze
   git commit -m "Release: Lift API freeze"
   ```

2. **Update version**:
   ```bash
   # Edit Cargo.toml to update version
   git add Cargo.toml
   git commit -m "Release: Bump version to X.Y.Z"
   ```

3. **Generate baseline**:
   ```bash
   just api-baseline
   git add .api-baseline/
   git commit -m "Release: Generate API baseline for X.Y.Z"
   ```

4. **Create tag**:
   ```bash
   git tag vX.Y.Z
   git push origin main --tags
   ```

5. **Publish**:
   ```bash
   cargo publish -p copybook-core
   cargo publish -p copybook-codec
   ```

6. **Re-establish freeze**:
   ```bash
   # Create .api-freeze file
   git add .api-freeze
   git commit -m "Release: Establish API freeze for next version"
   ```

## CI Enforcement

The `.github/workflows/api-freeze.yml` workflow enforces the API freeze:

### How It Works

1. **Detects freeze status**: Checks for `.api-freeze` file
2. **Analyzes changes**: Determines which files changed in the PR
3. **Enforces policy**:
   - If freeze is active and only docs/bench/tests changed → **PASS**
   - If freeze is active and API changes detected → **FAIL**
   - If freeze is not active → **PASS** (generate baseline)

### Example CI Output

**Passing (freeze active, docs only)**:
```
🔒 API freeze is ACTIVE
📚 Only docs/bench/tests changes detected
✅ Skipping API check - only docs/bench/tests changes detected during freeze
```

**Failing (freeze active, API changes)**:
```
🔒 API freeze is ACTIVE
📝 Non-docs/bench/tests changes detected
❌ ERROR: API freeze is active but API changes detected!

During API freeze, only the following changes are allowed:
  - Documentation changes (docs/)
  - Benchmark changes (copybook-bench/)
  - Test changes (tests/)
  - CI/CD changes (.github/, scripts/)

Changed files:
  copybook-codec/src/lib_api.rs

To make API changes:
  1. Remove the .api-freeze file
  2. Commit and push your changes
  3. After release, re-create .api-freeze for next freeze
```

## Troubleshooting

### API Check Fails

If `just api-check` fails:

1. **Review the error message** to understand what changed
2. **Check if the change is intentional**:
   - If yes → Remove `.api-freeze` and update baseline
   - If no → Revert the API change
3. **For breaking changes**: Consider if this is the right time to make them

### Freeze Status Incorrect

If `.api-freeze` file exists but freeze shouldn't be active:

1. **Verify the file contents** - check version and dates
2. **Remove the file** if freeze is no longer needed
3. **Commit the change** to update CI

### Baseline Missing

If you get "No baseline found" error:

1. **Generate a new baseline**:
   ```bash
   just api-baseline
   ```
2. **Commit the baseline**:
   ```bash
   git add .api-baseline/
   git commit -m "Generate API baseline"
   ```

## Best Practices

1. **Check API compatibility early**: Run `just api-check` before pushing
2. **Document API changes**: Update CHANGELOG.md when making API changes
3. **Plan releases in advance**: Allow time for freeze period
4. **Communicate changes**: Notify users of upcoming API changes
5. **Use semantic versioning**: Follow SemVer for version bumps
6. **Keep baselines updated**: Generate new baseline after each release

## Related Documentation

- [CHANGELOG.md](../CHANGELOG.md) - Release notes and version history
- [ROADMAP.md](ROADMAP.md) - Project roadmap and release planning
- [CONTRIBUTING.md](../CONTRIBUTING.md) - Contribution guidelines

## Tools

- **cargo-semver-checks**: Tool for detecting semver violations
- **just**: Task runner for common operations
- **GitHub Actions**: CI/CD enforcement

## Contact

For questions about API freeze or to request exceptions:
- Open an issue on GitHub
- Contact the maintainers
- Discuss in project meetings
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
