<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
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

The frozen public API scope is the stable surface in:

- `docs/stability/surface-registry.json`

Current stable packages in that registry are:
- **copybook**
- **copybook-charset**
- **copybook-cli**
- **copybook-cli-determinism**
- **copybook-codec**
- **copybook-codepage**
- **copybook-contracts**
- **copybook-core**
- **copybook-error**
- **copybook-error-reporter**

The public API includes:
- All `pub` functions and methods
- All `pub` structs and their fields
- All `pub` enums and their variants
- All `pub` traits and their methods
- All `pub` type aliases
- All `pub` constants

## Allowed Change Categories During Freeze

The freeze still allows routine non-contract updates in:

- Documentation (`docs/`, selected root docs files)
- Test-only files (`tests/`)
- CI/tooling (`.github/`, `scripts/`, `tools/`)

Those files are still validated by normal CI, but they do not force API/contract checks when they are the only changed files.

## Prohibited Changes During Freeze

Changes that affect the stable surface are validated and blocked when incompatible.

- Stable crates listed in `.api-baseline/stable-packages.txt`
- Stable contract source paths in `docs/contracts/stable-surface-contract.json`
- Contract reference docs (`docs/CLI_REFERENCE.md`, `docs/reference/ERROR_CODES.md`, `docs/reference/LIBRARY_API.md`, `schemas/record-format.json`)
- Stability metadata (`docs/stability/surface-registry.json`, `.api-baseline/*`)

Any changes that affect the public API are prohibited without a reviewed exception:

### Function/Method Changes
- Adding or removing public functions
- Changing function signatures (parameters, return types)
- Changing function visibility from `pub` to `pub(crate)`

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

# Check stable contract invariants (strict freeze contract check)
cargo run -p xtask -- docs freeze contracts

# Show baseline info
bash scripts/api-baseline.sh info

# Check freeze status
bash scripts/api-baseline.sh freeze-status
```

### Using cargo-semver-checks Directly

```bash
# Install the first release that supports rustdoc JSON v60
cargo install --locked cargo-semver-checks --version 0.49.0

# Run hostile preflight tests and report Rust, rustdoc, and cargo-semver-checks compatibility context
python3 scripts/ci/test_api_baseline_preflight.py
bash scripts/api-baseline.sh preflight

# Check API compatibility via the repository script (stable-surface baseline)
bash scripts/api-baseline.sh check
```

The API-freeze workflow uses Rust stable and pins `cargo-semver-checks` to
`0.49.0`, the first release whose `trustfall_rustdoc` loader supports rustdoc
JSON v60. Keep the workflow install, script version floor, and this procedure
aligned. The preflight runs the stable-surface semver probe against the current
revision, so it exercises the active rustdoc JSON parser without writing
baseline metadata. Its output records the active Rust/rustdoc versions, the
expected rustdoc JSON format, and the installed semver-checks version; if a
compatibility failure occurs, preserve that diagnostic instead of regenerating
the baseline. The workflow also runs hostile missing, old, malformed-version,
and parser-failure tests for the preflight’s fail-closed behavior.

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

5. **Publish** via the automated release pipeline: pushing the `vX.Y.Z` tag
   triggers `.github/workflows/publish.yml`, which publishes every crate in the
   generated `xtask publish plan` order and runs the registry smoke gates. See
   [RELEASE_RUNBOOK.md](RELEASE_RUNBOOK.md) (execution) and
   [RELEASE_PROCESS.md](RELEASE_PROCESS.md) (overview). Do not publish crates
   manually with `cargo publish -p`.

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
   - If freeze is active and only non-contract scope changes are detected: **PASS**
   - If freeze is active and contract-relevant or non-allowed files changed: **FAIL**
   - If freeze is not active: **PASS** (generate baseline)

### Example CI Output

**Passing (freeze active, non-contract-only changes)**:
~~~txt
API freeze is ACTIVE
Only non-contract files changed
Skipping API check - no contract-relevant files changed during freeze
~~~

**Failing (freeze active, contract-sensitive change)**:
~~~txt
API freeze is ACTIVE
Contract-relevant or non-allowed files changed

Allowed auto-pass scope only:
- Documentation changes (docs/, selected root docs files)
- Test changes (tests/)
- CI/tooling files (.github/, scripts/, tools/)

Changed files:
  crates/copybook-core/src/lib_api.rs

To make API changes:
  1. Remove the .api-freeze file
  2. Commit and push the change
  3. Re-establish freeze with updated .api-freeze
~~~

### API Check Fails

If `just api-check` fails:

1. **Review the error message** to understand what changed
2. **Check if the change is intentional**:
   - If yes -> Remove `.api-freeze` and update baseline
   - If no -> Revert the API change
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
