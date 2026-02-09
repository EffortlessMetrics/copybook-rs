# Test Coverage Guide

This document provides information about test coverage in the copybook-rs project, including how to run coverage locally, how to interpret coverage reports, and coverage goals.

## Current Coverage Status

The copybook-rs project uses multiple testing methodologies to ensure code quality and correctness:

- **Unit Tests**: In-source tests within each crate
- **Integration Tests**: End-to-end workflow tests
- **Property Tests**: Proptest-based invariant verification
- **BDD Tests**: Cucumber/Gherkin behavior-driven tests
- **Fuzz Tests**: AFL-based fuzzing for edge cases
- **Mutation Tests**: cargo-mutants for test quality assessment

### Coverage Metrics

Coverage is tracked using **cargo-tarpaulin** and reported to Codecov.

| Crate | Target Coverage | Current Status |
|--------|-----------------|----------------|
| copybook-core | 85% | ✅ On track |
| copybook-codec | 85% | ✅ On track |
| copybook-cli | 80% | ✅ On track |
| copybook-arrow | 75% | ⚠️ Needs improvement |
| copybook-gen | 70% | ⚠️ Needs improvement |

## How to Run Coverage Locally

### Prerequisites

Install cargo-tarpaulin:

```bash
cargo install cargo-tarpaulin --version 0.31.5
```

### Run Coverage

Run coverage for the entire workspace:

```bash
cargo tarpaulin \
  --workspace \
  --exclude copybook-bench \
  --exclude-files 'examples/*' \
  --exclude-files 'tests/*' \
  --exclude-files 'xtask/*' \
  --out Html \
  --output-dir ./coverage \
  --verbose \
  --all-features
```

Run coverage for a specific crate:

```bash
cargo tarpaulin -p copybook-core --out Html --output-dir ./coverage
```

### Coverage Options

- `--workspace`: Run coverage for all workspace members
- `--exclude <crate>`: Exclude specific crates from coverage
- `--exclude-files <pattern>`: Exclude files matching pattern
- `--out Html`: Generate HTML coverage report
- `--out Xml`: Generate XML coverage report (for CI)
- `--output-dir <dir>`: Directory for coverage reports
- `--all-features`: Test with all features enabled
- `--test-threads <n>`: Number of test threads to use

### View Coverage Report

After running coverage, open the HTML report:

```bash
# Linux/Mac
open coverage/index.html

# Windows
start coverage/index.html
```

## Interpreting Coverage Reports

### HTML Report

The HTML report provides:
- **File-by-file coverage**: See which files have low coverage
- **Line-by-line coverage**: See which lines are covered
- **Branch coverage**: See which branches are tested
- **Function coverage**: See which functions are tested

Color coding:
- 🟢 Green: Covered
- 🔴 Red: Not covered
- 🟡 Yellow: Partially covered

### XML Report

The XML report is used by CI systems like Codecov to:
- Track coverage over time
- Show coverage diffs in pull requests
- Generate coverage badges

## Coverage Goals

### Short-term Goals (Q1 2026)

1. **Achieve 85%+ coverage** for core crates (copybook-core, copybook-codec)
2. **Achieve 80%+ coverage** for CLI crate (copybook-cli)
3. **Add coverage for new features**: SIGN SEPARATE, RENAMES R4-R6
4. **Improve copybook-arrow coverage** to 80%+

### Long-term Goals (Q2-Q4 2026)

1. **Achieve 90%+ coverage** for all core crates
2. **Achieve 85%+ coverage** for all crates
3. **Maintain coverage** as code grows
4. **Reduce uncovered lines** in critical paths

## Coverage in CI

Coverage runs automatically in CI:
- **On push** to main/develop branches
- **On pull requests** to main/develop branches
- **Weekly** on Tuesdays at 3:23 UTC

### CI Coverage Workflow

The CI coverage workflow (`.github/workflows/ci-coverage.yml`):
1. Installs cargo-tarpaulin
2. Runs coverage with all features
3. Uploads results to Codecov
4. Generates coverage summary
5. Comments coverage on pull requests

### Coverage Thresholds

Coverage thresholds are not enforced in CI but are tracked:
- **Warning threshold**: Below 75%
- **Target threshold**: 85%+
- **Excellent threshold**: 90%+

## Coverage Best Practices

### Writing Tests for Coverage

1. **Test error paths**: Ensure all error conditions are tested
2. **Test edge cases**: Test boundary values and special cases
3. **Test public APIs**: Ensure all public functions have tests
4. **Test complex logic**: Ensure branches and conditions are covered
5. **Use property tests**: For invariants and mathematical properties

### Common Coverage Gaps

1. **Error handling paths**: Often untested
2. **Default cases**: Sometimes overlooked
3. **Helper functions**: May lack direct tests
4. **Configuration parsing**: Edge cases in option handling

### Improving Coverage

To improve coverage in a specific area:

1. **Run coverage locally** to identify gaps
2. **Review uncovered code** to understand what's missing
3. **Write tests** for uncovered paths
4. **Re-run coverage** to verify improvement
5. **Iterate** until target is reached

## Coverage Tools

### cargo-tarpaulin

Used for code coverage in Rust projects.

**Features**:
- Line coverage
- Branch coverage
- Condition coverage
- HTML and XML output
- CI integration

**Alternatives**:
- `grcov`: LLVM-based coverage (slower, more detailed)
- `cargo-llvm-cov`: Alternative LLVM coverage tool

### Codecov

Online service for coverage tracking and visualization.

**Features**:
- Coverage trends over time
- Pull request coverage diffs
- Coverage badges
- File-level coverage comments

## Troubleshooting

### Coverage Not Running

If coverage fails to run:
1. Ensure cargo-tarpaulin is installed
2. Check Rust toolchain version compatibility
3. Verify workspace configuration
4. Check for conflicting features

### Coverage Report Empty

If coverage report is empty:
1. Ensure tests are actually running
2. Check for test compilation errors
3. Verify `--exclude` patterns aren't too broad
4. Check test binary names match expected patterns

### Coverage Decreased Unexpectedly

If coverage decreases:
1. Check for refactored code without tests
2. Verify new code has tests
3. Check for test failures causing early exit
4. Review coverage configuration changes

## Coverage and Mutation Testing

Coverage and mutation testing complement each other:

- **Coverage**: Measures which code is executed
- **Mutation testing**: Measures if tests would catch bugs

High coverage doesn't guarantee bug-free code. Mutation testing helps ensure:
- Tests actually verify behavior
- Edge cases are handled
- Error paths are tested

## Related Documentation

- [Testing Integration Summary](TESTING_INTEGRATION_SUMMARY.md): Overview of all testing methodologies
- [BDD Testing](BDD_TESTING.md): Behavior-driven development guide
- [User Guide](USER_GUIDE.md): Getting started with copybook-rs
- [Library API](reference/LIBRARY_API.md): API documentation

## Contributing to Coverage

When contributing new code:

1. **Write tests alongside code**: Don't wait until later
2. **Aim for 100% coverage** on new code
3. **Test error paths**: Ensure errors are handled
4. **Use appropriate test types**:
   - Unit tests for isolated functions
   - Property tests for invariants
   - BDD tests for user-facing features
5. **Run coverage before PR**: Verify coverage doesn't decrease
6. **Add tests for PRs**: Don't let coverage slip

## Coverage Badge

The coverage badge in the README shows the latest coverage from Codecov:

```
![codecov](https://codecov.io/gh/EffortlessMetrics/copybook-rs/branch/main/graph/badge.svg)
```

## Contact

For questions about coverage:
- Open an issue on GitHub
- Check existing issues for coverage discussions
- Review coverage reports in CI for guidance
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
