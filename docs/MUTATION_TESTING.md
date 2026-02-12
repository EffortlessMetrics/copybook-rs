# Mutation Testing Guide

This guide explains the mutation testing setup for the copybook-rs project, including how to run mutation tests locally, interpret results, and maintain high test quality.

## Table of Contents

- [Overview](#overview)
- [Framework](#framework)
- [Configuration](#configuration)
- [Running Mutation Tests](#running-mutation-tests)
- [Interpreting Results](#interpreting-results)
- [Baseline Scores](#baseline-scores)
- [CI Integration](#ci-integration)
- [Dashboard](#dashboard)
- [Exclusions](#exclusions)
- [Adding New Targets](#adding-new-targets)
- [Troubleshooting](#troubleshooting)

## Overview

Mutation testing is a technique for evaluating the quality of test suites by introducing small changes (mutations) to the source code and checking if tests detect them. A test suite that catches more mutations is considered more effective.

### Why Mutation Testing?

- **Test Quality Assessment**: Identifies gaps in test coverage that traditional coverage tools miss
- **Regression Prevention**: Ensures tests actually verify behavior, not just execute code
- **Refactoring Confidence**: High mutation scores indicate tests will catch bugs during refactoring
- **Documentation**: Serves as living documentation of critical code paths

### Key Concepts

- **Mutation Score**: Percentage of mutants caught by tests (higher is better)
- **Caught Mutant**: A mutation that causes a test to fail (good - tests are working)
- **Missed Mutant**: A mutation that doesn't cause any test to fail (bad - test gap)
- **Unviable Mutant**: A mutation that doesn't compile or would always crash (ignored)
- **Timeout**: A mutation that causes tests to run too long (potential performance issue)

## Framework

We use [`cargo-mutants`](https://github.com/sourcefrog/cargo-mutants), a Rust mutation testing tool that integrates seamlessly with Cargo and supports both `cargo test` and `cargo nextest`.

### Installation

```bash
cargo install cargo-mutants
```

### Version

The project uses the latest stable version of `cargo-mutants`. The CI workflow automatically installs the tool using the `taiki-e/install-action`.

## Configuration

Mutation testing is configured via [`mutants.toml`](../mutants.toml) in the project root.

### Key Configuration Options

```toml
[mutants]
# Minimum mutation score threshold (percentage)
threshold = 70

# Timeout for each mutation test in seconds
timeout = 300

# Maximum number of concurrent mutation tests
jobs = 4

# Test tool to use (cargo, nextest)
test_tool = "nextest"

# Additional cargo arguments
additional_cargo_args = ["--in-place"]
```

### Per-Crate Thresholds

Different crates have different thresholds based on their importance and code complexity:

| Crate | Threshold | Rationale |
|-------|-----------|-----------|
| copybook-core | 75% | Critical parsing logic, high impact |
| copybook-codec | 75% | Critical encoding/decoding, high impact |
| copybook-cli | 65% | More boilerplate, lower criticality |
| copybook-bench | 60% | Benchmark infrastructure, lower priority |
| copybook-gen | 60% | Test generation utilities, lower priority |

### Mutation Operators

The following mutation operators are enabled:

- **Arithmetic**: `+` → `-`, `*` → `/`, etc.
- **Boolean**: `true` → `false`, `&&` → `||`
- **Bitwise**: `&` → `|`, `^` → `&`
- **Comparison**: `<` → `<=`, `==` → `!=`
- **Function Call**: `saturating_add` → `wrapping_add`
- **Iterator**: `iter()` → `iter_mut()`
- **Logical**: `&&` → `||`
- **Pattern Matching**: `Some(x)` → `None`
- **String**: `to_string()` → `to_owned()`
- **Integer Overflow**: `wrapping_add` → `saturating_add`

Certain operators are disabled for specific code paths where they would create false positives (e.g., panic mutations in error handling code).

## Running Mutation Tests

### Using Just (Recommended)

The project provides convenient `just` commands for mutation testing:

```bash
# Run mutation testing on all workspace crates
just mutants

# Run mutation testing on a specific crate
just mutants-crate copybook-core

# Run mutation testing with custom threshold
just mutants-threshold 80

# Run mutation testing with verbose output
just mutants-verbose

# Run mutation testing on core crates only (core, codec)
just mutants-core

# Generate mutation testing report
just mutants-report
```

### Using cargo-mutants Directly

```bash
# Run on all workspace crates
cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place

# Run on specific package
cargo mutants --package copybook-core --file mutants.toml --test-tool nextest --in-place

# Run with custom threshold
cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place --threshold 80

# Run with verbose output
cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place -vV

# List mutants without running tests
cargo mutants --workspace --list
```

### Common Options

| Option | Description | Default |
|--------|-------------|---------|
| `--timeout SECONDS` | Timeout per mutation test | 300 |
| `--jobs N` | Number of concurrent tests | 4 |
| `--threshold N` | Minimum score percentage | 70 |
| `--test-tool TOOL` | Test tool to use | nextest |
| `--in-place` | Test in-place (faster) | true |
| `--file PATH` | Configuration file | mutants.toml |
| `-vV` | Verbose output | false |
| `--list` | List mutants without testing | false |

## Interpreting Results

### Understanding the Output

When mutation testing completes, you'll see output similar to:

```
Caught: 142
Missed: 38
Unviable: 12
Timeout: 0
Score: 78.9%
```

### Score Interpretation

| Score Range | Interpretation | Action |
|-------------|----------------|--------|
| 90%+ | Excellent | Tests are very comprehensive |
| 80-89% | Good | Tests cover most critical paths |
| 70-79% | Acceptable | Meets minimum threshold |
| <70% | Needs Improvement | Add more tests |

### Analyzing Missed Mutants

When you have missed mutants, review them to understand test gaps:

```bash
# View missed mutants in detail
cargo mutants --workspace --file mutants.toml --test-tool nextest --in-place -vV

# The output will show details like:
# MissedMutant: replace `==` with `!=` in copybook-core/src/parser.rs:123
#   Function: parse_field
```

### Common Causes of Missed Mutants

1. **Missing Test Cases**: The mutated code path isn't tested
2. **Incomplete Assertions**: Tests don't verify all aspects of behavior
3. **Test Dependencies**: Tests rely on external state that masks mutations
4. **Dead Code**: The mutated code is never executed by any test

### Improving Mutation Scores

1. **Add Test Cases**: Cover the mutated code path
2. **Strengthen Assertions**: Verify more aspects of behavior
3. **Remove Test Dependencies**: Make tests more isolated
4. **Remove Dead Code**: Delete unused code or add tests for it

## Baseline Scores

### Current Baseline Scores

The following baseline scores are used as targets for mutation testing:

| Crate | Baseline | Target | Current |
|-------|----------|--------|---------|
| copybook-core | 75% | 80% | TBD |
| copybook-codec | 75% | 80% | TBD |
| copybook-cli | 65% | 70% | TBD |
| copybook-bench | 60% | 65% | TBD |
| copybook-gen | 60% | 65% | TBD |

**Note**: Baseline scores will be established after the first full mutation testing run.

### Establishing Baselines

To establish baseline scores:

1. Run mutation testing on all crates: `just mutants`
2. Review results and note the scores
3. Update this document with the actual scores
4. Set targets slightly above current scores for continuous improvement

## CI Integration

### Scheduled Runs

Mutation tests run **weekly** on Sundays at 2 AM UTC via GitHub Actions.

### Manual Triggers

You can manually trigger mutation testing via the GitHub Actions UI:

1. Go to Actions > Mutation Testing
2. Click "Run workflow"
3. Select branch and configure options:
   - **Threshold**: Minimum score percentage (default: 70)
   - **Workspace**: Run on all crates or specific ones

### Pull Request Integration

Mutation tests can be triggered on pull requests by adding the `mutation-test` label:

1. Open the pull request
2. Click the gear icon next to "Labels"
3. Add the `mutation-test` label
4. The mutation testing workflow will run automatically

### Feature Flag Control

Mutation testing can be controlled via the `COPYBOOK_MUTATION_TESTING` environment variable:

- **Scheduled runs**: Always enabled
- **Manual triggers**: Always enabled
- **Pull requests**: Disabled by default, enabled with `mutation-test` label

### CI Workflow Details

The CI workflow (`.github/workflows/ci-mutants.yml`) includes:

- Feature flag checking
- Per-crate testing with matrix strategy
- Result aggregation and summary
- PR comment integration
- Historical data upload for dashboard
- Artifact retention (30 days)

## Dashboard

A Grafana dashboard is available for tracking mutation testing trends:

- **Dashboard**: [Mutation Testing Dashboard](../grafana/dashboards/mutation-testing.json)
- **Metrics**: Mutation scores, caught/missed mutants, trends over time
- **Alerts**: Configurable alerts for score thresholds and trends

### Dashboard Features

- **Overall Score**: Average mutation score across all crates
- **Per-Crate Scores**: Individual scores for each crate
- **Trend Analysis**: Historical trends over 30 and 90 days
- **Caught vs Missed**: Comparison of caught and missed mutants
- **Alerts**: Configurable alerts for score thresholds

### Integrating with Prometheus

To integrate mutation testing metrics with Prometheus:

1. The CI workflow uploads historical data as artifacts
2. A separate job (or external tool) can parse these artifacts
3. Metrics are exposed in Prometheus format:
   - `mutation_score{crate="..."}`: Mutation score percentage
   - `mutation_caught_total{crate="..."}`: Total caught mutants
   - `mutation_missed_total{crate="..."}`: Total missed mutants
   - `mutation_total{crate="..."}`: Total mutants tested

## Exclusions

Certain code is excluded from mutation testing to avoid false positives and focus on critical code paths.

### Excluded Code Patterns

```toml
exclude = [
    # Generated code
    "copybook-gen/src/*_gen.rs",
    "copybook-gen/examples/*_gen.rs",

    # Test code (don't mutate tests)
    "*/tests/*",
    "*/benches/*",

    # Build scripts
    "*/build.rs",

    # Boilerplate and error handling
    "*/error.rs",
    "*/error_reporter.rs",

    # Memory management (low-level, well-tested)
    "copybook-codec/src/memory/*",

    # CLI boilerplate
    "copybook-cli/src/exit_codes.rs",
    "copybook-cli/src/utils.rs",

    # Benchmark utilities
    "copybook-bench/src/baseline.rs",
    "copybook-bench/src/reporting.rs",
]
```

### Rationale for Exclusions

| Category | Reason |
|----------|--------|
| Generated Code | Not human-written, mutations are meaningless |
| Test Code | Don't mutate tests, only production code |
| Build Scripts | Build infrastructure, not product logic |
| Error Handling | Many error paths are expected and handled |
| Memory Management | Low-level code with different testing approach |
| CLI Boilerplate | Argument parsing is well-tested elsewhere |
| Benchmark Utilities | Performance code, not product logic |

### Adding New Exclusions

To add a new exclusion:

1. Identify the code that should be excluded
2. Add the pattern to the `exclude` list in [`mutants.toml`](../mutants.toml)
3. Document the rationale in this section
4. Run mutation testing to verify the exclusion works

## Adding New Targets

### Adding a New Crate

To add a new crate to mutation testing:

1. Add the crate to the workspace in [`Cargo.toml`](../Cargo.toml)
2. Add the crate to the CI workflow matrix in [`.github/workflows/ci-mutants.yml`](../.github/workflows/ci-mutants.yml)
3. Add per-crate configuration to [`mutants.toml`](../mutants.toml)
4. Update the justfile with a new command if needed
5. Update this documentation with the new crate's threshold

### Adding a New Module

To add a new module to an existing crate:

1. Ensure the module has adequate test coverage
2. Run mutation testing on the crate: `just mutants-crate <crate>`
3. Review results and add tests as needed
4. Update the baseline score if significantly changed

### Adding New Mutation Operators

To enable additional mutation operators:

1. Review the operator in [`mutants.toml`](../mutants.toml)
2. Enable the operator if disabled
3. Run mutation testing to evaluate impact
4. Adjust thresholds if needed

## Troubleshooting

### Common Issues

#### Issue: Timeout Errors

**Symptom**: Many mutants timeout during testing

**Solutions**:
- Increase the timeout in [`mutants.toml`](../mutants.toml)
- Reduce the number of concurrent jobs
- Investigate slow tests with `cargo nextest run --profile slow`

#### Issue: Low Mutation Scores

**Symptom**: Mutation scores are consistently below threshold

**Solutions**:
- Review missed mutants and add tests
- Check if exclusions are too broad
- Verify test coverage is adequate
- Consider if threshold is too high for the code

#### Issue: Too Many Unviable Mutants

**Symptom**: High percentage of unviable mutants

**Solutions**:
- This is normal for some code patterns
- Review if code structure can be improved
- Add exclusions for consistently unviable patterns

#### Issue: CI Fails Locally

**Symptom**: Mutation tests pass locally but fail in CI

**Solutions**:
- Check Rust version matches (1.92.0)
- Verify configuration files are committed
- Check for environment-specific behavior
- Review CI logs for specific failures

### Debugging Tips

1. **Verbose Output**: Use `-vV` flag for detailed output
2. **Single Crate**: Test one crate at a time to isolate issues
3. **Specific Mutant**: Test a specific mutant to understand behavior
4. **Test Logs**: Review test logs for clues about missed mutants

### Getting Help

- **Documentation**: [`cargo-mutants` documentation](https://github.com/sourcefrog/cargo-mutants)
- **Issues**: Report issues in the copybook-rs GitHub repository
- **Discussions**: Use GitHub Discussions for questions

## Best Practices

1. **Run Regularly**: Run mutation testing regularly to catch test gaps early
2. **Review Results**: Always review missed mutants and improve tests
3. **Set Realistic Thresholds**: Use thresholds that reflect code criticality
4. **Document Exclusions**: Document why code is excluded from mutation testing
5. **Track Trends**: Monitor mutation scores over time for quality trends
6. **Integrate with CI**: Use CI to ensure mutation scores don't regress
7. **Educate Team**: Ensure team understands mutation testing and its value

## References

- [cargo-mutants GitHub](https://github.com/sourcefrog/cargo-mutants)
- [Mutation Testing Wikipedia](https://en.wikipedia.org/wiki/Mutation_testing)
- [Grafana Dashboard](../grafana/dashboards/mutation-testing.json)
- [CI Workflow](../.github/workflows/ci-mutants.yml)
- [Configuration](../mutants.toml)
