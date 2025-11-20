# Contributing to copybook-rs

Thank you for your interest in contributing to copybook-rs! This guide will help you get started with contributing to our enterprise-grade COBOL copybook parsing and mainframe data processing library.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Environment](#development-environment)
- [Making Changes](#making-changes)
- [Testing](#testing)
- [Documentation](#documentation)
- [Submitting Changes](#submitting-changes)
- [Release Process](#release-process)

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## Getting Started

### Prerequisites

- Rust 1.90+ (MSRV)
- Git
- A GitHub account

### Repository Structure

copybook-rs is organized as a Rust workspace with the following crates:

- **copybook-core**: COBOL parsing (lexer, parser, AST, layout)
- **copybook-codec**: Data encoding/decoding, character conversion
- **copybook-cli**: CLI with subcommands (parse, inspect, decode, encode, verify)
- **copybook-gen**: Test fixture generation
- **copybook-bench**: Performance benchmarks

## Development Environment

### Initial Setup

```bash
# Clone the repository
git clone https://github.com/EffortlessMetrics/copybook-rs.git
cd copybook-rs

# Build the workspace
cargo build --workspace

# Run tests to verify everything works
cargo test --workspace
```

### Development Commands

We use `just` as our task runner. Install it with:

```bash
cargo install just
```

Common development commands:

```bash
# Show all available commands
just

# Build and test (quick check)
just ci-quick

# Full CI validation (includes docs, deny)
just ci-full

# Format code
just fmt

# Run linting
just lint

# Run performance benchmarks (requires PERF=1)
PERF=1 just bench
```

### Using xtask

We also provide an `xtask` automation system:

```bash
# Run comprehensive CI checks locally
cargo xtask ci

# Quick CI (skips long-running tests)
cargo xtask ci --quick

# Generate test fixtures
cargo xtask fixtures

# Validate copybook parsing
cargo xtask validate path/to/copybook.cpy
```

## Making Changes

### Branch Strategy

1. Fork the repository
2. Create a feature branch from `main`:

   ```bash
   git checkout -b feature/your-feature-name
   ```

3. Make your changes
4. Push to your fork and create a pull request

### Semantic Validation for Infrastructure Changes

For changes to tooling, CI, support matrix, or performance infrastructure, you must validate **correctness** — not just "does it compile?" but "does it behave correctly and fail in the right ways?"

#### Validation Scripts

We provide three levels of validation:

1. **`./scripts/ci/offline-semantic.sh`** (recommended for WSL/unstable environments)
   - Runs format checks (`cargo fmt --all --check`)
   - Validates support matrix drift detection (`cargo run -p xtask -- docs verify-support-matrix`)
   - Runs semantic tests (`cargo test -p xtask`, `cargo test -p copybook-cli --test support_cli`)
   - **Use this as your primary gate** when full rebuilds trigger rustc ICEs

2. **`./scripts/ci/quick.sh`** (opportunistic hygiene)
   - Adds clippy pedantic checks
   - Runs broader workspace tests
   - **Use when environment is stable** for extra coverage

3. **`./scripts/ci/offline-all.sh`** (comprehensive validation)
   - Complete CI pipeline including benchmarks
   - **Use for final pre-merge validation** on stable systems

#### What "Semantically Validated" Means

For infrastructure changes, you must prove:

✅ **It parses correctly** — not just compiles, but processes inputs as expected
✅ **It fails correctly** — adversarial tests show it catches errors and rejects invalid inputs
✅ **It's mathematically sound** — manual verification of key calculations (perf throughput, SLO boundaries, etc.)
✅ **It's drift-resistant** — registry ↔ docs ↔ CLI stay in sync automatically

#### Adversarial Testing Checklist

When adding or modifying infrastructure:

- [ ] **Drift detection**: Temporarily remove/modify a row in docs, verify tool detects it
- [ ] **Malformed input**: Feed invalid JSON/data, verify clear error messages
- [ ] **Boundary cases**: Test exact SLO thresholds (80.0 pass, 79.9 fail)
- [ ] **Manual math check**: For perf/throughput, recompute by hand and verify against tool output
- [ ] **Round-trip equality**: Registry → JSON → parse should be identical

#### Example Workflow

```bash
# 1. Make your infrastructure changes
vim copybook-core/src/support_matrix.rs

# 2. Run semantic validation
./scripts/ci/offline-semantic.sh

# 3. Adversarial test: remove a feature from docs
vim docs/reference/COBOL_SUPPORT_MATRIX.md  # comment out a row
cargo run -p xtask -- docs verify-support-matrix  # should fail with clear error
git checkout docs/reference/COBOL_SUPPORT_MATRIX.md  # restore

# 4. If stable environment, run full gate
./scripts/ci/quick.sh

# 5. Commit with evidence
git commit -m "feat(infra): add support matrix validation"
```

#### Recording Evidence in PRs

For infrastructure PRs, include in the description:

```markdown
## Semantic Validation

Local validation:
- ✅ `./scripts/ci/offline-semantic.sh` passed
- ✅ Adversarial tests: drift detection fires correctly
- ✅ Boundary validation: 80.0 MiB/s passes, 79.9 fails
- ✅ Manual math check: verified throughput calculation by hand
```

This gives reviewers confidence that you've validated **behavior**, not just compilation.

### Commit Messages

We follow conventional commit format:

- `feat:` - New features
- `fix:` - Bug fixes
- `docs:` - Documentation changes
- `test:` - Test improvements
- `perf:` - Performance improvements
- `refactor:` - Code refactoring

Examples:

```bash
feat(core): add Level-88 condition value support
fix(codec): resolve COMP-3 decoding edge case
docs(cli): update decode command examples
```

### Code Style

- Run `cargo fmt` before committing
- Ensure `cargo clippy` passes with pedantic warnings
- Follow existing code patterns and naming conventions
- Add documentation for public APIs
- Include tests for new functionality

## Testing

### Test Categories

- **Unit tests**: Test individual functions and modules
- **Integration tests**: Test crate interactions
- **Golden fixtures**: Comprehensive end-to-end validation
- **Property-based tests**: Using `proptest` for edge cases
- **Performance tests**: Benchmark validation

### Running Tests

We use `just` to provide simple commands for running tests. The test suite is split into two main categories:

1.  **Standard Tests**: These are fast-running unit and integration tests that verify the core functionality. They are run on every pull request.
2.  **Ignored Tests**: These are long-running tests for performance, stress, and other comprehensive validation scenarios. They are marked with `#[ignore]` to avoid slowing down the development cycle.

-   To run the standard test suite, use:
    ```bash
    just test
    ```

-   To run the **entire** test suite, including the long-running ignored tests, use:
    ```bash
    just test-all
    ```

It is recommended to run `just test-all` before submitting a pull request for a significant change, especially if it might impact performance.

### Adding Tests

When adding new functionality:

1. Add unit tests in the same file or `tests/` module
2. Add integration tests in `tests/` directory if needed
3. Update golden fixtures for end-to-end scenarios
4. Add property-based tests for complex parsing logic

Example test structure:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_feature() {
        // Arrange
        let input = "...";

        // Act
        let result = your_function(input);

        // Assert
        assert_eq!(result, expected);
    }
}
```

## Documentation

### Types of Documentation

- **API Documentation**: Rust doc comments (`///`)
- **User Guide**: High-level usage in `docs/USER_GUIDE.md`
- **CLI Reference**: Command documentation in `docs/CLI_REFERENCE.md`
- **Architecture Docs**: Technical details in `docs/explanation/`

### Writing Documentation

- Use clear, concise language
- Include examples for public APIs
- Update relevant documentation when changing functionality
- Follow the Diátaxis framework (tutorial, how-to, reference, explanation)

Example API documentation:

```rust
/// Parses a COBOL copybook into a structured schema.
///
/// This function takes COBOL copybook source text and produces
/// a parsed schema that can be used for data conversion.
///
/// # Examples
///
/// ```rust
/// use copybook_core::parse_copybook;
///
/// let copybook = "01 CUSTOMER-RECORD.\n   05 CUSTOMER-ID PIC 9(6).";
/// let schema = parse_copybook(copybook)?;
/// ```
///
/// # Errors
///
/// Returns `ParseError` if the copybook contains syntax errors.
pub fn parse_copybook(source: &str) -> Result<Schema, ParseError> {
    // implementation
}
```

## Submitting Changes

### Pull Request Process

1. Ensure your branch is up to date with `main`
2. Run the full CI pipeline locally:

   ```bash
   just ci-full
   ```

3. Create a pull request with:

   - Clear title following conventional commit format
   - Description of changes and motivation
   - Link to related issues
   - Checklist of testing completed

### Pull Request Template

Your PR should include:

- [ ] Tests added/updated for new functionality
- [ ] Documentation updated (if applicable)
- [ ] `cargo fmt` and `cargo clippy` pass
- [ ] All tests pass locally
- [ ] Performance benchmarks validate (if applicable)
- [ ] Breaking changes documented with migration guide

### Benchmark Triggers

Performance benchmarks are **opt-in** to reduce CI costs and execution time:

- To run benchmarks on a PR, add the `perf:run` label
- Benchmark results are advisory—PRs do not block on performance gates
- Benchmark receipts (`perf.json`) are uploaded as artifacts:
  - **PR runs**: 7-day retention
  - **main branch**: 90-day retention
- See [Performance Requirements](#performance-requirements) for current baselines

### Review Process

- All changes require review from maintainers
- Automated CI must pass
- Performance benchmarks must validate (when triggered)
- Documentation must be updated for user-facing changes

## Release Process

### Version Strategy

We follow semantic versioning (semver):

- **MAJOR**: Breaking API changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

### Release Checklist

1. Update version in `Cargo.toml`
2. Update `CHANGELOG.md`
3. Run full test suite and benchmarks
4. Create release PR
5. Tag release after merge
6. Publish to crates.io

### Performance Requirements

copybook-rs maintains established performance baselines (as of 2025-10-22):

- **DISPLAY-heavy**: ~900-1000 MiB/s baseline (CI floor: ≥ 80 MiB/s)
- **COMP-3-heavy**: ~9 MiB/s baseline (CI floor: ≥ 40 MiB/s)
- **Memory**: < 256 MiB steady-state for multi-GB files
- **Variance**: ~5% (DISPLAY), ~8% (COMP-3) across benchmark runs

Performance gates are **neutral/advisory** with realistic floors. See `copybook-bench/BASELINE_METHODOLOGY.md` for measurement procedures and `docs/REPORT.md` for detailed analysis.

Changes that regress performance require justification and optimization.

## Getting Help

- **Issues**: Create a GitHub issue for bugs or feature requests
- **Discussions**: Use GitHub Discussions for questions
- **Documentation**: Check `docs/` directory for detailed guides

Thank you for contributing to copybook-rs!
