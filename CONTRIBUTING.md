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

```bash
# All tests
cargo test --workspace

# Specific crate
cargo test -p copybook-core

# Golden fixtures
cargo test --test golden_fixtures_comprehensive

# With strict comments mode
COPYBOOK_TEST_STRICT_COMMENTS=1 cargo test -p copybook-core
```

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

### Review Process

- All changes require review from maintainers
- Automated CI must pass
- Performance benchmarks must validate
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

copybook-rs maintains strict performance targets:

- **DISPLAY-heavy**: ≥ 2.5 GiB/s throughput
- **COMP-3-heavy**: ≥ 100 MiB/s throughput
- **Memory**: < 256 MiB steady-state for multi-GB files
- **Variance**: < 5% across benchmark runs

Changes that regress performance require justification and optimization.

## Getting Help

- **Issues**: Create a GitHub issue for bugs or feature requests
- **Discussions**: Use GitHub Discussions for questions
- **Documentation**: Check `docs/` directory for detailed guides

Thank you for contributing to copybook-rs!
