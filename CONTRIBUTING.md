<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Contributing to copybook-rs

Thank you for your interest in contributing to copybook-rs. This guide will help you get started with development and submitting contributions.

## Getting Started

### Prerequisites

- **Rust 1.92+** (MSRV enforced at workspace level)
- **Git** for version control
- **Cargo** (comes with Rust)

### Setting Up Your Development Environment

```bash
# Fork the repository on GitHub, then clone your fork
git clone https://github.com/YOUR_USERNAME/copybook-rs.git
cd copybook-rs

# Build the workspace
cargo build --workspace

# Run tests to verify setup
cargo test --workspace
```

## Development Workflow

```bash
# Build and test
cargo build --workspace --release
cargo test --workspace                                      # 1550+ tests
cargo nextest run --workspace                              # Preferred

# Code quality
cargo clippy --workspace -- -D warnings -W clippy::pedantic
cargo fmt --all

# Benchmarks and fixtures
cargo bench --package copybook-bench
PERF=1 cargo bench -p copybook-bench
cargo test --test golden_fixtures_comprehensive

# Quick CI validation
just ci-quick
```

See [CLAUDE.md](CLAUDE.md) for complete command reference.

## Code Style

### Quality Standards

- **Zero unsafe code** in public APIs
- **Clippy pedantic compliance** enforced in CI
- **Comprehensive test coverage** for new features
- **MSRV compatibility** (Rust 1.92+)
- **Idiomatic Rust patterns** (div_ceil, is_empty, range contains)
- **Safe type conversions** (try_from() instead of unsafe casts)
- **Display trait** for user-facing types
- **Scratch buffer patterns** for performance optimization

### Error Handling

Use structured error taxonomy with stable codes:

- `CBKP*`: Parse errors (syntax, unsupported features)
- `CBKS*`: Schema validation (ODO counters, record limits, projection)
- `CBKD*`: Data errors (invalid decimals, truncated records, edited PIC decode)
- `CBKE*`: Encoding errors (type mismatches, bounds, edited PIC encode)
- `CBKR*`: Record format errors (RDW processing, fixed-length records)

See [ERROR_CODES.md](docs/reference/ERROR_CODES.md) for complete reference.

## Testing

### Test Categories

- **Unit tests**: Function-level validation in same file
- **Integration tests**: Cross-crate workflow validation in `tests/` directory
- **Golden fixtures**: Structural validation with SHA-256 verification (ODO, Level-88, REDEFINES)
- **Benchmarks**: Performance regression detection

### Adding Tests

- Add comprehensive tests for new functionality
- Name tests by feature: `cobol_*`, `enterprise_*`, `parsing_*`, `encoding_*`
- Update golden fixtures for end-to-end scenarios

## Pull Requests

### PR Process

1. **Fork** the repository and create a feature branch from `main`
2. **Make changes** with comprehensive tests
3. **Run validation**: `cargo clippy`, `cargo test`, `cargo fmt`
4. **Update docs** if you changed APIs or CLI commands
5. **Fill out PR template** completely (see [.github/PULL_REQUEST_TEMPLATE.md](.github/PULL_REQUEST_TEMPLATE.md))
6. **Submit PR** with descriptive title

### Commit Messages

Use [conventional commit format](https://www.conventionalcommits.org/): `<type>(<crate>): <description>`

Examples: `feat(core): add ODO support`, `fix(codec): correct COMP-3 decoding`, `docs(readme): update baseline`

**Types**: `feat`, `fix`, `docs`, `test`, `refactor`, `perf`, `chore`, `ci`

### What Reviewers Look For

- Correctness and test coverage
- Adherence to code style and clippy pedantic
- Documentation updates for API changes
- Performance impact (benchmark results if applicable)
- Breaking changes documented with migration guide
- Error codes properly categorized (CBKP*/CBKS*/CBKD*/CBKE*/CBKR*)

## Issue Templates

### Bug Reports

Use [bug_report.yml](.github/ISSUE_TEMPLATE/bug_report.yml) template. Include:

- copybook-rs version, Rust version, OS
- Steps to reproduce with minimal example
- Expected vs. actual behavior
- Copybook snippet and test data (sanitized)
- Error code (e.g., CBKP021, CBKS301, CBKD421)

### Feature Requests

Use [feature_request.yml](.github/ISSUE_TEMPLATE/feature_request.yml) template. Include:

- Clear motivation and use case
- COBOL/mainframe context
- Expected behavior with examples
- Affected crate(s) and potential breaking changes
- Performance or compliance requirements

## Architecture Overview

copybook-rs is a Cargo workspace with 4 published crates and 2 dev-only crates:

- **copybook-core**: COBOL copybook parsing (lexer, parser, AST, layout resolution)
- **copybook-codec**: Data encoding/decoding, character conversion, record framing
- **copybook-arrow**: Apache Arrow and Parquet format conversion (experimental)
- **copybook-cli**: Command-line interface (parse, inspect, decode, encode, verify, determinism)
- **copybook-gen**: Test fixture and synthetic data generation (dev-only)
- **copybook-bench**: Performance benchmarks and regression detection (dev-only)

**Processing Flow**: copybook-core (parse) → copybook-codec (encode/decode) → copybook-cli (commands)

See [CLAUDE.md](CLAUDE.md) for detailed architecture.

## Release Process

See [RELEASE_RUNBOOK.md](docs/RELEASE_RUNBOOK.md) for detailed procedures.

**Release Train**: Minor releases every 6-8 weeks; patch releases as-needed

**Pre-release**: Build/test pass, benchmarks generated, CHANGELOG updated, GitHub release tagged

## Performance Standards

**CI Floors**: DISPLAY ≥80 MiB/s, COMP-3 ≥40 MiB/s
**Baseline** (2025-09-30): DISPLAY 205 MiB/s, COMP-3 58 MiB/s, Memory <256 MiB

See [BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for details.

## Project Status & Resources

**Status**: Engineering Preview (v0.4.3-dev on main; v0.4.2 latest tag)

**Key Documentation**:
- [ROADMAP.md](docs/ROADMAP.md) - Project status and adoption guidance
- [REPORT.md](docs/REPORT.md) - Detailed readiness assessment
- [USER_GUIDE.md](docs/USER_GUIDE.md) - Getting started guide
- [CLI_REFERENCE.md](docs/CLI_REFERENCE.md) - Command-line reference
- [COBOL_SUPPORT_MATRIX.md](docs/reference/COBOL_SUPPORT_MATRIX.md) - Feature coverage

## Contributor License Agreement

Contributors must sign a CLA before contributions can be accepted. See [CLA.md](CLA.md) for details.

## License

This project is licensed under the **GNU Affero General Public License v3.0 or later** (AGPL-3.0-or-later). See [LICENSE](LICENSE) for full license text.

For commercial licensing or alternative arrangements, contact the maintainers.

## Getting Help

- **Documentation**: Comprehensive guides in [docs/](docs/) directory
- **Issues**: Search existing issues or file new ones using templates
- **Discussions**: Use GitHub Discussions for questions

Thank you for contributing to copybook-rs!
