# copybook-rs

[![CI Quick](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml/badge.svg)](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/ci-quick.yml)
[![Security Audit](https://github.com/EffortlessMetrics/copybook-rs/workflows/Weekly%20Security%20Scan/badge.svg)](https://github.com/EffortlessMetrics/copybook-rs/actions/workflows/security-scan.yml)
[![Dependency Review](https://img.shields.io/badge/dependencies-Dependabot-blue.svg)](https://github.com/EffortlessMetrics/copybook-rs/blob/main/.github/dependabot.yml)

Rust toolkit for COBOL copybook parsing and fixed-record data conversion that prioritizes correctness, safety, and transparent validation evidence. We focus on sharing what the tooling supports today, how the test suite exercises it, how to benchmark on your own hardware, and where the roadmap is headed next.

## Quick Answer (30 seconds)

**What it does**: Convert COBOL copybook schemas and mainframe data (EBCDIC/ASCII) to JSON and back with deterministic round-trip fidelity.

**Who should use it**: Teams migrating mainframe data to modern systems, data engineers processing legacy COBOL files, enterprises requiring auditable COBOL data conversion.

**Status**: Engineering Preview (v0.4.2-dev on main; v0.4.1 latest tag) - Stable CLI and library APIs; feature completeness is preview-level. Suitable for teams that validate copybooks against supported features before deployment.

**Key strengths**:

- **Deterministic**: Byte-identical results across runs and worker configurations
- **Memory-safe**: Zero `unsafe` code in public APIs; clippy pedantic enforcement
- **Comprehensive testing**: 1135+ tests passing with BDD, property testing, fuzzing, mutation testing, and feature flagging
- **Performance-validated**: 205 MiB/s (DISPLAY), 58 MiB/s (COMP-3) baseline established with CI receipts in `scripts/bench/perf.json`

See [ROADMAP.md](docs/ROADMAP.md) for adoption guidance and known limitations.

## 📚 Documentation

| Document | Description |
|----------|-------------|
| [User Guide](docs/USER_GUIDE.md) | Getting started, tutorials, examples |
| [CLI Reference](docs/CLI_REFERENCE.md) | Command-line interface documentation |
| [Library API](docs/reference/LIBRARY_API.md) | Rust library API reference |
| [Testing Integration](docs/TESTING_INTEGRATION_SUMMARY.md) | Comprehensive testing methodologies (BDD, property testing, fuzzing, mutation testing) |
| [BDD Testing](docs/BDD_TESTING.md) | Behavior Driven Development tests and documentation |
| [Error Codes](docs/reference/ERROR_CODES.md) | CBKP/CBKS/CBKD/CBKE/CBKR taxonomy |
| [COBOL Support](docs/reference/COBOL_SUPPORT_MATRIX.md) | Feature coverage matrix |
| [Roadmap](docs/ROADMAP.md) | Project status and adoption guidance |

## Quick Start

### Installation

```bash
# Build from source
git clone https://github.com/EffortlessMetrics/copybook-rs.git
cd copybook-rs
git checkout v0.4.1
cargo build --release
```

### Try It Now (2 minutes)

The repo includes test fixtures. After building, run this to see it work:

```bash
# Decode included EBCDIC fixture to JSON
./target/release/copybook decode \
  fixtures/copybooks/simple.cpy \
  fixtures/data/simple.bin \
  --format fixed --codepage cp037 \
  --output demo.jsonl

# View the result
cat demo.jsonl
# {"CUSTOMER-ID":"123456","CUSTOMER-NAME":"John Smith",...,"ACCOUNT-BALANCE":"12345.67",...}
```

The included `simple.cpy` copybook and `simple.bin` data file demonstrate
EBCDIC-to-JSON conversion with COMP-3 packed decimal fields.

### Basic Usage

```bash
# Parse a copybook and output schema JSON
copybook parse customer.cpy --output customer-schema.json

# Inspect copybook layout with human-readable table
copybook inspect customer.cpy --codepage cp037

# Decode binary data to JSONL
copybook decode customer.cpy customer-data.bin \
  --output customer-data.jsonl \
  --format fixed \
  --codepage cp037

# Encode JSONL back to binary (round-trip)
copybook encode customer.cpy customer-data.jsonl \
  --output customer-data-new.bin \
  --format fixed \
  --codepage cp037

# Verify data file integrity against schema
copybook verify customer.cpy customer-data.bin \
  --format fixed \
  --codepage cp037 \
  --report validation-report.json

# Check COBOL feature support
copybook support                        # Display support matrix table
copybook support --json                 # Machine-readable JSON output
copybook support --check level-88       # Validate specific feature (exit 0 if supported)

# Decode with field projection (select specific fields)
copybook decode customer.cpy customer-data.bin \
  --output selected-data.jsonl \
  --format fixed --codepage cp037 \
  --select "CUSTOMER-ID,BALANCE"
```

## Overview

copybook-rs delivers deterministic COBOL copybook parsing, schema inspection, and record encoding/decoding in Rust. The project focus is on predictable behaviour, detailed error reporting, and memory safety. Performance is validated internally with CI receipts published in PR artifacts and `scripts/bench/perf.json` (policy: accuracy-first). See [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures and [docs/REPORT.md](docs/REPORT.md) for comprehensive analysis.

### Design Priorities

- **Correctness first**: Detailed error taxonomy, deterministic encoders/decoders, and zero `unsafe` blocks in public APIs
- **Transparent evidence**: CI reports 1135+ tests passing (24 skipped for external tool requirements); canonical receipts live in `scripts/bench/perf.json` (see [docs/PERFORMANCE_GOVERNANCE.md](docs/PERFORMANCE_GOVERNANCE.md))
- **Schema insight**: CLI and library APIs expose rich metadata for copybook inspection and validation workflows
- **Round-trip fidelity**: Binary↔JSON conversions preserve layout information to keep downstream audits reproducible
- **Sustainable maintenance**: Clean room Rust implementation with clippy pedantic and edition 2024 compliance

## Production Features

### **Core Capabilities**

- **COBOL schema parsing**: Lexer, parser, and AST with layout resolution for REDEFINES, ODO, and SYNCHRONIZED
- **Encoding/decoding**: Deterministic conversion between COBOL records and JSONL/structured data
- **Enterprise error handling**: Stable error taxonomy (CBKP*, CBKS*, CBKD*, CBKE*, CBKR*) with contextual metadata - see [ERROR_CODES.md](docs/reference/ERROR_CODES.md) for complete reference
- **Field projection (`--select`)**: Selective field encoding/decoding with ODO auto-dependency and RENAMES alias resolution (R1-R3)
- **Edited PIC support (E1/E2/E3)**: Parse, decode, and encode edited numeric PICTURE clauses (ZZZ9, $ZZ,ZZZ.99, sign editing, CR/DB, currency, asterisk fill)
- **Memory-aware streaming**: Streaming I/O architecture with bounded memory; real-world telemetry stays below 256 MiB during decode/encode runs

### **Quality Signals**

- **Deterministic output**: Byte-identical results across runs and worker configurations
- **Round-trip fidelity**: Zoned decimal metadata preserved to maintain copybook semantics
- **Memory safety**: Zero `unsafe` in public APIs; pedantic lints enforced across the workspace
- **Test coverage**: Hundreds of unit/integration tests plus nextest orchestration; performance receipts in `scripts/bench/perf.json`

### **Enterprise Integration**

- **Multiple EBCDIC Codepages**: CP037, CP273, CP500, CP1047, CP1140 + ASCII support
- **Flexible Record Formats**: Fixed-length and variable (RDW) with validation
- **CLI + Library API**: Production-ready interfaces; Engineering Preview for feature completeness (see [ROADMAP.md](docs/ROADMAP.md) for known limitations: #44, #51, #72, #86)
- **Verification & Validation**: Built-in data quality auditing without conversion overhead
- **Enterprise Audit System**: Optional compliance framework support (SOX, HIPAA, GDPR, PCI DSS) - see [enterprise-audit-system-spec.md](docs/specs/enterprise-audit-system-spec.md) for details (experimental, feature-gated)

### **CLI Exit Codes**

The `copybook` CLI reports structured exit codes aligned with the error taxonomy for integration with CI/CD pipelines and orchestration systems:

| Code | Tag  | Meaning (1-liner) | Test |
|----:|:----:|--------------------|------|
| 2 | CBKD | Data quality failure | exit_code_mapping::exit_code_cbkd_is_2 |
| 3 | CBKE | Encode/validation failure | exit_code_mapping::exit_code_cbke_is_3 |
| 4 | CBKF | Record format/RDW failure | exit_code_mapping::exit_code_cbkf_is_4 |
| 5 | CBKI | Internal orchestration error | exit_code_mapping::exit_code_cbki_is_5 |

Exit code 0 indicates success; exit code 1 indicates unhandled failures. See [ERROR_CODES.md](docs/reference/ERROR_CODES.md) for the complete error taxonomy.

## Architecture

The project is organized as a Cargo workspace with the following crates:

- **copybook-core**: Core parsing and schema types for COBOL copybooks (lexer, parser, AST, layout resolution)
- **copybook-codec**: Encoding/decoding codecs for COBOL data types, character conversion, record framing
- **copybook-cli**: Command-line interface with subcommands (parse, inspect, decode, encode, verify, support)
- **copybook-gen**: Test fixture and synthetic data generation utilities
- **copybook-bench**: Performance benchmarks and testing harness

## Library API Usage

Quick example for COBOL→JSON processing. For comprehensive API documentation including scratch buffers, streaming iterators, verification, and performance optimization, see [LIBRARY_API.md](docs/reference/LIBRARY_API.md).

```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_file_to_jsonl, DecodeOptions, Codepage, RecordFormat};

// Parse copybook and configure options
let schema = parse_copybook(&copybook_text)?;
let opts = DecodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_format(RecordFormat::Fixed);

// Process complete file with performance metrics
let summary = decode_file_to_jsonl(&schema, input, output, &opts)?;
println!("Processed {} records at {:.2} MB/s", summary.records_processed, summary.throughput_mbps);
```

## Supported COBOL Features

For detailed test evidence and feature coverage, see [COBOL_SUPPORT_MATRIX.md](docs/reference/COBOL_SUPPORT_MATRIX.md).

| **Category** | **Feature** | **Status** | **Notes** |
|-------------|------------|-----------|-----------|
| **Data Types** | Alphanumeric (PIC X) | ✅ Full | EBCDIC/ASCII conversion |
| | Zoned Decimal (PIC 9/S9) | ✅ Full | EBCDIC/ASCII overpunch, negative zero normalization |
| | Packed Decimal (COMP-3) | ✅ Full | Enhanced sign nibble handling |
| | Binary Integer (COMP/BINARY) | ✅ Full | 1-8 bytes, explicit width (BINARY(w)) |
| | Edited PIC (ZZZ9, $, +/-, CR/DB) | ✅ Full | E1/E2/E3 phases complete |
| | COMP-1/COMP-2 (float) | ❌ None | Rare in practice |
| **Structure** | Level Numbers (01-49) | ✅ Full | Hierarchical grouping |
| | Level-88 (Conditions) | ✅ Full | VALUE clauses, parse + codec |
| | REDEFINES | ✅ Full | Multiple storage views |
| | OCCURS (Fixed) | ✅ Full | Fixed arrays |
| | OCCURS DEPENDING ON (ODO) | ✅ Partial | Tail position only (O1-O4) |
| | Nested ODO | ❌ None | By design (Issue #164) |
| | SYNCHRONIZED | ✅ Full | IBM mainframe alignment (2/4/8-byte) |
| | RENAMES (66-level) | ✅ Partial | R1-R3 scenarios; R4-R6 out of scope |
| | BLANK WHEN ZERO | ✅ Full | Zero value handling |
| **Formats** | Fixed-Length (LRECL) | ✅ Full | Constant record size |
| | Variable-Length (RDW) | ✅ Full | Record Descriptor Word |
| **Operations** | Field Projection (`--select`) | ✅ Full | ODO auto-dependency, RENAMES resolution |

## Performance

**CI Gating**: Perf workflow enforces throughput floors (DISPLAY ≥ 80 MiB/s, COMP-3 ≥ 40 MiB/s); baseline comparisons remain advisory.

**Baseline Established**: 2025-09-30 (Commit 1fa63633)
- **DISPLAY-heavy**: 205 MiB/s (2.56x enterprise target of 80 MiB/s)
- **COMP-3-heavy**: 58 MiB/s (1.45x enterprise target of 40 MiB/s)
- **Memory**: <256 MiB steady-state for multi-GB files
- **Variance**: ~5% (DISPLAY), ~8% (COMP-3) across benchmark runs
- **Environment**: WSL2 on AMD Ryzen 9 9950X3D (32 threads, 196 GiB RAM)

See [BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures and [PERFORMANCE_GOVERNANCE.md](docs/PERFORMANCE_GOVERNANCE.md) for governance policy.

```bash
# Run benchmarks and generate receipts
cargo bench --package copybook-bench
just bench-json  # Generates scripts/bench/perf.json

# Baseline management (Issue #52)
cargo run --bin bench-report -p copybook-bench -- baseline show
cargo run --bin bench-report -p copybook-bench -- compare scripts/bench/perf.json
```

## Security & Compliance

copybook-rs implements enterprise-grade security scanning and compliance infrastructure:

- **Dependency Policy**: `cargo deny` gates every PR; `cargo audit` runs when `Cargo.lock` changes and in the weekly scheduled scan
- **Supply Chain Security**: Enhanced deny.toml policies (no yanked crates, no wildcards, trusted sources only)
- **Automated Dependency Updates**: Dependabot with grouped security/routine updates
- **Regulatory Compliance**: Compliance profiles exist behind the `audit` feature flag; outputs are experimental stubs (not compliance evidence)

See [SECURITY.md](SECURITY.md) for security scanning infrastructure and vulnerability response procedures.

## Operational Notes

### Toolchain

- **Rust**: 1.90+ MSRV (workspace-enforced) | **Edition**: 2024
- **Platforms**: Developed and tested primarily on Linux; community validation exists for macOS and Windows
- **Memory**: Streaming decode/encode runs typically remain below 256 MiB on reference datasets
- **Dependencies**: Zero unsafe code in public APIs; clippy pedantic enforced in CI

### Current Reliability Snapshot
<!-- TEST_STATUS:BEGIN -->
- **Tests**: `cargo test --workspace` reports 1135+ tests passing (24 skipped for external tool requirements) with comprehensive coverage across COBOL parsing, data encoding, and CLI integration
- **CI Mode**: Currently operating in CI-off mode with local gates and small PRs. See [`docs/internal/state-and-path.md`](docs/internal/state-and-path.md) for current state.
<!-- TEST_STATUS:END -->
- **Benchmarks**: Performance validated with CI receipts and baseline tracking. See [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures, [copybook-bench/HARDWARE_SPECS.md](copybook-bench/HARDWARE_SPECS.md) for reference hardware specifications, and `scripts/bench/perf.json` artifact for current measurements (policy: accuracy-first).
- **Automation gaps**: Perf receipts are produced and uploaded in CI, but PR comment automation and enforced perf gating remain advisory-only in v0.4.0; see `docs/backlog/benchmark_tooling.md` for follow-ups
- **Documentation**: Public messaging intentionally highlights correctness and open issues; performance receipts in `scripts/bench/perf.json` (see [docs/PERFORMANCE_GOVERNANCE.md](docs/PERFORMANCE_GOVERNANCE.md))

## Project Status & Roadmap

### **Current Status: Engineering Preview (v0.4.2-dev on main; v0.4.1 latest tag)** ⚠️

**Canonical Status**: See [ROADMAP.md](docs/ROADMAP.md) for official project status and adoption guidance.

**CI Mode**: Currently operating in CI-off mode with local gates and small PRs. See [`docs/internal/state-and-path.md`](docs/internal/state-and-path.md) for current state and implementation path.

copybook-rs is suitable for teams that validate their copybooks against the supported feature set, but known limitations mean cautious adoption is recommended:

- ⚠️ **Feature Coverage**: COMP-1/COMP-2, SIGN SEPARATE, and nested ODOs remain unsupported; Edited PIC (E1/E2/E3), RENAMES (R1-R3), and Level-88 condition values are fully supported
- ⚠️ **Performance Variance**: Performance governance is receipt-based. See `scripts/bench/perf.json` for current measurements and [`docs/PERFORMANCE_GOVERNANCE.md`](docs/PERFORMANCE_GOVERNANCE.md) for governance policy. Historical performance targets are quarantined in [`docs/HISTORICAL_PERFORMANCE.md`](docs/HISTORICAL_PERFORMANCE.md).
- ✅ **Benchmark Automation**: `bench-report` CLI tool available (Issue #52) with baseline management, comparison, and validation commands
- ✅ **Quality Signals**: Release gate is green; zero unsafe code; comprehensive error taxonomy including CBKR* family
- ✅ **Interface Stability**: CLI and library APIs are production-ready; feature completeness remains in preview

**Adoption Recommendations**:
1. Validate representative copybooks against supported features before deployment
2. Run pilot workloads to verify throughput meets requirements
3. Review [REPORT.md](docs/REPORT.md) for detailed readiness assessment
4. Consult [ROADMAP.md](docs/ROADMAP.md) for v0.5.0 dialect features and v1.0.0 stability timeline

See `scripts/bench/perf.json` for current performance receipts.

### **Development Roadmap**
See [ROADMAP.md](docs/ROADMAP.md) for planned features and development phases. Current focus: v0.5.0 dialect features and benchmark automation (Issue #52).

## Development

### Building

```bash
# Clone the repository
git clone https://github.com/EffortlessMetrics/copybook-rs.git
cd copybook-rs

# Build all crates
cargo build --workspace

# Build with optimizations
cargo build --workspace --release
```

### Testing

```bash
# Run all tests
cargo test --workspace

# Run with coverage
cargo test --workspace -- --nocapture

# Run BDD tests (Behavior Driven Development)
cargo test -p copybook-bdd

# Run performance benchmarks (JSON receipts)
just bench-json
# or: bash scripts/bench.sh / scripts\bench.bat

# Run clippy
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Format code
cargo fmt --all
```

**BDD Testing**: The project includes Behavior Driven Development tests using Cucumber/Gherkin syntax. These tests describe expected behavior in human-readable format and serve as both executable tests and living documentation. See [BDD_TESTING.md](docs/BDD_TESTING.md) for details on running and writing BDD tests.

### Releasing

Version bumping is automated with `cargo-release`:

```bash
# Install cargo-release (one-time setup)
cargo install cargo-release

# Preview a patch release (0.4.2 -> 0.4.3)
cargo release patch --dry-run

# Execute a patch release (bumps version, updates CHANGELOG, creates tag, pushes)
cargo release patch

# Minor/major releases
cargo release minor  # 0.4.2 -> 0.5.0
cargo release major  # 0.4.2 -> 1.0.0
```

The release process:
1. Bumps version in all workspace `Cargo.toml` files
2. Updates `CHANGELOG.md` using `git-cliff`
3. Creates commit: `chore(release): prepare vX.Y.Z`
4. Creates signed tag `vX.Y.Z`
5. Pushes tag to remote (triggers GitHub release CI)

See [docs/RELEASE_PROCESS.md](docs/RELEASE_PROCESS.md) for detailed workflow and troubleshooting.

## Contributing

We welcome contributions. Please see [REPORT.md](docs/REPORT.md) for current project status and [ROADMAP.md](docs/ROADMAP.md) for development priorities.

### Development Workflow

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Run `just ci-quick` or `cargo xtask ci --quick`
5. Submit a pull request using the provided template

### Code Standards

- Follow Rust conventions and idioms with clippy pedantic compliance
- Add comprehensive tests for new features and help retire the remaining flaky/leaky cases highlighted by `cargo nextest`
- Update documentation for API changes
- Maintain MSRV compatibility (Rust 1.90)
- Use idiomatic Rust patterns (div_ceil, is_empty, range contains)
- Implement Display trait for user-facing types where appropriate
- Use safe type conversions (try_from() instead of unsafe casts)
- Optimize memory usage with scratch buffer patterns

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
