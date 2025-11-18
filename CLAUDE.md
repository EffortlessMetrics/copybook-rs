# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

copybook-rs is a Rust workspace for enterprise mainframe data processing. Provides comprehensive COBOL copybook parsing and data conversion with focus on correctness and safety.

**Status**: **Engineering Preview** (v0.3.1 maintenance) - See [ROADMAP.md](docs/ROADMAP.md) for adoption guidance
**Performance**: Baseline established (DISPLAY: 205 MiB/s, COMP-3: 58 MiB/s; 2025-09-30, commit 1fa63633)
**Quality**: 704 tests passing (58 skipped), zero unsafe code, clippy pedantic compliance, comprehensive error taxonomy

**Adoption Guidance**: Suitable for teams that validate copybooks against supported features (see Known Limitations & Roadmap below). Production deployment requires pilot validation on representative workloads.

**MSRV**: Rust 1.90 enforced at workspace level and validated in CI (see .github/workflows/ci.yml).

See [REPORT.md](docs/REPORT.md) for detailed readiness assessment and [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for performance baseline details.

## Workspace Structure

5 crates:
- **copybook-core**: COBOL parsing (lexer, parser, AST, layout) with Level-88 condition value support
- **copybook-codec**: Data encoding/decoding, character conversion with structural validation and determinism validation
- **copybook-cli**: CLI with subcommands (parse, inspect, decode, encode, verify, determinism)
- **copybook-gen**: Test fixture generation with golden fixture framework
- **copybook-bench**: Performance benchmarks with regression detection

## Development Commands

```bash
# Build and test
cargo build --workspace --release
cargo test --workspace
cargo clippy --workspace -- -D warnings -W clippy::pedantic
cargo fmt --all

# Benchmarks (output to canonical location: scripts/bench/perf.json)
cargo bench --package copybook-bench
PERF=1 cargo bench -p copybook-bench -- --output-format json > target/perf.json
mkdir -p scripts/bench && cp target/perf.json scripts/bench/perf.json

# Benchmark reporting (Issue #52)
cargo run --bin bench-report -p copybook-bench -- validate scripts/bench/perf.json       # Validate performance JSON
cargo run --bin bench-report -p copybook-bench -- baseline promote scripts/bench/perf.json  # Promote to baseline
cargo run --bin bench-report -p copybook-bench -- baseline show            # Show current baseline
cargo run --bin bench-report -p copybook-bench -- compare scripts/bench/perf.json        # Check for regressions
cargo run --bin bench-report -p copybook-bench -- summary                  # Show performance status

# Golden fixture testing
cargo test --test golden_fixtures_comprehensive    # All golden fixtures
cargo test --test golden_fixtures_odo             # ODO-specific fixtures
cargo test --test golden_fixtures_level88         # Level-88 fixtures
cargo test --test golden_fixtures_enterprise      # Enterprise scenarios

# Full validation pipeline
cargo build --workspace --release && cargo test --workspace && cargo clippy --workspace -- -D warnings -W clippy::pedantic && cargo fmt --all --check
```

## CLI Usage

```bash
# CLI access
cargo run --bin copybook -- [SUBCOMMAND]

# Key commands
cargo run --bin copybook -- parse copybook.cpy                    # Parse to schema JSON
cargo run --bin copybook -- inspect copybook.cpy                  # Human-readable layout
cargo run --bin copybook -- verify --format fixed --codepage cp037 copybook.cpy data.bin  # Validate data

# Decode COBOL data to JSONL
cargo run --bin copybook -- decode customer.cpy data.bin --output data.jsonl --format fixed --codepage cp037 --threads 4

# High-performance decode with options
cargo run --bin copybook -- decode file.cpy data.bin --output data.jsonl --format fixed --codepage cp037 --json-number lossless --emit-meta --threads 8

# Encode JSONL to binary
cargo run --bin copybook -- encode --format fixed --codepage cp037 copybook.cpy input.jsonl output.bin

# Encode with error handling options
cargo run --bin copybook -- encode --format fixed --codepage cp037 --fail-fast --max-errors 5 copybook.cpy input.jsonl output.bin

# Determinism validation (Issue #112)
cargo run --bin copybook -- determinism decode --format fixed --codepage cp037 schema.cpy data.bin           # Verify decode determinism
cargo run --bin copybook -- determinism encode --format fixed --codepage cp037 schema.cpy input.jsonl        # Verify encode determinism
cargo run --bin copybook -- determinism round-trip --format fixed --codepage cp037 schema.cpy data.bin      # Verify round-trip determinism

# Determinism with JSON output for CI integration
cargo run --bin copybook -- determinism decode --output json --format fixed --codepage cp037 schema.cpy data.bin

# Determinism exit codes: 0=deterministic, 1=non-deterministic, 2=usage error, 3=codec error
```

## Library API

### Core Functions

```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_record, DecodeOptions, Codepage, JsonNumberMode};

// Parse copybook
let schema = parse_copybook(&copybook_text)?;

// Parse copybook with custom options (supports Level-88 condition values)
use copybook_core::{parse_copybook_with_options, ParseOptions};
let parse_options = ParseOptions {
    allow_inline_comments: false, // Disable COBOL-2002 inline comments (*>)
    ..ParseOptions::default()
};
let schema = parse_copybook_with_options(&copybook_text, &parse_options)?;

// Configure decoder options
let options = DecodeOptions::new()
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless)
    .with_emit_meta(true);

// Decode record
let json_value = decode_record(&schema, &record_data, &options)?;

// High-performance with scratch buffers
use copybook_codec::{decode_record_with_scratch, memory::ScratchBuffers};
let mut scratch = ScratchBuffers::new();
let json_value = decode_record_with_scratch(&schema, &record_data, &options, &mut scratch)?;

// Streaming iterator
use copybook_codec::iter_records_from_file;
let iterator = iter_records_from_file("data.bin", &schema, &options)?;
for record_result in iterator {
    let json_value = record_result?;
    // Process record
}

// File processing with metrics
use copybook_codec::decode_file_to_jsonl;
let summary = decode_file_to_jsonl(&schema, input, output, &options)?;

// Golden fixture testing with structural validation
use copybook_gen::golden::{GoldenTest, GoldenTestSuite, TestConfig};

// Create golden test with specific configuration
let config = TestConfig {
    codepage: "cp037".to_string(),
    record_format: "fixed".to_string(),
    json_number_mode: "lossless".to_string(),
    flags: vec!["level88".to_string(), "odo".to_string()],
};
let golden_test = GoldenTest::new_with_config("test_name", &copybook_text, &test_data, config);

// Create test suite for enterprise scenarios
let mut suite = GoldenTestSuite::new("enterprise_tests", "Production mainframe scenarios");
suite.add_test(golden_test);

// Validate fixture outputs with SHA-256 verification
let is_valid = golden_test.validate_string_output("json", &output_json);
```

## Architecture

### Processing Flow
1. **copybook-core**: Parses COBOL copybook text → Schema AST
2. **copybook-codec**: Schema → encode/decode binary data ↔ JSON
3. **copybook-cli**: User-friendly command orchestration

### Key Data Types
- `Schema`: Parsed copybook structure with field lookup methods and Level-88 condition support
- `Field`/`FieldKind`: COBOL field definitions with JSON processing and condition value handling
- `DecodeOptions`/`EncodeOptions`: Configuration (codepage, JSON modes, metadata)
- `ScratchBuffers`: Reusable memory buffers for performance
- `RecordFormat`: Fixed-length vs RDW formats
- `Codepage`: EBCDIC variants (CP037, CP273, CP500, CP1047, CP1140)
- `GoldenTest`/`GoldenTestSuite`: Structural validation fixtures with SHA-256 verification

### Error Handling
Structured error taxonomy with stable codes:
- `CBKP*`: Parse errors (syntax, unsupported features)
- `CBKS*`: Schema validation (ODO counters, record limits)
- `CBKD*`: Data errors (invalid decimals, truncated records)
- `CBKE*`: Encoding errors (type mismatches, bounds)
- `CBKR*`: Record format errors (RDW processing, fixed-length records)

### Performance Features
- Scratch buffer optimization for hot paths
- Streaming I/O with bounded memory (<256 MiB for multi-GB files)
- Parallel processing with deterministic output
- Zero-copy operations where possible
- FILLER fields named by byte offset (`_filler_00000XXX`)

## Requirements

- Rust 1.90+ (MSRV), Edition 2024
- Workspace dependencies for consistent versions

## Performance

**CI Gating**: Perf workflow enforces throughput floors (DISPLAY ≥ 80 MiB/s, COMP-3 ≥ 40 MiB/s); baseline comparisons remain advisory.

**Targets vs Achieved**:
- DISPLAY-heavy: ≥80 MB/s → **205 MiB/s (2.56x exceeded)**
- COMP-3-heavy: ≥40 MB/s → **58 MiB/s (1.45x exceeded)**
- Memory: <256 MiB steady-state for multi-GB files
- Variance: ~5% across benchmark runs (WSL2 environment)
- Overhead budget: 20% reserved for enterprise security/audit features

**Baseline Established**: 2025-09-30 (Commit 1fa63633)
- Measurement Environment: WSL2 on AMD Ryzen 9 9950X3D (32 threads, 196 GiB RAM)
- Sample Count: 5 independent runs with statistical analysis
- See `copybook-bench/HARDWARE_SPECS.md` for complete hardware specifications
- See `copybook-bench/BASELINE_METHODOLOGY.md` for measurement procedures

**Benchmarking**:
```bash
cargo bench --package copybook-bench           # All benchmarks
cargo bench --package copybook-bench -- slo_validation  # SLO validation
PERF=1 cargo bench -p copybook-bench          # Performance mode

# Baseline management (Issue #49)
cargo run --bin bench-report -p copybook-bench -- baseline show
cargo run --bin bench-report -p copybook-bench -- baseline promote perf.json
```

**Performance Notes**:
- Measurements taken in WSL2 environment; native Linux may show 5-15% improvement
- DISPLAY-heavy workloads benefit from parallel processing (up to 177 MiB/s on 8 threads)
- COMP-3 decoding performance limited by packed decimal conversion complexity
- Baseline provides foundation for regression detection and performance tracking

**Benchmark Container** (Issue #113):
```bash
# Pull pre-built container from GHCR
docker pull ghcr.io/effortlessmetrics/copybook-rs/bench:latest
docker run -v $(pwd)/output:/workspace/output ghcr.io/effortlessmetrics/copybook-rs/bench:latest
cat output/perf.json

# Or build locally
docker build -t copybook-rs-bench .
docker run -v $(pwd)/output:/workspace/output copybook-rs-bench
```

Provides reproducible benchmark environment with:
- Rust 1.90 toolchain (MSRV)
- All benchmark dependencies pre-installed
- Automated perf.json receipt generation
- Performance summary with SLO comparison
- See [Operator Runbook](docs/perf/OPERATOR_RUNBOOK.md) for detailed usage

## Golden Fixtures System

The Golden Fixtures framework provides comprehensive structural validation for enterprise COBOL scenarios with SHA-256 verification and performance regression detection.

### Key Features

- **Structural Validation**: ODO (Occurs Depending On), Level-88 condition values, REDEFINES interactions
- **Enterprise Scenarios**: Production mainframe patterns from banking, insurance, retail, manufacturing
- **Performance Integration**: Automated performance regression detection with baselines
- **SHA-256 Verification**: Cryptographic validation of test outputs for consistency
- **Comprehensive Coverage**: Golden fixtures form a substantial subset of the 704 tests (58 skipped) in the workspace, covering ODO, Level-88, REDEFINES, and enterprise layouts

### Usage Patterns

```bash
# Run comprehensive golden fixture validation
cargo test --workspace --test "*golden*"

# Run specific fixture categories
cargo test --test golden_fixtures_odo                    # ODO structural validation
cargo test --test golden_fixtures_level88               # Level-88 condition values
cargo test --test golden_fixtures_redefines            # REDEFINES interactions
cargo test --test golden_fixtures_enterprise           # Enterprise production scenarios
cargo test --test golden_fixtures_comprehensive        # Full comprehensive suite

# Performance integration testing
cargo test --test golden_fixtures_ac6_performance_integration
PERF=1 cargo bench --package copybook-bench -- golden_fixtures

# Generate new golden fixtures
cargo run --package copybook-gen -- generate-golden-fixtures --enterprise --output fixtures/enterprise/
```

### Structural Validation Coverage

- **ODO Tail Validation**: Ensures ODO arrays are properly positioned as tail elements
- **Level-88 After ODO**: Validates non-storage condition values following variable arrays
- **Child Inside ODO**: Validates internal structure of variable arrays
- **Sibling After ODO**: Validates rejection of storage fields after ODO (negative test)
- **REDEFINES + Level-88**: Complex interactions between memory redefinition and conditions
- **Enterprise Patterns**: Real-world mainframe record layouts with authentic constraints

### Error Code Coverage

The golden fixtures comprehensively test structural error conditions:

- `CBKP021_ODO_NOT_TAIL`: ODO array positioning validation
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field validation
- `CBKS301_ODO_CLIPPED`: ODO bounds enforcement
- `CBKS302_ODO_RAISED`: ODO minimum value validation
- Plus comprehensive coverage of parsing, schema, and data validation error codes

### Performance Standards

Golden fixtures maintain strict performance requirements aligned with established baselines:

- **DISPLAY-heavy**: 205 MiB/s baseline (2.56x enterprise target of 80 MiB/s)
- **COMP-3-heavy**: 58 MiB/s baseline (1.45x enterprise target of 40 MiB/s)
- **Memory**: <256 MiB steady-state for multi-GB fixture sets
- **Variance**: ~5% (DISPLAY), ~8% (COMP-3) across benchmark runs
- **Regression Detection**: Automated baseline comparison with <2% tolerance

Baseline established 2025-09-30 (commit 1fa63633) on WSL2/AMD Ryzen 9 9950X3D. See [copybook-bench/BASELINE_METHODOLOGY.md](copybook-bench/BASELINE_METHODOLOGY.md) for measurement procedures and [docs/REPORT.md](docs/REPORT.md) for complete performance analysis.

## Known Limitations & Roadmap

**Current Status**: Engineering Preview (v0.3.1 maintenance) - ROADMAP.md is the canonical status source.

### Unsupported COBOL Features
- COMP-1/COMP-2 floating-point types (rare in practice)
- Edited PIC clauses (Z, /, comma, $, CR, DB)
- SIGN LEADING/TRAILING SEPARATE directives
- Nested OCCURS DEPENDING ON arrays
- 66-level (RENAMES) items – parser + same-scope resolver implemented; nested group semantics and codec projection are still pending (see docs/design/RENAMES_NESTED_GROUPS.md and Issues #110 / #133)

**Note**: Level-88 condition values are fully supported (parse + codec + structural validation). See COBOL_SUPPORT_MATRIX.md for detailed test evidence.

### Performance Considerations
- Baseline established at 205 MiB/s (DISPLAY) and 58 MiB/s (COMP-3) on reference hardware (commit 1fa63633)
- WSL2 environment introduces 10-30% overhead versus native Linux
- Current measurements show variance (66-95 MiB/s DISPLAY, 18-25 MiB/s COMP-3) based on system load
- Advisory-only performance policy active (Issues #74, #75); historic SLOs (4.1 GiB/s DISPLAY, 560 MiB/s COMP-3) replaced with empirical baseline
- CI enforces realistic throughput floors (DISPLAY ≥80 MiB/s, COMP-3 ≥40 MiB/s)
- See [docs/ROADMAP.md](docs/ROADMAP.md) for performance improvement plans

### Outstanding Work
- `bench-report` CLI tool available in copybook-bench for baseline management (Issue #52)
- Benchmark automation includes: baseline promote/show, compare, validate, summary commands
- Performance tracking infrastructure active; regression detection advisory-only (Issues #74, #75)
- See [docs/ROADMAP.md](docs/ROADMAP.md) for v0.5.0 dialect features and v1.0.0 stability plans

### Adoption Recommendations
- Validate representative copybooks against supported feature set before production deployment
- Run pilot workloads to verify throughput meets requirements
- Review [docs/REPORT.md](docs/REPORT.md) for detailed readiness assessment
- Consult [docs/ROADMAP.md](docs/ROADMAP.md) for upcoming features and release timeline
