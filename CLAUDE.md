# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

copybook-rs is a **production-ready** Rust workspace for enterprise mainframe data processing. Provides comprehensive COBOL copybook parsing and high-performance data conversion with battle-tested reliability.

**Status**: **PRODUCTION READY** - Ready for immediate enterprise deployment
**Performance**: Exceeds enterprise targets by 15-52x (DISPLAY: 4.1+ GiB/s, COMP-3: 560+ MiB/s)
**Quality**: 458+ tests passing, zero unsafe code, clippy pedantic compliance, comprehensive error taxonomy

**Enterprise Assessment**: System ready for production mainframe workloads with substantial performance safety margins.

See [REPORT.md](REPORT.md) for complete production readiness analysis.

## Workspace Structure

5 crates:
- **copybook-core**: COBOL parsing (lexer, parser, AST, layout) with Level-88 condition value support
- **copybook-codec**: Data encoding/decoding, character conversion with structural validation
- **copybook-cli**: CLI with subcommands (parse, inspect, decode, encode, verify)
- **copybook-gen**: Test fixture generation with golden fixture framework
- **copybook-bench**: Performance benchmarks with regression detection

## Development Commands

```bash
# Build and test
cargo build --workspace --release
cargo test --workspace
cargo clippy --workspace -- -D warnings -W clippy::pedantic
cargo fmt --all

# Benchmarks
cargo bench --package copybook-bench
PERF=1 cargo bench  # Performance mode

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

**Targets vs Achieved**:
- DISPLAY-heavy: ≥80 MB/s → **4.1-4.2 GiB/s (52x exceeded)**
- COMP-3-heavy: ≥40 MB/s → **560-580 MiB/s (15x exceeded)**
- Memory: <256 MiB steady-state for multi-GB files
- Variance: <5% across benchmark runs

**Benchmarking**:
```bash
cargo bench --package copybook-bench           # All benchmarks
cargo bench --package copybook-bench -- slo_validation  # SLO validation
```

## Golden Fixtures System

The Golden Fixtures framework provides comprehensive structural validation for enterprise COBOL scenarios with SHA-256 verification and performance regression detection.

### Key Features

- **Structural Validation**: ODO (Occurs Depending On), Level-88 condition values, REDEFINES interactions
- **Enterprise Scenarios**: Production mainframe patterns from banking, insurance, retail, manufacturing
- **Performance Integration**: Automated performance regression detection with baselines
- **SHA-256 Verification**: Cryptographic validation of test outputs for consistency
- **Comprehensive Coverage**: 458+ tests including edge cases and enterprise production scenarios

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

Golden fixtures maintain strict performance requirements:

- **DISPLAY-heavy**: 4.1+ GiB/s throughput (52x enterprise target)
- **COMP-3-heavy**: 560+ MiB/s throughput (15x enterprise target)
- **Memory**: <256 MiB steady-state for multi-GB fixture sets
- **Variance**: <5% performance variance across runs
- **Regression Detection**: Automated baseline comparison with <2% tolerance

See `docs/golden-fixtures-spec.md` for complete architectural specification.
