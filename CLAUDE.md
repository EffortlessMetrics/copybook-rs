# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

copybook-rs is a Rust workspace for parsing COBOL copybooks and converting mainframe data formats. The project provides both library crates and a CLI tool for processing legacy COBOL data structures.

For detailed project status, performance benchmarks, and integration readiness assessment, see [REPORT.md](REPORT.md).

## Workspace Structure

This is a Cargo workspace with 5 crates:
- **copybook-core**: Core parsing and schema types for COBOL copybooks (lexer, parser, AST, layout resolution)
- **copybook-codec**: Encoding/decoding codecs for COBOL data types, character conversion, record framing
- **copybook-cli**: Command-line interface with subcommands (parse, inspect, decode, encode, verify)
- **copybook-gen**: Test fixture and synthetic data generation utilities
- **copybook-bench**: Performance benchmarks and testing harness

## Development Commands

### Building
```bash
# Build all workspace crates
cargo build --workspace

# Release build with optimizations
cargo build --workspace --release
```

### Testing
```bash
# Run all tests across workspace (117 tests passing)
cargo test --workspace

# Run performance benchmarks
cargo bench --package copybook-bench

# Run with performance environment variable
PERF=1 cargo bench

# Linting (clippy pedantic compliance - complete compliance achieved)
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Code formatting
cargo fmt --all

# Validation pipeline (comprehensive quality checks - 117 tests passing)
cargo build --workspace --release && \
cargo test --workspace && \
cargo clippy --workspace -- -D warnings -W clippy::pedantic && \
cargo fmt --all --check
```

### Performance Benchmarks

```bash
# Run all benchmarks in copybook-bench
cargo bench --package copybook-bench

# Run specific benchmark suites
cargo bench --package copybook-bench -- encode_performance
cargo bench --package copybook-bench -- decode_performance

# Run SLO validation benchmarks
cargo bench --package copybook-bench -- slo_validation

# Generate HTML performance report
cargo bench --package copybook-bench -- --output-format html
```

### CLI Usage
```bash
# After building, the CLI binary is available as:
cargo run --bin copybook-cli -- [SUBCOMMAND]

# Or after installation:
copybook [SUBCOMMAND]
```

## Architecture Notes

### Core Processing Flow
1. **copybook-core** parses COBOL copybook text into structured Schema AST
2. **copybook-codec** uses Schema to encode/decode binary data records with full schema context
3. **copybook-cli** orchestrates the pipeline with user-friendly commands

**Recent Improvements (PR #25)**:
- Fixed critical compilation errors in JsonEncoder methods
- Added schema parameter threading throughout encoding pipeline for full REDEFINES support
- Implemented `Schema::find_redefining_fields` for proper COBOL precedence rules

### Key Data Types
- `Schema`: Top-level parsed copybook structure from copybook-core with enhanced field lookup methods (including `find_redefining_fields`)
- `Field`/`FieldKind`: Individual copybook field definitions with COBOL semantics (now with proper JSON field processing)
- `SmallDecimal`: Decimal type with Display trait implementation for improved debugging and formatting
- `ScratchBuffers`: Reusable memory buffers for performance optimization
- `DecodeOptions`/`EncodeOptions`: Configuration for codec operations
- `RecordFormat`: Fixed-length vs RDW (Record Descriptor Word) formats
- `Codepage`: EBCDIC character encoding variants (CP037, CP273, CP500, CP1047, CP1140)
- `RunSummary`: Processing statistics and performance metrics

### Performance Optimization Features
- **Scratch Buffer Optimization**: Reusable memory buffers minimize allocations in hot paths
- **Optimized Numeric Codecs**: Fast paths for zoned/packed decimal and binary integer processing
- **Memory Management**: `ScratchBuffers` with `DigitBuffer` (SmallVec), byte buffers, and string buffers
- **Benchmark Infrastructure**: Comprehensive performance testing in `copybook-bench` crate with proper result handling

### Error Handling
Uses structured error taxonomy with stable error codes:
- Parse errors: `CBKP*` (copybook syntax, unsupported features)
- Schema validation: `CBKS*` (ODO counter validation, record size limits)
- Data errors: `CBKD*` (invalid packed decimal, zoned decimal signs, field type mismatches, truncated records)
- Encoding errors: `CBKE*` (JSON type mismatches, array bounds, REDEFINES ambiguity)

### Enhanced Error Reporting
- **Truncated Record Detection**: Automatic detection with CBKD301_RECORD_TOO_SHORT for precise byte count reporting
- **Fail-Fast Validation**: RecordIterator requires LRECL for fixed-format processing
- **Precise Context**: Enhanced error messages with record indexing and byte offset information
- **Performance Optimized**: 4-23% performance improvements with robust validation

### Performance Optimization Features
- **Scratch Buffer Optimization**: Reusable memory buffers minimize allocations in hot paths
- **Optimized Numeric Codecs**: Fast paths for zoned/packed decimal and binary integer processing
- **Memory Management**: `ScratchBuffers` with `DigitBuffer` (SmallVec), byte buffers, and string buffers
- **Benchmark Infrastructure**: Comprehensive performance testing in `copybook-bench` crate with proper result handling

### Performance Features
- Streaming I/O with bounded memory usage for multi-GB files
- Parallel processing with deterministic output ordering
- Zero-copy operations where possible
- Achieved throughput: 4.6+ GiB/s for DISPLAY data (target: ≥80 MB/s), 557+ MiB/s for COMP-3 data (target: ≥40 MB/s)

### Parser Stability
- Infinite loop prevention through unexpected token skipping and recursion depth limits
- Robust error handling for malformed copybook structures
- Memory-safe field hierarchy traversal with comprehensive ODO/REDEFINES validation
- Enhanced error reporting with comprehensive context (record index, field path, byte offset)
- Strict/lenient mode support for ODO counter validation with warning/clamping behavior
- Idiomatic Rust patterns for improved reliability (div_ceil, is_empty, range contains)
- Clippy pedantic compliance for additional safety checks (complete compliance achieved)
- Binary field alignment following IBM mainframe SYNCHRONIZED standards

## Requirements

- Rust 1.89+ (MSRV)
- Edition 2024
- Uses workspace dependencies for consistent versions across crates

## Performance Benchmarks

The `copybook-bench` crate provides comprehensive performance testing:

### Benchmark Suites
- **encode_performance**: Encoding benchmarks for numeric types with scratch buffer optimizations
- **decode_performance**: Comprehensive decoding benchmarks for different data types
- **SLO Validation**: Service Level Objective validation for throughput targets

### Running Benchmarks
```bash
# Run all benchmarks
cargo bench --package copybook-bench

# View benchmark results with throughput measurements
cargo bench --package copybook-bench -- --verbose

# Compare standard vs optimized implementations
cargo bench --package copybook-bench -- encode_performance
```

### Performance Targets
- **DISPLAY-heavy workloads**: ≥80 MB/s throughput (current: 4.26-4.40 GiB/s achieved)
- **COMP-3-heavy workloads**: ≥40 MB/s throughput (current: 547-574 MiB/s achieved)
- **Memory usage**: <256 MiB steady-state for multi-GB files
- **Deterministic output**: Identical results across thread counts
- **Enhanced Validation**: Truncated record detection with maintained performance gains

### Performance Impact - PR #25
The schema threading improvements in PR #25 showed mixed performance results:
- **COMP-3 Performance**: 30-140% improvement (enhanced schema context provides optimization opportunities)
- **DISPLAY Performance**: 10-15% regression (additional schema parameter overhead)
- **Overall**: All workloads still significantly exceed target thresholds (80 MB/s DISPLAY, 40 MB/s COMP-3)
- **Benefit**: Compilation errors resolved and full REDEFINES support enabled with acceptable performance trade-off
