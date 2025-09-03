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
# Run all tests across workspace
cargo test --workspace

# Run performance benchmarks (requires PERF=1)
PERF=1 cargo bench

# Linting (pedantic clippy compliance enforced)
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Code formatting
cargo fmt --all

# Validation pipeline (comprehensive quality checks - 93 tests passing)
cargo build --workspace --release && \
cargo test --workspace && \
cargo clippy --workspace -- -D warnings -W clippy::pedantic && \
cargo fmt --all --check
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
2. **copybook-codec** uses Schema to encode/decode binary data records
3. **copybook-cli** orchestrates the pipeline with user-friendly commands

### Key Data Types
- `Schema`: Top-level parsed copybook structure from copybook-core
- `Field`/`FieldKind`: Individual copybook field definitions with COBOL semantics (now with proper JSON field processing)
- `SmallDecimal`: Decimal type with Display trait implementation for improved debugging and formatting
- `DecodeOptions`/`EncodeOptions`: Configuration for codec operations
- `RecordFormat`: Fixed-length vs RDW (Record Descriptor Word) formats
- `Codepage`: EBCDIC character encoding variants (CP037, CP273, CP500, CP1047, CP1140)
- `RunSummary`: Processing statistics and performance metrics

### Error Handling
Uses structured error taxonomy with stable error codes:
- Parse errors: `CBKP*` (copybook syntax, unsupported features)
- Data errors: `CBKD*` (invalid packed decimal, zoned decimal signs)
- Encoding errors: `CBKE*` (JSON type mismatches, array bounds)

### Performance Features
- Streaming I/O with bounded memory usage for multi-GB files
- Parallel processing with deterministic output ordering
- Zero-copy operations where possible
- Achieved throughput: 17.25+ GiB/s for DISPLAY data (target: ≥80 MB/s), 51.6+ MiB/s for COMP-3 data (target: ≥40 MB/s)

### Parser Stability
- Infinite loop prevention through unexpected token skipping
- Robust error handling for malformed copybook structures
- Memory-safe field hierarchy traversal
- Enhanced error reporting with comprehensive context
- Idiomatic Rust patterns for improved reliability (div_ceil, is_empty, range contains)
- Clippy pedantic compliance for additional safety checks

## Requirements

- Rust 1.89+ (MSRV)
- Edition 2024
- Uses workspace dependencies for consistent versions across crates