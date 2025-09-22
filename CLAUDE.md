# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

copybook-rs is a Rust workspace for parsing COBOL copybooks and converting mainframe data formats. Provides library crates and CLI tool for processing legacy COBOL data structures.

**Performance**: Exceeds targets by 14-52x (DISPLAY: 4.1+ GiB/s, COMP-3: 560+ MiB/s)
**Quality**: 127 tests passing, clippy pedantic compliance, comprehensive error handling

See [REPORT.md](REPORT.md) for detailed status and benchmarks.

## Workspace Structure

5 crates:
- **copybook-core**: COBOL parsing (lexer, parser, AST, layout)
- **copybook-codec**: Data encoding/decoding, character conversion
- **copybook-cli**: CLI with subcommands (parse, inspect, decode, encode, verify)
- **copybook-gen**: Test fixture generation
- **copybook-bench**: Performance benchmarks

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

// Parse copybook with custom options
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
```

## Architecture

### Processing Flow
1. **copybook-core**: Parses COBOL copybook text → Schema AST
2. **copybook-codec**: Schema → encode/decode binary data ↔ JSON
3. **copybook-cli**: User-friendly command orchestration

### Key Data Types
- `Schema`: Parsed copybook structure with field lookup methods
- `Field`/`FieldKind`: COBOL field definitions with JSON processing
- `DecodeOptions`/`EncodeOptions`: Configuration (codepage, JSON modes, metadata)
- `ScratchBuffers`: Reusable memory buffers for performance
- `RecordFormat`: Fixed-length vs RDW formats
- `Codepage`: EBCDIC variants (CP037, CP273, CP500, CP1047, CP1140)

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
