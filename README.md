# copybook-rs

A modern, memory-safe parser/codec for COBOL copybooks and fixed-record data.

## Overview

copybook-rs is a Rust implementation of a COBOL copybook parser and data codec that provides deterministic, reproducible conversion of mainframe-encoded records into accessible formats like JSON. With robust field processing and comprehensive COBOL data type support, it enables organizations to unlock mainframe data for analytics, system integration, and modernization efforts without requiring COBOL runtime environments.

### Key Benefits

- **Mainframe Data Liberation**: Convert legacy COBOL data formats to modern JSON without COBOL runtime
- **ETL Integration**: Stream processing of multi-GB mainframe files with bounded memory usage
- **Audit Compliance**: Deterministic output with byte-identical results across runs
- **Round-Trip Fidelity**: Lossless conversion preserves original data integrity with proper COBOL field processing
- **Production Ready**: Comprehensive error handling with stable error codes and truncated record detection
- **Memory Safety**: Complete clippy pedantic compliance with safe type conversions and optimized memory management

## Features

- **Deterministic Output**: Byte-identical results across runs and parallel processing
- **Round-Trip Fidelity**: Unchanged JSON data re-encodes to identical binary
- **Memory Safety**: No unsafe code in public API paths with complete clippy pedantic compliance
- **Streaming Architecture**: Bounded memory usage for multi-GB files with scratch buffer optimizations
- **Comprehensive Error Handling**: Stable error codes with structured context and enhanced truncated record detection
- **COBOL Feature Support**: REDEFINES, OCCURS DEPENDING ON, SYNCHRONIZED (IBM mainframe alignment standards), packed/zoned decimals
- **Character Encoding**: Full EBCDIC support (CP037, CP273, CP500, CP1047, CP1140) and ASCII
- **Performance**: **Complete COBOL→JSON Processing**: 4.1-4.2 GiB/s for DISPLAY-heavy data (target: ≥80 MB/s - **50-52x exceeded**), 560-580 MiB/s for COMP-3-heavy data (target: ≥40 MB/s - **14-15x exceeded**)
- **Parser Stability**: Infinite loop prevention with robust error handling, safe type conversions, and fail-fast validation

## Architecture

The project is organized as a Cargo workspace with the following crates:

- **copybook-core**: Core parsing and schema types for COBOL copybooks (lexer, parser, AST, layout resolution)
- **copybook-codec**: Encoding/decoding codecs for COBOL data types, character conversion, record framing
- **copybook-cli**: Command-line interface with subcommands (parse, inspect, decode, encode, verify)
- **copybook-gen**: Test fixture and synthetic data generation utilities
- **copybook-bench**: Performance benchmarks and testing harness

## Quick Start

### Installation

```bash
# Install from crates.io
cargo install copybook-cli

# Or build from source
git clone https://github.com/copybook-rs/copybook-rs.git
cd copybook-rs
cargo build --release
```

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
```

## Detailed Usage Examples

### Working with Fixed-Length Records

```bash
# Decode fixed-length mainframe data
copybook decode schema.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --output data.jsonl \
  --emit-meta \
  --threads 4

# Strict mode - stop on first error
copybook decode schema.cpy data.bin \
  --format fixed \
  --strict \
  --output data.jsonl

# Lenient mode with error limit
copybook decode schema.cpy data.bin \
  --format fixed \
  --max-errors 100 \
  --output data.jsonl
```

### Working with Variable-Length Records (RDW)

```bash
# Decode RDW (Record Descriptor Word) format
copybook decode schema.cpy data.bin \
  --format rdw \
  --codepage cp037 \
  --output data.jsonl

# Preserve raw RDW headers for round-trip
copybook decode schema.cpy data.bin \
  --format rdw \
  --emit-raw record+rdw \
  --output data.jsonl

# Use raw data for encoding (preserves reserved bytes)
copybook encode schema.cpy data.jsonl \
  --format rdw \
  --use-raw \
  --output data-roundtrip.bin
```

### Character Encoding Options

```bash
# Different EBCDIC code pages
copybook decode schema.cpy data.bin --codepage cp037   # US/Canada
copybook decode schema.cpy data.bin --codepage cp273   # Germany/Austria  
copybook decode schema.cpy data.bin --codepage cp500   # International
copybook decode schema.cpy data.bin --codepage cp1047  # Open Systems
copybook decode schema.cpy data.bin --codepage cp1140  # US/Canada Euro

# ASCII data (not Windows-1252)
copybook decode schema.cpy data.bin --codepage ascii

# Handle unmappable characters
copybook decode schema.cpy data.bin \
  --codepage cp037 \
  --on-decode-unmappable replace  # or error, skip
```

### Advanced Options

```bash
# Include FILLER fields in output (named by byte offset: _filler_00000XXX)
# FILLER fields use consistent byte-offset naming across parsing sessions
copybook decode schema.cpy data.bin \
  --emit-filler \
  --output data.jsonl

# Add metadata (schema fingerprint, record index, byte offsets)
copybook decode schema.cpy data.bin \
  --emit-meta \
  --output data.jsonl

# Control JSON number representation
copybook decode schema.cpy data.bin \
  --json-number lossless \  # strings for decimals (default, preserves precision)
  --output data.jsonl

copybook decode schema.cpy data.bin \
  --json-number native \    # JSON numbers where possible (integers, small decimals)
  --output data.jsonl

# BLANK WHEN ZERO encoding policy
copybook encode schema.cpy data.jsonl \
  --bwz-encode on \         # emit spaces for zero values
  --output data.bin
```

### Performance and Parallel Processing

```bash
# Parallel processing (maintains deterministic output)
copybook decode schema.cpy large-data.bin \
  --threads 8 \
  --output data.jsonl

# Performance monitoring
copybook decode schema.cpy data.bin \
  --output data.jsonl \
  --verbose  # shows throughput and memory usage
```

### Data Validation and Verification

The `verify` command validates data files against copybook schemas without generating output files, making it ideal for data quality audits and integrity checks.

```bash
# Basic verification - check data file structure
copybook verify schema.cpy data.bin \
  --format fixed \
  --codepage cp037

# Generate detailed JSON verification report
copybook verify schema.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --report validation-report.json
# Report schema: docs/VERIFY_REPORT.schema.json

# Verify RDW format data
copybook verify schema.cpy data.bin \
  --format rdw \
  --codepage cp1047

# Verify multiple files with different formats
copybook verify schema.cpy fixed-data.bin --format fixed --report fixed-report.json
copybook verify schema.cpy rdw-data.bin --format rdw --report rdw-report.json
```

#### Verification Features

- **Record-Level Validation**: Detects truncated records, invalid numeric formats, and field type mismatches
- **Comprehensive Error Reporting**: Provides error codes, record indices, field paths, and byte offsets
- **JSON Report Generation**: Machine-readable reports for integration with data quality pipelines
- **Exit Code Compliance**: Returns 0 for valid files, 3 for validation errors, 2 for fatal errors (IO/schema)
- **Performance Optimized**: Validation-only processing without JSON conversion overhead

## JSON Output Quality

copybook-rs produces clean, properly typed JSON output with comprehensive COBOL field processing:

- **Proper Field Values**: COBOL fields are decoded to their correct string or numeric representations (no unintended null values)
- **Numeric Precision**: Zoned and packed decimals maintain precision with proper sign handling
- **Character Conversion**: EBCDIC and ASCII character data converted to UTF-8 strings
- **Hierarchical Structure**: Group fields create nested JSON objects matching copybook structure
- **REDEFINES Support**: Enhanced JSON encoding with proper REDEFINES field resolution and precedence rules
- **Schema-Aware Processing**: Full schema context enables advanced field lookups and ODO counter management

### Example JSON Output

For a COBOL record with various field types:

```json
{
  "CUSTOMER-ID": "00123",
  "CUSTOMER-NAME": "JOHN DOE",
  "ACCOUNT-BALANCE": "-1234.56",
  "LAST-PAYMENT": "890.12",
  "ORDER-COUNT": "3",
  "CUSTOMER-ADDRESS": {
    "STREET": "123 MAIN ST",
    "CITY": "ANYTOWN",
    "ZIP-CODE": "12345"
  }
}
```

## Library API Usage

### Complete COBOL→JSON Processing

The library provides comprehensive COBOL record decoding with complete field processing:

```rust
use copybook_core::parse_copybook;
use copybook_codec::{
    decode_record, decode_file_to_jsonl, DecodeOptions, Codepage, RecordFormat, 
    JsonNumberMode, RawMode, UnmappablePolicy
};
use std::path::Path;

// Parse copybook
let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

// Configure decode options for complete JSON output
let opts = DecodeOptions {
    codepage: Codepage::Cp037,
    format: RecordFormat::Fixed,
    json_number_mode: JsonNumberMode::Lossless, // Preserve decimal precision
    strict_mode: false,
    max_errors: Some(100),
    emit_filler: false, // When true, FILLER fields named as _filler_00000XXX (by computed byte offset)
    emit_meta: true,
    emit_raw: RawMode::Off,
    on_decode_unmappable: UnmappablePolicy::Error,
    threads: 1,
};

// Single record decoding with complete field processing
let record_data = std::fs::read("single-record.bin")?;
let json_value = decode_record(&schema, &record_data, &opts)?;
// json_value now contains properly typed COBOL fields, not nulls
println!("Record: {}", serde_json::to_string_pretty(&json_value)?);

// Complete file processing with performance metrics
let input = std::fs::File::open("data.bin")?;
let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(&schema, input, output, &opts)?;

println!("Processed {} records with {} errors at {:.2} MB/s", 
         summary.records_processed, summary.records_with_errors, summary.throughput_mbps);
```

### High-Performance Streaming Processing

```rust
use copybook_codec::{RecordIterator, DecodeOptions, iter_records_from_file, 
                     decode_record_with_scratch, memory::ScratchBuffers};

// Create iterator for processing records one at a time with complete field processing
// Note: Fixed format now requires LRECL to be specified in schema for truncation detection
let mut iter = iter_records_from_file("data.bin", &schema, &opts)?;

for (record_idx, record_result) in iter.enumerate() {
    match record_result {
        Ok(json_value) => {
            // Process individual record with properly typed COBOL fields
            // Example: {"CUSTOMER-ID": "00123", "CUSTOMER-NAME": "JOHN DOE", "BALANCE": "-1234.56"}
            println!("{}", serde_json::to_string(&json_value)?);
        }
        Err(e) => {
            // Enhanced error reporting includes truncated record detection
            // Example: "Record 15 too short: expected 120 bytes, got 85 bytes"
            eprintln!("Record {} error: {}", record_idx + 1, e);
        }
    }
}

// For maximum performance, use scratch buffers to minimize allocations
let mut scratch = ScratchBuffers::new();
for record_data in records {
    let json_value = decode_record_with_scratch(&schema, &record_data, &opts, &mut scratch)?;
    // Process with 15+ GiB/s throughput for DISPLAY data, 45+ MiB/s for COMP-3
    process_record(json_value);
    // scratch buffers automatically cleared for next iteration
}
```

#### Enhanced RecordIterator API

- **LRECL Requirement**: Fixed-format processing now requires `schema.lrecl_fixed` to be set for proper truncation detection
- **Fail-Fast Validation**: RecordIterator constructor validates LRECL availability early
- **Enhanced Error Messages**: Precise byte counts and record indexing for truncation errors
- **Performance Optimized**: 4-23% performance improvements with enhanced validation

### Data Verification API

```rust
use copybook_codec::{iter_records_from_file, DecodeOptions, RecordFormat, Codepage};
use copybook_core::{parse_copybook, Error};

// Parse copybook
let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

// Configure verification options (no output needed)
let opts = DecodeOptions::new()
    .with_format(RecordFormat::Fixed)
    .with_codepage(Codepage::Cp037);

// Collect validation errors without generating output
let mut errors: Vec<Error> = Vec::new();
let mut iter = iter_records_from_file("data.bin", &schema, &opts)?;

while let Some(result) = iter.next() {
    if let Err(e) = result {
        let idx = iter.current_record_index();
        errors.push(e.with_record(idx));
    }
}

println!("Validation complete: {} errors found in {} records", 
         errors.len(), iter.current_record_index());

// Process errors for reporting
for error in errors {
    if let Some(ctx) = &error.context {
        println!("Record {}: {} - {}", 
                 ctx.record_index.unwrap_or(0),
                 error.code, 
                 error.message);
    }
}
```

## Numeric Data Type Examples

copybook-rs provides comprehensive numeric data type handling with proper precision preservation and improved formatting support. The SmallDecimal type now implements the Display trait for improved debugging and string representation.

### Zoned Decimal Processing
```bash
# Decode EBCDIC zoned decimals with sign zones (C=+, D=-)
copybook decode schema.cpy mainframe-data.bin \
  --codepage cp037 \
  --json-number lossless \
  --output financial-data.jsonl
```

### Packed Decimal (COMP-3) Processing
```bash
# Decode packed decimal fields with nibble signs
copybook decode schema.cpy comp3-data.bin \
  --codepage cp037 \
  --json-number lossless \
  --output packed-data.jsonl
```

### Binary Integer Processing
```bash
# Process binary integers with explicit widths
copybook decode schema.cpy binary-data.bin \
  --json-number native \  # integers can use JSON numbers
  --output binary-data.jsonl

# Handle SYNCHRONIZED binary fields with automatic alignment
copybook decode schema.cpy aligned-binary-data.bin \
  --codepage cp037 \
  --json-number native \
  --output aligned-data.jsonl
```

### Binary Field Alignment (SYNCHRONIZED)

copybook-rs implements IBM mainframe SYNCHRONIZED alignment standards for binary fields:

- **16-bit binary fields**: Aligned to 2-byte boundaries
- **32-bit binary fields**: Aligned to 4-byte boundaries  
- **64-bit binary fields**: Aligned to 8-byte boundaries
- **Padding insertion**: Automatic insertion of alignment padding bytes (0x00) when SYNCHRONIZED is specified
- **Cross-platform consistency**: Alignment behavior matches IBM COBOL compilers across platforms

## Supported COBOL Features

### Data Types
- **Alphanumeric**: `PIC X(n)` - Character data with EBCDIC/ASCII conversion
- **Zoned Decimal**: `PIC 9(n)V9(m)`, `PIC S9(n)V9(m)` - Display numeric with EBCDIC/ASCII sign zones
  - **EBCDIC Overpunch**: Zone nibbles (C/F = positive, D = negative) in sign position
  - **ASCII Overpunch**: Special characters in sign position (A-I = +1 to +9, } = +0, J-R = -1 to -9)
  - **Negative Zero Normalization**: `-0` (negative zero) automatically normalizes to `"0"` in JSON output
  - **Sign Preservation**: Non-zero negative values preserve their negative sign (`-1234` remains `"-1234"`)
- **Packed Decimal**: `PIC 9(n)V9(m) COMP-3`, `PIC S9(n)V9(m) COMP-3` - Binary-coded decimal with nibble signs
  - Enhanced sign nibble handling (0xC/0xF = positive, 0xD/0xB = negative)
- **Binary Integer**: `PIC 9(n) COMP/BINARY`, `PIC S9(n) COMP/BINARY` - Big-endian integers (1-8 bytes)
- **Explicit Binary Width**: `PIC 9(n) BINARY(w)` - Binary integers with explicit byte width (1, 2, 4, 8)
- **Signed Fields**: Full support for signed zoned, packed, and binary types with proper sign handling

### Structure Features
- **Level Numbers**: 01-49 hierarchical grouping
- **REDEFINES**: Multiple views over same storage area
- **OCCURS**: Fixed arrays and variable arrays (OCCURS DEPENDING ON)
- **SYNCHRONIZED**: Field alignment on natural boundaries following IBM mainframe standards (2/4/8-byte boundaries for binary fields)
- **BLANK WHEN ZERO**: Special handling for zero values

### Record Formats
- **Fixed-Length**: Constant LRECL records
- **Variable-Length**: RDW (Record Descriptor Word) format
- **ODO Support**: Variable arrays at tail position only

### Limitations
- **Unsupported**: COMP-1 (single-precision float), COMP-2 (double-precision float)
- **Unsupported**: Edited PIC clauses (Z, /, comma, $, CR, DB)
- **Unsupported**: SIGN LEADING/TRAILING SEPARATE
- **Unsupported**: Nested ODO arrays (ODO within ODO)
- **Unsupported**: 66-level (RENAMES) and 88-level (condition names) items

## Error Handling

copybook-rs uses a comprehensive error taxonomy with stable codes:

### Parse Errors (CBKP*)
- `CBKP001_SYNTAX`: Copybook syntax errors
- `CBKP011_UNSUPPORTED_CLAUSE`: Unsupported COBOL clause or feature
- `CBKP021_ODO_NOT_TAIL`: ODO array not at tail position
- `CBKP051_UNSUPPORTED_EDITED_PIC`: Edited PIC clauses not supported

### Schema Validation Errors (CBKS*)
- `CBKS121_COUNTER_NOT_FOUND`: ODO counter field not found
- `CBKS141_RECORD_TOO_LARGE`: Record size exceeds maximum limit
- `CBKS301_ODO_CLIPPED`: ODO counter exceeds maximum (strict: fatal, lenient: warning with clamping)
- `CBKS302_ODO_RAISED`: ODO counter below minimum (strict: fatal, lenient: warning with clamping)

### Data Errors (CBKD*)
- `CBKD101_INVALID_FIELD_TYPE`: Invalid field type for operation
- `CBKD301_RECORD_TOO_SHORT`: Record too short for field
- `CBKD401_COMP3_INVALID_NIBBLE`: Invalid packed decimal data
- `CBKD411_ZONED_BAD_SIGN`: Invalid zoned decimal sign
- `CBKD412_ZONED_BLANK_IS_ZERO`: BLANK WHEN ZERO field decoded as zero

### Encoding Errors (CBKE*)
- `CBKE501_JSON_TYPE_MISMATCH`: JSON type doesn't match field type or REDEFINES ambiguity
- `CBKE521_ARRAY_LEN_OOB`: Array length out of bounds

See [ERROR_CODES.md](docs/ERROR_CODES.md) for complete error reference and [REPORT.md](REPORT.md) for detailed project status and performance analysis.

## Requirements

- **Rust**: 1.89 or later (MSRV)
- **Edition**: 2024
- **Platform**: Linux, macOS, Windows

## Performance

### Performance Results - Complete COBOL→JSON Processing
- **DISPLAY-heavy data**: **4.1-4.2 GiB/s achieved** (target: ≥80 MB/s) - **50-52x performance target exceeded**
- **COMP-3-heavy data**: **560-580 MiB/s achieved** (target: ≥40 MB/s) - **14-15x performance target exceeded**
- **Performance Stability**: <5% variance across benchmark runs with comprehensive processing
- **Memory usage**: <256 MiB steady-state for multi-GB files
- **SLO Validation**: Continuous benchmark validation ensures targets are consistently exceeded

**Performance Evaluation Complete**: Comprehensive benchmarking demonstrates exceptional throughput with substantial safety margins above targets, validating production readiness for mainframe data processing workloads.

### Optimization Features
- **Scratch Buffer Optimization**: Reusable memory buffers minimize allocations in hot paths
- **Safe Type Conversions**: Complete elimination of unsafe `u32` to `u8` casts with `try_from()`
- **Streaming I/O**: Bounded memory usage with optimized buffer management
- **Parallel Processing**: Deterministic output with worker pool optimization
- **Zero-copy Operations**: Where possible with memory-safe implementations
- **Static Lookup Tables**: EBCDIC conversion with performance-optimized character mapping
- **Idiomatic Rust Patterns**: Complete clippy pedantic compliance (140+ violations resolved)
- **Performance Monitoring**: Comprehensive benchmark infrastructure with SLO validation

## Development

### Building

```bash
# Clone the repository
git clone https://github.com/copybook-rs/copybook-rs.git
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

# Run performance benchmarks
cargo bench --package copybook-bench

# Run with performance environment variable
PERF=1 cargo bench

# Run clippy
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Format code
cargo fmt --all
```

### Contributing

We welcome contributions! Please see [REPORT.md](REPORT.md) for current project status and development priorities.

#### Development Workflow
1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Run `cargo test --workspace` and `cargo clippy --workspace`
5. Submit a pull request

#### Code Standards
- Follow Rust conventions and idioms with complete clippy pedantic compliance
- Add comprehensive tests for new features (127 tests passing)
- Update documentation for API changes
- Maintain MSRV compatibility (Rust 1.89)
- Use idiomatic Rust patterns (div_ceil, is_empty, range contains)
- Implement Display trait for user-facing types where appropriate
- Use safe type conversions (try_from() instead of unsafe casts)
- Optimize memory usage with scratch buffer patterns

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
