# copybook-rs

A modern, memory-safe parser/codec for COBOL copybooks and fixed-record data.

## Overview

copybook-rs is a Rust implementation of a COBOL copybook parser and data codec that provides deterministic, reproducible conversion of mainframe-encoded records into accessible formats like JSON. With robust field processing and comprehensive COBOL data type support, it enables organizations to unlock mainframe data for analytics, system integration, and modernization efforts without requiring COBOL runtime environments.

### Key Benefits

- **Mainframe Data Liberation**: Convert legacy COBOL data formats to modern JSON without COBOL runtime
- **ETL Integration**: Stream processing of multi-GB mainframe files with bounded memory usage
- **Audit Compliance**: Deterministic output with byte-identical results across runs
- **Round-Trip Fidelity**: Lossless conversion preserves original data integrity with proper COBOL field processing
- **Production Ready**: Comprehensive error handling with stable error codes

## Features

- **Deterministic Output**: Byte-identical results across runs and parallel processing
- **Round-Trip Fidelity**: Unchanged JSON data re-encodes to identical binary
- **Memory Safety**: No unsafe code in public API paths
- **Streaming Architecture**: Bounded memory usage for multi-GB files
- **Comprehensive Error Handling**: Stable error codes with structured context
- **COBOL Feature Support**: REDEFINES, OCCURS DEPENDING ON, SYNCHRONIZED, packed/zoned decimals
- **Character Encoding**: Full EBCDIC support (CP037, CP273, CP500, CP1047, CP1140) and ASCII
- **Performance**: ≥80 MB/s throughput for DISPLAY-heavy data, ≥40 MB/s for COMP-3-heavy
- **Parser Stability**: Infinite loop prevention with robust error handling

## Architecture

The project is organized as a Cargo workspace with the following crates:

- **copybook-core**: Core parsing and schema types for COBOL copybooks
- **copybook-codec**: Encoding and decoding codecs for COBOL data types
- **copybook-cli**: Command-line interface for copybook processing
- **copybook-gen**: Test fixture and synthetic data generation
- **copybook-bench**: Performance benchmarks and testing

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
# Include FILLER fields in output
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

## JSON Output Quality

copybook-rs produces clean, properly typed JSON output with comprehensive COBOL field processing:

- **Proper Field Values**: COBOL fields are decoded to their correct string or numeric representations (no unintended null values)
- **Numeric Precision**: Zoned and packed decimals maintain precision with proper sign handling
- **Character Conversion**: EBCDIC and ASCII character data converted to UTF-8 strings
- **Hierarchical Structure**: Group fields create nested JSON objects matching copybook structure

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

### Basic Rust Integration

```rust
use copybook_core::parse_copybook;
use copybook_codec::{
    decode_file_to_jsonl, DecodeOptions, Codepage, RecordFormat, 
    JsonNumberMode, RawMode, UnmappablePolicy
};
use std::path::Path;

// Parse copybook
let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

// Configure decode options
let opts = DecodeOptions {
    codepage: Codepage::Cp037,
    format: RecordFormat::Fixed,
    json_number_mode: JsonNumberMode::Lossless, // Preserve decimal precision
    strict_mode: false,
    max_errors: Some(100),
    emit_filler: false,
    emit_meta: true,
    emit_raw: RawMode::Off,
    on_decode_unmappable: UnmappablePolicy::Error,
    threads: 1,
};

// Decode to JSONL
let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(
    &schema,
    Path::new("data.bin"),
    &opts,
    output,
)?;

println!("Processed {} records with {} errors", 
         summary.records_processed, summary.error_count);
```

### Streaming Record Processing

```rust
use copybook_codec::{RecordIterator, DecodeOptions, iter_records_from_file};

// Create iterator for processing records one at a time
let mut iter = iter_records_from_file("data.bin", &schema, &opts)?;

for record_result in iter {
    match record_result {
        Ok(json_value) => {
            // Process individual record - now properly typed fields, not nulls
            // Example output: {"CUSTOMER-ID": "00123", "NAME": "JOHN DOE"}
            println!("{}", serde_json::to_string(&json_value)?);
        }
        Err(e) => {
            eprintln!("Record {} error: {}", record_idx, e);
        }
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
```

## Supported COBOL Features

### Data Types
- **Alphanumeric**: `PIC X(n)` - Character data with EBCDIC/ASCII conversion
- **Zoned Decimal**: `PIC 9(n)V9(m)`, `PIC S9(n)V9(m)` - Display numeric with EBCDIC/ASCII sign zones
  - Supports EBCDIC zone nibbles (C/F = positive, D = negative)
  - Supports ASCII overpunch characters (A-I = +1 to +9, } = +0, J-R = -1 to -9)
- **Packed Decimal**: `PIC 9(n)V9(m) COMP-3`, `PIC S9(n)V9(m) COMP-3` - Binary-coded decimal with nibble signs
  - Enhanced sign nibble handling (0xC/0xF = positive, 0xD/0xB = negative)
- **Binary Integer**: `PIC 9(n) COMP/BINARY`, `PIC S9(n) COMP/BINARY` - Big-endian integers (1-8 bytes)
- **Explicit Binary Width**: `PIC 9(n) BINARY(w)` - Binary integers with explicit byte width (1, 2, 4, 8)
- **Signed Fields**: Full support for signed zoned, packed, and binary types with proper sign handling

### Structure Features
- **Level Numbers**: 01-49 hierarchical grouping
- **REDEFINES**: Multiple views over same storage area
- **OCCURS**: Fixed arrays and variable arrays (OCCURS DEPENDING ON)
- **SYNCHRONIZED**: Field alignment on natural boundaries
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
- `CBKP051_UNSUPPORTED_EDITED_PIC`: Edited PIC clauses not supported
- `CBKP021_ODO_NOT_TAIL`: ODO array not at tail position

### Data Errors (CBKD*)
- `CBKD401_COMP3_INVALID_NIBBLE`: Invalid packed decimal data
- `CBKD411_ZONED_BAD_SIGN`: Invalid zoned decimal sign
- `CBKD412_ZONED_BLANK_IS_ZERO`: BLANK WHEN ZERO field decoded as zero

### Encoding Errors (CBKE*)
- `CBKE501_JSON_TYPE_MISMATCH`: JSON type doesn't match field type
- `CBKE521_ARRAY_LEN_OOB`: Array length out of bounds

See [ERROR_CODES.md](docs/ERROR_CODES.md) for complete error reference.

## Requirements

- **Rust**: 1.89 or later (MSRV)
- **Edition**: 2024
- **Platform**: Linux, macOS, Windows

## Performance

### Throughput Targets
- **DISPLAY-heavy data**: ≥80 MB/s (maintained through code quality improvements)
- **COMP-3-heavy data**: ≥40 MB/s (maintained through code quality improvements) 
- **Memory usage**: <256 MiB steady-state for multi-GB files

### Optimization Features
- Streaming I/O with bounded memory
- Parallel processing with deterministic output
- Zero-copy operations where possible
- Static lookup tables for EBCDIC conversion
- Idiomatic Rust patterns for improved performance (div_ceil, is_empty, range contains)
- Clippy pedantic compliance for performance and safety optimizations

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

# Run performance benchmarks (requires PERF=1)
PERF=1 cargo bench

# Run clippy
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Format code
cargo fmt --all
```

### Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

#### Development Workflow
1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Run `cargo test --workspace` and `cargo clippy --workspace`
5. Submit a pull request

#### Code Standards
- Follow Rust conventions and idioms with clippy pedantic compliance
- Add comprehensive tests for new features
- Update documentation for API changes
- Maintain MSRV compatibility (Rust 1.89)
- Use idiomatic Rust patterns (div_ceil, is_empty, range contains)
- Implement Display trait for user-facing types where appropriate

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.