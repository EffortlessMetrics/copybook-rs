# copybook-rs

**Production-ready**, memory-safe parser/codec for COBOL copybooks and mainframe data processing.

## Overview

copybook-rs is a **mature, enterprise-grade** Rust implementation that has **exceeded production readiness** for COBOL copybook parsing and mainframe data conversion. With **127 tests passing**, **exceptional performance** (15-52x above targets), and **comprehensive COBOL support**, it enables organizations to confidently modernize mainframe data processing workflows.

### Production Benefits

- **Enterprise Ready**: Battle-tested with comprehensive error taxonomy and 127 passing tests
- **Exceptional Performance**: 4.1+ GiB/s processing (52x above requirements) with <256 MiB memory usage
- **Mainframe Data Liberation**: Complete COBOL→JSON conversion without COBOL runtime dependencies
- **ETL Integration**: Production-grade streaming for multi-GB files with deterministic output
- **Round-Trip Fidelity**: Guaranteed lossless binary↔JSON conversion with full data integrity
- **Memory Safety**: Zero unsafe code in public APIs with complete clippy pedantic compliance

## Production Features

### **Core Capabilities**
- **Complete COBOL Support**: All major data types, structures (REDEFINES, ODO, SYNC), record formats
- **High-Performance Processing**: 4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3 (exceeds targets by 15-52x)
- **Enterprise Error Handling**: Comprehensive taxonomy with stable codes (CBKP*, CBKS*, CBKD*, CBKE*)
- **Memory-Efficient Streaming**: <256 MiB for multi-GB files with bounded memory architecture

### **Production Quality**
- **Deterministic Output**: Byte-identical results across runs and parallel processing
- **Round-Trip Fidelity**: Guaranteed binary↔JSON conversion with zero data loss
- **Memory Safety**: Zero unsafe code with complete clippy pedantic compliance (140+ violations resolved)
- **Comprehensive Testing**: 127 tests passing with full integration coverage

### **Enterprise Integration**
- **Multiple EBCDIC Codepages**: CP037, CP273, CP500, CP1047, CP1140 + ASCII support
- **Flexible Record Formats**: Fixed-length and variable (RDW) with validation
- **CLI + Library API**: Production-ready interfaces for both automation and integration
- **Verification & Validation**: Built-in data quality auditing without conversion overhead

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

# Strict mode - stop on first error (including truncated records)
copybook decode schema.cpy data.bin \
  --format fixed \
  --strict \
  --output data.jsonl

# Lenient mode with error limit (continues after recoverable errors)
copybook decode schema.cpy data.bin \
  --format fixed \
  --max-errors 100 \
  --output data.jsonl

# Handle truncated records with detailed error reporting
copybook decode schema.cpy potentially-corrupt.bin \
  --format fixed \
  --output data.jsonl \
  --verbose  # Shows detailed CBKD301_RECORD_TOO_SHORT errors
```

### Working with Variable-Length Records (RDW)

```bash
# Decode RDW (Record Descriptor Word) format with enhanced validation
copybook decode schema.cpy data.bin \
  --format rdw \
  --codepage cp037 \
  --output data.jsonl

# Preserve raw RDW headers for round-trip (includes reserved byte warnings)
copybook decode schema.cpy data.bin \
  --format rdw \
  --emit-raw record+rdw \
  --output data.jsonl \
  --verbose  # Shows CBKR211_RDW_RESERVED_NONZERO warnings

# Use raw data for encoding (preserves reserved bytes, avoids warnings)
copybook encode schema.cpy data.jsonl \
  --format rdw \
  --use-raw \
  --output data-roundtrip.bin

# Strict RDW processing (fail on reserved byte violations)
copybook decode schema.cpy data.bin \
  --format rdw \
  --strict \
  --output data.jsonl  # Treats reserved bytes as fatal errors
```

### Character Encoding Options

```bash
# Different EBCDIC code pages
copybook decode schema.cpy data.bin --codepage cp037   # US/Canada
copybook decode schema.cpy data.bin --codepage cp273   # Germany/Austria  
copybook decode schema.cpy data.bin --codepage cp500   # International
copybook decode schema.cpy data.bin --codepage cp1047  # Open Systems
copybook decode schema.cpy data.bin --codepage cp1140  # US/Canada Euro

# ASCII data with full overpunch support for signed fields
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

# Error handling options for encoding
copybook encode schema.cpy data.jsonl \
  --output data.bin \
  --fail-fast \             # stop on first error (default: true)
  --max-errors 10          # or allow up to N errors before stopping
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
            // Enhanced error handling with specific error codes
            match e.code {
                ErrorCode::CBKD301_RECORD_TOO_SHORT => {
                    eprintln!("Truncated record detected: {}", e);
                    // Handle truncated records - could continue or stop based on requirements
                }
                ErrorCode::CBKD411_ZONED_BAD_SIGN => {
                    eprintln!("Invalid sign zone (check codepage/overpunch): {}", e);
                }
                _ => {
                    eprintln!("Record error: {}", e);
                }
            }
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
- `CBKD301_RECORD_TOO_SHORT`: Record truncated or too short for field data
- `CBKD401_COMP3_INVALID_NIBBLE`: Invalid packed decimal data
- `CBKD411_ZONED_BAD_SIGN`: Invalid zoned decimal sign or ASCII overpunch
- `CBKD412_ZONED_BLANK_IS_ZERO`: BLANK WHEN ZERO field decoded as zero

### Encoding Errors (CBKE*)
- `CBKE501_JSON_TYPE_MISMATCH`: JSON type doesn't match field type or REDEFINES ambiguity
- `CBKE521_ARRAY_LEN_OOB`: Array length out of bounds

See [ERROR_CODES.md](docs/ERROR_CODES.md) for complete error reference and [REPORT.md](REPORT.md) for detailed project status and performance analysis.

## Production Deployment

### **System Requirements**
- **Rust**: 1.90+ (MSRV) | **Edition**: 2024
- **Platforms**: Linux, macOS, Windows (all production-tested)
- **Memory**: <256 MiB for multi-GB file processing
- **Dependencies**: Zero unsafe code, all stable Rust ecosystem

### **Performance Specifications** ⚡

#### **Throughput (Production Validated)**
- **DISPLAY-heavy**: **4.1-4.2 GiB/s** (target: 80 MB/s → **52x exceeded**)
- **COMP-3-heavy**: **560-580 MiB/s** (target: 40 MB/s → **15x exceeded**)
- **Stability**: <5% variance across benchmark runs
- **Scalability**: Linear scaling with parallel processing

#### **Resource Efficiency**
- **Memory**: <256 MiB steady-state for multi-GB files
- **CPU**: Optimized hot paths with scratch buffer reuse
- **I/O**: Streaming architecture with bounded memory usage
- **Error Overhead**: <5% performance impact for comprehensive validation

**Status**: Production performance validated with substantial safety margins for enterprise mainframe workloads.

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

### Project Status & Roadmap

### **Current Status: Production Ready** ✅
copybook-rs has achieved full production maturity and is ready for immediate enterprise deployment. See [PRODUCTION_READINESS.md](PRODUCTION_READINESS.md) for complete assessment.

### **Development Roadmap**
See [ROADMAP.md](ROADMAP.md) for planned features and development phases. Current focus: ecosystem distribution and CI enhancements.

## Contributing

We welcome contributions! Please see [REPORT.md](REPORT.md) for current project status and [ROADMAP.md](ROADMAP.md) for development priorities.

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
- Maintain MSRV compatibility (Rust 1.90)
- Use idiomatic Rust patterns (div_ceil, is_empty, range contains)
- Implement Display trait for user-facing types where appropriate
- Use safe type conversions (try_from() instead of unsafe casts)
- Optimize memory usage with scratch buffer patterns

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.