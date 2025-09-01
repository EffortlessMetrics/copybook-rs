# Design Document

## Overview

copybook-rs is a modern, memory-safe parser/codec for COBOL copybooks and fixed-record data implemented in Rust. The system provides deterministic, corpus-driven functionality through both CLI and library interfaces, targeting mainframe data modernization and ETL integration scenarios.

The tool bridges legacy COBOL data formats with modern systems by providing deterministic, reproducible conversion of mainframe-encoded records into accessible JSON formats. This enables organizations to unlock mainframe data for analytics, system integration, and modernization efforts without requiring COBOL runtime environments.

## Architecture

The system is designed with a modular architecture that separates concerns into distinct, testable components:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   copybook-cli  │    │ copybook-codec  │    │  copybook-core  │
│                 │    │                 │    │                 │
│ • CLI Interface │    │ • Encode/Decode │    │ • Parser        │
│ • File I/O      │    │ • EBCDIC/ASCII  │    │ • Layout Engine │
│ • Error Display │    │ • RDW Handling  │    │ • Schema Types  │
│ • Parallelism   │    │ • Type Codecs   │    │ • Validation    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         └───────────────────────┼───────────────────────┘
                                 │
                    ┌─────────────────┐
                    │  copybook-gen   │
                    │                 │
                    │ • Test Fixtures │
                    │ • Synthetic     │
                    │   Data Gen      │
                    │ • Golden Tests  │
                    └─────────────────┘
```

### Component Responsibilities

**copybook-core**: Handles copybook parsing, AST construction, layout resolution, and schema validation. This is the foundational component that understands COBOL data structures.

**copybook-codec**: Implements the actual encoding/decoding logic for all COBOL data types, character set conversion, and record framing (fixed/RDW).

**copybook-cli**: Provides the command-line interface, file I/O operations, error reporting, and parallel processing coordination.

**copybook-gen**: Generates synthetic test data and fixtures for comprehensive testing and validation.

## Components and Interfaces

### 1. Copybook Parser (copybook-core)

The parser component transforms COBOL copybook text into a structured schema representation.

#### Key Types

```rust
pub struct Schema {
    pub fields: Vec<Field>,
    pub lrecl_fixed: Option<u32>,
    pub tail_odo: Option<TailODO>,
}

pub struct Field {
    pub path: String,                 // "ROOT.CUSTOMER.ID"
    pub kind: FieldKind,
    pub offset: u32,                  // byte offset
    pub len: u32,                     // field length in bytes
    pub redefines_of: Option<String>, // path of redefined field
    pub occurs: Option<Occurs>,
    pub sync: Option<u16>,            // alignment bytes if SYNCHRONIZED
}

pub enum FieldKind {
    Alphanum { len: u32 },
    ZonedDecimal { digits: u16, scale: i16, signed: bool },
    BinaryInt { bits: u16, signed: bool },
    PackedDecimal { digits: u16, scale: i16, signed: bool },
    Group,
}

pub enum Occurs {
    Fixed { count: u32 },
    ODO { min: u32, max: u32, counter_path: String },
}
```

#### Parser Implementation Strategy

1. **Lexical Analysis**: Use `logos` crate for tokenization with support for:
   - Fixed-form COBOL (columns 1-6 sequence, 7 indicator, 8-72 text)
   - Free-form COBOL with comment handling
   - Continuation line processing

2. **Syntax Analysis**: Recursive descent parser that builds an AST from tokens:
   - Level number hierarchy validation
   - PIC clause parsing and validation
   - USAGE clause interpretation
   - REDEFINES relationship tracking
   - OCCURS clause processing

3. **Layout Resolution**: Single-pass algorithm that computes:
   - Field byte offsets with alignment
   - REDEFINES cluster sizing (max of all variants)
   - SYNCHRONIZED padding insertion
   - ODO validation and space allocation

### 2. Data Codec (copybook-codec)

The codec component handles the actual conversion between binary data and structured values.

#### Encoding Specifications

**EBCDIC/ASCII Conversion**:
- Static lookup tables for supported code pages (cp037, cp273, cp500, cp1047, cp1140)
- UTF-8 output for all text fields
- Error handling for unmappable characters

**Numeric Type Handling**:

*Zoned Decimal (DISPLAY)*:
- Each digit stored as one character byte
- Sign encoded in zone nibble of last character (C/F=+, D=-)
- BLANK WHEN ZERO: all spaces decode to numeric zero

*Packed Decimal (COMP-3)*:
- Two digits per byte, high nibble first
- Last nibble contains sign (C/F=+, D=-)
- Byte length = ceil((digits + 1) / 2)

*Binary (COMP/BINARY)*:
- Big-endian integers (IBM mainframe convention)
- Width by PIC digits: ≤4→2B, 5-9→4B, 10-18→8B
- Two's complement for signed values

#### Record Framing

**Fixed Records**:
- Constant LRECL (Logical Record Length)
- Back-to-back record layout
- Length validation against schema

**RDW Records (Variable)**:
- 4-byte Record Descriptor Word header
- Bytes 0-1: big-endian data length (excluding RDW)
- Bytes 2-3: reserved (expected 0x0000)
- Corruption detection for ASCII transfer issues

### 3. CLI Interface (copybook-cli)

The CLI provides user-friendly access to all functionality with comprehensive error reporting.

#### Command Structure

```bash
copybook parse   <schema.cpy> --json > schema.json
copybook inspect <schema.cpy> [--codepage cp037]
copybook decode  <schema.cpy> <data.bin> [options] --out out.jsonl
copybook encode  <schema.cpy> <in.jsonl> [options] --out out.bin
copybook verify  <schema.cpy> <data.bin> [options] --report verify.json
```

#### Key Features

- **Streaming I/O**: Process records one at a time to maintain bounded memory usage
- **Parallel Processing**: Ordered pipeline with `--threads` option
- **Error Resilience**: Configurable strict/lenient modes with detailed error reporting
- **Progress Reporting**: Summary statistics and performance metrics

### 4. Library API (copybook-core/codec)

The library provides a clean Rust API for programmatic integration.

#### Core Functions

```rust
// Parse copybook into schema
pub fn parse_copybook(text: &str) -> Result<Schema, Error>;

// Decode binary data to JSON
pub fn decode_file_to_jsonl(
    schema: &Schema,
    data_path: &Path,
    opts: &DecodeOptions,
    out: impl Write,
) -> Result<RunSummary, Error>;

// Encode JSON to binary
pub fn encode_jsonl_to_file(
    schema: &Schema,
    jsonl_path: &Path,
    opts: &EncodeOptions,
    out_path: &Path,
) -> Result<RunSummary, Error>;
```

## Data Models

### Schema Representation

The internal schema model captures all necessary information for deterministic encoding/decoding:

- **Hierarchical Structure**: Preserves COBOL level relationships
- **Byte-Level Precision**: Exact offsets and lengths for all fields
- **Type Safety**: Strongly typed field kinds with validation
- **Metadata Preservation**: REDEFINES relationships, OCCURS constraints, alignment requirements

### JSON Output Format

The JSON output follows strict deterministic rules:

1. **Key Ordering**: Schema pre-order traversal (groups before children)
2. **Numeric Representation**: 
   - Packed/Zoned decimals as canonical strings for precision
   - Binary integers as JSON numbers (up to 64-bit) or strings
3. **REDEFINES Handling**: All views emitted in declaration order
4. **Array Representation**: OCCURS as JSON arrays, ODO with actual length only

### Round-Trip Fidelity

To ensure byte-identical round-trips, the system supports raw byte capture:

- `--emit-raw=record|field`: Capture original bytes as base64
- `--use-raw`: Use captured bytes when encoding if values match
- Guarantees: `encode(decode(bytes)) == bytes` when JSON unchanged

## Error Handling

### Error Taxonomy

The system uses a comprehensive, stable error code taxonomy:

**Parse Errors (CBKP\*)**:
- CBKP001_SYNTAX: Copybook syntax errors
- CBKP011_UNSUPPORTED_CLAUSE: Unsupported COBOL features
- CBKP021_ODO_NOT_TAIL: ODO not at tail position

**Schema Errors (CBKS\*)**:
- CBKS121_COUNTER_NOT_FOUND: ODO counter field missing
- CBKS301_ODO_CLIPPED: ODO count exceeds maximum

**Data Errors (CBKD\*)**:
- CBKD401_COMP3_INVALID_NIBBLE: Invalid packed decimal data
- CBKD411_ZONED_BAD_SIGN: Invalid zoned decimal sign

**Encoding Errors (CBKE\*)**:
- CBKE501_JSON_TYPE_MISMATCH: JSON type doesn't match field type
- CBKE521_ARRAY_LEN_OOB: Array length out of bounds

### Error Handling Strategy

1. **Parse Errors**: Fail fast with detailed context (line numbers, expected vs found)
2. **Data Errors**: Configurable handling:
   - Strict mode: Stop on first error
   - Lenient mode: Skip bad records, continue processing
   - Always report error counts and context
3. **Structured Context**: All errors include relevant metadata (record number, field path, byte offset)

## Testing Strategy

### Unit Testing

- **Parser Components**: Tokenizer, AST construction, layout resolution
- **Codec Functions**: Each data type encoding/decoding with property tests
- **Edge Cases**: Alignment, REDEFINES clusters, ODO boundary conditions

### Integration Testing

- **End-to-End Flows**: Complete decode→encode round-trips
- **CLI Testing**: All command combinations with various options
- **Error Scenarios**: Invalid inputs, corruption detection, boundary conditions

### Synthetic Test Generation

The `copybook-gen` component creates comprehensive test suites:

- **Deterministic Generation**: Seeded random generation for reproducible tests
- **Coverage Matrix**: All combinations of field types, alignments, structures
- **Golden Tests**: SHA-256 verified outputs for regression detection
- **Negative Cases**: Invalid data patterns, corruption scenarios

### Performance Testing

- **Throughput Benchmarks**: Target ≥80 MB/s for DISPLAY-heavy, ≥40 MB/s for COMP-3-heavy
- **Memory Usage**: Steady-state <256 MiB for multi-GB files
- **Scalability**: Parallel processing validation with deterministic output ordering

## Performance Considerations

### Memory Management

- **Streaming Architecture**: Process one record at a time
- **Bounded Queues**: Prevent memory growth under parallel processing
- **Zero-Copy Operations**: Minimize allocations in hot paths
- **Static Tables**: Pre-computed EBCDIC conversion tables

### Optimization Strategies

1. **Efficient Parsing**: Hand-optimized lexer with minimal backtracking
2. **Codec Performance**: Direct byte manipulation without string intermediates
3. **I/O Optimization**: Buffered readers with appropriate chunk sizes
4. **Parallel Pipeline**: Ordered processing with work-stealing queues

### Resource Limits

- **Record Size**: Default 16 MiB maximum (configurable)
- **ODO Arrays**: Bounded by schema maximum
- **File Size**: No inherent limits (streaming processing)
- **Thread Count**: Configurable with deterministic output ordering

## Security Considerations

### Input Validation

- **Path Sanitization**: Prevent directory traversal attacks
- **Size Limits**: Prevent resource exhaustion attacks
- **Format Validation**: Strict parsing with clear error messages

### Memory Safety

- **No Unsafe Code**: Public API paths use only safe Rust
- **Bounded Operations**: All loops and recursions have explicit limits
- **Error Propagation**: Structured error handling prevents panics

### Data Integrity

- **Checksum Validation**: Optional integrity checking for critical data
- **Corruption Detection**: Heuristics for common transfer issues
- **Audit Trails**: Comprehensive logging and error reporting

This design provides a solid foundation for implementing a robust, performant, and maintainable COBOL data processing system that meets the needs of modern data engineering workflows while preserving the precision and reliability required for financial and business-critical applications.