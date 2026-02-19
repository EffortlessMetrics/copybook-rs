<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Design Document

## Overview

copybook-rs is a modern, memory-safe parser/codec for COBOL copybooks and fixed-record data implemented in Rust. The system provides deterministic, corpus-driven functionality through both CLI and library interfaces, targeting mainframe data modernization and ETL integration scenarios.

The tool bridges legacy COBOL data formats with modern systems by providing deterministic, reproducible conversion of mainframe-encoded records into accessible JSON formats. This enables organizations to unlock mainframe data for analytics, system integration, and modernization efforts without requiring COBOL runtime environments.

### Key Design Principles

- **Deterministic Output**: Byte-identical results across runs and parallel processing
- **Round-Trip Fidelity**: Unchanged JSON data re-encodes to identical binary
- **Memory Safety**: No unsafe code in public API paths
- **Streaming Architecture**: Bounded memory usage for multi-GB files
- **Comprehensive Error Handling**: Stable error codes with structured context

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
   - **Fixed-form COBOL**: Cols 1-6 sequence (ignored), col 7 indicators (* comment, - continuation, / page break), cols 8-72 text, 73-80 ignored
   - **Free-form COBOL**: `*>` inline comments, leading `*` at col 1 as comment, no block comments
   - **Continuation processing**: Only column-7 `-` continues lines; strip trailing/leading spaces when joining
   - **Edited PIC parsing**: Parse into EditedNumeric FieldKind (E1/E2/E3 supported); only Space (B) insertion returns CBKP051_UNSUPPORTED_EDITED_PIC

2. **Syntax Analysis**: Recursive descent parser that builds an AST from tokens:
   - Level number hierarchy validation (01-49)
   - PIC clause parsing: X(n), 9(n)[V9(m)], optional S for signed
   - USAGE clause interpretation: DISPLAY, COMP/BINARY, COMP-3
   - REDEFINES relationship tracking with target validation
   - OCCURS clause processing (fixed and DEPENDING ON)
   - VALUE/66/88 levels parsed and ignored (metadata only)

3. **Layout Resolution**: Single-pass algorithm that computes:
   - Field byte offsets with SYNCHRONIZED alignment (2/4/8 byte boundaries)
   - REDEFINES cluster sizing (max of all variants, deterministic)
   - SYNCHRONIZED padding insertion (0x00 bytes)
   - ODO validation: driver precedes array, tail-position only, not in REDEFINES/ODO
   - Overflow protection: all offset/length math in u64 with explicit checks

### 2. Data Codec (copybook-codec)

The codec component handles the actual conversion between binary data and structured values.

#### Encoding Specifications

**EBCDIC/ASCII Conversion**:
- Static lookup tables for supported code pages (cp037, cp273, cp500, cp1047, cp1140)
- UTF-8 output for all text fields
- Configurable unmappable character policy: `--on-decode-unmappable=error|replace|skip`
- Replace mode uses U+FFFD and logs CBKC301_INVALID_EBCDIC_BYTE
- Binary/packed fields never undergo character conversion

**Numeric Type Handling**:

*Zoned Decimal (DISPLAY)*:
- Each digit stored as one character byte
- **Dual sign tables**: EBCDIC overpunch zones (C/F=+, D=-) and ASCII overpunch zones
- Sign table selection by codepage at runtime
- **BLANK WHEN ZERO**: all spaces decode to numeric zero with warning CBKD412_ZONED_BLANK_IS_ZERO
- **Fixed-scale rendering**: Always render with exactly `scale` digits after decimal; scale=0 → integer form
- **Normalization**: -0 → 0 in all output

*Packed Decimal (COMP-3)*:
- Two digits per byte, high nibble first
- Last nibble contains sign (C/F=+, D=-)
- Byte length = ceil((digits + 1) / 2)
- **Strict validation**: Invalid nibbles → CBKD401_COMP3_INVALID_NIBBLE

*Binary (COMP/BINARY)*:
- Big-endian integers (IBM mainframe convention)
- Width by PIC digits: ≤4→2B, 5-9→4B, 10-18→8B
- Two's complement for signed values
- **Range validation**: Overflow on encode → error

#### Record Framing

**Fixed Records**:
- Constant LRECL (Logical Record Length)
- Back-to-back record layout
- Length validation against schema
- ODO tail records have variable length within fixed LRECL

**RDW Records (Variable)**:
- 4-byte Record Descriptor Word header
- Bytes 0-1: big-endian data length (excluding RDW)
- Bytes 2-3: reserved (expected 0x0000)
- **Reserved byte policy**: Non-zero → warning CBKR211_RDW_RESERVED_NONZERO (fatal in strict mode)
- **ASCII transfer detection**: Heuristic for ASCII digit patterns → CBKF104_RDW_SUSPECT_ASCII
- **Zero-length records**: Valid only when schema fixed prefix == 0; else CBKF221_RDW_UNDERFLOW
- **Raw RDW preservation**: `--emit-raw=record+rdw` preserves reserved bytes for round-trip

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

#### Key CLI Options

- **Format**: `--format fixed|rdw` (explicit, no auto-detection by default)
- **Codepage**: `--codepage cp037|cp273|cp500|cp1047|cp1140|ascii`
- **JSON Numbers**: `--json-number lossless|native` (default: lossless)
- **Error Handling**: `--strict` (default: lenient), `--max-errors N`
- **Raw Capture**: `--emit-raw off|record|field|record+rdw`, `--use-raw`
- **Performance**: `--threads N` (maintains deterministic ordering)
- **Output**: `--emit-filler`, `--emit-meta` (schema fingerprint, provenance)
- **BWZ Policy**: `--bwz-encode on|off` (default: off for determinism)

#### Key Features

- **Streaming I/O**: Process records one at a time to maintain bounded memory usage
- **Parallel Processing**: Ordered pipeline with sequence tracking for deterministic output
- **Error Resilience**: Configurable strict/lenient modes with comprehensive error reporting
- **Atomic Outputs**: Write to temporary files and rename on success
- **Progress Reporting**: Summary statistics, performance metrics, and schema fingerprinting

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

1. **Key Ordering**: Schema pre-order traversal (groups before children, declaration order)
2. **Numeric Representation**: 
   - **Packed/Zoned decimals**: Canonical strings with fixed scale (e.g., "123.45" for scale=2)
   - **Binary integers**: JSON numbers (up to 64-bit) or strings for larger values
   - **Normalization**: -0 → 0, no thousands separators
3. **REDEFINES Handling**: All views emitted in declaration order (primary then redefiners)
4. **Array Representation**: OCCURS as JSON arrays, ODO with actual length only (no placeholders)
5. **FILLER Fields**: Omitted by default; `--emit-filler` includes as `_filler_<offset>`
6. **Duplicate Names**: Siblings with same name get `NAME__dup2`, `NAME__dup3` suffixes

### Round-Trip Fidelity

To ensure byte-identical round-trips, the system supports raw byte capture:

- **Raw capture modes**: `--emit-raw=off|record|field|record+rdw`
  - `record`: Add `"__raw_b64"` for entire record (excluding RDW)
  - `field`: Add `"__raw_b64"` siblings per scalar field
  - `record+rdw`: Include RDW header in raw capture
- **Raw usage**: `--use-raw` on encode uses captured bytes when values match canonical decode
- **REDEFINES encode precedence**: 
  1. Raw bytes (if `--use-raw` and values match)
  2. Single non-null view
  3. Error CBKE501_JSON_TYPE_MISMATCH (ambiguous)
- **Guarantees**: `encode(decode(bytes)) == bytes` when JSON unchanged and raw present

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
   - **Strict mode**: Stop on first error; ODO out-of-bounds → fatal
   - **Lenient mode**: Skip bad records, continue processing; ODO out-of-bounds → clamp with warning
   - **Max errors**: `--max-errors N` caps lenient mode processing
   - Always report error counts and context
3. **Structured Context**: All errors include relevant metadata (record number, field path, byte offset)
4. **Exit Codes**: 
   - 0: Success (warnings only)
   - 1: Completed with errors (even if skipped in lenient mode)
   - 2: Fatal error (unable to continue)

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

1. **Efficient Parsing**: Hand-optimized lexer with minimal backtracking, avoid regex in hot paths
2. **Codec Performance**: Direct byte manipulation without string intermediates
3. **Memory Efficiency**: 
   - Reusable scratch buffers per worker thread
   - SmallVec for digit buffers (≤32 bytes on stack)
   - Streaming JSON writer (no intermediate Maps)
4. **I/O Optimization**: Buffered readers with appropriate chunk sizes
5. **Parallel Pipeline**: Bounded channels with sequence tracking for ordered output
6. **Static Tables**: Pre-computed EBCDIC conversion and sign overpunch tables

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

## Normative Design Decisions

The following decisions are normative and must be implemented exactly as specified to ensure deterministic behavior and prevent implementation drift:

### 1. Numeric Canonicalization (NORMATIVE)
- **Fixed-scale rendering**: Zoned/packed decimals always render with exactly `scale` digits after decimal
- **Scale=0 format**: Render as integer without decimal point
- **Normalization**: Always convert -0 to 0 in output
- **Encode validation**: Input strings must match field's scale exactly; reject with CBKE501_JSON_TYPE_MISMATCH

### 2. REDEFINES Encode Precedence (NORMATIVE)
1. If `--use-raw` and record-level `__raw_b64` present and values match canonical decode → emit raw bytes
2. Else if exactly one view under the cluster is non-null → emit from that view  
3. Else → error CBKE501_JSON_TYPE_MISMATCH (ambiguous write)

### 3. ODO Strict vs Lenient Behavior (NORMATIVE)
- **Lenient mode** (default): Clamp out-of-bounds counter to min/max with warnings CBKS301_ODO_CLIPPED/CBKS302_ODO_RAISED
- **Strict mode** (`--strict`): Treat out-of-bounds ODO as fatal error and abort immediately

### 4. RDW Raw Preservation (NORMATIVE)
- `--emit-raw=record+rdw` captures both payload and RDW header
- `--use-raw` preserves RDW reserved bytes verbatim when payload values match
- Recompute length field if payload changed during round-trip

### 5. Duplicate Name Disambiguation (NORMATIVE)
- Sibling fields with identical names: suffix as `NAME__dup2`, `NAME__dup3` in declaration order
- FILLER fields (when emitted): `_filler_<offset>` where offset is zero-padded decimal byte offset

### 6. ASCII Zoned Sign Mapping (NORMATIVE)
- Maintain separate EBCDIC and ASCII overpunch sign tables
- Select table by `--codepage` at runtime
- Invalid zone in either table → CBKD411_ZONED_BAD_SIGN

### 7. Grammar Rules (NORMATIVE)
- **Fixed-form**: Only column-7 `-` is continuation; `*` at col 1 is comment; cols 1-6 and 73-80 ignored
- **Free-form**: Only `*>` (inline) and `*` at column 1 are comments; no other comment starts recognized
- **Continuation join**: Strip trailing spaces on continued line and leading spaces on continuation line; do not collapse interior whitespace
- **Edited PICs**: Parse into EditedNumeric FieldKind (E1/E2/E3 supported); only Space (B) insertion fails with CBKP051_UNSUPPORTED_EDITED_PIC
- **SIGN clauses**: SIGN LEADING/TRAILING [SEPARATE] fails with CBKP011_UNSUPPORTED_CLAUSE

### 8. Additional Normative Decisions (NORMATIVE)
- **Alphanumeric handling**: Decode preserves all spaces (no trimming); encode pads with spaces, over-length → CBKE501_JSON_TYPE_MISMATCH
- **OCCURS nesting**: Nested fixed OCCURS allowed; ODO only at tail of containing group, not nested under another ODO
- **Binary width mapping**: Digits → width (≤4→2B, 5-9→4B, 10-18→8B); explicit USAGE BINARY(n) accepts n ∈ {1,2,4,8}
- **Codepage semantics**: Default cp037; ascii is transparent 8-bit with ASCII overpunch table (not Windows-1252)
- **Exit codes**: 0=success (warnings allowed), 1=completed with errors, 2=fatal (parse failure, strict violation)
- **Parallel determinism**: Writer emits by sequence ID with bounded reordering window; output identical for any thread count

These normative decisions eliminate ambiguity and ensure consistent behavior across implementations and deployments.

## Implementation Readiness

This design provides a solid foundation for implementing a robust, performant, and maintainable COBOL data processing system that meets the needs of modern data engineering workflows while preserving the precision and reliability required for financial and business-critical applications.

The specification is now complete with:
- ✅ Clear architectural boundaries and component responsibilities
- ✅ Normative decisions for all ambiguous areas
- ✅ Comprehensive error taxonomy with stable codes
- ✅ Performance targets with realistic optimization strategies
- ✅ Security considerations and memory safety guarantees
- ✅ Deterministic output guarantees for audit compliance