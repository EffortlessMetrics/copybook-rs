# Library API Reference

Complete reference for using copybook-rs as a Rust library.

## Overview

copybook-rs provides a comprehensive Rust API for parsing COBOL copybooks and encoding/decoding mainframe data with full Level-88 condition value support and structural validation. The library is designed for integration into ETL pipelines, data processing applications, and other systems that need to work with legacy COBOL data formats.

## Core Concepts

### Schema
A parsed representation of a COBOL copybook that defines the structure and layout of records.

### Field
An individual data element within a record, with type information, byte offset, length, and support for Level-88 condition values.

### Codec
Encoding and decoding logic for converting between binary data and structured values.

## Quick Start

Add copybook-rs to your `Cargo.toml`:

```toml
[dependencies]
copybook-core = "0.1"
copybook-codec = "0.1"
```

Basic usage:

```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_file_to_jsonl, DecodeOptions, Codepage, RecordFormat};
use std::path::Path;

// Parse copybook
let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

// Configure decode options
let opts = DecodeOptions {
    codepage: Codepage::Cp037,
    format: RecordFormat::Fixed,
    ..Default::default()
};

// Decode to JSONL
let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(&schema, Path::new("data.bin"), &opts, output)?;
```

## Core Types

### Schema

```rust
pub struct Schema {
    pub fields: Vec<Field>,
    pub lrecl_fixed: Option<u32>,
    pub tail_odo: Option<TailODO>,
    pub fingerprint: String,
}
```

**Fields:**
- `fields` - Hierarchical list of all fields in schema order
- `lrecl_fixed` - Fixed record length (if applicable)
- `tail_odo` - Information about tail ODO array (if present)
- `fingerprint` - SHA-256 hash of canonical schema for provenance

**Methods:**
```rust
impl Schema {
    pub fn find_field(&self, path: &str) -> Option<&Field>;
    pub fn max_record_size(&self) -> u32;
    pub fn validate(&self) -> Result<(), Error>;
}
```

### Field

```rust
pub struct Field {
    pub path: String,
    pub kind: FieldKind,
    pub offset: u32,
    pub len: u32,
    pub redefines_of: Option<String>,
    pub occurs: Option<Occurs>,
    pub sync: Option<u16>,
}
```

**Fields:**
- `path` - Dot-separated field path (e.g., "ROOT.CUSTOMER.ID")
- `kind` - Field type and characteristics
- `offset` - Byte offset within record
- `len` - Field length in bytes
- `redefines_of` - Path of redefined field (if applicable)
- `occurs` - Array information (if applicable)
- `sync_padding` - Alignment padding bytes (if SYNCHRONIZED) following IBM mainframe standards
- `synchronized` - Boolean flag indicating if field uses SYNCHRONIZED alignment

### FieldKind

```rust
pub enum FieldKind {
    Alphanum { len: u32 },
    ZonedDecimal { digits: u16, scale: i16, signed: bool },
    BinaryInt { bits: u16, signed: bool },
    PackedDecimal { digits: u16, scale: i16, signed: bool },
    Group,
    Condition { values: Vec<String> }, // Level-88 condition
}
```

### Occurs

```rust
pub enum Occurs {
    Fixed { count: u32 },
    ODO { min: u32, max: u32, counter_path: String },
}
```

## Configuration Types

### DecodeOptions

```rust
pub struct DecodeOptions {
    pub codepage: Codepage,
    pub format: RecordFormat,
    pub strict: bool,
    pub max_errors: Option<u32>,
    pub emit_filler: bool,
    pub emit_meta: bool,
    pub emit_raw: RawMode,
    pub json_number: JsonNumberMode,
    pub on_decode_unmappable: UnmappablePolicy,
    pub preserve_zoned_encoding: bool,
    pub preferred_zoned_encoding: ZonedEncodingFormat,
}
```

**Zoned Encoding Configuration:**

- `preserve_zoned_encoding`: Enable detection and preservation of original zoned decimal encoding format (ASCII vs EBCDIC digit zones) for binary round-trip fidelity
- `preferred_zoned_encoding`: Fallback encoding format when auto-detection is ambiguous (all-zero fields, mixed encodings)

### EncodeOptions

```rust
pub struct EncodeOptions {
    pub codepage: Codepage,
    pub format: RecordFormat,
    pub strict: bool,
    pub max_errors: Option<u32>,
    pub use_raw: bool,
    pub bwz_encode: bool,
    pub preferred_zoned_encoding: ZonedEncodingFormat,
    pub zoned_encoding_override: Option<ZonedEncodingFormat>,
}
```

**Zoned Encoding Configuration:**

- `preferred_zoned_encoding`: Fallback encoding format when neither preserved metadata nor an explicit override selects a format. `Auto` resolves according to the active `codepage`.
- `zoned_encoding_override`: Explicit override for zoned decimal encoding format. When `Some(format)`, forces all zoned decimal fields to use the specified format, overriding any preserved format from decode operations. When `None`, respects preserved formats from decode metadata.

### Enums

```rust
pub enum Codepage {
    Cp037,    // US/Canada EBCDIC
    Cp273,    // Germany/Austria EBCDIC
    Cp500,    // International EBCDIC
    Cp1047,   // Open Systems EBCDIC
    Cp1140,   // US/Canada Euro EBCDIC
    Ascii,    // ASCII (8-bit transparent)
}

pub enum RecordFormat {
    Fixed,    // Fixed-length records
    Rdw,      // Variable-length with RDW header
}

pub enum JsonNumberMode {
    Lossless, // Decimals as strings, preserve precision
    Native,   // Use JSON numbers where possible
}

pub enum RawMode {
    Off,         // No raw capture
    Record,      // Capture entire record (payload only) as `raw_b64` on the envelope
    Field,       // Capture individual fields as `<field>_raw_b64`
    RecordRdw,   // Capture record + RDW header as `raw_b64`
}

pub enum UnmappablePolicy {
    Error,    // Fail on unmappable characters
    Replace,  // Replace with U+FFFD
    Skip,     // Skip unmappable characters
}

pub enum ZonedEncodingFormat {
    Ascii,    // ASCII digit zones (0x30-0x39)
    Ebcdic,   // EBCDIC digit zones (0xF0-0xF9)
    Auto,     // Automatic detection from data
}
```

## Codepage Notes

copybook-rs ships with an explicit allowlist of production codepages. The encoder/decoder
refuses to instantiate for any value outside the `Codepage` enum and returns
`ErrorCode::CBKC301_INVALID_EBCDIC_BYTE` with the message `Unsupported codepage: <NAME>`.
This ensures unsupported locales fail fast instead of silently corrupting data.

| Codepage | Family | Default zoned digits* | Notes |
| -------- | ------ | --------------------- | ----- |
| `ascii`  | ASCII  | ASCII zones (`0x3?` / `0x7?`) | Transparent 8-bit path; no translation table required. |
| `cp037`  | EBCDIC | EBCDIC zones (`0xF?` / `0xD?`) | U.S./Canada baseline; soak SLO target for throughput. |
| `cp273`  | EBCDIC | EBCDIC zones (`0xF?` / `0xD?`) | German variant with umlaut support (AE/OE/UE). |
| `cp500`  | EBCDIC | EBCDIC zones (`0xF?` / `0xD?`) | International set (Latin-1 superset of CP037). |
| `cp1047` | EBCDIC | EBCDIC zones (`0xF?` / `0xD?`) | Open Systems variant; matches z/OS USS defaults. |
| `cp1140` | EBCDIC | EBCDIC zones (`0xF?` / `0xD?`) | Euro-enabled CP037 successor (0x9F maps to EUR sign). |

\*Zoned digit defaults apply when `preferred_zoned_encoding` is left at `Auto`. ASCII jobs
emit `0x3`/`0x7` sign zones; EBCDIC jobs use `0xF`/`0xD`.

### Preserving and Overriding Zoned Formats

- `DecodeOptions::preserve_zoned_encoding = true` captures the original zoned digit family
  (ASCII vs. EBCDIC) in the metadata. Subsequent encodes reuse the preserved format.
- `EncodeOptions::preferred_zoned_encoding = Auto` selects ASCII zones when `codepage == Ascii`
  and EBCDIC zones otherwise. Override with `Ascii` or `Ebcdic` to force a specific style.
- `EncodeOptions::zoned_encoding_override = Some(...)` wins over both preserved metadata
  and `preferred_zoned_encoding`. This is how the soak matrix exercises
  `override-ascii` / `override-ebcdic` scenarios regardless of the decoded input.

### Choosing Codepages at Runtime

Both `DecodeOptions` and `EncodeOptions` accept a `Codepage` value. Higher-level callers
should validate user-supplied strings before reaching the codec layer. The CLI, for example,
maps command-line `--codepage` arguments into the enum and surfaces a validation error if the
value is not recognised. Library consumers should adopt the same pattern to guarantee that
unsupported codepages never reach the decoding core.

## JSON Envelope & Raw Data

Decoded records are wrapped in a stable JSON envelope:

```json
{
  "schema": "copybook.v1",
  "record_index": 0,
  "codepage": "cp037",
  "fields": { "FIELD1": "value" }
}
```

- `schema` – Versioned schema identifier (`copybook.v1`)
- `record_index` – Zero-based record sequence number
- `codepage` – Decoder code page (e.g., `cp037`)
- `fields` – Map of decoded field values (nested for groups)
- `schema_fingerprint`, `offset`, `length` – Added when `emit_meta` is enabled

When `emit_raw` is enabled, record-level payloads are emitted as **`raw_b64`** (with the legacy
`__raw_b64` still present for backwards compatibility). Field-level capture uses the
`<field>_raw_b64` naming pattern:

```rust
// RawMode::Record - capture record payload only
let opts = DecodeOptions::new().with_emit_raw(RawMode::Record);
// JSON excerpt: { "raw_b64": "AAABBBCCC..." }

// RawMode::RecordRdw - capture payload + 4-byte RDW header
let opts = DecodeOptions::new().with_emit_raw(RawMode::RecordRdw);
// JSON excerpt: { "raw_b64": "AAAAAAhBBBCCC..." }

// RawMode::Field - capture individual field payloads
let opts = DecodeOptions::new().with_emit_raw(RawMode::Field);
// JSON excerpt: { "fields": { "FIELD1": "decoded", "FIELD1_raw_b64": "AAA..." } }
```

**Roundtrip Encoding**:
When `use_raw` is enabled in `EncodeOptions`, the encoder consumes `raw_b64` (or the legacy
`__raw_b64`) from the JSON input to produce **bit-exact** output:

```rust
// Decode with raw preservation
let decode_opts = DecodeOptions::new()
    .with_emit_raw(RawMode::RecordRdw);
let json_value = decode_record(&schema, &original_data, &decode_opts)?;

// Encode using raw data (preserves reserved bytes, avoids recomputation)
let encode_opts = EncodeOptions::new()
    .with_use_raw(true);
let encoded_data = encode_record(&schema, &json_value, &encode_opts)?;

// Verify bit-exact roundtrip
assert_eq!(original_data, encoded_data);
```

**RDW-Specific Considerations**:
- **Reserved Bytes**: `RawMode::RecordRdw` preserves bytes 2-3 of RDW header (reserved, typically zero)
- **Length Recomputation**: When `use_raw=false`, encoder recomputes RDW length from payload size
- **Truncation Detection**: Fixed-format records validate expected length against actual data
- **Error Codes**:
  - `CBKR211_RDW_RESERVED_NONZERO` - Non-zero reserved bytes warning (lenient mode)
  - `CBKR311_RDW_UNDERFLOW` - Incomplete RDW header or payload
  - `CBKE501_JSON_TYPE_MISMATCH` - Invalid base64 in `raw_b64` / `__raw_b64`

## Core Functions

### Parsing with Enterprise Safety

```rust
pub fn parse_copybook(text: &str) -> Result<Schema, Error>
```

Parse a COBOL copybook into a schema with **panic-safe operations**.

**Enterprise Safety Features:**
- **Zero panic risk** - All operations use structured error handling
- **Bounds checking** - Safe array and slice access throughout
- **Overflow protection** - Integer conversions with overflow detection
- **Memory safety** - Zero unsafe code for production reliability

**Parameters:**
- `text` - COBOL copybook source text

**Returns:**
- `Ok(Schema)` - Parsed schema with validated structure
- `Err(Error)` - Parse error with detailed context and suggestions

**Example:**
```rust
use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions};

let copybook = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID    PIC 9(8).
   05 CUSTOMER-NAME  PIC X(30).
   05 BALANCE        PIC S9(7)V99 COMP-3.
   05 STATUS-CODE    PIC X(1).
       88 ACTIVE     VALUE 'A'.
       88 INACTIVE   VALUE 'I'.
"#;

// Basic parsing with enterprise safety
let schema = parse_copybook(copybook)?;
println!("Parsed {} fields with panic-safe operations", schema.fields.len());

// Advanced parsing with custom options
let parse_options = ParseOptions {
    allow_inline_comments: false, // Disable COBOL-2002 inline comments (*>)
    max_field_depth: 20,         // Prevent stack overflow from deep nesting
    ..ParseOptions::default()
};

let schema_custom = parse_copybook_with_options(copybook, &parse_options)?;
```

### Enhanced Safe Operations Module

The copybook-core crate provides comprehensive panic-safe operations in the `utils::safe_ops` module:

```rust
use copybook_core::utils::safe_ops;

// Safe integer conversions with overflow checking
let safe_u32 = safe_ops::safe_u64_to_u32(large_value, "field offset calculation")?;
let safe_u16 = safe_ops::safe_u64_to_u16(value, "sync padding calculation")?;
let safe_u32_from_usize = safe_ops::safe_usize_to_u32(array_len, "record length")?;

// Safe string and slice operations
let parsed_number = safe_ops::safe_parse_u16("123", "PIC clause parsing")?;
let char_at_index = safe_ops::safe_string_char_at(&pic_string, index, "PIC character access")?;
let token = safe_ops::safe_slice_get(&tokens, index, "parser token access")?;

// Safe arithmetic operations
let divided_result = safe_ops::safe_divide(numerator, denominator, "field size calculation")?;
let array_bound = safe_ops::safe_array_bound(base_offset, count, item_size, "ODO array sizing")?;

// Safe formatting operations for JSON generation
let mut json_buffer = String::new();
safe_ops::safe_write(&mut json_buffer, format_args!("{{\"field\": \"{}\"}}", value))?;
safe_ops::safe_write_str(&mut json_buffer, ",\n")?;
```

**Key Safety Features:**
- **Panic elimination** - All `.unwrap()` and `.expect()` calls replaced with structured error handling
- **Context-aware errors** - Every operation includes descriptive context for debugging
- **Performance preservation** - <5% overhead while maintaining enterprise throughput targets
- **Hardware optimization** - Uses CPU overflow detection for maximum performance

### Enhanced High-Performance Codec Operations

copybook-rs provides enterprise-grade encoding/decoding with comprehensive panic-safe operations:

```rust
use copybook_codec::{decode_record_with_scratch, memory::ScratchBuffers};

// High-performance decoding with scratch buffer optimization
let mut scratch = ScratchBuffers::new();
let json_value = decode_record_with_scratch(&schema, &record_data, &options, &mut scratch)?;

// Panic-safe iteration over large files
use copybook_codec::iter_records_from_file;
let iterator = iter_records_from_file("data.bin", &schema, &options)?;

for (record_num, record_result) in iterator.enumerate() {
    match record_result {
        Ok(json_value) => {
            // Process successful record
            println!("Record {}: processed", record_num + 1);
        }
        Err(decode_error) => {
            // Handle individual record errors without stopping batch
            tracing::warn!(
                record_number = %(record_num + 1),
                error = %decode_error,
                "Record decode failed - continuing with next record"
            );
        }
    }
}
```

**Enterprise Performance Features:**
- **Scratch buffer optimization** - Reusable memory buffers for hot paths
- **Bounded memory usage** - <256 MiB steady-state for multi-GB files
- **Panic-safe iteration** - Graceful handling of individual record failures
- **Zero-copy operations** - Minimal memory movement during processing
- **Streaming processing** - Process files larger than available memory

### File-Level Decoding

```rust
pub fn decode_file_to_jsonl<W: Write>(
    schema: &Schema,
    data_path: &Path,
    opts: &DecodeOptions,
    output: W,
) -> Result<RunSummary, Error>
```

Decode an entire file to JSONL format with **enterprise reliability**.

**Parameters:**
- `schema` - Parsed copybook schema
- `data_path` - Path to binary data file
- `opts` - Decode configuration options
- `output` - Writer for JSONL output

**Returns:**
- `Ok(RunSummary)` - Processing statistics
- `Err(Error)` - Processing error

**Example:**
```rust
let opts = DecodeOptions {
    codepage: Codepage::Cp037,
    format: RecordFormat::Fixed,
    emit_meta: true,
    preserve_zoned_encoding: true, // Enable encoding preservation
    preferred_zoned_encoding: ZonedEncodingFormat::Ebcdic, // Fallback format
    ..Default::default()
};

let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(&schema, Path::new("data.bin"), &opts, output)?;

println!("Processed {} records with {} errors",
         summary.records_processed, summary.records_with_errors);
```

### Telemetry & Metrics (opt-in)

Enable the `metrics` cargo feature on `copybook-codec` when you want the decoder to emit counters and gauges that can be scraped by any [`metrics`](https://crates.io/crates/metrics) compatible recorder:

```toml
[dependencies]
copybook-codec = { version = "0.3", features = ["metrics"] }
```

The CLI forwards the same feature:

```bash
cargo install copybook-cli --features metrics
# or inside the workspace
cargo run -p copybook-cli --features metrics -- decode ...
```

Once enabled, every `decode_file_to_jsonl` invocation updates the following series:

- `copybook_records_total{format,codepage,zero_policy}` (counter; +1 per decoded record)
- `copybook_bytes_total{format,codepage,zero_policy}` (counter; +N per record payload)
- `copybook_decode_errors_total{family}` (counter; tagged with the `CBK*` family prefix)
- `copybook_decode_seconds{format,codepage}` (histogram; file-level runtime in seconds)
- `copybook_throughput_mibps{format,codepage}` (gauge; last-run MiB/s throughput)

Labels stay bounded: `format`, `codepage`, and `zero_policy` (plus `family` for errors). The CLI keeps the feature off by default and only binds the HTTP exporter when invoked with `--metrics-listen <addr>`, so builds without the feature or runs without the flag behave identically to previous releases.

When built with the `metrics` feature and a global recorder is installed, the codec emits low-cardinality counters and a per-file timing histogram. When the feature is disabled or no recorder is installed, metrics calls are no-ops and introduce no overhead.

Even without the feature, the library emits an `INFO` log with target `copybook::decode` summarising each run’s totals and options. Hook it up to `tracing-subscriber` or your existing logging pipeline to capture receipts alongside the metrics stream.

### File-Level Encoding

```rust
pub fn encode_jsonl_to_file(
    schema: &Schema,
    jsonl_path: &Path,
    opts: &EncodeOptions,
    out_path: &Path,
) -> Result<RunSummary, Error>
```

Encode JSONL data to binary format.

**Parameters:**
- `schema` - Parsed copybook schema
- `jsonl_path` - Path to JSONL input file
- `opts` - Encode configuration options
- `out_path` - Path for binary output file

**Returns:**
- `Ok(RunSummary)` - Processing statistics
- `Err(Error)` - Processing error

**Example:**
```rust
let opts = EncodeOptions {
    codepage: Codepage::Cp037,
    format: RecordFormat::Fixed,
    use_raw: true,
    zoned_encoding_override: None, // Respect preserved formats
    ..Default::default()
};

// Or with explicit format override:
let opts_override = EncodeOptions {
    codepage: Codepage::Cp037,
    format: RecordFormat::Fixed,
    zoned_encoding_override: Some(ZonedEncodingFormat::Ascii), // Force ASCII zones
    ..Default::default()
};

let summary = encode_jsonl_to_file(
    &schema,
    Path::new("input.jsonl"),
    &opts,
    Path::new("output.bin"),
)?;
```

### Record-Level Processing

```rust
pub struct RecordDecoder {
    // Internal fields
}

impl RecordDecoder {
    pub fn new(schema: &Schema, opts: &DecodeOptions) -> Result<Self, Error>;
    
    pub fn decode_record(&mut self, data: &[u8]) -> Result<serde_json::Value, Error>;
    
    pub fn decode_file<P: AsRef<Path>>(&mut self, path: P) -> Result<RecordIterator, Error>;
}

// Enhanced RecordIterator with truncated record detection
pub struct RecordIterator<R: Read> {
    // Internal fields
}

impl<R: Read> RecordIterator<R> {
    /// Create new RecordIterator with enhanced validation
    /// 
    /// For fixed-format processing, requires schema.lrecl_fixed to be set
    /// for proper truncated record detection.
    pub fn new(reader: R, schema: &Schema, options: &DecodeOptions) -> Result<Self>;
}

impl<R: Read> Iterator for RecordIterator<R> {
    type Item = Result<serde_json::Value, Error>;
    
    fn next(&mut self) -> Option<Self::Item>;
}
```

### JSON Writer with Schema Access

```rust
pub struct JsonWriter<W: Write> {
    // Internal fields
}

impl<W: Write> JsonWriter<W> {
    /// Create a new JSON writer with schema access for field path resolution
    pub fn new(writer: W, schema: Schema, options: DecodeOptions) -> Self;
    
    /// Write a single record as JSON line with schema-based field processing
    pub fn write_record(
        &mut self,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()>;
    
    /// Write a record using optimized streaming approach with direct schema access
    pub fn write_record_streaming(
        &mut self,
        record_data: &[u8],
        record_index: u64,
        byte_offset: u64,
    ) -> Result<()>;
    
    /// Finish writing and return the inner writer
    pub fn finish(self) -> Result<W>;
}
```

**Key Features:**
- **Schema Integration**: Direct access to schema structure for field path resolution
- **REDEFINES Handling**: Enhanced cluster processing with proper size calculation using schema field lookup
- **ODO Processing**: Improved counter field lookup using schema-based field path resolution
- **Metadata Integration**: Automatic schema fingerprint inclusion in JSON output
- **Streaming Performance**: Optimized JSON generation with schema-aware field ordering
- **LRECL Requirement**: Fixed-format processing requires `schema.lrecl_fixed` for truncation detection
- **Fail-Fast Validation**: Constructor validates LRECL availability early
- **Enhanced Error Messages**: CBKD301_RECORD_TOO_SHORT with precise byte counts
- **Performance Optimized**: 4-23% performance improvements with validation

**Example:**
```rust
use copybook_codec::{JsonWriter, DecodeOptions};
use std::io::Cursor;

// Create JsonWriter with schema access
let output_buffer = Vec::new();
let cursor = Cursor::new(output_buffer);
let mut json_writer = JsonWriter::new(cursor, schema.clone(), opts);

// Write records with automatic schema-based field processing
let record_data = &[0x01, 0x02, 0x03, /* ... */];
json_writer.write_record(record_data, 0, 0)?;

// Alternative streaming approach for better performance
json_writer.write_record_streaming(record_data, 1, record_size as u64)?;

// Finish and retrieve output
let cursor = json_writer.finish()?;
let json_output = cursor.into_inner();
```

```rust
let mut decoder = RecordDecoder::new(&schema, &opts)?;

// Decode single record
let record_data = &[0x01, 0x02, 0x03, /* ... */];
let json_value = decoder.decode_record(record_data)?;

// Enhanced iterator with truncation detection
let file = std::fs::File::open("data.bin")?;
let mut iter = RecordIterator::new(file, &schema, &opts)?;

for record_result in iter {
    match record_result {
        Ok(json_value) => println!("{}", serde_json::to_string(&json_value)?),
        Err(e) => {
            // Enhanced error messages for truncated records:
            // "Record 15 too short: expected 120 bytes, got 85 bytes"
            eprintln!("Record error: {}", e);
        }
    }
}
```

```rust
pub struct RecordEncoder {
    // Internal fields
}

impl RecordEncoder {
    pub fn new(schema: &Schema, opts: &EncodeOptions) -> Result<Self, Error>;
    
    pub fn encode_record(&mut self, json: &serde_json::Value) -> Result<Vec<u8>, Error>;
    
    pub fn encode_jsonl_file<P: AsRef<Path>, W: Write>(
        &mut self, 
        jsonl_path: P, 
        output: W
    ) -> Result<RunSummary, Error>;
}
```

## Error Handling with Panic Safety

### Error Type

```rust
pub struct Error {
    pub code: ErrorCode,
    pub message: String,
    pub context: ErrorContext,
}

pub enum ErrorCode {
    // Parse errors (CBKP*)
    CBKP001_SYNTAX,                    // Syntax errors in copybook
    CBKP021_ODO_NOT_TAIL,             // ODO array positioning issues
    CBKP051_UNSUPPORTED_EDITED_PIC,   // Unsupported PIC editing

    // Schema validation errors (CBKS*)
    CBKS121_COUNTER_NOT_FOUND,        // ODO counter field missing
    CBKS141_RECORD_TOO_LARGE,         // Record size exceeds limits
    CBKS301_ODO_CLIPPED,              // ODO bounds enforcement
    CBKS302_ODO_RAISED,               // ODO minimum value validation

    // Data processing errors (CBKD*)
    CBKD101_INVALID_FIELD_TYPE,       // Type mismatch in data
    CBKD201_TRUNCATED_RECORD,         // Record shorter than expected

    // Encoding errors (CBKE*)
    CBKE501_JSON_TYPE_MISMATCH,       // JSON encoding type issues
    // ... other error codes
}

pub struct ErrorContext {
    pub record_index: Option<u64>,
    pub field_path: Option<String>,
    pub byte_offset: Option<u64>,
    pub line_number: Option<u32>,
    pub operation_context: Option<String>,  // Enhanced: Operation being performed
    pub safety_context: Option<String>,     // Enhanced: Safety-related context
    pub additional: HashMap<String, String>,
}
```

**Enhanced Error Context:**
- **Operation context** - Specific operation that failed (e.g., "field offset calculation")
- **Safety context** - Information about panic-safe operation that was used
- **Detailed diagnostics** - Comprehensive information for debugging production issues

### Panic-Safe Error Handling Patterns

```rust
use copybook_core::{parse_copybook, Error, ErrorCode};
use copybook_core::utils::{OptionExt, VecExt};

// Enhanced error handling with panic safety
match parse_copybook(text) {
    Ok(schema) => {
        tracing::info!(
            fields = %schema.fields.len(),
            fixed_length = ?schema.fixed_record_length,
            "Schema parsed successfully with panic-safe operations"
        );
    },
    Err(e) => match e.code {
        ErrorCode::CBKP001_SYNTAX => {
            tracing::error!(
                error_code = ?e.code,
                message = %e.message,
                context = ?e.context,
                "Copybook syntax error - check field definitions and level numbers"
            );
            // Provide specific suggestions based on error context
        },
        ErrorCode::CBKP021_ODO_NOT_TAIL => {
            tracing::error!(
                error_code = ?e.code,
                message = %e.message,
                suggestion = "Move ODO array to end of record structure",
                "ODO positioning error detected"
            );
        },
        ErrorCode::CBKS141_RECORD_TOO_LARGE => {
            tracing::error!(
                error_code = ?e.code,
                message = %e.message,
                max_size = "16 MiB",
                "Record size exceeds enterprise limits"
            );
        },
        _ => {
            tracing::error!(
                error_code = ?e.code,
                message = %e.message,
                context = ?e.context,
                "Unexpected parsing error"
            );
            return Err(e);
        }
    }
}

// Using panic-safe extension traits
use copybook_core::utils::{OptionExt, VecExt, SliceExt};

// Safe option unwrapping with structured errors
let field = schema.fields
    .first()
    .ok_or_cbkp_error(
        ErrorCode::CBKP001_SYNTAX,
        "Schema must contain at least one field"
    )?;

// Safe vector operations
let mut field_stack = Vec::new();
field_stack.push(field);
let current_field = field_stack
    .pop_or_cbkp_error(
        ErrorCode::CBKP001_SYNTAX,
        "Field stack underflow during parsing"
    )?;

// Safe slice access
let token = tokens
    .get_or_cbkp_error(
        token_index,
        ErrorCode::CBKP001_SYNTAX,
        format!("Token index {} out of bounds", token_index)
    )?;

// Collect errors during processing
let mut errors = Vec::new();
let opts = DecodeOptions {
    strict: false,
    max_errors: Some(100),
    ..Default::default()
};

match decode_file_to_jsonl(&schema, path, &opts, output) {
    Ok(summary) => {
        if summary.error_count > 0 {
            println!("Completed with {} errors", summary.error_count);
        }
    },
    Err(e) => {
        eprintln!("Fatal error: {}", e);
    }
}
```

## RunSummary

```rust
pub struct RunSummary {
    pub records_processed: u64,
    pub records_skipped: u64,
    pub error_count: u64,
    pub warning_count: u64,
    pub bytes_processed: u64,
    pub duration: Duration,
    pub throughput_mbps: f64,
}
```

## Advanced Usage

### Enhanced REDEFINES Cluster Handling

The JsonWriter now provides enhanced REDEFINES processing with proper cluster size calculation using direct schema access:

```rust
// Example schema with REDEFINES cluster
let copybook = r#"
01 RECORD.
   05 DATA-FIELD      PIC X(10).
   05 FIELD-A         REDEFINES DATA-FIELD PIC X(5).
   05 FIELD-B         REDEFINES DATA-FIELD PIC 9(8) COMP-3.
   05 FIELD-C         REDEFINES DATA-FIELD PIC X(15).
"#;

// Schema-based cluster size calculation
impl JsonEncoder {
    /// Calculate cluster size accounting for all redefining fields
    fn calculate_redefines_cluster_size(&self, field: &Field) -> Result<usize> {
        let cluster_path = self.get_redefines_cluster_path(field);
        let mut max_size = field.len as usize;
        
        // Find all fields in the REDEFINES cluster using schema lookup
        for check_field in &self.schema.fields {
            if let Some(ref check_redefines) = check_field.redefines_of {
                if *check_redefines == cluster_path || check_field.path == cluster_path {
                    max_size = max_size.max(check_field.len as usize);
                }
            } else if check_field.path == cluster_path {
                max_size = max_size.max(check_field.len as usize);
            }
        }
        
        Ok(max_size)
    }
}

// Enhanced view resolution using schema.find_redefining_fields()
let redefining_fields = self.schema.find_redefining_fields(&primary_field.path);
for redefining_field in redefining_fields {
    // Process each redefining field with proper type checking
    if let Some(value) = json_obj.get(&redefining_field.name) {
        cluster_views.push((redefining_field, value));
    }
}
```

**Key Improvements:**
- **Accurate Cluster Sizing**: Uses schema traversal to find maximum size across all redefining fields
- **Complete Field Discovery**: `find_redefining_fields()` method finds all fields that redefine a target
- **Type-Safe Processing**: Schema access ensures proper field type and offset information
- **Memory Safety**: Bounds checking uses actual calculated cluster size, not field-specific sizes

### Improved ODO Counter Processing with Schema-Based Field Lookup

The JsonWriter provides enhanced ODO (OCCURS DEPENDING ON) processing using direct schema field lookup:

```rust
// Example schema with ODO arrays
let copybook = r#"
01 RECORD.
   05 ITEM-COUNT     PIC 9(3) COMP-3.
   05 ITEMS          OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-ID     PIC 9(8).
      10 ITEM-NAME   PIC X(20).
"#;

// Schema-based ODO counter field lookup
impl JsonEncoder {
    /// Update ODO counter using schema field lookup
    fn update_odo_counter(
        &self,
        counter_path: &str,
        count: u32,
        _json_obj: &Map<String, Value>,
        record_data: &mut [u8],
    ) -> Result<()> {
        // Find counter field using schema.find_field() method
        let counter_field = self
            .schema
            .find_field(counter_path)
            .ok_or_else(|| {
                Error::new(
                    ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                    format!("Field not found: {}", counter_path),
                )
            })?;
            
        // Encode count value with proper type handling
        match &counter_field.kind {
            FieldKind::ZonedDecimal { digits, scale, signed } => {
                let encoded = crate::numeric::encode_zoned_decimal(
                    &count_str, *digits, *scale, *signed, self.options.codepage
                )?;
                // Write to counter field at correct offset
                let end_offset = (counter_field.offset + counter_field.len) as usize;
                record_data[counter_field.offset as usize..end_offset]
                    .copy_from_slice(&encoded);
            }
            FieldKind::PackedDecimal { digits, scale, signed } => {
                let encoded = crate::numeric::encode_packed_decimal(
                    &count_str, *digits, *scale, *signed
                )?;
                // Write with proper offset and bounds checking
            }
            FieldKind::BinaryInt { bits, signed } => {
                let encoded = crate::numeric::encode_binary_int(
                    i64::from(count), *bits, *signed
                )?;
                // Write with schema-provided offset information
            }
            _ => {
                return Err(Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Invalid counter field type for ODO")
                ));
            }
        }
        
        Ok(())
    }
}
```

**Key Improvements:**
- **Schema Field Lookup**: Uses `schema.find_field(counter_path)` for reliable counter field resolution
- **Type-Safe Counter Encoding**: Proper handling of zoned decimal, packed decimal, and binary integer counters
- **Accurate Offset Calculation**: Uses schema-provided field offset and length information
- **Comprehensive Error Handling**: Clear error messages for missing or invalid counter fields
- **Memory Safety**: Proper bounds checking using schema field dimensions

**ODO Array Processing:**
```rust
// Enhanced array count resolution
fn get_actual_array_count(&self, occurs: &Occurs, record_data: &[u8]) -> Result<u32> {
    match occurs {
        Occurs::Fixed { count } => Ok(*count),
        Occurs::ODO { min, max, counter_path } => {
            // Schema-based counter field lookup (when fully implemented)
            let counter_field = self.schema.find_field(counter_path)?;
            let counter_value = self.read_counter_value(counter_field, record_data)?;
            
            // Validate against ODO bounds
            if counter_value < *min || counter_value > *max {
                return Err(/* ODO bounds error */);
            }
            
            Ok(counter_value)
        }
    }
}
```

### Custom Field Processing

```rust
// Process specific field types
for field in &schema.fields {
    match &field.kind {
        FieldKind::PackedDecimal { digits, scale, signed } => {
            println!("COMP-3 field: {} digits, scale {}", digits, scale);
        },
        FieldKind::ZonedDecimal { digits, scale, signed } => {
            println!("Zoned field: {} digits, scale {}", digits, scale);
        },
        _ => {}
    }
}
```

### Schema Validation

```rust
// Validate schema before use
schema.validate()?;

// Check for specific features
if schema.tail_odo.is_some() {
    println!("Schema has ODO array");
}

if schema.fields.iter().any(|f| f.redefines_of.is_some()) {
    println!("Schema has REDEFINES");
}
```

### Parallel Processing

```rust
use std::sync::Arc;
use std::thread;

let schema = Arc::new(schema);
let opts = Arc::new(opts);

let handles: Vec<_> = (0..num_threads).map(|i| {
    let schema = Arc::clone(&schema);
    let opts = Arc::clone(&opts);
    
    thread::spawn(move || {
        let mut decoder = RecordDecoder::new(&schema, &opts)?;
        // Process chunk of data
        Ok(())
    })
}).collect();

for handle in handles {
    handle.join().unwrap()?;
}
```

### Custom Codepage Support

```rust
// Extend with custom codepage (requires feature flag)
#[cfg(feature = "custom-codepage")]
use copybook_codec::CustomCodepage;

let custom_cp = CustomCodepage::from_table(&conversion_table)?;
let opts = DecodeOptions {
    codepage: Codepage::Custom(custom_cp),
    ..Default::default()
};
```

## Integration Examples

### Serde Integration

```rust
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
struct Customer {
    customer_id: String,
    customer_name: String,
    balance: String, // Decimal as string for precision
}

// Decode to typed struct
let json_value = decoder.decode_record(data)?;
let customer: Customer = serde_json::from_value(json_value)?;

// Encode from typed struct
let json_value = serde_json::to_value(&customer)?;
let binary_data = encoder.encode_record(&json_value)?;
```

### Tokio Integration

```rust
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncWriteExt};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file = File::open("data.bin").await?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).await?;
    
    // Process with copybook-rs
    let json_value = decoder.decode_record(&buffer)?;
    
    let mut output = File::create("output.jsonl").await?;
    output.write_all(serde_json::to_string(&json_value)?.as_bytes()).await?;
    
    Ok(())
}
```

### Streaming with Channels

```rust
use crossbeam_channel::{bounded, Receiver, Sender};
use std::thread;

fn streaming_decode(
    schema: Schema,
    opts: DecodeOptions,
) -> (Sender<Vec<u8>>, Receiver<serde_json::Value>) {
    let (input_tx, input_rx) = bounded(100);
    let (output_tx, output_rx) = bounded(100);
    
    thread::spawn(move || {
        let mut decoder = RecordDecoder::new(&schema, &opts).unwrap();
        
        while let Ok(data) = input_rx.recv() {
            match decoder.decode_record(&data) {
                Ok(json) => output_tx.send(json).unwrap(),
                Err(e) => eprintln!("Decode error: {}", e),
            }
        }
    });
    
    (input_tx, output_rx)
}
```

## Performance Considerations

### Memory Management

```rust
// Reuse buffers for better performance
let mut buffer = Vec::with_capacity(1024);
let mut decoder = RecordDecoder::new(&schema, &opts)?;

for record_data in record_iterator {
    buffer.clear();
    buffer.extend_from_slice(record_data);
    
    let json_value = decoder.decode_record(&buffer)?;
    // Process json_value
}
```

### Batch Processing

```rust
// Process records in batches
const BATCH_SIZE: usize = 1000;
let mut batch = Vec::with_capacity(BATCH_SIZE);

for record_result in decoder.decode_file("data.bin")? {
    batch.push(record_result?);
    
    if batch.len() >= BATCH_SIZE {
        process_batch(&batch)?;
        batch.clear();
    }
}

if !batch.is_empty() {
    process_batch(&batch)?;
}
```

## Testing

### Unit Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_simple_decode() {
        let copybook = r#"
        01 TEST-RECORD.
           05 ID    PIC 9(4).
           05 NAME  PIC X(10).
        "#;
        
        let schema = parse_copybook(copybook).unwrap();
        let opts = DecodeOptions::default().with_emit_meta(true);
        let mut decoder = RecordDecoder::new(&schema, &opts).unwrap();

        let data = b"1234JOHN      ";
        let json = decoder.decode_record(data).unwrap();

        assert_eq!(json["ID"], "1234");
        assert_eq!(json["NAME"], "JOHN      ");
        assert_eq!(json["__schema_id"], schema.fingerprint);
    }
}
```

### Property Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_roundtrip_fidelity(
        id in 0u32..9999,
        name in "[A-Z ]{10}",
        balance in -999999i32..999999
    ) {
        let json = json!({
            "ID": format!("{:04}", id),
            "NAME": name,
            "BALANCE": format!("{:08}", balance)
        });
        
        let binary = encoder.encode_record(&json)?;
        let decoded = decoder.decode_record(&binary)?;
        
        prop_assert_eq!(json, decoded);
    }
}
```

## Zoned Decimal Encoding Preservation

copybook-rs provides comprehensive support for preserving zoned decimal encoding formats during decode/encode cycles, enabling **binary round-trip fidelity** essential for enterprise mainframe data processing.

### Core API

```rust
use copybook_codec::{DecodeOptions, EncodeOptions, ZonedEncodingFormat};

// Configure encoding preservation during decode
let decode_opts = DecodeOptions::new()
    .with_preserve_zoned_encoding(true)
    .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic);

// Configure encoding format during encode
let encode_opts = EncodeOptions::new()
    .with_zoned_encoding_override(None); // Respect preserved formats
```

### ZonedEncodingFormat API

```rust
impl ZonedEncodingFormat {
    /// Check encoding format types
    pub const fn is_ascii(self) -> bool;
    pub const fn is_ebcdic(self) -> bool;
    pub const fn is_auto(self) -> bool;

    /// Get human-readable description
    pub const fn description(self) -> &'static str;

    /// Detect encoding from byte data
    pub fn detect_from_byte(byte: u8) -> Option<Self>;
}
```

### Builder Pattern Configuration

```rust
// Decode with encoding preservation
let decode_opts = DecodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_format(RecordFormat::Fixed)
    .with_preserve_zoned_encoding(true)
    .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic)
    .with_emit_meta(true);

// Encode with format override
let encode_opts = EncodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_format(RecordFormat::Fixed)
    .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ascii));
```

### Round-Trip Example

```rust
use copybook_core::parse_copybook;
use copybook_codec::{
    decode_record, encode_record, DecodeOptions, EncodeOptions,
    ZonedEncodingFormat, Codepage, RecordFormat
};

// Parse schema
let copybook = r#"
    01 CUSTOMER-RECORD.
       05 CUSTOMER-ID    PIC 9(8).
       05 ACCOUNT-BALANCE PIC S9(7)V9(2).
"#;
let schema = parse_copybook(copybook)?;

// Original binary data with ASCII zoned decimals
let original_data = &[
    0x30, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, // CUSTOMER-ID: ASCII zones
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, // ACCOUNT-BALANCE: ASCII zones
];

// Step 1: Decode with encoding preservation
let decode_opts = DecodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_format(RecordFormat::Fixed)
    .with_preserve_zoned_encoding(true)
    .with_emit_meta(true);

let json_value = decode_record(&schema, original_data, &decode_opts)?;

// JSON now contains encoding metadata:
// {
//   "CUSTOMER_ID": "12345678",
//   "ACCOUNT_BALANCE": "1234567.89",
//   "_encoding_metadata": {
//     "CUSTOMER_ID": {"zoned_encoding": "ascii", "detection_confidence": 1.0},
//     "ACCOUNT_BALANCE": {"zoned_encoding": "ascii", "detection_confidence": 1.0}
//   }
// }

// Step 2: Encode preserving original format
let encode_opts = EncodeOptions::new()
    .with_codepage(Codepage::Cp037)
    .with_format(RecordFormat::Fixed)
    .with_zoned_encoding_override(None); // Use preserved formats

let encoded_data = encode_record(&schema, &json_value, &encode_opts)?;

// Verify binary round-trip fidelity
assert_eq!(original_data, encoded_data.as_slice());
```

### Format Override Scenarios

```rust
// Force ASCII encoding for all zoned decimals
let ascii_opts = EncodeOptions::new()
    .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ascii));

// Force EBCDIC encoding for all zoned decimals
let ebcdic_opts = EncodeOptions::new()
    .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ebcdic));

// Respect preserved formats (default behavior)
let preserved_opts = EncodeOptions::new()
    .with_zoned_encoding_override(None);
```

### Error Handling

```rust
use copybook_core::{Error, ErrorCode};

match decode_record(&schema, &data, &opts) {
    Ok(json) => {
        // Process successful decode
        println!("Decoded: {}", json);
    }
    Err(e) => match e.code {
        ErrorCode::CBKD413_ZONED_INVALID_ENCODING => {
            eprintln!("Invalid zoned decimal encoding format");
        }
        ErrorCode::CBKD414_ZONED_MIXED_ENCODING => {
            eprintln!("Mixed ASCII/EBCDIC encoding in single field");
        }
        ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED => {
            eprintln!("Unable to reliably detect encoding format");
        }
        _ => {
            eprintln!("Other error: {}", e);
        }
    }
}
```

### Performance Considerations

Encoding preservation adds minimal overhead:

```rust
// Benchmark encoding detection performance
use std::time::Instant;

let start = Instant::now();
let json_value = decode_record(&schema, &data, &decode_opts)?;
let decode_time = start.elapsed();

println!("Decode with encoding preservation: {:?}", decode_time);
// Typical overhead: <1% of decode time
```

## Best Practices

1. **Error Handling**: Always handle errors appropriately for your use case
2. **Resource Management**: Reuse decoders/encoders for better performance
3. **Memory Usage**: Use streaming APIs for large files
4. **Validation**: Validate schemas before processing data
5. **Testing**: Include round-trip tests for critical data
6. **Logging**: Use structured logging for production deployments
7. **Configuration**: Use configuration files for complex setups

## Migration from Other Libraries

### From IBM COBOL Tools

```rust
// IBM-style field access
let customer_id = record.get_field("CUSTOMER-ID")?;

// copybook-rs equivalent
let json = decoder.decode_record(data)?;
let customer_id = json["CUSTOMER_ID"].as_str().unwrap();
```

### From Java COBOL Libraries

```rust
// Java-style configuration
// CobolDecoder decoder = new CobolDecoder(schema, "CP037", true);

// copybook-rs equivalent
let opts = DecodeOptions {
    codepage: Codepage::Cp037,
    strict: true,
    ..Default::default()
};
let decoder = RecordDecoder::new(&schema, &opts)?;
```

## API Stability

The copybook-rs library follows semantic versioning:

- **Major versions** (1.0, 2.0): Breaking API changes
- **Minor versions** (1.1, 1.2): New features, backward compatible
- **Patch versions** (1.1.1, 1.1.2): Bug fixes, no API changes

Current stability guarantees:
- Core parsing API is stable
- Configuration types may evolve in minor versions
- Error codes are stable within major versions
- Performance characteristics are not part of API stability
