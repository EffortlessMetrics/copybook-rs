<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Library API Reference

Complete reference for using `copybook` as a Rust library.

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

Add the canonical `copybook` facade crate to your `Cargo.toml`:

```toml
[dependencies]
copybook = "0.5"
```

`copybook-core` and `copybook-codec` remain available for advanced users who need a smaller dependency surface.
For direct character conversion, use `copybook::charset`; the older
`copybook::codepage` path is retained only as a deprecated migration alias.

### Record-format dispatch ownership

Record-format dispatch is owned by `copybook-codec`. New callers should use
`copybook::codec::record` or `copybook::codec::file::dispatch`; the
`copybook::record_io` facade and `copybook-record-io` package are compatibility
forwarders for the 0.5 surface.

The payload-oriented `read_record` helper is suitable for normal codec
operations. Use `read_rdw_record` when RDW header and reserved bytes must be
preserved losslessly.

Basic usage:

```rust
use copybook::core::parse_copybook;
use copybook::charset::Codepage;
use copybook::codec::{decode_file_to_jsonl, DecodeOptions, RecordFormat};

// Parse copybook
let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

// Configure decode options
let opts = DecodeOptions {
    codepage: Codepage::CP037,
    format: RecordFormat::Fixed,
    ..Default::default()
};

// Decode to JSONL
let input = std::fs::File::open("data.bin")?;
let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(&schema, input, output, &opts)?;
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
    /// Find a field by its full dot-separated path (recursive).
    pub fn find_field(&self, path: &str) -> Option<&Field>;
    /// Find a field by path, falling back to level-66 RENAMES alias lookup by name.
    pub fn find_field_or_alias(&self, name_or_path: &str) -> Option<&Field>;
    /// Resolve a RENAMES alias to its first storage-bearing target field.
    pub fn resolve_alias_to_target(&self, name_or_path: &str) -> Option<&Field>;
    /// Find all fields whose REDEFINES clause targets the given path.
    pub fn find_redefining_fields<'a>(&'a self, target_path: &str) -> Vec<&'a Field>;
}
```

### Field

```rust
pub struct Field {
    pub path: String,
    pub name: String,
    pub level: u8,
    pub kind: FieldKind,
    pub offset: u32,
    pub len: u32,
    pub redefines_of: Option<String>,
    pub occurs: Option<Occurs>,
    pub sync_padding: Option<u16>,
    pub synchronized: bool,
    pub blank_when_zero: bool,
    pub resolved_renames: Option<ResolvedRenames>,
    pub children: Vec<Field>,
}
```

**Fields:**
- `path` - Dot-separated field path (e.g., "ROOT.CUSTOMER.ID")
- `name` - Field name (last component of path)
- `level` - Level number from copybook (e.g., 01, 05, 66, 88)
- `kind` - Field type and characteristics
- `offset` - Byte offset within record
- `len` - Field length in bytes
- `redefines_of` - Path of redefined field (if applicable)
- `occurs` - Array information (if applicable)
- `sync_padding` - Alignment padding bytes (if SYNCHRONIZED) following IBM mainframe standards
- `synchronized` - Boolean flag indicating if field uses SYNCHRONIZED alignment
- `blank_when_zero` - Whether field has BLANK WHEN ZERO
- `resolved_renames` - Resolved RENAMES information (level-66 fields only)
- `children` - Child fields (for groups)

### FieldKind

```rust
pub enum FieldKind {
    Alphanum { len: u32 },
    ZonedDecimal {
        digits: u16,
        scale: i16,
        signed: bool,
        sign_separate: Option<SignSeparateInfo>,
    },
    BinaryInt { bits: u16, signed: bool },
    PackedDecimal { digits: u16, scale: i16, signed: bool },
    Group,
    /// Level-88 condition field (conditional values)
    Condition { values: Vec<String> },
    /// Level-66 RENAMES field (field aliasing/regrouping)
    Renames { from_field: String, thru_field: String },
    /// Edited numeric field (e.g., PIC ZZZ9, PIC $ZZ,ZZ9.99, PIC 9(7)V99CR)
    EditedNumeric {
        pic_string: String,
        width: u16,
        scale: u16,
        signed: bool,
    },
    /// Single-precision floating-point (COMP-1, IEEE 754 binary32, 4 bytes)
    FloatSingle,
    /// Double-precision floating-point (COMP-2, IEEE 754 binary64, 8 bytes)
    FloatDouble,
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
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub json_number_mode: JsonNumberMode,
    pub emit_filler: bool,
    pub emit_meta: bool,
    pub emit_raw: RawMode,
    pub strict_mode: bool,
    pub max_errors: Option<u64>,
    pub on_decode_unmappable: UnmappablePolicy,
    pub threads: usize,
    pub preserve_zoned_encoding: bool,
    pub preferred_zoned_encoding: ZonedEncodingFormat,
    pub float_format: FloatFormat,
}
```

**Zoned Encoding Configuration:**

- `preserve_zoned_encoding`: Enable detection and preservation of original zoned decimal encoding format (ASCII vs EBCDIC digit zones) for binary round-trip fidelity
- `preferred_zoned_encoding`: Fallback encoding format when auto-detection is ambiguous (all-zero fields, mixed encodings)

### EncodeOptions

```rust
pub struct EncodeOptions {
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub preferred_zoned_encoding: ZonedEncodingFormat,
    pub use_raw: bool,
    pub bwz_encode: bool,
    pub strict_mode: bool,
    pub max_errors: Option<u64>,
    pub threads: usize,
    pub coerce_numbers: bool,
    pub on_encode_unmappable: UnmappablePolicy,
    pub json_number_mode: JsonNumberMode,
    pub zoned_encoding_override: Option<ZonedEncodingFormat>,
    pub float_format: FloatFormat,
}
```

**Zoned Encoding Configuration:**

- `preferred_zoned_encoding`: Fallback encoding format when neither preserved metadata nor an explicit override selects a format. `Auto` resolves according to the active `codepage`.
- `zoned_encoding_override`: Explicit override for zoned decimal encoding format. When `Some(format)`, forces all zoned decimal fields to use the specified format, overriding any preserved format from decode operations. When `None`, respects preserved formats from decode metadata.

### Enums

```rust
pub enum Codepage {
    ASCII,    // ASCII (8-bit transparent)
    CP037,    // US/Canada EBCDIC
    CP273,    // Germany/Austria EBCDIC
    CP500,    // International EBCDIC
    CP1047,   // Open Systems EBCDIC
    CP1140,   // US/Canada Euro EBCDIC
}

pub enum RecordFormat {
    Fixed,    // Fixed-length records
    RDW,      // Variable-length with RDW header
}

pub enum JsonNumberMode {
    Lossless, // Decimals as strings, preserve precision
    Native,   // Use JSON numbers where possible
}

pub enum RawMode {
    Off,         // No raw capture
    Record,      // Capture entire record (payload only) as `__raw_b64` on the envelope
    Field,       // Capture individual fields as `<FIELD>__raw_b64`
    RecordRDW,   // Capture record + RDW header as `__raw_b64`
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

pub enum FloatFormat {
    IeeeBigEndian, // IEEE-754 big-endian binary format (default)
    IbmHex,        // IBM hexadecimal floating-point format
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
- `record_index` – One-based record sequence number for streaming JSONL decode (the first
  record is `1`). Direct library decode APIs preserve the caller-supplied index.
- `codepage` – Decoder code page (e.g., `cp037`)
- `fields` – Map of decoded field values (nested for groups)
- `schema_fingerprint`, `__schema_id`, `offset`, `length`, `__record_index`, `__length` – Added when
  `emit_meta` is enabled for streaming decode; `offset` is the zero-based physical source offset
  of the decoded record

For a scalar-target group `REDEFINES` nested under a level-01 record (that is, a group at level
greater than 1) without `OCCURS`, child fields are emitted as flattened views in the immediate
enclosing JSON map at the group's declaration position. The named group view is also retained and
emitted after that enclosing sibling scan completes. A level-01 redefining group follows root
traversal and emits its children without a named group wrapper. Scalar `REDEFINES` remain in
declaration order. A group `REDEFINES` with fixed `OCCURS` takes the array path instead and emits
the redefining group as a named array (without scalar-target flattening). Group-over-group
`REDEFINES` without `OCCURS` are omitted. This ordering and view shape are identical for
`decode_record` and `decode_record_with_scratch`.
When a flattened, nested, or reverse-order view collides with an existing field in the enclosing
JSON map, the later decoded value receives the deterministic `__dupN` suffix instead of replacing
the earlier value. This collision naming is identical for standard and scratch decoding; encoding
metadata and raw-sidecar identity are separate contracts.

When `emit_raw` is enabled, record-level payloads are emitted as **`raw_b64`** (with the legacy
`__raw_b64` key also present). The `raw_capture` marker records whether those bytes are payload-only
(`record`) or an RDW header plus payload (`record+rdw`). Field-level capture uses the
`<FIELD>__raw_b64` naming pattern:

```rust
// RawMode::Record - capture record payload only
let opts = DecodeOptions::new().with_emit_raw(RawMode::Record);
// JSON excerpt: { "raw_capture": "record", "raw_b64": "AAABBBCCC...", "__raw_b64": "AAABBBCCC..." }

// RawMode::RecordRDW - capture payload + 4-byte RDW header
let opts = DecodeOptions::new().with_emit_raw(RawMode::RecordRDW);
// JSON excerpt: { "raw_capture": "record+rdw", "raw_b64": "AAAAAAhBBBCCC...", "__raw_b64": "AAAAAAhBBBCCC..." }

// RawMode::Field - capture individual field payloads
let opts = DecodeOptions::new().with_emit_raw(RawMode::Field);
// JSON excerpt: { "fields": { "FIELD1": "decoded", "FIELD1__raw_b64": "AAA..." } }
```

For scalar `OCCURS` fields, the `<FIELD>_raw_b64` value is an array aligned with
the decoded field array. Duplicate emitted field names retain separate sidecar
arrays, such as `AMOUNT_raw_b64` and `AMOUNT__dup2_raw_b64`. Group-array
sidecar topology is not part of this contract.

**Roundtrip Encoding**:
When `use_raw` is enabled in `EncodeOptions`, the encoder consumes `raw_b64` (or the legacy
`__raw_b64`) from the JSON input. For RDW output, `raw_capture` routes payload-only and framed
bytes explicitly; marker-absent legacy input retains the historical header-plus-payload
interpretation:

```rust
// Decode with raw preservation
let decode_opts = DecodeOptions::new()
    .with_emit_raw(RawMode::RecordRDW);
let json_value = decode_record(&schema, &original_data, &decode_opts)?;

// Encode using raw data (unchanged valid RDW data replays byte-for-byte)
let encode_opts = EncodeOptions::new()
    .with_use_raw(true);
let encoded_data = encode_record(&schema, &json_value, &encode_opts)?;

// Verify bit-exact roundtrip
assert_eq!(original_data, encoded_data);
```

**RDW-Specific Considerations**:
- **Reserved Bytes**: `RawMode::RecordRDW` preserves bytes 2-3 of RDW header (reserved, typically zero)
- **Raw Capture Mode**: RDW `use_raw=true` wraps `raw_capture: "record"` bytes in a new header;
  `raw_capture: "record+rdw"` validates the supplied frame and preserves its reserved bytes.
  The marker selects framing, not immutability: changed fields rebuild the framed payload and
  length. Missing markers retain the legacy framed interpretation; provenance is never inferred
  from byte length or contents.
- **Length Recomputation**: When fields change under `use_raw=true`, the encoder recomputes the
  RDW payload length while preserving reserved bytes. Unchanged valid raw RDW data is replayed
  byte-for-byte. With `use_raw=false`, the encoder constructs a new RDW header from the payload.
- **Framing Bounds**: Raw RDW values shorter than the 4-byte header, frames whose declared payload
  length disagrees with the bytes present, and mutated payloads larger than 65,535 bytes fail with
  `CBKF102_RECORD_LENGTH_INVALID`
- **Truncation Detection**: Fixed-format records validate expected length against actual data
- **Error Codes**:
  - `CBKR201_RDW_READ_ERROR` - Non-EOF I/O failure while reading an RDW header
    or its declared payload
  - `CBKR202_RDW_WRITE_ERROR` - RDW header, payload, or flush write failure
  - `CBKR211_RDW_RESERVED_NONZERO` - Non-zero reserved bytes warning (lenient mode)
  - `CBKF221_RDW_UNDERFLOW` - EOF before an RDW header or declared payload is complete
  - `CBKE501_JSON_TYPE_MISMATCH` - Invalid base64 in `raw_b64` / `__raw_b64`
    or invalid/conflicting `raw_capture`

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
    emit_filler: true,            // Emit FILLER fields in the parsed schema
    allow_inline_comments: false, // Disable COBOL-2002 inline comments (*>)
    strict: true,                 // Strict mode with less error tolerance
    ..ParseOptions::default()
};

let schema_custom = parse_copybook_with_options(copybook, &parse_options)?;
```

**ParseOptions fields:**
- `emit_filler: bool` - Whether to emit FILLER fields in the parsed schema output
- `codepage: String` - Codepage identifier used for fingerprint calculation (e.g., `"cp037"`)
- `allow_inline_comments: bool` - Whether to allow COBOL-2002 inline comments (`*>`)
- `strict: bool` - Whether to run in strict mode with less error tolerance
- `strict_comments: bool` - Whether to enforce strict comment parsing rules
- `dialect: Dialect` - Dialect for ODO `min_count` interpretation

### Parsing with Explicit Feature Flags

```rust
pub fn parse_copybook_with_feature_flags(
    text: &str,
    options: &ParseOptions,
    feature_flags: &FeatureFlags,
) -> Result<Schema, Error>
```

Parse a copybook using an explicit feature-flag snapshot instead of the process-global feature configuration. The supplied flags are used consistently during parser and layout resolution, which makes isolated tests and concurrent callers deterministic.

```rust
use copybook_core::{
    parse_copybook_with_feature_flags, Feature, FeatureFlags, ParseOptions,
};

let mut feature_flags = FeatureFlags::default();
feature_flags.disable(Feature::Comp1);

let result = parse_copybook_with_feature_flags(
    "01 VALUE-FIELD PIC S9(4) COMP-1.",
    &ParseOptions::default(),
    &feature_flags,
);
assert!(result.is_err());
```

This entry point returns the same structured parse errors as `parse_copybook` and `parse_copybook_with_options`, including stable unsupported-clause errors for disabled features. It does not mutate the global feature configuration.

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
use copybook_codec::{decode_record_with_scratch, runtime::ScratchBuffers};

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
pub fn decode_file_to_jsonl(
    schema: &Schema,
    input: impl Read,
    output: impl Write,
    options: &DecodeOptions,
) -> Result<RunSummary>
```

Decode an entire input stream to JSONL format with **enterprise reliability**.

**Parameters:**
- `schema` - Parsed copybook schema
- `input` - Reader for binary record data
- `output` - Writer for JSONL output
- `options` - Decode configuration options

**Returns:**
- `Ok(RunSummary)` - Processing statistics
- `Err(Error)` - Processing error

**Example:**
```rust
let opts = DecodeOptions {
    codepage: Codepage::CP037,
    format: RecordFormat::Fixed,
    emit_meta: true,
    preserve_zoned_encoding: true, // Enable encoding preservation
    preferred_zoned_encoding: ZonedEncodingFormat::Ebcdic, // Fallback format
    ..Default::default()
};

let input = std::fs::File::open("data.bin")?;
let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(&schema, input, output, &opts)?;

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
    input: impl Read,
    output: impl Write,
    options: &EncodeOptions,
) -> Result<RunSummary>
```

Encode JSONL data to binary format.

**Parameters:**
- `schema` - Parsed copybook schema
- `input` - Reader for JSONL input
- `output` - Writer for binary output
- `options` - Encode configuration options

**Returns:**
- `Ok(RunSummary)` - Processing statistics
- `Err(Error)` - Processing error

**Example:**
```rust
let opts = EncodeOptions {
    codepage: Codepage::CP037,
    format: RecordFormat::Fixed,
    use_raw: true,
    zoned_encoding_override: None, // Respect preserved formats
    ..Default::default()
};

// Or with explicit format override:
let opts_override = EncodeOptions {
    codepage: Codepage::CP037,
    format: RecordFormat::Fixed,
    zoned_encoding_override: Some(ZonedEncodingFormat::Ascii), // Force ASCII zones
    ..Default::default()
};

let input = std::fs::File::open("input.jsonl")?;
let output = std::fs::File::create("output.bin")?;
let summary = encode_jsonl_to_file(&schema, input, output, &opts)?;
```

### Record-Level Processing

For decoding individual records or streaming through a file, `copybook-codec`
exposes two complementary APIs. See
[Record Iterators](iterators.md) for full iterator documentation.

**Single-record decode** (`decode_record`, crate root):

```rust
/// Decode one record's bytes into a JSON value.
pub fn decode_record(
    schema: &Schema,
    data: &[u8],
    options: &DecodeOptions,
) -> Result<serde_json::Value>;
```

```rust
use copybook_codec::{decode_record, DecodeOptions};
use copybook_core::parse_copybook;

let schema = parse_copybook(copybook_src)?;
let json = decode_record(&schema, &record_bytes, &DecodeOptions::default())?;
```

**Bulk file decode** (`decode_file_to_jsonl`, crate root):

```rust
/// Decode an entire input stream to JSONL written to `output`.
/// Multi-threaded via `options.with_threads(n)`.
pub fn decode_file_to_jsonl(
    schema: &Schema,
    input: impl Read,
    output: impl Write,
    options: &DecodeOptions,
) -> Result<RunSummary>;
```

**Streaming record iterator** (`RecordIterator`, crate root):

```rust
pub struct RecordIterator<R: Read> { /* private fields */ }

impl<R: Read> RecordIterator<R> {
    /// Construct a new iterator. Validation of `schema.lrecl_fixed` is
    /// *deferred* to the first read, not performed here — `new` is
    /// infallible with respect to format/schema configuration.
    pub fn new(reader: R, schema: &Schema, options: &DecodeOptions) -> Result<Self>;

    pub fn current_record_index(&self) -> u64;
    pub fn is_eof(&self) -> bool;
    pub fn schema(&self) -> &Schema;
    pub fn options(&self) -> &DecodeOptions;

    /// Read raw record bytes without JSON decoding.
    pub fn read_raw_record(&mut self) -> Result<Option<Vec<u8>>>;
}

impl<R: Read> Iterator for RecordIterator<R> {
    type Item = Result<serde_json::Value>;
    fn next(&mut self) -> Option<Self::Item>;
}

/// Convenience constructors (crate root):
pub fn iter_records<R: Read>(
    reader: R, schema: &Schema, options: &DecodeOptions,
) -> Result<RecordIterator<R>>;

pub fn iter_records_from_file<P: AsRef<Path>>(
    file_path: P, schema: &Schema, options: &DecodeOptions,
) -> Result<RecordIterator<std::fs::File>>;
```

```rust
use copybook_codec::{iter_records, DecodeOptions};
use copybook_core::parse_copybook;
use std::io::Cursor;

let schema = parse_copybook(copybook_src)?;
let iter = iter_records(Cursor::new(data), &schema, &DecodeOptions::default())?;

for result in iter {
    match result {
        Ok(json) => println!("{json}"),
        Err(e) => eprintln!("record error: {e}"), // iteration continues
    }
}
```

> **Error recovery:** decode errors yield `Some(Err(...))` and the iterator
> *continues* to the next record — no need to restart. A truncated trailing
> record at EOF is a clean stop (`Ok(None)`), not an error. Missing
> `lrecl_fixed` with `Fixed` format yields `Err(CBKI001)` on the first read.

**Single-record encode** (`encode_record`, crate root):

```rust
/// Encode one JSON record to binary bytes using the provided schema.
pub fn encode_record(
    schema: &Schema,
    json: &serde_json::Value,
    options: &EncodeOptions,
) -> Result<Vec<u8>>;
```

The input JSON may either be a bare object of field values or a full decode
envelope with a `fields` object. When `options.use_raw` is enabled and a
`raw_b64` / `__raw_b64` key is present, the raw payload is used for bit-exact
round-tripping.

```rust
use copybook_core::parse_copybook;
use copybook_codec::{encode_record, EncodeOptions};
use copybook_codec::options::{Codepage, RecordFormat};
use serde_json::json;

let schema = parse_copybook("01 FLD PIC X(5).")?;
let json = json!({"fields": {"FLD": "HELLO"}});
let options = EncodeOptions::new()
    .with_codepage(Codepage::ASCII)
    .with_format(RecordFormat::Fixed);
let binary = encode_record(&schema, &json, &options)?;
assert_eq!(&binary[..5], b"HELLO");
```

## Error Handling with Panic Safety

### Error Type

```rust
pub struct Error {
    pub code: ErrorCode,
    pub message: String,
    pub context: Option<ErrorContext>,
}

pub enum ErrorCode {
    // Parse errors (CBKP*)
    CBKP001_SYNTAX,                    // Syntax errors in copybook
    CBKP021_ODO_NOT_TAIL,             // ODO array positioning issues
    CBKP051_UNSUPPORTED_EDITED_PIC,   // Reserved for future unsupported patterns; all current edited PICs supported

    // Schema validation errors (CBKS*)
    CBKS121_COUNTER_NOT_FOUND,        // ODO counter field missing
    CBKS141_RECORD_TOO_LARGE,         // Record size exceeds limits
    CBKS301_ODO_CLIPPED,              // ODO bounds enforcement
    CBKS302_ODO_RAISED,               // ODO minimum value validation

    // Data processing errors (CBKD*)
    CBKD101_INVALID_FIELD_TYPE,       // Type mismatch in data
    CBKD301_RECORD_TOO_SHORT,         // Record shorter than expected

    // Encoding errors (CBKE*)
    CBKE501_JSON_TYPE_MISMATCH,       // JSON encoding type issues
    // ... other error codes
}

pub struct ErrorContext {
    pub record_index: Option<u64>,
    pub field_path: Option<String>,
    pub byte_offset: Option<u64>,
    pub line_number: Option<u32>,
    pub details: Option<String>,
}
```

**Error Context:**
- **record_index** - Record number where the error occurred (for data processing errors)
- **field_path** - Hierarchical field path in dot notation (e.g., "customer.address.street")
- **byte_offset** - Byte offset within the record or file where the error occurred
- **line_number** - Line number in the copybook source (for parse errors)
- **details** - Free-form text providing extra details relevant to the specific error

### Panic-Safe Error Handling Patterns

```rust
use copybook_core::{parse_copybook, Error, ErrorCode};
use copybook_core::utils::{OptionExt, VecExt};

// Enhanced error handling with panic safety
match parse_copybook(text) {
    Ok(schema) => {
        tracing::info!(
            fields = %schema.fields.len(),
            fixed_length = ?schema.lrecl_fixed,
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
let opts = DecodeOptions {
    strict_mode: false,
    max_errors: Some(100),
    ..Default::default()
};

match decode_file_to_jsonl(&schema, input, output, &opts) {
    Ok(summary) => {
        if summary.records_with_errors > 0 {
            println!("Completed with {} errored records", summary.records_with_errors);
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
    pub records_with_errors: u64,
    pub warnings: u64,
    pub processing_time_ms: u64,
    pub bytes_processed: u64,
    pub schema_fingerprint: String,
    pub throughput_mbps: f64,
    pub peak_memory_bytes: Option<u64>,
    pub threads_used: usize,
    /// The first `MAX_CAPTURED_FAILURES` record failures seen during the run.
    pub failures: Vec<RecordFailure>,
}

pub struct RecordFailure {
    /// 1-based index of the record within the input.
    pub record_index: u64,
    /// The error that caused this record to fail, with its code and context.
    pub error: Error,
}
```

`records_with_errors` is the full count of failed records. `failures` carries the
detail for the first `MAX_CAPTURED_FAILURES` (10) of them, so a caller can report
*which* records failed and *why* without holding an unbounded list for a file that
fails on every record. `undisclosed_failure_count()` returns how many failures
occurred beyond the retained ones.

```rust
let summary = decode_file_to_jsonl(&schema, input, &mut output, &options)?;
for failure in &summary.failures {
    eprintln!("record {}: {}", failure.record_index, failure.error);
}
```

## Advanced Usage

### REDEFINES Cluster Inspection

`Schema::find_redefining_fields` returns every field that redefines a given target path, letting callers reason about a REDEFINES cluster (e.g. to pick the widest field for size calculations, or to inspect which alternate views are defined):

```rust
// Example schema with REDEFINES cluster
let copybook = r#"
01 RECORD.
   05 DATA-FIELD      PIC X(10).
   05 FIELD-A         REDEFINES DATA-FIELD PIC X(5).
   05 FIELD-B         REDEFINES DATA-FIELD PIC 9(8) COMP-3.
   05 FIELD-C         REDEFINES DATA-FIELD PIC X(15).
"#;
let schema = parse_copybook(copybook)?;

// Find every field that redefines DATA-FIELD
// (find_redefining_fields matches the bare name written after REDEFINES;
// find_field matches the fully-qualified dotted path)
let redefining_fields = schema.find_redefining_fields("DATA-FIELD");
let max_size = redefining_fields
    .iter()
    .map(|f| f.len)
    .chain(std::iter::once(
        schema.find_field("RECORD.DATA-FIELD").unwrap().len,
    ))
    .max()
    .unwrap();
```

**Key Points:**
- **Accurate Cluster Sizing**: Traverse `find_redefining_fields()` results to find the maximum size across all redefining fields
- **Complete Field Discovery**: `find_redefining_fields()` method finds all fields that redefine a target
- **Type-Safe Processing**: Schema access ensures proper field type and offset information
- **Memory Safety**: Bounds checking uses actual calculated cluster size, not field-specific sizes

### ODO (OCCURS DEPENDING ON) Schema Inspection

ODO counter resolution, encoding, and bounds validation happen automatically inside `decode_record`/`encode_record` — callers don't drive this directly. To *inspect* an ODO array's schema (e.g. to report its counter field or bounds), match on `Occurs::ODO`:

```rust
// Example schema with an ODO array
let copybook = r#"
01 RECORD.
   05 ITEM-COUNT     PIC 9(3) COMP-3.
   05 ITEMS          OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-ID     PIC 9(8).
      10 ITEM-NAME   PIC X(20).
"#;
let schema = parse_copybook(copybook)?;

let items_field = schema.find_field("RECORD.ITEMS").unwrap();
if let Some(Occurs::ODO { min, max, counter_path }) = &items_field.occurs {
    println!("ITEMS occurs {min}..={max} times, counted by {counter_path}");
}
```

**Key Points:**
- **Automatic Resolution**: `decode_record`/`encode_record` read and write the counter field and validate `min`/`max` bounds internally; no manual counter handling is required.
- **Schema-Only Inspection**: `Occurs::ODO { min, max, counter_path }` is available for callers who want to report on or validate schema structure without decoding data.

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

### Schema Inspection

```rust
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
        // Use decode_record_with_scratch for per-thread decode with reused buffers
        let mut scratch = copybook_codec::runtime::ScratchBuffers::new();
        // Process chunk of data: decode_record_with_scratch(&schema, data, &opts, &mut scratch)
        let _ = (schema, opts, scratch);
        Ok(())
    })
}).collect();

for handle in handles {
    handle.join().unwrap()?;
}
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
let json_value = decode_record(&schema, data, &decode_opts)?;
let customer: Customer = serde_json::from_value(json_value)?;

// Encode from typed struct
let json_value = serde_json::to_value(&customer)?;
let binary_data = encode_record(&schema, &json_value, &encode_opts)?;
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
    let json_value = decode_record(&schema, &buffer, &opts)?;
    
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
        use copybook_codec::decode_record;

        while let Ok(data) = input_rx.recv() {
            match decode_record(&schema, &data, &opts) {
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
// Reuse scratch buffers for better performance across many records
use copybook_codec::{decode_record_with_scratch, runtime::ScratchBuffers};

let mut scratch = ScratchBuffers::new();

for record_data in record_chunks {
    let json_value = decode_record_with_scratch(&schema, &record_data, &opts, &mut scratch)?;
    // Process json_value
}
```

### Batch Processing

```rust
// Process records in batches via the iterator
use copybook_codec::{iter_records_from_file, DecodeOptions};

const BATCH_SIZE: usize = 1000;
let mut batch = Vec::with_capacity(BATCH_SIZE);

for record_result in iter_records_from_file("data.bin", &schema, &opts)? {
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

        let data = b"1234JOHN      ";
        let json = decode_record(&schema, data, &opts).unwrap();

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
        
        let binary = encode_record(&schema, &json, &encode_opts)?;
        let decoded = decode_record(&schema, &binary, &decode_opts)?;
        
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
    .with_codepage(Codepage::CP037)
    .with_format(RecordFormat::Fixed)
    .with_preserve_zoned_encoding(true)
    .with_preferred_zoned_encoding(ZonedEncodingFormat::Ebcdic)
    .with_emit_meta(true);

// Encode with format override
let encode_opts = EncodeOptions::new()
    .with_codepage(Codepage::CP037)
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
    .with_codepage(Codepage::CP037)
    .with_format(RecordFormat::Fixed)
    .with_preserve_zoned_encoding(true)
    .with_emit_meta(true);

let json_value = decode_record(&schema, original_data, &decode_opts)?;

// JSON now contains encoding metadata (flat field-name -> format strings):
// {
//   "CUSTOMER_ID": "12345678",
//   "ACCOUNT_BALANCE": "1234567.89",
//   "_encoding_metadata": {
//     "CUSTOMER_ID": "ascii",
//     "ACCOUNT_BALANCE": "ascii"
//   }
// }

// Step 2: Encode preserving original format
let encode_opts = EncodeOptions::new()
    .with_codepage(Codepage::CP037)
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
        ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS => {
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
let json = decode_record(&schema, data, &opts)?;
let customer_id = json["CUSTOMER_ID"].as_str().unwrap();
```

### From Java COBOL Libraries

```rust
// Java-style configuration
// CobolDecoder decoder = new CobolDecoder(schema, "CP037", true);

// copybook-rs equivalent
let opts = DecodeOptions {
    codepage: Codepage::CP037,
    strict_mode: true,
    ..Default::default()
};
// Decode records via decode_record / decode_file_to_jsonl / the iterator API
// — see "Record-Level Processing" above.
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
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
