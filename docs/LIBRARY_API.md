# Library API Reference

Complete reference for using copybook-rs as a Rust library.

## Overview

copybook-rs provides a comprehensive Rust API for parsing COBOL copybooks and encoding/decoding mainframe data. The library is designed for integration into ETL pipelines, data processing applications, and other systems that need to work with legacy COBOL data formats.

## Core Concepts

### Schema
A parsed representation of a COBOL copybook that defines the structure and layout of records.

### Field
An individual data element within a record, with type information, byte offset, and length.

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
- `sync` - Alignment padding bytes (if SYNCHRONIZED)

### FieldKind

```rust
pub enum FieldKind {
    Alphanum { len: u32 },
    ZonedDecimal { digits: u16, scale: i16, signed: bool },
    BinaryInt { bits: u16, signed: bool },
    PackedDecimal { digits: u16, scale: i16, signed: bool },
    Group,
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
}
```

### EncodeOptions

```rust
pub struct EncodeOptions {
    pub codepage: Codepage,
    pub format: RecordFormat,
    pub strict: bool,
    pub max_errors: Option<u32>,
    pub use_raw: bool,
    pub bwz_encode: bool,
}
```

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
    Record,      // Capture entire record
    Field,       // Capture individual fields
    RecordRdw,   // Capture record + RDW header
}

pub enum UnmappablePolicy {
    Error,    // Fail on unmappable characters
    Replace,  // Replace with U+FFFD
    Skip,     // Skip unmappable characters
}
```

## Core Functions

### Parsing

```rust
pub fn parse_copybook(text: &str) -> Result<Schema, Error>
```

Parse a COBOL copybook into a schema.

**Parameters:**
- `text` - COBOL copybook source text

**Returns:**
- `Ok(Schema)` - Parsed schema
- `Err(Error)` - Parse error with context

**Example:**
```rust
let copybook = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID    PIC 9(8).
   05 CUSTOMER-NAME  PIC X(30).
   05 BALANCE        PIC S9(7)V99 COMP-3.
"#;

let schema = parse_copybook(copybook)?;
println!("Parsed {} fields", schema.fields.len());
```

### File-Level Decoding

```rust
pub fn decode_file_to_jsonl<W: Write>(
    schema: &Schema,
    data_path: &Path,
    opts: &DecodeOptions,
    output: W,
) -> Result<RunSummary, Error>
```

Decode an entire file to JSONL format.

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
    ..Default::default()
};

let output = std::fs::File::create("output.jsonl")?;
let summary = decode_file_to_jsonl(&schema, Path::new("data.bin"), &opts, output)?;

println!("Processed {} records with {} errors", 
         summary.records_processed, summary.error_count);
```

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
```

**Example:**
```rust
let mut decoder = RecordDecoder::new(&schema, &opts)?;

// Decode single record
let record_data = &[0x01, 0x02, 0x03, /* ... */];
let json_value = decoder.decode_record(record_data)?;

// Decode file with iterator
for record_result in decoder.decode_file("data.bin")? {
    match record_result {
        Ok(json_value) => println!("{}", serde_json::to_string(&json_value)?),
        Err(e) => eprintln!("Record error: {}", e),
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

## Error Handling

### Error Type

```rust
pub struct Error {
    pub code: ErrorCode,
    pub message: String,
    pub context: ErrorContext,
}

pub enum ErrorCode {
    // Parse errors
    CBKP001_SYNTAX,
    CBKP051_UNSUPPORTED_EDITED_PIC,
    // ... other error codes
}

pub struct ErrorContext {
    pub record_index: Option<u64>,
    pub field_path: Option<String>,
    pub byte_offset: Option<u64>,
    pub line_number: Option<u32>,
    pub additional: HashMap<String, String>,
}
```

### Error Handling Patterns

```rust
// Handle specific error types
match parse_copybook(text) {
    Ok(schema) => { /* use schema */ },
    Err(e) => match e.code {
        ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC => {
            eprintln!("Edited PIC not supported: {}", e.message);
            // Handle gracefully
        },
        _ => return Err(e),
    }
}

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
        let opts = DecodeOptions::default();
        let mut decoder = RecordDecoder::new(&schema, &opts).unwrap();
        
        let data = b"1234JOHN      ";
        let json = decoder.decode_record(data).unwrap();
        
        assert_eq!(json["ID"], "1234");
        assert_eq!(json["NAME"], "JOHN      ");
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
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
