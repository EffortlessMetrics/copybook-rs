<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Getting Started with copybook-rs

A step-by-step tutorial for newcomers to COBOL copybook parsing with enterprise-grade reliability.

## What You'll Learn

In this tutorial, you'll learn how to:
- Parse COBOL copybooks into structured schemas
- Convert mainframe data to JSON with panic-safe operations
- Handle errors gracefully in production environments
- Use copybook-rs's enterprise reliability features

## Prerequisites

- Rust 1.92+ installed
- Basic understanding of COBOL data structures
- Sample COBOL copybook and data files

## Step 1: Project Setup

Create a new Rust project and add copybook-rs dependencies:

```bash
cargo new copybook_tutorial
cd copybook_tutorial
```

Add to your `Cargo.toml`:

```toml
[dependencies]
copybook-rs = "0.4.3"
# or: copybook = "0.4.3"
```

## Step 2: Your First COBOL Copybook

Create a simple COBOL copybook file `customer.cpy`:

```cobol
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID         PIC 9(6).
           05  CUSTOMER-NAME       PIC X(30).
           05  ACCOUNT-BALANCE     PIC S9(8)V99 COMP-3.
           05  STATUS-CODE         PIC X(1).
               88  ACTIVE          VALUE 'A'.
               88  INACTIVE        VALUE 'I'.
               88  SUSPENDED       VALUE 'S'.
```

## Step 3: Parse the Copybook with Error Handling

copybook-rs uses panic-safe operations throughout. Here's how to parse a copybook with proper error handling:

```rust
use copybook_rs::{parse_copybook, parse_copybook_with_options, ParseOptions};
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Read copybook file safely
    let copybook_text = fs::read_to_string("customer.cpy")?;

    // Parse with default options (enterprise-safe)
    let schema = parse_copybook(&copybook_text)?;

    println!("Successfully parsed {} fields", schema.fields.len());

    // Alternative: Parse with custom options for specialized needs
    let parse_options = ParseOptions {
        allow_inline_comments: false, // Disable COBOL-2002 inline comments
        ..ParseOptions::default()
    };
    let schema_custom = parse_copybook_with_options(&copybook_text, &parse_options)?;

    Ok(())
}
```

**Key Safety Features:**
- No `.unwrap()` or `.expect()` calls that could panic
- Structured error handling with detailed context
- Memory-safe operations throughout

## Step 4: Configure Data Decoding Options

copybook-rs provides comprehensive options for enterprise data processing:

```rust
use copybook_rs::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat};

fn configure_decoder() -> DecodeOptions {
    DecodeOptions::new()
        .with_codepage(Codepage::CP037)           // EBCDIC encoding
        .with_format(RecordFormat::Fixed)         // Fixed-length records
        .with_json_number_mode(JsonNumberMode::Lossless)  // Preserve precision
        .with_emit_meta(true)                     // Include metadata
}
```

**Enterprise Features:**
- Multiple EBCDIC codepage support (CP037, CP273, CP500, CP1047, CP1140)
- Lossless numeric conversion for financial data
- Structural validation for data integrity
- Metadata emission for audit trails

## Step 5: Decode Data with High-Performance Safety

Process binary data with enterprise reliability:

```rust
use copybook_rs::{decode_record, decode_record_with_scratch, memory::ScratchBuffers};

fn decode_customer_data(
    schema: &copybook_rs::Schema,
    record_data: &[u8],
    options: &DecodeOptions,
) -> copybook_rs::Result<serde_json::Value> {

    // Method 1: Simple decoding (suitable for small volumes)
    let json_value = decode_record(schema, record_data, options)?;

    // Method 2: High-performance with scratch buffers (for high throughput)
    let mut scratch = ScratchBuffers::new();
    let json_value_fast = decode_record_with_scratch(
        schema,
        record_data,
        options,
        &mut scratch
    )?;

    Ok(json_value)
}
```

**Performance Features:**
- Scratch buffer optimization for hot paths
- Zero-copy operations where possible
- Memory efficiency: <256 MiB for multi-GB files
- DISPLAY: 2.5+ GiB/s, COMP-3: 100+ MiB/s throughput

## Step 6: Process Files with Streaming

For production workloads, use streaming APIs:

```rust
use copybook_rs::{decode_file_to_jsonl, iter_records_from_file};
use std::{fs::File, path::Path};

fn process_mainframe_file(
    schema: &copybook_rs::Schema,
    input_path: &Path,
    output_path: &Path,
    options: &DecodeOptions,
) -> Result<(), Box<dyn std::error::Error>> {

    // Method 1: Iterator for custom processing
    let iterator = iter_records_from_file(input_path, schema, options)?;

    for (record_num, record_result) in iterator.enumerate() {
        let json_value = record_result?;

        // Process record (validation, transformation, etc.)
        println!("Record {}: {}", record_num + 1, json_value);
    }

    // Method 2: Direct file conversion with metrics
    let input_file = File::open(input_path)?;
    let output_file = std::fs::File::create(output_path)?;
    let summary = decode_file_to_jsonl(schema, input_file, output_file, options)?;

    println!("Processed {} records in {:.3} seconds",
             summary.records_processed,
             summary.processing_time_seconds());

    Ok(())
}
```

**Enterprise Streaming Features:**
- Bounded memory usage for multi-GB files
- Parallel processing with deterministic output
- Comprehensive error reporting with record context
- Progress tracking and metrics

## Step 7: Handle Errors Like a Pro

copybook-rs provides a comprehensive error taxonomy for production monitoring:

```rust
use copybook_rs::{Error, ErrorCode};

fn handle_parsing_errors(result: Result<copybook_rs::Schema, Error>) {
    match result {
        Ok(schema) => {
            println!("Successfully parsed schema");
        }
        Err(error) => {
            match error.code {
                ErrorCode::CBKP001_SYNTAX => {
                    eprintln!("Syntax error in copybook: {}", error.message);
                    // Handle syntax issues
                }
                ErrorCode::CBKP021_ODO_NOT_TAIL => {
                    eprintln!("ODO array positioning error: {}", error.message);
                    // Handle structural issues
                }
                ErrorCode::CBKS141_RECORD_TOO_LARGE => {
                    eprintln!("Record size exceeds limits: {}", error.message);
                    // Handle size constraints
                }
                _ => {
                    eprintln!("Other error: {} ({})", error.message, error.code);
                }
            }
        }
    }
}
```

**Error Categories:**
- `CBKP*`: Parse errors (syntax, unsupported features)
- `CBKS*`: Schema validation (ODO counters, record limits)
- `CBKD*`: Data errors (invalid decimals, truncated records)
- `CBKE*`: Encoding errors (type mismatches, bounds)

## Step 8: Complete Working Example

Here's a complete example that demonstrates enterprise-safe COBOL processing:

```rust
use copybook_rs::{
    Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl, parse_copybook,
};
use std::fs::File;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Parse copybook with error handling
    let copybook_text = std::fs::read_to_string("customer.cpy")?;
    let schema = parse_copybook(&copybook_text)?;

    // 2. Configure enterprise-grade options
    let options = DecodeOptions::new()
        .with_codepage(Codepage::CP037)
        .with_format(RecordFormat::Fixed)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_meta(true);

    // 3. Process data with streaming and metrics
    let input_file = File::open("customer_data.bin")?;
    let output_file = File::create("customer_data.jsonl")?;

    let summary = decode_file_to_jsonl(&schema, input_file, output_file, &options)?;

    // 4. Report results
    println!("✅ Processing complete!");
    println!("📊 Records processed: {}", summary.records_processed);
    println!("⏱️  Processing time: {:.3} seconds", summary.processing_time_seconds());
    println!("💾 Memory usage: <256 MiB (bounded)");

    Ok(())
}
```

## Next Steps

Now that you've learned the basics of panic-safe COBOL processing:

1. **Enterprise Deployment**: Learn about production deployment patterns in the [Enterprise Deployment Tutorial](enterprise-deployment.md)
2. **Error Handling**: Master production error handling in the [Error Handling Guide](../how-to/error-handling-production.md)
3. **Performance**: Optimize for high throughput in the [Performance Guide](../how-to/performance-optimization.md)
4. **API Reference**: Explore the complete API in the [Library Reference](../reference/LIBRARY_API.md)

## Key Takeaways

- copybook-rs eliminates all panic risks with enterprise-safe operations
- Structured error handling provides detailed context for debugging
- High-performance streaming supports multi-GB files with bounded memory
- Comprehensive validation ensures data integrity for financial workloads
- Zero unsafe code guarantees memory safety for production environments

Welcome to reliable COBOL data processing with copybook-rs! 🎉
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
