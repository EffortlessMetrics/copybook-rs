# Adapters

This document provides an overview of available adapters for integrating copybook-rs with various data processing systems.

## Overview

copybook-rs provides adapters to bridge COBOL mainframe data with modern data processing ecosystems:

- **Arrow/Parquet Adapter** - Convert COBOL data to Apache Arrow format and write to Parquet files
- **Kafka Adapter** - Stream decoded COBOL data to Apache Kafka topics

## Arrow/Parquet Adapter

The Arrow/Parquet adapter (`copybook-arrow`) provides functionality to convert COBOL copybook data to Apache Arrow format and write to Parquet files for analytics and data warehousing.

### Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
copybook-arrow = { version = "0.4.2" }
copybook-codec = { version = "0.4.2" }
copybook-core = { version = "0.4.2" }
```

### Basic Usage

#### Decode to Arrow

```rust
use copybook_arrow::{json_to_record_batch, json_to_schema, ArrowWriter};
use copybook_codec::{DecodeOptions, RecordFormat, Codepage, JsonNumberMode, UnmappablePolicy};
use copybook_core::parse_copybook;

// Parse copybook
let copybook = r#"
   01 CUSTOMER-RECORD.
      05 CUSTOMER-ID        PIC 9(9).
      05 CUSTOMER-NAME      PIC X(50).
      05 ACCOUNT-BALANCE    PIC S9(13)V99 COMP-3.
"#;
let schema = parse_copybook(copybook)?;

// Decode binary data
let options = DecodeOptions::new()
    .with_format(RecordFormat::Fixed)
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless);

let json_value = copybook_codec::decode_record(&schema, binary_data, &options)?;

// Convert to Arrow
let arrow_schema = json_to_schema(&json_value)?;
let mut writer = ArrowWriter::new(arrow_schema);
let batch = json_to_record_batch(writer.schema(), &json_value)?;
writer.add_batch(batch);
```

#### Write to Parquet

```rust
use copybook_arrow::{json_to_schema, ParquetFileWriter};
use parquet::file::properties::{Compression, WriterProperties};

// Create Arrow schema
let arrow_schema = json_to_schema(&json_value)?;

// Configure Parquet writer
let writer_properties = WriterProperties::builder()
    .set_compression(Compression::SNAPPY)
    .build();

let parquet_writer = ParquetFileWriter::new(arrow_schema)
    .with_writer_properties(writer_properties);

// Write to file
parquet_writer.write_json_records("output.parquet", &json_records)?;
```

### Examples

The `copybook-arrow` crate includes several examples:

- **`decode_to_arrow`** - Basic record decoding to Arrow format
- **`decode_to_parquet`** - Writing decoded records to Parquet files
- **`batch_processing`** - Efficient batch processing of large datasets

Run examples:

```bash
cargo run --example decode_to_arrow -p copybook-arrow
cargo run --example decode_to_parquet -p copybook-arrow
cargo run --example batch_processing -p copybook-arrow
```

### Type Mapping

The adapter automatically maps COBOL types to Arrow types:

| COBOL Type | Arrow Type | Notes |
|------------|------------|-------|
| `PIC X(n)` | `Utf8` | Alphanumeric strings |
| `PIC 9(n)` | `Int64` | Numeric integers |
| `PIC S9(n)V99` | `Float64` | Packed decimal (COMP-3) |
| `PIC 9(n)V99` | `Float64` | Decimal numbers |
| `COMP-5` | `Int64`/`Float64` | Binary numeric |

### Performance Considerations

- **Batch Size**: Process records in batches (1000-10000 records per batch) for optimal performance
- **Compression**: Use SNAPPY compression for best balance of speed and compression ratio
- **Memory**: Arrow uses columnar memory layout; ensure sufficient memory for large datasets
- **Parallelism**: Use multiple threads for CPU-intensive operations

### Building the Adapter

```bash
# Build the crate
cargo build -p copybook-arrow

# Build examples
cargo build -p copybook-arrow --examples

# Run tests
cargo test -p copybook-arrow
```

## Kafka Adapter

The Kafka adapter (`examples/kafka_pipeline`) provides a streaming pipeline for processing COBOL data and publishing to Kafka topics.

### Installation

The Kafka adapter is provided as an example. Build it:

```bash
cargo build --example kafka_pipeline
```

### Configuration

Configure via environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `KAFKA_BROKERS` | Comma-separated Kafka brokers | `localhost:9092` |
| `KAFKA_TOPIC` | Kafka topic name | `copybook-data` |
| `COPYBOOK_PATH` | Path to COBOL copybook file | `test-data/simple.cpy` |
| `DATA_PATH` | Path to binary data file | `test-data/simple.bin` |

### Running the Example

```bash
# Set environment variables
export KAFKA_BROKERS="localhost:9092"
export KAFKA_TOPIC="copybook-data"
export COPYBOOK_PATH="path/to/copybook.cpy"
export DATA_PATH="path/to/data.bin"

# Run the pipeline
cargo run --example kafka_pipeline
```

### Features

- **Automatic Retry**: Messages are retried up to 3 times with exponential backoff
- **Error Handling**: Decode errors are logged but don't stop processing
- **Progress Tracking**: Regular progress updates during processing
- **Metadata Support**: Includes audit metadata in decoded records

### Architecture

```
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│   Copybook  │────▶│  copybook-rs │────▶│   Kafka     │
│    Files    │     │   Decoder    │     │   Producer  │
└─────────────┘     └──────────────┘     └─────────────┘
                          │
                          ▼
                    ┌─────────────┐
                    │  JSON Data  │
                    └─────────────┘
```

### Performance Tips

- **Batch Messages**: Send multiple records per message for higher throughput
- **Compression**: Enable Kafka compression (gzip, snappy, lz4)
- **Parallel Producers**: Use multiple producer instances for high-volume scenarios
- **Message Keys**: Use meaningful keys for partitioning

### Building the Example

```bash
# Build the example
cargo build --example kafka_pipeline

# Build with release optimizations
cargo build --example kafka_pipeline --release
```

## Building and Running Examples

### Build All Examples

```bash
cargo build --workspace --examples
```

### Build Specific Examples

```bash
# Arrow adapter examples
cargo build -p copybook-arrow --examples

# Kafka pipeline example
cargo build --example kafka_pipeline
```

### Run Examples

```bash
# Arrow examples
cargo run --example decode_to_arrow -p copybook-arrow
cargo run --example decode_to_parquet -p copybook-arrow
cargo run --example batch_processing -p copybook-arrow

# Kafka example
cargo run --example kafka_pipeline
```

### Using Just

The project includes `just` targets for building examples:

```bash
just examples           # Build all examples
just example-arrow      # Build Arrow adapter
just example-kafka      # Build Kafka example
```

## CI Integration

All examples are built and tested in CI to ensure they compile correctly:

- **Ubuntu, macOS, Windows** - Examples are built on all supported platforms
- **Stable Rust** - Uses the latest stable Rust toolchain
- **Compilation Check** - Ensures examples compile without errors

See [`.github/workflows/ci.yml`](../.github/workflows/ci.yml) for details.

## Troubleshooting

### Arrow/Parquet Issues

**Issue**: Schema conversion fails
```
Error: JSON conversion error: Expected JSON object for schema
```

**Solution**: Ensure the JSON value is an object with key-value pairs, not an array or primitive.

**Issue**: Parquet write fails
```
Error: Parquet write error: Failed to create writer
```

**Solution**: Check disk space and write permissions for the output directory.

### Kafka Issues

**Issue**: Connection refused
```
Error: Failed to create producer: Connection refused
```

**Solution**: Ensure Kafka broker is running and accessible at the configured address.

**Issue**: Topic not found
```
Error: Failed to send message: Local: Unknown topic or partition
```

**Solution**: Create the topic before running the example:
```bash
kafka-topics --create --topic copybook-data --bootstrap-server localhost:9092
```

## Contributing

When adding new adapters:

1. Follow the existing crate structure
2. Include comprehensive examples
3. Add error handling appropriate for production use
4. Document type mappings and performance considerations
5. Update this documentation
6. Add CI checks to ensure examples compile

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
