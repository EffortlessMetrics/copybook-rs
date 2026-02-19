<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Kafka Pipeline Example

This example demonstrates how to integrate copybook-rs with Apache Kafka to process mainframe COBOL data in a streaming pipeline.

## Overview

The Kafka pipeline example shows:
- Reading COBOL copybook files
- Decoding binary mainframe data (EBCDIC)
- Producing decoded JSON records to Kafka topics
- Error handling and retry logic
- Progress tracking and logging

## Prerequisites

### Required

1. **Apache Kafka broker** - A running Kafka instance
   ```bash
   # Using Docker
   docker run -d --name kafka \
     -p 9092:9092 \
     -e KAFKA_ZOOKEEPER_CONNECT=host.docker.internal:2181 \
     -e KAFKA_ADVERTISED_LISTENERS=PLAINTEXT://localhost:9092 \
     confluentinc/cp-kafka:latest
   ```

2. **Create a topic** for the data:
   ```bash
   kafka-topics --create \
     --topic copybook-data \
     --bootstrap-server localhost:9092 \
     --partitions 3 \
     --replication-factor 1
   ```

### Optional

- **Kafka consumer** - To verify messages are being produced:
  ```bash
  kafka-console-consumer \
    --topic copybook-data \
    --bootstrap-server localhost:9092 \
    --from-beginning
  ```

## Configuration

Configure the pipeline using environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `KAFKA_BROKERS` | Comma-separated list of Kafka brokers | `localhost:9092` |
| `KAFKA_TOPIC` | Kafka topic name | `copybook-data` |
| `COPYBOOK_PATH` | Path to COBOL copybook file | `test-data/simple.cpy` |
| `DATA_PATH` | Path to binary data file | `test-data/simple.bin` |

### Example Configuration

```bash
export KAFKA_BROKERS="localhost:9092"
export KAFKA_TOPIC="copybook-data"
export COPYBOOK_PATH="path/to/your/copybook.cpy"
export DATA_PATH="path/to/your/data.bin"
```

## Running the Example

### Build the example

```bash
cargo build -p kafka-pipeline
```

### Run with default configuration

```bash
cargo run -p kafka-pipeline
```

### Run with custom configuration

```bash
KAFKA_BROKERS="kafka1:9092,kafka2:9092" \
KAFKA_TOPIC="my-topic" \
COPYBOOK_PATH="/data/records.cpy" \
DATA_PATH="/data/records.bin" \
cargo run -p kafka-pipeline
```

## Example Output

```
INFO Starting Kafka Pipeline Example
INFO Kafka configuration: brokers=localhost:9092, topic=copybook-data
INFO Reading copybook from: test-data/simple.cpy
INFO Parsing copybook schema...
INFO Schema loaded: 5 fields, LRECL: Some(100)
INFO Reading binary data from: test-data/simple.bin
INFO Processing 1000 records (100 bytes each)...
INFO Message sent successfully to topic: copybook-data
INFO Processed 100/1000 records (0 errors)
INFO Processed 200/1000 records (0 errors)
...
INFO Processing complete: 1000 records processed, 0 errors
INFO Successfully processed 1000 records
INFO Pipeline completed successfully
```

## Architecture

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

## Error Handling

The pipeline includes comprehensive error handling:

- **Copybook parsing errors** - Reported and processing stops
- **Data read errors** - Reported and processing stops
- **Decode errors** - Logged, processing continues with next record
- **Kafka send errors** - Automatic retry (max 3 attempts) with exponential backoff

## Performance Considerations

- **Batch size**: The example sends records one at a time. For production, consider batching multiple records per message.
- **Compression**: Enable Kafka compression (gzip, snappy, lz4) for better throughput.
- **Parallelism**: Use multiple producer instances for high-throughput scenarios.
- **Record size**: Large records may require adjusting Kafka's `message.max.bytes` configuration.

## Troubleshooting

### Connection Refused

```
Error: Failed to create producer: Connection refused
```

**Solution**: Ensure Kafka broker is running and accessible at the configured address.

### Topic Not Found

```
Error: Failed to send message: Local: Unknown topic or partition
```

**Solution**: Create the topic before running the example:
```bash
kafka-topics --create --topic copybook-data --bootstrap-server localhost:9092
```

### Copybook Not Found

```
Error: Copybook file not found: test-data/simple.cpy
```

**Solution**: Set the `COPYBOOK_PATH` environment variable to point to a valid copybook file.

### Data File Not Found

```
Error: Data file not found: test-data/simple.bin
```

**Solution**: Set the `DATA_PATH` environment variable to point to a valid binary data file.

## Extending the Example

### Add Schema Registry Integration

```rust
use schema_registry_converter::async_impl::schema_registry_client::SrSettings;
use schema_registry_converter::async_impl::avro::AvroEncoder;

// Create Avro encoder
let sr_settings = SrSettings::new("http://localhost:8081".to_string());
let encoder = AvroEncoder::new(sr_settings);
```

### Add Compression

```rust
let producer: ThreadedProducer<_> = ClientConfig::new()
    .set("bootstrap.servers", &config.brokers)
    .set("compression.type", "snappy")  // Enable compression
    .create()?;
```

### Add Message Keys

```rust
// Use a field from the record as the key
let key = json_value["customer_id"].as_str().unwrap_or("");
pipeline.send_message(Some(key), &json_bytes)?;
```

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
