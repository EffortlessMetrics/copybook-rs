<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Kafka Streaming Example

This example demonstrates a producer and consumer workflow for streaming COBOL copybook
records with Kafka.

## Structure

- `src/producer.rs` - Decodes copybook data and publishes JSON to Kafka.
- `src/consumer.rs` - Consumes JSON messages from Kafka and prints decoded records.
- `sample_data/schema.cpy` - Example copybook schema.
- `sample_data/data.bin` - Example fixed-length binary record payload.
- `docker-compose.yml` - Local Kafka + ZooKeeper setup for testing.

## Prerequisites

- Rust toolchain and required native dependencies for `rdkafka` (typically OpenSSL/dev libs).
- Docker and Docker Compose for local Kafka broker.

## Environment Variables

| Variable       | Description | Default |
| -------------- | ----------- | ------- |
| `KAFKA_BROKERS` | Comma-separated list of Kafka brokers | `localhost:9092` |
| `KAFKA_TOPIC` | Kafka topic name | `copybook-data` |
| `COPYBOOK_PATH` | Path to copybook schema | `sample_data/schema.cpy` |
| `DATA_PATH` | Path to fixed-length record data | `sample_data/data.bin` |
| `CODEPAGE` | `ascii` or EBCDIC variants (`cp037`, `cp273`, `cp500`, `cp1047`, `cp1140`) | `ascii` |
| `KAFKA_GROUP_ID` | Consumer group id (consumer only) | `copybook-streaming-consumer` |
| `MAX_MESSAGES` | Maximum messages to consume before exit (consumer only) | consume indefinitely |
| `POLL_INTERVAL_MS` | Poll backoff on consumer errors (consumer only) | `250` |

## Build

```bash
cargo build --manifest-path examples/kafka_streaming/Cargo.toml
```

## Run with Docker Compose

```bash
cd examples/kafka_streaming
docker compose up -d
```

Create a test topic:

```bash
docker compose exec kafka kafka-topics --create --topic copybook-data --partitions 3 --replication-factor 1 --bootstrap-server kafka:9092
```

In separate terminals, run producer and consumer:

```bash
cd examples/kafka_streaming

# Producer
cargo run --manifest-path examples/kafka_streaming/Cargo.toml --example producer

# Consumer
cargo run --manifest-path examples/kafka_streaming/Cargo.toml --example consumer
```

Override configuration from workspace root:

```bash
cd H:\Code\Rust\copybook-rs
$env:KAFKA_BROKERS="localhost:9092"
$env:KAFKA_TOPIC="copybook-data"
$env:CODEPAGE="ascii"
cargo run --manifest-path examples/kafka_streaming/Cargo.toml --example producer
```

## Default local sample data

- Schema: `sample_data/schema.cpy`
- Data: `sample_data/data.bin`

The sample records use ASCII fields, so the producer defaults to `CODEPAGE=ascii`.

## Troubleshooting

- `Failed to create producer`: confirm Kafka is running and reachable from the process.
- `Failed to subscribe`: confirm the topic exists (topic auto-creation may be disabled).
- Empty or broken output: verify `COPYBOOK_PATH` and `DATA_PATH` point to fixed-length records.

## Stopping local Kafka

```bash
cd examples/kafka_streaming
docker compose down
```

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
