<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-rs Examples

This directory contains practical examples demonstrating copybook-rs usage patterns for COBOL copybook parsing and mainframe data processing.

## Example Categories

### 📚 [Basic Examples](basic/)
Simple, focused examples for getting started:
- **[decode_record.rs](basic/test_decode_record.rs)** - Basic record decoding with REDEFINES and ODO arrays

### 🔧 [Integration Examples](integration/)
Complete workflows showing real-world usage:
- End-to-end file processing
- CLI integration patterns
- Error handling strategies

### 🏢 [Enterprise Examples](enterprise/)
Production-ready patterns for enterprise environments:
- High-performance batch processing
- Enterprise audit logging
- Mainframe compatibility patterns

## Running Examples

The standalone example files (`basic/`, `integration/`, `enterprise/`) are reference
implementations demonstrating copybook-rs API usage patterns. They are not registered
as cargo examples — review them as code samples.

There are two Kafka examples:

- [kafka_pipeline](kafka_pipeline/) is a historical standalone producer-only crate.
- [kafka_streaming](kafka_streaming/) is a producer + consumer streaming example with local compose support.

```bash
# Build the Kafka pipeline examples
cargo build --manifest-path examples/kafka_pipeline/Cargo.toml
cargo build --manifest-path examples/kafka_streaming/Cargo.toml
```

```bash
# Run the streaming example
cargo run --manifest-path examples/kafka_streaming/Cargo.toml --example producer
cargo run --manifest-path examples/kafka_streaming/Cargo.toml --example consumer
```

The [copybook-fixed-clean-room](copybook-fixed-clean-room/) project is a
standalone proof that fixed-LRECL framing can be consumed without the schema,
codec, or CLI packages:

```bash
cargo run --locked --manifest-path examples/copybook-fixed-clean-room/Cargo.toml
```

## Example Data

Examples use test data from:
- `../test-data/` - Simple test copybooks and data
- `../fixtures/` - Comprehensive test fixtures and golden corpus

## Contributing Examples

When adding new examples:
1. Choose the appropriate category (basic/integration/enterprise)
2. Include comprehensive comments explaining COBOL concepts
3. Add error handling appropriate for the example level
4. Update this README with a brief description

For enterprise examples, ensure:
- Production-ready error handling
- Performance considerations documented
- Security best practices followed
- Mainframe compatibility verified
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
