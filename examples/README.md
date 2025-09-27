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

All examples can be run directly with cargo:

```bash
# Run a basic example
cargo run --example basic/test_decode_record

# Run with specific features
cargo run --example enterprise/batch_processor --features comp3_fast

# Build all examples
cargo build --examples
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
