# copybook-rs

A modern, memory-safe parser/codec for COBOL copybooks and fixed-record data.

## Overview

copybook-rs is a Rust implementation of a COBOL copybook parser and data codec that provides deterministic, reproducible conversion of mainframe-encoded records into accessible formats like JSON. This enables organizations to unlock mainframe data for analytics, system integration, and modernization efforts without requiring COBOL runtime environments.

## Features

- **Deterministic Output**: Byte-identical results across runs and parallel processing
- **Round-Trip Fidelity**: Unchanged JSON data re-encodes to identical binary
- **Memory Safety**: No unsafe code in public API paths
- **Streaming Architecture**: Bounded memory usage for multi-GB files
- **Comprehensive Error Handling**: Stable error codes with structured context

## Architecture

The project is organized as a Cargo workspace with the following crates:

- **copybook-core**: Core parsing and schema types for COBOL copybooks
- **copybook-codec**: Encoding and decoding codecs for COBOL data types
- **copybook-cli**: Command-line interface for copybook processing
- **copybook-gen**: Test fixture and synthetic data generation

## Quick Start

```bash
# Parse a copybook and output schema JSON
copybook parse schema.cpy --output schema.json

# Inspect copybook layout
copybook inspect schema.cpy --codepage cp037

# Decode binary data to JSONL
copybook decode schema.cpy data.bin --output data.jsonl --format fixed

# Encode JSONL back to binary
copybook encode schema.cpy data.jsonl --output data.bin --format fixed
```

## Requirements

- Rust 1.70 or later
- Cargo

## Installation

```bash
cargo install copybook-cli
```

## Development

```bash
# Clone the repository
git clone https://github.com/copybook-rs/copybook-rs.git
cd copybook-rs

# Build all crates
cargo build --workspace

# Run tests
cargo test --workspace

# Run clippy
cargo clippy --workspace -- -D warnings -W clippy::pedantic

# Format code
cargo fmt --all
```

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.
