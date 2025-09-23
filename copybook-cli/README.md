# copybook-cli

CLI for parsing/decoding/encoding COBOL copybooks at GiB/s throughput.

## Overview

`copybook-cli` provides a command-line interface for COBOL copybook processing with enterprise-grade performance and reliability. Process mainframe data files with simple commands.

## Installation

```bash
cargo install copybook-cli
```

## Quick Start

### Parse and Inspect Copybooks

```bash
# Parse copybook to JSON schema
copybook parse customer.cpy

# Human-readable layout inspection
copybook inspect customer.cpy
```

### Decode Binary Data

```bash
# Basic decode to JSONL
copybook decode customer.cpy data.bin --output data.jsonl --format fixed --codepage cp037

# High-performance with options
copybook decode file.cpy data.bin --output data.jsonl \
  --format fixed --codepage cp037 --json-number lossless \
  --emit-meta --threads 8
```

### Encode JSON to Binary

```bash
# Encode JSONL to binary format
copybook encode --format fixed --codepage cp037 customer.cpy input.jsonl output.bin

# With error handling
copybook encode --format fixed --codepage cp037 --fail-fast --max-errors 5 \
  customer.cpy input.jsonl output.bin
```

### Verify Data Quality

```bash
# Validate binary data against schema
copybook verify --format fixed --codepage cp037 customer.cpy data.bin
```

## Command Reference

| Command | Purpose |
|---------|---------|
| `parse` | Parse copybook to JSON schema |
| `inspect` | Human-readable layout display |
| `decode` | Convert binary data to JSONL |
| `encode` | Convert JSONL to binary format |
| `verify` | Validate data without conversion |

## Supported Formats

- **Codepages**: CP037, CP273, CP500, CP1047, CP1140, ASCII
- **Record Formats**: Fixed-length, Variable (RDW)
- **Output**: JSON Lines (JSONL) for streaming compatibility

## API Documentation

See the [full documentation](https://docs.rs/copybook-cli) for detailed CLI reference.

## Performance & Production Use

For performance specifications, benchmarks, and production deployment guidance, see the [main copybook-rs README](https://github.com/EffortlessMetrics/copybook-rs#readme).

## License

Licensed under AGPL-3.0-or-later. See [LICENSE](../LICENSE) for details.