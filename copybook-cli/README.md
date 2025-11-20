# copybook-cli

CLI for parsing/decoding/encoding COBOL copybooks at high throughput.

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

## Operational Controls

- `--strict-policy` — enforce policy warnings as failures.
- `--no-strict-policy` — temporarily disable an environment default for one invocation.
- Precedence: `--strict-policy` → `--no-strict-policy` → `COPYBOOK_STRICT_POLICY`.
- Telemetry logs are emitted on `stderr`, keeping machine-readable output (JSON, CSV) on `stdout`.

Example diagnostic emitted on `stderr` when compatibility mode is triggered:

```text
WARN  copybook::commands::decode compat: prefer --preserve-zoned-encoding when using --preferred-zoned-encoding (future strict mode will fail) invocation_id=pid4242-ts1697052800000000000 code_tag=CBKE code=3 family=policy precedence_rank=2 subcode=401 effective_exit=0 op=decode path="data/input.bin"
```

Strict policy escalation example:

```text
ERROR copybook::commands::decode --preferred-zoned-encoding requires --preserve-zoned-encoding in strict mode invocation_id=pid4242-ts1697052800000000000 code_tag=CBKE code=3 family=policy precedence_rank=2 subcode=401 effective_exit=3 op=decode path="data/input.bin"
```

## Supported Formats

- **Codepages**: CP037, CP273, CP500, CP1047, CP1140, ASCII
- **Record Formats**: Fixed-length, Variable (RDW)
- **Output**: JSON Lines (JSONL) for streaming compatibility

## API Documentation

See the [full documentation](https://docs.rs/copybook-cli) for detailed CLI reference.

## Performance & Production Use

For performance specifications, benchmarks, and production deployment guidance, see the [main copybook-rs README](https://github.com/EffortlessMetrics/copybook-rs#readme).

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
