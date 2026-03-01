# copybook-cli-determinism

Determinism command execution for copybook CLI workflows.

## Overview

This crate implements the `determinism` subcommand for the copybook CLI. It parses CLI arguments
and orchestrates decode, encode, and round-trip determinism checks, producing formatted output
(text or JSON) that validates whether codec operations produce identical results across runs.

## Usage

```rust
use copybook_cli_determinism::DeterminismCommand;
use clap::Parser;

// Typically used via the CLI:
//   copybook determinism decode --format fixed --codepage cp037 schema.cpy data.bin
//   copybook determinism encode --format fixed --codepage cp037 schema.cpy input.jsonl
//   copybook determinism round-trip --format fixed --codepage cp037 schema.cpy data.bin
```

## License

AGPL-3.0-or-later
