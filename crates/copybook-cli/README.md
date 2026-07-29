<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-cli

CLI for working with COBOL copybooks and mainframe record data.

`copybook` supports parsing, inspection, decode/encode, verification, and support-matrix checks.

## What it does

- Parse copybooks into machine-readable layout JSON.
- Decode fixed-length and RDW binaries to JSONL with codepage and type controls.
- Encode JSONL back to COBOL binary layout.
- Verify input data and enforce policy/quality checks.
- Inspect feature support and validate compatibility flags.

## Commands

| Command | Purpose |
|---|---|
| `parse` | Parse copybook to JSON schema |
| `inspect` | Show formatted copybook layout |
| `decode` | Convert copybook-encoded binary to JSONL |
| `encode` | Convert JSONL to copybook binary |
| `verify` | Validate data against schema only |
| `support` | Show or check supported features |
| `determinism` | Verify decode/encode/round-trip determinism |

## Quick start

```bash
copybook parse fixtures/copybooks/simple.cpy --output schema.json
copybook decode fixtures/copybooks/simple.cpy fixtures/data/simple.bin \
  --output demo.jsonl --format fixed --codepage cp037
copybook encode fixtures/copybooks/simple.cpy demo.jsonl \
  --output demo.bin --format fixed --codepage cp037
copybook verify fixtures/copybooks/simple.cpy demo.bin \
  --format fixed --codepage cp037
```

### Feature checks

```bash
copybook support
copybook support --json
copybook support --check level-88
```

### Field projection

```bash
# Decode only selected fields
copybook decode schema.cpy data.bin --output selected.jsonl \
  --format fixed --codepage cp037 --select "CUSTOMER-ID,BALANCE"
```

### Dialect lever

```bash
# IBM Enterprise COBOL (zero-tolerant ODO)
copybook decode schema.cpy data.bin --output data.jsonl \
  --format fixed --codepage cp037 --dialect 0

# Micro Focus COBOL (one-tolerant ODO)
copybook parse schema.cpy --dialect 1
```

### Determinism validation

```bash
# Verify round-trip determinism
copybook determinism round-trip --format fixed --codepage cp037 schema.cpy data.bin

# JSON output for CI integration
copybook determinism decode --output json --format fixed --codepage cp037 schema.cpy data.bin
```

## Installation

```bash
cargo install copybook-cli@0.5.0 --locked
```

For source builds:

```bash
cargo build -p copybook-cli --release
```

## Documentation

`copybook-cli` is intentionally a binary-only package, so it does not expose a library rustdoc target on docs.rs. Use the maintained [CLI reference](../../docs/CLI_REFERENCE.md) for commands, flags, environment/config precedence, outputs, and exit behavior. Rust library users should depend on the canonical [`copybook`](../copybook/README.md) facade instead of CLI internals.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
