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

## Installation

```bash
cargo install --path . -p copybook-cli
# Or build locally:
cargo build -p copybook-cli --release
```

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
