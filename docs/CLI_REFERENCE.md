# CLI Reference

Complete reference for the copybook command-line interface.

## Synopsis

```
copybook <COMMAND> [OPTIONS]
```

## Commands

### parse
Parse a COBOL copybook and output schema JSON.

```
copybook parse <COPYBOOK> [OPTIONS]
```

**Arguments:**
- `<COPYBOOK>` - Path to COBOL copybook file

**Options:**
- `--output <FILE>` - Output file (default: stdout)
- `--pretty` - Pretty-print JSON output
- `--validate` - Validate schema after parsing

**Examples:**
```bash
# Parse copybook to stdout
copybook parse customer.cpy

# Parse and save to file
copybook parse customer.cpy --output customer-schema.json

# Parse with pretty formatting
copybook parse customer.cpy --pretty
```

### inspect
Display human-readable copybook layout information.

```
copybook inspect <COPYBOOK> [OPTIONS]
```

**Arguments:**
- `<COPYBOOK>` - Path to COBOL copybook file

**Options:**
- `--codepage <CP>` - Character encoding (default: cp037)
- `--format <FORMAT>` - Output format: table, json, yaml (default: table)
- `--show-offsets` - Include byte offsets in output
- `--show-lengths` - Include field lengths in output

**Binary widths:** `≤4 → 16-bit`, `5–9 → 32-bit`, `10–18 → 64-bit`.

**Examples:**
```bash
# Basic layout inspection
copybook inspect customer.cpy

# Show detailed layout with offsets
copybook inspect customer.cpy --show-offsets --show-lengths

# Output as JSON
copybook inspect customer.cpy --format json
```

### decode
Convert binary data to JSONL using copybook schema.

```
copybook decode <COPYBOOK> <DATA> [OPTIONS]
```

**Arguments:**
- `<COPYBOOK>` - Path to COBOL copybook file
- `<DATA>` - Path to binary data file

**Options:**

**Output:**
- `--output <FILE>` - Output file (default: stdout)
- `--format <FORMAT>` - Record format: fixed, rdw (required)

**Character Encoding:**
- `--codepage <CP>` - Character encoding: cp037, cp273, cp500, cp1047, cp1140, ascii (default: cp037)
- `--on-decode-unmappable <POLICY>` - Handle unmappable chars: error, replace, skip (default: error)

**Error Handling:**
- `--strict` - Stop on first error (default: lenient)
- `--max-errors <N>` - Maximum errors before stopping
- `--verbose` - Verbose error reporting

**Output Control:**
- `--emit-filler` - Include FILLER fields in output
- `--emit-meta` - Include metadata fields (__schema_id, __record_index, etc.)
- `--emit-raw <MODE>` - Capture raw bytes: off, record, field, record+rdw (default: off)
- `--json-number <MODE>` - JSON number format: lossless, native (default: lossless)

**Performance:**
- `--threads <N>` - Number of worker threads (default: CPU count)

**Examples:**
```bash
# Basic decode
copybook decode customer.cpy data.bin --format fixed --output data.jsonl

# Decode with EBCDIC CP037
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --output data.jsonl

# Decode RDW format with error tolerance
copybook decode customer.cpy data.bin \
  --format rdw \
  --max-errors 100 \
  --output data.jsonl

# Decode with metadata and raw capture
copybook decode customer.cpy data.bin \
  --format fixed \
  --emit-meta \
  --emit-raw record \
  --output data.jsonl

# Parallel processing
copybook decode customer.cpy large-data.bin \
  --format fixed \
  --threads 8 \
  --output data.jsonl
```

### encode
Convert JSONL data to binary using copybook schema.

```
copybook encode <COPYBOOK> <JSONL> [OPTIONS]
```

**Arguments:**
- `<COPYBOOK>` - Path to COBOL copybook file
- `<JSONL>` - Path to JSONL input file

**Options:**

**Output:**
- `--output <FILE>` - Output file (required)
- `--format <FORMAT>` - Record format: fixed, rdw (required)

**Character Encoding:**
- `--codepage <CP>` - Character encoding: cp037, cp273, cp500, cp1047, cp1140, ascii (default: cp037)

**Encoding Options:**
- `--use-raw` - Use raw bytes from __raw_b64 fields when available
- `--bwz-encode` - Encode zero values as spaces for BLANK WHEN ZERO fields

**Error Handling:**
- `--strict` - Stop on first error (default: lenient)
- `--max-errors <N>` - Maximum errors before stopping
- `--verbose` - Verbose error reporting

**Performance:**
- `--threads <N>` - Number of worker threads (default: CPU count)

**Examples:**
```bash
# Basic encode
copybook encode customer.cpy data.jsonl \
  --format fixed \
  --output data.bin

# Encode with raw byte preservation
copybook encode customer.cpy data.jsonl \
  --format rdw \
  --use-raw \
  --output data.bin

# Encode with BLANK WHEN ZERO policy
copybook encode customer.cpy data.jsonl \
  --format fixed \
  --bwz-encode \
  --output data.bin
```

### verify
Verify data integrity and schema compliance.

```
copybook verify <COPYBOOK> <DATA> [OPTIONS]
```

**Arguments:**
- `<COPYBOOK>` - Path to COBOL copybook file
- `<DATA>` - Path to binary data file

**Options:**
- `--format <FORMAT>` - Record format: fixed, rdw (required)
- `--codepage <CP>` - Character encoding (default: cp037)
- `--report <FILE>` - Output verification report (JSON format)
- `--summary` - Show summary statistics only

**Examples:**
```bash
# Basic verification
copybook verify customer.cpy data.bin --format fixed --codepage cp037

# Generate detailed report
copybook verify customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --report verification-report.json
# Exit codes: 0 = ok, 3 = validation errors, 2 = fatal (I/O/schema)
# Report schema: docs/VERIFY_REPORT.schema.json

# Quick summary
copybook verify customer.cpy data.bin --format fixed --summary
```

## Global Options

These options are available for all commands:

- `--help` - Show help information
- `--version` - Show version information
- `--config <FILE>` - Load configuration from file
- `--log-level <LEVEL>` - Set logging level: error, warn, info, debug, trace (default: info)

## Configuration File

You can specify default options in a configuration file:

```toml
# copybook.toml
[decode]
codepage = "cp037"
format = "fixed"
emit_meta = true
threads = 4

[encode]
codepage = "cp037"
format = "fixed"
use_raw = true
```

Use with `--config copybook.toml`.

## Environment Variables

- `COPYBOOK_LOG_LEVEL` - Set default log level
- `COPYBOOK_THREADS` - Set default thread count
- `COPYBOOK_CODEPAGE` - Set default codepage

## Validation Modes

### `--strict`
Enforces normative validation and hard failures.

- **ODO (OCCURS DEPENDING ON)**: Counter must exist, precede the array, and be in range. Violations → error.
- **REDEFINES**: Single unambiguous view may encode; ambiguity → error.
- **Edited PIC**: Always an error with `CBKP051_UNSUPPORTED_EDITED_PIC` (e.g., `ZZ9.99`, trailing sign, `CR`/`DB`, `B` blanks).
- **Fixed-form**: Column-7 continuation and sequence areas handled; tokens after the terminating `.` on the same line are ignored.

### Default (lenient)
Designed for exploration and ingestion of imperfect copybooks.

- **ODO** out-of-range: clamped with a warning in encoder paths; schema still loads.
- **REDEFINES** ambiguity: warn and refuse encoding, but schema loads.
- **Edited PIC**: still a hard error (unsupported).

### Examples
```bash
# Parse & inspect copybook (strict)
copybook inspect --strict path/to/schema.cpy

# Parse & inspect copybook (lenient default)
copybook inspect path/to/schema.cpy

# Parse copybook (strict)
copybook parse --strict path/to/schema.cpy

# Parse copybook (lenient default)
copybook parse path/to/schema.cpy
```

## Exit Codes

- `0` - Success (warnings allowed)
- `1` - Completed with errors
- `2` - Fatal error (parse failure, invalid arguments)

## Character Encodings

### EBCDIC Code Pages

| Code Page | Description | Regions |
|-----------|-------------|---------|
| cp037 | US/Canada EBCDIC | North America |
| cp273 | Germany/Austria EBCDIC | Central Europe |
| cp500 | International EBCDIC | International |
| cp1047 | Open Systems EBCDIC | Unix/Linux mainframes |
| cp1140 | US/Canada Euro EBCDIC | North America with Euro |

### ASCII Mode
- `ascii` - Transparent 8-bit ASCII (not Windows-1252)
- Uses ASCII overpunch sign table for zoned decimals
- No character conversion applied

### Binary Widths
Binary field sizes are determined by PIC digits: ≤4→16b, 5–9→32b, 10–18→64b

## Record Formats

### Fixed-Length Records
- Constant LRECL (Logical Record Length)
- Records stored back-to-back
- Length determined by copybook schema
- Use `--format fixed`

### Variable-Length Records (RDW)
- 4-byte Record Descriptor Word header
- Bytes 0-1: big-endian data length (excluding RDW)
- Bytes 2-3: reserved (should be 0x0000)
- Use `--format rdw`

## JSON Output Format

### Field Ordering
- Fields output in schema order (pre-order traversal)
- Groups before children, declaration order within groups
- REDEFINES: all views in declaration order

### Numeric Representation

**Lossless Mode (default):**
- Packed/zoned decimals as strings with fixed scale
- Binary integers as JSON numbers (up to 64-bit)
- Preserves exact precision

**Native Mode:**
- Use JSON numbers where possible
- May lose precision for large decimals
- Better performance for numeric processing

### Special Fields

**Metadata (--emit-meta):**
- `__schema_id` - Schema fingerprint (SHA-256)
- `__record_index` - Zero-based record number
- `__offset` - Byte offset in file
- `__length` - Record length in bytes

**Raw Bytes (--emit-raw):**
- `__raw_b64` - Base64-encoded raw bytes
- Used for round-trip fidelity
- Modes: record, field, record+rdw

**FILLER Fields (--emit-filler):**
- `_filler_<offset>` - FILLER field at byte offset
- Normally omitted from output
- Useful for debugging layout issues

## Performance Tuning

### Thread Count
- Default: CPU core count
- Increase for I/O-bound workloads
- Decrease if memory-constrained
- Output remains deterministic regardless of thread count

### Memory Usage
- Streaming architecture maintains bounded memory
- Typical usage: <256 MiB for multi-GB files
- Memory scales with thread count and record size

### Throughput Targets
- DISPLAY-heavy data: ≥80 MB/s
- COMP-3-heavy data: ≥40 MB/s
- Actual performance depends on hardware and data characteristics

The codec uses an optimized fast path for COMP-3 processing by default, providing enhanced performance with no behavior changes.

## Common Patterns

### ETL Pipeline Integration
```bash
# Extract mainframe data
copybook decode schema.cpy mainframe-data.bin \
  --format fixed \
  --codepage cp037 \
  --emit-meta \
  --threads 8 \
  --output extracted.jsonl

# Transform with jq or other tools
jq '.customer_name = (.customer_name | ascii_upcase)' extracted.jsonl > transformed.jsonl

# Load back to mainframe format
copybook encode schema.cpy transformed.jsonl \
  --format fixed \
  --codepage cp037 \
  --output mainframe-data-new.bin
```

### Data Quality Validation
```bash
# Strict validation
copybook decode schema.cpy data.bin \
  --format fixed \
  --strict \
  --output /dev/null

# Lenient with error reporting
copybook decode schema.cpy data.bin \
  --format fixed \
  --max-errors 1000 \
  --verbose \
  --output validated.jsonl 2> errors.log
```

### Round-Trip Testing
```bash
# Decode with raw capture
copybook decode schema.cpy original.bin \
  --format fixed \
  --emit-raw record \
  --output data.jsonl

# Encode with raw preservation
copybook encode schema.cpy data.jsonl \
  --format fixed \
  --use-raw \
  --output roundtrip.bin

# Verify identical
diff original.bin roundtrip.bin
```

## Troubleshooting

### Common Issues

**"No such file or directory"**
- Check file paths are correct
- Ensure files are readable
- Use absolute paths if needed

**"Invalid record format"**
- Specify `--format fixed` or `--format rdw`
- Check data file format matches expectation

**"Unsupported COBOL feature"**
- See [ERROR_CODES.md](ERROR_CODES.md) for details
- Modify copybook to use supported features

**"Character encoding errors"**
- Verify correct `--codepage` setting
- Use `--on-decode-unmappable replace` for tolerance
- Check for binary data in text fields

### Getting Help

1. Use `copybook <command> --help` for command-specific help
2. Check error codes in [ERROR_CODES.md](ERROR_CODES.md)
3. Use `--verbose` for detailed diagnostics
4. Test with small data samples first
5. Refer to examples in [README.md](../README.md)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
