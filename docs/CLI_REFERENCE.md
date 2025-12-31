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
- `--strict` - Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

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
- `--strict` - Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

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

**Zoned Decimal Encoding (Experimental):**
- `--preserve-zoned-encoding` - Preserve original encoding format (ASCII/EBCDIC zones) for round-trip fidelity
- `--preferred-zoned-encoding <FORMAT>` - Preferred format for ambiguous detection: ascii, ebcdic, auto (default: auto)

**Error Handling:**
- `--strict` - Stop on first error (default: lenient)
- `--max-errors <N>` - Maximum errors before stopping
- `--verbose` - Verbose error reporting

**Parsing Options:**
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

**Output Control:**
- `--emit-filler` - Include FILLER fields in output
- `--emit-meta` - Add metadata keys (`schema_fingerprint`, `record_index`, `offset`, `length`)
- `--emit-raw <MODE>` - Capture raw bytes (`raw_b64`): off, record, field, record+rdw (default: off)
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

# Decode with zoned encoding preservation (when CLI flags are implemented)
copybook decode financial.cpy mainframe-data.bin \
  --format fixed \
  --codepage cp037 \
  --preserve-zoned-encoding \
  --output preserved.jsonl

# Decode with preferred encoding fallback
copybook decode legacy.cpy mixed-data.bin \
  --format fixed \
  --preferred-zoned-encoding ebcdic \
  --output detected.jsonl
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
- `--use-raw` - Use raw bytes from `raw_b64` when available
- `--bwz-encode` - Encode zero values as spaces for BLANK WHEN ZERO fields

**Zoned Decimal Encoding (Experimental):**
- `--zoned-encoding-override <FORMAT>` - Override zoned decimal format: ascii, ebcdic (default: respect preserved formats)

**Error Handling:**
- `--fail-fast` - Stop on first error (default: true)
- `--strict` - Enable strict mode validation
- `--max-errors <N>` - Maximum errors before stopping
- `--verbose` - Verbose error reporting

**Parsing Options:**
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

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

# Encode with zoned encoding override (when CLI flags are implemented)
copybook encode financial.cpy data.jsonl \
  --format fixed \
  --zoned-encoding-override ascii \
  --output ascii-zones.bin

# Encode respecting preserved formats (default behavior)
copybook encode financial.cpy preserved.jsonl \
  --format fixed \
  --output roundtrip.bin
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
- `--strict` - Enable strict mode validation
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility (affects copybook parsing only, not data validation)
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)
- `--report <FILE>` - Output verification report (JSON format)
- `--max-errors <N>` - Maximum errors before stopping
- `--sample <N>` - Number of sample records to include in report (default: 5)

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

## Dialect Lever

The dialect lever controls how `min_count` is interpreted for `OCCURS DEPENDING ON` (ODO) arrays. Different COBOL dialects have different requirements for the minimum bound in ODO declarations.

### Dialect Modes

| Mode | Flag | Description | Behavior |
|------|------|-------------|----------|
| **Normative** (default) | `--dialect n` | Strict enforcement | `min_count` is enforced as declared |
| **Zero-Tolerant** | `--dialect 0` | IBM Enterprise mode | `min_count` is ignored (always treated as 0) |
| **One-Tolerant** | `--dialect 1` | Micro Focus mode | `min_count` is clamped to 1 (min ≥ 1) |

### When to Use

- **Normative (`n`)**: Default behavior, suitable for most use cases
- **Zero-Tolerant (`0`)**: For IBM Enterprise COBOL copybooks where `min_count` should always be 0
- **One-Tolerant (`1`)**: For Micro Focus COBOL copybooks where minimum count is always at least 1

### Configuration

**CLI Flag** (highest precedence):
```bash
copybook decode schema.cpy data.bin --dialect 0 --format fixed --output data.jsonl
```

**Environment Variable**:
```bash
export COPYBOOK_DIALECT=0
copybook decode schema.cpy data.bin --format fixed --output data.jsonl
```

**Precedence Order**:
1. CLI `--dialect` flag (highest priority)
2. `COPYBOOK_DIALECT` environment variable
3. Default value (`n` - Normative)

### Examples

```bash
# Use normative dialect (default)
copybook parse schema.cpy --dialect n

# Use zero-tolerant dialect for IBM Enterprise COBOL
copybook decode schema.cpy data.bin --format fixed --codepage cp037 --dialect 0

# Use one-tolerant dialect for Micro Focus COBOL
copybook encode schema.cpy data.jsonl output.bin --format fixed --dialect 1

# Environment variable override
export COPYBOOK_DIALECT=0
copybook verify schema.cpy data.bin --format fixed

# CLI flag takes precedence over environment variable
export COPYBOOK_DIALECT=0
copybook decode schema.cpy data.bin --format fixed --dialect 1  # Uses one-tolerant
```

### COBOL Copybook Impact

```cobol
      * Example: ODO array with min_count > 0
       01  RECORD.
           05  COUNTER      PIC 9(3).
           05  ITEMS        OCCURS 1 TO 10 DEPENDING ON COUNTER
                            PIC X(10).
```

**Behavior by Dialect**:
- `--dialect n`: `min_count=1` enforced (counter must be ≥ 1)
- `--dialect 0`: `min_count` ignored (counter can be 0-10)
- `--dialect 1`: `min_count=1` enforced (counter must be ≥ 1)

```cobol
      * Example: ODO array with min_count = 0
       01  RECORD.
           05  COUNTER      PIC 9(3).
           05  ITEMS        OCCURS 0 TO 10 DEPENDING ON COUNTER
                            PIC X(10).
```

**Behavior by Dialect**:
- `--dialect n`: `min_count=0` allowed (counter can be 0-10)
- `--dialect 0`: `min_count=0` allowed (counter can be 0-10)
- `--dialect 1`: `min_count` raised to 1 (counter must be ≥ 1)

### Available on All Commands

The `--dialect` flag is supported on all copybook-processing commands:
- `parse`
- `inspect`
- `decode`
- `encode`
- `verify`

## Environment Variables

- `COPYBOOK_LOG_LEVEL` - Set default log level
- `COPYBOOK_THREADS` - Set default thread count
- `COPYBOOK_CODEPAGE` - Set default codepage
- `COPYBOOK_DIALECT` - Set default dialect mode (n, 0, or 1)

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

## Comment Modes

### Default (allow inline comments)
Supports COBOL-2002 inline comments (`*>`) for modern copybooks.

- **Inline comments**: `*>` comments allowed anywhere on a line after column 7
- **End-of-line comments**: `*>` consumes the rest of the line
- **Backward compatible**: Still supports traditional full-line comments (`*` in column 7)

### `--strict-comments`
Enforces COBOL-85 compatibility by disabling inline comments.

- **Inline comments disabled**: `*>` treated as regular tokens, causing parse errors if used
- **COBOL-85 compatible**: Only traditional full-line comments (`*` in column 7) are supported
- **Legacy copybooks**: Use this flag for strict compliance with older COBOL standards
- **Library equivalent**: Maps to `ParseOptions::allow_inline_comments = false` when using the library API

### Examples
```bash
# Parse & inspect copybook (strict validation)
copybook inspect --strict path/to/schema.cpy

# Parse & inspect copybook (lenient default)
copybook inspect path/to/schema.cpy

# Parse copybook (strict validation)
copybook parse --strict path/to/schema.cpy

# Parse copybook (lenient default)
copybook parse path/to/schema.cpy

# Parse copybook with COBOL-85 comment compatibility
copybook parse --strict-comments path/to/legacy-schema.cpy

# Parse copybook with both strict validation and strict comments
copybook parse --strict --strict-comments path/to/legacy-schema.cpy

# Decode with strict comment mode for legacy copybooks
copybook decode legacy-schema.cpy data.bin --format fixed --strict-comments --output data.jsonl
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

**Envelope (always present):**
- `schema` - JSONL schema version (currently `copybook.v1`)
- `record_index` - Zero-based record number
- `codepage` - Code page identifier used for decoding
- `fields` - Object containing decoded field values

**Metadata (--emit-meta):**
- `schema_fingerprint` - Schema fingerprint (SHA-256)
- `offset` - Byte offset in file
- `length` - Record length in bytes

**Raw Bytes (--emit-raw):**
- `raw_b64` - Base64-encoded raw record bytes (record/record+rdw modes)
- `<field>_raw_b64` - Base64 payload for individual fields (field mode)
- Enables byte-perfect round trips when re-encoding

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
