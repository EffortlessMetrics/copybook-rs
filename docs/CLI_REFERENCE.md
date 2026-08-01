<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# CLI Reference

Complete reference for the copybook command-line interface.

## Synopsis

```
copybook [GLOBAL OPTIONS] <COMMAND> [OPTIONS]
```

Global options (see [Global Options](#global-options)) must be placed before the subcommand.

## Commands

### parse
Parse a COBOL copybook and output schema JSON.

```
copybook parse <COPYBOOK> [OPTIONS]
```

**Arguments:**
- `<COPYBOOK>` - Path to COBOL copybook file

**Options:**
- `-o, --output <FILE>` - Output file (default: stdout)
- `--strict` - Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

**Examples:**
```bash
# Parse copybook to stdout
copybook parse customer.cpy

# Parse and save to file
copybook parse customer.cpy --output customer-schema.json

# Parse with strict validation
copybook parse customer.cpy --strict
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
- `--strict` - Enforce normative validation (ODO bounds/order, REDEFINES ambiguity as errors)
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

**Binary widths:** `≤4 → 16-bit`, `5–9 → 32-bit`, `10–18 → 64-bit`.

**Examples:**
```bash
# Basic layout inspection
copybook inspect customer.cpy

# Inspect with strict validation
copybook inspect customer.cpy --strict

# Inspect with an alternate codepage
copybook inspect customer.cpy --codepage cp500
```

**Output:** a header block followed by one row per field. The `Type` column
reproduces the source PIC clause (`digits` are split back into integer and
decimal positions), and the `Details` column carries the clauses that are not
part of the picture — `OCCURS`, `REDEFINES`, `SYNCHRONIZED`, and
`BLANK WHEN ZERO`. The `Details` column is omitted when no field has any.

```text
Copybook Layout
===============
Codepage: CP037 (EBCDIC Code Page 037 (US/Canada))
Fixed LRECL: 31 bytes
Fields: 4

Field Path                       Offset   Length   Type
------------------------------------------------------------------------
CUSTOMER-RECORD                  0        31       GROUP
CUSTOMER-RECORD.CUST-ID          0        6        PIC 9(6)
CUSTOMER-RECORD.CUST-NAME        6        20       PIC X(20)
CUSTOMER-RECORD.CUST-BALANCE     26       5        PIC S9(7)V9(2) COMP-3
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
- `-o, --output <FILE>` - Output JSONL file (required; use `-` for stdout)
- `--format <FORMAT>` - Record format: fixed, rdw (required)
- `--select <FIELD[,FIELD...]>` - Include only specific fields in output (comma-separated or repeated); ODO counters and parent groups are included automatically

**Character Encoding:**
- `--codepage <CP>` - Character encoding: cp037, cp273, cp500, cp1047, cp1140, ascii (default: cp037)
- `--on-decode-unmappable <POLICY>` - Handle unmappable chars: error, replace, skip (default: error)

**Zoned Decimal Encoding (Experimental):**
- `--preserve-zoned-encoding` - Preserve original encoding format (ASCII/EBCDIC zones) for round-trip fidelity
- `--preferred-zoned-encoding <FORMAT>` - Preferred format when neither preserved nor overridden: preferred, ascii, ebcdic, auto (default: preferred)

**Floating Point:**
- `--float-format <FORMAT>` - COMP-1/COMP-2 binary format: ieee-be, ibm-hex (default: ieee-be)

**Error Handling:**
- `--strict` - Enable strict mode validation (default: false for lenient mode)
- `--fail-fast` - Stop on first error (default: false)
- `--max-errors <N>` - Maximum errors before stopping

**Parsing Options:**
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

**Output Control:**
- `--emit-filler` - Include FILLER fields in output
- `--emit-meta` - Add metadata keys (`schema_fingerprint`, `record_index`, `offset`, `length`)
- `--emit-raw <MODE>` - Capture raw bytes (`raw_b64`): off, record, field, record+rdw (default: off)
- `--json-number <MODE>` - JSON number format: lossless, native (default: lossless)

**Performance:**
- `--threads <N>` - Number of worker threads (default: 1)

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

# Decode with zoned encoding preservation
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

# Decode only selected fields
copybook decode customer.cpy data.bin \
  --format fixed \
  --select "CUSTOMER-ID,BALANCE" \
  --output selected.jsonl
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
- `-o, --output <FILE>` - Output binary file (required; use `-` for stdout)
- `--format <FORMAT>` - Record format: fixed, rdw (required)
- `--select <FIELD[,FIELD...]>` - Validate only specific fields during encoding (comma-separated or repeated); ODO counters and parent groups are included automatically

**Character Encoding:**
- `--codepage <CP>` - Character encoding: cp037, cp273, cp500, cp1047, cp1140, ascii (default: cp037)

**Encoding Options:**
- `--use-raw` - Use raw bytes from `raw_b64` (or legacy `__raw_b64`) when available
- `--bwz-encode` - Encode zero values as spaces for BLANK WHEN ZERO fields
- `--coerce-numbers` - Coerce non-string JSON numbers to strings before encoding

**Zoned Decimal Encoding (Experimental):**
- `--zoned-encoding-override <FORMAT>` - Override zoned decimal format: ascii, ebcdic (default: respect preserved formats)

**Floating Point:**
- `--float-format <FORMAT>` - COMP-1/COMP-2 binary format: ieee-be, ibm-hex (default: ieee-be)

**Error Handling:**
- `--fail-fast` - Stop on first error (default: true)
- `--strict` - Enable strict mode validation (default: false for lenient mode)
- `--max-errors <N>` - Maximum errors before stopping

**Parsing Options:**
- `--strict-comments` - Disable inline comments (*>) - enforce COBOL-85 compatibility
- `--dialect <MODE>` - Dialect mode: n (normative), 0 (zero-tolerant), 1 (one-tolerant) (default: n)

**Performance:**
- `--threads <N>` - Number of worker threads (default: 1)

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

# Encode with zoned encoding override
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
- `--select <FIELD[,FIELD...]>` - Validate only specific fields (comma-separated or repeated); ODO counters and parent groups are included automatically

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

# Validate only selected fields
copybook verify customer.cpy data.bin --format fixed --select "CUSTOMER-ID,BALANCE"
```

### support
Display the COBOL support matrix or check feature compatibility.

```
copybook support [OPTIONS]
```

**Options:**
- `--format <FORMAT>` - Output format: table, json (default: table)
- `--check <FEATURE_ID>` - Check support for a specific feature ID, e.g. `level-88`, `occurs-depending`, `edited-pic` (exit 0 only if supported)
- `--status <FILTER>` - Filter by support status: supported, partial, planned, not-planned
- `--with-governance` - Include governance and feature-flag linkage metadata

**Examples:**
```bash
# Show the full support matrix
copybook support

# Support matrix as JSON
copybook support --format json

# Check a specific feature
copybook support --check level-88

# Show only supported features with governance metadata
copybook support --status supported --with-governance
```

### determinism
Validate determinism of encode/decode operations (byte-identical output across runs).

```
copybook determinism <MODE> <COPYBOOK> <INPUT> [OPTIONS]
```

**Modes:**
- `decode <COPYBOOK> <DATA>` - Check decode determinism (binary -> JSON)
- `encode <COPYBOOK> <JSON>` - Check encode determinism (JSON -> binary; first line is used)
- `round-trip <COPYBOOK> <DATA>` - Check full round-trip determinism (binary -> JSON -> binary -> JSON)

**Options (shared by all modes):**
- `--format <FORMAT>` - Record format: fixed, rdw (default: fixed)
- `--codepage <CP>` - Character encoding (default: cp037)
- `--json-number <MODE>` - JSON number format: lossless, native (default: lossless)
- `--emit-meta` - Include metadata in JSON output
- `--output <FORMAT>` - Output rendering: human, json (default: human)
- `--max-diffs <N>` - Maximum number of byte diffs to report

**Exit codes:** 0 = deterministic (hashes match), 2 = non-deterministic (drift detected), 3 = codec/usage error.

**Examples:**
```bash
# Check decode determinism
copybook determinism decode customer.cpy data.bin --format fixed

# Check round-trip determinism with JSON output for CI
copybook determinism round-trip customer.cpy data.bin --output json
```

### audit
Enterprise audit system for regulatory compliance (SOX, HIPAA, GDPR, PCI DSS), performance auditing, security monitoring, and data lineage tracking.

**Note:** The `audit` subcommand is only available when the CLI is built with the non-default `audit` cargo feature (`cargo build -p copybook-cli --features audit`).

```
copybook audit <SUBCOMMAND> [OPTIONS]
```

**Subcommands:**
- `report` - Generate comprehensive audit reports
- `validate` - Validate compliance against regulatory frameworks
- `lineage` - Analyze data lineage and transformation impact

**Examples:**
```bash
copybook audit validate --compliance sox,gdpr schema.cpy
copybook audit report --include-performance schema.cpy data.bin -o report.json
copybook audit lineage source.cpy --source-system mainframe -o lineage.json
```

## Global Options

These options are defined at the top level and must be placed **before** the subcommand (e.g. `copybook -v decode ...`):

- `-h, --help` - Show help information
- `-V, --version` - Show version information
- `-v, --verbose` - Enable verbose logging
- `--strict-policy` - Enforce policy checks. Precedence: `--strict-policy` > `--no-strict-policy` > `COPYBOOK_STRICT_POLICY`
- `--no-strict-policy` - Disable strict checks for this run, even if `COPYBOOK_STRICT_POLICY=1`

**Feature Flags:**
- `--enable-features <FEATURE[,FEATURE...]>` - Enable specific feature flags (comma-separated)
- `--disable-features <FEATURE[,FEATURE...]>` - Disable specific feature flags (takes precedence over `--enable-features` and environment variables)
- `--enable-category <CATEGORY>` - Enable all features in a category: experimental, enterprise, performance, debug, testing
- `--disable-category <CATEGORY>` - Disable all features in a category
- `--feature-flags-config <PATH>` - Load feature flags from a TOML or JSON configuration file
- `--list-features` - List all available feature flags and their status, then exit

**Metrics (only when built with the non-default `metrics` cargo feature):**
- `--metrics-listen <ADDR>` - Expose Prometheus metrics at this address (e.g. `0.0.0.0:9300`)
- `--metrics-grace-ms <MS>` - Delay after run completion so scrapes can observe final metrics (default: 0)

## Feature Flags Configuration File

Feature flags can be loaded from a configuration file with `--feature-flags-config <PATH>`. The file can be in TOML or JSON format:

```toml
# feature-flags.toml
[feature_flags]
enabled = ["sign_separate", "verbose_logging"]
disabled = ["lru_cache"]
```

```json
{
  "feature_flags": {
    "enabled": ["sign_separate", "verbose_logging"],
    "disabled": ["lru_cache"]
  }
}
```

CLI flags (`--enable-features`, `--disable-features`, `--enable-category`, `--disable-category`) are applied on top of the config file. Use `copybook --list-features` to see all available flags and their current status.

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
copybook decode schema.cpy data.bin --format fixed --codepage cp037 --dialect 0 --output data.jsonl

# Use one-tolerant dialect for Micro Focus COBOL
copybook encode schema.cpy data.jsonl --output output.bin --format fixed --dialect 1

# Environment variable override
export COPYBOOK_DIALECT=0
copybook verify schema.cpy data.bin --format fixed

# CLI flag takes precedence over environment variable
export COPYBOOK_DIALECT=0
copybook decode schema.cpy data.bin --format fixed --dialect 1 --output data.jsonl  # Uses one-tolerant
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

- `COPYBOOK_DIALECT` - Set default dialect mode (n, 0, or 1); overridden by the `--dialect` flag
- `COPYBOOK_STRICT_POLICY` - Enforce policy checks (`1`, `true`, `yes`, or `on`); overridden by `--strict-policy`/`--no-strict-policy`
- `COPYBOOK_FF_<FEATURE>` - Enable a feature flag by name, e.g. `COPYBOOK_FF_SIGN_SEPARATE=1`
- `RUST_LOG` - Tracing filter for log output (overrides the default `warn`, or `debug` with `-v/--verbose`)

## Validation Modes

### `--strict`
Enforces normative validation and hard failures.

- **ODO (OCCURS DEPENDING ON)**: Counter must exist, precede the array, and be in range. Violations → error.
- **REDEFINES**: Single unambiguous view may encode; ambiguity → error.
- **Edited PIC**: Fully supported (E1/E2/E3); only Space (`B`) insertion returns `CBKP051_UNSUPPORTED_EDITED_PIC`.
- **Fixed-form**: Column-7 continuation and sequence areas handled; tokens after the terminating `.` on the same line are ignored.

### Default (lenient)
Designed for exploration and ingestion of imperfect copybooks.

- **ODO** out-of-range: clamped with a warning in encoder paths; schema still loads.
- **REDEFINES** ambiguity: warn and refuse encoding, but schema loads.
- **Edited PIC**: Fully supported (E1/E2/E3); only Space (`B`) insertion is unsupported.

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

| Code | Tag | Meaning |
|-----:|:---:|---------|
| 0 | OK | Success |
| 1 | CBK? | Unknown/unclassified failure (e.g. unknown `support --check` feature ID) |
| 2 | CBKD | Data quality failure |
| 3 | CBKE | Encode/validation failure (including structural parse/schema rejections) |
| 4 | CBKF | Record format/RDW failure |
| 5 | CBKI | Internal orchestration error (including panics and otherwise unmapped errors) |

Some subcommands document additional command-specific semantics: `verify` reports 3 for validation errors and 2 for fatal I/O/schema errors; `determinism` reports 0 for deterministic, 2 for drift detected, and 3 for codec/usage errors.

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
- `raw_b64` - Canonical base64-encoded raw record bytes (record/record+rdw modes); legacy `__raw_b64` is also emitted for backward compatibility
- `<FIELD>__raw_b64` - Base64 payload for individual fields (field mode)
- Enables byte-perfect round trips when re-encoding

**FILLER Fields (--emit-filler):**
- `_filler_<offset>` - FILLER field at byte offset
- Normally omitted from output
- Useful for debugging layout issues

## Performance Tuning

### Thread Count
- Default: 1 (set `--threads <N>` to parallelize)
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

# Lenient with error reporting (note: -v/--verbose is a global flag
# and goes before the subcommand)
copybook -v decode schema.cpy data.bin \
  --format fixed \
  --max-errors 1000 \
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
3. Use `-v/--verbose` (before the subcommand) for detailed diagnostics
4. Test with small data samples first
5. Refer to examples in [README.md](../README.md)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
