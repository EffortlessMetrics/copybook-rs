# CLI Examples and Manpage

Comprehensive examples for copybook-rs command-line usage patterns.

## Basic Usage Patterns

### Parse and Inspect

```bash
# Parse copybook and output schema
copybook parse customer.cpy --output customer-schema.json

# Inspect layout with human-readable table
copybook inspect customer.cpy --show-offsets --show-lengths

# Validate schema after parsing
copybook parse customer.cpy --validate
```

### Simple Decode/Encode

```bash
# Decode fixed-length records
copybook decode customer.cpy data.bin \
  --format fixed \
  --output data.jsonl

# Encode back to binary
copybook encode customer.cpy data.jsonl \
  --format fixed \
  --output data-new.bin
```

## Error Handling Modes

### Strict vs Lenient Processing

```bash
# Strict mode - stop on first error
copybook decode customer.cpy data.bin \
  --format fixed \
  --strict \
  --output data.jsonl

# Lenient mode with error limit
copybook decode customer.cpy data.bin \
  --format fixed \
  --max-errors 100 \
  --output data.jsonl

# Lenient mode with comprehensive logging
copybook decode customer.cpy data.bin \
  --format fixed \
  --verbose \
  --output data.jsonl \
  2> processing.log
```

## Character Encoding Examples

### EBCDIC Code Pages

```bash
# US/Canada mainframe (most common)
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --output data.jsonl

# German/Austrian systems
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp273 \
  --output data.jsonl

# International systems
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp500 \
  --output data.jsonl
```# Unix/Li
nux mainframes
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp1047 \
  --output data.jsonl

# European with Euro support
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp1140 \
  --output data.jsonl

# ASCII systems (non-mainframe COBOL)
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage ascii \
  --output data.jsonl
```

### Unmappable Character Policies

```bash
# Error on unmappable characters (default)
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --on-decode-unmappable error \
  --output data.jsonl

# Replace unmappable with U+FFFD
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --on-decode-unmappable replace \
  --output data.jsonl

# Skip unmappable characters
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --on-decode-unmappable skip \
  --output data.jsonl
```

## BLANK WHEN ZERO (BWZ) Policies

### Decoding BWZ Fields

```bash
# Standard decode (spaces become zero with warning)
copybook decode customer.cpy data.bin \
  --format fixed \
  --output data.jsonl

# Verbose mode to see BWZ warnings
copybook decode customer.cpy data.bin \
  --format fixed \
  --verbose \
  --output data.jsonl
```

### Encoding BWZ Fields

```bash
# Standard encode (zeros as numeric zeros)
copybook encode customer.cpy data.jsonl \
  --format fixed \
  --output data.bin

# BWZ encode (zeros as spaces)
copybook encode customer.cpy data.jsonl \
  --format fixed \
  --bwz-encode \
  --output data.bin
```

## Record Format Examples

### Fixed-Length Records

```bash
# Basic fixed-length processing
copybook decode customer.cpy fixed-data.bin \
  --format fixed \
  --output data.jsonl

# Fixed-length with metadata
copybook decode customer.cpy fixed-data.bin \
  --format fixed \
  --emit-meta \
  --output data.jsonl

# Fixed-length with FILLER fields
copybook decode customer.cpy fixed-data.bin \
  --format fixed \
  --emit-filler \
  --output data.jsonl
```

### Variable-Length Records (RDW)

```bash
# Basic RDW processing
copybook decode customer.cpy rdw-data.bin \
  --format rdw \
  --output data.jsonl

# RDW with raw header preservation
copybook decode customer.cpy rdw-data.bin \
  --format rdw \
  --emit-raw record+rdw \
  --output data.jsonl

# RDW encoding with raw preservation
copybook encode customer.cpy data.jsonl \
  --format rdw \
  --use-raw \
  --output rdw-data.bin
```

## Advanced Output Control

### JSON Number Formats

```bash
# Lossless mode (decimals as strings)
copybook decode customer.cpy data.bin \
  --format fixed \
  --json-number lossless \
  --output data.jsonl

# Native mode (JSON numbers where possible)
copybook decode customer.cpy data.bin \
  --format fixed \
  --json-number native \
  --output data.jsonl
```

### Raw Byte Capture

```bash
# Capture raw record bytes
copybook decode customer.cpy data.bin \
  --format fixed \
  --emit-raw record \
  --output data.jsonl

# Capture raw field bytes
copybook decode customer.cpy data.bin \
  --format fixed \
  --emit-raw field \
  --output data.jsonl

# Capture record + RDW header
copybook decode customer.cpy data.bin \
  --format rdw \
  --emit-raw record+rdw \
  --output data.jsonl
```

### Metadata Inclusion

```bash
# Include schema fingerprint and record info
copybook decode customer.cpy data.bin \
  --format fixed \
  --emit-meta \
  --output data.jsonl

# Combined metadata and raw capture
copybook decode customer.cpy data.bin \
  --format fixed \
  --emit-meta \
  --emit-raw record \
  --output data.jsonl
```

## Performance and Parallel Processing

### Thread Configuration

```bash
# Single-threaded processing
copybook decode customer.cpy large-data.bin \
  --format fixed \
  --threads 1 \
  --output data.jsonl

# Multi-threaded processing
copybook decode customer.cpy large-data.bin \
  --format fixed \
  --threads 8 \
  --output data.jsonl

# Auto-detect thread count
copybook decode customer.cpy large-data.bin \
  --format fixed \
  --output data.jsonl
```

### Performance Monitoring

```bash
# Verbose performance reporting
copybook decode customer.cpy large-data.bin \
  --format fixed \
  --verbose \
  --output data.jsonl

# Performance with error limits
copybook decode customer.cpy large-data.bin \
  --format fixed \
  --max-errors 1000 \
  --verbose \
  --output data.jsonl
```

## Round-Trip Processing

### Complete Round-Trip

```bash
# Step 1: Decode with raw capture
copybook decode customer.cpy original.bin \
  --format fixed \
  --emit-raw record \
  --output decoded.jsonl

# Step 2: Encode with raw preservation
copybook encode customer.cpy decoded.jsonl \
  --format fixed \
  --use-raw \
  --output roundtrip.bin

# Step 3: Verify identical
diff original.bin roundtrip.bin
```

### Round-Trip with Modifications

```bash
# Decode original
copybook decode customer.cpy original.bin \
  --format fixed \
  --emit-raw record \
  --output original.jsonl

# Transform data (example with jq)
jq '.CUSTOMER_NAME = (.CUSTOMER_NAME | ascii_upcase)' \
  original.jsonl > modified.jsonl

# Encode modified (without raw to allow changes)
copybook encode customer.cpy modified.jsonl \
  --format fixed \
  --output modified.bin
```

## Data Quality and Validation

### Comprehensive Validation

```bash
# Parse validation
copybook parse customer.cpy --validate

# Data verification
copybook verify customer.cpy data.bin \
  --format fixed \
  --report verification.json

# Decode with strict validation
copybook decode customer.cpy data.bin \
  --format fixed \
  --strict \
  --output validated.jsonl
```

### Error Analysis

```bash
# Collect all errors
copybook decode customer.cpy data.bin \
  --format fixed \
  --max-errors 10000 \
  --verbose \
  --output data.jsonl \
  2> all-errors.log

# Summary-only verification
copybook verify customer.cpy data.bin \
  --format fixed \
  --summary
```

## ETL Pipeline Integration

### Extract Phase

```bash
# Extract mainframe data with metadata
copybook decode mainframe-schema.cpy mainframe-data.bin \
  --format fixed \
  --codepage cp037 \
  --emit-meta \
  --emit-raw record \
  --threads 8 \
  --output extracted.jsonl
```

### Transform Phase

```bash
# Transform with external tools
jq -c '
  .customer_name = (.customer_name | ascii_upcase) |
  .balance = (.balance | tonumber | . / 100) |
  .processed_date = now | strftime("%Y-%m-%d")
' extracted.jsonl > transformed.jsonl
```

### Load Phase

```bash
# Load back to mainframe format
copybook encode mainframe-schema.cpy transformed.jsonl \
  --format fixed \
  --codepage cp037 \
  --output processed-data.bin

# Or load to different format
copybook encode target-schema.cpy transformed.jsonl \
  --format rdw \
  --codepage ascii \
  --output target-data.bin
```

## Batch Processing Examples

### Multiple File Processing

```bash
#!/bin/bash
# Process all files in directory
for file in data/*.bin; do
    base=$(basename "$file" .bin)
    copybook decode schema.cpy "$file" \
      --format fixed \
      --output "output/${base}.jsonl" \
      --verbose
done
```

### Parallel Batch Processing

```bash
#!/bin/bash
# Process files in parallel
find data -name "*.bin" | xargs -P 4 -I {} bash -c '
    base=$(basename "{}" .bin)
    copybook decode schema.cpy "{}" \
      --format fixed \
      --output "output/${base}.jsonl"
'
```

## Debugging and Troubleshooting

### Schema Debugging

```bash
# Detailed schema inspection
copybook inspect problematic.cpy \
  --show-offsets \
  --show-lengths \
  --format json > schema-debug.json

# Parse with verbose errors
copybook parse problematic.cpy \
  --verbose 2> parse-errors.log
```

### Data Debugging

```bash
# Decode small sample with all options
head -c 1000 data.bin > sample.bin
copybook decode schema.cpy sample.bin \
  --format fixed \
  --emit-raw field \
  --emit-filler \
  --emit-meta \
  --verbose \
  --output debug.jsonl

# Examine raw bytes
jq -r '.[0] | to_entries[] | select(.key | endswith("__raw_b64")) | 
  "\(.key): \(.value)"' debug.jsonl | 
  while read field b64; do
    echo "$field:"
    echo "$b64" | base64 -d | hexdump -C
  done
```

### Performance Debugging

```bash
# Profile performance
time copybook decode schema.cpy large-data.bin \
  --format fixed \
  --threads 1 \
  --output /dev/null \
  --verbose

# Compare thread performance
for threads in 1 2 4 8; do
  echo "Testing $threads threads:"
  time copybook decode schema.cpy large-data.bin \
    --format fixed \
    --threads $threads \
    --output /dev/null
done
```

## Configuration File Usage

### Configuration File Example

```toml
# copybook.toml
[decode]
codepage = "cp037"
format = "fixed"
emit_meta = true
emit_raw = "record"
threads = 4
max_errors = 1000

[encode]
codepage = "cp037"
format = "fixed"
use_raw = true
bwz_encode = false
```

### Using Configuration

```bash
# Use configuration file
copybook decode customer.cpy data.bin \
  --config copybook.toml \
  --output data.jsonl

# Override configuration options
copybook decode customer.cpy data.bin \
  --config copybook.toml \
  --threads 8 \
  --output data.jsonl
```

## Environment Variables

### Setting Defaults

```bash
# Set environment defaults
export COPYBOOK_CODEPAGE=cp037
export COPYBOOK_THREADS=8
export COPYBOOK_LOG_LEVEL=info

# Use with commands
copybook decode customer.cpy data.bin \
  --format fixed \
  --output data.jsonl
```

## Exit Code Handling

### Script Integration

```bash
#!/bin/bash
# Handle exit codes properly

copybook decode customer.cpy data.bin \
  --format fixed \
  --output data.jsonl

case $? in
  0)
    echo "Success: Processing completed"
    ;;
  1)
    echo "Warning: Completed with errors"
    # Continue with warnings
    ;;
  2)
    echo "Error: Fatal error occurred"
    exit 1
    ;;
esac
```

### Pipeline Integration

```bash
# ETL pipeline with error handling
copybook decode schema.cpy data.bin \
  --format fixed \
  --max-errors 100 \
  --output stage1.jsonl || {
  echo "Decode failed with exit code $?"
  exit 1
}

# Transform
jq -c '.processed = true' stage1.jsonl > stage2.jsonl

# Encode
copybook encode schema.cpy stage2.jsonl \
  --format fixed \
  --output final.bin || {
  echo "Encode failed with exit code $?"
  exit 1
}

echo "Pipeline completed successfully"
```

## Man Page Style Examples

### NAME
copybook - Modern COBOL copybook parser and data codec

### SYNOPSIS
**copybook** *COMMAND* [*OPTIONS*]

### DESCRIPTION
copybook-rs provides deterministic, reproducible conversion of mainframe-encoded records into accessible JSON formats and vice versa.

### EXAMPLES

**Parse a copybook:**
```
copybook parse customer.cpy --output schema.json
```

**Decode mainframe data:**
```
copybook decode customer.cpy data.bin --format fixed --output data.jsonl
```

**Round-trip processing:**
```
copybook decode schema.cpy data.bin --emit-raw record --output temp.jsonl
copybook encode schema.cpy temp.jsonl --use-raw --output roundtrip.bin
```

### SEE ALSO
**copybook-parse**(1), **copybook-decode**(1), **copybook-encode**(1)

For complete documentation, see the copybook-rs documentation at:
https://github.com/copybook-rs/copybook-rs