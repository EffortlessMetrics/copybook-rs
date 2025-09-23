# Verify Command

The `copybook verify` command validates data files against COBOL copybook schemas, providing comprehensive error reporting and data integrity validation without generating output files.

## Overview

The verify command performs complete parse → decode validation of binary data records, detecting:

- **Schema violations**: Field type mismatches, invalid packed decimal data, zoned decimal format errors
- **Record structure issues**: Truncated records, invalid RDW headers, field alignment problems
- **Data integrity problems**: Character encoding errors, numeric overflow, ODO counter validation failures

Results are reported via exit codes and optional JSON reports for integration with automated workflows.

## Usage

### Basic Validation

```bash
# Verify fixed-length records with EBCDIC CP037 encoding
copybook verify --format fixed --codepage cp037 customer.cpy customer-data.bin

# Verify variable-length RDW records with EBCDIC CP1047 encoding
copybook verify --format rdw --codepage cp1047 transaction.cpy transaction-data.bin
```

### Advanced Options

```bash
# Generate detailed JSON verification report
copybook verify --format fixed --codepage cp037 \
    --report verification-report.json \
    --sample 10 \
    --max-errors 100 \
    customer.cpy customer-data.bin

# Strict mode validation (fail on ODO counter issues)
copybook verify --format fixed --codepage cp037 \
    --strict \
    --report strict-validation.json \
    copybook.cpy data.bin
```

## Exit Codes

| Code | Meaning | Description |
|------|---------|-------------|
| 0 | Success | All records validated successfully, no errors found |
| 2 | Fatal Error | I/O errors, schema parsing failures, or system errors |
| 3 | Validation Errors | Data validation errors found (see report for details) |

## JSON Report Format

When `--report` is specified, verification results are written as JSON conforming to [VERIFY_REPORT.schema.json](VERIFY_REPORT.schema.json).

### Report Structure

```json
{
  "report_version": 1,
  "schema_fingerprint": "sha256:a1b2c3...",
  "record_format": "fixed",
  "records_total": 1000,
  "errors_total": 3,
  "truncated": false,
  "cli_opts": {
    "codepage": "cp037",
    "strict": false,
    "max_errors": 1000,
    "sample": 5
  },
  "errors": [...],
  "sample": [...]
}
```

### Error Reporting

Each error includes detailed context for debugging:

```json
{
  "index": 42,
  "code": "CBKD201",
  "field": "CUSTOMER.ACCOUNT_BALANCE",
  "offset": 156,
  "msg": "Invalid packed decimal data: nibble 0x0F in unexpected position",
  "hex": "00 1C 45 3F 67 89"
}
```

**Key Fields:**
- `index`: 0-based record number where error occurred
- `code`: Structured error code from copybook-rs error taxonomy (CBKP*, CBKS*, CBKD*, CBKE*)
- `field`: Dot-separated field path (e.g., `CUSTOMER.ADDRESS.ZIP_CODE`)
- `offset`: **Record-relative** byte offset where error occurred
- `hex`: Centered hex dump around the error location for debugging

### Sample Data

Sample records are included for debugging and validation workflows:

```json
{
  "sample": [
    {
      "index": 0,
      "hex": "C3E4E2E3D6D4C5D9 40404040404040 F1F2F3F4F5"
    },
    {
      "index": 1,
      "hex": "C1C3D4C540C3D6D9 D7404040404040 F0F0F1F2F3"
    }
  ]
}
```

## Error Code Reference

### Parse Errors (CBKP*)
- **CBKP051**: Unsupported COBOL language features
- **CBKP052**: Copybook syntax errors or malformed structures

### Schema Validation (CBKS*)
- **CBKS101**: ODO counter validation failures
- **CBKS102**: Record size constraint violations

### Data Errors (CBKD*)
- **CBKD201**: Invalid packed decimal (COMP-3) data
- **CBKD202**: Invalid zoned decimal sign nibbles
- **CBKD301**: Truncated records (insufficient data length)
- **CBKD401**: Binary integer field type mismatches

### Encoding Errors (CBKE*)
- **CBKE501**: Character encoding conversion failures
- **CBKE502**: JSON type mapping conflicts

## Integration Examples

### CI/CD Pipeline Integration

```bash
#!/bin/bash
# Validate data files in CI pipeline

copybook verify --format fixed --codepage cp037 \
    --report validation-report.json \
    --max-errors 0 \
    schema.cpy production-data.bin

exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo "✅ Data validation passed"
elif [ $exit_code -eq 3 ]; then
    echo "❌ Data validation failed - see validation-report.json"
    jq '.errors[] | "\(.index): \(.field) - \(.msg)"' validation-report.json
    exit 1
else
    echo "💥 Fatal error during validation"
    exit 1
fi
```

### Data Quality Monitoring

```python
import json
import subprocess

def validate_data_file(copybook_path, data_path):
    """Validate data file and return structured results."""
    result = subprocess.run([
        'copybook', 'verify',
        '--format', 'fixed',
        '--codepage', 'cp037',
        '--report', 'report.json',
        '--sample', '20',
        copybook_path, data_path
    ], capture_output=True, text=True)

    if result.returncode == 2:
        raise RuntimeError(f"Fatal validation error: {result.stderr}")

    with open('report.json') as f:
        report = json.load(f)

    return {
        'valid': result.returncode == 0,
        'records_total': report['records_total'],
        'errors_total': report['errors_total'],
        'error_rate': report['errors_total'] / report['records_total'],
        'errors': report['errors'][:10]  # First 10 errors
    }

# Usage
results = validate_data_file('customer.cpy', 'daily-extract.bin')
if not results['valid']:
    print(f"Validation failed: {results['error_rate']:.2%} error rate")
    for error in results['errors']:
        print(f"Record {error['index']}: {error['field']} - {error['msg']}")
```

## Performance Notes

- **Throughput**: High-performance validation - see [Performance Specifications](../README.md#performance-specifications) for detailed benchmarks
- **Memory usage**: <256 MiB steady-state for multi-GB files
- **Error handling**: Fast-fail on fatal errors, configurable error limits for data issues
- **Output**: No data conversion overhead - validation only

## Schema Requirements

- Copybook must parse successfully (exit code 2 on parse failures)
- Record format must be specified explicitly (`--format fixed|rdw`)
- Character encoding must match data file encoding (`--codepage`)

For additional details, see the main [copybook-rs documentation](../README.md) and [CLAUDE.md](../CLAUDE.md).
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
