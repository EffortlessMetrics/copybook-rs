# Error Codes Reference

copybook-rs uses a comprehensive error taxonomy with stable codes for reliable error handling and troubleshooting.

## Error Code Format

Error codes follow the pattern `CBK[Category][Number]_[Description]`:

- **CBK**: copybook-rs prefix
- **Category**: Single letter indicating error category
- **Number**: 3-digit sequential number within category
- **Description**: Descriptive name in UPPER_SNAKE_CASE

## Error Categories

### Parse Errors (CBKP*)

Errors that occur during copybook parsing and schema generation.

#### CBKP001_SYNTAX
**Description**: General syntax error in copybook
**Severity**: Fatal
**Context**: Line number, column, expected vs found tokens
**Resolution**: Fix COBOL syntax in copybook

```
Error: CBKP001_SYNTAX at line 15, column 8
Expected: PIC clause
Found: USAGE
```

#### CBKP011_UNSUPPORTED_CLAUSE
**Description**: Unsupported COBOL clause encountered
**Severity**: Fatal
**Context**: Clause name, field path
**Resolution**: Remove or replace unsupported clause

```
Error: CBKP011_UNSUPPORTED_CLAUSE
Clause: COMP-1
Field: ROOT.CUSTOMER.BALANCE
```

#### CBKP021_ODO_NOT_TAIL
**Description**: OCCURS DEPENDING ON array not at tail position
**Severity**: Fatal
**Context**: Field path, containing group
**Resolution**: Move ODO array to end of containing group

```
Error: CBKP021_ODO_NOT_TAIL
Field: ROOT.CUSTOMER.ORDERS
Group: ROOT.CUSTOMER
```

#### CBKP051_UNSUPPORTED_EDITED_PIC
**Description**: Edited PIC clause not supported
**Severity**: Fatal
**Context**: PIC clause, field path
**Resolution**: Use non-edited PIC clause

```
Error: CBKP051_UNSUPPORTED_EDITED_PIC
PIC: ZZ9.99
Field: ROOT.CUSTOMER.AMOUNT
```

### Schema Errors (CBKS*)

Errors in schema validation and layout resolution.

#### CBKS121_COUNTER_NOT_FOUND
**Description**: ODO counter field not found or invalid
**Severity**: Fatal
**Context**: Counter field path, ODO array path
**Resolution**: Ensure counter field exists and precedes array

```
Error: CBKS121_COUNTER_NOT_FOUND
Counter: ROOT.CUSTOMER.ORDER_COUNT
Array: ROOT.CUSTOMER.ORDERS
```

#### CBKS141_RECORD_TOO_LARGE
**Description**: Computed record size exceeds maximum
**Severity**: Fatal
**Context**: Computed size, maximum allowed
**Resolution**: Reduce record size or increase limit

```
Error: CBKS141_RECORD_TOO_LARGE
Computed size: 67108864 bytes
Maximum: 16777216 bytes
```

#### CBKS301_ODO_CLIPPED
**Description**: ODO counter value exceeds maximum, clipped
**Severity**: Warning (lenient mode)
**Context**: Record number, field path, actual vs maximum count
**Resolution**: Fix data or increase ODO maximum

```
Warning: CBKS301_ODO_CLIPPED at record 1234
Field: ROOT.CUSTOMER.ORDERS
Counter value: 150, clipped to maximum: 100
```

#### CBKS302_ODO_RAISED
**Description**: ODO counter value below minimum, raised
**Severity**: Warning (lenient mode)
**Context**: Record number, field path, actual vs minimum count
**Resolution**: Fix data or decrease ODO minimum

```
Warning: CBKS302_ODO_RAISED at record 5678
Field: ROOT.CUSTOMER.ORDERS
Counter value: 0, raised to minimum: 1
```

### Record Format Errors (CBKR*)

Errors in record framing and I/O processing.

#### CBKR211_RDW_RESERVED_NONZERO
**Description**: RDW reserved bytes are non-zero
**Severity**: Warning (lenient), Fatal (strict)
**Context**: Record number, reserved bytes value
**Resolution**: Check for data corruption or use --emit-raw

```
Warning: CBKR211_RDW_RESERVED_NONZERO at record 100
Reserved bytes: 0x1234 (expected 0x0000)
```

#### CBKR221_RDW_UNDERFLOW
**Description**: RDW length less than minimum record size
**Severity**: Fatal
**Context**: Record number, RDW length, minimum required
**Resolution**: Check data integrity or record format

```
Error: CBKR221_RDW_UNDERFLOW at record 200
RDW length: 50 bytes
Minimum required: 120 bytes
```

### Character Encoding Errors (CBKC*)

Errors in character set conversion and text processing.

#### CBKC201_JSON_WRITE_ERROR
**Description**: Error writing JSON output data
**Severity**: Fatal
**Context**: Record number, I/O error details
**Resolution**: Check output file permissions or disk space

```
Error: CBKC201_JSON_WRITE_ERROR at record 75
I/O Error: No space left on device (os error 28)
```

#### CBKC301_INVALID_EBCDIC_BYTE
**Description**: Invalid EBCDIC byte encountered during conversion
**Severity**: Warning (replace mode), Fatal (error mode)
**Context**: Record number, field path, byte offset, hex value
**Resolution**: Fix data corruption or use replacement mode

```
Warning: CBKC301_INVALID_EBCDIC_BYTE at record 300
Field: ROOT.CUSTOMER.NAME
Offset: 15, byte: 0xFF (unmappable in CP037)
Replaced with: U+FFFD
```

### Data Decoding Errors (CBKD*)

Errors during binary data decoding to JSON.

#### CBKD401_COMP3_INVALID_NIBBLE
**Description**: Invalid nibble in packed decimal field
**Severity**: Fatal (strict), Warning (lenient)
**Context**: Record number, field path, byte offset, nibble value
**Resolution**: Fix data corruption

```
Error: CBKD401_COMP3_INVALID_NIBBLE at record 400
Field: ROOT.CUSTOMER.BALANCE
Offset: 25, nibble: 0xE (expected 0-9, A-F for sign)
```

#### CBKD411_ZONED_BAD_SIGN
**Description**: Invalid sign zone in zoned decimal field
**Severity**: Fatal (strict), Warning (lenient)
**Context**: Record number, field path, byte offset, zone value
**Resolution**: Fix data corruption or check codepage

```
Error: CBKD411_ZONED_BAD_SIGN at record 500
Field: ROOT.CUSTOMER.AMOUNT
Offset: 30, zone: 0x4 (expected C/D/F for EBCDIC)
```

#### CBKD412_ZONED_BLANK_IS_ZERO
**Description**: BLANK WHEN ZERO field contains all spaces
**Severity**: Warning
**Context**: Record number, field path
**Resolution**: Normal behavior, no action needed

```
Warning: CBKD412_ZONED_BLANK_IS_ZERO at record 600
Field: ROOT.CUSTOMER.DISCOUNT
All spaces decoded as zero
```

#### CBKD301_RECORD_TOO_SHORT
**Description**: Record data is shorter than required by field layout
**Severity**: Fatal
**Context**: Record number, field path, expected vs actual length
**Resolution**: Check record boundaries and field offsets

```
Error: CBKD301_RECORD_TOO_SHORT at record 150
Field: ROOT.CUSTOMER.PHONE
Expected: 80 bytes, found: 60 bytes
```

### Data Encoding Errors (CBKE*)

Errors during JSON to binary encoding.

#### CBKE501_JSON_TYPE_MISMATCH
**Description**: JSON value type doesn't match field type
**Severity**: Fatal
**Context**: Record number, field path, expected vs actual type
**Resolution**: Fix JSON data type or schema

```
Error: CBKE501_JSON_TYPE_MISMATCH at record 700
Field: ROOT.CUSTOMER.ID
Expected: string (zoned decimal)
Found: number
```

#### CBKE521_ARRAY_LEN_OOB
**Description**: JSON array length out of bounds for OCCURS
**Severity**: Fatal
**Context**: Record number, field path, array length, min/max bounds
**Resolution**: Adjust array length or OCCURS bounds

```
Error: CBKE521_ARRAY_LEN_OOB at record 800
Field: ROOT.CUSTOMER.ORDERS
Array length: 150
Bounds: min=1, max=100
```

### File I/O Errors (CBKF*)

Errors in file operations and transfer corruption detection.

#### CBKF104_RDW_SUSPECT_ASCII
**Description**: RDW header suggests ASCII transfer corruption
**Severity**: Warning
**Context**: Record number, RDW bytes as ASCII interpretation
**Resolution**: Check file transfer mode (binary vs text)

```
Warning: CBKF104_RDW_SUSPECT_ASCII at record 900
RDW bytes: 0x30303030 (ASCII "0000")
Possible text-mode transfer corruption
```

## Error Handling Modes

### Strict Mode (`--strict`)
- Stop processing on first data error
- ODO out-of-bounds → fatal error
- RDW reserved bytes non-zero → fatal error
- Exit code: 2 (fatal error)

### Lenient Mode (default)
- Continue processing after recoverable errors
- ODO out-of-bounds → clamp with warning
- RDW reserved bytes non-zero → warning
- Skip bad records and continue
- Exit code: 1 if any errors occurred, 0 if warnings only

### Max Errors (`--max-errors N`)
- Stop after N errors in lenient mode
- Useful for large files with systematic issues
- Exit code: 1 (completed with errors)

## Troubleshooting Guide

### Common Issues and Solutions

#### "CBKP051_UNSUPPORTED_EDITED_PIC"
**Problem**: Copybook contains edited PIC clauses
**Solution**: 
- Remove editing characters (Z, /, comma) from PIC
- Use post-processing for formatting
- Example: `PIC ZZ9.99` → `PIC 999V99`

#### "CBKD401_COMP3_INVALID_NIBBLE"
**Problem**: Corrupted packed decimal data
**Solutions**:
- Check file transfer mode (should be binary)
- Verify record boundaries and alignment
- Use `--emit-raw` to inspect raw bytes

#### "CBKC301_INVALID_EBCDIC_BYTE"
**Problem**: Invalid EBCDIC characters
**Solutions**:
- Verify correct codepage (`--codepage`)
- Use `--on-decode-unmappable replace` for tolerance
- Check for binary data in text fields

#### "CBKR211_RDW_RESERVED_NONZERO"
**Problem**: RDW reserved bytes contain data
**Solutions**:
- Use `--emit-raw record+rdw` to preserve
- Check for non-standard RDW format
- Verify variable-length record format

#### "CBKS301_ODO_CLIPPED"
**Problem**: ODO counter exceeds maximum
**Solutions**:
- Increase OCCURS maximum in copybook
- Use strict mode to fail fast
- Check data integrity

### Performance Issues

#### Slow Processing
**Symptoms**: Low throughput, high CPU usage
**Solutions**:
- Use `--threads N` for parallel processing
- Check for excessive error logging
- Profile with `--verbose` flag

#### High Memory Usage
**Symptoms**: Memory growth during processing
**Solutions**:
- Verify streaming mode is active
- Check for large ODO arrays
- Reduce thread count if memory-constrained

#### Inconsistent Output
**Symptoms**: Different results across runs
**Solutions**:
- Ensure deterministic mode (default)
- Check for race conditions in parallel processing
- Verify input data stability

### Data Quality Issues

#### Missing Fields in JSON
**Symptoms**: Expected fields not in output
**Solutions**:
- Use `--emit-filler` to include FILLER fields
- Check REDEFINES relationships
- Verify field names and paths

#### Incorrect Numeric Values
**Symptoms**: Wrong numbers in JSON output
**Solutions**:
- Verify codepage for zoned decimals
- Check PIC clause scale (V position)
- Use `--json-number lossless` for precision

#### Round-Trip Failures
**Symptoms**: Encoded data differs from original
**Solutions**:
- Use `--emit-raw` and `--use-raw` for fidelity
- Check for REDEFINES ambiguity
- Verify BLANK WHEN ZERO handling

## Getting Help

1. **Check Error Context**: Error messages include detailed context
2. **Use Verbose Mode**: `--verbose` provides additional diagnostics
3. **Inspect Schema**: Use `copybook inspect` to understand layout
4. **Test with Samples**: Use small data samples for debugging
5. **Check Documentation**: Refer to [README.md](../README.md) and examples
6. **Report Issues**: File bug reports with error codes and context

## Error Code Index

| Code | Category | Severity | Description |
|------|----------|----------|-------------|
| CBKP001 | Parse | Fatal | Syntax error |
| CBKP011 | Parse | Fatal | Unsupported clause |
| CBKP021 | Parse | Fatal | ODO not at tail |
| CBKP051 | Parse | Fatal | Edited PIC unsupported |
| CBKS121 | Schema | Fatal | Counter not found |
| CBKS141 | Schema | Fatal | Record too large |
| CBKS301 | Schema | Warning | ODO clipped |
| CBKS302 | Schema | Warning | ODO raised |
| CBKR211 | Record | Warning/Fatal | RDW reserved non-zero |
| CBKR221 | Record | Fatal | RDW underflow |
| CBKC201 | Charset | Fatal | JSON write error |
| CBKC301 | Charset | Warning/Fatal | Invalid EBCDIC byte |
| CBKD301 | Decode | Fatal | Record too short |
| CBKD401 | Decode | Fatal/Warning | COMP-3 invalid nibble |
| CBKD411 | Decode | Fatal/Warning | Zoned bad sign |
| CBKD412 | Decode | Warning | Zoned blank is zero |
| CBKE501 | Encode | Fatal | JSON type mismatch |
| CBKE521 | Encode | Fatal | Array length OOB |
| CBKF104 | File | Warning | RDW suspect ASCII |