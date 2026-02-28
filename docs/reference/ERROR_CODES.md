<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Error Codes Reference

copybook-rs uses a comprehensive error taxonomy with stable codes for reliable error handling and troubleshooting. **All error handling is panic-safe** with zero risk of unwrap() or expect() panics in production environments.

**Canonical Status**: This is the authoritative source for error code semantics. Any historical or proposed code adjustments referenced in release docs are date-labeled as `[Historical: YYYY-MM-DD]`.

## Error Code Format

Error codes follow the pattern `CBK[Category][Number]_[Description]`:

- **CBK**: copybook-rs prefix
- **Category**: Single letter indicating error category
- **Number**: 3-digit sequential number within category
- **Description**: Descriptive name in UPPER_SNAKE_CASE

## Error Categories

### Parse Errors (CBKP*) - Panic-Safe

Errors that occur during copybook parsing and schema generation. **All parsing operations use panic-safe error handling** with structured error reporting and zero unwrap() risk.

#### CBKP001_SYNTAX
**Description**: General syntax error in copybook
**Severity**: Fatal
**Context**: Line number, column, expected vs found tokens
**Resolution**: Fix COBOL syntax in copybook
**Panic Safety**: Uses safe_ops::safe_slice_get() for token access, safe_ops::safe_string_char_at() for character access

```
Error: CBKP001_SYNTAX at line 15, column 8
Expected: PIC clause
Found: USAGE
Context: Safe parser token access at position 142
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
**Panic Safety**: Uses safe_ops::safe_array_bound() for overflow-safe size calculations

```
Error: CBKP021_ODO_NOT_TAIL
Field: ROOT.CUSTOMER.ORDERS
Group: ROOT.CUSTOMER
Context: Safe array bounds validation with overflow protection
```

#### CBKP051_UNSUPPORTED_EDITED_PIC
**Description**: **unsupported edited PIC token**. Triggered only by Space (`B`) insertion, which remains unsupported. All other edited PIC patterns (Z, $, +/-, CR/DB, commas, asterisk, currency) are fully supported in E1/E2/E3 phases.
**Severity**: Fatal
**Context**: PIC clause, field path, unsupported token
**Resolution**: Remove Space (`B`) insertion from PIC clause, or use alternative formatting

```
Error: CBKP051_UNSUPPORTED_EDITED_PIC
PIC: 99B99B99
Field: ROOT.CUSTOMER.PHONE
Token: B (space insertion)
```

### Schema Errors (CBKS*) - Enterprise Safety

Errors in schema validation and layout resolution. **Enhanced with panic-safe integer conversions** and overflow protection for enterprise reliability.

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
**Panic Safety**: Uses safe_ops::safe_u64_to_u32() and safe_ops::safe_usize_to_u32() for overflow-safe integer conversions

```
Error: CBKS141_RECORD_TOO_LARGE
Computed size: 67108864 bytes
Maximum: 16777216 bytes
Context: Safe integer conversion with overflow detection
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

#### CBKS701_PROJECTION_INVALID_ODO
**Description**: Selected field contains OCCURS DEPENDING ON but counter field is not accessible
**Severity**: Fatal
**Context**: Field path, counter field path, projection selection
**Resolution**: Include the ODO counter field in selection, or select the entire parent group

```
Error: CBKS701_PROJECTION_INVALID_ODO
Field: ROOT.CUSTOMER.ORDERS (OCCURS DEPENDING ON ORDER-COUNT)
Counter: ROOT.CUSTOMER.ORDER-COUNT (not in projection)
Resolution: Add ORDER-COUNT to --select or select parent group
```

#### CBKS702_PROJECTION_UNRESOLVED_ALIAS
**Description**: RENAMES alias spans fields that are not all selected
**Severity**: Fatal
**Context**: Alias name, aliased fields, missing fields in selection
**Resolution**: Select all fields covered by the RENAMES alias, or use underlying field names directly

```
Error: CBKS702_PROJECTION_UNRESOLVED_ALIAS
Alias: CUSTOMER-HEADER (RENAMES CUSTOMER-ID THRU CUSTOMER-NAME)
Missing fields: CUSTOMER-NAME (not in projection)
Resolution: Select all fields in RENAMES range or use field names directly
```

#### CBKS703_PROJECTION_FIELD_NOT_FOUND
**Description**: Selected field name does not exist in schema
**Severity**: Fatal
**Context**: Field name, available fields suggestion
**Resolution**: Verify field name spelling and case sensitivity, use `copybook inspect` to list available fields

```
Error: CBKS703_PROJECTION_FIELD_NOT_FOUND
Field: CUSTMER-ID (not found in schema)
Suggestion: Did you mean CUSTOMER-ID?
Resolution: Check field name with `copybook inspect schema.cpy`
```

### Record Format Errors (CBKR*)

Errors in record framing and I/O processing.

#### CBKR101_FIXED_RECORD_ERROR
**Description**: Error processing fixed-length record
**Severity**: Fatal
**Context**: Record number, error details
**Resolution**: Check record format and data integrity

```
Error: CBKR101_FIXED_RECORD_ERROR at record 75
Fixed-length record processing failed
```

#### CBKR201_RDW_READ_ERROR
**Description**: Error reading Record Descriptor Word (RDW) header
**Severity**: Fatal
**Context**: Record number, I/O error details
**Resolution**: Check file integrity and record format

```
Error: CBKR201_RDW_READ_ERROR at record 100
Failed to read RDW header: Unexpected end of file
```

#### CBKR211_RDW_RESERVED_NONZERO
**Description**: RDW reserved bytes are non-zero
**Severity**: Warning (lenient), Fatal (strict)
**Context**: Record number, reserved bytes value
**Resolution**: Check for data corruption or use --emit-raw

```
Warning: CBKR211_RDW_RESERVED_NONZERO at record 100
Reserved bytes: 0x1234 (expected 0x0000)
```

#### CBKF221_RDW_UNDERFLOW
**Description**: RDW length less than minimum record size
**Severity**: Fatal
**Context**: Record number, RDW length, minimum required
**Resolution**: Check data integrity or record format

```
Error: CBKF221_RDW_UNDERFLOW at record 200
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

#### CBKD301_RECORD_TOO_SHORT
**Description**: Record is too short to contain the required field data
**Severity**: Fatal
**Context**: Record number, field path, byte offset, expected vs actual length
**Resolution**: Check data integrity, file transfer mode, or record boundaries

```
Error: CBKD301_RECORD_TOO_SHORT at record 150
Field: ROOT.CUSTOMER.BALANCE  
Expected record length: 120 bytes
Actual record length: 85 bytes
```

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
**Description**: Invalid sign zone in zoned decimal field or ASCII overpunch character
**Severity**: Fatal (strict), Warning (lenient)
**Context**: Record number, field path, byte offset, zone/character value
**Resolution**: Fix data corruption, verify codepage (EBCDIC vs ASCII), or check overpunch encoding

```
Error: CBKD411_ZONED_BAD_SIGN at record 500
Field: ROOT.CUSTOMER.AMOUNT  
Offset: 30, zone: 0x4 (expected C/D/F for EBCDIC)

Error: CBKD411_ZONED_BAD_SIGN at record 501
Field: ROOT.CUSTOMER.DISCOUNT
Invalid ASCII overpunch character 0x40 (expected 0-9, {A-I}, {J-R})
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

#### CBKD421_EDITED_PIC_INVALID_FORMAT
**Description**: Data does not match edited PICTURE pattern (Phase E2 decode)
**Severity**: Fatal
**Context**: Record number, field path, PIC pattern, actual data bytes
**Resolution**: Verify data format matches copybook PIC clause, check codepage conversion, inspect raw data with `--emit-raw`

```
Error: CBKD421_EDITED_PIC_INVALID_FORMAT at record 150
Field: ROOT.CUSTOMER.AMOUNT (PIC $ZZ,ZZZ.99)
Expected pattern: currency symbol, digits with comma, decimal point
Actual data: "  1234.56" (missing $ symbol)
Resolution: Check data generation process or EBCDIC conversion
```

#### CBKD422_EDITED_PIC_SIGN_MISMATCH
**Description**: Sign character mismatch in edited numeric field (expected +/-, CR, or DB)
**Severity**: Fatal
**Context**: Record number, field path, expected sign, actual character
**Resolution**: Check sign editing in PIC clause matches data, verify codepage

```
Error: CBKD422_EDITED_PIC_SIGN_MISMATCH at record 200
Field: ROOT.CUSTOMER.BALANCE (PIC +ZZZ9-)
Expected: trailing '-' or '+' sign character
Actual: 0x20 (space)
Resolution: Verify sign editing format and data integrity
```

#### CBKD423_EDITED_PIC_BLANK_WHEN_ZERO
**Description**: Edited numeric field with BLANK WHEN ZERO is all blanks (decoded as zero)
**Severity**: Warning (informational)
**Context**: Record number, field path
**Resolution**: Expected behavior for BLANK WHEN ZERO fields, no action needed

```
Warning: CBKD423_EDITED_PIC_BLANK_WHEN_ZERO at record 300
Field: ROOT.CUSTOMER.DISCOUNT (PIC ZZZ9 BLANK WHEN ZERO)
All spaces decoded as "0"
```

#### CBKD101_INVALID_FIELD_TYPE
**Description**: Invalid field type encountered during processing
**Severity**: Fatal
**Context**: Record number, field path, field type
**Resolution**: Check schema definition and field type compatibility

```
Error: CBKD101_INVALID_FIELD_TYPE at record 50
Field: ROOT.CUSTOMER.ID
Type: Unknown field type
```

#### CBKD301_RECORD_TOO_SHORT
**Description**: Record data is shorter than required LRECL (fixed-length records)
**Severity**: Fatal
**Context**: Record number, expected vs actual byte count, precise truncation point
**Resolution**: Check record boundaries, file truncation, or LRECL specification
**Enhanced**: Now with fail-fast validation and improved performance (4-23% gains)

```
Error: CBKD301_RECORD_TOO_SHORT
Record 15 too short: expected 120 bytes, got 85 bytes
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

### Iterator and Infrastructure Errors (CBKI*)

Iterator pipelines emit these errors when runtime configuration is inconsistent or missing required context.

#### CBKI001_INVALID_STATE
**Description**: Iterator detected an invalid state (e.g., fixed-format without configured LRECL)
**Severity**: Fatal
**Context**: Record format, iterator index, configuration hint
**Resolution**: Set `schema.lrecl_fixed` or switch to `RecordFormat::Variable` before iterating

```
Error: CBKI001_INVALID_STATE on first record
Fixed format iterator requires LRECL; set schema.lrecl_fixed or use RecordFormat::Variable
```

### File I/O Errors (CBKF*)

Errors in file operations and transfer corruption detection.

#### CBKF102_RECORD_LENGTH_INVALID
**Description**: RDW header length references an incomplete or oversized payload
**Severity**: Fatal
**Context**: Record number, byte offset, expected vs available bytes
**Resolution**: Verify binary transfer mode, ensure RDW length matches payload size, re-run with `--emit-raw` to inspect truncated data

```
Error: CBKF102_RECORD_LENGTH_INVALID at record 42
Expected payload: 64 bytes
Available payload: 17 bytes (file truncated or RDW corrupted)
```

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

#### "CBKD301_RECORD_TOO_SHORT"
**Problem**: Record data is truncated or incomplete
**Solutions**:
- Check file transfer mode (binary vs text mode corruption)
- Verify LRECL setting matches actual data record length
- Check for premature EOF or file corruption
- Use `copybook inspect` to verify expected record size
- For RDW format, ensure RDW headers are intact and correctly formatted

#### "CBKP051_UNSUPPORTED_EDITED_PIC"
**Problem**: Copybook contains Space (`B`) insertion in PIC clause
**Solution**:
- **v0.4.3+**: All edited PIC phases (E1/E2/E3) are fully supported except Space (`B`) insertion
- Remove `B` tokens from PIC clause: `PIC 99B99B99` → `PIC 999999` (post-process for formatting)
- All other edited patterns (Z, $, +/-, CR/DB, commas, asterisk, currency) work correctly

#### "CBKD421_EDITED_PIC_INVALID_FORMAT"
**Problem**: Edited numeric data doesn't match PIC pattern
**Solutions**:
- Verify data matches edited PIC pattern (e.g., `$ZZ,ZZZ.99` requires $ symbol)
- Check codepage conversion (EBCDIC→ASCII) for symbol characters
- Use `--emit-raw` to inspect raw byte values
- Validate against COBOL data generation process

#### "CBKD422_EDITED_PIC_SIGN_MISMATCH"
**Problem**: Sign character mismatch in edited field
**Solutions**:
- Verify sign editing matches PIC clause (+, -, CR, DB)
- Check data integrity and field alignment
- Confirm codepage handles sign characters correctly

#### "CBKS703_PROJECTION_FIELD_NOT_FOUND"
**Problem**: Selected field doesn't exist in schema
**Solutions**:
- Use `copybook inspect schema.cpy` to list all available fields
- Check field name spelling and case sensitivity
- Verify field is not a FILLER (FILLER fields named as `_filler_00000XXX`)
- For RENAMES aliases, use `--select` with alias name directly

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

#### "CBKD411_ZONED_BAD_SIGN"
**Problem**: Invalid sign zone or ASCII overpunch character in zoned decimal
**Solutions**:
- For EBCDIC data: Verify correct codepage and check for C/D/F sign zones
- For ASCII data: Ensure proper overpunch encoding (0-9, {A-I}, {J-R})
- Check data integrity and field alignment
- Verify that the field is properly signed (PIC S9) if sign data is present
- Use `--emit-raw` to inspect raw byte values for debugging

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
| CBKP051 | Parse | Fatal | Unsupported edited PIC token (Space `B` only) |
| CBKS121 | Schema | Fatal | Counter not found |
| CBKS141 | Schema | Fatal | Record too large |
| CBKS301 | Schema | Warning | ODO clipped |
| CBKS302 | Schema | Warning | ODO raised |
| CBKS701 | Schema | Fatal | Projection: Invalid ODO (counter not accessible) |
| CBKS702 | Schema | Fatal | Projection: Unresolved RENAMES alias |
| CBKS703 | Schema | Fatal | Projection: Field not found |
| CBKR101 | Record | Fatal | Fixed record error |
| CBKR201 | Record | Fatal | RDW read error |
| CBKR211 | Record | Warning/Fatal | RDW reserved non-zero |
| CBKF102 | File | Fatal | RDW length invalid |
| CBKF221 | File | Fatal | RDW underflow |
| CBKC201 | Charset | Fatal | JSON write error |
| CBKC301 | Charset | Warning/Fatal | Invalid EBCDIC byte |
| CBKD101 | Decode | Fatal | Invalid field type |
| CBKD301 | Decode | Fatal | Record too short |
| CBKD401 | Decode | Fatal/Warning | COMP-3 invalid nibble |
| CBKD411 | Decode | Fatal/Warning | Zoned bad sign |
| CBKD412 | Decode | Warning | Zoned blank is zero |
| CBKD421 | Decode | Fatal | Edited PIC: Invalid format (Phase E2) |
| CBKD422 | Decode | Fatal | Edited PIC: Sign mismatch (Phase E2) |
| CBKD423 | Decode | Warning | Edited PIC: Blank when zero (Phase E2) |
| CBKE501 | Encode | Fatal | JSON type mismatch |
| CBKE521 | Encode | Fatal | Array length OOB |
| CBKF104 | File | Warning | RDW suspect ASCII |

## Panic Elimination Architecture

### Enterprise Safety Implementation

copybook-rs has **eliminated all panic risks** through systematic replacement of `.unwrap()` and `.expect()` calls with structured error handling. This ensures **zero panic risk** in production environments.

#### Safe Operations Module

The `copybook_core::utils::safe_ops` module provides comprehensive panic-safe operations:

```rust
// Safe integer conversions with overflow checking
let field_offset = safe_ops::safe_u64_to_u32(offset_u64, "field offset calculation")?;
let sync_padding = safe_ops::safe_u64_to_u16(padding_u64, "sync padding calculation")?;
let record_length = safe_ops::safe_usize_to_u32(length_usize, "record length conversion")?;

// Safe string and slice operations
let token = safe_ops::safe_slice_get(&tokens, index, "parser token access")?;
let char_at = safe_ops::safe_string_char_at(&pic_string, pos, "PIC character access")?;
let parsed_num = safe_ops::safe_parse_u16(&num_str, "PIC digits parsing")?;

// Safe arithmetic with overflow protection
let array_size = safe_ops::safe_array_bound(base, count, item_size, "ODO array sizing")?;
let division_result = safe_ops::safe_divide(numerator, denominator, "field size calculation")?;

// Safe JSON formatting operations
safe_ops::safe_write(&mut buffer, format_args!("{{\"field\": {}}}", value))?;
safe_ops::safe_write_str(&mut buffer, ",\n")?;
```

#### Extension Traits for Collections

Panic-safe extension traits for common collection operations:

```rust
use copybook_core::utils::{OptionExt, VecExt, SliceExt};

// Safe option unwrapping
let field = schema.fields
    .first()
    .ok_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "Empty schema not allowed")?;

// Safe vector operations
let mut parser_stack = Vec::new();
parser_stack.push(field);
let current = parser_stack
    .pop_or_cbkp_error(ErrorCode::CBKP001_SYNTAX, "Parser stack underflow")?;

// Safe slice indexing
let token = tokens
    .get_or_cbkp_error(index, ErrorCode::CBKP001_SYNTAX, "Token index out of bounds")?;
```

#### Performance Impact

Panic elimination has **minimal performance impact**:

- **<5% overhead** while maintaining enterprise throughput targets
- **DISPLAY processing**: 205 MiB/s (reference baseline, advisory floor is 80 MiB/s)
- **COMP-3 processing**: 58 MiB/s (reference baseline, advisory floor is 40 MiB/s)
- **Hardware optimization**: Uses CPU overflow detection for maximum efficiency

#### Error Context Enhancement

All panic-safe operations include comprehensive error context:

```rust
// Before (panic risk)
let value = vector[index];  // Could panic with index out of bounds

// After (panic-safe with context)
let value = safe_ops::safe_slice_get(&vector, index, "field offset lookup")
    .map_err(|e| e.with_context("layout resolution", field_path))?;
```

#### Enterprise Reliability Features

- **Zero unsafe code** - Memory safety guaranteed
- **Structured error taxonomy** - Comprehensive CBKP*/CBKS*/CBKD*/CBKE* error codes
- **Contextual error reporting** - Detailed information for debugging
- **Graceful failure handling** - Individual record failures don't stop batch processing
- **Production monitoring** - Error aggregation and alerting patterns

#### Validation and Testing

All panic elimination changes are validated through:

- **458+ tests passing** with comprehensive coverage
- **Mutation testing** to verify error handling paths
- **Performance regression testing** to ensure targets are maintained
- **Integration testing** with enterprise-scale datasets
- **Continuous validation** through CI/CD pipelines

### Migration Benefits

The panic elimination implementation provides:

1. **Production Safety** - Zero risk of runtime panics
2. **Debugging Enhancement** - Detailed error context for troubleshooting
3. **Performance Preservation** - Enterprise throughput targets maintained
4. **Monitoring Integration** - Structured errors enable automated alerting
5. **Compliance Ready** - Suitable for regulated environments requiring high reliability

This comprehensive panic elimination ensures copybook-rs is ready for enterprise production deployments with the highest reliability standards.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
