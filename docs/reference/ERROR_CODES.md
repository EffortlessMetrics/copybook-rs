<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Error Codes Reference

copybook-rs uses a comprehensive error taxonomy with stable codes for reliable error handling and troubleshooting. **All error handling is panic-safe** with zero risk of unwrap() or expect() panics in production environments.

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

#### CBKP022_NESTED_ODO
**Description**: OCCURS DEPENDING ON array nested inside another OCCURS/ODO array
**Severity**: Fatal
**Context**: Field path of the nested ODO array
**Resolution**: Restructure the copybook so the ODO array is not contained in another OCCURS/ODO array (see `docs/design/NESTED_ODO_BEHAVIOR.md`)

```
Error: CBKP022_NESTED_ODO
Nested ODO not supported: field 'ROOT.CUSTOMER.ORDERS.ITEMS' has
OCCURS DEPENDING ON inside another OCCURS/ODO array
```

#### CBKP023_ODO_REDEFINES
**Description**: OCCURS DEPENDING ON array inside a REDEFINES region
**Severity**: Fatal
**Context**: Field path of the ODO array in the REDEFINES region
**Resolution**: Move the ODO array out of the REDEFINES region or replace it with a fixed OCCURS

```
Error: CBKP023_ODO_REDEFINES
ODO over REDEFINES not supported: field 'ROOT.CUSTOMER.ALT-VIEW.ORDERS' has
OCCURS DEPENDING ON inside a REDEFINES region
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

#### CBKP101_INVALID_PIC
**Description**: Invalid PIC clause syntax or illegal characters in the PIC string
**Severity**: Fatal
**Context**: PIC clause text, field path
**Resolution**: Fix the PIC clause to use valid COBOL picture characters

```
Error: CBKP101_INVALID_PIC
Field: ROOT.CUSTOMER.CODE
PIC: 9Q9 (illegal character 'Q')
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

#### CBKS601_RENAME_UNKNOWN_FROM
**Description**: RENAMES `from` field not found in the enclosing record scope
**Severity**: Fatal
**Context**: RENAMES alias name, missing `from` field name
**Resolution**: Ensure the field named after RENAMES exists in the same 01-level record

```
Error: CBKS601_RENAME_UNKNOWN_FROM
Alias: CUSTOMER-HEADER (RENAMES CUSTMER-ID THRU CUSTOMER-NAME)
Field CUSTMER-ID not found in record scope
```

#### CBKS602_RENAME_UNKNOWN_THRU
**Description**: RENAMES `THRU` field not found in the enclosing record scope
**Severity**: Fatal
**Context**: RENAMES alias name, missing `THRU` field name
**Resolution**: Ensure the field named after THRU exists in the same 01-level record

```
Error: CBKS602_RENAME_UNKNOWN_THRU
Alias: CUSTOMER-HEADER (RENAMES CUSTOMER-ID THRU CUSTOMER-NAM)
Field CUSTOMER-NAM not found in record scope
```

#### CBKS603_RENAME_NOT_CONTIGUOUS
**Description**: RENAMES range is not contiguous (gap between `from` and `THRU` fields)
**Severity**: Fatal
**Context**: RENAMES alias name, `from`/`THRU` field offsets
**Resolution**: Adjust the RENAMES range so it covers a contiguous byte range. Positive alignment gaps are rejected; overlapping offsets remain valid only where the applicable REDEFINES policy permits them.

#### CBKS604_RENAME_REVERSED_RANGE
**Description**: RENAMES range is reversed (`from` field starts after the `THRU` field)
**Severity**: Fatal
**Context**: RENAMES alias name, `from`/`THRU` field positions
**Resolution**: Swap the `from` and `THRU` field names so the range runs forward

```
Error: CBKS604_RENAME_REVERSED_RANGE
Alias: CUSTOMER-HEADER (RENAMES CUSTOMER-NAME THRU CUSTOMER-ID)
Range is reversed: CUSTOMER-NAME starts after CUSTOMER-ID
```

#### CBKS605_RENAME_FROM_CROSSES_GROUP
**Description**: RENAMES `from` field crosses a group boundary
**Severity**: Fatal
**Context**: RENAMES alias name, `from` field path, group boundary
**Resolution**: Restrict the RENAMES range so it does not straddle group boundaries (see `docs/design/RENAMES_NESTED_GROUPS.md`)

#### CBKS606_RENAME_THRU_CROSSES_GROUP
**Description**: RENAMES `THRU` field crosses a group boundary
**Severity**: Fatal
**Context**: RENAMES alias name, `THRU` field path, group boundary
**Resolution**: Restrict the RENAMES range so it does not straddle group boundaries (see `docs/design/RENAMES_NESTED_GROUPS.md`)

#### CBKS607_RENAME_CROSSES_OCCURS
**Description**: RENAMES range crosses an OCCURS array boundary
**Severity**: Fatal
**Context**: RENAMES alias name, OCCURS field in range
**Resolution**: Keep the RENAMES range entirely outside (or entirely inside a single element of) OCCURS arrays

```
Error: CBKS607_RENAME_CROSSES_OCCURS
Alias: ORDER-SUMMARY (RENAMES ORDER-ID THRU ORDER-ITEMS)
Range crosses OCCURS boundary at ORDER-ITEMS
```

#### CBKS608_RENAME_QUALIFIED_NAME_NOT_FOUND
**Description**: Qualified name in RENAMES clause (e.g., `FIELD OF GROUP`) could not be resolved
**Severity**: Fatal
**Context**: RENAMES alias name, qualified name
**Resolution**: Verify the qualified name path matches the record structure

#### CBKS609_RENAME_OVER_REDEFINES
**Description**: RENAMES alias spans REDEFINES field(s) (R4 scenario)
**Severity**: Fatal
**Context**: RENAMES alias name, REDEFINES fields in range
**Resolution**: Exclude REDEFINES fields from the RENAMES range or restructure the copybook

#### CBKS610_RENAME_MULTIPLE_REDEFINES
**Description**: RENAMES range spans multiple REDEFINES alternatives (R4 scenario)
**Severity**: Fatal
**Context**: RENAMES alias name, REDEFINES alternatives in range
**Resolution**: Limit the RENAMES range to a single REDEFINES alternative

#### CBKS611_RENAME_PARTIAL_OCCURS
**Description**: RENAMES range spans partial array elements (R5 scenario)
**Severity**: Fatal
**Context**: RENAMES alias name, OCCURS field partially covered
**Resolution**: Cover whole OCCURS arrays in the RENAMES range, not partial elements

#### CBKS612_RENAME_ODO_NOT_SUPPORTED
**Description**: RENAMES over OCCURS DEPENDING ON arrays is not supported (R5 scenario)
**Severity**: Fatal
**Context**: RENAMES alias name, ODO field in range
**Resolution**: Remove the ODO array from the RENAMES range; RENAMES over variable-length regions is unsupported

```
Error: CBKS612_RENAME_ODO_NOT_SUPPORTED
Alias: ORDER-VIEW (RENAMES ORDER-ID THRU ORDER-ITEMS)
ORDER-ITEMS has OCCURS DEPENDING ON; RENAMES over ODO not supported
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
**Description**: Error processing fixed-length record framing or I/O
**Severity**: Fatal
**Context**: Record number, byte offset when available, error details
**Resolution**: Check the configured LRECL, record boundaries, input/output integrity, and available address space

Use this code for fixed-format framing failures such as truncated input,
oversize output records, and read/write/flush failures. RDW framing paths that
use `CBKF*` continue to use those record-format codes; other RDW-specific
failures, such as `CBKR201_RDW_READ_ERROR` and `CBKR211_RDW_RESERVED_NONZERO`,
remain separate.

```text
Error: CBKR101_FIXED_RECORD_ERROR at record 75
Fixed-length record processing failed: incomplete record at end of file
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

#### CBKR202_RDW_WRITE_ERROR

**Description**: Error writing or flushing Record Descriptor Word (RDW) header or payload
**Severity**: Fatal
**Context**: Record number, byte offset when available, I/O error details
**Resolution**: Check output integrity, permissions, and available disk space

```text
Error: CBKR202_RDW_WRITE_ERROR at record 100
Failed to write or flush RDW output: Broken pipe
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
**Description**: Record data is too short for the required field data or LRECL
**Severity**: Fatal
**Context**: Record number, field path (when field-level), expected vs actual byte count
**Resolution**: Check data integrity, file transfer mode (binary vs text), record boundaries, or LRECL specification; use `copybook inspect` to verify expected record size

```
Error: CBKD301_RECORD_TOO_SHORT
Record 15 too short: expected 120 bytes, got 85 bytes
```

#### CBKD302_EDITED_PIC_NOT_IMPLEMENTED
**Description**: Legacy taxonomy identifier retained for compatibility; current edited PIC decode is implemented in Phase E2 and does not emit CBKD302
**Severity**: Legacy, not emitted by current decode paths
**Context**: Historical Phase E1 edited PIC decode boundary
**Resolution**: Use the current edited PIC diagnostics: CBKD421 for invalid format, CBKD422 for sign mismatch, and CBKD423 for blank-when-zero warnings

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

#### CBKD410_ZONED_OVERFLOW
**Description**: Zoned decimal value exceeded numeric capacity during decode
**Severity**: Error
**Context**: Record number, field path, digit count
**Resolution**: Verify the PIC clause digit count matches the data, check for corruption

```
Error: CBKD410_ZONED_OVERFLOW at record 450
Field: ROOT.CUSTOMER.BALANCE
Value exceeds capacity for PIC S9(18)
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

#### CBKD413_ZONED_INVALID_ENCODING
**Description**: Invalid zoned decimal encoding format detected (bytes match neither the expected EBCDIC nor ASCII zoned layout)
**Severity**: Error
**Context**: Record number, field path, byte values
**Resolution**: Verify codepage selection and zoned encoding options, inspect raw bytes with `--emit-raw`

#### CBKD414_ZONED_MIXED_ENCODING
**Description**: Mixed ASCII/EBCDIC encoding detected within a single zoned decimal field
**Severity**: Error
**Context**: Record number, field path, per-byte encoding classification
**Resolution**: Fix upstream data conversion so each field uses a single encoding, verify file transfer mode

#### CBKD415_ZONED_ENCODING_AMBIGUOUS
**Description**: Zoned encoding auto-detection failed or remains ambiguous
**Severity**: Error
**Context**: Record number, field path
**Resolution**: Specify the zoned encoding explicitly instead of relying on auto-detection

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

#### CBKD431_FLOAT_NAN
**Description**: Floating-point field (COMP-1/COMP-2) contains NaN; the value is decoded as JSON null
**Severity**: Reserved — not currently emitted (see note)
**Context**: Record number, field path
**Resolution**: Check upstream data generation for uninitialized float fields; null output is the documented behavior
**Note**: The current decode path converts NaN to JSON `null` and does **not** raise this code — it is reserved in the taxonomy for a future strict float-handling policy. The null-decode contract is pinned by `crates/copybook-codec/tests/numeric_evidence_matrix.rs::float_special_values_decode_to_null`.

#### CBKD432_FLOAT_INFINITY
**Description**: Floating-point field (COMP-1/COMP-2) contains infinity; the value is decoded as JSON null
**Severity**: Reserved — not currently emitted (see note)
**Context**: Record number, field path
**Resolution**: Check upstream data generation for overflowed float values; null output is the documented behavior
**Note**: The current decode path converts ±Infinity to JSON `null` and does **not** raise this code — it is reserved in the taxonomy for a future strict float-handling policy. The null-decode contract is pinned by `crates/copybook-codec/tests/numeric_evidence_matrix.rs::float_special_values_decode_to_null`.

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

#### CBKE505_SCALE_MISMATCH
**Description**: Decimal scale mismatch during field encoding (JSON value has more fractional digits than the PIC clause allows)
**Severity**: Fatal
**Context**: Field path, PIC scale, provided value scale
**Resolution**: Round or reformat the JSON value to match the field's implied decimal scale (V position)

```
Error: CBKE505_SCALE_MISMATCH
Field: ROOT.CUSTOMER.BALANCE (PIC S9(7)V99)
Value: "123.456" has scale 3, field allows scale 2
```

#### CBKE510_NUMERIC_OVERFLOW
**Description**: Numeric value exceeds the field's digit capacity during encoding
**Severity**: Fatal
**Context**: Field path, digit capacity, provided value
**Resolution**: Reduce the value or widen the PIC clause digit count

```
Error: CBKE510_NUMERIC_OVERFLOW
Field: ROOT.CUSTOMER.COUNT (PIC 9(3))
Value: 12345 exceeds 3-digit capacity
```

#### CBKE515_STRING_LENGTH_VIOLATION
**Description**: String value exceeds the field's declared size during encoding
**Severity**: Fatal
**Context**: Field path, field size, encoded byte length
**Resolution**: Truncate the string or widen the PIC X(n) field size

```text
Error: CBKE515_STRING_LENGTH_VIOLATION
Field: ROOT.CUSTOMER.NAME (PIC X(20))
Encoded byte length: 27 exceeds field capacity 20
```

#### CBKE521_ARRAY_LEN_OOB
**Description**: JSON array length out of bounds for OCCURS. Also raised for OCCURS DEPENDING ON (ODO) fields when the JSON counter field's value does not equal the JSON array's length, since the two are encoded independently and a mismatch would silently drop or fabricate elements on decode.
**Severity**: Fatal
**Context**: Record number, field path, array length, min/max bounds (or counter field, counter value, array length for an ODO counter/array mismatch)
**Resolution**: Adjust array length or OCCURS bounds, or make the ODO counter value match the array length

```
Error: CBKE521_ARRAY_LEN_OOB at record 800
Field: ROOT.CUSTOMER.ORDERS
Array length: 150
Bounds: min=1, max=100
```

#### CBKE530_SIGN_SEPARATE_ENCODE_ERROR
**Description**: SIGN SEPARATE field could not be encoded (invalid sign character or value inconsistent with the separate sign position)
**Severity**: Error
**Context**: Field path, sign position (LEADING/TRAILING), provided value
**Resolution**: Ensure the JSON value's sign is representable for the SIGN SEPARATE field configuration

#### CBKE531_FLOAT_ENCODE_OVERFLOW
**Description**: Float encode overflow: f64 JSON value is too large for an f32 COMP-1 field
**Severity**: Error
**Context**: Field path, provided value
**Resolution**: Reduce the value to f32 range or change the field to COMP-2 (f64)

```
Error: CBKE531_FLOAT_ENCODE_OVERFLOW
Field: ROOT.MEASUREMENT.RATIO (COMP-1)
Value: 3.5e50 exceeds f32 range
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

#### CBKF001_FILE_READ_ERROR
**Description**: A file named on the command line could not be opened or read
**Severity**: Fatal
**Context**: The path, and which argument it came from (copybook or input file)
**Resolution**: Check the path, spelling, and read permissions

```
Error: CBKF001_FILE_READ_ERROR
failed to read copybook '/nope.cpy': No such file or directory (os error 2)
```

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

### Audit Errors (CBKA*)

Errors in performance and compliance audit operations.

#### CBKA001_BASELINE_ERROR
**Description**: Performance baseline operation failed (baseline file could not be read, parsed, serialized, or written)
**Severity**: Error
**Context**: Baseline file path, underlying I/O or serialization error
**Resolution**: Check that the baseline file exists, is valid JSON, and is writable

```
Error: CBKA001_BASELINE_ERROR
Failed to read baseline file: No such file or directory (os error 2)
```

### Arrow/Writer Errors (CBKW*)

Errors in Apache Arrow and Parquet conversion (copybook-arrow integration).

#### CBKW001_SCHEMA_CONVERSION
**Description**: COBOL schema could not be converted to an Arrow schema
**Severity**: Error
**Context**: Schema details, conversion failure reason
**Resolution**: Check the copybook for constructs unsupported in Arrow conversion

#### CBKW002_TYPE_MAPPING
**Description**: A `FieldKind` has no valid Arrow type mapping
**Severity**: Error
**Context**: Field path, field kind
**Resolution**: Remove or restructure the unsupported field type for Arrow output

#### CBKW003_DECIMAL_OVERFLOW
**Description**: Decimal precision exceeds the Decimal128 limit (38 digits)
**Severity**: Error
**Context**: Field path, PIC digit count
**Resolution**: Reduce the field's digit count to 38 or fewer for Arrow output

#### CBKW004_BATCH_BUILD
**Description**: Arrow `RecordBatch` construction failed
**Severity**: Error
**Context**: Batch details, underlying Arrow error
**Resolution**: Check input data consistency and memory availability

#### CBKW005_PARQUET_WRITE
**Description**: Parquet file write failed
**Severity**: Error
**Context**: Output path, underlying I/O or Parquet error
**Resolution**: Check output file permissions, disk space, and Parquet writer configuration

## Error Handling Modes

The processing mode controls whether recoverable record problems stop the
operation or are accumulated; it does not replace the taxonomy-based process
exit contract. When a command reports an error, use the emitted `CBK*` code as
the precise remediation key and the family mapping below as the automation
key.

### Strict Mode (`--strict`)
- Stop processing on first data error
- ODO out-of-bounds → fatal error
- RDW reserved bytes non-zero → fatal error
- The exit code remains the code for the emitted error family (for example,
  `CBKD*` → 2 or `CBKR*` → 4).

### Lenient Mode (default)
- Continue processing after recoverable errors
- ODO out-of-bounds → clamp with warning
- RDW reserved bytes non-zero → warning
- Skip bad records and continue
- Warnings alone exit 0. Errors still return the code for the emitted error
  family; lenient processing does not turn them into exit code 1.

### Max Errors (`--max-errors N`)
- Stop after N errors in lenient mode
- Useful for large files with systematic issues
- When the limit is reached, return the code for the emitted error family.

### Exit-code remediation map

| Error family | Process exit | First remediation step |
|:---:|---:|---|
| `CBKD*` | 2 | Inspect the data value, record bytes, and code-page/format options. |
| `CBKE*`, `CBKP*`, `CBKS*` | 3 | Fix the encode input, copybook syntax, or schema constraint named by the code. |
| `CBKF*`, `CBKR*` | 4 | Check the input path, transfer integrity, record boundaries, or RDW framing. |
| `CBKI*` | 5 | Preserve the full diagnostic and report an internal orchestration failure. |

The complete command-level table, including `CBK?` and command-specific
`verify`/`determinism` semantics, is maintained in
[`docs/CLI_REFERENCE.md`](../CLI_REFERENCE.md#exit-codes).

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
- **v0.4.0+**: All edited PIC phases (E1/E2/E3) are fully supported except Space (`B`) insertion
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

All 65 stable error codes across 10 families:

| Code | Category | Severity | Description |
|------|----------|----------|-------------|
| CBKP001 | Parse | Fatal | Syntax error |
| CBKP011 | Parse | Fatal | Unsupported clause |
| CBKP021 | Parse | Fatal | ODO not at tail |
| CBKP022 | Parse | Fatal | Nested ODO not supported |
| CBKP023 | Parse | Fatal | ODO over REDEFINES not supported |
| CBKP051 | Parse | Fatal | Unsupported edited PIC token (Space `B` only) |
| CBKP101 | Parse | Fatal | Invalid PIC clause |
| CBKS121 | Schema | Fatal | Counter not found |
| CBKS141 | Schema | Fatal | Record too large |
| CBKS301 | Schema | Warning | ODO clipped |
| CBKS302 | Schema | Warning | ODO raised |
| CBKS601 | Schema | Fatal | RENAMES: from field not found |
| CBKS602 | Schema | Fatal | RENAMES: thru field not found |
| CBKS603 | Schema | Fatal | RENAMES: range not contiguous |
| CBKS604 | Schema | Fatal | RENAMES: reversed range |
| CBKS605 | Schema | Fatal | RENAMES: from crosses group boundary |
| CBKS606 | Schema | Fatal | RENAMES: thru crosses group boundary |
| CBKS607 | Schema | Fatal | RENAMES: range crosses OCCURS |
| CBKS608 | Schema | Fatal | RENAMES: qualified name not found |
| CBKS609 | Schema | Fatal | RENAMES: spans REDEFINES field(s) |
| CBKS610 | Schema | Fatal | RENAMES: spans multiple REDEFINES alternatives |
| CBKS611 | Schema | Fatal | RENAMES: partial OCCURS coverage |
| CBKS612 | Schema | Fatal | RENAMES: ODO not supported |
| CBKS701 | Schema | Fatal | Projection: Invalid ODO (counter not accessible) |
| CBKS702 | Schema | Fatal | Projection: Unresolved RENAMES alias |
| CBKS703 | Schema | Fatal | Projection: Field not found |
| CBKR101 | Record | Fatal | Fixed record error |
| CBKR201 | Record | Fatal | RDW read error |
| CBKR202 | Record | Fatal | RDW write error |
| CBKR211 | Record | Warning/Fatal | RDW reserved non-zero |
| CBKC201 | Charset | Fatal | JSON write error |
| CBKC301 | Charset | Warning/Fatal | Invalid EBCDIC byte |
| CBKD101 | Decode | Fatal | Invalid field type |
| CBKD301 | Decode | Fatal | Record too short |
| CBKD302 | Decode | Error | Edited PIC decode not implemented (Phase E1) |
| CBKD401 | Decode | Fatal/Warning | COMP-3 invalid nibble |
| CBKD410 | Decode | Error | Zoned overflow |
| CBKD411 | Decode | Fatal/Warning | Zoned bad sign |
| CBKD412 | Decode | Warning | Zoned blank is zero |
| CBKD413 | Decode | Error | Zoned invalid encoding |
| CBKD414 | Decode | Error | Zoned mixed ASCII/EBCDIC encoding |
| CBKD415 | Decode | Error | Zoned encoding ambiguous |
| CBKD421 | Decode | Fatal | Edited PIC: Invalid format (Phase E2) |
| CBKD422 | Decode | Fatal | Edited PIC: Sign mismatch (Phase E2) |
| CBKD423 | Decode | Warning | Edited PIC: Blank when zero (Phase E2) |
| CBKD431 | Decode | Reserved | Float NaN (decoded as null; code not currently emitted) |
| CBKD432 | Decode | Reserved | Float infinity (decoded as null; code not currently emitted) |
| CBKI001 | Infrastructure | Fatal | Invalid iterator/internal state |
| CBKE501 | Encode | Fatal | JSON type mismatch |
| CBKE505 | Encode | Fatal | Decimal scale mismatch |
| CBKE510 | Encode | Fatal | Numeric overflow |
| CBKE515 | Encode | Fatal | String length violation |
| CBKE521 | Encode | Fatal | Array length OOB |
| CBKE530 | Encode | Error | SIGN SEPARATE encode error |
| CBKE531 | Encode | Error | Float encode overflow (f64 to f32) |
| CBKF001 | File | Fatal | Input file could not be read |
| CBKF102 | File | Fatal | RDW length invalid |
| CBKF104 | File | Warning | RDW suspect ASCII |
| CBKF221 | File | Fatal | RDW underflow |
| CBKA001 | Audit | Error | Performance baseline error |
| CBKW001 | Arrow/Writer | Error | Arrow schema conversion failed |
| CBKW002 | Arrow/Writer | Error | No Arrow type mapping for field kind |
| CBKW003 | Arrow/Writer | Error | Decimal exceeds Decimal128 (38 digits) |
| CBKW004 | Arrow/Writer | Error | RecordBatch build failure |
| CBKW005 | Arrow/Writer | Error | Parquet write failure |

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
- **DISPLAY processing**: 2.15+ GiB/s (32x enterprise baseline)
- **COMP-3 processing**: 100+ MiB/s (3x enterprise baseline)
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
