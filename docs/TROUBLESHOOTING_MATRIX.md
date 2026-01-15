# Troubleshooting Matrix

Quick reference matrix for diagnosing and resolving copybook-rs issues, organized by error codes and common symptoms.

## Error Code Quick Reference

| Error Code | Category | Severity | Quick Fix | Section |
|------------|----------|----------|-----------|---------|
| CBKP001 | Parse | Fatal | Fix copybook syntax | [Parse Errors](#parse-errors) |
| CBKP011 | Parse | Fatal | Remove unsupported clause | [Parse Errors](#parse-errors) |
| CBKP021 | Parse | Fatal | Move ODO to tail | [Parse Errors](#parse-errors) |
| CBKP051 | Parse | Fatal | Remove edited PIC | [Parse Errors](#parse-errors) |
| CBKS121 | Schema | Fatal | Fix counter field | [Schema Errors](#schema-errors) |
| CBKS141 | Schema | Fatal | Reduce record size | [Schema Errors](#schema-errors) |
| CBKS301 | Schema | Warning | Check ODO bounds | [Schema Errors](#schema-errors) |
| CBKS302 | Schema | Warning | Check ODO bounds | [Schema Errors](#schema-errors) |
| CBKR211 | Record | Warning/Fatal | Check RDW format | [Record Errors](#record-errors) |
| CBKF221 | File | Fatal | Check record length | [File Errors](#file-errors) |
| CBKC301 | Charset | Warning/Fatal | Check codepage | [Character Errors](#character-errors) |
| CBKD401 | Decode | Fatal/Warning | Check COMP-3 data | [Data Errors](#data-errors) |
| CBKD411 | Decode | Fatal/Warning | Check sign zone | [Data Errors](#data-errors) |
| CBKD412 | Decode | Warning | Normal BWZ behavior | [Data Errors](#data-errors) |
| CBKE501 | Encode | Fatal | Fix JSON type | [Encode Errors](#encode-errors) |
| CBKE521 | Encode | Fatal | Fix array length | [Encode Errors](#encode-errors) |
| CBKF104 | File | Warning | Check transfer mode | [File Errors](#file-errors) |
| CBKI001 | Iterator | Fatal | Provide LRECL before iterating | [Iterator Errors](#iterator-errors) |

## Parse Errors

### CBKP001_SYNTAX

**Symptoms:**
- "Syntax error at line X, column Y"
- "Expected PIC clause, found USAGE"
- "Unexpected token"

**Diagnosis:**
```bash
# Check copybook syntax
copybook parse problematic.cpy --verbose
```

**Common Causes & Solutions:**

| Cause | Solution | Example |
|-------|----------|---------|
| Missing PIC clause | Add PIC clause | `05 FIELD.` → `05 FIELD PIC X(10).` |
| Invalid level number | Use 01-49 | `00 RECORD` → `01 RECORD` |
| Malformed PIC | Fix PIC syntax | `PIC X(` → `PIC X(10)` |
| Missing period | Add period | `05 FIELD PIC X(10)` → `05 FIELD PIC X(10).` |
| Invalid characters | Remove/escape | Control chars, non-ASCII |

**Advanced Diagnosis:**
```bash
# Check specific line
sed -n '15p' problematic.cpy  # Show line 15

# Check for hidden characters
cat -A problematic.cpy | head -20

# Validate COBOL syntax
copybook parse problematic.cpy 2>&1 | grep -A5 -B5 "CBKP001"
```

### CBKP011_UNSUPPORTED_CLAUSE

**Symptoms:**
- "Unsupported COBOL clause: COMP-1"
- "Feature not implemented"

**Common Unsupported Features:**

| Feature | Alternative | Migration |
|---------|-------------|-----------|
| COMP-1 | COMP-3 | `PIC S9(3)V99 COMP-1` → `PIC S9(3)V99 COMP-3` |
| COMP-2 | COMP-3 | `PIC S9(5)V99 COMP-2` → `PIC S9(5)V99 COMP-3` |
| EXTERNAL | Remove | `05 FIELD PIC X(10) EXTERNAL` → `05 FIELD PIC X(10)` |
| GLOBAL | Remove | `05 FIELD PIC X(10) GLOBAL` → `05 FIELD PIC X(10)` |

**Solution Steps:**
1. Identify unsupported clause in error message
2. Find alternative from table above
3. Update copybook
4. Test with sample data

### CBKP021_ODO_NOT_TAIL

**Symptoms:**
- "OCCURS DEPENDING ON array not at tail position"
- "ODO must be last field in group"

**Diagnosis:**
```bash
# Inspect layout to see field positions
copybook inspect problematic.cpy --show-offsets
```

**Solution:**
```cobol
// Before (incorrect)
01 RECORD.
   05 ORDER-COUNT PIC 9(3).
   05 ORDERS OCCURS 1 TO 100 DEPENDING ON ORDER-COUNT.
      10 ORDER-ID PIC 9(8).
   05 CUSTOMER-NAME PIC X(30).  // Problem: field after ODO

// After (correct)
01 RECORD.
   05 ORDER-COUNT PIC 9(3).
   05 CUSTOMER-NAME PIC X(30).  // Move before ODO
   05 ORDERS OCCURS 1 TO 100 DEPENDING ON ORDER-COUNT.
      10 ORDER-ID PIC 9(8).
```

### CBKP051_UNSUPPORTED_EDITED_PIC

**v0.4.2+ Note:** All edited PIC patterns are now fully supported (E1/E2/E3 phases), including Space (`B`) insertion. This error code is reserved for future unsupported editing tokens if any are discovered.

**Supported Edited PIC Patterns (All Now Supported):**
- `PIC ZZ9.99` - Zero suppression with decimal ✅
- `PIC $$$,$$9.99` - Currency with comma ✅
- `PIC +999.99` / `PIC -999.99` - Sign editing ✅
- `PIC 999.99CR` / `PIC 999.99DB` - Credit/debit indicators ✅
- `PIC ***,**9.99` - Check protection (asterisk fill) ✅
- `PIC 99B99B99` - Space (B) insertion ✅

## Schema Errors

### CBKS121_COUNTER_NOT_FOUND

**Symptoms:**
- "ODO counter field not found"
- "Counter field must precede array"

**Diagnosis:**
```bash
# Check field order and paths
copybook inspect schema.cpy --show-offsets | grep -E "(COUNTER|ARRAY)"
```

**Common Issues & Solutions:**

| Issue | Solution |
|-------|----------|
| Counter after array | Move counter before array in copybook |
| Wrong counter name | Fix DEPENDING ON clause |
| Counter in REDEFINES | Move counter outside REDEFINES |
| Typo in field name | Correct field name spelling |

### CBKS141_RECORD_TOO_LARGE

**Symptoms:**
- "Record size exceeds maximum"
- "Computed size: X bytes, maximum: Y bytes"

**Solutions:**

| Approach | Method |
|----------|--------|
| Reduce ODO maximum | Lower OCCURS maximum count |
| Split record | Break into multiple record types |
| Increase limit | Use `--max-record-size` flag |
| Remove unused fields | Eliminate unnecessary FILLER |

### CBKS301_ODO_CLIPPED / CBKS302_ODO_RAISED

**Symptoms:**
- "ODO counter value exceeds maximum, clipped"
- "ODO counter value below minimum, raised"

**Diagnosis:**
```bash
# Check ODO counter values in data
copybook decode schema.cpy data.bin --emit-meta --verbose 2>&1 | grep ODO
```

**Solutions:**

| Mode | Behavior | When to Use |
|------|----------|-------------|
| Lenient (default) | Clamp and warn | Data quality issues expected |
| Strict (`--strict`) | Fail immediately | High data quality requirements |
| Adjust bounds | Modify copybook | Counter values are valid |

## Record Errors

### CBKR211_RDW_RESERVED_NONZERO

**Symptoms:**
- "RDW reserved bytes are non-zero"
- "Expected 0x0000, found 0xXXXX"

**Diagnosis:**
```bash
# Examine RDW headers
hexdump -C data.bin | head -10
```

**Solutions:**

| Scenario | Solution |
|----------|----------|
| Non-standard RDW | Use `--emit-raw record+rdw` and `--use-raw` |
| Data corruption | Check file transfer mode |
| Different RDW format | Verify record format specification |
| Lenient processing | Use default mode (warnings only) |
| Strict validation | Use `--strict` to fail on non-zero |

### CBKF221_RDW_UNDERFLOW

**Symptoms:**
- "RDW length less than minimum record size"
- "Record too short for schema"

**Diagnosis:**
```bash
# Check RDW lengths vs schema
copybook inspect schema.cpy  # Note minimum record size
hexdump -C data.bin | head -20  # Check RDW values
```

**Solutions:**
1. Verify record format (`--format rdw` vs `--format fixed`)
2. Check schema matches data structure
3. Examine file for corruption
4. Validate RDW calculation

## Character Errors

### CBKC301_INVALID_EBCDIC_BYTE

**Symptoms:**
- "Invalid EBCDIC byte encountered"
- "Unmappable character at offset X"

**Diagnosis:**
```bash
# Test different codepages
copybook decode schema.cpy data.bin --codepage cp037 --output test1.jsonl
copybook decode schema.cpy data.bin --codepage cp500 --output test2.jsonl
copybook decode schema.cpy data.bin --codepage ascii --output test3.jsonl

# Compare results
wc -l test*.jsonl
```

**Solutions:**

| Issue | Solution | Command |
|-------|----------|---------|
| Wrong codepage | Try different codepage | `--codepage cp500` |
| Mixed encoding | Use replacement mode | `--on-decode-unmappable replace` |
| Binary in text field | Check field definitions | Review copybook |
| Corrupted data | Skip unmappable | `--on-decode-unmappable skip` |

**Codepage Selection Guide:**

| Region/System | Codepage | Usage |
|---------------|----------|-------|
| US/Canada | cp037 | Most common mainframe |
| Germany/Austria | cp273 | Central European |
| International | cp500 | Multi-national |
| Unix mainframes | cp1047 | Open systems |
| Euro support | cp1140 | European with Euro |
| ASCII systems | ascii | Non-mainframe COBOL |

## Data Errors

### CBKD401_COMP3_INVALID_NIBBLE

**Symptoms:**
- "Invalid nibble in packed decimal field"
- "Expected 0-9 or A-F, found X"

**Diagnosis:**
```bash
# Examine packed decimal fields
copybook decode schema.cpy data.bin --emit-raw field --output debug.jsonl
jq -r 'select(.FIELD_NAME__raw_b64) | .FIELD_NAME__raw_b64' debug.jsonl | base64 -d | hexdump -C
```

**Common Causes & Solutions:**

| Cause | Symptoms | Solution |
|-------|----------|----------|
| File transfer corruption | ASCII digits in binary | Use binary transfer mode |
| Wrong field type | Text data in COMP-3 field | Check copybook field types |
| Alignment issues | Shifted data | Verify record boundaries and SYNCHRONIZED field alignment |
| Uninitialized data | Random nibbles | Check data source |

### CBKD411_ZONED_BAD_SIGN

**Symptoms:**
- "Invalid sign zone in zoned decimal"
- "Bad overpunch character"

**Diagnosis:**
```bash
# Check sign zones
copybook decode schema.cpy data.bin --codepage cp037 --verbose 2>&1 | grep ZONED
copybook decode schema.cpy data.bin --codepage ascii --verbose 2>&1 | grep ZONED
```

**Solutions:**

| Issue | Solution |
|-------|----------|
| Wrong codepage | Switch between EBCDIC/ASCII |
| Corrupted sign | Check data integrity |
| Non-standard signs | Use lenient mode |
| Mixed encoding | Verify consistent encoding |

### CBKD412_ZONED_BLANK_IS_ZERO

**Symptoms:**
- "BLANK WHEN ZERO field contains all spaces"
- "Decoded as zero"

**This is normal behavior - no action needed.**

## Encode Errors

### CBKE501_JSON_TYPE_MISMATCH

**Symptoms:**
- "JSON value type doesn't match field type"
- "Expected string, found number"

**Common Mismatches & Solutions:**

| Field Type | Expected JSON | Common Error | Solution |
|------------|---------------|--------------|----------|
| Zoned decimal | String with exact scale | Number or wrong scale | `"123.45"` not `123.45` |
| Packed decimal | String with exact scale | Number or wrong scale | `"123.45"` not `"123.4"` |
| Binary integer | Number | String | `123` not `"123"` |
| Alphanumeric | String | Number | `"123"` not `123` |

**REDEFINES Ambiguity:**
```json
// Problem: multiple non-null views
{
  "FIELD_A": "123",
  "FIELD_B": "456"  // REDEFINES FIELD_A
}

// Solution: use only one view
{
  "FIELD_A": "123",
  "FIELD_B": null
}
```

### CBKE521_ARRAY_LEN_OOB

**Symptoms:**
- "Array length out of bounds"
- "Length X exceeds maximum Y"

**Solutions:**
1. Adjust JSON array length to fit bounds
2. Increase OCCURS maximum in copybook
3. Use ODO for variable-length arrays

## File Errors

### CBKF104_RDW_SUSPECT_ASCII

**Symptoms:**
- "RDW header suggests ASCII transfer corruption"
- "Possible text-mode transfer"

**Diagnosis:**
```bash
# Check file transfer integrity
file data.bin
hexdump -C data.bin | head -5
```

**Solutions:**
1. Re-transfer file in binary mode
2. Check FTP/transfer settings
3. Verify file hasn't been processed as text

## Performance Issues

### Slow Processing

**Symptoms:**
- Low throughput (< 10 MB/s)
- High CPU usage
- Long processing times

**Diagnosis:**
```bash
# Monitor performance
copybook decode schema.cpy data.bin --verbose --output /dev/null

# Check system resources
top -p $(pgrep copybook)
```

**Solutions:**

| Issue | Solution | Command |
|-------|----------|---------|
| Single-threaded | Use parallel processing | `--threads 8` |
| Excessive errors | Reduce error logging | `--max-errors 100` |
| Large records | Increase buffer size | System tuning |
| I/O bottleneck | Use faster storage | SSD, RAM disk |

### High Memory Usage

**Symptoms:**
- Memory growth during processing
- Out of memory errors
- System swapping

**Solutions:**
1. Verify streaming mode is active
2. Reduce thread count
3. Check for memory leaks
4. Process in smaller batches

### Inconsistent Output

**Symptoms:**
- Different results across runs
- Non-deterministic behavior

**Diagnosis:**
```bash
# Test determinism
copybook decode schema.cpy data.bin --threads 1 --output test1.jsonl
copybook decode schema.cpy data.bin --threads 8 --output test2.jsonl
diff test1.jsonl test2.jsonl
```

**Solutions:**
1. Ensure input data is stable
2. Check for race conditions
3. Verify deterministic mode
4. Report bug if confirmed

| Symptom                                             | Code       | Likely Cause                                  | Fix                                                                 |
|-----------------------------------------------------|------------|-----------------------------------------------|----------------------------------------------------------------------|
| "edited PIC …" error when loading schema            | CBKP051    | Edited picture (e.g., `ZZ9.99`, `CR`, `DB`)   | Replace with supported PIC; drop edited formatting.                 |
| "Invalid character in PIC clause: ."                | CBKP001    | Content after `.` parsed into PIC             | Ensure only spaces after the `.` in columns 8–72; we now ignore tail |
| "Invalid level 99" with line context                | CBKP001    | Non-standard level at BOL                     | Use 01–49, 66, 77, or 88; or fix misaligned columns                  |
| ODO array fails to encode                           | CBKP0xx    | Counter missing/out of range/behind array     | Define counter first; check `OCCURS … DEPENDING ON` bounds           |
| REDEFINES ambiguous on encode                       | CBKP0xx    | Multiple views match                           | Provide disambiguating data; encode a single view only               |

### Binary Field Alignment Issues

**Symptoms:**
- Binary fields reading incorrect values
- Data appears shifted or corrupted
- SYNCHRONIZED fields not working as expected

**Common Causes:**
1. **Missing SYNCHRONIZED**: Binary fields require SYNCHRONIZED for proper alignment
2. **Platform differences**: Alignment behavior differs between compilers
3. **Manual padding**: User-added padding conflicts with automatic alignment

**Diagnosis:**
```bash
# Inspect field layout with alignment information
copybook inspect schema.cpy --emit-offsets

# Check for SYNCHRONIZED usage
grep -i "SYNC\|SYNCHRONIZED" schema.cpy

# Compare with and without alignment
copybook decode schema.cpy data.bin --output aligned.jsonl
# Remove SYNCHRONIZED from copybook and retry
copybook decode schema-no-sync.cpy data.bin --output unaligned.jsonl
diff aligned.jsonl unaligned.jsonl
```

**Solutions:**
1. **Add SYNCHRONIZED**: Apply to binary fields requiring alignment
2. **IBM compatibility**: Use copybook-rs alignment which follows IBM mainframe standards
3. **Remove manual padding**: Let copybook-rs handle alignment automatically
4. **Verify alignment boundaries**: 16-bit=2-byte, 32-bit=4-byte, 64-bit=8-byte boundaries

## General Troubleshooting Workflow

### 1. Initial Assessment
```bash
# Basic validation
copybook parse schema.cpy
copybook inspect schema.cpy
file data.bin
```

### 2. Incremental Testing
```bash
# Test with small sample
head -c 1000 data.bin > sample.bin
copybook decode schema.cpy sample.bin --output sample.jsonl
```

### 3. Verbose Diagnostics
```bash
# Enable detailed logging
copybook decode schema.cpy data.bin --verbose --output debug.jsonl 2> debug.log
```

### 4. Comparative Analysis
```bash
# Compare with reference tool
reference-tool --input data.bin --output reference.json
copybook decode schema.cpy data.bin --output copybook.jsonl
# Compare outputs accounting for format differences
```

### 5. Isolation Testing
```bash
# Test individual components
copybook parse schema.cpy --output schema.json
copybook verify schema.cpy data.bin --format fixed
```

## Iterator Errors

### CBKI001_INVALID_STATE

**Symptoms:**
- "Fixed format requires a fixed record length (LRECL)."
- `RecordIterator::next()` fails on the first call.

**Diagnosis:**
```bash
# Confirm the schema exposes a fixed LRECL
copybook inspect schema.cpy --show-metadata | grep LRECL
```

**Solution:**
- Populate `schema.lrecl_fixed` before iterating in fixed mode; downstream retries will succeed once the length is set.
- If the copybook truly varies per record, switch to `RecordFormat::Variable` (or provide an ODO-derived length) instead of forcing fixed mode.

**CBKI001_INVALID_STATE** on first `next()` with `RecordFormat::Fixed`:
- Cause: `schema.lrecl_fixed` is `None` (variable schema, ODO present, or not set).
- Fix: Fixed format? Set `schema.lrecl_fixed` or change `RecordFormat` to `Variable` / supply ODO length. Error text: `Fixed format requires a fixed record length (LRECL).`

## Getting Additional Help

### Documentation Resources
- [README.md](../README.md) - Usage examples
- [CLI_REFERENCE.md](CLI_REFERENCE.md) - Complete CLI documentation
- [ERROR_CODES.md](ERROR_CODES.md) - Detailed error descriptions
- [MIGRATION_GUIDE.md](MIGRATION_GUIDE.md) - Migration from other tools

### Diagnostic Commands
```bash
# Version information
copybook --version

# Help for specific command
copybook decode --help

# Schema validation
copybook parse schema.cpy --validate

# Data verification
copybook verify schema.cpy data.bin --report report.json
```

### Reporting Issues
When reporting issues, include:
1. copybook-rs version
2. Complete error message with context
3. Minimal reproducing example
4. System information (OS, architecture)
5. Expected vs actual behavior
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
