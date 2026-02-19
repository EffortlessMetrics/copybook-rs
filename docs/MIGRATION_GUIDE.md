# Migration Guide

Guide for migrating from other COBOL data processing tools to copybook-rs.

## Overview

This guide helps users migrate from existing COBOL data processing tools to copybook-rs. We cover common migration scenarios, feature mappings, and provide practical examples for smooth transitions.

## Supported Migration Paths

- **IBM COBOL Tools** (Enterprise COBOL, COBOL for z/OS)
- **Micro Focus COBOL** (Visual COBOL, Net Express)
- **Open Source Tools** (GnuCOBOL, cb2xml)
- **Java Libraries** (COBOL copybook parsers)
- **Python Tools** (pycopybook, cobrix)
- **Legacy Mainframe Utilities**

## General Migration Strategy

### 1. Assessment Phase
- Inventory existing copybooks and data files
- Identify COBOL features in use
- Document current processing workflows
- Test with sample data

### 2. Conversion Phase
- Convert copybooks to supported format
- Update data processing scripts
- Validate output against existing tools
- Performance testing and tuning

### 3. Deployment Phase
- Parallel processing validation
- Gradual rollout with monitoring
- Documentation updates
- Team training

## Feature Compatibility Matrix

| Feature | copybook-rs | IBM COBOL | Micro Focus | GnuCOBOL | Notes |
|---------|-------------|-----------|-------------|----------|-------|
| Level 01-49 | ✅ | ✅ | ✅ | ✅ | Full support |
| PIC X(n) | ✅ | ✅ | ✅ | ✅ | Alphanumeric |
| PIC 9(n) | ✅ | ✅ | ✅ | ✅ | Numeric display |
| PIC S9(n) | ✅ | ✅ | ✅ | ✅ | Signed numeric |
| COMP/BINARY | ✅ | ✅ | ✅ | ✅ | Binary integers |
| COMP-3 | ✅ | ✅ | ✅ | ✅ | Packed decimal |
| COMP-1 | ⚠️ | ✅ | ✅ | ✅ | Experimental via `comp_1` feature flag |
| COMP-2 | ⚠️ | ✅ | ✅ | ✅ | Experimental via `comp_2` feature flag |
| REDEFINES | ✅ | ✅ | ✅ | ✅ | Full support |
| OCCURS | ✅ | ✅ | ✅ | ✅ | Fixed arrays |
| OCCURS DEPENDING ON | ✅ | ✅ | ✅ | ✅ | Tail position only |
| SYNCHRONIZED | ✅ | ✅ | ✅ | ✅ | Alignment support |
| BLANK WHEN ZERO | ✅ | ✅ | ✅ | ✅ | Special handling |
| Edited PIC | ⚠️ | ✅ | ✅ | ✅ | Supported for E1-E3.7 |
| SIGN SEPARATE | ⚠️ | ✅ | ✅ | ✅ | Supported behind `sign_separate` |
| 66/88 levels | ✅ | ✅ | ✅ | ✅ | 66/88-level names/values supported (`66` aliasing and `88` condition handling) |

Legend: ✅ Supported, ❌ Not supported, ⚠️ Partial support

## Migration from IBM COBOL Tools

### Enterprise COBOL / COBOL for z/OS

**Common Differences:**
- copybook-rs treats COMP-1/COMP-2 as experimental under feature flags and defaults to rejection
- Edited PIC clauses are supported for E1-E3.7, but older runtimes should still validate before migration
- SIGN SEPARATE clauses are now supported behind `sign_separate` and may vary by pattern

**Migration Steps:**

1. **Convert Copybooks:**
```cobol
// Before (IBM COBOL)
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID    PIC 9(8).
   05 CUSTOMER-NAME  PIC X(30).
   05 BALANCE        PIC ZZ,ZZ9.99.     // Edited PIC
   05 RATE           PIC S9(3)V99 COMP-1. // Floating point

// After (copybook-rs compatible)
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID    PIC 9(8).
   05 CUSTOMER-NAME  PIC X(30).
   05 BALANCE        PIC 9(5)V99.       // Non-edited
   05 RATE           PIC S9(3)V99 COMP-3. // Packed decimal
```

2. **Update Processing Scripts:**
```bash
# Before (IBM utilities)
copybook2json -i customer.cpy -d data.bin -o output.json

# After (copybook-rs)
copybook decode customer.cpy data.bin --format fixed --output output.jsonl
```

3. **Handle EBCDIC Conversion:**
```bash
# Specify appropriate code page
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --output output.jsonl
```

### IBM File Manager Integration

**Before:**
```jcl
//STEP1    EXEC PGM=FILEAID
//SYSPRINT DD SYSOUT=*
//COPYBOOK DD DSN=MY.COPYBOOK.LIB(CUSTOMER),DISP=SHR
//INPUT    DD DSN=MY.DATA.FILE,DISP=SHR
//OUTPUT   DD DSN=MY.JSON.FILE,DISP=(NEW,CATLG)
```

**After:**
```bash
# Download files from mainframe first
copybook decode customer.cpy customer-data.bin \
  --format fixed \
  --codepage cp037 \
  --output customer-data.jsonl
```

## Migration from Micro Focus COBOL

### Visual COBOL / Net Express

**Common Differences:**
- Different binary integer formats (little-endian vs big-endian)
- Different COMP-3 packing in some cases
- File format differences

**Migration Steps:**

1. **Check Binary Format:**
```cobol
// Micro Focus may use little-endian
05 BINARY-FIELD PIC 9(8) COMP.

// copybook-rs uses big-endian (IBM mainframe standard)
// Data may need byte-order conversion
```

2. **Update File Processing:**
```bash
# Before (Micro Focus utilities)
cobrun fileconv -f fixed -l 100 input.dat output.txt

# After (copybook-rs)
copybook decode schema.cpy input.dat \
  --format fixed \
  --codepage ascii \
  --output output.jsonl
```

3. **Handle Character Encoding:**
```bash
# Micro Focus often uses ASCII
copybook decode schema.cpy data.bin \
  --codepage ascii \
  --output output.jsonl
```

## Migration from Open Source Tools

### GnuCOBOL

**Migration Steps:**

1. **Copybook Compatibility:**
Most GnuCOBOL copybooks work directly with copybook-rs.

2. **Data File Conversion:**
```bash
# Before (GnuCOBOL runtime)
cobrun program < input.dat > output.txt

# After (copybook-rs)
copybook decode schema.cpy input.dat \
  --format fixed \
  --output output.jsonl
```

### cb2xml

**Before:**
```bash
# Convert copybook to XML schema
cb2xml -i customer.cpy -o customer.xml

# Process data with custom tools
java -jar data-processor.jar customer.xml data.bin
```

**After:**
```bash
# Direct processing with copybook-rs
copybook decode customer.cpy data.bin \
  --format fixed \
  --output data.jsonl

# Optional: convert to XML if needed
jq -r '@xml' data.jsonl > data.xml
```

## Migration from Java Libraries

### Common Java COBOL Libraries

**Before (Java):**
```java
CobolCopybook copybook = new CobolCopybook("customer.cpy");
CobolDecoder decoder = new CobolDecoder(copybook, "CP037", true);

FileInputStream input = new FileInputStream("data.bin");
while (input.available() > 0) {
    CobolRecord record = decoder.decode(input);
    System.out.println(record.toJson());
}
```

**After (copybook-rs CLI):**
```bash
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --strict \
  --output data.jsonl
```

**After (copybook-rs Rust API):**
```rust
use copybook_core::parse_copybook;
use copybook_codec::{RecordDecoder, DecodeOptions, Codepage};

let copybook_text = std::fs::read_to_string("customer.cpy")?;
let schema = parse_copybook(&copybook_text)?;

let opts = DecodeOptions {
    codepage: Codepage::Cp037,
    strict: true,
    ..Default::default()
};

let mut decoder = RecordDecoder::new(&schema, &opts)?;
for record_result in decoder.decode_file("data.bin")? {
    let json = record_result?;
    println!("{}", serde_json::to_string(&json)?);
}
```

## Migration from Python Tools

### pycopybook

**Before (Python):**
```python
from pycopybook import CopybookParser

parser = CopybookParser()
copybook = parser.parse_file('customer.cpy')

with open('data.bin', 'rb') as f:
    for record in copybook.parse_records(f):
        print(json.dumps(record))
```

**After (copybook-rs):**
```bash
copybook decode customer.cpy data.bin \
  --format fixed \
  --output data.jsonl
```

### cobrix (Spark)

**Before (Scala/Spark):**
```scala
val df = spark.read
  .format("cobol")
  .option("copybook", "customer.cpy")
  .option("encoding", "cp037")
  .load("data.bin")

df.write.json("output")
```

**After (copybook-rs + Spark):**
```bash
# Convert to JSONL first
copybook decode customer.cpy data.bin \
  --format fixed \
  --codepage cp037 \
  --output data.jsonl

# Then load into Spark
val df = spark.read.json("data.jsonl")
```

## Common Migration Challenges

### 1. Edited PIC Clauses

**Problem:** copybook-rs doesn't support edited PIC clauses.

**Solution:** Convert to non-edited format and handle formatting in post-processing.

```cobol
// Before
05 AMOUNT PIC $$$,$$9.99.

// After
05 AMOUNT PIC 9(6)V99.
```

```bash
# Post-process for formatting
copybook decode schema.cpy data.bin --output data.jsonl
jq '.AMOUNT = (.AMOUNT | tonumber | . / 100 | "$\(. | tostring)")' data.jsonl
```

### 2. COMP-1/COMP-2 Floating Point

**Problem:** copybook-rs doesn't support floating point types.

**Solution:** Convert to packed decimal or handle as binary.

```cobol
// Before
05 RATE PIC S9(3)V99 COMP-1.

// After
05 RATE PIC S9(3)V99 COMP-3.
```

### 3. SIGN SEPARATE

**Problem:** copybook-rs doesn't support separate sign.

**Solution:** Convert to standard signed format.

```cobol
// Before
05 BALANCE PIC S9(7)V99 SIGN LEADING SEPARATE.

// After
05 BALANCE PIC S9(7)V99.
```

### 4. Different Binary Formats

**Problem:** Different tools use different binary formats.

**Solution:** Use data conversion utilities or adjust expectations.

```bash
# Check binary format with hex dump
hexdump -C data.bin | head

# Convert if necessary (example for little-endian to big-endian)
# Use custom conversion script or tools
```

### 5. Character Encoding Issues

**Problem:** Different default character encodings.

**Solution:** Explicitly specify codepage.

```bash
# Test different codepages
copybook decode schema.cpy data.bin --codepage cp037 --output test-cp037.jsonl
copybook decode schema.cpy data.bin --codepage ascii --output test-ascii.jsonl

# Compare results
diff test-cp037.jsonl test-ascii.jsonl
```

## Enabling Strict Validation

Turn on `--strict` to enforce normative rules: ODO bounds/ordering, REDEFINES ambiguity as hard errors, and explicit `CBKP051` for edited PIC.
Lenient mode preserves loader robustness while surfacing warnings.

## Validation and Testing

### 1. Output Comparison

```bash
# Generate reference output with old tool
old-tool --input data.bin --copybook schema.cpy --output reference.json

# Generate output with copybook-rs
copybook decode schema.cpy data.bin --output test.jsonl

# Compare (accounting for format differences)
python compare-outputs.py reference.json test.jsonl
```

### 2. Round-Trip Testing

```bash
# Test round-trip fidelity
copybook decode schema.cpy original.bin \
  --emit-raw record \
  --output decoded.jsonl

copybook encode schema.cpy decoded.jsonl \
  --use-raw \
  --output roundtrip.bin

# Verify identical
diff original.bin roundtrip.bin
```

### 3. Performance Comparison

```bash
# Benchmark old tool
time old-tool --input large-data.bin --output old-output.json

# Benchmark copybook-rs
time copybook decode schema.cpy large-data.bin --output new-output.jsonl

# Compare throughput
```

## Performance Optimization

### 1. Parallel Processing

```bash
# Use multiple threads for large files
copybook decode schema.cpy large-data.bin \
  --threads 8 \
  --output data.jsonl
```

### 2. Memory Management

```bash
# For very large files, ensure streaming mode
copybook decode schema.cpy huge-data.bin \
  --output data.jsonl \
  --verbose  # Monitor memory usage
```

### 3. Error Handling

```bash
# Optimize error handling for production
copybook decode schema.cpy data.bin \
  --max-errors 1000 \
  --output data.jsonl \
  2> errors.log
```

## Integration Patterns

### 1. ETL Pipeline Integration

```bash
#!/bin/bash
# ETL pipeline script

# Extract
copybook decode customer.cpy mainframe-data.bin \
  --format fixed \
  --codepage cp037 \
  --emit-meta \
  --output extracted.jsonl

# Transform
jq '.customer_name = (.customer_name | ascii_upcase)' extracted.jsonl > transformed.jsonl

# Load
copybook encode customer.cpy transformed.jsonl \
  --format fixed \
  --codepage cp037 \
  --output processed-data.bin
```

### 2. Streaming Integration

```rust
// Rust streaming integration
use copybook_codec::RecordDecoder;
use tokio::io::{AsyncBufReadExt, BufReader};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut decoder = RecordDecoder::new(&schema, &opts)?;
    let file = tokio::fs::File::open("data.bin").await?;
    let reader = BufReader::new(file);
    
    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await? {
        let json = decoder.decode_record(line.as_bytes())?;
        // Process record
    }
    
    Ok(())
}
```

### 3. Batch Processing

```bash
# Process multiple files
for file in data/*.bin; do
    output="${file%.bin}.jsonl"
    copybook decode schema.cpy "$file" --output "$output"
done
```

## Troubleshooting Migration Issues

### 1. Schema Parsing Errors

**Error:** `CBKP051_UNSUPPORTED_EDITED_PIC`
**Solution:** Remove Space (`B`) insertion from PIC clause (all other edited patterns supported in v0.4.0+)

**Error:** `CBKP021_ODO_NOT_TAIL`
**Solution:** Move ODO arrays to end of containing group

### 2. Data Decoding Errors

**Error:** `CBKD401_COMP3_INVALID_NIBBLE`
**Solution:** Check file transfer mode and data integrity

**Error:** `CBKC301_INVALID_EBCDIC_BYTE`
**Solution:** Verify correct codepage setting

### 3. Performance Issues

**Problem:** Slower than expected processing
**Solutions:**
- Use parallel processing (`--threads`)
- Check for excessive error logging
- Verify streaming mode is active

### 4. Output Format Differences

**Problem:** JSON structure differs from previous tool
**Solutions:**
- Use `--emit-meta` for additional context
- Check REDEFINES handling
- Verify field name mapping

## Best Practices for Migration

1. **Start Small:** Begin with simple copybooks and small data files
2. **Validate Thoroughly:** Compare outputs extensively before production use
3. **Document Changes:** Keep track of copybook modifications and workarounds
4. **Test Performance:** Benchmark against existing tools
5. **Plan Rollback:** Have a rollback plan in case of issues
6. **Train Team:** Ensure team understands new tool capabilities and limitations
7. **Monitor Production:** Watch for errors and performance issues after deployment

## Getting Help

1. **Check Error Codes:** Refer to [ERROR_CODES.md](ERROR_CODES.md)
2. **Review Examples:** See [README.md](../README.md) for usage examples
3. **Test Incrementally:** Start with simple cases and build complexity
4. **Use Verbose Mode:** Enable detailed logging for troubleshooting
5. **Community Support:** Join discussions and report issues on GitHub

## Migration Checklist

- [ ] Inventory existing copybooks and identify unsupported features
- [ ] Validate whether `sign_separate`, `comp_1`, and `comp_2` feature flags are required for your schema
- [ ] Keep edited PIC clauses only where supported patterns are confirmed via parser tests
- [ ] Replace unsupported nested ODO or R4-R6 RENAMES patterns with safer alternatives
- [ ] Test copybook parsing with `copybook parse`
- [ ] Validate data decoding with sample files
- [ ] Compare outputs with existing tools
- [ ] Test round-trip encoding/decoding
- [ ] Benchmark performance with production-sized data
- [ ] Update processing scripts and documentation
- [ ] Train team on new tool usage
- [ ] Plan phased rollout with monitoring
- [ ] Prepare rollback procedures
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
