<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Binary Round-Trip Fidelity in copybook-rs

## Overview

Binary round-trip fidelity is a critical capability in copybook-rs that ensures **byte-perfect data preservation** during decode/encode cycles. This document explains the concepts, importance, and implementation of this feature for enterprise mainframe data processing.

## What is Binary Round-Trip Fidelity?

Binary round-trip fidelity means that when you:
1. **Decode** binary mainframe data to JSON
2. **Encode** that JSON back to binary data

The resulting binary data is **byte-identical** to the original input. This is achieved by preserving critical formatting information during the JSON conversion process.

### Simple Example

```
Original Binary: [0x31, 0x32, 0x33, 0x34, 0x35]  // ASCII zoned decimal "12345"
↓ decode
JSON: {"FIELD": "12345", "_encoding_metadata": {"FIELD": {"zoned_encoding": "ascii"}}}
↓ encode
Result Binary:  [0x31, 0x32, 0x33, 0x34, 0x35]  // Identical to original!
```

## Why Binary Round-Trip Fidelity Matters

### Enterprise Requirements

1. **Regulatory Compliance**: Financial and healthcare industries often require byte-perfect audit trails
2. **Legal Documentation**: Court-admissible evidence requires data integrity guarantees
3. **System Integration**: Mainframe systems expect data in specific binary formats
4. **Data Migration**: Modernization projects need zero data transformation guarantees

### Business Scenarios

**Scenario 1: Regulatory Audit**
```
A bank must prove that customer account balances extracted from mainframe systems
for regulatory reporting maintain exact binary representation from the source system.
```

**Scenario 2: Legal Discovery**
```
A healthcare organization needs to provide court-admissible evidence that patient
records extracted for legal proceedings are byte-identical to mainframe originals.
```

**Scenario 3: System Modernization**
```
An insurance company migrating from mainframe COBOL to cloud-native systems
requires guarantee that no data transformation occurs during the migration process.
```

## The Zoned Decimal Challenge

The primary challenge for binary round-trip fidelity in COBOL data processing comes from **zoned decimal fields**, which can be encoded in two different formats:

### ASCII Zoned Decimals
- **Zone Nibbles**: `0x3` (digits stored as 0x30-0x39)
- **Common in**: ASCII mainframe exports, modernized systems
- **Example**: Number `123` stored as `[0x31, 0x32, 0x33]`

### EBCDIC Zoned Decimals
- **Zone Nibbles**: `0xF` (digits stored as 0xF0-0xF9)
- **Common in**: Native mainframe COBOL, legacy systems
- **Example**: Number `123` stored as `[0xF1, 0xF2, 0xF3]`

### The Problem

Both formats represent the **same logical value** (`123`) but have **different binary representations**. Without encoding preservation:

```
Input:  [0x31, 0x32, 0x33]  (ASCII zones)
JSON:   {"AMOUNT": "123"}    (logical value only)
Output: [0xF1, 0xF2, 0xF3]  (EBCDIC zones - DEFAULT)
Result: BINARY MISMATCH! ❌
```

## How copybook-rs Solves This

### 1. Encoding Detection

copybook-rs automatically analyzes zone nibbles during decode to detect the original encoding format:

```rust
// Automatic detection during decode
let opts = DecodeOptions::new()
    .with_preserve_zoned_encoding(true);

let json = decode_record(&schema, &data, &opts)?;
// JSON now contains format metadata
```

### 2. Metadata Preservation

Encoding information is stored in JSON metadata fields:

```json
{
  "CUSTOMER_ID": "12345",
  "ACCOUNT_BALANCE": "-1234.56",
  "_encoding_metadata": {
    "CUSTOMER_ID": {
      "zoned_encoding": "ascii",
      "detection_confidence": 1.0
    },
    "ACCOUNT_BALANCE": {
      "zoned_encoding": "ebcdic",
      "detection_confidence": 0.95
    }
  }
}
```

### 3. Format-Aware Encoding

During encode, copybook-rs uses preserved format information:

```rust
// Encode respecting original formats
let opts = EncodeOptions::new()
    .with_zoned_encoding_override(None); // Use preserved formats

let binary = encode_record(&schema, &json, &opts)?;
// Binary matches original encoding
```

## Format Detection Algorithm

### Zone Analysis

copybook-rs examines the upper 4 bits (zone nibble) of each byte:

```
Byte Value: 0x31
Binary:     0011 0001
            ^^^^ ^^^^
            Zone Digit
            0x3  0x1

Detection: ASCII (zone = 0x3)
```

### Confidence Scoring

The detection algorithm provides confidence scores:

- **1.0**: All zones consistent with single format
- **0.8-0.9**: Mostly consistent with minor anomalies
- **0.5-0.7**: Mixed encoding or ambiguous data
- **0.0-0.4**: Unreliable detection

### Mixed Encoding Handling

When fields contain mixed ASCII/EBCDIC zones:

```rust
// Strict mode: fail on mixed encoding
let opts = DecodeOptions::new()
    .with_preserve_zoned_encoding(true)
    .with_strict_mode(true);

// Lenient mode: warn and continue with best guess
let opts = DecodeOptions::new()
    .with_preserve_zoned_encoding(true)
    .with_max_errors(Some(100));
```

## Implementation Architecture

### Precedence Hierarchy

copybook-rs uses a clear precedence order for format selection:

1. **Explicit Override** (highest precedence)
   ```rust
   .with_zoned_encoding_override(Some(ZonedEncodingFormat::Ascii))
   ```

2. **Preserved Format** (from decode metadata)
   ```json
   "_encoding_metadata": {"FIELD": {"zoned_encoding": "ascii"}}
   ```

3. **EBCDIC Default** (lowest precedence)
   - Maintains backward compatibility
   - Standard mainframe format

### Performance Optimization

- **Detection**: <1% overhead during decode operations
- **Metadata**: Scales linearly with field count
- **Memory**: Minimal storage for encoding information
- **Enterprise Targets**: Maintains 4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3

## Error Handling

### New Error Codes

copybook-rs provides specific error codes for encoding issues:

- **CBKD413**: Invalid zoned decimal encoding format
- **CBKD414**: Mixed ASCII/EBCDIC encoding in single field
- **CBKD415**: Unable to detect zoned decimal encoding format

### Error Strategies

```rust
match decode_result {
    Err(e) if e.code == ErrorCode::CBKD414_ZONED_MIXED_ENCODING => {
        // Handle mixed encoding gracefully
        log::warn!("Mixed encoding detected, using fallback format");
        continue_with_fallback();
    }
    Err(e) => {
        // Handle other errors
        return Err(e);
    }
    Ok(json) => {
        // Process successful decode
        process_record(json);
    }
}
```

## Testing and Validation

### Round-Trip Verification

```bash
# Decode with preservation
copybook decode schema.cpy original.bin --preserve-zoned-encoding --output temp.jsonl

# Encode with preserved formats
copybook encode schema.cpy temp.jsonl --output roundtrip.bin

# Binary comparison
cmp original.bin roundtrip.bin
echo $? # Should be 0 for identical files
```

### Property Testing

copybook-rs includes property-based tests that verify round-trip fidelity:

```rust
proptest! {
    #[test]
    fn round_trip_preserves_encoding(
        digits in prop::collection::vec(0u8..=9, 1..=18),
        encoding in prop::sample::select(vec![
            ZonedEncodingFormat::Ascii,
            ZonedEncodingFormat::Ebcdic
        ])
    ) {
        let original = generate_zoned_data(&digits, encoding);
        let json = decode_with_preservation(&original)?;
        let roundtrip = encode_with_preservation(&json)?;

        prop_assert_eq!(original, roundtrip);
    }
}
```

## Enterprise Best Practices

### 1. Always Enable Preservation for Critical Data

```rust
let opts = DecodeOptions::new()
    .with_preserve_zoned_encoding(true)
    .with_emit_meta(true); // Include metadata for audit trails
```

### 2. Validate Round-Trip Integrity

```rust
// Test round-trip before production deployment
fn validate_round_trip_integrity(test_data: &[u8]) -> Result<(), Error> {
    let json = decode_record(&schema, test_data, &decode_opts)?;
    let roundtrip = encode_record(&schema, &json, &encode_opts)?;

    if test_data != roundtrip.as_slice() {
        return Err(Error::new(
            ErrorCode::CBKD999_ROUNDTRIP_FAILED,
            "Binary round-trip integrity check failed"
        ));
    }

    Ok(())
}
```

### 3. Monitor Detection Confidence

```rust
if let Some(meta) = json.get("_encoding_metadata") {
    for (field, info) in meta.as_object().unwrap() {
        if let Some(confidence) = info.get("detection_confidence") {
            if confidence.as_f64().unwrap() < 0.8 {
                log::warn!("Low confidence encoding detection for field {}: {}",
                          field, confidence);
            }
        }
    }
}
```

### 4. Document Format Requirements

```rust
/// Enterprise data processing pipeline with binary fidelity guarantees
///
/// # Binary Round-Trip Fidelity
///
/// This pipeline guarantees byte-perfect preservation of zoned decimal
/// encoding formats. All processing maintains original ASCII/EBCDIC
/// zone representations for regulatory compliance.
///
/// # Error Handling
///
/// Mixed encoding detection triggers warnings but continues processing
/// with fallback to EBCDIC format for mainframe compatibility.
pub fn process_customer_data(input: &Path, output: &Path) -> Result<RunSummary> {
    // Implementation with fidelity guarantees
}
```

## Future Enhancements

### Planned Features

1. **Extended Metadata**: Additional format information (sign conventions, padding)
2. **Compression**: Optimized metadata storage for large datasets
3. **Validation**: Built-in round-trip integrity checking
4. **Reporting**: Detailed encoding analysis and confidence metrics

### Research Areas

1. **Machine Learning**: Advanced encoding pattern recognition
2. **Heuristics**: Improved mixed-encoding resolution strategies
3. **Performance**: Further optimization of detection algorithms
4. **Standards**: Integration with emerging mainframe modernization standards

## Conclusion

Binary round-trip fidelity in copybook-rs provides enterprise-grade guarantees for data integrity during mainframe modernization and integration projects. By automatically detecting and preserving zoned decimal encoding formats, copybook-rs enables organizations to meet regulatory requirements, maintain legal compliance, and ensure seamless system integration while achieving exceptional performance targets.

The implementation balances automatic detection with explicit control, providing both ease of use for simple scenarios and fine-grained control for complex enterprise requirements.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
