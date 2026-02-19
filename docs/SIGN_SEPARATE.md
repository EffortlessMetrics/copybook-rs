<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# SIGN SEPARATE Decode Semantics

## Overview

This document specifies the behavior of SIGN SEPARATE clause support in copybook-rs. SIGN SEPARATE is a COBOL clause that specifies that the sign of a numeric field is stored in a separate byte rather than being overpunched in the zone portion of the last digit.

## Feature Status

- **Feature Flag**: `sign_separate`
- **Default**: Disabled (false)
- **Category**: Experimental
- **Error Code**: CBKP051 (when flag is disabled)

## COBOL Syntax

```cobol
PIC S9(n) SIGN IS LEADING SEPARATE
PIC S9(n) SIGN IS TRAILING SEPARATE
```

## Behavior Specification

### Storage Layout

SIGN SEPARATE fields store the sign in a dedicated byte separate from the numeric digits:

- **LEADING SEPARATE**: Sign byte precedes the numeric digits
- **TRAILING SEPARATE**: Sign byte follows the numeric digits

Total storage = 1 byte (sign) + n bytes (digits)

### Sign Representation

#### ASCII Encoding

| Value | Sign Byte |
|-------|-----------|
| Positive (+) | 0x2B (`+`) |
| Negative (-) | 0x2D (`-`) |
| Unsigned (no sign) | 0x20 (space) or 0x30 (`0`) |

#### EBCDIC Encoding

| Value | Sign Byte |
|-------|-----------|
| Positive (+) | 0x4E (`+`) |
| Negative (-) | 0x60 (`-`) |
| Unsigned (no sign) | 0x40 (space) or 0xF0 (`0`) |

### Digit Representation

Digits are stored as standard display characters:

#### ASCII Encoding

| Digit | Byte Value |
|-------|------------|
| 0-9   | 0x30-0x39 |

#### EBCDIC Encoding

| Digit | Byte Value |
|-------|------------|
| 0-9   | 0xF0-0xF9 |

## Dialect-Specific Notes

### IBM Enterprise COBOL

- Default behavior: SIGN SEPARATE uses LEADING placement
- Supports both LEADING and TRAILING SEPARATE
- Sign byte uses EBCDIC encoding
- Unsigned fields may have space (0x40) or zero (0xF0) in sign position

### Micro Focus COBOL

- Default behavior: SIGN SEPARATE uses LEADING placement
- Supports both LEADING and TRAILING SEPARATE
- Sign byte uses ASCII encoding
- Unsigned fields typically use space (0x20) in sign position

### Fujitsu COBOL

- Default behavior: SIGN SEPARATE uses LEADING placement
- Supports both LEADING and TRAILING SEPARATE
- Sign byte encoding matches system codepage

## Error Handling

### When Feature Flag is Disabled

If the `sign_separate` feature flag is disabled:

- **Error Code**: CBKP051_UNSUPPORTED_EDITED_PIC
- **Error Message**: "SIGN clause on field '{field_name}' is not supported yet"
- **Action**: Reject parsing of copybook

### When Feature Flag is Enabled

The following validation rules apply:

1. **Field Type**: SIGN SEPARATE can only be used with numeric display fields (PIC 9 or PIC S9)
2. **Placement**: Must specify LEADING or TRAILING (defaults to LEADING if omitted)
3. **Scale**: V (implied decimal) is supported
4. **Blank When Zero**: Cannot be combined with SIGN SEPARATE

### Invalid Input Handling

| Condition | Error Code | Behavior |
|-----------|------------|----------|
| Invalid sign byte (not +, -, space, or 0) | CBKD411_ZONED_BAD_SIGN | Reject decode |
| Non-digit character in digit bytes | CBKD301_RECORD_TOO_SHORT | Reject decode |
| Record too short for field length | CBKD301_RECORD_TOO_SHORT | Reject decode |

## Examples

### Example 1: Leading Separate Sign (ASCII)

```cobol
05 AMOUNT PIC S9(5) SIGN IS LEADING SEPARATE.
```

Binary data (7 bytes):
```
+ 1 2 3 4 5
0x2B 0x31 0x32 0x33 0x34 0x35
```

Decoded value: `12345`

Binary data (7 bytes, negative):
```
- 1 2 3 4 5
0x2D 0x31 0x32 0x33 0x34 0x35
```

Decoded value: `-12345`

### Example 2: Trailing Separate Sign (EBCDIC)

```cobol
05 BALANCE PIC S9(7)V99 SIGN IS TRAILING SEPARATE.
```

Binary data (10 bytes):
```
1 2 3 4 5 6 7 8 9 +
0xF1 0xF2 0xF3 0xF4 0xF5 0xF6 0xF7 0xF8 0xF9 0x4E
```

Decoded value: `1234567.89`

### Example 3: Unsigned with Space Sign (ASCII)

```cobol
05 QUANTITY PIC 9(4) SIGN IS LEADING SEPARATE.
```

Binary data (5 bytes):
```
  1 2 3 4
0x20 0x31 0x32 0x33 0x34
```

Decoded value: `1234`

## Implementation Notes

### Parser Changes

1. When `sign_separate` flag is enabled, accept SIGN SEPARATE clause
2. Parse placement (LEADING/TRAILING) - default to LEADING
3. Store sign information in field metadata

### Layout Changes

1. Account for separate sign byte in field length calculation
2. Update offset calculations for subsequent fields
3. For LEADING: sign byte at offset, digits at offset+1
4. For TRAILING: digits at offset, sign byte at offset+digits

### Codec Changes

1. Read sign byte based on placement (LEADING/TRAILING)
2. Decode sign from ASCII/EBCDIC byte
3. Read digit bytes and convert to numeric value
4. Apply sign to numeric value
5. Handle scale (decimal places) if present

### Schema Representation

The field schema will include:

```rust
FieldKind::ZonedDecimal {
    digits: u16,
    scale: i16,
    signed: bool,
    sign_separate: Option<SignSeparateInfo>,
}

struct SignSeparateInfo {
    placement: SignPlacement, // Leading or Trailing
}
```

## Migration Path

### Phase 3.1 (Current)
- Implement decode semantics behind feature flag
- Default: disabled
- Error: CBKP051 when flag disabled

### Phase 3.2 (Future)
- Add encode semantics
- Comprehensive testing
- Performance validation

### Phase 3.3 (Future)
- Consider promoting to default-on
- Remove feature flag after stabilization

## References

- IBM Enterprise COBOL Language Reference: SIGN Clause
- Micro Focus COBOL Language Reference: SIGN Clause
- COBOL 2014 Standard: Section 8.5.4 SIGN clause
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
