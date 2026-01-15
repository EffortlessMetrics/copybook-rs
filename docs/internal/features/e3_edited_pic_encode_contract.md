# E3.0 Edited PIC Encode Contract

**Version:** v0.5.0  
**Feature:** Edited PIC Encode (E3)  
**Status:** Implementation in progress  
**Related Code:** `copybook-codec/src/edited_pic.rs:294-555` (E2 decode reference)

---

## Overview

This document defines the contract for implementing Edited PIC Encode (E3) in copybook-rs v0.5.0. Edited PIC encoding converts numeric values into formatted strings according to COBOL PICTURE clause editing symbols.

### PicToken Enum Reference

```rust
pub enum PicToken {
    Digit,           // Numeric digit (9)
    ZeroSuppress,    // Z - displays space if leading zero
    ZeroInsert,      // 0 - always displays '0'
    AsteriskFill,    // * - displays '*' for leading zeros
    Space,           // B
    Comma,           // ,
    Slash,           // /
    DecimalPoint,    // .
    Currency,        // $
    LeadingPlus,     // +
    LeadingMinus,    // -
    TrailingPlus,    // +
    TrailingMinus,   // -
    Credit,          // CR (two characters)
    Debit,           // DB (two characters)
}
```

### E2 Decode Behavior Reference

The encode implementation must maintain symmetry with the existing decode behavior:

1. **Zero suppression (Z)**: Leading zeros become spaces, but preserved as '0' in extracted digits
2. **Negative zero handling**: If all digits are '0', sign is forced to Positive
3. **Sign zones**:
   - `LeadingPlus`: '+' or space → Positive
   - `LeadingMinus`: '-' → Negative, space → Positive
   - `TrailingPlus`: '+' or space → Positive
   - `TrailingMinus`: '-' → Negative, space → Positive
   - `Credit`: "CR" → Negative, "  " → Positive
   - `Debit`: "DB" → Negative, "  " → Positive
4. **BLANK WHEN ZERO**: All spaces → decode as 0 with warning CBKD423
5. **Rounding/Scale**: Scale parameter from schema determines decimal placement

---

## Contract: Supported Patterns for v0.5.0

### E3.1 (Minimal Encode Path)

**Goal:** Establish the foundational encode path with basic numeric formatting.

**Supported Tokens:**
| Token | Symbol | Description |
|-------|--------|-------------|
| `Digit` | `9` | Numeric digit placeholder |
| `DecimalPoint` | `.` | Decimal point separator |
| `ZeroSuppress` | `Z` | Replace leading zeros with spaces |
| `ZeroInsert` | `0` | Always display '0' for each position |
| `LeadingPlus` | `+` | Leading plus sign (positive values) |
| `LeadingMinus` | `-` | Leading minus sign (negative values) |

**Patterns:**
- `9(5)` - Basic digits
- `9(3).9(2)` - Decimal point
- `ZZZ9` - Zero suppression
- `0009` - Zero insert
- `+999` - Leading plus
- `-999` - Leading minus

---

### E3.2 (Sign Leading/Trailing)

**Goal:** Extend sign handling to trailing positions and combined suppression.

**Supported Tokens:**
| Token | Symbol | Description |
|-------|--------|-------------|
| `TrailingPlus` | `+` | Trailing plus sign |
| `TrailingMinus` | `-` | Trailing minus sign |

**Patterns:**
- `999+` - Trailing plus
- `999-` - Trailing minus
- `+ZZZ9` - Leading plus with zero suppression
- `-ZZZ9` - Leading minus with zero suppression
- `ZZZ9+` - Trailing plus with zero suppression
- `ZZZ9-` - Trailing minus with zero suppression
- `+0009` - Leading plus with zero insert
- `-0009` - Leading minus with zero insert

---

### E3.3 (CR/DB)

**Goal:** Implement credit/debit sign indicators.

**Supported Tokens:**
| Token | Symbol | Description |
|-------|--------|-------------|
| `Credit` | `CR` | Credit indicator (2 chars) |
| `Debit` | `DB` | Debit indicator (2 chars) |

**Patterns:**
- `999CR` - Credit indicator
- `999DB` - Debit indicator
- `ZZZ9CR` - Credit with zero suppression
- `ZZZ9DB` - Debit with zero suppression
- `0009CR` - Credit with zero insert
- `0009DB` - Debit with zero insert

**Behavior:**
- Positive values: spaces in CR/DB position
- Negative values: "CR" or "DB" displayed

---

### E3.4 (Commas/Separators)

**Goal:** Add numeric separators for readability.

**Supported Tokens:**
| Token | Symbol | Description |
|-------|--------|-------------|
| `Comma` | `,` | Thousands separator |
| `Slash` | `/` | Date separator |

**Patterns:**
- `ZZZ,ZZ9` - Comma separator with suppression
- `ZZZ/ZZ9` - Slash separator with suppression
- `000,009` - Comma with zero insert
- `000/009` - Slash with zero insert
- `+ZZZ,ZZ9` - Leading plus with comma
- `-ZZZ,ZZ9` - Leading minus with comma

**Behavior:**
- Comma/slash positions are preserved regardless of digit values
- Leading zeros before comma/slash are subject to suppression rules

---

### E3.5 (Asterisk Fill)

**Goal:** Implement check protection with asterisk fill.

**Supported Tokens:**
| Token | Symbol | Description |
|-------|--------|-------------|
| `AsteriskFill` | `*` | Replace leading zeros with asterisks |

**Patterns:**
- `***9` - Asterisk fill
- `****9.99` - Asterisk fill with decimal
- `***,**9` - Asterisk fill with comma
- `+***9` - Leading plus with asterisk fill
- `-***9` - Leading minus with asterisk fill

**Behavior:**
- Leading zeros are replaced with asterisks
- Non-zero digits are displayed normally
- Used for check protection in financial applications

---

### E3.6 (Currency Symbols)

**Goal:** Add currency symbol support.

**Supported Tokens:**
| Token | Symbol | Description |
|-------|--------|-------------|
| `Currency` | `$` | Dollar sign |

**Patterns:**
- `$999` - Leading currency symbol
- `$ZZZ9` - Currency with zero suppression
- `$***9` - Currency with asterisk fill
- `$ZZZ,ZZ9.99` - Currency with comma and decimal
- `$$$9` - Floating currency (if supported)

**Behavior:**
- Currency symbol is always displayed
- For floating currency ($$$9), symbol floats to position of first non-zero digit

---

## Test Matrix

### E3.1 Test Cases

| PIC Pattern | Input Value | Expected Output | Notes |
|-------------|-------------|-----------------|-------|
| `9999` | 1234 | "1234" | Basic digits |
| `9999` | 0 | "0000" | Zero with zero insert |
| `ZZZ9` | 123 | " 123" | Z suppression |
| `ZZZ9` | 0 | "   0" | Zero suppression with last digit |
| `ZZZ9` | 1 | "   1" | Single digit |
| `0009` | 123 | "0123" | Zero insert |
| `0009` | 0 | "0000" | All zeros |
| `9.99` | 12.34 | "12.34" | Decimal point |
| `9.99` | 0.00 | "0.00" | Zero decimal |
| `+999` | 123 | "+123" | Leading plus positive |
| `+999` | -123 | "-123" | Leading plus negative |
| `-999` | 123 | " 123" | Leading minus positive (space) |
| `-999` | -123 | "-123" | Leading minus negative |
| `+9.99` | 12.34 | "+12.34" | Leading plus with decimal |
| `-9.99` | -12.34 | "-12.34" | Leading minus with decimal |

### E3.2 Test Cases

| PIC Pattern | Input Value | Expected Output | Notes |
|-------------|-------------|-----------------|-------|
| `999+` | 123 | "123+" | Trailing plus positive |
| `999+` | -123 | "123-" | Trailing plus negative |
| `999-` | 123 | "123 " | Trailing minus positive (space) |
| `999-` | -123 | "123-" | Trailing minus negative |
| `+ZZZ9` | 123 | "+123" | Leading plus with Z |
| `+ZZZ9` | 0 | "+   0" | Leading plus zero |
| `-ZZZ9` | -123 | "-123" | Leading minus with Z |
| `-ZZZ9` | 0 | "   0" | Leading minus zero (positive) |
| `ZZZ9+` | 123 | " 123+" | Trailing plus with Z |
| `ZZZ9-` | -123 | " 123-" | Trailing minus with Z |
| `+0009` | 123 | "+0123" | Leading plus with 0 |
| `-0009` | -123 | "-0123" | Leading minus with 0 |

### E3.3 Test Cases

| PIC Pattern | Input Value | Expected Output | Notes |
|-------------|-------------|-----------------|-------|
| `999CR` | 123 | "123  " | Credit positive (spaces) |
| `999CR` | -123 | "123CR" | Credit negative |
| `999DB` | 123 | "123  " | Debit positive (spaces) |
| `999DB` | -123 | "123DB" | Debit negative |
| `ZZZ9CR` | 123 | " 123 " | Credit with Z positive |
| `ZZZ9CR` | -123 | " 123CR" | Credit with Z negative |
| `ZZZ9DB` | -123 | " 123DB" | Debit with Z negative |
| `0009CR` | 123 | "0123 " | Credit with 0 positive |
| `0009CR` | -123 | "0123CR" | Credit with 0 negative |
| `0009DB` | -123 | "0123DB" | Debit with 0 negative |
| `ZZZ9CR` | 0 | "   0 " | Zero with credit (positive) |

### E3.4 Test Cases

| PIC Pattern | Input Value | Expected Output | Notes |
|-------------|-------------|-----------------|-------|
| `ZZZ,ZZ9` | 12345 | "12,345" | Comma separator |
| `ZZZ,ZZ9` | 1234 | "01,234" | Comma with leading zero |
| `ZZZ,ZZ9` | 123 | "00,123" | Comma with two leading zeros |
| `ZZZ,ZZ9` | 0 | "   ,  0" | Comma with suppression |
| `ZZZ/ZZ9` | 12345 | "12/345" | Slash separator |
| `ZZZ/ZZ9` | 0 | "   /  0" | Slash with suppression |
| `000,009` | 12345 | "012,345" | Comma with zero insert |
| `000/009` | 12345 | "012/345" | Slash with zero insert |
| `+ZZZ,ZZ9` | 12345 | "+12,345" | Leading plus with comma |
| `-ZZZ,ZZ9` | -12345 | "-12,345" | Leading minus with comma |
| `ZZZ,ZZ9.99` | 12345.67 | "12,345.67" | Comma with decimal |
| `ZZZ,ZZ9.99` | 0.00 | "   ,  0.00" | Zero with comma and decimal |

### E3.5 Test Cases

| PIC Pattern | Input Value | Expected Output | Notes |
|-------------|-------------|-----------------|-------|
| `***9` | 123 | "***123" | Asterisk fill |
| `***9` | 1 | "***1" | Single digit |
| `***9` | 0 | "***0" | Zero |
| `****9` | 123 | "****123" | Four asterisks |
| `***,**9` | 12345 | "**,***123" | Asterisk with comma |
| `***,**9` | 1234 | "**,**1234" | Asterisk with comma partial |
| `***,**9` | 0 | "***,**0" | Zero with comma |
| `+***9` | 123 | "+***123" | Leading plus with asterisk |
| `-***9` | -123 | "-***123" | Leading minus with asterisk |
| `***9.99` | 123.45 | "***123.45" | Asterisk with decimal |
| `***9.99` | 0.00 | "***0.00" | Zero with decimal |

### E3.6 Test Cases

| PIC Pattern | Input Value | Expected Output | Notes |
|-------------|-------------|-----------------|-------|
| `$999` | 123 | "$123" | Leading currency |
| `$999` | 0 | "$000" | Zero with currency |
| `$ZZZ9` | 123 | "$ 123" | Currency with Z |
| `$ZZZ9` | 0 | "$   0" | Zero with currency and Z |
| `$***9` | 123 | "$***123" | Currency with asterisk |
| `$***9` | 0 | "$***0" | Zero with currency and asterisk |
| `$ZZZ,ZZ9.99` | 12345.67 | "$12,345.67" | Currency with comma and decimal |
| `$ZZZ,ZZ9.99` | 0.00 | "$   ,  0.00" | Zero with full formatting |
| `$$$9` | 123 | "$123" | Floating currency (if supported) |
| `$$$9` | 1 | "$  1" | Floating currency single digit |
| `$$$9` | 0 | "$   0" | Floating currency zero |

---

## Unsupported Patterns for v0.5.0

The following patterns are explicitly NOT supported in v0.5.0 and should return appropriate errors.

| Pattern | Reason | Error Code | Error Message Shape |
|---------|--------|------------|---------------------|
| `PIC S9(5) EDITED` | SIGN clause not supported for edited PIC | CBKP051 | "SIGN clause not supported with EDITED PIC clause" |
| `PIC P(5)` | P scaling not supported | CBKP051 | "P scaling not supported in EDITED PIC" |
| `PIC 9(5)V99` with editing | V in edited patterns not supported | CBKP051 | "V (implicit decimal) not supported in EDITED PIC - use explicit decimal point" |
| `PIC 9(5) BLANK WHEN ZERO` | BLANK WHEN ZERO clause not supported | CBKP051 | "BLANK WHEN ZERO clause not supported in v0.5.0" |
| `PIC 9(5) JUSTIFIED RIGHT` | JUSTIFIED clause not supported | CBKP051 | "JUSTIFIED clause not supported in EDITED PIC" |
| `PIC 9(5) USAGE DISPLAY` | USAGE clause not supported | CBKP051 | "USAGE clause not supported in EDITED PIC" |
| `PIC 9(5) OCCURS 3` | OCCURS clause not supported | CBKP051 | "OCCURS clause not supported in EDITED PIC" |
| `PIC 9(5) REDEFINES X` | REDEFINES clause not supported | CBKP051 | "REDEFINES clause not supported in EDITED PIC" |
| `PIC 9(5) VALUE 123` | VALUE clause not supported | CBKP051 | "VALUE clause not supported in EDITED PIC" |
| `PIC 9(5) EXTERNAL` | EXTERNAL clause not supported | CBKP051 | "EXTERNAL clause not supported in EDITED PIC" |
| `PIC 9(5) GLOBAL` | GLOBAL clause not supported | CBKP051 | "GLOBAL clause not supported in EDITED PIC" |

---

## Error Codes

### Existing Error Codes

| Code | Description | Severity | Usage |
|------|-------------|----------|-------|
| `CBKD302_EDITED_PIC_NOT_IMPLEMENTED` | Encode not implemented (E3) | Error | Temporary error during implementation |
| `CBKD421_EDITED_PIC_INVALID_FORMAT` | Input doesn't match pattern | Error | When input value cannot be encoded to the pattern |
| `CBKD422_EDITED_PIC_SIGN_MISMATCH` | Sign editing symbol mismatch | Error | When sign handling fails |
| `CBKD423_EDITED_PIC_BLANK_WHEN_ZERO` | Field is blank (warning) | Warning | When decoding blank fields (E2 only) |

### New Error Codes for E3

| Code | Description | Severity | Usage |
|------|-------------|----------|-------|
| `CBKP051_UNSUPPORTED_EDITED_PIC` | Unsupported edited PIC token | Error | Reserved for future unsupported patterns (all tokens now supported) |

**Note (v0.4.2+):** Space (B) insertion is now fully supported in E3.7. This error code is reserved for potential future unsupported tokens.

---

## Implementation Phases

### E3.1 Scope (Minimal Encode Path)

**Deliverables:**
- Basic encode function skeleton
- Support for: `Digit`, `DecimalPoint`, `ZeroSuppress`, `ZeroInsert`, `LeadingPlus`, `LeadingMinus`
- Unit tests for all E3.1 test matrix cases
- Integration tests for roundtrip with E2 decode

**Acceptance Criteria:**
- All E3.1 test cases pass
- Roundtrip encode→decode produces original value
- Error handling for invalid inputs

---

### E3.2 Scope (Sign Leading/Trailing)

**Deliverables:**
- Support for: `TrailingPlus`, `TrailingMinus`
- Combined sign with suppression/insert patterns
- Unit tests for all E3.2 test matrix cases

**Acceptance Criteria:**
- All E3.2 test cases pass
- Trailing signs work with all suppression/insert combinations
- Roundtrip encode→decode produces original value

---

### E3.3 Scope (CR/DB)

**Deliverables:**
- Support for: `Credit`, `Debit`
- Two-character sign indicators
- Unit tests for all E3.3 test matrix cases

**Acceptance Criteria:**
- All E3.3 test cases pass
- CR/DB indicators display correctly for positive/negative values
- Roundtrip encode→decode produces original value

---

### E3.4 Scope (Commas/Separators)

**Deliverables:**
- Support for: `Comma`, `Slash`
- Separator preservation
- Unit tests for all E3.4 test matrix cases

**Acceptance Criteria:**
- All E3.4 test cases pass
- Commas/slashes preserved in correct positions
- Roundtrip encode→decode produces original value

---

### E3.5 Scope (Asterisk Fill)

**Deliverables:**
- Support for: `AsteriskFill`
- Check protection behavior
- Unit tests for all E3.5 test matrix cases

**Acceptance Criteria:**
- All E3.5 test cases pass
- Asterisks replace leading zeros correctly
- Roundtrip encode→decode produces original value

---

### E3.6 Scope (Currency Symbols)

**Deliverables:**
- Support for: `Currency`
- Currency symbol handling
- Floating currency support (if feasible)
- Unit tests for all E3.6 test matrix cases

**Acceptance Criteria:**
- All E3.6 test cases pass
- Currency symbols display correctly
- Roundtrip encode→decode produces original value

---

## Edge Cases and Special Behaviors

### Negative Zero Handling
- If all digits are '0', sign is forced to Positive
- This matches E2 decode behavior (lines 546-548 in edited_pic.rs)

### Zero Suppression Edge Cases
- Last digit in Z pattern is never suppressed (becomes '0')
- All-zero values: Z pattern produces spaces except last position

### Scale and Rounding
- Scale parameter from schema determines decimal placement
- Rounding behavior must match E2 decode expectations

### BLANK WHEN ZERO (Future)
- Not supported in v0.5.0
- Future implementation should output all spaces for zero values

---

## References

- **E2 Decode Implementation:** `copybook-codec/src/edited_pic.rs:294-555`
- **PicToken Enum:** `copybook-codec/src/edited_pic.rs:14-82`
- **Error Codes:** Defined in `copybook-codec/src/error.rs`
- **Schema Format:** See `schemas/copybook-schema.json`

---

**Document Version:** 1.0  
**Last Updated:** 2025-12-23  
**Maintainer:** copybook-rs team
