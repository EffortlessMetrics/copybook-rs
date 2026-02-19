<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Dialect Fixtures Documentation

This directory contains comprehensive fixtures for testing the dialect lever behavior across all three modes ("n", "0", "1") with ODO (OCCURS DEPENDING ON) arrays.

## Overview

The dialect lever controls how ODO array counter validation is performed:

- **"n" (normative)**: min_count is enforced - counter must be ≥ min_count
- **"0" (zero-tolerant)**: min_count is ignored - counter can be 0..max_count
- **"1" (one-tolerant)**: min_count is clamped to 1 - counter must be ≥ max(1, min_count)

## Copybook Structure

### File: `dialect_test.cpy`

```
       01  DIALECT-RECORD.
           05  RECORD-ID          PIC 9(3).
           05  ARRAY-COUNT        PIC 9(3).
           05  DATA-ARRAY         OCCURS 5 TO 10 TIMES
                                   DEPENDING ON ARRAY-COUNT.
               10  DATA-VALUE     PIC X(10).
           05  STATUS-CODE        PIC 9(2).
           05  FILLER             PIC X(5).
```

**Key features:**
- ODO array with `min_count = 5` and `max_count = 10`
- Counter field `ARRAY-COUNT` (PIC 9(3))
- Array elements are 10-character strings
- Additional fields for realistic record structure

## Test Data Files

### `dialect_normative.bin`
- **Purpose**: Data with counter = min_count (valid in all modes)
- **Counter value**: 5 (equals min_count)
- **Structure**:
  - RECORD-ID: "001"
  - ARRAY-COUNT: "005"
  - DATA-ARRAY: 5 elements ("VALUE001" through "VALUE005")
  - STATUS-CODE: "00"
  - FILLER: "*****"
- **Valid in**: "n", "0", "1" modes

### `dialect_zero_tolerant.bin`
- **Purpose**: Data with counter = 0 (only valid in "0" mode)
- **Counter value**: 0 (below min_count)
- **Structure**:
  - RECORD-ID: "002"
  - ARRAY-COUNT: "000"
  - DATA-ARRAY: 0 elements (empty)
  - STATUS-CODE: "00"
  - FILLER: "*****"
- **Valid in**: "0" mode only
- **Invalid in**: "n" and "1" modes

### `dialect_one_tolerant.bin`
- **Purpose**: Data with counter = 1 (valid in "0" and "1" modes)
- **Counter value**: 1 (below min_count of 5)
- **Structure**:
  - RECORD-ID: "003"
  - ARRAY-COUNT: "001"
  - DATA-ARRAY: 1 element ("VALUE001")
  - STATUS-CODE: "00"
  - FILLER: "*****"
- **Valid in**: "0" and "1" modes
- **Invalid in**: "n" mode

## Golden Fixtures (Expected Output)

### `json/dialect_normative.jsonl`
Expected JSONL output when decoding `dialect_normative.bin`:
```json
{"RECORD-ID":1,"ARRAY-COUNT":5,"DATA-ARRAY":["VALUE001","VALUE002","VALUE003","VALUE004","VALUE005"],"STATUS-CODE":0,"FILLER":"*****"}
```

### `json/dialect_zero_tolerant.jsonl`
Expected JSONL output when decoding `dialect_zero_tolerant.bin`:
```json
{"RECORD-ID":2,"ARRAY-COUNT":0,"DATA-ARRAY":[],"STATUS-CODE":0,"FILLER":"*****"}
```

### `json/dialect_one_tolerant.jsonl`
Expected JSONL output when decoding `dialect_one_tolerant.bin`:
```json
{"RECORD-ID":3,"ARRAY-COUNT":1,"DATA-ARRAY":["VALUE001"],"STATUS-CODE":0,"FILLER":"*****"}
```

## Expected Outcomes by Dialect Mode

| Test Data | Counter | "n" (normative) | "0" (zero-tolerant) | "1" (one-tolerant) |
|-----------|---------|-----------------|---------------------|---------------------|
| dialect_normative.bin | 5 | ✅ Valid | ✅ Valid | ✅ Valid |
| dialect_zero_tolerant.bin | 0 | ❌ Invalid | ✅ Valid | ❌ Invalid |
| dialect_one_tolerant.bin | 1 | ❌ Invalid | ✅ Valid | ✅ Valid |

## Running the Tests

### Run all dialect fixture tests:
```bash
cargo test --package copybook-cli dialect_fixtures_d3_tests
```

### Run a specific test:
```bash
cargo test --package copybook-cli normative_mode_accepts_counter_at_min_count
```

### Run tests with output:
```bash
cargo test --package copybook-cli dialect_fixtures_d3_tests -- --nocapture
```

## Using the Fixtures Manually

### Decode with normative mode:
```bash
copybook decode --format fixed --codepage ascii --dialect n \
  test-data/dialect_test.cpy test-data/dialect_normative.bin \
  --output output.jsonl
```

### Decode with zero-tolerant mode:
```bash
copybook decode --format fixed --codepage ascii --dialect 0 \
  test-data/dialect_test.cpy test-data/dialect_zero_tolerant.bin \
  --output output.jsonl
```

### Decode with one-tolerant mode:
```bash
copybook decode --format fixed --codepage ascii --dialect 1 \
  test-data/dialect_test.cpy test-data/dialect_one_tolerant.bin \
  --output output.jsonl
```

### Using environment variable:
```bash
export COPYBOOK_DIALECT=0
copybook decode --format fixed --codepage ascii \
  test-data/dialect_test.cpy test-data/dialect_zero_tolerant.bin \
  --output output.jsonl
```

## Test Coverage

The integration tests in `copybook-cli/tests/dialect_fixtures_d3_tests.rs` verify:

1. **Normative mode behavior**:
   - Accepts counter ≥ min_count
   - Rejects counter < min_count
   - Default mode is normative

2. **Zero-tolerant mode behavior**:
   - Accepts counter = 0
   - Accepts counter = 1
   - Ignores min_count constraint

3. **One-tolerant mode behavior**:
   - Accepts counter = 1 even when min_count > 1
   - Rejects counter = 0
   - Clamps min_count to 1

4. **CLI and environment variable integration**:
   - CLI flag produces expected output for each mode
   - Environment variable produces expected output for each mode
   - CLI flag overrides environment variable

## Notes

- All binary data files use ASCII encoding for PIC 9 fields (digits as ASCII characters)
- The FILLER field is included to maintain realistic record structure
- STATUS-CODE is included to demonstrate additional numeric fields
- Golden fixtures use JSONL format (one JSON object per line)

## Parser Limitations

### ODO Array Max Value Restriction

**Issue**: The parser's lexer tokenizes numbers in the range 01-49 as `Level` tokens (used for COBOL level numbers) rather than `Number` tokens. This is because the Level regex (`0[1-9]|[1-4][0-9]`) has higher priority (5) than the Number regex (`[1-9][0-9]*`) (priority 4).

**Impact**: When using OCCURS DEPENDING ON syntax, the max value must be a single digit (1-9) to avoid being tokenized as a Level number. For example:
- ✅ `OCCURS 5 TO 9 TIMES DEPENDING ON COUNTER` - Works (max value is single digit)
- ❌ `OCCURS 5 TO 10 TIMES DEPENDING ON COUNTER` - Fails (max value "10" is tokenized as Level(10))

**Workaround**: Use max values < 10 in ODO array definitions. If you need larger arrays, consider:
1. Using a different array structure (e.g., fixed-size OCCURS without DEPENDING ON)
2. Modifying the lexer to improve tokenization (this is a core implementation change)

**Location**: The issue is in [`copybook-core/src/lexer.rs:16`](../copybook-core/src/lexer.rs:16) where the Level regex has higher priority than the Number regex.

**Note**: This is a known parser limitation that affects copybook syntax. The dialect implementation (D1, D2) is complete and working correctly; this is a separate lexer/parsing issue.
