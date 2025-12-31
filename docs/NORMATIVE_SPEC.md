# Normative Specification Addendum

This document defines the normative behaviors and decisions for copybook-rs to ensure deterministic, reproducible output across implementations and deployments.

## Overview

Normative decisions eliminate ambiguity in COBOL data processing and ensure consistent behavior. These specifications are binding for all copybook-rs implementations and must be followed exactly to maintain compatibility and determinism.

## 1. Numeric Canonicalization (NORMATIVE)

### 1.1 Fixed-Scale Rendering

**Rule**: Zoned and packed decimal fields MUST render with exactly `scale` digits after the decimal point.

**Implementation**:
- Scale = 0: Render as integer without decimal point (e.g., "123")
- Scale > 0: Render with exactly `scale` decimal places (e.g., "123.45" for scale=2)
- Scale < 0: Apply scaling factor (e.g., scale=-2 means multiply by 100)

**Examples**:
```
PIC 9(5)V99 COMP-3, value 12345 → "123.45"
PIC 9(3), value 123 → "123"
PIC 9(3)P99, value 123 → "12300" (scale=-2)
```

**Rationale**: Ensures consistent decimal representation regardless of internal storage format.

### 1.2 Zero Normalization

**Rule**: All numeric representations MUST normalize -0 to 0.

**Implementation**:
- Detect negative zero in all numeric types
- Convert to positive zero in output
- Apply to both JSON numbers and decimal strings

**Examples**:
```
Zoned decimal -0 → "0"
Packed decimal -0 → "0"
Binary integer -0 → 0
```

**Rationale**: Eliminates ambiguity in zero representation and ensures consistent JSON output.

### 1.3 Encode Validation

**Rule**: JSON input strings for decimal fields MUST match the field's scale exactly.

**Implementation**:
- Count decimal places in input string
- Reject if count ≠ field scale
- Error code: CBKE501_JSON_TYPE_MISMATCH

**Examples**:
```
PIC 9(3)V99, input "123.4" → ERROR (scale mismatch)
PIC 9(3)V99, input "123.45" → OK
PIC 9(3), input "123.0" → ERROR (scale mismatch)
PIC 9(3), input "123" → OK
```

**Rationale**: Prevents silent data corruption from scale mismatches.

## 2. REDEFINES Encode Precedence (NORMATIVE)

### 2.1 Precedence Rules

**Rule**: When encoding JSON with REDEFINES clusters, apply the following precedence:

1. **Raw bytes** (if `--use-raw` and record-level `__raw_b64` present and values match canonical decode)
2. **Single view** (if exactly one view under the cluster is non-null)
3. **Error** (if multiple views are non-null → CBKE501_JSON_TYPE_MISMATCH)

### 2.2 Raw Byte Matching

**Implementation**:
- Decode raw bytes using current schema
- Compare decoded values with JSON values
- Use raw bytes only if values match exactly
- Otherwise fall back to view-based encoding

### 2.3 View Selection

**Implementation**:
- Examine all fields in REDEFINES cluster
- Count non-null JSON values
- If count = 1: encode from that view
- If count = 0: encode as zeros/spaces
- If count > 1: error with cluster path in context

**Example**:
```json
{
  "FIELD_A": "123",
  "FIELD_B": null,     // REDEFINES FIELD_A
  "FIELD_C": "456"     // REDEFINES FIELD_A
}
// Result: ERROR - multiple non-null views
```

**Rationale**: Prevents ambiguous writes that could corrupt data.

## 3. ODO Strict vs Lenient Behavior (NORMATIVE)

### 3.1 Lenient Mode (Default)

**Rule**: In lenient mode, out-of-bounds ODO counters MUST be clamped with warnings.

**Implementation**:
- Counter < min: clamp to min, emit CBKS302_ODO_RAISED
- Counter > max: clamp to max, emit CBKS301_ODO_CLIPPED
- Continue processing with clamped value
- Include record number and field path in warning

### 3.2 Strict Mode

**Rule**: In strict mode (`--strict`), out-of-bounds ODO counters MUST cause fatal errors.

**Implementation**:
- Counter < min or > max: emit error and abort immediately
- Error code: CBKS301_ODO_CLIPPED or CBKS302_ODO_RAISED
- Include full context (record number, field path, actual vs expected)

### 3.3 Counter Validation

**Rule**: ODO counter fields MUST be validated before array processing.

**Implementation**:
- Read counter field value first
- Validate against min/max bounds
- Apply strict/lenient behavior
- Use validated count for array processing

**Rationale**: Provides predictable behavior for malformed data while allowing recovery in lenient mode.

## 4. RDW Raw Preservation (NORMATIVE)

### 4.1 Raw Capture Modes

**Rule**: `--emit-raw=record+rdw` MUST capture both payload and RDW header.

**Implementation**:
- Include 4-byte RDW header in raw capture
- Preserve reserved bytes exactly as found
- Base64-encode entire RDW + payload

### 4.2 Raw Usage

**Rule**: `--use-raw` MUST preserve RDW reserved bytes when payload values match.

**Implementation**:
- Decode raw bytes to verify payload matches JSON
- If match: use original RDW header verbatim
- If mismatch: recompute length field, preserve reserved bytes
- Always use big-endian length encoding

### 4.3 Length Recomputation

**Rule**: RDW length field MUST be recomputed if payload changes during round-trip.

**Implementation**:
- Calculate new payload length
- Update bytes 0-1 with big-endian length
- Preserve bytes 2-3 (reserved) from original
- Emit warning if reserved bytes were non-zero

**Rationale**: Enables byte-identical round-trips while handling data modifications.

## 5. Duplicate Name Disambiguation (NORMATIVE)

### 5.1 Sibling Disambiguation

**Rule**: Sibling fields with identical names MUST be suffixed in declaration order.

**Implementation**:
- First occurrence: no suffix
- Second occurrence: `__dup2` suffix
- Third occurrence: `__dup3` suffix
- Continue incrementally for additional duplicates

**Example**:
```cobol
01 RECORD.
   05 NAME PIC X(10).
   05 NAME PIC X(20).
   05 NAME PIC X(30).
```
```json
{
  "NAME": "...",
  "NAME__dup2": "...",
  "NAME__dup3": "..."
}
```

### 5.2 FILLER Field Naming

**Rule**: FILLER fields (when emitted) MUST be named `_filler_<offset>`.

**Implementation**:
- Use zero-padded decimal byte offset
- Format: `_filler_000123` for offset 123
- Pad to 6 digits minimum
- Include in output only when `--emit-filler` is specified

**Rationale**: Provides deterministic, unique names for debugging and analysis.

## 6. ASCII Zoned Sign Mapping (NORMATIVE)

### 6.1 Dual Sign Tables

**Rule**: Maintain separate EBCDIC and ASCII overpunch sign tables.

**EBCDIC Sign Table**:
```
Positive: C0-C9 (0-9), F0-F9 (0-9)
Negative: D0-D9 (0-9)
```

**ASCII Sign Table**:
```
Positive: 30-39 (0-9)
Negative: 70-79 (p-y)
```

### 6.2 Runtime Selection

**Rule**: Select sign table based on `--codepage` setting at runtime.

**Implementation**:
- `--codepage ascii`: use ASCII sign table
- All other codepages: use EBCDIC sign table
- Apply during both encoding and decoding
- Invalid zone → CBKD411_ZONED_BAD_SIGN

**Rationale**: Supports both mainframe and ASCII-based COBOL systems.

## 7. Grammar Rules (NORMATIVE)

### 7.1 Fixed-Form COBOL

**Rule**: Fixed-form COBOL MUST follow strict column rules.

**Implementation**:
- Columns 1-6: sequence area (ignored)
- Column 7: indicator area
  - `*`: comment line
  - `-`: continuation line (only valid continuation)
  - `/`: page break (treated as comment)
  - space: normal line
- Columns 8-72: program text area
- Columns 73-80: identification area (ignored)

### 7.2 Free-Form COBOL

**Rule**: Free-form COBOL MUST recognize only specific comment formats.

**Implementation**:
- `*>`: inline comment (rest of line ignored)
- `*` at column 1: full-line comment
- No other comment formats recognized
- No block comments supported

### 7.3 Continuation Processing

**Rule**: Line continuation MUST follow exact whitespace rules.

**Implementation**:
- Only column-7 `-` indicates continuation
- Strip trailing spaces from continued line
- Strip leading spaces from continuation line
- Preserve interior whitespace exactly
- Join with single space if needed

**Example**:
```cobol
      05 LONG-FIELD-NAME    
-        PIC X(100).
```
Becomes: `05 LONG-FIELD-NAME PIC X(100).`

### 7.4 Edited PIC Detection

**Rule**: Edited PIC clauses MUST be parsed into EditedNumeric FieldKind with full E1/E2/E3 support. Only Space (`B`) insertion remains unsupported.

**Implementation**:
- Parse edited characters: Z, /, comma, $, +, -, CR, DB, asterisk (*) into EditedToken enum
- Support decode (E2) and encode (E3) for all tokens except Space (B)
- Emit CBKP051_UNSUPPORTED_EDITED_PIC only for Space (B) insertion patterns

### 7.5 SIGN Clause Handling

**Rule**: SIGN LEADING/TRAILING [SEPARATE] MUST be rejected as unsupported clause.

**Implementation**:
- Detect SIGN clauses during parsing
- Emit CBKP011_UNSUPPORTED_CLAUSE
- Suggest using standard signed numeric format

**Rationale**: SIGN SEPARATE clauses require specialized encoding logic not yet implemented.

## 8. Additional Normative Decisions

### 8.1 Alphanumeric Handling

**Rule**: Alphanumeric fields MUST preserve all spaces during decode and pad with spaces during encode.

**Implementation**:
- Decode: preserve leading, trailing, and interior spaces
- Encode: pad with spaces to field length
- Over-length input → CBKE501_JSON_TYPE_MISMATCH

### 8.2 OCCURS Nesting

**Rule**: Nested OCCURS MUST follow specific restrictions.

**Implementation**:
- Nested fixed OCCURS: allowed
- ODO arrays: only at tail of containing group
- ODO under ODO: not allowed
- ODO under fixed OCCURS: allowed if at tail

### 8.3 Binary Width Mapping

**Rule**: Binary field width MUST be determined by PIC digits.

**Implementation**:
- ≤4 digits → 2 bytes (16-bit)
- 5-9 digits → 4 bytes (32-bit)
- 10-18 digits → 8 bytes (64-bit)
- Explicit USAGE BINARY(n): accept n ∈ {1,2,4,8}

### 8.4 Codepage Semantics

**Rule**: Codepage handling MUST follow specific semantics.

**Implementation**:
- Default: cp037 (US/Canada EBCDIC)
- `ascii`: transparent 8-bit with ASCII overpunch (not Windows-1252)
- EBCDIC codepages: use static lookup tables
- Binary fields: never apply character conversion

### 8.5 Exit Codes

**Rule**: Exit codes MUST follow consistent semantics.

**Implementation**:
- 0: success (warnings allowed)
- 1: completed with errors (even if skipped in lenient mode)
- 2: fatal error (parse failure, strict mode violation)

### 8.6 Parallel Determinism

**Rule**: Parallel processing MUST produce identical output regardless of thread count.

**Implementation**:
- Writer emits by sequence ID
- Bounded reordering window for efficiency
- Output identical for `--threads 1` vs `--threads 8`
- Maintain record order in all cases

**Rationale**: Ensures reproducible results for audit and compliance.

## 9. Schema Fingerprinting (NORMATIVE)

### 9.1 Fingerprint Calculation

**Rule**: Schema fingerprint MUST be SHA-256 of canonical schema JSON.

**Implementation**:
- Serialize schema to canonical JSON (sorted keys, no whitespace)
- Include codepage and processing options in hash input
- Use lowercase hexadecimal representation
- Include in `__schema_id` metadata field

### 9.2 Canonical Schema Format

**Rule**: Canonical schema MUST use deterministic field ordering and formatting.

**Implementation**:
- Fields in pre-order traversal order
- JSON keys sorted alphabetically
- No whitespace in serialization
- Consistent numeric formatting

**Rationale**: Enables provenance tracking and schema change detection.

## 10. Error Context Requirements (NORMATIVE)

### 10.1 Structured Context

**Rule**: All errors MUST include relevant context information.

**Implementation**:
- Record number (when applicable)
- Field path (when applicable)
- Byte offset (when applicable)
- Line number (for parse errors)
- Additional context as key-value pairs

### 10.2 Error Code Stability

**Rule**: Error codes MUST remain stable within major versions.

**Implementation**:
- No renumbering of existing codes
- New codes use next available number in category
- Deprecated codes marked but not removed
- Context format may evolve in minor versions

**Rationale**: Enables reliable error handling and monitoring.

## 11. Compliance and Validation

### 11.1 Implementation Requirements

All copybook-rs implementations MUST:
- Follow normative decisions exactly
- Pass comprehensive test suite
- Produce byte-identical output for identical inputs
- Handle all specified error conditions
- Maintain deterministic behavior

### 11.2 Test Coverage

Normative behaviors MUST be covered by:
- Unit tests for individual decisions
- Integration tests for combined behaviors
- Golden tests with SHA-256 validation
- Round-trip tests for fidelity
- Parallel processing determinism tests

### 11.3 Documentation Requirements

Normative decisions MUST be:
- Documented in user-facing documentation
- Included in API documentation
- Covered in migration guides
- Explained in troubleshooting guides
- Referenced in error messages

## 12. Version Compatibility

### 12.1 Backward Compatibility

**Rule**: Normative decisions are binding within major versions.

**Implementation**:
- Minor versions may add new normative decisions
- Existing decisions cannot be changed in minor versions
- Breaking changes require major version increment
- Clear migration path for breaking changes

### 12.2 Forward Compatibility

**Rule**: Implementations should be designed for future extensibility.

**Implementation**:
- Schema format supports additional fields
- Error codes have room for expansion
- Configuration options use extensible formats
- APIs designed for backward compatibility

**Rationale**: Enables long-term stability while allowing evolution.

## Conclusion

These normative specifications ensure that copybook-rs provides deterministic, reproducible behavior suitable for production use in financial and business-critical applications. All implementations must adhere to these specifications to maintain compatibility and reliability.

For questions about normative behaviors or requests for clarification, please refer to the project documentation or file an issue with the development team.