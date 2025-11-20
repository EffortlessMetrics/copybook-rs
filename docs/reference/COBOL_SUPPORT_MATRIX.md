# COBOL Support Matrix

This document outlines the current state of COBOL feature support in `copybook-rs`.

## Feature Support Legend

| Status | Definition |
|:---:|---|
| ✅ | **Fully Supported**: Feature is implemented, tested, and verified in golden fixtures. |
| ⚠️ | **Partial Support**: Feature is implemented but has known limitations or edge cases. |
| ❌ | **Unsupported**: Feature is not currently implemented or explicitly out of scope. |

## Data Types

| Feature | Status | Notes |
|---|:---:|---|
| **Alphanumeric** (`PIC X`) | ✅ | Full support for EBCDIC/ASCII conversion. |
| **Zoned Decimal** (`PIC 9`) | ✅ | Full support including overpunch handling and sign processing. |
| **Packed Decimal** (`COMP-3`) | ✅ | Full support for standard and IBM-specific nibble handling. |
| **Binary Integer** (`COMP`) | ✅ | Big-endian integer support (2/4/8 bytes). |
| **Explicit Binary** (`BINARY(n)`) | ✅ | Support for explicit byte-width binary fields. |
| **Floating Point** (`COMP-1/2`) | ❌ | Single/Double precision float not supported. |
| **Edited PIC** (`Z`, `$`, etc.) | ❌ | Presentation-only clauses are currently ignored or trigger errors. |

## Structure & Organization

| Feature | Status | Notes |
|---|:---:|---|
| **Hierarchy** (01-49) | ✅ | Full support for standard record hierarchy. |
| **REDEFINES** | ✅ | Supported for providing multiple views of the same storage. |
| **OCCURS** (Fixed) | ✅ | Supported for fixed-length arrays. |
| **OCCURS DEPENDING ON** | ✅ | Supported for variable-length arrays (at tail position). |
| **Nested ODO** | ❌ | Nested `OCCURS DEPENDING ON` is not supported. |
| **SYNCHRONIZED** | ✅ | Supported with automatic slack byte insertion. |
| **BLANK WHEN ZERO** | ✅ | Supported via specific handling in codec. |
| **Level-88** (Condition Names) | ✅ | **Fully Supported**. Parsed and validated against parent fields. |
| **RENAMES** (66-level) | ⚠️ | **Partial Support**. Parser and resolver support basics, but complex nested group semantics are pending. |

## Record Formats

| Feature | Status | Notes |
|---|:---:|---|
| **Fixed Length** | ✅ | Constant LRECL records. |
| **Variable (RDW)** | ✅ | IBM Record Descriptor Word format. |
| **Line Sequential** | ❌ | Text-based records (newline delimited) not supported. |

## Detailed Feature Notes

### Level-88 Condition Values
Level-88 condition values are fully supported. They do not occupy storage but provide semantic aliases for values of their parent field.
- **Status**: ✅ Fully Supported
- **Validation**: Verified via `golden_fixtures_level88` and `enhanced_edge_case_validation`.

### RENAMES (66-level)
The `RENAMES` clause allows alternative groupings of elementary items.
- **Status**: ⚠️ Partial Support
- **Current State**: The parser accepts the syntax, and the resolver can handle basic aliasing. However, complex scenarios involving nested groups or codec projection are not yet fully implemented.
- **Recommendation**: Avoid relying on 66-level items for critical data mapping until full support is finalized.

### COMP-1 / COMP-2
Floating-point types are rare in commercial transaction processing and are currently out of scope.
- **Status**: ❌ Unsupported
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
