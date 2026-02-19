<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# RENAMES R4-R6 Advanced Scenarios Specification

**Status**: Specification Complete | Implementation: Phase 3.2
**Last Updated**: 2026-02-07
**Related Issues**: Phase 3.2 of implementation roadmap

## Overview

This specification defines the behavior of advanced RENAMES scenarios (R4-R6) in copybook-rs. These scenarios involve interactions between level-66 RENAMES and other COBOL features (REDEFINES, OCCURS, Level-88).

## Background

RENAMES (level-66) is a COBOL feature that creates aliases for fields or groups of fields. The current implementation supports:

- **R1**: Simple same-scope field RENAMES ✅
- **R2**: Same-scope group RENAMES ✅
- **R3**: Nested group RENAMES ✅

This specification defines the behavior for advanced scenarios:

- **R4**: RENAMES + REDEFINES (overlapping storage)
- **R5**: RENAMES + OCCURS (array segment aliasing)
- **R6**: RENAMES + Level-88 (condition value aliasing)

## R4: RENAMES + REDEFINES

### Definition

R4 involves a level-66 RENAMES that spans fields that include REDEFINES declarations. This creates overlapping storage scenarios where multiple interpretations of the same bytes exist.

### Example

```cobol
       01  TRANSACTION-RECORD.
           05  TRANS-TYPE      PIC X(1).
           05  TRANS-DATA      PIC X(20).
               10  CHECK-DATA   REDEFINES TRANS-DATA.
                   15  CHECK-NUM  PIC 9(8).
                   15  CHECK-AMT  PIC 9(10).
               10  CARD-DATA    REDEFINES TRANS-DATA.
                   15  CARD-NUM   PIC 9(16).
                   15  CARD-EXP   PIC 9(4).
           66  PAYMENT-INFO RENAMES CHECK-DATA THRU CHECK-DATA.
```

### Behavior Specification

#### When Feature Flag Disabled (Default)

- **Parse**: ✅ Allowed - syntax is valid COBOL
- **Resolver**: ❌ Rejected with error code `CBKS609_RENAME_OVER_REDEFINES`
- **Codec**: N/A - rejected during layout resolution
- **Error Message**: "RENAMES alias 'PAYMENT-INFO' spans REDEFINES field(s). This pattern is not supported. Use the base field name or enable the RenamesR4R6 feature flag."

#### When Feature Flag Enabled

- **Parse**: ✅ Allowed
- **Resolver**: ✅ Resolves to the first REDEFINES alternative in the range
- **Codec**: ✅ Decodes using the resolved alternative
- **Constraint**: Only single-alternative RENAMES (FROM==THRU) is supported

### Supported Patterns (Feature Flag Enabled)

| Pattern | Status | Notes |
|---------|--------|-------|
| RENAMES of single REDEFINES alternative | ✅ | FROM==THRU pointing to one REDEFINES field |
| RENAMES spanning multiple REDEFINES alternatives | ❌ | Rejected with CBKS610_RENAME_MULTIPLE_REDEFINES |
| RENAMES of base field with REDEFINES | ⚠️ | Resolves to base field semantics |

### Error Codes

| Code | Description | Severity |
|------|-------------|----------|
| CBKS609_RENAME_OVER_REDEFINES | RENAMES spans REDEFINES field(s) | Fatal (when flag disabled) |
| CBKS610_RENAME_MULTIPLE_REDEFINES | RENAMES spans multiple REDEFINES alternatives | Fatal (even when flag enabled) |

## R5: RENAMES + OCCURS

### Definition

R5 involves a level-66 RENAMES that spans fields including OCCURS (fixed or ODO) arrays. This creates scenarios where an alias references array segments or entire arrays.

### Example

```cobol
       01  ORDER-RECORD.
           05  ORDER-ID        PIC 9(8).
           05  LINE-ITEMS      OCCURS 10 TIMES.
               10  ITEM-CODE   PIC X(5).
               10  QUANTITY    PIC 9(3).
           66  ORDER-ITEMS RENAMES LINE-ITEMS.
```

### Behavior Specification

#### When Feature Flag Disabled (Default)

- **Parse**: ✅ Allowed - syntax is valid COBOL
- **Resolver**: ❌ Rejected with error code `CBKS607_RENAME_CROSSES_OCCURS` (existing code)
- **Codec**: N/A - rejected during layout resolution
- **Error Message**: "RENAMES alias 'ORDER-ITEMS' crosses OCCURS boundary. This pattern is not supported. Use the base field name or enable the RenamesR4R6 feature flag."

#### When Feature Flag Enabled

- **Parse**: ✅ Allowed
- **Resolver**: ✅ Resolves for single-array RENAMES (FROM==THRU pointing to OCCURS field)
- **Codec**: ✅ Decodes as array (same shape as OCCURS field)
- **Constraint**: Only single-array RENAMES is supported; partial array spans are rejected

### Supported Patterns (Feature Flag Enabled)

| Pattern | Status | Notes |
|---------|--------|-------|
| RENAMES of entire OCCURS array | ✅ | FROM==THRU pointing to OCCURS field |
| RENAMES spanning partial array elements | ❌ | Rejected with CBKS611_RENAME_PARTIAL_OCCURS |
| RENAMES with ODO | ❌ | Rejected with CBKS612_RENAME_ODO_NOT_SUPPORTED |

### Error Codes

| Code | Description | Severity |
|------|-------------|----------|
| CBKS607_RENAME_CROSSES_OCCURS | RENAMES crosses OCCURS boundary | Fatal (when flag disabled) |
| CBKS611_RENAME_PARTIAL_OCCURS | RENAMES spans partial array elements | Fatal (even when flag enabled) |
| CBKS612_RENAME_ODO_NOT_SUPPORTED | RENAMES with ODO arrays | Fatal (even when flag enabled) |

## R6: RENAMES + Level-88

### Definition

R6 involves a level-66 RENAMES that creates an alias for Level-88 condition values. Level-88 fields define named constants or condition names for a field.

### Example

```cobol
       01  STATUS-RECORD.
           05  STATUS-CODE     PIC X(1).
               88  STATUS-OK   VALUE 'A'.
               88  STATUS-ERR VALUE 'E'.
           66  STATUS-FLAG RENAMES STATUS-CODE THRU STATUS-CODE.
```

### Behavior Specification

#### When Feature Flag Disabled (Default)

- **Parse**: ✅ Allowed - syntax is valid COBOL
- **Resolver**: ⚠️ Resolves, but Level-88 fields are excluded from members
- **Codec**: ⚠️ Level-88 fields are non-storage and not included in JSON output
- **Note**: This is essentially R1 behavior; Level-88 fields are naturally excluded

#### When Feature Flag Enabled

- **Parse**: ✅ Allowed
- **Resolver**: ✅ Resolves with explicit Level-88 metadata tracking
- **Codec**: ✅ Includes Level-88 condition values in JSON as metadata (optional)
- **Enhancement**: When flag enabled, can expose condition names in JSON output

### Supported Patterns

| Pattern | Status | Notes |
|---------|--------|-------|
| RENAMES of field with Level-88 children | ✅ | Level-88 excluded from members (non-storage) |
| RENAMES with explicit Level-88 metadata | ✅ | Feature flag enables metadata tracking |

### Error Codes

No new error codes for R6. Level-88 fields are naturally handled as non-storage elements.

## Feature Flag Governance

### Flag Key

`RenamesR4R6` (alias: `renames_r4_r6`)

### Default State

**Disabled (false)** - Advanced RENAMES patterns are rejected with appropriate error codes.

### Owner

TBD - To be assigned during code review.

### Rollout Plan

1. **Phase 1** (Current): Specification and error code definition
2. **Phase 2**: Implementation of resolver support with feature flag
3. **Phase 3**: Gradual testing in staging environments
4. **Phase 4**: Promotion to production with monitoring
5. **Phase 5**: Cleanup and documentation updates

### Kill-Switch Behavior

When the feature flag is disabled:
- R4 patterns are rejected with `CBKS609_RENAME_OVER_REDEFINES`
- R5 patterns are rejected with `CBKS607_RENAME_CROSSES_OCCURS` (existing)
- R6 patterns work as R1 (Level-88 excluded naturally)

### Cleanup Milestone

v1.3.0 or appropriate based on user feedback and production validation.

## Dialect-Specific Notes

### IBM Enterprise COBOL

All R4-R6 patterns are syntactically valid in IBM Enterprise COBOL. The implementation follows IBM semantics where applicable.

### Micro Focus COBOL

All R4-R6 patterns are syntactically valid in Micro Focus COBOL. The implementation follows Micro Focus semantics where applicable.

### ANSI COBOL-85

All R4-R6 patterns are syntactically valid in ANSI COBOL-85. The implementation follows ANSI semantics where applicable.

## Error Handling Summary

### Rejection Behavior (Feature Flag Disabled)

| Scenario | Error Code | Message |
|----------|------------|---------|
| R4: RENAMES over REDEFINES | CBKS609_RENAME_OVER_REDEFINES | RENAMES alias spans REDEFINES field(s) |
| R5: RENAMES over OCCURS | CBKS607_RENAME_CROSSES_OCCURS | RENAMES crosses OCCURS boundary |
| R5: RENAMES over ODO | CBKS612_RENAME_ODO_NOT_SUPPORTED | RENAMES with ODO arrays |

### Rejection Behavior (Feature Flag Enabled - Invalid Patterns)

| Scenario | Error Code | Message |
|----------|------------|---------|
| R4: Multiple REDEFINES alternatives | CBKS610_RENAME_MULTIPLE_REDEFINES | RENAMES spans multiple REDEFINES alternatives |
| R5: Partial array span | CBKS611_RENAME_PARTIAL_OCCURS | RENAMES spans partial array elements |

## Implementation Notes

### Resolver Support

The resolver must:
1. Detect R4-R6 patterns during resolution
2. Check feature flag state
3. Return appropriate error codes when disabled
4. Implement resolution logic when enabled

### Codec Support

The codec must:
1. Handle R4-R6 aliases during decode (when flag enabled)
2. Maintain data integrity - RENAMES does not change underlying storage
3. Skip RENAMES during encode (non-storage semantics)

### Data Integrity

RENAMES is a non-storage feature. The underlying storage interpretation must remain unchanged regardless of RENAMES aliases. This is enforced by:
1. RENAMES fields have zero byte length
2. Codec operations work on storage fields only
3. Aliases are resolved to storage fields before data access

## Testing Requirements

### BDD Scenarios

See `copybook-core/tests/renames_r4_r6_bdd_tests.rs` for comprehensive BDD scenarios covering:
- R4 valid/invalid patterns
- R5 valid/invalid patterns
- R6 behavior with Level-88

### Golden Fixtures

See `fixtures/renames_r4_r6/` for copybook fixtures and expected JSON outputs.

### Property Tests

1. **renames resolution is stable**: Same input produces same output
2. **renames does not change underlying storage interpretation**: Data integrity

### Mutation Tests

Scope mutants to renames resolver logic in `copybook-core/src/layout.rs`.

## References

- [RENAMES Nested Groups Design](design/RENAMES_NESTED_GROUPS.md)
- [COBOL Support Matrix](reference/COBOL_SUPPORT_MATRIX.md)
- [Error Codes Reference](reference/ERROR_CODES.md)
- [Feature Flags Documentation](FEATURE_FLAGS.md)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
