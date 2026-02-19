<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# COBOL Structural Validation Rules Specification

## Document Information

- **Document Type**: Technical Specification
- **Component**: copybook-rs COBOL Structural Validation
- **Issue**: #53 - Golden Fixtures Enhancement
- **Status**: Implementation Ready
- **Version**: 1.0
- **Date**: 2025-09-25

## Executive Summary

This specification defines the comprehensive set of COBOL structural validation rules that copybook-rs enforces to ensure mainframe data integrity. These rules govern the interaction between ODO (Occurs Depending On), Level-88 condition values, REDEFINES clauses, and other COBOL structural elements in enterprise production environments.

## COBOL Structural Fundamentals

### Storage vs Non-Storage Elements

COBOL data structures distinguish between storage and non-storage elements, which is fundamental to structural validation:

**Storage Elements** (consume memory):
- Elementary data fields with PIC clauses
- Group items containing storage elements
- OCCURS clauses (fixed and variable)
- REDEFINES clauses

**Non-Storage Elements** (no memory consumption):
- Level-88 condition names
- Level-66 RENAMES clauses (when supported)
- Certain compiler directives

## Core Structural Validation Rules

### Rule Set S1: ODO (Occurs Depending On) Constraints

#### S1.1: ODO Tail Positioning Rule

**Rule**: ODO arrays MUST be the last storage element within their containing group.

**Rationale**: Variable-length arrays require all subsequent data to have predictable offsets. Storage elements after ODO would have indeterminate positions.

**Error Code**: `CBKP021_ODO_NOT_TAIL`

**Valid Examples**:
```cobol
01 VALID-RECORD.
   05 COUNT-FIELD     PIC 9(3).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
      10 ITEM-NAME    PIC X(20).
   *> Level-88 after ODO is valid (non-storage)
   88 STATUS-ACTIVE   VALUE 'Y'.

01 VALID-NESTED.
   05 OUTER-GROUP.
      10 ITEM-COUNT   PIC 9(2).
      10 ITEMS OCCURS 1 TO 5 TIMES DEPENDING ON ITEM-COUNT.
         15 ITEM-ID   PIC 9(6).
   05 OTHER-DATA      PIC X(10).  *> Valid: ODO ended at group level
```

**Invalid Examples**:
```cobol
01 INVALID-RECORD.
   05 COUNT-FIELD     PIC 9(3).
   05 VARIABLE-ARRAY OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
      10 ITEM-NAME    PIC X(20).
   05 TRAILER-FIELD   PIC X(1).    *> INVALID: Storage after ODO
```

#### S1.2: ODO Counter Accessibility Rule

**Rule**: ODO counter fields MUST be accessible and precede the ODO array in the data layout.

**Rationale**: Runtime ODO evaluation requires the counter value to be available before processing the array.

**Error Code**: `CBKS121_COUNTER_NOT_FOUND`

**Valid Examples**:
```cobol
01 VALID-COUNTER.
   05 ITEM-COUNT      PIC 9(3).    *> Counter precedes ODO
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-DATA    PIC X(50).

01 VALID-NESTED-COUNTER.
   05 HEADER-DATA     PIC X(10).
   05 COUNT-GROUP.
      10 ITEM-COUNT   PIC 9(3).
   05 ITEMS OCCURS 1 TO 50 TIMES DEPENDING ON COUNT-GROUP.ITEM-COUNT.
      10 ITEM-ID      PIC 9(6).
```

**Invalid Examples**:
```cobol
01 INVALID-COUNTER.
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON MISSING-COUNT.
      10 ITEM-DATA    PIC X(50).
   *> MISSING-COUNT is not defined

01 INVALID-ORDER.
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-DATA    PIC X(50).
   05 ITEM-COUNT      PIC 9(3).    *> INVALID: Counter after ODO
```

#### S1.3: ODO Bounds Validation Rule

**Rule**: ODO counter values MUST be within specified bounds during processing.

**Rationale**: Out-of-bounds values can cause buffer overflows or memory corruption.

**Error Codes**:
- `CBKS301_ODO_CLIPPED` (warning in lenient mode)
- `CBKS302_ODO_RAISED` (warning in lenient mode)

**Processing Behavior**:
```cobol
05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
   10 ITEM-DATA PIC X(10).

*> Counter value: 0   → Raised to minimum (1), warning CBKS302
*> Counter value: 50  → Valid, no warning
*> Counter value: 150 → Clipped to maximum (100), warning CBKS301
```

### Rule Set S2: Level-88 Condition Value Constraints

#### S2.1: Level-88 Non-Storage Rule

**Rule**: Level-88 condition names do NOT constitute storage elements and do not affect ODO tail constraints.

**Rationale**: Level-88 entries are metadata associated with data items and do not consume additional storage.

**Valid Examples**:
```cobol
01 VALID-LEVEL88-AFTER-ODO.
   05 STATUS-COUNT    PIC 9(2).
   05 STATUS-ARRAY OCCURS 1 TO 20 TIMES DEPENDING ON STATUS-COUNT.
      10 STATUS-CODE  PIC X(1).
   88 ALL-ACTIVE      VALUE 'Y'.   *> Valid: Non-storage after ODO
   88 ALL-INACTIVE    VALUE 'N'.   *> Valid: Non-storage after ODO

01 VALID-NESTED-88.
   05 COUNT-FIELD     PIC 9(3).
   05 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON COUNT-FIELD.
      10 ITEM-TYPE    PIC X(1).
      88 TYPE-ACTIVE  VALUE 'A'.   *> Valid: 88 within ODO element
      10 ITEM-VALUE   PIC 9(5).
```

#### S2.2: Level-88 Value Validation Rule

**Rule**: Level-88 VALUE clauses MUST specify valid literal values compatible with the associated data item.

**Rationale**: Condition names must have meaningful values for proper conditional processing.

**Valid Examples**:
```cobol
05 STATUS-CODE       PIC X(1).
   88 STATUS-ACTIVE  VALUE 'A'.
   88 STATUS-PENDING VALUE 'P'.
   88 STATUS-CLOSED  VALUE 'C'.

05 NUMERIC-CODE      PIC 9(2).
   88 CODE-VALID     VALUE 01 THRU 99.
   88 CODE-SPECIAL   VALUE 00.
```

### Rule Set S3: REDEFINES Interaction Constraints

#### S3.1: REDEFINES Storage Equivalence Rule

**Rule**: REDEFINES clauses MUST specify storage areas of equivalent or smaller size than the original definition.

**Rationale**: Storage redefinition cannot exceed the allocated memory space.

**Valid Examples**:
```cobol
01 ORIGINAL-LAYOUT.
   05 DATA-AREA       PIC X(100).

01 REDEFINED-LAYOUT REDEFINES ORIGINAL-LAYOUT.
   05 HEADER-PART     PIC X(20).
   05 DETAIL-PART     PIC X(80).    *> Total: 100 bytes (valid)

01 NUMERIC-AREA       PIC 9(10).
01 PACKED-AREA REDEFINES NUMERIC-AREA PIC S9(8) COMP-3.  *> Smaller (valid)
```

#### S3.2: REDEFINES with ODO Constraint Rule

**Rule**: REDEFINES of ODO structures MUST account for variable-length semantics.

**Rationale**: Variable-length redefinition requires careful handling of dynamic sizing.

**Implementation Note**: Current copybook-rs implementation may have specific limitations on REDEFINES with ODO that should be documented and validated.

#### S3.3: Level-88 on REDEFINES Rule

**Rule**: Level-88 condition names MAY be applied to REDEFINES storage areas.

**Rationale**: Conditional values can provide meaningful interpretation of redefined storage.

**Valid Examples**:
```cobol
01 BINARY-DATA        PIC X(4).
01 NUMERIC-VIEW REDEFINES BINARY-DATA PIC S9(9) COMP.
   88 POSITIVE-VALUE  VALUE 1 THRU 999999999.
   88 NEGATIVE-VALUE  VALUE -999999999 THRU -1.
   88 ZERO-VALUE      VALUE 0.
```

### Rule Set S4: Nested Structure Constraints

#### S4.1: Hierarchical Storage Rule

**Rule**: Storage elements within nested groups MUST respect ODO tail constraints at each hierarchical level.

**Rationale**: ODO constraints apply within each group scope, not globally.

**Valid Examples**:
```cobol
01 MULTI-LEVEL-RECORD.
   05 LEVEL1-COUNT    PIC 9(2).
   05 LEVEL1-ARRAY OCCURS 1 TO 5 TIMES DEPENDING ON LEVEL1-COUNT.
      10 LEVEL2-COUNT PIC 9(2).
      10 LEVEL2-ARRAY OCCURS 1 TO 3 TIMES DEPENDING ON LEVEL2-COUNT.
         15 ITEM-DATA PIC X(10).
      *> ODO ends at level1 scope - next sibling is valid
   05 TRAILER-DATA    PIC X(5).    *> Valid: Different scope level
```

#### S4.2: Cross-Level Reference Rule

**Rule**: ODO counter references across hierarchical levels MUST use qualified names.

**Rationale**: Unqualified references can create ambiguity in nested structures.

**Valid Examples**:
```cobol
01 QUALIFIED-REFERENCE.
   05 OUTER-GROUP.
      10 ITEM-COUNT   PIC 9(2).
   05 INNER-GROUP.
      10 ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON OUTER-GROUP.ITEM-COUNT.
         15 ITEM-ID   PIC 9(6).
```

### Rule Set S5: Enterprise Production Constraints

#### S5.1: Record Size Limits Rule

**Rule**: Computed record sizes MUST NOT exceed implementation-defined maximum limits.

**Rationale**: Memory allocation and buffer management require reasonable size constraints.

**Error Code**: `CBKS141_RECORD_TOO_LARGE`

**Default Limits**:
- Maximum record size: 16,777,216 bytes (16 MiB)
- Maximum ODO array size: 1,000,000 elements
- Maximum nesting depth: 50 levels

#### S5.2: Performance Constraint Rule

**Rule**: Structural validation MUST complete within performance targets for production use.

**Performance Targets**:
- Parse validation: <100ms per copybook
- Structure analysis: <50ms per copybook
- Memory usage: <256 MiB steady state

#### S5.3: Mainframe Compatibility Rule

**Rule**: All structural validation MUST align with IBM Enterprise COBOL and Micro Focus COBOL semantics.

**Rationale**: Enterprise production requires compatibility with existing mainframe compilers.

## Error Handling Patterns

### Structural Error Categories

```rust
pub enum StructuralErrorCategory {
    /// Critical structural violations that prevent processing
    Critical,
    /// Warnings about potential issues that allow processing
    Warning,
    /// Performance-related constraints
    Performance,
    /// Enterprise compatibility concerns
    Compatibility,
}
```

### Error Context Requirements

All structural validation errors MUST provide:

1. **Field Path**: Fully qualified path to the problematic element
2. **Rule Violation**: Specific rule that was violated
3. **Remediation**: Clear guidance on how to fix the issue
4. **Impact**: Description of the production impact

**Example Error Context**:
```
Error: CBKP021_ODO_NOT_TAIL
Field: ROOT.CUSTOMER-RECORD.ORDER-ITEMS
Group: ROOT.CUSTOMER-RECORD
Rule: ODO arrays must be the last storage element in their containing group
Remediation: Move ORDER-ITEMS to the end of CUSTOMER-RECORD, or move subsequent fields to a different group
Impact: Variable-length array processing requires predictable field offsets
```

## Validation Implementation Requirements

### Parser Integration

```rust
pub struct StructuralValidator {
    rules: Vec<Box<dyn ValidationRule>>,
    config: ValidationConfig,
}

pub trait ValidationRule {
    fn validate(&self, schema: &Schema) -> Vec<ValidationResult>;
    fn error_code(&self) -> ErrorCode;
    fn severity(&self) -> ValidationSeverity;
}

// Implementation examples
pub struct OdoTailRule;
pub struct Level88NonStorageRule;
pub struct RedefinesStorageRule;
```

### Runtime Validation

```rust
pub struct RuntimeStructuralValidator {
    schema: Schema,
    odo_bounds: OdoBoundsValidator,
}

impl RuntimeStructuralValidator {
    pub fn validate_odo_bounds(&self, counter_value: u32, field_path: &str) -> Result<u32> {
        // Validate and potentially clip ODO counter values
    }
}
```

## Testing Requirements

### Validation Test Matrix

| Rule Category | Positive Cases | Negative Cases | Edge Cases |
|---------------|----------------|----------------|------------|
| ODO Constraints | 12 | 8 | 4 |
| Level-88 Rules | 8 | 4 | 2 |
| REDEFINES Rules | 10 | 6 | 3 |
| Nested Structures | 15 | 10 | 5 |
| Production Constraints | 6 | 4 | 2 |

### Performance Validation

```rust
#[cfg(test)]
mod structural_performance_tests {
    #[test]
    fn validate_parse_performance() {
        // Ensure structural validation meets performance targets
    }

    #[test]
    fn validate_memory_constraints() {
        // Ensure structural validation respects memory limits
    }
}
```

## Compliance and Standards

### IBM Enterprise COBOL Alignment

- ODO semantics match IBM Enterprise COBOL V6.4
- Level-88 processing aligns with COBOL-2014 standard
- REDEFINES handling compatible with mainframe behavior

### Micro Focus COBOL Compatibility

- Support for Micro Focus COBOL extensions where applicable
- Clear documentation of differences from IBM behavior
- Fallback behavior for unsupported extensions

### Performance Standards

- Sub-millisecond validation for typical enterprise copybooks
- Linear scaling with copybook complexity
- <2% performance variance across validation runs

This specification establishes the comprehensive structural validation rules that ensure copybook-rs maintains enterprise-grade compatibility with mainframe COBOL semantics while providing clear error reporting and remediation guidance.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
