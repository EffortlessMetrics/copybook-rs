# Nested ODO & OCCURS Behavior (Issue #164)

**Status**: Design Contract (Phase N1)
**Last Updated**: 2025-11-20
**Scope**: OCCURS / OCCURS DEPENDING ON interactions with group structure, REDEFINES, RENAMES
**Related**: AC3 (children inside ODO), AC4 (siblings after ODO), Issue #133 (RENAMES)

## Problem Statement

### Current Behavior

- ODO is supported as a tail array with `OCCURS ... DEPENDING ON`
- **AC3 tests** (`golden_fixtures_ac3_child_inside_odo.rs`): ODO arrays CAN contain child fields and nested groups (✅ supported)
- **AC4 tests** (`golden_fixtures_ac4_sibling_after_odo_fail.rs`): Storage fields after ODO arrays are rejected with `CBKP021_ODO_NOT_TAIL` (🚫 enforced)
- **Existing error codes**:
  - `CBKP021_ODO_NOT_TAIL`: ODO must be tail element (no storage siblings after)
  - `CBKS301_ODO_CLIPPED`: ODO counter exceeds maximum (lenient mode: clamp with warning)
  - `CBKS302_ODO_RAISED`: ODO counter below minimum (lenient mode: clamp with warning)

### Goal

Define exactly which nested/complex ODO patterns are:
- ✅ **Supported**: Schema + codec handle correctly
- 🚫 **Rejected by design**: Clear error codes with documented rationale
- ⏳ **Future consideration**: Explicit design decision deferred to later phases

## Scenarios

| ID  | Scenario                                | Status | Error Code            | Notes                              |
|-----|-----------------------------------------|--------|-----------------------|------------------------------------|
| O1  | Simple tail ODO                         | ✅     | -                     | Already supported (AC3 tests)      |
| O2  | Tail ODO with DYNAMIC (AC1/AC2)         | ✅     | -                     | Already supported (codec tests)    |
| O3  | Group-with-ODO tail (AC3)               | ✅     | -                     | Fully supported with nested groups |
| O4  | ODO with sibling after (AC4)            | 🚫     | CBKP021_ODO_NOT_TAIL  | By design: must be tail-only       |
| O5  | Nested ODO (ODO inside ODO)             | 🚫     | CBKP022_NESTED_ODO    | Phase N1: reject; Phase N2: review |
| O6  | ODO over REDEFINES                      | 🚫     | CBKP023_ODO_REDEFINES | Needs dedicated design (Phase N3)  |
| O7  | ODO over RENAMES span (R4/R5 scenarios) | 🚫     | Out of scope          | RENAMES R4-R6 explicitly deferred  |

## Detailed Scenarios

### O1 – Simple Tail ODO (✅ Supported)

**COBOL Example:**

```cobol
01 SIMPLE-RECORD.
   05 ITEM-COUNT  PIC 9(3).
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-CODE    PIC X(10).
      10 ITEM-QTY     PIC 9(5).
```

**Layout Expectations:**
- `ITEMS` is a variable-length array with min=1, max=100, counter=`ITEM-COUNT`
- Each occurrence is a pair `{item_code, item_qty}`
- Array is at tail position (no storage siblings after)

**Expected JSON:**

```json
{
  "item_count": 3,
  "items": [
    {"item_code": "ABC123", "item_qty": 10},
    {"item_code": "DEF456", "item_qty": 25},
    {"item_code": "GHI789", "item_qty": 5}
  ]
}
```

**Test Evidence**: `copybook-core/tests/golden_fixtures_ac3_child_inside_odo.rs::test_ac3_basic_child_inside_odo_pass`

---

### O2 – Tail ODO with DYNAMIC (✅ Supported)

**COBOL Example:**

```cobol
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID     PIC X(12).
   05 ACCOUNT-COUNT   PIC 9(4).
   05 ACCOUNTS OCCURS 1 TO 500 TIMES DEPENDING ON ACCOUNT-COUNT.
      10 ACCOUNT-NUM  PIC 9(10).
      10 BALANCE      PIC S9(9)V99 COMP-3.
```

**Layout Expectations:**
- Same as O1, but with different data types and scale
- COMP-3 (packed decimal) fields supported within ODO

**Expected JSON:**

```json
{
  "customer_id": "CUST00012345",
  "account_count": 2,
  "accounts": [
    {"account_num": 1234567890, "balance": 12500.50},
    {"account_num": 9876543210, "balance": -250.00}
  ]
}
```

**Test Evidence**: `copybook-codec/tests/odo_comprehensive.rs` (multiple AC1/AC2 scenarios)

---

### O3 – Group-with-ODO Tail (✅ Supported)

**COBOL Example:**

```cobol
01 EMPLOYEE-DIR.
   05 DEPT-CODE   PIC X(4).
   05 EMP-COUNT   PIC 9(3).
   05 EMPLOYEES OCCURS 1 TO 100 TIMES DEPENDING ON EMP-COUNT.
      10 EMP-HEADER.
         15 EMP-ID       PIC X(10).
         15 EMP-NAME     PIC X(40).
      10 CONTACT-INFO.
         15 EMAIL        PIC X(60).
         15 PHONE        PIC X(15).
      10 ADDRESS-INFO.
         15 STREET       PIC X(50).
         15 CITY         PIC X(25).
```

**Layout Expectations:**
- `EMPLOYEES` is a tail ODO array
- Each occurrence contains nested groups: `EMP-HEADER`, `CONTACT-INFO`, `ADDRESS-INFO`
- Hierarchical structure maintained within each array element

**Expected JSON:**

```json
{
  "dept_code": "D001",
  "emp_count": 1,
  "employees": [
    {
      "emp_header": {
        "emp_id": "E001",
        "emp_name": "John Doe"
      },
      "contact_info": {
        "email": "john.doe@example.com",
        "phone": "555-1234"
      },
      "address_info": {
        "street": "123 Main St",
        "city": "Springfield"
      }
    }
  ]
}
```

**Test Evidence**: `copybook-core/tests/golden_fixtures_ac3_child_inside_odo.rs::test_ac3_nested_groups_inside_odo_pass`

---

### O4 – ODO with Sibling After (🚫 Rejected)

**COBOL Example (Invalid):**

```cobol
01 INVALID-RECORD.
   05 ITEM-COUNT      PIC 9(3).
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM-CODE    PIC X(10).
   05 RECORD-TRAILER  PIC X(10).  -- ❌ Storage sibling after ODO
```

**Contract:**
- ODO arrays **MUST** be at tail position within their group
- Storage-bearing siblings after ODO violate memory layout constraints
- Error: `CBKP021_ODO_NOT_TAIL`

**Rationale:**
- Variable-length ODO arrays cannot have fixed-offset siblings after them
- Attempting to access `RECORD-TRAILER` would require knowing exact ODO runtime length
- Mainframe COBOL compilers enforce similar constraints

**Test Evidence**: `copybook-core/tests/golden_fixtures_ac4_sibling_after_odo_fail.rs::test_ac4_basic_storage_after_odo_fail`

**Non-storage Exception:**
- **Level-88 condition values** after ODO are permitted (they have no storage)
- **Level-66 RENAMES** after ODO are explicitly out of scope (see O7)

---

### O5 – Nested ODO (ODO inside ODO) (🚫 Rejected for Phase N1)

**COBOL Example (Not Supported):**

```cobol
01 OUTER-REC.
   05 GROUP-COUNT     PIC 9(2).
   05 GROUPS OCCURS 1 TO 5 TIMES DEPENDING ON GROUP-COUNT.
      10 SUB-COUNT    PIC 9(2).
      10 SUB-ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON SUB-COUNT.
         15 VALUE     PIC X(10).
```

**Phase N1 Decision: 🚫 Reject**

**Error Code (New):** `CBKP022_NESTED_ODO`

**Rationale:**
- Nested variable-length arrays introduce significant complexity:
  - **Schema complexity**: Requires tracking ODO context stack during parsing
  - **Codec complexity**: Nested counter resolution with multiple runtime dependencies
  - **Memory layout**: Multi-dimensional variable-length allocation
  - **JSON representation**: Nested arrays of arrays with runtime-determined dimensions

**Examples in the Wild:**
- Banking: Transaction batches with variable sub-transactions per transaction
- Telecom: Call detail records with variable event logs per call
- Insurance: Policy records with variable riders per coverage type

**Phase N2 Consideration:**
- If user demand emerges, Phase N2 could design support for:
  - **Constrained nested ODO**: Inner ODO must still be tail within outer ODO occurrence
  - **Counter scoping rules**: Clear resolution order for nested counters
  - **JSON nesting**: Arrays of arrays with proper structural validation

**Current Test Strategy:**
- Add negative test in Phase N1: parse copybook with nested ODO → expect `CBKP022_NESTED_ODO`
- Document explicit rejection in support matrix

---

### O6 – ODO over REDEFINES (🚫 Rejected for Phase N1)

**COBOL Example (Not Supported):**

```cobol
01 COMPLEX-RECORD.
   05 ITEM-COUNT      PIC 9(3).
   05 DATA-AREA.
      10 DATA-FIXED   PIC X(100).
   05 DATA-VARIABLE REDEFINES DATA-AREA
      OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM         PIC X(10).
```

**Phase N1 Decision: 🚫 Reject**

**Error Code (New):** `CBKP023_ODO_REDEFINES`

**Rationale:**
- **Semantic conflict**: REDEFINES implies fixed overlay; ODO implies variable length
- **Memory model**: How does a variable-length array redefine a fixed-length area?
- **Codec ambiguity**: Which view takes precedence during decode/encode?

**Alternative Patterns:**
- Use REDEFINES without OCCURS DEPENDING ON (fixed OCCURS is OK)
- Use ODO without REDEFINES
- Separate the variable array from the REDEFINES overlay

**Phase N3 Consideration:**
- Dedicated design for REDEFINES + OCCURS interactions
- Potential support scenarios:
  - REDEFINES with fixed OCCURS (no ODO) ✅ already supported
  - ODO that redefines another ODO with same bounds (niche use case)
  - Explicit codec rules for which view is "canonical"

**Current Test Strategy:**
- Add negative test in Phase N1: parse ODO over REDEFINES → expect `CBKP023_ODO_REDEFINES`

---

### O7 – ODO over RENAMES Span (🚫 Out of Scope)

**COBOL Example (Out of Scope):**

```cobol
01 RECORD-WITH-RENAMES.
   05 ITEM-COUNT     PIC 9(3).
   05 ITEMS OCCURS 1 TO 100 TIMES DEPENDING ON ITEM-COUNT.
      10 START-FIELD PIC X(10).
      10 MID-FIELD   PIC X(20).
      10 END-FIELD   PIC X(10).
   66 ITEM-RANGE RENAMES START-FIELD THRU END-FIELD.
```

**Decision: 🚫 Explicitly Out of Scope**

**Rationale:**
- RENAMES R4-R6 scenarios (RENAMES + REDEFINES/OCCURS/Level-88) are explicitly deferred per `docs/design/RENAMES_NESTED_GROUPS.md`
- Alias semantics over variable-length arrays are undefined
- Current RENAMES support (R1-R3) does NOT cover ODO interactions

**Documented in:**
- `docs/design/RENAMES_NESTED_GROUPS.md` (R4-R6 out of scope)
- `docs/reference/COBOL_SUPPORT_MATRIX.md` (RENAMES section)

**Test Strategy:**
- No explicit negative test needed (covered by RENAMES R4-R6 policy)
- If encountered, likely rejected during RENAMES resolution or parsing

---

## Error Model

### Existing Error Codes

| Code                 | Severity | Meaning                                     | Mode Behavior                   |
|----------------------|----------|---------------------------------------------|---------------------------------|
| CBKP021_ODO_NOT_TAIL | Fatal    | ODO array has storage sibling after it     | Always fatal (parser-level)     |
| CBKS301_ODO_CLIPPED  | Warning  | ODO counter exceeds maximum at runtime     | Lenient: clamp; Strict: fatal   |
| CBKS302_ODO_RAISED   | Warning  | ODO counter below minimum at runtime       | Lenient: clamp; Strict: fatal   |

### New Error Codes (Phase N1)

| Code                  | Severity | Meaning                                     | Mode Behavior                   |
|-----------------------|----------|---------------------------------------------|---------------------------------|
| CBKP022_NESTED_ODO    | Fatal    | ODO array contains another ODO array       | Always fatal (parser-level)     |
| CBKP023_ODO_REDEFINES | Fatal    | ODO array used with REDEFINES clause       | Always fatal (parser-level)     |

### Error Context

All ODO-related errors should include:
- **Field path**: Fully qualified path to the ODO field (e.g., `OUTER-REC.GROUPS.SUB-ITEMS`)
- **Level number**: COBOL level of the violating field
- **Line/column**: Source location in copybook (if available)
- **Counter field**: Name of the DEPENDING ON counter field (where applicable)

---

## Implementation Notes

### Phase N1 (Design Contract - Current Phase)

**Deliverables:**
1. ✅ This design document (`NESTED_ODO_BEHAVIOR.md`)
2. ⏳ Update `docs/reference/COBOL_SUPPORT_MATRIX.md` with O1-O7 rows
3. ⏳ Update `CLAUDE.md` to reference nested ODO design
4. ⏳ Create negative tests for O5/O6 (if not already covered)

**No code changes required** - O1-O4 are already correctly handled.

### Phase N2 (Optional - Nested ODO Support)

**Triggered by:** User demand for nested ODO scenarios (O5)

**Design Questions:**
1. How to represent nested arrays in `Schema` AST?
2. Counter resolution order (outer-to-inner vs inner-to-outer)?
3. JSON shape: nested arrays vs flattened with paths?
4. Memory layout: how to track nested variable-length offsets?
5. Codec streaming: can we maintain <256 MiB memory for deeply nested ODO?

**Acceptance Criteria:**
- Updated design doc with nested ODO semantics
- Parser changes to allow (but validate) nested ODO structures
- Layout module changes to compute nested variable offsets
- Codec changes to handle nested counter resolution
- Comprehensive golden fixtures for nested ODO patterns
- Performance validation: streaming with bounded memory

### Phase N3 (Future - REDEFINES + ODO Interactions)

**Triggered by:** Design decision to support O6 patterns

**Dependencies:**
- Comprehensive REDEFINES support (currently basic scenarios only)
- Decision on canonical view for REDEFINES with variable length

**Design Questions:**
1. Which REDEFINES view is canonical during decode/encode?
2. How to validate ODO bounds against REDEFINES overlay sizes?
3. JSON representation: both views, or canonical only?
4. Error handling: what happens if ODO runtime length exceeds REDEFINES area?

---

## Test Strategy

### Existing Tests (O1-O4)

| Scenario | Test File                                               | Status |
|----------|---------------------------------------------------------|--------|
| O1       | `golden_fixtures_ac3_child_inside_odo.rs::test_ac3_basic_child_inside_odo_pass` | ✅ Pass |
| O2       | `odo_comprehensive.rs` (multiple codec tests)           | ✅ Pass |
| O3       | `golden_fixtures_ac3_child_inside_odo.rs::test_ac3_nested_groups_inside_odo_pass` | ✅ Pass |
| O4       | `golden_fixtures_ac4_sibling_after_odo_fail.rs::test_ac4_basic_storage_after_odo_fail` | ✅ Fail (expected) |

### New Tests Needed (O5-O6)

**File:** `copybook-core/tests/nested_odo_structure_tests.rs` (to be created in Phase N1)

```rust
#[test]
fn o5_nested_odo_rejected_with_cbkp022() {
    const CPY: &str = r"
01 OUTER-REC.
   05 GROUP-COUNT     PIC 9(2).
   05 GROUPS OCCURS 1 TO 5 TIMES DEPENDING ON GROUP-COUNT.
      10 SUB-COUNT    PIC 9(2).
      10 SUB-ITEMS OCCURS 1 TO 10 TIMES DEPENDING ON SUB-COUNT.
         15 VALUE     PIC X(10).
";
    let err = parse_copybook(CPY).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP022_NESTED_ODO);
}

#[test]
fn o6_odo_over_redefines_rejected_with_cbkp023() {
    const CPY: &str = r"
01 COMPLEX-RECORD.
   05 ITEM-COUNT      PIC 9(3).
   05 DATA-AREA.
      10 DATA-FIXED   PIC X(100).
   05 DATA-VARIABLE REDEFINES DATA-AREA
      OCCURS 1 TO 10 TIMES DEPENDING ON ITEM-COUNT.
      10 ITEM         PIC X(10).
";
    let err = parse_copybook(CPY).unwrap_err();
    assert_eq!(err.code, ErrorCode::CBKP023_ODO_REDEFINES);
}
```

**Initially:** Mark these tests as `#[ignore]` if the parser doesn't yet emit these specific codes.

**Phase N1 Completion:** Remove `#[ignore]` once error codes are added to `ErrorCode` enum.

---

## JSON Stability Contract

### O1-O4 (Supported Scenarios)

**JSON shape is stable** for:
- Simple tail ODO → array of objects/scalars
- Nested groups inside ODO → array of nested objects
- Runtime array size determined by counter field value

**Example:**

```json
{
  "item_count": 2,
  "items": [
    {"code": "A", "qty": 10},
    {"code": "B", "qty": 20}
  ]
}
```

**Array length = `item_count` value** (clamped to min/max bounds per CBKS301/CBKS302 rules).

### O5-O6 (Rejected Scenarios)

**No JSON representation** - parsing fails before codec stage.

### Future Nested ODO (If Supported in Phase N2)

**Proposed JSON shape:**

```json
{
  "group_count": 2,
  "groups": [
    {
      "sub_count": 3,
      "sub_items": [
        {"value": "A1"},
        {"value": "A2"},
        {"value": "A3"}
      ]
    },
    {
      "sub_count": 2,
      "sub_items": [
        {"value": "B1"},
        {"value": "B2"}
      ]
    }
  ]
}
```

**Design principle:** Nested arrays of arrays, with counter fields co-located with their corresponding arrays.

---

## References

- **AC3 Tests**: `copybook-core/tests/golden_fixtures_ac3_child_inside_odo.rs`
- **AC4 Tests**: `copybook-core/tests/golden_fixtures_ac4_sibling_after_odo_fail.rs`
- **ODO Tail Validation**: `copybook-core/tests/odo_tail_validation.rs`
- **RENAMES Design**: `docs/design/RENAMES_NESTED_GROUPS.md` (R4-R6 out of scope)
- **Support Matrix**: `docs/reference/COBOL_SUPPORT_MATRIX.md`
- **Error Codes**: `docs/reference/ERROR_CODES.md`
- **Issue Tracking**: Issue #164 (this design), Issue #133 (RENAMES)

---

## Decision Log

| Date       | Decision                                      | Rationale                                                |
|------------|-----------------------------------------------|----------------------------------------------------------|
| 2025-11-20 | O1-O4: Affirm existing behavior as correct    | AC3/AC4 tests comprehensively validate current support  |
| 2025-11-20 | O5: Reject nested ODO in Phase N1             | Complexity vs user demand; revisit if needed             |
| 2025-11-20 | O6: Reject ODO+REDEFINES in Phase N1          | Semantic ambiguity; needs dedicated design               |
| 2025-11-20 | O7: Out of scope per RENAMES R4-R6 policy     | RENAMES interactions explicitly deferred                 |
| 2025-11-20 | Add CBKP022/CBKP023 error codes for O5/O6     | Explicit rejection better than silent failure            |

---

## Appendix: COBOL Compiler Behavior

### IBM Enterprise COBOL

- **Nested ODO**: ✅ Supported (with restrictions on counter scoping)
- **ODO + REDEFINES**: ⚠️ Allowed but semantically complex; discouraged in style guides
- **ODO tail constraint**: ✅ Enforced (no storage after ODO)

### Micro Focus COBOL

- **Nested ODO**: ✅ Supported (with detailed documentation on counter resolution)
- **ODO + REDEFINES**: ⚠️ Allowed with specific rules (REDEFINES must encompass entire ODO)
- **ODO tail constraint**: ✅ Enforced

### GnuCOBOL

- **Nested ODO**: ✅ Partial support (with warnings)
- **ODO + REDEFINES**: ⚠️ Allowed but flagged as potential data integrity issue
- **ODO tail constraint**: ✅ Enforced

### copybook-rs Decision

For Phase N1:
- **Be conservative**: Reject patterns (O5/O6) that introduce ambiguity
- **Be explicit**: Clear error codes and documentation
- **Be pragmatic**: Revisit if user demand emerges with concrete use cases

For future phases:
- **Follow IBM Enterprise COBOL semantics** where possible (industry standard)
- **Provide escape hatches**: Pre-processing tools or normalization for unsupported patterns
- **Maintain JSON stability**: No breaking changes to supported scenarios (O1-O4)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
