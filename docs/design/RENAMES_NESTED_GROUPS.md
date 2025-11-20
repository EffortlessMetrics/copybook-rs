# RENAMES Nested Group Semantics (Issue #133)

## Status

**Current**: Ready to Implement (Phase R1 complete; R2/R3 pending)
**Author**: Claude Code (automated analysis of Issue #133)
**Last Updated**: 2025-11-19
**Related Issues**: #133

> Resolver-based approach (Approach 2 – Resolver-Based Semantic Attach) is selected.
> This doc is now the implementation contract; copybook-core and copybook-codec are expected to match it.

## Scenarios and Expected Semantics

This section defines concrete RENAMES scenarios and the expected behavior in both
the schema and the JSON codec. R2 (implementation) must make the code match this table.

| ID  | Scenario                              | Example anchor     | Expected attach point          | JSON representation                        | Current support        |
|-----|---------------------------------------|--------------------|---------------------------------|--------------------------------------------|------------------------|
| R1  | Simple same-scope field RENAMES       | 66 for a child     | Immediate parent group         | Alias name points to same scalar value     | ✅ Implemented         |
| R2  | Same-scope group RENAMES              | 66 for a group     | Group being renamed            | Alias object with same fields as group     | ⚠️ Parser only         |
| R3  | Nested group RENAMES (intra-branch)   | 66 inside subtree  | Closest common ancestor group  | Alias object with subtree fields           | ⏳ Planned (R2)        |
| R4  | RENAMES + REDEFINES (overlapping)     | 66 over REDEFINES  | Storage group being redefined  | Alias reflects active redefine view only   | 🚫 Not supported (R2+) |
| R5  | RENAMES + OCCURS (array segment)      | 66 over OCCURS     | Parent group of OCCURS         | Alias JSON mirrors array segment semantics | 🚫 Not supported (R2+) |
| R6  | RENAMES + Level-88                    | 66 name for flags  | Same group as 88 conditions    | Alias exposes same boolean interpretation  | ⚠️ Needs explicit spec |

Legend:

- ✅ Implemented: parser + resolver + codec behavior are defined and tested.
- ⚠️ Parser only: parsed into AST, but resolver/codec semantics are incomplete.
- ⏳ Planned: part of Phase R2 work.
- 🚫 Not supported: explicitly out of scope for the next phase unless promoted.

In Phase R2 we will:

- Bring **R3** to "✅ Implemented".
- Decide whether **R4–R6** stay "not supported" or become explicit future work.

### R1 – Simple same-scope field RENAMES (✅)

**Example**:

```cobol
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID      PIC X(10).
   05 CUSTOMER-NAME    PIC X(30).
   66 PRIMARY-CUSTOMER RENAMES CUSTOMER-ID.
```

**Semantics**:

* `PRIMARY-CUSTOMER` is an alias of `CUSTOMER-ID` in the same storage.
* Schema:
  * One storage field; alias symbol is modeled as an additional logical name.
* JSON:
  * We **do not** duplicate data; JSON exposes only the storage name
    (e.g. `customer_id`) to avoid multiple mutable views of the same bytes.
* Current state: already supported by parser + resolver; codec does not treat
  the alias as a separate JSON key.

> **Contract**: R2 **must not** introduce additional JSON keys for simple aliases
> without an explicit design change; we keep JSON "single source of truth" for
> the storage field.

### R2 – Same-scope group RENAMES (⚠️ parser only)

**Example**:

```cobol
01 CUSTOMER-RECORD.
   05 CUSTOMER-INFO.
      10 NAME     PIC X(30).
      10 ADDRESS  PIC X(60).
   66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO.
```

**Semantics**:

* `CUSTOMER-DETAILS` is an alias for the group `CUSTOMER-INFO`.
* Attach point: `CUSTOMER-INFO` group in the schema tree.
* JSON:
  * Base behavior (R2) keeps JSON unchanged: the object remains under the
    storage name (e.g. `customer_info`).
  * We *may* eventually expose an alias view in schema metadata, but not as a
    duplicate JSON object.

**Current state**:

* Parsed to AST.
* Resolver currently attaches at level-01; this must be fixed in R2 to attach to
  the correct group node.

### R3 – Nested group RENAMES (intra-branch) (⏳ planned)

**Example**:

```cobol
01 POLICY-RECORD.
   05 POLICY-INFO.
      10 POLICY-NUMBER PIC X(10).
      10 POLICY-DATES.
         15 START-DATE PIC X(8).
         15 END-DATE   PIC X(8).
   66 POLICY-PERIOD RENAMES POLICY-DATES.
```

**Semantics**:

* `POLICY-PERIOD` should be attached to the `POLICY-INFO` subtree, pointing
  specifically at the `POLICY-DATES` group.
* Attach point:
  * The **closest common ancestor** of the `RENAMES` level and the target group.
  * In this example, that is `POLICY-INFO`.
* JSON:
  * As with R2, JSON remains under the storage group name (`policy_dates`).
  * The alias is carried only in schema metadata:
    * e.g. `Schema` knows that `policy_period` is an alias of `policy_dates`.

**Implementation contract (R2)**:

* Resolver must:
  * Build a stable mapping from `POLICY-PERIOD` to the target group node.
  * Prevent alias cycles and ambiguous ranges.
* Codec must:
  * Treat accesses via alias name as equivalent to accesses via the storage name.
  * Keep JSON stable (no duplicate objects).

## Problem Statement

### Current Behavior

The parser attaches level-66 RENAMES entries at the level-01 record using a "pop to level-01" heuristic:

```cobol
01 ROOT-REC.
   05 GROUP-A.
      10 FIELD-A PIC X(5).
      10 FIELD-B PIC 9(3).
      66 ALIAS-A RENAMES FIELD-A THRU FIELD-B.
```

**Current**: `ALIAS-A` is attached as a child of `ROOT-REC` (level 01)
**Problem**: Resolver cannot find `FIELD-A` or `FIELD-B` because they are children of `GROUP-A`, not `ROOT-REC`

### Desired Behavior

RENAMES should be attached under the nearest enclosing group that contains the from/thru fields:

```cobol
01 ROOT-REC.
   05 GROUP-A.
      10 FIELD-A PIC X(5).
      10 FIELD-B PIC 9(3).
      66 ALIAS-A RENAMES FIELD-A THRU FIELD-B.  ← Should be child of GROUP-A
```

**Desired**: `ALIAS-A` attached as child of `GROUP-A` (level 05) so resolver can find sibling fields

## Constraints Discovered

### Ambiguity of Level Numbers

Level numbers alone (05 vs 10 vs 66) are insufficient to distinguish:

1. **Nested group RENAMES** (desired behavior for #133):
   ```cobol
   05 GROUP-A.
      10 FIELD-A PIC X(5).
      10 FIELD-B PIC 9(3).
      66 ALIAS-A RENAMES FIELD-A THRU FIELD-B.  ← Want: child of GROUP-A
   ```

2. **Cross-sibling RENAMES** (valid COBOL, should attach at level-01):
   ```cobol
   05 FIELD-A PIC X(5).
   05 GROUP-B.
      10 FIELD-B PIC 9(3).
   66 ALIAS RENAMES FIELD-A THRU GROUP-B.  ← Want: child of ROOT-REC
   ```

Both have identical stack patterns from the parser's perspective (levels 05, 10, 66).

### Heuristic Failures

Attempted heuristics that proved unreliable:

#### 1. Level Gap Detection
**Approach**: Stop popping when `top.level < first_popped_level`

**Failure**: Cannot distinguish:
- Nested: `05 GROUP-A` → `10 FIELD-A` → `10 FIELD-B` → `66 ALIAS`
- Cross-sibling: `05 FIELD-A` → `05 GROUP-B` → `66 ALIAS`

Both trigger the same level-gap condition.

#### 2. Child Count Heuristic
**Approach**: Stop at groups with multiple children (strong signal of nested group)

**Failure**: Breaks for:
- Single-child groups (valid but rare)
- Groups populated after the RENAMES is processed
- Shallow multi-level patterns (02, 03, 49, 66)

#### 3. Same-Level Sibling Count
**Approach**: Count how many fields at the same level before deciding parent

**Failure**: Complex to implement correctly, still fails on:
- Shallow nesting (levels 02, 03, 49, 66)
- Groups with mixed-level children

### Edge Cases Identified

1. **Shallow multi-level RENAMES**:
   ```cobol
   02 FIELD-A PIC X(1).
   03 FIELD-B PIC X(1).
   49 FIELD-C PIC X(1).
   66 ALIAS RENAMES FIELD-A THRU FIELD-C.
   ```
   Level-based heuristics fail because there's no clear "parent group" signal.

2. **Cross-group boundaries**:
   ```cobol
   05 FIELD-A PIC X(5).
   05 GROUP-B.
      10 FIELD-B PIC 9(3).
   66 ALIAS RENAMES FIELD-A THRU GROUP-B.
   ```
   Should be detected as semantic error (CBKS606), not parser error.

## Candidate Approaches

### 1. Parser-Only (Attempted - Rejected)

**Approach**: Use level numbers and stack state to guess correct parent.

**Pros**:
- No semantic coupling
- Localized to parser

**Cons**:
- Fundamentally ambiguous - level numbers insufficient
- Multiple edge cases with different heuristics needed
- Brittle - small changes break assumptions
- Still cannot handle all valid COBOL patterns

**Decision**: ❌ **Rejected** - Cannot reliably distinguish nested vs cross-sibling patterns

### 2. Resolver-Based Semantic Attach (Recommended)

**Approach**: Parser attaches RENAMES with placeholder parent; resolver determines correct scope using field names.

#### Phase 1: Parser Changes
- Attach RENAMES where it naturally falls (keep simple "pop to 01" or generic stack rules)
- Store `from_field`, `thru_field`, and `decl_scope` (path to declaring node)
- Example: `decl_scope: "ROOT-REC.GROUP-A"` or pointer to GROUP-A node

#### Phase 2: Resolver Changes
- Given `decl_scope` and from/thru field names, search for actual fields
- Identify the smallest group whose children include both from and thru
- Validate contiguity, group boundaries, OCCURS constraints using full semantic info
- Emit appropriate error codes (CBKS601-608) based on semantic analysis

**Pros**:
- Has full semantic information (field names, paths)
- Can accurately detect cross-group boundaries vs nested scopes
- Error messages can reference actual field names and locations
- Handles all COBOL patterns including edge cases

**Cons**:
- More invasive changes (touches both parser and resolver)
- Requires careful error-code mapping
- May need to re-parent in AST or treat RENAMES as "symbol table entries"

**Benefits**:
- Correct by construction - uses semantic info instead of heuristics
- Future-proof for additional COBOL dialects
- Clear separation of concerns (parser = syntax, resolver = semantics)

### 3. Hybrid: Symbol Table Approach

**Approach**: Treat RENAMES as symbol table entries associated with scopes, not AST parent/child relationships.

- Parser: Build AST with simple placement rules
- Schema: RENAMES entries stored separately with scope metadata
- Resolver: Lookup RENAMES by scope + name, validate against field tree

**Pros**:
- Clean separation: AST structure vs semantic aliases
- Avoids re-parenting complexity
- Aligns with how RENAMES actually works in COBOL (aliasing, not storage)

**Cons**:
- Larger architectural change
- May affect existing field lookup APIs

## Recommendation

**Choose Approach 2: Resolver-Based Semantic Attach**

### Rationale

1. **Semantic Problem**: RENAMES scope determination is fundamentally semantic (requires field name resolution), not syntactic (level numbers insufficient)

2. **Correctness**: Resolver has all information needed to make correct decisions:
   - Field names (from/thru)
   - Parent-child relationships
   - Offset information for contiguity
   - Group membership for boundary validation

3. **Error Quality**: Semantic approach enables precise error messages with field names and paths

4. **Maintainability**: Clear separation of concerns - parser handles syntax, resolver handles semantics

### Implementation Plan

#### Phase 1: Schema Metadata (Low Risk)
- Add `decl_scope: Option<String>` to RENAMES variant in `FieldKind`
- Parser populates with parent path or node reference
- **No behavioral changes** - just metadata capture

#### Phase 2: Resolver Semantic Lookup (Medium Risk)
- Update resolver to use `decl_scope` for field searches
- Implement scope-aware field lookup: search within scope, then parent scopes
- Keep existing error codes, improve error messages

#### Phase 3: Validation Enhancement (Low Risk)
- Use semantic info for better cross-group boundary detection
- Improve contiguity validation with offset information
- Add test coverage for nested group patterns

#### Phase 4: Documentation & Tests
- Update COBOL_SUPPORT_MATRIX.md
- Un-ignore nested group tests
- Document supported patterns and limitations

## Test Coverage

### Currently Ignored Tests (Will Pass After Fix)

1. `renames_placement_with_nested_groups` (copybook-core/tests/renames_hierarchy_tests.rs:38)
   - Validates ALIAS-A under GROUP-A, ALIAS-B under GROUP-B

2. `renames_within_nested_group_valid` (copybook-core/tests/renames_resolver_negative_tests.rs:220)
   - Validates RENAMES resolves correctly within nested group scope

### Edge Cases to Validate

1. **Deep nesting**: 05 → 10 → 15 → 66
2. **Shallow nesting**: 02 → 03 → 66
3. **Cross-sibling at same level**: Should attach at 01 (existing behavior)
4. **Cross-sibling at different levels**: Should detect CBKS606 (crosses group)

## References

- **Spike Branch**: `spike/renames-nested-groups-heuristics` (preserved for future reference)
- **Issue**: #133
- **Related Error Codes**: CBKS601-608 (RENAMES validation)
- **Parser Implementation**: copybook-core/src/parser.rs:157-195 (current "pop to 01" logic)
- **Resolver Implementation**: copybook-core/src/resolver.rs (RENAMES field lookup)

## Example Copybooks for Phase R2 Fixtures

The following copybook snippets align with scenarios R1–R3 and should be converted
to golden fixtures during Phase R2 implementation.

### R1: Simple same-scope field RENAMES

```cobol
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID      PIC X(10).
          05 CUSTOMER-NAME    PIC X(30).
          66 PRIMARY-CUSTOMER RENAMES CUSTOMER-ID.
```

**Test data**: `"CUST001234Jane Doe                      "`
**Expected JSON**: `{"customer_id": "CUST001234", "customer_name": "Jane Doe"}`
**Validation**: Alias `PRIMARY-CUSTOMER` resolves to same value as `CUSTOMER-ID`

### R2: Same-scope group RENAMES

```cobol
       01 CUSTOMER-RECORD.
          05 CUSTOMER-INFO.
             10 NAME     PIC X(30).
             10 ADDRESS  PIC X(60).
          66 CUSTOMER-DETAILS RENAMES CUSTOMER-INFO.
```

**Test data**: `"John Smith                    123 Main St, Anytown, USA                            "`
**Expected JSON**: `{"customer_info": {"name": "John Smith", "address": "123 Main St, Anytown, USA"}}`
**Validation**: Alias `CUSTOMER-DETAILS` attached under `CUSTOMER-RECORD`, references `CUSTOMER-INFO` group

### R3: Nested group RENAMES

```cobol
       01 POLICY-RECORD.
          05 POLICY-INFO.
             10 POLICY-NUMBER PIC X(10).
             10 POLICY-DATES.
                15 START-DATE PIC X(8).
                15 END-DATE   PIC X(8).
          66 POLICY-PERIOD RENAMES POLICY-DATES.
```

**Test data**: `"POL1234567202501012025123"`
**Expected JSON**: `{"policy_info": {"policy_number": "POL1234567", "policy_dates": {"start_date": "20250101", "end_date": "20251231"}}}`
**Validation**: Alias `POLICY-PERIOD` attached under `POLICY-INFO`, references `POLICY-DATES` subtree

### R4: RENAMES + REDEFINES (future work)

```cobol
       01 TRANSACTION-RECORD.
          05 TRANS-TYPE  PIC X(1).
          05 TRANS-DATA.
             10 CHECK-DATA REDEFINES TRANS-DATA.
                15 CHECK-NUM PIC 9(8).
             10 CARD-DATA REDEFINES TRANS-DATA.
                15 CARD-NUM  PIC 9(16).
          66 PAYMENT-INFO RENAMES CHECK-DATA THRU CHECK-DATA.
```

**Status**: 🚫 Not supported in R2 - requires design for REDEFINES alias semantics

### R5: RENAMES + OCCURS (future work)

```cobol
       01 ORDER-RECORD.
          05 LINE-ITEMS OCCURS 10 TIMES.
             10 ITEM-CODE PIC X(5).
             10 QUANTITY  PIC 9(3).
          66 ORDER-ITEMS RENAMES LINE-ITEMS.
```

**Status**: 🚫 Not supported in R2 - requires design for OCCURS array alias semantics

## Next Steps

### Phase R1 ✅ Complete
- Design doc updated with scenarios table and expected semantics
- Support matrix updated with split RENAMES categories
- Example copybooks documented for R2 fixture development

### Phase R2 (Implementation)
1. Implement resolver-based semantic attach for R3 (nested group RENAMES)
2. Add codec projection for alias access (all scenarios)
3. Create golden fixtures from example copybooks above
4. Update `COBOL_SUPPORT_MATRIX.md` to mark R2/R3 as ✅
5. Un-ignore nested group tests in test suites

### Phase R3 (Documentation & CI)
1. Update `CLAUDE.md` COBOL feature section
2. Update `docs/REPORT.md` completeness assessment
3. Update `docs/ROADMAP.md` milestone tracking
4. Add CI validation hook for RENAMES golden fixtures

---

**Document Status**: Implementation contract (Phase R1 complete)
**Last Updated**: 2025-11-19
**Author**: Claude Code (automated analysis of Issue #133)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
