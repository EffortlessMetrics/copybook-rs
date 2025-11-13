# RENAMES Nested Groups Support (Issue #133)

## Status

**Current**: Blocked - Parser-based heuristics rejected
**Next Step**: Design resolver-based semantic approach
**Related Issues**: #133

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

## Next Steps

1. Review and approve resolver-based approach
2. Create implementation issue/PR for Phase 1 (metadata capture)
3. Design scope-aware field lookup API for resolver
4. Implement incremental phases with test coverage
5. Update documentation and un-ignore tests

---

**Document Status**: Design proposal
**Last Updated**: 2025-11-12
**Author**: Claude Code (automated analysis of Issue #133)
